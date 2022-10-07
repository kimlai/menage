port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, label, li, main_, text, time, ul)
import Html.Attributes exposing (attribute, checked, class, for, id, name, required, type_)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Http exposing (Error(..))
import Iso8601
import Json.Decode exposing (Decoder, andThen, at, fail, field, list, map3, map4, string, succeed)
import Json.Encode as Encode
import ListExtra
import Platform exposing (Task)
import RemoteData exposing (RemoteData(..))
import Task
import Time exposing (Posix, Weekday(..), Zone, millisToPosix, posixToMillis, toHour, toMillis, toMinute, toSecond)



-- Main


main : Program (Maybe String) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type alias Model =
    { tasks : RemoteData (List Task)
    , completions : RemoteData (List Completion)
    , user : Maybe String
    , now : Maybe ( Posix, Zone )
    , usernameInput : String
    }


type alias TaskID =
    String


type alias Task =
    { id : TaskID
    , name : TaskName
    , frequency : Frequency
    }


type Frequency
    = TwiceADay
    | FourTimesAWeek
    | TwiceAWeek
    | EveryWeek
    | EveryOtherWeek
    | EveryMonth


type alias TaskName =
    String


type alias CompletionID =
    String


type alias Completion =
    { id : CompletionID
    , user : String
    , taskId : String
    , completedAt : Posix
    }


type alias Todo =
    { task : Task
    , start : Posix
    }


type TodoStatus
    = Done String TimeAgo Completion
    | NotDone


type TimeAgo
    = DaysAgo Int
    | WeeksAgo Int
    | LongAgo


init : Maybe String -> ( Model, Cmd Msg )
init user =
    ( { tasks = Loading
      , completions = Loading
      , user = user
      , now = Nothing
      , usernameInput = ""
      }
    , Cmd.batch
        [ airtableGetTasks
        , airtableGet "/completions" GotCompletions completionsDecoder
        , Task.perform GotCurrentTime (Time.now |> Task.andThen (\now -> Task.map (Tuple.pair now) Time.here))
        ]
    )


todoList : Maybe ( Posix, Zone ) -> List Task -> List Completion -> List ( Todo, TodoStatus )
todoList maybeNow tasks completions =
    case maybeNow of
        Just now ->
            tasks
                |> List.concatMap (todosFromTask now)
                |> computeStatus now completions

        Nothing ->
            []


{-| Using a recursive function to go through the list enables removing completions from the potential matches as we go.
-}
computeStatus : ( Posix, Zone ) -> List Completion -> List Todo -> List ( Todo, TodoStatus )
computeStatus now completions todos =
    case todos of
        [] ->
            []

        todo :: rest ->
            let
                status =
                    todoStatus now completions todo

                completionsLeft =
                    case status of
                        Done _ _ completion ->
                            List.filter (\c -> c /= completion) completions

                        NotDone ->
                            completions
            in
            ( todo, status ) :: computeStatus now completionsLeft rest


todosFromTask : ( Posix, Zone ) -> Task -> List Todo
todosFromTask now task =
    let
        thisWeek =
            { task = task
            , start = lastMonday now
            }

        today_ =
            { task = task
            , start = today now
            }
    in
    case task.frequency of
        TwiceADay ->
            List.repeat 2 today_

        FourTimesAWeek ->
            List.repeat 4 thisWeek

        TwiceAWeek ->
            List.repeat 2 thisWeek

        EveryWeek ->
            [ thisWeek ]

        EveryOtherWeek ->
            [ { task = task
              , start = lastHalfDayOfMonth now
              }
            ]

        EveryMonth ->
            [ { task = task
              , start = firstDayOfMonth now
              }
            ]


previousDay : Posix -> Posix
previousDay time =
    millisToPosix
        (posixToMillis time - 1000 * 60 * 60 * 24)


atMidnight : Posix -> Zone -> Posix
atMidnight time zone =
    let
        hours =
            toHour zone time * 60 * 60 * 1000

        minutes =
            toMinute zone time * 60 * 1000

        seconds =
            toSecond zone time * 1000

        millis =
            toMillis zone time
    in
    millisToPosix
        (posixToMillis time - hours - minutes - seconds - millis)


today : ( Posix, Zone ) -> Posix
today ( now, zone ) =
    atMidnight now zone


lastMonday : ( Posix, Zone ) -> Posix
lastMonday ( now, zone ) =
    if Time.toWeekday zone now == Mon then
        atMidnight now zone

    else
        lastMonday ( previousDay now, zone )


firstDayOfMonth : ( Posix, Zone ) -> Posix
firstDayOfMonth ( now, zone ) =
    if Time.toDay zone now == 1 then
        atMidnight now zone

    else
        firstDayOfMonth ( previousDay now, zone )


lastHalfDayOfMonth : ( Posix, Zone ) -> Posix
lastHalfDayOfMonth ( now, zone ) =
    if Time.toDay zone now == 1 || Time.toDay zone now == 15 then
        atMidnight now zone

    else
        lastHalfDayOfMonth ( previousDay now, zone )


todoStatus : ( Posix, Zone ) -> List Completion -> Todo -> TodoStatus
todoStatus now completions todo =
    completions
        |> List.filter (completionMatchesTodo todo)
        |> List.head
        |> Maybe.map
            (\completion ->
                Done completion.user (timeAgo completion.completedAt now) completion
            )
        |> Maybe.withDefault NotDone


completionMatchesTodo : Todo -> Completion -> Bool
completionMatchesTodo todo completion =
    completion.taskId == todo.task.id && posixToMillis completion.completedAt > posixToMillis todo.start


timeAgo : Posix -> ( Time.Posix, Zone ) -> TimeAgo
timeAgo time ( now, zone ) =
    let
        deltaInMillis =
            posixToMillis (atMidnight now zone) - posixToMillis (atMidnight time zone)

        deltaInDays =
            floor (toFloat deltaInMillis / 1000 / 60 / 60 / 24)
    in
    if deltaInDays < 7 then
        DaysAgo deltaInDays

    else if deltaInDays < 30 then
        WeeksAgo (floor (toFloat deltaInDays / 7))

    else
        LongAgo



-- Update


type Msg
    = GotTasks (RemoteData (List Task))
    | GotCompletions (RemoteData (List Completion))
    | GotCurrentTime ( Posix, Zone )
    | TodoChecked Todo TodoStatus Bool
    | MarkTotoAsDone Todo Posix
    | CompletionSaved Completion (RemoteData Completion)
    | CompletionDeleted (RemoteData String)
    | UsernameInput String
    | SaveUsername


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCurrentTime hereAndNow ->
            ( { model | now = Just hereAndNow }
            , Cmd.none
            )

        UsernameInput str ->
            ( { model | usernameInput = str }
            , Cmd.none
            )

        SaveUsername ->
            ( { model | user = Just model.usernameInput, usernameInput = "" }
            , setStorage model.usernameInput
            )

        GotTasks tasks ->
            ( { model | tasks = tasks }
            , Cmd.none
            )

        GotCompletions result ->
            ( { model | completions = result }
            , Cmd.none
            )

        CompletionSaved oldCompletion (Failure _) ->
            ( { model
                | completions =
                    RemoteData.map
                        (List.filter (\completion -> completion == oldCompletion))
                        model.completions
              }
            , Cmd.none
            )

        CompletionSaved oldCompletion (Success savedCompletion) ->
            ( { model
                | completions =
                    RemoteData.map
                        (List.map
                            (\completion ->
                                if completion == oldCompletion then
                                    savedCompletion

                                else
                                    completion
                            )
                        )
                        model.completions
              }
            , Cmd.none
            )

        CompletionSaved _ _ ->
            ( model
            , Cmd.none
            )

        CompletionDeleted _ ->
            ( model
            , Cmd.none
            )

        TodoChecked todo NotDone _ ->
            ( model
            , Task.perform (MarkTotoAsDone todo) Time.now
            )

        TodoChecked _ (Done _ _ completion) _ ->
            ( { model
                | completions =
                    RemoteData.map
                        (List.filter (\c -> c /= completion))
                        model.completions
              }
            , deleteCompletion completion
            )

        MarkTotoAsDone todo now ->
            case model.user of
                Just user ->
                    let
                        completion =
                            Completion "tmp-id" user todo.task.id now
                    in
                    ( { model
                        | completions =
                            RemoteData.map ((::) completion) model.completions
                      }
                    , postCompletion completion
                    )

                Nothing ->
                    ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    main_
        []
        [ case model.user of
            Just _ ->
                viewLoggedIn model

            Nothing ->
                viewLogin
        ]


viewLoggedIn : Model -> Html Msg
viewLoggedIn model =
    case model.tasks of
        Success tasks ->
            let
                ( done, notDone ) =
                    model.completions
                        |> RemoteData.withDefault []
                        |> todoList model.now tasks
                        |> List.partition (\( _, status ) -> status /= NotDone)

                sortByCompletedAtDesc =
                    List.sortWith
                        (\( _, a ) ( _, b ) ->
                            case ( a, b ) of
                                ( Done _ _ compA, Done _ _ compB ) ->
                                    compare (posixToMillis compB.completedAt) (posixToMillis compA.completedAt)

                                -- all those other branches cannot happen for a list of "done" todos
                                -- might be worth it to refactor away
                                ( NotDone, NotDone ) ->
                                    GT

                                ( NotDone, Done _ _ _ ) ->
                                    LT

                                ( Done _ _ _, NotDone ) ->
                                    GT
                        )
            in
            div
                []
                [ viewTodoList 0 (ListExtra.unique notDone)
                , viewTodoList 1 (sortByCompletedAtDesc done)
                ]

        Loading ->
            text "Chargement des tâches"

        Failure err ->
            case err of
                BadBody message ->
                    text ("Erreur de chargement des tâches: " ++ message)

                _ ->
                    text "Erreur de chargement des tâches"


viewLogin : Html Msg
viewLogin =
    form
        [ onSubmit SaveUsername ]
        [ label [ for "name" ] [ text "Votre nom" ]
        , input [ id "name", name "name", required True, onInput UsernameInput ] []
        , button [] [ text "Sauvegarder" ]
        ]


viewTodoList : Int -> List ( Todo, TodoStatus ) -> Html Msg
viewTodoList i todoList_ =
    todoList_
        |> List.indexedMap (\j todos -> viewTodo (String.fromInt i ++ String.fromInt j) todos)
        |> ul [ attribute "role" "list" ]


viewTodo : String -> ( Todo, TodoStatus ) -> Html Msg
viewTodo index ( todo, status ) =
    let
        isDone =
            case status of
                Done _ _ _ ->
                    True

                NotDone ->
                    False

        id_ =
            todo.task.name ++ index
    in
    li
        [ class "todo" ]
        [ div [] [ input [ type_ "checkbox", checked isDone, id id_, onCheck (TodoChecked todo status) ] [] ]
        , div
            []
            [ label [ for id_ ] [ text todo.task.name ]
            , case status of
                Done user timeAgo_ _ ->
                    div
                        [ class "completion-tags" ]
                        [ div [ class "tag user" ] [ text user ]
                        , div [ class "tag time-ago" ] [ text (viewTimeAgo timeAgo_) ]
                        ]

                NotDone ->
                    div [ class ("tag " ++ frequencyToClass todo.task.frequency) ] [ text (frequencyToString todo.task.frequency) ]
            ]
        ]


viewTimeAgo : TimeAgo -> String
viewTimeAgo timeAgo_ =
    case timeAgo_ of
        DaysAgo 0 ->
            "aujourd'hui"

        DaysAgo 1 ->
            "hier"

        DaysAgo days ->
            "il y a " ++ String.fromInt days ++ " jours"

        WeeksAgo 1 ->
            "la semaine dernière"

        WeeksAgo weeks ->
            "il y a " ++ String.fromInt weeks ++ " semaines"

        LongAgo ->
            "il y a plus d'un mois"


frequencyToString : Frequency -> String
frequencyToString frequency =
    case frequency of
        TwiceADay ->
            "2x/jour"

        FourTimesAWeek ->
            "4x/semaine"

        TwiceAWeek ->
            "2x/semaine"

        EveryWeek ->
            "1x/semaine"

        EveryOtherWeek ->
            "2x/mois"

        EveryMonth ->
            "1x/mois"


frequencyToClass : Frequency -> String
frequencyToClass frequency =
    case frequency of
        TwiceADay ->
            "twice-a-day"

        TwiceAWeek ->
            "twice-a-week"

        FourTimesAWeek ->
            "four-times-a-week"

        EveryWeek ->
            "every-week"

        EveryOtherWeek ->
            "every-other-week"

        EveryMonth ->
            "every-month"



-- Http


airtableGet : String -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
airtableGet path msg decoder =
    airtableRequest "GET" path Http.emptyBody msg decoder


airtablePost : String -> Http.Body -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
airtablePost path body msg decoder =
    airtableRequest "POST" path body msg decoder


airtableDelete : String -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
airtableDelete path msg decoder =
    airtableRequest "DELETE" path Http.emptyBody msg decoder


airtableRequest : String -> String -> Http.Body -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
airtableRequest method path body msg decoder =
    Http.request
        { method = method
        , headers = [ Http.header "Authorization" "Bearer keyDcHbvnKUCGYo9l" ]
        , url = "https://api.airtable.com/v0/appzk3oeSLhSwr9Dd" ++ path
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


airtableGetTasks : Cmd Msg
airtableGetTasks =
    airtableGet
        "/tasks?sort%5B0%5D%5Bfield%5D=frequency&sort%5B0%5D%5Bdirection%5D=asc"
        GotTasks
        tasksDecoder


airTableGetCompletions : Cmd Msg
airTableGetCompletions =
    airtableGet "/completions" GotCompletions completionsDecoder


postCompletion : Completion -> Cmd Msg
postCompletion completion =
    apiPost "/create-completion" (Http.jsonBody (completionEncoder completion)) (CompletionSaved completion) completionDecoder


deleteCompletion : Completion -> Cmd Msg
deleteCompletion { id } =
    airtableDelete ("/completions/" ++ id) CompletionDeleted (Json.Decode.succeed "Ok")


apiGet : String -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
apiGet path msg decoder =
    apiRequest "GET" path Http.emptyBody msg decoder


apiPost : String -> Http.Body -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
apiPost path body msg decoder =
    apiRequest "POST" path body msg decoder


apiDelete : String -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
apiDelete path msg decoder =
    apiRequest "DELETE" path Http.emptyBody msg decoder


apiRequest : String -> String -> Http.Body -> (RemoteData a -> msg) -> Decoder a -> Cmd msg
apiRequest method path body msg decoder =
    Http.request
        { method = method
        , headers = []
        , url = "/.netlify/functions" ++ path
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> msg) decoder
        , timeout = Nothing
        , tracker = Nothing
        }


completionEncoder : Completion -> Encode.Value
completionEncoder completion =
    Encode.object
        [ ( "user", Encode.string completion.user )
        , ( "completed_at", Iso8601.encode completion.completedAt )
        , ( "task", Encode.list Encode.string [ completion.taskId ] )
        ]


tasksDecoder : Decoder (List Task)
tasksDecoder =
    field "records" (list taskDecoder)


taskDecoder : Decoder Task
taskDecoder =
    map3 Task
        (field "id" string)
        (at [ "fields", "name" ] string)
        (at [ "fields", "frequency" ] frequencyDecoder)


frequencyDecoder : Decoder Frequency
frequencyDecoder =
    string
        |> andThen
            (\str ->
                case str of
                    "twice a day" ->
                        succeed TwiceADay

                    "every other week" ->
                        succeed EveryOtherWeek

                    "every week" ->
                        succeed EveryWeek

                    "four times a week" ->
                        succeed FourTimesAWeek

                    "twice a week" ->
                        succeed TwiceAWeek

                    "every month" ->
                        succeed EveryMonth

                    _ ->
                        fail "Could not decode frequency"
            )


completionsDecoder : Decoder (List Completion)
completionsDecoder =
    field "records" (list completionDecoder)


completionDecoder : Decoder Completion
completionDecoder =
    map4 Completion
        (field "id" string)
        (at [ "fields", "user" ] string)
        (at [ "fields", "task" ] taskIdDecoder)
        (at [ "fields", "completed_at" ] Iso8601.decoder)


taskIdDecoder : Decoder TaskID
taskIdDecoder =
    list string
        |> andThen
            (\ids ->
                case ids of
                    id :: [] ->
                        succeed id

                    _ ->
                        fail "Expected exactly one task"
            )



-- Ports


port setStorage : String -> Cmd msg
