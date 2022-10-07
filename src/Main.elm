port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, label, li, main_, text, time, ul)
import Html.Attributes exposing (attribute, checked, class, for, id, name, required, type_)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Http exposing (Error(..))
import Iso8601
import Json.Decode exposing (Decoder, andThen, at, fail, field, list, map3, string, succeed)
import Json.Encode as Encode
import Platform exposing (Task)
import Task
import Time exposing (Posix, Weekday(..), Zone, millisToPosix, posixToMillis, toDay, toHour, toMillis, toMinute, toMonth, toSecond, toYear)



-- Main


main : Program (Maybe String) Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type alias Model =
    { tasks : Maybe (Result Http.Error (List Task))
    , completions : Maybe (Result Http.Error (List Completion))
    , user : Maybe String
    , now : Maybe ( Posix, Zone )
    , usernameInput : String
    }


type alias Task =
    { id : String
    , name : TaskName
    , frequency : Frequency
    }


type Frequency
    = TwiceAWeek
    | EveryWeek
    | EveryOtherWeek
    | EveryMonth


type alias TaskName =
    String


type alias Completion =
    { user : String
    , taskId : String
    , completedAt : Posix
    }


type alias Todo =
    { task : Task
    , start : Posix
    }


type TodoStatus
    = Done String TimeAgo
    | NotDone


type TimeAgo
    = DaysAgo Int
    | WeeksAgo Int
    | LongAgo


init : Maybe String -> ( Model, Cmd Msg )
init user =
    ( { tasks = Nothing
      , completions = Nothing
      , user = user
      , now = Nothing
      , usernameInput = ""
      }
    , Cmd.batch
        [ airtableGet "/tasks?sort%5B0%5D%5Bfield%5D=frequency&sort%5B0%5D%5Bdirection%5D=asc" GotTasks tasksDecoder
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
                ( status, maybeCompletion ) =
                    todoStatus now completions todo

                completionsLeft =
                    case maybeCompletion of
                        Just completion ->
                            List.filter (\c -> c /= completion) completions

                        Nothing ->
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
    in
    case task.frequency of
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


todoStatus : ( Posix, Zone ) -> List Completion -> Todo -> ( TodoStatus, Maybe Completion )
todoStatus now completions todo =
    completions
        |> List.filter (completionMatchesTodo todo)
        |> List.head
        |> Maybe.map
            (\completion ->
                ( Done completion.user (timeAgo completion.completedAt now), Just completion )
            )
        |> Maybe.withDefault ( NotDone, Nothing )


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
    = GotTasks (Result Http.Error (List Task))
    | GotCompletions (Result Http.Error (List Completion))
    | GotCurrentTime ( Posix, Zone )
    | TodoChecked Todo Bool
    | TodoCheckedWithTime Todo Posix
    | CompletionSent (Result Http.Error Completion)
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

        GotTasks result ->
            ( { model | tasks = Just result }
            , Cmd.none
            )

        GotCompletions result ->
            ( { model | completions = Just result }
            , Cmd.none
            )

        CompletionSent _ ->
            ( model, Cmd.none )

        TodoChecked todo checked ->
            if checked then
                ( model
                , Task.perform (TodoCheckedWithTime todo) Time.now
                )

            else
                ( model, Cmd.none )

        TodoCheckedWithTime todo now ->
            case model.user of
                Just user ->
                    let
                        completion =
                            Completion user todo.task.id now
                    in
                    ( { model
                        | completions =
                            model.completions
                                |> Maybe.map (Result.map ((::) completion))
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
        Just httpResult ->
            case httpResult of
                Ok tasks ->
                    model.completions
                        |> Maybe.map (Result.withDefault [])
                        |> Maybe.withDefault []
                        |> todoList model.now tasks
                        |> List.partition (\( _, status ) -> status /= NotDone)
                        |> (\( done, notDone ) ->
                                [ notDone, done ]
                                    |> List.map viewTodoList
                                    |> div []
                           )

                Err err ->
                    case err of
                        BadBody message ->
                            text ("Erreur de chargement des tâches: " ++ message)

                        _ ->
                            text "Erreur de chargement des tâches"

        Nothing ->
            text "Chargement des tâches..."


viewLogin : Html Msg
viewLogin =
    form
        [ onSubmit SaveUsername ]
        [ label [ for "name" ] [ text "Votre nom" ]
        , input [ id "name", name "name", required True, onInput UsernameInput ] []
        , button [] [ text "Sauvegarder" ]
        ]


viewTodoList : List ( Todo, TodoStatus ) -> Html Msg
viewTodoList todoList_ =
    ul [ attribute "role" "list" ] (List.indexedMap viewTodo todoList_)


viewTodo : Int -> ( Todo, TodoStatus ) -> Html Msg
viewTodo index ( todo, status ) =
    let
        isDone =
            case status of
                Done _ _ ->
                    True

                NotDone ->
                    False

        id_ =
            todo.task.name ++ String.fromInt index
    in
    li
        [ class "todo" ]
        [ div [] [ input [ type_ "checkbox", checked isDone, id id_, onCheck (TodoChecked todo) ] [] ]
        , div
            []
            [ label [ for id_ ] [ text todo.task.name ]
            , div [ class ("tag " ++ frequencyToClass todo.task.frequency) ] [ text (frequencyToString todo.task.frequency) ]
            , case status of
                Done user timeAgo_ ->
                    div
                        [ class "completion-tags" ]
                        [ div [ class "tag user" ] [ text user ]
                        , div [ class "tag time-ago" ] [ text (viewTimeAgo timeAgo_) ]
                        ]

                NotDone ->
                    text ""
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
        TwiceAWeek ->
            "twice-a-week"

        EveryWeek ->
            "every-week"

        EveryOtherWeek ->
            "every-other-week"

        EveryMonth ->
            "every-month"



-- Http


airtableGet : String -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
airtableGet path msg decoder =
    airtableRequest "GET" path Http.emptyBody msg decoder


airtablePost : String -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
airtablePost path body msg decoder =
    airtableRequest "POST" path body msg decoder


airtableRequest : String -> String -> Http.Body -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
airtableRequest method path body msg decoder =
    Http.request
        { method = method
        , headers = [ Http.header "Authorization" "Bearer keyDcHbvnKUCGYo9l" ]
        , url = "https://api.airtable.com/v0/appzk3oeSLhSwr9Dd" ++ path
        , body = body
        , expect = Http.expectJson msg decoder
        , timeout = Nothing
        , tracker = Nothing
        }


postCompletion : Completion -> Cmd Msg
postCompletion completion =
    airtablePost "/completions" (Http.jsonBody (completionEncoder completion)) CompletionSent completionDecoder


completionEncoder : Completion -> Encode.Value
completionEncoder completion =
    Encode.object
        [ ( "fields"
          , Encode.object
                [ ( "user", Encode.string completion.user )
                , ( "completed_at", Iso8601.encode completion.completedAt )
                , ( "task", Encode.list Encode.string [ completion.taskId ] )
                ]
          )
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
                    "every other week" ->
                        succeed EveryOtherWeek

                    "every week" ->
                        succeed EveryWeek

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
    map3 Completion
        (at [ "fields", "user" ] string)
        (at [ "fields", "task" ] taskIdDecoder)
        (at [ "fields", "completed_at" ] Iso8601.decoder)


taskIdDecoder : Decoder String
taskIdDecoder =
    list string
        |> andThen
            (\names ->
                case names of
                    name :: [] ->
                        succeed name

                    _ ->
                        fail "Expected exactly one task"
            )



-- Ports


port setStorage : String -> Cmd msg
