port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, input, label, li, main_, output, text, ul)
import Html.Attributes exposing (attribute, checked, class, for, id, name, required, type_)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Iso8601
import Json.Decode exposing (Decoder, andThen, at, fail, field, list, map2, map3, map4, string, succeed)
import Json.Encode as Encode
import ListExtra
import RemoteData exposing (RemoteData(..))
import Task
import Time exposing (Posix, Weekday(..), Zone, posixToMillis)
import TodoList exposing (..)



-- Main


main : Program (Maybe String) TopModel Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type TopModel
    = LoggedOut String (Maybe ( Posix, Zone ))
    | TopLoading User (Maybe ( Posix, Zone )) (Maybe ( List TaskDefinition, List Completion ))
    | TopSuccess Model
    | TopFailure Http.Error


type alias Model =
    { tasks : List TaskDefinition
    , completions : List Completion
    , user : User
    , now : ( Posix, Zone )
    , toasts : List Toast
    }


type alias Toast =
    { title : String
    , message : String
    }


init : Maybe String -> ( TopModel, Cmd Msg )
init user =
    case user of
        Just name ->
            ( TopLoading name Nothing Nothing
            , Cmd.batch
                [ getTasksAndCompletions
                , getCurrentTime
                ]
            )

        Nothing ->
            ( LoggedOut "" Nothing
            , getCurrentTime
            )


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GotCurrentTime (Time.now |> Task.andThen (\now -> Task.map (Tuple.pair now) Time.here))


initialModel : User -> ( Posix, Zone ) -> ( List TaskDefinition, List Completion ) -> Model
initialModel user now ( tasks, completions ) =
    { tasks = tasks
    , completions = completions
    , user = user
    , now = now
    , toasts = []
    }


toastFromHttpError : Http.Error -> Toast
toastFromHttpError error =
    case error of
        Timeout ->
            Toast "Erreur serveur" "Le serveur a mis trop de temps à répondre"

        NetworkError ->
            Toast "Problème de connexion" "Veuillez vérifier votre connexion internet"

        BadStatus code ->
            let
                message =
                    if code < 500 then
                        "Veuillez contacter le support"

                    else
                        "Veuillez réessayer plus tard"
            in
            Toast ("Erreur serveur (" ++ String.fromInt code ++ ")") message

        BadBody err ->
            Toast "Erreur serveur" ("La réponse du serveur est inattendue : " ++ err)

        BadUrl url ->
            Toast "Erreur serveur" ("Url invalide : " ++ url)



-- Update


type Msg
    = GotTasksAndCompletions (RemoteData ( List TaskDefinition, List Completion ))
    | GotCurrentTime ( Posix, Zone )
    | TodoChecked TodoItem Bool
    | MarkTodoAsDone TodoItem Posix
    | CompletionSaved Completion (RemoteData Completion)
    | CompletionDeleted Completion (RemoteData String)
    | UsernameInput String
    | SaveUsername
    | Refresh Posix
    | RemoveToast Int


update : Msg -> TopModel -> ( TopModel, Cmd Msg )
update msg topModel =
    case ( msg, topModel ) of
        ( GotTasksAndCompletions (Success ( tasks, completions )), TopLoading user Nothing _ ) ->
            ( TopLoading user Nothing (Just ( tasks, completions ))
            , Cmd.none
            )

        ( GotTasksAndCompletions (Success ( tasks, completions )), TopLoading user (Just now) _ ) ->
            ( TopSuccess (initialModel user now ( tasks, completions ))
            , Cmd.none
            )

        ( GotTasksAndCompletions (Success ( tasks, completions )), TopSuccess model ) ->
            ( TopSuccess { model | tasks = tasks, completions = completions }
            , Cmd.none
            )

        ( GotTasksAndCompletions (Failure error), _ ) ->
            ( TopFailure error
            , Cmd.none
            )

        ( GotCurrentTime now, TopLoading user _ Nothing ) ->
            ( TopLoading user (Just now) Nothing
            , Cmd.none
            )

        ( GotCurrentTime now, TopLoading user _ (Just ( tasks, completions )) ) ->
            ( TopSuccess (initialModel user now ( tasks, completions ))
            , Cmd.none
            )

        ( GotCurrentTime now, LoggedOut user _ ) ->
            ( LoggedOut user (Just now)
            , Cmd.none
            )

        ( GotCurrentTime now, TopSuccess model ) ->
            ( TopSuccess { model | now = now }
            , Cmd.none
            )

        ( UsernameInput str, LoggedOut _ now ) ->
            ( LoggedOut str now
            , Cmd.none
            )

        ( SaveUsername, LoggedOut usernameInputValue now ) ->
            ( TopLoading usernameInputValue now Nothing
            , Cmd.batch
                [ setStorage usernameInputValue
                , getTasksAndCompletions
                ]
            )

        ( _, TopSuccess model ) ->
            let
                ( newModel, cmd ) =
                    updateSuccess msg model
            in
            ( TopSuccess newModel
            , cmd
            )

        ( _, _ ) ->
            ( topModel, Cmd.none )


updateSuccess : Msg -> Model -> ( Model, Cmd Msg )
updateSuccess msg model =
    case msg of
        Refresh now ->
            ( { model | now = ( now, Tuple.second model.now ) }
            , getTasksAndCompletions
            )

        RemoveToast index ->
            ( { model
                | toasts = ListExtra.dropAtIndex index model.toasts
              }
            , Cmd.none
            )

        CompletionSaved oldCompletion (Failure err) ->
            ( { model
                | completions =
                    List.filter (\completion -> completion /= oldCompletion) model.completions
                , toasts =
                    model.toasts ++ [ toastFromHttpError err ]
              }
            , Cmd.none
            )

        CompletionSaved oldCompletion (Success savedCompletion) ->
            ( { model
                | completions =
                    List.map
                        (\completion ->
                            if completion == oldCompletion then
                                savedCompletion

                            else
                                completion
                        )
                        model.completions
              }
            , Cmd.none
            )

        CompletionSaved _ Loading ->
            ( model
            , Cmd.none
            )

        CompletionDeleted completion (Failure err) ->
            ( { model
                | completions = completion :: model.completions
                , toasts = model.toasts ++ [ toastFromHttpError err ]
              }
            , Cmd.none
            )

        CompletionDeleted _ _ ->
            ( model
            , Cmd.none
            )

        TodoChecked todoItem _ ->
            case todoItem.status of
                NotDone ->
                    ( model
                    , Task.perform (MarkTodoAsDone todoItem) Time.now
                    )

                Done _ _ completion ->
                    ( { model
                        | completions = List.filter (\c -> c /= completion) model.completions
                      }
                    , deleteCompletion completion
                    )

        MarkTodoAsDone todo now ->
            let
                completion =
                    Completion "tmp-id" model.user todo.task.id now
            in
            ( { model
                | completions = completion :: model.completions
              }
            , postCompletion completion
            )

        GotTasksAndCompletions (Success ( tasks, completions )) ->
            ( { model | tasks = tasks, completions = completions }
            , Cmd.none
            )

        GotTasksAndCompletions _ ->
            ( model
            , Cmd.none
            )

        GotCurrentTime now ->
            ( { model | now = now }
            , Cmd.none
            )

        UsernameInput _ ->
            ( model, Cmd.none )

        SaveUsername ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : TopModel -> Sub Msg
subscriptions _ =
    Time.every (1000 * 60) Refresh



-- View


view : TopModel -> Html Msg
view topModel =
    case topModel of
        LoggedOut _ _ ->
            main_ [] [ viewLoginForm ]

        TopLoading _ _ _ ->
            main_ [] [ viewLoading ]

        TopFailure error ->
            main_ [] [ viewLoadingFailed error ]

        TopSuccess model ->
            div
                []
                [ main_ [] [ viewSuccess model ]
                , viewToasts model.toasts
                ]


viewSuccess : Model -> Html Msg
viewSuccess model =
    let
        ( done, notDone ) =
            model.completions
                |> TodoList.fromTasksAndCompletions model.now model.tasks
                |> List.partition (\todoItem -> todoItem.status /= NotDone)

        sortByCompletedAtDesc =
            List.sortWith
                (\a b ->
                    case ( a.status, b.status ) of
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
        [ class "todolists-container" ]
        [ viewTodoListNotDone 0 (ListExtra.uniqueWithCount notDone)
        , viewTodoListDone 1 (sortByCompletedAtDesc done)
        ]


viewLoginForm : Html Msg
viewLoginForm =
    form
        [ onSubmit SaveUsername ]
        [ label [ for "name" ] [ text "Votre nom" ]
        , input [ id "name", name "name", required True, onInput UsernameInput ] []
        , button [] [ text "Sauvegarder" ]
        ]


viewLoading : Html msg
viewLoading =
    text "Chargement des tâches"


viewLoadingFailed : Http.Error -> Html msg
viewLoadingFailed err =
    case err of
        BadBody message ->
            text ("Erreur de chargement des tâches: " ++ message)

        _ ->
            text "Erreur de chargement des tâches"


viewToasts : List Toast -> Html Msg
viewToasts toasts =
    toasts
        |> List.indexedMap viewToast
        |> div [ class "toast-list" ]


viewToast : Int -> Toast -> Html Msg
viewToast index toast =
    output
        [ class "toast", attribute "role" "alert" ]
        [ div
            []
            [ div [] [ text toast.title ]
            , div [ class "message" ] [ text toast.message ]
            ]
        , button [ onClick (RemoveToast index) ] [ text "fermer" ]
        ]


viewTodoListDone : Int -> List TodoItem -> Html Msg
viewTodoListDone i todoList_ =
    todoList_
        |> List.indexedMap (\j todolist -> viewTodoDone (String.fromInt i ++ String.fromInt j) todolist)
        |> ul [ attribute "role" "list" ]


viewTodoListNotDone : Int -> List ( TodoItem, Int ) -> Html Msg
viewTodoListNotDone i todoList_ =
    todoList_
        |> List.indexedMap (\j todolist -> viewTodoNotDone (String.fromInt i ++ String.fromInt j) todolist)
        |> ul [ attribute "role" "list" ]


viewTodoDone : String -> TodoItem -> Html Msg
viewTodoDone index todoItem =
    let
        id_ =
            todoItem.task.name ++ index
    in
    li
        [ class "todo" ]
        [ div
            []
            [ input [ type_ "checkbox", checked True, id id_, onCheck (TodoChecked todoItem) ] [] ]
        , div
            []
            [ label [ for id_ ] [ text todoItem.task.name ]
            , case todoItem.status of
                Done user timeAgo_ _ ->
                    div
                        [ class "completion-tags" ]
                        [ div [ class "tag user" ] [ text user ]
                        , div [ class "tag time-ago" ] [ text (viewTimeAgo timeAgo_) ]
                        ]

                NotDone ->
                    div
                        [ class ("tag " ++ frequencyToClass todoItem.task.frequency) ]
                        [ text (frequencyToString todoItem.task.frequency 1) ]
            ]
        ]


viewTodoNotDone : String -> ( TodoItem, Int ) -> Html Msg
viewTodoNotDone index ( todoItem, count ) =
    let
        id_ =
            todoItem.task.name ++ index
    in
    li
        [ class "todo" ]
        [ div
            []
            [ input [ type_ "checkbox", checked False, id id_, onCheck (TodoChecked todoItem) ] [] ]
        , div
            []
            [ label [ for id_ ] [ text todoItem.task.name ]
            , case todoItem.status of
                Done user timeAgo_ _ ->
                    div
                        [ class "completion-tags" ]
                        [ div [ class "tag user" ] [ text user ]
                        , div [ class "tag time-ago" ] [ text (viewTimeAgo timeAgo_) ]
                        ]

                NotDone ->
                    div
                        [ class ("tag " ++ frequencyToClass todoItem.task.frequency) ]
                        [ text (frequencyToString todoItem.task.frequency count) ]
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


frequencyToString : Frequency -> Int -> String
frequencyToString frequency count =
    case frequency of
        TwiceADay ->
            "aujourd'hui"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        FourTimesAWeek ->
            "cette semaine"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        TwiceAWeek ->
            "cette semaine"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        EveryWeek ->
            "cette semaine"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        EveryOtherWeek ->
            "ce mois-ci"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        EveryMonth ->
            "ce mois-ci"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )


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


getTasksAndCompletions : Cmd Msg
getTasksAndCompletions =
    apiGet "/get-tasks-and-completions" GotTasksAndCompletions tasksAndCompletionsDecoder


postCompletion : Completion -> Cmd Msg
postCompletion completion =
    apiPost "/create-completion" (Http.jsonBody (completionEncoder completion)) (CompletionSaved completion) completionDecoder


deleteCompletion : Completion -> Cmd Msg
deleteCompletion completion =
    apiDelete ("/delete-completion?id=" ++ completion.id) (CompletionDeleted completion) (Json.Decode.succeed "Ok")


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


tasksAndCompletionsDecoder : Decoder ( List TaskDefinition, List Completion )
tasksAndCompletionsDecoder =
    map2 Tuple.pair
        (field "tasks" (list taskDecoder))
        (field "completions" (list completionDecoder))


taskDecoder : Decoder TaskDefinition
taskDecoder =
    map3 TaskDefinition
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
