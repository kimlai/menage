port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, form, h2, input, label, li, main_, output, span, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (attribute, checked, class, for, id, name, property, required, type_)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Iso8601
import Json.Decode exposing (Decoder, andThen, at, fail, field, list, map2, map3, map5, string, succeed)
import Json.Encode as Encode
import ListExtra
import RemoteData exposing (RemoteData(..), withDefault)
import Task
import Time exposing (Posix, Weekday(..), Zone, posixToMillis)
import TodoList exposing (..)



-- Main


main : Program (Maybe String) TopModel Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type TopModel
    = Login LoginModel
    | TopLoading LoadingModel
    | TopSuccess Model
    | TopFailure FailureModel


type alias LoginModel =
    { usernameInputValue : String
    , maybeNow : Maybe ( Posix, Zone )
    }


type alias LoadingModel =
    { user : User
    , maybeNow : Maybe ( Posix, Zone )
    , maybeTasksAndCompletions : Maybe ( List TaskDefinition, List Completion )
    }


type alias FailureModel =
    { error : Http.Error
    }


type alias Model =
    { tasks : List TaskDefinition
    , completions : List Completion
    , user : User
    , now : ( Posix, Zone )
    , toasts : List Toast
    , flipTarget : Maybe TodoItem
    , flipState : FlipState
    }


type FlipState
    = Inert
    | SaveState
    | Run


type alias Toast =
    { title : String
    , message : String
    }


init : Maybe String -> ( TopModel, Cmd Msg )
init user =
    case user of
        Just name ->
            ( TopLoading (LoadingModel name Nothing Nothing)
            , Cmd.batch
                [ getTasksAndCompletions
                , getCurrentTime
                ]
            )

        Nothing ->
            ( Login (LoginModel "" Nothing)
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
    , flipTarget = Nothing
    , flipState = Inert
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
    | TodoCheckedStartAnimation TodoItem Bool
    | TodoCheckAnimationDone
    | TodoCheckedFlipStateSaved
    | TodoChecked TodoItem
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
        ( GotTasksAndCompletions (Success tasksAndCompletions), TopLoading model ) ->
            case model.maybeNow of
                Just now ->
                    ( TopSuccess (initialModel model.user now tasksAndCompletions)
                    , Cmd.none
                    )

                Nothing ->
                    ( TopLoading { model | maybeTasksAndCompletions = Just tasksAndCompletions }
                    , Cmd.none
                    )

        ( GotTasksAndCompletions (Success ( tasks, completions )), TopSuccess model ) ->
            ( TopSuccess { model | tasks = tasks, completions = completions }
            , Cmd.none
            )

        ( GotTasksAndCompletions (Failure error), TopLoading _ ) ->
            ( TopFailure (FailureModel error)
            , Cmd.none
            )

        ( GotTasksAndCompletions (Failure error), TopSuccess model ) ->
            let
                toast =
                    toastFromHttpError error
            in
            ( TopSuccess
                { model
                    | toasts = model.toasts ++ [ { toast | message = "Nous n'avons pas pu rafraîchr les données" } ]
                }
            , Cmd.none
            )

        ( GotCurrentTime now, TopLoading model ) ->
            case model.maybeTasksAndCompletions of
                Just tasksAndCompletions ->
                    ( TopSuccess (initialModel model.user now tasksAndCompletions)
                    , Cmd.none
                    )

                Nothing ->
                    ( TopLoading { model | maybeNow = Just now }
                    , Cmd.none
                    )

        ( GotCurrentTime now, Login model ) ->
            ( Login { model | maybeNow = Just now }
            , Cmd.none
            )

        ( GotCurrentTime now, TopSuccess model ) ->
            ( TopSuccess { model | now = now }
            , Cmd.none
            )

        ( UsernameInput str, Login model ) ->
            ( Login { model | usernameInputValue = str }
            , Cmd.none
            )

        ( SaveUsername, Login model ) ->
            ( TopLoading (LoadingModel model.usernameInputValue model.maybeNow Nothing)
            , Cmd.batch
                [ setStorage model.usernameInputValue
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

        TodoCheckedStartAnimation todoItem _ ->
            ( { model | flipTarget = Just todoItem, flipState = SaveState }
            , Cmd.none
            )

        TodoCheckedFlipStateSaved ->
            case model.flipTarget of
                Just todoItem ->
                    updateSuccess (TodoChecked todoItem) { model | flipState = Run }

                Nothing ->
                    ( model, Cmd.none )

        TodoCheckAnimationDone ->
            ( { model | flipTarget = Nothing, flipState = Inert }
            , Cmd.none
            )

        TodoChecked todoItem ->
            case todoItem.status of
                NotDone ->
                    ( model
                    , Cmd.batch [ Task.perform (MarkTodoAsDone todoItem) Time.now ]
                    )

                Done _ _ completion ->
                    ( { model
                        | completions = List.filter (\c -> c /= completion) model.completions
                        , flipTarget = Just { todoItem | status = NotDone }
                      }
                    , deleteCompletion completion
                    )

        MarkTodoAsDone todo now ->
            let
                completion =
                    Completion "tmp-id" model.user todo.task.id todo.task.name now
            in
            ( { model
                | completions = completion :: model.completions
                , flipTarget = Just { todo | status = Done model.user (timeAgo now model.now) completion }
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
        Login _ ->
            main_ [] [ viewLoginForm ]

        TopLoading _ ->
            main_ [] [ viewLoading ]

        TopFailure { error } ->
            main_ [] [ viewLoadingFailed error ]

        TopSuccess model ->
            div
                []
                [ main_ [] (viewSuccess model)
                , viewToasts model.toasts
                ]


viewSuccess : Model -> List (Html Msg)
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

        flipState =
            case model.flipState of
                Inert ->
                    "inert"

                SaveState ->
                    "saveState"

                Run ->
                    "run"
    in
    [ div
        [ class "todolists-container" ]
        [ viewTodoListNotDone model.flipTarget 0 (ListExtra.uniqueWithCount notDone)
        , viewTodoListDone model.flipTarget 1 (sortByCompletedAtDesc done)
        ]
    , viewHistory model.now (model.completions |> List.sortBy (.completedAt >> posixToMillis) |> List.reverse)
    , Html.node "gsap-flip"
        [ property "status" (Encode.string flipState)
        , Html.Events.on "flip-state-saved" (Json.Decode.succeed TodoCheckedFlipStateSaved)
        , Html.Events.on "flip-done" (Json.Decode.succeed TodoCheckAnimationDone)
        ]
        []
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
    div
        [ class "loading-error" ]
        [ case err of
            BadBody message ->
                text ("Erreur de chargement des tâches: " ++ message)

            _ ->
                text "Erreur de chargement des tâches"
        ]


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


viewTodoListDone : Maybe TodoItem -> Int -> List TodoItem -> Html Msg
viewTodoListDone flipTarget i todoList =
    todoList
        |> List.foldl
            (\todoItem ( currentID, result ) ->
                if Just todoItem == flipTarget then
                    ( currentID, result ++ [ ( "done-t", todoItem ) ] )

                else
                    ( currentID + 1, result ++ [ ( "done-" ++ String.fromInt currentID, todoItem ) ] )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.indexedMap (\j todoItem -> viewTodoDone (String.fromInt i ++ String.fromInt j) todoItem)
        |> ul [ attribute "role" "list" ]


viewTodoListNotDone : Maybe TodoItem -> Int -> List ( TodoItem, Int ) -> Html Msg
viewTodoListNotDone flipTarget i todoList =
    todoList
        |> List.foldl
            (\( todoItem, count ) ( currentID, result ) ->
                if Just todoItem == flipTarget then
                    ( currentID + min 2 count - 1, result ++ [ ( "not-done-t", ( todoItem, count, True ) ) ] )

                else
                    ( currentID + 1, result ++ [ ( "not-done-" ++ String.fromInt currentID, ( todoItem, count, False ) ) ] )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.indexedMap (\j todoItem -> viewTodoNotDone (String.fromInt i ++ String.fromInt j) todoItem)
        |> ul [ attribute "role" "list" ]


viewTodoDone : String -> ( String, TodoItem ) -> Html Msg
viewTodoDone index ( flipID, todoItem ) =
    let
        id_ =
            todoItem.task.name ++ index
    in
    li
        [ class "todo", attribute "data-flip-id" flipID ]
        [ div
            []
            [ input [ type_ "checkbox", checked True, id id_, onCheck (TodoCheckedStartAnimation todoItem) ] [] ]
        , div
            []
            [ label [ for id_ ] [ text todoItem.task.name ]
            , case todoItem.status of
                Done user timeAgo_ _ ->
                    div
                        [ class "completion-tags" ]
                        [ div [ class "tag user" ] [ text user ]
                        , div [ class "tag time-ago" ] [ text (viewTimeAgo timeAgo_ False) ]
                        ]

                -- impossible, probably can be refactored out
                NotDone ->
                    div
                        [ class ("tag " ++ frequencyToClass todoItem.task.frequency) ]
                        [ text (frequencyToString todoItem.task.frequency 1) ]
            ]
        ]


viewTodoNotDone : String -> ( String, ( TodoItem, Int, Bool ) ) -> Html Msg
viewTodoNotDone index ( flipID, ( todoItem, count, isAnimating ) ) =
    let
        id_ =
            todoItem.task.name ++ index

        animationAttrs =
            if isAnimating then
                [ attribute "data-animate" "true" ]

            else
                []
    in
    li
        ([ class "todo", attribute "data-flip-id" flipID ] ++ animationAttrs)
        [ div
            []
            [ input [ type_ "checkbox", checked isAnimating, id id_, onCheck (TodoCheckedStartAnimation todoItem) ] [] ]
        , div
            []
            [ label [ for id_ ] [ text todoItem.task.name ]
            , case todoItem.status of
                -- impossible, probably can be refactored out
                Done user timeAgo_ _ ->
                    div
                        [ class "completion-tags" ]
                        [ div [ class "tag user" ] [ text user ]
                        , div [ class "tag time-ago" ] [ text (viewTimeAgo timeAgo_ False) ]
                        ]

                NotDone ->
                    div
                        [ class ("tag " ++ frequencyToClass todoItem.task.frequency) ]
                        [ text (frequencyToString todoItem.task.frequency count) ]
            ]
        ]


viewTimeAgo : TimeAgo -> Bool -> String
viewTimeAgo timeAgo_ short =
    case timeAgo_ of
        DaysAgo 0 ->
            "aujourd'hui"

        DaysAgo 1 ->
            "hier"

        DaysAgo days ->
            if short then
                String.fromInt days ++ " jours"

            else
                "il y a " ++ String.fromInt days ++ " jours"

        WeeksAgo 1 ->
            if short then
                "1 semaine"

            else
                "la semaine dernière"

        WeeksAgo weeks ->
            if short then
                String.fromInt weeks ++ " semaines"

            else
                "il y a " ++ String.fromInt weeks ++ " semaines"

        LongAgo ->
            if short then
                "+ d'un mois"

            else
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


viewHistory : ( Posix, Zone ) -> List Completion -> Html msg
viewHistory now completions =
    div
        [ class "history-container", attribute "data-flip-id" "history-container" ]
        [ div
            [ class "history" ]
            [ h2 [] [ text "Historique" ]
            , table []
                [ tbody []
                    (List.map (viewCompletion now) completions)
                ]
            ]
        ]


viewCompletion : ( Posix, Zone ) -> Completion -> Html msg
viewCompletion now completion =
    tr
        [ class "completion" ]
        [ td [] [ text completion.taskName ]
        , td [] [ span [ class "tag time-ago" ] [ text (viewTimeAgo (TodoList.timeAgo completion.completedAt now) True) ] ]
        , td [] [ span [ class "tag user" ] [ text completion.user ] ]
        ]



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
    map5 Completion
        (field "id" string)
        (at [ "fields", "user" ] string)
        (at [ "fields", "task" ] singleElementArrayDecoder)
        (at [ "fields", "task_name" ] singleElementArrayDecoder)
        (at [ "fields", "completed_at" ] Iso8601.decoder)


singleElementArrayDecoder : Decoder TaskID
singleElementArrayDecoder =
    list string
        |> andThen
            (\elements ->
                case elements of
                    element :: [] ->
                        succeed element

                    _ ->
                        fail "Expected exactly one element"
            )


todoItemEncoder : TodoItem -> Encode.Value
todoItemEncoder todoItem =
    Encode.object
        [ ( "task", taskDefinitionEncoder todoItem.task )
        , ( "status", todoStatusEncoder todoItem.status )
        ]


taskDefinitionEncoder : TaskDefinition -> Encode.Value
taskDefinitionEncoder taskDefinition =
    Encode.object
        [ ( "id", Encode.string taskDefinition.id )
        , ( "name", Encode.string taskDefinition.name )
        , ( "frequency", frequencyEncoder taskDefinition.frequency )
        ]


frequencyEncoder : Frequency -> Encode.Value
frequencyEncoder frequency =
    case frequency of
        TwiceADay ->
            Encode.string "twice a day"

        FourTimesAWeek ->
            Encode.string "four times a week"

        TwiceAWeek ->
            Encode.string "twice a week"

        EveryWeek ->
            Encode.string "every week"

        EveryOtherWeek ->
            Encode.string "every other week"

        EveryMonth ->
            Encode.string "every month"


todoStatusEncoder : TodoStatus -> Encode.Value
todoStatusEncoder todoStatus =
    case todoStatus of
        NotDone ->
            Encode.object
                [ ( "type", Encode.string "not done" ) ]

        Done user timeAgo completion ->
            Encode.object
                [ ( "type", Encode.string "done" )
                , ( "user", Encode.string user )
                , ( "completion", completionEncoder completion )
                , ( "timeAgo", timeAgoEncoder timeAgo )
                ]


timeAgoEncoder : TimeAgo -> Encode.Value
timeAgoEncoder timeAgo =
    case timeAgo of
        DaysAgo days ->
            Encode.object
                [ ( "type", Encode.string "days ago" )
                , ( "days", Encode.string (String.fromInt days) )
                ]

        WeeksAgo weeks ->
            Encode.object
                [ ( "type", Encode.string "weeks ago" )
                , ( "weeks", Encode.string (String.fromInt weeks) )
                ]

        LongAgo ->
            Encode.object
                [ ( "type", Encode.string "long ago" ) ]



-- Ports


port setStorage : String -> Cmd msg
