port module Main exposing (..)

import Array
import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, button, div, fieldset, form, h2, input, label, legend, li, main_, output, span, table, tbody, td, text, tr, ul)
import Html.Attributes exposing (attribute, checked, class, classList, disabled, for, href, id, name, property, required, type_)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Icons
import Iso8601
import Json.Decode exposing (Decoder, andThen, at, fail, field, list, map2, map3, map5, string, succeed)
import Json.Encode as Encode
import ListExtra
import Random
import RemoteData exposing (RemoteData(..))
import Task
import Time exposing (Posix, Weekday(..), Zone, posixToMillis)
import TodoList exposing (..)
import TodoListData exposing (..)
import Url
import Url.Parser



-- Main


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- Model


type Page
    = LoginPage LoginPageModel
    | TodoListPage TodoListPageModel
    | StatisticsPage
    | NewTaskPage NewTaskPageModel
    | NotFoundPage


pageFromUrl : Url.Url -> Page
pageFromUrl url =
    url
        |> Url.Parser.parse
            (Url.Parser.oneOf
                [ Url.Parser.map (TodoListPage initTodoListPageModel) Url.Parser.top
                , Url.Parser.map (LoginPage initLoginPageModel) (Url.Parser.s "login")
                , Url.Parser.map StatisticsPage (Url.Parser.s "statistics")
                , Url.Parser.map (NewTaskPage initNewTaskPageModel) (Url.Parser.s "new-task")
                ]
            )
        |> Maybe.withDefault NotFoundPage


type alias Model =
    { key : Nav.Key
    , maybeUser : Maybe User
    , toasts : List Toast
    , todoListData : TodoListData
    , page : Page
    }


type alias LoginPageModel =
    { usernameInputValue : String
    }


initLoginPageModel : LoginPageModel
initLoginPageModel =
    { usernameInputValue = ""
    }


type alias TodoListPageModel =
    { flipTarget : Maybe TodoItem
    , flipState : FlipState
    , congratsMessage : Maybe String
    }


initTodoListPageModel : TodoListPageModel
initTodoListPageModel =
    { flipTarget = Nothing
    , flipState = Inert
    , congratsMessage = Nothing
    }


type FlipState
    = Inert
    | SaveState
    | Run


type alias Toast =
    { title : String
    , message : String
    }


type alias NewTaskPageModel =
    { name : String
    , repeats : Repeats
    , times : Int
    , saving : Bool
    }


type Repeats
    = EveryDay
    | EveryWeek
    | EveryMonth


initNewTaskPageModel : NewTaskPageModel
initNewTaskPageModel =
    { name = ""
    , repeats = EveryDay
    , times = 1
    , saving = False
    }


congratsMessages : List String
congratsMessages =
    [ "Bravo !"
    , "Bien ouej !"
    , "Nice !"
    ]


congratsEmojis : List String
congratsEmojis =
    [ "👏"
    , "🎉"
    , "💪"
    , "❤️"
    ]


randomCongratsMessage : Random.Generator String
randomCongratsMessage =
    Random.map2
        (\emoji message ->
            (congratsEmojis |> Array.fromList |> Array.get emoji |> Maybe.withDefault "👏")
                ++ " "
                ++ (congratsMessages |> Array.fromList |> Array.get message |> Maybe.withDefault "Bravo !")
        )
        (Random.int 0 (List.length congratsEmojis - 1))
        (Random.int 0 (List.length congratsMessages - 1))


init : Maybe User -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init user url key =
    let
        ( model, cmd ) =
            ( { key = key
              , maybeUser = user
              , toasts = []
              , todoListData = DataLoading Nothing Nothing
              , page = pageFromUrl url
              }
            , Cmd.batch [ getTasksAndCompletions, getCurrentTime ]
            )
    in
    case user of
        Just _ ->
            ( model, cmd )

        Nothing ->
            ( model, Cmd.batch [ cmd, Nav.replaceUrl key "/login" ] )


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GotCurrentTime (Time.now |> Task.andThen (\now -> Task.map (Tuple.pair now) Time.here))


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
    = UrlChanged Url.Url
    | LinkClicked Browser.UrlRequest
    | GotTasksAndCompletions (RemoteData ( List TaskDefinition, List Completion ))
    | GotCurrentTime ( Posix, Zone )
    | RemoveToast Int
    | TodoListMsg TodoListMsg
    | LoginMsg LoginMsg
    | NewTaskMsg NewTaskMsg


type TodoListMsg
    = NewCongratsMessage String
    | TodoCheckedStartAnimation TodoItem Bool
    | TodoCheckAnimationDone
    | TodoCheckedFlipStateSaved
    | MarkTodoAsDone TodoItem Posix
    | CompletionSaved Completion (RemoteData Completion)
    | CompletionDeleted Completion (RemoteData String)
    | RemoveCongratsMessage
    | Refresh Posix


type LoginMsg
    = UsernameInput String
    | SaveUsername


type NewTaskMsg
    = SetRepeats Repeats
    | SetTaskName String
    | Increment
    | Decrement
    | CreateNewTask
    | TaskSaved (RemoteData TaskDefinition)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            ( { model | page = pageFromUrl url }
            , Cmd.none
            )

        ( GotTasksAndCompletions (Success tasksAndCompletions), _ ) ->
            ( { model | todoListData = TodoListData.setTasksAndCompletions tasksAndCompletions model.todoListData }
            , case model.todoListData of
                DataFailure _ ->
                    getCurrentTime

                _ ->
                    Cmd.none
            )

        ( GotTasksAndCompletions (Failure error), _ ) ->
            let
                newModel =
                    { model | todoListData = TodoListData.setError error model.todoListData }

                toast =
                    toastFromHttpError error
            in
            case model.todoListData of
                DataSuccess _ _ _ ->
                    ( { newModel
                        | toasts =
                            model.toasts
                                ++ [ { toast | message = "Nous n'avons pas pu rafraîchr les données" } ]
                      }
                    , Cmd.none
                    )

                _ ->
                    ( newModel
                    , Cmd.none
                    )

        ( GotCurrentTime now, _ ) ->
            ( { model | todoListData = TodoListData.setCurrentTime now model.todoListData }
            , Cmd.none
            )

        ( RemoveToast index, _ ) ->
            ( { model
                | toasts = ListExtra.dropAtIndex index model.toasts
              }
            , Cmd.none
            )

        ( TodoListMsg subMsg, TodoListPage pageModel ) ->
            let
                ( newModel, newPageModel, cmd ) =
                    updateTodoList subMsg model pageModel
            in
            ( { newModel | page = TodoListPage newPageModel }
            , cmd
            )

        ( LoginMsg subMsg, LoginPage pageModel ) ->
            let
                ( newModel, newPageModel, cmd ) =
                    updateLogin subMsg model pageModel
            in
            ( { newModel | page = LoginPage newPageModel }
            , cmd
            )

        ( NewTaskMsg subMsg, NewTaskPage pageModel ) ->
            let
                ( newModel, newPageModel, cmd ) =
                    updateNewTask subMsg model pageModel
            in
            ( { newModel | page = NewTaskPage newPageModel }
            , cmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )


updateTodoList : TodoListMsg -> Model -> TodoListPageModel -> ( Model, TodoListPageModel, Cmd Msg )
updateTodoList msg model pageModel =
    case msg of
        Refresh now ->
            ( { model | todoListData = TodoListData.refreshCurrentTime now model.todoListData }
            , pageModel
            , getTasksAndCompletions
            )

        RemoveCongratsMessage ->
            ( model
            , { pageModel | congratsMessage = Nothing }
            , Cmd.none
            )

        CompletionSaved completion (Failure err) ->
            ( { model
                | todoListData = TodoListData.removeCompletion completion model.todoListData
                , toasts =
                    model.toasts ++ [ toastFromHttpError err ]
              }
            , pageModel
            , Cmd.none
            )

        CompletionSaved oldCompletion (Success savedCompletion) ->
            ( { model | todoListData = TodoListData.updateCompletion oldCompletion savedCompletion model.todoListData }
            , pageModel
            , Cmd.none
            )

        CompletionSaved _ Loading ->
            ( model, pageModel, Cmd.none )

        CompletionDeleted completion (Failure err) ->
            ( { model
                | todoListData = TodoListData.prependCompletion completion model.todoListData
                , toasts = model.toasts ++ [ toastFromHttpError err ]
              }
            , pageModel
            , Cmd.none
            )

        CompletionDeleted _ _ ->
            ( model, pageModel, Cmd.none )

        TodoCheckedStartAnimation todoItem checked ->
            ( model
            , { pageModel | flipTarget = Just todoItem, flipState = SaveState }
            , if checked then
                Random.generate (TodoListMsg << NewCongratsMessage) randomCongratsMessage

              else
                Cmd.none
            )

        NewCongratsMessage message ->
            ( model
            , { pageModel | congratsMessage = Just message }
            , Cmd.none
            )

        TodoCheckedFlipStateSaved ->
            case pageModel.flipTarget of
                Just todoItem ->
                    case todoItem.status of
                        NotDone ->
                            ( model
                            , pageModel
                            , Task.perform (TodoListMsg << MarkTodoAsDone todoItem) Time.now
                            )

                        Done _ _ completion ->
                            ( { model | todoListData = TodoListData.removeCompletion completion model.todoListData }
                            , { pageModel
                                | flipTarget = Just { todoItem | status = NotDone }
                                , flipState = Run
                              }
                            , deleteCompletion completion
                            )

                Nothing ->
                    ( model, pageModel, Cmd.none )

        TodoCheckAnimationDone ->
            ( model
            , { pageModel | flipTarget = Nothing, flipState = Inert }
            , Cmd.none
            )

        MarkTodoAsDone todo now ->
            case ( model.maybeUser, model.todoListData ) of
                ( Just user, DataSuccess nowWithTimeZone _ _ ) ->
                    let
                        completion =
                            Completion "tmp-id" user todo.task.id todo.task.name now

                        newTodo =
                            { todo | status = Done user (timeAgo now nowWithTimeZone) completion }
                    in
                    ( { model | todoListData = TodoListData.prependCompletion completion model.todoListData }
                    , { pageModel
                        | flipTarget = Just newTodo
                        , flipState = Run
                      }
                    , postCompletion completion
                    )

                ( _, _ ) ->
                    ( model, pageModel, Cmd.none )


updateLogin : LoginMsg -> Model -> LoginPageModel -> ( Model, LoginPageModel, Cmd Msg )
updateLogin msg model pageModel =
    case msg of
        UsernameInput str ->
            ( model
            , { pageModel | usernameInputValue = str }
            , Cmd.none
            )

        SaveUsername ->
            ( { model | maybeUser = Just pageModel.usernameInputValue }
            , { pageModel | usernameInputValue = "" }
            , Cmd.batch
                [ setStorage pageModel.usernameInputValue
                , Nav.pushUrl model.key "/"
                ]
            )


updateNewTask : NewTaskMsg -> Model -> NewTaskPageModel -> ( Model, NewTaskPageModel, Cmd Msg )
updateNewTask msg model pageModel =
    case msg of
        SetRepeats repeats ->
            ( model
            , { pageModel | repeats = repeats }
            , Cmd.none
            )

        SetTaskName name ->
            ( model
            , { pageModel | name = name }
            , Cmd.none
            )

        Increment ->
            ( model
            , { pageModel | times = pageModel.times + 1 }
            , Cmd.none
            )

        Decrement ->
            ( model
            , { pageModel | times = pageModel.times - 1 }
            , Cmd.none
            )

        CreateNewTask ->
            let
                task =
                    { id = "tmp-id"
                    , name = pageModel.name
                    , recurrence =
                        case pageModel.repeats of
                            EveryDay ->
                                Daily pageModel.times

                            EveryWeek ->
                                Weekly pageModel.times

                            EveryMonth ->
                                Monthly pageModel.times
                    }
            in
            ( model
            , { pageModel | saving = True }
            , postTask task
            )

        TaskSaved (Success task) ->
            ( { model | todoListData = TodoListData.addTask task model.todoListData }
            , { pageModel | saving = False }
            , Nav.pushUrl model.key "/"
            )

        TaskSaved (Failure error) ->
            ( { model | toasts = model.toasts ++ [ toastFromHttpError error ] }
            , { pageModel | saving = False }
            , Cmd.none
            )

        TaskSaved Loading ->
            ( model, pageModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (1000 * 60) (TodoListMsg << Refresh)



-- View


view : Model -> Browser.Document Msg
view model =
    { title = "Ménage"
    , body =
        [ Html.node "menage-notifications" [] []
        , viewToasts model.toasts
        , div
            [ class "main-container" ]
            [ case model.page of
                LoginPage _ ->
                    main_ [] [ viewLoginForm ]

                NewTaskPage pageModel ->
                    main_ [] [ viewNewTaskForm pageModel ]

                TodoListPage todoListPageModel ->
                    case model.todoListData of
                        DataLoading _ _ ->
                            main_ [] [ viewLoading ]

                        DataFailure error ->
                            main_ [] [ viewLoadingFailed error ]

                        DataSuccess now tasks completions ->
                            div
                                []
                                [ main_ [] (viewSuccess now tasks completions todoListPageModel)
                                , viewCongratsMessage todoListPageModel.congratsMessage
                                ]

                StatisticsPage ->
                    case model.todoListData of
                        DataLoading _ _ ->
                            main_ [] [ viewLoading ]

                        DataFailure error ->
                            main_ [] [ viewLoadingFailed error ]

                        DataSuccess _ tasks completions ->
                            main_ [] [ viewStatistics tasks completions ]

                NotFoundPage ->
                    main_ [] [ text "404 : Cette page n'existe pas" ]
            ]
        ]
    }


viewSuccess : ( Posix, Zone ) -> List TaskDefinition -> List Completion -> TodoListPageModel -> List (Html Msg)
viewSuccess now tasks completions model =
    let
        ( done, notDone ) =
            completions
                |> TodoList.fromTasksAndCompletions now tasks
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
        [ div
            []
            [ viewTodoListNotDone model.flipTarget 0 (ListExtra.uniqueWithCount notDone)
            , a
                [ class "new-task-button"
                , href "/new-task"
                , attribute "data-flip-id" "new-task-button"
                ]
                [ text "nouvelle tâche" ]
            ]
        , viewTodoListDone model.flipTarget 1 (sortByCompletedAtDesc done)
        ]
    , viewHistory now (completions |> List.sortBy (.completedAt >> posixToMillis) |> List.reverse)
    , Html.node "gsap-flip"
        [ property "status" (Encode.string flipState)
        , Html.Events.on "flip-state-saved" (Json.Decode.succeed (TodoListMsg TodoCheckedFlipStateSaved))
        , Html.Events.on "flip-done" (Json.Decode.succeed (TodoListMsg TodoCheckAnimationDone))
        ]
        []
    ]


viewLoginForm : Html Msg
viewLoginForm =
    form
        [ onSubmit (LoginMsg SaveUsername) ]
        [ label [ for "name" ] [ text "Votre nom" ]
        , input [ id "name", name "name", required True, onInput (LoginMsg << UsernameInput) ] []
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


viewNewTaskForm : NewTaskPageModel -> Html Msg
viewNewTaskForm model =
    form
        [ class "new-task-form"
        , onSubmit (NewTaskMsg CreateNewTask)
        ]
        [ div
            []
            [ label
                [ for "task-name" ]
                [ text "Nom de la tâche" ]
            , input
                [ id "task-name"
                , name "task-name"
                , required True
                , onInput (NewTaskMsg << SetTaskName)
                ]
                []
            ]
        , fieldset
            []
            (legend [] [ text "Répétitions" ]
                :: ([ ( "daily", "chaque jour", EveryDay )
                    , ( "weekly", "chaque semaine", EveryWeek )
                    , ( "monthly", "chaque mois", EveryMonth )
                    ]
                        |> List.map (viewRepeatRadio model.repeats)
                   )
            )
        , viewFrequencyCounter model.repeats model.times
        , button
            [ type_ "submit"
            , classList [ ( "saving", model.saving ) ]
            ]
            [ Icons.arrowPath [ attribute "aria-hidden" "true" ]
            , div [] [ text "Ajouter" ]
            ]
        ]


viewRepeatRadio : Repeats -> ( String, String, Repeats ) -> Html Msg
viewRepeatRadio selected ( id_, text_, repeats ) =
    div [ class "radio-field" ]
        [ input
            [ type_ "radio"
            , id id_
            , name "repeats"
            , checked (repeats == selected)
            , onInput (\_ -> (NewTaskMsg << SetRepeats) repeats)
            ]
            []
        , label [ for id_ ] [ text text_ ]
        ]


viewFrequencyCounter : Repeats -> Int -> Html Msg
viewFrequencyCounter repeats times =
    div
        [ class "frequency-counter" ]
        [ div [] [ text "à faire" ]
        , div
            [ class "counter-input" ]
            [ button
                [ onClick (NewTaskMsg Decrement)
                , type_ "button"
                , disabled (times <= 1)
                ]
                [ text "-" ]
            , div [] [ text (String.fromInt times ++ " fois") ]
            , button
                [ onClick (NewTaskMsg Increment), type_ "button" ]
                [ text "+" ]
            ]
        , div [] [ text (repeatsToString repeats) ]
        ]


repeatsToString : Repeats -> String
repeatsToString repeats =
    case repeats of
        EveryDay ->
            "par jour"

        EveryWeek ->
            "par semaine"

        EveryMonth ->
            "par mois"


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
            [ input
                [ type_ "checkbox"
                , checked True
                , id id_
                , onCheck (TodoListMsg << TodoCheckedStartAnimation todoItem)
                ]
                []
            ]
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
                        [ class ("tag " ++ recurrenceToClass todoItem.task.recurrence) ]
                        [ text (recurrenceToString todoItem.task.recurrence 1) ]
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
            [ input
                [ type_ "checkbox"
                , checked isAnimating
                , id id_
                , onCheck (TodoListMsg << TodoCheckedStartAnimation todoItem)
                ]
                []
            ]
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
                        [ class ("tag " ++ recurrenceToClass todoItem.task.recurrence) ]
                        [ text (recurrenceToString todoItem.task.recurrence count) ]
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


recurrenceToString : Recurrence -> Int -> String
recurrenceToString recurrence count =
    case recurrence of
        Daily _ ->
            "aujourd'hui"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        Weekly _ ->
            "cette semaine"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )

        Monthly _ ->
            "ce mois-ci"
                ++ (if count > 1 then
                        " (x" ++ String.fromInt count ++ ")"

                    else
                        ""
                   )


recurrenceToClass : Recurrence -> String
recurrenceToClass recurrence =
    case recurrence of
        Daily count ->
            "daily-" ++ String.fromInt count

        Weekly count ->
            "weekly-" ++ String.fromInt count

        Monthly count ->
            "monthly-" ++ String.fromInt count


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


viewStatistics : List TaskDefinition -> List Completion -> Html msg
viewStatistics tasks completions =
    div
        [ class "statistics" ]
        (viewTaskStatistics completions Nothing
            :: (tasks
                    |> List.map Just
                    |> List.map (viewTaskStatistics completions)
               )
        )


viewTaskStatistics : List Completion -> Maybe TaskDefinition -> Html msg
viewTaskStatistics completions maybeTask =
    let
        taskCompletions =
            maybeTask
                |> Maybe.map (\task -> List.filter (\{ taskId } -> taskId == task.id) completions)
                |> Maybe.withDefault completions

        users =
            completions
                |> List.map .user
                |> ListExtra.unique

        countCompletions user_ =
            taskCompletions
                |> List.filter (\{ user } -> user == user_)
                |> List.length

        counts =
            users
                |> List.map (\user -> ( user, countCompletions user ))
                |> List.sortBy Tuple.second
                |> List.reverse
    in
    div
        []
        [ h2 [] [ maybeTask |> Maybe.map .name |> Maybe.withDefault "Total" |> text ]
        , table
            []
            (List.map viewCount counts)
        ]


viewCount : ( User, Int ) -> Html msg
viewCount ( user, count ) =
    tr
        []
        [ td [] [ text user ]
        , td [] [ text (String.fromInt count) ]
        ]


viewCongratsMessage : Maybe String -> Html Msg
viewCongratsMessage maybeMessage =
    case maybeMessage of
        Just message ->
            output
                [ class "congrats-message-container"
                , Html.Events.on "animationend"
                    (field "animationName" string
                        |> Json.Decode.andThen
                            (\name ->
                                if name == "fade-out" then
                                    Json.Decode.succeed (TodoListMsg RemoveCongratsMessage)

                                else
                                    Json.Decode.fail "silent failure, not the right animation"
                            )
                    )
                ]
                [ div
                    [ class "congrats-message" ]
                    [ text message ]
                ]

        Nothing ->
            text ""



-- Http


getTasksAndCompletions : Cmd Msg
getTasksAndCompletions =
    apiGet "/get-tasks-and-completions" GotTasksAndCompletions tasksAndCompletionsDecoder


postCompletion : Completion -> Cmd Msg
postCompletion completion =
    apiPost
        "/create-completion"
        (Http.jsonBody (completionEncoder completion))
        (TodoListMsg << CompletionSaved completion)
        completionDecoder


deleteCompletion : Completion -> Cmd Msg
deleteCompletion completion =
    apiDelete
        ("/delete-completion?id=" ++ completion.id)
        (TodoListMsg << CompletionDeleted completion)
        (Json.Decode.succeed "Ok")


postTask : TaskDefinition -> Cmd Msg
postTask task =
    apiPost
        "/create-task"
        (Http.jsonBody (taskDefinitionEncoder task))
        (NewTaskMsg << TaskSaved)
        taskDecoder


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
        (at [ "fields", "frequency" ] recurrenceDecoder)


recurrenceDecoder : Decoder Recurrence
recurrenceDecoder =
    string
        |> andThen
            (\str ->
                case str of
                    "twice a day" ->
                        succeed (Daily 2)

                    "every other week" ->
                        succeed (Monthly 2)

                    "every week" ->
                        succeed (Weekly 1)

                    "four times a week" ->
                        succeed (Weekly 4)

                    "twice a week" ->
                        succeed (Weekly 2)

                    "every month" ->
                        succeed (Monthly 1)

                    _ ->
                        fail "Could not decode recurrence"
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
        , ( "frequency", recurrenceEncoder taskDefinition.recurrence )
        ]


recurrenceEncoder : Recurrence -> Encode.Value
recurrenceEncoder recurrence =
    case recurrence of
        Daily 2 ->
            Encode.string "twice a day"

        Weekly 4 ->
            Encode.string "four times a week"

        Weekly 2 ->
            Encode.string "twice a week"

        Weekly 1 ->
            Encode.string "every week"

        Monthly 2 ->
            Encode.string "every other week"

        Monthly 1 ->
            Encode.string "every month"

        _ ->
            Encode.string "unknown recurrence"


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
