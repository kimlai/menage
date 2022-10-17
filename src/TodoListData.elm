module TodoListData exposing (..)

import Http
import Time exposing (Posix, Zone)
import TodoList exposing (Completion, TaskDefinition)


{-| To create a TodoList we need three things:

  - the current time
  - task definitions (like "do the laundry once a week")
  - task completions (like "Joe did the dishes last Tuesday")
    Instead of using things like Maybe.map3 everywhere in the code,
    this module models the fact that 3 things are needed before
    we can use this state.

-}
type TodoListData
    = DataLoading (Maybe ( Posix, Zone )) (Maybe ( List TaskDefinition, List Completion ))
    | DataSuccess ( Posix, Zone ) (List TaskDefinition) (List Completion)
    | DataFailure Http.Error


loading : TodoListData
loading =
    DataLoading Nothing Nothing


setTasksAndCompletions : ( List TaskDefinition, List Completion ) -> TodoListData -> TodoListData
setTasksAndCompletions ( tasks, completions ) todoListData =
    case todoListData of
        DataLoading (Just now) _ ->
            DataSuccess now tasks completions

        DataLoading Nothing _ ->
            DataLoading Nothing (Just ( tasks, completions ))

        DataFailure _ ->
            DataLoading Nothing (Just ( tasks, completions ))

        DataSuccess now _ _ ->
            DataSuccess now tasks completions


setError : Http.Error -> TodoListData -> TodoListData
setError error todoListData =
    case todoListData of
        -- data had already been loaded, this is a refresh error it should not erase previous data
        DataSuccess _ _ _ ->
            todoListData

        _ ->
            DataFailure error


setCurrentTime : ( Posix, Zone ) -> TodoListData -> TodoListData
setCurrentTime now todoListData =
    case todoListData of
        DataSuccess _ tasks completions ->
            DataSuccess now tasks completions

        DataLoading _ (Just ( tasks, completions )) ->
            DataSuccess now tasks completions

        DataLoading _ Nothing ->
            DataLoading (Just now) Nothing

        DataFailure _ ->
            todoListData


refreshCurrentTime : Posix -> TodoListData -> TodoListData
refreshCurrentTime now todoListData =
    case todoListData of
        DataSuccess ( _, zone ) tasks completions ->
            DataSuccess ( now, zone ) tasks completions

        _ ->
            todoListData


removeCompletion : Completion -> TodoListData -> TodoListData
removeCompletion completionToRemove todoListData =
    case todoListData of
        DataSuccess now tasks completions ->
            DataSuccess now tasks (List.filter ((/=) completionToRemove) completions)

        _ ->
            todoListData


updateCompletion : Completion -> Completion -> TodoListData -> TodoListData
updateCompletion oldCompletion newCompletion todoListData =
    case todoListData of
        DataSuccess now tasks completions ->
            DataSuccess now
                tasks
                (List.map
                    (\completion ->
                        if completion == oldCompletion then
                            newCompletion

                        else
                            completion
                    )
                    completions
                )

        _ ->
            todoListData


prependCompletion : Completion -> TodoListData -> TodoListData
prependCompletion completion todoListData =
    case todoListData of
        DataSuccess now tasks completions ->
            DataSuccess now tasks (completion :: completions)

        _ ->
            todoListData


addTask : TaskDefinition -> TodoListData -> TodoListData
addTask task todoListData =
    case todoListData of
        DataSuccess now tasks completions ->
            DataSuccess now (task :: tasks) completions

        _ ->
            todoListData
