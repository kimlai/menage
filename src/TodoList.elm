module TodoList exposing (Completion, Frequency(..), TaskDefinition, TaskID, TimeAgo(..), TodoItem, TodoStatus(..), User, fromTasksAndCompletions)

import Time exposing (Posix, Weekday(..), Zone, millisToPosix, posixToMillis, toHour, toMillis, toMinute, toSecond)


type alias User =
    String


type alias TaskID =
    String


type alias TaskDefinition =
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
    , user : User
    , taskId : String
    , completedAt : Posix
    }


type alias Task =
    { task: TaskDefinition
    , start : Posix
    }

type alias TodoItem =
    { task : TaskDefinition
    , status : TodoStatus
    }


type TodoStatus
    = Done User TimeAgo Completion
    | NotDone


type TimeAgo
    = DaysAgo Int
    | WeeksAgo Int
    | LongAgo


fromTasksAndCompletions : ( Posix, Zone ) -> List TaskDefinition -> List Completion -> List TodoItem
fromTasksAndCompletions now tasks completions =
    tasks
        |> List.concatMap (tasksFromDefinition now)
        |> fromTaskInstancesAndCompletions now completions


{-| Using a recursive function to go through the list enables removing completions from the potential matches as we go.
-}
fromTaskInstancesAndCompletions : ( Posix, Zone ) -> List Completion -> List Task -> List TodoItem
fromTaskInstancesAndCompletions now completions tasks =
    case tasks of
        [] ->
            []

        task :: rest ->
            let
                status =
                    todoStatus now completions task

                completionsLeft =
                    case status of
                        Done _ _ completion ->
                            List.filter (\c -> c /= completion) completions

                        NotDone ->
                            completions
            in
            TodoItem task.task status :: fromTaskInstancesAndCompletions now completionsLeft rest


tasksFromDefinition : ( Posix, Zone ) -> TaskDefinition -> List Task
tasksFromDefinition now task =
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


todoStatus : ( Posix, Zone ) -> List Completion -> Task -> TodoStatus
todoStatus now completions task =
    completions
        |> List.filter (completionMatchesTask task)
        |> List.head
        |> Maybe.map
            (\completion ->
                Done completion.user (timeAgo completion.completedAt now) completion
            )
        |> Maybe.withDefault NotDone


completionMatchesTask : Task -> Completion -> Bool
completionMatchesTask task completion =
    completion.taskId == task.task.id && posixToMillis completion.completedAt > posixToMillis task.start


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
