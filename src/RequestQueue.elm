module RequestQueue exposing (Msg, Queue, empty, enqueue, send, sendAfter, update)

import Platform.Cmd as Cmd
import Process
import Task


type Queue parentMsg
    = Queue
        { delayMs : Float
        , pending : List parentMsg
        , dispatcherActive : Bool
        }


type Msg
    = Dispatch
    | CooldownComplete


empty : Float -> Queue parentMsg
empty delayMs =
    Queue
        { delayMs = delayMs
        , pending = []
        , dispatcherActive = False
        }


enqueue : (Msg -> parentMsg) -> List parentMsg -> Queue parentMsg -> ( Queue parentMsg, Cmd parentMsg )
enqueue toParent messages (Queue queue) =
    if List.isEmpty messages then
        ( Queue queue, Cmd.none )

    else
        let
            updatedQueue =
                Queue
                    { queue
                        | pending = queue.pending ++ messages
                        , dispatcherActive = True
                    }
        in
        ( updatedQueue
        , if queue.dispatcherActive then
            Cmd.none

          else
            send <| toParent Dispatch
        )


update : (Msg -> parentMsg) -> Msg -> Queue parentMsg -> ( Queue parentMsg, Cmd parentMsg )
update toParent msg (Queue queue) =
    case msg of
        Dispatch ->
            case queue.pending of
                message :: remainingMessages ->
                    ( Queue { queue | pending = remainingMessages }
                    , Cmd.batch
                        [ send message
                        , sendAfter queue.delayMs <| toParent CooldownComplete
                        ]
                    )

                [] ->
                    ( Queue { queue | dispatcherActive = False }, Cmd.none )

        CooldownComplete ->
            if List.isEmpty queue.pending then
                ( Queue { queue | dispatcherActive = False }, Cmd.none )

            else
                ( Queue queue, send <| toParent Dispatch )


send : msg -> Cmd msg
send message =
    Task.perform identity <| Task.succeed message


sendAfter : Float -> msg -> Cmd msg
sendAfter delay message =
    Process.sleep delay
        |> Task.perform (\_ -> message)
