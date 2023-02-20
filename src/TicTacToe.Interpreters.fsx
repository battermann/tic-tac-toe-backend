module TicTacToe.Interpreters

#load "TicTacToe.Instructions.fsx"
open TicTacToe
open TicTacToe.Core
open TicTacToe.Instructions
open Dsls

#nowarn "40"
type Actor<'T> = MailboxProcessor<'T>

open FSharpPlus.Data

type Effect<'a> = ResultT<Async<Result<'a, string>>>

module Domain =
    open Dsls.Domain

    let interpret : Domain<Effect<'a>> -> Effect<'a> = function
    | Handle ((state, cmd), cont) -> Domain.handle state cmd |> ResultT.hoist >>= cont
    | Replay (events, cont)       -> Domain.replay events    |> ResultT.hoist >>= cont


module EventStore =
    open Dsls.EventStore

    type Stream = { mutable events: (Event * Version) list }

    type EventStoreMsg = 
        | AppendToStream of string * int * Event list * AsyncReplyChannel<Result<unit, string>>
        | TryGetStream of string * AsyncReplyChannel<Stream option>

    let eventStoreActor = 
        Actor.Start (fun inbox ->
            let rec loop (eventStore: Map<string, Stream>) = async {
                let! msg = inbox.Receive ()
                match msg with
                | AppendToStream (streamId, v, events, rc) ->
                    let eventsWithVersion = events |> List.mapi (fun i e -> (e, Version (v + i + 1)))
                    match eventStore.TryFind streamId with
                    | Some stream ->
                        if (stream.events |> List.last |> snd) <> Version v then
                            rc.Reply (Error "resource has been modified, cannot make changes")
                        else
                            ignore (stream.events <- stream.events @ eventsWithVersion)
                            rc.Reply (Ok ())
                        return! loop eventStore
                    | None ->
                        rc.Reply (Ok ())
                        return! loop (eventStore.Add (streamId, { events = eventsWithVersion }))
                | TryGetStream (streamId, rc) ->
                    rc.Reply (eventStore.TryFind streamId)
                    return! loop eventStore
            }
            loop Map.empty)

    let interpret = function
    | GetStream (GameId gId, cont) ->
        async {
            let! maybeStream = eventStoreActor.PostAndAsyncReply(fun rc -> TryGetStream (gId.ToString(), rc))
            return 
                match maybeStream with
                | Some stream ->
                    stream.events
                    |> List.map fst
                | None -> []
                |> Ok
        } |> ResultT >>= cont

    | Append ((GameId gId, Version v, newEvents), cont) ->
        async {
            let! result = eventStoreActor.PostAndAsyncReply (fun rc -> AppendToStream (string gId, v, newEvents, rc))
            return result
        } |> ResultT >>= cont


module EventBus =
    open Dsls.EventBus

    type Actor<'T> = MailboxProcessor<'T>

    type EventBusMsg = 
        | Subscribe of (GameId * Event -> unit)
        | PublishEvents of GameId * Event list

    let eventBusActor = Actor.Start (fun inbox ->
        let rec loop (handlers: List<GameId * Event -> unit>) = async {
            let! msg = inbox.Receive()
            match msg with
            | Subscribe handler -> return! loop (handler :: handlers)
            | PublishEvents (id, events) ->
                handlers |> List.iter (fun handle -> 
                    events |> List.iter (fun event ->
                        handle (id, event)))
                return! loop handlers
            }
        loop List.empty)

    let interpret : EventBus<Effect<'t>> -> Effect<'t> = function
    | Publish ((id, events), cont) ->
        do eventBusActor.Post (PublishEvents (id, events))
        () |> Ok |> ResultT.hoist >>= cont


module ReadModel =
    open Dsls.ReadModel

    let emptyGridRm = [
        [""; ""; ""]
        [""; ""; ""]
        [""; ""; ""]
        ]

    let updateGrid (rm: GameRm) pos mark =
        let update m (grid: GridRm) (v, h) =
            grid |> List.mapi (fun v' hLine ->
                if v' = v then hLine |> List.mapi (fun h' m' -> if h' = h then m else m')
                else hLine) 

        let mapToCoords = function
        | Top    , Left    -> 0, 0
        | Top    , HCenter -> 0, 1
        | Top    , Right   -> 0, 2
        | VCenter, Left    -> 1, 0
        | VCenter, HCenter -> 1, 1
        | VCenter, Right   -> 1, 2
        | Bottom , Left    -> 2, 0
        | Bottom , HCenter -> 2, 1
        | Bottom , Right   -> 2, 2

        mapToCoords pos |> update mark rm.grid

    type ReadModelMsg = 
        | AddGame of GameId * string
        | UpdateGame of GameId * Position * string * string
        | UpdateGameStatus of GameId * string
        | UpsertGames of GameId * GameListItemRm
        | TryFind of GameId * AsyncReplyChannel<GameRm option>
        | GameList of AsyncReplyChannel<GameListItemRm list>

    let readModelsActor = Actor.Start(fun inbox ->
        let rec loop (games: Map<GameId, GameRm>, gameList: Map<GameId, GameListItemRm>) = async {
            match! inbox.Receive () with
            | AddGame (GameId gameId, status) ->
                return! loop (games.Add (GameId gameId, { id = gameId.ToString(); grid = emptyGridRm; status = status }), gameList)
            | UpdateGameStatus (gameId, status) ->
                match games.TryFind gameId with
                | Some { id = id; grid = grid; status = _ } ->
                    return! loop (games.Add (gameId, { id = id; grid = grid; status = status }), gameList)
                | _ -> return! loop (games, gameList)
            | UpdateGame (gameId, pos, marker, status) ->
                match games.TryFind gameId with
                | Some rm ->
                    let grid = updateGrid rm pos marker
                    return! loop (games.Add (gameId, { rm with grid = grid; status = status }), gameList)
                | None -> return! loop (games, gameList)
            | UpsertGames (gameId, listItem) -> return! loop (games, gameList.Add (gameId, listItem))
            | TryFind (gameId, rc) ->
                rc.Reply (games.TryFind gameId)
                return! loop (games, gameList)
            | GameList rc ->
                rc.Reply (gameList |> Map.toList |> List.map snd)
                return! loop (games, gameList)
        }
        loop (Map.empty, Map.empty))

    let eventHandler (GameId id, event) =
        match event with
        | Started ->
            do readModelsActor.Post (UpsertGames (GameId id, { id = id.ToString(); status = "running" }))
            do readModelsActor.Post (AddGame (GameId id, "player X to play"))
        | PlayerXPlayed pos ->
            do readModelsActor.Post (UpdateGame (GameId id, pos, "X", "player O to play"))
            do readModelsActor.Post (UpdateGameStatus (GameId id, "player O to play"))
        | PlayerOPlayed pos ->
            do readModelsActor.Post (UpdateGame (GameId id, pos, "O", "player X to play"))
            do readModelsActor.Post (UpdateGameStatus (GameId id, "player X to play"))
        | PlayerXWon ->
            do readModelsActor.Post (UpsertGames (GameId id, { id = id.ToString(); status = "finished (player X won)" }))
            do readModelsActor.Post (UpdateGameStatus (GameId id, "player X won"))
        | PlayerOWon ->
            do readModelsActor.Post (UpsertGames (GameId id, { id = id.ToString(); status = "finished (player O won)" }))
            do readModelsActor.Post (UpdateGameStatus (GameId id, "player O won"))
        | Tied ->
            do readModelsActor.Post (UpsertGames (GameId id, { id = id.ToString(); status = "finished (tie)" }))
            do readModelsActor.Post (UpdateGameStatus (GameId id, "tie"))

    let interpret: ReadModel<Effect<'t>> -> Effect<'t> = function
        | SubscribeToEventBus (_, cont) ->
            do EventBus.eventBusActor.Post (EventBus.Subscribe eventHandler)
            () |> cont
        | Game (id, cont) ->
            async {
                let! maybeGame = readModelsActor.PostAndAsyncReply (fun rc -> TryFind (id, rc))
                return maybeGame |> FSharpPlus.Option.toResultWith "not found"
            } |> ResultT >>= cont
        | Games (_, cont) ->
            async {
                let! games = readModelsActor.PostAndAsyncReply GameList
                return games |> Ok
            } |> ResultT >>= cont


module TicTacToe =
    open Dsls.TicTacToeDsl

    let rec interpret (dsl: TicTacToeDsl<_>) =
        let go = function
        | InL (InL x) -> Domain.interpret x
        | InL (InR x) -> EventStore.interpret x
        | InR (InL x) -> ReadModel.interpret x
        | InR (InR x) -> EventBus.interpret x
        Free.iterM go dsl
