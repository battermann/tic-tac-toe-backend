module TicTacToe.Dsls

#load "TicTacToe.fsx"

open TicTacToe
open TicTacToe.Core

type Continuation<'output, 'next> = 'output -> 'next


module Domain =
    type Domain<'next> =
    | Handle of (State * Command) * Continuation<Version * Event list, 'next>
    | Replay of Event list        * Continuation<State, 'next>
    with
        static member Map (x, f) =
            match x with
            | Handle(v, cont) -> Handle(v, cont >> f)
            | Replay(v, cont) -> Replay(v, cont >> f)


module ReadModel =
    type GridRm = string list list
    type GameRm = {
        id: string
        grid: GridRm
        status: string
    }
    type GameListItemRm = {
        id: string
        status: string
    }

    type ReadModel<'next> =
    | SubscribeToEventBus of unit   * Continuation<unit, 'next>
    | Game                of GameId * Continuation<GameRm, 'next>
    | Games               of unit   * Continuation<GameListItemRm list, 'next>
    with
        static member Map (x, f) =
            match x with
            | SubscribeToEventBus(v, cont) -> SubscribeToEventBus(v, cont >> f)
            | Game(v, cont)                -> Game(v, cont >> f)
            | Games(v, cont)               -> Games(v, cont >> f)


module EventStore =
    type EventStore<'next> =
    | GetStream of GameId *
                   Continuation<Event list, 'next>
    | Append    of (GameId * Version * Event list) *
                   Continuation<unit, 'next>
    with
        static member Map (x, f) =
            match x with
            | GetStream(v, cont) -> GetStream(v, cont >> f)
            | Append(v, cont)    -> Append(v, cont >> f)


module EventBus = 
    type EventBus<'next> =
    | Publish of (GameId * Event list) * Continuation<unit, 'next>
    with
        static member Map (Publish(v, cont), f) = Publish (v, cont >> f)


module TicTacToeDsl =
    type TicTacToeDsl<'next> =
    | Domain     of Domain.Domain<'next>
    | EventStore of EventStore.EventStore<'next>
    | ReadModel  of ReadModel.ReadModel<'next>
    | EventBus   of EventBus.EventBus<'next>
    with
        static member Map (dsl : TicTacToeDsl<'a>, f: 'a -> 'b) : TicTacToeDsl<'b> =
            match dsl with
            | Domain d      -> Domain.Domain<'a>.Map (d, f) |> Domain
            | EventStore es -> EventStore.EventStore<'a>.Map (es, f) |> EventStore
            | EventBus bus  -> EventBus.EventBus<'a>.Map (bus, f) |> EventBus
            | ReadModel rm  -> ReadModel.ReadModel<'a>.Map (rm, f) |> ReadModel