module TicTacToe.Dsls

#load @"TicTacToe.fsx"

open TicTacToe

type Continuation<'output, 'next> = 'output -> 'next


module Domain =
    type Domain<'next> =
    | Handle of (State * Command) * Continuation<Version * Event list, 'next>
    | Replay of Event list        * Continuation<State, 'next>

    let map f x =
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

    let map f x =
        match x with
        | SubscribeToEventBus(v, cont) -> SubscribeToEventBus(v, cont >> f)
        | Game(v, cont)                -> Game(v, cont >> f)
        | Games(v, cont)               -> Games(v, cont >> f)


module EventStore =
    type EventStore<'next> =
    | GetStream of GameId                          * Continuation<Event list, 'next>
    | Append    of (GameId * Version * Event list) * Continuation<unit, 'next>

    let map f x =
        match x with
        | GetStream(v, cont) -> GetStream(v, cont >> f)
        | Append(v, cont)    -> Append(v, cont >> f)


module EventBus = 
    type HandleEvent = Event -> unit

    type EventBus<'next> =
    | Publish of (GameId * Event list) * Continuation<unit, 'next>
    let map f x =
        match x with
        | Publish(v, cont)   -> Publish(v, cont >> f)


module TicTacToeDsl =
    type TicTacToeDsl<'next> =
    | Domain     of Domain.Domain<'next>
    | EventStore of EventStore.EventStore<'next>
    | ReadModel  of ReadModel.ReadModel<'next>
    | EventBus   of EventBus.EventBus<'next>


    let map (f: 'a -> 'b) (dsl : TicTacToeDsl<'a>) : TicTacToeDsl<'b> =
        match dsl with
        | Domain d      -> Domain.map f d |> Domain
        | EventStore es -> EventStore.map f es |> EventStore
        | EventBus bus  -> EventBus.map f bus |> EventBus
        | ReadModel rm  -> ReadModel.map f rm |> ReadModel


module Free =
    open TicTacToeDsl

    type Free<'a> =
    | Pure of 'a
    | Free of TicTacToeDsl<Free<'a>>    

    [<RequireQualifiedAccess>]
    module FreeMonad =
        let rec bind (f: 'a -> Free<'b>) (dsl : Free<'a>) : Free<'b> =
            match dsl with
            | Pure value -> f value
            | Free t  -> map (bind f) t |> Free

        let liftF(dsl:TicTacToeDsl<'a>) : Free<'a> =
            Free (map Pure dsl)

    type FreeBuilder() =
        member x.Bind(dsl, f) = FreeMonad.bind f dsl
        member x.Return(value) = Pure value
        member x.Zero() = Pure ()

    let free = new FreeBuilder()