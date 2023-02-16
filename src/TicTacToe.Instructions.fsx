module TicTacToe.Instructions

#load "TicTacToe.Dsls.fsx"
open TicTacToe.Core
open FSharpPlus
open FSharpPlus.Data
open Dsls.TicTacToeDsl

module private Domain =
    open Dsls.Domain

    let handle v = Free.liftF(Handle(v, id) |> Domain)
    let replay v = Free.liftF(Replay(v, id) |> Domain)

module private EventBus =
    open Dsls.EventBus

    let publish v = Free.liftF(Publish(v, id) |> EventBus)

module private EventStore =
    open Dsls.EventStore

    let append v = Free.liftF(Append(v, id) |> EventStore)
    let getStream v = Free.liftF(GetStream(v, id) |> EventStore)

module ReadModel =
    open Dsls.ReadModel

    let subscribe v = Free.liftF(SubscribeToEventBus(v, id) |> ReadModel)

module Queries =
    open Dsls.ReadModel

    let game v = Free.liftF(Game(v, id) |> ReadModel)
    let games = Free.liftF(Games((), id) |> ReadModel)

module Commands =
    let handle (id: GameId, cmd: Command) =
        monad {
            let! events = EventStore.getStream id
            let! state = Domain.replay events
            let! (v, newEvents) = Domain.handle (state, cmd)
            do! EventStore.append (id, v, newEvents)
            do! EventBus.publish (id, newEvents) 
            return ()
        }
        