module TicTacToe.Instructions

#load @"TicTacToe.Dsls.fsx"

open Dsls.Free
open Dsls.TicTacToeDsl

module private Domain =
    open Dsls.Domain

    let handle v = FreeMonad.liftF(Handle(v, id) |> Domain)
    let replay v = FreeMonad.liftF(Replay(v, id) |> Domain)

module private EventBus =
    open Dsls.EventBus

    let publish v = FreeMonad.liftF(Publish(v, id) |> EventBus)

module private EventStore =
    open Dsls.EventStore

    let append v = FreeMonad.liftF(Append(v, id) |> EventStore)
    let getStream v = FreeMonad.liftF(GetStream(v, id) |> EventStore)

module ReadModel =
    open Dsls.ReadModel

    let subscribe v = FreeMonad.liftF(SubscribeToEventBus(v, id) |> ReadModel)

module Queries =
    open Dsls.ReadModel

    let game v = FreeMonad.liftF(Game(v, id) |> ReadModel)
    let games = FreeMonad.liftF(Games((), id) |> ReadModel)

module Commands =
    let handle (id: GameId, cmd: Command) =
        free {
            let! events = EventStore.getStream id
            let! state = Domain.replay events
            let! (v, newEvents) = Domain.handle (state, cmd)
            do! EventStore.append (id, v, newEvents)
            do! EventBus.publish (id, newEvents) 
            return ()
        }
        