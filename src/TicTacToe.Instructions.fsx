module TicTacToe.Instructions

#load "TicTacToe.Dsls.fsx"
open TicTacToe.Core
open FSharpPlus
open FSharpPlus.Data
open Dsls.TicTacToeDsl

module private Domain =
    open Dsls.Domain

    let handle v : TicTacToeDsl<_> = Handle(v, id) |> InL |> InL |> Free.liftF
    let replay v : TicTacToeDsl<_> = Replay(v, id) |> InL |> InL |> Free.liftF

module private EventBus =
    open Dsls.EventBus

    let publish v : TicTacToeDsl<_> = Publish(v, id) |> InR |> InR |> Free.liftF

module private EventStore =
    open Dsls.EventStore

    let append v : TicTacToeDsl<_> = Append(v, id) |> InR |> InL |> Free.liftF
    let getStream v : TicTacToeDsl<_> = GetStream(v, id) |> InR |> InL |> Free.liftF

module ReadModel =
    open Dsls.ReadModel

    let subscribe v : TicTacToeDsl<_> = SubscribeToEventBus(v, id) |> InL |> InR |> Free.liftF

module Queries =
    open Dsls.ReadModel

    let game v : TicTacToeDsl<_> = Game(v, id) |> InL |> InR |> Free.liftF
    let games : TicTacToeDsl<_> = Games((), id) |> InL |> InR |> Free.liftF

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
        