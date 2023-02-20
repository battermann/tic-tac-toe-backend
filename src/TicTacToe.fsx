module TicTacToe.Core

#r "nuget: FSharpPlus,1.4.0"

open System
open FSharpPlus

[<AutoOpen>]
module Types =

    type GameId = GameId of Guid

    type Marker = X | O | Empty

    type Horizontal = Left | HCenter | Right
    type Vertical = Top | VCenter | Bottom

    type Position = Vertical * Horizontal

    type Square = Vertical * Horizontal * Marker

    type Player = 
    | PlayerX
    | PlayerO

    type Grid = Square list

    type Game =
    | Initial
    | PlayerXToPlay of Grid
    | PlayerOToPlay of Grid
    | PlayerXWins of Grid
    | PlayerOWins of Grid
    | Tie of Grid

    type Version = Version of int
    type State = Version * Game

    type Command = 
    | Start
    | PlayX of Position
    | PlayO of Position

    type Event =
    | Started
    | PlayerXPlayed of Position
    | PlayerOPlayed of Position
    | PlayerXWon
    | PlayerOWon
    | Tied

    type VersionedEvents = GameId * Version * Event list


module Domain =
    let private hLines = [
        [(Top, Left); (Top, HCenter); (Top, Right)]
        [(VCenter, Left); (VCenter, HCenter); (VCenter, Right)]
        [(Bottom, Left); (Bottom, HCenter); (Bottom, Right)]
    ]
    
    let private vLines = [
        [(Top, Left); (VCenter, Left); (Bottom, Left)]
        [(Top, HCenter); (VCenter, HCenter); (Bottom, HCenter)]
        [(Top, Right); (VCenter, Right); (Bottom, Right)]
    ]
    
    let private diag = [
        [(Top, Left); (VCenter, HCenter); (Bottom, Right)]
        [(Bottom, Left); (VCenter, HCenter); (Top, Right)]
    ]

    let private marker = function PlayerX _ -> X | PlayerO _ -> O

    let private equals (v, h) (v', h', _) = v' = v && h' = h

    let private placeMarker grid player (v, h) =
        if grid |> List.exists (equals (v, h)) then
            Error "square already marked"
        else
            Ok ((v, h, marker player) :: grid, player, v, h)

    let private getMarker (grid: Grid) pos =
        let square = grid |> List.tryFind (equals pos)
        match square with
        | Some (_,_,m) -> m
        | None         -> Empty

    let private isLineOnlyPlayerBy player grid line =
        line
        |> List.map (getMarker grid)
        |> List.forall (fun m -> 
            match m, player with
            | X, PlayerX -> true
            | O, PlayerO -> true
            | _          -> false)

    let private eval (grid, player, v, h) =
        let wins = isLineOnlyPlayerBy player grid

        match hLines @ vLines @ diag |> List.exists wins, player with
        | true, PlayerX -> PlayerXWins grid
        | true, PlayerO -> PlayerOWins grid
        | _, PlayerX when grid |> List.length < 9 -> PlayerOToPlay grid
        | _, PlayerO when grid |> List.length < 9 -> PlayerXToPlay grid
        | _ -> Tie grid    

    let replay (events: Event list) : Result<State, string> = 

        let replayError err = sprintf "replay error, %s" err

        let apply state event: Result<Game, string> =
            match state, event with
            | Initial, Started -> 
                PlayerXToPlay [] |> Ok
            | PlayerXToPlay grid, PlayerXPlayed pos -> 
                placeMarker grid PlayerX pos |> Result.map eval |> Result.mapError replayError
            | PlayerOToPlay grid, PlayerOPlayed pos -> 
                placeMarker grid PlayerO pos |> Result.map eval |> Result.mapError replayError
            | PlayerXToPlay grid, _ -> 
                Error "replay error, wrong turn"
            | PlayerOToPlay grid, _ -> Error "replay error, wrong turn"
            | _ -> 
                Error "replay error, game is finished"

        let folder errorOrState event = 
            monad {
                let! (Version v, s) = errorOrState
                let! game = apply s event
                return v + 1 |> Version, game
            }

        events |> List.fold folder (Ok (Version -1, Initial))

    let handle (version: Version, game: Game) (cmd: Command): Result<Version * Event list, string> =
        let xToPlayAndXPlays grid pos = 
            monad {
                let! state = placeMarker grid PlayerX pos
                let evaluatedState = eval state
                let events = 
                    match evaluatedState with
                    | PlayerOToPlay _ -> [PlayerXPlayed pos]
                    | PlayerXWins _ -> [PlayerXPlayed pos; PlayerXWon]
                    | _ -> []
                return version, events
            }   

        let oToPlayAndOPlays grid pos = 
            monad {
                let! state = placeMarker grid PlayerO pos
                let evaluatedState = eval state
                let events = 
                    match evaluatedState with
                    | PlayerXToPlay _ -> [PlayerOPlayed pos]
                    | PlayerOWins _ -> [PlayerOPlayed pos; PlayerOWon]
                    | Tie _ -> [PlayerOPlayed pos; Tied]
                    | _ -> []
                return version, events
            }                    

        match game, cmd with
        | Initial, Start -> 
            Ok (version, [Started])
        | PlayerXToPlay grid, PlayX pos ->
            xToPlayAndXPlays grid pos
        | PlayerOToPlay grid, PlayO pos -> 
            oToPlayAndOPlays grid pos
        | PlayerXToPlay grid, PlayO _ -> 
            Error "not your turn"
        | PlayerOToPlay grid, PlayX _ -> 
            Error "not your turn"
        | _ -> Error "game is finished"
            