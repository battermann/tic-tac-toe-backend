#I "../packages"
#r "FParsec/lib/net40-client/FParsec.dll"
#r "FParsec/lib/net40-client/FParsecCS.dll"
#r "Aether/lib/net35/Aether.dll"
#r "Chiron/lib/net40/Chiron.dll"
#r "System.Runtime.Serialization"
#r "Suave/lib/net40/Suave.dll"
#load "TicTacToe.Interpreters.fsx"
#load "HalSharp.fsx"

open System

open Chiron

open Suave.Web
open Suave.Successful
open Suave.Operators
open Suave.Http
open Suave.ServerErrors
open System.IO
open Suave
open Suave.Filters
open Suave.RequestErrors
open System
open System.Net

open Chessie.ErrorHandling

open HalSharp

open TicTacToe
open Dsls.TicTacToeDsl
open Dsls.Free
open Instructions
open Types
open Interpreters
open Effects

let [|_; port|] = fsi.CommandLineArgs

let config =
    let ip = IPAddress.Parse "0.0.0.0"
    { defaultConfig with
        logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Info
        bindings=[ HttpBinding.mk HTTP ip (uint16 port) ] }

type PlayerId = PlayerId of Guid

type Players = {
    x: PlayerId option
    o: PlayerId option
}

type PlayerMapMessage = 
    | Add     of GameId * Players
    | TryJoin of GameId * PlayerId * AsyncReplyChannel<Result<unit, Error>> 
    | TryFind of GameId * AsyncReplyChannel<Players option>

[<AutoOpen>]
module Requests =
    type Play = {
        vertical: string
        horizontal: string
    }
        with
        static member FromJson (x:Play) = json {
            let! v = Json.read "vertical"
            let! h = Json.read "horizontal"
            return { vertical = v; horizontal = h }
        }

[<AutoOpen>]
module Serialization =
    let inline serializeList (list: 'a list) : Json =
        list |> List.map Json.serialize |> Json.serialize

    let serializeGrid (grid: string list list) =
        grid |> List.map (serializeList >> Json.serialize) |> Json.serialize

[<AutoOpen>]
module Mappers =
    let toGameList url (rms: (Dsls.ReadModel.GameListItemRm * bool) list) =
        let toListItem (rm: Dsls.ReadModel.GameListItemRm, closed) = { 
            Resource.links = Map.ofList [ yield "self", [ Link.simple (sprintf "%s/games/%s" url rm.id) ]
                                          if not closed then yield (sprintf "%s/rels/join" url, [ Link.simple (sprintf "%s/games/%s/join" url rm.id) ]) ]
            properties = Map.ofList [ "id", Chiron.String rm.id
                                      "status", Chiron.String rm.status ]
            embedded = Map.empty 
        }    
        { 
            Resource.links = Map.ofList [ "self", [ Link.simple (sprintf "%s/games" url) ] ]
            properties = Map.empty
            embedded = Map.ofList [ sprintf "%s/rels/games" url, rms |> List.map toListItem ] 
        }    

    let toGameResponse url playerId closedForJoin (rm: Dsls.ReadModel.GameRm) =
        let urlPart =
            match playerId with
            | Some pId -> sprintf "/players/%s" pId
            | _        -> ""

        { 
            Resource.links = Map.ofList [ yield "self", [ Link.simple (sprintf "%s/games/%s%s" url rm.id urlPart) ]
                                          yield "collection", [ Link.simple (sprintf "%s/games" url) ]
                                          if not closedForJoin then yield (sprintf "%s/rels/join" url, [ Link.simple (sprintf "%s/games/%s/join" url rm.id) ])  ]
            properties = Map.ofList [ "status", Chiron.String rm.status
                                      "id", Chiron.String rm.id
                                      "grid", serializeGrid rm.grid ]
            embedded = Map.empty
        }                         

let urlWithHost (request : HttpRequest) = 
  let host = 
    request.headers
    |> List.find (fst >> (=) "host")
    |> snd
  sprintf "%s://%s" request.url.Scheme host

[<AutoOpen>]
module Deserialization =
    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm = Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm
        |> getString
        |> Json.parse
        |> Json.deserialize
        
let bothPlayersJoined (playerMap: Actor<PlayerMapMessage>) gameId =
    async {
        let! players = playerMap.PostAndAsyncReply(fun rc -> TryFind (gameId, rc))
        match players with
        | Some { x = Some _ ; o = Some _ } -> return true
        | _ -> return false
    }

let game interpret (playerMap: Actor<PlayerMapMessage>) (id: string) playerId baseUrl: WebPart =
    let gameId = Guid(id) |> GameId
    fun (ctx: HttpContext) ->
        async {
            let! rm = interpret (Queries.game(gameId)) |> Async.ofAsyncResult
            let! closedForJoin = bothPlayersJoined playerMap gameId
            return! 
                match rm with
                | Ok (v,_) -> OK (v |> toGameResponse baseUrl playerId closedForJoin |> Resource.toJson |> Json.format) ctx
                | Bad errs -> INTERNAL_ERROR (errs |> String.concat ", ") ctx
        }

let gamesWithJoinableFlag (interpret: Free<_> -> Effect<_>) (playerMap: Actor<PlayerMapMessage>) =
        asyncTrial {
            let! games = interpret (Queries.games) 
            let! gamesWithFlag = 
                games |> List.map (fun (g: Dsls.ReadModel.GameListItemRm) -> bothPlayersJoined playerMap (Guid(g.id) |> GameId) |> Async.map (fun b -> g,b)) 
                |> Async.Parallel
            return gamesWithFlag |> Array.toList
        }

let games interpret (playerMap: Actor<PlayerMapMessage>) baseUrl: WebPart =
    fun (ctx: HttpContext) ->
        async {
            let! rmWithJoinableFlag = gamesWithJoinableFlag interpret playerMap |> Async.ofAsyncResult
            return! 
                match rmWithJoinableFlag with
                | Ok (v,_) -> OK (v |> toGameList baseUrl |> Resource.toJson |> Json.format) ctx
                | Bad errs -> INTERNAL_ERROR (errs |> String.concat ", ") ctx
        }   

let start interpret (playerMap: Actor<PlayerMapMessage>) baseUrl: WebPart =
    let gameId = Guid.NewGuid()
    let playerId = Guid.NewGuid()
    do playerMap.Post(Add(GameId gameId, { x = PlayerId playerId |> Some; o = None }))
    // handle cmd asynchronously
    do interpret (Commands.handle(GameId gameId, Start)) |> Async.ofAsyncResult|> Async.map ignore |> Async.Start
    ACCEPTED "" >=> Writers.setHeader "Location" (sprintf "%s/games/%s/players/%s" baseUrl (gameId.ToString()) (playerId.ToString()))

let join (playerMap: Actor<PlayerMapMessage>) (gameId: string) baseUrl: WebPart =
    let playerId = Guid.NewGuid()
    fun (ctx: HttpContext) ->
        async {
            let! result = playerMap.PostAndAsyncReply(fun rc -> TryJoin (Guid(gameId) |> GameId, playerId |> PlayerId, rc))
            match result with
            | Ok _ ->
                return! (ACCEPTED "" >=> Writers.setHeader "Location" (sprintf "%s/games/%s/players/%s" baseUrl (gameId.ToString()) (playerId.ToString()))) ctx
            | Bad errs ->
                return! INTERNAL_ERROR (errs |> String.concat ", ") ctx
        }

let play interpret (playerMap: Actor<PlayerMapMessage>) (id: string) (playerId: string) (play:Play) baseUrl: WebPart =
    let toPosition (v: string, h: string) =
        match (v.ToLower(),h.ToLower()) with
        | "top", "left" -> (Top, Left) |> Some
        | "top", "hcenter" -> (Top, HCenter) |> Some
        | "top", "right" -> (Top, Right) |> Some
        | "vcenter", "left" -> (VCenter, Left) |> Some
        | "vcenter", "hcenter" -> (VCenter, HCenter) |> Some
        | "vcenter", "right" -> (VCenter, Right) |> Some
        | "bottom", "left" -> (Bottom, Left) |> Some
        | "bottom", "hcenter" -> (Bottom, HCenter) |> Some
        | "bottom", "right" -> (Bottom, Right) |> Some
        | _ -> None
    
    let maybePosition = toPosition (play.vertical, play.horizontal)
    let gameId = Guid(id) |> GameId
    fun (ctx: HttpContext) ->
        async {
            let! players = playerMap.PostAndAsyncReply(fun rc -> TryFind (gameId, rc))
            let! closedForJoin = bothPlayersJoined playerMap gameId
            match players, maybePosition, closedForJoin with
            | Some { x = Some plX; o = Some plO }, Some(v,h), true ->
                let cmd = if (Guid(playerId) |> PlayerId) = plX then PlayX else PlayO
                // handle cmd asyncronously
                do interpret (Commands.handle(gameId, cmd (v, h))) |> Async.ofAsyncResult|> Async.map ignore |> Async.Start
                return! (ACCEPTED "" >=> Writers.setHeader "Location" (sprintf "%s/games/%s/players/%s" baseUrl (id.ToString()) playerId)) ctx
            | Some _, Some _, false -> return! CONFLICT "opponent hasn't joined game yet" ctx
            | _, None, _            -> return! BAD_REQUEST "unknown position" ctx
            | _                     -> return! NOT_FOUND "unknown player id" ctx
        }

let playersMapActor = 
    Actor.Start(fun inbox ->
        let rec loop (playersMap: Map<GameId, Players>) =
            async {
                let! msg = inbox.Receive()
                match msg with
                | Add (gameId, players) ->
                    return! loop (playersMap.Add(gameId, players))
                | TryJoin (gameId, player, rc) -> 
                    match playersMap.TryFind gameId with
                    | Some { x = Some _; o = Some _ } ->
                        rc.Reply(fail "game already running")
                        return! loop playersMap
                    | Some { x = Some pX; o = None } ->
                        rc.Reply(ok ())
                        return! loop (playersMap.Add(gameId, { x = Some pX; o = player |> Some }))
                    | _ -> 
                        rc.Reply(fail "game invalid")
                        return! loop playersMap
                | TryFind (gameId, rc) ->
                    rc.Reply(playersMap.TryFind gameId)
                    return! loop playersMap
            }
        loop Map.empty)

let interpret free =
    TicTacToe.interpret
        Domain.interpret
        EventBus.interpret
        EventStore.interpret
        ReadModel.interpret free

let app =
    let setJsonHeader = Writers.setMimeType "application/hal+json"

    let setCorsHaeders = 
        Writers.setHeader "Access-Control-Allow-Origin" "*" 
        >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type" 
        >=> Writers.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS, DELETE, PATCH"

    let setHeaders = setJsonHeader >=> setCorsHaeders

    choose [ 
        GET >=> choose [
            path "/" >=> request (urlWithHost >> fun url -> 
                { Resource.empty with links = Map.ofList [ "self", [ Link.simple "/" ]
                                                           sprintf "%s/rels/games" url, [ Link.simple (sprintf "%s/games" url) ] ] }
                |> Resource.toJson |> Json.format |> OK) 
                >=> setHeaders 
            path "/games" >=> request (urlWithHost >> games interpret playersMapActor) >=> setHeaders
            pathScan "/games/%s/player/%s" (fun (gameId, playerId) -> 
                request (urlWithHost >> game interpret playersMapActor gameId (Some playerId))) >=> setHeaders
            pathScan "/games/%s" (fun gameId -> 
                request (urlWithHost >> game interpret playersMapActor gameId None)) >=> setHeaders                                    
        ]
        POST >=> choose [
            path "/games" >=> request (urlWithHost >> start interpret playersMapActor) >=> setHeaders
        ]
        PATCH >=> choose [
            pathScan "/games/%s/players/%s" (fun (gameId, playerId) -> 
                request (fun req -> play interpret playersMapActor gameId playerId (getResourceFromReq req) (urlWithHost req)))
                >=> setHeaders
        ]
        PUT >=> choose [
            pathScan "/games/%s/join" (fun gameId ->
                request (urlWithHost >> join playersMapActor gameId)) >=> setHeaders                              
        ]
    ]

interpret (ReadModel.subscribe())

startWebServer config app