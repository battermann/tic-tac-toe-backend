#I "../packages"
#r "FParsec/lib/net40-client/FParsec.dll"
#r "FParsec/lib/net40-client/FParsecCS.dll"
#r "Aether/lib/net35/Aether.dll"
#r "Chiron/lib/net40/Chiron.dll"
#r "System.Runtime.Serialization"
#r "Suave/lib/net40/Suave.dll"
#r "Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#load "TicTacToe.Interpreters.fsx"

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

[<AutoOpen>]
module Responses =
    type Link = {
        rel: string
        href: string
    }
        with
        static member ToJson (x:Link) = json {
            do! Json.write "rel" x.rel
            do! Json.write "href" x.href
        }

    type ListItemResponse = {
        id: string
        status: string
        links: Link list
    } 
        with
        static member ToJson (x:ListItemResponse) = json {
            do! Json.write "id" x.id
            do! Json.write "status" x.status
        }

    type Home = {
        links: Link list
    }
        with
        static member ToJson (x:Home) = json {
            do! Json.write "_links" "todo"
        }    

    type GameResponse = {
        id: string
        grid: string list list
        status: string
        links: Link list
    }
        with
        static member ToJson (x:GameResponse) = json {
            do! Json.write "id" x.id
            do! Json.write "status" x.status
        }    

    type Join = {
        id: string
        links: Link list
    }
        with
        static member ToJson (x:Join) = json {
            do! Json.write "id" x.id
        }    

[<AutoOpen>]
module Mappers =
    let toListItem url (rm: Dsls.ReadModel.GameListItemRm) =
        { ListItemResponse.id = rm.id
          status = rm.status
          links = [{ rel = "details"; href = sprintf "%s/games/%s" url rm.id }] }

    let toGameResponse url playerId closedForJoin (rm: Dsls.ReadModel.GameRm) =
        let urlPart =
            match playerId with
            | Some pId -> sprintf "/players/%s" pId
            | _        -> ""
        { GameResponse.id = rm.id
          grid = rm.grid
          status = rm.status
          links = { rel = "self"; href = sprintf "%s/games/%s%s" url rm.id urlPart }
                  :: if closedForJoin then []
                     else [{ rel = "join"; href = sprintf "%s/games/%s/join" url rm.id }] }

let urlWithHost (request : HttpRequest) = 
  let host = 
    request.headers
    |> List.find (fst >> (=) "host")
    |> snd
  sprintf "%s://%s" request.url.Scheme host

[<AutoOpen>]
module Serialization =
    open Newtonsoft.Json
    open Newtonsoft.Json.Serialization

    let fromJson<'a> json =
        JsonConvert.DeserializeObject(json, typeof<'a>) :?> 'a

    let getResourceFromReq<'a> (req : HttpRequest) =
        let getString rawForm =
            Text.Encoding.UTF8.GetString(rawForm)
        req.rawForm |> getString |> fromJson<'a>

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
                | Ok (v,_) -> OK (v |> toGameResponse baseUrl playerId closedForJoin |> Json.serialize |> Json.formatWith JsonFormattingOptions.Compact) ctx
                | Bad errs -> INTERNAL_ERROR (errs |> String.concat ", ") ctx
        }

let games interpret baseUrl: WebPart =
    fun (ctx: HttpContext) ->
        async {
            let! rm = interpret (Queries.games) |> Async.ofAsyncResult
            return! 
                match rm with
                | Ok (v,_) -> OK (v |> List.map (fun rm -> toListItem baseUrl rm) |> Json.serialize |> Json.formatWith JsonFormattingOptions.Compact) ctx
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
    let setJsonHeader = Writers.setMimeType "application/json; charset=utf-8"

    let setCorsHaeders = 
        Writers.setHeader "Access-Control-Allow-Origin" "*" 
        >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type" 
        >=> Writers.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS, DELETE, PATCH"

    let setHeaders = setJsonHeader >=> setCorsHaeders

    choose [ 
        GET >=> choose [
            path "/" >=> request (urlWithHost >> fun url -> 
                { Home.links = [{ rel = "games"; href = sprintf "%s/games" url }] } |> Json.serialize |> Json.formatWith JsonFormattingOptions.Compact |> OK) 
                >=> setHeaders 
            path "/games" >=> request (urlWithHost >> games interpret) >=> setHeaders
            pathScan "/games/%s/player/%s" (fun (gameId, playerId) -> 
                request (urlWithHost >> game interpret playersMapActor gameId (Some playerId))) >=> setHeaders
            pathScan "/games/%s/join" (fun gameId -> 
                request (urlWithHost >> fun url ->
                    { Join.id = gameId; links = [ { rel = "self"; href = sprintf "%s/games/%s/join" url gameId } ] } |> Json.serialize |> Json.formatWith JsonFormattingOptions.Compact |> OK )) 
                    >=> setHeaders
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