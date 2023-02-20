#load "TicTacToe.Interpreters.fsx"
#load "Hypermedia.fsx"
#r "nuget: Microsoft.AspNetCore"
#r "nuget: Microsoft.AspNetCore.Http"

open System
open System.Net
open System.IO
open System.Threading.Tasks

open FSharpPlus
open FSharpPlus.Data
open FSharp.Data

open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting

open Hypermedia
open Hal

open TicTacToe
open TicTacToe.Core
open Instructions
open Interpreters

module [<AutoOpen>] WebServer =

    type WebPart<'a> = 'a -> ResultT<Async<Result<'a, unit>>>

    module HttpAdapter =
        let appMap (path: string) (map: IApplicationBuilder -> unit) (app: IApplicationBuilder) : IApplicationBuilder =
            app.Map (PathString path, Action<_> map)

    type HeaderKey (key: string) =
        let lower = key.ToLowerInvariant ()
        member _.ToLowerInvariant () = lower
        override _.ToString () = key
        override _.GetHashCode () = hash lower
        override this.Equals (obj: obj) = match obj with | :? HeaderKey as k -> this.ToLowerInvariant().Equals (k.ToLowerInvariant ()) | _ -> false
        interface IComparable with
            member this.CompareTo otherObject =
                let other = otherObject :?> HeaderKey
                this.ToLowerInvariant().CompareTo(other.ToLowerInvariant ())
    
    module WebPart =
        let inline fail (_: 'a) : ResultT<Async<Result<'a, unit>>> = ResultT <| async.Return (Error ())

    module [<AutoOpen>] Http =
        type Response = {
            statusCode:  int option
            content:     string option
            contentType: string option
            headers:     Map<HeaderKey, string>
        }

        type Context = { request:HttpRequest; response:Response }
        module Response =
            let empty = { statusCode=None; content=None; contentType=None ; headers=Map.empty}
        module Context =
            let ofHttpContext (httpContext: HttpContext) =
                { request = httpContext.Request; response = Response.empty }
        let yieldToResponse (from: Response) (to': HttpResponse) =
            match from.contentType with Some contentType -> to'.ContentType <- contentType | _ -> ()
            match from.statusCode  with Some statusCode  -> to'.StatusCode  <- statusCode  | _ -> ()
            match from.content     with Some content -> to'.WriteAsync content | _ -> Task.CompletedTask

    module [<AutoOpen>] Writers =
        let private succeed x = async.Return (Ok x)
        let setStatusAndContent statusCode content =
            ResultT << fun ctx -> { ctx with response = { ctx.response with statusCode = Some statusCode; content = Some content } } |> succeed
        
        let setHeader key value =
            ResultT << fun ctx -> { ctx with response = { ctx.response with headers = Map.add (HeaderKey key) value ctx.response.headers }} |> succeed

    let OK             s = setStatusAndContent (int HttpStatusCode.OK) s
    let ACCEPTED       s = setStatusAndContent (int HttpStatusCode.Accepted) s
    let BAD_REQUEST    s = setStatusAndContent (int HttpStatusCode.BadRequest) s 
    let INTERNAL_ERROR s = setStatusAndContent (int HttpStatusCode.InternalServerError) s
    let NOT_FOUND      s = setStatusAndContent (int HttpStatusCode.NotFound) s
    let CONFLICT       s = setStatusAndContent (int HttpStatusCode.Conflict) s
    
    let response (method: string) = ResultT << fun (x : Context) -> async.Return (if (method = x.request.Method) then Ok x else Error ())

    let GET  (x: Http.Context) = response "GET" x
    let POST (x: Http.Context) = response "POST" x
    let path s = let path = implicit s in ResultT << fun (x: Http.Context) -> async.Return (if path = x.request.Path then Ok x else Error ())

    let inline pathScan path routeHandler : WebPart<Context> = fun (x: Http.Context) ->
        match string x.request.Path |> trySscanf path with
        | Some p -> routeHandler p x
        | _      -> WebPart.fail x

    let appRun (app: WebPart<Context>) (appBuilder: IApplicationBuilder) =
        let appRun (func: HttpContext -> #Task) (b: IApplicationBuilder) =
            b.Run (RequestDelegate (fun ctx -> func ctx :> Task))
        
        let runApp context = task {
            let ctx = Context.ofHttpContext context
            match! app ctx |> ResultT.run |> Async.StartAsTask with
            | Ok res   -> return! Http.yieldToResponse res.response context.Response
            | Error () -> return! Task.CompletedTask
        }
        appRun runApp appBuilder

    let request apply : WebPart<Http.Context> = fun ctx -> apply ctx.request ctx


let (!!) x = Path.Combine (__SOURCE_DIRECTORY__, x)
let (</>) path1 path2 = Path.Combine(path1, path2).Replace('\\','/')

let GAMES = "games"
let GAME = "game"
let MOVES = "moves"
let JOIN = "join"
let PLAY = "play"
let NEWGAME = "newgame"


module Paths =
    let api = "api"
    let games = api </> GAMES
    let game id = (api </> GAMES) </> id
    let moves id = game id </> MOVES
    let join id = game id </> JOIN
    let rels = sprintf "docs/rels/%s"

module Routes =
    let game = new PrintfFormat<(string -> string),unit,string,string,string>(("/" </> Paths.games) </> "%s")
    let moves = new PrintfFormat<(string -> string),unit,string,string,string>((("/" </> Paths.games) </> "%s") </> MOVES)
    let join = new PrintfFormat<(string -> string),unit,string,string,string>((("/" </> Paths.games) </> "%s") </> JOIN)
  
let gamesRel host = host </> ("games" |> Paths.rels)
let joinRel host = host </> ("join" |> Paths.rels)
let playRel host = host </> ("play" |> Paths.rels)
let newGameRel host = host </> ("newgame" |> Paths.rels)

type PlayerId = PlayerId of Guid

type Players = {
    x: PlayerId option
    o: PlayerId option
}

type PlayerMapMessage = 
    | Add     of GameId * Players
    | TryJoin of GameId * PlayerId * AsyncReplyChannel<Result<unit, string>>
    | TryFind of GameId * AsyncReplyChannel<Players option>

[<AutoOpen>]
module Responses =
    type Error = {
        error: string
    }
        with
        static member ToJson (x:Error) =
            JsonValue.Record [| "error", JsonValue.String(x.error) |]


[<AutoOpen>]
module Requests =
    let findString s arr = arr |> Array.find (fun (k,v) -> k = s) |> snd |> fun v -> v.ToString()

    type Play = {
        vertical: string
        horizontal: string
        playerId: string
    }
        with
        static member FromJson (x:JsonValue) =
            match x with
            | JsonValue.Record props ->
                { vertical = (props |> findString "vertical").Replace("\"","")
                  horizontal = (props |> findString "horizontal").Replace("\"","")
                  playerId = (props |> findString "playerId").Replace("\"","") }
            | _ -> failwith "bad format"

    type Player = {
        playerId: string
    }
        with
        static member FromJson (x:JsonValue) = 
            match x with
            | JsonValue.Record props ->
                  { playerId = props |> findString "playerId" }
            | _ -> failwith "bad format"

        static member ToJson (x:Player) =
            JsonValue.Record [| "playerId", JsonValue.String(x.playerId) |]

[<AutoOpen>]
module Serialization =
    let serializeList (list: string list) : JsonValue =
        list |> List.map JsonValue.String |> List.toArray |> JsonValue.Array

    let serializeGrid (grid: string list list) : JsonValue =
        grid |> List.map serializeList |> List.toArray |> JsonValue.Array

[<AutoOpen>]
module Mappers =
    let jsonToString (json: JsonValue) = json.ToString(JsonSaveOptions.DisableFormatting)

    let toGameList url (rms: (Dsls.ReadModel.GameListItemRm * bool) list) =
        let toListItem (rm: Dsls.ReadModel.GameListItemRm, closed) = {
            Resource.empty with
                Resource.links = Map.ofList [ yield "self", Singleton (Link.create (Uri (url </> Paths.game rm.id)))
                                              if not closed then yield (joinRel url, Singleton (Link.create (Uri (url </> Paths.join rm.id)))) ]
                properties = Map.ofList [ "id", JObject <| JsonValue.String(rm.id)
                                          "status", JObject <| JsonValue.String(rm.status) ]
        }    
        { 
            Resource.empty with
                Resource.links = Map.ofList [ "self", Link.create (Uri (url </> Paths.games)) |> Singleton
                                              newGameRel url, Link.create (Uri (url </> Paths.games)) |> Singleton ]
                embedded = Map.ofList [ gamesRel url, rms |> List.map toListItem |> Collection ] 
        }    

    let toGameResponse url closedForJoin (rm: Dsls.ReadModel.GameRm) =
        { 
            Resource.empty with
                Resource.links = Map.ofList [ yield "self", Singleton (Link.create (Uri (url </> Paths.game rm.id)))
                                              yield "collection", Singleton (Link.create (Uri (url </> Paths.games)))
                                              yield playRel url, Singleton (Link.create (Uri (url </> Paths.moves rm.id)))
                                              if not closedForJoin then yield (joinRel url, Singleton (Link.create (Uri (url </> Paths.join rm.id)) ))  ]
                properties = Map.ofList [ "status", JObject <| JsonValue.String(rm.status)
                                          "id", JObject <| JsonValue.String(rm.id)
                                          "grid", JObject <| serializeGrid rm.grid]
        }

[<AutoOpen>]
module Deserialization =
    let getPlay (req: HttpRequest) =
        let rawRequestBody = (new IO.StreamReader (req.Body)).ReadToEnd ()
        rawRequestBody        
        |> JsonValue.Parse
        |> Play.FromJson
        
let bothPlayersJoined (playerMap: Actor<PlayerMapMessage>) gameId =
    async {
        let! players = playerMap.PostAndAsyncReply(fun rc -> TryFind (gameId, rc))
        match players with
        | Some { x = Some _ ; o = Some _ } -> return true
        | _ -> return false
    }

let game (interpret: Free<_, _> -> Effect<_>) (playerMap: Actor<PlayerMapMessage>) (id: string) baseUrl: WebPart<_> =
    let gameId = Guid id |> GameId
    fun (ctx: Http.Context) ->
        monad {
            let! rm = interpret (Queries.game gameId)
            let! closedForJoin = bothPlayersJoined playerMap gameId |> ResultT.lift
            return! OK (rm |> toGameResponse baseUrl closedForJoin |> FSharpDataIntepreter.Hal.toJson |> jsonToString) ctx }
        </catch/> fun e -> INTERNAL_ERROR ({ error = e } |> (Error.ToJson >> jsonToString)) ctx

let gamesWithJoinableFlag (interpret: Free<_, _> -> Effect<_>) (playerMap: Actor<PlayerMapMessage>) =
        monad {
            let! games = interpret Queries.games
            let! gamesWithFlag =
                games |> List.map (fun (g: Dsls.ReadModel.GameListItemRm) -> bothPlayersJoined playerMap (Guid g.id |> GameId) |> Async.map (fun b -> g, b))
                |> Async.Parallel |> ResultT.lift
            return gamesWithFlag |> Array.toList
        }

let games interpret (playerMap: Actor<PlayerMapMessage>) baseUrl : WebPart<_> =
    fun (ctx: Http.Context) ->
        monad {
            let! rmWithJoinableFlag = gamesWithJoinableFlag interpret playerMap
            return! OK (rmWithJoinableFlag |> toGameList baseUrl |> FSharpDataIntepreter.Hal.toJson |> jsonToString) ctx
        }
        </catch/> fun e -> INTERNAL_ERROR ({ error = e } |> Error.ToJson |> jsonToString) ctx

let start interpret (playerMap: Actor<PlayerMapMessage>) baseUrl: WebPart<_> =
    let gameId = Guid.NewGuid()
    let playerId = Guid.NewGuid()
    do playerMap.Post(Add(GameId gameId, { x = PlayerId playerId |> Some; o = None }))
    // handle cmd asynchronously
    do interpret (Commands.handle(GameId gameId, Start)) |> ResultT.run |> Async.map ignore |> Async.Start
    let body = { playerId = playerId.ToString() } |> Player.ToJson |> jsonToString
    ACCEPTED body >=> Writers.setHeader "Location" (baseUrl </> Paths.game (gameId.ToString()))

let join (playerMap: Actor<PlayerMapMessage>) (gameId: string) baseUrl : WebPart<_> =
    let playerId = Guid.NewGuid ()
    fun (ctx: Http.Context) ->
        monad {
            do! playerMap.PostAndAsyncReply (fun rc -> TryJoin (Guid gameId |> GameId, playerId |> PlayerId, rc)) |> ResultT
            let body = { playerId = string playerId } |> Player.ToJson |> jsonToString
            return!
                (ACCEPTED body >=> Writers.setHeader "Location" (baseUrl </> Paths.game (string gameId))) ctx
        }
        </catch/> fun err ->
            if err = "game already running" then
                CONFLICT ({ error = err } |> Error.ToJson |> jsonToString) ctx
            elif err = "game invalid" then
                NOT_FOUND ({ error = err } |> Error.ToJson |> jsonToString) ctx
            else 
                INTERNAL_ERROR ({ error = err } |> Error.ToJson |> jsonToString) ctx

let play interpret (playerMap: Actor<PlayerMapMessage>) (id: string) (play:Play) baseUrl: WebPart<_> =
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
    let gameId = Guid id |> GameId
    fun (ctx: Http.Context) ->
        monad {
            let! players = playerMap.PostAndAsyncReply (fun rc -> TryFind (gameId, rc)) |> ResultT.lift
            let! closedForJoin = bothPlayersJoined playerMap gameId |> ResultT.lift
            match players, maybePosition, closedForJoin with
            | Some { x = Some plX; o = Some plO }, Some(v,h), true ->
                let cmd = if Guid play.playerId |> PlayerId = plX then PlayX else PlayO
                // handle cmd asyncronously
                do interpret (Commands.handle(gameId, cmd (v, h))) |> ResultT.run |> Async.map ignore |> Async.Start
                return!
                    (ACCEPTED "{}" >=> Writers.setHeader "Location" (baseUrl </> Paths.game (gameId.ToString()))) ctx
            | Some _, Some _, false -> return! CONFLICT ({ error = "opponent hasn't joined game yet" } |> Error.ToJson |> jsonToString) ctx
            | _, None, _            -> return! BAD_REQUEST ({ error = "unknown position" } |> Error.ToJson |> jsonToString) ctx
            | _                     -> return! NOT_FOUND ({ error = "unknown player id" } |> Error.ToJson |> jsonToString) ctx
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
                        rc.Reply(Error "game already running")
                        return! loop playersMap
                    | Some { x = Some pX; o = None } ->
                        rc.Reply(Ok ())
                        return! loop (playersMap.Add(gameId, { x = Some pX; o = player |> Some }))
                    | _ -> 
                        rc.Reply(Error "game invalid")
                        return! loop playersMap
                | TryFind (gameId, rc) ->
                    rc.Reply(playersMap.TryFind gameId)
                    return! loop playersMap
            }
        loop Map.empty)

let interpret = TicTacToe.interpret

let app =
    let urlWithHost (request : HttpRequest) = 
        let host = 
            request.Headers
            |> Seq.find (fun (KeyValue (k, _)) -> String.toLower k = "host")
            |> (|KeyValue|)
            |> snd
            |> string
        sprintf "%s://%s" request.Scheme host
    
    let setJsonHeader = Writers.setHeader "Content-Type" "application/hal+json"

    let setCorsHaeders = 
        Writers.setHeader "Access-Control-Allow-Origin" "*" 
        >=> Writers.setHeader "Access-Control-Allow-Headers" "content-type" 
        >=> Writers.setHeader "Access-Control-Expose-Headers" "content-type, location" 
        >=> Writers.setHeader "Access-Control-Allow-Methods" "POST, GET, OPTIONS, DELETE, PATCH"

    let setHeaders = setJsonHeader >=> setCorsHaeders

    choice [
        GET >=> choice [
            path ("/" </> Paths.api) >=> request (urlWithHost >> fun host ->
                { Resource.empty with links = Map.ofList [ "self", Singleton (Link.create (Uri (host </> Paths.api)))
                                                           gamesRel host, Singleton (Link.create (Uri (host </> Paths.games)))  ] }
                |> FSharpDataIntepreter.Hal.toJson |> jsonToString |> OK)
                >=> setHeaders
            path ("/" </> Paths.games) >=> request (urlWithHost >> games interpret playersMapActor) >=> setHeaders
            pathScan Routes.game (fun gameId ->
                request (urlWithHost >> game interpret playersMapActor gameId)) >=> setHeaders
            path ("/" </> Paths.rels GAMES) >=> OK (File.ReadAllText !!"../public/rels/games.html") >=> setCorsHaeders
            path ("/" </> Paths.rels NEWGAME) >=> OK (File.ReadAllText !!"../public/rels/newgame.html") >=> setCorsHaeders
            path ("/" </> Paths.rels JOIN) >=> OK (File.ReadAllText !!"../public/rels/join.html") >=> setCorsHaeders            
            path ("/" </> Paths.rels PLAY) >=> OK (File.ReadAllText !!"../public/rels/play.html") >=> setCorsHaeders
        ]
        POST >=> choice [
            path ("/" </> Paths.games) >=> request (urlWithHost >> start interpret playersMapActor) >=> setHeaders
            pathScan Routes.join (fun gameId ->
                request (urlWithHost >> join playersMapActor gameId)) >=> setHeaders
            pathScan Routes.moves (fun gameId -> 
                request (fun req -> play interpret playersMapActor gameId (getPlay req) (urlWithHost req)))
                >=> setHeaders
        ]
    ]


let config =
    let ip = "0.0.0.0"
    let port =
        match fsi.CommandLineArgs with
        | [|_; port|] -> port
        | _           -> "8080"
    ip + ":" + string port

let buildWebHost args =
    Microsoft.AspNetCore.WebHost.CreateDefaultBuilder(args)
        .Configure(fun (b: IApplicationBuilder) -> HttpAdapter.appMap "" (appRun app) b |> ignore)
        .UseUrls($"http://{config}/")
        .Build ()

interpret (ReadModel.subscribe ())

buildWebHost([||]).Run ()
