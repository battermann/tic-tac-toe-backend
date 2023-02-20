module Hypermedia

#r "nuget: FSharp.Data,5.0.2"

let merge (maps: Map<_,_> seq): Map<_,_> = 
    List.concat (maps |> Seq.map Map.toList) |> Map.ofList

/// Represents a minimal generic Json object to describe a hypermedia resource.
type JsonModel<'a> =
    | JObject of 'a
    | JRecord of Map<string, JsonModel<'a>>
    | JString of string
    | JArray of JsonModel<'a> list
    | JBool of bool

/// Define Hypertext Application Language (HAL) resources.
/// Transform HAL resource to specific Json implementations.
module Hal =

    // todo ensure that _links and _embedded are unique

    open System
    open System.Reflection
    open Microsoft.FSharp.Reflection

    /// A link representation according to the HAL specification (https://tools.ietf.org/html/draft-kelly-json-hal-08).
    type Link = {
        href: Uri
        templated: bool option
        mediaType: string option
        deprication: Uri option
        name: string option
        profile: Uri option
        title: string option
        hreflang: string option
    }

    type Curies = Map<string, Uri>

    type MaybeSingleton<'a> =
        | Singleton of 'a
        | Collection of 'a list

    module MaybeSingleton =
        let map f maybeSingleton =
            match maybeSingleton with
            | Singleton x   -> Singleton (f x)
            | Collection xs -> Collection (xs |> List.map f)

    /// A resource representation according to the HAL specification (https://tools.ietf.org/html/draft-kelly-json-hal-08).
    type Resource<'a> = {
        curies: Curies
        links: Map<string, MaybeSingleton<Link>>
        embedded: Map<string, MaybeSingleton<Resource<'a>>>
        properties: Map<string, JsonModel<'a>>
        payload: obj option
    }

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal Curies =
        let internal tryFindName (curies: Curies) (relation: string): string option =
            let tryCreateUri str =
                match Uri.TryCreate(str, UriKind.Absolute) with
                | true, uri -> Some uri
                | _         -> None

            let matchUriWithTemplate (name: string, template: Uri) (uri: Uri): string option =
                if template.Segments |> Array.length <> (uri.Segments |> Array.length) then 
                    None
                else
                    Array.zip template.Segments uri.Segments
                    |> Array.tryFind (fst >> ((=) "%7Brel%7D"))
                    |> Option.map (snd >> sprintf "%s:%s" name)
            
            curies 
            |> Map.toSeq 
            |> Seq.map matchUriWithTemplate 
            |> Seq.choose (fun tryMatch -> tryCreateUri relation |> Option.bind tryMatch)
            |> Seq.tryHead

        let internal replace (curies: Curies) (map: Map<string, MaybeSingleton<_>>) =
            map
            |> Map.toList
            |> List.map (fun (relation, x) ->
                match tryFindName curies relation with
                | Some name -> name, x
                | _         -> relation, x)
            |> Map.ofList    

        let rec replaceRelations curies resource =
            { resource with 
                links = replace curies resource.links
                embedded = (replace curies resource.embedded) |> Map.map (fun _ emb -> emb |> MaybeSingleton.map (replaceRelations curies))
            }

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Link =
        let create href = {
            href = href
            templated = None
            mediaType = None
            deprication = None
            name = None
            profile = None
            title = None
            hreflang = None 
        }

        let internal serializeSingleLink (link: Link) : JsonModel<'a> =
            Map.ofList
                [ yield ("href", JString (string link.href))
                  yield! match link.templated with Some b -> [ "templated", JBool b ] | _ -> []
                  yield! match link.mediaType with Some mt -> [ "type", JString mt ] | _ -> []
                  yield! match link.deprication with Some dep -> [ "deprication", JString (dep.ToString()) ] | _ -> []
                  yield! match link.name with Some name -> [ "name", JString name ] | _ -> []
                  yield! match link.profile with Some prof -> [ "profile", JString (prof.ToString()) ] | _ -> []
                  yield! match link.title with Some title -> [ "title", JString title ] | _ -> []
                  yield! match link.hreflang with Some lang -> [ "hreflang", JString lang ] | _ -> [] ]
                |> JRecord

        let internal serialize (links: Map<string, MaybeSingleton<Link>>) (curies: Curies) : JsonModel<'a> option =

            let linksWithCuries =
                curies 
                |> Map.toList 
                |> List.map (fun (name, href) -> { create href with name = Some name; templated = Some true })
                |> fun x -> "curies", Collection x
                |> links.Add

            let serializeLinkList = function
                | Singleton l   -> serializeSingleLink l
                | Collection ls -> JArray (ls |> List.map serializeSingleLink)

            let nonEmptyLinks = 
                linksWithCuries 
                |> Map.filter (fun _ link -> 
                    match link with 
                    | Singleton _   -> true
                    | Collection xs -> not (xs |> List.isEmpty))

            if nonEmptyLinks |> Map.isEmpty then
                None
            else
                nonEmptyLinks
                |> Map.map (fun _ l -> serializeLinkList l)
                |> JRecord
                |> Some

    /// Contains functions to transform resources to Json representations.
    [<RequireQualifiedAccess>]
    module Resource =

        /// Returns an empty resource object the represents a valid HAL resource.
        let empty = {
            curies = Map.empty
            links = Map.empty
            embedded = Map.empty
            properties = Map.empty
            payload = None
        }

        let internal tryToMap(x: 'T) =
            let objectToMap(obj: obj) =
                obj.GetType().GetProperties(BindingFlags.DeclaredOnly ||| BindingFlags.Public ||| BindingFlags.Instance) 
                |> Array.map (fun prop -> prop.Name, prop.GetValue(obj, null))
                |> Map.ofArray

            let recordToMap (record:'T) = 
                [ for p in FSharpType.GetRecordFields(typeof<'T>) ->
                    p.Name, p.GetValue(record) ]
                |> Map.ofSeq

            try
                if FSharp.Reflection.FSharpType.IsRecord(typeof<'T>) then
                    x |> recordToMap
                else
                    x |> objectToMap
            with
            | _ -> Map.empty    

        let rec internal serialize (resource: Resource<'a>) : JsonModel<'a> =
            
            let withResolvedCuries = Curies.replaceRelations resource.curies resource
                
            let embedded =        
                let embeddedMap = 
                    withResolvedCuries.embedded
                    |> Map.map (fun rel res -> 
                        match res with
                        | Singleton x   -> serialize x
                        | Collection xs -> JArray (xs |> List.map serialize))
                    
                if embeddedMap |> Map.isEmpty then
                    Map.empty
                else
                    Map.ofList [ "_embedded", embeddedMap |> JRecord ]

            let links = 
                match Link.serialize withResolvedCuries.links resource.curies with
                | Some ls -> Map.ofList [ "_links", ls ]
                | _       -> Map.empty

            let properties =
                match resource.payload with
                | Some pl ->
                    let payload =
                        try
                            pl |> tryToMap |> Map.map (fun _ v -> JObject (v :?> 'a))
                        with
                        | _ -> Map.empty
                    merge [ resource.properties; payload ]
                | _ -> resource.properties

            [ links; properties; embedded ]
            |> merge
            |> JRecord

        /// Serializes a HAL resource representation to a specific Json representation.
        /// The interpreter transforms the generic Json representation to a specific representation.
        let toJson interpreter resource =
            resource |> serialize |> interpreter

        let withPayload (payload: obj) resource : Resource<obj> =
            { resource with payload = payload |> Some }

        let withLinks links resource =
            { resource with links = Map.ofList links }
        let addLink rel link resource =
            { resource with links = resource.links.Add(rel, Singleton link) }

        let addLinkCollection rel link resource =
            { resource with links = resource.links.Add(rel, Collection link) }             

        let addEmbedded name embedded resource =
            { resource with embedded = resource.embedded.Add(name, Singleton embedded) }

        let addEmbeddedCollection name embedded resource =
            { resource with embedded = resource.embedded.Add(name, Collection embedded) }        

        let withCuries curies resource =
            { resource with curies = Map.ofList curies }

        let withProperties props resource = 
            { resource with properties = props |> Map.ofList |> Map.map (fun _ x -> JObject x) }

        let addProperty name prop resource = 
            { resource with properties = resource.properties.Add(name, JObject prop) }

module Siren =

    open System

    type Rel = Rel of string
    type Title = Title of string
    type Class = Class of string
    type MediaType = MediaType of string
    type Href = Href of Uri
    type Name = Name of string
    type Value = Value of string

    type HttpMethod = GET | PUT | POST | DELETE | PATCH

    type InputType = 
        | Hidden | Text | Search | Tel | Url | Email | Password
        | Datetime | Date | Month | Week | Time | DatetimeLocal | Number
        | Range | Color | Checkbox | Radio | File

    type Field = {
        classes: Class list
        inputType: InputType option
        value: Value option
        title: Title option
    }

    type Action = {
        classes: Class list
        httpMethod: HttpMethod option
        href: Href
        title: Title option
        mediaType: MediaType option
        fields: Map<Name, Field>
    }

    type Link = {
        href: Href
        rel: Rel * Rel list
        classes: Class list
        title: Title option
        mediaType: MediaType option
    }

    type Entity<'a> = {
        properties: Map<Name, JsonModel<'a>>
        entities: SubEntity<'a> list
        actions: Map<Name, Action>
        links: Link list
        classes: Class list
        title: Title option
    }

    and SubEntity<'a> =
        | EmbeddedRepresentation of Entity<'a> * Rel
        | EmbeddedLink of Link

    let mkProperty name items =
        if items |> List.isEmpty then
            List.empty
        else
            [ name, JArray items ]

    [<AutoOpen>]
    module internal Attributes =
        let CLASS = "class"
        let HREF = "href"
        let TITLE = "title"
        let TYPE = "type"
        let FIELDS = "fields"
        let NAME = "name"
        let REL = "rel"
        let LINKS = "links"
        let VALUE = "value"
        let METHOD = "method"
        let ACTIONS = "actions"
        let ENTITIES = "entities"
        let PROPERTIES = "properties"

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal InputType =
        let serialize inputType =
            match inputType with
            | Hidden -> "hidden"
            | Text -> "text"
            | Search -> "search"
            | Tel -> "tel"
            | Url -> "url"
            | Email -> "email"
            | Password -> "password"
            | Datetime -> "datetime"
            | Date -> "date"
            | Month -> "month"
            | Week -> "week"
            | Time -> "time"
            | DatetimeLocal -> "datetime-local"
            | Number-> "number"
            | Range -> "range"
            | Color -> "color"
            | Checkbox -> "checkbox"
            | Radio -> "radio"
            | File-> "file"

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal HttpMethod =
        let serialize httpMethod =
            match httpMethod with
            | GET -> "GET"
            | PUT -> "PUT"
            | POST -> "POST"
            | DELETE -> "DELETE"
            | PATCH -> "PATCH"

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal Field =
        let empty : Field = {
            classes = List.empty
            inputType = None
            value = None
            title = None
        }

        let internal serialize name (field: Field) =
            field.classes |> List.map (fun (Class c) -> JString c) |> mkProperty CLASS
            |> fun props ->
                let inputType = 
                    match field.inputType with Some t -> t | _ -> InputType.Text
                    |> InputType.serialize
                    |> fun x -> TYPE, JString x
                inputType :: props
            |> fun props -> 
                match field.value with Some (Value v) -> (VALUE, JString v) :: props | _ -> props
            |> fun props ->
                match field.title with Some (Title t) -> (TITLE, JString t) :: props | _ -> props
            |> fun props -> (NAME, JString name) :: props
            |> Map.ofList
            |> JRecord


    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal Action =
        let create href : Action = {
            classes = List.empty
            httpMethod = None
            href = href
            title = None
            mediaType = None
            fields = Map.empty
        }

        let serialize name (action: Action) =
            action.classes |> List.map (fun (Class c) -> JString c) |> mkProperty CLASS
            |> fun props ->
                let httpMethod =
                    match action.httpMethod with Some m -> m | _ -> HttpMethod.GET
                    |> HttpMethod.serialize
                    |> fun x -> METHOD, JString x
                httpMethod :: props
            |> fun props -> 
                let (Href href) = action.href
                (HREF, JString (href.ToString())) :: props
            |> fun props ->
                match action.title with Some (Title t) -> (TITLE, JString t) :: props | _ -> props
            |> fun props ->
                let mediaType =
                    match action.mediaType with Some (MediaType mt) -> mt | _ -> "application/x-www-form-urlencoded"
                    |> fun x -> TYPE, JString x
                mediaType :: props
            |> fun props ->
                List.concat [ props; action.fields |> Map.toList |>  List.map (fun (Name n, f) -> f |> Field.serialize n) |> mkProperty FIELDS ]
            |> fun props -> (NAME, JString name) :: props
            |> Map.ofList
            |> JRecord

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module Link =

        let create rel href : Link = {
            href = href
            rel = rel, []
            classes = List.empty
            title = None
            mediaType = None
        }

        let internal serialize (link: Link) =
            link.classes |> List.map (fun (Class c) -> JString c) |> mkProperty CLASS
            |> fun props -> 
                let (Href href) = link.href
                (HREF, JString (href.ToString())) :: props
            |> fun props ->
                match link.title with Some (Title t) -> (TITLE, JString t) :: props | _ -> props
            |> fun props ->
                match link.mediaType with Some (MediaType mt) -> (TYPE, JString mt) :: props | _ -> props
            |> fun props ->
                List.concat [ props; fst link.rel :: snd link.rel |> List.map (fun (Rel rel) -> JString rel) |> mkProperty REL ]
            |> Map.ofList
            |> JRecord

        let withClasses classes link : Link =
            { link with classes = classes |> List.map Class }

    [<RequireQualifiedAccess>]
    [<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module internal Entity =

        let empty : Entity<'a> = {
            properties = Map.empty
            entities = List.empty
            actions = Map.empty
            links = List.empty
            classes = List.empty
            title = None
        }

        let rec serializeRec (rel: Rel option) (entity: Entity<'a>) =
            entity.classes |> List.map (fun (Class c) -> JString c) |> mkProperty CLASS
            |> fun props ->
                match entity.title with Some (Title t) -> (TITLE, JString t) :: props | _ -> props
            |> fun props ->
                match rel with Some (Rel r) -> (REL, JArray [ JString r ]) :: props | _ -> props
            |> fun props ->
                if entity.properties |> (not << Map.isEmpty) then   
                    let properties = (PROPERTIES, entity.properties |> Map.toList |> List.map (fun (Name n, v) -> n,v) |> Map.ofList |> JRecord)
                    properties :: props
                else
                    props
            |> fun props ->
                let embedded = 
                    entity.entities 
                    |> List.map 
                        (function
                        | EmbeddedRepresentation (e,r) -> serializeRec (Some r) e
                        | EmbeddedLink link            -> link |> Link.serialize)
                    |> mkProperty ENTITIES
                [ embedded; props ]
            |> fun propss ->
                (entity.links |> List.map Link.serialize |> mkProperty LINKS) :: propss
            |> fun propss ->
                List.concat ((entity.actions |> Map.toList |> List.map (fun (Name n, v) -> Action.serialize n v) |> mkProperty ACTIONS) :: propss)
            |> Map.ofList
            |> JRecord
        
        let withClasses classes entity : Entity<_> =
            { entity with classes = classes |> List.map Class }

        let addProperty name prop entity: Entity<'a> = 
            { entity with properties = entity.properties.Add(Name name, JObject prop) }  

        let withLinks links entity: Entity<'a> =
            { entity with links = links }

        let addEmbeddedLink link entity =
            { entity with entities = EmbeddedLink link :: entity.entities }

        let addEmbeddedEntity embedded rel entity =
            { entity with entities = EmbeddedRepresentation (embedded, Rel rel) :: entity.entities }   

        let withActions actions entity =
            { entity with actions = actions |> List.map (fun (k,v) -> Name k, v) |> Map.ofList }                

        let serialize entity = serializeRec None entity

        let toJson interpreter entity =
            entity |> serialize |> interpreter        
            
/// Contains the interpreter to transform an `JsonModel<obj>` into an `obj`.
module ObjectInterpreter =

    /// Transforms an `JsonModel<obj>` into an `obj`.
    let rec interpret (instance: JsonModel<obj>) : obj =
        match instance with
        | JObject a     -> a
        | JBool b       -> b :> obj
        | JString s     -> s :> obj
        | JRecord map   -> map |> Map.map (fun _ v -> interpret v) :> obj
        | JArray a      -> a |> List.map interpret :> obj

    [<RequireQualifiedAccess>]
    module Siren =
        let toJson entity = Siren.Entity.toJson interpret entity

    [<RequireQualifiedAccess>]
    module Hal =
        /// Serializes a HAL resource as `obj`
        let toJson resource = Hal.Resource.toJson interpret resource    

module FSharpDataIntepreter =

    open FSharp.Data

    /// Transforms an `JsonModel<FSharp.Data.JsonValue>` into a `FSharp.Data.JsonValue`.
    let rec internal interpret (instance: JsonModel<JsonValue>) : JsonValue =
        match instance with
        | JObject a     -> a
        | JBool b       -> JsonValue.Boolean b
        | JString s     -> JsonValue.String s
        | JRecord map   -> JsonValue.Record (map |> Map.toArray |> Array.map (fun (k,v) -> k, interpret v))
        | JArray a      -> JsonValue.Array (a |> List.map interpret |> List.toArray)

    [<RequireQualifiedAccess>]
    module Hal =
        /// Serializes a HAL resource as `FSharp.Data.JsonValue`
        let toJson resource = Hal.Resource.toJson interpret resource

    [<RequireQualifiedAccess>]
    module Siren =
        let toJSon entity = Siren.Entity.toJson interpret entity        