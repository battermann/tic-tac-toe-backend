module HalSharp

#I "../packages"
#r "FParsec/lib/net40-client/FParsec.dll"
#r "FParsec/lib/net40-client/FParsecCS.dll"
#r "Aether/lib/net35/Aether.dll"
#r "Chiron/lib/net40/Chiron.dll"
#r "System.Runtime.Serialization"

open System
open Chiron

type Link = {
    href: string
    templated: bool option
    mediaType: string option
    deprication: Uri option
    name: string option
    profile: Uri option
    title: string option
    hreflang: string option
}

type Resource = {
    links: Map<string, Link list>
    embedded: Map<string, Resource list>
    properties: Map<string, Json>
}

[<RequireQualifiedAccess>]
module internal Link =
    let simple href = {
        href = href
        templated = None
        mediaType = None
        deprication = None
        name = None
        profile = None
        title = None
        hreflang = None 
    }

    let singleLinkToJson link : Json =
        Object <| Map.ofList
            [ yield ("href", String link.href)
              yield! match link.templated with Some b -> [ "templated", Bool b ] | _ -> []
              yield! match link.mediaType with Some mt -> [ "type", String mt ] | _ -> []
              yield! match link.deprication with Some dep -> [ "deprication", String (dep.ToString()) ] | _ -> []
              yield! match link.name with Some name -> [ "name", String name ] | _ -> []
              yield! match link.profile with Some prof -> [ "profile", String (prof.ToString()) ] | _ -> []
              yield! match link.title with Some title -> [ "title", String title ] | _ -> []
              yield! match link.hreflang with Some lang -> [ "hreflang", String lang ] | _ -> [] ]

    let toJson (links: Map<string, Link list>) : Json option =
        let linkListToJson links =
            match links |> List.length with
            | 1 -> singleLinkToJson (links |> List.head)
            | _ -> Array (links |> List.map singleLinkToJson)

        let nonEmptyLinks = links |> Map.filter (fun _ linkList -> not (List.isEmpty linkList))

        if nonEmptyLinks |> Map.isEmpty then
            None
        else
            nonEmptyLinks
            |> Map.map (fun _ linkList -> linkListToJson linkList)
            |> Object
            |> Some

[<RequireQualifiedAccess>]
module Resource =
    let empty = {
        links = Map.empty
        embedded = Map.empty
        properties = Map.empty
    }
    let rec toJson resource : Json =
        let merge (maps: Map<'a,'b> seq): Map<'a,'b> = 
            Map.ofList <| List.concat (maps |> Seq.map Map.toList)
        
        let embedded =
            let serializeEmbedded resources =
                match resources |> List.length with
                | 0 -> Object <| Map.empty
                | 1 -> toJson (resources |> List.head)
                | _ -> Array (resources |> List.map toJson)

            let embeddedMap = 
                resource.embedded
                |> Map.map (fun rel res -> serializeEmbedded res)
                
            if embeddedMap |> Map.isEmpty then
                Map.empty
            else
                Map.ofList [ "_embedded", embeddedMap |> Object ]

        let links = 
            match Link.toJson resource.links with
            | Some ls -> Map.ofList [ "_links", ls ]
            | _       -> Map.empty

        [ links; resource.properties; embedded ]
        |> merge
        |> Object