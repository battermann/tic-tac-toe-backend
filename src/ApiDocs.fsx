module ApiDocs

type Request = {
    url: string
    httpMethod: string
    urlParams: string
    contentType: string
    dataParams: string
    example: string  
}

type Response = {
    description: string
    contentType: string
    example: string
}

type DocModel = {
    title: string
    description: string
    request: Request
    response: Response
    success: string
    error: string
}

let join = {
    title = "Join a new game"
    description = "Join a game that has been started by someone else. The player that joins will be player O. Games can only be joined if no one else has joined them before."
    request = 
        {
            url = "/games/{id}/join"
            httpMethod = "POST"
            urlParams = "none"
            contentType = "any"
            dataParams = "none"
            example = ""
        }
    response = 
        {
            description = "The response contains the player ID for player O. In subsequent request on the game (to make plays) this ID has to be transmitted in the request body. The URL of the game can be found in the Location Header."
            contentType = "application/hal+json"
            example = """Location http://localhost:8000/games/94a26c4a-75f7-45b2-8e20-a39f0f8818e8

{ 
    "playerId": "6acd905b-044e-47f8-9692-4b68d0c6ee07" 
}"""
        }
    success = "202 Accpeted"
    error = "409 Conflict, 404 Not Found, 500 Internal Server Error"
}

let games = {
    title = "Show all games"
    description = "A list of all the games."
    request = 
        {
            url = "/games"
            httpMethod = "GET"
            urlParams = "none"
            contentType = "any"
            dataParams = "none"
            example = ""
        }
    response = 
        {
            description = "The response contains a list of games."
            contentType = "application/hal+json"
            example = """{
   "_embedded":{
      "http://localhost:8000/rels/games":[
         {
            "_links":{
               "http://localhost:8000/rels/join":{
                  "href":"http://localhost:8000/games/06cee5ca-3ba5-4c9c-8d00-ac51e0d0f8c8/join"
               },
               "self":{
                  "href":"http://localhost:8000/games/06cee5ca-3ba5-4c9c-8d00-ac51e0d0f8c8"
               }
            },
            "id":"06cee5ca-3ba5-4c9c-8d00-ac51e0d0f8c8",
            "status":"running"
         },
         {
            "_links":{
               "http://localhost:8000/rels/join":{
                  "href":"http://localhost:8000/games/960005e1-0904-4eb9-850b-fbdcbd263e5e/join"
               },
               "self":{
                  "href":"http://localhost:8000/games/960005e1-0904-4eb9-850b-fbdcbd263e5e"
               }
            },
            "id":"960005e1-0904-4eb9-850b-fbdcbd263e5e",
            "status":"running"
         }
      ]
   },
   "_links":{
      "http://localhost:8000/rels/newgame":{
         "href":"http://localhost:8000/games"
      },
      "self":{
         "href":"http://localhost:8000/games"
      }
   }
}"""
        }
    success = "202 Accpeted"
    error = "500 Interal Server Error"
}

let newGame = {
    title = "Start a new game"
    description = "Start a new game. The game will be created and appended to the game list. The game is now open for player O to join."
    request = 
        {
            url = "/games"
            httpMethod = "POST"
            urlParams = "none"
            contentType = "any"
            dataParams = "none"
            example = ""
        }
    response = 
        {
            description = "The response contains the player ID for player X. In subsequent request on the game (to make plays) this ID has to be transmitted in the request body. The request is processed asynchronously. Therefore the response code is 202 Accepted. The URL of the new game is provided by the Location Header."
            contentType = "application/hal+json"
            example = """Location http://localhost:8000/games/3f81eac5-26fc-4242-9444-126f785c43f6

{ 
    "playerId": "6acd905b-044e-47f8-9692-4b68d0c6ee07" 
}"""
        }
    success = "202 Accpeted"
    error = ""
}

let play = {
    title = "Make a play"
    description = """This is the relation for making a play. The request has to provide the player ID and a description of the move in the request body. The move has a vertical and a horizontal position. Valid vertical positions are: "top", "vcenter", "bottom". Valid horizontal positions are: "left", "hcenter", "right"."""
    request = 
        {
            url = "/games/{id}/moves"
            httpMethod = "POST"
            urlParams = "none"
            contentType = "application/json"
            dataParams = "vertical : string, horizontal : string, playerId : string"
            example = """{
    "vertical": "top",
    "horizontal": "left",
    "playerId": "47769197-0ca6-414c-a018-610896eb645b"
}"""
        }
    response = 
        {
            description = "The request will be processed asynchronously. Therefore the response is empty. To get the current state of the game a new request to the game URL has to made."
            contentType = ""
            example = ""
        }
    success = "202 Accpeted"
    error = "409 Conflict, 404 Not Found, 400 Bad Request"
}