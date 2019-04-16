port module Settings exposing
    ( Person
    , Settings
    , decoder
    , settingsUpdated
    )

import Json.Decode as Json


port settingsUpdated : Settings -> Cmd msg


type alias Person =
    { name : String
    , contribution : Int
    }


type alias Settings =
    { personOne : Person, personTwo : Person }


decoder : Json.Decoder Settings
decoder =
    Json.map2 Settings
        (Json.field "personOne" personDecoder)
        (Json.field "personTwo" personDecoder)


personDecoder : Json.Decoder Person
personDecoder =
    Json.map2 Person
        (Json.field "name" Json.string)
        (Json.field "contribution" Json.int)
