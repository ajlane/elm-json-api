port module Output exposing (..)

import Elm.Pretty exposing (pretty)
import Json.Decode
import Json.Encode


type Output
    = Generated (List { path : String, content : String })
    | InputError Json.Decode.Error


port output : Json.Encode.Value -> Cmd msg


sendOutput : Output -> Cmd msg
sendOutput value =
    output (encodeOutput value)


encodeOutput : Output -> Json.Encode.Value
encodeOutput value =
    case value of
        Generated files ->
            let
                encodeFile file =
                    Json.Encode.object
                        [ ( "path", Json.Encode.string file.path )
                        , ( "content", Json.Encode.string file.content )
                        ]
            in
            Json.Encode.list encodeFile files

        InputError err ->
            Json.Encode.object
                [ ( "error", Json.Encode.string (Json.Decode.errorToString err) )
                ]
