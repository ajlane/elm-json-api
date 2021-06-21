port module Input exposing (..)

import Json.Decode
import Spec exposing (Spec, specDecoder)


type alias Namespace =
    List String


type Input
    = Generate Namespace Spec
    | BadInput Json.Decode.Error


port input : (Json.Decode.Value -> msg) -> Sub msg


subscribeToInput : (Input -> msg) -> Sub msg
subscribeToInput toMsg =
    input (decodeInput >> toMsg)


decodeInput : Json.Decode.Value -> Input
decodeInput value =
    let
        decoder =
            Json.Decode.map2 Generate
                (Json.Decode.field "namespace" Json.Decode.string |> Json.Decode.map (String.split "."))
                (Json.Decode.field "spec" specDecoder)
    in
    case Json.Decode.decodeValue decoder value of
        Ok generate ->
            generate

        Err err ->
            BadInput err
