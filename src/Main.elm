module Main exposing (main)

import Elm.Pretty exposing (pretty)
import Generator exposing (specFiles)
import Input exposing (Input(..), decodeInput, subscribeToInput)
import Json.Decode
import Output exposing (Output(..), sendOutput)


init : Json.Decode.Value -> ( (), Cmd Input )
init flags =
    ( (), Cmd.none )


update : Input -> () -> ( (), Cmd Input )
update msg model =
    case msg of
        Generate namespace apiSpec ->
            let
                files =
                    specFiles namespace apiSpec
                        |> List.map (\{ path, content } -> { path = path, content = content |> pretty 80 })
            in
            ( (), sendOutput (Generated files) )

        BadInput err ->
            ( (), sendOutput (InputError err) )


subscriptions : () -> Sub Input
subscriptions model =
    subscribeToInput identity


main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
