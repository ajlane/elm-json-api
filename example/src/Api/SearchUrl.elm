module Api.SearchUrl exposing (..)

import Api.SearchResponse
import Dict
import Http
import Json.Decode
import Json.Encode
import Url.Interpolate


{-| A link to the results of a search.


-}
type alias SearchUrl =
    String


{-| Template parameters for interpolating a SearchUrl


-}
type alias SearchUrlParams =
    { after : String, q : String, size : String }


{-| Converts the given SearchUrl into a URL string.


-}
toUrl : SearchUrlParams -> SearchUrl -> String
toUrl { after, q, size } template =
    Url.Interpolate.interpolate
        template
        (Dict.fromList [ ( "after", after ), ( "q", q ), ( "size", size ) ])


get :
    (Api.SearchResponse.SearchResponse -> msg)
    -> (Http.Error -> msg)
    -> Maybe { a
        | headers : List Http.Header
        , timeout : Maybe Float
        , tracker : Maybe String
    }
    -> SearchUrlParams
    -> SearchUrl
    -> Cmd msg
get msg otherwise meta { after, q, size } template =
    let
        msgOrOtherwise result =
            case result of
                Ok value ->
                    msg value

                Err err ->
                    otherwise err
    in
    Http.request
        { method = "GET"
        , headers = meta |> Maybe.map (\m -> m.headers) |> Maybe.withDefault []
        , url = toUrl { after = after, q = q, size = size } template
        , body = Http.emptyBody
        , expect = Http.expectJson msgOrOtherwise Api.SearchResponse.decoder
        , timeout = meta |> Maybe.andThen (\m -> m.timeout)
        , tracker = meta |> Maybe.andThen (\m -> m.tracker)
        }


{-| Encodes SearchUrl values as JSON.


-}
encode : SearchUrl -> Json.Encode.Value
encode =
    Json.Encode.string


{-| Decodes JSON as a SearchUrl value.


-}
decoder : Json.Decode.Decoder SearchUrl
decoder =
    Json.Decode.string
