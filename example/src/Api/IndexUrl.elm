module Api.IndexUrl exposing (..)

import Api.Index
import Api.NewArticleResponse
import Api.NewDraft
import Dict
import Http
import Json.Decode
import Json.Encode
import Url.Interpolate


{-| A link to the API entry-point.


-}
type alias IndexUrl =
    String


{-| Converts the given IndexUrl into a URL string.


-}
toUrl : IndexUrl -> String
toUrl url =
    Url.Interpolate.interpolate url (Dict.fromList [])


get :
    (Api.Index.Index -> msg)
    -> (Http.Error -> msg)
    -> Maybe { a
        | headers : List Http.Header
        , timeout : Maybe Float
        , tracker : Maybe String
    }
    -> IndexUrl
    -> Cmd msg
get msg otherwise meta url =
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
        , url = toUrl url
        , body = Http.emptyBody
        , expect = Http.expectJson msgOrOtherwise Api.Index.decoder
        , timeout = meta |> Maybe.andThen (\m -> m.timeout)
        , tracker = meta |> Maybe.andThen (\m -> m.tracker)
        }


post :
    (Api.NewArticleResponse.NewArticleResponse -> msg)
    -> (Http.Error -> msg)
    -> Maybe { a
        | headers : List Http.Header
        , timeout : Maybe Float
        , tracker : Maybe String
    }
    -> Api.NewDraft.NewDraft
    -> IndexUrl
    -> Cmd msg
post msg otherwise meta body url =
    let
        msgOrOtherwise result =
            case result of
                Ok value ->
                    msg value

                Err err ->
                    otherwise err
    in
    Http.request
        { method = "POST"
        , headers = meta |> Maybe.map (\m -> m.headers) |> Maybe.withDefault []
        , url = toUrl url
        , body = Http.jsonBody (body |> Api.NewDraft.encode)
        , expect = Http.expectJson msgOrOtherwise Api.NewArticleResponse.decoder
        , timeout = meta |> Maybe.andThen (\m -> m.timeout)
        , tracker = meta |> Maybe.andThen (\m -> m.tracker)
        }


{-| Encodes IndexUrl values as JSON.


-}
encode : IndexUrl -> Json.Encode.Value
encode =
    Json.Encode.string


{-| Decodes JSON as a IndexUrl value.


-}
decoder : Json.Decode.Decoder IndexUrl
decoder =
    Json.Decode.string
