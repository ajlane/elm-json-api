module Api.ArticleUrl exposing (..)

import Api.Article
import Api.UpdateArticleResponse
import Api.UpdateDraft
import Dict
import Http
import Json.Decode
import Json.Encode
import Url.Interpolate


{-| A link to the content of an article.


-}
type alias ArticleUrl =
    String


{-| Converts the given ArticleUrl into a URL string.


-}
toUrl : ArticleUrl -> String
toUrl url =
    Url.Interpolate.interpolate url (Dict.fromList [])


get :
    (Api.Article.Article -> msg)
    -> (Http.Error -> msg)
    -> Maybe { a
        | headers : List Http.Header
        , timeout : Maybe Float
        , tracker : Maybe String
    }
    -> ArticleUrl
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
        , expect = Http.expectJson msgOrOtherwise Api.Article.decoder
        , timeout = meta |> Maybe.andThen (\m -> m.timeout)
        , tracker = meta |> Maybe.andThen (\m -> m.tracker)
        }


put :
    (Api.UpdateArticleResponse.UpdateArticleResponse -> msg)
    -> (Http.Error -> msg)
    -> Maybe { a
        | headers : List Http.Header
        , timeout : Maybe Float
        , tracker : Maybe String
    }
    -> Api.UpdateDraft.UpdateDraft
    -> ArticleUrl
    -> Cmd msg
put msg otherwise meta body url =
    let
        msgOrOtherwise result =
            case result of
                Ok value ->
                    msg value

                Err err ->
                    otherwise err
    in
    Http.request
        { method = "PUT"
        , headers = meta |> Maybe.map (\m -> m.headers) |> Maybe.withDefault []
        , url = toUrl url
        , body = Http.jsonBody (body |> Api.UpdateDraft.encode)
        , expect =
            Http.expectJson msgOrOtherwise Api.UpdateArticleResponse.decoder
        , timeout = meta |> Maybe.andThen (\m -> m.timeout)
        , tracker = meta |> Maybe.andThen (\m -> m.tracker)
        }


delete :
    (String -> msg)
    -> (Http.Error -> msg)
    -> Maybe { a
        | headers : List Http.Header
        , timeout : Maybe Float
        , tracker : Maybe String
    }
    -> ArticleUrl
    -> Cmd msg
delete msg otherwise meta url =
    let
        msgOrOtherwise result =
            case result of
                Ok value ->
                    msg value

                Err err ->
                    otherwise err
    in
    Http.request
        { method = "DELETE"
        , headers = meta |> Maybe.map (\m -> m.headers) |> Maybe.withDefault []
        , url = toUrl url
        , body = Http.emptyBody
        , expect = Http.expectJson msgOrOtherwise Json.Decode.string
        , timeout = meta |> Maybe.andThen (\m -> m.timeout)
        , tracker = meta |> Maybe.andThen (\m -> m.tracker)
        }


{-| Encodes ArticleUrl values as JSON.


-}
encode : ArticleUrl -> Json.Encode.Value
encode =
    Json.Encode.string


{-| Decodes JSON as a ArticleUrl value.


-}
decoder : Json.Decode.Decoder ArticleUrl
decoder =
    Json.Decode.string
