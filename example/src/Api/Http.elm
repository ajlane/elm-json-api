module Api.Http exposing (..)

import Api.Decode
import Api.Encode
import Api.Model
import Dict
import Http
import Json.Decode
import Json.Encode
import Task
import Time
import Url
import Url.Interpolate


searchUrlToString { after, q, size } (Api.Model.SearchUrl template) =
    Url.Interpolate.interpolate
        template
        (Dict.fromList
            ([ after |> Maybe.map (\p -> ( "after", p ))
             , Just ( "q", q )
             , size |> Maybe.map (\p -> ( "size", p ))
             ]
                |> List.filterMap identity
            )
        )


getSearchUrl params ok err url =
    { method = "GET"
    , url = searchUrlToString params url
    , body = Http.emptyBody
    , expect = expectSearchResponse (reduceResponse ok err)
    , headers = [ Http.header "Accept" "application/json" ]
    , timeout = Nothing
    , tracker = Nothing
    }


indexUrlToString (Api.Model.IndexUrl url) =
    Url.Interpolate.interpolate url Dict.empty


getIndexUrl ok err url =
    { method = "GET"
    , url = indexUrlToString url
    , body = Http.emptyBody
    , expect = expectIndex (reduceResponse ok err)
    , headers = [ Http.header "Accept" "application/json" ]
    , timeout = Nothing
    , tracker = Nothing
    }


postIndexUrl body ok err url =
    { method = "POST"
    , url = indexUrlToString url
    , body = body |> newDraftBody
    , expect = expectNewArticleResponse (reduceResponse ok err)
    , headers = [ Http.header "Accept" "application/json" ]
    , timeout = Nothing
    , tracker = Nothing
    }


articleUrlToString (Api.Model.ArticleUrl url) =
    Url.Interpolate.interpolate url Dict.empty


getArticleUrl ok err url =
    { method = "GET"
    , url = articleUrlToString url
    , body = Http.emptyBody
    , expect = expectArticle (reduceResponse ok err)
    , headers = [ Http.header "Accept" "application/json" ]
    , timeout = Nothing
    , tracker = Nothing
    }


putArticleUrl body ok err url =
    { method = "PUT"
    , url = articleUrlToString url
    , body = body |> updateDraftBody
    , expect = expectUpdateArticleResponse (reduceResponse ok err)
    , headers = [ Http.header "Accept" "application/json" ]
    , timeout = Nothing
    , tracker = Nothing
    }


deleteArticleUrl ok err url =
    { method = "DELETE"
    , url = articleUrlToString url
    , body = Http.emptyBody
    , expect = Http.expectJson (reduceResponse ok err) Json.Decode.string
    , headers = [ Http.header "Accept" "application/json" ]
    , timeout = Nothing
    , tracker = Nothing
    }


expectArticle msg =
    Http.expectJson msg Api.Decode.article


expectIndex msg =
    Http.expectJson msg Api.Decode.index


expectNewArticleResponse msg =
    Http.expectJson msg Api.Decode.newArticleResponse


expectSearchResponse msg =
    Http.expectJson msg Api.Decode.searchResponse


expectUpdateArticleResponse msg =
    Http.expectJson msg Api.Decode.updateArticleResponse


newDraftBody body =
    body |> Api.Encode.newDraft |> Http.jsonBody


updateDraftBody body =
    body |> Api.Encode.updateDraft |> Http.jsonBody


reduceResponse ok err result =
    case result of
        Result.Ok value ->
            ok value

        Result.Err value ->
            err value


headers list req =
    { req | headers = list |> List.map (\( k, v ) -> Http.header k v) }


header key value req =
    { req | headers = req.headers ++ [ Http.header key value ] }


timeout t req =
    { req | timeout = Just t }


noTimeout req =
    { req | timeout = Nothing }


tracker name req =
    { req | tracker = Just name }


noTracker name req =
    { req | tracker = Nothing }


request =
    Http.request


mock res req =
    case req.url |> Url.fromString of
        Just reqUrl ->
            res reqUrl |> Task.succeed |> Task.perform identity

        Nothing ->
            Cmd.none
