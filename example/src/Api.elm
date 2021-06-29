module Api exposing (..)

import Dict
import Http
import Json.Decode
import Json.Encode
import Time
import Url.Interpolate


{-| A draft of an article.


-}
type alias UpdateDraft =
    { base : Int, body : String, title : String }


{-| Encodes UpdateDraft values as JSON.


-}
encodeUpdateDraft : UpdateDraft -> Json.Encode.Value
encodeUpdateDraft record =
    Json.Encode.object
        ([ Just ( "base", record.base |> Json.Encode.int )
         , Just ( "body", record.body |> Json.Encode.string )
         , Just ( "title", record.title |> Json.Encode.string )
         ]
            |> List.filterMap identity
        )


{-| Decodes JSON as UpdateDraft values.


-}
decodeUpdateDraft : Json.Decode.Decoder UpdateDraft
decodeUpdateDraft =
    Json.Decode.map3
        UpdateDraft
        (Json.Decode.field "base" Json.Decode.int)
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)


{-| The response to a request to update an existing article


-}
type UpdateArticleResponse
    = UpdateArticleResponseOk Article
    | UpdateArticleResponseOutdated Article


encodeUpdateArticleResponse value =
    case value of
        UpdateArticleResponseOk v ->
            Json.Encode.object [ ( "ok", v |> encodeArticle ) ]

        UpdateArticleResponseOutdated v ->
            Json.Encode.object [ ( "outdated", v |> encodeArticle ) ]


decodeUpdateArticleResponse =
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\dict ->
                case dict |> Dict.keys of
                    [ "ok" ] ->
                        Json.Decode.field "ok" decodeArticle
                            |> Json.Decode.map UpdateArticleResponseOk

                    [ "outdated" ] ->
                        Json.Decode.field "outdated" decodeArticle
                            |> Json.Decode.map UpdateArticleResponseOutdated

                    _ ->
                        Json.Decode.fail
                            "Expected exactly one of: ok, outdated."
            )


{-| A link to the results of a search.


-}
type SearchUrl
    = SearchUrl String


withSearchUrl { after, q, size } (SearchUrl template) =
    { headers = []
    , url =
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
    , timeout = Nothing
    , tracker = Nothing
    , get = decodeSearchResponse
    }


encodeSearchUrl (SearchUrl template) =
    Json.Encode.string template


decodeSearchUrl =
    Json.Decode.map SearchUrl Json.Decode.string


{-| A list of search results.


-}
type alias SearchResults =
    List SearchHit


encodeSearchResults =
    Json.Encode.list encodeSearchHit


decodeSearchResults : Json.Decode.Decoder SearchResults
decodeSearchResults =
    Json.Decode.list decodeSearchHit


{-| A response to a query.


-}
type SearchResponse
    = SearchResponseNone
    | SearchResponseSome SearchResults


encodeSearchResponse value =
    case value of
        SearchResponseNone ->
            Json.Encode.object [ ( "none", Json.Encode.object [] ) ]

        SearchResponseSome v ->
            Json.Encode.object [ ( "some", v |> encodeSearchResults ) ]


decodeSearchResponse =
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\dict ->
                case dict |> Dict.keys of
                    [ "none" ] ->
                        Json.Decode.succeed SearchResponseNone

                    [ "some" ] ->
                        Json.Decode.field "some" decodeSearchResults
                            |> Json.Decode.map SearchResponseSome

                    _ ->
                        Json.Decode.fail "Expected exactly one of: none, some."
            )


{-| A single result for a search query.


-}
type alias SearchHit =
    { href : ArticleUrl, id : String, snippet : String, title : String }


{-| Encodes SearchHit values as JSON.


-}
encodeSearchHit : SearchHit -> Json.Encode.Value
encodeSearchHit record =
    Json.Encode.object
        ([ Just ( "href", record.href |> encodeArticleUrl )
         , Just ( "id", record.id |> Json.Encode.string )
         , Just ( "snippet", record.snippet |> Json.Encode.string )
         , Just ( "title", record.title |> Json.Encode.string )
         ]
            |> List.filterMap identity
        )


{-| Decodes JSON as SearchHit values.


-}
decodeSearchHit : Json.Decode.Decoder SearchHit
decodeSearchHit =
    Json.Decode.map4
        SearchHit
        (Json.Decode.field "href" decodeArticleUrl)
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "snippet" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)


{-| A draft of a new article.


-}
type alias NewDraft =
    { base : Int, body : String, title : String }


{-| Encodes NewDraft values as JSON.


-}
encodeNewDraft : NewDraft -> Json.Encode.Value
encodeNewDraft record =
    Json.Encode.object
        ([ Just ( "base", record.base |> Json.Encode.int )
         , Just ( "body", record.body |> Json.Encode.string )
         , Just ( "title", record.title |> Json.Encode.string )
         ]
            |> List.filterMap identity
        )


{-| Decodes JSON as NewDraft values.


-}
decodeNewDraft : Json.Decode.Decoder NewDraft
decodeNewDraft =
    Json.Decode.map3
        NewDraft
        (Json.Decode.field "base" Json.Decode.int)
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)


{-| A response to a request to publish a new article.


-}
type NewArticleResponse
    = NewArticleResponseOk Article


encodeNewArticleResponse value =
    case value of
        NewArticleResponseOk v ->
            Json.Encode.object [ ( "ok", v |> encodeArticle ) ]


decodeNewArticleResponse =
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\dict ->
                case dict |> Dict.keys of
                    [ "ok" ] ->
                        Json.Decode.field "ok" decodeArticle
                            |> Json.Decode.map NewArticleResponseOk

                    _ ->
                        Json.Decode.fail "Expected exactly one of: ok."
            )


{-| A link to the API entry-point.


-}
type IndexUrl
    = IndexUrl String


withIndexUrl (IndexUrl url) =
    { headers = []
    , url =
        Url.Interpolate.interpolate
            url
            (Dict.fromList ([] |> List.filterMap identity))
    , timeout = Nothing
    , tracker = Nothing
    , post = { accept = encodeNewDraft, expect = decodeNewArticleResponse }
    , get = decodeIndex
    }


encodeIndexUrl (IndexUrl url) =
    Json.Encode.string url


decodeIndexUrl =
    Json.Decode.map IndexUrl Json.Decode.string


{-| A directory of available services provided by the API.


-}
type alias Index =
    { featured : Article, search : SearchUrl, self : IndexUrl }


{-| Encodes Index values as JSON.


-}
encodeIndex : Index -> Json.Encode.Value
encodeIndex record =
    Json.Encode.object
        ([ Just ( "featured", record.featured |> encodeArticle )
         , Just ( "search", record.search |> encodeSearchUrl )
         , Just ( "self", record.self |> encodeIndexUrl )
         ]
            |> List.filterMap identity
        )


{-| Decodes JSON as Index values.


-}
decodeIndex : Json.Decode.Decoder Index
decodeIndex =
    Json.Decode.map3
        Index
        (Json.Decode.field "featured" decodeArticle)
        (Json.Decode.field "search" decodeSearchUrl)
        (Json.Decode.field "self" decodeIndexUrl)


{-| A link to the content of an article.


-}
type ArticleUrl
    = ArticleUrl String


withArticleUrl (ArticleUrl url) =
    { headers = []
    , url =
        Url.Interpolate.interpolate
            url
            (Dict.fromList ([] |> List.filterMap identity))
    , timeout = Nothing
    , tracker = Nothing
    , delete = Json.Decode.string
    , put = { accept = encodeUpdateDraft, expect = decodeUpdateArticleResponse }
    , get = decodeArticle
    }


encodeArticleUrl (ArticleUrl url) =
    Json.Encode.string url


decodeArticleUrl =
    Json.Decode.map ArticleUrl Json.Decode.string


{-| An article.


-}
type alias Article =
    { body : String
    , created : Time.Posix
    , self : ArticleUrl
    , title : String
    , updated : Maybe Time.Posix
    , version : Int
    }


{-| Encodes Article values as JSON.


-}
encodeArticle : Article -> Json.Encode.Value
encodeArticle record =
    Json.Encode.object
        ([ Just ( "body", record.body |> Json.Encode.string )
         , Just
            ( "created"
            , record.created |> (Time.posixToMillis >> Json.Encode.int)
            )
         , Just ( "self", record.self |> encodeArticleUrl )
         , Just ( "title", record.title |> Json.Encode.string )
         , record.updated
            |> Maybe.map
                (\v -> ( "updated", (Time.posixToMillis >> Json.Encode.int) v ))
         , Just ( "version", record.version |> Json.Encode.int )
         ]
            |> List.filterMap identity
        )


{-| Decodes JSON as Article values.


-}
decodeArticle : Json.Decode.Decoder Article
decodeArticle =
    Json.Decode.map6
        Article
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field
            "created"
            (Json.Decode.map Time.millisToPosix Json.Decode.int)
        )
        (Json.Decode.field "self" decodeArticleUrl)
        (Json.Decode.field "title" Json.Decode.string)
        (Json.Decode.field "updated" (Json.Decode.succeed ())
            |> Json.Decode.maybe
            |> Json.Decode.andThen
                (\m ->
                    case m of
                        Just _ ->
                            Json.Decode.field
                                "updated"
                                (Json.Decode.map
                                    Time.millisToPosix
                                    Json.Decode.int
                                )
                                |> Json.Decode.map Just

                        Nothing ->
                            Json.Decode.succeed Nothing
                )
        )
        (Json.Decode.field "version" Json.Decode.int)


headers list request =
    { request | headers = list |> List.map (\( k, v ) -> Http.header k v) }


header key value request =
    { request | headers = request.headers ++ [ Http.header key value ] }


timeout t request =
    { request | timeout = Just t }


noTimeout request =
    { request | timeout = Nothing }


tracker name request =
    { request | tracker = Just name }


noTracker name request =
    { request | tracker = Nothing }


get ok err request =
    let
        toMsg r =
            case r of
                Result.Ok value ->
                    ok value

                Result.Err value ->
                    err value
    in
    Http.request
        { method = "GET"
        , headers = request.headers
        , url = request.url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg request.get
        , timeout = request.timeout
        , tracker = request.tracker
        }


put ok err request =
    let
        toMsg r =
            case r of
                Result.Ok value ->
                    ok value

                Result.Err value ->
                    err value
    in
    Http.request
        { method = "PUT"
        , headers = request.headers
        , url = request.url
        , body = request.put.accept
        , expect = Http.expectJson toMsg request.put.expect
        , timeout = request.timeout
        , tracker = request.tracker
        }


post ok err request =
    let
        toMsg r =
            case r of
                Result.Ok value ->
                    ok value

                Result.Err value ->
                    err value
    in
    Http.request
        { method = "POST"
        , headers = request.headers
        , url = request.url
        , body = request.post.accept
        , expect = Http.expectJson toMsg request.post.expect
        , timeout = request.timeout
        , tracker = request.tracker
        }


delete ok err request =
    let
        toMsg r =
            case r of
                Result.Ok value ->
                    ok value

                Result.Err value ->
                    err value
    in
    Http.request
        { method = "DELETE"
        , headers = request.headers
        , url = request.url
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg request.delete
        , timeout = request.timeout
        , tracker = request.tracker
        }
