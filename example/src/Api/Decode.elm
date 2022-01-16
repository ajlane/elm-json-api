module Api.Decode exposing (..)

import Api.Model
import Dict
import Http
import Json.Decode
import Json.Encode
import Time
import Url.Interpolate


{-| Decodes JSON as UpdateDraft values.


-}
updateDraft : Json.Decode.Decoder Api.Model.UpdateDraft
updateDraft =
    Json.Decode.succeed Api.Model.UpdateDraft
        |> andMap (Json.Decode.field "base" Json.Decode.int)
        |> andMap (Json.Decode.field "body" Json.Decode.string)
        |> andMap (Json.Decode.field "title" Json.Decode.string)


updateArticleResponse =
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\dict ->
                case dict |> Dict.keys of
                    [ "ok" ] ->
                        Json.Decode.field "ok" article
                            |> Json.Decode.map Api.Model.UpdateArticleResponseOk

                    [ "outdated" ] ->
                        Json.Decode.field "outdated" article
                            |> Json.Decode.map
                                Api.Model.UpdateArticleResponseOutdated

                    _ ->
                        Json.Decode.fail
                            "Expected exactly one of: ok, outdated."
            )


searchUrl =
    Json.Decode.map Api.Model.SearchUrl Json.Decode.string


searchResults : Json.Decode.Decoder Api.Model.SearchResults
searchResults =
    Json.Decode.list searchHit


searchResponse =
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\dict ->
                case dict |> Dict.keys of
                    [ "none" ] ->
                        Json.Decode.succeed Api.Model.SearchResponseNone

                    [ "some" ] ->
                        Json.Decode.field "some" searchResults
                            |> Json.Decode.map Api.Model.SearchResponseSome

                    _ ->
                        Json.Decode.fail "Expected exactly one of: none, some."
            )


{-| Decodes JSON as SearchHit values.


-}
searchHit : Json.Decode.Decoder Api.Model.SearchHit
searchHit =
    Json.Decode.succeed Api.Model.SearchHit
        |> andMap (Json.Decode.field "href" articleUrl)
        |> andMap (Json.Decode.field "id" Json.Decode.string)
        |> andMap (Json.Decode.field "snippet" Json.Decode.string)
        |> andMap (Json.Decode.field "title" Json.Decode.string)


{-| Decodes JSON as NewDraft values.


-}
newDraft : Json.Decode.Decoder Api.Model.NewDraft
newDraft =
    Json.Decode.succeed Api.Model.NewDraft
        |> andMap (Json.Decode.field "base" Json.Decode.int)
        |> andMap (Json.Decode.field "body" Json.Decode.string)
        |> andMap (Json.Decode.field "title" Json.Decode.string)


newArticleResponse =
    Json.Decode.dict (Json.Decode.succeed ())
        |> Json.Decode.andThen
            (\dict ->
                case dict |> Dict.keys of
                    [ "ok" ] ->
                        Json.Decode.field "ok" article
                            |> Json.Decode.map Api.Model.NewArticleResponseOk

                    _ ->
                        Json.Decode.fail "Expected exactly one of: ok."
            )


indexUrl =
    Json.Decode.map Api.Model.IndexUrl Json.Decode.string


{-| Decodes JSON as Index values.


-}
index : Json.Decode.Decoder Api.Model.Index
index =
    Json.Decode.succeed Api.Model.Index
        |> andMap (Json.Decode.field "featured" article)
        |> andMap (Json.Decode.field "search" searchUrl)
        |> andMap (Json.Decode.field "self" indexUrl)


articleUrl =
    Json.Decode.map Api.Model.ArticleUrl Json.Decode.string


{-| Decodes JSON as Article values.


-}
article : Json.Decode.Decoder Api.Model.Article
article =
    Json.Decode.succeed Api.Model.Article
        |> andMap (Json.Decode.field "body" Json.Decode.string)
        |> andMap
            (Json.Decode.field
                "created"
                (Json.Decode.map Time.millisToPosix Json.Decode.int)
            )
        |> andMap (Json.Decode.field "self" articleUrl)
        |> andMap (Json.Decode.field "title" Json.Decode.string)
        |> andMap
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
        |> andMap (Json.Decode.field "version" Json.Decode.int)


{-| Applicative map


-}
andMap =
    Json.Decode.map2 (|>)
