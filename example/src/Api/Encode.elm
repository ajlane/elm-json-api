module Api.Encode exposing (..)

import Api.Encode
import Api.Model
import Dict
import Http
import Json.Decode
import Json.Encode
import Time
import Url.Interpolate


{-| Encodes UpdateDraft values as JSON.


-}
updateDraft : Api.Model.UpdateDraft -> Json.Encode.Value
updateDraft value =
    Json.Encode.object
        ([ Just ( "base", value.base |> Json.Encode.int )
         , Just ( "body", value.body |> Json.Encode.string )
         , Just ( "title", value.title |> Json.Encode.string )
         ]
            |> List.filterMap identity
        )


updateArticleResponse value =
    case value of
        Api.Model.UpdateArticleResponseOk v ->
            Json.Encode.object [ ( "ok", v |> Api.Encode.article ) ]

        Api.Model.UpdateArticleResponseOutdated v ->
            Json.Encode.object [ ( "outdated", v |> Api.Encode.article ) ]


searchUrl (Api.Model.SearchUrl template) =
    Json.Encode.string template


searchResults =
    Json.Encode.list Api.Encode.searchHit


searchResponse value =
    case value of
        Api.Model.SearchResponseNone ->
            Json.Encode.object [ ( "none", Json.Encode.object [] ) ]

        Api.Model.SearchResponseSome v ->
            Json.Encode.object [ ( "some", v |> Api.Encode.searchResults ) ]


{-| Encodes SearchHit values as JSON.


-}
searchHit : Api.Model.SearchHit -> Json.Encode.Value
searchHit value =
    Json.Encode.object
        ([ Just ( "href", value.href |> Api.Encode.articleUrl )
         , Just ( "id", value.id |> Json.Encode.string )
         , Just ( "snippet", value.snippet |> Json.Encode.string )
         , Just ( "title", value.title |> Json.Encode.string )
         ]
            |> List.filterMap identity
        )


{-| Encodes NewDraft values as JSON.


-}
newDraft : Api.Model.NewDraft -> Json.Encode.Value
newDraft value =
    Json.Encode.object
        ([ Just ( "base", value.base |> Json.Encode.int )
         , Just ( "body", value.body |> Json.Encode.string )
         , Just ( "title", value.title |> Json.Encode.string )
         ]
            |> List.filterMap identity
        )


newArticleResponse value =
    case value of
        Api.Model.NewArticleResponseOk v ->
            Json.Encode.object [ ( "ok", v |> Api.Encode.article ) ]


indexUrl (Api.Model.IndexUrl url) =
    Json.Encode.string url


{-| Encodes Index values as JSON.


-}
index : Api.Model.Index -> Json.Encode.Value
index value =
    Json.Encode.object
        ([ Just ( "featured", value.featured |> Api.Encode.article )
         , Just ( "search", value.search |> Api.Encode.searchUrl )
         , Just ( "self", value.self |> Api.Encode.indexUrl )
         ]
            |> List.filterMap identity
        )


articleUrl (Api.Model.ArticleUrl url) =
    Json.Encode.string url


{-| Encodes Article values as JSON.


-}
article : Api.Model.Article -> Json.Encode.Value
article value =
    Json.Encode.object
        ([ Just ( "body", value.body |> Json.Encode.string )
         , Just
            ( "created"
            , value.created |> (Time.posixToMillis >> Json.Encode.int)
            )
         , Just ( "self", value.self |> Api.Encode.articleUrl )
         , Just ( "title", value.title |> Json.Encode.string )
         , value.updated
            |> Maybe.map
                (\v -> ( "updated", (Time.posixToMillis >> Json.Encode.int) v ))
         , Just ( "version", value.version |> Json.Encode.int )
         ]
            |> List.filterMap identity
        )
