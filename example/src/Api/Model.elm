module Api.Model exposing (..)

import Api.Model
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


{-| The response to a request to update an existing article


-}
type UpdateArticleResponse
    = UpdateArticleResponseOk Article
    | UpdateArticleResponseOutdated Article


{-| A link to the results of a search.


-}
type SearchUrl
    = SearchUrl String


{-| A list of search results.


-}
type alias SearchResults =
    List SearchHit


{-| A response to a query.


-}
type SearchResponse
    = SearchResponseNone
    | SearchResponseSome SearchResults


{-| A single result for a search query.


-}
type alias SearchHit =
    { href : Api.Model.ArticleUrl
    , id : String
    , snippet : String
    , title : String
    }


{-| A draft of a new article.


-}
type alias NewDraft =
    { base : Int, body : String, title : String }


{-| A response to a request to publish a new article.


-}
type NewArticleResponse
    = NewArticleResponseOk Article


{-| A link to the API entry-point.


-}
type IndexUrl
    = IndexUrl String


{-| A directory of available services provided by the API.


-}
type alias Index =
    { featured : Api.Model.Article
    , search : Api.Model.SearchUrl
    , self : Api.Model.IndexUrl
    }


{-| A link to the content of an article.


-}
type ArticleUrl
    = ArticleUrl String


{-| An article.


-}
type alias Article =
    { body : String
    , created : Time.Posix
    , self : Api.Model.ArticleUrl
    , title : String
    , updated : Maybe Time.Posix
    , version : Int
    }
