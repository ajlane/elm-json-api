module Msg exposing (..)

import Api.Model exposing (Article, Index, SearchResponse)
import Http


type Msg
    = IndexReceived Index
    | IndexUnavailable Http.Error
    | ArticleReceived Article
    | ArticleUnavailable Http.Error
    | QueryUpdated String
    | SearchResultsReceived SearchResponse
    | SearchResultsUnavailable Http.Error
