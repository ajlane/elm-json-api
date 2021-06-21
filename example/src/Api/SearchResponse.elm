module Api.SearchResponse exposing (..)

import Api.SearchResults


{-| A response to a query.


-}
type SearchResponse
    = None
    | Some Api.SearchResults.SearchResults
