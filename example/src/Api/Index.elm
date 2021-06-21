module Api.Index exposing (..)

import Api.Article
import Api.IndexUrl
import Api.SearchUrl
import Json.Decode
import Json.Encode


{-| A directory of available services provided by the API.


-}
type alias Index =
    { featured : Api.Article.Article
    , search : Api.SearchUrl.SearchUrl
    , self : Api.IndexUrl.IndexUrl
    }


{-| Encodes Index values as JSON.


-}
encode : Index -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "featured", record.featured |> Api.Article.encode )
        , ( "search", record.search |> Api.SearchUrl.encode )
        , ( "self", record.self |> Api.IndexUrl.encode )
        ]


{-| Decodes JSON as a Index value.


-}
decoder : Json.Decode.Decoder Index
decoder =
    Json.Decode.map3
        Index
        (Json.Decode.field "featured" Api.Article.decoder)
        (Json.Decode.field "search" Api.SearchUrl.decoder)
        (Json.Decode.field "self" Api.IndexUrl.decoder)
