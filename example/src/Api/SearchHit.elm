module Api.SearchHit exposing (..)

import Api.ArticleUrl
import Json.Decode
import Json.Encode


{-| A single result for a search query.


-}
type alias SearchHit =
    { href : Api.ArticleUrl.ArticleUrl
    , id : String
    , snippet : String
    , title : String
    }


{-| Encodes SearchHit values as JSON.


-}
encode : SearchHit -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "href", record.href |> Api.ArticleUrl.encode )
        , ( "id", record.id |> Json.Encode.string )
        , ( "snippet", record.snippet |> Json.Encode.string )
        , ( "title", record.title |> Json.Encode.string )
        ]


{-| Decodes JSON as a SearchHit value.


-}
decoder : Json.Decode.Decoder SearchHit
decoder =
    Json.Decode.map4
        SearchHit
        (Json.Decode.field "href" Api.ArticleUrl.decoder)
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "snippet" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
