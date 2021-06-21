module Api.NewDraft exposing (..)

import Json.Decode
import Json.Encode


{-| A draft of a new article.


-}
type alias NewDraft =
    { base : Int, body : String, title : String }


{-| Encodes NewDraft values as JSON.


-}
encode : NewDraft -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "base", record.base |> Json.Encode.int )
        , ( "body", record.body |> Json.Encode.string )
        , ( "title", record.title |> Json.Encode.string )
        ]


{-| Decodes JSON as a NewDraft value.


-}
decoder : Json.Decode.Decoder NewDraft
decoder =
    Json.Decode.map3
        NewDraft
        (Json.Decode.field "base" Json.Decode.int)
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
