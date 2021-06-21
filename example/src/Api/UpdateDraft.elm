module Api.UpdateDraft exposing (..)

import Json.Decode
import Json.Encode


{-| A draft of an article.


-}
type alias UpdateDraft =
    { base : Int, body : String, title : String }


{-| Encodes UpdateDraft values as JSON.


-}
encode : UpdateDraft -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "base", record.base |> Json.Encode.int )
        , ( "body", record.body |> Json.Encode.string )
        , ( "title", record.title |> Json.Encode.string )
        ]


{-| Decodes JSON as a UpdateDraft value.


-}
decoder : Json.Decode.Decoder UpdateDraft
decoder =
    Json.Decode.map3
        UpdateDraft
        (Json.Decode.field "base" Json.Decode.int)
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field "title" Json.Decode.string)
