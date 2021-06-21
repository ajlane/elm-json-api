module Api.Article exposing (..)

import Api.ArticleUrl
import Json.Decode
import Json.Encode
import Time


{-| An article.


-}
type alias Article =
    { body : String
    , created : Time.Posix
    , self : Api.ArticleUrl.ArticleUrl
    , title : String
    , updated : Maybe Time.Posix
    , version : Int
    }


{-| Encodes Article values as JSON.


-}
encode : Article -> Json.Encode.Value
encode record =
    Json.Encode.object
        [ ( "body", record.body |> Json.Encode.string )
        , ( "created"
          , record.created |> (Time.posixToMillis >> Json.Encode.int)
          )
        , ( "self", record.self |> Api.ArticleUrl.encode )
        , ( "title", record.title |> Json.Encode.string )
        , ( "updated"
          , record.updated
                |> Maybe.map (Time.posixToMillis >> Json.Encode.int)
                |> Maybe.withDefault Json.Encode.null
          )
        , ( "version", record.version |> Json.Encode.int )
        ]


{-| Decodes JSON as a Article value.


-}
decoder : Json.Decode.Decoder Article
decoder =
    Json.Decode.map6
        Article
        (Json.Decode.field "body" Json.Decode.string)
        (Json.Decode.field
            "created"
            (Json.Decode.map Time.millisToPosix Json.Decode.int)
        )
        (Json.Decode.field "self" Api.ArticleUrl.decoder)
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
