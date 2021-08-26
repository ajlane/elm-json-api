module DecodeTests exposing (suite)

import Api.Decode
import Api.Model
import Expect
import Json.Decode
import Test exposing (describe, test)


suite =
    describe "updateDraft"
        [ test "does not parse empty files" <|
            \_ ->
                """"""
                    |> Json.Decode.decodeString Api.Decode.updateDraft
                    |> Expect.err
        , test "decodes valid values" <|
            \_ ->
                """{"base":1,"title":"Article","body":"Lorem ipsum"}"""
                    |> Json.Decode.decodeString Api.Decode.updateDraft
                    |> Expect.equal (Ok { base = 1, title = "Article", body = "Lorem ipsum" })
        ]
