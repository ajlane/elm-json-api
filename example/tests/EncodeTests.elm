module EncodeTests exposing (suite)

import Api.Encode
import Api.Model
import Expect
import Json.Encode
import Test exposing (describe, test)


suite =
    describe "updateDraft"
        [ test "encodes valid values" <|
            \_ ->
                { base = 1, title = "Article", body = "Lorem ipsum" }
                    |> Api.Encode.updateDraft
                    |> Json.Encode.encode 0
                    |> Expect.equal """{"base":1,"body":"Lorem ipsum","title":"Article"}"""
        ]
