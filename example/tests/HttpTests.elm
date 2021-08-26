module HttpTests exposing (suite)

import Api.Http
import Api.Model exposing (SearchUrl(..))
import Expect
import Test exposing (describe, test)


suite =
    describe "searchUrlToString"
        [ test "interpolates urls with parameters" <|
            \_ ->
                SearchUrl "http://test.example/search{?q,after,size}"
                    |> Api.Http.searchUrlToString { q = "*", after = Just "a", size = Just "10" }
                    |> Expect.equal "http://test.example/search?q=%2A&after=a&size=10"
        , test "interpolates urls with pre-filled parameters" <|
            \_ ->
                SearchUrl "http://test.example/search{?q,after}&size=10"
                    |> Api.Http.searchUrlToString { q = "*", after = Just "a", size = Nothing }
                    |> Expect.equal "http://test.example/search?q=%2A&after=a&size=10"
        , test "interpolates urls omitting missing parameters" <|
            \_ ->
                SearchUrl "http://test.example/search{?q,after,size}"
                    |> Api.Http.searchUrlToString { q = "*", after = Nothing, size = Nothing }
                    |> Expect.equal "http://test.example/search?q=%2A&after=&size="

        {-
           Missing parameters should be omitted entirely, not filled with blank strings.
           TODO: Contribute a fix upstream to ericgj/elm-uri-template to bring it in line with the spec
                   |> Expect.equal "http://test.example/search?q=%2A"
        -}
        ]
