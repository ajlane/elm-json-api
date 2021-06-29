module SpecTests exposing (suite)

import Dict
import Expect
import Json.Decode
import Json.Encode
import Spec exposing (CustomType(..), Property(..), Spec, TypeRef(..), UrlParam(..), defaultMethods)
import Test exposing (..)


decodeSpec spec =
    Json.Decode.decodeString Spec.specDecoder spec


suite =
    describe "specDecoder"
        [ test "does not parse empty files" <|
            \_ ->
                ""
                    |> decodeSpec
                    |> Expect.err
        , test "does not parse empty objects" <|
            \_ ->
                "{}"
                    |> decodeSpec
                    |> Expect.err
        , test "requires description" <|
            \_ ->
                """
                { "types" : {} }
                """
                    |> decodeSpec
                    |> Expect.err
        , test "requires types" <|
            \_ ->
                """
                { "description" : "Test" }
                """
                    |> decodeSpec
                    |> Expect.err
        , test "ignores undefined fields" <|
            \_ ->
                let
                    json =
                        """
                        { "description" : "Test"
                        , "types" : {}
                        , "undefined" : true
                        }
                        """

                    spec =
                        { description = "Test"
                        , types = []
                        }
                in
                decodeSpec json
                    |> Expect.equal (Ok spec)
        , test "parses custom records" <|
            \_ ->
                let
                    json =
                        """
                        { "description" : "Test"
                        , "types" :
                          { "Splat" :
                            { "is" : "Record"
                            , "description" : "A splat"
                            , "has":
                              { "a" :
                                { "is" : "Int"
                                , "description" : "The a of a splat"
                                }
                              }
                            }
                          }
                        }
                        """

                    spec =
                        { description = "Test"
                        , types =
                            [ CustomRecord "Splat"
                                { description = "A splat"
                                , has =
                                    [ IntProperty "a"
                                        { description = "The a of a splat"
                                        , optional = False
                                        }
                                    ]
                                }
                            ]
                        }
                in
                decodeSpec json
                    |> Expect.equal (Ok spec)
        , test "parses custom urls" <|
            \_ ->
                let
                    json =
                        """
                        { "description" : "Test"
                        , "types" :
                          { "SplatUrl" :
                            { "is" : "Url"
                            , "description" : "A link to a splat"
                            }
                          }
                        }
                        """

                    spec =
                        { description = "Test"
                        , types =
                            [ CustomUrl "SplatUrl"
                                { description = "A link to a splat"
                                , params = []
                                , methods = defaultMethods
                                }
                            ]
                        }
                in
                decodeSpec json
                    |> Expect.equal (Ok spec)
        , test "parses custom urls with a get method" <|
            \_ ->
                let
                    json =
                        """
                        { "description" : "Test"
                        , "types" :
                          { "Splat" :
                            { "is" : "String"
                            , "description": "A splat"
                            }
                          , "SplatUrl" :
                            { "is" : "Url"
                            , "description" : "A link to a splat"
                            , "methods":
                              { "get" : "Splat"
                              }
                            }
                          }
                        }
                        """

                    spec =
                        { description = "Test"
                        , types =
                            [ CustomString "Splat" { description = "A splat", minLength = Nothing, maxLength = Nothing, matches = Nothing, equals = Nothing }
                            , CustomUrl "SplatUrl"
                                { description = "A link to a splat"
                                , params = []
                                , methods =
                                    { defaultMethods
                                        | get = CustomTypeRef "Splat" |> Just
                                    }
                                }
                            ]
                        }
                in
                decodeSpec json
                    |> Expect.equal (Ok spec)
        , test "parses custom urls with template parameters" <|
            \_ ->
                let
                    json =
                        """
                        { "description" : "Test"
                        , "types" :
                          { "SplatUrl" :
                            { "is" : "Url"
                            , "description" : "A link to a splat"
                            , "params" :
                              { "q" : { "description" : "A query" }
                              , "size" : { "description" : "The page size", "optional" : true }
                              }
                            }
                          }
                        }
                        """

                    spec =
                        { description = "Test"
                        , types =
                            [ CustomUrl "SplatUrl"
                                { description = "A link to a splat"
                                , params =
                                    [ UrlParam "q" { description = "A query", optional = False }
                                    , UrlParam "size" { description = "The page size", optional = True }
                                    ]
                                , methods = defaultMethods
                                }
                            ]
                        }
                in
                decodeSpec json
                    |> Expect.equal (Ok spec)
        , test "does not parse custom urls with template parameter arrays" <|
            \_ ->
                let
                    json =
                        """
                        { "description" : "Test"
                        , "types" :
                          { "SplatUrl" :
                            { "is" : "Url"
                            , "description" : "A link to a splat"
                            , "params" :
                              [ "q"
                              , "size"
                              ]
                            }
                          }
                        }
                        """
                in
                decodeSpec json
                    |> Expect.err
        ]
