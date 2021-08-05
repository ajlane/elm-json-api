module GeneratorTests exposing (suite)

import Expect
import Generator exposing (specFiles)
import Spec exposing (CustomType(..), Property(..), Spec, TypeRef(..), defaultMethods)
import Test exposing (..)


example : Spec
example =
    { description = "An example spec for a JSON API."
    , types =
        [ CustomUrl
            "AdderUrl"
            { description = "A link to a service that adds numbers together."
            , methods =
                { defaultMethods
                    | post =
                        Just
                            { accept = CustomTypeRef "IntPair"
                            , expect = IntTypeRef
                            }
                }
            , params = []
            }
        , CustomRecord
            "HealthCheck"
            { description = "A summary of the health of the service."
            , has =
                [ BoolProperty "adder" { description = "Whether the adder service is ready.", optional = False }
                , BoolProperty "multiplier" { description = "Whether the multiplier service is ready.", optional = False }
                , InstantProperty "time" { description = "The time that the health check was performed.", optional = False }
                ]
            }
        , CustomRecord
            "Index"
            { description = "A directory of available services provided by the API."
            , has =
                [ CustomProperty "adder" { is = "AdderUrl", description = "A link to a service that adds numbers together.", optional = True }
                , CustomProperty "health" { is = "HealthUrl", description = "A link to the health check.", optional = False }
                , CustomProperty "multiplier" { is = "MultiplierUrl", description = "A link to a service that multiplies numbers together.", optional = True }
                , CustomProperty "self" { is = "IndexUrl", description = "The canonical url of the service.", optional = False }
                , StringProperty "whoami" { description = "The username of the requesting user, if known.", optional = True }
                ]
            }
        , CustomUrl
            "HealthUrl"
            { description = "A link to a service that reports on the health of the system."
            , methods =
                { defaultMethods
                    | get =
                        Just
                            (CustomTypeRef "HealthCheck")
                }
            , params = []
            }
        , CustomUrl
            "IndexUrl"
            { description = "The entry-point to the API."
            , methods =
                { defaultMethods
                    | get =
                        Just
                            (CustomTypeRef "Index")
                }
            , params = []
            }
        , CustomRecord
            "IntPair"
            { description = "A pair of integers."
            , has =
                [ IntProperty "a" { description = "The first number of the pair.", optional = False }
                , IntProperty "b" { description = "The second number of the pair.", optional = False }
                ]
            }
        , CustomUrl
            "MultiplierUrl"
            { description = "A link to a service that multiplies numbers together."
            , methods =
                { defaultMethods
                    | post =
                        Just
                            { accept = CustomTypeRef "IntPair"
                            , expect = IntTypeRef
                            }
                }
            , params = []
            }
        ]
    }


suite =
    describe "generate"
        [ test "produces the right set of files" <|
            \_ ->
                specFiles [ "MyApp", "Api" ] example
                    |> List.map (\{ path } -> path)
                    |> List.sort
                    |> Expect.equal
                        [ "MyApp/Decode.elm", "MyApp/Encode.elm", "MyApp/Http.elm", "MyApp/Model.elm" ]
        ]
