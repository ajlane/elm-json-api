module Generator exposing (..)

import Dict
import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Set exposing (Set)
import Spec exposing (CustomFloatDefinition, CustomType(..), Property(..), Spec, TypeName, TypeRef(..))


type alias FilePath =
    String


type alias File =
    { path : FilePath
    , content : Elm.CodeGen.File
    }


type alias Namespace =
    List String


propertyAnn : ModuleName -> ModuleName -> Spec.Property -> ( String, Elm.CodeGen.TypeAnnotation )
propertyAnn namespace moduleName property =
    case property of
        Spec.StringProperty propertyName stringProperty ->
            ( propertyName
            , if stringProperty.optional then
                maybeAnn stringAnn

              else
                stringAnn
            )

        Spec.IntProperty propertyName intProperty ->
            ( propertyName
            , if intProperty.optional then
                maybeAnn intAnn

              else
                intAnn
            )

        Spec.FloatProperty propertyName floatProperty ->
            ( propertyName
            , if floatProperty.optional then
                maybeAnn floatAnn

              else
                floatAnn
            )

        Spec.BoolProperty propertyName boolProperty ->
            ( propertyName
            , if boolProperty.optional then
                maybeAnn boolAnn

              else
                boolAnn
            )

        Spec.InstantProperty propertyName instantProperty ->
            ( propertyName
            , if instantProperty.optional then
                maybeAnn (fqTyped [ "Time" ] "Posix" [])

              else
                fqTyped [ "Time" ] "Posix" []
            )

        Spec.UrlProperty propertyName urlProperty ->
            ( propertyName
            , if urlProperty.optional then
                maybeAnn stringAnn

              else
                stringAnn
            )

        Spec.CustomProperty propertyName customProperty ->
            ( propertyName
            , if customProperty.optional then
                maybeAnn (apiModelType namespace moduleName customProperty.is [])

              else
                apiModelType namespace moduleName customProperty.is []
            )


jsonEncodeType =
    fqTyped [ "Json", "Encode" ]


jsonEncodeFun =
    fqFun [ "Json", "Encode" ]


jsonDecodeType =
    fqTyped [ "Json", "Decode" ]


jsonDecodeFun =
    fqFun [ "Json", "Decode" ]


jsonDecodeVal =
    fqVal [ "Json", "Decode" ]


apiModelModuleName namespace =
    namespace ++ [ "Model" ]


apiEncodeModuleName namespace =
    namespace ++ [ "Encode" ]


apiDecodeModuleName namespace =
    namespace ++ [ "Decode" ]


apiHttpModuleName namespace =
    namespace ++ [ "Http" ]


apiModelType namespace moduleName name =
    if moduleName == apiModelModuleName namespace then
        typed name

    else
        fqTyped (apiModelModuleName namespace) name


apiModelConstruct namespace moduleName name =
    if moduleName == apiModelModuleName namespace then
        construct name

    else
        fqConstruct (apiModelModuleName namespace) name


apiModelNamedPattern namespace moduleName name =
    if moduleName == apiModelModuleName namespace then
        namedPattern name

    else
        fqNamedPattern (apiModelModuleName namespace) name


importApiModel : ModuleName -> Import
importApiModel namespace =
    importStmt (apiModelModuleName namespace) Nothing Nothing


importApiEncode : ModuleName -> Import
importApiEncode namespace =
    importStmt (apiEncodeModuleName namespace) Nothing Nothing


importApiDecode : ModuleName -> Import
importApiDecode namespace =
    importStmt (apiDecodeModuleName namespace) Nothing Nothing


importApiHttp : ModuleName -> Import
importApiHttp namespace =
    importStmt (apiHttpModuleName namespace) Nothing Nothing


importTime : Import
importTime =
    importStmt [ "Time" ] Nothing Nothing


importDict : Import
importDict =
    importStmt [ "Dict" ] Nothing Nothing


importJsonDecode : Import
importJsonDecode =
    importStmt [ "Json", "Decode" ] Nothing Nothing


importJsonEncode : Import
importJsonEncode =
    importStmt [ "Json", "Encode" ] Nothing Nothing


importHttp : Import
importHttp =
    importStmt [ "Http" ] Nothing Nothing


importUrl : Import
importUrl =
    importStmt [ "Url" ] Nothing Nothing


importTask : Import
importTask =
    importStmt [ "Task" ] Nothing Nothing


importUrlInterpolate : Import
importUrlInterpolate =
    importStmt [ "Url", "Interpolate" ] Nothing Nothing


urlParamsPatterns : ModuleName -> ModuleName -> TypeName -> List Spec.UrlParam -> List Pattern
urlParamsPatterns namespace moduleName typeName params =
    if params |> List.isEmpty then
        [ apiModelNamedPattern namespace moduleName typeName [ varPattern "url" ] ]

    else
        [ params
            |> List.map
                (\param ->
                    case param of
                        Spec.UrlParam name _ ->
                            name
                )
            |> recordPattern
        , apiModelNamedPattern namespace moduleName typeName [ varPattern "template" ]
        ]


typeEncoderFun : ModuleName -> ModuleName -> TypeRef -> Expression
typeEncoderFun namespace moduleName ref =
    case ref of
        CustomTypeRef name ->
            if moduleName == apiEncodeModuleName namespace then
                fun (toCamelCase name)

            else
                fqFun (apiEncodeModuleName namespace) (toCamelCase name)

        UrlTypeRef ->
            jsonEncodeFun "string"

        StringTypeRef ->
            jsonEncodeFun "string"

        IntTypeRef ->
            jsonEncodeFun "int"

        FloatTypeRef ->
            jsonEncodeFun "float"

        BoolTypeRef ->
            jsonEncodeFun "bool"

        InstantTypeRef ->
            parens (applyBinOp (fqFun [ "Time" ] "posixToMillis") composer (jsonEncodeFun "int"))


typeDecoderVal : ModuleName -> ModuleName -> TypeRef -> Expression
typeDecoderVal namespace moduleName ref =
    case ref of
        CustomTypeRef name ->
            if moduleName == apiDecodeModuleName namespace then
                val (toCamelCase name)

            else
                fqVal (apiDecodeModuleName namespace) (toCamelCase name)

        UrlTypeRef ->
            jsonDecodeVal "string"

        StringTypeRef ->
            jsonDecodeVal "string"

        IntTypeRef ->
            jsonDecodeVal "int"

        FloatTypeRef ->
            jsonDecodeVal "float"

        BoolTypeRef ->
            jsonDecodeVal "bool"

        InstantTypeRef ->
            parens (apply [ jsonDecodeFun "map", fqFun [ "Time" ] "millisToPosix", jsonDecodeFun "int" ])


typeAnn : TypeRef -> TypeAnnotation
typeAnn ref =
    case ref of
        CustomTypeRef name ->
            typed name []

        UrlTypeRef ->
            stringAnn

        StringTypeRef ->
            stringAnn

        IntTypeRef ->
            intAnn

        FloatTypeRef ->
            floatAnn

        BoolTypeRef ->
            boolAnn

        InstantTypeRef ->
            fqTyped [ "Time" ] "Posix" []


chainAnn : List TypeAnnotation -> TypeAnnotation
chainAnn list =
    case list of
        head :: tail ->
            case tail of
                [] ->
                    head

                _ ->
                    funAnn head (chainAnn tail)

        [] ->
            unitAnn


updateRecord record setters =
    RecordUpdateExpression (Node emptyRange record) (setters |> List.map (\( fieldName, expr ) -> ( Node emptyRange fieldName, Node emptyRange expr )) |> List.map (Node emptyRange))


toCamelCase s =
    if s |> String.isEmpty then
        ""

    else if (s |> String.length) <= 1 then
        s |> String.toLower

    else
        (s |> String.left 2 |> String.toLower) ++ (s |> String.dropLeft 2)


modelFile namespace spec =
    { path = (namespace |> String.join "/") ++ "/Model.elm"
    , content =
        file
            (normalModule (apiModelModuleName namespace) [])
            [ importDict, importHttp, importJsonDecode, importJsonEncode, importUrlInterpolate, importTime ]
            (spec.types
                |> List.map
                    (\type_ ->
                        case type_ of
                            CustomUrl typeName typeDef ->
                                [ customTypeDecl
                                    (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    (List.singleton ( typeName, [ stringAnn ] ))
                                ]

                            CustomRecord typeName typeDef ->
                                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    (recordAnn
                                        (typeDef.has |> List.map (propertyAnn namespace (apiModelModuleName namespace)))
                                    )
                                ]

                            CustomEnum typeName typeDef ->
                                [ customTypeDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    (typeDef.oneOf
                                        |> Dict.toList
                                        |> List.map
                                            (\( name, variantDefinition ) ->
                                                ( typeName ++ name
                                                , variantDefinition.is |> Maybe.map (\t -> [ typeAnn t ]) |> Maybe.withDefault []
                                                )
                                            )
                                    )
                                ]

                            CustomList typeName typeDef ->
                                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    (listAnn (typeAnn typeDef.of_))
                                ]

                            CustomBool typeName typeDef ->
                                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    boolAnn
                                ]

                            CustomString typeName typeDef ->
                                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    stringAnn
                                ]

                            CustomInt typeName typeDef ->
                                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    intAnn
                                ]

                            CustomFloat typeName typeDef ->
                                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                    typeName
                                    []
                                    floatAnn
                                ]
                    )
                |> List.foldl (++) []
            )
            Nothing
    }


encodeFile namespace spec =
    { path = (namespace |> String.join "/") ++ "/Encode.elm"
    , content =
        file
            (normalModule (apiEncodeModuleName namespace) [])
            [ importApiModel namespace, importDict, importHttp, importJsonDecode, importJsonEncode, importUrlInterpolate, importTime ]
            (spec.types
                |> List.map
                    (\type_ ->
                        case type_ of
                            CustomUrl typeName typeDef ->
                                [ funDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    [ apiModelNamedPattern namespace
                                        (apiEncodeModuleName namespace)
                                        typeName
                                        [ if (typeDef.params |> List.length) > 1 then
                                            varPattern "template"

                                          else
                                            varPattern "url"
                                        ]
                                    ]
                                    (apply
                                        [ jsonEncodeFun "string"
                                        , if (typeDef.params |> List.length) > 1 then
                                            val "template"

                                          else
                                            val "url"
                                        ]
                                    )
                                ]

                            CustomRecord typeName typeDef ->
                                [ funDecl
                                    (emptyDocComment |> markdown ("Encodes " ++ typeName ++ " values as JSON.") |> Just)
                                    (funAnn
                                        (apiModelType namespace (apiEncodeModuleName namespace) typeName [])
                                        (jsonEncodeType "Value" [])
                                        |> Just
                                    )
                                    (toCamelCase typeName)
                                    [ varPattern "value" ]
                                    (apply
                                        [ jsonEncodeFun "object"
                                        , parens
                                            (applyBinOp
                                                (list
                                                    (typeDef.has
                                                        |> List.map
                                                            (\p ->
                                                                let
                                                                    encodedProperty propertyName propertyDefinition encoder =
                                                                        if propertyDefinition.optional then
                                                                            applyBinOp (access (val "value") propertyName) piper (apply [ fqFun [ "Maybe" ] "map", lambda [ varPattern "v" ] (tuple [ string propertyName, apply [ encoder, val "v" ] ]) ])

                                                                        else
                                                                            construct "Just" [ parens (tuple [ string propertyName, binOpChain (access (val "value") propertyName) piper [ apply [ encoder ] ] ]) ]
                                                                in
                                                                case p of
                                                                    CustomProperty propertyName customPropertyDefinition ->
                                                                        encodedProperty propertyName customPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) (CustomTypeRef customPropertyDefinition.is))

                                                                    BoolProperty propertyName boolPropertyDefinition ->
                                                                        encodedProperty propertyName boolPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) BoolTypeRef)

                                                                    InstantProperty propertyName instantPropertyDefinition ->
                                                                        encodedProperty propertyName instantPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) InstantTypeRef)

                                                                    StringProperty propertyName stringPropertyDefinition ->
                                                                        encodedProperty propertyName stringPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) StringTypeRef)

                                                                    IntProperty propertyName intPropertyDefinition ->
                                                                        encodedProperty propertyName intPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) IntTypeRef)

                                                                    FloatProperty propertyName floatPropertyDefinition ->
                                                                        encodedProperty propertyName floatPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) FloatTypeRef)

                                                                    UrlProperty propertyName urlPropertyDefinition ->
                                                                        encodedProperty propertyName urlPropertyDefinition (typeEncoderFun namespace (apiEncodeModuleName namespace) UrlTypeRef)
                                                            )
                                                    )
                                                )
                                                piper
                                                (apply [ fqFun [ "List" ] "filterMap", val "identity" ])
                                            )
                                        ]
                                    )
                                ]

                            CustomEnum typeName typeDef ->
                                [ funDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    [ varPattern "value" ]
                                    (caseExpr (val "value")
                                        (typeDef.oneOf
                                            |> Dict.toList
                                            |> List.map
                                                (\( variantName, variantDef ) ->
                                                    ( apiModelNamedPattern namespace
                                                        (apiEncodeModuleName namespace)
                                                        (typeName ++ variantName)
                                                        (case variantDef.is of
                                                            Just _ ->
                                                                [ varPattern "v" ]

                                                            Nothing ->
                                                                []
                                                        )
                                                    , apply
                                                        [ jsonEncodeFun "object"
                                                        , list
                                                            [ tuple
                                                                [ string (variantName |> toCamelCase)
                                                                , case variantDef.is of
                                                                    Just variantType ->
                                                                        applyBinOp (val "v") piper (typeEncoderFun namespace (apiEncodeModuleName namespace) variantType)

                                                                    Nothing ->
                                                                        apply [ jsonEncodeFun "object", list [] ]
                                                                ]
                                                            ]
                                                        ]
                                                    )
                                                )
                                        )
                                    )
                                ]

                            CustomList typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (apply [ jsonEncodeFun "list", typeEncoderFun namespace (apiEncodeModuleName namespace) typeDef.of_ ])
                                ]

                            CustomBool typeName typeDef ->
                                [ funDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    [ varPattern "value" ]
                                    (apply [ jsonEncodeFun "bool" ])
                                ]

                            CustomString typeName typeDef ->
                                [ funDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    [ varPattern "value" ]
                                    (apply [ jsonEncodeFun "string" ])
                                ]

                            CustomInt typeName typeDef ->
                                [ funDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    [ varPattern "value" ]
                                    (apply [ jsonEncodeFun "int" ])
                                ]

                            CustomFloat typeName typeDef ->
                                [ funDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    [ varPattern "value" ]
                                    (apply [ jsonEncodeFun "float" ])
                                ]
                    )
                |> List.foldl (++) []
            )
            Nothing
    }


decodeFile namespace spec =
    { path = (namespace |> String.join "/") ++ "/Decode.elm"
    , content =
        file
            (normalModule (apiDecodeModuleName namespace) [])
            [ importApiModel namespace, importDict, importHttp, importJsonDecode, importJsonEncode, importUrlInterpolate, importTime ]
            (spec.types
                |> List.map
                    (\type_ ->
                        case type_ of
                            CustomUrl typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (apply [ jsonDecodeFun "map", apiModelConstruct namespace (apiDecodeModuleName namespace) typeName [], jsonDecodeFun "string" ])
                                ]

                            CustomRecord typeName typeDef ->
                                [ valDecl
                                    (emptyDocComment |> markdown ("Decodes JSON as " ++ typeName ++ " values.") |> Just)
                                    (jsonDecodeType "Decoder" [ apiModelType namespace (apiDecodeModuleName namespace) typeName [] ] |> Just)
                                    (toCamelCase typeName)
                                    (apply
                                        ([ jsonDecodeFun
                                            (if (typeDef.has |> List.length) > 1 then
                                                "map" ++ (typeDef.has |> List.length |> String.fromInt)
                                                -- TODO: Deal with records with even more properties

                                             else
                                                "map"
                                            )
                                         , apiModelConstruct namespace (apiDecodeModuleName namespace) typeName []
                                         ]
                                            ++ (typeDef.has
                                                    |> List.map
                                                        (\p ->
                                                            let
                                                                fieldDecoder name optional typeRef =
                                                                    if optional then
                                                                        parens
                                                                            (binOpChain
                                                                                (apply [ jsonDecodeFun "field", string name, parens (apply [ jsonDecodeFun "succeed", unit ]) ])
                                                                                piper
                                                                                [ jsonDecodeFun "maybe"
                                                                                , apply
                                                                                    [ jsonDecodeFun "andThen"
                                                                                    , lambda [ varPattern "m" ]
                                                                                        (caseExpr (val "m")
                                                                                            [ ( namedPattern "Just" [ varPattern "_" ], applyBinOp (apply [ jsonDecodeFun "field", string name, typeDecoderVal namespace (apiDecodeModuleName namespace) typeRef ]) piper (apply [ jsonDecodeFun "map", construct "Just" [] ]) )
                                                                                            , ( namedPattern "Nothing" [], apply [ jsonDecodeFun "succeed", construct "Nothing" [] ] )
                                                                                            ]
                                                                                        )
                                                                                    ]
                                                                                ]
                                                                            )

                                                                    else
                                                                        parens (apply [ jsonDecodeFun "field", string name, typeDecoderVal namespace (apiDecodeModuleName namespace) typeRef ])
                                                            in
                                                            case p of
                                                                StringProperty name { optional } ->
                                                                    fieldDecoder name optional StringTypeRef

                                                                BoolProperty name { optional } ->
                                                                    fieldDecoder name optional BoolTypeRef

                                                                InstantProperty name { optional } ->
                                                                    fieldDecoder name optional InstantTypeRef

                                                                IntProperty name { optional } ->
                                                                    fieldDecoder name optional IntTypeRef

                                                                FloatProperty name { optional } ->
                                                                    fieldDecoder name optional FloatTypeRef

                                                                UrlProperty name { optional } ->
                                                                    fieldDecoder name optional UrlTypeRef

                                                                CustomProperty name { optional, is } ->
                                                                    fieldDecoder name optional (CustomTypeRef is)
                                                        )
                                               )
                                        )
                                    )
                                ]

                            CustomEnum typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (applyBinOp
                                        (apply [ jsonDecodeFun "dict", parens (apply [ jsonDecodeFun "succeed", unit ]) ])
                                        piper
                                        (apply
                                            [ jsonDecodeFun "andThen"
                                            , parens
                                                (lambda [ varPattern "dict" ]
                                                    (caseExpr (applyBinOp (val "dict") piper (fqFun [ "Dict" ] "keys"))
                                                        ((typeDef.oneOf
                                                            |> Dict.toList
                                                            |> List.map
                                                                (\( variantName, variantDef ) ->
                                                                    case variantDef.is of
                                                                        Just t ->
                                                                            ( listPattern [ variantName |> toCamelCase |> stringPattern ], applyBinOp (apply [ jsonDecodeFun "field", string (variantName |> toCamelCase), typeDecoderVal namespace (apiDecodeModuleName namespace) t ]) piper (apply [ jsonDecodeFun "map", apiModelConstruct namespace (apiDecodeModuleName namespace) (typeName ++ variantName) [] ]) )

                                                                        Nothing ->
                                                                            ( listPattern [ variantName |> toCamelCase |> stringPattern ], apply [ jsonDecodeFun "succeed", apiModelConstruct namespace (apiDecodeModuleName namespace) (typeName ++ variantName) [] ] )
                                                                )
                                                         )
                                                            ++ [ ( namedPattern "_" [], apply [ jsonDecodeFun "fail", string ("Expected exactly one of: " ++ (typeDef.oneOf |> Dict.keys |> List.map toCamelCase |> String.join ", ") ++ ".") ] )
                                                               ]
                                                        )
                                                    )
                                                )
                                            ]
                                        )
                                    )
                                ]

                            CustomList typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    (Just (jsonDecodeType "Decoder" [ apiModelType namespace (apiDecodeModuleName namespace) typeName [] ]))
                                    (toCamelCase typeName)
                                    (apply [ jsonDecodeFun "list", typeDecoderVal namespace (apiDecodeModuleName namespace) typeDef.of_ ])
                                ]

                            CustomBool typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (apply [ jsonDecodeFun "bool" ])
                                ]

                            CustomString typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (apply [ jsonDecodeFun "string" ])
                                ]

                            CustomInt typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (apply [ jsonDecodeFun "int" ])
                                ]

                            CustomFloat typeName typeDef ->
                                [ valDecl
                                    Nothing
                                    Nothing
                                    (toCamelCase typeName)
                                    (apply [ jsonDecodeFun "float" ])
                                ]
                    )
                |> List.foldl (++) []
            )
            Nothing
    }


expectableTypes spec =
    spec.types
        |> List.map
            (\type_ ->
                case type_ of
                    CustomUrl _ typeDef ->
                        [ typeDef.methods.get
                            |> Maybe.map
                                (\expect ->
                                    case expect of
                                        CustomTypeRef expectTypeName ->
                                            Just expectTypeName

                                        _ ->
                                            Nothing
                                )
                        , typeDef.methods.post
                            |> Maybe.map
                                (\{ expect } ->
                                    case expect of
                                        CustomTypeRef expectTypeName ->
                                            Just expectTypeName

                                        _ ->
                                            Nothing
                                )
                        , typeDef.methods.put
                            |> Maybe.map
                                (\{ expect } ->
                                    case expect of
                                        CustomTypeRef expectTypeName ->
                                            Just expectTypeName

                                        _ ->
                                            Nothing
                                )
                        , typeDef.methods.delete
                            |> Maybe.map
                                (\expect ->
                                    case expect of
                                        CustomTypeRef expectTypeName ->
                                            Just expectTypeName

                                        _ ->
                                            Nothing
                                )
                        ]
                            |> List.filterMap (Maybe.andThen identity)

                    _ ->
                        []
            )
        |> List.foldl (++) []
        |> Set.fromList
        |> Set.toList


sendableTypes spec =
    spec.types
        |> List.map
            (\type_ ->
                case type_ of
                    CustomUrl _ typeDef ->
                        [ typeDef.methods.post
                            |> Maybe.map
                                (\{ accept } ->
                                    case accept of
                                        CustomTypeRef acceptTypeName ->
                                            Just acceptTypeName

                                        _ ->
                                            Nothing
                                )
                        , typeDef.methods.put
                            |> Maybe.map
                                (\{ accept } ->
                                    case accept of
                                        CustomTypeRef acceptTypeName ->
                                            Just acceptTypeName

                                        _ ->
                                            Nothing
                                )
                        ]
                            |> List.filterMap (Maybe.andThen identity)

                    _ ->
                        []
            )
        |> List.foldl (++) []
        |> Set.fromList
        |> Set.toList


customUrlToStringFunDecl namespace typeName typeDef =
    funDecl
        Nothing
        Nothing
        (toCamelCase typeName ++ "ToString")
        (urlParamsPatterns namespace (apiEncodeModuleName namespace) typeName typeDef.params)
        (apply
            [ fqFun [ "Url", "Interpolate" ] "interpolate"
            , if typeDef.params |> List.isEmpty then
                val "url"

              else
                val "template"
            , parens
                (if typeDef.params |> List.isEmpty then
                    fqVal [ "Dict" ] "empty"

                 else
                    apply
                        [ fqFun [ "Dict" ] "fromList"
                        , parens
                            (applyBinOp
                                (list
                                    (typeDef.params
                                        |> List.map
                                            (\param ->
                                                case param of
                                                    Spec.UrlParam name paramDef ->
                                                        if paramDef.optional then
                                                            applyBinOp (val name) piper (apply [ fqFun [ "Maybe" ] "map", parens (lambda [ varPattern "p" ] (tuple [ string name, val "p" ])) ])

                                                        else
                                                            construct "Just" [ tuple [ string name, val name ] ]
                                            )
                                    )
                                )
                                piper
                                (apply [ fqFun [ "List" ] "filterMap", fun "identity" ])
                            )
                        ]
                )
            ]
        )


expectTypeFun typeRef msg =
    case typeRef of
        CustomTypeRef typeName ->
            apply [ fun ("expect" ++ typeName), msg ]

        UrlTypeRef ->
            apply [ fqFun [ "Http" ] "expectJson", msg, fqFun [ "Json", "Decode" ] "string" ]

        StringTypeRef ->
            apply [ fqFun [ "Http" ] "expectJson", msg, fqFun [ "Json", "Decode" ] "string" ]

        IntTypeRef ->
            apply [ fqFun [ "Http" ] "expectJson", msg, fqFun [ "Json", "Decode" ] "int" ]

        FloatTypeRef ->
            apply [ fqFun [ "Http" ] "expectJson", msg, fqFun [ "Json", "Decode" ] "float" ]

        BoolTypeRef ->
            apply [ fqFun [ "Http" ] "expectJson", msg, fqFun [ "Json", "Decode" ] "bool" ]

        InstantTypeRef ->
            apply [ fqFun [ "Http" ] "expectJson", msg, fqFun [ "Json", "Decode" ] "string" ]


typeBodyFun typeRef msg =
    case typeRef of
        CustomTypeRef typeName ->
            binOpChain msg piper [ fun (toCamelCase typeName ++ "Body") ]

        UrlTypeRef ->
            binOpChain msg piper [ fqFun [ "Json", "Encode" ] "string", fqFun [ "Http" ] "jsonBody" ]

        StringTypeRef ->
            binOpChain msg piper [ fqFun [ "Json", "Encode" ] "string", fqFun [ "Http" ] "jsonBody" ]

        IntTypeRef ->
            binOpChain msg piper [ fqFun [ "Json", "Encode" ] "int", fqFun [ "Http" ] "jsonBody" ]

        FloatTypeRef ->
            binOpChain msg piper [ fqFun [ "Json", "Encode" ] "float", fqFun [ "Http" ] "jsonBody" ]

        BoolTypeRef ->
            binOpChain msg piper [ fqFun [ "Json", "Encode" ] "bool", fqFun [ "Http" ] "jsonBody" ]

        InstantTypeRef ->
            binOpChain msg piper [ fqFun [ "Json", "Encode" ] "string", fqFun [ "Http" ] "jsonBody" ]


httpFile namespace spec =
    { path = (namespace |> String.join "/") ++ "/Http.elm"
    , content =
        file
            (normalModule (apiHttpModuleName namespace) [])
            [ importApiModel namespace, importApiEncode namespace, importApiDecode namespace, importDict, importHttp, importJsonDecode, importJsonEncode, importUrlInterpolate, importUrl, importTask, importTime ]
            ((spec.types
                |> List.map
                    (\type_ ->
                        case type_ of
                            CustomUrl typeName typeDef ->
                                [ customUrlToStringFunDecl namespace typeName typeDef |> Just
                                , typeDef.methods.get
                                    |> Maybe.map
                                        (\expect ->
                                            funDecl Nothing
                                                Nothing
                                                ("get" ++ typeName)
                                                (if typeDef.params |> List.isEmpty then
                                                    [ varPattern "ok", varPattern "err", varPattern "url" ]

                                                 else
                                                    [ varPattern "params", varPattern "ok", varPattern "err", varPattern "url" ]
                                                )
                                                (record
                                                    [ ( "method", string "GET" )
                                                    , ( "url"
                                                      , apply
                                                            (if typeDef.params |> List.isEmpty then
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "url" ]

                                                             else
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "params", val "url" ]
                                                            )
                                                      )
                                                    , ( "body", fqVal [ "Http" ] "emptyBody" )
                                                    , ( "expect", expectTypeFun expect (parens (apply [ fun "reduceResponse", val "ok", val "err" ])) )
                                                    , ( "headers", list [ apply [ fqFun [ "Http" ] "header", string "Accept", string "application/json" ] ] )
                                                    , ( "timeout", val "Nothing" )
                                                    , ( "tracker", val "Nothing" )
                                                    ]
                                                )
                                        )
                                , typeDef.methods.post
                                    |> Maybe.map
                                        (\{ accept, expect } ->
                                            funDecl Nothing
                                                Nothing
                                                ("post" ++ typeName)
                                                ([ varPattern "body" ]
                                                    ++ (if typeDef.params |> List.isEmpty then
                                                            [ varPattern "ok", varPattern "err", varPattern "url" ]

                                                        else
                                                            [ varPattern "params", varPattern "ok", varPattern "err", varPattern "url" ]
                                                       )
                                                )
                                                (record
                                                    [ ( "method", string "POST" )
                                                    , ( "url"
                                                      , apply
                                                            (if typeDef.params |> List.isEmpty then
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "url" ]

                                                             else
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "params", val "url" ]
                                                            )
                                                      )
                                                    , ( "body", typeBodyFun accept (val "body") )
                                                    , ( "expect", expectTypeFun expect (parens (apply [ fun "reduceResponse", val "ok", val "err" ])) )
                                                    , ( "headers", list [ apply [ fqFun [ "Http" ] "header", string "Accept", string "application/json" ] ] )
                                                    , ( "timeout", val "Nothing" )
                                                    , ( "tracker", val "Nothing" )
                                                    ]
                                                )
                                        )
                                , typeDef.methods.put
                                    |> Maybe.map
                                        (\{ accept, expect } ->
                                            funDecl Nothing
                                                Nothing
                                                ("put" ++ typeName)
                                                ([ varPattern "body" ]
                                                    ++ (if typeDef.params |> List.isEmpty then
                                                            [ varPattern "ok", varPattern "err", varPattern "url" ]

                                                        else
                                                            [ varPattern "params", varPattern "ok", varPattern "err", varPattern "url" ]
                                                       )
                                                )
                                                (record
                                                    [ ( "method", string "PUT" )
                                                    , ( "url"
                                                      , apply
                                                            (if typeDef.params |> List.isEmpty then
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "url" ]

                                                             else
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "params", val "url" ]
                                                            )
                                                      )
                                                    , ( "body", typeBodyFun accept (val "body") )
                                                    , ( "expect", expectTypeFun expect (parens (apply [ fun "reduceResponse", val "ok", val "err" ])) )
                                                    , ( "headers", list [ apply [ fqFun [ "Http" ] "header", string "Accept", string "application/json" ] ] )
                                                    , ( "timeout", val "Nothing" )
                                                    , ( "tracker", val "Nothing" )
                                                    ]
                                                )
                                        )
                                , typeDef.methods.delete
                                    |> Maybe.map
                                        (\expect ->
                                            funDecl Nothing
                                                Nothing
                                                ("delete" ++ typeName)
                                                (if typeDef.params |> List.isEmpty then
                                                    [ varPattern "ok", varPattern "err", varPattern "url" ]

                                                 else
                                                    [ varPattern "params", varPattern "ok", varPattern "err", varPattern "url" ]
                                                )
                                                (record
                                                    [ ( "method", string "DELETE" )
                                                    , ( "url"
                                                      , apply
                                                            (if typeDef.params |> List.isEmpty then
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "url" ]

                                                             else
                                                                [ fun (toCamelCase typeName ++ "ToString"), val "params", val "url" ]
                                                            )
                                                      )
                                                    , ( "body", fqVal [ "Http" ] "emptyBody" )
                                                    , ( "expect", expectTypeFun expect (parens (apply [ fun "reduceResponse", val "ok", val "err" ])) )
                                                    , ( "headers", list [ apply [ fqFun [ "Http" ] "header", string "Accept", string "application/json" ] ] )
                                                    , ( "timeout", val "Nothing" )
                                                    , ( "tracker", val "Nothing" )
                                                    ]
                                                )
                                        )
                                ]
                                    |> List.filterMap identity

                            _ ->
                                []
                    )
                |> List.foldl (++) []
             )
                ++ (expectableTypes spec |> List.map (\type_ -> funDecl Nothing Nothing ("expect" ++ type_) [ varPattern "msg" ] (apply [ fqFun [ "Http" ] "expectJson", val "msg", typeDecoderVal namespace (apiHttpModuleName namespace) (CustomTypeRef type_) ])))
                ++ (sendableTypes spec |> List.map (\type_ -> funDecl Nothing Nothing (toCamelCase type_ ++ "Body") [ varPattern "body" ] (binOpChain (val "body") piper [ typeEncoderFun namespace (apiHttpModuleName namespace) (CustomTypeRef type_), fqFun [ "Http" ] "jsonBody" ])))
                ++ [ funDecl
                        Nothing
                        Nothing
                        "reduceResponse"
                        [ varPattern "ok", varPattern "err", varPattern "result" ]
                        (caseExpr (val "result") [ ( fqNamedPattern [ "Result" ] "Ok" [ varPattern "value" ], apply [ val "ok", val "value" ] ), ( fqNamedPattern [ "Result" ] "Err" [ varPattern "value" ], apply [ val "err", val "value" ] ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "headers"
                        [ varPattern "list", varPattern "req" ]
                        (updateRecord "req" [ ( "headers", applyBinOp (val "list") piper (apply [ fqFun [ "List" ] "map", lambda [ tuplePattern [ varPattern "k", varPattern "v" ] ] (apply [ fqFun [ "Http" ] "header", val "k", val "v" ]) ]) ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "header"
                        [ varPattern "key", varPattern "value", varPattern "req" ]
                        (updateRecord "req" [ ( "headers", applyBinOp (access (val "req") "headers") append (list [ apply [ fqFun [ "Http" ] "header", val "key", val "value" ] ]) ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "timeout"
                        [ varPattern "t", varPattern "req" ]
                        (updateRecord "req" [ ( "timeout", construct "Just" [ val "t" ] ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "noTimeout"
                        [ varPattern "req" ]
                        (updateRecord "req" [ ( "timeout", construct "Nothing" [] ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "tracker"
                        [ varPattern "name", varPattern "req" ]
                        (updateRecord "req" [ ( "tracker", construct "Just" [ val "name" ] ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "noTracker"
                        [ varPattern "name", varPattern "req" ]
                        (updateRecord "req" [ ( "tracker", construct "Nothing" [] ) ])
                   , funDecl
                        Nothing
                        Nothing
                        "request"
                        []
                        (fqFun [ "Http" ] "request")
                   , funDecl
                        Nothing
                        Nothing
                        "mock"
                        [ varPattern "res", varPattern "req" ]
                        (caseExpr
                            (applyBinOp (access (val "req") "url")
                                piper
                                (fqFun [ "Url" ] "fromString")
                            )
                            [ ( namedPattern "Just" [ varPattern "reqUrl" ], binOpChain (apply [ fun "res", val "reqUrl" ]) piper [ fqFun [ "Task" ] "succeed", apply [ fqFun [ "Task" ] "perform", val "identity" ] ] )
                            , ( namedPattern "Nothing" [], fqFun [ "Cmd" ] "none" )
                            ]
                        )
                   ]
            )
            Nothing
    }


specFiles : ModuleName -> Spec -> List File
specFiles namespace spec =
    [ modelFile namespace spec
    , encodeFile namespace spec
    , decodeFile namespace spec
    , httpFile namespace spec
    ]
