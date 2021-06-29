module Generator exposing (..)

import Dict
import Elm.CodeGen exposing (..)
import Elm.Syntax.Expression exposing (Expression(..))
import Elm.Syntax.Node exposing (Node(..))
import Elm.Syntax.Range exposing (emptyRange)
import Spec exposing (CustomFloatDefinition, CustomType(..), Property(..), Spec, TypeName, TypeRef(..))


type alias FilePath =
    String


type alias File =
    { path : FilePath
    , content : Elm.CodeGen.File
    }


type alias Namespace =
    List String


propertyAnn : Spec.Property -> ( String, Elm.CodeGen.TypeAnnotation )
propertyAnn property =
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
                maybeAnn (typed customProperty.is [])

              else
                typed customProperty.is []
            )


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


importUrlInterpolate : Import
importUrlInterpolate =
    importStmt [ "Url", "Interpolate" ] Nothing Nothing


urlParamsPatterns : TypeName -> List Spec.UrlParam -> List Pattern
urlParamsPatterns typeName params =
    if params |> List.isEmpty then
        [ namedPattern typeName [ varPattern "url" ] ]

    else
        [ params
            |> List.map
                (\param ->
                    case param of
                        Spec.UrlParam name _ ->
                            name
                )
            |> recordPattern
        , namedPattern typeName [ varPattern "template" ]
        ]


typeEncoderFun : TypeRef -> Expression
typeEncoderFun ref =
    case ref of
        CustomTypeRef name ->
            fun ("encode" ++ name)

        UrlTypeRef ->
            fqFun [ "Json", "Encode" ] "string"

        StringTypeRef ->
            fqFun [ "Json", "Encode" ] "string"

        IntTypeRef ->
            fqFun [ "Json", "Encode" ] "int"

        FloatTypeRef ->
            fqFun [ "Json", "Encode" ] "float"

        BoolTypeRef ->
            fqFun [ "Json", "Encode" ] "bool"

        InstantTypeRef ->
            parens (applyBinOp (fqFun [ "Time" ] "posixToMillis") composer (fqFun [ "Json", "Encode" ] "int"))


typeDecoderVal : TypeRef -> Expression
typeDecoderVal ref =
    case ref of
        CustomTypeRef name ->
            val ("decode" ++ name)

        UrlTypeRef ->
            fqVal [ "Json", "Decode" ] "string"

        StringTypeRef ->
            fqVal [ "Json", "Decode" ] "string"

        IntTypeRef ->
            fqVal [ "Json", "Decode" ] "int"

        FloatTypeRef ->
            fqVal [ "Json", "Decode" ] "float"

        BoolTypeRef ->
            fqVal [ "Json", "Decode" ] "bool"

        InstantTypeRef ->
            parens (apply [ fqFun [ "Json", "Decode" ] "map", fqFun [ "Time" ] "millisToPosix", fqFun [ "Json", "Decode" ] "int" ])


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


specFiles : ModuleName -> Spec -> List File
specFiles namespace spec =
    [ { path = (namespace |> String.join "/") ++ ".elm"
      , content =
            file
                (normalModule namespace [])
                [ importDict, importHttp, importJsonDecode, importJsonEncode, importUrlInterpolate, importTime ]
                ((spec.types
                    |> List.map
                        (\type_ ->
                            case type_ of
                                CustomUrl typeName typeDef ->
                                    [ customTypeDecl
                                        (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        (List.singleton ( typeName, [ stringAnn ] ))
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("with" ++ typeName)
                                        (urlParamsPatterns typeName typeDef.params)
                                        (record
                                            ([ typeDef.methods.get |> Maybe.map (\expect -> [ ( "get", typeDecoderVal expect ) ]) |> Maybe.withDefault []
                                             , typeDef.methods.post |> Maybe.map (\{ accept, expect } -> [ ( "post", record [ ( "accept", typeEncoderFun accept ), ( "expect", typeDecoderVal expect ) ] ) ]) |> Maybe.withDefault []
                                             , typeDef.methods.put |> Maybe.map (\{ accept, expect } -> [ ( "put", record [ ( "accept", typeEncoderFun accept ), ( "expect", typeDecoderVal expect ) ] ) ]) |> Maybe.withDefault []
                                             , typeDef.methods.delete |> Maybe.map (\expect -> [ ( "delete", typeDecoderVal expect ) ]) |> Maybe.withDefault []
                                             , [ ( "headers", list [] )
                                               , ( "url"
                                                 , apply
                                                    [ fqFun [ "Url", "Interpolate" ] "interpolate"
                                                    , if typeDef.params |> List.isEmpty then
                                                        val "url"

                                                      else
                                                        val "template"
                                                    , parens
                                                        (apply
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
                                               , ( "timeout", construct "Nothing" [] )
                                               , ( "tracker", construct "Nothing" [] )
                                               ]
                                             ]
                                                |> List.foldl (++) []
                                            )
                                        )
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        [ namedPattern typeName
                                            [ if (typeDef.params |> List.length) > 1 then
                                                varPattern "template"

                                              else
                                                varPattern "url"
                                            ]
                                        ]
                                        (apply
                                            [ fqFun [ "Json", "Encode" ] "string"
                                            , if (typeDef.params |> List.length) > 1 then
                                                val "template"

                                              else
                                                val "url"
                                            ]
                                        )
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("decode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Decode" ] "map", construct typeName [], fqFun [ "Json", "Decode" ] "string" ])
                                    ]

                                CustomRecord typeName typeDef ->
                                    [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        (recordAnn
                                            (typeDef.has |> List.map propertyAnn)
                                        )
                                    , funDecl
                                        (emptyDocComment |> markdown ("Encodes " ++ typeName ++ " values as JSON.") |> Just)
                                        (funAnn
                                            (typed typeName [])
                                            (fqTyped [ "Json", "Encode" ] "Value" [])
                                            |> Just
                                        )
                                        ("encode" ++ typeName)
                                        [ varPattern "record" ]
                                        (apply
                                            [ fqFun [ "Json", "Encode" ] "object"
                                            , parens
                                                (applyBinOp
                                                    (list
                                                        (typeDef.has
                                                            |> List.map
                                                                (\p ->
                                                                    let
                                                                        encodedProperty propertyName propertyDefinition encoder =
                                                                            if propertyDefinition.optional then
                                                                                applyBinOp (access (val "record") propertyName) piper (apply [ fqFun [ "Maybe" ] "map", lambda [ varPattern "v" ] (tuple [ string propertyName, apply [ encoder, val "v" ] ]) ])

                                                                            else
                                                                                construct "Just" [ parens (tuple [ string propertyName, binOpChain (access (val "record") propertyName) piper [ apply [ encoder ] ] ]) ]
                                                                    in
                                                                    case p of
                                                                        CustomProperty propertyName customPropertyDefinition ->
                                                                            encodedProperty propertyName customPropertyDefinition (typeEncoderFun (CustomTypeRef customPropertyDefinition.is))

                                                                        BoolProperty propertyName boolPropertyDefinition ->
                                                                            encodedProperty propertyName boolPropertyDefinition (typeEncoderFun BoolTypeRef)

                                                                        InstantProperty propertyName instantPropertyDefinition ->
                                                                            encodedProperty propertyName instantPropertyDefinition (typeEncoderFun InstantTypeRef)

                                                                        StringProperty propertyName stringPropertyDefinition ->
                                                                            encodedProperty propertyName stringPropertyDefinition (typeEncoderFun StringTypeRef)

                                                                        IntProperty propertyName intPropertyDefinition ->
                                                                            encodedProperty propertyName intPropertyDefinition (typeEncoderFun IntTypeRef)

                                                                        FloatProperty propertyName floatPropertyDefinition ->
                                                                            encodedProperty propertyName floatPropertyDefinition (typeEncoderFun FloatTypeRef)

                                                                        UrlProperty propertyName urlPropertyDefinition ->
                                                                            encodedProperty propertyName urlPropertyDefinition (typeEncoderFun UrlTypeRef)
                                                                )
                                                        )
                                                    )
                                                    piper
                                                    (apply [ fqFun [ "List" ] "filterMap", val "identity" ])
                                                )
                                            ]
                                        )
                                    , valDecl
                                        (emptyDocComment |> markdown ("Decodes JSON as " ++ typeName ++ " values.") |> Just)
                                        (fqTyped [ "Json", "Decode" ] "Decoder" [ typed typeName [] ] |> Just)
                                        ("decode" ++ typeName)
                                        (apply
                                            ([ fqFun [ "Json", "Decode" ]
                                                (if (typeDef.has |> List.length) > 1 then
                                                    "map" ++ (typeDef.has |> List.length |> String.fromInt)
                                                    -- TODO: Deal with records with even more properties

                                                 else
                                                    "map"
                                                )
                                             , construct typeName []
                                             ]
                                                ++ (typeDef.has
                                                        |> List.map
                                                            (\p ->
                                                                let
                                                                    fieldDecoder name optional typeRef =
                                                                        if optional then
                                                                            parens
                                                                                (binOpChain
                                                                                    (apply [ fqFun [ "Json", "Decode" ] "field", string name, parens (apply [ fqFun [ "Json", "Decode" ] "succeed", unit ]) ])
                                                                                    piper
                                                                                    [ fqFun [ "Json", "Decode" ] "maybe"
                                                                                    , apply
                                                                                        [ fqFun [ "Json", "Decode" ] "andThen"
                                                                                        , lambda [ varPattern "m" ]
                                                                                            (caseExpr (val "m")
                                                                                                [ ( namedPattern "Just" [ varPattern "_" ], applyBinOp (apply [ fqFun [ "Json", "Decode" ] "field", string name, typeDecoderVal typeRef ]) piper (apply [ fqFun [ "Json", "Decode" ] "map", construct "Just" [] ]) )
                                                                                                , ( namedPattern "Nothing" [], apply [ fqFun [ "Json", "Decode" ] "succeed", construct "Nothing" [] ] )
                                                                                                ]
                                                                                            )
                                                                                        ]
                                                                                    ]
                                                                                )

                                                                        else
                                                                            parens (apply [ fqFun [ "Json", "Decode" ] "field", string name, typeDecoderVal typeRef ])
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
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        [ varPattern "value" ]
                                        (caseExpr (val "value")
                                            (typeDef.oneOf
                                                |> Dict.toList
                                                |> List.map
                                                    (\( variantName, variantDef ) ->
                                                        ( namedPattern (typeName ++ variantName)
                                                            (case variantDef.is of
                                                                Just _ ->
                                                                    [ varPattern "v" ]

                                                                Nothing ->
                                                                    []
                                                            )
                                                        , apply
                                                            [ fqFun [ "Json", "Encode" ] "object"
                                                            , list
                                                                [ tuple
                                                                    [ string (variantName |> toCamelCase)
                                                                    , case variantDef.is of
                                                                        Just variantType ->
                                                                            applyBinOp (val "v") piper (typeEncoderFun variantType)

                                                                        Nothing ->
                                                                            apply [ fqFun [ "Json", "Encode" ] "object", list [] ]
                                                                    ]
                                                                ]
                                                            ]
                                                        )
                                                    )
                                            )
                                        )
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("decode" ++ typeName)
                                        (applyBinOp
                                            (apply [ fqFun [ "Json", "Decode" ] "dict", parens (apply [ fqFun [ "Json", "Decode" ] "succeed", unit ]) ])
                                            piper
                                            (apply
                                                [ fqFun [ "Json", "Decode" ] "andThen"
                                                , parens
                                                    (lambda [ varPattern "dict" ]
                                                        (caseExpr (applyBinOp (val "dict") piper (fqFun [ "Dict" ] "keys"))
                                                            ((typeDef.oneOf
                                                                |> Dict.toList
                                                                |> List.map
                                                                    (\( variantName, variantDef ) ->
                                                                        case variantDef.is of
                                                                            Just t ->
                                                                                ( listPattern [ variantName |> toCamelCase |> stringPattern ], applyBinOp (apply [ fqFun [ "Json", "Decode" ] "field", string (variantName |> toCamelCase), typeDecoderVal t ]) piper (apply [ fqFun [ "Json", "Decode" ] "map", construct (typeName ++ variantName) [] ]) )

                                                                            Nothing ->
                                                                                ( listPattern [ variantName |> toCamelCase |> stringPattern ], apply [ fqFun [ "Json", "Decode" ] "succeed", construct (typeName ++ variantName) [] ] )
                                                                    )
                                                             )
                                                                ++ [ ( namedPattern "_" [], apply [ fqFun [ "Json", "Decode" ] "fail", string ("Expected exactly one of: " ++ (typeDef.oneOf |> Dict.keys |> List.map toCamelCase |> String.join ", ") ++ ".") ] )
                                                                   ]
                                                            )
                                                        )
                                                    )
                                                ]
                                            )
                                        )
                                    ]

                                CustomList typeName typeDef ->
                                    [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        (listAnn (typeAnn typeDef.of_))
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Encode" ] "list", typeEncoderFun typeDef.of_ ])
                                    , valDecl
                                        Nothing
                                        (Just (fqTyped [ "Json", "Decode" ] "Decoder" [ typed typeName [] ]))
                                        ("decode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Decode" ] "list", typeDecoderVal typeDef.of_ ])
                                    ]

                                CustomBool typeName typeDef ->
                                    [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        boolAnn
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        [ varPattern "value" ]
                                        (apply [ fqFun [ "Json", "Encode" ] "bool" ])
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("decode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Decode" ] "bool" ])
                                    ]

                                CustomString typeName typeDef ->
                                    [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        stringAnn
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        [ varPattern "value" ]
                                        (apply [ fqFun [ "Json", "Encode" ] "string" ])
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("decode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Decode" ] "string" ])
                                    ]

                                CustomInt typeName typeDef ->
                                    [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        intAnn
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        [ varPattern "value" ]
                                        (apply [ fqFun [ "Json", "Encode" ] "int" ])
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("decode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Decode" ] "int" ])
                                    ]

                                CustomFloat typeName typeDef ->
                                    [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                                        typeName
                                        []
                                        floatAnn
                                    , funDecl
                                        Nothing
                                        Nothing
                                        ("encode" ++ typeName)
                                        [ varPattern "value" ]
                                        (apply [ fqFun [ "Json", "Encode" ] "float" ])
                                    , valDecl
                                        Nothing
                                        Nothing
                                        ("decode" ++ typeName)
                                        (apply [ fqFun [ "Json", "Decode" ] "float" ])
                                    ]
                        )
                    |> List.foldl (++) []
                 )
                    ++ [ funDecl
                            Nothing
                            Nothing
                            "headers"
                            [ varPattern "list", varPattern "request" ]
                            (updateRecord "request" [ ( "headers", applyBinOp (val "list") piper (apply [ fqFun [ "List" ] "map", lambda [ tuplePattern [ varPattern "k", varPattern "v" ] ] (apply [ fqFun [ "Http" ] "header", val "k", val "v" ]) ]) ) ])
                       , funDecl
                            Nothing
                            Nothing
                            "header"
                            [ varPattern "key", varPattern "value", varPattern "request" ]
                            (updateRecord "request" [ ( "headers", applyBinOp (access (val "request") "headers") append (list [ apply [ fqFun [ "Http" ] "header", val "key", val "value" ] ]) ) ])
                       , funDecl
                            Nothing
                            Nothing
                            "timeout"
                            [ varPattern "t", varPattern "request" ]
                            (updateRecord "request" [ ( "timeout", construct "Just" [ val "t" ] ) ])
                       , funDecl
                            Nothing
                            Nothing
                            "noTimeout"
                            [ varPattern "request" ]
                            (updateRecord "request" [ ( "timeout", construct "Nothing" [] ) ])
                       , funDecl
                            Nothing
                            Nothing
                            "tracker"
                            [ varPattern "name", varPattern "request" ]
                            (updateRecord "request" [ ( "tracker", construct "Just" [ val "name" ] ) ])
                       , funDecl
                            Nothing
                            Nothing
                            "noTracker"
                            [ varPattern "name", varPattern "request" ]
                            (updateRecord "request" [ ( "tracker", construct "Nothing" [] ) ])
                       , funDecl
                            Nothing
                            Nothing
                            "get"
                            [ varPattern "ok", varPattern "err", varPattern "request" ]
                            (letExpr [ letFunction "toMsg" [ varPattern "r" ] (caseExpr (val "r") [ ( fqNamedPattern [ "Result" ] "Ok" [ varPattern "value" ], apply [ fun "ok", val "value" ] ), ( fqNamedPattern [ "Result" ] "Err" [ varPattern "value" ], apply [ fun "err", val "value" ] ) ]) ]
                                (apply
                                    [ fqFun [ "Http" ] "request"
                                    , record
                                        [ ( "method", string "GET" )
                                        , ( "headers", access (val "request") "headers" )
                                        , ( "url", access (val "request") "url" )
                                        , ( "body", apply [ fqFun [ "Http" ] "emptyBody" ] )
                                        , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "toMsg", access (val "request") "get" ] )
                                        , ( "timeout", access (val "request") "timeout" )
                                        , ( "tracker", access (val "request") "tracker" )
                                        ]
                                    ]
                                )
                            )
                       , funDecl
                            Nothing
                            Nothing
                            "put"
                            [ varPattern "ok", varPattern "err", varPattern "request" ]
                            (letExpr [ letFunction "toMsg" [ varPattern "r" ] (caseExpr (val "r") [ ( fqNamedPattern [ "Result" ] "Ok" [ varPattern "value" ], apply [ fun "ok", val "value" ] ), ( fqNamedPattern [ "Result" ] "Err" [ varPattern "value" ], apply [ fun "err", val "value" ] ) ]) ]
                                (apply
                                    [ fqFun [ "Http" ] "request"
                                    , record
                                        [ ( "method", string "PUT" )
                                        , ( "headers", access (val "request") "headers" )
                                        , ( "url", access (val "request") "url" )
                                        , ( "body", access (access (val "request") "put") "accept" )
                                        , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "toMsg", access (access (val "request") "put") "expect" ] )
                                        , ( "timeout", access (val "request") "timeout" )
                                        , ( "tracker", access (val "request") "tracker" )
                                        ]
                                    ]
                                )
                            )
                       , funDecl
                            Nothing
                            Nothing
                            "post"
                            [ varPattern "ok", varPattern "err", varPattern "request" ]
                            (letExpr [ letFunction "toMsg" [ varPattern "r" ] (caseExpr (val "r") [ ( fqNamedPattern [ "Result" ] "Ok" [ varPattern "value" ], apply [ fun "ok", val "value" ] ), ( fqNamedPattern [ "Result" ] "Err" [ varPattern "value" ], apply [ fun "err", val "value" ] ) ]) ]
                                (apply
                                    [ fqFun [ "Http" ] "request"
                                    , record
                                        [ ( "method", string "POST" )
                                        , ( "headers", access (val "request") "headers" )
                                        , ( "url", access (val "request") "url" )
                                        , ( "body", access (access (val "request") "post") "accept" )
                                        , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "toMsg", access (access (val "request") "post") "expect" ] )
                                        , ( "timeout", access (val "request") "timeout" )
                                        , ( "tracker", access (val "request") "tracker" )
                                        ]
                                    ]
                                )
                            )
                       , funDecl
                            Nothing
                            Nothing
                            "delete"
                            [ varPattern "ok", varPattern "err", varPattern "request" ]
                            (letExpr [ letFunction "toMsg" [ varPattern "r" ] (caseExpr (val "r") [ ( fqNamedPattern [ "Result" ] "Ok" [ varPattern "value" ], apply [ fun "ok", val "value" ] ), ( fqNamedPattern [ "Result" ] "Err" [ varPattern "value" ], apply [ fun "err", val "value" ] ) ]) ]
                                (apply
                                    [ fqFun [ "Http" ] "request"
                                    , record
                                        [ ( "method", string "DELETE" )
                                        , ( "headers", access (val "request") "headers" )
                                        , ( "url", access (val "request") "url" )
                                        , ( "body", apply [ fqFun [ "Http" ] "emptyBody" ] )
                                        , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "toMsg", access (val "request") "delete" ] )
                                        , ( "timeout", access (val "request") "timeout" )
                                        , ( "tracker", access (val "request") "tracker" )
                                        ]
                                    ]
                                )
                            )
                       ]
                )
                Nothing
      }
    ]
