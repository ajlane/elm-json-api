module Generator exposing (..)

import Dict
import Elm.CodeGen exposing (..)
import Spec exposing (CustomFloatDefinition, Property(..), Spec, TypeName, TypeRef(..))


type alias FilePath =
    String


type alias File =
    { path : FilePath
    , content : Elm.CodeGen.File
    }


type alias Namespace =
    List String


filePath : Namespace -> Spec.TypeName -> FilePath
filePath namespace typeName =
    let
        fileName =
            typeName ++ ".elm"
    in
    if namespace |> List.isEmpty then
        fileName

    else
        namespace ++ [ fileName ] |> String.join "/"


generatedFileComment =
    Nothing


propertyAnn : ModuleName -> Spec.Property -> ( String, Elm.CodeGen.TypeAnnotation )
propertyAnn namespace property =
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
                maybeAnn (fqTyped (namespace ++ [ customProperty.is ]) customProperty.is [])

              else
                fqTyped (namespace ++ [ customProperty.is ]) customProperty.is []
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


importType : ModuleName -> TypeRef -> Maybe Import
importType namespace p =
    case p of
        InstantTypeRef ->
            importTime |> Just

        CustomTypeRef t ->
            importStmt (namespace ++ [ t ]) Nothing Nothing |> Just

        BoolTypeRef ->
            Nothing

        StringTypeRef ->
            Nothing

        IntTypeRef ->
            Nothing

        FloatTypeRef ->
            Nothing

        UrlTypeRef ->
            Nothing


importTypeRef : ModuleName -> TypeRef -> Maybe Import
importTypeRef namespace typeRef =
    case typeRef of
        InstantTypeRef ->
            importTime |> Just

        CustomTypeRef name ->
            importStmt (namespace ++ [ name ]) Nothing Nothing |> Just

        BoolTypeRef ->
            Nothing

        StringTypeRef ->
            Nothing

        IntTypeRef ->
            Nothing

        FloatTypeRef ->
            Nothing

        UrlTypeRef ->
            Nothing


importPropertyType : ModuleName -> Property -> Maybe Import
importPropertyType namespace p =
    case p of
        InstantProperty _ _ ->
            importTime |> Just

        CustomProperty _ def ->
            importStmt (namespace ++ [ def.is ]) Nothing Nothing |> Just

        BoolProperty _ _ ->
            Nothing

        StringProperty _ _ ->
            Nothing

        IntProperty _ _ ->
            Nothing

        FloatProperty _ _ ->
            Nothing

        UrlProperty _ _ ->
            Nothing


importPropertyTypes : ModuleName -> List Property -> List Import
importPropertyTypes namespace properties =
    properties
        |> List.filterMap (importPropertyType namespace)


customRecordFiles : List String -> String -> Spec.CustomRecordDefinition -> List File
customRecordFiles namespace typeName typeDef =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                (importPropertyTypes namespace typeDef.has ++ [ importJsonEncode, importJsonDecode ])
                [ aliasDecl (emptyDocComment |> markdown typeDef.description |> Just)
                    typeName
                    []
                    (recordAnn
                        (typeDef.has |> List.map (propertyAnn namespace))
                    )
                , funDecl
                    (emptyDocComment |> markdown ("Encodes " ++ typeName ++ " values as JSON.") |> Just)
                    (funAnn
                        (typed typeName [])
                        (fqTyped [ "Json", "Encode" ] "Value" [])
                        |> Just
                    )
                    "encode"
                    [ varPattern "record" ]
                    (apply
                        [ fqFun [ "Json", "Encode" ] "object"
                        , list
                            (typeDef.has
                                |> List.map
                                    (\p ->
                                        let
                                            encodedProperty propertyName propertyDefinition encoder =
                                                if propertyDefinition.optional then
                                                    tuple [ string propertyName, binOpChain (access (val "record") propertyName) piper [ apply [ fqFun [ "Maybe" ] "map", encoder ], apply [ fqFun [ "Maybe" ] "withDefault", fqVal [ "Json", "Encode" ] "null" ] ] ]

                                                else
                                                    tuple [ string propertyName, binOpChain (access (val "record") propertyName) piper [ apply [ encoder ] ] ]
                                        in
                                        case p of
                                            CustomProperty propertyName customPropertyDefinition ->
                                                encodedProperty propertyName customPropertyDefinition (typeEncoderVal namespace (CustomTypeRef customPropertyDefinition.is))

                                            BoolProperty propertyName boolPropertyDefinition ->
                                                encodedProperty propertyName boolPropertyDefinition (typeEncoderVal namespace BoolTypeRef)

                                            InstantProperty propertyName instantPropertyDefinition ->
                                                encodedProperty propertyName instantPropertyDefinition (typeEncoderVal namespace InstantTypeRef)

                                            StringProperty propertyName stringPropertyDefinition ->
                                                encodedProperty propertyName stringPropertyDefinition (typeEncoderVal namespace StringTypeRef)

                                            IntProperty propertyName intPropertyDefinition ->
                                                encodedProperty propertyName intPropertyDefinition (typeEncoderVal namespace IntTypeRef)

                                            FloatProperty propertyName floatPropertyDefinition ->
                                                encodedProperty propertyName floatPropertyDefinition (typeEncoderVal namespace FloatTypeRef)

                                            UrlProperty propertyName urlPropertyDefinition ->
                                                encodedProperty propertyName urlPropertyDefinition (typeEncoderVal namespace UrlTypeRef)
                                    )
                            )
                        ]
                    )
                , valDecl
                    (emptyDocComment |> markdown ("Decodes JSON as a " ++ typeName ++ " value.") |> Just)
                    (fqTyped [ "Json", "Decode" ] "Decoder" [ typed typeName [] ] |> Just)
                    "decoder"
                    (apply
                        ([ fqFun [ "Json", "Decode" ]
                            (if (typeDef.has |> List.length) > 1 then
                                "map" ++ (typeDef.has |> List.length |> String.fromInt)
                                -- TODO: Deal with records with even more properties

                             else
                                "map"
                             -- TODO: Deal with records with no properties
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
                                                                            [ ( namedPattern "Just" [ varPattern "_" ], applyBinOp (apply [ fqFun [ "Json", "Decode" ] "field", string name, typeDecoderVal namespace typeRef ]) piper (apply [ fqFun [ "Json", "Decode" ] "map", construct "Just" [] ]) )
                                                                            , ( namedPattern "Nothing" [], apply [ fqFun [ "Json", "Decode" ] "succeed", construct "Nothing" [] ] )
                                                                            ]
                                                                        )
                                                                    ]
                                                                ]
                                                            )

                                                    else
                                                        parens (apply [ fqFun [ "Json", "Decode" ] "field", string name, typeDecoderVal namespace typeRef ])
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
                generatedFileComment
      }
    ]


urlParamsPatterns : List Spec.UrlParam -> List Pattern
urlParamsPatterns params =
    if params |> List.isEmpty then
        [ varPattern "url" ]

    else
        [ params
            |> List.map
                (\param ->
                    case param of
                        Spec.UrlParam name _ ->
                            name
                )
            |> recordPattern
        , varPattern "template"
        ]


urlParamVal : Spec.UrlParam -> ( String, Expression )
urlParamVal param =
    case param of
        Spec.UrlParam name _ ->
            ( name, val name )


urlParamsVals : List Spec.UrlParam -> List Expression
urlParamsVals params =
    if params |> List.isEmpty then
        [ val "url" ]

    else
        [ params |> List.map urlParamVal |> record ]
            ++ [ val "template" ]


customUrlToUrlFunDecl : TypeName -> Spec.CustomUrlDefinition -> Declaration
customUrlToUrlFunDecl typeName customUrlDefinition =
    funDecl
        (emptyDocComment |> markdown ("Converts the given " ++ typeName ++ " into a URL string.") |> Just)
        ((if customUrlDefinition.params |> List.isEmpty then
            funAnn (typed typeName []) stringAnn

          else
            chainAnn [ typed (typeName ++ "Params") [], typed typeName [], stringAnn ]
         )
            |> Just
        )
        "toUrl"
        (urlParamsPatterns customUrlDefinition.params)
        (apply
            [ fqFun [ "Url", "Interpolate" ] "interpolate"
            , if customUrlDefinition.params |> List.isEmpty then
                val "url"

              else
                val "template"
            , parens
                (apply
                    [ fqFun [ "Dict" ] "fromList"
                    , list
                        (customUrlDefinition.params
                            |> List.map
                                (\param ->
                                    case param of
                                        Spec.UrlParam name _ ->
                                            tuple [ string name, val name ]
                                )
                        )
                    ]
                )
            ]
        )


customTypeEncoder : ModuleName -> TypeName -> Expression
customTypeEncoder namespace typeName =
    fqFun (namespace ++ [ typeName ]) "encode"


typeEncoderVal : ModuleName -> TypeRef -> Expression
typeEncoderVal namespace ref =
    case ref of
        CustomTypeRef name ->
            customTypeEncoder namespace name

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


typeDecoderVal : ModuleName -> TypeRef -> Expression
typeDecoderVal namespace ref =
    case ref of
        CustomTypeRef name ->
            fqFun (namespace ++ [ name ]) "decoder"

        UrlTypeRef ->
            fqFun [ "Json", "Decode" ] "string"

        StringTypeRef ->
            fqFun [ "Json", "Decode" ] "string"

        IntTypeRef ->
            fqFun [ "Json", "Decode" ] "int"

        FloatTypeRef ->
            fqFun [ "Json", "Decode" ] "float"

        BoolTypeRef ->
            fqFun [ "Json", "Decode" ] "bool"

        InstantTypeRef ->
            parens (apply [ fqFun [ "Json", "Decode" ] "map", fqFun [ "Time" ] "millisToPosix", fqFun [ "Json", "Decode" ] "int" ])


typeAnn : ModuleName -> TypeRef -> TypeAnnotation
typeAnn namespace ref =
    case ref of
        CustomTypeRef name ->
            fqTyped (namespace ++ [ name ]) name []

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


customUrlMetaAnn =
    extRecordAnn "a"
        [ ( "headers", listAnn (fqTyped [ "Http" ] "Header" []) )
        , ( "timeout", maybeAnn floatAnn )
        , ( "tracker", maybeAnn stringAnn )
        ]


httpErrorAnn =
    fqTyped [ "Http" ] "Error" []


customUrlParamsAnn typeName params =
    if params |> List.isEmpty then
        Nothing

    else
        typed (typeName ++ "Params") [] |> Just


cmdAnn var =
    typed "Cmd" [ typeVar var ]


customUrlGetFunDecl : ModuleName -> TypeName -> List Spec.UrlParam -> TypeRef -> List Declaration
customUrlGetFunDecl namespace typeName params expect =
    [ funDecl
        Nothing
        ([ funAnn (typeAnn namespace expect) (typeVar "msg") |> Just
         , funAnn httpErrorAnn (typeVar "msg") |> Just
         , maybeAnn customUrlMetaAnn |> Just
         , customUrlParamsAnn typeName params
         , typed typeName [] |> Just
         , cmdAnn "msg" |> Just
         ]
            |> List.filterMap identity
            |> chainAnn
            |> Just
        )
        "get"
        ([ varPattern "msg", varPattern "otherwise", varPattern "meta" ] ++ urlParamsPatterns params)
        (letExpr
            [ letFunction "msgOrOtherwise"
                [ varPattern "result" ]
                (caseExpr (val "result")
                    [ ( namedPattern "Ok" [ varPattern "value" ], apply [ fun "msg", val "value" ] )
                    , ( namedPattern "Err" [ varPattern "err" ], apply [ fun "otherwise", val "err" ] )
                    ]
                )
            ]
            (apply
                [ fqFun [ "Http" ] "request"
                , record
                    [ ( "method", string "GET" )
                    , ( "headers", applyBinOp (val "meta") piper (applyBinOp (apply [ fqFun [ "Maybe" ] "map", lambda [ varPattern "m" ] (access (val "m") "headers") ]) piper (apply [ fqFun [ "Maybe" ] "withDefault", list [] ])) )
                    , ( "url", apply ([ fun "toUrl" ] ++ urlParamsVals params) )
                    , ( "body", fqVal [ "Http" ] "emptyBody" )
                    , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "msgOrOtherwise", typeDecoderVal namespace expect ] )
                    , ( "timeout", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "timeout") ]) )
                    , ( "tracker", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "tracker") ]) )
                    ]
                ]
            )
        )
    ]


customUrlPostFunDecl : ModuleName -> TypeName -> List Spec.UrlParam -> TypeRef -> TypeRef -> List Declaration
customUrlPostFunDecl namespace typeName params accept expect =
    [ funDecl
        Nothing
        ([ funAnn (typeAnn namespace expect) (typeVar "msg") |> Just
         , funAnn httpErrorAnn (typeVar "msg") |> Just
         , maybeAnn customUrlMetaAnn |> Just
         , typeAnn namespace accept |> Just
         , customUrlParamsAnn typeName params
         , typed typeName [] |> Just
         , cmdAnn "msg" |> Just
         ]
            |> List.filterMap identity
            |> chainAnn
            |> Just
        )
        "post"
        ([ varPattern "msg", varPattern "otherwise", varPattern "meta", varPattern "body" ] ++ urlParamsPatterns params)
        (letExpr
            [ letFunction "msgOrOtherwise"
                [ varPattern "result" ]
                (caseExpr (val "result")
                    [ ( namedPattern "Ok" [ varPattern "value" ], apply [ fun "msg", val "value" ] )
                    , ( namedPattern "Err" [ varPattern "err" ], apply [ fun "otherwise", val "err" ] )
                    ]
                )
            ]
            (apply
                [ fqFun [ "Http" ] "request"
                , record
                    [ ( "method", string "POST" )
                    , ( "headers", applyBinOp (val "meta") piper (applyBinOp (apply [ fqFun [ "Maybe" ] "map", lambda [ varPattern "m" ] (access (val "m") "headers") ]) piper (apply [ fqFun [ "Maybe" ] "withDefault", list [] ])) )
                    , ( "url", apply ([ fun "toUrl" ] ++ urlParamsVals params) )
                    , ( "body", apply [ fqVal [ "Http" ] "jsonBody", parens (applyBinOp (val "body") piper (typeEncoderVal namespace accept)) ] )
                    , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "msgOrOtherwise", typeDecoderVal namespace expect ] )
                    , ( "timeout", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "timeout") ]) )
                    , ( "tracker", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "tracker") ]) )
                    ]
                ]
            )
        )
    ]


customUrlPutFunDecl : ModuleName -> TypeName -> List Spec.UrlParam -> TypeRef -> TypeRef -> List Declaration
customUrlPutFunDecl namespace typeName params accept expect =
    [ funDecl
        Nothing
        ([ funAnn (typeAnn namespace expect) (typeVar "msg") |> Just
         , funAnn httpErrorAnn (typeVar "msg") |> Just
         , maybeAnn customUrlMetaAnn |> Just
         , typeAnn namespace accept |> Just
         , customUrlParamsAnn typeName params
         , typed typeName [] |> Just
         , cmdAnn "msg" |> Just
         ]
            |> List.filterMap identity
            |> chainAnn
            |> Just
        )
        "put"
        ([ varPattern "msg", varPattern "otherwise", varPattern "meta", varPattern "body" ] ++ urlParamsPatterns params)
        (letExpr
            [ letFunction "msgOrOtherwise"
                [ varPattern "result" ]
                (caseExpr (val "result")
                    [ ( namedPattern "Ok" [ varPattern "value" ], apply [ fun "msg", val "value" ] )
                    , ( namedPattern "Err" [ varPattern "err" ], apply [ fun "otherwise", val "err" ] )
                    ]
                )
            ]
            (apply
                [ fqFun [ "Http" ] "request"
                , record
                    [ ( "method", string "PUT" )
                    , ( "headers", applyBinOp (val "meta") piper (applyBinOp (apply [ fqFun [ "Maybe" ] "map", lambda [ varPattern "m" ] (access (val "m") "headers") ]) piper (apply [ fqFun [ "Maybe" ] "withDefault", list [] ])) )
                    , ( "url", apply ([ fun "toUrl" ] ++ urlParamsVals params) )
                    , ( "body", apply [ fqVal [ "Http" ] "jsonBody", parens (applyBinOp (val "body") piper (typeEncoderVal namespace accept)) ] )
                    , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "msgOrOtherwise", typeDecoderVal namespace expect ] )
                    , ( "timeout", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "timeout") ]) )
                    , ( "tracker", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "tracker") ]) )
                    ]
                ]
            )
        )
    ]


customUrlDeleteFunDecl : ModuleName -> TypeName -> List Spec.UrlParam -> TypeRef -> List Declaration
customUrlDeleteFunDecl namespace typeName params expect =
    [ funDecl
        Nothing
        ([ funAnn (typeAnn namespace expect) (typeVar "msg") |> Just
         , funAnn httpErrorAnn (typeVar "msg") |> Just
         , maybeAnn customUrlMetaAnn |> Just
         , customUrlParamsAnn typeName params
         , typed typeName [] |> Just
         , cmdAnn "msg" |> Just
         ]
            |> List.filterMap identity
            |> chainAnn
            |> Just
        )
        "delete"
        ([ varPattern "msg", varPattern "otherwise", varPattern "meta" ] ++ urlParamsPatterns params)
        (letExpr
            [ letFunction "msgOrOtherwise"
                [ varPattern "result" ]
                (caseExpr (val "result")
                    [ ( namedPattern "Ok" [ varPattern "value" ], apply [ fun "msg", val "value" ] )
                    , ( namedPattern "Err" [ varPattern "err" ], apply [ fun "otherwise", val "err" ] )
                    ]
                )
            ]
            (apply
                [ fqFun [ "Http" ] "request"
                , record
                    [ ( "method", string "DELETE" )
                    , ( "headers", applyBinOp (val "meta") piper (applyBinOp (apply [ fqFun [ "Maybe" ] "map", lambda [ varPattern "m" ] (access (val "m") "headers") ]) piper (apply [ fqFun [ "Maybe" ] "withDefault", list [] ])) )
                    , ( "url", apply ([ fun "toUrl" ] ++ urlParamsVals params) )
                    , ( "body", fqVal [ "Http" ] "emptyBody" )
                    , ( "expect", apply [ fqFun [ "Http" ] "expectJson", val "msgOrOtherwise", typeDecoderVal namespace expect ] )
                    , ( "timeout", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "timeout") ]) )
                    , ( "tracker", applyBinOp (val "meta") piper (apply [ fqFun [ "Maybe" ] "andThen", lambda [ varPattern "m" ] (access (val "m") "tracker") ]) )
                    ]
                ]
            )
        )
    ]


customUrlFiles : ModuleName -> TypeName -> Spec.CustomUrlDefinition -> List File
customUrlFiles namespace typeName customUrlDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                ([ importDict, importUrlInterpolate, importHttp, importJsonEncode, importJsonDecode ]
                    ++ (customUrlDefinition.methods.get |> Maybe.andThen (\expect -> importType namespace expect) |> Maybe.map List.singleton |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.post |> Maybe.andThen (\{ accept, expect } -> importType namespace expect) |> Maybe.map List.singleton |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.put |> Maybe.andThen (\{ accept, expect } -> importType namespace expect) |> Maybe.map List.singleton |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.post |> Maybe.andThen (\{ accept, expect } -> importType namespace accept) |> Maybe.map List.singleton |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.put |> Maybe.andThen (\{ accept, expect } -> importType namespace accept) |> Maybe.map List.singleton |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.delete |> Maybe.andThen (\expect -> importType namespace expect) |> Maybe.map List.singleton |> Maybe.withDefault [])
                )
                ([ aliasDecl (emptyDocComment |> markdown customUrlDefinition.description |> Just)
                    typeName
                    []
                    stringAnn
                 ]
                    ++ (if customUrlDefinition.params |> List.isEmpty then
                            []

                        else
                            [ aliasDecl (emptyDocComment |> markdown ("Template parameters for interpolating a " ++ typeName) |> Just)
                                (typeName ++ "Params")
                                []
                                (recordAnn
                                    (customUrlDefinition.params
                                        |> List.map
                                            (\param ->
                                                case param of
                                                    Spec.UrlParam name _ ->
                                                        ( name, stringAnn )
                                            )
                                    )
                                )
                            ]
                       )
                    ++ [ customUrlToUrlFunDecl typeName customUrlDefinition
                       ]
                    ++ (customUrlDefinition.methods.get |> Maybe.map (\expect -> customUrlGetFunDecl namespace typeName customUrlDefinition.params expect) |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.put |> Maybe.map (\{ accept, expect } -> customUrlPutFunDecl namespace typeName customUrlDefinition.params accept expect) |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.post |> Maybe.map (\{ accept, expect } -> customUrlPostFunDecl namespace typeName customUrlDefinition.params accept expect) |> Maybe.withDefault [])
                    ++ (customUrlDefinition.methods.delete |> Maybe.map (\expect -> customUrlDeleteFunDecl namespace typeName customUrlDefinition.params expect) |> Maybe.withDefault [])
                    ++ [ valDecl
                            (emptyDocComment |> markdown ("Encodes " ++ typeName ++ " values as JSON.") |> Just)
                            (funAnn
                                (typed typeName [])
                                (fqTyped [ "Json", "Encode" ] "Value" [])
                                |> Just
                            )
                            "encode"
                            (fqFun [ "Json", "Encode" ] "string")
                       , valDecl
                            (emptyDocComment |> markdown ("Decodes JSON as a " ++ typeName ++ " value.") |> Just)
                            (fqTyped [ "Json", "Decode" ] "Decoder" [ typed typeName [] ] |> Just)
                            "decoder"
                            (fqFun [ "Json", "Decode" ] "string")
                       ]
                )
                generatedFileComment
      }
    ]


customStringFiles : ModuleName -> TypeName -> Spec.CustomStringDefinition -> List File
customStringFiles namespace typeName customStringDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                []
                [ aliasDecl (emptyDocComment |> markdown customStringDefinition.description |> Just)
                    typeName
                    []
                    stringAnn
                ]
                generatedFileComment
      }
    ]


customIntFiles : ModuleName -> TypeName -> Spec.CustomIntDefinition -> List File
customIntFiles namespace typeName customIntDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                []
                [ aliasDecl (emptyDocComment |> markdown customIntDefinition.description |> Just)
                    typeName
                    []
                    intAnn
                ]
                generatedFileComment
      }
    ]


customFloatFiles : ModuleName -> TypeName -> CustomFloatDefinition -> List File
customFloatFiles namespace typeName customFloatDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                []
                [ aliasDecl (emptyDocComment |> markdown customFloatDefinition.description |> Just)
                    typeName
                    []
                    floatAnn
                ]
                generatedFileComment
      }
    ]


customBoolFiles : ModuleName -> TypeName -> Spec.CustomBoolDefinition -> List File
customBoolFiles namespace typeName customBoolDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                []
                [ aliasDecl (emptyDocComment |> markdown customBoolDefinition.description |> Just)
                    typeName
                    []
                    boolAnn
                ]
                generatedFileComment
      }
    ]


customEnumFiles : ModuleName -> TypeName -> Spec.CustomEnumDefinition -> List File
customEnumFiles namespace typeName customEnumDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                (customEnumDefinition.oneOf |> Dict.values |> List.filterMap (\v -> v.is |> Maybe.andThen (importTypeRef namespace)))
                [ customTypeDecl (emptyDocComment |> markdown customEnumDefinition.description |> Just)
                    typeName
                    []
                    (customEnumDefinition.oneOf
                        |> Dict.toList
                        |> List.map
                            (\( name, variantDefinition ) ->
                                ( name
                                , variantDefinition.is |> Maybe.map (\t -> [ typeAnn namespace t ]) |> Maybe.withDefault []
                                )
                            )
                    )
                ]
                generatedFileComment
      }
    ]


customListFiles : ModuleName -> TypeName -> Spec.CustomListDefinition -> List File
customListFiles namespace typeName customListDefinition =
    [ { path = filePath namespace typeName
      , content =
            file
                (normalModule (namespace ++ [ typeName ]) [])
                []
                [ aliasDecl (emptyDocComment |> markdown customListDefinition.description |> Just)
                    typeName
                    []
                    (listAnn (typeVar "a"))
                ]
                generatedFileComment
      }
    ]


customTypeFiles : ModuleName -> Spec.CustomType -> List File
customTypeFiles namespace typeSpec =
    case typeSpec of
        Spec.CustomRecord typeName definition ->
            customRecordFiles namespace typeName definition

        Spec.CustomUrl typeName definition ->
            customUrlFiles namespace typeName definition

        Spec.CustomString typeName definition ->
            customStringFiles namespace typeName definition

        Spec.CustomInt typeName definition ->
            customIntFiles namespace typeName definition

        Spec.CustomFloat typeName definition ->
            customFloatFiles namespace typeName definition

        Spec.CustomBool typeName definition ->
            customBoolFiles namespace typeName definition

        Spec.CustomEnum typeName definition ->
            customEnumFiles namespace typeName definition

        Spec.CustomList typeName definition ->
            customListFiles namespace typeName definition


specFiles : ModuleName -> Spec -> List File
specFiles namespace apiSpec =
    apiSpec.types
        |> List.map (customTypeFiles namespace)
        |> List.foldl (++) []
