module Spec exposing (CustomBoolDefinition, CustomEnumDefinition, CustomFloatDefinition, CustomIntDefinition, CustomListDefinition, CustomRecordDefinition, CustomStringDefinition, CustomType(..), CustomUrlDefinition, Property(..), PropertyName, Spec, TypeName, TypeRef(..), UrlParam(..), defaultMethods, specDecoder)

import Dict exposing (Dict)
import Json.Decode


optionalField name decoder =
    Json.Decode.field name (Json.Decode.succeed ())
        |> Json.Decode.maybe
        |> Json.Decode.andThen
            (\m ->
                case m of
                    Just _ ->
                        Json.Decode.field name decoder |> Json.Decode.map Just

                    Nothing ->
                        Json.Decode.succeed Nothing
            )


type alias TypeName =
    String


typeNameDecoder : Json.Decode.Decoder TypeName
typeNameDecoder =
    Json.Decode.string


type TypeRef
    = CustomTypeRef TypeName
    | UrlTypeRef
    | StringTypeRef
    | IntTypeRef
    | FloatTypeRef
    | BoolTypeRef
    | InstantTypeRef


typeRefDecoder : List TypeName -> Json.Decode.Decoder TypeRef
typeRefDecoder customTypeNames =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case s of
                    "String" ->
                        Json.Decode.succeed StringTypeRef

                    "Url" ->
                        Json.Decode.succeed UrlTypeRef

                    "Int" ->
                        Json.Decode.succeed IntTypeRef

                    "Float" ->
                        Json.Decode.succeed FloatTypeRef

                    "Bool" ->
                        Json.Decode.succeed BoolTypeRef

                    "Instant" ->
                        Json.Decode.succeed InstantTypeRef

                    name ->
                        if customTypeNames |> List.any ((==) name) then
                            Json.Decode.succeed (CustomTypeRef name)

                        else
                            Json.Decode.fail (name ++ " is not one of String, Url, Int, Float, Bool, Instant, " ++ (customTypeNames |> String.join ", ") ++ ".")
            )


type alias VariantName =
    String


type alias Variant =
    { is : Maybe TypeRef
    , description : String
    }


variantDecoder : List TypeName -> Json.Decode.Decoder Variant
variantDecoder customTypeNames =
    Json.Decode.map2 Variant
        (optionalField "is" (typeRefDecoder customTypeNames))
        (Json.Decode.field "description" Json.Decode.string)


type alias PropertyName =
    String


type alias BoolPropertyDefinition =
    { optional : Bool
    , description : String
    }


boolPropertyDefinitionDecoder : Json.Decode.Decoder BoolPropertyDefinition
boolPropertyDefinitionDecoder =
    Json.Decode.map2 BoolPropertyDefinition
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type alias InstantPropertyDefinition =
    { optional : Bool
    , description : String
    }


instantPropertyDefinitionDecoder : Json.Decode.Decoder InstantPropertyDefinition
instantPropertyDefinitionDecoder =
    Json.Decode.map2 InstantPropertyDefinition
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type alias StringPropertyDefinition =
    { optional : Bool
    , description : String
    }


stringPropertyDefinitionDecoder : Json.Decode.Decoder StringPropertyDefinition
stringPropertyDefinitionDecoder =
    Json.Decode.map2 StringPropertyDefinition
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type alias IntPropertyDefinition =
    { optional : Bool
    , description : String
    }


intPropertyDefinitionDecoder : Json.Decode.Decoder IntPropertyDefinition
intPropertyDefinitionDecoder =
    Json.Decode.map2 IntPropertyDefinition
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type alias FloatPropertyDefinition =
    { optional : Bool
    , description : String
    }


floatPropertyDefinitionDecoder : Json.Decode.Decoder FloatPropertyDefinition
floatPropertyDefinitionDecoder =
    Json.Decode.map2 FloatPropertyDefinition
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type alias UrlPropertyDefinition =
    { optional : Bool
    , description : String
    }


urlPropertyDefinitionDecoder : Json.Decode.Decoder UrlPropertyDefinition
urlPropertyDefinitionDecoder =
    Json.Decode.map2 UrlPropertyDefinition
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type alias CustomPropertyDefinition =
    { is : TypeName
    , optional : Bool
    , description : String
    }


customPropertyDefinitionDecoder : Json.Decode.Decoder CustomPropertyDefinition
customPropertyDefinitionDecoder =
    Json.Decode.map3 CustomPropertyDefinition
        (Json.Decode.field "is" typeNameDecoder)
        (optionalField "optional" Json.Decode.bool |> Json.Decode.map (Maybe.withDefault False))
        (Json.Decode.field "description" Json.Decode.string)


type UnnamedProperty
    = UnnamedBoolProperty BoolPropertyDefinition
    | UnnamedInstantProperty InstantPropertyDefinition
    | UnnamedStringProperty StringPropertyDefinition
    | UnnamedIntProperty IntPropertyDefinition
    | UnnamedFloatProperty FloatPropertyDefinition
    | UnnamedUrlProperty UrlPropertyDefinition
    | UnnamedCustomProperty CustomPropertyDefinition


nameUnnamedProperty ( name, property ) =
    case property of
        UnnamedBoolProperty definition ->
            BoolProperty name definition

        UnnamedInstantProperty definition ->
            InstantProperty name definition

        UnnamedStringProperty definition ->
            StringProperty name definition

        UnnamedIntProperty definition ->
            IntProperty name definition

        UnnamedFloatProperty definition ->
            FloatProperty name definition

        UnnamedUrlProperty definition ->
            UrlProperty name definition

        UnnamedCustomProperty definition ->
            CustomProperty name definition


type Property
    = BoolProperty PropertyName BoolPropertyDefinition
    | InstantProperty PropertyName InstantPropertyDefinition
    | StringProperty PropertyName StringPropertyDefinition
    | IntProperty PropertyName IntPropertyDefinition
    | FloatProperty PropertyName FloatPropertyDefinition
    | UrlProperty PropertyName UrlPropertyDefinition
    | CustomProperty PropertyName CustomPropertyDefinition


unnamedPropertyDecoder : List TypeName -> Json.Decode.Decoder UnnamedProperty
unnamedPropertyDecoder customTypeNames =
    Json.Decode.field "is" Json.Decode.string
        |> Json.Decode.andThen
            (\name ->
                case name of
                    "Bool" ->
                        Json.Decode.map UnnamedBoolProperty boolPropertyDefinitionDecoder

                    "Instant" ->
                        Json.Decode.map UnnamedInstantProperty instantPropertyDefinitionDecoder

                    "String" ->
                        Json.Decode.map UnnamedStringProperty stringPropertyDefinitionDecoder

                    "Int" ->
                        Json.Decode.map UnnamedIntProperty intPropertyDefinitionDecoder

                    "Float" ->
                        Json.Decode.map UnnamedFloatProperty floatPropertyDefinitionDecoder

                    "Url" ->
                        Json.Decode.map UnnamedUrlProperty urlPropertyDefinitionDecoder

                    _ ->
                        if customTypeNames |> List.any ((==) name) then
                            Json.Decode.map UnnamedCustomProperty customPropertyDefinitionDecoder

                        else
                            Json.Decode.fail (name ++ " is not one of String, Url, Int, Float, Bool, Instant, " ++ (customTypeNames |> String.join ", ") ++ ".")
            )


type alias CustomRecordDefinition =
    { description : String
    , has : List Property
    }


customRecordDefinitionDecoder : List TypeName -> Json.Decode.Decoder CustomRecordDefinition
customRecordDefinitionDecoder customTypeNames =
    Json.Decode.map2 CustomRecordDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "has"
            (Json.Decode.dict (unnamedPropertyDecoder customTypeNames)
                |> Json.Decode.andThen
                    (\dict ->
                        if dict |> Dict.isEmpty then
                            Json.Decode.fail "Records must have at least one property."

                        else
                            Json.Decode.succeed dict
                    )
                |> Json.Decode.map (Dict.toList >> List.map nameUnnamedProperty)
            )
        )


type alias CustomEnumDefinition =
    { description : String
    , oneOf : Dict VariantName Variant
    }


customEnumDefinitionDecoder : List TypeName -> Json.Decode.Decoder CustomEnumDefinition
customEnumDefinitionDecoder customTypeNames =
    Json.Decode.map2 CustomEnumDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "oneOf" (Json.Decode.dict (variantDecoder customTypeNames)))


type alias CustomFloatDefinition =
    { description : String
    , min : Maybe Float
    , max : Maybe Float
    , equals : Maybe Float
    }


customFloatDefinitionDecoder : Json.Decode.Decoder CustomFloatDefinition
customFloatDefinitionDecoder =
    Json.Decode.map4 CustomFloatDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "min" Json.Decode.float)
        (optionalField "max" Json.Decode.float)
        (optionalField "equals" Json.Decode.float)


type alias CustomIntDefinition =
    { description : String
    , min : Maybe Int
    , max : Maybe Int
    , equals : Maybe Int
    }


customIntDefinitionDecoder : Json.Decode.Decoder CustomIntDefinition
customIntDefinitionDecoder =
    Json.Decode.map4 CustomIntDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "min" Json.Decode.int)
        (optionalField "max" Json.Decode.int)
        (optionalField "equals" Json.Decode.int)


type alias CustomStringDefinition =
    { description : String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    , matches : Maybe String
    , equals : Maybe String
    }


customStringDefinitionDecoder : Json.Decode.Decoder CustomStringDefinition
customStringDefinitionDecoder =
    Json.Decode.map5 CustomStringDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "minLength" Json.Decode.int)
        (optionalField "maxLength" Json.Decode.int)
        (optionalField "matches" Json.Decode.string)
        (optionalField "equals" Json.Decode.string)


type alias CustomBoolDefinition =
    { description : String
    , equals : Maybe Bool
    }


customBoolDefinitionDecoder : Json.Decode.Decoder CustomBoolDefinition
customBoolDefinitionDecoder =
    Json.Decode.map2 CustomBoolDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "equals" Json.Decode.bool)


type alias CustomListDefinition =
    { of_ : TypeRef
    , description : String
    , minLength : Maybe Int
    , maxLength : Maybe Int
    }


customListDefinitionDecoder : List TypeName -> Json.Decode.Decoder CustomListDefinition
customListDefinitionDecoder customTypeNames =
    Json.Decode.map4 CustomListDefinition
        (Json.Decode.field "of" (typeRefDecoder customTypeNames))
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "minLength" Json.Decode.int)
        (optionalField "maxLength" Json.Decode.int)


type alias CustomUrlDefinition =
    { description : String
    , params : List UrlParam
    , methods : UrlMethods
    }


type alias UrlParamName =
    String


type UrlParam
    = UrlParam UrlParamName { description : String, optional : Bool }


type alias UrlMethods =
    { get : Maybe TypeRef
    , put : Maybe { accept : TypeRef, expect : TypeRef }
    , post : Maybe { accept : TypeRef, expect : TypeRef }
    , patch : Maybe { accept : TypeRef, expect : TypeRef }
    , delete : Maybe TypeRef
    }


defaultMethods =
    { get = Nothing
    , put = Nothing
    , post = Nothing
    , patch = Nothing
    , delete = Nothing
    }


urlMethodsDecoder : List TypeName -> Json.Decode.Decoder UrlMethods
urlMethodsDecoder customTypeNames =
    Json.Decode.map5 UrlMethods
        (optionalField "get" (typeRefDecoder customTypeNames))
        (optionalField "put" (Json.Decode.map2 (\a b -> { accept = a, expect = b }) (Json.Decode.field "accept" (typeRefDecoder customTypeNames)) (Json.Decode.field "expect" (typeRefDecoder customTypeNames))))
        (optionalField "post" (Json.Decode.map2 (\a b -> { accept = a, expect = b }) (Json.Decode.field "accept" (typeRefDecoder customTypeNames)) (Json.Decode.field "expect" (typeRefDecoder customTypeNames))))
        (optionalField "patch" (Json.Decode.map2 (\a b -> { accept = a, expect = b }) (Json.Decode.field "accept" (typeRefDecoder customTypeNames)) (Json.Decode.field "expect" (typeRefDecoder customTypeNames))))
        (optionalField "delete" (typeRefDecoder customTypeNames))


urlParamDecoder : Json.Decode.Decoder { description : String, optional : Bool }
urlParamDecoder =
    Json.Decode.map2 (\s o -> { description = s, optional = o })
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "optional" Json.Decode.bool
            |> Json.Decode.map (Maybe.withDefault False)
        )


customUrlDefinitionDecoder : List TypeName -> Json.Decode.Decoder CustomUrlDefinition
customUrlDefinitionDecoder customTypeNames =
    Json.Decode.map3 CustomUrlDefinition
        (Json.Decode.field "description" Json.Decode.string)
        (optionalField "params" (Json.Decode.dict urlParamDecoder |> Json.Decode.map (Dict.toList >> List.map (\( k, v ) -> UrlParam k v))) |> Json.Decode.map (Maybe.withDefault []))
        (optionalField "methods" (urlMethodsDecoder customTypeNames) |> Json.Decode.map (Maybe.withDefault defaultMethods))


type UnnamedCustomType
    = UnnamedCustomRecord CustomRecordDefinition
    | UnnamedCustomEnum CustomEnumDefinition
    | UnnamedCustomList CustomListDefinition
    | UnnamedCustomBool CustomBoolDefinition
    | UnnamedCustomString CustomStringDefinition
    | UnnamedCustomInt CustomIntDefinition
    | UnnamedCustomFloat CustomFloatDefinition
    | UnnamedCustomUrl CustomUrlDefinition


unnamedCustomTypeDecoder : List TypeName -> Json.Decode.Decoder UnnamedCustomType
unnamedCustomTypeDecoder customTypeNames =
    Json.Decode.field "is" Json.Decode.string
        |> Json.Decode.andThen
            (\name ->
                case name of
                    "Record" ->
                        Json.Decode.map UnnamedCustomRecord (customRecordDefinitionDecoder customTypeNames)

                    "Enum" ->
                        Json.Decode.map UnnamedCustomEnum (customEnumDefinitionDecoder customTypeNames)

                    "List" ->
                        Json.Decode.map UnnamedCustomList (customListDefinitionDecoder customTypeNames)

                    "Bool" ->
                        Json.Decode.map UnnamedCustomBool customBoolDefinitionDecoder

                    "String" ->
                        Json.Decode.map UnnamedCustomString customStringDefinitionDecoder

                    "Int" ->
                        Json.Decode.map UnnamedCustomInt customIntDefinitionDecoder

                    "Float" ->
                        Json.Decode.map UnnamedCustomFloat customFloatDefinitionDecoder

                    "Url" ->
                        Json.Decode.map UnnamedCustomUrl (customUrlDefinitionDecoder customTypeNames)

                    _ ->
                        Json.Decode.fail (name ++ " is not one of Record, Enum, List, String, Url, Int, Float, Bool.")
            )


nameUnnamedCustomType ( name, customType ) =
    case customType of
        UnnamedCustomRecord definition ->
            CustomRecord name definition

        UnnamedCustomEnum definition ->
            CustomEnum name definition

        UnnamedCustomList definition ->
            CustomList name definition

        UnnamedCustomBool definition ->
            CustomBool name definition

        UnnamedCustomString definition ->
            CustomString name definition

        UnnamedCustomInt definition ->
            CustomInt name definition

        UnnamedCustomFloat definition ->
            CustomFloat name definition

        UnnamedCustomUrl definition ->
            CustomUrl name definition


type CustomType
    = CustomRecord TypeName CustomRecordDefinition
    | CustomEnum TypeName CustomEnumDefinition
    | CustomList TypeName CustomListDefinition
    | CustomBool TypeName CustomBoolDefinition
    | CustomString TypeName CustomStringDefinition
    | CustomInt TypeName CustomIntDefinition
    | CustomFloat TypeName CustomFloatDefinition
    | CustomUrl TypeName CustomUrlDefinition


type alias Spec =
    { description : String
    , types : List CustomType
    }


specDecoder : Json.Decode.Decoder Spec
specDecoder =
    Json.Decode.map2
        Spec
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "types"
            (Json.Decode.dict (Json.Decode.succeed ())
                |> Json.Decode.andThen
                    (\dict ->
                        Json.Decode.dict (unnamedCustomTypeDecoder (dict |> Dict.keys))
                            |> Json.Decode.map (Dict.toList >> List.map nameUnnamedCustomType)
                    )
            )
        )
