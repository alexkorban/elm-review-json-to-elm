module Json exposing (DecoderStyle(..), GeneratorOptions, ImportDetails(..), JsonString, NamingStyle(..), Output, convert)

import Array exposing (Array)
import Char exposing (isDigit)
import Cons exposing (Cons)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import List.Extra exposing (unique)
import Set exposing (Set)
import String.Extra


type JsonValue
    = JString
    | JFloat
    | JInt
    | JBool
    | JList (List JsonValue)
    | JObj (List ( String, JsonValue ))
    | JNull


type alias JsonString =
    String


type alias Path =
    Cons String


type DecoderStyle
    = ApplicativeDecoders
    | PipelineDecoders
    | PlainDecoders


type NamingStyle
    = VerbNaming
    | NounNaming


type ImportDetails
    = ExposingAll
    | ExposingNone
    | ExposingSome (List String)
    | NoImport


type alias Output =
    { imports : List String, types : List String, decoders : List String, encoders : List String }


type alias GeneratorOptions =
    { decode : ( String, ImportDetails )
    , encode : ( String, ImportDetails )
    , pipeline : ( String, ImportDetails )
    , extra : ( String, ImportDetails )
    , decoderStyle : DecoderStyle
    , namingStyle : NamingStyle
    , rootTypeName : String
    }


type alias GeneratorConfig =
    { decode :
        { bool : String
        , decoder : String
        , field : String
        , float : String
        , int : String
        , list : String
        , map : String
        , map2 : String
        , map3 : String
        , map4 : String
        , map5 : String
        , map6 : String
        , map7 : String
        , map8 : String
        , null : String
        , oneOf : String
        , string : String
        , succeed : String
        }
    , decoderStyle : DecoderStyle
    , encode :
        { bool : String
        , float : String
        , int : String
        , list : String
        , null : String
        , object : String
        , string : String
        , value : String
        }
    , extra :
        { andMap : String }
    , namingStyle : NamingStyle
    , pipeline :
        { required : String }
    , rootTypeName : String
    }


perhapsQualified symbolName importDetails =
    let
        qualifiedName =
            Tuple.first importDetails ++ "." ++ symbolName
    in
    case Tuple.second importDetails of
        ExposingAll ->
            symbolName

        ExposingNone ->
            qualifiedName

        ExposingSome exposedSymbols ->
            if List.member symbolName exposedSymbols then
                symbolName

            else
                qualifiedName

        NoImport ->
            qualifiedName


configFromOptions options =
    let
        perhapsQualifiedDecode symbolName =
            perhapsQualified symbolName options.decode

        perhapsQualifiedEncode symbolName =
            perhapsQualified symbolName options.encode
    in
    { decode =
        { bool = perhapsQualifiedDecode "bool"
        , decoder = perhapsQualifiedDecode "Decoder"
        , field = perhapsQualifiedDecode "field"
        , float = perhapsQualifiedDecode "float"
        , int = perhapsQualifiedDecode "int"
        , list = perhapsQualifiedDecode "list"
        , map = perhapsQualifiedDecode "map"
        , map2 = perhapsQualifiedDecode "map2"
        , map3 = perhapsQualifiedDecode "map3"
        , map4 = perhapsQualifiedDecode "map4"
        , map5 = perhapsQualifiedDecode "map5"
        , map6 = perhapsQualifiedDecode "map6"
        , map7 = perhapsQualifiedDecode "map7"
        , map8 = perhapsQualifiedDecode "map8"
        , null = perhapsQualifiedDecode "null"
        , oneOf = perhapsQualifiedDecode "oneOf"
        , string = perhapsQualifiedDecode "string"
        , succeed = perhapsQualifiedDecode "succeed"
        }
    , decoderStyle = options.decoderStyle
    , encode =
        { bool = perhapsQualifiedEncode "bool"
        , float = perhapsQualifiedEncode "float"
        , int = perhapsQualifiedEncode "int"
        , list = perhapsQualifiedEncode "list"
        , null = perhapsQualifiedEncode "null"
        , object = perhapsQualifiedEncode "object"
        , string = perhapsQualifiedEncode "string"
        , value = perhapsQualifiedEncode "Value"
        }
    , extra =
        { andMap = perhapsQualified "andMap" options.extra }
    , namingStyle = options.namingStyle
    , pipeline =
        { required = perhapsQualified "required" options.pipeline }
    , rootTypeName =
        if String.isEmpty options.rootTypeName then
            "Root"

        else
            options.rootTypeName
    }


convert : GeneratorOptions -> JsonString -> Result String Output
convert options jsonStr =
    case Decode.decodeString jsonDecoder jsonStr of
        Err err ->
            Err <| Decode.errorToString err

        Ok tree ->
            let
                rootPath =
                    Cons.singleton options.rootTypeName

                config =
                    configFromOptions options
            in
            Ok
                { imports = imports options.decoderStyle
                , types = typesAndAliases rootPath tree
                , decoders = decoders config rootPath tree
                , encoders = encoders config rootPath tree
                }


jsonDecoder : Decoder JsonValue
jsonDecoder =
    Decode.oneOf
        [ Decode.map (\_ -> JString) Decode.string
        , Decode.map (\_ -> JInt) Decode.int
        , Decode.map (\_ -> JFloat) Decode.float
        , Decode.map (\_ -> JBool) Decode.bool
        , Decode.map JList <| Decode.list <| Decode.lazy (\_ -> jsonDecoder)
        , Decode.map (JObj << List.sortBy (adorn 0 << Tuple.first)) <| Decode.keyValuePairs <| Decode.lazy (\_ -> jsonDecoder)
        , Decode.null JNull
        ]


indexNouns : Array String
indexNouns =
    Array.fromList [ "Object", "Member", "Entity", "Thing", "Instance", "Constituent", "Specimen", "Gadget", "Widget", "Gizmo", "Part", "Chunk", "Piece", "Thingy", "Thingamajig", "Whatsit", "Doodad" ]


strFromIndex : Int -> String
strFromIndex index =
    Maybe.withDefault ("Alias" ++ String.fromInt index) <| Array.get index indexNouns



-- GENERATION OF IMPORTS --


imports : DecoderStyle -> List String
imports decoderStyle =
    let
        importStrs =
            [ "import Json.Decode", "import Json.Encode" ]
                ++ (case decoderStyle of
                        ApplicativeDecoders ->
                            [ "import Json.Decode.Extra" ]

                        PipelineDecoders ->
                            [ "import Json.Decode.Pipeline" ]

                        PlainDecoders ->
                            []
                   )

        commentStrs =
            [ "-- Required packages:"
            , "-- * elm/json"
            ]
                ++ (case decoderStyle of
                        ApplicativeDecoders ->
                            [ "-- * elm-community/json-extra" ]

                        PipelineDecoders ->
                            [ "-- * NoRedInk/elm-json-decode-pipeline" ]

                        PlainDecoders ->
                            []
                   )
    in
    importStrs ++ [ "\n" ] ++ commentStrs



-- GENERATION OF TYPES AND TYPE ALIASES --


typesAndAliases : Path -> JsonValue -> List String
typesAndAliases path node =
    case node of
        JList nodes ->
            listTypesAndAliases path nodes

        JObj nodeTuples ->
            objTypeAliases path nodeTuples

        _ ->
            []


{-| `JsonValue`s have to be compared in order to find the set of distinct value types,
and it's done by converting them into strings with this function
-}
asStr : JsonValue -> String
asStr value =
    case value of
        JBool ->
            "Bool"

        JInt ->
            "Int"

        JFloat ->
            "Float"

        JString ->
            "String"

        JNull ->
            "()"

        JList items ->
            "List [" ++ (String.join ", " <| List.map asStr items) ++ "]"

        JObj items ->
            "{" ++ (String.join ", " <| List.map (\( label, item ) -> label ++ ": " ++ asStr item) items) ++ "}"


{-| A value is non-trivial if it requires type or alias definitions (and corresponding decoders & encoders)
-}
isNonTrivial node =
    let
        areHeterogeneous nodes =
            (List.length <| List.Extra.uniqueBy asStr nodes) > 1
    in
    case node of
        JList nodes ->
            List.any isNonTrivial nodes || areHeterogeneous nodes

        JObj _ ->
            True

        _ ->
            False


listTypesAndAliases : Path -> List JsonValue -> List String
listTypesAndAliases path nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes
                |> List.sortBy decoderSortOrder

        isHeterogeneous =
            List.length uniqueItems > 1
    in
    if isHeterogeneous then
        customType path uniqueItems
            :: (uniqueItems
                    |> List.indexedMap
                        (\i node ->
                            if isNonTrivial node then
                                typesAndAliases (Cons.appendList path [ strFromIndex i ]) node

                            else
                                []
                        )
                    |> List.concat
               )

    else
        case List.head uniqueItems of
            Nothing ->
                []

            Just childNode ->
                typesAndAliases (Cons.appendList path [ strFromIndex 0 ]) childNode


typeAliasName : Path -> String
typeAliasName path =
    String.Extra.classify <|
        if Cons.length path > 1 then
            String.join " " <| Cons.toList path

        else
            Cons.head path


elmType : Path -> JsonValue -> String
elmType path value =
    case value of
        JBool ->
            "Bool"

        JFloat ->
            "Float"

        JInt ->
            "Int"

        JString ->
            "String"

        JObj _ ->
            typeAliasName path

        JList nodes ->
            let
                uniqueItems =
                    List.Extra.uniqueBy asStr nodes
            in
            "List " ++ (paren <| listItemTypeName path uniqueItems)

        JNull ->
            "()"


objTypeAliases : Path -> List ( String, JsonValue ) -> List String
objTypeAliases path nodeTuples =
    let
        fieldStr =
            nodeTuples
                |> List.indexedMap
                    (\i node ->
                        (adorn i <| Tuple.first node)
                            ++ " : "
                            ++ elmType (Cons.appendList path [ Tuple.first node ]) (Tuple.second node)
                    )
                |> String.join "\n    , "

        mainAlias =
            ("type alias " ++ typeAliasName path ++ " =\n")
                ++ "    "
                ++ (if String.isEmpty fieldStr then
                        "{}"

                    else
                        "{ " ++ fieldStr ++ "\n    }"
                   )
    in
    mainAlias
        :: (nodeTuples
                |> List.filter (Tuple.second >> isNonTrivial)
                |> List.map (\( label, n ) -> typesAndAliases (Cons.appendList path [ label ]) n)
                |> List.concat
           )


{-| This function expects a list of _unique items_ in a list (rather than all of them)
-}
listItemTypeName : Path -> List JsonValue -> String
listItemTypeName path uniqueItems =
    let
        isHeterogeneous =
            List.length uniqueItems > 1
    in
    if isHeterogeneous then
        typeAliasName path

    else
        case List.head uniqueItems of
            Nothing ->
                "()"

            Just node ->
                elmType (Cons.appendList path [ strFromIndex 0 ]) node


paren : String -> String
paren t =
    if String.contains " " t then
        "(" ++ t ++ ")"

    else
        t


withApplyArrow : String -> String
withApplyArrow s =
    if String.contains " " s then
        "<| " ++ s

    else
        s


{-| An empty list has to be tried last so that other list decoders have a chance
to succeed before it
-}
decoderSortOrder : JsonValue -> Int
decoderSortOrder node =
    case node of
        JList [] ->
            1

        _ ->
            0


{-| This function expects a list of unique types
-}
customType : Path -> List JsonValue -> String
customType path nodes =
    let
        name =
            typeAliasName path
    in
    "type "
        ++ name
        ++ "\n    = "
        ++ (nodes
                |> List.indexedMap
                    (\i node ->
                        name
                            ++ String.fromInt i
                            ++ " "
                            ++ (paren <| elmType (Cons.appendList path [ strFromIndex i ]) node)
                    )
                |> String.join "\n    | "
           )



-- GENERATION OF PLAIN DECODERS --


decoders : GeneratorConfig -> Path -> JsonValue -> List String
decoders config path node =
    case node of
        JList nodes ->
            listDecoders config path node nodes

        JObj nodeTuples ->
            objDecoders config path nodeTuples

        _ ->
            let
                funcName =
                    decoderFuncName config.namingStyle <| typeAliasName path
            in
            [ (funcName ++ " : " ++ config.decode.decoder ++ " " ++ elmType path node ++ "\n")
                ++ (funcName ++ " = \n")
                ++ "    "
                ++ decoderName config path node
            ]


listDecoders : GeneratorConfig -> Path -> JsonValue -> List JsonValue -> List String
listDecoders config path node nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes
                |> List.sortBy decoderSortOrder

        isHeterogeneous =
            List.length uniqueItems > 1

        typeName =
            typeAliasName path

        funcName =
            decoderFuncName config.namingStyle typeName

        itemDecoder =
            if isHeterogeneous then
                config.decode.list ++ "  " ++ decoderFuncName config.namingStyle (typeName ++ "Item")

            else
                case List.head uniqueItems of
                    Nothing ->
                        config.decode.list ++ " <| " ++ config.decode.succeed ++ " ()"

                    Just childNode ->
                        decoderName config (Cons.appendList path [ strFromIndex 0 ]) childNode

        itemFuncName =
            decoderFuncName config.namingStyle (typeName ++ "Item")

        listDecoder =
            (funcName ++ " : " ++ config.decode.decoder ++ " " ++ (paren <| elmType path node) ++ "\n")
                ++ (funcName ++ " = \n")
                ++ (String.repeat 4 " " ++ listDecoderName config path uniqueItems)

        mainDecoders =
            (if Cons.length path == 1 then
                -- Usually we don't need to generate a separate decoder for the list itself,
                -- instead expressing things in terms of the item decoder. However,
                -- if the top level value is an array, we do need to generate a decoder for it,
                -- so we add it here
                [ listDecoder ]

             else
                []
            )
                ++ (if isHeterogeneous then
                        [ (itemFuncName ++ " : " ++ config.decode.decoder ++ " " ++ (paren <| listItemTypeName path uniqueItems) ++ "\n")
                            ++ (itemFuncName ++ " = \n")
                            ++ "    "
                            ++ (config.decode.oneOf ++ "\n")
                            ++ String.repeat 8 " "
                            ++ "[ "
                            ++ (uniqueItems
                                    |> List.indexedMap
                                        (\i n ->
                                            config.decode.map
                                                ++ " "
                                                ++ typeName
                                                ++ String.fromInt i
                                                ++ " <| "
                                                ++ decoderName config (Cons.appendList path [ strFromIndex i ]) n
                                        )
                                    |> String.join ("\n" ++ String.repeat 8 " " ++ ", ")
                               )
                            ++ "\n"
                            ++ String.repeat 8 " "
                            ++ "]"
                        ]

                    else
                        []
                   )
    in
    mainDecoders
        ++ (uniqueItems
                |> List.indexedMap
                    (\i n ->
                        if isNonTrivial n then
                            decoders config (Cons.appendList path [ strFromIndex i ]) n

                        else
                            []
                    )
                |> List.concat
           )


objDecoders : GeneratorConfig -> Path -> List ( String, JsonValue ) -> List String
objDecoders config path nodeTuples =
    let
        typeName =
            typeAliasName path

        funcName =
            decoderFuncName config.namingStyle typeName

        mainDecoder =
            (funcName ++ " : " ++ config.decode.decoder ++ " " ++ typeName ++ "\n")
                ++ (funcName ++ " = \n")
                ++ (case ( List.length nodeTuples, config.decoderStyle ) of
                        ( 0, _ ) ->
                            "    " ++ config.decode.succeed ++ " " ++ typeName

                        ( 1, PlainDecoders ) ->
                            "    " ++ config.decode.map ++ " " ++ typeName ++ "\n" ++ objFieldDecoders 8 config path nodeTuples

                        ( fieldCount, PlainDecoders ) ->
                            if fieldCount > 8 then
                                stagedObjDecoders config typeName path nodeTuples

                            else
                                "    "
                                    ++ config.decode.map
                                    ++ (String.fromInt <| List.length nodeTuples)
                                    ++ " "
                                    ++ typeName
                                    ++ "\n"
                                    ++ objFieldDecoders 8 config path nodeTuples

                        ( _, PipelineDecoders ) ->
                            "    "
                                ++ config.decode.succeed
                                ++ " "
                                ++ typeName
                                ++ "\n"
                                ++ pipelineObjFieldDecoders 8 config path nodeTuples

                        ( _, ApplicativeDecoders ) ->
                            "    "
                                ++ config.decode.succeed
                                ++ " "
                                ++ typeName
                                ++ "\n"
                                ++ applicativeObjFieldDecoders 8 config path nodeTuples
                   )
    in
    mainDecoder
        :: (nodeTuples
                |> List.filter (Tuple.second >> isNonTrivial)
                |> List.map (\( label, n ) -> decoders config (Cons.appendList path [ label ]) n)
                |> List.concat
           )


objFieldDecoders : Int -> GeneratorConfig -> Path -> List ( String, JsonValue ) -> String
objFieldDecoders indent config path nodeTuples =
    nodeTuples
        |> List.map
            (\( label, value ) ->
                String.repeat indent " "
                    ++ "("
                    ++ config.decode.field
                    ++ " \""
                    ++ label
                    ++ "\" "
                    ++ (withApplyArrow <| decoderName config (Cons.appendList path [ label ]) value)
                    ++ ")"
            )
        |> String.join "\n"


stagedObjDecoders : GeneratorConfig -> String -> Path -> List ( String, JsonValue ) -> String
stagedObjDecoders config typeName path nodeTuples =
    let
        initFieldSet =
            List.take 8 nodeTuples

        fieldSets =
            nodeTuples
                |> List.drop 8
                |> List.Extra.greedyGroupsOf 7

        mapN i =
            config.decode
                |> (case i of
                        1 ->
                            .map

                        2 ->
                            .map2

                        3 ->
                            .map3

                        4 ->
                            .map4

                        5 ->
                            .map5

                        6 ->
                            .map6

                        7 ->
                            .map7

                        _ ->
                            -- not supposed to happen
                            .map
                   )
    in
    "    let\n"
        ++ (String.repeat 8 " " ++ "fieldSet0 = \n")
        ++ (String.repeat 12 " " ++ config.decode.map8 ++ " " ++ typeName ++ "\n")
        ++ objFieldDecoders 16 config path initFieldSet
        ++ "\n"
        ++ (fieldSets
                |> List.indexedMap
                    (\index fieldSet ->
                        if List.length fieldSet == 7 && index < List.length fieldSets - 1 then
                            ("\n" ++ String.repeat 8 " " ++ "fieldSet" ++ String.fromInt (index + 1) ++ " =\n")
                                ++ (String.repeat 12 " " ++ config.decode.map8 ++ " (<|)\n")
                                ++ (String.repeat 16 " " ++ "fieldSet" ++ String.fromInt index ++ "\n")
                                ++ objFieldDecoders 16 config path fieldSet

                        else
                            "    in\n"
                                ++ (String.repeat 4 " " ++ mapN (1 + List.length fieldSet) ++ " (<|)\n")
                                ++ (String.repeat 8 " " ++ "fieldSet" ++ String.fromInt (List.length fieldSets - 1) ++ "\n")
                                ++ objFieldDecoders 8 config path fieldSet
                    )
                |> String.join "\n"
           )


listDecoderName : GeneratorConfig -> Path -> List JsonValue -> String
listDecoderName config path nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes

        isHeterogeneous =
            List.length uniqueItems > 1

        itemDecoderFuncName =
            if config.namingStyle == NounNaming then
                String.Extra.decapitalize <| typeAliasName path ++ "ItemDecoder"

            else
                "decode" ++ typeAliasName path ++ "Item"
    in
    config.decode.list
        ++ " "
        ++ (if isHeterogeneous then
                itemDecoderFuncName

            else
                case List.head uniqueItems of
                    Nothing ->
                        withApplyArrow <| config.decode.succeed ++ " ()"

                    Just node ->
                        paren <| decoderName config (Cons.appendList path [ strFromIndex 0 ]) node
           )


decoderFuncName : NamingStyle -> String -> String
decoderFuncName namingStyle typeName =
    if namingStyle == NounNaming then
        String.Extra.decapitalize <| typeName ++ "Decoder"

    else
        "decode" ++ typeName


decoderName : GeneratorConfig -> Path -> JsonValue -> String
decoderName config path value =
    case value of
        JInt ->
            config.decode.int

        JFloat ->
            config.decode.float

        JString ->
            config.decode.string

        JBool ->
            config.decode.bool

        JList nodes ->
            listDecoderName config path nodes

        JObj nodeTuples ->
            decoderFuncName config.namingStyle <| typeAliasName path

        JNull ->
            config.decode.null ++ " ()"



-- GENERATION OF PIPELINE DECODERS


pipelineObjFieldDecoders : Int -> GeneratorConfig -> Path -> List ( String, JsonValue ) -> String
pipelineObjFieldDecoders indent config path nodeTuples =
    nodeTuples
        |> List.map
            (\( label, node ) ->
                String.repeat indent " "
                    ++ "|> "
                    ++ config.pipeline.required
                    ++ " \""
                    ++ label
                    ++ "\" "
                    ++ (paren <| decoderName config (Cons.appendList path [ label ]) node)
            )
        |> String.join "\n"



-- GENERATION OF APPLICATIVE DECODERS


applicativeObjFieldDecoders : Int -> GeneratorConfig -> Path -> List ( String, JsonValue ) -> String
applicativeObjFieldDecoders indent config path nodeTuples =
    nodeTuples
        |> List.map
            (\( label, node ) ->
                String.repeat indent " "
                    ++ "|> "
                    ++ config.extra.andMap
                    ++ " ("
                    ++ config.decode.field
                    ++ " \""
                    ++ label
                    ++ "\" "
                    ++ (withApplyArrow <| decoderName config (Cons.appendList path [ label ]) node)
                    ++ ")"
            )
        |> String.join "\n"



-- GENERATION OF ENCODERS --


encoders : GeneratorConfig -> Path -> JsonValue -> List String
encoders config path node =
    let
        namePrefix =
            if config.namingStyle == NounNaming then
                "encoded"

            else
                "encode"

        typeName =
            typeAliasName path
    in
    case node of
        JList nodes ->
            listEncoders config path node nodes

        JObj nodes ->
            objEncoders config path nodes

        _ ->
            [ (namePrefix ++ typeName ++ " : " ++ elmType path node ++ " -> " ++ config.encode.value ++ "\n")
                ++ (namePrefix ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " =\n")
                ++ "    "
                ++ encoderName config (String.Extra.decapitalize typeName) path node
            ]


objEncoders : GeneratorConfig -> Path -> List ( String, JsonValue ) -> List String
objEncoders config path nodeTuples =
    let
        typeName =
            typeAliasName path

        fieldEncoders =
            nodeTuples
                |> List.indexedMap
                    (\i ( label, node ) ->
                        "( \""
                            ++ label
                            ++ "\", "
                            ++ encoderName config (String.Extra.decapitalize typeName ++ "." ++ adorn i label) (Cons.appendList path [ label ]) node
                            ++ " )"
                    )
                |> String.join ("\n" ++ String.repeat 8 " " ++ ", ")

        mainEncoder =
            (encoderNamePrefix config.namingStyle ++ typeName ++ " : " ++ typeName ++ " -> " ++ config.encode.value ++ "\n")
                ++ (encoderNamePrefix config.namingStyle ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " = \n")
                ++ "    "
                ++ config.encode.object
                ++ "\n"
                ++ String.repeat 8 " "
                ++ (if String.isEmpty fieldEncoders then
                        "[]"

                    else
                        "[ " ++ fieldEncoders ++ ("\n" ++ String.repeat 8 " " ++ "]")
                   )
    in
    mainEncoder
        :: (nodeTuples
                |> List.filter (Tuple.second >> isNonTrivial)
                |> List.map (\( label, n ) -> encoders config (Cons.appendList path [ label ]) n)
                |> List.concat
           )


listEncoders : GeneratorConfig -> Path -> JsonValue -> List JsonValue -> List String
listEncoders config path node childNodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr childNodes
                |> List.sortBy decoderSortOrder

        isHeterogeneous =
            List.length uniqueItems > 1

        typeName =
            typeAliasName path

        listEncoder =
            (encoderNamePrefix config.namingStyle ++ typeName ++ " : ")
                ++ ("List " ++ (paren <| listItemTypeName path uniqueItems) ++ " -> " ++ config.encode.value ++ "\n")
                ++ (encoderNamePrefix config.namingStyle ++ typeName ++ " " ++ String.Extra.decapitalize typeName ++ " =\n")
                ++ (String.repeat 4 " " ++ listEncoderName config path uniqueItems ++ " " ++ String.Extra.decapitalize typeName)

        mainEncoders =
            (if Cons.length path == 1 then
                -- Usually we don't need to generate a separate encoder function for the list itself,
                -- instead expressing things in terms of the item encoder function. However,
                -- if the top level value is an array, we do need to generate an encoder function for it,
                -- so we add it here
                [ listEncoder ]

             else
                []
            )
                ++ (if isHeterogeneous then
                        [ (encoderNamePrefix config.namingStyle ++ typeName ++ "Item : ")
                            ++ (listItemTypeName path uniqueItems ++ " -> " ++ config.encode.value ++ "\n")
                            ++ (encoderNamePrefix config.namingStyle ++ typeName ++ "Item " ++ String.Extra.decapitalize typeName ++ " =\n")
                            ++ String.repeat 4 " "
                            ++ ("case " ++ String.Extra.decapitalize typeName ++ " of\n")
                            ++ String.repeat 8 " "
                            ++ (uniqueItems
                                    |> List.indexedMap
                                        (\i n ->
                                            (typeName ++ String.fromInt i ++ " value ->\n")
                                                ++ (String.repeat 12 " " ++ encoderName config "value" (Cons.appendList path [ strFromIndex i ]) n)
                                        )
                                    |> String.join ("\n\n" ++ String.repeat 8 " ")
                               )
                        ]

                    else
                        []
                   )
    in
    mainEncoders
        ++ (uniqueItems
                |> List.indexedMap
                    (\i n ->
                        if isNonTrivial n then
                            encoders config (Cons.appendList path [ strFromIndex i ]) n

                        else
                            []
                    )
                |> List.concat
           )


listEncoderName : GeneratorConfig -> Path -> List JsonValue -> String
listEncoderName config path nodes =
    let
        uniqueItems =
            List.Extra.uniqueBy asStr nodes

        isHeterogeneous =
            List.length uniqueItems > 1
    in
    config.encode.list
        ++ " "
        ++ (if isHeterogeneous then
                encoderNamePrefix config.namingStyle ++ typeAliasName path ++ "Item"

            else
                case List.head uniqueItems of
                    Nothing ->
                        "(\\_ -> " ++ config.encode.null ++ ")"

                    Just node ->
                        paren <| encoderName config "" (Cons.appendList path [ strFromIndex 0 ]) node
           )


encoderName : GeneratorConfig -> String -> Path -> JsonValue -> String
encoderName config valueName path value =
    String.trimRight <|
        case value of
            JInt ->
                config.encode.int ++ " " ++ valueName

            JFloat ->
                config.encode.float ++ " " ++ valueName

            JString ->
                config.encode.string ++ " " ++ valueName

            JBool ->
                config.encode.bool ++ " " ++ valueName

            JNull ->
                config.encode.null

            JList nodes ->
                listEncoderName config path nodes ++ " " ++ valueName

            JObj _ ->
                encoderNamePrefix config.namingStyle ++ typeAliasName path ++ " " ++ valueName


encoderNamePrefix : NamingStyle -> String
encoderNamePrefix namingStyle =
    if namingStyle == NounNaming then
        "encoded"

    else
        "encode"



-- HELPERS --


keywords =
    Set.fromList
        [ "if"
        , "then"
        , "else"
        , "case"
        , "of"
        , "let"
        , "in"
        , "type"
        , "module"
        , "where"
        , "import"
        , "exposing"
        , "as"
        , "port"
        ]


adorn : Int -> String -> String
adorn fieldIndex fieldName =
    let
        startsWithDigit s =
            s
                |> String.left 1
                |> String.any isDigit

        isAllowed c =
            Char.isAlphaNum c || c == '-' || c == '_'
    in
    case String.toInt fieldName of
        Just _ ->
            "field" ++ fieldName

        Nothing ->
            fieldName
                |> String.filter (not << (==) ' ')
                |> String.toList
                |> List.map
                    (\c ->
                        if not <| isAllowed c then
                            "U" ++ (String.fromInt <| Char.toCode c)

                        else
                            String.fromChar c
                    )
                |> String.concat
                |> String.Extra.classify
                |> String.Extra.decapitalize
                |> (\name ->
                        if String.isEmpty name then
                            "field" ++ (String.fromChar <| Char.fromCode <| fieldIndex + 65)

                        else if startsWithDigit name then
                            "field" ++ name

                        else
                            name
                   )
                |> (\name ->
                        if Set.member name keywords then
                            name ++ "_"

                        else
                            name
                   )
