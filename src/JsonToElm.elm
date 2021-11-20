module JsonToElm exposing (rule)

{-|

@docs rule

-}

import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Elm.Project
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Exposing as Exposing
import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Infix as Infix exposing (InfixDirection(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)
import ElmCodeGenerator
import Review.Fix
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, ModuleKey, Rule)


{-| Generates Elm code from a JSON sample in JsonString

    config =
        [ Json2Elm.rule
        ]


## Before fix

The fix runs on top-level values with a JsonString type annotation. It generates
JSON decoders and encoders as well as the necessary types and type aliases.

    import Json.Decode
    import Json.Encode

    sample : JsonString
    sample =
        """{"a": 1}"""


## After fix

    import Json.Decode
    import Json.Encode

    type alias Sample =
        { a : Int
        }

    sampleDecoder : Json.Decode.Decoder Sample
    sampleDecoder =
        Json.Decode.map Sample
            (Json.Decode.field "a" Json.Decode.int)

    encodedSample : Sample -> Json.Encode.Value
    encodedSample sample =
        Json.Encode.object
            [ ( "a", Json.Encode.int sample.a )
            ]

Note that the imports in the module are examined to determine the style of decoders.

If a `Json.Decode.Extra` import is present, then applicative-style decoders
using `Json.Decode.Extra.andMap` will be generated.

If a `Json.Decode.Pipeline` import is present, then pipeline decoders are
generated.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template alexkorban/elm-review-json2elm/example --rules Json2Elm
```

-}
rule : Rule
rule =
    Rule.newProjectRuleSchema "Json2Elm" initialProjectContext
        |> Rule.withContextFromImportedModules
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = Rule.initContextCreator fromProjectToModule |> Rule.withModuleNameLookupTable |> Rule.withMetadata
            , fromModuleToProject = Rule.initContextCreator fromModuleToProject |> Rule.withModuleKey |> Rule.withMetadata
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withElmJsonProjectVisitor elmJsonVisitor
        |> Rule.withFinalProjectEvaluation finalProjectEvaluation
        |> Rule.fromProjectRuleSchema



-- MODULE VISITOR


defaultConfig : ElmCodeGenerator.GeneratorOptions
defaultConfig =
    { decodeImport = { importAlias = "Json.Decode", exposingSpec = ElmCodeGenerator.ExposingNone }
    , encodeImport = { importAlias = "Json.Encode", exposingSpec = ElmCodeGenerator.ExposingNone }
    , decoderStyle = ElmCodeGenerator.PlainDecoders
    , namingStyle = ElmCodeGenerator.NounNaming
    , rootTypeName = "Sample"
    }


importVisitor : Node Import -> ModuleContext -> ( List (Error {}), ModuleContext )
importVisitor importNode context =
    let
        importThing : Import
        importThing =
            Node.value importNode

        modName =
            Node.value importNode |> .moduleName |> Node.value

        nameOrAlias : ModuleName
        nameOrAlias =
            importThing.moduleAlias
                |> Maybe.map Node.value
                |> Maybe.withDefault modName

        exposingList : ElmCodeGenerator.ExposingSpec
        exposingList =
            importThing.exposingList
                |> Maybe.map Node.value
                |> (\exposingL ->
                        case exposingL of
                            Just (Exposing.All _) ->
                                ElmCodeGenerator.ExposingAll

                            Just (Exposing.Explicit exposed) ->
                                exposed
                                    |> List.map Node.value
                                    |> List.filterMap
                                        (\exposedItem ->
                                            case exposedItem of
                                                Exposing.FunctionExpose name ->
                                                    Just name

                                                Exposing.TypeOrAliasExpose name ->
                                                    Just name

                                                _ ->
                                                    Nothing
                                        )
                                    |> ElmCodeGenerator.ExposingSome

                            Nothing ->
                                ElmCodeGenerator.ExposingNone
                   )

        setField : ElmCodeGenerator.ImportSpec -> ElmCodeGenerator.GeneratorOptions -> ElmCodeGenerator.GeneratorOptions
        setField value config =
            case modName of
                [ "Json", "Decode" ] ->
                    { config | decodeImport = value }

                [ "Json", "Encode" ] ->
                    { config | encodeImport = value }

                [ "Json", "Decode", "Pipeline" ] ->
                    { config | decoderStyle = ElmCodeGenerator.PipelineDecoders value }

                [ "Json", "Decode", "Extra" ] ->
                    { config
                        | decoderStyle =
                            if config.decoderStyle == ElmCodeGenerator.PlainDecoders then
                                ElmCodeGenerator.ApplicativeDecoders value

                            else
                                config.decoderStyle
                    }

                _ ->
                    config
    in
    ( []
    , context
        |> updateConfig (setField { importAlias = nameOrAlias |> String.join ".", exposingSpec = exposingList })
    )


updateConfig : (ElmCodeGenerator.GeneratorOptions -> ElmCodeGenerator.GeneratorOptions) -> ModuleContext -> ModuleContext
updateConfig updateFn context =
    { context | config = updateFn context.config }


moduleVisitor : Rule.ModuleRuleSchema {} ModuleContext -> Rule.ModuleRuleSchema { hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.withImportVisitor importVisitor


expressionVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionVisitor expression context =
    ( [], context )



-- case Node.value expression of
--     Expression.Application [ Node _ (Expression.FunctionOrValue [ "Debug" ] "todo"), Node _ (Expression.Literal literalString) ] ->
--         if literalString |> String.startsWith "@json" then
--             let
--                 jsonString : String
--                 jsonString =
--                     String.dropLeft 5 literalString
--             in
--             ( [ Rule.errorWithFix
--                     { message = "Here's my attempt to complete this stub"
--                     , details = [ "" ]
--                     }
--                     (Node.range expression)
--                     [ Review.Fix.replaceRangeBy (Node.range expression) <|
--                         withHugeIndentToEnsureValidSyntax (jsonAsElm context.config jsonString)
--                     ]
--               ]
--             , context
--             )
--         else
--             ( [], context )
--     _ ->
--         ( [], context )
-- CONTEXT


type alias ProjectContext =
    { samples : List ( ModuleName, JsonSample )
    , moduleKeys : Dict ModuleName ModuleKey
    }


type alias JsonSampleData =
    { functionName : String
    , range : Range
    , parameters : List (Node Pattern)

    --, signature : Signature
    , sampleText : String
    , config : ElmCodeGenerator.GeneratorOptions
    }


type JsonSample
    = JsonSample JsonSampleData


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable.ModuleNameLookupTable
    , samples : List JsonSample
    , currentModule : ModuleName
    , moduleLookupTable : ModuleNameLookupTable
    , config : ElmCodeGenerator.GeneratorOptions
    }


initialProjectContext : ProjectContext
initialProjectContext =
    { samples = []
    , moduleKeys = Dict.empty
    }


fromProjectToModule : ModuleNameLookupTable -> Rule.Metadata -> ProjectContext -> ModuleContext
fromProjectToModule lookupTable metadata projectContext =
    let
        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { lookupTable = lookupTable
    , samples =
        List.filterMap
            (\( moduleName_, sample ) ->
                if moduleName_ == moduleName then
                    Just sample

                else
                    Nothing
            )
            projectContext.samples
    , currentModule = moduleName
    , moduleLookupTable = lookupTable
    , config = defaultConfig
    }


fromModuleToProject : Rule.ModuleKey -> Rule.Metadata -> ModuleContext -> ProjectContext
fromModuleToProject moduleKey metadata moduleContext =
    let
        moduleName : ModuleName
        moduleName =
            Rule.moduleNameFromMetadata metadata
    in
    { samples = List.map (\sample -> ( moduleName, sample )) moduleContext.samples
    , moduleKeys = Dict.singleton moduleName moduleKey
    }


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext previousContext =
    { samples = newContext.samples ++ previousContext.samples
    , moduleKeys = Dict.union newContext.moduleKeys previousContext.moduleKeys
    }



-- ELM.JSON VISITOR


elmJsonVisitor : Maybe { elmJsonKey : Rule.ElmJsonKey, project : Elm.Project.Project } -> ProjectContext -> ( List nothing, ProjectContext )
elmJsonVisitor maybeElmJson projectContext =
    case maybeElmJson |> Maybe.map .project of
        Just (Elm.Project.Package _) ->
            ( [], projectContext )

        Just (Elm.Project.Application _) ->
            ( [], projectContext )

        Nothing ->
            ( [], projectContext )



-- DECLARATION VISITOR


declarationVisitor : List (Node Declaration) -> ModuleContext -> ( List a, ModuleContext )
declarationVisitor declarations context =
    ( []
    , { context
        | samples =
            List.filterMap
                (\declaration ->
                    case declaration of
                        Node range (Declaration.FunctionDeclaration function) ->
                            maybeJsonSample context range function

                        _ ->
                            Nothing
                )
                declarations
                ++ context.samples
      }
    )


typeAnnotationReturnValue : Node TypeAnnotation -> Node TypeAnnotation
typeAnnotationReturnValue typeAnnotation =
    case typeAnnotation of
        Node _ (TypeAnnotation.FunctionTypeAnnotation _ node_) ->
            typeAnnotationReturnValue node_

        _ ->
            typeAnnotation


maybeJsonSample : ModuleContext -> Range -> Function -> Maybe JsonSample
maybeJsonSample ({ config } as context) declarationRange function =
    let
        declaration : Expression.FunctionImplementation
        declaration =
            Node.value function.declaration

        newThing : Node Expression.FunctionImplementation -> Maybe JsonSample
        newThing (Node _ impl) =
            let
                sampleDecl (Node _ f) =
                    case f.expression of
                        Node _ (Expression.Application [ Node _ (Expression.FunctionOrValue [ "Debug" ] "todo"), Node _ (Expression.Literal literalString) ]) ->
                            if String.startsWith "@json" literalString then
                                Just <| String.dropLeft 5 literalString

                            else
                                Nothing

                        _ ->
                            Nothing
            in
            sampleDecl function.declaration
                |> Maybe.andThen
                    (\s ->
                        JsonSample
                            { functionName = Node.value impl.name
                            , range = declarationRange
                            , parameters = declaration.arguments

                            --, signature = signature
                            , sampleText = s
                            , config = { config | rootTypeName = Node.value impl.name }
                            }
                            |> Just
                    )
    in
    newThing function.declaration



-- case function.signature of
--     Just (Node _ signature) ->
--         case typeAnnotationReturnValue signature.typeAnnotation of
--             Node _ (TypeAnnotation.Typed (Node _ ( [], "JsonString" )) []) ->
--                 newThing signature
--             _ ->
--                 Nothing
--     _ ->
--         Nothing


generateJsonHandlingCode : JsonSampleData -> String
generateJsonHandlingCode jsonSample =
    jsonAsElm jsonSample.config jsonSample.sampleText


jsonAsElm : ElmCodeGenerator.GeneratorOptions -> String -> String
jsonAsElm config jsonString =
    let
        _ =
            Debug.log "Config" config
    in
    case ElmCodeGenerator.fromJsonSample config jsonString of
        Err err ->
            err

        Ok { imports, types, decoders, encoders } ->
            if List.isEmpty types then
                String.join "\n\n\n"
                    [ String.join "\n\n\n" decoders
                    , String.join "\n\n\n" encoders
                    ]

            else
                String.join "\n\n\n"
                    [ String.join "\n\n\n" types
                    , String.join "\n\n\n" decoders
                    , String.join "\n\n\n" encoders
                    ]


finalProjectEvaluation : ProjectContext -> List (Error { useErrorForModule : () })
finalProjectEvaluation projectContext =
    projectContext.samples
        |> List.filterMap
            (\( moduleName, sample ) ->
                case sample of
                    JsonSample jsonSample ->
                        case Dict.get moduleName projectContext.moduleKeys of
                            Just moduleKey ->
                                let
                                    fix : String
                                    fix =
                                        generateJsonHandlingCode jsonSample
                                in
                                Rule.errorForModuleWithFix
                                    moduleKey
                                    { message = "Here's my attempt to complete this stub"
                                    , details = [ "" ]
                                    }
                                    jsonSample.range
                                    (Review.Fix.replaceRangeBy jsonSample.range fix
                                        :: List.filterMap
                                            (\( moduleName_, fix_ ) ->
                                                if moduleName_ == moduleName then
                                                    Just fix_

                                                else
                                                    Nothing
                                            )
                                            []
                                    )
                                    |> Just

                            _ ->
                                Nothing
            )
