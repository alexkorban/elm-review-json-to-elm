module JsonToElmTest exposing (all)

import JsonToElm
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "tests"
        [ testCase "int array JSON"
            """
import Json.Decode
import Json.Encode
"""
            "sample =\n    Debug.todo \"@json [1]\""
            """sampleDecoder : Json.Decode.Decoder (List Int)
sampleDecoder = 
    Json.Decode.list Json.Decode.int


encodedSample : List Int -> Json.Encode.Value
encodedSample sample =
    Json.Encode.list Json.Encode.int sample"""
        , testCase "bool JSON"
            """
import Json.Decode
import Json.Encode
"""
            "root =\n    Debug.todo \"@json false\""
            """rootDecoder : Json.Decode.Decoder Bool
rootDecoder = 
    Json.Decode.bool


encodedRoot : Bool -> Json.Encode.Value
encodedRoot root =
    Json.Encode.bool root"""
        , testCase "applicative decoders"
            """
import Json.Decode
import Json.Encode
import Json.Decode.Extra
"""
            "sample =\n    Debug.todo \"\"\"@json {\"a\": 1}\"\"\""
            """type alias Sample =
    { a : Int
    }


sampleDecoder : Json.Decode.Decoder Sample
sampleDecoder = 
    Json.Decode.succeed Sample
        |> Json.Decode.Extra.andMap (Json.Decode.field "a" Json.Decode.int)


encodedSample : Sample -> Json.Encode.Value
encodedSample sample = 
    Json.Encode.object
        [ ( "a", Json.Encode.int sample.a )
        ]"""
        , testCase "pipeline decoders"
            """
import Json.Decode
import Json.Encode
import Json.Decode.Pipeline
import Json.Decode.Extra
"""
            "sample =\n    Debug.todo \"\"\"@json {\"a\": 1}\"\"\""
            """type alias Sample =
    { a : Int
    }


sampleDecoder : Json.Decode.Decoder Sample
sampleDecoder = 
    Json.Decode.succeed Sample
        |> Json.Decode.Pipeline.required "a" Json.Decode.int


encodedSample : Sample -> Json.Encode.Value
encodedSample sample = 
    Json.Encode.object
        [ ( "a", Json.Encode.int sample.a )
        ]"""
        , testCase "customised imports for plain decoders"
            """
import Json.Decode as Decode exposing (Decoder, field, list)
import Json.Encode as Encode exposing (Value, list, object)
"""
            "sample =\n    Debug.todo \"@json {\\\"a\\\": [1]}\""
            """type alias Sample =
    { a : List Int
    }


sampleDecoder : Decoder Sample
sampleDecoder = 
    Decode.map Sample
        (field "a" <| list Decode.int)


encodedSample : Sample -> Value
encodedSample sample = 
    object
        [ ( "a", list Encode.int sample.a )
        ]"""
        , testCase "customised imports for pipeline decoders"
            """
import Json.Decode
import Json.Encode
import Json.Decode.Pipeline exposing (required)
"""
            "sample =\n    Debug.todo \"@json {\\\"a\\\": 1}\""
            """type alias Sample =
    { a : Int
    }


sampleDecoder : Json.Decode.Decoder Sample
sampleDecoder = 
    Json.Decode.succeed Sample
        |> required "a" Json.Decode.int


encodedSample : Sample -> Json.Encode.Value
encodedSample sample = 
    Json.Encode.object
        [ ( "a", Json.Encode.int sample.a )
        ]"""
        , testCase "customised imports for applicative decoders"
            """
import Json.Decode
import Json.Encode
import Json.Decode.Extra exposing (andMap)
"""
            "sample =\n    Debug.todo \"@json {\\\"a\\\": 1}\""
            """type alias Sample =
    { a : Int
    }


sampleDecoder : Json.Decode.Decoder Sample
sampleDecoder = 
    Json.Decode.succeed Sample
        |> andMap (Json.Decode.field "a" Json.Decode.int)


encodedSample : Sample -> Json.Encode.Value
encodedSample sample = 
    Json.Encode.object
        [ ( "a", Json.Encode.int sample.a )
        ]"""
        , describe "expression context"
            [ test "expression example" <|
                \_ ->
                    let
                        startCode : String
                        startCode =
                            """module A exposing (..)

import Json.Decode
import Json.Encode

sample =
    "{}" 
"""
                    in
                    startCode
                        |> Review.Test.run JsonToElm.rule
                        |> Review.Test.expectNoErrors
            ]
        ]


testCase : String -> String -> String -> String -> Test
testCase name imports errorThing expectedBody =
    test name <|
        \_ ->
            let
                expected : String
                expected =
                    "module A exposing (..)\n\n"
                        ++ imports
                        ++ "\n\n"
                        ++ expectedBody
                        |> String.replace "\u{000D}" ""
            in
            "module A exposing (..)\n\n"
                ++ imports
                ++ "\n\n"
                ++ errorThing
                |> String.replace "\u{000D}" ""
                |> Review.Test.run JsonToElm.rule
                |> Review.Test.expectErrors
                    [ Review.Test.error { message = "Here's my attempt to complete this stub", details = [ "" ], under = errorThing }
                        |> Review.Test.whenFixed expected
                    ]
