module DictTest exposing (..)

import Dict exposing (Dict)
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Protobuf.Utils.Dict as Dict
import Test exposing (..)


suite : Test
suite =
    describe "Dict dictDecoder"
        [ fuzz (dictFuzzer Fuzz.int Fuzz.string) "decode succeeds with int based dict" <|
            \dict ->
                dict
                    |> Json.Encode.dict String.fromInt Json.Encode.string
                    |> Json.Decode.decodeValue (Dict.dictDecoder decodeInt Json.Decode.string)
                    |> Expect.equal (Ok dict)
        , test "fails with the expected error message" <|
            \_ ->
                Json.Decode.decodeString (Dict.dictDecoder decodeInt Json.Decode.string) """{"NaN": "some string"}"""
                    |> Result.mapError Json.Decode.errorToString
                    |> Result.mapError (String.contains "Expected int")
                    |> Expect.equal (Err True)
        ]


decodeInt : String -> Result String Int
decodeInt =
    String.toInt >> Result.fromMaybe "Expected int"


dictFuzzer : Fuzzer comparable -> Fuzzer v -> Fuzzer (Dict comparable v)
dictFuzzer keyFuzzer valueFuzzer =
    Fuzz.pair keyFuzzer valueFuzzer
        |> Fuzz.list
        |> Fuzz.map Dict.fromList
