module FloatTest exposing (..)

import Expect
import Fuzz
import Json.Decode
import Json.Encode
import Protobuf.Utils.Float as Float
import Test exposing (..)


suite : Test
suite =
    describe "Float stringOrFloatJsonDecoder"
        [ fuzz Fuzz.niceFloat "decodes floats" <|
            \float ->
                Json.Encode.float float
                    |> Json.Decode.decodeValue Float.stringOrFloatJsonDecoder
                    |> Expect.equal (Ok float)
        , fuzz Fuzz.niceFloat "decodes floats in string format" <|
            \float ->
                String.fromFloat float
                    |> Json.Encode.string
                    |> Json.Decode.decodeValue Float.stringOrFloatJsonDecoder
                    |> Expect.equal (Ok float)
        , test "decodes floats in exponent notation" <|
            \_ ->
                Json.Decode.decodeString Float.stringOrFloatJsonDecoder "3.14e5"
                    |> Expect.equal (Ok 3.14e5)
        , test "decodes strings in exponent notation" <|
            \_ ->
                Json.Decode.decodeString Float.stringOrFloatJsonDecoder "\"3.14e5\""
                    |> Expect.equal (Ok 3.14e5)
        , test "decodes Infinity" <|
            \_ ->
                Json.Decode.decodeString Float.stringOrFloatJsonDecoder "\"Infinity\""
                    |> Expect.equal (Ok <| 1 / 0)
        , test "decodes -Infinity" <|
            \_ ->
                Json.Decode.decodeString Float.stringOrFloatJsonDecoder "\"-Infinity\""
                    |> Expect.equal (Ok <| -1 / 0)
        , test "decodes NaN" <|
            \_ ->
                Json.Decode.decodeString Float.stringOrFloatJsonDecoder "\"NaN\""
                    |> Result.map isNaN
                    |> Expect.equal (Ok True)
        ]
