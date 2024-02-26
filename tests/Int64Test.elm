module Int64Test exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Protobuf.Types.Int64 as Int64
import Protobuf.Utils.Int64 as Int64
import Test exposing (..)


suite : Test
suite =
    describe "Int64"
        [ fuzz fuzzInt64 "from/toUnsignedString" <|
            \int64 ->
                Int64.toUnsignedString int64
                    |> Int64.fromUnsignedString
                    |> Expect.equal (Just int64)
        , fuzz fuzzInt64 "from/toSignedString" <|
            \int64 ->
                Int64.toSignedString int64
                    |> Int64.fromSignedString
                    |> Expect.equal (Just int64)
        , fuzz Fuzz.int "fromInt 32-bit range" <|
            \n ->
                Int64.fromInt n
                    |> Int64.toSignedString
                    |> Expect.equal (String.fromInt n)
        , test "fromInt some large JS int" <|
            \_ ->
                Int64.fromInt 2251799822073856
                    |> Int64.toSignedString
                    |> Expect.equal "2251799822073856"
        , test "fromInt some large negative JS int" <|
            \_ ->
                Int64.fromInt -2251799822073856
                    |> Int64.toSignedString
                    |> Expect.equal "-2251799822073856"
        , test "fromInt min safe JS int" <|
            \_ ->
                Int64.fromInt -(2 ^ 53 - 1)
                    |> Int64.toSignedString
                    |> Expect.equal "-9007199254740991"
        , test "fromInt max safe JS int" <|
            \_ ->
                Int64.fromInt (2 ^ 53 - 1)
                    |> Int64.toSignedString
                    |> Expect.equal "9007199254740991"
        , test "signed int json decoder accepts ints" <|
            \_ ->
                Json.Decode.decodeString Int64.int64JsonDecoder "-9007199254740987"
                    |> Result.map Int64.toSignedString
                    |> Expect.equal (Ok "-9007199254740987")
        , test "signed int json decoder fails for ints outside of the safe JS range" <|
            \_ ->
                Json.Decode.decodeString Int64.int64JsonDecoder "-90071992547409870"
                    |> Result.map Int64.toSignedString
                    |> Expect.notEqual (Ok "-90071992547409870")
        , test "signed int json decoder accepts strings" <|
            \_ ->
                Json.Decode.decodeString Int64.int64JsonDecoder "\"-9007199254740987\""
                    |> Result.map Int64.toSignedString
                    |> Expect.equal (Ok "-9007199254740987")
        , test "signed int json decoder accepts strings outside of the safe JS range" <|
            \_ ->
                Json.Decode.decodeString Int64.int64JsonDecoder "\"-90071992547409870\""
                    |> Result.map Int64.toSignedString
                    |> Expect.equal (Ok "-90071992547409870")
        , test "signed int json encoder produces strings" <|
            \_ ->
                Int64.fromSignedString "-90071992547409870"
                    |> Maybe.map Int64.int64JsonEncoder
                    |> Maybe.map (Json.Encode.encode 0)
                    |> Expect.equal (Just "\"-90071992547409870\"")
        , test "unsigned int json decoder accepts ints" <|
            \_ ->
                Json.Decode.decodeString Int64.uint64JsonDecoder "9007199254740987"
                    |> Result.map Int64.toUnsignedString
                    |> Expect.equal (Ok "9007199254740987")
        , test "unsigned int json decoder fails for ints outside of the safe JS range" <|
            \_ ->
                Json.Decode.decodeString Int64.uint64JsonDecoder "-90071992547409870"
                    |> Result.map Int64.toUnsignedString
                    |> Expect.notEqual (Ok "-90071992547409870")
        , test "unsigned int json decoder accepts strings" <|
            \_ ->
                Json.Decode.decodeString Int64.uint64JsonDecoder "\"9007199254740987\""
                    |> Result.map Int64.toUnsignedString
                    |> Expect.equal (Ok "9007199254740987")
        , test "unsigned int json decoder accepts strings outside of the safe JS range" <|
            \_ ->
                Json.Decode.decodeString Int64.uint64JsonDecoder "\"90071992547409870\""
                    |> Result.map Int64.toUnsignedString
                    |> Expect.equal (Ok "90071992547409870")
        , test "unsigned int json encoder produces strings" <|
            \_ ->
                Int64.fromUnsignedString "90071992547409870"
                    |> Maybe.map Int64.uint64JsonEncoder
                    |> Maybe.map (Json.Encode.encode 0)
                    |> Expect.equal (Just "\"90071992547409870\"")
        ]


fuzzInt64 : Fuzzer Int64.Int64
fuzzInt64 =
    Fuzz.map2 Int64.fromInts Fuzz.int Fuzz.int
