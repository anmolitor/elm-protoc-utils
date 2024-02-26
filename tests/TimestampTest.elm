module TimestampTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Protobuf.Types.Int64 as Int64 exposing (Int64)
import Protobuf.Utils.Timestamp as Timestamp
import Test exposing (..)


suite : Test
suite =
    describe "Timestamp"
        [ fuzz safeTimestampFuzzer "roundtrips in the safe JS int range" <|
            \timestamp ->
                Timestamp.timestampToPosix timestamp
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal timestamp
        , test "corrects positive seconds with negative nanos" <|
            \_ ->
                Timestamp.timestampToPosix { nanos = -1000000, seconds = Int64.fromInts 0 1 }
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal { nanos = 999000000, seconds = Int64.fromInts 0 0 }
        , test "corrects negative seconds with positive nanos" <|
            \_ ->
                Timestamp.timestampToPosix { nanos = 1000000, seconds = Int64.fromInts -1 -1 }
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal { nanos = -999000000, seconds = Int64.fromInts 0 0 }
        , test "corrects nanos over second barrier" <|
            \_ ->
                Timestamp.timestampToPosix { nanos = 1000000000, seconds = Int64.fromInts 0 0 }
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal { nanos = 0, seconds = Int64.fromInts 0 1 }
        , test "loses nanosecond precision because posix is millisecond based" <|
            \_ ->
                Timestamp.timestampToPosix { nanos = 1100000, seconds = Int64.fromInts 0 0 }
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal { nanos = 1000000, seconds = Int64.fromInts 0 0 }
        , test "does not roundtrip in the unsafe int range" <|
            \_ ->
                let
                    timestamp =
                        { nanos = 1000000, seconds = Int64.fromInts (maxSafeHigherBitsInt + 1) 0 }
                in
                Timestamp.timestampToPosix timestamp
                    |> Timestamp.posixToTimestamp
                    |> Expect.notEqual timestamp
        , test "roundtrips on the edge of the unsafe int range" <|
            \_ ->
                let
                    timestamp =
                        { nanos = 1000000, seconds = Int64.fromInts maxSafeHigherBitsInt 0 }
                in
                Timestamp.timestampToPosix timestamp
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal timestamp
        , test "does not roundtrip in the negative unsafe int range" <|
            \_ ->
                let
                    timestamp =
                        { nanos = -1000000, seconds = Int64.fromInts (-maxSafeHigherBitsInt - 1) 0 }
                in
                Timestamp.timestampToPosix timestamp
                    |> Timestamp.posixToTimestamp
                    |> Expect.notEqual timestamp
        , test "roundtrips in the negative edge of the unsafe int range" <|
            \_ ->
                let
                    timestamp =
                        { nanos = -1000000, seconds = Int64.fromInts -maxSafeHigherBitsInt 0 }
                in
                Timestamp.timestampToPosix timestamp
                    |> Timestamp.posixToTimestamp
                    |> Expect.equal timestamp
        , fuzz safeForIso8601FormatTimestampFuzzer "json decoding roundtrips for safe timestamps" <|
            \timestamp ->
                Timestamp.timestampJsonEncoder timestamp
                    |> Json.Decode.decodeValue Timestamp.timestampJsonDecoder
                    |> Expect.equal (Ok timestamp)
        , test "json decoding fails when the year 10000 is passed" <|
            \_ ->
                Timestamp.timestampJsonEncoder { nanos = 0, seconds = Int64.fromInts 58 4294311936 }
                    |> Json.Decode.decodeValue Timestamp.timestampJsonDecoder
                    |> Expect.err
        , test "json decoding fails when the year 0 is passed" <|
            \_ ->
                Timestamp.timestampJsonEncoder { nanos = 0, seconds = Int64.fromInts -15 0 }
                    |> Json.Decode.decodeValue Timestamp.timestampJsonDecoder
                    |> Expect.err
        ]


safeTimestampFuzzer : Fuzzer Timestamp
safeTimestampFuzzer =
    Fuzz.map3
        (\secH secL nanos ->
            let
                secs =
                    Int64.fromInts secH secL
            in
            -- Make sure seconds and nanos have the same sign
            if (secH == 0 && nanos < 0) || secH * nanos < 0 then
                { seconds = secs, nanos = -1 * nanos }

            else
                { seconds = secs, nanos = nanos }
        )
        safeHigherSecondsBitsFuzzer
        Fuzz.int
        safeNanosFuzzer


{-| The Iso8601 Format cannot deal with the year 10000 since the format is YYYY-...
-}
safeForIso8601FormatTimestampFuzzer : Fuzzer Timestamp
safeForIso8601FormatTimestampFuzzer =
    safeTimestampFuzzer
        |> Fuzz.map
            (\{ seconds, nanos } ->
                let
                    ( secH, secL ) =
                        Int64.toInts seconds

                    newSecH =
                        clamp -14 57 secH
                in
                { seconds = Int64.fromInts newSecH secL, nanos = nanos }
            )


safeNanosFuzzer : Fuzzer Int
safeNanosFuzzer =
    Fuzz.map (\n -> (n // 1000000) * 1000000) Fuzz.int
        |> Fuzz.map (remainderBy 1000000000)


safeHigherSecondsBitsFuzzer : Fuzzer Int
safeHigherSecondsBitsFuzzer =
    Fuzz.intRange (-1 * maxSafeHigherBitsInt) maxSafeHigherBitsInt


maxSafeHigherBitsInt : Int
maxSafeHigherBitsInt =
    floor ((2 ^ 53 - 1) / (2 ^ 32 * 1000))


type alias Timestamp =
    { seconds : Int64
    , nanos : Int
    }
