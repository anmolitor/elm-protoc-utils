module DurationTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Json.Encode
import Protobuf.Types.Int64 as Int64 exposing (Int64)
import Protobuf.Utils.Duration as Duration
import Protobuf.Utils.Internal as Internal
import Test exposing (..)


suite : Test
suite =
    describe "Duration"
        [ fuzz safeDurationFuzzer "conversion to millis roundtrips" <|
            \duration ->
                Duration.durationToMillis duration
                    |> Duration.millisToDuration
                    |> Expect.equal duration
        , test "corrects positive seconds with negative nanos" <|
            \_ ->
                Duration.durationToMillis { nanos = -1000000, seconds = Int64.fromInts 0 1 }
                    |> Duration.millisToDuration
                    |> Expect.equal { nanos = 999000000, seconds = Int64.fromInts 0 0 }
        , test "corrects negative seconds with positive nanos" <|
            \_ ->
                Duration.durationToMillis { nanos = 1000000, seconds = Int64.fromInts -1 -1 }
                    |> Duration.millisToDuration
                    |> Expect.equal { nanos = -999000000, seconds = Int64.fromInts 0 0 }
        , test "corrects nanos over second barrier" <|
            \_ ->
                Duration.durationToMillis { nanos = 1000000000, seconds = Int64.fromInts 0 0 }
                    |> Duration.millisToDuration
                    |> Expect.equal { nanos = 0, seconds = Int64.fromInts 0 1 }
        , test "loses nanosecond precision because posix is millisecond based" <|
            \_ ->
                Duration.durationToMillis { nanos = 1100000, seconds = Int64.fromInts 0 0 }
                    |> Duration.millisToDuration
                    |> Expect.equal { nanos = 1000000, seconds = Int64.fromInts 0 0 }
        , test "does not roundtrip in the unsafe int range" <|
            \_ ->
                let
                    duration =
                        { nanos = 1000000, seconds = Int64.fromInts (maxSafeHigherBitsInt + 1) 0 }
                in
                Duration.durationToMillis duration
                    |> Duration.millisToDuration
                    |> Expect.notEqual duration
        , test "roundtrips on the edge of the unsafe int range" <|
            \_ ->
                let
                    duration =
                        { nanos = 1000000, seconds = Int64.fromInts maxSafeHigherBitsInt 0 }
                in
                Duration.durationToMillis duration
                    |> Duration.millisToDuration
                    |> Expect.equal duration
        , test "does not roundtrip in the negative unsafe int range" <|
            \_ ->
                let
                    duration =
                        { nanos = -1000000, seconds = Int64.fromInts (-maxSafeHigherBitsInt - 1) 0 }
                in
                Duration.durationToMillis duration
                    |> Duration.millisToDuration
                    |> Expect.notEqual duration
        , test "roundtrips in the negative edge of the unsafe int range" <|
            \_ ->
                let
                    duration =
                        { nanos = -1000000, seconds = Int64.fromInts -maxSafeHigherBitsInt 0 }
                in
                Duration.durationToMillis duration
                    |> Duration.millisToDuration
                    |> Expect.equal duration
        , test "json encoding" <|
            \_ ->
                { nanos = 5000000, seconds = Int64.fromInts 2 43 }
                    |> Duration.durationJsonEncoder
                    |> Json.Encode.encode 0
                    |> Expect.equal "\"8589934635.005s\""
        , fuzz safeDurationFuzzer "json encoding roundtrips" <|
            \duration ->
                Duration.durationJsonEncoder duration
                    |> Json.Decode.decodeValue Duration.durationJsonDecoder
                    |> Expect.equal (Ok duration)
        ]


safeDurationFuzzer : Fuzzer Internal.TimestampOrDuration
safeDurationFuzzer =
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
