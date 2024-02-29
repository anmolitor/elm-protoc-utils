module Protobuf.Utils.Duration exposing (millisToDuration, durationToMillis, durationJsonEncoder, durationJsonDecoder)

{-| Conversions between the Protobuf Well-Known Type "Duration" and Milliseconds as a Int to be compatable with Elm's `Time.Posix`.

@docs millisToDuration, durationToMillis, durationJsonEncoder, durationJsonDecoder

-}

import Json.Decode
import Json.Encode
import Protobuf.Types.Int64 exposing (Int64)
import Protobuf.Utils.Int64 as Int64


type alias Duration =
    { seconds : Int64
    , nanos : Int
    }


{-| Convert a number of milliseconds to a Duration.
Note that this only guarantees keeping precision in the millisecond range -
micro/nanoseconds can be lost due to floating point arithmetic.
-}
millisToDuration : Float -> Duration
millisToDuration millis =
    let
        seconds =
            -- avoid int32 math since millis may be in the "unsafe" 2^31 - 2^53 range
            if millis < 0 then
                -1 * floor (millis / -1000)

            else
                floor (millis / 1000)

        nanos =
            floor (millis - toFloat (1000 * seconds)) * 1000000
    in
    { seconds = Int64.fromInt seconds, nanos = nanos }


{-| Convert a Duration to a number of milliseconds.
Note that this only guarantees keeping precision in the millisecond range -
micro/nanoseconds can be lost due to floating point arithmetic.
-}
durationToMillis : Duration -> Float
durationToMillis { seconds, nanos } =
    let
        int53Seconds =
            Int64.toIntUnsafe seconds

        millis =
            (toFloat int53Seconds * 1000) + (toFloat nanos / 1000000)
    in
    millis


{-| Custom Json Encoder for Duration, which formats a duration as a string.
For example, 123 seconds and 450 milliseconds is encoded as "123.45s".
-}
durationJsonEncoder : Duration -> Json.Encode.Value
durationJsonEncoder duration =
    String.fromFloat (durationToMillis duration / 1000)
        ++ "s"
        |> Json.Encode.string


{-| Custom Json Decoder for Duration, which accepts a duration as a string
matching the above format (number of seconds with a "s" suffix).
-}
durationJsonDecoder : Json.Decode.Decoder Duration
durationJsonDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                if String.right 1 str /= "s" then
                    Json.Decode.fail "Expected float with 's' suffix"

                else
                    case String.toFloat (String.dropRight 1 str) of
                        Just seconds ->
                            (seconds * 1000)
                                |> millisToDuration
                                |> Json.Decode.succeed

                        Nothing ->
                            Json.Decode.fail "Expected float with 's' suffix"
            )
