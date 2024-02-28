module Protobuf.Utils.Duration exposing (millisToDuration, durationToMillis, durationJsonEncoder, durationJsonDecoder)

{-| Conversions between the Protobuf Well-Known Type "Duration" and Milliseconds as a Int to be compatable with Elm's `Time.Posix`.

@docs millisToDuration, durationToMillis, durationJsonEncoder, durationJsonDecoder

-}

import Json.Decode
import Json.Encode
import Protobuf.Utils.Internal as Internal


{-| Convert a number of milliseconds to a Duration.
Note that this only guarantees keeping precision in the millisecond range -
micro/nanoseconds can be lost due to floating point arithmetic.
-}
millisToDuration : Float -> Internal.TimestampOrDuration
millisToDuration =
    Internal.millisToTimestampOrDuration


{-| Convert a Duration to a number of milliseconds.
Note that this only guarantees keeping precision in the millisecond range -
micro/nanoseconds can be lost due to floating point arithmetic.
-}
durationToMillis : Internal.TimestampOrDuration -> Float
durationToMillis =
    Internal.timestampOrDurationToMillis


{-| Custom Json Encoder for Duration, which formats a duration as a string.
For example, 123 seconds and 450 milliseconds is encoded as "123.45s".
-}
durationJsonEncoder : Internal.TimestampOrDuration -> Json.Encode.Value
durationJsonEncoder duration =
    String.fromFloat (durationToMillis duration / 1000)
        ++ "s"
        |> Json.Encode.string


{-| Custom Json Decoder for Duration, which accepts a duration as a string
matching the above format (number of seconds with a "s" suffix).
-}
durationJsonDecoder : Json.Decode.Decoder Internal.TimestampOrDuration
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
