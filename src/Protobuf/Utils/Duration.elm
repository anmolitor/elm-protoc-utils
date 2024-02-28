module Protobuf.Utils.Duration exposing (millisToDuration, durationToMillis, durationJsonEncoder, durationJsonDecoder)

{-| Conversions between the Protobuf Well-Known Type "Duration" and Milliseconds as a Int to be compatable with Elm's `Time.Posix`.

@docs millisToDuration, durationToMillis, durationJsonEncoder, durationJsonDecoder

-}

import Json.Decode
import Json.Encode
import Protobuf.Utils.Internal as Internal


millisToDuration : Float -> Internal.TimestampOrDuration
millisToDuration =
    Internal.millisToTimestampOrDuration


durationToMillis : Internal.TimestampOrDuration -> Float
durationToMillis =
    Internal.timestampOrDurationToMillis


durationJsonEncoder : Internal.TimestampOrDuration -> Json.Encode.Value
durationJsonEncoder duration =
    String.fromFloat (durationToMillis duration / 1000)
        ++ "s"
        |> Json.Encode.string


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
