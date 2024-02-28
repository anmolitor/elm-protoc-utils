module Protobuf.Utils.Timestamp exposing (posixToTimestamp, timestampToPosix, timestampJsonEncoder, timestampJsonDecoder)

{-| Conversions between the Protobuf Well-Known Type "Timestamp" and Elm's `Time.Posix`.

@docs posixToTimestamp, timestampToPosix, timestampJsonEncoder, timestampJsonDecoder

-}

import Iso8601
import Json.Decode
import Json.Encode
import Protobuf.Types.Int64 as Int64
import Protobuf.Utils.Int64 as Int64
import Protobuf.Utils.Internal as Internal
import Time


{-| Convert a posix millisecond timestamp into the protobuf seconds/nanos representation
-}
posixToTimestamp : Time.Posix -> Internal.TimestampOrDuration
posixToTimestamp posix =
    Internal.millisToTimestampOrDuration (toFloat <| Time.posixToMillis posix)


{-| Convert the protobuf seconds/nanos representation into a posix millisecond timestamp
-}
timestampToPosix : Internal.TimestampOrDuration -> Time.Posix
timestampToPosix timestamp =
    Time.millisToPosix (floor <| Internal.timestampOrDurationToMillis timestamp)


{-| Custom JSON encoder for timestamps as ISO-8601 date strings
-}
timestampJsonEncoder : Internal.TimestampOrDuration -> Json.Encode.Value
timestampJsonEncoder timestamp =
    timestampToPosix timestamp
        |> Iso8601.fromTime
        |> Json.Encode.string


{-| Custom JSON decoder for timestamps as ISO-8601 date strings
-}
timestampJsonDecoder : Json.Decode.Decoder Internal.TimestampOrDuration
timestampJsonDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case Iso8601.toTime str of
                    Ok posix ->
                        Json.Decode.succeed (posixToTimestamp posix)

                    Err _ ->
                        Json.Decode.fail <| "Expected Iso8601 string, received: " ++ str
            )
