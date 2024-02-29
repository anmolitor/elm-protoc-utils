module Protobuf.Utils.Timestamp exposing (posixToTimestamp, timestampToPosix, timestampJsonEncoder, timestampJsonDecoder)

{-| Conversions between the Protobuf Well-Known Type "Timestamp" and Elm's `Time.Posix`.

@docs posixToTimestamp, timestampToPosix, timestampJsonEncoder, timestampJsonDecoder

-}

import Iso8601
import Json.Decode
import Json.Encode
import Protobuf.Types.Int64 as Int64 exposing (Int64)
import Protobuf.Utils.Int64 as Int64
import Time


type alias Timestamp =
    { seconds : Int64
    , nanos : Int
    }


{-| Convert a posix millisecond timestamp into the protobuf seconds/nanos representation
-}
posixToTimestamp : Time.Posix -> Timestamp
posixToTimestamp posix =
    let
        millis =
            Time.posixToMillis posix

        seconds =
            -- avoid int32 math since millis may be in the "unsafe" 2^31 - 2^53 range
            if millis < 0 then
                -1 * floor (toFloat millis / -1000)

            else
                floor (toFloat millis / 1000)

        nanos =
            remainderBy 1000 millis * 1000000
    in
    { seconds = Int64.fromInt seconds, nanos = nanos }


{-| Convert the protobuf seconds/nanos representation into a posix millisecond timestamp
-}
timestampToPosix : Timestamp -> Time.Posix
timestampToPosix { seconds, nanos } =
    let
        int53Seconds =
            Int64.toIntUnsafe seconds

        millis =
            (int53Seconds * 1000) + (nanos // 1000000)
    in
    Time.millisToPosix millis


{-| Custom JSON encoder for timestamps as ISO-8601 date strings
-}
timestampJsonEncoder : Timestamp -> Json.Encode.Value
timestampJsonEncoder timestamp =
    timestampToPosix timestamp
        |> Iso8601.fromTime
        |> Json.Encode.string


{-| Custom JSON decoder for timestamps as ISO-8601 date strings
-}
timestampJsonDecoder : Json.Decode.Decoder Timestamp
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
