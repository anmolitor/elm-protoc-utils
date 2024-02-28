module Protobuf.Utils.Internal exposing (..)

import Protobuf.Types.Int64 exposing (Int64)
import Protobuf.Utils.Int64 as Int64


type alias TimestampOrDuration =
    { seconds : Int64
    , nanos : Int
    }


millisToTimestampOrDuration : Float -> TimestampOrDuration
millisToTimestampOrDuration millis =
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


timestampOrDurationToMillis : TimestampOrDuration -> Float
timestampOrDurationToMillis { seconds, nanos } =
    let
        int53Seconds =
            Int64.toIntUnsafe seconds

        millis =
            (toFloat int53Seconds * 1000) + (toFloat nanos / 1000000)
    in
    millis
