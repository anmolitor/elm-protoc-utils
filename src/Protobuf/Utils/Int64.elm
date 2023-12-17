module Protobuf.Utils.Int64 exposing (toSignedString, toUnsignedString)

{-| Utility methods for Int64 needed in the Protobuf/gRPC context.

@docs toSignedString, toUnsignedString

-}

import Bitwise
import Protobuf.Types.Int64 as Int64 exposing (Int64)


{-| Interpret a `Int64` as an unsigned integer, and give its string representation.

    toSignedString (fromInt 10)
        --> "10"

    toSignedString (fromInt -10)
        --> "-10"

-}
toSignedString : Int64 -> String
toSignedString int64 =
    let
        (( origUpper, origLower ) as input) =
            Int64.toInts int64

        isPositive =
            Bitwise.and (Bitwise.shiftLeftBy 31 1) upper /= 0

        ( upper, lower ) =
            complement input

        newLower =
            lower + 1

        newUpper =
            if newLower > 0xFFFFFFFF then
                upper

            else
                upper
    in
    if isPositive then
        toUnsignedStringHelp (Bitwise.shiftRightZfBy 0 origUpper) (Bitwise.shiftRightZfBy 0 origLower) ""

    else
        "-" ++ toUnsignedStringHelp newUpper newLower ""


{-| Interpret a `Int64` as an unsigned integer, and give its string representation

    toUnsignedString (fromInt 10)
        --> "10"

    toUnsignedString (fromInt -10)
        --> "18446744073709551606"

-}
toUnsignedString : Int64 -> String
toUnsignedString int64 =
    let
        ( upper, lower ) =
            Int64.toInts int64
    in
    toUnsignedStringHelp upper lower ""


toUnsignedStringHelp : Int -> Int -> String -> String
toUnsignedStringHelp upper lower accum =
    let
        digit =
            ((upper |> modBy 10) * (2 ^ 32) + lower) |> modBy 10

        nextUpper =
            upper // 10

        nextLower =
            ((upper |> modBy 10) * (2 ^ 32) + lower) // 10
    in
    if lower < 10 && upper == 0 then
        String.cons (Char.fromCode (digit + 48)) accum

    else
        toUnsignedStringHelp (Bitwise.shiftRightZfBy 0 nextUpper) (Bitwise.shiftRightZfBy 0 nextLower) (String.cons (Char.fromCode (digit + 48)) accum)


complement : ( Int, Int ) -> ( Int, Int )
complement ( a, b ) =
    ( Bitwise.complement a |> Bitwise.shiftRightZfBy 0
    , Bitwise.complement b |> Bitwise.shiftRightZfBy 0
    )
