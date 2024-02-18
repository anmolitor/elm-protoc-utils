module Protobuf.Utils.Int64 exposing
    ( toSignedString, toUnsignedString
    ,fromSignedString, fromUnsignedString
    )

{-| Utility methods for Int64 needed in the Protobuf/gRPC context.

@docs toSignedString, toUnsignedString
@docs fromSignedString, fromUnsignedString

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
    toUnsignedStringHelp (Bitwise.shiftRightZfBy 0 upper) (Bitwise.shiftRightZfBy 0 lower) ""


{-| Parse a signed Int64 from its string representation

    fromSignedString "10"
        --> Just (Int64 { higher = 0, lower = 10 })

    fromSignedString "-10"
        --> Just (Int64 { higher = -1, lower = -10 })

    fromSignedString "not-a-number"
        --> Nothing

    fromSignedString "9223372036854775807"
        --> Just (Int64 { higher = 2147483647, lower = -1 })

    fromSignedString "-9223372036854775808"
        --> Just (Int64 { higher = -2147483648, lower = 0 })

    fromSignedString "-9223372036854775809"
        --> Nothing

-}
fromSignedString : String -> Maybe Int64
fromSignedString str =
    case String.uncons str of
        Just ( '-', rest ) ->
            if (String.length rest == 19 && rest > "9223372036854775808") || String.length rest > 19 then
                Nothing

            else
                Maybe.map
                    (\int64 ->
                        let
                            ( newH, newL ) =
                                complement int64 |> add 1
                        in
                        Int64.fromInts newH newL
                    )
                    (fromUnsignedStringHelp rest ( 0, 0 ))

        _ ->
            if (String.length str == 19 && str > "9223372036854775807") || String.length str > 19 then
                Nothing

            else
                fromUnsignedStringHelp str ( 0, 0 )
                    |> Maybe.map (\( x, y ) -> Int64.fromInts x y)


{-| Parse an unsigned Int64 from its string representation

    fromUnsignedString "10"
        --> Just (Int64 { higher = 0, lower = 10 })

    fromUnsignedString "-10"
        --> Nothing

    fromUnsignedString "not-a-number"
        --> Nothing

    fromUnsignedString "9223372036854775807"
        --> Just (Int64 { higher = 2147483647, lower = -1 })

    fromUnsignedString "18446744073709551615"
        --> Just (Int64 { higher = -1, lower = -1 })

    fromUnsignedString "18446744073709551616"
        --> Nothing

-}
fromUnsignedString : String -> Maybe Int64
fromUnsignedString str =
    if (String.length str == 20 && str > "18446744073709551615") || String.startsWith "-" str || String.length str > 20 then
        Nothing

    else
        fromUnsignedStringHelp str ( 0, 0 ) |> Maybe.map (\( x, y ) -> Int64.fromInts x y)


maxSafeDigitsAtOnce : Int
maxSafeDigitsAtOnce =
    6


fromUnsignedStringHelp : String -> ( Int, Int ) -> Maybe ( Int, Int )
fromUnsignedStringHelp str int64 =
    case ( String.slice 0 maxSafeDigitsAtOnce str |> String.toInt, String.dropLeft maxSafeDigitsAtOnce str ) of
        ( Nothing, _ ) ->
            Nothing

        ( Just int, "" ) ->
            multiply (10 ^ String.length str) int64 |> add int |> Just

        ( Just int, rest ) ->
            multiply (10 ^ maxSafeDigitsAtOnce) int64
                |> add int
                |> fromUnsignedStringHelp rest


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


multiply : Int -> ( Int, Int ) -> ( Int, Int )
multiply n ( higher, lower ) =
    let
        lowerFull =
            lower * n

        lowerCarry =
            lowerFull // 0x0000000100000000

        newLower =
            lowerFull - lowerCarry * 0x0000000100000000

        newHigher =
            higher * n + lowerCarry
    in
    ( newHigher, newLower )


add : Int -> ( Int, Int ) -> ( Int, Int )
add n ( higher, lower ) =
    let
        newLower =
            lower + n
    in
    if newLower > 0xFFFFFFFF then
        ( higher + 1, Bitwise.and 0xFFFFFFFF newLower )

    else
        ( higher, newLower )
