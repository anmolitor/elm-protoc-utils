module Protobuf.Utils.Int64 exposing
    ( toSignedString, toUnsignedString
    , fromSignedString, fromUnsignedString, fromInt, toIntUnsafe
    , int64JsonDecoder, uint64JsonDecoder, int64JsonEncoder, uint64JsonEncoder
    )

{-| Utility methods for Int64 needed in the Protobuf/gRPC context.

@docs toSignedString, toUnsignedString
@docs fromSignedString, fromUnsignedString, fromInt, toIntUnsafe
@docs int64JsonDecoder, uint64JsonDecoder, int64JsonEncoder, uint64JsonEncoder

-}

import Bitwise
import Json.Decode
import Json.Encode
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
                upper + 1

            else
                upper
    in
    if isPositive then
        toUnsignedStringHelp (Bitwise.shiftRightZfBy 0 origUpper) (Bitwise.shiftRightZfBy 0 origLower) ""

    else
        "-" ++ toUnsignedStringHelp newUpper (Bitwise.shiftRightZfBy 0 newLower) ""


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


{-| Encodes an Int64 as a signed integer JSON string
-}
int64JsonEncoder : Int64 -> Json.Encode.Value
int64JsonEncoder int64 =
    Json.Encode.string (toSignedString int64)


{-| Encodes an Int64 as an unsigned integer JSON string
-}
uint64JsonEncoder : Int64 -> Json.Encode.Value
uint64JsonEncoder int64 =
    Json.Encode.string (toUnsignedString int64)


{-| Decodes an Int64 from a signed integer JSON string (i.e. {"key": "-123456789"})
or a JSON number literal (i.e. {"key": -123})
-}
int64JsonDecoder : Json.Decode.Decoder Int64
int64JsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    case fromSignedString str of
                        Just int64 ->
                            Json.Decode.succeed int64

                        Nothing ->
                            Json.Decode.fail "Expected int64"
                )
        , Json.Decode.int |> Json.Decode.map fromInt
        ]


{-| Decodes an Int64 from an unsigned integer JSON string (i.e. {"key": "123456789"})
or a JSON number literal (i.e. {"key": 123})
-}
uint64JsonDecoder : Json.Decode.Decoder Int64
uint64JsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    case fromUnsignedString str of
                        Just int64 ->
                            Json.Decode.succeed int64

                        Nothing ->
                            Json.Decode.fail "Expected int64"
                )
        , Json.Decode.int |> Json.Decode.map fromInt
        ]


{-| Constructs an Int64 from an Int. The Int can be bigger than 32-bit,
you can expect Integers in the safe int range in JS (-(2^53 - 1) to 2^53 - 1) to work.
-}
fromInt : Int -> Int64
fromInt raw =
    if raw < 0 then
        let
            lower =
                raw
                    |> abs
                    |> Bitwise.complement
                    |> Bitwise.shiftRightZfBy 0
                    |> (+) 1

            upper =
                if lower > 0xFFFFFFFF then
                    raw // (2 ^ 32)

                else
                    raw // (2 ^ 32) - 1
        in
        Int64.fromInts
            (upper |> Bitwise.shiftRightZfBy 0)
            (lower |> Bitwise.shiftRightZfBy 0)

    else if raw > 0xFFFFFFFF then
        Int64.fromInts (raw // (2 ^ 32)) (Bitwise.shiftRightZfBy 0 raw)

    else
        Int64.fromInts 0 raw


{-| Attempts to convert the Int64 into a JS Int.
This should be safe for the safe int range in JS (-(2^53 - 1) to 2^53 - 1).
Expect weird results for anything outside that range.
-}
toIntUnsafe : Int64 -> Int
toIntUnsafe int64 =
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
                upper + 1

            else
                upper
    in
    if isPositive then
        origUpper * 2 ^ 32 + Bitwise.shiftRightZfBy 0 origLower

    else
        -1 * ((newUpper * 2 ^ 32) + Bitwise.shiftRightZfBy 0 newLower)


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
