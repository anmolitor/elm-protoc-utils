module BytesTest exposing (..)

import Bytes exposing (Bytes)
import Bytes.Decode
import Bytes.Encode
import Expect
import Fuzz exposing (Fuzzer)
import Json.Decode
import Protobuf.Utils.Bytes as Bytes
import Test exposing (..)


suite : Test
suite =
    describe "Bytes"
        [ fuzz bytesFuzzer "json encoder and decoder roundtrip" <|
            \bytes ->
                Bytes.jsonEncoder bytes
                    |> Json.Decode.decodeValue Bytes.jsonDecoder
                    -- Bytes equality is bugged in Elm so we compare the bytes as ints instead
                    |> Result.map (bytesEqual bytes)
                    |> Expect.equal (Ok True)
        ]


bytesFuzzer : Fuzzer Bytes
bytesFuzzer =
    Fuzz.list Fuzz.int
        |> Fuzz.map
            (List.map (Bytes.Encode.signedInt32 Bytes.BE)
                >> Bytes.Encode.sequence
                >> Bytes.Encode.encode
            )


bytesEqual : Bytes -> Bytes -> Bool
bytesEqual b1 b2 =
    bytesToIntList b1 == bytesToIntList b2


bytesToIntList : Bytes -> Maybe (List Int)
bytesToIntList bytes =
    let
        decoder =
            Bytes.Decode.loop ( [], Bytes.width bytes )
                (\( prevInts, leftoverBytes ) ->
                    if leftoverBytes <= 0 then
                        Bytes.Decode.Done prevInts
                            |> Bytes.Decode.succeed

                    else
                        Bytes.Decode.signedInt8
                            |> Bytes.Decode.map (\int -> Bytes.Decode.Loop ( int :: prevInts, leftoverBytes - 1 ))
                )
    in
    Bytes.Decode.decode decoder bytes
