module Protobuf.Utils.Int32 exposing (int32JsonDecoder, uint32JsonDecoder)

{-| Utility methods for Int64 needed in the Protobuf/gRPC context.

@docs int32JsonDecoder, uint32JsonDecoder

-}

import Json.Decode


{-| Decodes an Int from a signed integer JSON string (i.e. {"key": "-123456789"})
or a JSON number literal (i.e. {"key": -123}).
Fails if the number is outside of the int32 range (-2^31 to 2^31 - 1)
-}
int32JsonDecoder : Json.Decode.Decoder Int
int32JsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.int
        , Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    case String.toInt str of
                        Just int ->
                            Json.Decode.succeed int

                        Nothing ->
                            Json.Decode.fail "Expected int"
                )
        ]
        |> Json.Decode.andThen
            (\int ->
                if int > 2 ^ 31 - 1 || int < -(2 ^ 31) then
                    Json.Decode.fail "Int was outside of int32 boundary"

                else
                    Json.Decode.succeed int
            )


{-| Decodes an Int from an unsigned integer JSON string (i.e. {"key": "123456789"})
or a JSON number literal (i.e. {"key": 123}).
Fails if the number is outside of the int32 range (0 to 2^32 - 1)
-}
uint32JsonDecoder : Json.Decode.Decoder Int
uint32JsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.int
        , Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    case String.toInt str of
                        Just int ->
                            Json.Decode.succeed int

                        Nothing ->
                            Json.Decode.fail "Expected uint"
                )
        ]
        |> Json.Decode.andThen
            (\int ->
                if int > 2 ^ 32 - 1 || int < 0 then
                    Json.Decode.fail "Int was outside of uint32 boundary"

                else
                    Json.Decode.succeed int
            )
