module Protobuf.Utils.Float exposing (..)

{-| Utility methods for Float needed in the Protobuf/gRPC context.

@docs stringOrFloatJsonDecoder

-}

import Json.Decode


{-| Decode a float from a JSON float (1.23, 2.3e4)
or a String formatted in the same way ("1.23", "2.3e4").
Also accepts the special strings "Infinity", "-Infinity" and "NaN" resulting in
`1/0`, `-1/0` and `0/0` respectively.
-}
stringOrFloatJsonDecoder : Json.Decode.Decoder Float
stringOrFloatJsonDecoder =
    Json.Decode.oneOf
        [ Json.Decode.float
        , Json.Decode.string
            |> Json.Decode.andThen
                (\str ->
                    if str == "NaN" then
                        Json.Decode.succeed (0 / 0)

                    else
                        case String.toFloat str of
                            Just f ->
                                Json.Decode.succeed f

                            Nothing ->
                                Json.Decode.fail "Expected float"
                )
        ]
