module Protobuf.Utils.Bytes exposing (jsonEncoder, jsonDecoder)

{-| Utility methods for Bytes needed in the Protobuf/gRPC context.

@docs jsonEncoder, jsonDecoder

-}

import Base64
import Bytes exposing (Bytes)
import Json.Decode
import Json.Encode


{-| Encode bytes to a the format expected by JSON-based gRPC APIs (base64 standard with padding)
-}
jsonEncoder : Bytes -> Json.Encode.Value
jsonEncoder bytes =
    Json.Encode.string (Base64.fromBytes bytes |> Maybe.withDefault "")


{-| Decode bytes from the format expected by JSON-based gRPC APIs (base64 standard with padding)
-}
jsonDecoder : Json.Decode.Decoder Bytes
jsonDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case Base64.toBytes str of
                    Just bytes ->
                        Json.Decode.succeed bytes

                    Nothing ->
                        Json.Decode.fail <| "Expected base64 sequence of chars but received: " ++ str
            )
