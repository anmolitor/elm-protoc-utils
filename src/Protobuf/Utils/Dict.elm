module Protobuf.Utils.Dict exposing (dictDecoder)

{-| Utility methods for Dict needed in the Protobuf/gRPC context.

@docs dictDecoder

-}

import Dict exposing (Dict)
import Json.Decode


{-| Decode a dict from JSON with non-String key type.
This is done by first decoding into a dict/list of key/value pairs with Strings as keys
and then decoding each key while inserting them into a new dict.
-}
dictDecoder : (String -> Result String comparable) -> Json.Decode.Decoder v -> Json.Decode.Decoder (Dict comparable v)
dictDecoder keyDecoder valueDecoder =
    Json.Decode.keyValuePairs valueDecoder
        |> Json.Decode.andThen
            (List.foldl
                (\( k, v ) ->
                    Json.Decode.andThen
                        (\d ->
                            case keyDecoder k of
                                Ok ok ->
                                    Json.Decode.succeed (Dict.insert ok v d)

                                Err err ->
                                    Json.Decode.fail err
                        )
                )
                (Json.Decode.succeed Dict.empty)
            )
