module MonstrosityDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import MonstrosityType exposing (..)


decodeMonstrosity : Decoder Monstrosity
decodeMonstrosity =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "NotSpecial" ->
                        decode NotSpecial

                    "OkayIGuess" ->
                        decode OkayIGuess
                            |> required "contents" decodeMonstrosity

                    "Ridiculous" ->
                        decode Ridiculous
                            |> required "contents" (index 0 int)
                            |> required "contents" (index 1 string)
                            |> required "contents" (index 2 (list decodeMonstrosity))

                    _ ->
                        fail "Constructor not matched"
            )
