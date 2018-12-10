module PositionDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import PositionType exposing (..)


decodePosition : Decoder Position
decodePosition =
    string
        |> andThen
            (\x ->
                case x of
                    "Beginning" ->
                        Json.Decode.succeed Beginning

                    "Middle" ->
                        Json.Decode.succeed Middle

                    "End" ->
                        Json.Decode.succeed End

                    _ ->
                        fail "Constructor not matched"
            )
