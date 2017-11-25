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
                        decode Beginning

                    "Middle" ->
                        decode Middle

                    "End" ->
                        decode End

                    _ ->
                        fail "Constructor not matched"
            )
