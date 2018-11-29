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
                        succeed Beginning

                    "Middle" ->
                        succeed Middle

                    "End" ->
                        succeed End

                    _ ->
                        fail "Constructor not matched"
            )
