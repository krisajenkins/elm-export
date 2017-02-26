module MonstrosityDecoder exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import MonstrosityType exposing (..)


decodeMonstrosity : Decoder Monstrosity
decodeMonstrosity =
    field "tag" string |> andThen ( \x ->
        if x == "NotSpecial" then decode NotSpecial
        else if x == "OkayIGuess" then decode OkayIGuess |> required "contents" decodeMonstrosity
        else if x == "Ridiculous" then decode Ridiculous |> required "contents" (index 0 int) |> required "contents" (index 1 string) |> required "contents" (index 2 (list decodeMonstrosity))
        else fail "Constructor not matched" )
