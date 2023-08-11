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
                        succeed NotSpecial

                    "OkayIGuess" ->
                        succeed OkayIGuess
                            |> required "contents" decodeMonstrosity

                    "Ridiculous" ->
                        succeed Ridiculous
                            |> required "contents" (index 0 int)
                            |> required "contents" (index 1 string)
                            |> required "contents" (index 2 (list decodeMonstrosity))
                            |> required "contents" (index 3 (map Set.fromList (list float)))

                    "Dicts" ->
                        succeed Dicts
                            |> required "contents" (index 0 (Json.Decode.Extra.dict2 int (succeed ())))
                            |> required "contents" (index 1 (Json.Decode.Extra.dict2 float float))

                    _ ->
                        fail "Constructor not matched"
            )
