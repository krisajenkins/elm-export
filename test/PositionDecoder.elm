module Main exposing (..)


fromStringPosition : String -> Result String Position
fromStringPosition string = 
    case string of
        "Beginning" -> Result.Ok Beginning
        "End" -> Result.Ok End
        "Middle" -> Result.Ok Middle
        _ -> Result.Err ("Not valid pattern for decoder to Position. Pattern: " ++ (toString string))

decodePosition : Json.Decode.Decoder Position
decodePosition =
    Json.Decode.string `Json.Decode.andThen` fromStringPosition
