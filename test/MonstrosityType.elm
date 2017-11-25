module MonstrosityType exposing (..)


type Monstrosity
    = NotSpecial
    | OkayIGuess Monstrosity
    | Ridiculous Int String (List (Monstrosity))
