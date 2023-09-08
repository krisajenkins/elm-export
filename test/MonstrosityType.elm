module MonstrosityType exposing (..)


type Monstrosity
    = NotSpecial
    | OkayIGuess Monstrosity
    | Ridiculous Int String (List (Monstrosity)) (Set (Float))
    | Dicts (Dict (Int) (())) (Dict (Float) (Float))
    | SortDicts (Sort.Dict.Dict (Id) (String)) (Sort.Dict.Dict (School) (())) (Sort.Dict.Dict (Color) (())) (Dict (Int) (String))
    | SortSet (Sort.Set.Set (School)) (Set (Int))
