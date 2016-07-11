module Main exposing (..)


type alias Post =
    { postId : Int
    , postName : String
    , postAge : Maybe Float
    , postComments : List Comment
    , postPromoted : Maybe Comment
    , postAuthor : Maybe String
    }
