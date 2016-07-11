module PostType exposing (..)

import CommentType exposing (..)


type alias Post =
    { id : Int
    , name : String
    , age : Maybe Float
    , comments : List Comment
    , promoted : Maybe Comment
    , author : Maybe String
    }
