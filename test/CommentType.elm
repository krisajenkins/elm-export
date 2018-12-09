module CommentType exposing (..)

import Time
import Dict exposing (Dict)


type alias Comment =
    { postId : Int
    , text : String
    , mainCategories : (String, String)
    , published : Bool
    , created : Time.Posix
    , tags : Dict (String) (Int)
    }
