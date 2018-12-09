module CommentTypeWithOptions exposing (..)

import Time
import Dict exposing (Dict)


type alias Comment =
    { commentPostId : Int
    , commentText : String
    , commentMainCategories : (String, String)
    , commentPublished : Bool
    , commentCreated : Time.Posix
    , commentTags : Dict (String) (Int)
    }
