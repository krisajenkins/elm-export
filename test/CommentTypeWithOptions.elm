module CommentTypeWithOptions exposing (Comment)

import Dict exposing (Dict)
import Time


type alias Comment =
    { commentPostId : Int
    , commentText : String
    , commentMainCategories : ( String, String )
    , commentPublished : Bool
    , commentCreated : Time.Posix
    , commentTags : Dict String Int
    }
