module CommentTypeWithOptions exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)


type alias Comment =
    { commentPostId : Int
    , commentText : String
    , commentMainCategories : ( String, String )
    , commentPublished : Bool
    , commentCreated : Date
    , commentTags : Dict String Int
    }
