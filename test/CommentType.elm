module CommentType exposing (..)

import Date exposing (Date)
import Dict exposing (Dict)


type alias Comment =
    { postId : Int
    , text : String
    , mainCategories : ( String, String )
    , published : Bool
    , created : Date
    , tags : Dict String Int
    }
