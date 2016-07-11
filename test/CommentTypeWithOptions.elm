module Main exposing (..)


type alias Comment =
    { commentPostId : Int
    , commentText : String
    , commentMainCategories : ( String, String )
    , commentPublished : Bool
    , commentCreated : Date
    , commentTags : Dict String Int
    }
