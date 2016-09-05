module Main exposing (..)


type alias Comment =
  { postId : Int
  , text : String
  , mainCategories : ( String, String )
  , published : Bool
  , created : Date
  , tags : Dict String Int
  }
