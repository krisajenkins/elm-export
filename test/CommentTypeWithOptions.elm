module Main (..) where


type alias Comment =
  { commentPostId : Int
  , commentText : String
  , commentPublished : Bool
  , commentCreated : Date
  }
