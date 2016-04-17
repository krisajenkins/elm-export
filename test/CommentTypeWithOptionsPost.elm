module Main (..) where


type alias Comment =
  { postPostId : Int
  , postText : String
  , postMainCategories : ( String, String )
  , postPublished : Bool
  , postCreated : Date
  , postTags : Dict String Int
  }
