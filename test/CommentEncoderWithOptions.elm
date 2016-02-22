module Main (..) where


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
  Json.Encode.object
    [ ( "commentPostId", Json.Encode.int x.postId )
    , ( "commentText", Json.Encode.string x.text )
    , ( "commentPublished", Json.Encode.bool x.published )
    , ( "commentCreated", (Json.Encode.string << Exts.Date.toISOString) x.created )
    ]
