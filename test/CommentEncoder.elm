module Main (..) where


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
  Json.Encode.object
    [ ( "postId", Json.Encode.int x.postId )
    , ( "text", Json.Encode.string x.text )
    , ( "published", Json.Encode.bool x.published )
    ]
