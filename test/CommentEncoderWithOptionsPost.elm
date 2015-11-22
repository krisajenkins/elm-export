module Main (..) where


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
  Json.Encode.object
    [ ( "postPostId", Json.Encode.int x.postId )
    , ( "postText", Json.Encode.string x.text )
    , ( "postMainCategories", Exts.Json.Encode.tuple2 Json.Encode.string Json.Encode.string x.mainCategories )
    , ( "postPublished", Json.Encode.bool x.published )
    , ( "postCreated", (Json.Encode.string << Exts.Date.toISOString) x.created )
    , ( "postTags", Exts.Json.Encode.dict Json.Encode.string Json.Encode.int x.tags )
    ]
