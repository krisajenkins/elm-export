module Main (..) where


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
  Json.Encode.object
    [ ( "commentPostId", Json.Encode.int x.postId )
    , ( "commentText", Json.Encode.string x.text )
    , ( "commentMainCategories", Exts.Json.Encode.tuple2 Json.Encode.string Json.Encode.string x.mainCategories )
    , ( "commentPublished", Json.Encode.bool x.published )
    , ( "commentCreated", (Json.Encode.string << Exts.Date.toISOString) x.created )
    , ( "commentTags", Exts.Json.Encode.dict Json.Encode.string Json.Encode.int x.tags )
    ]
