module Main (..) where


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
  Json.Encode.object
    [ ( "postId", Json.Encode.int x.postId )
    , ( "text", Json.Encode.string x.text )
    , ( "mainCategories", Exts.Json.Encode.tuple2 Json.Encode.string Json.Encode.string x.mainCategories )
    , ( "published", Json.Encode.bool x.published )
    , ( "created", (Json.Encode.string << Exts.Date.toISOString) x.created )
    , ( "tags", Exts.Json.Encode.dict Json.Encode.string Json.Encode.int x.tags )
    ]
