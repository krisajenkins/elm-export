module Main (..) where


decodeComment : Json.Decode.Decoder Comment
decodeComment =
  Json.Decode.succeed Comment
    |: ("postPostId" := Json.Decode.int)
    |: ("postText" := Json.Decode.string)
    |: ("postMainCategories" := Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.string)
    |: ("postPublished" := Json.Decode.bool)
    |: ("postCreated" := Json.Decode.Extra.date)
    |: ("postTags" := Json.Decode.map Dict.fromList (Json.Decode.list (Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.int)))
