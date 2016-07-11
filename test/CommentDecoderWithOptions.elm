module Main exposing (..)


decodeComment : Json.Decode.Decoder Comment
decodeComment =
    Json.Decode.succeed Comment
        |: ("commentPostId" := Json.Decode.int)
        |: ("commentText" := Json.Decode.string)
        |: ("commentMainCategories" := Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.string)
        |: ("commentPublished" := Json.Decode.bool)
        |: ("commentCreated" := Json.Decode.Extra.date)
        |: ("commentTags" := Json.Decode.map Dict.fromList (Json.Decode.list (Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.int)))
