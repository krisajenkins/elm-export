module Main exposing (..)


decodeComment : Json.Decode.Decoder Comment
decodeComment =
  Json.Decode.succeed Comment
    |: ("postId" := Json.Decode.int)
    |: ("text" := Json.Decode.string)
    |: ("mainCategories" := Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.string)
    |: ("published" := Json.Decode.bool)
    |: ("created" := Json.Decode.Extra.date)
    |: ("tags" := Json.Decode.map Dict.fromList (Json.Decode.list (Json.Decode.tuple2 (,) Json.Decode.string Json.Decode.int)))
