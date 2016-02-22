module Main (..) where


decodeComment : Json.Decode.Decoder Comment
decodeComment =
  Json.Decode.succeed Comment
    |: ("postId" := Json.Decode.int)
    |: ("text" := Json.Decode.string)
    |: ("published" := Json.Decode.bool)
    |: ("created" := Json.Decode.Extra.date)
