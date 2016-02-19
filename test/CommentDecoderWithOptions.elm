module Main (..) where


decodeComment : Json.Decode.Decoder Comment
decodeComment =
  Json.Decode.succeed Comment
    |: ("commentPostId" := Json.Decode.int)
    |: ("commentText" := Json.Decode.string)
    |: ("commentPublished" := Json.Decode.bool)
