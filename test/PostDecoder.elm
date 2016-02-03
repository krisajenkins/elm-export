decodePost : Json.Decode.Decoder Post
decodePost =
  Json.Decode.succeed Post
    |: ("id" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("age" := Json.Decode.maybe Json.Decode.float)
    |: ("comments" := Json.Decode.list decodeComment)
    |: ("promoted" := Json.Decode.maybe decodeComment)
    |: ("author" := Json.Decode.maybe Json.Decode.string)
