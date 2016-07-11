module Main (..) where


decodePost : Json.Decode.Decoder Post
decodePost =
  Json.Decode.succeed Post
    |: ("id" := Json.Decode.int)
    |: ("name" := Json.Decode.string)
    |: ("age" := Json.Decode.maybe Json.Decode.float)
    |: ("comments" := Json.Decode.list decodeComment)
    |: ("promoted" := Json.Decode.maybe decodeComment)
    |: ("author" := Json.Decode.maybe Json.Decode.string)
    |: ("nothing" := Json.Decode.customDecoder (Json.Decode.list Json.Decode.value)
          (\jsonList ->
             case jsonList of
               [] -> Result.Ok ()
               _  -> Result.Err "expecting a zero-length array"))
