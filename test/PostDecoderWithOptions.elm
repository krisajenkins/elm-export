module Main (..) where


decodePost : Json.Decode.Decoder Post
decodePost =
  Json.Decode.succeed Post
    |: ("postId" := Json.Decode.int)
    |: ("postName" := Json.Decode.string)
    |: ("postAge" := Json.Decode.maybe Json.Decode.float)
    |: ("postComments" := Json.Decode.list decodeComment)
    |: ("postPromoted" := Json.Decode.maybe decodeComment)
    |: ("postAuthor" := Json.Decode.maybe Json.Decode.string)
    |: ("postNothing" := Json.Decode.customDecoder (Json.Decode.list Json.Decode.value)
          (\jsonList ->
             case jsonList of
               [] -> Result.Ok ()
               _  -> Result.Err "expecting a zero-length array"))
