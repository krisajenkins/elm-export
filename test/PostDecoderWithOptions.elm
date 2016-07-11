module PostDecoderWithOptions exposing (..)

import CommentDecoder exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import PostType exposing (..)


decodePost : Decoder Post
decodePost =
    decode Post
        |> required "postId" int
        |> required "postName" string
        |> required "postAge" (maybe float)
        |> required "postComments" (list decodeComment)
        |> required "postPromoted" (maybe decodeComment)
        |> required "postAuthor" (maybe string)
