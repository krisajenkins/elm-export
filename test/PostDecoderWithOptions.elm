module PostDecoderWithOptions exposing (..)

import CommentDecoder exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import PostType exposing (..)


decodePost : Decoder Post
decodePost =
    Json.Decode.succeed Post
        |> required "postId" int
        |> required "postName" string
        |> required "postAge" (nullable float)
        |> required "postComments" (list decodeComment)
        |> required "postPromoted" (nullable decodeComment)
        |> required "postAuthor" (nullable string)
