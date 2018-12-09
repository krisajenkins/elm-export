module PostDecoder exposing (..)

import CommentDecoder exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import PostType exposing (..)


decodePost : Decoder Post
decodePost =
    Json.Decode.succeed Post
        |> required "id" int
        |> required "name" string
        |> required "age" (nullable float)
        |> required "comments" (list decodeComment)
        |> required "promoted" (nullable decodeComment)
        |> required "author" (nullable string)
