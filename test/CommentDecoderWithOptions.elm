module CommentDecoderWithOptions exposing (..)

import CommentType exposing (..)
import Dict
import Exts.Json.Decode exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    decode Comment
        |> required "commentPostId" int
        |> required "commentText" string
        |> required "commentMainCategories" (map2 (,) (index 0 string) (index 1 string))
        |> required "commentPublished" bool
        |> required "commentCreated" decodeDate
        |> required "commentTags" (dict int)
