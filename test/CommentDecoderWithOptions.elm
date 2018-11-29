module CommentDecoderWithOptions exposing (..)

import CommentType exposing (..)
import Dict
import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    succeed Comment
        |> required "commentPostId" int
        |> required "commentText" string
        |> required "commentMainCategories" (map2 Tuple.pair (index 0 string) (index 1 string))
        |> required "commentPublished" bool
        |> required "commentCreated" Iso8601.decoder
        |> required "commentTags" (dict int)
