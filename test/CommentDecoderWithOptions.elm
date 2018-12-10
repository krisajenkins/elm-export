module CommentDecoderWithOptions exposing (..)

import CommentType exposing (..)
import Dict
import Exts.Json.Decode exposing (..)
import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    Json.Decode.succeed Comment
        |> required "commentPostId" int
        |> required "commentText" string
        |> required "commentMainCategories" (map2 (,) (index 0 string) (index 1 string))
        |> required "commentPublished" bool
        |> required "commentCreated" Iso8601.decoder
        |> required "commentTags" (map Dict.fromList (list (map2 (,) (index 0 string) (index 1 int))))
