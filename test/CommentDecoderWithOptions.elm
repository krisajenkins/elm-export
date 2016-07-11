module CommentDecoderWithOptions exposing (..)

import CommentType exposing (..)
import Date
import Dict
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeComment : Decoder Comment
decodeComment =
    decode Comment
        |> required "commentPostId" int
        |> required "commentText" string
        |> required "commentMainCategories" (tuple2 (,) string string)
        |> required "commentPublished" bool
        |> required "commentCreated" (customDecoder string Date.fromString)
        |> required "commentTags" (map Dict.fromList (list (tuple2 (,) string int)))
