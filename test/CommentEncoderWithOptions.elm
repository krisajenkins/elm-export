module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Exts.Json.Encode exposing (..)
import Iso8601
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "commentPostId", Json.Encode.int x.postId )
        , ( "commentText", Json.Encode.string x.text )
        , ( "commentMainCategories", (Exts.Json.Encode.tuple2 Json.Encode.string Json.Encode.string) x.mainCategories )
        , ( "commentPublished", Json.Encode.bool x.published )
        , ( "commentCreated", Iso8601.encode x.created )
        , ( "commentTags", (Exts.Json.Encode.dict Json.Encode.string Json.Encode.int) x.tags )
        ]
