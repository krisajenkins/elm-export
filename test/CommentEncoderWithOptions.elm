module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "commentPostId", Json.Encode.int x.postId )
        , ( "commentText", Json.Encode.string x.text )
        , ( "commentMainCategories", (tuple2 Json.Encode.string Json.Encode.string) x.mainCategories )
        , ( "commentPublished", Json.Encode.bool x.published )
        , ( "commentCreated", (Json.Encode.string << toString) x.created )
        , ( "commentTags", (dict Json.Encode.string Json.Encode.int) x.tags )
        ]
