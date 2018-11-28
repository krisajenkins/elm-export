module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Iso8601
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "commentPostId", Json.Encode.int x.postId )
        , ( "commentText", Json.Encode.string x.text )
        , ( "commentMainCategories", (\(x, y) -> list identity [ Json.Encode.string x, Json.Encode.string y ]) x.mainCategories )
        , ( "commentPublished", Json.Encode.bool x.published )
        , ( "commentCreated", (Iso8601.encode) x.created )
        , ( "commentTags", (Json.Encode.dict Json.Encode.string Json.Encode.int) x.tags )
        ]
