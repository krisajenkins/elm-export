module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Iso8601
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "commentPostId", Json.Encode.int x.commentPostId )
        , ( "commentText", Json.Encode.string x.commentText )
        , ( "commentMainCategories", (\(m0, n0) -> Json.Encode.list identity [ Json.Encode.string m0, Json.Encode.string n0 ]) x.commentMainCategories )
        , ( "commentPublished", Json.Encode.bool x.commentPublished )
        , ( "commentCreated", (Iso8601.encode) x.commentCreated )
        , ( "commentTags", (Json.Encode.dict Json.Encode.string Json.Encode.int) x.commentTags )
        ]
