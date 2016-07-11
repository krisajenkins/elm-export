module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Exts.Date exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode exposing (..)


encodeComment : Comment -> Value
encodeComment x =
    object
        [ ( "commentPostId", int x.postId )
        , ( "commentText", string x.text )
        , ( "commentMainCategories", tuple2 string string x.mainCategories )
        , ( "commentPublished", bool x.published )
        , ( "commentCreated", (string << toISOString) x.created )
        , ( "commentTags", dict string int x.tags )
        ]
