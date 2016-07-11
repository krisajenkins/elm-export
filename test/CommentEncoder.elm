module CommentEncoder exposing (..)

import CommentType exposing (..)
import Exts.Date exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode exposing (..)


encodeComment : Comment -> Value
encodeComment x =
    object
        [ ( "postId", int x.postId )
        , ( "text", string x.text )
        , ( "mainCategories", tuple2 string string x.mainCategories )
        , ( "published", bool x.published )
        , ( "created", (string << toISOString) x.created )
        , ( "tags", dict string int x.tags )
        ]
