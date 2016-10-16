module CommentEncoder exposing (..)

import CommentType exposing (..)
import Exts.Date exposing (..)
import Exts.Json.Encode exposing (..)
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.postId )
        , ( "text", Json.Encode.string x.text )
        , ( "mainCategories", (tuple2 Json.Encode.string Json.Encode.string) x.mainCategories )
        , ( "published", Json.Encode.bool x.published )
        , ( "created", (Json.Encode.string << toISOString) x.created )
        , ( "tags", (dict Json.Encode.string Json.Encode.int) x.tags )
        ]
