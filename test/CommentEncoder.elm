module CommentEncoder exposing (..)

import CommentType exposing (..)
import Exts.Json.Encode exposing (..)
import Iso8601
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.postId )
        , ( "text", Json.Encode.string x.text )
        , ( "mainCategories", (Exts.Json.Encode.tuple2 Json.Encode.string Json.Encode.string) x.mainCategories )
        , ( "published", Json.Encode.bool x.published )
        , ( "created", Iso8601.encode x.created )
        , ( "tags", (Exts.Json.Encode.dict Json.Encode.string Json.Encode.int) x.tags )
        ]
