module CommentEncoder exposing (..)

import CommentType exposing (..)
import Iso8601
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.postId )
        , ( "text", Json.Encode.string x.text )
        , ( "mainCategories", (\(x, y) -> list identity [ Json.Encode.string x, Json.Encode.string y ]) x.mainCategories )
        , ( "published", Json.Encode.bool x.published )
        , ( "created", (Iso8601.encode) x.created )
        , ( "tags", (Json.Encode.dict Json.Encode.string Json.Encode.int) x.tags )
        ]
