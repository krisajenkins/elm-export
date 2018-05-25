module CommentEncoder exposing (..)

import CommentType exposing (..)
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.postId )
        , ( "text", Json.Encode.string x.text )
        , ( "mainCategories", (Tuple.mapFirst (Json.Encode.string) >> Tuple.mapSecond (Json.Encode.string) >> (\( x, y ) -> Json.Encode.list [ x, y ]) >> Json.Encode.list) x.mainCategories )
        , ( "published", Json.Encode.bool x.published )
        , ( "created", (Json.Encode.string << toString) x.created )
        , ( "tags", (Dict.toList >> List.map (Tuple.mapFirst (Json.Encode.string) >> Tuple.mapSecond (Json.Encode.int) >> (\( x, y ) -> Json.Encode.list [ x, y ])) >> Json.Encode.list) x.tags )
        ]
