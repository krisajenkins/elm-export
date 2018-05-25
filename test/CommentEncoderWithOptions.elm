module CommentEncoderWithOptions exposing (..)

import CommentType exposing (..)
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "commentPostId", Json.Encode.int x.postId )
        , ( "commentText", Json.Encode.string x.text )
        , ( "commentMainCategories", (Tuple.mapFirst (Json.Encode.string) >> Tuple.mapSecond (Json.Encode.string) >> (\( x, y ) -> Json.Encode.list [ x, y ]) >> Json.Encode.list) x.mainCategories )
        , ( "commentPublished", Json.Encode.bool x.published )
        , ( "commentCreated", (Json.Encode.string << toString) x.created )
        , ( "commentTags", (Dict.toList >> List.map (Tuple.mapFirst (Json.Encode.string) >> Tuple.mapSecond (Json.Encode.int) >> (\( x, y ) -> Json.Encode.list [ x, y ])) >> Json.Encode.list) x.tags )
        ]
