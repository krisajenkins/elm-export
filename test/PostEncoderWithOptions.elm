module PostEncoderWithOptions exposing (..)

import CommentEncoder exposing (..)
import Json.Encode
import PostType exposing (..)


encodePost : Post -> Json.Encode.Value
encodePost x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.postId )
        , ( "postName", Json.Encode.string x.postName )
        , ( "postAge", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.postAge )
        , ( "postComments", (Json.Encode.list encodeComment) x.postComments )
        , ( "postPromoted", (Maybe.withDefault Json.Encode.null << Maybe.map encodeComment) x.postPromoted )
        , ( "postAuthor", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.postAuthor )
        ]
