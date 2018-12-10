module PostEncoderWithOptions exposing (..)

import CommentEncoder exposing (..)
import Json.Encode
import PostType exposing (..)


encodePost : Post -> Json.Encode.Value
encodePost x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.id )
        , ( "postName", Json.Encode.string x.name )
        , ( "postAge", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.age )
        , ( "postComments", (Json.Encode.list encodeComment) x.comments )
        , ( "postPromoted", (Maybe.withDefault Json.Encode.null << Maybe.map encodeComment) x.promoted )
        , ( "postAuthor", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.author )
        ]
