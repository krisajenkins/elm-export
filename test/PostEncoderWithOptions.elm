module PostEncoderWithOptions exposing (..)

import CommentEncoder exposing (..)
import Json.Encode exposing (..)
import PostType exposing (..)


encodePost : Post -> Value
encodePost x =
    object
        [ ( "postId", int x.id )
        , ( "postName", string x.name )
        , ( "postAge", (Maybe.withDefault null << Maybe.map float) x.age )
        , ( "postComments", (list << List.map encodeComment) x.comments )
        , ( "postPromoted", (Maybe.withDefault null << Maybe.map encodeComment) x.promoted )
        , ( "postAuthor", (Maybe.withDefault null << Maybe.map string) x.author )
        ]
