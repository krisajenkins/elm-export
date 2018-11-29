module PostEncoder exposing (..)

import CommentEncoder exposing (..)
import Json.Encode
import PostType exposing (..)


encodePost : Post -> Json.Encode.Value
encodePost x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "name", Json.Encode.string x.name )
        , ( "age", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.age )
        , ( "comments", (Json.Encode.list encodeComment) x.comments )
        , ( "promoted", (Maybe.withDefault Json.Encode.null << Maybe.map encodeComment) x.promoted )
        , ( "author", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.author )
        ]
