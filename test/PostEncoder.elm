module PostEncoder exposing (..)

import CommentEncoder exposing (..)
import Json.Encode exposing (..)
import PostType exposing (..)


encodePost : Post -> Value
encodePost x =
    object
        [ ( "id", int x.id )
        , ( "name", string x.name )
        , ( "age", (Maybe.withDefault null << Maybe.map float) x.age )
        , ( "comments", (list << List.map encodeComment) x.comments )
        , ( "promoted", (Maybe.withDefault null << Maybe.map encodeComment) x.promoted )
        , ( "author", (Maybe.withDefault null << Maybe.map string) x.author )
        ]
