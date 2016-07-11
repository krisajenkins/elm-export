module Main (..) where


encodePost : Post -> Json.Encode.Value
encodePost x =
  Json.Encode.object
    [ ( "id", Json.Encode.int x.id )
    , ( "name", Json.Encode.string x.name )
    , ( "age", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.float) x.age )
    , ( "comments", (Json.Encode.list << List.map encodeComment) x.comments )
    , ( "promoted", (Maybe.withDefault Json.Encode.null << Maybe.map encodeComment) x.promoted )
    , ( "author", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.author )
    , ( "nothing", (\_ -> Json.Encode.list []) x.nothing )
    ]
