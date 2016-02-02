encodePost : Post -> JS.Value
encodePost x =
  JS.object
    [("id", JS.int x.id)
    ,("name", JS.string x.name)
    ,("age", (Maybe.withDefault JS.null << Maybe.map JS.float) x.age)
    ,("comments", (JS.list << List.map encodeComment) x.comments)
    ,("promoted", (Maybe.withDefault JS.null << Maybe.map encodeComment) x.promoted)
    ,("author", (Maybe.withDefault JS.null << Maybe.map JS.string) x.author)]
