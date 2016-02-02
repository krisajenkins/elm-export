encodePost : Post -> JS.Value
encodePost x =
  JS.object
    [("id", JS.int x.id)
    ,("name", JS.string x.name)
    ,("age", 
      (\y ->
        case y of
          Just val -> JS.float val
          Nothing -> JS.null) x.age)
    ,("comments", (JS.list << List.map encodeComment) x.comments)
    ,("promoted", 
      (\y ->
        case y of
          Just val -> encodeComment val
          Nothing -> JS.null) x.promoted)
    ,("author", 
      (\y ->
        case y of
          Just val -> JS.string val
          Nothing -> JS.null) x.author)]
