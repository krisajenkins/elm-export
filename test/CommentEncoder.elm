encodeComment : Comment -> JS.Value
encodeComment x =
  JS.object
    [("postId", JS.int x.postId)
    ,("text", JS.string x.text)
    ,("published", JS.bool x.published)]
