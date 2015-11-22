decodePost : Decoder Post
decodePost =
  succeed Post
    |: ("id" := int)
    |: ("name" := string)
    |: ("age" := maybe float)
    |: ("comments" := list decodeComment)
    |: ("promoted" := maybe decodeComment)
    |: ("author" := maybe string)
