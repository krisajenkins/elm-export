decodeComment : Decoder Comment
decodeComment =
  succeed Comment
    |: ("postId" := int)
    |: ("text" := string)
    |: ("published" := bool)
