module FieldWithFromElmEncoder exposing (..)

import Json.Encode
import FieldWithFromElmType exposing (..)


encodeFieldWithFromElm : FieldWithFromElm -> Json.Encode.Value
encodeFieldWithFromElm x =
    Json.Encode.object
        [ ( "fieldWithFromElm", existingEncodeFromElm x.fieldWithFromElm )
        ]
