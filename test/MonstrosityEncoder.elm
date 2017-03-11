module MonstrosityEncoder exposing (..)

import Json.Encode
import MonstrosityType exposing (..)


encodeMonstrosity : Monstrosity -> Json.Encode.Value
encodeMonstrosity x =
    case x of
        NotSpecial ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "NotSpecial" )
                , ( "contents", Json.Encode.list [] )
                ]

        OkayIGuess y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "OkayIGuess" )
                , ( "contents", encodeMonstrosity y0 )
                ]

        Ridiculous y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Ridiculous" )
                , ( "contents", Json.Encode.list [ Json.Encode.int y0, Json.Encode.string y1, (Json.Encode.list << List.map encodeMonstrosity) y2 ] )
                ]
