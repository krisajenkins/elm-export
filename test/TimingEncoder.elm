module TimingEncoder exposing (..)

import Json.Encode
import TimingType exposing (..)


encodeTiming : Timing -> Json.Encode.Value
encodeTiming x =
    case x of
        Start ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Start" )
                , ( "contents", Json.Encode.list [] )
                ]

        Continue y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Continue" )
                , ( "contents", Json.Encode.float y0 )
                ]

        Stop ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Stop" )
                , ( "contents", Json.Encode.list [] )
                ]
