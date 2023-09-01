module MonstrosityEncoder exposing (..)

import Json.Encode
import MonstrosityType exposing (..)


encodeMonstrosity : Monstrosity -> Json.Encode.Value
encodeMonstrosity x =
    case x of
        NotSpecial ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "NotSpecial" )
                , ( "contents", Json.Encode.list identity [] )
                ]

        OkayIGuess y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "OkayIGuess" )
                , ( "contents", encodeMonstrosity y0 )
                ]

        Ridiculous y0 y1 y2 y3 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Ridiculous" )
                , ( "contents", Json.Encode.list identity [ Json.Encode.int y0, Json.Encode.string y1, (Json.Encode.list encodeMonstrosity) y2, (Json.Encode.set Json.Encode.float) y3 ] )
                ]

        Dicts y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "Dicts" )
                , ( "contents", Json.Encode.list identity [ (Json.Encode.dict String.fromInt Json.Encode.null) y0, (Json.Encode.dict String.fromFloat Json.Encode.float) y1 ] )
                ]

        SortDicts y0 y1 y2 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SortDicts" )
                , ( "contents", Json.Encode.list identity [ (\dict -> dict
                        |> Sort.Dict.toList
                        |> Json.Encode.list (\( key, value ) -> Json.Encode.list identity [ encodeId key, Json.Encode.string value ])
                        ) y0, (\dict -> dict
                        |> Sort.Dict.toList
                        |> Json.Encode.list (\( key, value ) -> Json.Encode.list identity [ encodeSchool key, Json.Encode.null value ])
                        ) y1, (\dict -> dict
                        |> Sort.Dict.toList
                        |> List.map (\( k, v ) -> ( Json.Encode.encode 0 (encodeColor k), Json.Encode.null v ))
                        |> Json.Encode.object
                    ) y2 ] )
                ]

        SortSet y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SortSet" )
                , ( "contents", (Sort.Set.toList >> Json.Encode.list encodeSchool) y0 )
                ]
