module FavoritePlacesType exposing (..)

import PositionType exposing (..)


type alias FavoritePlaces =
    { positionsByUser : Dict String (List Position)
    }
