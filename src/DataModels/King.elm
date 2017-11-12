module DataModels.King exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : Position
    }


initialWhitePlayerKingState : Model
initialWhitePlayerKingState =
    { position = { x = 8, y = E } }


initialBlackPlayerKingState : Model
initialBlackPlayerKingState =
    { position = { x = 1, y = E } }
