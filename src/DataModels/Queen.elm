module DataModels.Queen exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : Position
    }


initialWhitePlayerQueenState : Model
initialWhitePlayerQueenState =
    { position = { x = 8, y = D } }


initialBlackPlayerQueenState : Model
initialBlackPlayerQueenState =
    { position = { x = 1, y = D } }
