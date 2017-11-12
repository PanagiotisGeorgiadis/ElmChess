module DataModels.Knight exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : Position
    }


initialWhitePlayerKnightState : List Model
initialWhitePlayerKnightState =
    [ { position = { x = 8, y = B } }
    , { position = { x = 8, y = G } }
    ]


initialBlackPlayerKnightState : List Model
initialBlackPlayerKnightState =
    [ { position = { x = 1, y = B } }
    , { position = { x = 1, y = G } }
    ]
