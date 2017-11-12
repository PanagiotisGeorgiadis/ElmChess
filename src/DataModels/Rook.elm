module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : Position
    }


initialWhitePlayerRookState : List Model
initialWhitePlayerRookState =
    [ { position = { x = 8, y = A } }
    , { position = { x = 8, y = H } }
    ]


initialBlackPlayerRookState : List Model
initialBlackPlayerRookState =
    [ { position = { x = 1, y = A } }
    , { position = { x = 1, y = H } }
    ]
