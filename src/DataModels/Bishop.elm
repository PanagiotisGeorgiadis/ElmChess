module DataModels.Bishop exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : Position
    }


initialWhitePlayerBishopState : List Model
initialWhitePlayerBishopState =
    [ { position = { x = 8, y = C } }
    , { position = { x = 8, y = F } }
    ]


initialBlackPlayerBishopState : List Model
initialBlackPlayerBishopState =
    [ { position = { x = 1, y = C } }
    , { position = { x = 1, y = F } }
    ]
