module DataModels.Bishop exposing (..)

import DataModels.Common exposing (..)


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
    [ { position = { x = 7, y = C }
      , type_ = BishopPiece
      , action = RevealBishopMovement (getIndexFromPosition { x = 7, y = C })
      , color = White
      }
    , { position = { x = 7, y = F }
      , type_ = BishopPiece
      , action = RevealBishopMovement (getIndexFromPosition { x = 7, y = F })
      , color = White
      }
    ]


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
    [ { position = { x = 0, y = C }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      }
    , { position = { x = 0, y = F }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      }
    ]
