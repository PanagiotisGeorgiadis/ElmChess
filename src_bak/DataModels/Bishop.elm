module DataModels.Bishop exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = C }
      , type_ = BishopPiece

      -- , action = RevealBishopMovement (getIndexFromPosition { x = 7, y = C })
      , action = RevealBishopMovement 59
      , color = White
      , isThreatened = False
      , index = 59
      }
    , { position = { x = 7, y = F }
      , type_ = BishopPiece

      -- , action = RevealBishopMovement (getIndexFromPosition { x = 7, y = F })
      , action = RevealBishopMovement 62
      , color = White
      , isThreatened = False
      , index = 62
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = C }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 3
      }
    , { position = { x = 0, y = F }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 6
      }
    ]
