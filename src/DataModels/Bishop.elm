module DataModels.Bishop exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = C }
      , type_ = BishopPiece
      , action = RevealBishopMovement (getIndexFromPosition { x = 7, y = C })
      , color = White
      , isThreatened = False
      }
    , { position = { x = 7, y = F }
      , type_ = BishopPiece
      , action = RevealBishopMovement (getIndexFromPosition { x = 7, y = F })
      , color = White
      , isThreatened = False
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = C }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    , { position = { x = 0, y = F }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    ]
