module DataModels.King exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = E }
      , type_ = KingPiece

      -- , action = RevealKingMovement (getIndexFromPosition { x = 7, y = E })
      , action = RevealKingMovement 61
      , color = White
      , isThreatened = False
      , index = 61
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = E }
      , type_ = KingPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 5
      }
    ]
