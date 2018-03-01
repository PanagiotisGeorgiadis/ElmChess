module DataModels.King exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = E }
      , type_ = KingPiece
      , action = RevealKingMovement (getIndexFromPosition { x = 7, y = E })
      , color = White
      , isThreatened = False
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = E }
      , type_ = KingPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    ]
