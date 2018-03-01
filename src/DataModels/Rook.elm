module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = A }
      , type_ = RookPiece

      -- , action = RevealRookMovement (getIndexFromPosition { x = 7, y = A })
      , action = RevealRookMovement 57
      , color = White
      , isThreatened = False
      , index = 57
      }
    , { position = { x = 7, y = H }
      , type_ = RookPiece
      , action = RevealRookMovement 64
      , color = White
      , isThreatened = False
      , index = 64
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = A }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 1
      }
    , { position = { x = 0, y = H }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 8
      }
    ]
