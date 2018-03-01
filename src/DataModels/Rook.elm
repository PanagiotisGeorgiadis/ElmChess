module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = A }
      , type_ = RookPiece
      , action = RevealRookMovement (getIndexFromPosition { x = 7, y = A })
      , color = White
      , isThreatened = False
      }
    , { position = { x = 7, y = H }
      , type_ = RookPiece
      , action = RevealRookMovement (getIndexFromPosition { x = 7, y = H })
      , color = White
      , isThreatened = False
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = A }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    , { position = { x = 0, y = H }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    ]
