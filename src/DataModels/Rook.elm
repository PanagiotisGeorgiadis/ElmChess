module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
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


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
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
