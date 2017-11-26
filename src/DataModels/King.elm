module DataModels.King exposing (..)

import DataModels.Common exposing (..)


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
    [ { position = { x = 7, y = E }
      , type_ = KingPiece
      , action = RevealKingMovement (getIndexFromPosition { x = 7, y = E })
      , color = White
      , isThreatened = False
      }
    ]


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
    [ { position = { x = 0, y = E }
      , type_ = KingPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    ]
