module DataModels.Knight exposing (..)

import DataModels.Common exposing (..)


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
    [ { position = { x = 7, y = B }
      , type_ = KnightPiece
      , action = RevealKnightMovement (getIndexFromPosition { x = 7, y = B })
      , color = White
      , isThreatened = False
      }
    , { position = { x = 7, y = G }
      , type_ = KnightPiece
      , action = RevealKnightMovement (getIndexFromPosition { x = 7, y = G })
      , color = White
      , isThreatened = False
      }
    ]


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
    [ { position = { x = 0, y = B }
      , type_ = KnightPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    , { position = { x = 0, y = G }
      , type_ = KnightPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    ]
