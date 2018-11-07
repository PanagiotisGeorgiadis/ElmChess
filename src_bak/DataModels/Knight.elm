module DataModels.Knight exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = B }
      , type_ = KnightPiece

      -- , action = RevealKnightMovement (getIndexFromPosition { x = 7, y = B })
      , action = RevealKnightMovement 63
      , color = White
      , isThreatened = False
      , index = 63
      }
    , { position = { x = 7, y = G }
      , type_ = KnightPiece

      -- , action = RevealKnightMovement (getIndexFromPosition { x = 7, y = G })
      , action = RevealKnightMovement 58
      , color = White
      , isThreatened = False
      , index = 58
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = B }
      , type_ = KnightPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 2
      }
    , { position = { x = 0, y = G }
      , type_ = KnightPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 7
      }
    ]
