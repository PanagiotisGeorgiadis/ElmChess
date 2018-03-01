module DataModels.Queen exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = D }
      , type_ = QueenPiece

      -- , action = RevealQueenMovement (getIndexFromPosition { x = 7, y = D })
      , action = RevealQueenMovement 60
      , color = White
      , isThreatened = False
      , index = 60
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = D }
      , type_ = QueenPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 4
      }
    ]
