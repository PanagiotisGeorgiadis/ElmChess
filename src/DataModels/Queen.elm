module DataModels.Queen exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = D }
      , type_ = QueenPiece
      , action = RevealQueenMovement (getIndexFromPosition { x = 7, y = D })
      , color = White
      , isThreatened = False
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = D }
      , type_ = QueenPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      }
    ]
