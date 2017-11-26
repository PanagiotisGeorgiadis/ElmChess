module DataModels.Queen exposing (..)

import DataModels.Common exposing (..)


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
    [ { position = { x = 7, y = D }
      , type_ = QueenPiece
      , action = RevealQueenMovement (getIndexFromPosition { x = 7, y = D })
      , color = White
      }
    ]


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
    [ { position = { x = 0, y = D }
      , type_ = QueenPiece
      , action = NoOp
      , color = Black
      }
    ]
