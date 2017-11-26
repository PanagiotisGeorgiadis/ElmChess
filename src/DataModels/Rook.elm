module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
    [ { position = { x = 7, y = A }
      , type_ = RookPiece
      , action = RevealRookMovement (getIndexFromPosition { x = 7, y = A })
      , color = White
      }
    , { position = { x = 7, y = H }
      , type_ = RookPiece
      , action = RevealRookMovement (getIndexFromPosition { x = 7, y = H })
      , color = White
      }
    ]


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
    [ { position = { x = 0, y = A }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      }
    , { position = { x = 0, y = H }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      }
    ]
