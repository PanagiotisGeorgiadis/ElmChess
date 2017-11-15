module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MoveOwnRook


initialWhitePlayerRookState : List Model
initialWhitePlayerRookState =
    [ { position = { x = 7, y = A }
      , type_ = RookPiece
      , action = MoveOwnRook
      , color = White
      }
    , { position = { x = 7, y = H }
      , type_ = RookPiece
      , action = MoveOwnRook
      , color = White
      }
    ]


initialBlackPlayerRookState : List Model
initialBlackPlayerRookState =
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


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index rooks =
    List.head <|
        List.foldl
            (\rook result ->
                if getIndexFromPosition rook.position == index then
                    rook :: result
                else
                    result
            )
            []
            rooks
