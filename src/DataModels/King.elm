module DataModels.King exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MoveOwnKing


initialWhitePlayerKingState : List Model
initialWhitePlayerKingState =
    [ { position = { x = 7, y = E }
      , type_ = KingPiece
      , action = MoveOwnKing
      , color = White
      }
    ]


initialBlackPlayerKingState : List Model
initialBlackPlayerKingState =
    [ { position = { x = 0, y = E }
      , type_ = KingPiece
      , action = NoOp
      , color = Black
      }
    ]


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index kings =
    List.head <|
        List.foldl
            (\king result ->
                if getIndexFromPosition king.position == index then
                    king :: result
                else
                    result
            )
            []
            kings
