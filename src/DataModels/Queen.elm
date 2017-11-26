module DataModels.Queen exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MoveOwnQueen


initialWhitePlayerQueenState : List Model
initialWhitePlayerQueenState =
    [ { position = { x = 7, y = D }
      , type_ = QueenPiece
      , action = MoveOwnQueen
      , color = White
      }
    ]


initialBlackPlayerQueenState : List Model
initialBlackPlayerQueenState =
    [ { position = { x = 0, y = D }
      , type_ = QueenPiece
      , action = NoOp
      , color = Black
      }
    ]


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index queens =
    List.head <|
        List.foldl
            (\queen result ->
                if getIndexFromPosition queen.position == index then
                    queen :: result
                else
                    result
            )
            []
            queens
