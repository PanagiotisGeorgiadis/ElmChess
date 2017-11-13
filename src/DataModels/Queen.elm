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


initialWhitePlayerQueenState : Model
initialWhitePlayerQueenState =
    { position = { x = 8, y = D }
    , type_ = QueenPiece
    , action = MoveOwnQueen
    , color = White
    }


initialBlackPlayerQueenState : Model
initialBlackPlayerQueenState =
    { position = { x = 1, y = D }
    , type_ = QueenPiece
    , action = NoOp
    , color = Black
    }


getStateFromIndex : Int -> Model -> Maybe Model
getStateFromIndex index queen =
    if getIndexFromPosition queen.position == (index + 1) then
        Just queen
    else
        Nothing
