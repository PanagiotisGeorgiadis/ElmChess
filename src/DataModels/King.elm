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


initialWhitePlayerKingState : Model
initialWhitePlayerKingState =
    { position = { x = 7, y = E }
    , type_ = KingPiece
    , action = MoveOwnKing
    , color = White
    }


initialBlackPlayerKingState : Model
initialBlackPlayerKingState =
    { position = { x = 0, y = E }
    , type_ = KingPiece
    , action = NoOp
    , color = Black
    }


getStateFromIndex : Int -> Model -> Maybe Model
getStateFromIndex index king =
    if getIndexFromPosition king.position == index then
        Just king
    else
        Nothing
