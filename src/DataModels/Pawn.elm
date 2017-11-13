module DataModels.Pawn exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MoveOwnPawn


initialWhitePlayerPawnState : List Model
initialWhitePlayerPawnState =
    List.foldl
        (\letter result ->
            { position = { x = 7, y = letter }
            , type_ = PawnPiece
            , action = MoveOwnPawn
            , color = White
            }
                :: result
        )
        []
        lettersList


initialBlackPlayerPawnState : List Model
initialBlackPlayerPawnState =
    List.foldl
        (\letter result ->
            { position = { x = 2, y = letter }
            , type_ = PawnPiece
            , action = NoOp
            , color = Black
            }
                :: result
        )
        []
        lettersList


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index pawns =
    List.head <|
        List.foldl
            (\pawn result ->
                if getIndexFromPosition pawn.position == (index + 1) then
                    pawn :: result
                else
                    result
            )
            []
            pawns
