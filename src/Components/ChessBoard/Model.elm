module Components.ChessBoard.Model exposing (..)

import Components.ChessBoard.Update exposing (..)
import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook


type alias PlayerPieces =
    { pawns : List Pawn.Model
    , knights : List Knight.Model
    , bishops : List Bishop.Model
    , rooks : List Rook.Model
    , queen : Queen.Model
    , king : King.Model
    }


getEmptyTileState : Int -> BoardTile Msg
getEmptyTileState index =
    { type_ = EmptyPiece
    , position = getPositionFromIndex index
    , action = NoOp
    , color = NoColor
    }


initialModel : Model
initialModel =
    { boardTiles = getInitialBoardState
    }


getInitialBoardState : List (BoardTile Msg)
getInitialBoardState =
    List.map
        (\index -> getTileState index ())
        (List.range 1 64)


getTileState : Int -> a -> BoardTile Msg
getTileState index _ =
    let
        whitePieces =
            getInitialWhitePlayerState

        blackPieces =
            getInitialBlackPlayerState

        tileState =
            if isPawnOnTile whitePieces index then
                case Pawn.getStateFromIndex index whitePieces.pawns of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (PawnMsg (Pawn.resolveAction index action)) color

                    Nothing ->
                        getEmptyTileState index
            else if isBishopOnTile whitePieces index then
                case Bishop.getStateFromIndex index whitePieces.bishops of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (BishopMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isKnightOnTile whitePieces index then
                case Knight.getStateFromIndex index whitePieces.knights of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KnightMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isRookOnTile whitePieces index then
                case Rook.getStateFromIndex index whitePieces.rooks of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (RookMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isQueenOnTile whitePieces index then
                case Queen.getStateFromIndex index whitePieces.queen of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (QueenMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isKingOnTile whitePieces index then
                case King.getStateFromIndex index whitePieces.king of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KingMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPawnOnTile blackPieces index then
                case Pawn.getStateFromIndex index blackPieces.pawns of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (PawnMsg (Pawn.resolveAction index action)) color

                    Nothing ->
                        getEmptyTileState index
            else if isBishopOnTile blackPieces index then
                case Bishop.getStateFromIndex index blackPieces.bishops of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (BishopMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isKnightOnTile blackPieces index then
                case Knight.getStateFromIndex index blackPieces.knights of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KnightMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isRookOnTile blackPieces index then
                case Rook.getStateFromIndex index blackPieces.rooks of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (RookMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isQueenOnTile blackPieces index then
                case Queen.getStateFromIndex index blackPieces.queen of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (QueenMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isKingOnTile blackPieces index then
                case King.getStateFromIndex index blackPieces.king of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KingMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else
                getEmptyTileState index
    in
    tileState


getInitialWhitePlayerState : PlayerPieces
getInitialWhitePlayerState =
    { pawns = Pawn.initialWhitePlayerPawnState
    , knights = Knight.initialWhitePlayerKnightState
    , bishops = Bishop.initialWhitePlayerBishopState
    , rooks = Rook.initialWhitePlayerRookState
    , queen = Queen.initialWhitePlayerQueenState
    , king = King.initialWhitePlayerKingState
    }


getInitialBlackPlayerState : PlayerPieces
getInitialBlackPlayerState =
    { pawns = Pawn.initialBlackPlayerPawnState
    , knights = Knight.initialBlackPlayerKnightState
    , bishops = Bishop.initialBlackPlayerBishopState
    , rooks = Rook.initialBlackPlayerRookState
    , queen = Queen.initialBlackPlayerQueenState
    , king = King.initialBlackPlayerKingState
    }


isPawnOnTile : PlayerPieces -> Int -> Bool
isPawnOnTile { pawns } index =
    List.any
        (\pawn ->
            if getIndexFromPosition pawn.position == index then
                True
            else
                False
        )
        pawns


isKnightOnTile : PlayerPieces -> Int -> Bool
isKnightOnTile { knights } index =
    List.any
        (\knight ->
            if getIndexFromPosition knight.position == index then
                True
            else
                False
        )
        knights


isBishopOnTile : PlayerPieces -> Int -> Bool
isBishopOnTile { bishops } index =
    List.any
        (\bishop ->
            if getIndexFromPosition bishop.position == index then
                True
            else
                False
        )
        bishops


isRookOnTile : PlayerPieces -> Int -> Bool
isRookOnTile { rooks } index =
    List.any
        (\rook ->
            if getIndexFromPosition rook.position == index then
                True
            else
                False
        )
        rooks


isQueenOnTile : PlayerPieces -> Int -> Bool
isQueenOnTile { queen } index =
    if getIndexFromPosition queen.position == index then
        True
    else
        False


isKingOnTile : PlayerPieces -> Int -> Bool
isKingOnTile { king } index =
    if getIndexFromPosition king.position == index then
        True
    else
        False
