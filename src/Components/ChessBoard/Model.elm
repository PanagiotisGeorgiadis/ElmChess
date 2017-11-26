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
    , queens : List Queen.Model
    , kings : List King.Model
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
    , selectedPiece = Nothing
    }


getInitialBoardState : List (BoardTile Msg)
getInitialBoardState =
    List.map
        getTileState
        (List.range 1 64)


getTileState : Int -> BoardTile Msg
getTileState index =
    let
        whitePieces =
            getInitialWhitePlayerState

        blackPieces =
            getInitialBlackPlayerState

        tileState =
            if isPieceOnTile whitePieces.pawns index then
                case Pawn.getStateFromIndex index whitePieces.pawns of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (PawnMsg (Pawn.resolveAction index action)) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile whitePieces.bishops index then
                case Bishop.getStateFromIndex index whitePieces.bishops of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (BishopMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile whitePieces.knights index then
                case Knight.getStateFromIndex index whitePieces.knights of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KnightMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile whitePieces.rooks index then
                case Rook.getStateFromIndex index whitePieces.rooks of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (RookMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile whitePieces.queens index then
                case Queen.getStateFromIndex index whitePieces.queens of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (QueenMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile whitePieces.kings index then
                case King.getStateFromIndex index whitePieces.kings of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KingMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile blackPieces.pawns index then
                case Pawn.getStateFromIndex index blackPieces.pawns of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (PawnMsg (Pawn.resolveAction index action)) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile blackPieces.bishops index then
                case Bishop.getStateFromIndex index blackPieces.bishops of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (BishopMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile blackPieces.knights index then
                case Knight.getStateFromIndex index blackPieces.knights of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (KnightMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile blackPieces.rooks index then
                case Rook.getStateFromIndex index blackPieces.rooks of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (RookMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile blackPieces.queens index then
                case Queen.getStateFromIndex index blackPieces.queens of
                    Just { type_, position, action, color } ->
                        BoardTile type_ position (QueenMsg action) color

                    Nothing ->
                        getEmptyTileState index
            else if isPieceOnTile blackPieces.kings index then
                case King.getStateFromIndex index blackPieces.kings of
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
    , queens = Queen.initialWhitePlayerQueenState
    , kings = King.initialWhitePlayerKingState
    }


getInitialBlackPlayerState : PlayerPieces
getInitialBlackPlayerState =
    { pawns = Pawn.initialBlackPlayerPawnState
    , knights = Knight.initialBlackPlayerKnightState
    , bishops = Bishop.initialBlackPlayerBishopState
    , rooks = Rook.initialBlackPlayerRookState
    , queens = Queen.initialBlackPlayerQueenState
    , kings = King.initialBlackPlayerKingState
    }


isPieceOnTile : List { a | position : Position } -> Int -> Bool
isPieceOnTile pieceList index =
    List.any
        (\piece ->
            getIndexFromPosition piece.position == index
        )
        pieceList
