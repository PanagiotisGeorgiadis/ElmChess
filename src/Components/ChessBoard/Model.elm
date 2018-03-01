module Components.ChessBoard.Model exposing (..)

import Components.ChessBoard.Update exposing (..)
import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook


getEmptyTileState : Int -> BoardTile BoardTileMsg
getEmptyTileState index =
    { type_ = EmptyPiece
    , position = getPositionFromIndex index
    , action = NoOp
    , color = NoColor
    , isThreatened = False
    }


initialModel : Model
initialModel =
    { boardTiles = getInitialBoardState
    , selectedPiece = Nothing
    }


isPieceOnTile : List (BoardTile BoardTileMsg) -> Int -> Bool
isPieceOnTile boardTiles index =
    List.any
        (\boardTile ->
            getIndexFromPosition boardTile.position == index
        )
        boardTiles


getInitialBoardState : List (BoardTile BoardTileMsg)
getInitialBoardState =
    List.map
        getInitialTileState
        (List.range 1 64)


getInitialTileState : Int -> BoardTile BoardTileMsg
getInitialTileState index =
    let
        whitePieces =
            getInitialWhitePlayerState

        blackPieces =
            getInitialBlackPlayerState

        isWhitePieceTile =
            isPieceOnTile whitePieces index

        isBlackPieceTile =
            isPieceOnTile blackPieces index

        tileState =
            if isWhitePieceTile then
                getPieceStateFromIndex whitePieces index
            else if isBlackPieceTile then
                getPieceStateFromIndex blackPieces index
            else
                getEmptyTileState index
    in
    tileState


getInitialWhitePlayerState : List (BoardTile BoardTileMsg)
getInitialWhitePlayerState =
    List.append Pawn.initialWhitePiecesState <|
        List.append Bishop.initialWhitePiecesState <|
            List.append Knight.initialWhitePiecesState <|
                List.append Rook.initialWhitePiecesState <|
                    List.append Queen.initialWhitePiecesState King.initialWhitePiecesState


getInitialBlackPlayerState : List (BoardTile BoardTileMsg)
getInitialBlackPlayerState =
    List.append Pawn.initialBlackPiecesState <|
        List.append Bishop.initialBlackPiecesState <|
            List.append Knight.initialBlackPiecesState <|
                List.append Rook.initialBlackPiecesState <|
                    List.append Queen.initialBlackPiecesState King.initialBlackPiecesState


getPieceStateFromIndex : List (BoardTile BoardTileMsg) -> Int -> BoardTile BoardTileMsg
getPieceStateFromIndex boardTiles index =
    List.foldl
        (\piece emptyPiece ->
            if getIndexFromPosition piece.position == index then
                piece
            else
                emptyPiece
        )
        (getEmptyTileState index)
        boardTiles
