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
        getTileState
        (List.range 1 64)


getTileState : Int -> BoardTile BoardTileMsg
getTileState index =
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
    List.append Pawn.initialWhitePlayerState <|
        List.append Bishop.initialWhitePlayerState <|
            List.append Knight.initialWhitePlayerState <|
                List.append Rook.initialWhitePlayerState <|
                    List.append Queen.initialWhitePlayerState King.initialWhitePlayerState


getInitialBlackPlayerState : List (BoardTile BoardTileMsg)
getInitialBlackPlayerState =
    List.append Pawn.initialBlackPlayerState <|
        List.append Bishop.initialBlackPlayerState <|
            List.append Knight.initialBlackPlayerState <|
                List.append Rook.initialBlackPlayerState <|
                    List.append Queen.initialBlackPlayerState King.initialBlackPlayerState


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
