module Components.ChessBoard.Update exposing (..)

import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook


type alias Model =
    { boardTiles : List (BoardTile BoardTileMsg)
    , selectedPiece : Maybe (BoardTile BoardTileMsg)
    }


type Msg
    = BoardTileMsg BoardTileMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardTileMsg submsg ->
            case submsg of
                RevealPawnMovement index ->
                    let
                        instructions =
                            Pawn.getMovementInstructions <|
                                getPositionFromIndex index

                        updatedBoardTiles =
                            List.map
                                (\bt ->
                                    let
                                        boardTile =
                                            resetBoardTileThreatenedState <|
                                                resetBoardTileType bt
                                    in
                                    List.foldl
                                        (\instruction tile ->
                                            let
                                                instructionIndex =
                                                    getIndexFromPosition instruction

                                                tileIndex =
                                                    getIndexFromPosition tile.position
                                            in
                                            if instructionIndex == tileIndex then
                                                case tile.type_ of
                                                    EmptyPiece ->
                                                        BoardTile RevealedPiece tile.position (MovePawn tile.position) tile.color False

                                                    RevealedPiece ->
                                                        tile

                                                    _ ->
                                                        { tile
                                                            | isThreatened = True
                                                        }
                                            else
                                                tile
                                        )
                                        boardTile
                                        instructions
                                )
                                model.boardTiles
                    in
                    ( { model
                        | boardTiles = updatedBoardTiles
                        , selectedPiece = getTileFromIndex index updatedBoardTiles
                      }
                    , Cmd.none
                    )

                MovePawn targetPosition ->
                    let
                        updatedBoardTiles =
                            case model.selectedPiece of
                                Just selectedPiece ->
                                    List.map
                                        (\boardTile ->
                                            let
                                                boardTileIndex =
                                                    getIndexFromPosition boardTile.position

                                                selectedPieceIndex =
                                                    getIndexFromPosition selectedPiece.position

                                                targetTileIndex =
                                                    getIndexFromPosition targetPosition
                                            in
                                            if boardTileIndex == selectedPieceIndex then
                                                BoardTile EmptyPiece boardTile.position NoOp NoColor False
                                            else if boardTileIndex == targetTileIndex then
                                                BoardTile PawnPiece boardTile.position (RevealPawnMovement boardTileIndex) boardTile.color False
                                            else
                                                resetBoardTileType boardTile
                                        )
                                        model.boardTiles

                                Nothing ->
                                    model.boardTiles
                    in
                    ( { model
                        | boardTiles = updatedBoardTiles
                        , selectedPiece = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


getTileFromIndex : Int -> List (BoardTile BoardTileMsg) -> Maybe (BoardTile BoardTileMsg)
getTileFromIndex index boardTiles =
    List.head <|
        List.reverse <|
            List.take index boardTiles


resetBoardTileType : BoardTile BoardTileMsg -> BoardTile BoardTileMsg
resetBoardTileType tile =
    if tile.type_ == RevealedPiece then
        { tile | type_ = EmptyPiece, action = NoOp }
    else
        tile


resetBoardTileThreatenedState : BoardTile BoardTileMsg -> BoardTile BoardTileMsg
resetBoardTileThreatenedState tile =
    { tile
        | isThreatened = False
    }
