module Components.ChessBoard.Update exposing (..)

import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook


type alias Model =
    { boardTiles : List (BoardTile Msg)
    , selectedPiece : Maybe (BoardTile Msg)
    }


type Msg
    = NoOp
    | PawnMsg Pawn.Msg
    | KnightMsg Knight.Msg
    | BishopMsg Bishop.Msg
    | RookMsg Rook.Msg
    | QueenMsg Queen.Msg
    | KingMsg King.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PawnMsg submsg ->
            case submsg of
                Pawn.RevealPawnMovement index ->
                    let
                        instructions =
                            Pawn.movementInstructions <|
                                getPositionFromIndex index

                        updatedBoardTiles =
                            List.map
                                (\bt ->
                                    let
                                        boardTile =
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
                                                BoardTile RevealedPiece tile.position (PawnMsg (Pawn.MovePawn tile.position)) tile.color
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

                Pawn.MovePawn position ->
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
                                                    getIndexFromPosition position
                                            in
                                            if boardTileIndex == selectedPieceIndex then
                                                BoardTile EmptyPiece boardTile.position NoOp NoColor
                                            else if boardTileIndex == targetTileIndex then
                                                BoardTile PawnPiece boardTile.position (PawnMsg (Pawn.RevealPawnMovement boardTileIndex)) boardTile.color
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

                Pawn.NoOp ->
                    ( model, Cmd.none )

        KnightMsg submsg ->
            ( model, Cmd.none )

        BishopMsg submsg ->
            ( model, Cmd.none )

        RookMsg submsg ->
            ( model, Cmd.none )

        QueenMsg submsg ->
            ( model, Cmd.none )

        KingMsg submsg ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getTileFromIndex : Int -> List (BoardTile Msg) -> Maybe (BoardTile Msg)
getTileFromIndex index boardTiles =
    List.head <|
        List.reverse <|
            List.take index boardTiles


updateTileOnPosition : Int -> List (BoardTile Msg) -> List (BoardTile Msg)
updateTileOnPosition index boardTiles =
    let
        listBody =
            List.take (index - 1) boardTiles

        listTail =
            List.drop index boardTiles

        selectedItem =
            List.head <| List.drop (index - 1) boardTiles
    in
    case selectedItem of
        Just item ->
            let
                updatedItem =
                    { item | type_ = KingPiece }

                updatedBoardTiles =
                    List.append listBody <| updatedItem :: listTail
            in
            updatedBoardTiles

        Nothing ->
            boardTiles


resetBoardTileType : BoardTile Msg -> BoardTile Msg
resetBoardTileType tile =
    if tile.type_ == RevealedPiece then
        { tile | type_ = EmptyPiece }
    else
        tile
