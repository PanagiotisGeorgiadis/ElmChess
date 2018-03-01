module Components.ChessBoard.Update exposing (..)

-- import DataModels.Bishop as Bishop
-- import DataModels.King as King
-- import DataModels.Knight as Knight

import DataModels.Common exposing (..)
import DataModels.Pawn as Pawn


-- import DataModels.Queen as Queen
-- import DataModels.Rook as Rook


type alias Model =
    { boardTiles : List (BoardTile BoardTileMsg)
    , selectedPiece : Maybe (BoardTile BoardTileMsg)
    }


type Msg
    = BoardTileMsg BoardTileMsg


update : Msg -> Model -> PlayerType -> ( Model, Cmd Msg )
update msg model playerType =
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
                                            resetBoardTileAction <|
                                                resetBoardTileThreatenedState <|
                                                    resetBoardTileType bt
                                    in
                                    List.foldl
                                        (\instruction tile ->
                                            let
                                                instructionIndex =
                                                    getIndexFromPosition instruction.position

                                                tileIndex =
                                                    -- getIndexFromPosition tile.position
                                                    tile.index
                                            in
                                            if instructionIndex == tileIndex then
                                                case tile.type_ of
                                                    EmptyPiece ->
                                                        if instruction.isCapturableMove then
                                                            tile
                                                        else
                                                            BoardTile RevealedPiece tile.position (MovePawn tile.position) tile.color False tile.index

                                                    RevealedPiece ->
                                                        tile

                                                    _ ->
                                                        let
                                                            ( isThreatened, action ) =
                                                                case ( playerType, tile.color, instruction.isCapturableMove ) of
                                                                    ( WhitePlayer, Black, True ) ->
                                                                        ( True, MovePawn tile.position )

                                                                    ( BlackPlayer, White, True ) ->
                                                                        ( True, MovePawn tile.position )

                                                                    _ ->
                                                                        ( False, tile.action )
                                                        in
                                                        { tile
                                                            | isThreatened = isThreatened
                                                            , action = action
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
                                                    -- getIndexFromPosition boardTile.position
                                                    boardTile.index

                                                _ =
                                                    Debug.log "boardTileIndex" boardTileIndex

                                                selectedPieceIndex =
                                                    -- getIndexFromPosition selectedPiece.position
                                                    selectedPiece.index

                                                targetTileIndex =
                                                    getIndexFromPosition targetPosition
                                            in
                                            if boardTileIndex == selectedPieceIndex then
                                                BoardTile EmptyPiece boardTile.position NoOp NoColor False boardTileIndex
                                            else if boardTileIndex == targetTileIndex then
                                                BoardTile PawnPiece boardTile.position (RevealPawnMovement boardTileIndex) selectedPiece.color False boardTileIndex
                                            else
                                                resetBoardTileThreatenedState <|
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
    { tile | isThreatened = False }


resetBoardTileAction : BoardTile BoardTileMsg -> BoardTile BoardTileMsg
resetBoardTileAction tile =
    case tile.action of
        MovePawn _ ->
            { tile | action = NoOp }

        _ ->
            tile
