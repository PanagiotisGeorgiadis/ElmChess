module Components.ChessBoard.Update exposing (..)

import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook
import Dict


type alias Model =
    { boardTiles : List (BoardTile BoardTileMsg)
    , selectedPiece : Maybe (BoardTile BoardTileMsg)
    }


initialModel : Model
initialModel =
    { boardTiles = getInitialBoardState
    , selectedPiece = Nothing
    }


getEmptyTileState : Int -> BoardTile BoardTileMsg
getEmptyTileState index =
    { type_ = EmptyPiece
    , position = getPositionFromIndex index
    , action = NoOp
    , color = NoColor
    , isThreatened = False
    , index = index
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

                RevealRookMovement index ->
                    let
                        instructions =
                            Rook.getMovementInstructions <|
                                getPositionFromIndex index

                        instructionsX =
                            Rook.getXAxisMovementInstructions <|
                                getPositionFromIndex index

                        instructionsY =
                            Rook.getYAxisMovementInstructions <|
                                getPositionFromIndex index

                        _ =
                            Debug.log "instructionsX" instructionsX

                        _ =
                            Debug.log "instructionsY" instructionsY

                        boardTilesDict =
                            Dict.fromList <|
                                List.indexedMap (,) model.boardTiles

                        updateBoardTile instruction tile =
                            ( Just <| BoardTile RevealedPiece tile.position (MovePawn tile.position) tile.color False tile.index
                            , False
                            )

                        -- case tile.type_ of
                        -- EmptyPiece ->
                        --     ( BoardTile RevealedPiece tile.position (MovePawn tile.position) tile.color False tile.index
                        --     , False
                        --     )
                        --
                        -- RevealedPiece ->
                        --     ( tile
                        --     , False
                        --     )
                        --
                        -- _ ->
                        --     let
                        --         ( isThreatened, action ) =
                        --             case ( playerType, tile.color, instruction.isCapturableMove ) of
                        --                 ( WhitePlayer, Black, True ) ->
                        --                     ( True, MoveRook tile.position )
                        --
                        --                 ( BlackPlayer, White, True ) ->
                        --                     ( True, MoveRook tile.position )
                        --
                        --                 _ ->
                        --                     ( False, tile.action )
                        --     in
                        --     ( { tile
                        --         | isThreatened = isThreatened
                        --         , action = action
                        --       }
                        --     , True
                        --     )
                        updatedBoardTiles =
                            List.map Tuple.second <|
                                Dict.toList <|
                                    Tuple.first <|
                                        List.foldl
                                            (\instruction ( result, isBlocked ) ->
                                                let
                                                    instructionIndex =
                                                        getIndexFromPosition instruction.position - 1

                                                    targetBoardTile =
                                                        Dict.get instructionIndex result

                                                    _ =
                                                        Debug.log "isBlocked" isBlocked

                                                    ( updatedBoardTile, updatedIsBlocked ) =
                                                        case targetBoardTile of
                                                            Just target ->
                                                                updateBoardTile instruction target

                                                            Nothing ->
                                                                ( targetBoardTile, isBlocked )
                                                in
                                                if updatedIsBlocked then
                                                    ( result
                                                    , isBlocked
                                                    )
                                                else
                                                    ( Dict.update instructionIndex (\_ -> updatedBoardTile) result
                                                    , updatedIsBlocked
                                                    )
                                            )
                                            ( boardTilesDict, False )
                                            instructionsY

                        -- _ = Debug.log "instructions" instructions
                        -- updatedBoardTiles =
                        --     List.map
                        --         (\bt ->
                        --             let
                        --                 boardTile =
                        --                     resetBoardTileAction <|
                        --                         resetBoardTileThreatenedState <|
                        --                             resetBoardTileType bt
                        --                 _ =
                        --                     Debug.log "testBoardTiles" testBoardTiles
                        --
                        --                 _ =
                        --                     Debug.log "getIndexFromPosition " <|
                        --                         List.map
                        --                             (\i ->
                        --                                 getIndexFromPosition i.position
                        --                             )
                        --                             instructions
                        --             in
                        --             Tuple.first <|
                        --                 List.foldl
                        --                     (\instruction ( tile, isBlocked ) ->
                        --                         let
                        --                             instructionIndex =
                        --                                 getIndexFromPosition instruction.position
                        --
                        --                             tileIndex =
                        --                                 -- getIndexFromPosition tile.position
                        --                                 tile.index
                        --                         in
                        --                         if instructionIndex == tileIndex && not isBlocked then
                        --                             case tile.type_ of
                        --                                 EmptyPiece ->
                        --                                     -- if instruction.isCapturableMove then
                        --                                     --     tile
                        --                                     -- else
                        --                                     -- BoardTile RevealedPiece tile.position (MovePawn tile.position) tile.color False tile.index
                        --                                     ( BoardTile RevealedPiece tile.position (MoveRook tile.position) tile.color False tile.index
                        --                                     , isBlocked
                        --                                     )
                        --
                        --                                 RevealedPiece ->
                        --                                     ( tile
                        --                                     , isBlocked
                        --                                     )
                        --
                        --                                 _ ->
                        --                                     let
                        --                                         ( isThreatened, action ) =
                        --                                             case ( playerType, tile.color, instruction.isCapturableMove ) of
                        --                                                 ( WhitePlayer, Black, True ) ->
                        --                                                     ( True, MoveRook tile.position )
                        --
                        --                                                 ( BlackPlayer, White, True ) ->
                        --                                                     ( True, MoveRook tile.position )
                        --
                        --                                                 _ ->
                        --                                                     ( False, tile.action )
                        --                                     in
                        --                                     ( { tile
                        --                                         | isThreatened = isThreatened
                        --                                         , action = action
                        --                                       }
                        --                                     , isPieceOnTile model.boardTiles tileIndex
                        --                                     )
                        --                         else
                        --                             ( tile
                        --                             , isBlocked
                        --                             )
                        --                     )
                        --                     ( boardTile, False )
                        --                     instructions
                        --         )
                        --         model.boardTiles
                    in
                    ( { model
                        | boardTiles = updatedBoardTiles
                        , selectedPiece = getTileFromIndex index updatedBoardTiles
                      }
                    , Cmd.none
                    )

                MoveRook targetPosition ->
                    let
                        updatedBoardTiles =
                            case model.selectedPiece of
                                Just selectedPiece ->
                                    List.map
                                        (\boardTile ->
                                            let
                                                boardTileIndex =
                                                    boardTile.index

                                                selectedPieceIndex =
                                                    selectedPiece.index

                                                targetTileIndex =
                                                    getIndexFromPosition targetPosition
                                            in
                                            if boardTileIndex == selectedPieceIndex then
                                                BoardTile EmptyPiece boardTile.position NoOp NoColor False boardTileIndex
                                            else if boardTileIndex == targetTileIndex then
                                                BoardTile RookPiece boardTile.position (RevealRookMovement boardTileIndex) selectedPiece.color False boardTileIndex
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


isPieceOnTile : List (BoardTile BoardTileMsg) -> Int -> Bool
isPieceOnTile boardTiles index =
    List.any ((==) index << .index) boardTiles


getInitialBoardState : List (BoardTile BoardTileMsg)
getInitialBoardState =
    List.map getInitialTileState (List.range 1 64)


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
            -- if getIndexFromPosition piece.position == index then
            if piece.index == index then
                piece
            else
                emptyPiece
        )
        (getEmptyTileState index)
        boardTiles
