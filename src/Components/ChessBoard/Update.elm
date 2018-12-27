module Components.ChessBoard.Update exposing (..)

import DataModels.Common
    exposing
        ( BoardTile(..)
        , BoardTileAttributes
        , CapturableTileState
        , CastlingDirection(..)
        , CastlingPrerequisites
        , CastlingTileState
        , ChessBoard
        , ChessPieceType(..)
        , Color
        , EmptyPathTileState
        , clearRevealedPaths
        , getOccupiedTileByIndex
        , initialChessBoard
        , movePiece
        , revealBishopPath
        , revealKingPath
        , revealKnightPath
        , revealPawnPath
        , revealQueenPath
        , revealRookPath
        )
import Utils.Actions exposing (simpleAction)
import Utils.Setters exposing (updateKingMoved, updateKingRookMoved, updateQueenRookMoved)


type alias Props p =
    { p
        | playerColor : Color
    }


type alias Model =
    { chessBoard : ChessBoard Msg
    , castlingPrerequisites : CastlingPrerequisites
    }


initialModel : Props p -> Model
initialModel props =
    { chessBoard =
        initialChessBoard
            { noAction = \_ -> NoOp
            , action = RevealAvailablePaths
            , playerColor = props.playerColor
            }
    , castlingPrerequisites = CastlingPrerequisites False False False
    }


type Msg
    = NoOp
    | ClearRevealedPaths
    | RevealAvailablePaths BoardTileAttributes
    | MovePiece (EmptyPathTileState Msg)
    | CapturePiece (CapturableTileState Msg)
    | RevealPawnPath BoardTileAttributes
    | RevealKnightPath BoardTileAttributes
    | RevealBishopPath BoardTileAttributes
    | RevealRookPath BoardTileAttributes
    | RevealQueenPath BoardTileAttributes
    | RevealKingPath BoardTileAttributes
    | PerformCastlingMove CastlingTileState


type ExtMsg
    = None
    | PieceCaptured (BoardTile Msg)


update : Props p -> Model -> Msg -> ( Model, Cmd Msg, ExtMsg )
update props model msg =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , None
            )

        ClearRevealedPaths ->
            ( { model
                | chessBoard = clearRevealedPaths model.chessBoard
              }
            , Cmd.none
            , None
            )

        RevealAvailablePaths ({ color, type_, index } as tileAttributes) ->
            ( model
            , case type_ of
                Pawn ->
                    simpleAction (RevealPawnPath tileAttributes)

                Knight ->
                    simpleAction (RevealKnightPath tileAttributes)

                Bishop ->
                    simpleAction (RevealBishopPath tileAttributes)

                Rook ->
                    simpleAction (RevealRookPath tileAttributes)

                Queen ->
                    simpleAction (RevealQueenPath tileAttributes)

                King ->
                    simpleAction (RevealKingPath tileAttributes)
            , None
            )

        RevealPawnPath ({ isToggled } as tileAttributes) ->
            let
                updatedChessBoard =
                    if isToggled then
                        clearRevealedPaths model.chessBoard

                    else
                        revealPawnPath tileAttributes <|
                            clearRevealedPaths model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        RevealKnightPath ({ isToggled } as tileAttributes) ->
            let
                updatedChessBoard =
                    if isToggled then
                        clearRevealedPaths model.chessBoard

                    else
                        revealKnightPath tileAttributes <|
                            clearRevealedPaths model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        RevealBishopPath ({ isToggled } as tileAttributes) ->
            let
                updatedChessBoard =
                    if isToggled then
                        clearRevealedPaths model.chessBoard

                    else
                        revealBishopPath tileAttributes <|
                            clearRevealedPaths model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        RevealRookPath ({ isToggled } as tileAttributes) ->
            let
                updatedChessBoard =
                    if isToggled then
                        clearRevealedPaths model.chessBoard

                    else
                        revealRookPath tileAttributes <|
                            clearRevealedPaths model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        RevealQueenPath ({ isToggled } as tileAttributes) ->
            let
                updatedChessBoard =
                    if isToggled then
                        clearRevealedPaths model.chessBoard

                    else
                        revealQueenPath tileAttributes <|
                            clearRevealedPaths model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        RevealKingPath ({ isToggled } as tileAttributes) ->
            let
                updatedChessBoard =
                    if isToggled then
                        clearRevealedPaths model.chessBoard

                    else
                        revealKingPath props.playerColor tileAttributes model.castlingPrerequisites <|
                            clearRevealedPaths model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        MovePiece payload ->
            let
                updatedModel =
                    case payload.sourceTile of
                        Just (OccupiedTile { attributes }) ->
                            case ( attributes.type_, attributes.index ) of
                                ( Rook, 56 ) ->
                                    -- { model | queenRookMoved = True }
                                    { model
                                        | castlingPrerequisites =
                                            updateQueenRookMoved True model.castlingPrerequisites
                                    }

                                ( Rook, 63 ) ->
                                    -- { model | kingRookMoved = True }
                                    { model
                                        | castlingPrerequisites =
                                            updateKingRookMoved True model.castlingPrerequisites
                                    }

                                ( King, _ ) ->
                                    -- { model | kingMoved = True }
                                    { model
                                        | castlingPrerequisites =
                                            updateKingMoved True model.castlingPrerequisites
                                    }

                                _ ->
                                    model

                        _ ->
                            model

                -- _ = Debug.log "queenRookMoved" model.queenRookMoved
                -- _ = Debug.log "kingRookMoved" model.kingRookMoved
                -- _ = Debug.log "kingMoved" model.kingMoved
                updatedChessBoard =
                    movePiece payload updatedModel.chessBoard
            in
            ( { updatedModel
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , None
            )

        CapturePiece payload ->
            let
                -- _ =
                --     Debug.log "payload" payload
                updatedChessBoard =
                    movePiece payload model.chessBoard
            in
            ( { model
                | chessBoard = updatedChessBoard
              }
            , Cmd.none
            , PieceCaptured payload.previousState
            )

        PerformCastlingMove { castlingDiretion, index } ->
            let
                getByIndex =
                    getOccupiedTileByIndex model.chessBoard

                ( rookMovePath, kingMovePath ) =
                    case castlingDiretion of
                        KingSideCastling ->
                            ( EmptyPathTileState 61 (getByIndex 63)
                            , EmptyPathTileState 62 (getByIndex 60)
                            )

                        QueenSideCastling ->
                            ( EmptyPathTileState 59 (getByIndex 56)
                            , EmptyPathTileState 58 (getByIndex 60)
                            )
            in
            ( model
            , Cmd.batch
                [ simpleAction (MovePiece kingMovePath)
                , simpleAction (MovePiece rookMovePath)
                ]
            , None
            )
