module Components.ChessBoard.Update exposing (..)

import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook


type alias Model =
    { boardTiles : Int

    -- , whitePieces : PlayerPieces
    , whitePoints : Int

    -- , blackPieces : PlayerPieces
    , blackPoints : Int
    , boardState : List (BoardTile Msg)
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
                Pawn.MoveOwnPawn ->
                    ( model, Cmd.none )

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
