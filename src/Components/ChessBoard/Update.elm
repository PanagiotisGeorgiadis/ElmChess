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
                            Pawn.movementInstructions <| getPositionFromIndex index

                        updatedBoardTiles =
                            List.foldl
                                (\instruction result ->
                                    revealBoardTile result instruction (PawnMsg (Pawn.MovePawn instruction))
                                )
                                model.boardTiles
                                instructions
                    in
                    ( model
                    , Cmd.none
                    )

                Pawn.MovePawn position ->
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
                    { item
                        | type_ = KingPiece
                    }

                updatedBoardTiles =
                    List.append listBody <| updatedItem :: listTail
            in
            updatedBoardTiles

        Nothing ->
            boardTiles
