module Components.ChessBoard.View exposing (..)

import Components.ChessBoard.Update exposing (..)
import DataModels.Common exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)


letterItemHtml : BoardLetter -> Html Msg
letterItemHtml letterText =
    div [] [ text <| getStringFromBoardLetter letterText ]


lettersContainerView : Html Msg
lettersContainerView =
    div [ class "letters-container" ]
        (List.map letterItemHtml lettersList)


numberItemHtml : Int -> Html Msg
numberItemHtml numberText =
    div [] [ text <| toString numberText ]


numbersContainerView : Html Msg
numbersContainerView =
    div [ class "numbers-container" ]
        (List.map numberItemHtml <| List.reverse numbersList)


view : Model -> Html Msg
view model =
    div [ class "chess-board" ]
        [ lettersContainerView
        , numbersContainerView
        , div [ class "chess-board-inner" ]
            (boardContentsView model)
        ]


boardContentsView : Model -> List (Html Msg)
boardContentsView { boardTiles } =
    List.indexedMap boardTileHtml boardTiles


boardTileHtml : Int -> BoardTile BoardTileMsg -> Html Msg
boardTileHtml index boardTile =
    let
        rowClassList =
            if isEven (index // 8) then
                [ ( "board-tile even-row", True ) ]
            else
                [ ( "board-tile odd-row", True ) ]

        colorClassList =
            if boardTile.color == White then
                [ ( "whites", True ) ]
            else if boardTile.color == Black then
                [ ( "blacks", True ) ]
            else
                []

        pieceClassList =
            if boardTile.type_ == PawnPiece then
                [ ( "piece pawn", True ) ]
            else if boardTile.type_ == KnightPiece then
                [ ( "piece knight", True ) ]
            else if boardTile.type_ == BishopPiece then
                [ ( "piece bishop", True ) ]
            else if boardTile.type_ == RookPiece then
                [ ( "piece rook", True ) ]
            else if boardTile.type_ == QueenPiece then
                [ ( "piece queen", True ) ]
            else if boardTile.type_ == KingPiece then
                [ ( "piece king", True ) ]
            else
                []

        revealedClassList =
            if boardTile.type_ == RevealedPiece then
                [ ( "revealed", True ) ]
            else
                []

        threatenedClassList =
            if boardTile.isThreatened then
                [ ( "threatened", True ) ]
            else
                []

        tileClassList =
            rowClassList ++ pieceClassList ++ colorClassList ++ revealedClassList ++ threatenedClassList
    in
    Html.map BoardTileMsg <|
        div [ classList tileClassList, onClick boardTile.action ]
            [ text <| toString <| index + 1 ]
