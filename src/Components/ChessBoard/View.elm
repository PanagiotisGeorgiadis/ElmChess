module Components.ChessBoard.View exposing (..)

-- import Components.ChessBoard.Model exposing (..)

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
boardContentsView { boardState } =
    List.indexedMap boardTileHtml boardState


boardTileHtml : Int -> BoardTile Msg -> Html Msg
boardTileHtml index boardState =
    let
        rowClassList =
            if isEven (index // 8) then
                [ ( "board-tile even-row", True ) ]
            else
                [ ( "board-tile odd-row", True ) ]

        colorClassList =
            if boardState.color == White then
                [ ( "whites", True ) ]
            else
                [ ( "blacks", True ) ]

        pieceClassList =
            if boardState.type_ == PawnPiece then
                [ ( "piece pawn", True ) ]
            else if boardState.type_ == KnightPiece then
                [ ( "piece knight", True ) ]
            else if boardState.type_ == BishopPiece then
                [ ( "piece bishop", True ) ]
            else if boardState.type_ == RookPiece then
                [ ( "piece rook", True ) ]
            else if boardState.type_ == QueenPiece then
                [ ( "piece queen", True ) ]
            else if boardState.type_ == KingPiece then
                [ ( "piece king", True ) ]
            else
                []
    in
    div [ classList (rowClassList ++ pieceClassList ++ colorClassList), onClick boardState.action ] []



-- boardContentsView : Model -> List (Html Msg)
-- boardContentsView { boardTiles, whitePieces, blackPieces } =
--     List.indexedMap (\index _ -> boardTileHtml index whitePieces blackPieces) <|
--         List.repeat boardTiles 0
--
--
-- boardTileHtml : Int -> PlayerPieces -> PlayerPieces -> Html Msg
-- boardTileHtml index whitePieces blackPieces =
--     let
--         tileClassList =
--             if isEven (index // 8) then
--                 [ ( "board-tile even-row", True ) ]
--             else
--                 [ ( "board-tile odd-row", True ) ]
--
--         updatedClassList =
--             tileClassList
--                 ++ (if isPawnOnTile whitePieces index then
--                         [ ( "piece whites pawn", True ) ]
--                     else if isBishopOnTile whitePieces index then
--                         [ ( "piece whites bishop", True ) ]
--                     else if isKnightOnTile whitePieces index then
--                         [ ( "piece whites knight", True ) ]
--                     else if isRookOnTile whitePieces index then
--                         [ ( "piece whites rook", True ) ]
--                     else if isQueenOnTile whitePieces index then
--                         [ ( "piece whites queen", True ) ]
--                     else if isKingOnTile whitePieces index then
--                         [ ( "piece whites king", True ) ]
--                     else if isPawnOnTile blackPieces index then
--                         [ ( "piece blacks pawn", True ) ]
--                     else if isBishopOnTile blackPieces index then
--                         [ ( "piece blacks bishop", True ) ]
--                     else if isKnightOnTile blackPieces index then
--                         [ ( "piece blacks knight", True ) ]
--                     else if isRookOnTile blackPieces index then
--                         [ ( "piece blacks rook", True ) ]
--                     else if isQueenOnTile blackPieces index then
--                         [ ( "piece blacks queen", True ) ]
--                     else if isKingOnTile blackPieces index then
--                         [ ( "piece blacks king", True ) ]
--                     else
--                         []
--                    )
--     in
--     div [ classList updatedClassList ] []
