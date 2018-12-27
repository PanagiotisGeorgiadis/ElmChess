module Components.ChessBoard.View exposing (..)

import Components.ChessBoard.Update exposing (Model, Msg(..), Props)
import DataModels.Common
    exposing
        ( BoardTile(..)
        , CastlingDirection(..)
        , ChessPieceType(..)
        , Color(..)
        , getChessPieceImageSrc
        )
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class, classList, src, title)
import Html.Events exposing (onClick)


view : Props p -> Model -> Html Msg
view props model =
    div [ class "board" ]
        [ chessBoardHtml props model
        , chessBoardNumbersHtml
        , chessBoardLettersHtml
        ]


chessBoardHtml : Props p -> Model -> Html Msg
chessBoardHtml { playerColor } { chessBoard } =
    let
        occupiedTileHtml isThreatened tileState action =
            let
                chessPieceImageSrc =
                    getChessPieceImageSrc tileState.attributes

                { color, index, isToggled } =
                    tileState.attributes
            in
            div
                [ class "tile"
                , classList [ ( "threatened", isThreatened ) ]
                ]
                [ div
                    [ class "occupied"
                    , classList
                        [ ( "owned", color == playerColor )
                        , ( "toggled", isToggled )
                        ]
                    , onClick action
                    , title (toString index)
                    ]
                    [ img [ src chessPieceImageSrc ] []
                    ]
                ]

        tileHtml tile =
            case tile of
                EmptyTile index ->
                    div
                        [ class "tile"
                        , onClick ClearRevealedPaths
                        ]
                        [ text (toString index)
                        ]

                OccupiedTile tileState ->
                    occupiedTileHtml False tileState tileState.action

                EmptyPathTile tileState ->
                    div
                        [ class "tile revealed"
                        , onClick (MovePiece tileState)
                        ]
                        []

                CapturablePathTile tileState ->
                    case tileState.previousState of
                        OccupiedTile state ->
                            occupiedTileHtml True state (CapturePiece tileState)

                        otherState ->
                            let
                                _ =
                                    Debug.log "Invalid State that concluded in the Uncaught Error." otherState
                            in
                            div [ class "tile" ]
                                [ text "Uncaught Error!"
                                ]

                CastlingPathTile tileState ->
                    div
                        [ class "tile special"
                        , onClick (PerformCastlingMove tileState)
                        , title "Click this tile to perform castling"
                        ]
                        [ text (toString tileState.index)
                        ]
    in
    div [ class "chessboard" ]
        (List.map tileHtml chessBoard)


chessBoardNumbersHtml : Html Msg
chessBoardNumbersHtml =
    div [ class "chessboard-numbers" ]
        [ div [] [ text "1" ]
        , div [] [ text "2" ]
        , div [] [ text "3" ]
        , div [] [ text "4" ]
        , div [] [ text "5" ]
        , div [] [ text "6" ]
        , div [] [ text "7" ]
        , div [] [ text "8" ]
        ]


chessBoardLettersHtml : Html Msg
chessBoardLettersHtml =
    div [ class "chessboard-letters" ]
        [ div [] [ text "A" ]
        , div [] [ text "B" ]
        , div [] [ text "C" ]
        , div [] [ text "D" ]
        , div [] [ text "E" ]
        , div [] [ text "F" ]
        , div [] [ text "G" ]
        , div [] [ text "H" ]
        ]
