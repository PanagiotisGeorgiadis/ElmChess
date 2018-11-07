module Components.ChessBoard.View exposing (..)

import Components.ChessBoard.Update exposing (Model, Msg(..), Props)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


view : Props p -> Model -> Html Msg
view props model =
    div [ class "board" ]
        [ chessBoardHtml
        , chessBoardNumbersHtml
        , chessBoardLettersHtml
        ]


chessBoardHtml : Html Msg
chessBoardHtml =
    let
        tileHtml =
            div [ class "tile" ] []
    in
    div [ class "chessboard" ]
        (List.repeat 64 tileHtml)


chessBoardNumbersHtml : Html Msg
chessBoardNumbersHtml =
    div [ class "chessboard-numbers" ]
        (List.map
            (\index ->
                div [] [ text (toString index) ]
            )
            (List.range 1 8)
        )


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
