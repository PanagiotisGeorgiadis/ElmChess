module Main exposing (..)

import Components.ChessBoard as ChessBoard
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class)


---- MODEL ----


type alias Model =
    { players : Int
    , playerType : PlayerType
    , chessBoard : ChessBoard.Model
    }


type PlayerType
    = White
    | Black


init : ( Model, Cmd Msg )
init =
    ( { players = 0
      , playerType = White
      , chessBoard = ChessBoard.initialModel
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | ChessBoardMsg ChessBoard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChessBoardMsg submsg ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        [ Html.map ChessBoardMsg <| ChessBoard.view model.chessBoard
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
