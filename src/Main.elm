module Main exposing (..)

import Components.ChessBoard.Model as ChessBoard
import Components.ChessBoard.Update as ChessBoard
import Components.ChessBoard.View as ChessBoard
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
            let
                ( updatedModel, subcmd ) =
                    ChessBoard.update submsg model.chessBoard
            in
            ( model, Cmd.none )

        -- ( { model
        --     | chessBoard = updatedModel
        --   }
        -- , Cmd.none
        -- )
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
