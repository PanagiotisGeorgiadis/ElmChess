module Main exposing (..)

-- import DataModels.Common exposing (PlayerType(..))

import Components.ChessBoard.Update as ChessBoard
import Components.ChessBoard.View as ChessBoard
import DataModels.Common exposing (PlayerType(..))
import Html exposing (Html, div, img, text)
import Html.Attributes exposing (class)


type alias Flags =
    { playerType : String
    }


type alias Model =
    -- { players : Int
    -- , playerType : PlayerType
    -- , chessBoard : ChessBoard.Model
    -- }
    { chessBoard : ChessBoard.Model
    , playerType : PlayerType
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    -- ( { players = 0
    --   , playerType = WhitePlayer
    --   , chessBoard = ChessBoard.initialModel
    --   }
    -- , Cmd.none
    -- )
    ( { chessBoard = ChessBoard.initialModel { playerType = WhitePlayer }
      , playerType = WhitePlayer
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | ChessBoardMsg ChessBoard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChessBoardMsg subMsg ->
            let
                ( updatedModel, subCmd, extMsg ) =
                    ChessBoard.update model model.chessBoard subMsg
            in
            ( { model
                | chessBoard = updatedModel
              }
            , Cmd.map ChessBoardMsg subCmd
            )

        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "app-container" ]
        -- [ Html.map ChessBoardMsg (ChessBoard.view model.chessBoard)
        -- ]
        [ Html.map ChessBoardMsg (ChessBoard.view model model.chessBoard)
        ]


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
