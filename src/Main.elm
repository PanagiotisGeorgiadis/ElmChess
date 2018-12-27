module Main exposing (..)

import Components.ChessBoard.Update as ChessBoard
import Components.ChessBoard.View as ChessBoard
import DataModels.Common
    exposing
        ( BoardTile(..)
        , BoardTileAttributes
        , Color
        , getChessPieceImageSrc
        , getPlayerColor
        )
import Html
    exposing
        ( Html
        , div
        , h2
        , img
        , text
        )
import Html.Attributes exposing (class, src)
import Utils.Actions exposing (simpleAction)


type alias Flags =
    { playerColor : String
    }


type alias Model =
    -- { players : Int
    -- , playerType : PlayerType
    -- , chessBoard : ChessBoard.Model
    -- }
    { chessBoard : ChessBoard.Model
    , playerColor : Color
    , playerCapturedPieces : List BoardTileAttributes
    , enemyCapturedPieces : List BoardTileAttributes
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        playerColor =
            getPlayerColor flags.playerColor
    in
    -- ( { players = 0
    --   , playerType = WhitePlayer
    --   , chessBoard = ChessBoard.initialModel
    --   }
    -- , Cmd.none
    -- )
    ( { chessBoard = ChessBoard.initialModel { playerColor = playerColor }
      , playerColor = playerColor
      , playerCapturedPieces = []
      , enemyCapturedPieces = []
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | AddCapturedPiece (Maybe BoardTileAttributes)
    | ChessBoardMsg ChessBoard.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChessBoardMsg subMsg ->
            let
                ( updatedModel, subCmd, extMsg ) =
                    ChessBoard.update model model.chessBoard subMsg

                capturedPieceAttributes =
                    case extMsg of
                        ChessBoard.PieceCaptured (OccupiedTile { attributes }) ->
                            Just attributes

                        ChessBoard.PieceCaptured _ ->
                            Nothing

                        ChessBoard.None ->
                            Nothing
            in
            ( { model
                | chessBoard = updatedModel
              }
            , Cmd.batch
                [ Cmd.map ChessBoardMsg subCmd
                , simpleAction (AddCapturedPiece capturedPieceAttributes)
                ]
            )

        AddCapturedPiece boardTileAttributes ->
            ( { model
                | playerCapturedPieces =
                    case boardTileAttributes of
                        Just attrs ->
                            List.append model.playerCapturedPieces [ attrs ]

                        Nothing ->
                            model.playerCapturedPieces
              }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ class "app-container no-select" ]
        [ enemyBoardHtml model
        , Html.map ChessBoardMsg (ChessBoard.view model model.chessBoard)
        , playerBoardHtml model
        ]


enemyBoardHtml : Model -> Html Msg
enemyBoardHtml model =
    div [ class "enemy-board" ]
        [ h2 [ class "enemy-name" ] [ text "Mr Meeseeks" ]
        , div [ class "captured-pieces" ]
            [ div []
                (List.map capturedPieceHtml model.enemyCapturedPieces)
            ]
        ]


playerBoardHtml : Model -> Html Msg
playerBoardHtml model =
    div [ class "player-board" ]
        [ h2 [ class "player-name" ] [ text "Panos" ]
        , div [ class "captured-pieces" ]
            [ div []
                (List.map capturedPieceHtml model.playerCapturedPieces)
            ]
        ]


capturedPieceHtml : BoardTileAttributes -> Html Msg
capturedPieceHtml boardTile =
    img [ src (getChessPieceImageSrc boardTile) ] []



-- { index : Int
-- , color : Color
-- , type_ : ChessPieceType
-- , isToggled : Bool
-- }


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
