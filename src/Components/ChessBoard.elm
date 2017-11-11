module Components.ChessBoard exposing (..)

import DataModels.Common exposing (..)
import DataModels.Pawn as Pawn
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)


type alias Model =
    { boardTiles : Int
    , whitePieces : PlayerPieces
    , whitePoints : Int
    , blackPieces : PlayerPieces
    , blackPoints : Int
    }


type alias PlayerPieces =
    { pawns : List Pawn.Model

    -- , knights : List Knight
    -- , bishops : List Bishop
    -- , rooks : List Rook
    -- , queen : Queen
    -- , king : King
    }


type Msg
    = NoOp


initialModel : Model
initialModel =
    { boardTiles = 8 * 8
    , whitePieces = getInitialWhitePlayerState
    , whitePoints = 0
    , blackPieces = getInitialBlackPlayerState
    , blackPoints = 0
    }


getInitialWhitePlayerState : PlayerPieces
getInitialWhitePlayerState =
    { pawns = Pawn.initialWhitePlayerPawnState

    -- , knights : getInitialKn
    }


getInitialBlackPlayerState : PlayerPieces
getInitialBlackPlayerState =
    { pawns = Pawn.initialBlackPlayerPawnState
    }


view : Model -> Html Msg
view model =
    div [ class "chess-board" ]
        [ lettersContainerView
        , numbersContainerView
        , div [ class "chess-board-inner" ]
            (boardContentsView model)
        ]


letterItemHtml : String -> Html Msg
letterItemHtml letterText =
    div [] [ text letterText ]


numberItemHtml : Int -> Html Msg
numberItemHtml numberText =
    div [] [ text <| toString numberText ]


lettersContainerView : Html Msg
lettersContainerView =
    div [ class "letters-container" ]
        (List.map letterItemHtml lettersList)


numbersContainerView : Html Msg
numbersContainerView =
    div [ class "numbers-container" ]
        (List.map numberItemHtml numbersList)


boardContentsView : Model -> List (Html Msg)
boardContentsView { boardTiles } =
    List.indexedMap (\index _ -> boardTileHtml index) <|
        List.repeat boardTiles 0


isEven : Int -> Bool
isEven number =
    if number % 2 == 0 then
        True
    else
        False


boardTileHtml : Int -> Html Msg
boardTileHtml index =
    let
        tileClassName =
            if isEven (index // 8) then
                "board-tile even-row"
            else
                "board-tile odd-row"
    in
    div [ class tileClassName ] []



-- if index == 0 then
--     div [ class "board-tile white-tile" ] [ text <| toString (isEven (index // 8)) ]
-- else if isEven index then
--     div [ class "board-tile white-tile" ] [ text <| toString (isEven (index // 8)) ]
-- else
--     div [ class "board-tile black-tile" ] [ text <| toString (isEven (index // 8)) ]
