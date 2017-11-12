module Components.ChessBoard exposing (..)

import DataModels.Bishop as Bishop
import DataModels.Common exposing (..)
import DataModels.King as King
import DataModels.Knight as Knight
import DataModels.Pawn as Pawn
import DataModels.Queen as Queen
import DataModels.Rook as Rook
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList)


type alias Model =
    { boardTiles : Int
    , whitePieces : PlayerPieces
    , whitePoints : Int
    , blackPieces : PlayerPieces
    , blackPoints : Int
    }


type alias PlayerPieces =
    { pawns : List Pawn.Model
    , knights : List Knight.Model
    , bishops : List Bishop.Model
    , rooks : List Rook.Model
    , queen : Queen.Model
    , king : King.Model
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
    , knights = Knight.initialWhitePlayerKnightState
    , bishops = Bishop.initialWhitePlayerBishopState
    , rooks = Rook.initialWhitePlayerRookState
    , queen = Queen.initialWhitePlayerQueenState
    , king = King.initialWhitePlayerKingState
    }


getInitialBlackPlayerState : PlayerPieces
getInitialBlackPlayerState =
    { pawns = Pawn.initialBlackPlayerPawnState
    , knights = Knight.initialBlackPlayerKnightState
    , bishops = Bishop.initialBlackPlayerBishopState
    , rooks = Rook.initialBlackPlayerRookState
    , queen = Queen.initialBlackPlayerQueenState
    , king = King.initialBlackPlayerKingState
    }


view : Model -> Html Msg
view model =
    div [ class "chess-board" ]
        [ lettersContainerView
        , numbersContainerView
        , div [ class "chess-board-inner" ]
            (boardContentsView model)
        ]


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


boardContentsView : Model -> List (Html Msg)
boardContentsView { boardTiles, whitePieces, blackPieces } =
    List.indexedMap (\index _ -> boardTileHtml index whitePieces blackPieces) <|
        List.repeat boardTiles 0


boardTileHtml : Int -> PlayerPieces -> PlayerPieces -> Html Msg
boardTileHtml index whitePieces blackPieces =
    let
        tileClassList =
            if isEven (index // 8) then
                [ ( "board-tile even-row", True ) ]
            else
                [ ( "board-tile odd-row", True ) ]

        updatedClassList =
            tileClassList
                ++ (if isPawnOnTile whitePieces index then
                        [ ( "piece whites pawn", True ) ]
                    else if isBishopOnTile whitePieces index then
                        [ ( "piece whites bishop", True ) ]
                    else if isKnightOnTile whitePieces index then
                        [ ( "piece whites knight", True ) ]
                    else if isRookOnTile whitePieces index then
                        [ ( "piece whites rook", True ) ]
                    else if isQueenOnTile whitePieces index then
                        [ ( "piece whites queen", True ) ]
                    else if isKingOnTile whitePieces index then
                        [ ( "piece whites king", True ) ]
                    else if isPawnOnTile blackPieces index then
                        [ ( "piece blacks pawn", True ) ]
                    else if isBishopOnTile blackPieces index then
                        [ ( "piece blacks bishop", True ) ]
                    else if isKnightOnTile blackPieces index then
                        [ ( "piece blacks knight", True ) ]
                    else if isRookOnTile blackPieces index then
                        [ ( "piece blacks rook", True ) ]
                    else if isQueenOnTile blackPieces index then
                        [ ( "piece blacks queen", True ) ]
                    else if isKingOnTile blackPieces index then
                        [ ( "piece blacks king", True ) ]
                    else
                        []
                   )
    in
    div [ classList updatedClassList ] []


isPawnOnTile : PlayerPieces -> Int -> Bool
isPawnOnTile { pawns } index =
    List.any
        (\pawn ->
            if getIndexFromPosition pawn.position == (index + 1) then
                True
            else
                False
        )
        pawns


isKnightOnTile : PlayerPieces -> Int -> Bool
isKnightOnTile { knights } index =
    List.any
        (\knight ->
            if getIndexFromPosition knight.position == (index + 1) then
                True
            else
                False
        )
        knights


isBishopOnTile : PlayerPieces -> Int -> Bool
isBishopOnTile { bishops } index =
    List.any
        (\bishop ->
            if getIndexFromPosition bishop.position == (index + 1) then
                True
            else
                False
        )
        bishops


isRookOnTile : PlayerPieces -> Int -> Bool
isRookOnTile { rooks } index =
    List.any
        (\rook ->
            if getIndexFromPosition rook.position == (index + 1) then
                True
            else
                False
        )
        rooks


isQueenOnTile : PlayerPieces -> Int -> Bool
isQueenOnTile { queen } index =
    if getIndexFromPosition queen.position == (index + 1) then
        True
    else
        False


isKingOnTile : PlayerPieces -> Int -> Bool
isKingOnTile { king } index =
    if getIndexFromPosition king.position == (index + 1) then
        True
    else
        False
