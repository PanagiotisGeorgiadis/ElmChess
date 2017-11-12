module DataModels.Pawn exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : Position
    }


initialWhitePlayerPawnState : List Model
initialWhitePlayerPawnState =
    List.foldl (\letter result -> { position = { x = 7, y = letter } } :: result) [] lettersList


initialBlackPlayerPawnState : List Model
initialBlackPlayerPawnState =
    List.foldl (\letter result -> { position = { x = 2, y = letter } } :: result) [] lettersList
