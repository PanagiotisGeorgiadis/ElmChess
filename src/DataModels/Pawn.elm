module DataModels.Pawn exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { position : ( String, Int )
    }


initialWhitePlayerPawnState : List Model
initialWhitePlayerPawnState =
    List.foldl (\letter result -> { position = ( letter, 2 ) } :: result) [] lettersList


initialBlackPlayerPawnState : List Model
initialBlackPlayerPawnState =
    List.foldl (\letter result -> { position = ( letter, 7 ) } :: result) [] lettersList
