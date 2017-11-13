module DataModels.Knight exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MoveOwnKnight


initialWhitePlayerKnightState : List Model
initialWhitePlayerKnightState =
    [ { position = { x = 8, y = B }
      , type_ = KnightPiece
      , action = MoveOwnKnight
      , color = White
      }
    , { position = { x = 8, y = G }
      , type_ = KnightPiece
      , action = MoveOwnKnight
      , color = White
      }
    ]


initialBlackPlayerKnightState : List Model
initialBlackPlayerKnightState =
    [ { position = { x = 1, y = B }
      , type_ = KnightPiece
      , action = NoOp
      , color = Black
      }
    , { position = { x = 1, y = G }
      , type_ = KnightPiece
      , action = NoOp
      , color = Black
      }
    ]


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index knights =
    List.head <|
        List.foldl
            (\knight result ->
                if getIndexFromPosition knight.position == (index + 1) then
                    knight :: result
                else
                    result
            )
            []
            knights
