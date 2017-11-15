module DataModels.Bishop exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MoveOwnBishop


initialWhitePlayerBishopState : List Model
initialWhitePlayerBishopState =
    [ { position = { x = 7, y = C }
      , type_ = BishopPiece
      , action = MoveOwnBishop
      , color = White
      }
    , { position = { x = 7, y = F }
      , type_ = BishopPiece
      , action = MoveOwnBishop
      , color = White
      }
    ]


initialBlackPlayerBishopState : List Model
initialBlackPlayerBishopState =
    [ { position = { x = 0, y = C }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      }
    , { position = { x = 0, y = F }
      , type_ = BishopPiece
      , action = NoOp
      , color = Black
      }
    ]


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index bishops =
    List.head <|
        List.foldl
            (\bishop result ->
                if getIndexFromPosition bishop.position == index then
                    bishop :: result
                else
                    result
            )
            []
            bishops
