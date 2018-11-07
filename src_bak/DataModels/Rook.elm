module DataModels.Rook exposing (..)

import DataModels.Common exposing (..)


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    [ { position = { x = 7, y = A }
      , type_ = RookPiece

      -- , action = RevealRookMovement (getIndexFromPosition { x = 7, y = A })
      , action = RevealRookMovement 57
      , color = White
      , isThreatened = False
      , index = 57
      }
    , { position = { x = 7, y = H }
      , type_ = RookPiece
      , action = RevealRookMovement 64
      , color = White
      , isThreatened = False
      , index = 64
      }
    ]


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    [ { position = { x = 0, y = A }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 1
      }
    , { position = { x = 0, y = H }
      , type_ = RookPiece
      , action = NoOp
      , color = Black
      , isThreatened = False
      , index = 8
      }
    ]


getXAxisMovementInstructions : Position -> List MovementInstructions
getXAxisMovementInstructions position =
    List.foldl
        (\columnLetter r ->
            -- if columnLetter == position.y then
            --     r
            -- else
            List.append r
                [ { position = Position position.x columnLetter
                  , isCapturableMove = True
                  }
                ]
        )
        []
        lettersList


getYAxisMovementInstructions : Position -> List MovementInstructions
getYAxisMovementInstructions position =
    List.foldl
        (\rowIndex r ->
            -- if rowIndex == (position.x + 1) then
            --     r
            -- else
            List.append r
                [ { position = Position rowIndex position.y
                  , isCapturableMove = True
                  }
                ]
        )
        []
        rowsIndexList


getMovementInstructions : Position -> List MovementInstructions
getMovementInstructions position =
    let
        availablePositionX =
            -- List.foldl
            --     (\rowIndex r ->
            --         if rowIndex == (position.x + 1) then
            --             r
            --         else
            --             List.append r
            --                 [ { position = Position rowIndex position.y
            --                   , isCapturableMove = True
            --                   }
            --                 ]
            --     )
            --     []
            --     rowsIndexList
            getXAxisMovementInstructions position

        availablePositionY =
            -- List.foldl
            --     (\columnLetter r ->
            --         if columnLetter == position.y then
            --             r
            --         else
            --             List.append r
            --                 [ { position = Position position.x columnLetter
            --                   , isCapturableMove = True
            --                   }
            --                 ]
            --     )
            --     []
            --     lettersList
            getYAxisMovementInstructions position
    in
    availablePositionX ++ availablePositionY



-- case ( position.x, position.y ) of
--     ( 6, _ ) ->
--         [ { position = Position (position.x - 1) position.y
--           , isCapturableMove = False
--           }
--         , { position = Position (position.x - 2) position.y
--           , isCapturableMove = False
--           }
--         ]
--             ++ getCapturableMovementInstructions position
--
--     _ ->
--         if position.x > 1 then
--             [ { position = Position (position.x - 1) position.y
--               , isCapturableMove = False
--               }
--             ]
--                 ++ getCapturableMovementInstructions position
--         else
--             []
--                 ++ getCapturableMovementInstructions position
-- getCapturableMovementInstructions : Position -> List MovementInstructions
-- getCapturableMovementInstructions position =
--     case ( incrementBoardLetter position.y, decrementBoardLetter position.y ) of
--         ( Just a, Just b ) ->
--             [ { position = Position (position.x - 1) a
--               , isCapturableMove = True
--               }
--             , { position = Position (position.x - 1) b
--               , isCapturableMove = True
--               }
--             ]
--
--         ( Just a, Nothing ) ->
--             [ { position = Position (position.x - 1) a
--               , isCapturableMove = True
--               }
--             ]
--
--         ( Nothing, Just b ) ->
--             [ { position = Position (position.x - 1) b
--               , isCapturableMove = True
--               }
--             ]
--
--         ( Nothing, Nothing ) ->
--             []
