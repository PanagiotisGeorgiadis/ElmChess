module DataModels.Pawn exposing (..)

import DataModels.Common exposing (..)


getMovementInstructions : Position -> List MovementInstructions
getMovementInstructions position =
    case ( position.x, position.y ) of
        ( 6, _ ) ->
            [ { position = Position (position.x - 1) position.y
              , isCapturableMove = False
              }
            , { position = Position (position.x - 2) position.y
              , isCapturableMove = False
              }
            ]
                ++ getCapturableMovementInstructions position

        _ ->
            if position.x > 1 then
                [ { position = Position (position.x - 1) position.y
                  , isCapturableMove = False
                  }
                ]
                    ++ getCapturableMovementInstructions position
            else
                []
                    ++ getCapturableMovementInstructions position


getCapturableMovementInstructions : Position -> List MovementInstructions
getCapturableMovementInstructions position =
    case ( incrementBoardLetter position.y, decrementBoardLetter position.y ) of
        ( Just a, Just b ) ->
            [ { position = Position (position.x - 1) a
              , isCapturableMove = True
              }
            , { position = Position (position.x - 1) b
              , isCapturableMove = True
              }
            ]

        ( Just a, Nothing ) ->
            [ { position = Position (position.x - 1) a
              , isCapturableMove = True
              }
            ]

        ( Nothing, Just b ) ->
            [ { position = Position (position.x - 1) b
              , isCapturableMove = True
              }
            ]

        ( Nothing, Nothing ) ->
            []


initialWhitePiecesState : List (BoardTile BoardTileMsg)
initialWhitePiecesState =
    List.foldl
        (\letter result ->
            { position = { x = 6, y = letter }
            , type_ = PawnPiece
            , action = RevealPawnMovement (getIndexFromPosition { x = 6, y = letter })
            , color = White
            , isThreatened = False
            }
                :: result
        )
        []
        lettersList


initialBlackPiecesState : List (BoardTile BoardTileMsg)
initialBlackPiecesState =
    List.foldl
        (\letter result ->
            { position = { x = 1, y = letter }
            , type_ = PawnPiece
            , action = NoOp
            , color = Black
            , isThreatened = False
            }
                :: result
        )
        []
        lettersList
