module DataModels.Pawn exposing (..)

import DataModels.Common exposing (..)


getMovementInstructions : Position -> List Position
getMovementInstructions position =
    case ( position.x, position.y ) of
        ( 6, _ ) ->
            [ Position (position.x - 1) position.y
            , Position (position.x - 2) position.y
            ]

        _ ->
            if position.x > 1 then
                [ Position (position.x - 1) position.y ]
            else
                let
                    _ =
                        Debug.log "This needs fix" ""
                in
                []


initialWhitePlayerState : List (BoardTile BoardTileMsg)
initialWhitePlayerState =
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


initialBlackPlayerState : List (BoardTile BoardTileMsg)
initialBlackPlayerState =
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
