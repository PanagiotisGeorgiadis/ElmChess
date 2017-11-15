module DataModels.Pawn exposing (..)

import DataModels.Common exposing (..)


type alias Model =
    { type_ : TileType
    , position : Position
    , action : Maybe Msg
    , color : PieceColor
    }


type Msg
    = NoOp
    | MovePawn Position
    | RevealPawnMovement Int


initialWhitePlayerPawnState : List Model
initialWhitePlayerPawnState =
    List.foldl
        (\letter result ->
            { position = { x = 6, y = letter }
            , type_ = PawnPiece
            , action = Nothing
            , color = White
            }
                :: result
        )
        []
        lettersList


initialBlackPlayerPawnState : List Model
initialBlackPlayerPawnState =
    List.foldl
        (\letter result ->
            { position = { x = 1, y = letter }
            , type_ = PawnPiece
            , action = Just NoOp
            , color = Black
            }
                :: result
        )
        []
        lettersList


getStateFromIndex : Int -> List Model -> Maybe Model
getStateFromIndex index pawns =
    List.head <|
        List.foldl
            (\pawn result ->
                if getIndexFromPosition pawn.position == index then
                    pawn :: result
                else
                    result
            )
            []
            pawns



-- Maybe Introduce MessageTypes in Common.elm and resolveActions based on this Common Msg type.


resolveAction : Int -> Maybe Msg -> Msg
resolveAction index msg =
    case msg of
        Just action ->
            action

        Nothing ->
            RevealPawnMovement index


movementInstructions : Position -> List Position
movementInstructions position =
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
