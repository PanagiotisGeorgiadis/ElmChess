module DataModels.Common exposing (..)

import Utils.Setters exposing (updateIndex, updateIsToggled)


type Color
    = Black
    | White


type alias ChessBoard msg =
    List (BoardTile msg)


type alias ChessBoardConfig msg =
    { noAction : BoardTileAttributes -> msg
    , action : BoardTileAttributes -> msg
    , playerColor : Color
    }


type
    BoardTile msg
    -- Basic Implementation Tiles.
    = EmptyTile Int
    | EmptyPathTile (EmptyPathTileState msg)
    | OccupiedTile (BoardTileState msg)
    | CapturablePathTile (CapturableTileState msg)
      -- Special Tiles.
    | CastlingPathTile CastlingTileState



-- | PromotionPathTile


type alias BoardTileState msg =
    { attributes : BoardTileAttributes
    , actionConstructor : BoardTileAttributes -> msg
    , action : msg
    }


type alias BoardTileAttributes =
    { index : Int
    , color : Color
    , type_ : ChessPieceType
    , isToggled : Bool
    }


type alias EmptyPathTileState msg =
    { targetIndex : Int
    , sourceTile : Maybe (BoardTile msg)
    }


type alias CapturableTileState msg =
    { targetIndex : Int
    , sourceTile : Maybe (BoardTile msg)

    -- Keep the previous state in case the user
    -- cancels their action OR if they have
    -- multiple capturable tiles and they select
    -- only one of those.
    , previousState : BoardTile msg
    }


type alias CastlingTileState =
    { castlingDiretion : CastlingDirection
    , index : Int
    }


type alias CastlingPrerequisites =
    { queenRookMoved : Bool
    , kingRookMoved : Bool
    , kingMoved : Bool
    }


type ChessPieceType
    = Pawn
    | Knight
    | Bishop
    | Rook
    | Queen
    | King


type BoardSide
    = Top
    | Bottom


type Direction
    = North
    | South
    | West
    | East
    | NorthWest
    | NorthEast
    | SouthWest
    | SouthEast


type CastlingDirection
    = QueenSideCastling
    | KingSideCastling


initialChessBoard : ChessBoardConfig msg -> ChessBoard msg
initialChessBoard { noAction, action, playerColor } =
    case playerColor of
        Black ->
            initialWhiteSide Top noAction
                ++ List.map EmptyTile (List.range 16 47)
                ++ initialBlackSide Bottom action

        White ->
            initialBlackSide Top noAction
                ++ List.map EmptyTile (List.range 16 47)
                ++ initialWhiteSide Bottom action


initialBlackSide : BoardSide -> (BoardTileAttributes -> msg) -> List (BoardTile msg)
initialBlackSide boardSide msg =
    let
        boardTileState index type_ =
            let
                attrs =
                    BoardTileAttributes index Black type_ False
            in
            BoardTileState attrs msg (msg attrs)

        tileStates =
            List.map2 boardTileState (tileIndexes boardSide) (boardPiecesList boardSide)
    in
    List.map OccupiedTile tileStates


initialWhiteSide : BoardSide -> (BoardTileAttributes -> msg) -> List (BoardTile msg)
initialWhiteSide boardSide msg =
    let
        boardTileState index type_ =
            let
                attrs =
                    BoardTileAttributes index White type_ False
            in
            BoardTileState attrs msg (msg attrs)

        tileStates =
            List.map2 boardTileState (tileIndexes boardSide) (boardPiecesList boardSide)
    in
    List.map OccupiedTile tileStates


tileIndexes : BoardSide -> List Int
tileIndexes boardSide =
    case boardSide of
        Top ->
            List.range 0 15

        Bottom ->
            List.range 48 63


getChessPieceImageSrc : BoardTileAttributes -> String
getChessPieceImageSrc { color, type_ } =
    String.join "/"
        [ "/fontawesome-svg"
        , case color of
            Black ->
                "solid"

            White ->
                "light"
        , case type_ of
            Pawn ->
                "chess-pawn.svg"

            Knight ->
                "chess-knight.svg"

            Bishop ->
                "chess-bishop.svg"

            Rook ->
                "chess-rook.svg"

            Queen ->
                "chess-queen.svg"

            King ->
                "chess-king.svg"
        ]


getPlayerColor : String -> Color
getPlayerColor color =
    case color of
        "White" ->
            White

        "Black" ->
            Black

        _ ->
            Debug.crash "Wrong color given to flags" color



-- getOpposingColor : Color -> Color
-- getOpposingColor color =
--     case color of
--         White ->
--             Black
--
--         Black ->
--             White


revealPawnPath : BoardTileAttributes -> ChessBoard msg -> ChessBoard msg
revealPawnPath ({ index } as tileAttributes) chessBoard =
    let
        initialPawnIndexes =
            List.range 48 55

        isInInitialPosition =
            List.any ((==) index) initialPawnIndexes

        capturablePathIndexes =
            -- let
            --     paths =
            --         if isInFirstColumn index then
            --             [ index - 7 ]
            --
            --         else if isInEighthColumn index then
            --             [ index - 9 ]
            --
            --         else
            --             [ index - 7
            --             , index - 9
            --             ]
            -- in
            -- -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) <|
            -- --     List.filter (isOccupied chessBoard) paths
            -- List.filter (not << isOutOfBoundsIndex) <|
            --     List.filter (not << isOccupiedBySameColor tileAttributes updatedChessBoard) paths
            getPawnCapturablePath tileAttributes updatedChessBoard index

        emptyPathIndexes =
            let
                paths =
                    if isInInitialPosition then
                        [ index - 8
                        , index - 16
                        ]

                    else
                        [ index - 8
                        ]
            in
            List.filter (not << isOccupied updatedChessBoard) paths

        updatedChessBoard =
            toggleSelectedPiece tileAttributes chessBoard

        sourceTile =
            getOccupiedTileByIndex updatedChessBoard index
    in
    List.map
        (\tile ->
            case tile of
                EmptyTile index_ ->
                    if List.any ((==) index_) emptyPathIndexes then
                        EmptyPathTile (EmptyPathTileState index_ sourceTile)

                    else
                        tile

                OccupiedTile tileState ->
                    let
                        isCapturablePath =
                            List.any ((==) tileState.attributes.index) capturablePathIndexes
                    in
                    if isCapturablePath then
                        CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))

                    else
                        tile

                _ ->
                    tile
        )
        updatedChessBoard


getPawnCapturablePath : BoardTileAttributes -> ChessBoard msg -> Int -> List Int
getPawnCapturablePath tileAttributes chessBoard index =
    let
        paths =
            if isInFirstColumn index then
                [ index - 7 ]

            else if isInEighthColumn index then
                [ index - 9 ]

            else
                [ index - 7
                , index - 9
                ]
    in
    -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) <|
    --     List.filter (isOccupied chessBoard) paths
    List.filter (not << isOutOfBoundsIndex) <|
        List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) paths


revealKnightPath : BoardTileAttributes -> ChessBoard msg -> ChessBoard msg
revealKnightPath ({ index } as tileAttributes) chessBoard =
    let
        updatedChessBoard =
            toggleSelectedPiece tileAttributes chessBoard

        validPathIndexes =
            getKnightValidPathIndexes index

        -- emptyPathIndexes =
        --     -- List.filter (not << isOccupied chessBoard) validPathIndexes
        --     List.filter (not << isOccupied updatedChessBoard) validPathIndexes
        --
        -- capturablePathIndexes =
        --     List.filter (not << isOutOfBoundsIndex) <|
        --         -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) validPathIndexes
        --         List.filter (not << isOccupiedBySameColor tileAttributes updatedChessBoard) validPathIndexes
        --
        -- sourceTile =
        --     getOccupiedTileByIndex index updatedChessBoard
    in
    -- List.map
    --     (\tile ->
    --         case tile of
    --             EmptyTile index_ ->
    --                 if List.any ((==) index_) emptyPathIndexes then
    --                     EmptyPathTile (EmptyPathTileState index_ sourceTile)
    --
    --                 else
    --                     tile
    --
    --             OccupiedTile tileState ->
    --                 let
    --                     isCapturablePath =
    --                         List.any ((==) tileState.attributes.index) capturablePathIndexes
    --                 in
    --                 if isCapturablePath then
    --                     CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))
    --
    --                 else
    --                     tile
    --
    --             _ ->
    --                 tile
    --     )
    --     updatedChessBoard
    revealPathsInBoard tileAttributes validPathIndexes updatedChessBoard


getKnightValidPathIndexes : Int -> List Int
getKnightValidPathIndexes index =
    if isInFirstColumn index then
        [ index - 15
        , index - 6
        , index + 17
        , index + 10
        ]

    else if isInSecondColumn index then
        [ index - 17
        , index - 15
        , index - 6
        , index + 17
        , index + 15
        , index + 10
        ]

    else if isInThirdColumn index || isInFourthColumn index || isInFifthColumn index || isInSixthColumn index then
        [ index - 17
        , index - 15
        , index - 10
        , index - 6
        , index + 17
        , index + 15
        , index + 10
        , index + 6
        ]

    else if isInSeventhColumn index then
        [ index - 17
        , index - 15
        , index - 10
        , index + 17
        , index + 15
        , index + 6
        ]

    else if isInEighthColumn index then
        [ index - 17
        , index - 10
        , index + 15
        , index + 6
        ]

    else
        []


revealBishopPath : BoardTileAttributes -> ChessBoard msg -> ChessBoard msg
revealBishopPath ({ index } as tileAttributes) chessBoard =
    let
        -- These are the valid multiples of each number
        -- based on the maximum distance that a bishop
        -- can cover in any given position.
        -- Invalid means that it would be out of boundries.
        -- ( multiplesOfSeven, multiplesOfNine ) =
        --     ( [ 7, 14, 21, 28, 35, 42, 49 ]
        --     , [ 9, 18, 27, 36, 45, 54, 63 ]
        --     )
        --
        -- subtractFromIndex indexes =
        --     List.map ((-) index) indexes
        --
        -- addToIndex indexes =
        --     List.map ((+) index) indexes
        updatedChessBoard =
            toggleSelectedPiece tileAttributes chessBoard

        -- Kept for backup.
        -- Seems that the new approach is working as intended but we may as well keep it for backups.
        -- preparePaths =
        --     filterOutBlockedPath updatedChessBoard << List.filter (not << isOutOfBoundsIndex)
        --
        -- validPathIndexes =
        --     -- Present Self:
        --     -- This algorithm was a result of writting out all specific scenarios and
        --     -- simplifying the patters that appeared in the results. Please don't be
        --     -- the smart guy and change it.
        --     --
        --     preparePaths (addToIndex (List.take (7 - getColumnIndex index) multiplesOfNine))
        --         ++ preparePaths (subtractFromIndex (List.take (7 - getColumnIndex index) multiplesOfSeven))
        --         ++ preparePaths (addToIndex (List.take (getColumnIndex index) multiplesOfSeven))
        --         ++ preparePaths (subtractFromIndex (List.take (getColumnIndex index) multiplesOfNine))
        -- _ =
        --     Debug.log "validPathIndexes" validPathIndexes
        -- preparePaths =
        --     filterOutBlockedPath updatedChessBoard
        validPathIndexes =
            -- preparePaths (getIndexesRelativeToPosition NorthWest index)
            --     ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
            --     ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
            --     ++ preparePaths (getIndexesRelativeToPosition SouthEast index)
            getBishopValidPathIndexes updatedChessBoard index

        -- sourceTile =
        --     -- getOccupiedTileByIndex index chessBoard
        --     getOccupiedTileByIndex index updatedChessBoard
        --
        -- emptyPathIndexes =
        --     -- List.filter (not << isOccupied chessBoard) validPathIndexes
        --     List.filter (not << isOccupied updatedChessBoard) validPathIndexes
        --
        -- capturablePathIndexes =
        --     List.filter (not << isOutOfBoundsIndex) <|
        --         -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) validPathIndexes
        --         List.filter (not << isOccupiedBySameColor tileAttributes updatedChessBoard) validPathIndexes
    in
    -- List.map
    --     (\tile ->
    --         case tile of
    --             EmptyTile index_ ->
    --                 if List.any ((==) index_) emptyPathIndexes then
    --                     EmptyPathTile (EmptyPathTileState index_ sourceTile)
    --
    --                 else
    --                     tile
    --
    --             OccupiedTile tileState ->
    --                 let
    --                     isCapturablePath =
    --                         List.any ((==) tileState.attributes.index) capturablePathIndexes
    --                 in
    --                 if isCapturablePath then
    --                     CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))
    --
    --                 else
    --                     tile
    --
    --             _ ->
    --                 tile
    --     )
    --     updatedChessBoard
    revealPathsInBoard tileAttributes validPathIndexes updatedChessBoard


getBishopValidPathIndexes : ChessBoard msg -> Int -> List Int
getBishopValidPathIndexes chessBoard index =
    let
        preparePaths =
            filterOutBlockedPath chessBoard
    in
    preparePaths (getIndexesRelativeToPosition NorthWest index)
        ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
        ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
        ++ preparePaths (getIndexesRelativeToPosition SouthEast index)


revealRookPath : BoardTileAttributes -> ChessBoard msg -> ChessBoard msg
revealRookPath ({ index } as tileAttributes) chessBoard =
    let
        updatedChessBoard =
            toggleSelectedPiece tileAttributes chessBoard

        -- preparePaths =
        --     filterOutBlockedPath updatedChessBoard
        validPathIndexes =
            -- preparePaths (getIndexesRelativeToPosition North index)
            --     ++ preparePaths (getIndexesRelativeToPosition South index)
            --     ++ preparePaths (getIndexesRelativeToPosition West index)
            --     ++ preparePaths (getIndexesRelativeToPosition East index)
            getRookValidPathIndexes updatedChessBoard index

        -- Should you be using the updatedChessBoard here ?
        -- Check other path functions.
        -- sourceTile =
        --     -- getOccupiedTileByIndex index chessBoard
        --     getOccupiedTileByIndex index updatedChessBoard
        --
        -- emptyPathIndexes =
        --     -- List.filter (not << isOccupied chessBoard) validPathIndexes
        --     List.filter (not << isOccupied updatedChessBoard) validPathIndexes
        --
        -- capturablePathIndexes =
        --     List.filter (not << isOutOfBoundsIndex) <|
        --         -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) validPathIndexes
        --         List.filter (not << isOccupiedBySameColor tileAttributes updatedChessBoard) validPathIndexes
    in
    -- List.map
    --     (\tile ->
    --         case tile of
    --             EmptyTile index_ ->
    --                 if List.any ((==) index_) emptyPathIndexes then
    --                     EmptyPathTile (EmptyPathTileState index_ sourceTile)
    --
    --                 else
    --                     tile
    --
    --             OccupiedTile tileState ->
    --                 let
    --                     isCapturablePath =
    --                         List.any ((==) tileState.attributes.index) capturablePathIndexes
    --                 in
    --                 if isCapturablePath then
    --                     CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))
    --
    --                 else
    --                     tile
    --
    --             _ ->
    --                 tile
    --     )
    --     updatedChessBoard
    revealPathsInBoard tileAttributes validPathIndexes updatedChessBoard


getRookValidPathIndexes : ChessBoard msg -> Int -> List Int
getRookValidPathIndexes chessBoard index =
    let
        preparePaths =
            filterOutBlockedPath chessBoard
    in
    preparePaths (getIndexesRelativeToPosition North index)
        ++ preparePaths (getIndexesRelativeToPosition South index)
        ++ preparePaths (getIndexesRelativeToPosition West index)
        ++ preparePaths (getIndexesRelativeToPosition East index)


revealQueenPath : BoardTileAttributes -> ChessBoard msg -> ChessBoard msg
revealQueenPath ({ index } as tileAttributes) chessBoard =
    let
        updatedChessBoard =
            toggleSelectedPiece tileAttributes chessBoard

        -- preparePaths =
        --     filterOutBlockedPath updatedChessBoard
        validPathIndexes =
            -- preparePaths (getIndexesRelativeToPosition North index)
            --     ++ preparePaths (getIndexesRelativeToPosition South index)
            --     ++ preparePaths (getIndexesRelativeToPosition West index)
            --     ++ preparePaths (getIndexesRelativeToPosition East index)
            --     ++ preparePaths (getIndexesRelativeToPosition NorthWest index)
            --     ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
            --     ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
            --     ++ preparePaths (getIndexesRelativeToPosition SouthEast index)
            getQueenValidPathIndexes updatedChessBoard index

        -- Should you be using the updatedChessBoard here ?
        -- Check other path functions.
        -- sourceTile =
        --     -- getOccupiedTileByIndex index chessBoard
        --     getOccupiedTileByIndex index updatedChessBoard
        --
        -- emptyPathIndexes =
        --     -- List.filter (not << isOccupied chessBoard) validPathIndexes
        --     List.filter (not << isOccupied updatedChessBoard) validPathIndexes
        --
        -- capturablePathIndexes =
        --     List.filter (not << isOutOfBoundsIndex) <|
        --         -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) validPathIndexes
        --         List.filter (not << isOccupiedBySameColor tileAttributes updatedChessBoard) validPathIndexes
    in
    -- List.map
    --     (\tile ->
    --         case tile of
    --             EmptyTile index_ ->
    --                 if List.any ((==) index_) emptyPathIndexes then
    --                     EmptyPathTile (EmptyPathTileState index_ sourceTile)
    --
    --                 else
    --                     tile
    --
    --             OccupiedTile tileState ->
    --                 let
    --                     isCapturablePath =
    --                         List.any ((==) tileState.attributes.index) capturablePathIndexes
    --                 in
    --                 if isCapturablePath then
    --                     CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))
    --
    --                 else
    --                     tile
    --
    --             _ ->
    --                 tile
    --     )
    --     updatedChessBoard
    revealPathsInBoard tileAttributes validPathIndexes updatedChessBoard


getQueenValidPathIndexes : ChessBoard msg -> Int -> List Int
getQueenValidPathIndexes chessBoard index =
    let
        preparePaths =
            filterOutBlockedPath chessBoard
    in
    preparePaths (getIndexesRelativeToPosition North index)
        ++ preparePaths (getIndexesRelativeToPosition South index)
        ++ preparePaths (getIndexesRelativeToPosition West index)
        ++ preparePaths (getIndexesRelativeToPosition East index)
        ++ preparePaths (getIndexesRelativeToPosition NorthWest index)
        ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
        ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
        ++ preparePaths (getIndexesRelativeToPosition SouthEast index)


revealKingPath : Color -> BoardTileAttributes -> CastlingPrerequisites -> ChessBoard msg -> ChessBoard msg
revealKingPath playerColor ({ index } as tileAttributes) castlingPrerequisites chessBoard =
    let
        updatedChessBoard =
            toggleSelectedPiece tileAttributes chessBoard

        -- preparePaths =
        --     List.take 1 << filterOutBlockedPath updatedChessBoard
        -- You cannot castle while you king is Checked.
        --
        -- You cannot castle through check.
        -- That means that if there is a threatened tile
        -- in the path of the castling then you cannot do it.
        --
        -- You cannot castle to a check position.
        -- That means that if the target tile for the King after a
        -- castle is a threatened one then you cannot perform that.
        --
        -- You cannot castle if your king has already moved.
        --
        -- You cannot castle if your rook has already moved.
        validPathIndexes =
            -- preparePaths (getIndexesRelativeToPosition North index)
            --     ++ preparePaths (getIndexesRelativeToPosition South index)
            --     ++ preparePaths (getIndexesRelativeToPosition West index)
            --     ++ preparePaths (getIndexesRelativeToPosition East index)
            --     ++ preparePaths (getIndexesRelativeToPosition NorthWest index)
            --     ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
            --     ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
            --     ++ preparePaths (getIndexesRelativeToPosition SouthEast index)
            getKingValidPathIndexes updatedChessBoard index

        -- Should you be using the updatedChessBoard here ?
        -- Check other path functions.
        -- sourceTile =
        --     -- getOccupiedTileByIndex index chessBoard
        --     getOccupiedTileByIndex index updatedChessBoard
        --
        -- emptyPathIndexes =
        --     -- List.filter (not << isOccupied chessBoard) validPathIndexes
        --     List.filter (not << isOccupied updatedChessBoard) validPathIndexes
        --
        -- capturablePathIndexes =
        --     List.filter (not << isOutOfBoundsIndex) <|
        --         -- List.filter (not << isOccupiedBySameColor tileAttributes chessBoard) validPathIndexes
        --         List.filter (not << isOccupiedBySameColor tileAttributes updatedChessBoard) validPathIndexes
    in
    -- List.map
    --     (\tile ->
    --         case tile of
    --             EmptyTile index_ ->
    --                 if List.any ((==) index_) emptyPathIndexes then
    --                     EmptyPathTile (EmptyPathTileState index_ sourceTile)
    --
    --                 else
    --                     tile
    --
    --             OccupiedTile tileState ->
    --                 let
    --                     isCapturablePath =
    --                         List.any ((==) tileState.attributes.index) capturablePathIndexes
    --                 in
    --                 if isCapturablePath then
    --                     CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))
    --
    --                 else
    --                     tile
    --
    --             _ ->
    --                 tile
    --     )
    --     updatedChessBoard
    -- revealSpecialPathsInBoard tileAttributes castlingPathIndexes <|
    revealCastlingPathsInBoard playerColor castlingPrerequisites <|
        revealPathsInBoard tileAttributes validPathIndexes updatedChessBoard


getKingValidPathIndexes : ChessBoard msg -> Int -> List Int
getKingValidPathIndexes chessBoard index =
    let
        preparePaths =
            List.take 1 << filterOutBlockedPath chessBoard
    in
    preparePaths (getIndexesRelativeToPosition North index)
        ++ preparePaths (getIndexesRelativeToPosition South index)
        ++ preparePaths (getIndexesRelativeToPosition West index)
        ++ preparePaths (getIndexesRelativeToPosition East index)
        ++ preparePaths (getIndexesRelativeToPosition NorthWest index)
        ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
        ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
        ++ preparePaths (getIndexesRelativeToPosition SouthEast index)


revealPathsInBoard : BoardTileAttributes -> List Int -> ChessBoard msg -> ChessBoard msg
revealPathsInBoard ({ index } as tileAttributes) validPathIndexes chessBoard =
    let
        sourceTile =
            getOccupiedTileByIndex chessBoard index
    in
    List.map
        (\tile ->
            case tile of
                EmptyTile index_ ->
                    let
                        emptyPathIndexes =
                            List.filter (not << isOccupied chessBoard) validPathIndexes
                    in
                    if List.any ((==) index_) emptyPathIndexes then
                        EmptyPathTile (EmptyPathTileState index_ sourceTile)

                    else
                        tile

                OccupiedTile tileState ->
                    let
                        isValidIndex index =
                            not (isOutOfBoundsIndex index)
                                && not (isOccupiedBySameColor tileAttributes chessBoard index)

                        capturablePathIndexes =
                            List.filter isValidIndex validPathIndexes

                        isCapturablePath =
                            List.any ((==) tileState.attributes.index) capturablePathIndexes
                    in
                    if isCapturablePath then
                        CapturablePathTile (CapturableTileState tileState.attributes.index sourceTile (OccupiedTile tileState))

                    else
                        tile

                _ ->
                    tile
        )
        chessBoard



{- Finish with that.
   -- You need to check if the king or the rooks have moved
   -- and if they haven't and all the criteria are met you would need to
   -- check if any of the itermediate tiles are being threatened.
   -- If they are not threatened then we can design the special movement
   -- tile.
   TODO: Also check if any of the path indexes are threatened.
-}


revealCastlingPathsInBoard : Color -> CastlingPrerequisites -> ChessBoard msg -> ChessBoard msg
revealCastlingPathsInBoard playerColor { queenRookMoved, kingRookMoved, kingMoved } chessBoard =
    let
        ( queenCastlingPathIndexes, kingCastlingPathIndexes ) =
            ( [ 57, 58, 59 ]
            , [ 61, 62 ]
            )

        isPathToRookClear =
            List.all ((==) Nothing) << List.map (getOccupiedTileByIndex chessBoard)

        ( isQueenCastlingAvailable, isKingCastlingAvailable ) =
            ( if kingMoved || queenRookMoved then
                False

              else
                isPathToRookClear queenCastlingPathIndexes
            , if kingMoved || kingRookMoved then
                False

              else
                isPathToRookClear kingCastlingPathIndexes
            )

        ( queenPaths, kingPaths ) =
            ( if isQueenCastlingAvailable then
                [ 58 ]

              else
                []
            , if isKingCastlingAvailable then
                [ 62 ]

              else
                []
            )

        _ =
            List.map (isTileThreatened playerColor chessBoard) queenCastlingPathIndexes
    in
    List.map
        (\tile ->
            case tile of
                EmptyTile index ->
                    if List.any ((==) index) queenPaths then
                        CastlingPathTile (CastlingTileState QueenSideCastling index)

                    else if List.any ((==) index) kingPaths then
                        CastlingPathTile (CastlingTileState KingSideCastling index)

                    else
                        tile

                _ ->
                    tile
        )
        chessBoard



{- TODO: Implement this one. Its going to be tricky I guess. -}


isTileThreatened : Color -> ChessBoard msg -> Int -> Bool
isTileThreatened color chessBoard index =
    let
        possibleThreatsIndex =
            let
                preparePaths =
                    filterOutBlockedPath chessBoard
            in
            preparePaths (getIndexesRelativeToPosition North index)
                ++ preparePaths (getIndexesRelativeToPosition South index)
                ++ preparePaths (getIndexesRelativeToPosition West index)
                ++ preparePaths (getIndexesRelativeToPosition East index)
                ++ preparePaths (getIndexesRelativeToPosition NorthWest index)
                ++ preparePaths (getIndexesRelativeToPosition NorthEast index)
                ++ preparePaths (getIndexesRelativeToPosition SouthWest index)
                ++ preparePaths (getIndexesRelativeToPosition SouthEast index)

        -- isThreatened =
        --     List.foldl
        --         (\tile result ->
        --             if result then
        --                 result
        --             else
        --                 case tile of
        --                     OccupiedTile tileState ->
        --                         let
        --                             { attributes } =
        --                                 tileState
        --                         in
        --                         if attributes.color
        --                         False
        --
        --                     _ ->
        --                         False
        --
        --         )
        --         False
        --         chessBoard
        _ =
            Debug.log "possibleThreatsIndex" <| List.sort possibleThreatsIndex

        _ =
            Debug.log "index" index

        _ =
            Debug.log "" "_________________________"
    in
    -- case color of
    --     White ->
    --         let
    --             updatedChessBoard =
    --                 List.foldl
    --                     (\tile result ->
    --                         if result then
    --                             result
    --
    --                         else
    --                             case tile of
    --                                 OccupiedTile tileState ->
    --                                     let
    --                                         { attributes } =
    --                                             tileState
    --
    --                                         -- type alias BoardTileAttributes =
    --                                         --     { index : Int
    --                                         --     , color : Color
    --                                         --     , type_ : ChessPieceType
    --                                         --     , isToggled : Bool
    --                                         --     }
    --                                     in
    --                                     if attributes.color /= color then
    --                                         case attributes.type_ of
    --                                             Pawn ->
    --                                                 let
    --                                                     capturablePaths =
    --                                                         getPawnCapturablePath attributes chessBoard attributes.index
    --
    --                                                     _ =
    --                                                         Debug.log "capturablePaths" capturablePaths
    --                                                 in
    --                                                 False
    --
    --                                             _ ->
    --                                                 False
    --
    --                                     else
    --                                         False
    --
    --                                 _ ->
    --                                     False
    --                     )
    --                     False
    --                     chessBoard
    --         in
    --         False
    --
    --     Black ->
    --         False
    False


clearRevealedPaths : ChessBoard msg -> ChessBoard msg
clearRevealedPaths chessBoard =
    List.map
        (\tile ->
            case tile of
                EmptyPathTile { targetIndex } ->
                    EmptyTile targetIndex

                OccupiedTile tileState ->
                    let
                        updatedAttributes =
                            updateIsToggled False tileState.attributes

                        updatedAction =
                            tileState.actionConstructor updatedAttributes
                    in
                    OccupiedTile (BoardTileState updatedAttributes tileState.actionConstructor updatedAction)

                EmptyTile index ->
                    tile

                CapturablePathTile tileState ->
                    tileState.previousState

                CastlingPathTile { index } ->
                    EmptyTile index
        )
        chessBoard


movePiece : { a | targetIndex : Int, sourceTile : Maybe (BoardTile msg) } -> ChessBoard msg -> ChessBoard msg
movePiece { targetIndex, sourceTile } chessBoard =
    case sourceTile of
        Just sourceTile_ ->
            let
                sourceTileIndex =
                    getTileIndex sourceTile_
            in
            List.map
                (\tile ->
                    let
                        tileIndex =
                            getTileIndex tile
                    in
                    if tileIndex == targetIndex then
                        case sourceTile_ of
                            OccupiedTile tileState ->
                                let
                                    updatedAttributes =
                                        updateIndex targetIndex <|
                                            updateIsToggled False tileState.attributes

                                    updatedAction =
                                        tileState.actionConstructor updatedAttributes
                                in
                                OccupiedTile (BoardTileState updatedAttributes tileState.actionConstructor updatedAction)

                            _ ->
                                tile

                    else if tileIndex == sourceTileIndex then
                        EmptyTile tileIndex

                    else
                        tile
                )
                (clearRevealedPaths chessBoard)

        Nothing ->
            clearRevealedPaths chessBoard


toggleSelectedPiece : BoardTileAttributes -> ChessBoard msg -> ChessBoard msg
toggleSelectedPiece tileAttributes chessBoard =
    List.map
        (\tile ->
            case tile of
                OccupiedTile tileState ->
                    if tileState.attributes.index == tileAttributes.index then
                        let
                            updatedAttributes =
                                updateIsToggled True tileState.attributes

                            updatedAction =
                                tileState.actionConstructor updatedAttributes
                        in
                        OccupiedTile (BoardTileState updatedAttributes tileState.actionConstructor updatedAction)

                    else
                        tile

                _ ->
                    tile
        )
        chessBoard


getTileIndex : BoardTile msg -> Int
getTileIndex tile =
    case tile of
        OccupiedTile tileState ->
            tileState.attributes.index

        EmptyTile index ->
            index

        EmptyPathTile { targetIndex } ->
            targetIndex

        CapturablePathTile { targetIndex } ->
            targetIndex

        -- TODO: Check out this case later.
        CastlingPathTile { index } ->
            index


getOccupiedTileByIndex : ChessBoard msg -> Int -> Maybe (BoardTile msg)
getOccupiedTileByIndex chessBoard index =
    List.foldl
        (\tile result ->
            if result /= Nothing then
                result

            else
                case tile of
                    OccupiedTile tileState ->
                        if tileState.attributes.index == index then
                            Just tile

                        else
                            Nothing

                    _ ->
                        Nothing
        )
        Nothing
        chessBoard


isOccupied : ChessBoard msg -> Int -> Bool
isOccupied chessBoard targetIndex =
    List.foldl
        (\tile r ->
            if r then
                True

            else
                case tile of
                    OccupiedTile tileState ->
                        tileState.attributes.index == targetIndex

                    _ ->
                        False
        )
        False
        chessBoard


isOccupiedBySameColor : BoardTileAttributes -> ChessBoard msg -> Int -> Bool
isOccupiedBySameColor sourceTileAttributes chessBoard targetIndex =
    List.foldl
        (\tile r ->
            if r then
                True

            else
                case tile of
                    OccupiedTile { attributes } ->
                        attributes.index == targetIndex && attributes.color == sourceTileAttributes.color

                    _ ->
                        False
        )
        False
        chessBoard


boardPiecesList : BoardSide -> List ChessPieceType
boardPiecesList boardSide =
    case boardSide of
        Top ->
            [ Rook
            , Knight
            , Bishop
            , Queen
            , King
            , Bishop
            , Knight
            , Rook
            ]
                ++ List.repeat 8 Pawn

        Bottom ->
            List.repeat 8 Pawn
                ++ [ Rook
                   , Knight
                   , Bishop
                   , Queen
                   , King
                   , Bishop
                   , Knight
                   , Rook
                   ]


isInFirstColumn : Int -> Bool
isInFirstColumn index =
    List.any ((==) index) [ 0, 8, 16, 24, 32, 40, 48, 56 ]


isInSecondColumn : Int -> Bool
isInSecondColumn index =
    List.any ((==) index) [ 1, 9, 17, 25, 33, 41, 49, 57 ]


isInThirdColumn : Int -> Bool
isInThirdColumn index =
    List.any ((==) index) [ 2, 10, 18, 26, 34, 42, 50, 58 ]


isInFourthColumn : Int -> Bool
isInFourthColumn index =
    List.any ((==) index) [ 3, 11, 19, 27, 35, 43, 51, 59 ]


isInFifthColumn : Int -> Bool
isInFifthColumn index =
    List.any ((==) index) [ 4, 12, 20, 28, 36, 44, 52, 60 ]


isInSixthColumn : Int -> Bool
isInSixthColumn index =
    List.any ((==) index) [ 5, 13, 21, 29, 37, 45, 53, 61 ]


isInSeventhColumn : Int -> Bool
isInSeventhColumn index =
    List.any ((==) index) [ 6, 14, 22, 30, 38, 46, 54, 62 ]


isInEighthColumn : Int -> Bool
isInEighthColumn index =
    List.any ((==) index) [ 7, 15, 23, 31, 39, 47, 55, 63 ]


isOutOfBoundsIndex : Int -> Bool
isOutOfBoundsIndex index =
    index < 0 || index > 63


getRowIndex : Int -> Int
getRowIndex index =
    index // 8


getColumnIndex : Int -> Int
getColumnIndex index =
    index % 8


getIndexesRelativeToPosition : Direction -> Int -> List Int
getIndexesRelativeToPosition direction index =
    let
        ( multiplesOfSeven, multiplesOfNine ) =
            ( [ 7, 14, 21, 28, 35, 42, 49 ]
            , [ 9, 18, 27, 36, 45, 54, 63 ]
            )

        subtractFromIndex indexes =
            List.map ((-) index) indexes

        addToIndex indexes =
            List.map ((+) index) indexes

        filterInvalidPaths =
            List.filter (not << isOutOfBoundsIndex)
    in
    case direction of
        North ->
            filterInvalidPaths
                [ index - 8
                , index - 16
                , index - 24
                , index - 32
                , index - 40
                , index - 48
                , index - 56
                ]

        South ->
            filterInvalidPaths
                [ index + 8
                , index + 16
                , index + 24
                , index + 32
                , index + 40
                , index + 48
                , index + 56
                ]

        West ->
            List.filter ((==) (getRowIndex index) << getRowIndex)
                [ index - 1
                , index - 2
                , index - 3
                , index - 4
                , index - 5
                , index - 6
                , index - 7
                ]

        East ->
            List.filter ((==) (getRowIndex index) << getRowIndex)
                [ index + 1
                , index + 2
                , index + 3
                , index + 4
                , index + 5
                , index + 6
                , index + 7
                ]

        NorthWest ->
            filterInvalidPaths (subtractFromIndex (List.take (getColumnIndex index) multiplesOfNine))

        NorthEast ->
            filterInvalidPaths (subtractFromIndex (List.take (7 - getColumnIndex index) multiplesOfSeven))

        SouthWest ->
            filterInvalidPaths (addToIndex (List.take (getColumnIndex index) multiplesOfSeven))

        SouthEast ->
            filterInvalidPaths (addToIndex (List.take (7 - getColumnIndex index) multiplesOfNine))


filterOutBlockedPath : ChessBoard msg -> List Int -> List Int
filterOutBlockedPath chessBoard pathIndexes =
    let
        ( validPathIndexes, _ ) =
            List.foldl
                (\index ( result, shouldContinue ) ->
                    if shouldContinue && isOccupied chessBoard index then
                        ( result ++ [ index ]
                        , False
                        )

                    else if shouldContinue then
                        ( result ++ [ index ]
                        , shouldContinue
                        )

                    else
                        ( result
                        , shouldContinue
                        )
                )
                ( [], True )
                pathIndexes
    in
    validPathIndexes
