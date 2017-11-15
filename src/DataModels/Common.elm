module DataModels.Common exposing (..)


type alias Position =
    { x : Int
    , y : BoardLetter
    }


type PieceColor
    = White
    | Black
    | NoColor


type BoardLetter
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type alias BoardTile msg =
    { type_ : TileType
    , position : Position
    , action : msg
    , color : PieceColor
    }


type TileType
    = PawnPiece
    | KnightPiece
    | BishopPiece
    | RookPiece
    | QueenPiece
    | KingPiece
    | EmptyPiece
    | RevealedPiece


lettersList : List BoardLetter
lettersList =
    [ A
    , B
    , C
    , D
    , E
    , F
    , G
    , H
    ]


numbersList : List Int
numbersList =
    List.range 1 totalRows


totalRows : Int
totalRows =
    8


getLetterFromNumber : Int -> BoardLetter
getLetterFromNumber number =
    if number == 1 then
        A
    else if number == 2 then
        B
    else if number == 3 then
        C
    else if number == 4 then
        D
    else if number == 5 then
        E
    else if number == 6 then
        F
    else if number == 7 then
        G
    else
        H


getStringFromBoardLetter : BoardLetter -> String
getStringFromBoardLetter letter =
    if letter == A then
        "A"
    else if letter == B then
        "B"
    else if letter == C then
        "C"
    else if letter == D then
        "D"
    else if letter == E then
        "E"
    else if letter == F then
        "F"
    else if letter == G then
        "G"
    else
        "H"


getNumberFromBoardLetter : BoardLetter -> Int
getNumberFromBoardLetter letter =
    if letter == A then
        1
    else if letter == B then
        2
    else if letter == C then
        3
    else if letter == D then
        4
    else if letter == E then
        5
    else if letter == F then
        6
    else if letter == G then
        7
    else
        8


isEven : Int -> Bool
isEven number =
    if number % 2 == 0 then
        True
    else
        False


getIndexFromPosition : Position -> Int
getIndexFromPosition position =
    (position.x * totalRows)
        + getNumberFromBoardLetter position.y


getPositionFromIndex : Int -> Position
getPositionFromIndex index =
    { x = index // 8
    , y = getLetterFromNumber (index % 8)
    }


getPrecedingBoardTilesFromPosition : List (BoardTile msg) -> Position -> List (BoardTile msg)
getPrecedingBoardTilesFromPosition boardTiles position =
    let
        index =
            getIndexFromPosition position

        precedingBoardTiles =
            List.take index boardTiles

        _ =
            Debug.log "precedingLength" <| List.length precedingBoardTiles

        _ =
            Debug.log "head" <| List.head precedingBoardTiles
    in
    precedingBoardTiles


getFollowingBoardTilesFromPosition : List (BoardTile msg) -> Position -> List (BoardTile msg)
getFollowingBoardTilesFromPosition boardTiles position =
    let
        index =
            getIndexFromPosition position

        followingBoardTiles =
            List.drop index boardTiles

        _ =
            Debug.log "followingLength" <| List.length followingBoardTiles

        _ =
            Debug.log "head" <| List.head followingBoardTiles
    in
    followingBoardTiles


getPrecedingBoardTilesFromIndex : List (BoardTile msg) -> Int -> List (BoardTile msg)
getPrecedingBoardTilesFromIndex boardTiles index =
    let
        _ =
            Debug.log "" ""
    in
    boardTiles


revealBoardTile : List (BoardTile msg) -> Position -> msg -> List (BoardTile msg)
revealBoardTile boardTiles position action =
    let
        precedingBoardTiles =
            getPrecedingBoardTilesFromPosition boardTiles position

        followingBoardTilesFromPosition =
            getFollowingBoardTilesFromPosition boardTiles position

        -- updatedBoardTiles =
        -- precedingBoardTiles
    in
    boardTiles



-- updateBoardTile : BoardTile msg -> Position -> BoardTile msg
-- updateBoardTile boardTile position action type_ =
--     let
--         _ =
--             Debug.log "boardTile" boardTile
--     in
--     boardTile
