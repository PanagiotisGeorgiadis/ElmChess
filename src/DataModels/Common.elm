module DataModels.Common exposing (..)


type alias Position =
    { x : Int
    , y : BoardLetter
    }


type PieceColor
    = White
    | Black


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
    List.range 1 8


totalRows : Int
totalRows =
    8


getLetterFromNumber : Int -> BoardLetter
getLetterFromNumber number =
    if number == 0 then
        A
    else if number == 1 then
        B
    else if number == 2 then
        C
    else if number == 3 then
        D
    else if number == 4 then
        E
    else if number == 5 then
        F
    else if number == 6 then
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
        - 8


getPositionFromIndex : Int -> Position
getPositionFromIndex index =
    let
        row =
            (index // 8) + 1
    in
    { x = row
    , y = getLetterFromNumber (index % 8)
    }
