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
    if index % 8 == 0 then
        { x = (index // 8) - 1
        , y = H
        }
    else
        { x = index // 8
        , y = getLetterFromNumber (index % 8)
        }
