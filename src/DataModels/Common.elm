module DataModels.Common exposing (..)


type PlayerType
    = WhitePlayer
    | BlackPlayer


type alias Position =
    { x : Int
    , y : BoardLetter
    }


type alias MovementInstructions =
    { position : Position
    , isCapturableMove : Bool
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
    , isThreatened : Bool
    , index : Int
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


type BoardTileMsg
    = NoOp
    | RevealPawnMovement Int
    | MovePawn Position
    | RevealKnightMovement Int
    | MoveKnight Position
    | RevealBishopMovement Int
    | MoveBishop Position
    | RevealRookMovement Int
    | MoveRook Position
    | RevealKingMovement Int
    | MoveKing Position
    | RevealQueenMovement Int
    | MoveQueen Position


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


rowsIndexList : List Int
rowsIndexList =
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


incrementBoardLetter : BoardLetter -> Maybe BoardLetter
incrementBoardLetter letter =
    if letter == A then
        Just B
    else if letter == B then
        Just C
    else if letter == C then
        Just D
    else if letter == D then
        Just E
    else if letter == E then
        Just F
    else if letter == F then
        Just G
    else if letter == G then
        Just H
    else
        Nothing


decrementBoardLetter : BoardLetter -> Maybe BoardLetter
decrementBoardLetter letter =
    if letter == A then
        Nothing
    else if letter == B then
        Just A
    else if letter == C then
        Just B
    else if letter == D then
        Just C
    else if letter == E then
        Just D
    else if letter == F then
        Just E
    else if letter == G then
        Just F
    else
        Just G
