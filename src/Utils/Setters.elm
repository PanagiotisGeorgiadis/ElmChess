module Utils.Setters exposing (..)


updateIsToggled : a -> { b | isToggled : a } -> { b | isToggled : a }
updateIsToggled val root =
    { root | isToggled = val }


updateIndex : a -> { b | index : a } -> { b | index : a }
updateIndex val root =
    { root | index = val }


updateQueenRookMoved : a -> { b | queenRookMoved : a } -> { b | queenRookMoved : a }
updateQueenRookMoved val root =
    { root | queenRookMoved = val }


updateKingRookMoved : a -> { b | kingRookMoved : a } -> { b | kingRookMoved : a }
updateKingRookMoved val root =
    { root | kingRookMoved = val }


updateKingMoved : a -> { b | kingMoved : a } -> { b | kingMoved : a }
updateKingMoved val root =
    { root | kingMoved = val }
