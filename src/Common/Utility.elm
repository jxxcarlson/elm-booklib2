module Common.Utility exposing
    ( replaceIf
    , softBreakAlt
    , toUtcDateString
    , toUtcString
    , toggleList
    )

import Regex
import Time exposing (Month(..), Posix)


toUtcString : Maybe Posix -> String
toUtcString maybeTime =
    case maybeTime of
        Nothing ->
            "--:--:--"

        Just time ->
            (String.fromInt (Time.toHour Time.utc time) |> String.padLeft 2 '0')
                ++ ":"
                ++ (String.fromInt (Time.toMinute Time.utc time) |> String.padLeft 2 '0')



-- ++ ":"
-- ++ (String.fromInt (Time.toSecond Time.utc time) |> String.padLeft 2 '0')


toUtcDateString : Maybe Posix -> String
toUtcDateString maybeTime =
    case maybeTime of
        Nothing ->
            "--/--/--"

        Just time ->
            String.fromInt (monthToInt <| Time.toMonth Time.utc time)
                ++ "/"
                ++ String.fromInt (Time.toDay Time.utc time)
                ++ "/"
                ++ String.fromInt (Time.toYear Time.utc time)


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


{-| Replace all values that satisfy a predicate with a replacement value.
-}
replaceIf : (a -> Bool) -> a -> List a -> List a
replaceIf predicate replacement list =
    updateIf predicate (always replacement) list


{-| Replace all values that satisfy a predicate by calling an update function.
-}
updateIf : (a -> Bool) -> (a -> a) -> List a -> List a
updateIf predicate update list =
    List.map
        (\item ->
            if predicate item then
                update item

            else
                item
        )
        list


{-| Remove the first occurrence of an element of a list
-}
remove : a -> List a -> List a
remove x xs =
    case xs of
        [] ->
            []

        y :: ys ->
            if x == y then
                ys

            else
                y :: remove x ys


{-| toggleList a list removes a from list if a is in the list.
If it is not in the list, then it prepends it
-}
toggleList : a -> List a -> List a
toggleList x xs =
    if List.member x xs then
        remove x xs

    else
        x :: xs


softBreak : Int -> String -> List String
softBreak width string =
    if width <= 0 then
        []

    else
        string
            |> Regex.find (softBreakRegexp width)
            |> List.map .match



-- softBreakLongLines : Int -> String -> List String
-- softBreakLongLines width string =
--   string
--     |> String.lines
--     |> String.


softBreakAltAux : Int -> String -> List String
softBreakAltAux width string =
    if String.length string < width then
        [ string ]

    else
        softBreak width string


softBreakAlt : Int -> String -> List String
softBreakAlt width string =
    string
        |> String.lines
        |> List.map (softBreakAltAux width)
        |> flattenListList


flattenListList : List (List String) -> List String
flattenListList stringListList =
    List.map flattenList stringListList


flattenList : List String -> String
flattenList stringList =
    let
        n =
            List.length stringList
    in
    if n < 2 then
        List.head stringList |> Maybe.withDefault ""

    else
        stringList |> String.join "\n"


softBreakRegexp : Int -> Regex.Regex
softBreakRegexp width =
    Maybe.withDefault Regex.never <|
        Regex.fromString (".{1," ++ String.fromInt width ++ "}(\\s+|$)|\\S+?(\\s+|$)")
