module Common.Book exposing (notesViewedAsMarkdown)

import Book.MarkdownExtra as MarkdownExtra
import Book.Types exposing (Book)
import Element exposing (..)
import Html exposing (..)
import Html.Attributes as HA


notesViewedAsMarkdown : String -> String -> Maybe Book -> Element msg
notesViewedAsMarkdown w h currentBook =
    case currentBook of
        Nothing ->
            Element.none

        Just book ->
            Element.html <| Html.div (markdownStyle w h) <| [ MarkdownExtra.view book.notes ]


markdownStyle w h =
    [ HA.style "height" h
    , HA.style "width" w
    , HA.style "font-size" "12px"
    , HA.style "line-height" "15px"
    , HA.style "overflow-y" "scroll"
    , HA.style "overflow-x" "hidden"
    , HA.style "padding-top" "20px"
    , HA.style "background-color" "#f7f6f4"
    , HA.style "padding-left" "15px"
    ]
