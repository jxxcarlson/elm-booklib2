module Common.Book exposing (notesViewedAsMarkdown)

import Html exposing (..)
import Html.Attributes as HA
import Element exposing (..)
import Markdown
import Book.MarkdownExtra as MarkdownExtra
import Book.Types exposing (Book)


notesViewedAsMarkdown : Maybe Book -> Element msg
notesViewedAsMarkdown currentBook =
    case currentBook of
        Nothing ->
            Element.none

        Just book ->
            Element.html <| Html.div (markdownStyle) <| [ MarkdownExtra.view book.notes ]


markdownStyle =
    [ HA.style "height" "630px"
    , HA.style "width" "500px"
    , HA.style "font-size" "12px"
    , HA.style "line-height" "15px"
    , HA.style "overflow-y" "scroll"
    , HA.style "overflow-x" "hidden"
    , HA.style "padding-top" "20px"
    , HA.style "background-color" "#f7f6f4"
    , HA.style "padding-left" "15px"
    ]
