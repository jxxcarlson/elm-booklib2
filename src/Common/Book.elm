module Common.Book exposing (notesViewedAsMarkdown, textViewedAsMarkdown, verbatimTextView)

import Book.MarkdownExtra as MarkdownExtra
import Book.Types exposing (Book)
import Element exposing (Element, column, el, height, px, width)
import Element.Font as Font
import Html exposing (..)
import Html.Attributes as HA


notesViewedAsMarkdown : Int -> String -> String -> Maybe Book -> Element msg
notesViewedAsMarkdown k w h currentBook =
    case currentBook of
        Nothing ->
            Element.none

        Just book ->
            Element.html <| Html.div (markdownStyle w h) <| [ MarkdownExtra.view k book.notes ]


textViewedAsMarkdown : Int -> String -> String -> String -> Element msg
textViewedAsMarkdown k w h text_ =
    Element.html <| Html.div (markdownStyle w h) <| [ MarkdownExtra.view k text_ ]


verbatimTextView : Int -> Int -> String -> Element msg
verbatimTextView w h text_ =
    column ([ width (px w), height (px h) ] ++ [ Font.size 12 ]) [ el [] (Element.text text_) ]


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
