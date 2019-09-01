module Book.MarkdownExtra exposing (view)

import Common.Utility as Utility
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown.Elm
import Option exposing (..)


view : Int -> String -> Html msg
view k markdownString =
    markdownString
        |> Markdown.Elm.toHtml ExtendedMath
