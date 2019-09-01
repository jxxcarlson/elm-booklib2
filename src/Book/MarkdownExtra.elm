module Book.MarkdownExtra exposing (view)

import Common.Utility as Utility
import Html exposing (Html)
import Html.Attributes as HA exposing (style)
import Markdown.Elm
import Option exposing (..)


view : Int -> String -> Html msg
view k markdownString =
    markdownString
        |> Markdown.Elm.toHtml Extended
        |> (\x -> Html.div [] [ x ])


renderedSourceStyle width height color =
    [ style "width" width -- "500px
    , style "height" height -- "450px"
    , style "padding" "15px"
    , style "background-color" color -- "#fff"
    , style "overflow-y" "scroll"

    -- , style "float" "left"
    ]
