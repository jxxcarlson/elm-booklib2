module Book.MarkdownExtra  exposing(view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))
import Markdown.Config exposing (Options, defaultOptions)
import Utility


view : String -> Html msg
view markdownString =
    markdownString
        |> Utility.softBreakAlt 70
        |> String.join("\n")
        |> Block.parse (Nothing) -- See answer 2 why
        |> List.map (customHtmlBlock)
        |> List.concat
        |> div []


view1 : String -> Html msg
view1 markdownString =
    markdownString
        |> Utility.softBreakAlt 55
        |> String.join("\n")
        |> Block.parse (Just customOptions) -- See answer 2 why
        |> List.map (customHtmlBlock)
        |> List.concat
        |> div []

customHtmlBlock : Block b i -> List (Html msg)
customHtmlBlock block =
    Block.defaultHtml
        (Just customHtmlBlock)
        (Just customHtmlInline)
        block


customHtmlInline : Inline i -> Html msg
customHtmlInline inline =
    case inline of
        Link url maybeTitle inlines ->
            if String.startsWith "http://elm-lang-yada.org" url then
                a [ href url
                  , title (Maybe.withDefault "" maybeTitle)
                  ] (List.map customHtmlInline inlines)

            else
                a [ href url
                  , title (Maybe.withDefault "" maybeTitle)
                  , target "_blank"
                  , rel "noopener noreferrer"
                  ] (List.map customHtmlInline inlines)


        _ ->
            Inline.defaultHtml (Just customHtmlInline) inline

customOptions : Options
customOptions =
    { defaultOptions
        | softAsHardLineBreak = True
    }