module Pages.About exposing
    ( AppState(..)
    , Model
    , Msg(..)
    , init
    , update
    , view
    )

import Common.Book
import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { appState : AppState
    }


type AppState
    = Default
    | MarkDown
    | Screenshot


init : Model
init =
    { appState = Default
    }


type Msg
    = NoOp String
    | SetStateDefault
    | SetStateMarkdown
    | SetStateScreenshot


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp str ->
            ( model, Cmd.none, NoUpdate )

        SetStateDefault ->
            ( { model | appState = Default }, Cmd.none, NoUpdate )

        SetStateMarkdown ->
            ( { model | appState = MarkDown }, Cmd.none, NoUpdate )

        SetStateScreenshot ->
            ( { model | appState = Screenshot }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    case model.appState of
        Default ->
            mainView sharedState model

        MarkDown ->
            markDownView sharedState model

        Screenshot ->
            screenShotView sharedState model


mainView sharedState model =
    row [ alignTop, spacing 10 ]
        [ image []
            { src = "https://www.hastac.org/sites/default/files/upload/images/post/books.jpg"
            , description = "Library"
            }
        , textColumn sharedState model
        ]


markDownView sharedState model =
    row [ spacing 12, paddingXY 20 40 ]
        [ aboutMarkDown sharedState model
        , textView markDownExample
        , column [ spacing 12 ]
            [ Common.Book.textViewedAsMarkdown 70 "400px" "500px" markDownExample
            ]
        ]


screenShotView sharedState model =
    row [ spacing 12, paddingXY 0 8 ]
        [ row [ padding 10, Border.width 1 ]
            [ image []
                { src = "http://noteimages.s3.amazonaws.com/app_images/booklib_screenshot2.png"
                , description = "BookLib screenshot"
                }
            ]
        , setStateDefaultButton
        ]


textView text_ =
    Input.multiline (textInputStyle (px 350) (px 520))
        { onChange = NoOp
        , text = text_
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 0, Font.bold ] (text "")
        , spellcheck = False
        }


textInputStyle w h =
    [ Style.preWrap
    , height h
    , width w
    , scrollbarY
    , clipX
    , Font.size 13
    , paddingXY 8 20
    , Background.color Style.lightGrey
    , Border.width 2
    ]


setStateDefaultButton =
    Input.button Style.button
        { onPress = Just SetStateDefault
        , label = Element.text "Back"
        }


setStateMarkdownButton =
    Input.button Style.button
        { onPress = Just SetStateMarkdown
        , label = Element.text "More about markdown ..."
        }


setStateScreenShotButton =
    Input.button Style.button
        { onPress = Just SetStateScreenshot
        , label = Element.text "BookLib Screenshot"
        }


textColumn sharedState model =
    column (Style.mainColumn fill fill ++ [ width (px 600), padding 50, Font.size 14 ])
        [ el [ Font.bold, Font.size 18 ] (text "About Booklib.io")
        , paragraph []
            [ text
                """With Booklib.io, you can keep a running list of the books you are
                reading and have read, along with a progress bar, page counts and your personal notes —
                press the "BookLib Screenshot" button below to see one user's list.

            """
            ]
        , paragraph
            []
            [ text """By default, your list and notes are entirely private.  However, if you wish, you can share
            selected books with the Booklib community.  If you are signed in, click on the "Shared books" tab to see
            what other users have shared.""" ]
        , paragraph
            []
            [ text """We will be adding a few new features as things progress, but our
            overarching goal is to keep things simple, useful, and enjoyable.
             Contact me at jxxcarlson@gmail.com with your ideas.""" ]
        , el [ Font.bold ] (text "Markdown")
        , paragraph
            []
            [ text """If you know about markdown, you can use it to write your notes.
                      If you don't, plain old text works just as well."""
            ]
        , row [ spacing 12 ] [ setStateMarkdownButton, setStateScreenShotButton ]
        ]


aboutMarkDown sharedState model =
    column [ spacing 24, width fill, height fill, width (px 400), height (px 500), Font.size 14 ]
        [ el [ Font.bold, Font.size 18 ] (text "About Markdown")
        , paragraph []
            [ text
                """Markdown gives you a simple way to format your text. For example,
                to make a heading, just put a hashmark (#) followed by a space in
                front of the title. Compare with the example on the right.  You can also add
                images by following the pattern ![Description](web-address).  The web address
                of an image is found by pressing control, then right-clicking
                on it.
            """
            ]
        , paragraph
            []
            [ text """If you scroll down below the Toucan image, you see how a
            link to another web page is made. Just Imitate  the pattern
            [Description](web-address).  You also see the subheading
            "Distribution and Habitat". Subheadings are made by like headings, except
            that you say ## in place of #.
            """ ]
        , paragraph
            []
            [ text """Next, we see a line with one word in bold, another in italic. To make a piece
              of text bold, put a double asterisk (**) immediately before and after the text.  To make
             word italic, put an asterisk (*) before and after.""" ]
        , paragraph
            []
            [ text """Finally, we see how to make a bulleted list.  Just preceded each item by a
            hyphen (-), followed by a space.""" ]
        , newTabLink [ Font.color Style.blue ]
            { url = "https://www.markdownguide.org/cheat-sheet/"
            , label = el [ Font.size 12 ] (text "More about markdown")
            }
        , setStateDefaultButton
        ]


markDownExample : String
markDownExample =
    """# On Birds

These are my notes on birds.  I am using markdown to
format them, add images, and add links to other web
pages.  Keep scrolling down in this window to se how
markdown is used. Compare what you see here with
what you see in the middle window.  There is more to
read below the Toucan!.

![Toucan](https://upload.wikimedia.org/wikipedia/commons/thumb/4/43/Ramphastos_sulfuratus_-Belize_Zoo-6a-2c.jpg/440px-Ramphastos_sulfuratus_-Belize_Zoo-6a-2c.jpg)

[Wikipedia artlce on Toucans](https://en.wikipedia.org/wiki/Keel-billed_toucan)



## Distribution and Habitat

The keel-billed toucan can be found from Southern
Mexico to Venezuela and Colombia. It roosts in
the canopies of tropical, subtropical, and
lowland rain forests, up to altitudes of
1,900 m (6,200 ft). It roosts in holes in
trees, often with several other toucans.
This can be very cramped, so the birds tuck their tails
and beaks under their bodies to conserve space while
sleeping. Adding to the lack of space, the
bottoms of the holes are often covered with pits
from the fruit the toucans have eaten.

**Conservation status:** Least Concern (*Population decreasing*)


## Habitat loss

The threats leading to population declines in birds are
many and varied:

- agriculture
- logging
- invasive species

are the most severe.

[Source](http://datazone.birdlife.org/sowb/casestudy/a-range-of-threats-drives-declines-in-bird-populations)

"""
