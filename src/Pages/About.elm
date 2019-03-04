module Pages.About exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { foo : String
    }


init : Model
init =
    { foo = "bar"
    }


type Msg
    = NoOp


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element msg
view sharedState model =
    row [ alignTop, spacing 10 ]
        [ image []
            { src = "https://www.hastac.org/sites/default/files/upload/images/post/books.jpg"
            , description = "Library"
            }
        , textColumn sharedState model
        ]


textColumn sharedState model =
    column (Style.mainColumn fill fill ++ [ width (px 600), padding 50, Font.size 14 ])
        [ el [ Font.bold, Font.size 18 ] (text "About Booklib.io")
        , paragraph []
            [ text
                """Booklib.io is devoted to the idea that reading books is a good thing ... 
            and is even better if we share our reading with others.  You will enjoy looking
            back at your book list and notes some years hence, and you will be surprised at how much you have read.
            """
            ]
        , paragraph
            []
            [ text """Consider joining BookLib, sharing your reading list with others,
            and writing up a few notes on what you read.  Notes don't have to be long to be
            meaningful — just a paragraph or two can do.""" ]
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
        , newTabLink [ Font.color Style.blue ] { url = "https://www.markdownguide.org/cheat-sheet/", label = el [] (text "Using Markdown") }
        ]
