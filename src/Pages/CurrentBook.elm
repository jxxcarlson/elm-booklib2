module Pages.CurrentBook exposing (Model, Msg(..), init, update, view)

import Http
import Html
import Html.Attributes as HA
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (required, hardcoded)
import Markdown
import Book.Types exposing (Book)
import Book.MarkdownExtra as MarkdownExtra
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Common.Style as Style
import Common.Utility as Utility
import Common.Indicator as Indicator
import Common.Days as Days
import Common.Book
import User.Types exposing (User)
import Configuration


type alias Model =
    { pagesRead : String }


type Msg
    = NoOp
    | ToggleBookPublic Bool
    | InputPagesRead String


init : Model
init =
    { pagesRead = "" }



--
-- UPDATE
--


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        InputPagesRead str ->
            ( model, Cmd.none, NoUpdate )

        ToggleBookPublic val ->
            case sharedState.currentBook of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just book ->
                    let
                        nextBook =
                            { book | public = not book.public }

                        -- command =
                        --     updateBook nextBook (userToken sharedState)
                        -- updatedBookList =
                        --     Utility.replaceIf (\runningBook -> runningBook.id == nextBook.id) nextBook model.bookList
                    in
                        ( model
                        , Cmd.none
                        , SharedState.UpdateCurrentBook (Just nextBook)
                        )



--
-- VIEW
--


view : SharedState -> Model -> Element Msg
view sharedState model =
    row (Style.mainColumn fill fill)
        [ currentBookPanel sharedState model
        , Common.Book.notesViewedAsMarkdown sharedState.currentBook
        ]


currentBookPanel : SharedState -> Model -> Element Msg
currentBookPanel sharedState model =
    case sharedState.currentBook of
        Nothing ->
            el [] (text "No book selected")

        Just book ->
            column ([ width (px 360), height (px 314), spacing 12 ] ++ Style.panel)
                [ column [ spacing 8 ]
                    [ el strongFieldStyle (text <| book.title)
                    , el fieldStyle (text <| book.subtitle)
                    , el fieldStyle (text <| book.category)
                    , el [ clipX ] (text <| "by " ++ book.author)
                    , publicCheckbox book
                    ]
                , el [ moveUp 0, width (px 300) ] (Indicator.indicator 300 15 "orange" (pageRatio book))
                , el [ moveUp 15, Font.color Style.blue, Font.bold, paddingXY 0 10 ] (text <| pageInfo book)
                , row [ spacing 15 ]
                    [ pagesInput sharedState model
                    , row [ spacing 5 ]
                        [ el [ Font.size 14, moveDown 10 ] (text <| startMessage book)
                        , el [ Font.size 14, moveDown 10 ] (text "==>")
                        , el [ Font.size 14, moveDown 10 ] (text <| finishMessage book)
                        ]
                    ]
                , readingRateDisplay sharedState book
                , row [ spacing 20, moveUp 10 ] [ editBookButton, currentDateDisplay sharedState, newBookButton ]
                ]


icon : Bool -> Element msg
icon status =
    case status of
        True ->
            el [ Font.size 14, Font.bold, moveDown 1 ] (text "Y")

        False ->
            el [ Font.size 14, Font.bold, moveDown 1 ] (text "N")


strongFieldStyle =
    [ Font.bold, width (px 300), clipX ]


fieldStyle =
    [ Font.size 14, width (px 300), clipX ]


pageRatio : Book -> Float
pageRatio book =
    (toFloat book.pagesRead) / (toFloat book.pages)


pageInfo book =
    let
        pp =
            (String.fromInt book.pagesRead) ++ "/" ++ (String.fromInt book.pages)

        pc =
            String.fromInt <| Basics.round <| 100 * (Basics.toFloat book.pagesRead) / (Basics.toFloat book.pages)
    in
        pp ++ " (" ++ pc ++ "%)"


pagesInput : SharedState -> Model -> Element Msg
pagesInput sharedState model =
    case sharedState.currentBook of
        Nothing ->
            Element.none

        Just book ->
            Input.text [ width (px 60), height (px 25) ]
                { text = String.fromInt book.pagesRead
                , placeholder = Nothing
                , onChange = InputPagesRead
                , label = Input.labelAbove [ Font.size 14 ] (text "Pages read")
                }


startMessage : Book -> String
startMessage book =
    case book.startDateString == "" of
        True ->
            "Start date"

        False ->
            book.startDateString


finishMessage : Book -> String
finishMessage book =
    case book.finishDateString == "" of
        True ->
            "Finish date"

        False ->
            book.finishDateString


readingRateDisplay : SharedState -> Book -> Element msg
readingRateDisplay sharedState book =
    case ( book.startDateString /= "", book.finishDateString /= "" ) of
        ( True, _ ) ->
            Element.el [ Font.size 14 ] (Element.text <| readingRateString sharedState book)

        ( _, _ ) ->
            Element.el [ Font.size 14 ] (Element.text <| "")


readingRateString : SharedState -> Book -> String
readingRateString sharedState book =
    (String.fromInt <| Basics.round <| readingRate sharedState book) ++ " pp/day (" ++ (String.fromInt <| daysToComplete sharedState book) ++ " days)"


daysToComplete : SharedState -> Book -> Int
daysToComplete sharedState book =
    case book.finishDateString /= "" of
        True ->
            Days.fromUSDate book.startDateString book.finishDateString

        False ->
            Days.fromUSDate book.startDateString (Utility.toUtcDateString <| Just sharedState.currentTime)


readingRate : SharedState -> Book -> Float
readingRate shareState book =
    (Basics.toFloat book.pagesRead) / (Basics.toFloat <| daysToComplete shareState book)


newBookButton : Element Msg
newBookButton =
    Input.button Style.smallButton
        { onPress = Just (NoOp) -- NewBook
        , label = Element.text "New book"
        }


editBookButton =
    Input.button (Style.buttonWithWidth 50)
        { onPress = Just NoOp -- EditBook
        , label = Element.text "Edit"
        }


currentDateDisplay : SharedState -> Element msg
currentDateDisplay sharedState =
    Element.el [ Font.size 12, moveRight 15, moveDown 1 ] (Element.text <| Utility.toUtcDateString <| Just sharedState.currentTime)


publicCheckbox : Book -> Element Msg
publicCheckbox book =
    Input.checkbox
        []
        { onChange = ToggleBookPublic
        , icon = icon
        , checked = book.public
        , label = Input.labelLeft [ Font.size 14, moveDown 1 ] (text "Share book:")
        }


bookTitle : Maybe Book -> String
bookTitle book_ =
    case book_ of
        Nothing ->
            "No book"

        Just book ->
            book.title