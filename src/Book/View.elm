module Books.View exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy
import Markdown
import Books.MarkdownExtra as MD
import Html as H
import Html.Attributes as HA
import Widget
    exposing
        ( buttonStyle
        , tableHeadingStyle
        , panelStyle
        , titleButtonStyle
        )
import Utility
import Indicator
import Books.Model exposing (Books, Model, BookMsg(..), AppState(..), NotesViewMode(..), TextDisplayMode(..), TextRenderMode(..), booksCompleted)
import Books.Request as Request
import Days
import Time exposing (Posix, Month(..))


-- import User.Model
-- VIEW: TOP


view : Model -> Element BookMsg
view model =
    case ( model.maybeUser, model.appState ) of
        ( Just user, CreatingNewBook ) ->
            newBookPanel model

        ( Just user, EditingBook ) ->
            editBookPanel model

        ( Just user, ReadingMyBooks ) ->
            preBookPanel model

        ( Just user, SharingBooks username ) ->
            sharedBookPanel model

        ( Nothing, _ ) ->
            Element.none



-- BOOK PANEL


preBookPanel model =
    if model.bookList == [] then
        startupBookPanel model
    else
        bookPanel model


bookPanel : Model -> Element BookMsg
bookPanel model =
    if model.maybeUser == Nothing then
        Element.el [] (Element.text (bookListTitle model))
    else
        Element.column []
            [ Element.row [ width (px 800) ]
                [ leftColumn model
                , rightColumn model
                ]
            , Element.row [ width (bookListDisplayWidth model), moveUp 23 ] [ bookListDisplay model ]
            ]


sharedBookPanel : Model -> Element BookMsg
sharedBookPanel model =
    if model.maybeUser == Nothing then
        Element.el [] (Element.text (bookListTitle model))
    else
        Element.column []
            [ Element.row [ width (px 800) ]
                [ leftColumnSharedBooks model
                , rightColumn model
                ]
            , Element.row [ moveUp 22 ] [ bookListDisplay model ]
            ]


startupBookPanel : Model -> Element BookMsg
startupBookPanel model =
    if model.maybeUser == Nothing then
        Element.el [] (Element.text (bookListTitle model))
    else
        Element.column []
            [ Element.row [ width (px 800) ]
                [ startupLeftColumn model
                , rightColumn model
                ]
            , Element.row [] [ bookListDisplay model ]
            ]


bookListTitle model =
    case model.maybeUser of
        Nothing ->
            ""

        Just user ->
            user.firstname ++ "'s book list"


leftColumn model =
    Element.column [ width (px 370), height (px 340), spacing 36 ]
        [ currentBookDisplay model
        ]


leftColumnSharedBooks model =
    Element.column [ width (px 370), height (px 340), spacing 36 ]
        [ sharedBookDisplay model
        ]


startupLeftColumn model =
    Element.column [ moveUp 15, Background.color Widget.grey, width (px 370), height (px 280), spacing 30, padding 20 ]
        [ Element.el [ Font.bold, Font.color Widget.blue ] (text "Welcome to BookLib.io!")
        , Element.el [ Font.color Widget.blue ] (text "To get started, press 'New book' below.")
        ]


newBookButton model =
    case model.appState of
        SharingBooks _ ->
            Element.none

        _ ->
            newBookButton_ model


rightColumn model =
    Element.column [ moveUp 9, width (px 450), height (px 354), spacing 36, padding 10 ]
        [ Element.row [ height (px 35), paddingXY 10 5, moveRight 0, moveDown 6, spacing 10, width (px 400), Background.color (Element.rgb 0 0 0) ]
            [ Element.el [] (toggleBlurbAndNotesButton model)
            , Element.el [] (toggleNotesViewModeButton model)
            , Element.el [] (editButton model)
            , wordCountDisplay model
            , toggleMarkdownViewButton model
            ]
        , Keyed.el [ moveUp 26 ] ( String.fromInt model.counter, textView model )
        ]


textView model =
    case ( model.textDisplayMode, model.textRenderMode ) of
        ( DisplayNotes, PlainTextView ) ->
            notesInput model

        ( DisplayBlurb, PlainTextView ) ->
            blurbInput model

        ( DisplayNotes, MarkDownView ) ->
            notesViewedAsMarkdown model

        ( DisplayBlurb, MarkDownView ) ->
            blurbViewedAsMarkdown model


toggleNotesViewModeButton : Model -> Element BookMsg
toggleNotesViewModeButton model =
    Input.button Widget.smallButtonStyle
        { onPress = Just (ToggleNotesViewMode)
        , label = Element.text (noteViewModeIndicator model)
        }


toggleBlurbAndNotesButton : Model -> Element BookMsg
toggleBlurbAndNotesButton model =
    Input.button Widget.smallButtonStyle
        { onPress = Just (ToggleBlurbAndNotes)
        , label = Element.text (noteAndBlurbModeIndicator model)
        }


editButton : Model -> Element BookMsg
editButton model =
    case ( model.textRenderMode, model.appState ) of
        ( _, SharingBooks _ ) ->
            Element.none

        ( MarkDownView, _ ) ->
            editButton_ model

        ( PlainTextView, _ ) ->
            Element.none


editButton_ model =
    Input.button Widget.smallColoredButtonStyle
        { onPress = Just (ToggleMarkdown)
        , label = Element.text "Edit"
        }


editButtonText model =
    case model.textRenderMode of
        PlainTextView ->
            "Save"

        MarkDownView ->
            "Edit"


noteAndBlurbModeIndicator : Model -> String
noteAndBlurbModeIndicator model =
    case model.textDisplayMode of
        DisplayNotes ->
            "Notes"

        DisplayBlurb ->
            "Recs"


noteViewModeIndicator : Model -> String
noteViewModeIndicator model =
    case model.notesViewMode of
        NotesViewShort ->
            "Short view"

        NotesViewLong ->
            "Long view"


textInput model =
    case model.textDisplayMode of
        DisplayBlurb ->
            blurbInput model

        DisplayNotes ->
            notesInput model


notesInput model =
    let
        notes =
            case model.currentBook of
                Nothing ->
                    ""

                Just book ->
                    book.notes
    in
        Input.multiline (textInputStyle model)
            { onChange = recordNotesChanges model
            , text = notes
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 0, Font.bold ] (text "")
            , spellcheck = False
            }


notesViewedAsMarkdown : Model -> Element msg
notesViewedAsMarkdown model =
    case model.currentBook of
        Nothing ->
            Element.none

        Just book ->
            Element.html <| H.div (markdownStyle model) <| [ MD.view book.notes ]


markdownStyle model =
    [ HA.style "height" (markdownViewHeight model)
    , HA.style "width" "385px"
    , HA.style "font-size" "12px"
    , HA.style "line-height" "15px"
    , HA.style "overflow-y" "scroll"
    , HA.style "overflow-x" "hidden"
    , HA.style "padding-top" "20px"
    , HA.style "background-color" "#f7f6f4"
    , HA.style "padding-left" "15px"
    ]


markdownViewHeight model =
    case model.notesViewMode of
        NotesViewShort ->
            "255px"

        NotesViewLong ->
            "532px"


toggleMarkdownViewButton model =
    Input.button Widget.smallButtonStyle
        { onPress = Just (ToggleMarkdown)
        , label = Element.text (toggleMarkdownButtonTitle model)
        }


toggleMarkdownButtonTitle model =
    case model.textRenderMode of
        PlainTextView ->
            "Text view"

        MarkDownView ->
            "Markdown view"


wordCount : String -> Int
wordCount str =
    str |> String.words |> List.length


wordCountOfText : Model -> Int
wordCountOfText model =
    case model.textDisplayMode of
        DisplayNotes ->
            case model.currentBook of
                Nothing ->
                    0

                Just book ->
                    wordCount book.notes

        DisplayBlurb ->
            case model.maybeUser of
                Nothing ->
                    0

                Just user ->
                    wordCount user.blurb


wordCountDisplay : Model -> Element BookMsg
wordCountDisplay model =
    Element.el [ Font.size 13, Font.color Widget.grey ] (text <| (String.fromInt <| wordCountOfText model) ++ " words")


recordNotesChanges model =
    \str -> InputNotes str


blurbViewedAsMarkdown : Model -> Element msg
blurbViewedAsMarkdown model =
    case model.appState of
        SharingBooks _ ->
            Element.html <| H.div (markdownStyle model) <| Markdown.toHtml Nothing model.sharedBlurb

        _ ->
            case model.maybeUser of
                Nothing ->
                    Element.none

                Just user ->
                    Element.html <| H.div (markdownStyle model) <| [ MD.view user.blurb ]


blurbInput model =
    let
        blurb =
            case model.appState of
                SharingBooks _ ->
                    model.sharedBlurb

                _ ->
                    case model.maybeUser of
                        Nothing ->
                            ""

                        Just user ->
                            user.blurb
    in
        Input.multiline (textInputStyle model)
            { onChange = InputBlurb
            , text = blurb
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 0, Font.bold ] (text "")
            , spellcheck = False
            }


textInputStyle model =
    [ Widget.preWrap
    , height (notesViewHeight model)
    , width (px 400)
    , scrollbarY
    , clipX
    , Font.size 13
    , paddingXY 0 20
    , Background.color Widget.lightGrey
    ]


notesViewHeight model =
    case model.notesViewMode of
        NotesViewShort ->
            (px 273)

        NotesViewLong ->
            (px 548)


bookListDisplay model =
    Element.column [ width (bookListDisplayWidth model), spacing 10, padding 10, Background.color Widget.grey ]
        [ Element.row [ spacing 15 ]
            [ Element.el [ Font.bold ] (text <| bookInfo model)
            , Element.el [ Font.size 14, Font.color Widget.blue ] (text <| totalsString model)
            ]
        , listBooks model
        ]



-- "Total pages read since " ++ model.beginningDate ++ ": " ++ (String.fromInt model.totalPagesRead)


newBookButton_ : Model -> Element BookMsg
newBookButton_ model =
    Input.button Widget.smallButtonStyle
        { onPress = Just (NewBook)
        , label = Element.text "New book"
        }



-- CURRENT BOOK DISPLAY


currentBookDisplay model =
    case model.currentBook of
        Nothing ->
            Element.none

        Just book ->
            currentBookPanel book model


sharedBookDisplay model =
    case model.currentBook of
        Nothing ->
            Element.none

        Just book ->
            currentSharedBookPanel book model


currentBookPanel book model =
    Element.column ([ width (px 360), height (px 314), spacing 12 ] ++ panelStyle)
        [ Element.column [ spacing 8 ]
            [ Element.el strongFieldStyle (text <| book.title)
            , Element.el fieldStyle (text <| book.subtitle)
            , Element.el fieldStyle (text <| book.category)
            , Element.el [ clipX ] (text <| "by " ++ book.author)
            , publicCheckbox model
            ]
        , Element.el [ moveUp 0, width (px 300) ] (Indicator.indicator 300 15 "orange" (pageRatio book))
        , Element.el [ moveUp 15, Font.color Widget.blue, Font.bold, paddingXY 0 10 ] (text <| pageInfo book)
        , Element.row [ spacing 15 ]
            [ pagesInput model
            , Element.row [ spacing 5 ]
                [ Element.el [ Font.size 14, moveDown 10 ] (Element.text <| startMessage book)
                , Element.el [ Font.size 14, moveDown 10 ] (Element.text "==>")
                , Element.el [ Font.size 14, moveDown 10 ] (Element.text <| finishMessage book)
                ]
            ]
        , readingRateDisplay model book
        , Element.row [ spacing 20, moveUp 10 ] [ editBookButton, currentDateDisplay model, newBookButton model ]
        ]



--  , Element.el [paddingXY 10 0] (newBookButton model)


readingRateDisplay : Model -> Books -> Element msg
readingRateDisplay model book =
    case ( book.startDateString /= "", book.finishDateString /= "" ) of
        ( True, _ ) ->
            Element.el [ Font.size 14, moveUp 10, moveRight 85 ] (Element.text <| readingRateString model book)

        ( _, _ ) ->
            Element.el [ Font.size 14, moveUp 10, moveRight 85 ] (Element.text <| "")


readingRateString : Model -> Books -> String
readingRateString model book =
    (String.fromInt <| Basics.round <| readingRate model book) ++ " pp/day (" ++ (String.fromInt <| daysToComplete model book) ++ " days)"


daysToComplete : Model -> Books -> Int
daysToComplete model book =
    case book.finishDateString /= "" of
        True ->
            Days.fromUSDate book.startDateString book.finishDateString

        False ->
            Days.fromUSDate book.startDateString (toUtcDateString model.currentTime)


readingRate : Model -> Books -> Float
readingRate model book =
    (Basics.toFloat book.pagesRead) / (Basics.toFloat <| daysToComplete model book)


startMessage : Books -> String
startMessage book =
    case book.startDateString == "" of
        True ->
            "Start date"

        False ->
            book.startDateString


finishMessage : Books -> String
finishMessage book =
    case book.finishDateString == "" of
        True ->
            "Finish date"

        False ->
            book.finishDateString


strongFieldStyle =
    [ Font.bold, width (px 300), clipX ]


fieldStyle =
    [ Font.size 14, width (px 300), clipX ]


currentSharedBookPanel book model =
    Element.column ([ width (px 360), height (px 315), spacing 12 ] ++ panelStyle)
        [ Element.column [ spacing 8 ]
            [ Element.el [ Font.bold, clipX ] (text <| book.title)
            , Element.el [ Font.size 14, clipX ] (text <| book.subtitle)
            , Element.el [ Font.size 14, clipX ] (text <| book.category)
            , Element.el [ clipX ] (text <| "by " ++ book.author)
            ]
        , Element.el [ moveUp 15 ] (Indicator.indicator 325 15 "orange" (pageRatio book))
        , Element.el [ moveUp 30, Font.color Widget.blue, Font.bold, paddingXY 0 10 ] (text <| pageInfo book)
        ]


publicCheckbox model =
    let
        public =
            case model.currentBook of
                Nothing ->
                    False

                Just book ->
                    book.public
    in
        Input.checkbox
            []
            { onChange = ToggleBookPublic
            , icon = icon
            , checked = public
            , label = Input.labelLeft [ Font.size 14, moveDown 1 ] (text "Share book:")
            }


icon : Bool -> Element msg
icon status =
    case status of
        True ->
            el [ Font.size 14, Font.bold, moveDown 1 ] (text "Y")

        False ->
            el [ Font.size 14, Font.bold, moveDown 1 ] (text "N")


pagesInput model =
    let
        pagesRead =
            case model.currentBook of
                Nothing ->
                    0

                Just book ->
                    book.pagesRead
    in
        Input.text [ width (px 60), height (px 25) ]
            { text = String.fromInt pagesRead
            , placeholder = Nothing
            , onChange = \new -> InputPagesRead new
            , label = Input.labelAbove [ Font.size 14 ] (text "Pages read")
            }


updateTextBORN model =
    Input.button (Widget.buttonStyleWithWidth 75)
        { onPress = Just UpdateCurrentBook -- updateTextMessage model
        , label = Element.text "Update"
        }


updateTextMessage model =
    case model.textDisplayMode of
        DisplayNotes ->
            Just UpdateCurrentBook

        DisplayBlurb ->
            Just UpdateBlurb


editBookButton =
    Input.button (Widget.buttonStyleWithWidth 50)
        { onPress = Just EditBook
        , label = Element.text "Edit"
        }



-- NEW BOOK PANEL


newBookPanel : Model -> Element BookMsg
newBookPanel model =
    Element.column [ paddingXY 10 10, spacing 10 ]
        [ Element.el [ Font.bold ] (text <| "New book")
        , inputTitle model
        , inputSubtitle model
        , inputCategory model
        , inputAuthor model
        , inputPages model
        , inputStartDate model
        , Element.row [ spacing 20 ] [ cancelNewBookButton 90 model, createNewBookButton 90 model ]
        ]


editBookPanel : Model -> Element BookMsg
editBookPanel model =
    Element.column [ paddingXY 10 10, spacing 10 ]
        [ Element.el [ Font.bold ] (text <| "Edit book")
        , inputTitle model
        , inputSubtitle model
        , inputCategory model
        , inputAuthor model
        , inputPages model
        , inputStartDate model
        , inputDateFinished model
        , Element.row [ spacing 20 ] [ cancelNewBookButton 75 model, saveChangesButton 140 model ]
        ]


saveChangesButton : Int -> Model -> Element BookMsg
saveChangesButton width_ model =
    Input.button (Widget.buttonStyleWithWidth width_)
        { onPress = Just (SaveBookEditChanges)
        , label = Element.text "Save Changes"
        }


cancelNewBookButton : Int -> Model -> Element BookMsg
cancelNewBookButton width_ model =
    Input.button (Widget.buttonStyleWithWidth width_)
        { onPress = Just (CancelNewBook)
        , label = Element.text "Cancel"
        }


createNewBookButton : Int -> Model -> Element BookMsg
createNewBookButton width_ model =
    Input.button (Widget.buttonStyleWithWidth width_)
        { onPress = Just (CreateNewBook)
        , label = Element.text "Create"
        }


inputTitle model =
    Input.text [ width (px 300), height (px 30) ]
        { text = model.title
        , placeholder = Nothing
        , onChange = \new -> InputTitle new
        , label = Input.labelAbove [ Font.size 14 ] (text "Title")
        }


inputStartDate model =
    Input.text [ width (px 300), height (px 30) ]
        { text = model.startDateString
        , placeholder = Just <| Input.placeholder [ moveUp 6 ] (Element.text "1/15/2018")
        , onChange = \dateString -> InputStartDate dateString
        , label = Input.labelAbove [ Font.size 14 ] (text "Started")
        }


inputDateFinished model =
    Input.text [ width (px 300), height (px 30) ]
        { text = model.finishDateString
        , placeholder = Just <| Input.placeholder [ moveUp 6 ] (Element.text "2/28/2018")
        , onChange = \dateString -> InputFinishDate dateString
        , label = Input.labelAbove [ Font.size 14 ] (text "Finished")
        }


inputSubtitle model =
    Input.text [ width (px 300), height (px 30) ]
        { text = model.subtitle
        , placeholder = Nothing
        , onChange = \new -> InputSubtitle new
        , label = Input.labelAbove [ Font.size 14 ] (text "Subtitle")
        }


inputCategory model =
    Input.text [ width (px 300), height (px 30) ]
        { text = model.category
        , placeholder = Nothing
        , onChange = \new -> InputCategory new
        , label = Input.labelAbove [ Font.size 14 ] (text "Category")
        }


inputAuthor model =
    Input.text [ width (px 300), height (px 30) ]
        { text = model.author
        , placeholder = Nothing
        , onChange = \new -> InputAuthor new
        , label = Input.labelAbove [ Font.size 14 ] (text "Author")
        }


inputPages model =
    Input.text [ width (px 300), height (px 30) ]
        { text = String.fromInt model.pages
        , placeholder = Nothing
        , onChange = \new -> InputPages new
        , label = Input.labelAbove [ Font.size 14 ] (text "Pages")
        }



-- LIST BOOKS


listBooks model =
    case model.notesViewMode of
        NotesViewShort ->
            listBooksWide model

        NotesViewLong ->
            listBooksNarrow model


listBooksWide model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px 215)
        , clipX
        ]
        { data = model.bookList
        , columns =
            [ { header = Element.el tableHeadingStyle (Element.text "Title")
              , width = px 200
              , view =
                    \book ->
                        titleButton book model.currentBook
              }
            , { header = Element.el tableHeadingStyle (Element.text "Author")
              , width = px 150
              , view =
                    \book ->
                        Element.text book.author
              }
            , { header = Element.el tableHeadingStyle (Element.text "Category")
              , width = px 150
              , view =
                    \book ->
                        Element.text book.category
              }
            , { header = Element.el tableHeadingStyle (Element.text "Progress")
              , width = px 110
              , view =
                    \book ->
                        Element.text (pageInfo book)
              }
            , { header = Element.el tableHeadingStyle (Element.text "")
              , width = px 110
              , view =
                    \book ->
                        Element.el [] (Indicator.indicator 100 10 "orange" (pageRatio book))
              }
            ]
        }


listBooksNarrow model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , height (px 215)
        , scrollbarY
        , clipX
        ]
        { data = model.bookList
        , columns =
            [ { header = Element.el tableHeadingStyle (Element.text "Title")
              , width = px 200
              , view =
                    \book ->
                        titleButton book model.currentBook
              }
            , { header = Element.el tableHeadingStyle (Element.text "Author")
              , width = px 200
              , view =
                    \book ->
                        Element.text book.author
              }
            ]
        }


truncatedDisplay : Int -> String -> String
truncatedDisplay n str =
    let
        truncatedStr =
            String.left n str
    in
        if String.length truncatedStr < String.length str then
            truncatedStr ++ " ..."
        else
            str



{- Inputs -}


currentDateDisplay : Model -> Element msg
currentDateDisplay model =
    Element.el [ Font.size 12, moveRight 15, moveDown 1 ] (Element.text <| toUtcDateString model.currentTime)


toUtcString : Maybe Posix -> String
toUtcString maybeTime =
    case maybeTime of
        Nothing ->
            "--:--:--"

        Just time ->
            (String.fromInt (Time.toHour Time.utc time) |> String.padLeft 2 '0')
                ++ ":"
                ++ (String.fromInt (Time.toMinute Time.utc time) |> String.padLeft 2 '0')
                ++ ":"
                ++ (String.fromInt (Time.toSecond Time.utc time) |> String.padLeft 2 '0')


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



-- UNUSED


booksButton model =
    Input.button buttonStyle
        { onPress = Just (Request.doRequestBookList model)
        , label = Element.text "List books"
        }
