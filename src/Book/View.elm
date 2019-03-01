module Books.View exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Html as H
import Html.Attributes as HA
import Markdown
import Time exposing (Month(..), Posix)



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
        { onPress = Just ToggleNotesViewMode
        , label = Element.text (noteViewModeIndicator model)
        }


toggleBlurbAndNotesButton : Model -> Element BookMsg
toggleBlurbAndNotesButton model =
    Input.button Widget.smallButtonStyle
        { onPress = Just ToggleBlurbAndNotes
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
        { onPress = Just ToggleMarkdown
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


toggleMarkdownViewButton model =
    Input.button Widget.smallButtonStyle
        { onPress = Just ToggleMarkdown
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
            px 273

        NotesViewLong ->
            px 548


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
        { onPress = Just NewBook
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



--  , Element.el [paddingXY 10 0] (newBookButton model)


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


saveChangesButton : Int -> Model -> Element BookMsg
saveChangesButton width_ model =
    Input.button (Widget.buttonStyleWithWidth width_)
        { onPress = Just SaveBookEditChanges
        , label = Element.text "Save Changes"
        }


cancelNewBookButton : Int -> Model -> Element BookMsg
cancelNewBookButton width_ model =
    Input.button (Widget.buttonStyleWithWidth width_)
        { onPress = Just CancelNewBook
        , label = Element.text "Cancel"
        }


createNewBookButton : Int -> Model -> Element BookMsg
createNewBookButton width_ model =
    Input.button (Widget.buttonStyleWithWidth width_)
        { onPress = Just CreateNewBook
        , label = Element.text "Create"
        }



-- LIST BOOKS


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
