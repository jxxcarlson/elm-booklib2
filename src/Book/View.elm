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



-- BOOK PANEL





bookListTitle model =
    case model.maybeUser of
        Nothing ->
            ""

        Just user ->
            user.firstname ++ "'s book list"


      ]



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



-- "Total pages read since " ++ model.beginningDate ++ ": " ++ (String.fromInt model.totalPagesRead)


newBookButton_ : Model -> Element BookMsg
newBookButton_ model =
    Input.button Widget.smallButtonStyle
        { onPress = Just NewBook
        , label = Element.text "New book"
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
