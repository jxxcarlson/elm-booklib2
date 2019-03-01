module Pages.CurrentBook exposing (Model, Msg(..), init, update, view)

import Book.Coders
import Book.Types exposing (Book)
import Common.Book
import Common.Days as Days
import Common.Indicator as Indicator
import Common.Style as Style
import Common.Utility as Utility
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Types exposing (User)


type alias Model =
    { title : String
    , author : String
    , subtitle : String
    , notes : String
    , category : String
    , pages : Int
    , pagesRead : Int
    , startDateString : String
    , finishDateString : String
    , textDisplayMode : TextDisplayMode
    , textRenderMode : TextRenderMode
    , counter : Int
    , appState : AppState
    }


type AppState
    = ReadingBook
    | EditingBook
    | CreatingBook


init : Model
init =
    { title = ""
    , author = ""
    , subtitle = ""
    , notes = ""
    , category = ""
    , pages = 0
    , pagesRead = 0
    , startDateString = ""
    , finishDateString = ""
    , textDisplayMode = DisplayNotes
    , textRenderMode = MarkDownView
    , counter = 0
    , appState = ReadingBook
    }


type Msg
    = NoOp
    | ToggleBookPublic Bool
    | InputPagesRead String
    | UpdateBook
    | BookIsUpdated (Result Http.Error String)
    | BookIsCreated (Result Http.Error String)
    | UpdateCurrentBook
    | InputNotes String
    | InputTitle String
    | InputSubtitle String
    | InputCategory String
    | InputAuthor String
    | InputPages String
    | InputBlurb String
    | InputStartDate String
    | InputFinishDate String
    | UpdateBlurb
    | NewBook
    | CancelNewBook
    | CreateNewBook
    | EditBook
    | SaveBookEditChanges
    | ToggleBlurbAndNotes
    | ToggleMarkdown
    | SetModeToReading
    | SetModeToEditing
    | SetModeToCreating


type TextDisplayMode
    = DisplayNotes
    | DisplayBlurb


type TextRenderMode
    = PlainTextView
    | MarkDownView



--
-- UPDATE
--


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
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

        UpdateBook ->
            case ( sharedState.currentBook, sharedState.currentUser ) of
                ( Nothing, Nothing ) ->
                    ( model, Cmd.none, NoUpdate )

                ( Just book, Just user ) ->
                    ( model, updateBook book user.token, SharedState.UpdateCurrentBook <| Just book )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        BookIsUpdated (Ok str) ->
            ( model, Cmd.none, NoUpdate )

        BookIsUpdated (Err err) ->
            ( model, Cmd.none, NoUpdate )

        UpdateCurrentBook ->
            let
                nextCurrentBook =
                    case sharedState.currentBook of
                        Nothing ->
                            Nothing

                        Just book ->
                            Just { book | pagesRead = model.pagesRead }

                token =
                    case sharedState.currentUser of
                        Nothing ->
                            ""

                        Just user ->
                            user.token

                updateBookCmd =
                    case nextCurrentBook of
                        Nothing ->
                            Cmd.none

                        Just book ->
                            updateBook book token
            in
            case nextCurrentBook of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just book ->
                    ( { model
                        | notes = book.notes
                        , pagesRead = book.pagesRead
                      }
                    , updateBookCmd
                    , SharedState.UpdateCurrentBook nextCurrentBook
                    )

        InputTitle str ->
            ( { model | title = str }, Cmd.none, NoUpdate )

        InputSubtitle str ->
            ( { model | subtitle = str }, Cmd.none, NoUpdate )

        InputCategory str ->
            ( { model | category = str }, Cmd.none, NoUpdate )

        InputAuthor str ->
            ( { model | author = str }, Cmd.none, NoUpdate )

        InputPages str ->
            ( { model | pages = str |> String.toInt |> Maybe.withDefault 0 }, Cmd.none, NoUpdate )

        InputPagesRead str ->
            let
                userid =
                    case sharedState.currentUser of
                        Nothing ->
                            0

                        Just user ->
                            user.id

                ( nextBook, deltaPages ) =
                    case sharedState.currentBook of
                        Nothing ->
                            ( Nothing, 0 )

                        Just book ->
                            ( Just { book | pagesRead = str |> String.toInt |> Maybe.withDefault 0 }
                            , (str |> String.toInt |> Maybe.withDefault 0) - book.pagesRead
                            )

                command =
                    case nextBook of
                        Nothing ->
                            Cmd.none

                        Just book ->
                            updateBook book (userToken sharedState)
            in
            ( { model
                | pagesRead = str |> String.toInt |> Maybe.withDefault 0
              }
            , command
            , SharedState.UpdateCurrentBook nextBook
            )

        InputNotes str ->
            let
                updatedBook =
                    case sharedState.currentBook of
                        Nothing ->
                            Nothing

                        Just book ->
                            Just { book | notes = str }

                command =
                    case updatedBook of
                        Nothing ->
                            Cmd.none

                        Just book ->
                            updateBook book (userToken sharedState)
            in
            ( model
            , command
            , SharedState.UpdateCurrentBook updatedBook
            )

        InputBlurb str ->
            let
                nextCurrentUser =
                    case sharedState.currentUser of
                        Nothing ->
                            Nothing

                        Just user ->
                            Just { user | blurb = str }
            in
            ( model, Cmd.none, UpdateCurrentUser nextCurrentUser )

        CreateNewBook ->
            let
                newBook =
                    { id = 0
                    , title = model.title
                    , subtitle = model.subtitle
                    , category = model.category
                    , author = model.author
                    , pages = model.pages
                    , notes = "Just started ..."
                    , pagesRead = 0
                    , rating = 0
                    , public = False
                    , startDateString = ""
                    , finishDateString = ""
                    }

                token =
                    case sharedState.currentUser of
                        Nothing ->
                            ""

                        Just user ->
                            user.token

                userid =
                    case sharedState.currentUser of
                        Nothing ->
                            0

                        Just user ->
                            user.id
            in
            ( { model
                | -- previousCurrentBook = model.currentBook
                  notes = newBook.notes
                , pagesRead = newBook.pagesRead
                , textRenderMode = PlainTextView
              }
            , createBook userid newBook token
            , SharedState.UpdateCurrentBook (Just newBook)
            )

        BookIsCreated (Ok str) ->
            ( model, Cmd.none, NoUpdate )

        BookIsCreated (Err err) ->
            ( model, Cmd.none, NoUpdate )

        InputStartDate str ->
            ( { model | startDateString = str }, Cmd.none, NoUpdate )

        InputFinishDate str ->
            ( { model | finishDateString = str }, Cmd.none, NoUpdate )

        ToggleBlurbAndNotes ->
            case model.textDisplayMode of
                DisplayNotes ->
                    ( { model | counter = model.counter + 1, textDisplayMode = DisplayBlurb }, Cmd.none, NoUpdate )

                DisplayBlurb ->
                    ( { model | counter = model.counter + 1, textDisplayMode = DisplayNotes }, Cmd.none, NoUpdate )

        ToggleMarkdown ->
            case model.textRenderMode of
                PlainTextView ->
                    ( { model | textRenderMode = MarkDownView }, Cmd.none, NoUpdate )

                MarkDownView ->
                    ( { model | textRenderMode = PlainTextView }, Cmd.none, NoUpdate )

        UpdateBlurb ->
            ( model, Cmd.none, NoUpdate )

        NewBook ->
            ( { model
                | appState = CreatingBook
                , title = ""
                , subtitle = ""
                , author = ""
                , pages = 1
                , pagesRead = 0
                , notes = "Just started"
              }
            , Cmd.none
            , NoUpdate
            )

        CancelNewBook ->
            ( { model
                | appState = ReadingBook
              }
            , Cmd.none
            , NoUpdate
            )

        EditBook ->
            let
                title =
                    case sharedState.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.title

                subtitle =
                    case sharedState.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.subtitle

                category =
                    case sharedState.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.category

                author =
                    case sharedState.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.author

                pages =
                    case sharedState.currentBook of
                        Nothing ->
                            1

                        Just book ->
                            book.pages
            in
            ( { model
                | appState = EditingBook
                , title = title
                , subtitle = subtitle
                , category = category
                , author = author
                , pages = pages
              }
            , Cmd.none
            , NoUpdate
            )

        SaveBookEditChanges ->
            case ( sharedState.currentBook, sharedState.currentUser ) of
                ( Just book, Just user ) ->
                    let
                        updatedBook =
                            { book
                                | title = model.title
                                , subtitle = model.subtitle
                                , category = model.category
                                , author = model.author
                                , pages = model.pages
                                , startDateString = model.startDateString
                                , finishDateString = model.finishDateString
                            }
                    in
                    ( model
                    , updateBook updatedBook user.token
                    , SharedState.UpdateCurrentBook (Just updatedBook)
                    )

                ( _, _ ) ->
                    ( model, Cmd.none, NoUpdate )

        SetModeToReading ->
            ( { model | appState = ReadingBook }, Cmd.none, SharedState.NoUpdate )

        SetModeToEditing ->
            ( { model | appState = EditingBook }, Cmd.none, SharedState.NoUpdate )

        SetModeToCreating ->
            ( { model | appState = CreatingBook }, Cmd.none, SharedState.NoUpdate )


createBook : Int -> Book -> String -> Cmd Msg
createBook userid book token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/books"
        , body = Http.jsonBody (Book.Coders.newBookRecordEncoder userid book)
        , expect = Http.expectJson BookIsCreated Book.Coders.statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateBook : Book -> String -> Cmd Msg
updateBook book token =
    Http.request
        { method = "Put"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/books/" ++ String.fromInt book.id
        , body = Http.jsonBody (Book.Coders.bookRecordEncoder book)
        , expect = Http.expectJson BookIsUpdated Book.Coders.statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- HELPERS
--


userToken : SharedState -> String
userToken sharedState =
    case sharedState.currentUser of
        Nothing ->
            ""

        Just user ->
            user.token



--
-- VIEW
--


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn2 fill fill)
        [ mainRow sharedState model
        , footer sharedState model
        ]


mainRow : SharedState -> Model -> Element Msg
mainRow sharedState model =
    row
        (Style.mainColumn2 fill fill
            ++ [ spacing 20 ]
        )
        [ currentBookPanel sharedState model
        , case model.appState of
            ReadingBook ->
                Common.Book.notesViewedAsMarkdown "400px" "509px" sharedState.currentBook

            EditingBook ->
                notesInput (px 400) (px 509) sharedState

            CreatingBook ->
                Element.none
        ]


grey p =
    rgb p p p


currentBookPanel : SharedState -> Model -> Element Msg
currentBookPanel sharedState model =
    case sharedState.currentBook of
        Nothing ->
            el [] (text "No book selected")

        Just book ->
            column [ Background.color <| grey 0.9, padding 30, width (px 360), height (px 310), spacing 12, alignTop, Border.width 1 ]
                [ column [ spacing 8 ]
                    [ el strongFieldStyle (text <| book.title)
                    , el fieldStyle (text <| book.subtitle)
                    , el fieldStyle (text <| book.category)
                    , el [ clipX ] (text <| "by " ++ book.author)
                    ]
                , column [ paddingXY 0 30, width (px 300) ]
                    [ Indicator.indicator 300 15 "orange" (pageRatio book)
                    , el [ moveDown 4, Font.size 16, Font.color Style.darkBlue, Font.bold, paddingXY 0 10 ] (text <| pageInfo book)
                    , row [ spacing 5, moveUp 10, Font.color Style.darkBlue ]
                        [ el [ Font.size 15, moveDown 10 ] (text <| startMessage book)
                        , el [ Font.size 15, moveDown 10 ] (text "—")
                        , el [ Font.size 15, moveDown 10 ] (text <| finishMessage book)
                        ]
                    , readingRateDisplay sharedState book
                    ]
                , column [ moveDown 30, moveLeft 30, paddingXY 20 20, Background.color <| grey 0.9, Border.width 1, width (px 360), height (px 200) ]
                    [ row [ spacing 15 ]
                        [ pagesInput sharedState model
                        , publicCheckbox book
                        ]
                    , row [ paddingXY 0 30 ] [ updateBookButton ]
                    , column
                        [ paddingXY 0 8, spacing 10 ]
                        [ row [ alignBottom, spacing 20 ] [ readBookButton model, editBookButton model, newBookButton model ]
                        ]
                    ]
                ]


notesInput h w sharedState =
    let
        notes =
            case sharedState.currentBook of
                Nothing ->
                    ""

                Just book ->
                    book.notes
    in
    Input.multiline (textInputStyle (px 350) (px 530))
        { onChange = InputNotes
        , text = notes
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
    , paddingXY 0 20
    , Background.color Style.lightGrey
    ]


recordNotesChanges =
    \str -> InputNotes str


strongFieldStyle =
    [ Font.size 24, Font.bold, width (px 300), clipX ]


fieldStyle =
    [ Font.size 18, width (px 300), clipX ]


pageRatio : Book -> Float
pageRatio book =
    toFloat book.pagesRead / toFloat book.pages


pageInfo book =
    let
        pp =
            String.fromInt book.pagesRead ++ "/" ++ String.fromInt book.pages

        pc =
            String.fromInt <| Basics.round <| 100 * Basics.toFloat book.pagesRead / Basics.toFloat book.pages
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
                , label = Input.labelLeft [ Font.size 16, moveDown 4 ] (text "Pages read ")
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
            Element.el [ moveDown 5, Font.size 16, Font.color Style.darkBlue ] (Element.text <| readingRateString sharedState book)

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
    Basics.toFloat book.pagesRead / (Basics.toFloat <| daysToComplete shareState book)


updateBookButton : Element Msg
updateBookButton =
    Input.button (Style.button ++ [ width (px 280) ])
        { onPress = Just UpdateBook
        , label = el [ centerX ] (Element.text "Update")
        }


newBookButton : Model -> Element Msg
newBookButton model =
    Input.button (Style.activeButton (model.appState == CreatingBook))
        { onPress = Just SetModeToCreating
        , label = Element.text "New"
        }


editBookButton model =
    Input.button (Style.activeButton (model.appState == EditingBook))
        { onPress = Just SetModeToEditing
        , label = Element.text "Edit"
        }


readBookButton model =
    Input.button (Style.activeButton (model.appState == ReadingBook))
        { onPress = Just SetModeToReading
        , label = Element.text "Read"
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
        , label = Input.labelLeft [ Font.size 16, moveDown 1 ] (text "Share book:")
        }


icon : Bool -> Element msg
icon status =
    case status of
        True ->
            el [ Font.size 18, Font.bold, Font.color Style.blue, moveDown 1 ] (text "Yes")

        False ->
            el [ Font.size 18, Font.bold, Font.color Style.darkRed, moveDown 1 ] (text "No")


bookTitle : Maybe Book -> String
bookTitle book_ =
    case book_ of
        Nothing ->
            "No book"

        Just book ->
            book.title


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        , el Style.footerItem (text <| userStatus sharedState.currentUser)
        ]


userStatus : Maybe User -> String
userStatus user_ =
    case user_ of
        Nothing ->
            "Not signed in."

        Just user ->
            "Signed in as " ++ user.username
