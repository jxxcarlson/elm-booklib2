module Pages.CurrentBook exposing (AppState(..), Model, Msg(..), init, update, view)

import Book.Coders
import Book.Types exposing (Book)
import Browser.Navigation exposing (pushUrl)
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
import Element.Keyed as Keyed
import Http
import Pages.Books
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
    , message : String
    , deleteBookState : DeleteBookState
    , previousBook : Maybe Book
    }


type AppState
    = ReadingBook
    | EditingBook
    | EditingNote
    | ViewingNote
    | CreatingBook


type DeleteBookState
    = Ready
    | Armed


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
    , message = ""
    , deleteBookState = Ready
    , previousBook = Nothing
    }


type Msg
    = NoOp
    | ToggleBookPublic Bool
    | InputPagesRead String
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
      --
    | NewBook
    | CreateBook
    | CancelCreateBook
    | EditBook
    | CancelEditBook
    | DeleteBook
    | CancelDeleteBook
    | UpdateBook
    | BookIsCreated (Result Http.Error String)
    | BookIsUpdated (Result Http.Error String)
    | BookIsDeleted (Result Http.Error String)
    | UpdateCurrentBook
    | SaveBookEditChanges
      --
    | ToggleBlurbAndNotes
    | ToggleMarkdown
    | UpdateNotes
    | DoneEditingNotes
    | SetModeToEditingBook
    | SetModeToEditingNote
    | SetModeToViewingNote
    | SetModeToCreating
    | SetInfoToDefaultState
    | ArmDeleteState
    | DisarmArmDeleteState


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

        BookIsDeleted (Ok str) ->
            ( { model | deleteBookState = Ready }, pushUrl sharedState.navKey "#books", NoUpdate )

        BookIsDeleted (Err err) ->
            ( { model | deleteBookState = Ready }, pushUrl sharedState.navKey "#books", NoUpdate )

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
            let
                nextBook =
                    Maybe.map (\book -> { book | title = str }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

        InputSubtitle str ->
            let
                nextBook =
                    Maybe.map (\book -> { book | subtitle = str }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

        InputCategory str ->
            let
                nextBook =
                    Maybe.map (\book -> { book | category = str }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

        InputAuthor str ->
            let
                nextBook =
                    Maybe.map (\book -> { book | author = str }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

        InputPages str ->
            let
                pages =
                    str |> String.toInt |> Maybe.withDefault 0

                nextBook =
                    Maybe.map (\book -> { book | pages = pages }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

        InputStartDate str ->
            let
                nextBook =
                    Maybe.map (\book -> { book | startDateString = str }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

        InputFinishDate str ->
            let
                nextBook =
                    Maybe.map (\book -> { book | finishDateString = str }) sharedState.currentBook
            in
            ( model, Cmd.none, SharedState.UpdateCurrentBook nextBook )

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

        NewBook ->
            ( { model | appState = CreatingBook, previousBook = sharedState.currentBook }
            , Cmd.none
            , SharedState.UpdateCurrentBook (Just Book.Types.blankBook)
            )

        CreateBook ->
            createNewBook sharedState model

        CancelCreateBook ->
            ( { model | appState = ReadingBook, previousBook = Nothing }
            , Cmd.none
            , SharedState.UpdateCurrentBook model.previousBook
            )

        BookIsCreated (Ok str) ->
            ( { model | message = str }, pushUrl sharedState.navKey "#books", NoUpdate )

        BookIsCreated (Err err) ->
            ( model, pushUrl sharedState.navKey "#books", NoUpdate )

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

        ArmDeleteState ->
            ( { model | deleteBookState = Armed }, Cmd.none, NoUpdate )

        DisarmArmDeleteState ->
            ( { model | deleteBookState = Ready }, Cmd.none, NoUpdate )

        DeleteBook ->
            case ( sharedState.currentBook, sharedState.currentUser ) of
                ( Nothing, _ ) ->
                    ( model, Cmd.none, NoUpdate )

                ( Just book, Just user ) ->
                    ( model, deleteBook book user.token, SharedState.UpdateCurrentBook Nothing )

                ( _, _ ) ->
                    ( { model | deleteBookState = Ready }, Cmd.none, NoUpdate )

        CancelDeleteBook ->
            ( { model | deleteBookState = Ready }, Cmd.none, NoUpdate )

        CancelEditBook ->
            ( { model | appState = ReadingBook }, Cmd.none, NoUpdate )

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

        UpdateNotes ->
            case ( sharedState.currentBook, sharedState.currentUser ) of
                ( Nothing, Nothing ) ->
                    ( model, Cmd.none, NoUpdate )

                ( Just book, Just user ) ->
                    ( model
                    , updateBook book user.token
                    , SharedState.UpdateCurrentBook <| Just book
                    )

                _ ->
                    ( model, Cmd.none, NoUpdate )

        DoneEditingNotes ->
            ( { model | appState = ReadingBook }, Cmd.none, NoUpdate )

        SetModeToEditingBook ->
            case model.appState of
                EditingBook ->
                    ( { model | appState = ReadingBook }, Cmd.none, SharedState.NoUpdate )

                _ ->
                    ( { model | appState = EditingBook }, Cmd.none, SharedState.NoUpdate )

        SetModeToEditingNote ->
            ( { model | appState = EditingNote }, Cmd.none, SharedState.NoUpdate )

        SetInfoToDefaultState ->
            case model.appState of
                CreatingBook ->
                    ( { model | appState = ReadingBook, previousBook = Nothing }
                    , Cmd.none
                    , SharedState.UpdateCurrentBook model.previousBook
                    )

                _ ->
                    ( { model | appState = ReadingBook }, Cmd.none, SharedState.NoUpdate )

        SetModeToViewingNote ->
            case model.appState of
                ViewingNote ->
                    ( { model | appState = ReadingBook }, Cmd.none, SharedState.NoUpdate )

                _ ->
                    ( { model | appState = ViewingNote }, Cmd.none, SharedState.NoUpdate )

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


deleteBook : Book -> String -> Cmd Msg
deleteBook book token =
    Http.request
        { method = "Delete"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/books/" ++ String.fromInt book.id
        , body = Http.jsonBody (Book.Coders.bookRecordEncoder book)
        , expect = Http.expectJson BookIsDeleted Book.Coders.statusDecoder
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


deviceIsPhone : SharedState -> Bool
deviceIsPhone sharedState =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            True

        _ ->
            False


showIf : Bool -> Element Msg -> Element Msg
showIf flag element =
    if flag then
        element

    else
        Element.none


view : SharedState -> Model -> Element Msg
view sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            phoneView sharedState model

        _ ->
            mainView sharedState model


mainView : SharedState -> Model -> Element Msg
mainView sharedState model =
    column (Style.mainColumn2 fill fill)
        [ mainRow sharedState model
        , footer sharedState model
        ]


phoneView : SharedState -> Model -> Element Msg
phoneView sharedState model =
    column [ width fill, height fill, clipY, clipX ]
        [ mainRowPhone sharedState model
        , footerForPhone sharedState model
        ]


verticalMargin : Int
verticalMargin =
    128


mainRow : SharedState -> Model -> Element Msg
mainRow sharedState model =
    row
        (Style.mainColumn2 fill fill
            ++ [ spacing 20 ]
        )
        [ mainPanel sharedState model
        , sidePanel sharedState model
        ]


mainRowPhone : SharedState -> Model -> Element Msg
mainRowPhone sharedState model =
    row
        []
        [ case model.appState of
            ReadingBook ->
                currentBookPanel sharedState model

            EditingNote ->
                row []
                    [ row [] [ notesInput sharedState model ]
                    ]

            ViewingNote ->
                row []
                    [ column [] [ Common.Book.notesViewedAsMarkdown 60 (notesWidthForPhone sharedState) (notesHeightForPhone sharedState) sharedState.currentBook ]
                    ]

            EditingBook ->
                editBookPanel sharedState model

            CreatingBook ->
                newBookPanel sharedState model
        ]


mainPanel sharedState model =
    case model.appState of
        CreatingBook ->
            newBookPanel sharedState model

        _ ->
            currentBookPanel sharedState model


notesWidthForPhone sharedState =
    String.fromInt sharedState.windowWidth ++ "px"


notesHeightForPhone sharedState =
    String.fromInt (sharedState.windowHeight - 110) ++ "px"


notesHeight sharedState =
    String.fromInt (sharedState.windowHeight - verticalMargin - 20) ++ "px"


sidePanel sharedState model =
    case model.appState of
        ReadingBook ->
            column [ Border.width 1 ]
                [ Common.Book.notesViewedAsMarkdown 70 "400px" (notesHeight sharedState) sharedState.currentBook
                ]

        EditingNote ->
            row [ spacing 12 ]
                [ row [ moveUp 4 ] [ notesInput sharedState model ]
                , column [ Border.width 1, moveUp 2 ] [ Common.Book.notesViewedAsMarkdown 70 "400px" (notesHeight sharedState) sharedState.currentBook ]
                ]

        ViewingNote ->
            row [ spacing 12 ]
                [ row [ moveUp 4 ] [ notesInput sharedState model ]
                , column [ Border.width 1, moveUp 2 ] [ Common.Book.notesViewedAsMarkdown 70 (notesWidth sharedState) (notesHeight sharedState) sharedState.currentBook ]
                ]

        EditingBook ->
            editBookPanel sharedState model

        CreatingBook ->
            Element.none


notesWidth : SharedState -> String
notesWidth sharedState =
    sharedState.windowWidth - 55 |> String.fromInt |> (\x -> x ++ "px")


grey p =
    rgb p p p


currentBookPanel : SharedState -> Model -> Element Msg
currentBookPanel sharedState model =
    case sharedState.currentBook of
        Nothing ->
            el [ Font.color Style.blue ] (text "No book selected.  Click on 'New' to add a new book")

        Just book ->
            column
                [ Background.color <| grey 0.9
                , padding 30
                , case deviceIsPhone sharedState of
                    True ->
                        width (px (sharedState.windowHeight - 20))

                    False ->
                        width (px 360)
                , case deviceIsPhone sharedState of
                    True ->
                        height (px (sharedState.windowHeight - 88))

                    False ->
                        height (px (sharedState.windowHeight - verticalMargin))
                , spacing 36
                , alignTop
                , case deviceIsPhone sharedState of
                    True ->
                        Border.width 0

                    False ->
                        Border.width 1
                ]
                [ bookAndAuthorInfo book
                , progressInfo sharedState book
                , changeParameters sharedState model book
                ]


editControls sharedState =
    case deviceIsPhone sharedState of
        True ->
            row [ paddingXY 20 0, spacing 12, Background.color <| grey 0.1, width <| px <| sharedState.windowWidth, height (px 20) ]
                [ el [ Font.size 14, Font.color Style.white ] (text "Editing note, auto-save on")
                ]

        False ->
            row [ paddingXY 20 0, spacing 12, Background.color <| grey 0.1, height (px 20), width <| px <| 400 ]
                [ el [ Font.size 14, Font.color Style.white ] (text "Editing note, auto-save on")
                ]


bookAndAuthorInfo book =
    column [ spacing 8 ]
        [ el strongFieldStyle (text <| book.title)
        , el fieldStyle (text <| book.subtitle)
        , el fieldStyle (text <| book.category)
        , el fieldStyle (text <| "by " ++ book.author)
        ]


progressInfo sharedState book =
    column [ paddingXY 0 30, width (px 300) ]
        [ Indicator.indicator 300 15 "orange" (pageRatio book)
        , el [ moveDown 4, Font.size 16, Font.color Style.darkBlue, Font.bold, paddingXY 0 10 ] (text <| pageInfo book)
        , row [ spacing 5, moveUp 10, Font.color Style.darkBlue ]
            [ el [ Font.size 15, moveDown 10 ] (text <| startMessage book)
            , el [ Font.size 15, moveDown 10 ] (text "—")
            , el [ Font.size 15, moveDown 10 ] (text <| finishMessage sharedState book)
            ]
        , readingRateDisplay sharedState book
        ]


changeParameters sharedState model book =
    column [ spacing 8 ]
        [ pagesInput sharedState model
        , publicCheckbox book
        , row [ paddingXY 0 4 ] [ updateBookButton ]
        ]


notesInput sharedState model =
    let
        notes =
            case sharedState.currentBook of
                Nothing ->
                    ""

                Just book ->
                    book.notes
    in
    column []
        [ Keyed.el []
            ( String.fromInt model.counter
            , Input.multiline (textInputStyle sharedState)
                { onChange = InputNotes
                , text = notes
                , placeholder = Nothing
                , label = Input.labelBelow [ Font.size 0, Font.bold ] (text "")
                , spellcheck = False
                }
            )
        , editControls sharedState
        ]


textInputStyle sharedState =
    [ Style.preWrap
    , case deviceIsPhone sharedState of
        True ->
            height <| px <| sharedState.windowHeight - 110

        -- 100, 130
        False ->
            height <| px <| sharedState.windowHeight - 146
    , case deviceIsPhone sharedState of
        True ->
            width <| px <| sharedState.windowWidth

        False ->
            width <| px <| 400
    , clipX
    , paddingXY 12 12
    , Font.size 13
    , case deviceIsPhone sharedState of
        True ->
            paddingXY 5 5

        False ->
            paddingXY 8 20
    , Background.color Style.lightGrey
    , case deviceIsPhone sharedState of
        True ->
            Border.width 0

        False ->
            Border.width 2
    ]


recordNotesChanges =
    \str -> InputNotes str


strongFieldStyle =
    [ Font.size 24, Font.bold, width (px 300), clipX ]


fieldStyle =
    [ Font.size 14, width (px 300), clipX ]


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


finishMessage : SharedState -> Book -> String
finishMessage sharedState book =
    case book.finishDateString == "" of
        True ->
            let
                n =
                    daysToFinish sharedState book
            in
            case n == 1 of
                True ->
                    String.fromInt n ++ " day to finish" ++ String.fromInt (daysRemaining book) ++ " pp"

                False ->
                    String.fromInt n ++ " days to finish: " ++ String.fromInt (daysRemaining book) ++ " pp"

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
    let
        daysToComplete_ =
            daysToComplete sharedState book

        daysString =
            case daysToComplete_ == 1 of
                True ->
                    " day)"

                False ->
                    " days)"
    in
    (String.fromInt <|
        Basics.round <|
            readingRate sharedState book
    )
        ++ " pp/day ("
        ++ (String.fromInt <| daysToComplete_)
        ++ daysString


daysToFinish : SharedState -> Book -> Int
daysToFinish sharedState book =
    let
        rate =
            List.maximum [ readingRate sharedState book, 1 ] |> Maybe.withDefault 1.0

        pagesRemaining =
            toFloat <| book.pages - book.pagesRead
    in
    round (pagesRemaining / rate)


daysRemaining : Book -> Int
daysRemaining book =
    book.pages - book.pagesRead


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
    Input.button Style.button
        { onPress = Just UpdateBook
        , label = el [ centerX ] (Element.text "Update")
        }


newBookButton : Model -> Element Msg
newBookButton model =
    Input.button (Style.activeButton (model.appState == CreatingBook))
        { onPress = Just NewBook
        , label = Element.text "New"
        }


bookInfoButton : Model -> Element Msg
bookInfoButton model =
    Input.button (Style.activeButtonDarkRed (model.appState /= ReadingBook))
        { onPress = Just SetInfoToDefaultState
        , label = Element.text "Info"
        }


deleteBookButton : Model -> Element Msg
deleteBookButton model =
    Input.button (Style.activeButtonRed (model.deleteBookState == Armed))
        { onPress = Just (deleteBookMsg model)
        , label = Element.text "Delete"
        }


cancelDeleteBookButton : Model -> Element Msg
cancelDeleteBookButton model =
    Input.button (Style.activeButton (model.deleteBookState == Armed))
        { onPress = Just CancelDeleteBook
        , label = Element.text "Cancel"
        }


deleteBookMsg : Model -> Msg
deleteBookMsg model =
    case model.deleteBookState of
        Ready ->
            ArmDeleteState

        Armed ->
            DeleteBook


createBookButton : Model -> Element Msg
createBookButton model =
    Input.button (Style.activeButton (model.appState == CreatingBook))
        { onPress = Just CreateBook
        , label = Element.text "Create"
        }


cancelCreateBookButton model =
    Input.button Style.button
        { onPress = Just CancelCreateBook
        , label = Element.text "Cancel"
        }


editBookButton sharedState model =
    let
        label =
            case deviceIsPhone sharedState of
                True ->
                    "Edit"

                False ->
                    "Edit book"
    in
    Input.button (Style.activeButton (model.appState == EditingBook))
        { onPress = Just SetModeToEditingBook
        , label = Element.text label
        }


editNoteButton sharedState model =
    let
        label =
            case deviceIsPhone sharedState of
                True ->
                    "Edit"

                False ->
                    "Edit note"
    in
    Input.button (Style.activeButton (model.appState == EditingNote))
        { onPress = Just SetModeToEditingNote
        , label = Element.text label
        }


viewNoteButton model =
    Input.button (Style.activeButton (model.appState == ViewingNote))
        { onPress = Just SetModeToViewingNote
        , label = Element.text "Notes"
        }


updateEditNotesButton model =
    Input.button Style.button
        { onPress = Just UpdateNotes
        , label = Element.text "Update"
        }


cancelEditNoteButton model =
    Input.button Style.button
        { onPress = Just CancelEditBook
        , label = Element.text "Cancel"
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
        [ case model.appState of
            ReadingBook ->
                deleteBookButton model

            _ ->
                bookInfoButton model
        , showIf (model.deleteBookState == Armed) (cancelDeleteBookButton model)
        , showIf (model.deleteBookState /= Armed) (newBookButton model)
        , showIf (model.appState /= CreatingBook) (editBookButton sharedState model)
        , showIf (model.appState /= CreatingBook) (editNoteButton sharedState model)
        , showIf (List.member model.appState [ ReadingBook, EditingNote, ViewingNote ]) (wordCountOfCurrentNotes sharedState)
        ]


footerForPhone : SharedState -> Model -> Element Msg
footerForPhone sharedState model =
    row [ spacing 8, Background.color Style.charcoal, paddingXY 12 8, alignBottom, width fill, Font.size 12 ]
        [ case model.appState of
            ReadingBook ->
                deleteBookButton model

            _ ->
                bookInfoButton model
        , showIf (model.deleteBookState == Armed) (cancelDeleteBookButton model)
        , showIf (model.deleteBookState /= Armed && (not <| List.member model.appState [ EditingNote, ViewingNote, EditingBook ])) (newBookButton model)
        , showIf (model.deleteBookState /= Armed && model.appState /= CreatingBook && (not <| List.member model.appState [ EditingNote, ViewingNote, EditingBook ])) (editBookButton sharedState model)
        , showIf (model.appState /= CreatingBook) (viewNoteButton model)
        , showIf (List.member model.appState [ ViewingNote, EditingNote, EditingBook ]) (editNoteButton sharedState model)
        ]


wordCountOfCurrentNotes : SharedState -> Element Msg
wordCountOfCurrentNotes sharedState =
    case sharedState.currentBook of
        Nothing ->
            Element.none

        Just book ->
            el Style.footerItem (text <| "Word count: " ++ String.fromInt (wordCount book.notes))


wordCount : String -> Int
wordCount str =
    str |> String.words |> List.length


userStatus : Maybe User -> String
userStatus user_ =
    case user_ of
        Nothing ->
            "Not signed in."

        Just user ->
            "Signed in as " ++ user.username



--
-- INPUT BOOK METADATA
--


editBookPanel : SharedState -> Model -> Element Msg
editBookPanel sharedState model =
    Element.column
        [ clipX
        , paddingXY 10 10
        , spacing 10
        , case deviceIsPhone sharedState of
            True ->
                width (px (sharedState.windowHeight - 20))

            False ->
                width (px 420)
        , case deviceIsPhone sharedState of
            True ->
                height (px (sharedState.windowHeight - 88))

            False ->
                height (px (sharedState.windowHeight - verticalMargin))
        , case deviceIsPhone sharedState of
            True ->
                Border.width 0

            False ->
                Border.width 1
        ]
        [ Element.el [ Font.bold ] (text <| "Edit book")
        , inputTitle sharedState
        , inputSubtitle sharedState
        , inputCategory sharedState
        , inputAuthor sharedState
        , inputPages sharedState
        , inputStartDate sharedState
        , inputDateFinished sharedState
        , row [ spacing 12 ] [ updateEditNotesButton model, cancelEditNoteButton model ]
        ]


phoneWidth sharedState =
    sharedState.windowWidth - 40


newBookPanel : SharedState -> Model -> Element Msg
newBookPanel sharedState model =
    Element.column
        [ paddingXY 10 10
        , spacing 10
        , case deviceIsPhone sharedState of
            True ->
                width (px (sharedState.windowHeight - 20))

            False ->
                width (px 430)
        , case deviceIsPhone sharedState of
            True ->
                height (px (sharedState.windowHeight - 88))

            False ->
                height (px (sharedState.windowHeight - verticalMargin))
        , case deviceIsPhone sharedState of
            True ->
                Border.width 0

            False ->
                Border.width 1
        ]
        [ Element.el [ Font.bold ] (text <| "New Book!!")
        , inputTitle sharedState
        , inputSubtitle sharedState
        , inputCategory sharedState
        , inputAuthor sharedState
        , inputPages sharedState
        , inputStartDate sharedState
        , row [ moveDown 18, spacing 12 ] [ createBookButton model, cancelCreateBookButton model ]
        ]


inputWidth sharedState =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            px (sharedState.windowWidth - 50)

        _ ->
            px 400


inputWidthSmall =
    px 120


inputStyle sharedState =
    case deviceIsPhone sharedState of
        True ->
            [ width (px (sharedState.windowWidth - 80)), height (px 25), Font.size 14 ]

        False ->
            [ width (px 400), height (px 30) ]


inputStyleSmall sharedState =
    case deviceIsPhone sharedState of
        True ->
            [ width (px 100), height (px 25), Font.size 14 ]

        False ->
            [ width (px 120), height (px 30) ]


inputTitle sharedState =
    Input.text (inputStyle sharedState)
        { text = sharedState.currentBook |> Maybe.map .title |> Maybe.withDefault ""
        , placeholder = Nothing
        , onChange = \new -> InputTitle new
        , label = Input.labelAbove [ Font.size 14 ] (text "Title")
        }


inputStartDate sharedState =
    Input.text (inputStyleSmall sharedState)
        { text = sharedState.currentBook |> Maybe.map .startDateString |> Maybe.withDefault ""
        , placeholder = Just <| Input.placeholder [ moveUp 6 ] (Element.text "1/15/2018")
        , onChange = \dateString -> InputStartDate dateString
        , label = Input.labelAbove [ Font.size 14 ] (text "Started")
        }


inputDateFinished sharedState =
    Input.text (inputStyleSmall sharedState)
        { text = sharedState.currentBook |> Maybe.map .finishDateString |> Maybe.withDefault ""
        , placeholder = Just <| Input.placeholder [ moveUp 6 ] (Element.text "2/28/2018")
        , onChange = \dateString -> InputFinishDate dateString
        , label = Input.labelAbove [ Font.size 14 ] (text "Finished")
        }


inputSubtitle sharedState =
    Input.text (inputStyle sharedState)
        { text = sharedState.currentBook |> Maybe.map .subtitle |> Maybe.withDefault ""
        , placeholder = Nothing
        , onChange = \new -> InputSubtitle new
        , label = Input.labelAbove [ Font.size 14 ] (text "Subtitle")
        }


inputCategory sharedState =
    Input.text (inputStyle sharedState)
        { text = sharedState.currentBook |> Maybe.map .category |> Maybe.withDefault ""
        , placeholder = Nothing
        , onChange = \new -> InputCategory new
        , label = Input.labelAbove [ Font.size 14 ] (text "Category")
        }


inputAuthor sharedState =
    Input.text (inputStyle sharedState)
        { text = sharedState.currentBook |> Maybe.map .author |> Maybe.withDefault ""
        , placeholder = Nothing
        , onChange = \new -> InputAuthor new
        , label = Input.labelAbove [ Font.size 14 ] (text "Author")
        }


inputPages sharedState =
    Input.text (inputStyleSmall sharedState)
        { text = sharedState.currentBook |> Maybe.map .pages |> Maybe.map String.fromInt |> Maybe.withDefault ""
        , placeholder = Nothing
        , onChange = InputPages
        , label = Input.labelAbove [ Font.size 14 ] (text "Pages")
        }



--
-- HELPERS
--


createNewBook : SharedState -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
createNewBook sharedState model =
    let
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

        cmd =
            case sharedState.currentBook of
                Nothing ->
                    Cmd.none

                Just book ->
                    createBook userid book token
    in
    ( { model
        | appState = CreatingBook
      }
    , cmd
    , SharedState.UpdateCurrentBook sharedState.currentBook
    )
