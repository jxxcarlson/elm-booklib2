module Book.Book exposing (update)

import Config exposing (configuration)
import Book.Model as Model exposing (..)
import Book.Request as Request
import Book.View exposing (view)
import Utility
import Time exposing (Posix)


update : Model -> BookMsg -> ( Model, Cmd BookMsg )
update model msg =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        NewBook ->
            ( { model
                | appState = CreatingNewBook
                , title = ""
                , subtitle = ""
                , author = ""
                , pages = 1
                , pagesRead = 0
                , notes = "Just started"
              }
            , Cmd.none
            )

        CancelNewBook ->
            ( { model
                | appState = ReadingMyBooks
                , -- currentBook = model.previousCurrentBook,
                  previousCurrentBook = Nothing
              }
            , Cmd.none
            )

        EditBook ->
            let
                title =
                    case model.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.title

                subtitle =
                    case model.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.subtitle

                category =
                    case model.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.category

                author =
                    case model.currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.author

                pages =
                    case model.currentBook of
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
                )

        SaveBookEditChanges ->
            case ( model.currentBook, model.maybeUser ) of
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

                        bookList =
                            Utility.replaceIf (\runningBook -> runningBook.id == updatedBook.id) updatedBook model.bookList
                    in
                        ( { model | appState = ReadingMyBooks, currentBook = Just updatedBook, bookList = bookList }
                        , Request.updateBook updatedBook user.token
                        )

                ( _, _ ) ->
                    ( model, Cmd.none )

        BookIsUpdated (Ok str) ->
            ( model, Cmd.none )

        BookIsUpdated (Err err) ->
            ( model, Cmd.none )

        BookIsCreated (Ok str) ->
            ( model, Cmd.none )

        BookIsCreated (Err err) ->
            ( model, Cmd.none )

        ReceiveBookList (Ok bookList) ->
            let
                currentBook =
                    List.head bookList

                pagesRead =
                    case currentBook of
                        Nothing ->
                            0

                        Just book ->
                            book.pagesRead

                notes =
                    case currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.notes

                startDateString =
                    case currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.startDateString

                finishDateString =
                    case currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.finishDateString
            in
                ( { model
                    | bookList = bookList
                    , currentBook = currentBook
                    , totalPagesRead = computeTotalPagesRead bookList
                    , pagesRead = pagesRead
                    , notes = notes
                    , startDateString = startDateString
                    , finishDateString = finishDateString
                  }
                , Cmd.none
                )

        ReceiveBookList (Err err) ->
            ( { model | errorMessage = "Error receiveing book list" }, Cmd.none )

        ComputePagesRead result ->
            case result of
                Ok bookList ->
                    ( { model | totalPagesRead = computeTotalPagesRead bookList }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        RequestBookList userid token ->
            ( model, Request.getBookList userid token )

        -- ###
        GetCurrentUserBookList ->
            case model.maybeUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | appState = ReadingMyBooks, textRenderMode = MarkDownView }, Request.getBookList user.id user.token )

        InputTitle str ->
            ( { model | title = str }, Cmd.none )

        InputSubtitle str ->
            ( { model | subtitle = str }, Cmd.none )

        InputCategory str ->
            ( { model | category = str }, Cmd.none )

        InputAuthor str ->
            ( { model | author = str }, Cmd.none )

        InputPages str ->
            ( { model | pages = str |> String.toInt |> Maybe.withDefault 0 }, Cmd.none )

        InputPagesRead str ->
            let
                userid =
                    case model.maybeUser of
                        Nothing ->
                            0

                        Just user ->
                            user.id

                ( nextBook, deltaPages ) =
                    case model.currentBook of
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
                            Request.updateBook book (userToken model)

                updatedBookList =
                    case nextBook of
                        Nothing ->
                            model.bookList

                        Just book ->
                            Utility.replaceIf (\runningBook -> runningBook.id == book.id) book model.bookList
            in
                ( { model
                    | currentBook = nextBook
                    , pagesRead = str |> String.toInt |> Maybe.withDefault 0
                    , bookList = updatedBookList
                    , totalPagesRead = model.totalPagesRead + deltaPages
                  }
                , command
                )

        InputNotes str ->
            let
                updatedBook =
                    case model.currentBook of
                        Nothing ->
                            Nothing

                        Just book ->
                            Just { book | notes = str }

                command =
                    case updatedBook of
                        Nothing ->
                            Cmd.none

                        Just book ->
                            Request.updateBook book (userToken model)

                updatedBookList =
                    case updatedBook of
                        Nothing ->
                            model.bookList

                        Just book ->
                            Utility.replaceIf (\runningBook -> runningBook.id == book.id) book model.bookList
            in
                ( { model | currentBook = updatedBook, bookList = updatedBookList }, command )

        InputBlurb str ->
            let
                nextMaybeUser =
                    case model.maybeUser of
                        Nothing ->
                            Nothing

                        Just user ->
                            Just { user | blurb = str }
            in
                ( { model | maybeUser = nextMaybeUser }, Cmd.none )

        ToggleBookPublic val ->
            case model.currentBook of
                Nothing ->
                    ( model, Cmd.none )

                Just book ->
                    let
                        nextBook =
                            { book | public = not book.public }

                        command =
                            Request.updateBook nextBook (userToken model)

                        updatedBookList =
                            Utility.replaceIf (\runningBook -> runningBook.id == nextBook.id) nextBook model.bookList
                    in
                        ( { model | currentBook = Just nextBook, bookList = updatedBookList }, command )

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
                    case model.maybeUser of
                        Nothing ->
                            ""

                        Just user ->
                            user.token

                userid =
                    case model.maybeUser of
                        Nothing ->
                            0

                        Just user ->
                            user.id
            in
                ( { model
                    | previousCurrentBook = model.currentBook
                    , currentBook = Just newBook
                    , notes = newBook.notes
                    , pagesRead = newBook.pagesRead
                    , bookList = newBook :: model.bookList
                    , appState = ReadingMyBooks
                    , textRenderMode = PlainTextView
                  }
                , Request.createBook userid newBook token
                )

        SetCurrentBook book ->
            ( { model
                | currentBook = Just book
                , counter = model.counter + 1
                , startDateString = book.startDateString
                , finishDateString = book.finishDateString
              }
            , Cmd.none
            )

        UpdateCurrentBook ->
            let
                nextCurrentBook =
                    case model.currentBook of
                        Nothing ->
                            Nothing

                        Just book ->
                            (Just { book | pagesRead = model.pagesRead })

                bookList =
                    case nextCurrentBook of
                        Nothing ->
                            model.bookList

                        Just book ->
                            Utility.replaceIf (\runningBook -> runningBook.id == book.id) book model.bookList

                token =
                    case model.maybeUser of
                        Nothing ->
                            ""

                        Just user ->
                            user.token

                updateBookCmd =
                    case nextCurrentBook of
                        Nothing ->
                            Cmd.none

                        Just book ->
                            Request.updateBook book token
            in
                case nextCurrentBook of
                    Nothing ->
                        ( model, Cmd.none )

                    Just book ->
                        ( { model
                            | currentBook = nextCurrentBook
                            , bookList = bookList
                            , notes = book.notes
                            , pagesRead = book.pagesRead
                          }
                        , updateBookCmd
                        )

        UpdateBlurb ->
            ( model, Cmd.none )

        GetSharedBooks username ->
            case model.maybeUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | appState = SharingBooks username, textRenderMode = MarkDownView }
                    , Cmd.batch
                        [ Request.getSharedBooks username user.token
                        , Request.getSharedBlurb username user.token
                        ]
                    )

        ReceiveSharedBlurb (Ok str) ->
            ( { model | sharedBlurb = str }, Cmd.none )

        ReceiveSharedBlurb (Err err) ->
            ( model, Cmd.none )

        ToggleNotesViewMode ->
            let
                nextMode =
                    case model.notesViewMode of
                        NotesViewShort ->
                            NotesViewLong

                        NotesViewLong ->
                            NotesViewShort
            in
                ( { model | notesViewMode = nextMode }, Cmd.none )

        ToggleBlurbAndNotes ->
            case model.textDisplayMode of
                DisplayNotes ->
                    ( { model | counter = model.counter + 1, textDisplayMode = DisplayBlurb }, Cmd.none )

                DisplayBlurb ->
                    ( { model | counter = model.counter + 1, textDisplayMode = DisplayNotes }, Cmd.none )

        ToggleMarkdown ->
            case model.textRenderMode of
                PlainTextView ->
                    ( { model | textRenderMode = MarkDownView }, Cmd.none )

                MarkDownView ->
                    ( { model | textRenderMode = PlainTextView }, Cmd.none )

        InputStartDate str ->
            ( { model | startDateString = str }, Cmd.none )

        InputFinishDate str ->
            ( { model | finishDateString = str }, Cmd.none )

        ReceiveTime t ->
            ( { model | currentTime = t }, Cmd.none )


userToken model =
    case model.maybeUser of
        Nothing ->
            ""

        Just user ->
            user.token


computeTotalPagesRead : List Book -> Int
computeTotalPagesRead bookList =
    bookList |> List.map .pagesRead |> List.sum
