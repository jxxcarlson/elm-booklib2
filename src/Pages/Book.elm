module Pages.Book
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        )

import Http
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (required, hardcoded)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Common.Style as Style
import Common.Utility as Utility
import User.Types exposing (User)
import Configuration


type alias Model =
    { currentBook : Maybe Book
    , previousCurrentBook : Maybe Book
    , bookList : List Book
    , totalPagesRead : Int
    , dateStartedReadingString : String
    , maybeUser : Maybe User
    , title : String
    , subtitle : String
    , author : String
    , pages : Int
    , pagesRead : Int
    , notes : String
    , category : String
    , blurb : String
    , sharedBlurb : String
    , startDateString : String
    , finishDateString : String
    , errorMessage : String
    , appState : AppState
    , notesViewMode : NotesViewMode
    , textDisplayMode : TextDisplayMode
    , textRenderMode : TextRenderMode
    , counter : Int
    , currentTime : Maybe Posix
    , beginningDate : String
    }


init : Model
init =
    { currentBook = Nothing
    , previousCurrentBook = Nothing
    , bookList = []
    , totalPagesRead = 0
    , dateStartedReadingString = ""
    , maybeUser = Nothing
    , title = ""
    , subtitle = ""
    , author = ""
    , pages = 0
    , pagesRead = 0
    , notes = ""
    , blurb = ""
    , sharedBlurb = ""
    , category = ""
    , errorMessage = ""
    , startDateString = ""
    , finishDateString = ""
    , appState = ReadingMyBooks
    , notesViewMode = NotesViewShort
    , textDisplayMode = DisplayNotes
    , textRenderMode = MarkDownView
    , counter = 0
    , currentTime = Nothing
    , beginningDate = ""
    }


type AppState
    = CreatingNewBook
    | ReadingMyBooks
    | EditingBook
    | SharingBooks String


type TextDisplayMode
    = DisplayNotes
    | DisplayBlurb


type NotesViewMode
    = NotesViewShort
    | NotesViewLong


type TextRenderMode
    = PlainTextView
    | MarkDownView


type alias Book =
    { id : Int
    , title : String
    , subtitle : String
    , author : String
    , notes : String
    , pages : Int
    , pagesRead : Int
    , rating : Int
    , public : Bool
    , category : String
    , startDateString : String
    , finishDateString : String
    }


bookIsCompleted : Book -> Int
bookIsCompleted book =
    if book.finishDateString == "" then
        0
    else
        1


booksCompleted : List Book -> Int
booksCompleted bookList =
    bookList |> List.foldl (\book count -> count + (bookIsCompleted book)) 0


type Msg
    = ReceiveBookList (Result Http.Error (List Book))
    | ComputePagesRead (Result Http.Error (List Book))
    | RequestBookList Int String
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
    | SetCurrentBook Book
    | UpdateCurrentBook
    | UpdateBlurb
    | BookIsUpdated (Result Http.Error String)
    | NewBook
    | CancelNewBook
    | CreateNewBook
    | EditBook
    | SaveBookEditChanges
    | BookIsCreated (Result Http.Error String)
    | ToggleBookPublic Bool
    | GetSharedBooks String
    | ReceiveSharedBlurb (Result Http.Error String)
    | GetCurrentUserBookList
    | ToggleNotesViewMode
    | ToggleBlurbAndNotes
    | ToggleMarkdown
    | NoOp


{-| NOTE that the Book udpdate function is of the usual
kind -- there is no SharedState parameter. Contrast
this with the update function for SettiÂngs.
-}
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg )
update sharedState msg model =
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
                        , updateBook updatedBook user.token
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
            ( model, getBookList userid token )

        -- ###
        GetCurrentUserBookList ->
            case model.maybeUser of
                Nothing ->
                    ( model, Cmd.none )

                Just user ->
                    ( { model | appState = ReadingMyBooks, textRenderMode = MarkDownView }, getBookList user.id user.token )

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
                            updateBook book (userToken sharedState)

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
                            updateBook book (userToken sharedState)

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
                            updateBook nextBook (userToken sharedState)

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
                , createBook userid newBook token
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
                            updateBook book token
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
                        [ getSharedBooks username user.token
                        , getSharedBlurb username user.token
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


view : SharedState -> Model -> Element Msg
view sharedState model =
    column Style.mainColumn
        [ el [ Font.size 24, Font.bold ] (text "Book page")
        , el [ Font.size 16 ] (text <| "Counter = " ++ String.fromInt model.counter)
        , footer sharedState model
        ]


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| userStatus sharedState.currentUser)
        , el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        ]



--
-- Cmd Msg (HTTP)
--


getBookList : Int -> String -> Cmd Msg
getBookList userid token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?userid=" ++ (String.fromInt userid)
        , body = Http.jsonBody (tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


computePagesRead : Int -> String -> Cmd Msg
computePagesRead userid token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?userid=" ++ (String.fromInt userid)
        , body = Http.jsonBody (tokenEncoder token)
        , expect = Http.expectJson ComputePagesRead bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getSharedBooks : String -> String -> Cmd Msg
getSharedBooks username token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?shared=" ++ username
        , body = Http.jsonBody (tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getSharedBlurb : String -> String -> Cmd Msg
getSharedBlurb username token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/blurb/" ++ username
        , body = Http.jsonBody (tokenEncoder token)
        , expect = Http.expectJson ReceiveSharedBlurb blurbDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateBook : Book -> String -> Cmd Msg
updateBook book token =
    Http.request
        { method = "Put"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/books/" ++ (String.fromInt book.id)
        , body = Http.jsonBody (bookRecordEncoder book)
        , expect = Http.expectJson BookIsUpdated statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


createBook : Int -> Book -> String -> Cmd Msg
createBook userid book token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/books"
        , body = Http.jsonBody (newBookRecordEncoder userid book)
        , expect = Http.expectJson BookIsCreated statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--- NN


doRequestBookList : Model -> Msg
doRequestBookList model =
    case model.maybeUser of
        Nothing ->
            NoOp

        Just author ->
            RequestBookList author.id author.token



--
-- ENCODERS AND DECODERS
--


tokenEncoder : String -> Encode.Value
tokenEncoder token =
    Encode.object
        [ ( "token", Encode.string token )
        ]


bookRecordEncoder : Book -> Encode.Value
bookRecordEncoder book =
    Encode.object
        [ ( "book", bookEncoder book )
        ]


bookEncoder : Book -> Encode.Value
bookEncoder book =
    Encode.object
        [ ( "id", Encode.int book.id )
        , ( "title", Encode.string book.title )
        , ( "subtitle", Encode.string book.subtitle )
        , ( "author", Encode.string book.author )
        , ( "notes", Encode.string book.notes )
        , ( "pages", Encode.int book.pages )
        , ( "pages_read", Encode.int book.pagesRead )
        , ( "rating", Encode.int book.rating )
        , ( "public", Encode.bool book.public )
        , ( "category", Encode.string book.category )
        , ( "start_date_string", Encode.string book.startDateString )
        , ( "finish_date_string", Encode.string book.finishDateString )
        ]


newBookRecordEncoder : Int -> Book -> Encode.Value
newBookRecordEncoder userid book =
    Encode.object
        [ ( "book", newBookEncoder userid book )
        ]


newBookEncoder : Int -> Book -> Encode.Value
newBookEncoder userid book =
    Encode.object
        [ ( "id", Encode.int book.id )
        , ( "user_id", Encode.int userid )
        , ( "title", Encode.string book.title )
        , ( "subtitle", Encode.string book.subtitle )
        , ( "author", Encode.string book.author )
        , ( "notes", Encode.string book.notes )
        , ( "pages", Encode.int book.pages )
        , ( "pages_read", Encode.int book.pagesRead )
        , ( "rating", Encode.int book.rating )
        , ( "public", Encode.bool book.public )
        , ( "category", Encode.string book.category )
        , ( "start_date_string", Encode.string book.startDateString )
        ]


bookDecoder : Decode.Decoder Book
bookDecoder =
    Decode.succeed Book
        |> required "id" Decode.int
        |> required "title" Decode.string
        |> required "subtitle" Decode.string
        |> required "author" Decode.string
        |> required "notes" Decode.string
        |> required "pages" Decode.int
        |> required "pagesRead" Decode.int
        |> required "rating" Decode.int
        |> required "public" Decode.bool
        |> required "category" Decode.string
        |> required "startDateString" Decode.string
        |> required "finishDateString" Decode.string


blurbDecoder : Decode.Decoder String
blurbDecoder =
    Decode.field "blurb" Decode.string


statusDecoder : Decode.Decoder String
statusDecoder =
    Decode.field "status" Decode.string


bookListDecoder : Decode.Decoder (List Book)
bookListDecoder =
    Decode.field "data" (Decode.list bookDecoder)



--
-- REQUEST
--
--
-- HELPERS
--


computeTotalPagesRead : List Book -> Int
computeTotalPagesRead bookList =
    bookList |> List.map .pagesRead |> List.sum


userToken : SharedState -> String
userToken sharedState =
    case sharedState.currentUser of
        Nothing ->
            ""

        Just user ->
            user.token


userStatus : Maybe User -> String
userStatus user_ =
    case user_ of
        Nothing ->
            "Not signed in."

        Just user ->
            "Signed is as " ++ user.username
