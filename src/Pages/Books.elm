module Pages.Books
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        , view
        , getBookListViaSharedState
        )

import Http
import Html
import Html.Attributes as HA
import Time exposing (Posix)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (required, hardcoded)
import Markdown
import Book.MarkdownExtra as MarkdownExtra
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Common.Style as Style
import Common.Utility as Utility
import Common.Indicator as Indicator
import Common.Days as Days
import User.Types exposing (User)
import Configuration


--
-- MODEL
--


type alias Model =
    { currentBook : Maybe Books
    , previousCurrentBook : Maybe Books
    , bookList : List Books
    , totalPagesRead : Int
    , dateStartedReadingString : String
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


type alias Books =
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


bookIsCompleted : Books -> Int
bookIsCompleted book =
    if book.finishDateString == "" then
        0
    else
        1


booksCompleted : List Books -> Int
booksCompleted bookList =
    bookList |> List.foldl (\book count -> count + (bookIsCompleted book)) 0



--
-- MSG
--


type Msg
    = ReceiveBookList (Result Http.Error (List Books))
    | ComputePagesRead (Result Http.Error (List Books))
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
    | SetCurrentBook Books
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



--
-- UPDATE
--


{-| NOTE that the Books udpdate function is of the usual
kind -- there is no SharedState parameter. Contrast
this with the update function for SettiÂngs.
-}
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

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
            , NoUpdate
            )

        CancelNewBook ->
            ( { model
                | appState = ReadingMyBooks
                , -- currentBook = model.previousCurrentBook,
                  previousCurrentBook = Nothing
              }
            , Cmd.none
            , NoUpdate
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
                , NoUpdate
                )

        SaveBookEditChanges ->
            case ( model.currentBook, sharedState.currentUser ) of
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
                        , NoUpdate
                        )

                ( _, _ ) ->
                    ( model, Cmd.none, NoUpdate )

        BookIsUpdated (Ok str) ->
            ( model, Cmd.none, NoUpdate )

        BookIsUpdated (Err err) ->
            ( model, Cmd.none, NoUpdate )

        BookIsCreated (Ok str) ->
            ( model, Cmd.none, NoUpdate )

        BookIsCreated (Err err) ->
            ( model, Cmd.none, NoUpdate )

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
                , NoUpdate
                )

        ReceiveBookList (Err err) ->
            ( { model | errorMessage = "Error receiveing book list" }, Cmd.none, NoUpdate )

        ComputePagesRead result ->
            case result of
                Ok bookList ->
                    ( { model | totalPagesRead = computeTotalPagesRead bookList }, Cmd.none, NoUpdate )

                Err _ ->
                    ( model, Cmd.none, NoUpdate )

        RequestBookList userid token ->
            ( model, getBookList userid token, NoUpdate )

        -- ###
        GetCurrentUserBookList ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    ( { model | appState = ReadingMyBooks, textRenderMode = MarkDownView }
                    , getBookList user.id user.token
                    , NoUpdate
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
                , NoUpdate
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
                ( { model | currentBook = updatedBook, bookList = updatedBookList }, command, NoUpdate )

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

        ToggleBookPublic val ->
            case model.currentBook of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just book ->
                    let
                        nextBook =
                            { book | public = not book.public }

                        command =
                            updateBook nextBook (userToken sharedState)

                        updatedBookList =
                            Utility.replaceIf (\runningBook -> runningBook.id == nextBook.id) nextBook model.bookList
                    in
                        ( { model | currentBook = Just nextBook, bookList = updatedBookList }, command, NoUpdate )

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
                    | previousCurrentBook = model.currentBook
                    , currentBook = Just newBook
                    , notes = newBook.notes
                    , pagesRead = newBook.pagesRead
                    , bookList = newBook :: model.bookList
                    , appState = ReadingMyBooks
                    , textRenderMode = PlainTextView
                  }
                , createBook userid newBook token
                , NoUpdate
                )

        SetCurrentBook book ->
            ( { model
                | currentBook = Just book
                , counter = model.counter + 1
                , startDateString = book.startDateString
                , finishDateString = book.finishDateString
              }
            , Cmd.none
            , NoUpdate
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
                            | currentBook = nextCurrentBook
                            , bookList = bookList
                            , notes = book.notes
                            , pagesRead = book.pagesRead
                          }
                        , updateBookCmd
                        , NoUpdate
                        )

        UpdateBlurb ->
            ( model, Cmd.none, NoUpdate )

        GetSharedBooks username ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    ( { model | appState = SharingBooks username, textRenderMode = MarkDownView }
                    , Cmd.batch
                        [ getSharedBooks username user.token
                        , getSharedBlurb username user.token
                        ]
                    , NoUpdate
                    )

        ReceiveSharedBlurb (Ok str) ->
            ( { model | sharedBlurb = str }, Cmd.none, NoUpdate )

        ReceiveSharedBlurb (Err err) ->
            ( model, Cmd.none, NoUpdate )

        ToggleNotesViewMode ->
            let
                nextMode =
                    case model.notesViewMode of
                        NotesViewShort ->
                            NotesViewLong

                        NotesViewLong ->
                            NotesViewShort
            in
                ( { model | notesViewMode = nextMode }, Cmd.none, NoUpdate )

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

        InputStartDate str ->
            ( { model | startDateString = str }, Cmd.none, NoUpdate )

        InputFinishDate str ->
            ( { model | finishDateString = str }, Cmd.none, NoUpdate )



--
-- VIEW
--


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill)
        [ bookListDisplay sharedState model
        , footer sharedState model
        ]


bookListDisplay sharedState model =
    Element.row []
        [ bookListTable sharedState model
        , notesViewedAsMarkdown model
        ]


bookListTable sharedState model =
    Element.column
        [ width fill
        , height (px 650)
        , spacing 10
        , padding 10
        , Background.color Style.charcoal
        , Font.color Style.white
        ]
        [ bookListTableHeader sharedState model
        , listBooks model
        ]


bookListTableHeader : SharedState -> Model -> Element Msg
bookListTableHeader sharedState model =
    Element.row [ spacing 15, Background.color Style.charcoal, Font.color Style.white ]
        [ Element.el [ Font.bold, Font.color Style.white ] (text <| bookInfo model)
        , Element.el [ Font.size 14, Font.color Style.orange ] (text <| totalsString sharedState model)
        ]



--
-- MARKDOWN
--


notesViewedAsMarkdown : Model -> Element msg
notesViewedAsMarkdown model =
    case model.currentBook of
        Nothing ->
            Element.none

        Just book ->
            Element.html <| Html.div (markdownStyle model) <| [ MarkdownExtra.view book.notes ]


markdownStyle model =
    [ HA.style "height" "630px"
    , HA.style "width" "500px"
    , HA.style "font-size" "12px"
    , HA.style "line-height" "15px"
    , HA.style "overflow-y" "scroll"
    , HA.style "overflow-x" "hidden"
    , HA.style "padding-top" "20px"
    , HA.style "background-color" "#f7f6f4"
    , HA.style "padding-left" "15px"
    ]


listBooks model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px 400)
        , Background.color Style.charcoal
        , Font.color Style.white
        , clipX
        ]
        { data = model.bookList
        , columns =
            [ { header = Element.el Style.tableHeading (Element.text "Title")
              , width = px 200
              , view =
                    \book ->
                        titleButton book model.currentBook
              }
            , { header = Element.el Style.tableHeading (Element.text "Author")
              , width = px 150
              , view =
                    \book ->
                        Element.text book.author
              }
            , { header = Element.el Style.tableHeading (Element.text "Category")
              , width = px 150
              , view =
                    \book ->
                        Element.text book.category
              }
            , { header = Element.el Style.tableHeading (Element.text "")
              , width = px 110
              , view =
                    \book ->
                        Element.el [] (Indicator.indicator 100 10 "orange" (pageRatio book))
              }
            , { header = Element.el Style.tableHeading (Element.text "Progress")
              , width = px 110
              , view =
                    \book ->
                        Element.text (pageInfo book)
              }
            ]
        }


bookInfo : Model -> String
bookInfo model =
    "Books: " ++ (String.fromInt <| booksCompleted model.bookList) ++ "/" ++ (String.fromInt <| List.length model.bookList)


pageInfo book =
    let
        pp =
            (String.fromInt book.pagesRead) ++ "/" ++ (String.fromInt book.pages)

        pc =
            String.fromInt <| Basics.round <| 100 * (Basics.toFloat book.pagesRead) / (Basics.toFloat book.pages)
    in
        pp ++ " (" ++ pc ++ "%)"


totalsString : SharedState -> Model -> String
totalsString sharedState model =
    let
        daysElapsed =
            Days.fromUSDate model.beginningDate (Utility.toUtcDateString <| Just sharedState.currentTime)

        pagesReadPerDay =
            Basics.round ((Basics.toFloat model.totalPagesRead) / (Basics.toFloat daysElapsed))
    in
        (String.fromInt model.totalPagesRead)
            ++ " pages since "
            ++ model.beginningDate
            ++ " — "
            ++ (String.fromInt pagesReadPerDay)
            ++ " pp/day"


bookListDisplayWidth model =
    (px 780)


pageRatio book =
    (toFloat book.pagesRead) / (toFloat book.pages)


titleButton book maybeCurrentBook =
    let
        highlighted =
            case maybeCurrentBook of
                Nothing ->
                    False

                Just currentBook ->
                    currentBook.id == book.id
    in
        Input.button (Style.titleButton highlighted)
            { onPress = Just (SetCurrentBook book)
            , label = Element.text book.title
            }


getBooksButton =
    Input.button Style.button
        { onPress = Just GetCurrentUserBookList
        , label = Element.text "Get books"
        }



--
-- FOOTER
--


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| userStatus sharedState.currentUser)
        , el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        ]



--
-- Cmd Msg (HTTP)
--


getBookListViaSharedState : SharedState -> Cmd Msg
getBookListViaSharedState sharedState =
    case sharedState.currentUser of
        Nothing ->
            Cmd.none

        Just user ->
            getBookList user.id user.token


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


updateBook : Books -> String -> Cmd Msg
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


createBook : Int -> Books -> String -> Cmd Msg
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


doRequestBookList : Maybe User -> Msg
doRequestBookList user_ =
    case user_ of
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


bookRecordEncoder : Books -> Encode.Value
bookRecordEncoder book =
    Encode.object
        [ ( "book", bookEncoder book )
        ]


bookEncoder : Books -> Encode.Value
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


newBookRecordEncoder : Int -> Books -> Encode.Value
newBookRecordEncoder userid book =
    Encode.object
        [ ( "book", newBookEncoder userid book )
        ]


newBookEncoder : Int -> Books -> Encode.Value
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


bookDecoder : Decode.Decoder Books
bookDecoder =
    Decode.succeed Books
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


bookListDecoder : Decode.Decoder (List Books)
bookListDecoder =
    Decode.field "data" (Decode.list bookDecoder)



--
-- REQUEST
--
--
-- HELPERS
--


computeTotalPagesRead : List Books -> Int
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
