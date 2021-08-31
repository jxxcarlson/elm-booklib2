module Pages.Books exposing
    ( Model
    , Msg(..)
    , getBookList
    , getBookListViaSharedState
    , init
    , update
    , view
    )

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
import Http
import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Coders
import User.Types exposing (User)
import File.Download as Download
import Codec


--
-- MODEL
--


type alias Model =
    { bookList : List Book
    , totalPagesRead : Int
    , pagesRead : Int
    , notes : String
    , finishDateString : String
    , errorMessage : String
    , startDateString : String
    , sortOrder : SortOrder
    }


init : Model
init =
    { bookList = []
    , totalPagesRead = 0
    , pagesRead = 0
    , notes = ""
    , errorMessage = ""
    , finishDateString = ""
    , startDateString = ""
    , sortOrder = NormalSortOrder
    }


type SortOrder
    = NormalSortOrder
    | SortById
    | SortByCategory
    | SortByTitle
    | SortByAuthor


bookIsCompleted : Book -> Int
bookIsCompleted book =
    if book.finishDateString == "" then
        0

    else
        1


booksCompleted : List Book -> Int
booksCompleted bookList =
    bookList |> List.foldl (\book count -> count + bookIsCompleted book) 0



--
-- MSG
--


type Msg
    = ReceiveBookList (Result Http.Error (List Book))
    | ComputePagesRead (Result Http.Error (List Book))
    | RequestBookList Int String
    | SetCurrentBook Book
    | GetSharedBooks String
    | GetCurrentUserBookList
    | SetSortOrder SortOrder
    | ExportJson
    | NoOp



--
-- UPDATE
--


{-| NOTE that the Book udpdate function is of the usual
kind -- there is no SharedState parameter. Contrast
this with the update function for SettiÂngs.
-}
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
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
                , totalPagesRead = computeTotalPagesRead bookList
                , pagesRead = pagesRead
                , notes = notes
                , startDateString = startDateString
                , finishDateString = finishDateString
              }
            , Cmd.none
            , SharedState.UpdateCurrentBook currentBook
            )

        ReceiveBookList (Err err) ->
            ( { model | errorMessage = "Error receiving book list" }, Cmd.none, SharedState.UpdateCurrentBook Nothing )

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
                    ( model
                    , getBookList user.id user.token
                    , NoUpdate
                    )

        SetCurrentBook book ->
            case Just book.id == Maybe.map .id sharedState.currentBook of
                True ->
                    ( model, pushUrl sharedState.navKey "#currentbook", NoUpdate )

                False ->
                    ( { model
                        | startDateString = book.startDateString
                        , finishDateString = book.finishDateString
                      }
                    , Cmd.none
                    , SharedState.UpdateCurrentBook (Just book)
                    )

        GetSharedBooks username ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    ( model
                    , Cmd.batch
                        [ getSharedBooks username user.token

                        -- , getSharedBlurb username user.token
                        ]
                    , NoUpdate
                    )
        ExportJson -> (model, exportJson model, NoUpdate)

        SetSortOrder sortOrder ->
            ( { model | sortOrder = sortOrder }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            phoneView sharedState model

        _ ->
            mainView sharedState model


mainView : SharedState -> Model -> Element Msg
mainView sharedState model =
    column (Style.mainColumn fill fill)
        [ bookListDisplayMain sharedState model
        , footer sharedState model
        ]


phoneView : SharedState -> Model -> Element Msg
phoneView sharedState model =
    column (Style.mainColumn fill fill)
        [ bookListDisplayPhone sharedState model
        , footerForPhone sharedState model
        ]


bookListDisplayMain sharedState model =
    Element.row []
        [ bookListTable sharedState model
        , case matchBookAndUserIds sharedState of
            False ->
                Element.none

            True ->
                column [ Border.width 1, moveRight 12 ] [ Common.Book.notesViewedAsMarkdown 70 "400px" (notesHeight sharedState) sharedState.currentBook ]
        ]


bookListDisplayPhone sharedState model =
    Element.row []
        [ bookListTable sharedState model
        ]


notesHeight sharedState =
    String.fromInt (sharedState.windowHeight - verticalMargin - 20) ++ "px"


matchBookAndUserIds : SharedState -> Bool
matchBookAndUserIds sharedState =
    let
        uid =
            Maybe.map .id sharedState.currentUser

        bookUid =
            Maybe.map .userId sharedState.currentBook
    in
    case ( uid, bookUid ) of
        ( Just id1, Just id2 ) ->
            id1 == id2

        ( _, _ ) ->
            False


verticalMargin : Int
verticalMargin =
    100


bookListTable sharedState model =
    Element.column
        [ width fill
        , height (px (sharedState.windowHeight - verticalMargin))
        , spacing 18
        , padding 10
        , Background.color Style.charcoal
        , Font.color Style.white
        ]
        [ bookListTableHeader sharedState model
        , listBooks sharedState model
        ]


bookListTableHeader : SharedState -> Model -> Element Msg
bookListTableHeader sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            column [ spacing 6 ]
                [ row [ spacing 15, Background.color Style.charcoal, Font.color Style.white ]
                    [ Element.el [ Font.bold, Font.color Style.white ] (text <| bookInfo model)
                    ]
                , Element.el [ Font.size 14, Font.color Style.orange ] (text <| totalsStringPhone sharedState model)
                , Element.el [ Font.size 14, Font.color Style.orange ] (text <| totalsStringPhone2 sharedState model)
                ]

        _ ->
            Element.row [ spacing 15, Background.color Style.charcoal, Font.color Style.white ]
                [ Element.el [ Font.bold, Font.color Style.white ] (text <| bookInfo model)
                , Element.el [ Font.size 14, Font.color Style.orange ] (text <| totalsString sharedState model)
                ]



--
-- MARKDOWN
--


listBooks sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            listBooksForPhone sharedState model

        _ ->
            listBooksMain sharedState model


sortBooks : SortOrder -> List ( Int, Book ) -> List ( Int, Book )
sortBooks sortOrder books =
    case sortOrder of
        NormalSortOrder ->
            books

        SortById ->
            List.sortBy (\b -> Tuple.second b |> .id) books

        SortByCategory ->
            List.sortBy (\b -> Tuple.second b |> .category) books

        SortByTitle ->
            List.sortBy (\b -> Tuple.second b |> .title) books

        SortByAuthor ->
            List.sortBy (\b -> Tuple.second b |> .author) books


titleHeadingButton : Model -> Element Msg
titleHeadingButton model =
    Input.button (Style.titleButton2 (model.sortOrder == SortByTitle))
        { onPress = Just (SetSortOrder SortByTitle)
        , label = Element.text "Title"
        }


indexButton : Model -> Element Msg
indexButton model =
    Input.button (Style.titleButton2 (model.sortOrder == NormalSortOrder))
        { onPress = Just (SetSortOrder NormalSortOrder)
        , label = Element.text "N"
        }


authorButton : Model -> Element Msg
authorButton model =
    Input.button (Style.titleButton2 (model.sortOrder == SortByAuthor))
        { onPress = Just (SetSortOrder SortByAuthor)
        , label = Element.text "Author"
        }


categoryButton : Model -> Element Msg
categoryButton model =
    Input.button (Style.titleButton2 (model.sortOrder == SortByCategory))
        { onPress = Just (SetSortOrder SortByCategory)
        , label = Element.text "Category"
        }


listBooksMain sharedState model =
    let
        nBooks =
            List.length model.bookList
    in
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px (sharedState.windowHeight - verticalMargin - 60))
        , Background.color Style.charcoal
        , Font.color Style.white
        , clipX
        ]
        { data = sortBooks model.sortOrder (List.indexedMap Tuple.pair model.bookList)
        , columns =
            [ { header = Element.el (Style.tableHeading ++ [ clipX ]) (indexButton model)
              , width = px 20
              , view =
                    \p ->
                        el [] (text <| String.fromInt <| nBooks - Tuple.first p)
              }
            , { header = Element.el (Style.tableHeading ++ [ clipX ]) (text "S")
              , width = px 20
              , view =
                    \p ->
                        displayShareStatus (Tuple.second p)
              }
            , { header = Element.el (Style.tableHeading ++ [ clipX ]) (titleHeadingButton model)
              , width = px 200
              , view =
                    \p ->
                        titleButton (Tuple.second p) sharedState.currentBook
              }
            , { header = Element.el Style.tableHeading (authorButton model)
              , width = px 150
              , view =
                    \p ->
                        el [ clipX ] (Element.text (Tuple.second p).author)
              }
            , { header = Element.el Style.tableHeading (categoryButton model)
              , width = px 150
              , view =
                    \p ->
                        el [ clipX ] (Element.text (Tuple.second p).category)
              }
            , { header = Element.el Style.tableHeading (Element.text "")
              , width = px 110
              , view =
                    \p ->
                        Element.el [] (Indicator.indicator 100 10 "orange" (pageRatio (Tuple.second p)))
              }
            , { header = Element.el Style.tableHeading (el [ moveRight 16 ] (Element.text "Progress"))
              , width = px 80
              , view =
                    \p ->
                        el [] (el [ alignRight, paddingXY 8 0 ] (Element.text (pageInfo (Tuple.second p))))
              }
            , { header = Element.el Style.tableHeading (el [ moveRight 9 ] (Element.text "%%"))
              , width = px 40
              , view =
                    \p ->
                        el [] (el [ alignRight, paddingXY 8 0 ] (Element.text (pageInfo2 (Tuple.second p))))
              }
            ]
        }


displayShareStatus book =
    case book.public of
        True ->
            el [ Font.color Style.white ] (text "s")

        False ->
            el [ Font.color Style.white, Font.bold ] (text "•")


listBooksForPhone sharedState model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px (sharedState.windowHeight - 190))
        , width fill
        , Background.color Style.charcoal
        , Font.color Style.white
        , clipX
        ]
        { data = model.bookList
        , columns =
            [ { header = Element.el (Style.tableHeading ++ [ clipX ]) (Element.text "Title")
              , width = px <| round <| 0.65 * toFloat sharedState.windowWidth - 20
              , view =
                    \book ->
                        titleButton book sharedState.currentBook
              }
            , { header = Element.el Style.tableHeading (Element.text "")
              , width = px 110
              , view =
                    \book ->
                        Element.el [] (Indicator.indicator 100 10 "orange" (pageRatio book))
              }
            ]
        }


bookInfo : Model -> String
bookInfo model =
    "Books: " ++ (String.fromInt <| booksCompleted model.bookList) ++ "/" ++ (String.fromInt <| List.length model.bookList)


pageInfo book =
    String.fromInt book.pagesRead ++ "/" ++ String.fromInt book.pages


pageInfo2 book =
    (String.fromInt <| Basics.round <| 100 * Basics.toFloat book.pagesRead / Basics.toFloat book.pages) ++ "%"


totalsString : SharedState -> Model -> String
totalsString sharedState model =
    let
        startDate =
            case sharedState.currentUser of
                Nothing ->
                    "6/1/2018"

                Just user ->
                    user.beginningDate

        nBooksRead : Float
        nBooksRead =
            toFloat <| booksCompleted model.bookList

        daysElapsed : Float
        daysElapsed =
            toFloat <| Days.fromUSDate startDate (Utility.toUtcDateString <| Just sharedState.currentTime)

        booksReadPerMonth : Float
        booksReadPerMonth =
            Utility.roundTo 1 <| nBooksRead / (daysElapsed / 30.5)

        booksReadPerMonthString : String
        booksReadPerMonthString =
            case round booksReadPerMonth == 1 of
                True ->
                    String.fromFloat booksReadPerMonth ++ " book/month"

                False ->
                    String.fromFloat booksReadPerMonth ++ " books/month"

        pagesReadPerDay =
            String.fromFloat <| Utility.roundTo 1 (Basics.toFloat model.totalPagesRead / daysElapsed)
    in
    Utility.stringFromIntWithCommas model.totalPagesRead
        ++ " pages since "
        ++ startDate
        ++ " — "
        ++ pagesReadPerDay
        ++ " pp/day"
        ++ " — "
        ++ booksReadPerMonthString


totalsStringPhone : SharedState -> Model -> String
totalsStringPhone sharedState model =
    let
        startDate =
            case sharedState.currentUser of
                Nothing ->
                    "6/1/2018"

                Just user ->
                    user.beginningDate
    in
    String.fromInt model.totalPagesRead
        ++ " pages since "
        ++ startDate


totalsStringPhone2 : SharedState -> Model -> String
totalsStringPhone2 sharedState model =
    let
        startDate =
            case sharedState.currentUser of
                Nothing ->
                    "6/1/2018"

                Just user ->
                    user.beginningDate

        nBooksRead : Float
        nBooksRead =
            toFloat <| booksCompleted model.bookList

        daysElapsed : Float
        daysElapsed =
            toFloat <| Days.fromUSDate startDate (Utility.toUtcDateString <| Just sharedState.currentTime)

        booksReadPerMonth : String
        booksReadPerMonth =
            String.fromFloat <| Utility.roundTo 1 <| nBooksRead / (daysElapsed / 30.5)

        pagesReadPerDay =
            String.fromFloat <| Utility.roundTo 1 (Basics.toFloat model.totalPagesRead / daysElapsed)
    in
    pagesReadPerDay
        ++ " pp/day"
        ++ " — "
        ++ booksReadPerMonth
        ++ " books/month"


bookListDisplayWidth model =
    px 780


pageRatio book =
    toFloat book.pagesRead / toFloat book.pages


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


fstyle sharedState =
    [ spacing 24, Background.color Style.charcoal, paddingXY 12 8, alignBottom, Font.size 14, width (px (sharedState.windowWidth - 15)) ]


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row (fstyle sharedState)
        [ el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        , el Style.footerItem (text <| userStatus sharedState.currentUser)
        , wordCountOfCurrentNotes sharedState
        , userBeginningDate sharedState
        , exportJsonButton
        ]


exportJson : Model -> Cmd msg
exportJson model =
    Download.string "books.json" "text/json" (Codec.encodeData model.bookList)


exportJsonButton : Element Msg
exportJsonButton =
    buttonTemplate ExportJson "Export"

buttonTemplate :  msg -> String -> Element msg
buttonTemplate  msg label_ =
   Input.button buttonStyle
            { onPress = Just msg
            , label = el [ Font.size 14 ] (text label_)
            }

buttonStyle : List (Element.Attr () msg)
buttonStyle =
    [ Font.color (Element.rgb255 255 255 255)
    , Element.paddingXY 15 8
    ]


footerForPhone : SharedState -> Model -> Element Msg
footerForPhone sharedState model =
    row (fstyle sharedState)
        [ el Style.footerItem (text <| userStatus sharedState.currentUser)
        , userBeginningDate sharedState
        ]


userBeginningDate : SharedState -> Element Msg
userBeginningDate sharedState =
    case sharedState.currentUser of
        Nothing ->
            Element.none

        Just user ->
            el Style.footerItem (text <| "Joined: " ++ user.beginningDate)


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
        , url = Configuration.backend ++ "/api/books?userid=" ++ String.fromInt userid
        , body = Http.jsonBody (User.Coders.tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


computePagesRead : Int -> String -> Cmd Msg
computePagesRead userid token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?userid=" ++ String.fromInt userid
        , body = Http.jsonBody (User.Coders.tokenEncoder token)
        , expect = Http.expectJson ComputePagesRead Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getSharedBooks : String -> String -> Cmd Msg
getSharedBooks username token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?shared=" ++ username
        , body = Http.jsonBody (User.Coders.tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


doRequestBookList : Maybe User -> Msg
doRequestBookList user_ =
    case user_ of
        Nothing ->
            NoOp

        Just author ->
            RequestBookList author.id author.token



--
-- REQUEST
--
--
-- HELPERS
--


computeTotalPagesRead : List Book -> Int
computeTotalPagesRead bookList =
    bookList |> List.map .pagesRead |> List.sum
