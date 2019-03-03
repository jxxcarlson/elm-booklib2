module Pages.SharedBooks exposing
    ( Model
    , Msg(..)
    , getBookList
    , getBookListViaSharedState
    , getPublicUsers
    , init
    , update
    , view
    )

import Book.Coders
import Book.Types exposing (Book)
import Common.Book
import Common.Days as Days
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
import User.Session
import User.Types exposing (AnnotatedUser, PublicUser, User)



--
-- MODEL
--


type alias Model =
    { bookList : List Book
    , publicUserList : List PublicUser
    , annotatedUserList : List AnnotatedUser
    , currentPublicUser : Maybe PublicUser
    , totalPagesRead : Int
    , pagesRead : Int
    , notes : String
    , finishDateString : String
    , errorMessage : String
    , startDateString : String
    }


init : Model
init =
    { bookList = []
    , publicUserList = []
    , annotatedUserList = []
    , currentPublicUser = Nothing
    , totalPagesRead = 0
    , pagesRead = 0
    , notes = ""
    , errorMessage = ""
    , finishDateString = ""
    , startDateString = ""
    }


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
    | ReceivePublicUsers (Result Http.Error (List PublicUser))
    | ReceiveUpdateUser (Result Http.Error String)
    | GetAnnotatedUserList
    | ReceiveAnnotatedUserList (Result Http.Error (List AnnotatedUser))
    | RequestBookList Int String
    | SetCurrentBook Book
    | GetSharedBooks String
      -- | ReceiveSharedBlurb (Result Http.Error String)
    | GetCurrentUserBookList
    | NoOp
      --
    | FollowUser String



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
            ( { model | errorMessage = "Error receiveing book list" }, Cmd.none, NoUpdate )

        ComputePagesRead result ->
            case result of
                Ok bookList ->
                    ( { model | totalPagesRead = computeTotalPagesRead bookList }, Cmd.none, NoUpdate )

                Err _ ->
                    ( model, Cmd.none, NoUpdate )

        RequestBookList userid token ->
            ( model, getBookList userid token, NoUpdate )

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
                    ( { model | currentPublicUser = Just { username = username } }
                    , Cmd.batch
                        [ getSharedBooks username user.token
                        ]
                    , NoUpdate
                    )

        ReceivePublicUsers (Ok publicUserList_) ->
            let
                currentUsername =
                    case sharedState.currentUser of
                        Nothing ->
                            "-"

                        Just user ->
                            user.username

                publicUserList =
                    List.filter (\pu -> pu.username /= currentUsername) publicUserList_
            in
            ( { model | publicUserList = publicUserList }, Cmd.none, NoUpdate )

        ReceivePublicUsers (Err _) ->
            ( { model | errorMessage = "ERROR, Cant't get public users" }, Cmd.none, NoUpdate )

        FollowUser newFollowedUserName ->
            let
                ( currentUser, cmd ) =
                    case sharedState.currentUser of
                        Nothing ->
                            ( Nothing, Cmd.none )

                        Just user ->
                            let
                                queryString =
                                    if List.member { username = newFollowedUserName } user.follow then
                                        "unfollow_user=" ++ newFollowedUserName

                                    else
                                        "follow_user=" ++ newFollowedUserName

                                updatedUser =
                                    { user | follow = Utility.toggleList { username = newFollowedUserName } user.follow }
                            in
                            ( Just updatedUser, updateUserWithQS user queryString updatedUser.token )
            in
            ( model, cmd, SharedState.UpdateCurrentUser currentUser )

        ReceiveUpdateUser (Ok status_) ->
            ( { model | errorMessage = status_ }
            , Cmd.none
            , NoUpdate
            )

        ReceiveUpdateUser (Err _) ->
            ( { model | errorMessage = "Status error" }, Cmd.none, NoUpdate )

        GetAnnotatedUserList ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    ( model, getAnnotatedUsers user.token, NoUpdate )

        ReceiveAnnotatedUserList (Ok annotatedUserList) ->
            ( { model | annotatedUserList = annotatedUserList }, Cmd.none, NoUpdate )

        ReceiveAnnotatedUserList (Err err) ->
            ( { model | errorMessage = "ERROR, Cant't get annotated user list" }, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill)
        [ bookListDisplay sharedState model
        , footer sharedState model
        ]


bookListDisplay : SharedState -> Model -> Element Msg
bookListDisplay sharedState model =
    Element.row [ spacing 20 ]
        [ sharedUserDisplay sharedState model
        , bookListTable sharedState model
        , case matchBookAndUserIds sharedState of
            False ->
                Element.none

            True ->
                column [ Border.width 1, moveRight 12 ] [ Common.Book.notesViewedAsMarkdown "400px" "579px" sharedState.currentBook ]
        ]


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
            id1 /= id2

        ( _, _ ) ->
            False


sharedUserDisplay : SharedState -> Model -> Element Msg
sharedUserDisplay sharedState model =
    Element.column
        [ width (px 400)
        , height (px 600)
        , spacing 10
        , padding 10
        , Background.color Style.charcoal
        , Font.color Style.white
        ]
        [ userInfoView sharedState model
        , followersView sharedState model
        , followView sharedState model
        ]


bookListTable : SharedState -> Model -> Element Msg
bookListTable sharedState model =
    Element.column
        [ width (px 500)
        , height (px 600)
        , spacing 10
        , padding 10
        , Background.color Style.charcoal
        , Font.color Style.white
        ]
        [ bookListTableHeader sharedState model
        , listBooks sharedState model
        ]


bookListTableHeader : SharedState -> Model -> Element Msg
bookListTableHeader sharedState model =
    Element.row [ spacing 15, Background.color Style.charcoal, Font.color Style.white ]
        [ Element.el [ Font.bold, Font.color Style.white ] (text <| bookInfo model)
        , Element.el [ Font.size 14, Font.color Style.orange ] (text <| totalsString sharedState model)
        ]


listBooks : SharedState -> Model -> Element Msg
listBooks sharedState model =
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
                        titleButton book sharedState.currentBook
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
            ]
        }


bookInfo : Model -> String
bookInfo model =
    "Book: " ++ (String.fromInt <| booksCompleted model.bookList) ++ "/" ++ (String.fromInt <| List.length model.bookList)


pageInfo book =
    let
        pp =
            String.fromInt book.pagesRead ++ "/" ++ String.fromInt book.pages

        pc =
            String.fromInt <| Basics.round <| 100 * Basics.toFloat book.pagesRead / Basics.toFloat book.pages
    in
    pp ++ " (" ++ pc ++ "%)"


totalsString : SharedState -> Model -> String
totalsString sharedState model =
    let
        startDate =
            case sharedState.currentUser of
                Nothing ->
                    "6/1/2018"

                Just user ->
                    user.beginningDate

        daysElapsed =
            Days.fromUSDate startDate (Utility.toUtcDateString <| Just sharedState.currentTime)

        pagesReadPerDay =
            Basics.round (Basics.toFloat model.totalPagesRead / Basics.toFloat daysElapsed)
    in
    String.fromInt model.totalPagesRead
        ++ " pages since "
        ++ model.startDateString
        ++ " — "
        ++ String.fromInt pagesReadPerDay
        ++ " pp/day"


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


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        , el Style.footerItem (text <| userStatus sharedState.currentUser)
        , wordCountOfCurrentNotes sharedState
        , userBegginingDate sharedState
        ]


userBegginingDate : SharedState -> Element Msg
userBegginingDate sharedState =
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
            "Signed is as " ++ user.username



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
        , body = Http.jsonBody (User.Session.tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateUserWithQS : User -> String -> String -> Cmd Msg
updateUserWithQS user queryString token =
    Http.request
        { method = "Put"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/users/" ++ String.fromInt user.id ++ "?" ++ queryString
        , body = Http.jsonBody (User.Coders.userRecordEncoder user)
        , expect = Http.expectJson ReceiveUpdateUser User.Coders.statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getAnnotatedUsers : String -> Cmd Msg
getAnnotatedUsers token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/users?all=annotated"
        , body = Http.jsonBody (User.Session.tokenEncoder token)
        , expect = Http.expectJson ReceiveAnnotatedUserList User.Coders.annotatedUserListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--
--
--


computePagesRead : Int -> String -> Cmd Msg
computePagesRead userid token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?userid=" ++ String.fromInt userid
        , body = Http.jsonBody (User.Session.tokenEncoder token)
        , expect = Http.expectJson ComputePagesRead Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


{-| Get shared books by username
-}
getSharedBooks : String -> String -> Cmd Msg
getSharedBooks username token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?shared=" ++ username
        , body = Http.jsonBody (User.Session.tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


getPublicUsers : SharedState -> Cmd Msg
getPublicUsers sharedState =
    case sharedState.currentUser of
        Nothing ->
            Cmd.none

        Just user ->
            Http.request
                { method = "Get"
                , headers = []
                , url = Configuration.backend ++ "/api/users?public=yes"
                , body = Http.jsonBody (User.Session.tokenEncoder user.token)
                , expect = Http.expectJson ReceivePublicUsers User.Coders.publicUserListDecoder
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



--
-- SHAREDSTUFF
--


userInfoView : SharedState -> Model -> Element Msg
userInfoView sharedState model =
    Element.column [ width (px 250), height (px 200), padding 15, spacing 10, scrollbarY ]
        [ Element.el [ Font.bold, Font.size 14 ] (Element.text (publicUserTitle model))
        , Element.column [ spacing 5 ] (List.map (\publicUser -> displayPublicUser sharedState model publicUser) (publicUsersMinusFollowers sharedState model))
        ]


followersView : SharedState -> Model -> Element Msg
followersView sharedState model =
    Element.column [ width (px 250), height (px 300), scrollbarY, padding 15, spacing 10 ]
        [ Element.el [ Font.bold, Font.size 14 ] (Element.text "Followers")
        , Element.column [ spacing 5 ] (List.map (\publicUser -> displayPublicUser sharedState model publicUser) (followerList sharedState))
        ]


followView : SharedState -> Model -> Element Msg
followView sharedState model =
    Element.column [ width (px 250), height (px 300), scrollbarY, padding 15, spacing 10 ]
        [ Element.el [ Font.bold, Font.size 14 ] (Element.text "Follow")
        , Element.column [ spacing 5 ] (List.map (\publicUser -> displayPublicUser sharedState model publicUser) (followList sharedState))
        ]


publicUsersMinusFollowers : SharedState -> Model -> List PublicUser
publicUsersMinusFollowers sharedUserState model =
    case sharedUserState.currentUser of
        Nothing ->
            []

        Just user ->
            model.publicUserList
                |> List.filter (\running_user -> not (List.member running_user user.followers))


followerList : SharedState -> List PublicUser
followerList sharedState =
    case sharedState.currentUser of
        Nothing ->
            []

        Just user ->
            user.followers


followList : SharedState -> List PublicUser
followList sharedState =
    case sharedState.currentUser of
        Nothing ->
            []

        Just user ->
            user.follow


fixBozoList list =
    if list == [ "" ] then
        []

    else
        list


publicUserTitle model =
    let
        n =
            List.length model.publicUserList |> String.fromInt
    in
    "Shared book lists: " ++ n


displayPublicUser : SharedState -> Model -> User.Types.PublicUser -> Element Msg
displayPublicUser sharedState model publicUser =
    row [ spacing 8 ]
        [ el [ Font.size 13, width (px 150) ] (getShareBooksButton model publicUser.username)
        , el [ Font.size 13, width (px 50) ] (followUserButton sharedState model publicUser.username)
        ]


followUserButton : SharedState -> Model -> String -> Element Msg
followUserButton sharedState model publicUsername =
    let
        indicator =
            case sharedState.currentUser of
                Nothing ->
                    ( "Follow", False )

                Just user ->
                    if List.member { username = publicUsername } user.follow then
                        ( "Following", True )

                    else
                        ( "Follow", False )
    in
    Input.button (Style.listElementButtonStyleWithWidth2 65 (Tuple.second indicator))
        { onPress = Just (FollowUser publicUsername)
        , label = Element.text (Tuple.first indicator)
        }


getShareBooksButton : Model -> String -> Element Msg
getShareBooksButton model publicUserName =
    Input.button (Style.activeButton (nameOfCurrentPublicUser model.currentPublicUser == publicUserName) ++ [ width (px 145) ])
        { onPress = Just (GetSharedBooks publicUserName)
        , label = Element.text publicUserName
        }


nameOfCurrentPublicUser : Maybe PublicUser -> String
nameOfCurrentPublicUser publicUser_ =
    case publicUser_ of
        Nothing ->
            ""

        Just user ->
            user.username
