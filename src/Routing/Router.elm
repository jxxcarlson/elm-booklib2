module Routing.Router exposing (Model, Msg(..), init, initialModel, pageView, update, updateBook, updateCurrentUser, view)

import Browser.Navigation exposing (Key)
import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Pages.Books as Books
import Pages.CurrentBook as CurrentBook exposing (AppState(..))
import Pages.CurrentUser as CurrentUser
import Pages.SharedBooks as SharedBooks
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)
import User.Types exposing (State(..))


type alias Model =
    { bookModel : Books.Model
    , sharedBookModel : SharedBooks.Model
    , currentBookModel : CurrentBook.Model
    , currentUserModel : CurrentUser.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | BookMsg Books.Msg
    | SharedBookMsg SharedBooks.Msg
    | CurrentBookMsg CurrentBook.Msg
    | CurrentUserMsg User.Types.Msg
    | NavigateTo Route


initialModel : Url -> Model
initialModel url =
    let
        bookModel =
            Books.init

        sharedBookModel =
            SharedBooks.init

        currentBookModel =
            CurrentBook.init

        currentUserModel =
            CurrentUser.initModel
    in
    { bookModel = bookModel
    , currentBookModel = currentBookModel
    , sharedBookModel = sharedBookModel
    , currentUserModel = currentUserModel
    , route = parseUrl url
    }


init : Url -> ( Model, Cmd Msg )
init url =
    let
        bookModel =
            Books.init

        currentBookModel =
            CurrentBook.init

        sharedBookModel =
            SharedBooks.init

        currentUserModel =
            CurrentUser.initModel
    in
    ( { bookModel = bookModel
      , sharedBookModel = sharedBookModel
      , currentBookModel = currentBookModel
      , currentUserModel = currentUserModel
      , route = parseUrl url
      }
    , Cmd.map BookMsg Cmd.none
    )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UrlChange location ->
            let
                route =
                    parseUrl location

                cmd =
                    case route of
                        BooksRoute ->
                            Books.getBookListViaSharedState sharedState |> Cmd.map BookMsg

                        SharedBooksRoute ->
                            SharedBooks.getPublicUsers sharedState |> Cmd.map SharedBookMsg

                        _ ->
                            Cmd.none

                oldUserModel =
                    model.currentUserModel

                newUserModel =
                    case route of
                        CurrentUserRoute ->
                            case sharedState.currentUser of
                                Nothing ->
                                    { oldUserModel | state = NotSignedIn }

                                Just _ ->
                                    { oldUserModel | state = SignedIn }

                        _ ->
                            oldUserModel
            in
            ( { model | route = route, currentUserModel = newUserModel }
            , cmd
            , NoUpdate
            )

        NavigateTo route ->
            let
                oldCurrentBookModel =
                    model.currentBookModel

                newCurrentBooksModel =
                    case route of
                        CurrentBookRoute ->
                            oldCurrentBookModel

                        _ ->
                            { oldCurrentBookModel | appState = ReadingBook }
            in
            ( { model | currentBookModel = newCurrentBooksModel }
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        BookMsg bookMsg ->
            updateBook sharedState model bookMsg

        SharedBookMsg sharedBookMsg ->
            updateSharedBook sharedState model sharedBookMsg

        CurrentBookMsg currentBookMsg ->
            updateCurrentBook sharedState model currentBookMsg

        CurrentUserMsg currentUserMsg ->
            updateCurrentUser sharedState model currentUserMsg


updateBook : SharedState -> Model -> Books.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateBook sharedState model bookMsg =
    let
        ( nextBookModel, bookCmd, sharedStateUpdate ) =
            Books.update sharedState bookMsg model.bookModel
    in
    ( { model | bookModel = nextBookModel }
    , Cmd.map BookMsg bookCmd
    , sharedStateUpdate
    )


updateSharedBook : SharedState -> Model -> SharedBooks.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateSharedBook sharedState model sharedBookMsg =
    let
        ( nextSharedBookModel, sharedBookCmd, sharedStateUpdate ) =
            SharedBooks.update sharedState sharedBookMsg model.sharedBookModel
    in
    ( { model | sharedBookModel = nextSharedBookModel }
    , Cmd.map SharedBookMsg sharedBookCmd
    , sharedStateUpdate
    )


updateCurrentBook : SharedState -> Model -> CurrentBook.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateCurrentBook sharedState model currentBookMsg =
    let
        ( nextCurrentBookModel, currentBookCmd, sharedStateUpdate ) =
            CurrentBook.update sharedState currentBookMsg model.currentBookModel
    in
    ( { model | currentBookModel = nextCurrentBookModel }
    , Cmd.map CurrentBookMsg currentBookCmd
    , sharedStateUpdate
    )


updateCurrentUser : SharedState -> Model -> User.Types.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateCurrentUser sharedState model userMsg =
    let
        ( nextUserModel, userCmd, sharedStateUpdate ) =
            CurrentUser.update sharedState userMsg model.currentUserModel
    in
    ( { model | currentUserModel = nextUserModel }
    , Cmd.map CurrentUserMsg userCmd
    , sharedStateUpdate
    )


view : (Msg -> msg) -> SharedState -> Model -> { body : List (Html.Html msg), title : String }
view msgMapper sharedState model =
    let
        title =
            case model.route of
                BooksRoute ->
                    "Books"

                SharedBooksRoute ->
                    "Shared Books"

                CurrentBookRoute ->
                    "Current Book"

                CurrentUserRoute ->
                    "Current User"

                NotFoundRoute ->
                    "404"

        body_ =
            column [ padding 20, Background.color Style.grey, width fill, height fill ]
                [ row
                    (Style.navBar fill)
                    [ el [ Font.bold, Font.color Style.white ] (text "BookLib")
                    , case sharedState.currentUser of
                        Nothing ->
                            Element.none

                        Just _ ->
                            Input.button (Style.activeButton (model.route == BooksRoute))
                                { onPress = Just (NavigateTo BooksRoute)
                                , label = el [] (text "Reading List")
                                }
                    , case matchBookAndUserIds sharedState of
                        False ->
                            Element.none

                        True ->
                            Input.button (Style.activeButton (model.route == CurrentBookRoute))
                                { onPress = Just (NavigateTo CurrentBookRoute)
                                , label = el [] (text "The Book")
                                }
                    , case sharedState.currentUser of
                        Nothing ->
                            Element.none

                        Just _ ->
                            Input.button (Style.activeButton (model.route == SharedBooksRoute))
                                { onPress = Just (NavigateTo SharedBooksRoute)
                                , label = el [] (text "Shared Books")
                                }
                    , Input.button (Style.activeButton (model.route == CurrentUserRoute))
                        { onPress = Just (NavigateTo CurrentUserRoute)
                        , label = el [] (text "User")
                        }
                    ]
                , pageView sharedState model
                ]
    in
    { title = "BookLib"
    , body = body_ |> Element.layoutWith { options = [ Style.myFocusStyle ] } [] |> Html.map msgMapper |> (\x -> [ x ])
    }


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


pageView : SharedState -> Model -> Element Msg
pageView sharedState model =
    row []
        [ case model.route of
            BooksRoute ->
                Books.view sharedState model.bookModel
                    |> Element.map BookMsg

            SharedBooksRoute ->
                SharedBooks.view sharedState model.sharedBookModel
                    |> Element.map SharedBookMsg

            CurrentBookRoute ->
                CurrentBook.view sharedState model.currentBookModel
                    |> Element.map CurrentBookMsg

            CurrentUserRoute ->
                CurrentUser.view sharedState model.currentUserModel
                    |> Element.map CurrentUserMsg

            NotFoundRoute ->
                el [] (text "404 :(")
        ]
