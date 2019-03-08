module Routing.Router exposing (Model, Msg(..), init, initialModel, pageView, update, updateBook, updateCurrentUser, view)

import Browser.Navigation exposing (Key)
import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Pages.About as About
import Pages.Admin as Admin
import Pages.Books as Books
import Pages.CurrentBook as CurrentBook exposing (AppState(..))
import Pages.CurrentUser as CurrentUser
import Pages.Groups as Groups
import Pages.SharedBooks as SharedBooks
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)
import User.Types exposing (State(..))
import User.Utility


type alias Model =
    { bookModel : Books.Model
    , sharedBookModel : SharedBooks.Model
    , currentBookModel : CurrentBook.Model
    , currentUserModel : CurrentUser.Model
    , aboutModel : About.Model
    , groupsModel : Groups.Model
    , adminModel : Admin.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | BookMsg Books.Msg
    | SharedBookMsg SharedBooks.Msg
    | CurrentBookMsg CurrentBook.Msg
    | CurrentUserMsg User.Types.Msg
    | AboutMsg About.Msg
    | GroupsMsg Groups.Msg
    | AdminMsg Admin.Msg
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

        aboutModel =
            About.init

        groupsModel =
            Groups.init

        adminModel =
            Admin.init
    in
    { bookModel = bookModel
    , currentBookModel = currentBookModel
    , sharedBookModel = sharedBookModel
    , currentUserModel = currentUserModel
    , aboutModel = aboutModel
    , groupsModel = groupsModel
    , adminModel = adminModel
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

        aboutModel =
            About.init

        groupsModel =
            Groups.init

        adminModel =
            Admin.init
    in
    ( { bookModel = bookModel
      , sharedBookModel = sharedBookModel
      , currentBookModel = currentBookModel
      , currentUserModel = currentUserModel
      , aboutModel = aboutModel
      , groupsModel = groupsModel
      , adminModel = adminModel
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

                        GroupsRoute ->
                            Groups.getGroupList |> Cmd.map GroupsMsg

                        CurrentBookRoute ->
                            Cmd.none

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
                                    { oldUserModel | state = SignedIn, tagString = User.Utility.tagsToString sharedState.currentUser }

                        _ ->
                            oldUserModel

                oldCurrentBookModel =
                    model.currentBookModel

                newCurrentBookModel =
                    case route of
                        CurrentBookRoute ->
                            { oldCurrentBookModel | appState = ReadingBook }

                        _ ->
                            oldCurrentBookModel
            in
            ( { model | route = route, currentUserModel = newUserModel, currentBookModel = newCurrentBookModel }
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

        AboutMsg aboutMsg ->
            updateAbout sharedState model aboutMsg

        GroupsMsg groupsMsg ->
            updateGroups sharedState model groupsMsg

        AdminMsg adminMsg ->
            updateAdmin sharedState model adminMsg


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


updateAbout : SharedState -> Model -> About.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateAbout sharedState model aboutMsg =
    let
        ( nextAboutModel, aboutCmd, sharedStateUpdate ) =
            About.update sharedState aboutMsg model.aboutModel
    in
    ( { model | aboutModel = nextAboutModel }
    , Cmd.map AboutMsg aboutCmd
    , sharedStateUpdate
    )


updateGroups : SharedState -> Model -> Groups.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateGroups sharedState model groupsMsg =
    let
        ( nextGroupsModel, groupsCmd, sharedStateUpdate ) =
            Groups.update sharedState groupsMsg model.groupsModel
    in
    ( { model | groupsModel = nextGroupsModel }
    , Cmd.map GroupsMsg groupsCmd
    , sharedStateUpdate
    )


updateAdmin : SharedState -> Model -> Admin.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateAdmin sharedState model adminMsg =
    let
        ( nextAdminModel, adminCmd, sharedStateUpdate ) =
            Admin.update sharedState adminMsg model.adminModel
    in
    ( { model | adminModel = nextAdminModel }
    , Cmd.map AdminMsg adminCmd
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

                AboutRoute ->
                    "About"

                GroupsRoute ->
                    "Groups"

                AdminRoute ->
                    "Admin"

                NotFoundRoute ->
                    "404"

        body_ =
            column [ paddingXY 0 0, Background.color Style.grey, width fill, height fill ]
                [ row
                    (Style.navBar fill)
                    [ el [ Font.bold, Font.color Style.white ] (text "BookLib")
                    , showIf (sharedState.currentUser /= Nothing)
                        (Input.button (Style.activeButton (model.route == BooksRoute))
                            { onPress = Just (NavigateTo BooksRoute)
                            , label = el [] (text "My Books")
                            }
                        )
                    , showIf (sharedState.currentUser /= Nothing)
                        (Input.button (Style.activeButton (model.route == CurrentBookRoute))
                            { onPress = Just (NavigateTo CurrentBookRoute)
                            , label = el [] (text "Book")
                            }
                        )
                    , showIf (sharedState.currentUser /= Nothing)
                        (Input.button (Style.activeButton (model.route == SharedBooksRoute))
                            { onPress = Just (NavigateTo SharedBooksRoute)
                            , label = el [] (text "Shared Books")
                            }
                        )
                    , showIf (currentUserIsMe sharedState)
                        (Input.button (Style.activeButton (model.route == GroupsRoute))
                            { onPress = Just (NavigateTo GroupsRoute)
                            , label = el [] (text "Groups")
                            }
                        )
                    , Input.button (Style.activeButton (model.route == CurrentUserRoute))
                        { onPress = Just (NavigateTo CurrentUserRoute)
                        , label = el [] (text "User")
                        }
                    , Input.button (Style.activeButton (model.route == AboutRoute))
                        { onPress = Just (NavigateTo AboutRoute)
                        , label = el [] (text "About")
                        }
                    , showIf (currentUserIsMe sharedState)
                        (Input.button (Style.activeButton (model.route == AdminRoute))
                            { onPress = Just (NavigateTo AdminRoute)
                            , label = el [] (text "Admin")
                            }
                        )
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

            AboutRoute ->
                About.view sharedState model.aboutModel
                    |> Element.map AboutMsg

            GroupsRoute ->
                Groups.view sharedState model.groupsModel
                    |> Element.map GroupsMsg

            AdminRoute ->
                Admin.view sharedState model.adminModel
                    |> Element.map AdminMsg

            NotFoundRoute ->
                el [] (text "404 :(")
        ]



--
-- HELPERS
--


showIf : Bool -> Element Msg -> Element Msg
showIf flag element =
    if flag then
        element

    else
        Element.none


currentUserIsMe : SharedState -> Bool
currentUserIsMe sharedState =
    case sharedState.currentUser of
        Nothing ->
            False

        Just user ->
            user.username == "jxxcarlson"
