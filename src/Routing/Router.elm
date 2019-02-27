module Routing.Router exposing (Model, Msg(..), initialModel, init, pageView, update, updateBook, updateCurrentUser, view)

import Browser
import Browser.Navigation exposing (Key)
import Pages.Book as Book
import Pages.CurrentUser as CurrentUser
import Routing.Helpers exposing (Route(..), parseUrl, reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Url exposing (Url)
import Html exposing (Html)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Background as Background
import Common.Style as Style
import User.Types


type alias Model =
    { bookModel : Book.Model
    , currentUserModel : CurrentUser.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | BookMsg Book.Msg
    | CurrentUserMsg User.Types.Msg
    | NavigateTo Route


initialModel : Url -> Model
initialModel url =
    let
        bookModel =
            Book.init

        currentUserModel =
            CurrentUser.initModel
    in
        { bookModel = bookModel
        , currentUserModel = currentUserModel
        , route = parseUrl url
        }


init : Url -> ( Model, Cmd Msg )
init url =
    let
        bookModel =
            Book.init

        currentUserModel =
            CurrentUser.initModel
    in
        ( { bookModel = bookModel
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
                    (parseUrl location)

                cmd =
                    case route of
                        BooksRoute ->
                            Book.getBookListViaSharedState sharedState

                        _ ->
                            Cmd.none
            in
                ( { model | route = route }
                , Cmd.map BookMsg cmd
                , NoUpdate
                )

        NavigateTo route ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        BookMsg bookMsg ->
            updateBook sharedState model bookMsg

        CurrentUserMsg currentUserMsg ->
            updateCurrentUser sharedState model currentUserMsg


updateBook : SharedState -> Model -> Book.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateBook sharedState model bookMsg =
    let
        ( nextBookModel, bookCmd, sharedStateUpdate ) =
            Book.update sharedState bookMsg model.bookModel
    in
        ( { model | bookModel = nextBookModel }
        , Cmd.map BookMsg bookCmd
        , sharedStateUpdate
        )


updateCurrentUser : SharedState -> Model -> User.Types.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateCurrentUser sharedState model settingsMsg =
    let
        ( nextSettingsModel, settingsCmd, sharedStateUpdate ) =
            CurrentUser.update sharedState settingsMsg model.currentUserModel
    in
        ( { model | currentUserModel = nextSettingsModel }
        , Cmd.map CurrentUserMsg settingsCmd
        , sharedStateUpdate
        )


view : (Msg -> msg) -> SharedState -> Model -> { body : List (Html.Html msg), title : String }
view msgMapper sharedState model =
    let
        title =
            case model.route of
                BooksRoute ->
                    "Books"

                CurrentUserRoute ->
                    "CurrentUser"

                NotFoundRoute ->
                    "404"

        body_ =
            column [ padding 20, Background.color Style.grey, width fill ]
                [ el [ paddingXY 0 20, Font.bold ]
                    (text "BookLib")
                , row
                    (Style.navBar (px 480))
                    [ Input.button (Style.activeButton (model.route == BooksRoute))
                        { onPress = Just (NavigateTo BooksRoute)
                        , label = el [] (text "Books")
                        }
                    , Input.button (Style.activeButton (model.route == CurrentUserRoute))
                        { onPress = Just (NavigateTo CurrentUserRoute)
                        , label = el [] (text "Sign in")
                        }
                    ]
                , pageView sharedState model
                ]
    in
        { title = "BookLib"
        , body = body_ |> Element.layoutWith { options = [ Style.myFocusStyle ] } [] |> Html.map msgMapper |> \x -> [ x ]
        }


pageView : SharedState -> Model -> Element Msg
pageView sharedState model =
    row []
        [ case model.route of
            BooksRoute ->
                Book.view sharedState model.bookModel
                    |> Element.map BookMsg

            CurrentUserRoute ->
                CurrentUser.view sharedState model.currentUserModel
                    |> Element.map CurrentUserMsg

            NotFoundRoute ->
                el [] (text "404 :(")
        ]
