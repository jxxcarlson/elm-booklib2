module Routing.Router exposing (Model, Msg(..), initialModel, init, pageView, update, updateHome, updateCurrentUser, view)

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
    { homeModel : Book.Model
    , currentUserModel : CurrentUser.Model
    , route : Route
    }


type Msg
    = UrlChange Url
    | HomeMsg Book.Msg
    | CurrentUserMsg User.Types.Msg
    | NavigateTo Route


initialModel : Url -> Model
initialModel url =
    let
        ( homeModel, homeCmd ) =
            Book.init

        currentUserModel =
            CurrentUser.initModel
    in
        { homeModel = homeModel
        , currentUserModel = currentUserModel
        , route = parseUrl url
        }


init : Url -> ( Model, Cmd Msg )
init url =
    let
        ( homeModel, homeCmd ) =
            Book.init

        currentUserModel =
            CurrentUser.initModel
    in
        ( { homeModel = homeModel
          , currentUserModel = currentUserModel
          , route = parseUrl url
          }
        , Cmd.map HomeMsg homeCmd
        )


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        UrlChange location ->
            ( { model | route = parseUrl location }
            , Cmd.none
            , NoUpdate
            )

        NavigateTo route ->
            ( model
            , Browser.Navigation.pushUrl sharedState.navKey (reverseRoute route)
            , NoUpdate
            )

        HomeMsg homeMsg ->
            updateHome model homeMsg

        CurrentUserMsg currentUserMsg ->
            updateCurrentUser sharedState model currentUserMsg


updateHome : Model -> Book.Msg -> ( Model, Cmd Msg, SharedStateUpdate )
updateHome model homeMsg =
    let
        ( nextHomeModel, homeCmd ) =
            Book.update homeMsg model.homeModel
    in
        ( { model | homeModel = nextHomeModel }
        , Cmd.map HomeMsg homeCmd
        , NoUpdate
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
                HomeRoute ->
                    "Book"

                SettingsRoute ->
                    "CurrentUser"

                NotFoundRoute ->
                    "404"

        body_ =
            column [ padding 40 ]
                [ el [ paddingXY 0 20, Font.bold ]
                    (text "BookLib")
                , row
                    (Style.navBar (px 480))
                    [ Input.button (Style.activeButton (model.route == HomeRoute))
                        { onPress = Just (NavigateTo HomeRoute)
                        , label = el [] (text "Book")
                        }
                    , Input.button (Style.activeButton (model.route == SettingsRoute))
                        { onPress = Just (NavigateTo SettingsRoute)
                        , label = el [] (text "Sign in")
                        }
                    ]
                , pageView sharedState model
                ]
    in
        { title = "Elm Shared State Demo"
        , body = body_ |> Element.layoutWith { options = [ Style.myFocusStyle ] } [] |> Html.map msgMapper |> \x -> [ x ]
        }


pageView : SharedState -> Model -> Element Msg
pageView sharedState model =
    row []
        [ case model.route of
            HomeRoute ->
                Book.view sharedState model.homeModel
                    |> Element.map HomeMsg

            SettingsRoute ->
                CurrentUser.view sharedState model.currentUserModel
                    |> Element.map CurrentUserMsg

            NotFoundRoute ->
                el [] (text "404 :(")
        ]
