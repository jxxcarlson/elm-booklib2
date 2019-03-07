module Pages.Admin exposing (Model, Msg(..), dateStringToInt, init, update, view)

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
import User.Coders
import User.Session
import User.Types exposing (User)


type alias Model =
    { users : List User
    , message : String
    }


type Msg
    = NoOp
    | GetUsers
    | GotUsers (Result Http.Error (List User))


init : Model
init =
    { users = []
    , message = ""
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        GetUsers ->
            ( model, getUsers sharedState, SharedState.NoUpdate )

        GotUsers (Ok users_) ->
            ( { model | users = users_ }, Cmd.none, SharedState.NoUpdate )

        GotUsers (Err err) ->
            ( { model | message = "Error decoding user list" }, Cmd.none, SharedState.NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill ++ [ padding 40 ])
        [ row [ spacing 8 ] [ getUsersButton, el [ Font.size 14 ] (text <| String.fromInt <| List.length model.users) ]
        , userList model
        ]


userList : Model -> Element Msg
userList model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px 400)
        , Background.color Style.charcoal
        , Font.color Style.white
        , clipX
        , padding 12
        ]
        { data = model.users |> List.sortBy (\user -> -(dateStringToInt user.beginningDate))
        , columns =
            [ { header = Element.el Style.tableHeading (Element.text "id")
              , width = px 50
              , view =
                    \user ->
                        text (String.fromInt user.id)
              }
            , { header = Element.el Style.tableHeading (Element.text "Username")
              , width = px 200
              , view =
                    \user ->
                        text user.username
              }
            , { header = Element.el Style.tableHeading (Element.text "Email")
              , width = px 200
              , view =
                    \user ->
                        text user.email
              }
            , { header = Element.el Style.tableHeading (Element.text "Joined")
              , width = px 150
              , view =
                    \user ->
                        text user.beginningDate
              }
            ]
        }


dateStringToInt : String -> Int
dateStringToInt dateString =
    let
        intList =
            dateString
                |> String.split "/"
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
    in
    List.map2 (*) [ 40, 1, 500 ] intList
        |> List.sum


getUsersButton : Element Msg
getUsersButton =
    Input.button Style.button
        { onPress = Just GetUsers
        , label = Element.text "Get users"
        }


getUsers : SharedState -> Cmd Msg
getUsers sharedState =
    case sharedState.currentUser of
        Nothing ->
            Cmd.none

        Just user ->
            Http.request
                { method = "Get"
                , headers = []
                , url = Configuration.backend ++ "/api/users?all=yes"
                , body = Http.jsonBody (User.Session.tokenEncoder user.token)
                , expect = Http.expectJson GotUsers User.Session.userListDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
