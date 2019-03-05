module Pages.Groups exposing
    ( Model
    , Msg(..)
    , init
    , update
    , view
    )

import Common.Book
import Common.Style as Style
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { appState : AppState
    }


type AppState
    = Default


init : Model
init =
    { appState = Default
    }


type Msg
    = NoOp


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    case Maybe.map .admin sharedState.currentUser of
        Just True ->
            mainView sharedState model

        _ ->
            altView sharedState model


altView sharedState model =
    column [ padding 40 ] [ el [] (text "Under construction ..") ]


mainView sharedState model =
    column [ padding 40 ] [ el [] (text "GROUPS") ]
