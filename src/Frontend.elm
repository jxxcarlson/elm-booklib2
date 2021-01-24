module Frontend exposing (app)

import Browser.Navigation
import Lamdera
import Types exposing(..)
import Url exposing(..)
import Html exposing(Html)

type alias Model = FrontendModel



app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Lamdera Alpha"
                , body = [ view model ]
                }
        }


init : Url -> Browser.Navigation.Key -> ( Model, Cmd FrontendMsg )
init url key = (initModel url key, Cmd.none)


initModel : Url -> Browser.Navigation.Key -> Model
initModel url key =
    {
      url = url
    , key = key
    , message = "Starting up"
    }


update : FrontendMsg -> Model -> (Model, Cmd FrontendMsg)
update  msg model =
   case msg of
     NoOpFrontendMsg -> (model,Cmd.none)
     UrlChanged url -> (model,Cmd.none)
     LinkClicked urlRequest -> (model,Cmd.none)

updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend->
            ( model, Cmd.none )


subscriptions model = Sub.none

view : Model -> Html FrontendMsg
view model = Html.text "HELLO!"