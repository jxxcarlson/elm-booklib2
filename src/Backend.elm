module Backend exposing (app)

import Lamdera exposing(SessionId, ClientId)
import Types exposing(..)

app =
    Lamdera.backend -- not Lamdera.Backend.application
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


init = ({message = "Hello from backend."}, Cmd.none)

update : BackendMsg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

updateFromFrontend : SessionId -> ClientId -> ToBackend -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

subscriptions model =
    Sub.none