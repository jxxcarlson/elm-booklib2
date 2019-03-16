module Pages.Chart exposing (Model, Msg(..), getUser, init, update, view)

import Common.Style as Style
import Common.Utility as Utility
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Http
import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Chart exposing (chart)
import User.Session
import User.Types exposing (Msg(..), ReadingStat, State(..), User)


type alias Model =
    { message : String
    }


type Msg
    = NoOp String
    | GotUser (Result Http.Error User)


init : Model
init =
    { message = ""
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp str ->
            ( model, Cmd.none, NoUpdate )

        GotUser (Ok user) ->
            ( model, Cmd.none, SharedState.UpdateCurrentUser (Just user) )

        GotUser (Err err) ->
            ( model, Cmd.none, NoUpdate )


testReadingStats =
    [ { dateString = "2019-01-31", pagesRead = 400 }
    , { dateString = "2019-02-28", pagesRead = 800 }
    , { dateString = "2019-03-31", pagesRead = 1000 }
    , { dateString = "2019-04-30", pagesRead = 1100 }
    , { dateString = "2019-05-31", pagesRead = 1600 }
    ]


view : SharedState -> Model -> Element Msg
view sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            column (Style.mainColumn fill fill)
                [ footer sharedState model
                ]

        _ ->
            column (Style.mainColumn fill fill)
                [ column
                    [ spacing 12
                    , width (px sharedState.windowWidth)
                    , height (px (sharedState.windowHeight - 100))
                    , Background.color (Style.makeGrey 0.4)
                    ]
                    [ chartPanelSwitch sharedState
                    ]
                , footer sharedState model
                ]


chartPanelSwitch : SharedState -> Element msg
chartPanelSwitch sharedState =
    case sharedState.currentUser of
        Nothing ->
            Element.none

        Just user ->
            case List.length user.readingStats < 2 of
                True ->
                    el
                        [ Font.color Style.white
                        , Font.size 18
                        , Background.color (Style.makeGrey 0.1)
                        , paddingXY 10 10
                        , centerX
                        , centerY
                        ]
                        (text "There will be a chart here soon")

                False ->
                    chartPanel user


chartPanel : User -> Element msg
chartPanel user =
    column
        [ centerX
        , centerY
        , Font.size 12
        , Background.color (Style.makeGrey 0.8)
        , Font.color Style.white
        ]
        [ chart user, el [ Font.size 14, Font.color Style.white, moveDown 24 ] (text "Pages read per month") ]


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        ]


getUser : SharedState -> Cmd Msg
getUser sharedState =
    case sharedState.currentUser of
        Nothing ->
            Cmd.none

        Just user ->
            Http.request
                { method = "Get"
                , headers = []
                , url = Configuration.backend ++ "/api/users?all=annotated"
                , body = Http.jsonBody (User.Session.tokenEncoder user.token)
                , expect = Http.expectJson GotUser User.Session.userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
