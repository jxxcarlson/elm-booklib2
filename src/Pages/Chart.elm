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
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just currentUser ->
                    ( model, Cmd.none, SharedState.UpdateCurrentUser (Just { user | token = currentUser.token }) )

        GotUser (Err err) ->
            ( model, Cmd.none, NoUpdate )


deviceIsPhone : SharedState -> Bool
deviceIsPhone sharedState =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            True

        _ ->
            False


view : SharedState -> Model -> Element Msg
view sharedState model =
    case classifyDevice { width = sharedState.windowWidth, height = sharedState.windowHeight } |> .class of
        Phone ->
            chartPanelSwitch sharedState

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
                    chartPanel sharedState user


chartPanel : SharedState -> User -> Element msg
chartPanel sharedState user =
    case deviceIsPhone sharedState of
        False ->
            mainChart user

        True ->
            phoneChart sharedState user


mainChart user =
    let
        summary =
            User.Chart.summary user

        avString =
            String.fromFloat <| Utility.roundTo 1 summary.averagePagesPerMonth

        lastMonthString =
            String.fromInt summary.pagesReadLastMonth

        thisMonthString =
            String.fromInt summary.pagesReadThisMonth

        infoString =
            "Average: "
                ++ avString
                ++ ", "
                ++ "last month: "
                ++ lastMonthString
                ++ ", "
                ++ "this month: "
                ++ thisMonthString
    in
    column
        [ centerX
        , centerY
        , Font.size 12
        , Background.color (Style.makeGrey 0.8)
        , Font.color Style.white
        ]
        [ chart user
        , el [ Font.size 14, Font.color Style.white, moveDown 34 ] (text "Pages read per month")
        , el [ Font.size 14, Font.color Style.white, moveDown 38 ] (text <| infoString)
        ]


phoneChart sharedState user =
    column
        [ moveDown 120
        , moveLeft 115
        ]
        [ column
            [ Font.size 12
            , rotate (radians -90)
            ]
            [ User.Chart.phoneChart (sharedState.windowHeight - 40) sharedState.windowWidth user ]
        ]


radians : Float -> Float
radians degrees =
    3.1416 * degrees / 180.0


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
                , url = Configuration.backend ++ "/api/users/" ++ String.fromInt user.id
                , body = Http.jsonBody (User.Session.tokenEncoder user.token)
                , expect = Http.expectJson GotUser User.Session.userDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
