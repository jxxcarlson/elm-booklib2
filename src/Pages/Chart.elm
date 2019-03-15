module Pages.Chart exposing (Model, Msg(..), init, update, view)

import Common.Style as Style
import Common.Utility as Utility
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Types exposing (Msg(..), ReadingStat, State(..), User)


type alias Model =
    { message : String
    }


type Msg
    = NoOp String


init : Model
init =
    { message = ""
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp str ->
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


prepareStats : List ReadingStat -> List ( Int, Int )
prepareStats stats =
    let
        prList =
            stats |> List.reverse |> List.map .pagesRead

        n =
            List.length prList

        deltas =
            List.map2 (-) (List.drop 1 prList) (List.take (n - 1) prList)
    in
    List.indexedMap Tuple.pair deltas


prepareStats2 : List ReadingStat -> List Data
prepareStats2 stats =
    prepareStats stats
        |> List.map dataFromTuple


dataFromTuple ( a, b ) =
    { month = toFloat (a + 1), pagesRead = toFloat b }


first : ( Int, Int ) -> Float
first ( a, b ) =
    toFloat (a + 1)


second : ( Int, Int ) -> Float
second ( a, b ) =
    toFloat b


chart : User -> Element msg
chart user =
    LineChart.viewCustom chartConfig
        [ LineChart.line Colors.blue Dots.square "Pages Read" (prepareStats2 user.readingStats) ]
        |> Element.html


type alias Data =
    { month : Float
    , pagesRead : Float
    }


chartConfig : LineChart.Config Data msg
chartConfig =
    { x = Axis.full 800 "Month" .month
    , y = Axis.full 400 "Pages" .pagesRead
    , container = Container.default "line-chart-1"
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.none
    , events = Events.default
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.stacked 0.3 -- Changed from the default!
    , line = Line.wider 2
    , dots = Dots.default
    }


ticksConfig : Ticks.Config msg
ticksConfig =
    Ticks.intCustom 7 Tick.int
