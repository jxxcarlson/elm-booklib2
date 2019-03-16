module User.Chart exposing (chart, prepareStats2)

import Element exposing (..)
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
import User.Types exposing (Msg(..), ReadingStat, State(..), User)


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
