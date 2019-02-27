module Common.Indicator exposing (indicator)

import Svg exposing (rect, svg)
import Svg.Attributes as SA
import Html.Attributes exposing (..)
import Element


indicator barWidth barHeight color fraction =
    Element.html (indicator_ barWidth barHeight color fraction)


indicator_ barWidth barHeight color fraction =
    svg
        [ SA.height <| String.fromInt barHeight
        ]
        [ horizontalBar barWidth barHeight "black" 1.0
        , horizontalBar barWidth barHeight color fraction
        ]


horizontalBar barWidth barHeight color fraction =
    svg
        [ SA.height <| String.fromInt (barHeight + 2) ]
        [ hRect barWidth barHeight color fraction ]


hRect barWidth barHeight color fraction =
    rect
        [ SA.width <| String.fromFloat <| fraction * barWidth
        , SA.height <| String.fromInt barHeight
        , SA.fill color
        ]
        []
