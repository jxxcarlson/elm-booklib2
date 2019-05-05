module User.Chart exposing (chart, phoneChart, summary)

import Element exposing (..)
import Graph exposing (Option(..), barChart)
import User.Types exposing (Msg(..), ReadingStat, State(..), User)


type alias StatSummary =
    { averagePagesPerMonth : Float
    , pagesReadLastMonth : Int
    , pagesReadThisMonth : Int
    }


summary : User -> StatSummary
summary user =
    let
        prList =
            user.readingStats |> List.map .pagesRead

        n =
            List.length prList

        deltas =
            List.map2 (-) (List.take (n - 1) prList) (List.drop 1 prList)

        average =
            toFloat (List.sum deltas) / toFloat (n - 1)

        thisMonth =
            List.head deltas |> Maybe.withDefault 0

        lastMonth =
            List.head (List.drop 1 deltas) |> Maybe.withDefault 0
    in
    { averagePagesPerMonth = average
    , pagesReadLastMonth = lastMonth
    , pagesReadThisMonth = thisMonth
    }


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


prepareStats2 : List ReadingStat -> List Float
prepareStats2 stats =
    prepareStats stats
        |> List.map second


dataFromTuple ( a, b ) =
    { month = toFloat (a + 1), pagesRead = toFloat b }


first : ( Int, Int ) -> Float
first ( a, b ) =
    toFloat (a + 1)


second : ( Int, Int ) -> Float
second ( a, b ) =
    toFloat b


barGraphAttributes =
    { graphHeight = 300
    , graphWidth = 600
    , options = [ Color "rgb(200,0,0)", DeltaX 15, YTickmarks 6, XTickmarks 12 ]
    }


barGraphAttributesForPhone =
    { graphHeight = 300
    , graphWidth = 500
    , options = [ Color "rgb(200,0,0)", DeltaX 15, YTickmarks 6, XTickmarks 12 ]
    }


chart : User -> Element msg
chart user =
    column [ padding 40, moveDown 20 ]
        [ Graph.barChart barGraphAttributes (prepareStats2 user.readingStats)
            |> Element.html
        ]


phoneChart : Int -> Int -> User -> Element msg
phoneChart w h user =
    Graph.barChart barGraphAttributesForPhone (prepareStats2 user.readingStats)
        |> Element.html


type alias Data =
    { month : Float
    , pagesRead : Float
    }
