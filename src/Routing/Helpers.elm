module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>))


type Route
    = BooksRoute
    | CurrentUserRoute
    | NotFoundRoute


reverseRoute : Route -> String
reverseRoute route =
    case route of
        CurrentUserRoute ->
            "#/currentuser"

        _ ->
            "#/"


routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map BooksRoute Url.Parser.top
        , Url.Parser.map CurrentUserRoute (Url.Parser.s "currentuser")
        ]


parseUrl : Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            BooksRoute

        Just fragment ->
            { url | path = fragment, fragment = Nothing }
                |> Url.Parser.parse routeParser
                |> Maybe.withDefault NotFoundRoute
