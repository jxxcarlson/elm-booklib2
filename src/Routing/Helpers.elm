module Routing.Helpers exposing (Route(..), parseUrl, reverseRoute, routeParser)

import Url exposing (Url)
import Url.Parser exposing ((</>))


type Route
    = BooksRoute
    | SharedBooksRoute
    | CurrentBookRoute
    | CurrentUserRoute
    | ChartRoute
    | AboutRoute
    | GroupsRoute
    | AdminRoute
    | NotFoundRoute


reverseRoute : Route -> String
reverseRoute route =
    case route of
        CurrentUserRoute ->
            "#/"

        ChartRoute ->
            "#/chart"

        BooksRoute ->
            "#/books"

        SharedBooksRoute ->
            "#/sharedbooks"

        CurrentBookRoute ->
            "#/currentbook"

        AboutRoute ->
            "#/about"

        GroupsRoute ->
            "#/groups"

        AdminRoute ->
            "#/admin"

        _ ->
            "#/"


routeParser =
    Url.Parser.oneOf
        [ Url.Parser.map CurrentUserRoute Url.Parser.top
        , Url.Parser.map BooksRoute (Url.Parser.s "books")
        , Url.Parser.map ChartRoute (Url.Parser.s "chart")
        , Url.Parser.map CurrentBookRoute (Url.Parser.s "currentbook")
        , Url.Parser.map SharedBooksRoute (Url.Parser.s "sharedbooks")
        , Url.Parser.map AboutRoute (Url.Parser.s "about")
        , Url.Parser.map GroupsRoute (Url.Parser.s "groups")
        , Url.Parser.map AdminRoute (Url.Parser.s "admin")
        ]


parseUrl : Url -> Route
parseUrl url =
    case url.fragment of
        Nothing ->
            CurrentUserRoute

        Just fragment ->
            { url | path = fragment, fragment = Nothing }
                |> Url.Parser.parse routeParser
                |> Maybe.withDefault NotFoundRoute
