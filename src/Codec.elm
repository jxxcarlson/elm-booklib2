module Codec exposing (decodeData, encodeData)

import Book.Types exposing (Book)
import Json.Decode as D
import Json.Decode.Pipeline as DP
import Json.Encode as E
import Time


decodeData : String -> Result D.Error (List Book)
decodeData str =
    D.decodeString dataDecoder str


dataDecoder : D.Decoder (List Book)
dataDecoder =
    D.list datumDecoder


datumDecoder : D.Decoder Book
datumDecoder =
    D.succeed Book
        |> DP.required "id" D.int
        |> DP.required "userId" D.int
        |> DP.required "title" D.string
        |> DP.required "subtitle" D.string
        |> DP.required "author" D.string
        |> DP.required "notes" D.string
        |> DP.required "pages" D.int
        |> DP.required "pagesRead" D.int
        |> DP.required "rating" D.int
        |> DP.required "public" D.bool
        |> DP.required "category" D.string
        |> DP.required "startDateString" D.string
        |> DP.required "finishDateString" D.string
        |> DP.required "pagesReadToday" D.int
        |> DP.required "averageReadingRate" D.float

datumEncoder : Book -> E.Value
datumEncoder datum =
    E.object
        [ ( "id", E.int datum.id )
        , ( "userId", E.int datum.userId )
        , ( "title", E.string datum.title )
        , ( "subtitle", E.string datum.subtitle )
        , ( "author", E.string datum.author )
        , ( "notes", E.string datum.notes )
        , ( "pages", E.int datum.pages )
        , ( "pagesRead", E.int datum.pagesRead )
        , ( "rating", E.int datum.rating )
        , ( "public", E.bool datum.public )
        , ( "category", E.string datum.category )
        , ( "startDateString", E.string datum.startDateString)
        , ( "finishDateString", E.string datum.finishDateString)
        , ( "pagesReadToday", E.int datum.pagesReadToday )
        , ( "averageReadingRate", E.float datum.averageReadingRate ) ]


mapToMaybeTimePosix : Int -> Maybe Time.Posix
mapToMaybeTimePosix k =
    if k <= 0 then
        Nothing

    else
        Just (Time.millisToPosix k)


encodeData : List Book -> String
encodeData data =
    E.encode 3 (dataEncoder data)


dataEncoder : List Book -> E.Value
dataEncoder data =
    E.list datumEncoder data





transformFinishDate : Maybe Time.Posix -> Int
transformFinishDate posix =
    case posix of
        Nothing ->
            0

        Just p ->
            Time.posixToMillis p
