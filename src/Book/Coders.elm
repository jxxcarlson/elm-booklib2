module Book.Coders exposing (blurbDecoder, bookDecoder, bookEncoder, bookListDecoder, bookRecordEncoder, newBookEncoder, newBookRecordEncoder, statusDecoder)

import Book.Types exposing (Book)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


bookRecordEncoder : Book -> Encode.Value
bookRecordEncoder book =
    Encode.object
        [ ( "book", bookEncoder book )
        ]


bookEncoder : Book -> Encode.Value
bookEncoder book =
    Encode.object
        [ ( "id", Encode.int book.id )
        , ( "title", Encode.string book.title )
        , ( "subtitle", Encode.string book.subtitle )
        , ( "author", Encode.string book.author )
        , ( "notes", Encode.string book.notes )
        , ( "pages", Encode.int book.pages )
        , ( "pages_read", Encode.int book.pagesRead )
        , ( "rating", Encode.int book.rating )
        , ( "public", Encode.bool book.public )
        , ( "category", Encode.string book.category )
        , ( "start_date_string", Encode.string book.startDateString )
        , ( "finish_date_string", Encode.string book.finishDateString )
        ]


newBookRecordEncoder : Int -> Book -> Encode.Value
newBookRecordEncoder userid book =
    Encode.object
        [ ( "book", newBookEncoder userid book )
        ]


newBookEncoder : Int -> Book -> Encode.Value
newBookEncoder userid book =
    Encode.object
        [ ( "id", Encode.int book.id )
        , ( "user_id", Encode.int userid )
        , ( "title", Encode.string book.title )
        , ( "subtitle", Encode.string book.subtitle )
        , ( "author", Encode.string book.author )
        , ( "notes", Encode.string book.notes )
        , ( "pages", Encode.int book.pages )
        , ( "pages_read", Encode.int book.pagesRead )
        , ( "rating", Encode.int book.rating )
        , ( "public", Encode.bool book.public )
        , ( "category", Encode.string book.category )
        , ( "start_date_string", Encode.string book.startDateString )
        ]


bookDecoder : Decode.Decoder Book
bookDecoder =
    Decode.succeed Book
        |> required "id" Decode.int
        |> required "title" Decode.string
        |> required "subtitle" Decode.string
        |> required "author" Decode.string
        |> required "notes" Decode.string
        |> required "pages" Decode.int
        |> required "pagesRead" Decode.int
        |> required "rating" Decode.int
        |> required "public" Decode.bool
        |> required "category" Decode.string
        |> required "startDateString" Decode.string
        |> required "finishDateString" Decode.string


blurbDecoder : Decode.Decoder String
blurbDecoder =
    Decode.field "blurb" Decode.string


statusDecoder : Decode.Decoder String
statusDecoder =
    Decode.field "status" Decode.string


bookListDecoder : Decode.Decoder (List Book)
bookListDecoder =
    Decode.field "data" (Decode.list bookDecoder)
