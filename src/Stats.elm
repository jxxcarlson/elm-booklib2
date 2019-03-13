module Stats exposing (Stats, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Stats =
    { users : Int
    , books : Int
    , booksRead : Int
    , pages : Int
    , pagesRead : Int
    }


decoder : Decoder Stats
decoder =
    Decode.succeed Stats
        |> required "users" Decode.int
        |> required "books" Decode.int
        |> required "books_read" Decode.int
        |> required "pages" Decode.int
        |> required "pages_read" Decode.int
