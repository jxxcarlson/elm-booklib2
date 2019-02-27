module Book.Data exposing(..)

import Json.Decode as D
import Json.Encode as E
import Json.Decode.Pipeline exposing (required)
import Http as H

import Config exposing(configuration)
import Book.Model exposing(Book)



tokenEncoder : String  -> E.Value
tokenEncoder token  =
   E.object
    [ ("token", E.string token)
    ]


bookRecordEncoder : Book -> E.Value 
bookRecordEncoder book = 
    E.object
    [ ("book", bookEncoder book)
    ]

bookEncoder : Book  -> E.Value
bookEncoder book  =
   E.object
    [ ("id", E.int book.id)
    , ("title", E.string book.title)
    , ("subtitle", E.string book.subtitle)
    , ("author", E.string book.author)
    , ("notes", E.string book.notes)
    , ("pages", E.int book.pages)
    , ("pages_read", E.int book.pagesRead)
    , ("rating", E.int book.rating)
    , ("public", E.bool book.public)
    , ("category", E.string book.category)
    , ("start_date_string", E.string book.startDateString)
    , ("finish_date_string", E.string book.finishDateString)
    ]

newBookRecordEncoder : Int -> Book -> E.Value 
newBookRecordEncoder userid book = 
    E.object
    [ ("book", newBookEncoder userid book)
    ]

newBookEncoder : Int -> Book  -> E.Value
newBookEncoder userid book  =
   E.object
    [ ("id", E.int book.id)
    , ("user_id", E.int userid)
    , ("title", E.string book.title)
    , ("subtitle", E.string book.subtitle)
    , ("author", E.string book.author)
    , ("notes", E.string book.notes)
    , ("pages", E.int book.pages)
    , ("pages_read", E.int book.pagesRead)
    , ("rating", E.int book.rating)
    , ("public", E.bool book.public)
    , ("category", E.string book.category)
    , ("start_date_string", E.string book.startDateString)
    ]


bookDecoder : D.Decoder Book
bookDecoder =
  D.succeed Book
    |> required "id" D.int
    |> required "title" D.string
    |> required "subtitle" D.string
    |> required "author" D.string
    |> required "notes" D.string
    |> required "pages" D.int
    |> required "pagesRead" D.int
    |> required "rating" D.int
    |> required "public" D.bool
    |> required "category" D.string
    |> required "startDateString" D.string
    |> required "finishDateString" D.string
 
blurbDecoder : D.Decoder String
blurbDecoder =
     D.field "blurb" D.string
  

statusDecoder : D.Decoder String 
statusDecoder =
  D.field "status" D.string

bookListDecoder : D.Decoder (List Book)
bookListDecoder =
  D.field "data" (D.list bookDecoder)


bookListRequest : Int -> String -> H.Request (List Book)
bookListRequest userid token = 
  H.request
    { method = "Get"
    , headers = []
    , url = configuration.backend ++ "/api/books?userid=" ++ (String.fromInt userid)
    , body = H.jsonBody (tokenEncoder token)
    , expect = H.expectJson bookListDecoder
    , timeout = Just 5000
    , withCredentials = False
    }

 
sharedBookListRequest : String -> String -> H.Request (List Book)
sharedBookListRequest username token = 
  H.request
    { method = "Get"
    , headers = []
    , url = configuration.backend ++ "/api/books?shared=" ++ username
    , body = H.jsonBody (tokenEncoder token)
    , expect = H.expectJson bookListDecoder
    , timeout = Just 5000
    , withCredentials = False
    }

sharedBlurbRequest  : String -> String -> H.Request String
sharedBlurbRequest username token = 
  H.request
    { method = "Get"
    , headers = []
    , url = configuration.backend ++ "/api/blurb/" ++ username
    , body = H.jsonBody (tokenEncoder token)
    , expect = H.expectJson blurbDecoder
    , timeout = Just 5000
    , withCredentials = False
    }