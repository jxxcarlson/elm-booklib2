module Book.Request exposing(..)
import Http as H


import Config exposing(configuration)

import Book.Data as Data exposing(..)
import Book.Model exposing(Model, BookMsg(..), Book)

getBookList : Int  -> String -> Cmd BookMsg 
getBookList userid  token =
    H.send ReceiveBookList <| bookListRequest userid token

computePagesRead : Int  -> String -> Cmd BookMsg 
computePagesRead userid  token =
   H.send ComputePagesRead <| bookListRequest userid token
 
getSharedBooks : String  -> String -> Cmd BookMsg 
getSharedBooks username  token =
    H.send ReceiveBookList <| sharedBookListRequest username token


getSharedBlurb : String  -> String -> Cmd BookMsg 
getSharedBlurb username  token =
    H.send ReceiveSharedBlurb <| sharedBlurbRequest username token

updateBookRequest : Book -> String -> H.Request String
updateBookRequest book token = 
  H.request
    { method = "Put"
    , headers = [H.header "Authorization" ("Bearer " ++ token)]
    , url = configuration.backend ++ "/api/books/" ++ (String.fromInt book.id)
    , body = H.jsonBody (bookRecordEncoder book)
    , expect = H.expectJson statusDecoder
    , timeout = Just 1000
    , withCredentials = False
    }

updateBook : Book  -> String -> Cmd BookMsg 
updateBook book token =
    H.send BookIsUpdated <| updateBookRequest book token


createBookRequest : Int -> Book -> String -> H.Request String
createBookRequest userid book token = 
  H.request
    { method = "Post"
    , headers = [H.header "Authorization" ("Bearer " ++ token)]
    , url = configuration.backend ++ "/api/books"
    , body = H.jsonBody (newBookRecordEncoder userid book)
    , expect = H.expectJson statusDecoder
    , timeout = Just 1000
    , withCredentials = False
    }

createBook : Int -> Book  -> String -> Cmd BookMsg 
createBook userid book token =
    H.send BookIsCreated <| createBookRequest userid book token


--- NN

doRequestBookList : Model -> BookMsg
doRequestBookList model = 
  case model.maybeUser of 
    Nothing -> NoOp
    Just author -> RequestBookList author.id author.token
