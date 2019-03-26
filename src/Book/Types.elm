module Book.Types exposing (Book, blankBook)


type alias Book =
    { id : Int
    , userId : Int
    , title : String
    , subtitle : String
    , author : String
    , notes : String
    , pages : Int
    , pagesRead : Int
    , rating : Int
    , public : Bool
    , category : String
    , startDateString : String
    , finishDateString : String
    , pagesReadToday : Int
    , averageReadingRate : Float
    }


blankBook =
    { id = 0
    , userId = 0
    , title = "New book"
    , subtitle = ""
    , author = ""
    , notes = ""
    , pages = 0
    , pagesRead = 0
    , rating = 0
    , public = False
    , category = ""
    , startDateString = ""
    , finishDateString = ""
    , pagesReadToday = 0
    , averageReadingRate = 0
    }
