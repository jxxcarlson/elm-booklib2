module Book.Types exposing (Book)


type alias Book =
    { id : Int
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
    }
