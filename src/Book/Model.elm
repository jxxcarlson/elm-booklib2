module Book.Model exposing (..)

import Http as H
import User.Types exposing (User)
import Time exposing (Posix)


type alias Model =
    { currentBook : Maybe Book
    , previousCurrentBook : Maybe Book
    , bookList : List Book
    , totalPagesRead : Int
    , dateStartedReadingString : String
    , maybeUser : Maybe User
    , title : String
    , subtitle : String
    , author : String
    , pages : Int
    , pagesRead : Int
    , notes : String
    , category : String
    , blurb : String
    , sharedBlurb : String
    , startDateString : String
    , finishDateString : String
    , errorMessage : String
    , appState : AppState
    , notesViewMode : NotesViewMode
    , textDisplayMode : TextDisplayMode
    , textRenderMode : TextRenderMode
    , counter : Int
    , currentTime : Maybe Posix
    , beginningDate : String
    }


init : Model
init =
    { currentBook = Nothing
    , previousCurrentBook = Nothing
    , bookList = []
    , totalPagesRead = 0
    , dateStartedReadingString = ""
    , maybeUser = Nothing
    , title = ""
    , subtitle = ""
    , author = ""
    , pages = 0
    , pagesRead = 0
    , notes = ""
    , blurb = ""
    , sharedBlurb = ""
    , category = ""
    , errorMessage = ""
    , startDateString = ""
    , finishDateString = ""
    , appState = ReadingMyBooks
    , notesViewMode = NotesViewShort
    , textDisplayMode = DisplayNotes
    , textRenderMode = MarkDownView
    , counter = 0
    , currentTime = Nothing
    , beginningDate = ""
    }


type AppState
    = CreatingNewBook
    | ReadingMyBooks
    | EditingBook
    | SharingBooks String


type TextDisplayMode
    = DisplayNotes
    | DisplayBlurb


type NotesViewMode
    = NotesViewShort
    | NotesViewLong


type TextRenderMode
    = PlainTextView
    | MarkDownView


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


bookIsCompleted : Book -> Int
bookIsCompleted book =
    if book.finishDateString == "" then
        0
    else
        1


booksCompleted : List Book -> Int
booksCompleted bookList =
    bookList |> List.foldl (\book count -> count + (bookIsCompleted book)) 0


type BookMsg
    = ReceiveBookList (Result H.Error (List Book))
    | ComputePagesRead (Result H.Error (List Book))
    | RequestBookList Int String
    | InputPagesRead String
    | InputNotes String
    | InputTitle String
    | InputSubtitle String
    | InputCategory String
    | InputAuthor String
    | InputPages String
    | InputBlurb String
    | InputStartDate String
    | InputFinishDate String
    | SetCurrentBook Book
    | UpdateCurrentBook
    | UpdateBlurb
    | BookIsUpdated (Result H.Error String)
    | NewBook
    | CancelNewBook
    | CreateNewBook
    | EditBook
    | SaveBookEditChanges
    | BookIsCreated (Result H.Error String)
    | ToggleBookPublic Bool
    | GetSharedBooks String
    | ReceiveSharedBlurb (Result H.Error String)
    | GetCurrentUserBookList
    | ToggleNotesViewMode
    | ToggleBlurbAndNotes
    | ToggleMarkdown
    | NoOp
    | ReceiveTime (Maybe Posix)
