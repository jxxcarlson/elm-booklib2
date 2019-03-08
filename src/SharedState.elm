module SharedState exposing (SharedState, SharedStateUpdate(..), initialSharedState, update)

import Book.Types exposing (Book)
import Browser.Navigation
import Time exposing (Posix)
import User.Types exposing (User)


type alias SharedState =
    { navKey : Browser.Navigation.Key
    , currentTime : Posix
    , currentUser : Maybe User
    , currentBook : Maybe Book
    , windowWidth : Int
    , windowHeight : Int
    }


type SharedStateUpdate
    = NoUpdate
    | UpdateTime Posix
    | UpdateCurrentUser (Maybe User)
    | InvalidateCurrentUser
    | UpdateCurrentBook (Maybe Book)


initialSharedState : Browser.Navigation.Key -> Posix -> Int -> Int -> Maybe User -> SharedState
initialSharedState navKey time w h currentUser =
    { navKey = navKey
    , currentTime = time
    , currentUser = currentUser
    , currentBook = Nothing
    , windowWidth = w
    , windowHeight = h
    }


update : SharedState -> SharedStateUpdate -> SharedState
update sharedState sharedStateUpdate =
    case sharedStateUpdate of
        UpdateTime time ->
            { sharedState | currentTime = time }

        UpdateCurrentUser currentUser ->
            { sharedState | currentUser = currentUser }

        UpdateCurrentBook currentBook ->
            { sharedState | currentBook = currentBook }

        InvalidateCurrentUser ->
            { sharedState | currentUser = Nothing, currentBook = Nothing }

        NoUpdate ->
            sharedState
