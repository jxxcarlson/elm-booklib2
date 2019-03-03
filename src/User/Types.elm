module User.Types exposing (AnnotatedUser, Msg(..), PublicUser, State(..), User)

import Http
import Routing.Helpers exposing (Route)


type alias User =
    { username : String
    , id : Int
    , firstname : String
    , email : String
    , token : String
    , blurb : String
    , public : Bool
    , follow : List PublicUser
    , followers : List PublicUser
    , admin : Bool
    , beginningDate : String
    }


type Msg
    = ProcessAuthentication (Result Http.Error User)
    | AcceptRegistration (Result Http.Error User)
    | NavigateTo Route
    | AcceptUsername String
    | AcceptEmail String
    | AcceptPassword String
    | SignIn
    | SignOut
    | Register
    | CancelRegistration
    | CancelSignin
    | SubmitRegistration
    | SetState State


type State
    = NotSignedIn
    | SigningIn
    | Registering
    | SignedIn


type alias PublicUser =
    { username : String }


type alias AnnotatedUser =
    { username : String
    , id : Int
    , firstname : String
    , email : String
    , token : String
    , blurb : String
    , public : Bool
    , follow : List String
    , followers : List String
    , admin : Bool
    , numberOfBooks : Int
    }
