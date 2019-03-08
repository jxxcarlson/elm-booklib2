module User.Types exposing (AnnotatedUser, Msg(..), PublicUser, State(..), User, UserRecord, testUser)

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
    , tags : List String
    }


type alias AnnotatedUser =
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
    , tags : List String
    , numberOfBooks : Int
    }


type alias UserRecord =
    { user : User }


testUser : User
testUser =
    { username = "fred007"
    , id = 1001
    , firstname = "Fred"
    , email = "fred@foo.io"
    , token = "6666555dfdfdf"
    , blurb = "Howdy!"
    , public = False
    , follow = [ { username = "Abe" } ]
    , followers = [ { username = "Bo" }, { username = "Sally" } ]
    , admin = False
    , beginningDate = "1/1/1800"
    , tags = []
    }


type Msg
    = ProcessAuthentication (Result Http.Error User)
    | AcceptRegistration (Result Http.Error User)
    | ReceiveUpdateUser (Result Http.Error String)
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
    | ToggleUserPublic Bool
    | InputTagString String
    | UpdateUserTags


type State
    = NotSignedIn
    | SigningIn
    | Registering
    | SignedIn


type alias PublicUser =
    { username : String }
