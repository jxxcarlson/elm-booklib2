module User.Session exposing (authenticate, registerUser, tokenEncoder, updateUser, userEncoder)

import Common.Days as Days
import Configuration
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import User.Coders
import User.Types exposing (Msg(..), PublicUser, User, UserRecord)


authenticate : String -> String -> Cmd Msg
authenticate email password =
    Http.post
        { url = Configuration.backend ++ "/api/users/authenticate"
        , body = Http.jsonBody (authorizationEncoder email password)
        , expect = Http.expectJson ProcessAuthentication userDecoder
        }


registerUser : String -> String -> String -> Cmd Msg
registerUser username email password =
    Http.post
        { url = Configuration.backend ++ "/api/users/"
        , body = Http.jsonBody (registrationEncoder username email password)
        , expect = Http.expectJson AcceptRegistration userDecoder
        }


updateUser : User -> String -> Cmd Msg
updateUser user token =
    Http.request
        { method = "Put"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/users/" ++ String.fromInt user.id
        , body = Http.jsonBody (User.Coders.userRecordEncoder user)
        , expect = Http.expectJson ReceiveUpdateUser User.Coders.statusDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- ENCODERS AND DECODERS
--


tokenEncoder : String -> Encode.Value
tokenEncoder token =
    Encode.object
        [ ( "token", Encode.string token )
        ]


authorizationEncoder : String -> String -> Encode.Value
authorizationEncoder email password =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


registrationEncoder : String -> String -> String -> Encode.Value
registrationEncoder username email password =
    Encode.object [ ( "user", preRegistrationEncoder username email password ) ]


preRegistrationEncoder : String -> String -> String -> Encode.Value
preRegistrationEncoder username email password =
    Encode.object
        [ ( "username", Encode.string username )
        , ( "firstname", Encode.string "Anon" )
        , ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


userRecordDecoder : Decoder UserRecord
userRecordDecoder =
    Decode.succeed UserRecord
        |> required "user" userDecoder


userDecoder : Decoder User
userDecoder =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "id" Decode.int
        |> required "firstname" Decode.string
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "blurb" Decode.string
        |> required "public" Decode.bool
        |> required "follow" (Decode.list publicUserDecoder)
        |> required "followers" (Decode.list publicUserDecoder)
        |> required "admin" Decode.bool
        |> required "inserted_at" (Decode.map usDateStringFromElixirDateString Decode.string)
        |> required "tags" (Decode.list Decode.string)


userRecordDecoder2 : Decoder UserRecord
userRecordDecoder2 =
    Decode.succeed UserRecord
        |> required "user" userDecoder2


userDecoder2 : Decoder User
userDecoder2 =
    Decode.succeed User
        |> required "username" Decode.string
        |> required "id" Decode.int
        |> required "firstname" Decode.string
        |> required "email" Decode.string
        |> required "token" Decode.string
        |> required "blurb" Decode.string
        |> required "public" Decode.bool
        |> required "follow" (Decode.list publicUserDecoder)
        |> required "followers" (Decode.list publicUserDecoder)
        |> required "admin" Decode.bool
        |> required "beginningDate" Decode.string
        |> required "tags" (Decode.list Decode.string)


publicUserDecoder : Decoder PublicUser
publicUserDecoder =
    Decode.succeed PublicUser
        |> required "username" Decode.string


userEncoder : User -> Encode.Value
userEncoder user =
    Encode.object
        [ ( "username", Encode.string user.username )
        , ( "id", Encode.int user.id )
        , ( "firstname", Encode.string user.firstname )
        , ( "email", Encode.string user.email )
        , ( "token", Encode.string user.token )
        , ( "blurb", Encode.string user.blurb )
        , ( "public", Encode.bool user.public )
        , ( "follow", followEncoder user.follow )
        , ( "followers", followEncoder user.followers )
        , ( "admin", Encode.bool user.admin )
        , ( "beginningDate", Encode.string user.beginningDate )
        ]


followEncoder : List PublicUser -> Encode.Value
followEncoder publicUserList =
    Encode.list publicUserEncoder publicUserList


publicUserEncoder : PublicUser -> Encode.Value
publicUserEncoder publicUser =
    Encode.object
        [ ( "username", Encode.string publicUser.username ) ]



--
-- HELPERS
--


intListFromElixirDateString : String -> List Int
intListFromElixirDateString str =
    let
        maybeElixirDate =
            String.split "T" str |> List.head
    in
    case maybeElixirDate of
        Nothing ->
            []

        Just str_ ->
            String.split "-" str_
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)


usDateStringFromElixirDateString : String -> String
usDateStringFromElixirDateString dateString =
    dateString
        |> intListFromElixirDateString
        |> Days.fromIntList
        |> Days.usDateStringFromDate
