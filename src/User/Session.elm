module User.Session exposing (authenticate, registerUser, userEncoder)

import Common.Days as Days
import Configuration
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import User.Types exposing (Msg(..), User)


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



--
-- ENCODERS AND DECODERS
--


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
        |> required "follow" (Decode.list Decode.string)
        |> required "followers" (Decode.list Decode.string)
        |> required "admin" Decode.bool
        |> required "inserted_at" (Decode.map usDateStringFromElixirDateString Decode.string)


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


followEncoder : List String -> Encode.Value
followEncoder stringList =
    Encode.list Encode.string stringList



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
