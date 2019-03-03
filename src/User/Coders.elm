module User.Coders exposing
    ( annotatedUserDecoder
    , annotatedUserListDecoder
    , publicUserDecoder
    , publicUserListDecoder
    , statusDecoder
    , userEncoder
    , userRecordEncoder
    )

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import User.Types exposing (AnnotatedUser, PublicUser, User)


publicUserDecoder : Decoder PublicUser
publicUserDecoder =
    Decode.succeed PublicUser
        |> required "username" Decode.string


publicUserListDecoder : Decoder (List PublicUser)
publicUserListDecoder =
    Decode.field "data" (Decode.list publicUserDecoder)


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
    Encode.list encodePublicUser publicUserList


encodePublicUser : PublicUser -> Encode.Value
encodePublicUser publicUser =
    Encode.object
        [ ( "username", Encode.string publicUser.username ) ]


userRecordEncoder : User -> Encode.Value
userRecordEncoder user =
    Encode.object
        [ ( "user", userEncoder user ) ]


statusDecoder : Decoder String
statusDecoder =
    Decode.field "message" Decode.string


annotatedUserListDecoder : Decoder (List AnnotatedUser)
annotatedUserListDecoder =
    Decode.field "data" (Decode.list annotatedUserDecoder)


annotatedUserDecoder : Decoder AnnotatedUser
annotatedUserDecoder =
    Decode.succeed AnnotatedUser
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
        |> required "numberOfBooks" Decode.int