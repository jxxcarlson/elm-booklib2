module User.Coders exposing (publicUserDecoder, publicUserListDecoder, statusDecoder, userRecordEncoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import User.Types exposing (PublicUser, User)


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


followEncoder : List String -> Encode.Value
followEncoder stringList =
    Encode.list Encode.string stringList


userRecordEncoder : User -> Encode.Value
userRecordEncoder user =
    Encode.object
        [ ( "user", userEncoder user ) ]


statusDecoder : Decoder String
statusDecoder =
    Decode.field "message" Decode.string
