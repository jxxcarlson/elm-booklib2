module User.Invitation exposing (Invitation, Status(..), encodeInvitation, invitationDecoder, invitationsDecoder, replyDecoder, statusAsString, stringToStatus)

import Configuration
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Invitation =
    { id : Int
    , invitee : String
    , inviter : String
    , groupName : String
    , groupId : Int
    , status : Status
    }


type Status
    = Waiting
    | Accepted
    | Rejected
    | Undefined



--
-- REQUESTS
--
--
-- CODERS
--


statusAsString : Status -> String
statusAsString status =
    case status of
        Waiting ->
            "Waiting"

        Accepted ->
            "Accepted"

        Rejected ->
            "Rejected"

        Undefined ->
            "Undefined"


stringToStatus : String -> Status
stringToStatus str =
    case str of
        "Waiting" ->
            Waiting

        "Accepted" ->
            Accepted

        "Rejected" ->
            Rejected

        _ ->
            Undefined


encodeInvitation : Invitation -> Encode.Value
encodeInvitation invitation =
    Encode.object
        [ ( "id", Encode.int invitation.id )
        , ( "invitee", Encode.string invitation.invitee )
        , ( "inviter", Encode.string invitation.inviter )
        , ( "group_name", Encode.string invitation.groupName )
        , ( "group_id", Encode.int invitation.groupId )
        , ( "status", Encode.string (statusAsString invitation.status) )
        ]


invitationDecoder : Decoder Invitation
invitationDecoder =
    Decode.succeed Invitation
        |> required "id" Decode.int
        |> required "invitee" Decode.string
        |> required "inviter" Decode.string
        |> required "group_name" Decode.string
        |> required "group_id" Decode.int
        |> required "status" (Decode.map stringToStatus Decode.string)


invitationsDecoder : Decoder (List Invitation)
invitationsDecoder =
    Decode.field "data" (Decode.list invitationDecoder)


replyDecoder : Decoder String
replyDecoder =
    Decode.field "reply" Decode.string
