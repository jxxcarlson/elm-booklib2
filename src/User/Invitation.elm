module User.Invitation exposing (..)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Http
import Configuration

type alias Invitation = {
    invitee : String
    , inviter : String
    , groupName : String
    , groupId : Int
    , status : Status
  }

type Status = Waiting | Accepted | Rejected | Undefined


--
-- REQUESTS
--


--
-- CODERS
--

statusAstring : Status -> String
statusAstring status =
    case status of
        Waiting -> "Waiting"
        Accepted -> "Accepted"
        Rejected -> "Rejected"
        Undefined -> "Undefined"


stringToStatus : String -> Status
stringToStatus str =
    case str of
        "Waiting" -> Waiting
        "Accepted" -> Accepted
        "Rejected" -> Rejected
        _ -> Undefined

encodeInvitation : Invitation -> Encode.Value
encodeInvitation invitation =
    Encode.object
      [ ("invitee", Encode.string invitation.invitee)
      , ("inviter", Encode.string invitation.inviter)
      , ("group_name", Encode.string invitation.groupName)
      , ("group_id", Encode.int invitation.groupId)
      , ("status", Encode.string (statusAstring invitation.status))
      ]

invitationDecoder : Decoder Invitation
invitationDecoder =
    Decode.succeed Invitation
      |> required "invitee" Decode.string
      |> required "inviter" Decode.string
      |> required "group_name" Decode.string
      |> required "group_id" Decode.int
      |> required "invitee" (Decode.map stringToStatus Decode.string)



