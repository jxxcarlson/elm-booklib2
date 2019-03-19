module User.Mail exposing (Msg(..), send, sendEmailToUsers)

import Configuration
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import User.Types exposing (User)


type Msg
    = AcknowledgeEmailSent (Result Http.Error String)


send : String -> String -> String -> User -> Cmd Msg
send tokenString subject text user =
    Http.request
        { method = "Post"
        , headers = [ Http.header "APIVersion" "V2", Http.header "Authorization" ("Bearer " ++ tokenString) ]
        , url = Configuration.backend ++ "/api/mail"
        , body = Http.jsonBody (encodeEmail user.email subject text)
        , expect = Http.expectJson AcknowledgeEmailSent replyDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


sendEmailToUsers : String -> List User -> String -> String -> Cmd Msg
sendEmailToUsers tokenString userList subject text =
    if userList /= [] && subject /= "" && text /= "" then
        sendEmailToUsers_ tokenString userList subject text

    else
        Cmd.none


sendEmailToUsers_ : String -> List User -> String -> String -> Cmd Msg
sendEmailToUsers_ tokenString userList subject text =
    userList
        |> List.map (send tokenString subject text)
        |> Cmd.batch


encodeEmail : String -> String -> String -> Encode.Value
encodeEmail recipient subject text =
    Encode.object
        [ ( "recipient", Encode.string recipient )
        , ( "subject", Encode.string subject )
        , ( "body", Encode.string text )
        , ( "type", Encode.string "plain" )
        ]


replyDecoder : Decoder String
replyDecoder =
    Decode.field "reply" Decode.string
