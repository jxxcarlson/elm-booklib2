port module OutsideInfo exposing
    ( InfoForElm(..)
    , InfoForOutside(..)
    , getInfoFromOutside
    , sendInfoOutside
    )

import Json.Decode as D
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import User.Types exposing (User)


port infoForOutside : GenericOutsideData -> Cmd msg


port infoForElm : (GenericOutsideData -> msg) -> Sub msg


type InfoForOutside
    = UserData Json.Encode.Value
    | AskToReconnectUser Json.Encode.Value
    | DisconnectUser Json.Encode.Value


type alias GenericOutsideData =
    { tag : String, data : Json.Encode.Value }


type InfoForElm
    = LocalStorageInfo User


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
    case info of
        UserData value ->
            infoForOutside { tag = "UserData", data = value }

        AskToReconnectUser value ->
            infoForOutside { tag = "AskToReconnectUser", data = value }

        DisconnectUser value ->
            infoForOutside { tag = "DisconnectUser", data = value }


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of
                "ReconnectUser" ->
                    case D.decodeValue userDecoderForOutside outsideInfo.data of
                        Ok localStorageRecord ->
                            tagger <| LocalStorageInfo localStorageRecord

                        Err e ->
                            onError "Error reconnecting user"

                _ ->
                    onError <| "Unexpected info from outside: "
        )


userDecoderForOutside : D.Decoder User
userDecoderForOutside =
    D.succeed User
        |> required "username" D.string
        |> required "id" (D.map stringToInt D.string)
        |> required "firstname" D.string
        |> required "email" D.string
        |> required "token" D.string
        |> required "blurb" D.string
        |> required "public" (D.map stringToBool D.string)
        |> required "follow" (D.map stringToList D.string)
        |> required "followers" (D.map stringToList D.string)
        |> required "admin" (D.map stringToBool D.string)
        |> required "beginningDate" D.string


stringToBool : String -> Bool
stringToBool str =
    if str == "true" then
        True

    else
        False


stringToInt : String -> Int
stringToInt str =
    str |> String.toInt |> Maybe.withDefault 0


stringToList : String -> List String
stringToList str =
    str |> String.words |> List.map normalize


normalize : String -> String
normalize str =
    if String.endsWith "," str then
        String.dropRight 1 str

    else
        str
