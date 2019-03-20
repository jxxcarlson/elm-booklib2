module Pages.Groups exposing
    ( Model
    , Msg(..)
    , getGroupList
    , init
    , update
    , view
    )

import Common.Book
import Common.Style as Style
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import SharedState exposing (SharedState, SharedStateUpdate(..))


type alias Model =
    { appState : AppState
    , currentGroup : Maybe Group
    , groupList : List Group
    , message : String
    , groupName : String
    , cochairName : String
    , blurb : String
    , membersString : String
    }


type alias Group =
    { id : Int
    , name : String
    , chair : String
    , cochair : String
    , blurb : String
    , members : List String
    }


type AppState
    = Default


init : Model
init =
    { appState = Default
    , currentGroup = Nothing
    , groupList = []
    , message = ""
    , groupName = ""
    , cochairName = ""
    , blurb = ""
    , membersString = ""
    }


type Msg
    = NoOp
    | ReceiveGroupList (Result Http.Error (List Group))
    | GroupCreated (Result Http.Error Group)
    | SetCurrentGroup Group
    | InputGroupName String
    | InputCochairName String
    | InputBlurb String
    | InputMembers String
    | CreateGroup


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        ReceiveGroupList (Ok groupList) ->
            ( { model | groupList = groupList }, Cmd.none, NoUpdate )

        ReceiveGroupList (Err err) ->
            ( { model | message = "Error getting group" }, Cmd.none, NoUpdate )

        GroupCreated (Ok group) ->
            ( model, Cmd.none, NoUpdate )

        GroupCreated (Err err) ->
            ( model, Cmd.none, NoUpdate )

        SetCurrentGroup group ->
            ( { model | currentGroup = Just group }, Cmd.none, NoUpdate )

        InputGroupName str ->
            ( { model | groupName = str }, Cmd.none, NoUpdate )

        InputCochairName str ->
            ( { model | cochairName = str }, Cmd.none, NoUpdate )

        InputMembers str ->
            ( { model | membersString = str }, Cmd.none, NoUpdate )

        InputBlurb str ->
            ( { model | blurb = str }, Cmd.none, NoUpdate )

        CreateGroup ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    let
                        group =
                            { id = -1
                            , name = model.groupName
                            , chair = user.username
                            , cochair = model.cochairName
                            , blurb = model.blurb
                            , members = itemsFromString model.membersString
                            }
                    in
                    ( model, createGroup group user.token, NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    case Maybe.map .admin sharedState.currentUser of
        Just True ->
            mainView sharedState model

        _ ->
            altView sharedState model


mainView sharedState model =
    row [ spacing 12 ]
        [ groupListView sharedState model
        , viewGroup model.currentGroup
        , createGroupPanel model
        ]


groupListView sharedState model =
    column [ padding 40, spacing 12 ]
        [ el [ Font.bold ] (text "Groups")
        , viewGroups model.currentGroup model.groupList
        ]


viewGroups : Maybe Group -> List Group -> Element Msg
viewGroups currentGroup groupList =
    column [ width (px 300), height (px 400), spacing 10, padding 20, Background.color (Style.makeGrey 0.4) ] (List.map (groupNameButton currentGroup) groupList)


groupNameButton currentGroup_ group =
    let
        highlighted =
            case currentGroup_ of
                Nothing ->
                    False

                Just currentGroup ->
                    currentGroup.id == group.id
    in
    Input.button (Style.titleButton highlighted)
        { onPress = Just (SetCurrentGroup group)
        , label = Element.text group.name
        }


viewGroup : Maybe Group -> Element Msg
viewGroup group_ =
    case group_ of
        Nothing ->
            Element.none

        Just group ->
            column [ width (px 300), height (px 400), spacing 8, Border.width 1, padding 20, moveDown 20 ]
                [ el [ Font.bold ] (text group.name)
                , el [ Font.size 14 ] (text <| "Members: " ++ String.join ", " group.members)
                , paragraph [ Font.size 14 ] [ text group.blurb ]
                ]


altView sharedState model =
    column [ padding 40 ] [ el [] (text "Under construction ..") ]


getGroupList : Cmd Msg
getGroupList =
    Http.get
        { url = Configuration.backend ++ "/api/groups"
        , expect = Http.expectJson ReceiveGroupList groupListDecoder
        }



--
-- INPUT
--


createGroupPanel model =
    column [ spacing 12 ]
        [ inputGroupName model
        , inputCochairName model
        , inputMembers model
        , inputBlurb model
        , createGroupButton
        ]


createGroupButton : Element Msg
createGroupButton =
    Input.button Style.button
        { onPress = Just CreateGroup
        , label = el [ centerX ] (Element.text "Creaate")
        }


inputStyle =
    [ width (px 200), height (px 35) ]


inputTextAreaStyle =
    [ width (px 300), height (px 200) ]


inputGroupName model =
    Input.text inputStyle
        { text = model.groupName
        , placeholder = Nothing
        , onChange = InputGroupName
        , label = Input.labelAbove [ Font.size 16 ] (text "Group name ")
        }


inputCochairName model =
    Input.text inputStyle
        { text = model.cochairName
        , placeholder = Nothing
        , onChange = InputCochairName
        , label = Input.labelAbove [ Font.size 16 ] (text "Cochair ")
        }


inputMembers model =
    Input.text inputStyle
        { text = model.membersString
        , placeholder = Nothing
        , onChange = InputMembers
        , label = Input.labelAbove [ Font.size 16 ] (text "Members ")
        }


inputBlurb model =
    Input.multiline inputTextAreaStyle
        { onChange = InputBlurb
        , text = model.blurb
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 16 ] (text "Blurb")
        , spellcheck = False
        }



--
-- DECODERS AND ENCODERS
--


groupDecoder : Decoder Group
groupDecoder =
    Decode.succeed Group
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "chair" Decode.string
        |> required "cochair" Decode.string
        |> required "blurb" Decode.string
        |> required "members" (Decode.list Decode.string)


groupEncoder : Group -> Encode.Value
groupEncoder group =
    Encode.object [ ( "group", groupEncoder_ group ) ]


groupEncoder_ : Group -> Encode.Value
groupEncoder_ group =
    Encode.object
        [ ( "name", Encode.string group.name )
        , ( "chair", Encode.string group.chair )
        , ( "cochair", Encode.string group.cochair )
        , ( "blurb", Encode.string group.blurb )
        , ( "members", Encode.list Encode.string group.members )
        ]


groupListDecoder : Decoder (List Group)
groupListDecoder =
    Decode.field "data" (Decode.list groupDecoder)


createGroup : Group -> String -> Cmd Msg
createGroup group token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/groups"
        , body = Http.jsonBody (groupEncoder group)
        , expect = Http.expectJson GroupCreated groupDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



--
-- HELPERS
--


itemsFromString : String -> List String
itemsFromString str =
    str
        |> String.split ","
        |> List.map String.trim
