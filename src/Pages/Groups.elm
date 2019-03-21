module Pages.Groups exposing
    ( Model
    , Msg(..)
    , getGroupList
    , getGroupListForUser
    , init
    , update
    , view
    )

import Common.Book
import Common.Style as Style
import Common.Utility as Utility
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
    | CreatingGroup
    | EditingGroup


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



--
-- MSG
--


type Msg
    = NoOp
    | ReceiveGroupList (Result Http.Error (List Group))
    | GroupCreated (Result Http.Error Group)
    | SetCurrentGroup Group
    | InputGroupName String
    | InputCochairName String
    | InputBlurb String
    | InputMembers String
    | NewGroup
    | CreateGroup
    | CancelCreateGroup
    | EditGroup
    | CancelEditGroup
    | UpdateGroup
    | GroupUpdated (Result Http.Error Group)


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
            let
                newGroupList =
                    group :: model.groupList
            in
            ( { model | appState = Default, groupList = newGroupList, currentGroup = Just group }, Cmd.none, NoUpdate )

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

        NewGroup ->
            let
                newModel =
                    clearGroupParams model
            in
            ( { newModel | appState = CreatingGroup, currentGroup = Nothing, message = "" }, Cmd.none, NoUpdate )

        CancelCreateGroup ->
            ( { model | appState = Default }, Cmd.none, NoUpdate )

        CreateGroup ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    let
                        members =
                            itemsFromString model.membersString
                                |> addToList user.username
                                |> addToList model.cochairName

                        group =
                            { id = -1
                            , name = model.groupName
                            , chair = user.username
                            , cochair = model.cochairName
                            , blurb = model.blurb
                            , members = members
                            }
                    in
                    case validateGroup group of
                        Invalid errorMessageList ->
                            ( { model | message = errorMessageList |> String.join ", " }, Cmd.none, NoUpdate )

                        Valid group_ ->
                            ( model, createGroup group_ user.token, NoUpdate )

        EditGroup ->
            ( setGroupParams { model | appState = EditingGroup }, Cmd.none, NoUpdate )

        CancelEditGroup ->
            ( { model | appState = Default, message = "Cancel Edit" }, Cmd.none, NoUpdate )

        UpdateGroup ->
            case ( getGroup model, sharedState.currentUser ) of
                ( Just group, Just user ) ->
                    case validateGroup group of
                        Invalid errorMessageList ->
                            ( { model | message = errorMessageList |> String.join ", " }, Cmd.none, NoUpdate )

                        Valid group_ ->
                            ( model, updateGroup group_ user.token, NoUpdate )

                ( _, _ ) ->
                    ( model, Cmd.none, NoUpdate )

        GroupUpdated (Ok group) ->
            let
                newGroupList =
                    Utility.listUpdateIf (\g -> g.id == group.id) (\g -> group) model.groupList
            in
            ( { model | appState = Default, groupList = newGroupList, currentGroup = Just group }, Cmd.none, NoUpdate )

        GroupUpdated (Err _) ->
            ( { model | appState = Default, message = "Error updating group" }, Cmd.none, NoUpdate )


type ResultGroup
    = Invalid (List String)
    | Valid Group


validateGroup : Group -> ResultGroup
validateGroup group =
    let
        errorList =
            validateGroupErrorList group
    in
    case errorList == [] of
        True ->
            Valid group

        False ->
            Invalid errorList


validateGroupErrorList : Group -> List String
validateGroupErrorList group =
    validateName group []
        |> validateCochair group
        |> validateBlurb group


validateName : Group -> List String -> List String
validateName group errorList =
    if String.length group.cochair > 3 then
        errorList

    else
        "Group name too short - need at least 3 characters" :: errorList


validateCochair : Group -> List String -> List String
validateCochair group errorList =
    if String.length group.cochair > 2 then
        errorList

    else
        "Co chair too short - need at least 2 characters" :: errorList


validateBlurb : Group -> List String -> List String
validateBlurb group errorList =
    if String.length group.blurb > 2 then
        errorList

    else
        "Blurb too short - need at least 2 characters" :: errorList


setGroupParams : Model -> Model
setGroupParams model =
    case model.currentGroup of
        Nothing ->
            model

        Just group ->
            { model
                | groupName = group.name
                , cochairName = group.cochair
                , blurb = group.blurb
                , membersString = stringOfItems <| group.members
            }


clearGroupParams : Model -> Model
clearGroupParams model =
    { model
        | groupName = ""
        , cochairName = ""
        , blurb = ""
        , membersString = ""
    }


getGroup : Model -> Maybe Group
getGroup model =
    case model.currentGroup of
        Nothing ->
            Nothing

        Just group ->
            Just
                { group
                    | name = model.groupName
                    , cochair = model.cochairName
                    , blurb = model.blurb
                    , members = itemsFromString model.membersString
                }


view : SharedState -> Model -> Element Msg
view sharedState model =
    column [ width (px <| sharedState.windowWidth), height (px <| sharedState.windowHeight - 45), centerY ]
        [ mainView sharedState model
        , footer sharedState model
        ]


mainView sharedState model =
    row [ spacing 12 ]
        [ groupListView sharedState model
        , Utility.showIf (model.appState == EditingGroup) (editGroupPanel model)
        , Utility.showIf (model.appState == Default) (viewGroup model.currentGroup)
        , Utility.showIf (model.appState == CreatingGroup) (createGroupPanel model)
        ]


groupListView sharedState model =
    column [ padding 40, spacing 12 ]
        [ el [ Font.bold ] (text "Groups")
        , viewGroups model.currentGroup model.groupList
        ]


viewGroups : Maybe Group -> List Group -> Element Msg
viewGroups currentGroup groupList =
    column
        [ width (px 300)
        , height (px 400)
        , spacing 10
        , padding 20
        , Background.color (Style.makeGrey 0.4)
        , Font.size inputFontSize
        ]
        (List.map (groupNameButton currentGroup) groupList)


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
                , el [ Font.size inputFontSize ] (text <| "Chair: " ++ group.chair)
                , el [ Font.size inputFontSize ] (text <| "Co-chair: " ++ group.cochair)
                , el [ Font.size inputFontSize ] (text <| "Members: " ++ String.join ", " group.members)
                , el [ Font.size inputFontSize, Font.bold ] (text <| "Blurb")
                , paragraph [ Font.size inputFontSize, Font.italic ] [ text group.blurb ]
                ]


altView sharedState model =
    column [ padding 40 ] [ el [] (text "Under construction ..") ]


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ newGroupButton
        , Utility.showIf (model.currentGroup /= Nothing) editGroupButton
        ]



--
-- INPUT
--


createGroupPanel model =
    column createPanelStyle
        [ inputGroupName model
        , inputCochairName model
        , inputMembers model
        , inputBlurb model
        , row [ spacing 12 ] [ createGroupButton, cancelCreateGroupButton ]
        , el [ Font.size inputFontSize, Font.color Style.blue ] (text model.message)
        ]


editPanelStyle =
    [ spacing 12, Border.width 1, paddingXY 18 18 ]


createPanelStyle =
    [ spacing 12, Border.width 1, paddingXY 18 18, moveDown 48 ]


editGroupPanel model =
    column editPanelStyle
        [ el [ Font.size inputFontSize ] (text <| "Chair:: " ++ chairName model)
        , inputGroupName model
        , inputCochairName model
        , inputMembers model
        , inputBlurb model
        , row [ spacing 12 ] [ updateGroupButton, cancelEditGroupButton ]
        ]


chairName : Model -> String
chairName model =
    case model.currentGroup of
        Nothing ->
            "No group defined"

        Just group ->
            group.chair


newGroupButton : Element Msg
newGroupButton =
    Input.button Style.button
        { onPress = Just NewGroup
        , label = el [ centerX ] (Element.text "New Group")
        }


createGroupButton : Element Msg
createGroupButton =
    Input.button Style.button
        { onPress = Just CreateGroup
        , label = el [ centerX ] (Element.text "Create")
        }


cancelCreateGroupButton : Element Msg
cancelCreateGroupButton =
    Input.button Style.button
        { onPress = Just CancelCreateGroup
        , label = el [ centerX ] (Element.text "Cancel")
        }


editGroupButton : Element Msg
editGroupButton =
    Input.button Style.button
        { onPress = Just EditGroup
        , label = el [ centerX ] (Element.text "Edit Group")
        }


updateGroupButton : Element Msg
updateGroupButton =
    Input.button Style.button
        { onPress = Just UpdateGroup
        , label = el [ centerX ] (Element.text "Update")
        }


cancelEditGroupButton : Element Msg
cancelEditGroupButton =
    Input.button Style.button
        { onPress = Just CancelEditGroup
        , label = el [ centerX ] (Element.text "Cancel")
        }


inputStyle =
    [ width (px 200)
    , height (px 35)
    , Font.size inputFontSize
    ]


inputTextAreaStyle h =
    [ width (px 300)
    , height (px h)
    , Font.size inputFontSize
    ]


inputGroupName model =
    Input.text inputStyle
        { text = model.groupName
        , placeholder = Nothing
        , onChange = InputGroupName
        , label = Input.labelAbove [ Font.size inputFontSize ] (text "Group name ")
        }


inputCochairName model =
    Input.text inputStyle
        { text = model.cochairName
        , placeholder = Nothing
        , onChange = InputCochairName
        , label = Input.labelAbove [ Font.size inputFontSize ] (text "Cochair ")
        }


inputMembers1 model =
    Input.text inputStyle
        { text = model.membersString
        , placeholder = Nothing
        , onChange = InputMembers
        , label = Input.labelAbove [ Font.size inputFontSize ] (text "Members ")
        }


inputMembers model =
    Input.multiline (inputTextAreaStyle 100)
        { onChange = InputMembers
        , text = model.membersString
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size inputFontSize ] (text "Members")
        , spellcheck = False
        }


inputBlurb model =
    Input.multiline (inputTextAreaStyle 100)
        { onChange = InputBlurb
        , text = model.blurb
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size inputFontSize ] (text "Blurb")
        , spellcheck = False
        }


inputFontSize =
    15



--
-- DECODERS AND ENCODERS
--


type alias GroupRecord =
    { group : Group }


groupRecordDecoder : Decoder Group
groupRecordDecoder =
    Decode.field "data" groupDecoder


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



--
-- REQUESTS
--


getGroupList : Cmd Msg
getGroupList =
    Http.get
        { url = Configuration.backend ++ "/api/groups"
        , expect = Http.expectJson ReceiveGroupList groupListDecoder
        }


getGroupListForUser : String -> Cmd Msg
getGroupListForUser username =
    Http.get
        { url = Configuration.backend ++ "/api/groups?user=" ++ username
        , expect = Http.expectJson ReceiveGroupList groupListDecoder
        }


createGroup : Group -> String -> Cmd Msg
createGroup group token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/groups"
        , body = Http.jsonBody (groupEncoder group)
        , expect = Http.expectJson GroupCreated groupRecordDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateGroup : Group -> String -> Cmd Msg
updateGroup group token =
    Http.request
        { method = "Put"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/groups/" ++ String.fromInt group.id
        , body = Http.jsonBody (groupEncoder group)
        , expect = Http.expectJson GroupUpdated groupRecordDecoder
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
        |> List.filter (\item -> item /= "")
        |> List.map String.trim


stringOfItems : List String -> String
stringOfItems list =
    String.join ", " list


addToList : a -> List a -> List a
addToList item list =
    case List.member item list of
        True ->
            list

        False ->
            item :: list
