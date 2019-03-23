module Pages.Groups exposing
    ( Model
    , Msg(..)
    , getGroupList
    , getGroupListForUser
    , getInvitations
    , init
    , update
    , view
    )

import Book.Coders
import Book.Types exposing (Book)
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
import User.Coders
import User.Invitation exposing(Invitation, encodeInvitation, invitationDecoder, invitationsDecoder, Status(..))


type alias Model =
    { appState : AppState
    , currentGroup : Maybe Group
    , groupList : List Group
    , bookList : List Book
    , currentUserName : Maybe String
    , currentBook : Maybe Book
    , message : String
    , groupName : String
    , cochairName : String
    , blurb : String
    , membersString : String
    , memberName : String
    , invitations : List Invitation
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
    | MakingInvitation
    | ViewingGroup
    | ViewingBookList
    | ViewingBook


init : Model
init =
    { appState = Default
    , currentGroup = Nothing
    , groupList = []
    , bookList = []
    , currentUserName = Nothing
    , currentBook = Nothing
    , message = ""
    , groupName = ""
    , cochairName = ""
    , blurb = ""
    , membersString = ""
    , memberName = ""
    , invitations = []
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
    | ViewUserBookList String
    | ReceiveBookList (Result Http.Error (List Book))
    | SetCurrentBook Book
    | GotInvitation (Result Http.Error Invitation)
    | InputMemberName String
    | CancelInvitation
    | SendInvitation
    | MakeInvitation
    | GotInvitations (Result Http.Error (List Invitation))


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
            ( { model | currentGroup = Just group, appState = Default }, getInvitations group.id, NoUpdate )

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

        ViewUserBookList userName ->
            ( { model | currentUserName = Just userName, appState = ViewingBookList }
            , getSharedBooks userName (getToken sharedState)
            , NoUpdate
            )

        ReceiveBookList (Ok bookList) ->
            let
                currentBook =
                    List.head bookList

                notes =
                    case currentBook of
                        Nothing ->
                            ""

                        Just book ->
                            book.notes
            in
            ( { model
                | bookList = bookList
              }
            , Cmd.none
            , SharedState.UpdateCurrentBook currentBook
            )

        ReceiveBookList (Err err) ->
            ( { model | message = "Error receiving book list" }, Cmd.none, SharedState.UpdateCurrentBook Nothing )

        SetCurrentBook book ->
            ( { model | currentBook = Just book, appState = ViewingBook }
            , Cmd.none
            , NoUpdate
            )

        GotInvitation (Ok _) ->
            ( {model | message = "Processing invitation"}, Cmd.none, NoUpdate )

        GotInvitation (Err _) ->
                    ( {model | message = "Error processing invitation"}, Cmd.none, NoUpdate )

        InputMemberName str ->
            ( {model | memberName = str}, Cmd.none, NoUpdate)

        CancelInvitation ->
            case model.currentGroup of
                Nothing -> ( {model | appState = Default}, Cmd.none, NoUpdate)
                Just group -> ( {model | appState = Default}, getInvitations group.id, NoUpdate)

        SendInvitation ->
            let
                cmd = case (sharedState.currentUser, model.currentGroup) of
                              (Just user, Just group) ->
                                  case  makeInvitation model of
                                      Nothing -> Cmd.none
                                      Just invitation -> createInvitation invitation user.token
                              (_, _) -> Cmd.none


            in
              ( {model | message = "Sending invitation"}, cmd, NoUpdate)

        MakeInvitation ->
            ( {model | appState = MakingInvitation}, Cmd.none, NoUpdate)

        GotInvitations (Ok invitations) ->
            ( {model | invitations  = invitations, message = "Got invitations"}, Cmd.none, NoUpdate)

        GotInvitations (Err _) ->
                    ( {model | message  = "Error getting invitations"}, Cmd.none, NoUpdate)

getToken : SharedState -> String
getToken sharedState =
    case sharedState.currentUser of
        Nothing ->
            "Invalid"

        Just user ->
            user.token


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
    column [ width (px <| sharedState.windowWidth), height (px <| sharedState.windowHeight - 45  - windowInset) ]
        [ mainView sharedState model
        , footer sharedState model
        ]


mainView sharedState model =
    row [ spacing 12, padding 20 ]
        [ groupListView sharedState model
        , Utility.showIf (model.appState == EditingGroup) (editGroupPanel model)
        , Utility.showIf (List.member model.appState [Default, ViewingBookList, ViewingBook]) (viewGroup sharedState model model.currentGroup)
        , Utility.showIf (model.appState == CreatingGroup) (createGroupPanel model)
        , Utility.showIf (model.appState == MakingInvitation) (invitationPanel model)
        , Utility.showIf (model.currentUserName /= Nothing && List.member model.appState [ViewingBookList, ViewingBook]) (bookListDisplay sharedState model)
        , Utility.showIf (model.currentUserName /= Nothing && model.currentBook /= Nothing && model.appState == ViewingBook) (row [ padding 20, Border.width 1 ] [ Common.Book.notesViewedAsMarkdown 70 "380px" (notesHeight sharedState) model.currentBook ])
        ]


inset =
    20


bookListDisplay : SharedState -> Model -> Element Msg
bookListDisplay sharedState model =
    Element.row [ spacing 20, alignTop ]
        [ bookListTable sharedState model
        ]


notesHeight sharedState =
    String.fromInt (sharedState.windowHeight - 178 - windowInset) ++ "px"


bookListTable : SharedState -> Model -> Element Msg
bookListTable sharedState model =
    Element.column
        [ width fill
        , clipY
        , height (px (sharedState.windowHeight - 115 - windowInset))
        , spacing 10
        , padding 10
        , Background.color Style.charcoal
        , Font.color Style.white
        ]
        [ bookListTableHeader sharedState model
        , listBooks sharedState model
        ]


bookListTableHeader : SharedState -> Model -> Element Msg
bookListTableHeader sharedState model =
    Element.row [ spacing 15, Background.color Style.charcoal, Font.color Style.white ]
        [--Element.el [ Font.bold, Font.color Style.white ] (text <| bookInfo model)
         -- , Element.el [ Font.size 14, Font.color Style.orange ] (text <| totalsString sharedState model)
        ]


listBooks : SharedState -> Model -> Element Msg
listBooks sharedState model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px (sharedState.windowHeight - 150))
        , Background.color Style.charcoal
        , Font.color Style.white
        , clipX
        ]
        { data = model.bookList
        , columns =
            [ { header = Element.el Style.tableHeading (Element.text "Title")
              , width = px 200
              , view =
                    \book ->
                        titleButton book model.currentBook
              }
            ]
        }


titleButton book maybeCurrentBook =
    let
        highlighted =
            case maybeCurrentBook of
                Nothing ->
                    False

                Just currentBook ->
                    currentBook.id == book.id
    in
    Input.button (Style.titleButton highlighted ++ [ clipX ])
        { onPress = Just (SetCurrentBook book)
        , label = Element.text book.title
        }


sendInvitationButton  =
    Input.button Style.button
            { onPress = Just SendInvitation
            , label = (text "Send invitation")
            }

makeInvitationButton  =
    Input.button Style.button
            { onPress = Just MakeInvitation
            , label = (text "New invitation")
            }



matchBookAndUserIds : SharedState -> Bool
matchBookAndUserIds sharedState =
    let
        uid =
            Maybe.map .id sharedState.currentUser

        bookUid =
            Maybe.map .userId sharedState.currentBook
    in
    case ( uid, bookUid ) of
        ( Just id1, Just id2 ) ->
            id1 /= id2

        ( _, _ ) ->
            False


groupListView sharedState model =
    column [ spacing 12 ]
        [ el [ Font.bold, paddingXY 0 0 ] (text "Groups")
        , viewGroups sharedState model.currentGroup model.groupList
        ]

windowInset = 10

viewGroups : SharedState -> Maybe Group -> List Group -> Element Msg
viewGroups sharedState currentGroup groupList =
    column
        [ width (px 200)
        , height (px (sharedState.windowHeight - 150 - windowInset))
        , spacing 10
        , Background.color (Style.makeGrey 0.4)
        , Font.size inputFontSize
        , padding 12
        ]
        (List.map (displayGroup currentGroup) groupList)


displayGroup : Maybe Group -> Group -> Element Msg
displayGroup currentGroup_ group =
    row [ Font.size inputFontSize ]
        [ el [] (groupInfoButton currentGroup_ group)
        ]


groupInfoButton currentGroup_ group =
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


viewGroup : SharedState -> Model -> Maybe Group -> Element Msg
viewGroup sharedState model group_ =
    case group_ of
        Nothing ->
            Element.none

        Just group ->
            column [ width (px 300), height (px (sharedState.windowHeight - 115 - windowInset)), spacing 12, Border.width 1, Background.color (Style.makeGrey 0.9), paddingXY 12 12, alignTop ]
                [ el [ Font.bold ] (text group.name)
                , el [ Font.size inputFontSize ] (text <| "Chair: " ++ group.chair)
                , el [ Font.size inputFontSize ] (text <| "Co-chair: " ++ group.cochair)
                , el [ Font.size inputFontSize, Font.bold ] (text <| "Blurb")
                , paragraph [ Font.size inputFontSize, Font.italic ] [ text group.blurb ]
                , el [ Font.size inputFontSize, Font.bold ] (text <| "Members")
                , column
                    [ spacing 8
                    , height (px (scale 0.5 (sharedState.windowHeight - 190 - windowInset)))
                    , width (px 250)
                    , centerX
                    , scrollbarY
                    , Border.width 1
                    , padding 8
                    ]
                    (List.map (memberButton model) group.members)
                , el [ Font.size inputFontSize, Font.bold ] (text <| "Invitations")
                , column
                    [ spacing 8
                    , height (px (scale 0.4 (sharedState.windowHeight - 190 - windowInset)))
                    , width (px 250)
                    , centerX
                    , scrollbarY
                    , Border.width 1
                    , padding 8
                    ]
                    (List.map viewInvitation model.invitations)
                ]

viewInvitation : Invitation -> Element Msg
viewInvitation invitation =
    el [ Font.size 14] (text invitation.invitee)

scale : Float -> Int -> Int
scale factor k =
    round <| factor * (toFloat k)

showMember : String -> Element Msg
showMember username =
    el [ Font.size inputFontSize ] (text username)


memberButton : Model -> String -> Element Msg
memberButton model username =
    Input.button (highlightColor (model.currentUserName == Just username) ++ [ width (px 145) ])
        { onPress = Just (ViewUserBookList username)
        , label = el [ Font.size 14 ] (Element.text username)
        }


highlightColor flag =
    if flag then
        [ Font.bold, Font.color Style.darkRed ]

    else
        [ Font.color Style.blue ]


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ newGroupButton
        , Utility.showIf (model.currentGroup /= Nothing) editGroupButton
        , Utility.showIf (model.currentGroup /= Nothing) makeInvitationButton
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
    [ spacing 12, Border.width 1, paddingXY 18 18 ]


editGroupPanel model =
    column editPanelStyle
        [ el [ Font.size inputFontSize ] (text <| "Chair:: " ++ chairName model)
        , inputGroupName model
        , inputCochairName model
        , inputMembers model
        , inputBlurb model
        , row [ spacing 12 ] [ updateGroupButton, cancelEditGroupButton ]
        ]

invitationPanel : Model -> Element Msg
invitationPanel model =
    case model.currentGroup of
        Nothing -> Element.none
        Just group ->
          column (editPanelStyle ++ [alignTop])
            [ el [ Font.size inputFontSize ] (text <| "Group: " ++ group.name)
            , inputMemberName model
            , row [ spacing 12 ] [ sendInvitationButton, cancelInvitationButton ]
            , el [Font.size 14] (text model.message)
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

cancelInvitationButton : Element Msg
cancelInvitationButton =
    Input.button Style.button
        { onPress = Just CancelInvitation
        , label = el [ centerX ] (Element.text "Done")
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

inputMemberName model =
   Input.text inputStyle
           { text = model.memberName
           , placeholder = Nothing
           , onChange = InputMemberName
           , label = Input.labelAbove [ Font.size inputFontSize ] (text "New member")
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


getSharedBooks : String -> String -> Cmd Msg
getSharedBooks username token =
    Http.request
        { method = "Get"
        , headers = []
        , url = Configuration.backend ++ "/api/books?shared=" ++ username
        , body = Http.jsonBody (User.Coders.tokenEncoder token)
        , expect = Http.expectJson ReceiveBookList Book.Coders.bookListDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


createInvitation : Invitation -> String -> Cmd Msg
createInvitation invitation token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/invite"
        , body = Http.jsonBody (encodeInvitation invitation)
        , expect = Http.expectJson GotInvitation invitationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


makeInvitation : Model -> Maybe Invitation
makeInvitation model =
    case model.currentGroup of
        Nothing -> Nothing
        Just group ->
            Just {
               invitee = model.memberName
               , inviter = group.chair
               , groupName = group.name
               , groupId = group.id
               , status = Waiting
            }

getInvitations :  Int -> Cmd Msg
getInvitations groupId  =
      Http.get
        { url = Configuration.backend ++ "/api/invitations?group=" ++ String.fromInt groupId
        , expect = Http.expectJson GotInvitations invitationsDecoder
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
