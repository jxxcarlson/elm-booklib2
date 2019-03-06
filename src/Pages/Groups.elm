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
    }


type Msg
    = NoOp
    | ReceiveGroupList (Result Http.Error (List Group))
    | SetCurrentGroup Group


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        ReceiveGroupList (Ok groupList) ->
            ( { model | groupList = groupList }, Cmd.none, NoUpdate )

        ReceiveGroupList (Err err) ->
            ( { model | message = "Error getting group" }, Cmd.none, NoUpdate )

        SetCurrentGroup group ->
            ( { model | currentGroup = Just group }, Cmd.none, NoUpdate )


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


groupDecoder : Decoder Group
groupDecoder =
    Decode.succeed Group
        |> required "id" Decode.int
        |> required "name" Decode.string
        |> required "chair" Decode.string
        |> required "cochair" Decode.string
        |> required "blurb" Decode.string
        |> required "members" (Decode.list Decode.string)


groupListDecoder : Decoder (List Group)
groupListDecoder =
    Decode.field "data" (Decode.list groupDecoder)
