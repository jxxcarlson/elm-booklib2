module Pages.Invitations exposing (..)



import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Invitation exposing(Invitation)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Common.Style as Style
import Common.Utility as Utility
import Http
import Configuration

--
-- MODEL
--


type alias Model =
    { invitations : List Invitation
    , message : String

    }


init : Model
init =
    { invitations = []
    , message = ""

    }



type Msg
    =
     NoOp
     | ConfirmRejection (Result Http.Error Invitation)
     | ConfirmAcceptance (Result Http.Error Invitation)
     | Accept Invitation
     | Reject Invitation




--
-- UPDATE
--


{-| NOTE that the Book udpdate function is of the usual
kind -- there is no SharedState parameter. Contrast
this with the update function for SettiÂngs.
-}
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )
        ConfirmRejection (Ok invitation) ->
            ( {model | message = "Confirmed: rejection"}, Cmd.none, NoUpdate)
        ConfirmRejection (Err _ ) ->
            ( {model | message = "Error: reject invitation"}, Cmd.none, NoUpdate)

        ConfirmAcceptance (Ok invitation) ->
                    ( {model | message = "Confirmed: acceptance"}, Cmd.none, NoUpdate)
        ConfirmAcceptance (Err _ ) ->
                    ( {model | message = "Error: accept invitation"}, Cmd.none, NoUpdate)
        Accept invitation ->
            ( { model | message = "Accepting invitation"},acceptInvitation invitation "" , NoUpdate)

        Reject invitation ->
             ( { model | message = "Rejecting invitation"},rejectInvitation invitation "" , NoUpdate)



view : SharedState -> Model -> Element Msg
view sharedState model =
    column [ width (px <| sharedState.windowWidth)
      , height (px <| sharedState.windowHeight - 45  - windowInset)
      , Background.color (Style.makeGrey 0.7)
      ]
        [ mainView sharedState model
        , el [Font.size 14, moveRight 20] (text model.message)
        , footer sharedState model
        ]

mainView sharedState model =
    row [ spacing 12, padding 20 ]
        [ invitationListView sharedState model

        ]

invitationListView sharedState model =
    column
        [ spacing 8
        , height (px (Utility.scale 0.4 (sharedState.windowHeight - 190 - windowInset)))
        , width (px 350)
        , centerX
        , scrollbarY
        , Border.width 1
        , padding 8
        , Background.color (Style.makeGrey 0.9)
        ]
          ([ el [Font.size 14, Font.bold] (text "Invitations")] ++
          (List.map viewInvitation sharedState.invitations))

viewInvitation : Invitation -> Element Msg
viewInvitation invitation =
    row [spacing 12] [
      el [ Font.size 14] (text <| invitation.inviter ++ ": " ++ invitation.groupName)
      , acceptButton invitation
      , rejectButton invitation
     ]


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el [Font.size 14, Font.color Style.white] (text "Invitations")
        ]



--
-- REQUESTS
--


acceptInvitation : Invitation -> String -> Cmd Msg
acceptInvitation invitation token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/acceptinvitation"
        , body = Http.jsonBody (User.Invitation.encodeInvitation invitation)
        , expect = Http.expectJson ConfirmAcceptance User.Invitation.invitationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }

rejectInvitation : Invitation -> String -> Cmd Msg
rejectInvitation invitation token =
    Http.request
        { method = "Post"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Configuration.backend ++ "/api/rejectinvitation"
        , body = Http.jsonBody (User.Invitation.encodeInvitation invitation)
        , expect = Http.expectJson ConfirmRejection User.Invitation.invitationDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


rejectButton invitation =
    Input.button Style.button
        { onPress = Just (Reject invitation)
        , label = el [ Font.size 14 ] (text "Reject")
        }

acceptButton invitation =
    Input.button Style.button
        { onPress = Just (Accept invitation)
        , label = el [ Font.size 14 ] (text "Accept")
        }

windowInset = 10

inset =
    20
