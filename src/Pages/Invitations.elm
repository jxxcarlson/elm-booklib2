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

--
-- MODEL
--


type alias Model =
    { invitations : List Invitation

    }


init : Model
init =
    { invitations = []

    }



type Msg
    =
     NoOp




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


view : SharedState -> Model -> Element Msg
view sharedState model =
    column [ width (px <| sharedState.windowWidth)
      , height (px <| sharedState.windowHeight - 45  - windowInset)
      , Background.color (Style.makeGrey 0.7)
      ]
        [ mainView sharedState model
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
        , width (px 250)
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
    el [ Font.size 14] (text <| invitation.inviter ++ ": " ++ invitation.groupName)


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el [Font.size 14, Font.color Style.white] (text "Invitations")
        ]


windowInset = 10

inset =
    20
