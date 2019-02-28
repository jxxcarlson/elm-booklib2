module Common.Widget exposing (footer)

import Element exposing (..)
import Common.Utility as Utility


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| userStatus sharedState.currentUser)
        , el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        ]
