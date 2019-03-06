module Pages.CurrentUser exposing
    ( Model
    , currentSharedStateView
    , initModel
    , update
    , view
    )

import Browser.Navigation exposing (pushUrl)
import Common.Style as Style
import Common.Utility as Utility
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Http exposing (Error(..))
import OutsideInfo exposing (InfoForOutside(..))
import Routing.Helpers exposing (Route(..), reverseRoute)
import SharedState exposing (SharedState, SharedStateUpdate(..))
import User.Session as Session
import User.Types exposing (Msg(..), State(..), User)


type alias Model =
    { message : String
    , email : String
    , password : String
    , username : String
    , state : State
    }


initModel : Model
initModel =
    { message = ""
    , username = ""
    , email = ""
    , password = ""
    , state = NotSignedIn
    }


{-| NOTE that the Settings updpate function takes a SharedState parameter.
In addition, its return value is a triple, with the last element
of type SharedStateUpdate.
-}
update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        AcceptEmail str ->
            ( { model | email = str }, Cmd.none, NoUpdate )

        AcceptPassword str ->
            ( { model | password = str }, Cmd.none, NoUpdate )

        AcceptUsername str ->
            ( { model | username = str }, Cmd.none, NoUpdate )

        SignIn ->
            ( { model | message = "" }, Session.authenticate model.email model.password, NoUpdate )

        SignOut ->
            ( { model
                | message = ""
                , username = ""
                , email = ""
                , password = ""
                , state = NotSignedIn
              }
            , Cmd.none
            , InvalidateCurrentUser
            )

        Register ->
            ( { model | message = "", state = Registering }, Cmd.none, NoUpdate )

        CancelRegistration ->
            ( { model | message = "", state = NotSignedIn }, Cmd.none, NoUpdate )

        CancelSignin ->
            ( { model | message = "", state = NotSignedIn }, Cmd.none, NoUpdate )

        SubmitRegistration ->
            ( { model | message = "", state = Registering }, Session.registerUser model.username model.email model.password, NoUpdate )

        ProcessAuthentication (Ok user) ->
            ( { model
                | message = "Success! Welcome " ++ user.username ++ "."
                , username = ""
                , email = ""
                , password = ""
                , state = SignedIn
              }
            , Cmd.batch
                [ pushUrl sharedState.navKey "#books"
                , OutsideInfo.sendInfoOutside (UserData (OutsideInfo.userEncoder user))
                ]
            , UpdateCurrentUser (Just user)
            )

        ProcessAuthentication (Err err) ->
            ( { model | message = "Invalid password or username", state = SigningIn }, Cmd.none, UpdateCurrentUser Nothing )

        AcceptRegistration (Ok user) ->
            ( { model
                | message = "Success! Welcome " ++ user.username ++ "."
                , username = ""
                , email = ""
                , password = ""
                , state = SignedIn
              }
            , Cmd.none
            , UpdateCurrentUser (Just user)
            )

        AcceptRegistration (Err err) ->
            ( { model | message = httpErrorExplanation err, state = SigningIn }, Cmd.none, UpdateCurrentUser Nothing )

        ReceiveUpdateUser (Ok user) ->
            ( { model | message = "Your profile has been updated" }, Cmd.none, NoUpdate )

        ReceiveUpdateUser (Err err) ->
            ( { model | message = "Error updating user" }, Cmd.none, NoUpdate )

        NavigateTo route ->
            ( model, pushUrl sharedState.navKey (reverseRoute route), NoUpdate )

        SetState state ->
            ( { model | state = state }, Cmd.none, NoUpdate )

        ToggleUserPublic flag ->
            case sharedState.currentUser of
                Nothing ->
                    ( model, Cmd.none, NoUpdate )

                Just user ->
                    let
                        nextUser =
                            { user | public = flag }

                        cmd =
                            Session.updateUser nextUser nextUser.token
                    in
                    ( model, cmd, SharedState.UpdateCurrentUser (Just nextUser) )


view : SharedState -> Model -> Element Msg
view sharedState model =
    column (Style.mainColumn fill fill)
        [ row [ spacing 20 ]
            [ welcomeColumn sharedState model
            , signInColumn sharedState model
            ]
        , footer sharedState model
        ]


welcomeColumn : SharedState -> Model -> Element Msg
welcomeColumn sharedState model =
    column [ alignTop, spacing 10 ]
        [ row [] [ el [ Font.bold ] (text "Welcome to Booklib.io") ]
        , row [] [ el [] (text "Create and share your annotated book list") ]
        , image []
            { src = "https://www.hastac.org/sites/default/files/upload/images/post/books.jpg"
            , description = "Library"
            }
        ]



-- https://www.hastac.org/sites/default/files/upload/images/post/books.jpg


signInColumn : SharedState -> Model -> Element Msg
signInColumn sharedState model =
    column (Style.signinColumn (px 480) fill)
        [ showIf (model.state /= SignedIn) (inputUsername model)
        , showIf (model.state /= SignedIn) (inputEmail model)
        , showIf (model.state /= SignedIn) (inputPassword model)
        , row [ moveRight 125, spacing 12 ] [ signInOrCancelButton model, registerButton model ]
        , el [ Font.size 18 ] (text model.message)
        , showIf (model.state == SignedIn) (publicCheckbox sharedState)
        , showIf (model.state == SignedIn) (el [] (text (tagsToString sharedState.currentUser)))
        ]


tagsToString : Maybe User -> String
tagsToString currentUser_ =
    case currentUser_ of
        Nothing ->
            "No tags"

        Just user ->
            user.tags
                |> String.join ", "
                |> (\x -> "Tags: " ++ x)


footer : SharedState -> Model -> Element Msg
footer sharedState model =
    row Style.footer
        [ el Style.footerItem (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        , el Style.footerItem (text <| stateAsString sharedState model.state)
        ]


showIf : Bool -> Element Msg -> Element Msg
showIf flag element =
    if flag then
        element

    else
        Element.none


inputEmail model =
    Input.text inputStyle
        { onChange = AcceptEmail
        , text = model.email
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 12, Font.bold, width (px 120) ] (text "Email")
        }


publicCheckbox : SharedState -> Element Msg
publicCheckbox sharedState =
    case sharedState.currentUser of
        Nothing ->
            Element.none

        Just user ->
            Input.checkbox
                []
                { onChange = ToggleUserPublic
                , icon = icon
                , checked = user.public
                , label = Input.labelLeft [ Font.size 16, moveDown 1 ] (text "Share your reading list?")
                }


icon : Bool -> Element msg
icon status =
    case status of
        True ->
            el [ Font.size 18, Font.bold, Font.color Style.blue, moveDown 1 ] (text "Yes")

        False ->
            el [ Font.size 18, Font.bold, Font.color Style.darkRed, moveDown 1 ] (text "No")


inputStyle =
    [ width (px 300), Background.color (Style.makeGrey 0.3), Font.color Style.white ]


inputUsername model =
    case model.state of
        Registering ->
            Input.text inputStyle
                { onChange = AcceptUsername
                , text = model.username
                , placeholder = Nothing
                , label = Input.labelLeft [ moveDown 12, Font.bold, width (px 120) ] (text "Username")
                }

        _ ->
            Element.none


inputPassword model =
    Input.currentPassword inputStyle
        { onChange = AcceptPassword
        , text = model.password
        , placeholder = Nothing
        , label = Input.labelLeft [ moveDown 12, Font.bold, width (px 120) ] (text "Password")
        , show = False
        }


signInOrCancelButton model =
    case model.state of
        NotSignedIn ->
            signInButton

        SigningIn ->
            signInButton

        Registering ->
            cancelRegistrationButton

        SignedIn ->
            signOutButton


signInButton =
    Input.button Style.button
        { onPress = Just SignIn
        , label = el [] (text "Sign In")
        }


signOutButton =
    Input.button Style.button
        { onPress = Just SignOut
        , label = el [] (text "Sign Out")
        }


cancelRegistrationButton =
    Input.button Style.button
        { onPress = Just CancelRegistration
        , label = el [] (text "Cancel")
        }


cancelSignInButton =
    Input.button Style.button
        { onPress = Just CancelSignin
        , label = el [] (text "Cancel")
        }


registerButton model =
    case model.state of
        NotSignedIn ->
            registerButton_

        SigningIn ->
            cancelSignInButton

        Registering ->
            submitRegistrationButton

        SignedIn ->
            Element.none


registerButton_ =
    Input.button Style.button
        { onPress = Just Register
        , label = el [] (text "Register")
        }


submitRegistrationButton =
    Input.button Style.button
        { onPress = Just SubmitRegistration
        , label = el [] (text "Submit registration")
        }


currentSharedStateView : SharedState -> Element never
currentSharedStateView sharedState =
    column [ paddingXY 0 20, spacing 24 ]
        [ el [ Font.size 16 ] (text <| "UTC: " ++ Utility.toUtcString (Just sharedState.currentTime))
        ]



--
-- HELPERS
--


stateAsString : SharedState -> State -> String
stateAsString sharedState state =
    case state of
        NotSignedIn ->
            "Not signed in"

        SignedIn ->
            "Signed in as " ++ userStatus sharedState.currentUser

        SigningIn ->
            "Signing in"

        Registering ->
            "Registering"


userStatus : Maybe User -> String
userStatus user_ =
    case user_ of
        Nothing ->
            ""

        Just user ->
            user.username


type Error
    = BadUrl String
    | Timeout
    | NetworkError
    | BadStatus Int
    | BadBody String


httpErrorExplanation : Http.Error -> String
httpErrorExplanation error =
    case error of
        Http.BadUrl str ->
            str

        Http.Timeout ->
            "Timeout (error)"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody str ->
            cleanupErrorMessage str


cleanupErrorMessage : String -> String
cleanupErrorMessage str =
    str
        |> String.lines
        |> List.filter (\x -> String.contains "message" x)
        |> List.head
        |> Maybe.withDefault ""
        |> String.trim
        |> String.replace "message" ""
        |> String.replace "\"" ""
        |> String.dropLeft 1
