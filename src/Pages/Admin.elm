module Pages.Admin exposing (Model, Msg(..), dateStringToInt, getAnnotatedUsers, init, update, view)

import Common.Style as Style
import Common.Utility as Utility
import Configuration
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Http
import SharedState exposing (SharedState, SharedStateUpdate(..))
import Stats exposing (Stats)
import User.Coders
import User.Mail exposing (Msg(..))
import User.Types exposing (AnnotatedUser, userFromAnnotatedUser)


type alias Model =
    { users : List AnnotatedUser
    , selectedUser : Maybe AnnotatedUser
    , emailText : String
    , emailSubject : String
    , message : String
    }


type Msg
    = NoOp
    | GetUsers
    | GotUsers (Result Http.Error (List AnnotatedUser))
    | SetUser AnnotatedUser
    | GotEmailSubject String
    | GotEmailText String
    | SendEmail
    | MailMsg User.Mail.Msg


init : Model
init =
    { users = []
    , selectedUser = Nothing
    , emailText = ""
    , emailSubject = ""
    , message = ""
    }


update : SharedState -> Msg -> Model -> ( Model, Cmd Msg, SharedStateUpdate )
update sharedState msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, NoUpdate )

        GetUsers ->
            ( model, getAnnotatedUsers sharedState, SharedState.NoUpdate )

        GotUsers (Ok users_) ->
            ( { model | users = users_ }, Cmd.none, SharedState.NoUpdate )

        GotUsers (Err err) ->
            ( { model | message = "Error decoding user list" }, Cmd.none, SharedState.NoUpdate )

        SetUser user ->
            ( { model | selectedUser = Just user, message = "Selected: " ++ user.username }, Cmd.none, SharedState.NoUpdate )

        GotEmailSubject str ->
            ( { model | emailSubject = str }, Cmd.none, SharedState.NoUpdate )

        GotEmailText str ->
            ( { model | emailText = str }, Cmd.none, SharedState.NoUpdate )

        SendEmail ->
            let
                tokenString =
                    case sharedState.currentUser of
                        Nothing ->
                            "invalidToken"

                        Just user ->
                            user.token

                ( username, cmd ) =
                    case model.selectedUser of
                        Nothing ->
                            ( "nobody", Cmd.none )

                        Just user ->
                            ( user.username
                            , Cmd.map MailMsg
                                (User.Mail.send
                                    tokenString
                                    model.emailSubject
                                    (personalize user model.emailText)
                                    (userFromAnnotatedUser user)
                                )
                            )
            in
            ( { model | message = "Sending mail to " ++ username }, cmd, SharedState.NoUpdate )

        MailMsg (AcknowledgeEmailSent (Ok str)) ->
            ( { model | message = "Message sent" }, Cmd.none, SharedState.NoUpdate )

        MailMsg (AcknowledgeEmailSent (Err _)) ->
            ( { model | message = "Error sending email" }, Cmd.none, SharedState.NoUpdate )


view : SharedState -> Model -> Element Msg
view sharedState model =
    row (Style.mainColumn fill fill ++ [ padding 40, spacing 24 ])
        [ statusPanel sharedState model
        , emailPanel sharedState model
        ]


personalize : AnnotatedUser -> String -> String
personalize user message =
    String.replace "[user]" user.username message


statusPanel : SharedState -> Model -> Element Msg
statusPanel sharedState model =
    column [ spacing 12 ]
        [ row [ spacing 8 ] [ getUsersButton ]
        , column [ spacing 12 ]
            [ displayStats sharedState.stats
            , userList sharedState model
            ]
        ]


emailPanel : SharedState -> Model -> Element Msg
emailPanel sharedState model =
    column (Style.mainColumn fill fill)
        [ el [] (text <| "EMAIL PANEL")
        , inputEmailSubject model
        , inputEmailText model
        , el [ Font.size 14 ] (text <| stringStats model.emailText)
        , mailUserButton model
        , el [ Font.size 14 ] (text model.message)
        ]


stringStats : String -> String
stringStats str =
    let
        w =
            wordCount str |> String.fromInt

        c =
            String.length str |> String.fromInt
    in
    c ++ " characters, " ++ w ++ " words"


wordCount : String -> Int
wordCount str =
    String.words str |> List.length


inputEmailSubject model =
    Input.text inputStyle
        { onChange = GotEmailSubject
        , text = model.emailSubject
        , placeholder = Nothing
        , label = Input.labelAbove [ Font.size 16 ] (text "Subject")
        }


inputEmailText model =
    Input.multiline
        [ width (px 400)
        , height (px 350)
        , paddingXY 10 10
        , scrollbarY
        , Background.color (Style.makeGrey 0.3)
        , Font.color Style.white
        , Font.size 16
        ]
        { onChange = GotEmailText
        , text = model.emailText
        , label = Input.labelAbove [ Font.size 16 ] (text "Message")
        , placeholder = Nothing
        , spellcheck = False
        }


inputStyle =
    [ width (px 300), Background.color (Style.makeGrey 0.3), Font.color Style.white ]


totalNumberOfUsers : Model -> Int
totalNumberOfUsers model =
    model.users
        |> List.length


totalNumberOfBooks : Model -> Int
totalNumberOfBooks model =
    model.users
        |> List.map .numberOfBooks
        |> List.sum


userList : SharedState -> Model -> Element Msg
userList sharedState model =
    Element.table
        [ Element.centerX
        , Font.size 13
        , Element.spacing 10
        , scrollbarY
        , height (px (sharedState.windowHeight - 320))
        , Background.color Style.charcoal
        , Font.color Style.white
        , clipX
        , padding 12
        ]
        { data = model.users |> List.sortBy (\user -> -(dateStringToInt user.beginningDate))
        , columns =
            [ { header = Element.el Style.tableHeading (Element.text "id")
              , width = px 50
              , view =
                    \user ->
                        text (String.fromInt user.id)
              }
            , { header = Element.el Style.tableHeading (Element.text "Username")
              , width = px 140
              , view =
                    \user ->
                        setUserButton model user
              }
            , { header = Element.el (Style.tableHeading ++ [ moveLeft 30 ]) (Element.text "Tags")
              , width = px 100
              , view =
                    \user ->
                        el [ moveLeft 30 ] (el [ alignRight ] (text <| String.join ", " <| user.tags))
              }
            , { header = Element.el (Style.tableHeading ++ [ moveLeft 30 ]) (Element.text "Pages read")
              , width = px 40
              , view =
                    \user ->
                        el [ moveLeft 30 ] (el [ alignRight ] (text <| String.fromInt <| user.numberOfBooks))
              }
            , { header = Element.el Style.tableHeading (Element.text "Email")
              , width = px 250
              , view =
                    \user ->
                        text user.email
              }
            , { header = Element.el Style.tableHeading (Element.text "Joined")
              , width = px 120
              , view =
                    \user ->
                        text user.beginningDate
              }
            ]
        }


setUserButton : Model -> AnnotatedUser -> Element Msg
setUserButton model user =
    Input.button (Style.titleButton (Just user == model.selectedUser))
        { onPress = Just (SetUser user)
        , label = Element.text user.username
        }


mailUserButton : Model -> Element Msg
mailUserButton model =
    Input.button (Style.activeButton (model.selectedUser /= Nothing))
        { onPress = Just SendEmail
        , label = Element.text "Send email"
        }


dateStringToInt : String -> Int
dateStringToInt dateString =
    let
        intList =
            dateString
                |> String.split "/"
                |> List.map String.toInt
                |> List.map (Maybe.withDefault 0)
    in
    List.map2 (*) [ 40, 1, 500 ] intList
        |> List.sum


getUsersButton : Element Msg
getUsersButton =
    Input.button Style.button
        { onPress = Just GetUsers
        , label = Element.text "Get users"
        }


getAnnotatedUsers : SharedState -> Cmd Msg
getAnnotatedUsers sharedState =
    case sharedState.currentUser of
        Nothing ->
            Cmd.none

        Just user ->
            Http.request
                { method = "Get"
                , headers = []
                , url = Configuration.backend ++ "/api/users?all=annotated"
                , body = Http.jsonBody (User.Coders.tokenEncoder user.token)
                , expect = Http.expectJson GotUsers User.Coders.annotatedUserListDecoder
                , timeout = Nothing
                , tracker = Nothing
                }


displayStats : Maybe Stats -> Element Msg
displayStats stats_ =
    case stats_ of
        Nothing ->
            Element.none

        Just stats ->
            column [ spacing 8 ]
                [ statsRow "Users" stats.users
                , statsRow "Books" stats.books
                , statsRow "Books read" stats.booksRead
                , statsRow "Pages" stats.pages
                , statsRow "Pages read" stats.pagesRead
                ]


statsRow a b =
    row [ Font.size 12 ] [ el [ width (px 40) ] (text a), el [ moveRight 50, width (px 40) ] (el [ alignRight ] (text <| Utility.stringFromIntWithCommas b)) ]
