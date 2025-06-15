module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (Html, a, b, button, div, h1, h2, h3, li, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, href, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder)
import List exposing (range)
import Maybe exposing (withDefault)
import Url
import Url.Parser exposing ((</>), Parser, fragment, oneOf, s, string, top)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- ROUTER


type Route
    = HomeRoute
    | BaseRosterHomeRoute
    | BaseRosterUsersRoute
    | BaseRosterShiftsRoute
    | BaseRosterUserRoute User
    | BaseRosterShiftRoute Shift


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map HomeRoute top
        , Url.Parser.map BaseRosterHomeRoute (s "base_roster")
        , Url.Parser.map BaseRosterUsersRoute (s "base_roster" </> s "users")
        , Url.Parser.map BaseRosterShiftsRoute (s "base_roster" </> s "shifts")
        , Url.Parser.map BaseRosterUserRoute (s "base_roster" </> s "user" </> string)
        , Url.Parser.map BaseRosterShiftRoute (s "base_roster" </> s "shift" </> string)
        ]


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        ( data, cmd ) =
            case Url.Parser.parse routeParser url of
                Just HomeRoute ->
                    ( Home, Cmd.none )

                Just BaseRosterHomeRoute ->
                    ( BaseRosterHome, Cmd.none )

                Just BaseRosterUsersRoute ->
                    ( Loading, getBaseRosterUsers )

                Just BaseRosterShiftsRoute ->
                    ( Loading, getBaseRosterShifts )

                Just (BaseRosterUserRoute user) ->
                    ( Loading, getBaseRosterUser user )

                Just (BaseRosterShiftRoute shift) ->
                    ( Loading, getBaseRosterShift shift )

                Nothing ->
                    ( Failure, Nav.pushUrl model.key "/base_roster" )
    in
    ( { model | url = url, data = data }, cmd )



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , data : Data
    }


type alias User =
    String


type alias Shift =
    String


type alias UserAssignment =
    { user : User
    , firstname : String
    , lastname : String
    }


type Data
    = Failure
    | Home
    | BaseRosterHome
    | Loading
    | LoadedBaseRosterUsers (List UserAssignment)
    | LoadedBaseRosterShifts (List Shift)
    | LoadedBaseRosterUser User BaseRosterUser
    | LoadedBaseRosterShift Shift BaseRosterShift


type alias BaseRosterUser =
    Dict String (List String)


type alias BaseRosterShift =
    Dict String (List UserAssignment)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url { key = key, url = url, data = Loading }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotBaseRosterUsers (Result Http.Error (List UserAssignment))
    | GotBaseRosterShifts (Result Http.Error (List Shift))
    | GotBaseRosterUser User (Result Http.Error BaseRosterUser)
    | GotBaseRosterShift Shift (Result Http.Error BaseRosterShift)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotBaseRosterUsers result ->
            ( { model
                | data =
                    case result of
                        Ok users ->
                            LoadedBaseRosterUsers users

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotBaseRosterShifts result ->
            ( { model
                | data =
                    case result of
                        Ok shifts ->
                            LoadedBaseRosterShifts shifts

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotBaseRosterUser user result ->
            ( { model
                | data =
                    case result of
                        Ok base_roster ->
                            LoadedBaseRosterUser user base_roster

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotBaseRosterShift shift result ->
            ( { model
                | data =
                    case result of
                        Ok base_roster ->
                            LoadedBaseRosterShift shift base_roster

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Information"
    , body =
        [ ul []
            [ li [] [ a [ href "/" ] [ text "Home" ] ]
            , li [] [ a [ href "/base_roster" ] [ text "Base roster" ] ]
            ]
        , viewBaseRoster model
        ]
    }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]


viewBaseRoster : Model -> Html Msg
viewBaseRoster model =
    case model.data of
        Home ->
            text "Information lies here."

        BaseRosterHome ->
            div []
                [ a [ href "/base_roster/users" ] [ h3 [] [ text "Users" ] ]
                , a [ href "/base_roster/shifts" ] [ h3 [] [ text "Shifts" ] ]
                ]

        Failure ->
            text "Something went wrong."

        Loading ->
            text "Loading..."

        LoadedBaseRosterUsers users ->
            viewUsers users

        LoadedBaseRosterShifts shifts ->
            viewShifts shifts

        LoadedBaseRosterUser user base_roster_user ->
            viewRosterGrid ("User: " ++ user) viewItemShift base_roster_user

        LoadedBaseRosterShift shift base_roster_shift ->
            viewRosterGrid ("Shift: " ++ shift) viewItemUser base_roster_shift


viewUsers : List UserAssignment -> Html Msg
viewUsers users =
    div []
        [ h3 [] [ text "Users" ]
        , users
            |> List.map (viewItemUser >> List.singleton >> li [])
            |> ul []
        ]


viewShifts : List Shift -> Html Msg
viewShifts shifts =
    div []
        [ h3 [] [ text "Shifts" ]
        , shifts
            |> List.map (viewItemShift >> List.singleton >> li [])
            |> ul []
        ]


viewRosterGrid : String -> (a -> Html Msg) -> Dict String (List a) -> Html Msg
viewRosterGrid header transform base_roster =
    div []
        [ h3 [] [ text header ]
        , table []
            [ [ "Monday"
              , "Tuesday"
              , "Wednesday"
              , "Thursday"
              , "Friday"
              ]
                |> List.map (\day -> th [ attribute "scope" "col" ] [ text day ])
                |> (::) (th [] [])
                |> tr []
                |> List.singleton
                |> thead []
            , range 1 4 |> List.map (viewRosterWeek transform base_roster) |> tbody []
            ]
        ]


viewRosterWeek : (a -> Html Msg) -> Dict String (List a) -> Int -> Html Msg
viewRosterWeek transform base_roster week =
    range 1 5
        |> List.map (viewRosterCell transform base_roster week)
        |> (::) (th [ attribute "scope" "row" ] [ "Week " ++ String.fromInt week |> text ])
        |> tr []


viewRosterCell : (a -> Html Msg) -> Dict String (List a) -> Int -> Int -> Html Msg
viewRosterCell transform base_roster week day =
    Dict.get ((week - 1) * 7 + day |> String.fromInt) base_roster
        |> viewRosterDay transform
        |> List.singleton
        |> td []


viewRosterDay : (a -> Html Msg) -> Maybe (List a) -> Html Msg
viewRosterDay transform assignments =
    withDefault [] assignments
        |> List.map (transform >> List.singleton >> li [])
        |> ul []


viewItemShift : String -> Html Msg
viewItemShift shift =
    a [ href ("/base_roster/shift/" ++ shift) ] [ text shift ]


viewItemUser : UserAssignment -> Html Msg
viewItemUser user =
    a [ href ("/base_roster/user/" ++ user.user) ] [ user.lastname ++ ", " ++ user.firstname |> text ]



-- HTTP


baseRosterUrl : String
baseRosterUrl =
    "https://api.easyrad.duckdns.org"


getBaseRosterUsers : Cmd Msg
getBaseRosterUsers =
    Http.get
        { url = baseRosterUrl ++ "/base_roster/users"
        , expect = Http.expectJson GotBaseRosterUsers (Json.Decode.list userAssignmentDecoder)
        }


getBaseRosterShifts : Cmd Msg
getBaseRosterShifts =
    Http.get
        { url = baseRosterUrl ++ "/base_roster/shifts"
        , expect = Http.expectJson GotBaseRosterShifts (Json.Decode.list Json.Decode.string)
        }


getBaseRosterUser : User -> Cmd Msg
getBaseRosterUser user =
    Http.get
        { url = baseRosterUrl ++ "/base_roster/user/" ++ user
        , expect = Http.expectJson (GotBaseRosterUser user) baseRosterUserDecoder
        }


getBaseRosterShift : Shift -> Cmd Msg
getBaseRosterShift shift =
    Http.get
        { url = baseRosterUrl ++ "/base_roster/shift/" ++ shift
        , expect = Http.expectJson (Url.percentDecode shift |> Maybe.withDefault shift |> GotBaseRosterShift) baseRosterShiftDecoder
        }


baseRosterUserDecoder : Decoder BaseRosterUser
baseRosterUserDecoder =
    Json.Decode.dict (Json.Decode.list Json.Decode.string)


baseRosterShiftDecoder : Decoder BaseRosterShift
baseRosterShiftDecoder =
    Json.Decode.dict (Json.Decode.list userAssignmentDecoder)


userAssignmentDecoder : Decoder UserAssignment
userAssignmentDecoder =
    Json.Decode.map3 UserAssignment
        (Json.Decode.index 0 Json.Decode.string)
        (Json.Decode.index 1 Json.Decode.string)
        (Json.Decode.index 2 Json.Decode.string)
