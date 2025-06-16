module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Column, Element, IndexedColumn, alignRight, centerY, column, el, fill, indexedTable, layout, link, padding, rgb255, row, spacing, table, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (a)
import Http
import Json.Decode exposing (Decoder)
import List exposing (head, range)
import Maybe exposing (withDefault)
import Url exposing (Protocol(..))
import Url.Builder
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


anchor : List (Element.Attribute msg)
anchor =
    [ Font.color (rgb255 0 0 255)
    , Font.underline
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Information"
    , body =
        [ column [ spacing 10 ]
            [ row [ spacing 10 ]
                [ link anchor { url = "/", label = text "Home" }
                , link anchor { url = "/base_roster", label = text "Base roster" }
                ]
            , viewBaseRoster model
            ]
            |> layout [ padding 10 ]
        ]
    }


viewBaseRoster : Model -> Element msg
viewBaseRoster model =
    case model.data of
        Home ->
            text "Information lies here."

        BaseRosterHome ->
            row [ spacing 10 ]
                [ link anchor { url = "/base_roster/users", label = text "Users" }
                , link anchor { url = "/base_roster/shifts", label = text "Shifts" }
                ]

        Failure ->
            text "Something went wrong."

        Loading ->
            text "Loading..."

        LoadedBaseRosterUsers users ->
            viewList "Users" users viewItemUser

        LoadedBaseRosterShifts shifts ->
            viewList "Shifts" shifts viewItemShift

        LoadedBaseRosterUser user base_roster_user ->
            viewRosterGrid ("User: " ++ user) (viewCell base_roster_user viewItemShift)

        LoadedBaseRosterShift shift base_roster_shift ->
            viewRosterGrid ("Shift: " ++ shift) (viewCell base_roster_shift viewItemUser)


viewList : String -> List a -> (a -> Element msg) -> Element msg
viewList title items transform =
    column []
        [ text title
        , items
            |> List.map transform
            |> column []
        ]


viewRosterGrid : String -> (Int -> Int -> Element msg) -> Element msg
viewRosterGrid title transform =
    let
        headerAttrs : List (Element.Attribute msg)
        headerAttrs =
            [ Font.bold
            , Font.center

            -- , Border.color (rgb255 0 0 0)
            , Border.width 1
            ]

        columns : List (IndexedColumn Int msg)
        columns =
            [ "Monday"
            , "Tuesday"
            , "Wednesday"
            , "Thursday"
            , "Friday"
            ]
                |> List.map (\dayName -> IndexedColumn (text dayName |> el headerAttrs) fill transform)
    in
    column [ spacing 10 ]
        [ text title
        , indexedTable [ Border.width 1 ]
            { data = range 0 3
            , columns = columns
            }
        ]


viewCell : Dict String (List a) -> (a -> Element msg) -> Int -> Int -> Element msg
viewCell base_roster transform dayIndex weekIndex =
    Dict.get (weekIndex * 7 + (dayIndex + 1) |> String.fromInt) base_roster
        |> Maybe.withDefault []
        |> List.map transform
        |> column [ padding 10, Border.width 1 ]


viewItemShift : String -> Element msg
viewItemShift shift =
    link anchor { url = "/base_roster/shift/" ++ shift, label = text shift }


viewItemUser : UserAssignment -> Element msg
viewItemUser user =
    link anchor { url = "/base_roster/user/" ++ user.user, label = user.lastname ++ ", " ++ user.firstname |> text }



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
