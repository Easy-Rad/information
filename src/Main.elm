module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Element exposing (Column, Element, centerY, column, el, fill, height, layout, link, mouseOver, padding, paddingXY, rgb255, row, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Http
import Json.Decode exposing (Decoder)
import List exposing (range)
import Maybe
import Task
import Time exposing (Month(..))
import Tuple exposing (first, second)
import Url
import Url.Parser exposing ((</>), Parser, oneOf, s, string, top)



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
    | RequestsHomeRoute
    | RequestsUsersRoute
    | RequestsShiftsRoute
    | RequestsUserRoute User
    | RequestsShiftRoute Shift


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map HomeRoute top
        , Url.Parser.map BaseRosterHomeRoute (s "base_roster")
        , Url.Parser.map BaseRosterUsersRoute (s "base_roster" </> s "users")
        , Url.Parser.map BaseRosterShiftsRoute (s "base_roster" </> s "shifts")
        , Url.Parser.map BaseRosterUserRoute (s "base_roster" </> s "user" </> string)
        , Url.Parser.map BaseRosterShiftRoute (s "base_roster" </> s "shift" </> string)
        , Url.Parser.map RequestsHomeRoute (s "requests")
        , Url.Parser.map RequestsUsersRoute (s "requests" </> s "users")
        , Url.Parser.map RequestsShiftsRoute (s "requests" </> s "shifts")
        , Url.Parser.map RequestsUserRoute (s "requests" </> s "user" </> string)
        , Url.Parser.map RequestsShiftRoute (s "requests" </> s "shift" </> string)
        ]


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        ( data, cmd ) =
            case Url.Parser.parse routeParser url of
                Nothing ->
                    ( Failure, Nav.pushUrl model.key "/" )

                Just HomeRoute ->
                    ( Home, Cmd.none )

                Just BaseRosterHomeRoute ->
                    ( BaseRosterHome, Cmd.none )

                Just BaseRosterUsersRoute ->
                    ( Loading, getBaseRosterUsers )

                Just BaseRosterShiftsRoute ->
                    ( Loading, getBaseRosterShifts )

                Just RequestsHomeRoute ->
                    ( RequestsHome, Cmd.none )

                Just (BaseRosterUserRoute user) ->
                    ( Loading, getBaseRosterUser user )

                Just (BaseRosterShiftRoute shift) ->
                    ( Loading, getBaseRosterShift shift )

                Just RequestsUsersRoute ->
                    ( Loading, getRequestsUsers )

                Just RequestsShiftsRoute ->
                    ( Loading, getRequestsShifts )

                Just (RequestsUserRoute user) ->
                    ( Loading, getRequestsUser user )

                Just (RequestsShiftRoute shift) ->
                    ( Loading, getRequestsShift shift )
    in
    ( { model | url = url, data = data }, cmd )



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , zone : Time.Zone
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
    | RequestsHome
    | Loading
    | LoadedBaseRosterUsers (List UserAssignment)
    | LoadedBaseRosterShifts (List Shift)
    | LoadedBaseRosterUser BaseRosterUser
    | LoadedBaseRosterShift BaseRosterShift
    | LoadedRequestsUsers (List UserAssignment)
    | LoadedRequestsShifts (List Shift)
    | LoadedRequestsUser (List RequestUser)
    | LoadedRequestsShift (List RequestShift)


type alias RequestShift =
    { added : Time.Posix
    , start : Time.Posix
    , finish : Time.Posix
    , user : UserAssignment
    }


type alias RequestUser =
    { start : Time.Posix
    , finish : Time.Posix
    , added : Time.Posix
    , shift : Shift
    }


type alias BaseRosterUser =
    Dict String (List String)


type alias BaseRosterShift =
    Dict String (List UserAssignment)


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, zone = Time.utc, data = Loading }, Time.here |> Task.perform InitWithZone )



-- UPDATE


type Msg
    = InitWithZone Time.Zone
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotBaseRosterUsers (Result Http.Error (List UserAssignment))
    | GotBaseRosterShifts (Result Http.Error (List Shift))
    | GotBaseRosterUser (Result Http.Error BaseRosterUser)
    | GotBaseRosterShift (Result Http.Error BaseRosterShift)
    | GotRequestsUsers (Result Http.Error (List UserAssignment))
    | GotRequestsShifts (Result Http.Error (List Shift))
    | GotRequestsUser (Result Http.Error (List RequestUser))
    | GotRequestsShift (Result Http.Error (List RequestShift))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitWithZone zone ->
            let
                newModel =
                    { model | zone = zone }
            in
            stepUrl newModel.url newModel

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

        GotBaseRosterUser result ->
            ( { model
                | data =
                    case result of
                        Ok base_roster ->
                            LoadedBaseRosterUser base_roster

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotBaseRosterShift result ->
            ( { model
                | data =
                    case result of
                        Ok base_roster ->
                            LoadedBaseRosterShift base_roster

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotRequestsUsers result ->
            ( { model
                | data =
                    case result of
                        Ok users ->
                            LoadedRequestsUsers users

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotRequestsShifts result ->
            ( { model
                | data =
                    case result of
                        Ok shifts ->
                            LoadedRequestsShifts shifts

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotRequestsUser result ->
            ( { model
                | data =
                    case result of
                        Ok requests ->
                            LoadedRequestsUser requests

                        Err _ ->
                            Failure
              }
            , Cmd.none
            )

        GotRequestsShift result ->
            ( { model
                | data =
                    case result of
                        Ok requests ->
                            LoadedRequestsShift requests

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
    [ Font.size 16
    , paddingXY 8 4
    , Border.rounded 4
    , width fill
    , mouseOver mouseOverAttrs
    ]
        ++ defaultAttrs


color =
    { blue = rgb255 0x72 0x9F 0xCF
    , purple = rgb255 0x00 0x14 0xFF
    , yellow = rgb255 0xFC 0xFF 0x38
    , darkCharcoal = rgb255 0x2E 0x34 0x36
    , darkBlue = rgb255 0x37 0x7F 0xC9
    , lightBlue = rgb255 0xC5 0xE8 0xF7
    , lightGrey = rgb255 0xE0 0xE0 0xE0
    , white = rgb255 0xFF 0xFF 0xFF
    }


defaultAttrs =
    [ Background.color color.lightBlue
    , Font.color color.darkCharcoal
    ]


activeAttrs =
    [ Background.color color.darkCharcoal
    , Font.color color.white
    ]


mouseOverAttrs =
    [ Background.color color.yellow
    , Font.color color.darkCharcoal
    ]


view : Model -> Browser.Document Msg
view model =
    { title = "Information"
    , body =
        [ column
            [ spacing 10
            , width fill
            ]
            [ Url.Parser.parse routeParser model.url |> viewNavBar
            , viewBaseRoster model |> el [ padding 8 ]
            ]
            |> layout []
        ]
    }


navBarLink : String -> String -> Bool -> Element msg
navBarLink url label active =
    link
        ([ Font.size 20
         , paddingXY 8 4
         , Border.rounded 4
         , width shrink
         , mouseOver mouseOverAttrs
         ]
            ++ (if active then
                    activeAttrs

                else
                    defaultAttrs
               )
        )
        { url = url
        , label = text label
        }


viewNavBar : Maybe Route -> Element msg
viewNavBar route =
    let
        baseRosterLink =
            navBarLink "/base_roster" "Base roster"

        requestsLink =
            navBarLink "/requests" "Requests"

        baseRosterUsersLink =
            navBarLink "/base_roster/users" "Users"

        baseRosterShiftsLink =
            navBarLink "/base_roster/shifts" "Shifts"

        requestsUsersLink =
            navBarLink "/requests/users" "Users"

        requestsShiftsLink =
            navBarLink "/requests/shifts" "Shifts"

        links : List (Bool -> Element msg)
        links =
            navBarLink "/" "Home"
                :: (case route of
                        Just BaseRosterHomeRoute ->
                            [ baseRosterLink
                            ]

                        Just RequestsHomeRoute ->
                            [ requestsLink
                            ]

                        Just BaseRosterUsersRoute ->
                            [ baseRosterLink
                            , baseRosterUsersLink
                            ]

                        Just BaseRosterShiftsRoute ->
                            [ baseRosterLink
                            , baseRosterShiftsLink
                            ]

                        Just (BaseRosterUserRoute user) ->
                            [ baseRosterLink
                            , baseRosterUsersLink
                            , navBarLink ("/base_roster/user/" ++ user) user
                            ]

                        Just (BaseRosterShiftRoute shift) ->
                            [ baseRosterLink
                            , baseRosterShiftsLink
                            , navBarLink ("/base_roster/shift/" ++ shift) (percentDecode shift)
                            ]

                        Just (RequestsUserRoute user) ->
                            [ requestsLink
                            , requestsUsersLink
                            , navBarLink ("/requests/user/" ++ user) user
                            ]

                        Just (RequestsShiftRoute shift) ->
                            [ requestsLink
                            , requestsShiftsLink
                            , navBarLink ("/requests/shift/" ++ shift) (percentDecode shift)
                            ]

                        _ ->
                            []
                   )
    in
    row
        [ width fill
        , padding 8
        , spacing 8
        , Background.color color.blue
        ]
    <|
        List.indexedMap (\index link -> link (index == List.length links - 1)) links


viewSubMenu : List ( String, String ) -> Element msg
viewSubMenu items =
    column [ spacing 4 ]
        (items
            |> List.map (\( url, label ) -> link anchor { url = url, label = text label })
        )


viewBaseRoster : Model -> Element msg
viewBaseRoster model =
    case model.data of
        Home ->
            viewSubMenu
                [ ( "/base_roster", "Base Roster" )
                , ( "/requests", "Requests" )
                ]

        BaseRosterHome ->
            viewSubMenu
                [ ( "/base_roster/users", "Users" )
                , ( "/base_roster/shifts", "Shifts" )
                ]

        RequestsHome ->
            viewSubMenu
                [ ( "/requests/users", "Users" )
                , ( "/requests/shifts", "Shifts" )
                ]

        Failure ->
            text "Something went wrong."

        Loading ->
            text "Loading..."

        LoadedBaseRosterUsers users ->
            users |> List.map transformUser |> viewSubMenu

        LoadedBaseRosterShifts shifts ->
            shifts |> List.map transformShift |> viewSubMenu

        LoadedBaseRosterUser base_roster_user ->
            viewCell base_roster_user transformShift |> viewRosterGrid

        LoadedBaseRosterShift base_roster_shift ->
            viewCell base_roster_shift transformUser |> viewRosterGrid

        LoadedRequestsUsers users ->
            users |> List.map transformRequestUser |> viewSubMenu

        LoadedRequestsShifts shifts ->
            shifts |> List.map transformRequestShift |> viewSubMenu

        LoadedRequestsUser requests ->
            viewRequests model.zone "User" (.shift >> transformRequestShift) requests

        LoadedRequestsShift requests ->
            viewRequests model.zone "Shifts" (.user >> transformRequestUser) requests


toMonth : Month -> Int
toMonth month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


posixToString : Time.Zone -> Time.Posix -> String
posixToString zone posix =
    (Time.toDay zone posix |> String.fromInt |> String.padLeft 2 '0')
        ++ "/"
        ++ (Time.toMonth zone posix |> toMonth |> String.fromInt |> String.padLeft 2 '0')
        ++ "/"
        ++ (Time.toYear zone posix |> String.fromInt)
        ++ " "
        ++ (Time.toHour zone posix |> String.fromInt |> String.padLeft 2 '0')
        ++ ":"
        ++ (Time.toMinute zone posix |> String.fromInt |> String.padLeft 2 '0')


viewRequests : Time.Zone -> String -> ({ request | start : Time.Posix, finish : Time.Posix, added : Time.Posix } -> ( String, String )) -> List { request | start : Time.Posix, finish : Time.Posix, added : Time.Posix } -> Element msg
viewRequests zone header transform requests =
    let
        viewPosix : Time.Posix -> Element msg
        viewPosix =
            posixToString zone >> text

        viewShift : ( String, String ) -> Element msg
        viewShift ( url, label ) =
            link anchor { url = url, label = text label }

        -- link anchor { url = "/requests/shift/" ++ shift, label = text shift }
        padCell : Element msg -> Element msg
        padCell =
            el [ paddingXY 4 0 ]

        headerCell : String -> Element msg
        headerCell =
            text >> el [ Font.bold, paddingXY 4 0 ]
    in
    table
        [ spacing 8
        ]
        { data = requests
        , columns =
            [ Column (headerCell header) fill (transform >> viewShift >> padCell)
            , Column (headerCell "Start") shrink (.start >> viewPosix >> padCell)
            , Column (headerCell "Finish") shrink (.finish >> viewPosix >> padCell)
            , Column (headerCell "Added") shrink (.added >> viewPosix >> padCell)
            ]
        }


viewRosterGrid : (Int -> Int -> Element msg) -> Element msg
viewRosterGrid transform =
    let
        headerBaseAttrs : List (Element.Attribute msg)
        headerBaseAttrs =
            [ Font.bold
            , padding 8
            ]

        columns : List (Column Int msg)
        columns =
            [ "Monday"
            , "Tuesday"
            , "Wednesday"
            , "Thursday"
            , "Friday"
            ]
                |> List.indexedMap (\dayIndex dayName -> transform dayIndex |> Column (text dayName |> el (center :: headerBaseAttrs)) fill)
                |> (::) (Column Element.none shrink (\week -> text ("Week " ++ String.fromInt (week + 1)) |> el (centerY :: headerBaseAttrs)))
    in
    table []
        { data = range 0 3
        , columns = columns
        }


viewCell : Dict String (List a) -> (a -> ( String, String )) -> Int -> Int -> Element msg
viewCell base_roster transform dayIndex weekIndex =
    Dict.get (weekIndex * 7 + (dayIndex + 1) |> String.fromInt) base_roster
        |> Maybe.withDefault []
        |> List.map (transform >> (\a -> link anchor { url = first a, label = second a |> text }))
        |> column
            [ spacing 4
            , padding 8
            , width fill
            , height fill
            , Background.color
                (if modBy 2 weekIndex == 0 then
                    color.darkCharcoal

                 else
                    color.blue
                )
            ]


transformShift : Shift -> ( String, String )
transformShift shift =
    ( "/base_roster/shift/" ++ shift, shift )


transformUser : UserAssignment -> ( String, String )
transformUser user =
    ( "/base_roster/user/" ++ user.user, user.lastname ++ ", " ++ user.firstname )


transformRequestShift : Shift -> ( String, String )
transformRequestShift shift =
    ( "/requests/shift/" ++ shift, shift )


transformRequestUser : UserAssignment -> ( String, String )
transformRequestUser user =
    ( "/requests/user/" ++ user.user, user.lastname ++ ", " ++ user.firstname )



-- HTTP


apiBaseUrl : String
apiBaseUrl =
    "https://api.easyrad.duckdns.org"


apiBaseRosterUrl : String
apiBaseRosterUrl =
    apiBaseUrl ++ "/base_roster"


apiRequestsUrl : String
apiRequestsUrl =
    apiBaseUrl ++ "/requests"


getBaseRosterUsers : Cmd Msg
getBaseRosterUsers =
    Http.get
        { url = apiBaseRosterUrl ++ "/users"
        , expect = Http.expectJson GotBaseRosterUsers (Json.Decode.list userAssignmentDecoder)
        }


getBaseRosterShifts : Cmd Msg
getBaseRosterShifts =
    Http.get
        { url = apiBaseRosterUrl ++ "/shifts"
        , expect = Http.expectJson GotBaseRosterShifts (Json.Decode.list Json.Decode.string)
        }


getBaseRosterUser : User -> Cmd Msg
getBaseRosterUser user =
    Http.get
        { url = apiBaseRosterUrl ++ "/user/" ++ user
        , expect = Http.expectJson GotBaseRosterUser baseRosterUserDecoder
        }


getBaseRosterShift : Shift -> Cmd Msg
getBaseRosterShift shift =
    Http.get
        { url = apiBaseRosterUrl ++ "/shift/" ++ shift
        , expect = Http.expectJson GotBaseRosterShift baseRosterShiftDecoder
        }


getRequestsUsers : Cmd Msg
getRequestsUsers =
    Http.get
        { url = apiRequestsUrl ++ "/users"
        , expect = Http.expectJson GotRequestsUsers (Json.Decode.list userAssignmentDecoder)
        }


getRequestsShifts : Cmd Msg
getRequestsShifts =
    Http.get
        { url = apiRequestsUrl ++ "/shifts"
        , expect = Http.expectJson GotRequestsShifts (Json.Decode.list Json.Decode.string)
        }


getRequestsUser : User -> Cmd Msg
getRequestsUser user =
    Http.get
        { url = apiRequestsUrl ++ "/user/" ++ user
        , expect = Http.expectJson GotRequestsUser (Json.Decode.list requestUserDecoder)
        }


getRequestsShift : Shift -> Cmd Msg
getRequestsShift shift =
    Http.get
        { url = apiRequestsUrl ++ "/shift/" ++ shift
        , expect = Http.expectJson GotRequestsShift (Json.Decode.list requestShiftDecoder)
        }


percentDecode : String -> String
percentDecode str =
    Url.percentDecode str |> Maybe.withDefault str


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


requestUserDecoder : Decoder RequestUser
requestUserDecoder =
    Json.Decode.map4 RequestUser
        (Json.Decode.index 0 posixDecoder)
        (Json.Decode.index 1 posixDecoder)
        (Json.Decode.index 2 posixDecoder)
        (Json.Decode.index 3 Json.Decode.string)


requestShiftDecoder : Decoder RequestShift
requestShiftDecoder =
    Json.Decode.map4 RequestShift
        (Json.Decode.index 0 posixDecoder)
        (Json.Decode.index 1 posixDecoder)
        (Json.Decode.index 2 posixDecoder)
    <|
        Json.Decode.map3 UserAssignment
            (Json.Decode.index 3 Json.Decode.string)
            (Json.Decode.index 4 Json.Decode.string)
            (Json.Decode.index 5 Json.Decode.string)


posixDecoder : Decoder Time.Posix
posixDecoder =
    Json.Decode.map ((*) 1000 >> Time.millisToPosix) Json.Decode.int
