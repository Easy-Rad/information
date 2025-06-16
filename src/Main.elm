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
    | LoadedBaseRosterUser BaseRosterUser
    | LoadedBaseRosterShift BaseRosterShift


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
    | GotBaseRosterUser (Result Http.Error BaseRosterUser)
    | GotBaseRosterShift (Result Http.Error BaseRosterShift)


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

        baseRosterUsersLink =
            navBarLink "/base_roster/users" "Users"

        baseRosterShiftsLink =
            navBarLink "/base_roster/shifts" "Shifts"

        links : List (Bool -> Element msg)
        links =
            navBarLink "/" "Home"
                :: (case route of
                        Just BaseRosterHomeRoute ->
                            [ baseRosterLink
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
                ]

        BaseRosterHome ->
            viewSubMenu
                [ ( "/base_roster/users", "Users" )
                , ( "/base_roster/shifts", "Shifts" )
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
        , expect = Http.expectJson GotBaseRosterUser baseRosterUserDecoder
        }


getBaseRosterShift : Shift -> Cmd Msg
getBaseRosterShift shift =
    Http.get
        { url = baseRosterUrl ++ "/base_roster/shift/" ++ shift
        , expect = Http.expectJson GotBaseRosterShift baseRosterShiftDecoder
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
