module Requests exposing (Model, Msg, Route, navBar, rootLink, routeParser, update, urlChanged, view)

import Common exposing (Shift, User, UserAssignment, anchor, api, decodedString, posixDecoder, posixToString, shiftPath, shiftsPath, transformShift, transformUser, userAssignmentDecoder, userPath, usersPath, viewHttpError, viewLoading, viewSubMenu)
import Element exposing (Column, Element, el, fill, link, paddingXY, shrink, spacing, table, text)
import Element.Font as Font
import Http
import Json.Decode exposing (Decoder)
import List
import Time exposing (Posix)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), Parser, oneOf, s, string, top)


type Model
    = NoData
    | Loading
    | HttpError Http.Error
    | Users (List UserAssignment)
    | Shifts (List Shift)
    | User (List UserRequest)
    | Shift (List ShiftRequest)


type alias UserRequest =
    { added : Posix
    , start : Posix
    , finish : Posix
    , shift : Shift
    }


type alias ShiftRequest =
    { added : Posix
    , start : Posix
    , finish : Posix
    , user : UserAssignment
    }



-- Routing


type Route
    = HomeRoute
    | UsersRoute
    | UserRoute User
    | ShiftsRoute
    | ShiftRoute Shift


rootPath : String
rootPath =
    "requests"


routeParser : Parser (Route -> a) a
routeParser =
    s rootPath
        </> oneOf
                [ Url.Parser.map HomeRoute top
                , Url.Parser.map UsersRoute (s usersPath)
                , Url.Parser.map UserRoute (s userPath </> string)
                , Url.Parser.map ShiftsRoute (s shiftsPath)
                , Url.Parser.map ShiftRoute (s shiftPath </> decodedString)
                ]


type Msg
    = GotUsers (Result Http.Error (List UserAssignment))
    | GotShifts (Result Http.Error (List Shift))
    | GotUser (Result Http.Error (List UserRequest))
    | GotShift (Result Http.Error (List ShiftRequest))


urlChanged : Route -> ( Model, Cmd Msg )
urlChanged route =
    case route of
        HomeRoute ->
            ( NoData, Cmd.none )

        UsersRoute ->
            ( Loading, getUsers )

        ShiftsRoute ->
            ( Loading, getShifts )

        UserRoute user ->
            ( Loading, getUser user )

        ShiftRoute shift ->
            ( Loading, getShift shift )


updateWith : (data -> Model) -> Result Http.Error data -> Model
updateWith transform result =
    case result of
        Ok r ->
            transform r

        Err err ->
            HttpError err


update : Msg -> Model
update msg =
    case msg of
        GotUsers result ->
            updateWith Users result

        GotShifts result ->
            updateWith Shifts result

        GotUser result ->
            updateWith User result

        GotShift result ->
            updateWith Shift result


rootLink : ( String, String )
rootLink =
    ( absolute [ rootPath ] [], "Requests" )


usersLink : ( String, String )
usersLink =
    ( absolute [ rootPath, usersPath ] [], "Users" )


shiftsLink : ( String, String )
shiftsLink =
    ( absolute [ rootPath, shiftsPath ] [], "Shifts" )


userUrl : String -> String
userUrl user =
    absolute [ rootPath, userPath, user ] []


shiftUrl : String -> String
shiftUrl shift =
    absolute [ rootPath, shiftPath, shift ] []



-- View


navBar : Route -> List ( String, String )
navBar route =
    rootLink
        :: (case route of
                HomeRoute ->
                    []

                UsersRoute ->
                    [ usersLink ]

                ShiftsRoute ->
                    [ shiftsLink ]

                UserRoute user ->
                    [ usersLink
                    , ( userUrl user, user )
                    ]

                ShiftRoute shift ->
                    [ shiftsLink
                    , ( shiftUrl shift, shift )
                    ]
           )


view : Time.Zone -> Model -> Element msg
view zone model =
    case model of
        NoData ->
            viewSubMenu
                [ usersLink
                , shiftsLink
                ]

        Loading ->
            viewLoading

        HttpError error ->
            viewHttpError error

        Users users ->
            List.map (transformUser userUrl) users |> viewSubMenu

        Shifts shifts ->
            List.map (transformShift shiftUrl) shifts |> viewSubMenu

        User requests ->
            viewRequests zone "User" (.shift >> transformShift shiftUrl) requests

        Shift requests ->
            viewRequests zone "Shifts" (.user >> transformUser userUrl) requests


viewRequests : Time.Zone -> String -> ({ request | start : Posix, finish : Posix, added : Posix } -> ( String, String )) -> List { request | start : Posix, finish : Posix, added : Posix } -> Element msg
viewRequests zone header transform requests =
    let
        viewPosix : Posix -> Element msg
        viewPosix =
            posixToString zone >> text

        viewShift : ( String, String ) -> Element msg
        viewShift ( url, label ) =
            link anchor { url = url, label = text label }

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



-- API


getUsers : Cmd Msg
getUsers =
    api [ rootPath, usersPath ] [] GotUsers (Json.Decode.list userAssignmentDecoder)


getShifts : Cmd Msg
getShifts =
    api [ rootPath, shiftsPath ] [] GotShifts (Json.Decode.list Json.Decode.string)


getUser : User -> Cmd Msg
getUser user =
    api [ rootPath, userPath, user ] [] GotUser (Json.Decode.list userRequestDecoder)


getShift : Shift -> Cmd Msg
getShift shift =
    api [ rootPath, shiftPath, shift ] [] GotShift (Json.Decode.list shiftRequestDecoder)



-- Decode


userRequestDecoder : Decoder UserRequest
userRequestDecoder =
    Json.Decode.map4 UserRequest
        (Json.Decode.index 0 posixDecoder)
        (Json.Decode.index 1 posixDecoder)
        (Json.Decode.index 2 posixDecoder)
        (Json.Decode.index 3 Json.Decode.string)


shiftRequestDecoder : Decoder ShiftRequest
shiftRequestDecoder =
    Json.Decode.map4 ShiftRequest
        (Json.Decode.index 0 posixDecoder)
        (Json.Decode.index 1 posixDecoder)
        (Json.Decode.index 2 posixDecoder)
    <|
        Json.Decode.map3 UserAssignment
            (Json.Decode.index 3 Json.Decode.string)
            (Json.Decode.index 4 Json.Decode.string)
            (Json.Decode.index 5 Json.Decode.string)
