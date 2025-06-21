module BaseRoster exposing (Model, Msg, Route, navBar, rootLink, rootUrl, routeParser, update, urlChanged, view)

import Common exposing (Shift, User, UserAssignment, anchor, apiBaseUrl, color, decodedString, transformShift, transformUser, userAssignmentDecoder, viewHttpError, viewLoading, viewSubMenu)
import Dict exposing (Dict)
import Element exposing (Column, Element, centerY, column, el, fill, height, link, padding, shrink, spacing, table, text, width)
import Element.Background as Background
import Element.Font as Font exposing (center)
import Http
import Json.Decode exposing (Decoder)
import List exposing (range)
import Tuple exposing (first, second)
import Url.Parser exposing ((</>), Parser, oneOf, s, string, top)


type Model
    = NoData
    | Loading
    | HttpError Http.Error
    | Users (List UserAssignment)
    | Shifts (List Shift)
    | User UserRoster
    | Shift ShiftRoster


type alias UserRoster =
    Dict String (List String)


type alias ShiftRoster =
    Dict String (List UserAssignment)



-- Routing


type Route
    = HomeRoute
    | UsersRoute
    | UserRoute User
    | ShiftsRoute
    | ShiftRoute Shift


rootUrl : String
rootUrl =
    "/base_roster"


routeParser : Parser (Route -> a) a
routeParser =
    s "base_roster"
        </> oneOf
                [ Url.Parser.map HomeRoute top
                , Url.Parser.map UsersRoute (s "users")
                , Url.Parser.map UserRoute (s "user" </> string)
                , Url.Parser.map ShiftsRoute (s "shifts")
                , Url.Parser.map ShiftRoute (s "shift" </> decodedString)
                ]


type Msg
    = GotUsers (Result Http.Error (List UserAssignment))
    | GotShifts (Result Http.Error (List Shift))
    | GotUser (Result Http.Error UserRoster)
    | GotShift (Result Http.Error ShiftRoster)


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
    ( rootUrl, "Base Roster" )


usersUrl : String
usersUrl =
    rootUrl ++ "/users"


usersLink : ( String, String )
usersLink =
    ( usersUrl, "Users" )


shiftsUrl : String
shiftsUrl =
    rootUrl ++ "/shifts"


shiftsLink : ( String, String )
shiftsLink =
    ( shiftsUrl, "Shifts" )


userUrl : String -> String
userUrl user =
    rootUrl ++ "/user/" ++ user


shiftUrl : String -> String
shiftUrl shift =
    rootUrl ++ "/shift/" ++ shift



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


view : Model -> Element msg
view model =
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

        User roster ->
            viewCell roster (transformShift shiftUrl) |> viewRosterGrid

        Shift roster ->
            viewCell roster (transformUser userUrl) |> viewRosterGrid


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



-- API


apiUrl : String
apiUrl =
    apiBaseUrl ++ rootUrl


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = apiUrl ++ "/users"
        , expect = Http.expectJson GotUsers (Json.Decode.list userAssignmentDecoder)
        }


getShifts : Cmd Msg
getShifts =
    Http.get
        { url = apiUrl ++ "/shifts"
        , expect = Http.expectJson GotShifts (Json.Decode.list Json.Decode.string)
        }


getUser : User -> Cmd Msg
getUser user =
    Http.get
        { url = apiUrl ++ "/user/" ++ user
        , expect = Http.expectJson GotUser userRosterDecoder
        }


getShift : Shift -> Cmd Msg
getShift shift =
    Http.get
        { url = apiUrl ++ "/shift/" ++ shift
        , expect = Http.expectJson GotShift shiftRosterDecoder
        }



-- Decode


userRosterDecoder : Decoder UserRoster
userRosterDecoder =
    Json.Decode.dict (Json.Decode.list Json.Decode.string)


shiftRosterDecoder : Decoder ShiftRoster
shiftRosterDecoder =
    Json.Decode.dict (Json.Decode.list userAssignmentDecoder)
