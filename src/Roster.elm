module Roster exposing (..)

import BaseRoster exposing (Msg)
import Common exposing (Shift, User, activeAttrs, anchor, api, color, defaultAttrs, mouseOverAttrs, viewHttpError, viewLoading)
import Date exposing (Date, Interval(..), Unit(..))
import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, alignTop, column, el, fill, height, htmlAttribute, inFront, link, mouseOver, padding, paddingXY, rgb255, rgba255, row, shrink, spacing, table, text, transparent, width)
import Element.Background
import Element.Border as Border
import Element.Font as Font exposing (bold)
import Element.Input as Input
import Html.Attributes
import Http
import Json.Decode exposing (Decoder, dict, index, int, string)
import List exposing (filterMap)
import Maybe
import Time exposing (Month(..), Weekday(..))
import Tuple exposing (pair)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, s)
import Url.Parser.Query



-- MODEL


type alias Model =
    { searchParams : SearchParams
    , tableView : Bool
    , data : Data
    }


type alias SearchParams =
    { dateRange : DateRange
    , filterUser : Maybe User
    , filterShift : Maybe Int
    }


type DateRange
    = Today
    | Day Int
    | Range Int Int


type Data
    = Loading
    | HttpError Http.Error
    | Data Calendar


type alias ShiftRecord =
    { shiftId : Int
    , shiftName : String
    , usersByDay : Dict Int (List User)
    }


type alias Calendar =
    { dates : List Date
    , shifts : List ShiftRecord
    , users : Dict User ( String, String )
    }



-- Routing


type alias Route =
    { start : Maybe Int
    , finish : Maybe Int
    , filterUser : Maybe User
    , filterShift : Maybe Int
    }


rootPath : String
rootPath =
    "calendar"


routeParser : Parser (Route -> a) a
routeParser =
    Url.Parser.map Route
        (s rootPath
            <?> Url.Parser.Query.int "start"
            <?> Url.Parser.Query.int "finish"
            <?> Url.Parser.Query.string "user"
            <?> Url.Parser.Query.int "shift"
        )


type Msg
    = GotCalendar (Result Http.Error Calendar)
    | TableView Bool


routeToParams : Route -> SearchParams
routeToParams route =
    SearchParams
        (case ( route.start, route.finish ) of
            ( Just start, Just finish ) ->
                Range start finish

            ( Just start, Nothing ) ->
                Day start

            ( Nothing, Just finish ) ->
                Day finish

            ( Nothing, Nothing ) ->
                Today
        )
        route.filterUser
        route.filterShift


urlChanged : Route -> ( Model, Cmd Msg )
urlChanged route =
    let
        model =
            Model (routeToParams route) False Loading
    in
    ( model, getCalendar model )


update : Model -> Msg -> Model
update model msg =
    case msg of
        GotCalendar data ->
            { model
                | data =
                    case data of
                        Ok r ->
                            Data r

                        Err err ->
                            HttpError err
            }

        TableView tableView ->
            { model | tableView = tableView }


rootLink : ( String, String )
rootLink =
    ( absolute [ rootPath ] [], "Calendar" )



-- View


navBar : Route -> List ( String, String )
navBar route =
    let
        params =
            routeToParams route
    in
    rootLink
        :: (case params.filterUser of
                Just user ->
                    [ ( buildUrl { params | filterShift = Nothing }, user ) ]

                Nothing ->
                    []
           )
        ++ (case params.filterShift of
                Just _ ->
                    [ ( buildUrl { params | filterUser = Nothing }, "Shift" ) ]

                Nothing ->
                    []
           )


view : Model -> Element Msg
view model =
    case model.data of
        Loading ->
            viewLoading

        HttpError e ->
            viewHttpError e

        Data calendar ->
            let
                toggle_calendar_view =
                    Input.checkbox []
                        { onChange = TableView
                        , icon = Input.defaultCheckbox
                        , checked = model.tableView
                        , label = Input.labelRight [] (text "Table view")
                        }
            in
            column [ spacing 8 ]
                [ row [ spacing 4 ] <|
                    (case ( model.searchParams.filterUser, model.searchParams.filterShift ) of
                        ( Nothing, Nothing ) ->
                            Element.none

                        _ ->
                            let
                                params =
                                    model.searchParams
                            in
                            link (anchor ++ activeAttrs) { url = buildUrl { params | filterUser = Nothing, filterShift = Nothing }, label = text "Clear filters" }
                    )
                        :: viewDateLinks model.searchParams (List.head calendar.dates)
                , case ( model.searchParams.filterUser, model.searchParams.filterShift, model.searchParams.dateRange ) of
                    ( Just _, Nothing, Range _ _ ) ->
                        toggle_calendar_view

                    ( Nothing, Just _, Range _ _ ) ->
                        toggle_calendar_view

                    _ ->
                        Element.none
                , case ( model.tableView, model.searchParams.dateRange ) of
                    ( False, Range first last ) ->
                        case ( model.searchParams.filterUser, model.searchParams.filterShift ) of
                            ( Just user, Nothing ) ->
                                viewCalendarForUser model.searchParams user calendar (fromDateInt first) (fromDateInt last)

                            ( Nothing, Just shiftId ) ->
                                viewCalendarForShift model.searchParams shiftId calendar (fromDateInt first) (fromDateInt last)

                            _ ->
                                viewTable model.searchParams calendar

                    _ ->
                        viewTable model.searchParams calendar
                ]


dateLinks : Date -> List ( String, DateRange )
dateLinks date =
    [ ( "Today", Today )
    , ( "-Week", Range (date |> Date.floor Monday |> Date.add Weeks -1 |> toDateInt) (date |> Date.ceiling Sunday |> Date.add Weeks -1 |> toDateInt) )
    , ( "Week", Range (date |> Date.floor Monday |> toDateInt) (date |> Date.ceiling Sunday |> toDateInt) )
    , ( "+Week", Range (date |> Date.floor Monday |> Date.add Weeks 1 |> toDateInt) (date |> Date.ceiling Sunday |> Date.add Weeks 1 |> toDateInt) )
    , ( "-Month", Range (date |> Date.floor Month |> Date.add Months -1 |> toDateInt) (date |> Date.floor Month |> Date.add Days -1 |> toDateInt) )
    , ( "Month", Range (date |> Date.floor Month |> toDateInt) (date |> Date.floor Month |> Date.add Months 1 |> Date.add Days -1 |> toDateInt) )
    , ( "+Month", Range (date |> Date.floor Month |> Date.add Months 1 |> toDateInt) (date |> Date.floor Month |> Date.add Months 2 |> Date.add Days -1 |> toDateInt) )
    ]


viewDateLinks : SearchParams -> Maybe Date -> List (Element msg)
viewDateLinks params date =
    let
        toLinkElement : ( String, DateRange ) -> Element msg
        toLinkElement ( label, dateRange ) =
            link anchor { url = buildUrl { params | dateRange = dateRange }, label = text label }
    in
    date |> Maybe.map (dateLinks >> List.map toLinkElement) |> Maybe.withDefault []


myTooltip : String -> Element msg
myTooltip str =
    el
        [ Element.Background.color (rgb255 0 0 0)
        , Font.color (rgb255 255 255 255)
        , padding 4
        , Border.rounded 5
        , Font.size 14
        , Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = rgba255 0 0 0 0.32 }
        ]
        (text str)


tooltip : (Element msg -> Attribute msg) -> Element Never -> Attribute msg
tooltip usher tooltip_ =
    inFront <|
        el
            [ width fill
            , height fill
            , transparent True
            , mouseOver [ transparent False ]
            , (usher << Element.map never) <|
                el
                    [ htmlAttribute (Html.Attributes.style "pointerEvents" "none")
                    , Element.moveUp 8
                    ]
                    tooltip_
            ]
            Element.none


cellAttrs : List (Element.Attribute msg)
cellAttrs =
    [ width fill
    , height fill
    , Border.width 1
    , padding 4
    ]


buildUrl : SearchParams -> String
buildUrl params =
    (case params.dateRange of
        Range start finish ->
            [ Url.Builder.int "start" start, Url.Builder.int "finish" finish ]

        Day day ->
            [ Url.Builder.int "start" day ]

        Today ->
            []
    )
        ++ List.filterMap identity
            [ Maybe.map (Url.Builder.string "user") params.filterUser
            , Maybe.map (Url.Builder.int "shift") params.filterShift
            ]
        |> Url.Builder.absolute [ rootPath ]


viewUser : List (Element.Attribute msg) -> (User -> SearchParams) -> ( String, ( String, String ) ) -> Element msg
viewUser attrs getSearchParams ( user_code, ( first_name, last_name ) ) =
    link
        (tooltip Element.above (myTooltip <| first_name ++ " " ++ last_name)
            :: attrs
        )
        { url = buildUrl (getSearchParams user_code)
        , label = user_code |> text
        }


viewShift : List (Element.Attribute msg) -> (Int -> SearchParams) -> Shift -> Int -> Element msg
viewShift attrs getSearchParams shift shift_id =
    link attrs
        { url = buildUrl (getSearchParams shift_id)
        , label = text shift
        }


viewCell : (User -> SearchParams) -> Dict User ( String, String ) -> List User -> Element msg
viewCell getSearchParams userDict users =
    users
        |> List.filterMap
            (\user_code ->
                Dict.get user_code userDict
                    |> Maybe.map (pair user_code)
            )
        |> List.map (viewUser [ width shrink ] getSearchParams)
        |> column cellAttrs


headerBaseAttrs : List (Element.Attribute msg)
headerBaseAttrs =
    Font.bold :: cellAttrs


viewColumnHeader : (DateRange -> SearchParams) -> Date -> Element msg
viewColumnHeader getSearchParams date =
    link headerBaseAttrs { url = buildUrl (toDateInt date |> Day |> getSearchParams), label = date |> Date.format "E d/MM" |> text }


viewRowHeader : SearchParams -> ShiftRecord -> Element msg
viewRowHeader params r =
    viewShift headerBaseAttrs (updateShift params) r.shiftName r.shiftId


updateShift : SearchParams -> Int -> SearchParams
updateShift searchParams shiftId =
    { searchParams | filterShift = Just shiftId }


updateDateRange : SearchParams -> DateRange -> SearchParams
updateDateRange searchParams dateRange =
    { searchParams | dateRange = dateRange }


updateFilterUser : SearchParams -> User -> SearchParams
updateFilterUser searchParams user =
    { searchParams | filterUser = Just user }


viewTable : SearchParams -> Calendar -> Element msg
viewTable params calendar =
    let
        mapper : Date -> Column ShiftRecord msg
        mapper date =
            Column
                (viewColumnHeader (updateDateRange params) date)
                shrink
                (.usersByDay >> Dict.get (Date.toRataDie date) >> Maybe.map (viewCell (updateFilterUser params) calendar.users) >> Maybe.withDefault (el cellAttrs Element.none))
    in
    table [ Border.width 1, width shrink ]
        { data = calendar.shifts
        , columns = Column (el headerBaseAttrs Element.none) shrink (viewRowHeader params) :: (calendar.dates |> List.map mapper)
        }


weekdayToName : Time.Weekday -> String
weekdayToName weekday =
    case weekday of
        Mon ->
            "Monday"

        Tue ->
            "Tuesday"

        Wed ->
            "Wednesday"

        Thu ->
            "Thursday"

        Fri ->
            "Friday"

        Sat ->
            "Saturday"

        Sun ->
            "Sunday"


weekdayToInterval : Time.Weekday -> Interval
weekdayToInterval weekday =
    case weekday of
        Mon ->
            Monday

        Tue ->
            Tuesday

        Wed ->
            Wednesday

        Thu ->
            Thursday

        Fri ->
            Friday

        Sat ->
            Saturday

        Sun ->
            Sunday


viewCalendar : (Date -> List ( Maybe String, String, SearchParams )) -> Date -> Date -> Element msg
viewCalendar getCellItems firstDate lastDate =
    let
        viewCellItem : ( Maybe String, String, SearchParams ) -> Element msg
        viewCellItem ( tooltip_label, item_label, searchParams ) =
            link (tooltip_label |> Maybe.map (myTooltip >> tooltip Element.above >> List.singleton) |> Maybe.withDefault [])
                { url = buildUrl searchParams
                , label = text item_label
                }

        viewCalendarCell : Date -> Element msg
        viewCalendarCell date =
            if Date.isBetween firstDate lastDate date then
                column
                    [ width fill
                    , height (Element.minimum 80 fill)
                    , Border.width 1
                    ]
                    [ link
                        ([ width fill
                         , padding 4
                         , mouseOver mouseOverAttrs
                         ]
                            ++ defaultAttrs
                        )
                        { url = buildUrl (onSelectDate date)
                        , label = date |> Date.format "d/MM/yy" |> text
                        }
                    , getCellItems date
                        |> List.map viewCellItem
                        |> column
                            [ height fill
                            , padding 4
                            , spacing 4
                            ]
                    ]

            else
                el cellAttrs Element.none

        onSelectDate : Date -> SearchParams
        onSelectDate date =
            SearchParams (toDateInt date |> Day) Nothing Nothing

        weekdayColumn : Date.Weekday -> Column Date msg
        weekdayColumn weekday =
            Column
                (weekdayToName weekday |> text |> el (Font.center :: headerBaseAttrs))
                shrink
                (Date.ceiling (weekdayToInterval weekday) >> viewCalendarCell)
    in
    table [ Border.width 1, width shrink ]
        { data = Date.range Date.Day 7 (Date.floor Monday firstDate) (Date.ceiling Sunday lastDate)
        , columns = List.range 1 7 |> List.map (Date.numberToWeekday >> weekdayColumn)
        }


viewCalendarForUser : SearchParams -> User -> Calendar -> Date -> Date -> Element msg
viewCalendarForUser searchParams user calendar =
    let
        mapUsers : ShiftRecord -> List User -> Maybe ( Maybe String, String, SearchParams )
        mapUsers shift users =
            if List.any ((==) user) users then
                Just ( Nothing, shift.shiftName, { searchParams | filterShift = Just shift.shiftId, filterUser = Nothing } )

            else
                Nothing

        filterByDate : Date -> ShiftRecord -> Maybe ( Maybe String, String, SearchParams )
        filterByDate date shift =
            Dict.get (Date.toRataDie date) shift.usersByDay
                |> Maybe.andThen (mapUsers shift)

        getCellItems : Date -> List ( Maybe String, String, SearchParams )
        getCellItems date =
            calendar.shifts |> List.filterMap (filterByDate date)
    in
    viewCalendar getCellItems


viewCalendarForShift : SearchParams -> Int -> Calendar -> Date -> Date -> Element msg
viewCalendarForShift searchParams shiftId calendar =
    let
        onUpdateUser : User -> SearchParams
        onUpdateUser user =
            { searchParams | filterUser = Just user, filterShift = Nothing }

        userLabel : User -> Maybe String
        userLabel user =
            Dict.get user calendar.users |> Maybe.map (\( first_name, last_name ) -> first_name ++ " " ++ last_name)

        mapFn : Int -> List User -> List ( Maybe String, String, SearchParams )
        mapFn _ =
            List.map (\user -> ( userLabel user, user, onUpdateUser user ))

        foldUsersByDay : List ShiftRecord -> Dict Int (List ( Maybe String, String, SearchParams ))
        foldUsersByDay shifts =
            case shifts of
                [] ->
                    Dict.empty

                shift :: xs ->
                    if shift.shiftId == shiftId then
                        shift.usersByDay
                            |> Dict.map mapFn

                    else
                        foldUsersByDay xs

        usersByDay : Dict Int (List ( Maybe String, String, SearchParams ))
        usersByDay =
            foldUsersByDay calendar.shifts

        getCellItems : Date -> List ( Maybe String, String, SearchParams )
        getCellItems date =
            Dict.get (Date.toRataDie date) usersByDay
                |> Maybe.withDefault []
    in
    viewCalendar getCellItems



-- API


toDateInt : Date -> Int
toDateInt date =
    10000
        * Date.year date
        + 100
        * (Date.month >> Date.monthToNumber) date
        + Date.day date


fromDateInt : Int -> Date
fromDateInt dateInt =
    Date.fromCalendarDate
        (dateInt // 10000)
        (dateInt // 100 |> modBy 100 |> Date.numberToMonth)
        (modBy 100 dateInt)


getCalendar : Model -> Cmd Msg
getCalendar model =
    let
        params : List Url.Builder.QueryParameter
        params =
            (case model.searchParams.dateRange of
                Today ->
                    []

                Day day ->
                    [ day |> Url.Builder.int "start" ]

                Range start finish ->
                    [ start |> Url.Builder.int "start"
                    , finish |> Url.Builder.int "finish"
                    ]
            )
                ++ List.filterMap identity
                    [ Maybe.map (Url.Builder.string "user") model.searchParams.filterUser
                    , Maybe.map (Url.Builder.int "shift") model.searchParams.filterShift
                    ]
    in
    api [ rootPath ] params GotCalendar calendarDecoder



-- Decode


dateDictDecoder : Decoder (Dict Int (List String))
dateDictDecoder =
    let
        filter ( key, value ) =
            String.toInt key |> Maybe.map (\i -> ( i |> fromDateInt |> Date.toRataDie, value ))
    in
    Json.Decode.keyValuePairs (Json.Decode.list Json.Decode.string)
        |> Json.Decode.map (List.filterMap filter >> Dict.fromList)


calendarDecoder : Decoder Calendar
calendarDecoder =
    Json.Decode.map3 Calendar
        (Json.Decode.map fromDateInt Json.Decode.int |> Json.Decode.list |> Json.Decode.field "dates")
        (Json.Decode.map3 ShiftRecord (index 0 int) (index 1 string) (index 2 dateDictDecoder) |> Json.Decode.list |> Json.Decode.field "shifts")
        (Json.Decode.map2 Tuple.pair (index 0 string) (index 1 string) |> dict |> Json.Decode.field "users")
