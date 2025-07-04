module Calendar exposing (..)

import Common exposing (Shift, User, activeAttrs, anchor, api, viewHttpError, viewLoading)
import Date exposing (Date, Interval(..), Unit(..))
import Dict exposing (Dict)
import Element exposing (Attribute, Column, Element, alignTop, column, el, fill, height, htmlAttribute, inFront, link, mouseOver, padding, rgb, rgba, row, shrink, spacing, table, text, transparent, width, wrappedRow)
import Element.Background
import Element.Border as Border
import Element.Font as Font
import Html.Attributes
import Http
import Json.Decode exposing (Decoder, dict, index, int, string)
import List
import Maybe
import Time exposing (Month(..))
import Tuple exposing (pair)
import Url.Builder exposing (absolute)
import Url.Parser exposing ((</>), (<?>), Parser, s)
import Url.Parser.Query



-- MODEL


type alias Model =
    { searchParams : SearchParams
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
    = GotCalendar SearchParams (Result Http.Error Calendar)


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
            Model (routeToParams route) Loading
    in
    ( model, getCalendar model )


update : Msg -> Model
update msg =
    case msg of
        GotCalendar searchParams data ->
            Model searchParams
                (case data of
                    Ok r ->
                        Data r

                    Err err ->
                        HttpError err
                )


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


view : Model -> Element msg
view model =
    case model.data of
        Loading ->
            viewLoading

        HttpError e ->
            viewHttpError e

        Data calendar ->
            let
                childAttrs =
                    [ alignTop, padding 8, spacing 8, Border.width 2, Border.rounded 5 ]
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

                -- , column childAttrs
                --     [ el [ Font.bold ] (text "Users")
                --     , wrappedRow [ spacing 8 ] (Dict.toList calendar.users |> List.map (viewUser model.searchParams))
                --     ]
                , viewCalendar model.searchParams calendar
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
        [ Element.Background.color (rgb 0 0 0)
        , Font.color (rgb 1 1 1)
        , padding 4
        , Border.rounded 5
        , Font.size 14
        , Border.shadow
            { offset = ( 0, 3 ), blur = 6, size = 0, color = rgba 0 0 0 0.32 }
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

    -- , Border.widthEach {bottom=0, left=0, right=1, top=1}
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


viewUser : SearchParams -> ( String, ( String, String ) ) -> Element msg
viewUser params ( user_code, ( first_name, last_name ) ) =
    link
        [ tooltip Element.above (myTooltip <| first_name ++ " " ++ last_name)
        , width shrink
        ]
        { url = buildUrl { params | filterUser = Just user_code }
        , label = user_code |> text
        }


viewShift : SearchParams -> Shift -> Int -> Element msg
viewShift params shift shift_id =
    link headerBaseAttrs
        { url = buildUrl { params | filterShift = Just shift_id }
        , label = text shift
        }


viewCell : SearchParams -> Dict User ( String, String ) -> List User -> Element msg
viewCell params userDict users =
    users
        |> List.filterMap
            (\user_code ->
                Dict.get user_code userDict
                    |> Maybe.map (pair user_code)
            )
        |> List.map (viewUser params)
        |> column cellAttrs


headerBaseAttrs : List (Element.Attribute msg)
headerBaseAttrs =
    Font.bold :: cellAttrs


dateFormat : String
dateFormat =
    "E d/MM"


viewColumnHeader : SearchParams -> Date -> Element msg
viewColumnHeader params date =
    link headerBaseAttrs { url = buildUrl { params | dateRange = Day (toDateInt date) }, label = date |> Date.format dateFormat |> text }


viewRowHeader : SearchParams -> ShiftRecord -> Element msg
viewRowHeader params r =
    viewShift params r.shiftName r.shiftId


viewCalendar : SearchParams -> Calendar -> Element msg
viewCalendar params calendar =
    let
        mapper : Date -> Column ShiftRecord msg
        mapper date =
            Column
                (viewColumnHeader params date)
                shrink
                (.usersByDay >> Dict.get (Date.toRataDie date) >> Maybe.map (viewCell params calendar.users) >> Maybe.withDefault (el cellAttrs Element.none))
    in
    table [ Border.width 1, width shrink ]
        { data = calendar.shifts
        , columns = Column (el headerBaseAttrs Element.none) shrink (viewRowHeader params) :: (calendar.dates |> List.map mapper)
        }



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
    api [ rootPath ] params (GotCalendar model.searchParams) calendarDecoder



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
