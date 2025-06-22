module Main exposing (..)

import BaseRoster
import Browser
import Browser.Navigation as Nav
import Calendar
import Common exposing (activeAttrs, color, defaultAttrs, mouseOverAttrs, viewLoading, viewSubMenu)
import Element exposing (Element, column, el, fill, layout, link, mouseOver, padding, paddingXY, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import List
import Requests
import Time
import TimeZone
import Tuple
import Url
import Url.Parser exposing ((</>), Parser, oneOf, top)



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
    | BaseRosterRoute BaseRoster.Route
    | RequestsRoute Requests.Route
    | CalendarRoute Calendar.Route


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ Url.Parser.map HomeRoute top
        , Url.Parser.map BaseRosterRoute BaseRoster.routeParser
        , Url.Parser.map RequestsRoute Requests.routeParser
        , Url.Parser.map CalendarRoute Calendar.routeParser
        ]


stepUrl : Url.Url -> Model -> ( Model, Cmd Msg )
stepUrl url model =
    let
        route =
            Url.Parser.parse routeParser url

        ( data, cmd ) =
            case route of
                Nothing ->
                    ( Loading, Nav.pushUrl model.key "/" )

                Just HomeRoute ->
                    ( Home, Cmd.none )

                Just (BaseRosterRoute subroute) ->
                    BaseRoster.urlChanged subroute |> Tuple.mapBoth BaseRosterData (Cmd.map BaseRosterMsg)

                Just (RequestsRoute subroute) ->
                    Requests.urlChanged subroute |> Tuple.mapBoth RequestsData (Cmd.map RequestsMsg)

                Just (CalendarRoute subroute) ->
                    Calendar.urlChanged subroute |> Tuple.mapBoth CalendarData (Cmd.map CalendarMsg)
    in
    ( { model | route = route, data = data }, cmd )



-- MODEL


type alias Model =
    { key : Nav.Key
    , route : Maybe Route
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
    = Loading
    | Home
    | BaseRosterData BaseRoster.Model
    | RequestsData Requests.Model
    | CalendarData Calendar.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    stepUrl url { key = key, route = Nothing, zone = TimeZone.pacific__auckland (), data = Loading }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | BaseRosterMsg BaseRoster.Msg
    | RequestsMsg Requests.Msg
    | CalendarMsg Calendar.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            stepUrl url model

        BaseRosterMsg subMsg ->
            ( { model | data = BaseRosterData (BaseRoster.update subMsg) }, Cmd.none )

        RequestsMsg subMsg ->
            ( { model | data = RequestsData (Requests.update subMsg) }, Cmd.none )

        CalendarMsg subMsg ->
            ( { model | data = CalendarData (Calendar.update subMsg) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Information"
    , body =
        [ column
            [ spacing 10
            , width fill
            ]
            [ viewNavBar model.route
            , viewData model |> el [ padding 8 ]
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


navLinks : Route -> List ( String, String )
navLinks route =
    ( "/", "Home" )
        :: (case route of
                HomeRoute ->
                    []

                BaseRosterRoute subroute ->
                    BaseRoster.navBar subroute

                RequestsRoute subroute ->
                    Requests.navBar subroute

                CalendarRoute subroute ->
                    Calendar.navBar subroute
           )


viewNavBar : Maybe Route -> Element msg
viewNavBar route =
    case route of
        Nothing ->
            Element.none

        Just r ->
            let
                links =
                    navLinks r
            in
            row
                [ width fill
                , padding 8
                , spacing 8
                , Background.color color.blue
                ]
            <|
                List.indexedMap (\index ( url, label ) -> navBarLink url label (index == List.length links - 1)) links


viewData : Model -> Element msg
viewData model =
    case model.data of
        Loading ->
            viewLoading

        Home ->
            viewSubMenu
                [ BaseRoster.rootLink
                , Requests.rootLink
                , Calendar.rootLink
                ]

        BaseRosterData data ->
            BaseRoster.view data

        RequestsData data ->
            Requests.view model.zone data

        CalendarData data ->
            Calendar.view data
