module Common exposing (..)

import Element exposing (Element, column, fill, link, mouseOver, paddingXY, rgb255, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Http
import Json.Decode exposing (Decoder)
import Time
import Url
import Url.Builder
import Url.Parser


api : List String -> List Url.Builder.QueryParameter -> (Result Http.Error a -> msg) -> Decoder a -> Cmd msg
api pathSegments parameters toMsg decoder =
    Http.get
        { url = Url.Builder.crossOrigin "https://api.easyrad.duckdns.org" pathSegments parameters
        , expect = Http.expectJson toMsg decoder
        }


usersPath : String
usersPath =
    "users"


userPath : String
userPath =
    "user"


shiftsPath : String
shiftsPath =
    "shifts"


shiftPath : String
shiftPath =
    "shift"



-- Types


type alias User =
    String


type alias UserAssignment =
    { user : User
    , firstname : String
    , lastname : String
    }


type alias Shift =
    String



-- Decoders


userAssignmentDecoder : Decoder UserAssignment
userAssignmentDecoder =
    Json.Decode.map3 UserAssignment
        (Json.Decode.index 0 Json.Decode.string)
        (Json.Decode.index 1 Json.Decode.string)
        (Json.Decode.index 2 Json.Decode.string)


posixDecoder : Decoder Time.Posix
posixDecoder =
    Json.Decode.map ((*) 1000 >> Time.millisToPosix) Json.Decode.int


decodedString : Url.Parser.Parser (String -> a) a
decodedString =
    Url.Parser.custom "STRING" Url.percentDecode



-- View Constants


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


anchor : List (Element.Attribute msg)
anchor =
    [ Font.size 16
    , paddingXY 8 4
    , Border.rounded 4
    , width fill
    , mouseOver mouseOverAttrs
    ]
        ++ defaultAttrs



-- View


viewLoading : Element msg
viewLoading =
    text "Loading..."


viewHttpError : Http.Error -> Element msg
viewHttpError error =
    case error of
        Http.BadBody _ ->
            text "Bad body"

        Http.BadUrl _ ->
            text "Bad URL"

        Http.Timeout ->
            text "Timeout"

        Http.NetworkError ->
            text "Network Error"

        Http.BadStatus _ ->
            text "Bad Status"


viewSubMenu : List ( String, String ) -> Element msg
viewSubMenu items =
    column [ spacing 4 ]
        (items
            |> List.map (\( url, label ) -> link anchor { url = url, label = text label })
        )


toMonth : Time.Month -> Int
toMonth month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
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


transformUser : (String -> String) -> UserAssignment -> ( String, String )
transformUser user_url user =
    ( user_url user.user, user.lastname ++ ", " ++ user.firstname )


transformShift : (String -> String) -> Shift -> ( String, String )
transformShift shift_url shift =
    ( shift_url shift, shift )


transformRequestShift : Shift -> ( String, String )
transformRequestShift shift =
    ( "/requests/shift/" ++ shift, shift )


transformRequestUser : UserAssignment -> ( String, String )
transformRequestUser user =
    ( "/requests/user/" ++ user.user, user.lastname ++ ", " ++ user.firstname )
