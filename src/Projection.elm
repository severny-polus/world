module Projection exposing (..)

import Animation exposing (Animation)
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import Geodata exposing (Geodata)
import Html exposing (Html)
import Json.Decode
import Math exposing (Line, Point, Polygon, Ring, isOuter, toTuple)
import Task
import Time
import TypedSvg exposing (circle, polygon, polyline, rect, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, strokeWidth, style, width, x, y)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (on)
import TypedSvg.Types exposing (Paint(..), percent, px)
import VirtualDom


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


type alias Projection =
    { size : Point
    , center : Point
    , colorBackground : Color
    , geodata : Maybe Geodata
    , angle : Animation
    , zoom : Animation
    , load : Animation
    , startAngle : Maybe Float
    , cursor : Cursor
    , shift : Bool
    , ctrl : Bool
    }


init : RGBA -> ( Projection, Cmd Msg )
init colorBackground =
    ( { size = ( 0, 0 )
      , center = ( 0, 0 )
      , colorBackground = Color.fromRgba colorBackground
      , geodata = Nothing
      , angle = Animation.init (Animation.Parabolic 0.5) 200 (3 * pi / 2 + initAngle)
      , zoom = Animation.init (Animation.Parabolic 0.5) 500 1
      , load = Animation.init (Animation.Parabolic 0.3) 1000 1 |> Animation.to 0
      , startAngle = Nothing
      , cursor = Grab
      , shift = False
      , ctrl = False
      }
    , getElement
    )


initAngle : Float
initAngle =
    degrees -38


type alias Point =
    ( Float, Float )


type Msg
    = Resize Int Int
    | Element (Maybe Browser.Dom.Element)
    | SetGeodata Geodata
    | TimeDelta Float
    | MouseDown Point
    | MouseMove Point
    | MouseUp
    | Wheel Float


subscriptions : Projection -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta TimeDelta
        , Browser.Events.onResize Resize
        ]


getAngle : Point -> Projection -> Float
getAngle ( x, y ) projection =
    let
        ( cx, cy ) =
            projection.center
    in
    atan2 (x - cx) (y - cy)


setSize : Point -> Projection -> Projection
setSize size projection =
    let
        ( w, h ) =
            size
    in
    { projection
        | size = size
        , center = ( w / 2, h / 2 )
    }


update : Msg -> Projection -> ( Projection, Cmd Msg )
update msg projection =
    case msg of
        Resize _ _ ->
            ( projection, getElement )

        Element maybeElement ->
            ( case maybeElement of
                Just element ->
                    projection |> setSize ( element.element.width, element.element.height )

                Nothing ->
                    projection
            , Cmd.none
            )

        SetGeodata geodata ->
            ( { projection
                | geodata = Just geodata
                , angle = Animation.run projection.angle
                , zoom = Animation.run projection.zoom
                , load = Animation.run projection.load
              }
            , Cmd.none
            )

        TimeDelta dt ->
            ( { projection
                | angle = Animation.step dt projection.angle
                , zoom = Animation.step dt projection.zoom
                , load = Animation.step dt projection.load
              }
            , Cmd.none
            )

        MouseDown point ->
            ( { projection
                | startAngle = projection |> getAngle point |> Just
                , cursor = Grabbing
              }
            , Cmd.none
            )

        MouseMove point ->
            ( case projection.startAngle of
                Just downAngle ->
                    let
                        stopAngle =
                            projection |> getAngle point

                        angleDelta =
                            stopAngle - downAngle

                        normalize ad =
                            if ad < -pi then
                                normalize <| ad + 2 * pi

                            else if ad > pi then
                                normalize <| ad - 2 * pi

                            else
                                ad
                    in
                    { projection
                        | angle =
                            projection.angle
                                |> Animation.to (projection.angle.stop + normalize angleDelta)
                        , startAngle = Just stopAngle
                    }

                Nothing ->
                    projection
            , Cmd.none
            )

        MouseUp ->
            ( { projection
                | startAngle = Nothing
                , cursor = Grab
              }
            , Cmd.none
            )

        Wheel dy ->
            ( { projection
                | zoom =
                    projection.zoom
                        |> Animation.to
                            (max 1 (min 10 (projection.zoom.stop - 0.2 * dy / 53)))
              }
            , Cmd.none
            )


getElement : Cmd Msg
getElement =
    Browser.Dom.getElement "projection"
        |> Task.attempt (Result.toMaybe >> Element)


preventDefault : Json.Decode.Decoder Msg -> VirtualDom.Handler Msg
preventDefault decoder =
    VirtualDom.MayPreventDefault <|
        Json.Decode.map2 Tuple.pair
            decoder
            (Json.Decode.succeed True)


mousePosition : Json.Decode.Decoder Point
mousePosition =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "offsetX" Json.Decode.float)
        (Json.Decode.field "offsetY" Json.Decode.float)


view : Projection -> Html Msg
view projection =
    svg
        [ id "projection"
        , width <| percent 100
        , height <| percent 100
        , style <| cursorStyle projection.cursor
        , on "mousedown" <|
            preventDefault <|
                Json.Decode.map MouseDown <|
                    mousePosition
        , on "mousemove" <|
            preventDefault <|
                Json.Decode.map MouseMove <|
                    mousePosition
        , on "mouseup" <|
            preventDefault <|
                Json.Decode.succeed MouseUp
        , on "wheel" <|
            preventDefault <|
                Json.Decode.map Wheel <|
                    Json.Decode.field "deltaY" <|
                        Json.Decode.float
        ]
    <|
        case projection.geodata of
            Just geodata ->
                let
                    -- turn geodata values into spherical coordinates (theta from north pole)
                    spherical ( lng, lat ) =
                        ( degrees lng, pi / 2 - degrees lat )

                    -- rotate the map according to model position
                    rotate ( phi, theta ) =
                        ( projection.angle.value - projection.load.value * initAngle + phi, theta )

                    -- calculate the scale value from model ones
                    z =
                        2 ^ (projection.zoom.value - projection.load.value)

                    -- calculate the distance from the point to the center of the screen
                    r beta =
                        sin <| min beta (pi / 2)

                    -- transform the angle according to zoom
                    transformTheta theta =
                        let
                            beta =
                                theta / 2

                            beta0 =
                                pi / 2
                        in
                        0.5 * z * r beta / r beta0

                    -- turn polar coordinates into Decart ones
                    transform ( phi, theta ) =
                        fromPolar
                            (transformTheta theta)
                            phi

                    -- get viewport metrics
                    ( w, h ) =
                        projection.size

                    -- get the side of the square fit into the viewport
                    a =
                        min w h

                    -- scale coordinates
                    scale ( x, y ) =
                        ( w / 2 + x * a / 2, h / 2 - y * a / 2 )

                    project =
                        toTuple
                            >> spherical
                            >> rotate
                            >> transform
                            >> scale

                    colorLand =
                        Color.rgb255 0 165 84

                    colorWater =
                        Color.rgb255 46 122 197

                    colorContour =
                        Color.rgb255 0 0 0

                    colorCity =
                        Color.rgb255 254 203 0

                    land : Ring -> Svg Msg
                    land ring =
                        polygon
                            [ points <| List.map project ring
                            , stroke <| Paint colorContour
                            , fill <| Paint colorLand
                            , strokeWidth <| px 1
                            ]
                            []

                    water : Ring -> Svg Msg
                    water ring =
                        polygon
                            [ points <| List.map project ring
                            , stroke <| Paint colorContour
                            , fill <| Paint colorWater
                            , strokeWidth <| px 1
                            ]
                            []

                    drawContinent : Polygon -> List (Svg Msg)
                    drawContinent pol =
                        List.concat
                            [ [ land pol.exterior ]
                            , List.map water <| List.filter (not << isOuter) pol.interiors
                            ]

                    drawSouthContinent : Polygon -> List (Svg Msg)
                    drawSouthContinent pol =
                        List.concat
                            [ [ water pol.exterior ]
                            , List.map water <| List.filter (not << isOuter) pol.interiors
                            ]

                    drawLake : Polygon -> List (Svg Msg)
                    drawLake pol =
                        List.concat
                            [ if not <| isOuter pol.exterior then
                                [ water pol.exterior ]

                              else
                                []
                            , List.map land <| List.filter (not << isOuter) pol.interiors
                            ]

                    drawRiver : Line -> Svg Msg
                    drawRiver line =
                        polyline
                            [ points <| List.map project line
                            , stroke <| Paint colorContour
                            , fill PaintNone
                            ]
                            []

                    drawCity point =
                        let
                            ( x, y ) =
                                project point
                        in
                        circle
                            [ InPx.cx x
                            , InPx.cy y
                            , InPx.r 2
                            , stroke <| Paint colorContour
                            , fill <| Paint colorCity
                            ]
                            []

                    drawEarthCircle : Maybe Color -> Maybe Color -> Svg Msg
                    drawEarthCircle strokeColor fillColor =
                        circle
                            [ InPx.cx <| w / 2
                            , InPx.cy <| h / 2
                            , InPx.r <| a / 2 * transformTheta pi
                            , stroke <|
                                Maybe.withDefault PaintNone <|
                                    Maybe.map Paint strokeColor
                            , fill <|
                                Maybe.withDefault PaintNone <|
                                    Maybe.map Paint fillColor
                            ]
                            []

                    shade : Svg Msg
                    shade =
                        rect
                            [ InPx.x 0
                            , InPx.y 0
                            , InPx.width w
                            , InPx.height h
                            , fill <|
                                Paint <|
                                    withAlpha projection.load.value projection.colorBackground
                            ]
                            []

                    findSouthLand : List Polygon -> ( Maybe Polygon, List Polygon )
                    findSouthLand polygons =
                        let
                            f p ( msp, lp ) =
                                case msp of
                                    Just _ ->
                                        ( msp, p :: lp )

                                    Nothing ->
                                        if isOuter p.exterior then
                                            ( Just p, lp )

                                        else
                                            ( Nothing, p :: lp )
                        in
                        List.foldl f ( Nothing, [] ) polygons

                    drawEarth : List Polygon -> List (Svg Msg)
                    drawEarth polygons =
                        let
                            ( maybeSouthLand, continents ) =
                                findSouthLand polygons
                        in
                        List.concat <|
                            case maybeSouthLand of
                                Just southLand ->
                                    [ [ drawEarthCircle Nothing (Just colorLand)
                                      ]
                                    , drawSouthContinent southLand
                                    , List.concatMap drawContinent continents
                                    ]

                                Nothing ->
                                    [ [ drawEarthCircle Nothing (Just colorWater)
                                      ]
                                    , List.concatMap drawContinent continents
                                    ]
                in
                List.concat
                    [ drawEarth geodata.land -- континенты
                    , List.map drawRiver geodata.rivers -- реки
                    , List.concatMap drawLake geodata.lakes -- озёра
                    , List.map drawCity geodata.cities -- населённые пункты
                    , [ drawEarthCircle (Just colorContour) Nothing -- контур вокруг карты
                      , shade -- переход при открытии
                      ]
                    ]

            Nothing ->
                []


fromPolar : Float -> Float -> Point
fromPolar rho phi =
    ( rho * cos phi, rho * sin phi )


withAlpha : Float -> Color -> Color
withAlpha alpha color =
    let
        rgba =
            Color.toRgba color
    in
    Color.rgba rgba.red rgba.green rgba.blue alpha


colorTransparent : Color
colorTransparent =
    Color.rgba 0 0 0 0


type Cursor
    = Grab
    | Grabbing


cursorStyle : Cursor -> String
cursorStyle cursor =
    String.concat
        [ "cursor: "
        , case cursor of
            Grab ->
                "grab"

            Grabbing ->
                "grabbing"
        , ";"
        ]
