module Projection exposing (..)


import Animation exposing (Animation)
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import Geodata exposing (Geodata)
import Html exposing (Html)
import Json.Decode
import Math exposing (Line, Point, Polygon, Ring)
import Task
import TypedSvg exposing (circle, polygon, polyline, rect, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, strokeWidth, style, width, x, y)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Events exposing (on, onMouseUp)
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
  , colorBackground : Color
  , geodata : Maybe Geodata
  , angle : Animation
  , shade : Animation
  , zoom : Animation
  , startAngle : Maybe Float
  , cursor : Cursor
  }


init : RGBA -> (Projection, Cmd Msg)
init colorBackground =
  ( { size = (0, 0)
    , colorBackground = Color.fromRgba colorBackground
    , geodata = Nothing
    , angle = Animation.init Animation.harmonic 1000 (3 * pi / 2)
      |> Animation.to (3 * pi / 2 - degrees 38)
    , shade = Animation.init Animation.harmonic 1000 1
      |> Animation.to 0
    , zoom = Animation.init Animation.harmonic 1000 0
      |> Animation.to 1
    , startAngle = Nothing
    , cursor = Grab
    }
  , getElement
  )


type alias Point =
  (Float, Float)


type Msg
  = Resize Int Int
  | Element (Maybe Browser.Dom.Element)
  | SetGeodata Geodata
  | TimeDelta Float
  | HoldAngle Point
  | MoveAngle Point
  | ReleaseAngle


type Cursor
  = Grab
  | Grabbing


cursorStyle : Cursor -> String
cursorStyle cursor =
  String.concat
    [ "cursor: "
    , ( case cursor of
        Grab -> "grab"
        Grabbing -> "grabbing"
      )
    , ";"
    ]


subscriptions : Projection -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onAnimationFrameDelta TimeDelta
    , Browser.Events.onResize Resize
    ]


getAngle : Point -> Point -> Float
getAngle ( x, y ) ( w, h ) =
  atan2 (x - w / 2) (y - h / 2)


update : Msg -> Projection -> (Projection, Cmd Msg)
update msg projection =
  case msg of
    Resize _ _ ->
      ( projection, getElement )

    Element maybeElement ->
      ( case maybeElement of
        Just element ->
          { projection
          | size =
            ( element.element.width
            , element.element.height
            )
          }

        Nothing ->
          projection

      , Cmd.none
      )

    SetGeodata geodata ->
      ( { projection
        | geodata = Just geodata
        , angle = Animation.run projection.angle
        , shade = Animation.run projection.shade
        , zoom = Animation.run projection.zoom
        }
      , Cmd.none
      )

    TimeDelta dt ->
      ( { projection
        | angle = Animation.step dt projection.angle
        , shade = Animation.step dt projection.shade
        , zoom = Animation.step dt projection.zoom
        }
      , Cmd.none
      )

    HoldAngle point ->
      ( { projection
        | startAngle = Just <| getAngle point projection.size
        , cursor = Grabbing
        }
      , Cmd.none
      )

    MoveAngle point ->
      ( case projection.startAngle of
        Just downAngle ->
          let
            stopAngle =
              getAngle point projection.size

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
              |> Animation.withDuration 200
              |> Animation.to (projection.angle.stop + normalize angleDelta)
          , startAngle = Just stopAngle
          }

        Nothing ->
          projection

      , Cmd.none
      )

    ReleaseAngle ->
      ( { projection
        | startAngle = Nothing
        , cursor = Grab
        }
      , Cmd.none
      )


getElement : Cmd Msg
getElement =
  Browser.Dom.getElement "projection"
    |> Task.attempt (Result.toMaybe >> Element)


mousePosition : Json.Decode.Decoder Point
mousePosition =
  Json.Decode.map2 Tuple.pair
    (Json.Decode.field "offsetX" Json.Decode.float)
    (Json.Decode.field "offsetY" Json.Decode.float)


preventDefault : Json.Decode.Decoder Msg -> VirtualDom.Handler Msg
preventDefault decoder =
  VirtualDom.MayPreventDefault
    <| Json.Decode.map2 Tuple.pair
      decoder
      (Json.Decode.succeed True)


view : Projection -> Html Msg
view projection =
  svg
    [ id "projection"
    , width <| percent 100
    , height <| percent 100
    , style <| cursorStyle projection.cursor
    , on "pointerdown"
      <| preventDefault
      <| Json.Decode.map HoldAngle
      <| mousePosition
    , on "pointermove"
      <| preventDefault
      <| Json.Decode.map MoveAngle
      <| mousePosition
    , on "pointerup"
      <| preventDefault
      <| Json.Decode.succeed ReleaseAngle
    ]
    <| case projection.geodata of
      Just geodata ->
        let
          ( w, h ) =
            projection.size

          a =
            min w h

          scale ( x, y ) =
            ( w / 2 + x * a / 2, h / 2 - y * a / 2 )

          r beta =
            sin <| min beta (pi / 2)

          z =
            2 ^ projection.zoom.value

          transformTheta theta =
            let
              beta =
                theta / z

              beta0 =
                pi / z
            in
            0.5 * z * r beta / r beta0

          transform ( phi, theta ) =
            fromPolar
              (transformTheta theta)
              phi

          rotate ( phi, theta ) =
            ( projection.angle.value + phi, theta )

          spherical ( lng, lat ) =
            ( degrees lng, pi / 2 - degrees lat )

          project =
            spherical
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

          landWater : Polygon -> List (Svg Msg)
          landWater pol =
            List.concat
              [ [ land pol.exterior ]
              , List.map water pol.interiors
              ]

          waterWater : Polygon -> List (Svg Msg)
          waterWater pol =
            List.concat
              [ [ water pol.exterior ]
              , List.map water pol.interiors
              ]

          waterLand : Polygon -> List (Svg Msg)
          waterLand pol =
            List.concat
              [ [ water pol.exterior ]
              , List.map land pol.interiors
              ]

          river : Line -> Svg Msg
          river line =
            polyline
              [ points <| List.map project line
              , stroke <| Paint colorContour
              , fill PaintNone
              ]
              []

          city : Point -> Svg Msg
          city point =
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

          earthCircle : Maybe Color -> Maybe Color -> Svg Msg
          earthCircle strokeColor fillColor =
            circle
              [ InPx.cx <| w / 2
              , InPx.cy <| h / 2
              , InPx.r <| a / 2 * transformTheta pi
              , stroke
                <| Maybe.withDefault PaintNone
                <| Maybe.map Paint strokeColor
              , fill
                <| Maybe.withDefault PaintNone
                <| Maybe.map Paint fillColor
              ]
              []

          shade : Svg Msg
          shade = rect
            [ InPx.x 0
            , InPx.y 0
            , InPx.width w
            , InPx.height h
            , fill
              <| Paint
              <| withAlpha projection.shade.value projection.colorBackground
            ]
            []
        in
        List.concat
          [ [ earthCircle Nothing (Just colorLand)
            ]
          , List.concatMap waterWater geodata.landAntarctica
          , List.concatMap landWater geodata.landWithoutAntarctica
          , List.map river geodata.rivers
          , List.concatMap waterLand geodata.lakes
          , List.map city geodata.cities
          , [ earthCircle (Just colorContour) Nothing
            , shade
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
