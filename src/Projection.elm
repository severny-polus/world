module Projection exposing (..)


import Browser.Dom
import Browser.Events
import Color exposing (Color, rgb255)
import Html exposing (Html)
import Math exposing (Point, Polygon)
import Task
import TypedSvg exposing (circle, polygon, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, width)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias Projection =
  { size : Size
  , exceptAntarctica : Geodata
  , antarctica : Geodata
  , angle : Float
  , zoom : Float
  }


type alias Size =
  (Float, Float)


type alias Geodata =
  List Polygon


type Msg
  = Resize Int Int
  | Element (Maybe Browser.Dom.Element)


init : Geodata -> Geodata -> (Projection, Cmd Msg)
init exceptAntarctica antarctica =
  let
    initAngle =
      pi + degrees 56
  in
  ( { size = (0, 0)
    , exceptAntarctica = exceptAntarctica
    , antarctica = antarctica
    , angle = initAngle
    , zoom = 1
    }
  , Browser.Dom.getElement "projection"
    |> Task.attempt (Result.toMaybe >> Element)
  )


subscriptions : Projection -> Sub Msg
subscriptions projection =
  Sub.batch <| List.concat
    [ [ Browser.Events.onResize Resize ]
    ]


update : Msg -> Projection -> (Projection, Cmd Msg)
update msg projection =
  case msg of
    Resize _ _ ->
      ( projection
      , Browser.Dom.getElement "projection"
        |> Task.attempt (Result.toMaybe >> Element)
      )

    Element element ->
      ( { projection
        | size =
          Maybe.withDefault (0, 0)
            <| Maybe.map2 Tuple.pair
              (Maybe.map (.element >> .width) element)
              (Maybe.map (.element >> .height) element)
        }
      , Cmd.none
      )


view : Projection -> Html Msg
view projection =
  let
    (w, h) =
      projection.size

    a =
      min w h

    scale (x, y) =
      (w / 2 + x * a / 2, h / 2 - y * a / 2)

    rotate (phi, theta) =
      (phi + projection.angle, theta)

    spherical (lng, lat) =
      (degrees lng, pi / 2 - degrees lat)

    project =
      spherical
        >> rotate
        >> transform projection.zoom
        >> scale

    exterior ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint colorLand
        , InPx.strokeWidth 1
        ]
        []

    interior ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint colorWater
        , InPx.strokeWidth 1
        ]
        []

    polygons : Polygon -> List (Svg Msg)
    polygons pol =
      List.concat
        [ [ exterior pol.exterior ]
        , List.map interior pol.interiors
        ]

    outerPolygons : Polygon -> List (Svg Msg)
    outerPolygons pol =
      List.concat
        [ [ interior pol.exterior ]
        , List.map interior pol.interiors
        ]

  in
  svg
    [ id "projection"
    , width <| percent 100
    , height <| percent 100
    ]
    <| List.concat
      [ [ circle
          [ InPx.cx <| w / 2
          , InPx.cy <| h / 2
          , InPx.r <| a / 2 * projection.zoom
          , fill <| Paint colorLand
          ]
          []
        ]
      , List.concatMap outerPolygons projection.antarctica
      , List.concatMap polygons projection.exceptAntarctica
      ]


r : Float -> Float
r theta =
  sin <| theta / 2


transform : Float -> Point -> Point
transform zoom (phi, theta) =
  fromPolar (zoom * r (theta / zoom) / r (pi / zoom)) phi


fromPolar : Float -> Float -> Point
fromPolar rho phi =
  (rho * cos phi, rho * sin phi)


colorLand : Color
colorLand =
  rgb255 0 165 84


colorWater : Color
colorWater =
  rgb255 46 122 197


rotationPeriodMs : Float
rotationPeriodMs =
  100


rotationFunction : Float -> Float
rotationFunction parameter =
  if parameter < 0.5 then
    2 * parameter ^ 2
  else
    1 - 2 * (parameter - 1) ^ 2


rotationDerivative : Float -> Float
rotationDerivative t =
  if t < 0.5 then
    4 * t
  else
    4 - 4 * t
