module Projection exposing (..)


import Color exposing (Color, rgb255)
import Html exposing (Html)
import Math exposing (Point, Polygon)
import TypedSvg exposing (circle, polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))


type alias Projection =
  { size : Size
  , land : Geodata
  , antarctica : Geodata
  , angle : Float
  }


type alias Size =
  (Float, Float)


type alias Geodata =
  List Polygon


type Msg
  = Size Size
  | AngleChange Float


init : Size -> Geodata -> Geodata -> Projection
init size land antarctica =
  { angle = 0
  , size = size
  , land = land
  , antarctica = antarctica
  }


update : Msg -> Projection -> Projection
update msg projection =
  case msg of
    Size size ->
      { projection | size = size }

    AngleChange da ->
      { projection | angle = projection.angle + da }


view : Projection -> Html Msg
view projection =
  let
    a =
      min
        (Tuple.first projection.size)
        (Tuple.second projection.size)

    (w, h) =
      (a, a)

    scale (x, y) =
      ((1 + x) * w / 2, (1 - y) * h / 2)

    rotate (phi, theta) =
      (phi + projection.angle, theta)

    spherical (lng, lat) =
      (degrees lng, pi / 2 - degrees lat)

    project =
      spherical
        >> rotate
        >> transform
        >> scale

    exterior ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint colorEarth
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
    [ InPx.width w
    , InPx.height h
    ]
    <| List.concat
      [ [ circle
          [ InPx.cx <| w / 2
          , InPx.cy <| h / 2
          , InPx.r <| a / 2
          , fill <| Paint colorEarth
          ]
          []
        ]
      , List.concatMap outerPolygons projection.antarctica
      , List.concatMap polygons projection.land
      ]


minSquare : Size -> Size
minSquare (w, h) =
  let a = min w h
  in (a, a)


transform : Point -> Point
transform (phi, theta) =
  polar
    (sin <| theta / 2)
    (pi + phi)


polar : Float -> Float -> Point
polar r phi =
  (r * cos phi, r * sin phi)


dropAntarctica : Polygon -> Bool
dropAntarctica pol =
  Maybe.withDefault False
    <| Maybe.map ((<) -60)
    <| List.maximum
    <| List.map Tuple.second pol.exterior


mod : Float -> Float -> Float
mod x y =
  x - y * (toFloat <| floor <| x / y)


colorWater : Color
colorWater =
  rgb255 46 122 197


colorEarth : Color
colorEarth =
  rgb255 0 165 84
