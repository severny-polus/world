module Projection exposing (..)


import Color
import Html exposing (Html)
import Math exposing (Point, Polygon)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..))


type alias Projection =
  { algorithm : Algorithm
  , size : Size
  , geodata : Geodata
  , angle : Float
  }


type alias Algorithm =
  { inscribe : Size -> Size
  , transform : Point -> Point
  , filter : Polygon -> Bool
  }


type alias Size =
  (Float, Float)


type alias Geodata =
  List Polygon


type Msg
  = Size Size
  | AngleChange Float


init : Algorithm -> Size -> Geodata -> Projection
init algorithm size geodata =
  { algorithm = algorithm
  , angle = 0
  , size = size
  , geodata = geodata
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
    (w, h) =
      projection.algorithm.inscribe projection.size

    scale (x, y) =
      ((1 + x) * w / 2, (1 - y) * h / 2)

    rotate (lng, lat) =
      (lng + projection.angle, lat)

    spherical (lng, lat) =
      (degrees lng, pi / 2 - degrees lat)

    project =
      spherical
        >> rotate
        >> projection.algorithm.transform
        >> scale

    exterior ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint Color.white
        , InPx.strokeWidth 1
        ]
        []

    interior ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint Color.white
        , InPx.strokeWidth 1
        ]
        []

    svgPolygons : Polygon -> List (Svg Msg)
    svgPolygons pol =
      List.append
        [ exterior pol.exterior ]
        <| List.map interior pol.interiors

  in
  svg
    [ InPx.width w
    , InPx.height h
    ]
    <| List.concatMap svgPolygons
    <| List.filter projection.algorithm.filter projection.geodata
      

-- TODO: fix
mecrator : Algorithm
mecrator =
  { inscribe =
    \(w, h) ->
      if w > 2 * h
        then (2 * h, h)
        else (w, w / 2)
  , transform =
    \(phi, theta) ->
      (phi / pi, 1 - theta / (pi / 2))
  , filter =
    \_ -> True
  }


disc : Algorithm
disc =
  { inscribe =
    \(w, h) ->
      let a = min w h
      in (a, a)
  , transform =
    \(phi, theta) ->
      polar
        (cos <| pi / 2 - theta / 2)
        (pi + phi)
  , filter =
    \pol ->
      Maybe.withDefault False
        <| Maybe.map ((<) -60)
        <| List.maximum
        <| List.map Tuple.second pol.exterior
  }


polar : Float -> Float -> Point
polar r phi =
  (r * cos phi, r * sin phi)


mod : Float -> Float -> Float
mod x y =
  x - y * (toFloat <| truncate <| x / y)
