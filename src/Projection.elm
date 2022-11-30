module Projection exposing (..)


import Color
import Html exposing (Html)
import Math exposing (Point, Polygon)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Types exposing (Paint(..))


type alias Projection =
  { algorithm : Algorithm
  , size : Size
  , data : Data
  , angle : Float
  }


type alias Algorithm =
  { inscribe : Size -> Size
  , transform : Point -> Point
  }


type alias Size =
  (Float, Float)


type alias Data =
  List Polygon


type Msg
  = Size Size
  | AngleChange Float


init : Algorithm -> Size -> Data -> Projection
init algorithm size data =
  { algorithm = algorithm
  , angle = 0
  , size = size
  , data = data
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

    fromDegrees (lng, lat) =
      (degrees lng, degrees lat)

    project =
      fromDegrees
        >> rotate
        >> projection.algorithm.transform
        >> scale

    inPol pol =
      polygon
        [ points <| List.map project pol.inclusion
        , stroke <| Paint Color.black
        , fill <| Paint Color.white
        , InPx.strokeWidth 1
        ]
        []

    exPol pol =
      polygon
        [ points <| List.map project pol.exclusion
        , stroke <| Paint Color.black
        , fill <| Paint Color.white
        , InPx.strokeWidth 1
        ]
        []
  in
  svg
    [ InPx.width w
    , InPx.height h
    ]
    <| List.append
      (List.map inPol projection.data)
      (List.map exPol projection.data)
      

-- TODO: fix
mecrator : Algorithm
mecrator =
  { inscribe =
    \(w, h) ->
      if w > 2 * h
        then (2 * h, h)
        else (w, w / 2)
  , transform =
    \(lng, lat) ->
      (lng / pi, lat / (pi / 2))
  }


disc : Algorithm
disc =
  { inscribe =
    \(w, h) ->
      let a = min w h
      in (a, a)
  , transform =
    \(lng, lat) ->
      polar
        (cos <| (pi / 2 + lat) / 2)
        (lng + pi)
  }


polar : Float -> Float -> Point
polar r phi =
  (r * cos phi, r * sin phi)


mod : Float -> Float -> Float
mod x y =
  x - y * (toFloat <| truncate <| x / y)
