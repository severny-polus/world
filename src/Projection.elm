module Projection exposing (..)


import Color
import Html exposing (Html)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke, viewBox, width)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias Projection =
  { inscribe : Size -> Size
  , transform : Point -> Point
  }


type Msg
  = Nothing


type alias Size = (Float, Float)


view : Projection -> Size -> Data -> Float -> Html Msg
view projection size data angle =
  let
    (w, h) = projection.inscribe size

    scale (x, y) = ((1 + x) * w / 2, (1 - y) * h / 2)

    rotate (lng, lat) = (lng + angle, lat)

    inPol : Polygon -> Svg Msg
    inPol pol =
      polygon
        [ points <| List.map (scale << projection.transform << rotate) pol.inclusion
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
    <| List.map inPol data


type alias Data = List Polygon


type alias Polygon =
  { inclusion : List Point
  , exclusion : List Point
  }


type alias Point = (Float, Float)


-- TODO: fix
default : Projection
default =
  { inscribe = \(w, h) -> if w > 2 * h then (2 * h, h) else (w, w / 2)
  , transform = \(lng, lat) -> (lng / 180, lat / 90)
  }


disc : Projection
disc =
  { inscribe = \(w, h) -> (min w h, min w h)
  , transform = \(lng, lat) -> polar (cos <| degrees <| (90 + lat) / 2) (degrees <| lng + 180)
  }


polar : Float -> Float -> (Float, Float)
polar r phi =
  (r * cos phi, r * sin phi)


mod : Float -> Float -> Float
mod x y =
  x - y * (toFloat <| truncate <| x / y)
