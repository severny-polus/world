module Projection exposing (..)


import Color
import Html exposing (Html)
import TypedSvg exposing (polygon, svg)
import TypedSvg.Attributes exposing (fill, points, stroke, viewBox, width)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias Projection =
  { size : Size -> Size
  , transform : Point -> Point
  }


type Msg
  = Nothing


type alias Size = (Float, Float)


view : Projection -> Size -> Data -> Html Msg
view projection size data =
  let
    (w, h) = projection.size size

    scale (x, y) = ((1 + x) * w / 2, (1 - y) * h / 2)

    inPol : Polygon -> Svg Msg
    inPol pol =
      polygon
        [ points <| List.map (scale << projection.transform) pol.inclusion
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


inclusion : List (Float, Float) -> Polygon
inclusion points =
  { inclusion = points
  , exclusion = []
  }


type alias Point = (Float, Float)


default : Projection
default =
  let transform (x, y) = (x / 180, y / 90)
  in
  { size = identity
  , transform = transform
  }


disc : Projection
disc =
  let
    size (w, h) = (min w h, min w h)
    transform (phi, theta) =
      let
        r = (90 + theta) / 2
        phi1 = phi + 180
      in
      ((cos (degrees r)) * cos (degrees phi1), (cos (degrees r)) * sin (degrees phi1))
  in
  { size = size
  , transform = transform
  }
