module Math exposing (..)


type alias Polygon =
  { inclusion : List Point
  , exclusion : List Point
  }


type alias Point = (Float, Float)


