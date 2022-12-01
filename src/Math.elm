module Math exposing (..)


type alias Polygon =
  { exterior : Ring
  , interiors : List Ring
  }


type alias Ring =
  List Point


type alias Point = (Float, Float)
