module Math exposing (..)


type alias Point =
  (Float, Float)


type alias Line =
  List Point


type alias Ring =
  List Point


type alias Polygon =
  { exterior : Ring
  , interiors : List Ring
  }
