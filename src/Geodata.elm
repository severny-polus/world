module Geodata exposing (..)


import Math exposing (Line, Polygon)


type alias Geodata =
  { landWithoutAntarctica : List Polygon
  , landAntarctica : List Polygon
  , lakes : List Polygon
  , rivers : List Line
  }


type Msg
  = LandWithoutAntarctica (List Polygon)
  | LandAntarctica (List Polygon)
  | Lakes (List Polygon)
  | Rivers (List Line)


init : Geodata
init =
  { landWithoutAntarctica = []
  , landAntarctica = []
  , rivers = []
  , lakes = []
  }


update : Msg -> Geodata -> Geodata
update msg geodata =
  case msg of
    LandWithoutAntarctica polygons ->
      { geodata | landWithoutAntarctica = polygons }

    LandAntarctica polygons ->
      { geodata | landAntarctica = polygons }

    Lakes polygons ->
      { geodata | lakes = polygons}

    Rivers lines ->
      { geodata | rivers = lines}
