module Geodata exposing (..)

import Math exposing (Line, Point, Polygon)


type alias Geodata =
    { landWithoutAntarctica : List Polygon
    , landAntarctica : List Polygon
    , land : List Polygon
    , lakes : List Polygon
    , rivers : List Line
    , cities : List Point
    }


type Msg
    = LandWithoutAntarctica (List Polygon)
    | LandAntarctica (List Polygon)
    | Land (List Polygon)
    | Lakes (List Polygon)
    | Rivers (List Line)
    | Cities (List Point)


init : Geodata
init =
    { landWithoutAntarctica = []
    , landAntarctica = []
    , land = []
    , rivers = []
    , lakes = []
    , cities = []
    }


update : Msg -> Geodata -> Geodata
update msg geodata =
    case msg of
        LandWithoutAntarctica polygons ->
            { geodata | landWithoutAntarctica = polygons }

        LandAntarctica polygons ->
            { geodata | landAntarctica = polygons }

        Land polygons ->
            { geodata | land = polygons }

        Lakes polygons ->
            { geodata | lakes = polygons }

        Rivers lines ->
            { geodata | rivers = lines }

        Cities cities ->
            { geodata | cities = cities }
