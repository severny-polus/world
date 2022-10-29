module GeoJson exposing (..)


import Json.Decode as Json
import Maybe.Extra
import Projection exposing (Point, Polygon)


type alias GeoJson =
  { features : List Feature
  }


type alias Feature =
  { geometry : Geometry
  }


type alias Geometry =
  { coordinates : List (List (List Float))
  }


decoder : Json.Decoder GeoJson
decoder =
  Json.map GeoJson
    <| Json.field "features"
    <| Json.list
    <| Json.map Feature
    <| Json.field "geometry"
    <| Json.map Geometry
    <| Json.field "coordinates"
    <| Json.list
    <| Json.list
    <| Json.list
    <| Json.float


toPolygons : GeoJson -> Maybe (List Polygon)
toPolygons data =
  Maybe.Extra.combine <| List.map toPolygon data.features


toPolygon : Feature -> Maybe Polygon
toPolygon feature =
  let
    coords = feature.geometry.coordinates
  in
  Maybe.map2 Polygon
    (Maybe.andThen (Maybe.Extra.combine << List.map toPoint) <| List.head coords)
    (Maybe.Extra.combine <| List.map toPoint <| Maybe.withDefault [] <| Maybe.andThen List.head <| List.tail coords)


toPoint : List Float -> Maybe Point
toPoint floats =
  Maybe.map2 Tuple.pair
    (List.head floats)
    (Maybe.andThen List.head <| List.tail floats)
