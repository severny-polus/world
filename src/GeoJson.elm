module GeoJson exposing (..)


import Json.Decode as Json
import Math exposing (Line, Point, Polygon)


polygons : Json.Decoder (List Polygon)
polygons =
  Json.field "features"
    <| Json.map (List.filterMap toPolygon)
    <| Json.list
    <| Json.field "geometry"
    <| Json.field "coordinates"
    <| Json.list
    <| points


lines : Json.Decoder (List Line)
lines =
  Json.field "features"
    <| Json.list
    <| Json.field "geometry"
    <| Json.field "coordinates"
    <| points


points : Json.Decoder (List Point)
points =
  Json.map (List.filterMap toPoint)
    <| Json.list
    <| Json.list
    <| Json.float


toPolygon : List (List Point) -> Maybe Polygon
toPolygon polygon =
  Maybe.map2 Polygon
    (List.head polygon)
    (List.tail polygon)


toPoint : List Float -> Maybe Point
toPoint floats =
  case floats of
    [x, y] ->
      Just (x, y)

    _ ->
      Nothing
