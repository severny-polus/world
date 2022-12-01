module GeoJson exposing (..)


import Json.Decode as Json
import Math exposing (Point, Polygon)
import Maybe.Extra


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
    exterior =
      List.head feature.geometry.coordinates

    interiors =
      List.tail feature.geometry.coordinates

    toRing =
      List.map toPoint
        >> Maybe.Extra.combine
  in
  Maybe.map2 Polygon
    (Maybe.andThen toRing exterior)
    (Maybe.andThen (List.map toRing >> Maybe.Extra.combine) interiors)


toPoint : List Float -> Maybe Point
toPoint floats =
  case floats of
    [x, y] ->
      Just (x, y)

    _ ->
      Nothing
