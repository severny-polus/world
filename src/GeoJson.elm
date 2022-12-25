module GeoJson exposing (..)


import Json.Decode as Json
import Math
import Maybe.Extra


type alias GeoJson =
  { features : List Feature
  }


type alias Feature =
  { geometry : Geometry
  }


type Geometry
  = Polygon Polygon
  | LineString Line


type alias Polygon =
  List Ring


type alias Ring =
  List Point


type alias Line =
  List Point


type alias Point =
  List Float


decoder : Json.Decoder GeoJson
decoder =
  Json.map GeoJson
    <| Json.field "features"
    <| Json.list
    <| Json.map Feature
    <| Json.field "geometry"
    <| Json.field "coordinates"
    <| Json.oneOf
      [ Json.map Polygon
        <| Json.list
        <| Json.list
        <| Json.list
        <| Json.float
      , Json.map LineString
        <| Json.list
        <| Json.list
        <| Json.float
      ]


getPolygons : GeoJson -> List Math.Polygon
getPolygons =
  .features
    >> List.filterMap toPolygon


getLines : GeoJson -> List Math.Line
getLines =
  .features
    >> List.filterMap toLine


toPolygon : Feature -> Maybe Math.Polygon
toPolygon feature =
  case feature.geometry of
    Polygon polygon ->
      let
        exterior =
          List.head polygon

        interiors =
          List.tail polygon

        toRing =
          List.map toPoint
            >> Maybe.Extra.combine
      in
      Maybe.map2 Math.Polygon
        (Maybe.andThen toRing exterior)
        (Maybe.andThen (List.map toRing >> Maybe.Extra.combine) interiors)

    _ ->
      Nothing


toLine : Feature -> Maybe Math.Line
toLine feature =
  case feature.geometry of
    LineString line ->
      List.map toPoint line
        |> Maybe.Extra.combine

    _ ->
      Nothing


toPoint : List Float -> Maybe Math.Point
toPoint floats =
  case floats of
    [x, y] ->
      Just (x, y)

    _ ->
      Nothing
