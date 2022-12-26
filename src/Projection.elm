module Projection exposing (..)


import Browser.Dom
import Browser.Events
import Color exposing (Color, rgb255)
import Geodata exposing (Geodata)
import Html exposing (Html)
import Math exposing (Line, Point, Polygon, Ring)
import Task
import TypedSvg exposing (circle, polygon, polyline, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, width)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias Projection =
  { size : Size
  , geodata : Geodata
  , angle : Float
  , angleVelocity : Float
  , angleAcceleration : Float
  , zoom : Float
  }


type alias Size =
  (Float, Float)


type Msg
  = Resize Int Int
  | Element (Maybe Browser.Dom.Element)
  | GeodataMsg Geodata.Msg
  | TimeDelta Float


init : (Projection, Cmd Msg)
init =
  let
    initAngle =
      pi + degrees 56
  in
  ( { size = (0, 0)
    , geodata = Geodata.init
    , angle = initAngle
    , angleVelocity = pi / 12 / 60
    , angleAcceleration = 0
    , zoom = 1
    }
  , getElement
  )


subscriptions : Projection -> Sub Msg
subscriptions _ =
  Sub.batch <| List.concat
    [ [ Browser.Events.onResize Resize
      , Browser.Events.onAnimationFrameDelta TimeDelta
      ]
    ]


update : Msg -> Projection -> (Projection, Cmd Msg)
update msg projection =
  case msg of
    Resize _ _ ->
      ( projection, getElement )

    Element maybeElement ->
      ( case maybeElement of
        Just element ->
          { projection | size = (element.element.width, element.element.height) }

        Nothing ->
          projection
      , Cmd.none
      )

    GeodataMsg geodataMsg ->
      ( { projection | geodata = Geodata.update geodataMsg projection.geodata }
      , Cmd.none
      )

    TimeDelta dt ->
      ( { projection
        | angle = projection.angle + projection.angleVelocity * dt / 1000
        , angleVelocity = projection.angleVelocity + projection.angleAcceleration * dt / 1000
        }
      , Cmd.none
      )


getElement : Cmd Msg
getElement =
  Browser.Dom.getElement "projection"
    |> Task.attempt (Result.toMaybe >> Element)


view : Projection -> Html Msg
view projection =
  let
    (w, h) =
      projection.size

    a =
      min w h

    scale (x, y) =
      (w / 2 + x * a / 2, h / 2 - y * a / 2)

    rotate (phi, theta) =
      (phi + projection.angle, theta)

    spherical (lng, lat) =
      (degrees lng, pi / 2 - degrees lat)

    project =
      spherical
        >> rotate
        >> transform projection.zoom
        >> scale

    land : Ring -> Svg Msg
    land ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint colorLand
        , InPx.strokeWidth 1
        ]
        []

    water : Ring -> Svg Msg
    water ring =
      polygon
        [ points <| List.map project ring
        , stroke <| Paint Color.black
        , fill <| Paint colorWater
        , InPx.strokeWidth 1
        ]
        []

    landWater : Polygon -> List (Svg Msg)
    landWater pol =
      List.concat
        [ [ land pol.exterior ]
        , List.map water pol.interiors
        ]

    waterWater : Polygon -> List (Svg Msg)
    waterWater pol =
      List.concat
        [ [ water pol.exterior ]
        , List.map water pol.interiors
        ]

    waterLand : Polygon -> List (Svg Msg)
    waterLand pol =
      List.concat
        [ [ water pol.exterior ]
        , List.map land pol.interiors
        ]

    river : Line -> Svg Msg
    river line =
      polyline
        [ points <| List.map project line
        , stroke <| Paint Color.black
        , fill PaintNone
        ]
        []

  in
  svg
    [ id "projection"
    , width <| percent 100
    , height <| percent 100
    ]
    <| List.concat
      [ [ circle
          [ InPx.cx <| w / 2
          , InPx.cy <| h / 2
          , InPx.r <| a / 2 * projection.zoom
          , fill <| Paint colorLand
          ]
          []
        ]
      , List.concatMap waterWater projection.geodata.landAntarctica
      , List.concatMap landWater projection.geodata.landWithoutAntarctica
      , List.map river projection.geodata.rivers
      , List.concatMap waterLand projection.geodata.lakes
      ]


r : Float -> Float
r theta =
  sin <| theta / 2


transform : Float -> Point -> Point
transform zoom (phi, theta) =
  fromPolar (zoom * r (theta / zoom) / r (pi / zoom)) phi


fromPolar : Float -> Float -> Point
fromPolar rho phi =
  (rho * cos phi, rho * sin phi)


colorLand : Color
colorLand =
  rgb255 0 165 84


colorWater : Color
colorWater =
  rgb255 46 122 197
