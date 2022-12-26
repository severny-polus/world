module Projection exposing (..)


import Browser.Dom
import Browser.Events
import Color exposing (Color, rgb255)
import Geodata exposing (Geodata)
import Html exposing (Html)
import Math exposing (Line, Point, Polygon, Ring)
import Task
import TypedSvg exposing (circle, polygon, polyline, rect, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, width)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias Projection =
  { size : Size
  , geodata : Maybe Geodata
  , angle : Float
  , angleVelocity : Float
  , angleAcceleration : Float
  , showParam : Float
  , showParamVelocity : Float
  , showParamAcceleration : Float
  , zoom : Float
  }


init : (Projection, Cmd Msg)
init =
  ( { size = (0, 0)
    , geodata = Nothing
    , angle = initAngle
    , angleVelocity = 0
    , angleAcceleration = 0
    , showParam = 0
    , showParamVelocity = 0
    , showParamAcceleration = 0
    , zoom = 1
    }
  , getElement
  )


type alias Size =
  (Float, Float)


type Msg
  = Resize Int Int
  | Element (Maybe Browser.Dom.Element)
  | SetGeodata Geodata
  | TimeDelta Float


subscriptions : Projection -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onResize Resize
    , Browser.Events.onAnimationFrameDelta TimeDelta
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

    SetGeodata geodata ->
      ( { projection
        | geodata = Just geodata
        , showParamAcceleration = maxShowParamAcceleration
        }
      , Cmd.none
      )

    TimeDelta dtMs ->
      let
        dt =
          dtMs / 1000
      in
      ( if projection.showParamAcceleration == 0 then
          { projection
          | angle = projection.angle + projection.angleVelocity * dt
          , angleVelocity = projection.angleVelocity + projection.angleAcceleration * dt
          }

        else
          let
            showParam =
              projection.showParam + projection.showParamVelocity * dt
          in
          if showParam < maxShowParam then
            { projection
            | showParam = showParam
            , showParamVelocity = projection.showParamVelocity + projection.showParamAcceleration * dt
            , showParamAcceleration =
              if showParam < 0.5 then
                maxShowParamAcceleration

              else
                -maxShowParamAcceleration
            }

          else
            { projection
            | showParam = maxShowParam
            , showParamVelocity = 0
            , showParamAcceleration = 0
            }
      , Cmd.none
      )


getElement : Cmd Msg
getElement =
  Browser.Dom.getElement "projection"
    |> Task.attempt (Result.toMaybe >> Element)


initAngle : Float
initAngle =
  pi + degrees 56


initAngleVelocity : Float
initAngleVelocity =
  pi / 12 / 60


maxShowParam : Float
maxShowParam =
  1


maxShowParamAcceleration : Float
maxShowParamAcceleration =
  2


view : Projection -> Html Msg
view projection =
  let
    (w, h) =
      projection.size

    a =
      projection.showParam * min w h

    scale (x, y) =
      (w / 2 + x * a / 2, h / 2 - y * a / 2)

    rotate (phi, theta) =
      (phi + projection.angle + pi / 3 * (1 - projection.showParam), theta)

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
    <| case projection.geodata of
      Just geodata ->
        List.concat
          [ [ circle
              [ InPx.cx <| w / 2
              , InPx.cy <| h / 2
              , InPx.r <| a / 2 * projection.zoom
              , fill <| Paint colorLand
              ]
              []
            ]
          , List.concatMap waterWater geodata.landAntarctica
          , List.concatMap landWater geodata.landWithoutAntarctica
          , List.map river geodata.rivers
          , List.concatMap waterLand geodata.lakes
          , [ rect
              [ InPx.x 0
              , InPx.y 0
              , InPx.width w
              , InPx.height h
              , fill <| Paint <| Color.rgba 1 1 1 (1 - projection.showParam)
              ]
              []
            ]
          ]

      Nothing ->
        []



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
