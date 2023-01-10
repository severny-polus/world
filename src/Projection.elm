module Projection exposing (..)


import Animation exposing (Animation)
import Browser.Dom
import Browser.Events
import Color exposing (Color, rgb255)
import Geodata exposing (Geodata)
import Html exposing (Html)
import Math exposing (Line, Point, Polygon, Ring)
import Task
import TypedSvg exposing (circle, polygon, polyline, rect, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, width, x, y)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias Projection =
  { size : Size
  , geodata : Maybe Geodata
  , angle : Animation
  , load : Animation
  , zoom : Animation
  }


init : (Projection, Cmd Msg)
init =
  ( { size = (0, 0)
    , geodata = Nothing
    , angle = Animation.init Animation.harmonic 500 <| -pi / 2
    , load = Animation.init Animation.harmonic 500 0
    , zoom = Animation.init Animation.harmonic 500 0
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
          { projection
          | size =
            ( element.element.width
            , element.element.height
            )
          }

        Nothing ->
          projection
      , Cmd.none
      )

    SetGeodata geodata ->
      ( { projection
        | geodata = Just geodata
        , angle = Animation.to (-pi / 2 - degrees 38) projection.angle
        , load = Animation.to 1 projection.load
        , zoom = Animation.to 1 projection.zoom
        }
      , Cmd.none
      )

    TimeDelta dt ->
      ( { projection
        | angle = Animation.move dt projection.angle
        , load = Animation.move dt projection.load
        , zoom = Animation.move dt projection.zoom
        }
      , Cmd.none
      )


getElement : Cmd Msg
getElement =
  Browser.Dom.getElement "projection"
    |> Task.attempt (Result.toMaybe >> Element)


view : Projection -> Html Msg
view projection =
  svg
    [ id "projection"
    , width <| percent 100
    , height <| percent 100
    ]
    <| List.append
      [ rect
        [ x <| percent 0
        , y <| percent 0
        , width <| percent 100
        , height <| percent 100
        , fill <| Paint colorBackground
        ]
        []
      ]
      <| case projection.geodata of
        Just geodata ->
          let
            (w, h) =
              projection.size

            a =
              min w h

            scale (x, y) =
              (w / 2 + x * a / 2, h / 2 - y * a / 2)

            r beta =
              sin <| min beta (pi / 2)

            z =
              2 ^ projection.zoom.value

            transformTheta theta =
              let
                beta =
                  theta / z

                beta0 =
                  pi / z
              in
              0.5 * z * r beta / r beta0

            transform (phi, theta) =
              fromPolar
                (transformTheta theta)
                phi

            rotate (phi, theta) =
              (phi + projection.angle.value, theta)

            spherical (lng, lat) =
              (degrees lng, pi / 2 - degrees lat)

            project =
              spherical
                >> rotate
                >> transform
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

            earthCircle : Svg Msg
            earthCircle =
              circle
                [ InPx.cx <| w / 2
                , InPx.cy <| h / 2
                , InPx.r <| a / 2 * transformTheta pi
                , fill <| Paint colorLand
                ]
                []

            shade : Svg Msg
            shade = rect
              [ InPx.x 0
              , InPx.y 0
              , InPx.width w
              , InPx.height h
              , fill <| Paint <| withAlpha (1 - projection.load.value) colorBackground
              ]
              []

          in
          List.concat
            [ [ earthCircle ]
            , List.concatMap waterWater geodata.landAntarctica
            , List.concatMap landWater geodata.landWithoutAntarctica
            , List.map river geodata.rivers
            , List.concatMap waterLand geodata.lakes
            , [ shade ]
            ]

        Nothing ->
          []


fromPolar : Float -> Float -> Point
fromPolar rho phi =
  (rho * cos phi, rho * sin phi)


withAlpha : Float -> Color -> Color
withAlpha alpha color =
  let
    rgba =
      Color.toRgba color
  in
  Color.rgba rgba.red rgba.green rgba.blue alpha


colorBackground : Color
colorBackground =
  Color.rgb 0 0 0


colorLand : Color
colorLand =
  Color.rgb255 0 165 84


colorWater : Color
colorWater =
  Color.rgb255 46 122 197
