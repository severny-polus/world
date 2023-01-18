module Projection exposing (..)


import Animation exposing (Animation)
import Browser.Dom
import Browser.Events
import Color exposing (Color)
import Geodata exposing (Geodata)
import Html exposing (Html)
import Math exposing (Line, Point, Polygon, Ring)
import Task
import TypedSvg exposing (circle, polygon, polyline, rect, svg)
import TypedSvg.Attributes exposing (fill, height, id, points, stroke, width, x, y)
import TypedSvg.Attributes.InPx as InPx
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Paint(..), percent)


type alias RGBA =
  { red : Float
  , green : Float
  , blue : Float
  , alpha : Float
  }


type alias Projection =
  { size : Size
  , colorBackground : Color
  , geodata : Maybe Geodata
  , angle : Animation
  , shade : Animation
  , zoom : Animation
  }


init : RGBA -> (Projection, Cmd Msg)
init colorBackground =
  ( { size = (0, 0)
    , colorBackground = Color.fromRgba colorBackground
    , geodata = Nothing
    , angle = Animation.init Animation.harmonic 1000 (-pi / 2)
      |> Animation.to (-pi / 2 - degrees 38)
    , shade = Animation.init Animation.harmonic 1000 1
      |> Animation.to 0
    , zoom = Animation.init Animation.harmonic 1000 0
      |> Animation.to 1
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
        , angle = Animation.run projection.angle
        , shade = Animation.run projection.shade
        , zoom = Animation.run projection.zoom
        }
      , Cmd.none
      )

    TimeDelta dt ->
      ( { projection
        | angle = Animation.step dt projection.angle
        , shade = Animation.step dt projection.shade
        , zoom = Animation.step dt projection.zoom
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
      [
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

            colorLand =
              Color.rgb255 0 165 84

            colorWater =
              Color.rgb255 46 122 197

            colorContour =
              Color.rgb255 0 0 0

            land : Ring -> Svg Msg
            land ring =
              polygon
                [ points <| List.map project ring
                , stroke <| Paint colorContour
                , fill <| Paint colorLand
                , InPx.strokeWidth 1
                ]
                []

            water : Ring -> Svg Msg
            water ring =
              polygon
                [ points <| List.map project ring
                , stroke <| Paint colorContour
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
                , stroke <| Paint colorContour
                , fill PaintNone
                ]
                []

            earthCircle : Maybe Color -> Maybe Color -> Svg Msg
            earthCircle strokeColor fillColor =
              circle
                [ InPx.cx <| w / 2
                , InPx.cy <| h / 2
                , InPx.r <| a / 2 * transformTheta pi
                , stroke
                  <| Maybe.withDefault PaintNone
                  <| Maybe.map Paint strokeColor
                , fill
                  <| Maybe.withDefault PaintNone
                  <| Maybe.map Paint fillColor
                ]
                []

            shade : Svg Msg
            shade = rect
              [ InPx.x 0
              , InPx.y 0
              , InPx.width w
              , InPx.height h
              , fill
                <| Paint
                <| withAlpha projection.shade.value projection.colorBackground
              ]
              []
          in
          List.concat
            [ [ earthCircle Nothing (Just colorLand)
              ]
            , List.concatMap waterWater geodata.landAntarctica
            , List.concatMap landWater geodata.landWithoutAntarctica
            , List.map river geodata.rivers
            , List.concatMap waterLand geodata.lakes
            , [ earthCircle (Just colorContour) Nothing
              , shade
              ]
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


colorTransparent : Color
colorTransparent =
  Color.rgba 0 0 0 0
