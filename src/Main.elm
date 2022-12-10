module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (centerX, centerY, el, layout, width)
import GeoJson exposing (GeoJson)
import Html exposing (Html)
import Json.Decode as Json
import Projection exposing (Projection)
import Task


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { projection : Maybe Projection
  , geojson : String
  , anglePerSecond : Float
  }


type Msg
  = Scene { width : Float, height : Float }
  | Resize Int Int
  | Frame Float
  | ProjectionMsg Projection.Msg


init : String -> (Model, Cmd Msg)
init geojson =
  ( { projection = Nothing
    , geojson = geojson
    , anglePerSecond = 2 * pi / 360
    }
  , Task.perform (.scene >> Scene) Browser.Dom.getViewport
  )


getGeodata : String -> Maybe Projection.Geodata
getGeodata string =
  Json.decodeString GeoJson.decoder string
    |> Result.toMaybe
    |> Maybe.andThen GeoJson.toPolygons


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ Browser.Events.onAnimationFrameDelta Frame
    , Browser.Events.onResize Resize
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  ( case msg of
    Scene scene ->
      { model
      | projection =
        Maybe.map
          (Projection.init Projection.disc (scene.width, scene.height))
          <| getGeodata model.geojson
      }

    Resize width height ->
      { model
      | projection =
        Maybe.map
          (Projection.update <| Projection.Size (toFloat width, toFloat height))
          model.projection
      }

    Frame ms ->
      { model
      | projection =
        Maybe.map
          (Projection.update <| Projection.AngleChange <| model.anglePerSecond * ms / 1000)
          model.projection
      }

    _ ->
      model
  , Cmd.none
  )


view : Model -> Html Msg
view model =
  layout []
    <| case model.projection of
      Just projection ->
        el
          [ centerX
          , centerY
          ]
          <| Element.html
          <| Html.map ProjectionMsg
          <| Projection.view projection

      Nothing ->
        Element.none
