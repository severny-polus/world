module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Element exposing (centerX, centerY, el, fill, layout, width)
import GeoJson exposing (GeoJson)
import Html exposing (Html)
import Json.Decode as Json
import Projection exposing (Polygon, Projection)
import Task
import Time


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { projection : Projection
  , size : Maybe Projection.Size
  , data : Maybe Projection.Data
  , anglePerSecond : Float
  , angle : Float
  }


type Msg
  = Scene { width : Float, height : Float }
  | Resize Int Int
  | Frame Float
  | ProjectionMsg Projection.Msg


init : String -> (Model, Cmd Msg)
init data =
  ( { projection = Projection.disc
    , size = Nothing
    , data = Maybe.andThen GeoJson.toPolygons
      <| Result.toMaybe
      <| Json.decodeString GeoJson.decoder data
    , anglePerSecond = 1
    , angle = 0
    }
  , Task.perform (.scene >> Scene) Browser.Dom.getViewport
  )


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
      { model | size = Just (scene.width, scene.height) }

    Resize width height ->
      { model | size = Just (toFloat width, toFloat height) }

    Frame ms ->
      { model | angle = model.angle + model.anglePerSecond * ms / 1000 }

    _ ->
      model
  , Cmd.none
  )


view : Model -> Html Msg
view model =
  layout []
    <| case Maybe.map2 Tuple.pair model.size model.data of
      Just (size, data) ->
        el
          [ centerX
          , centerY
          ]
          <| Element.html
          <| Html.map ProjectionMsg
          <| Projection.view model.projection size data model.angle

      Nothing ->
        Element.none