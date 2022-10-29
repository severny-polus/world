module Main exposing (main)

import Browser
import Browser.Dom
import Element exposing (centerX, el, fill, layout, width)
import GeoJson exposing (GeoJson)
import Html exposing (Html)
import Json.Decode as Json
import Projection exposing (Polygon, Projection)
import Task


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
  }


type Msg
  = Size { width : Float, height : Float }
  | ProjectionMsg Projection.Msg


init : String -> (Model, Cmd Msg)
init data =
  ( { projection = Projection.disc
    , size = Nothing
    , data = Maybe.andThen GeoJson.toPolygons
      <| Result.toMaybe
      <| Json.decodeString GeoJson.decoder data
    }
  , Task.perform (.scene >> Size) Browser.Dom.getViewport
  )


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Size size ->
      ({ model | size = Just (size.width, size.height) }, Cmd.none)

    _ ->
      (model, Cmd.none)


view : Model -> Html Msg
view model =
  layout []
    <| case Maybe.map2 Tuple.pair model.size model.data of
      Just (size, data) ->
        el
          [ centerX
          ]
          <| Element.html <| Html.map ProjectionMsg <| Projection.view model.projection size data

      Nothing ->
        Element.none