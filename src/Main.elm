module Main exposing (main)

import Browser
import Element exposing (centerX, centerY, el, fill, height, layout, width)
import GeoJson exposing (GeoJson)
import Html exposing (Html)
import Json.Decode as Json
import Projection exposing (Projection)


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { projection : Projection
  }


type alias Flags =
  { landWithoutAntarctica : String
  , landAntarctica : String
  }


type Msg
  = ProjectionMsg Projection.Msg


init : Flags -> (Model, Cmd Msg)
init flags =
  let
    (projection, cmd) =
      Projection.init
        (parseGeodata flags.landWithoutAntarctica)
        (parseGeodata flags.landAntarctica)
  in
  ( { projection = projection
    }
  , Cmd.map ProjectionMsg cmd
  )


parseGeodata : String -> Projection.Geodata
parseGeodata string =
  Json.decodeString GeoJson.decoder string
    |> Result.toMaybe
    |> Maybe.andThen GeoJson.toPolygons
    |> Maybe.withDefault []


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch <| List.concat
    [ [ Projection.subscriptions model.projection
        |> Sub.map ProjectionMsg
      ]
    ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ProjectionMsg projectionMsg ->
      let
        (projection, cmd) =
          Projection.update projectionMsg model.projection
      in
      ( { model | projection = projection }
      , Cmd.map ProjectionMsg cmd
      )


view : Model -> Html Msg
view model =
  layout
    [ width fill
    , height fill
    ]
    <| el
      [ centerX
      , centerY
      , width fill
      , height fill
      ]
      <| Element.html
      <| Html.map ProjectionMsg
      <| Projection.view model.projection
