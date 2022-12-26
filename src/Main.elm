module Main exposing (main)

import Browser
import Element exposing (centerX, centerY, el, fill, height, layout, width)
import GeoJson
import Geodata exposing (Msg(..))
import Html exposing (Html)
import Http
import Projection exposing (Projection)


config =
  { prefix = "https://raw.githubusercontent.com/severny-polus/world/master"
  , geodata =
    { landAntarctica = "ne_110m_land_antarctica"
    , landWithoutAntarctica = "ne_110m_land_no_antarctica"
    , rivers = "ne_110m_rivers_lake_centerlines"
    , lakes = "ne_110m_lakes"
    }
  }


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { projection : Projection
  , requestsLeft : Int
  , error : Maybe Http.Error
  }


type Msg
  = GeodataResult (Result Http.Error Geodata.Msg)
  | ProjectionMsg Projection.Msg


init : () -> (Model, Cmd Msg)
init _ =
  let
    requests =
      [ Http.get
        { url = geodataResource config.geodata.landWithoutAntarctica
        , expect = Http.expectJson (Result.map LandWithoutAntarctica) GeoJson.polygons
        }
      , Http.get
        { url = geodataResource config.geodata.landAntarctica
        , expect = Http.expectJson (Result.map LandAntarctica) GeoJson.polygons
        }
      , Http.get
        { url = geodataResource config.geodata.lakes
        , expect = Http.expectJson (Result.map Lakes) GeoJson.polygons
        }
      , Http.get
        { url = geodataResource config.geodata.rivers
        , expect = Http.expectJson (Result.map Rivers) GeoJson.lines
        }
      ]

    (projection, cmd) =
      Projection.init
  in
  ( { projection = projection
    , requestsLeft = List.length requests
    , error = Nothing
    }
  , Cmd.batch
    [ Cmd.map GeodataResult <| Cmd.batch requests
    , Cmd.map ProjectionMsg cmd
    ]
  )


geodataResource : String -> String
geodataResource name =
  config.prefix ++ "/geodata/" ++ name ++ ".min.geo.json"


subscriptions : Model -> Sub Msg
subscriptions model =
  Projection.subscriptions model.projection
    |> Sub.map ProjectionMsg


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GeodataResult result ->
      case result of
        Ok geodataMsg ->
          update (ProjectionMsg <| Projection.GeodataMsg geodataMsg) model

        Err error ->
          ( { model | error = Just (Debug.log "error" error) }
          , Cmd.none
          )

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
