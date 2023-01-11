module Main exposing (main)

import Browser
import Element exposing (Color, centerX, centerY, el, fill, height, layout, rgb255, toRgb, width)
import Element.Background as Background
import GeoJson
import Geodata exposing (Geodata, Msg(..))
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


geodataResource : String -> String
geodataResource name =
  config.prefix ++ "/geodata/" ++ name ++ ".min.geo.json"


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


type alias Model =
  { geodata : Geodata
  , projection : Projection
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
      Projection.init <| toRgb colorBackground
  in
  ( { geodata = Geodata.init
    , projection = projection
    , requestsLeft = List.length requests
    , error = Nothing
    }
  , Cmd.batch
    [ Cmd.map GeodataResult <| Cmd.batch requests
    , Cmd.map ProjectionMsg cmd
    ]
  )


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
          let
            modelNew =
              { model
              | geodata = Geodata.update geodataMsg model.geodata
              , requestsLeft = model.requestsLeft - 1
              }
          in
          if modelNew.requestsLeft == 0 then
            update
              (ProjectionMsg <| Projection.SetGeodata modelNew.geodata)
              modelNew
          else
            ( modelNew, Cmd.none )

        Err error ->
          ( { model
            | error = Just error
            }
          , Cmd.none
          )

    ProjectionMsg projectionMsg ->
      let
        (projection, cmd) =
          Projection.update projectionMsg model.projection
      in
      ( { model
        | projection = projection
        }
      , Cmd.map ProjectionMsg cmd
      )


colorBackground : Color
colorBackground =
  rgb255 36 36 36


view : Model -> Html Msg
view model =
  layout
    [ width fill
    , height fill
    , Background.color colorBackground
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
