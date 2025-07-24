module Config exposing (..)

import GeoJson
import Geodata exposing (Msg(..))
import Json.Decode


config : Config
config =
    { url = "https://raw.githubusercontent.com/severny-polus/world/master"
    , geodata =
        [ { name = "ne_110m_land_antarctica"
          , decoder = Json.Decode.map LandAntarctica GeoJson.polygons
          }
        , { name = "ne_110m_land_no_antarctica"
          , decoder = Json.Decode.map LandWithoutAntarctica GeoJson.polygons
          }
        , { name = "ne_110m_rivers_lake_centerlines"
          , decoder = Json.Decode.map Rivers GeoJson.lines
          }
        , { name = "ne_110m_lakes"
          , decoder = Json.Decode.map Lakes GeoJson.polygons
          }
        , { name = "ne_110m_populated_places_simple"
          , decoder = Json.Decode.map Cities GeoJson.points
          }
        ]
    }


type alias Config =
    { url : String
    , geodata : Geodata
    }


type alias Geodata =
    List GeodataResource


type alias GeodataResource =
    { name : String
    , decoder : Json.Decode.Decoder Geodata.Msg
    }
