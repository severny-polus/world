module Main exposing (main)

import Browser
import Config exposing (Config)
import Element exposing (Color, fill, height, layout, rgb255, toRgb, width)
import Element.Background as Background
import Geodata exposing (Geodata, Msg(..))
import Html exposing (Html)
import Http
import Projection exposing (Projection)


config : Config
config =
    Config.config


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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        geodataRequest { name, decoder } =
            { url = config.url ++ "/geodata/" ++ name ++ ".min.geo.json"
            , expect = Http.expectJson GeodataResult decoder
            }

        requests =
            List.map (Http.get << geodataRequest) config.geodata

        ( projection, cmd ) =
            Projection.init <| toRgb colorBackground
    in
    ( { geodata = Geodata.init
      , projection = projection
      , requestsLeft = List.length requests
      , error = Nothing
      }
    , Cmd.batch
        [ Cmd.batch requests
        , Cmd.map ProjectionMsg cmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Projection.subscriptions model.projection
        |> Sub.map ProjectionMsg


update : Msg -> Model -> ( Model, Cmd Msg )
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
                ( projection, cmd ) =
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
    <|
        Element.html <|
            Html.map ProjectionMsg <|
                Projection.view model.projection
