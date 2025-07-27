module Math exposing (..)

import Math.Vector2 exposing (Vec2, getX, getY)
import TypedSvg exposing (polygon)


type alias Point =
    Vec2


type alias Line =
    List Vec2


type alias Ring =
    List Vec2


toTuple : Vec2 -> ( Float, Float )
toTuple p =
    ( getX p, getY p )


isOuter : Ring -> Bool
isOuter polygon =
    {- Если многоугольник не лежит в Южном полушарии хотя бы наполовину, можно смело его отмести -}
    if List.length (List.filter (getY >> (>) 0) polygon) < List.length polygon // 2 then
        False

    else
        let
            {- сосчитать количество пересечений сторон многоугольника 180 меридианом -}
            countIntersections a poly cnt =
                case poly of
                    [] ->
                        cnt

                    b :: rem ->
                        let
                            aLng =
                                getX a

                            bLng =
                                getX b
                        in
                        if
                            (aLng - bLng > 180 && aLng > 0 && bLng < 0)
                                || (bLng - aLng > 180 && bLng > 0 && aLng < 0)
                        then
                            countIntersections b rem (cnt + 1)

                        else
                            countIntersections b rem cnt
        in
        case polygon of
            [] ->
                False

            a :: poly ->
                (countIntersections a (poly ++ [ a ]) 0 |> modBy 2) /= 0


type alias Polygon =
    { exterior : Ring
    , interiors : List Ring
    }
