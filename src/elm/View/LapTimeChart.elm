module View.LapTimeChart exposing (viewLapTimeChart)

import Analysis exposing (Analysis, Driver, History, Lap)
import Html exposing (Html)
import Path
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, svg, text_)
import TypedSvg.Attributes exposing (class, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)


w : Float
w =
    1000


h : Float
h =
    400


padding : Float
padding =
    50


xScale : ContinuousScale Float
xScale =
    Scale.linear ( padding, w - padding ) ( 0, 60 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - padding, padding ) ( 85000, 97500 )



-- VIEW


viewLapTimeChart : Analysis -> Html msg
viewLapTimeChart analysis =
    let
        drivers =
            analysis.summary.drivers

        histories =
            analysis.raceHistories

        standings =
            drivers
                |> List.map
                    (\driver ->
                        let
                            history =
                                histories
                                    |> List.filter (\d -> driver.carNumber == d.carNumber)
                                    |> List.head
                                    |> Maybe.withDefault (History "" (Driver "" "" "" "" "") [] [])
                        in
                        { driver = driver
                        , carNumber = history.carNumber
                        , laps = history.laps
                        , pitStops = history.pitStops
                        }
                    )
    in
    svg [ viewBox 0 0 w h, class [ "laptime-chart" ] ]
        (standings |> List.indexedMap viewLapHistory)


viewLapHistory : Int -> History -> Html msg
viewLapHistory i history =
    g [ class [ "history" ] ]
        [ text_ [ x 10, y (toFloat i * 20 + 15) ] [ Html.text history.carNumber ]
        , text_ [ x 35, y (toFloat i * 20 + 15) ] [ Html.text history.driver.name ]
        , g [] (history.laps |> List.map viewLapData)
        , drawCurve history.laps
        ]


viewLapData : Lap -> Svg msg
viewLapData lap =
    let
        dx =
            lap.lapCount |> Scale.convert xScale

        dy =
            lap.time |> Scale.convert yScale
    in
    g
        [ TypedSvg.Attributes.class [ "lap" ] ]
        [ circle [ cx dx, cy dy, r 2 ] []
        , text_ [ x dx, y dy ] [ Html.text (lap.lapCount |> String.fromFloat) ]
        ]


drawCurve : List Lap -> Svg msg
drawCurve laps =
    let
        scaleX x =
            x |> Scale.convert xScale

        scaleY y =
            y |> Scale.convert yScale

        points =
            laps |> List.map (\lap -> ( scaleX lap.lapCount, scaleY lap.time ))
    in
    points
        |> List.map Just
        |> Shape.line Shape.linearCurve
        |> (\path -> Path.element path [])
