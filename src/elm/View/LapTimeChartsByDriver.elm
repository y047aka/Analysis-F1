module View.LapTimeChartsByDriver exposing (viewLapTimeChartsByDriver)

import Analysis exposing (Analysis, Driver, History, Lap)
import Html exposing (Html, li, p, text, ul)
import Path
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, style, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r)
import TypedSvg.Core exposing (Svg)


w : Float
w =
    250


h : Float
h =
    100


padding : Float
padding =
    10


xScale : ContinuousScale Float
xScale =
    Scale.linear ( padding, w - padding ) ( 0, 60 )


yScale : ContinuousScale Float
yScale =
    Scale.linear ( h - padding, padding ) ( 85000, 97500 )



-- VIEW


viewLapTimeChartsByDriver : Analysis -> Html msg
viewLapTimeChartsByDriver analysis =
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
                                    |> Maybe.withDefault (History "" (Driver "" "" "" "" "") [] [] (Lap 0 0))
                        in
                        { driver = driver
                        , carNumber = history.carNumber
                        , laps = history.laps
                        , pitStops = history.pitStops
                        , fastestLap = history.fastestLap
                        }
                    )
    in
    ul [] (standings |> List.map viewLapTimeChart)


viewLapTimeChart : History -> Html msg
viewLapTimeChart history =
    li []
        [ p [] [ text (history.carNumber ++ " " ++ history.driver.name) ]
        , svg [ viewBox 0 0 w h ]
            [ drawCurve history
            , g [] (history.laps |> List.map (viewLapData history.driver.teamColor))
            ]
        ]


viewLapData : String -> Lap -> Svg msg
viewLapData color lap =
    let
        dx =
            lap.lapCount |> toFloat |> Scale.convert xScale

        dy =
            lap.time |> Scale.convert yScale

        colorSetting =
            "fill: " ++ color
    in
    g
        [ TypedSvg.Attributes.class [ "lap" ] ]
        [ circle [ cx dx, cy dy, r 1.5, style colorSetting ] []
        ]


drawCurve : History -> Svg msg
drawCurve history =
    let
        scaleX x =
            x |> toFloat |> Scale.convert xScale

        scaleY y =
            y |> Scale.convert yScale

        points =
            history.laps |> List.map (\lap -> ( scaleX lap.lapCount, scaleY lap.time ))

        colorSetting =
            "stroke: " ++ history.driver.teamColor
    in
    points
        |> List.map Just
        |> Shape.line Shape.linearCurve
        |> (\path -> Path.element path [ style colorSetting ])
