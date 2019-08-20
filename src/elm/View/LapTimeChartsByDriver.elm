module View.LapTimeChartsByDriver exposing (viewLapTimeChartsByDriver)

import Analysis exposing (Analysis, Driver, History, Lap)
import Axis exposing (tickCount, tickSizeInner, tickSizeOuter)
import Html exposing (Html, li, p, text, ul)
import Path
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (class, style, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Transform(..))


w : Float
w =
    250


h : Float
h =
    100


padding : Float
padding =
    15


xScaleFromDomain : ( Float, Float ) -> ContinuousScale Float
xScaleFromDomain domain =
    Scale.linear ( padding, w - padding ) domain


yScaleFromDomain : ( Float, Float ) -> ContinuousScale Float
yScaleFromDomain domain =
    Scale.linear ( h - padding, padding ) domain



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

        lapTotal =
            analysis.summary.lapTotal |> toFloat

        fastestLap =
            histories
                |> List.map .laps
                |> Analysis.fastestLap

        xScale =
            xScaleFromDomain ( 0, lapTotal )

        yScale =
            yScaleFromDomain ( fastestLap.time, fastestLap.time * 1.2 )
    in
    ul [] (standings |> List.map (viewLapTimeChart xScale yScale))


viewLapTimeChart : ContinuousScale Float -> ContinuousScale Float -> History -> Html msg
viewLapTimeChart xScale yScale history =
    let
        xAxis =
            g [ class [ "x-axis" ], transform [ Translate 0 (h - padding) ] ]
                [ Axis.bottom [ tickCount 5, tickSizeInner 4, tickSizeOuter 4 ] xScale ]

        yAxis =
            g [ class [ "y-axis" ], transform [ Translate padding 0 ] ]
                [ Axis.left [ tickCount 2, tickSizeInner 3, tickSizeOuter 3 ] yScale ]
    in
    li []
        [ p [] [ text (history.carNumber ++ " " ++ history.driver.name) ]
        , svg [ viewBox 0 0 w h ]
            [ xAxis
            , yAxis
            , drawCurve xScale yScale history
            , g [] (history.laps |> List.map (viewLapData xScale yScale history.driver.teamColor))
            ]
        ]


viewLapData : ContinuousScale Float -> ContinuousScale Float -> String -> Lap -> Svg msg
viewLapData xScale yScale color lap =
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


drawCurve : ContinuousScale Float -> ContinuousScale Float -> History -> Svg msg
drawCurve xScale yScale history =
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
