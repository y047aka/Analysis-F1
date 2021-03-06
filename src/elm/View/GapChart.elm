module View.GapChart exposing (viewGapChart)

import Analysis exposing (Analysis, Driver, History, Lap)
import Axis
import Html exposing (Html)
import Path
import Scale exposing (ContinuousScale)
import Shape
import TypedSvg exposing (circle, g, svg, text_)
import TypedSvg.Attributes exposing (class, style, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Transform(..))


w : Float
w =
    1000


h : Float
h =
    400


padding : { top : Float, left : Float, bottom : Float, right : Float }
padding =
    { top = 20, left = 60, bottom = 30, right = 20 }


xScaleFromDomain : ( Float, Float ) -> ContinuousScale Float
xScaleFromDomain domain =
    Scale.linear ( padding.left, w - padding.right ) domain


yScaleFromDomain : ( Float, Float ) -> ContinuousScale Float
yScaleFromDomain domain =
    Scale.linear ( h - padding.bottom, padding.top ) domain



-- VIEW


viewGapChart : Analysis -> Html msg
viewGapChart analysis =
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
                                    |> Maybe.withDefault (History "" (Driver "" "" "" "" "") [] [] (Lap 0 0 0))
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
            yScaleFromDomain ( fastestLap.time * 2.5, 0 )

        xAxis =
            g [ class [ "x-axis" ], transform [ Translate 0 (h - padding.bottom) ] ]
                [ Axis.bottom [] xScale ]

        yAxis =
            g [ class [ "y-axis" ], transform [ Translate padding.left 0 ] ]
                [ Axis.left [] yScale ]

        lapHistories =
            g []
                (standings |> List.indexedMap (viewLapHistory xScale yScale fastestLap))
    in
    svg [ viewBox 0 0 w h ]
        [ xAxis
        , yAxis
        , lapHistories
        ]


viewLapHistory : ContinuousScale Float -> ContinuousScale Float -> Lap -> Int -> History -> Html msg
viewLapHistory xScale yScale fastestLap i history =
    g [ class [ "history" ] ]
        [ --   text_ [ x 10, y (toFloat i * 20 + 15) ] [ Html.text history.carNumber ]
          -- , text_ [ x 35, y (toFloat i * 20 + 15) ] [ Html.text history.driver.name ]
          drawCurve xScale yScale fastestLap history
        , g [] (history.laps |> List.map (viewLapData xScale yScale fastestLap history.driver.teamColor))
        ]


viewLapData : ContinuousScale Float -> ContinuousScale Float -> Lap -> String -> Lap -> Svg msg
viewLapData xScale yScale fastestLap color lap =
    let
        dx =
            lap.lapCount |> toFloat |> Scale.convert xScale

        dy =
            (lap.elapsed - ((lap.lapCount |> toFloat) * fastestLap.time * 1.02)) |> Scale.convert yScale

        colorSetting =
            "fill: " ++ color
    in
    g
        [ TypedSvg.Attributes.class [ "lap" ] ]
        [ circle [ cx dx, cy dy, r 2, style colorSetting ] []
        , text_ [ x dx, y dy ] [ Html.text (lap.lapCount |> String.fromInt) ]
        ]


drawCurve : ContinuousScale Float -> ContinuousScale Float -> Lap -> History -> Svg msg
drawCurve xScale yScale fastestLap history =
    let
        scaleX x =
            x |> toFloat |> Scale.convert xScale

        scaleY y =
            y |> Scale.convert yScale

        points =
            history.laps |> List.map (\lap -> ( scaleX lap.lapCount, scaleY (lap.elapsed - ((lap.lapCount |> toFloat) * fastestLap.time * 1.02)) ))

        colorSetting =
            "stroke: " ++ history.driver.teamColor
    in
    points
        |> List.map Just
        |> Shape.line Shape.linearCurve
        |> (\path -> Path.element path [ style colorSetting ])
