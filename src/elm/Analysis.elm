module Analysis exposing (Analysis, Driver, History, Lap, analysisDecoder, fastestLap, getAnalysis, lapDecoder, lapsWithElapsed, personalBest)

import Http
import Json.Decode as Decode exposing (at, float, int, string)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import List.Extra



-- TYPE


type alias Analysis =
    { summary : RaceSummary
    , raceHistories : List History
    }


type alias RaceSummary =
    { eventName : String
    , seasonName : String
    , lapTotal : Int
    , drivers : List Driver
    }


type alias Driver =
    { name : String
    , carNumber : String
    , shortCode : String
    , teamName : String
    , teamColor : String
    }


type alias History =
    { carNumber : String
    , driver : Driver
    , laps : List Lap
    , pitStops : List Int
    , fastestLap : Lap
    }


type alias Lap =
    { lapCount : Int
    , time : Float
    , elapsed : Float
    }



-- DECODER


analysisDecoder : Decode.Decoder Analysis
analysisDecoder =
    Decode.succeed Analysis
        |> custom summaryDecoder
        |> required "raceHistory" (Decode.list historyDecoder)


summaryDecoder : Decode.Decoder RaceSummary
summaryDecoder =
    Decode.succeed RaceSummary
        |> custom (at [ "event", "name" ] string)
        |> custom (at [ "season", "name" ] string)
        |> required "lapTotal" int
        |> required "entries" (Decode.list driverDecoder)


driverDecoder : Decode.Decoder Driver
driverDecoder =
    Decode.succeed Driver
        |> required "driverName" string
        |> required "car" string
        |> required "driverShortCode" string
        |> required "teamName" string
        |> required "teamColour" string


historyDecoder : Decode.Decoder History
historyDecoder =
    Decode.succeed toHistory
        |> required "car" string
        |> required "lapTime" (Decode.list lapDecoder)
        |> optional "pit" (Decode.list int) []


toHistory : String -> List Lap -> List Int -> History
toHistory carNumber laps pitStops =
    { carNumber = carNumber
    , driver = Driver "" "" "" "" ""
    , laps = laps |> lapsWithElapsed
    , pitStops = pitStops
    , fastestLap = laps |> personalBest
    }


lapDecoder : Decode.Decoder Lap
lapDecoder =
    Decode.succeed Lap
        |> required "lap" int
        |> required "time" float
        |> hardcoded 0



-- API


getAnalysis : (Result Http.Error Analysis -> msg) -> String -> Cmd msg
getAnalysis toMsg raceName =
    Http.get
        { url = "https://y047aka.github.io/MotorSportsData/analysis/F1/2019/" ++ raceName ++ "/raceHistoryAnalytics.json"
        , expect = Http.expectJson toMsg analysisDecoder
        }


lapsWithElapsed : List Lap -> List Lap
lapsWithElapsed laps =
    let
        lapWithElapsed : Int -> Lap -> Lap
        lapWithElapsed i lap =
            { lapCount = lap.lapCount
            , time = lap.time
            , elapsed =
                laps
                    |> List.Extra.takeWhile (\d -> i + 1 >= d.lapCount)
                    |> List.map .time
                    |> List.sum
            }
    in
    laps |> List.indexedMap lapWithElapsed


personalBest : List Lap -> Lap
personalBest =
    List.sortBy .time
        >> List.head
        >> Maybe.withDefault (Lap 0 0 0)


fastestLap : List (List Lap) -> Lap
fastestLap =
    List.map personalBest
        >> List.sortBy .time
        >> List.head
        >> Maybe.withDefault (Lap 0 0 0)
