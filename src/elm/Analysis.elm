module Analysis exposing (Analysis, History, Lap, analysisDecoder, getAnalysis, lapDecoder)

import Driver exposing (carNumberToDriverName)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional, required)



-- TYPE


type alias Analysis =
    { summary : RaceSummary
    , raceHistories : List History
    }


type alias RaceSummary =
    { eventName : String
    , seasonName : String
    , lapTotal : Int
    }


type alias History =
    { carNumber : String
    , driverName : String
    , laps : List Lap
    , pitStops : List Int
    }


type alias Lap =
    { lapCount : Float
    , time : Float
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
        |> custom (Decode.at [ "event", "name" ] Decode.string)
        |> custom (Decode.at [ "season", "name" ] Decode.string)
        |> required "lapTotal" Decode.int


historyDecoder : Decode.Decoder History
historyDecoder =
    Decode.succeed toHistory
        |> required "car" Decode.string
        |> required "lapTime" (Decode.list lapDecoder)
        |> optional "pit" (Decode.list Decode.int) []


lapDecoder : Decode.Decoder Lap
lapDecoder =
    Decode.succeed Lap
        |> required "lap" Decode.float
        |> required "time" Decode.float


toHistory : String -> List Lap -> List Int -> History
toHistory carNumber laps pitStops =
    { carNumber = carNumber
    , driverName = carNumberToDriverName carNumber
    , laps = laps
    , pitStops = pitStops
    }



-- API


getAnalysis : (Result Http.Error Analysis -> msg) -> String -> Cmd msg
getAnalysis toMsg raceName =
    Http.get
        { url = "https://y047aka.github.io/MotorSportsData/analysis/F1/2019/" ++ raceName ++ "/raceHistoryAnalytics.json"
        , expect = Http.expectJson toMsg analysisDecoder
        }
