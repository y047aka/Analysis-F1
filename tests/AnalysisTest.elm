module AnalysisTest exposing (all)

import Analysis exposing (Lap, fastestLap, lapDecoder, lapsWithElapsed, personalBest)
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test, describe, test)



{-

   - [ ] analysisDecoder
       - [x] Lap型のデコード
           - [x] 文字列：{ \"lap\": 20, \"time\": 90.003, \"elapsed\": 0 } をデコードすると、Lap型を返す

       - [ ] History型のデコード

       - [ ] Analysis型のデコード

   - [ ] lapsWithElapsed
       - [x] ラップタイムの累積時間を計算する
           - [x] (List Lap)：[ Lap 1 100.0 0, Lap 2 20.0 0, Lap 3 30.0 0 ] を渡すと、(List Lap)：[ Lap 1 100.0 100.0, Lap 2 20.0 120.0, Lap 3 30.0 150.0 ] を返す

   - [ ] personalBest
       - [x] ドライバーの自己ベストラップを検索する
           - [x] (List Lap)：[ Lap 1 100.0　0, Lap 2 20.0 0, Lap 3 30.0 0 ] を渡すと、Lap型：Lap 2 20.0 を返す

   - [ ] fastestLap
       - [x] 各ドライバーの自己ベストから、最も早いラップを検索する
           - [x] (List (List Lap))：[ [ Lap 1 100.0 0 ], [ Lap 2 20.0 0 ], [ Lap 3 30.0 0 ] ] を渡すと、Lap型：Lap 2 20.0 0 を返す
-}


all : Test
all =
    describe "Analysis"
        [ describe "analysisDecoder"
            [ describe "Lap型のデコード"
                [ test "文字列：{ \"lap\": 20, \"time\": 90.003, \"elapsed\": 0 } をデコードすると、Lap型を返す" <|
                    \() ->
                        decodeString lapDecoder "{ \"lap\": 20, \"time\": 90.003, \"elapsed\": 0 }"
                            |> (\result ->
                                    case result of
                                        Ok lap ->
                                            Expect.equal (Lap 20 90.003 0) lap

                                        Err error ->
                                            Expect.equal (Lap 20 90.003 0) (Lap 0 0 0)
                               )
                ]
            ]
        , describe "lapsWithElapsed"
            [ describe "ラップタイムの累積時間を計算する"
                [ test "(List Lap)：[ Lap 1 100.0 0, Lap 2 20.0 0, Lap 3 30.0 0 ] を渡すと、(List Lap)：[ Lap 1 100.0 100.0, Lap 2 20.0 120.0, Lap 3 30.0 150.0 ] を返す" <|
                    \() ->
                        [ Lap 1 100.0 0, Lap 2 20.0 0, Lap 3 30.0 0 ]
                            |> lapsWithElapsed
                            |> Expect.equal [ Lap 1 100.0 100.0, Lap 2 20.0 120.0, Lap 3 30.0 150.0 ]
                ]
            ]
        , describe "personalBest"
            [ describe "ドライバーの自己ベストラップを検索する"
                [ test "(List Lap)：[ Lap 1 100.0 0, Lap 2 20.0 0, Lap 3 30.0 0 ] を渡すと、Lap型：Lap 2 20.0 0 を返す" <|
                    \() ->
                        [ Lap 1 100.0 0, Lap 2 20.0 0, Lap 3 30.0 0 ]
                            |> personalBest
                            |> Expect.equal (Lap 2 20.0 0)
                ]
            ]
        , describe "fastestLap"
            [ describe "各ドライバーの自己ベストから、最も早いラップを検索する"
                [ test "(List (List Lap))：[ [ Lap 1 100.0 0 ], [ Lap 2 20.0 0 ], [ Lap 3 30.0 0 ] ] を渡すと、Lap型：Lap 2 20.0 0 を返す" <|
                    \() ->
                        [ [ Lap 1 100.0 0 ], [ Lap 2 20.0 0 ], [ Lap 3 30.0 0 ] ]
                            |> fastestLap
                            |> Expect.equal (Lap 2 20.0 0)
                ]
            ]
        ]
