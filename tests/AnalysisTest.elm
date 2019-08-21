module AnalysisTest exposing (all)

import Analysis exposing (Lap, fastestLap, lapDecoder, personalBest)
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test, describe, test)



{-

   - [ ] analysisDecoder
       - [ ] Lap型のデコード
           - [x] 文字列：{ \"lap\": 20, \"time\": 90.003 } をデコードすると、Lap型を返す

       - [ ] History型のデコード

       - [ ] Analysis型のデコード

   - [ ] personalBest
       - [x] ドライバーの自己ベストラップを検索する
           - [x] (List Lap)：[ Lap 1 100.0, Lap 2 20.0, Lap 3 30.0 ] を渡すと、Lap型：Lap 2 20.0 を返す

   - [ ] fastestLap
       - [x] 各ドライバーの自己ベストから、最も早いラップを検索する
           - [x] (List (List Lap))：[ [ Lap 1 100.0 ], [ Lap 2 20.0 ], [ Lap 3 30.0 ] ] を渡すと、Lap型：Lap 2 20.0 を返す
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
