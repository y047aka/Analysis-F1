module AnalysisTest exposing (all)

import Analysis exposing (Lap, lapDecoder)
import Expect
import Json.Decode exposing (decodeString)
import Test exposing (Test, describe, test)



{-

   - [ ] Lap型のデコード
       - [x] 文字列：{ \"lap\": 20, \"time\": 90.003 } をデコードすると、Lap型を返す

   - [ ] Car型のデコード

   - [ ] Analysis型のデコード

-}


all : Test
all =
    describe "analysisDecoder"
        [ describe "Lap型のデコード"
            [ test "文字列：{ \"lap\": 20, \"time\": 90.003 } をデコードすると、Lap型を返す" <|
                \() ->
                    decodeString lapDecoder "{ \"lap\": 20, \"time\": 90.003 }"
                        |> (\result ->
                                case result of
                                    Ok lap ->
                                        Expect.equal (Lap 20 90.003) lap

                                    Err error ->
                                        Expect.equal (Lap 20 90.003) (Lap 0 0)
                           )
            ]
        ]
