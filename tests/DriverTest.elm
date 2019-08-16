module DriverTest exposing (all)

import Driver exposing (carNumberToDriverName)
import Expect
import Test exposing (Test, describe, test)



{-
   - [ ] 文字列が参戦ドライバーの番号の場合
       - [x] 文字列：33を渡すと文字列：Max Verstappenを返す
       - [x] 文字列：23を渡すと文字列：Alexander Albonを返す

   - [ ] 文字列が参戦ドライバーの番号以外の場合
       - [x] 文字列：00を渡すと空白を返す
       - [x] 文字列：空白を渡すと空白を返す
-}


all : Test
all =
    describe "ドライバー名の取得"
        [ describe "文字列が参戦ドライバーの番号の場合"
            [ test "文字列：33を渡すと文字列：Max Verstappenを返す" <|
                \() ->
                    "33"
                        |> carNumberToDriverName
                        |> Expect.equal "Max Verstappen"
            , test "文字列：23を渡すと文字列：Alexander Albonを返す" <|
                \() ->
                    "23"
                        |> carNumberToDriverName
                        |> Expect.equal "Alexander Albon"
            ]
        , describe "文字列が参戦ドライバーの番号以外の場合"
            [ test "文字列：00を渡すと空白を返す" <|
                \() ->
                    "00"
                        |> carNumberToDriverName
                        |> Expect.equal ""
            , test "文字列：空白を渡すと空白を返す" <|
                \() ->
                    ""
                        |> carNumberToDriverName
                        |> Expect.equal ""
            ]
        ]
