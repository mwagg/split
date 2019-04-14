module MoneyTest exposing (suite)

import Expect
import Fuzz
import Money
import Test exposing (..)


positiveIntFuzzer : Fuzz.Fuzzer Int
positiveIntFuzzer =
    Fuzz.int
        |> Fuzz.map abs


splittablePositiveIntFuzzer : Fuzz.Fuzzer Int
splittablePositiveIntFuzzer =
    positiveIntFuzzer
        |> Fuzz.map (\n -> n // 2)


suite : Test
suite =
    describe "Money"
        [ addDigitTest
        , toInt
        , splitTest
        , fromIntTest
        , dropDigitTest
        ]


addDigitTest : Test
addDigitTest =
    describe "addDigit"
        [ test "adding one digit" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.toString
                    |> Expect.equal "£0.01"
        , test "adding two digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.toString
                    |> Expect.equal "£0.12"
        , test "adding three digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.addDigit 3
                    |> Money.toString
                    |> Expect.equal "£1.23"
        , test "adding four digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.addDigit 3
                    |> Money.addDigit 4
                    |> Money.toString
                    |> Expect.equal "£12.34"
        , test "adding lot's of digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.addDigit 3
                    |> Money.addDigit 4
                    |> Money.addDigit 5
                    |> Money.addDigit 6
                    |> Money.toString
                    |> Expect.equal "£1234.56"
        ]


dropDigitTest : Test
dropDigitTest =
    describe "dropDigit"
        [ test "when zero" <|
            \_ ->
                Money.zero
                    |> Money.dropDigit
                    |> Expect.equal Money.zero
        , test "when amount is a single digit" <|
            \_ ->
                Money.fromInt 1
                    |> Money.dropDigit
                    |> Expect.equal Money.zero
        , test "when amount has multiple digits" <|
            \_ ->
                Money.fromInt 123
                    |> Money.dropDigit
                    |> Expect.equal (Money.fromInt 12)
        ]


toInt : Test
toInt =
    describe "toInt"
        [ test "adding one digit" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.toInt
                    |> Expect.equal 1
        , test "adding two digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.toInt
                    |> Expect.equal 12
        , test "adding three digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.addDigit 3
                    |> Money.toInt
                    |> Expect.equal 123
        , test "adding four digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.addDigit 3
                    |> Money.addDigit 4
                    |> Money.toInt
                    |> Expect.equal 1234
        , test "adding lot's of digits" <|
            \_ ->
                Money.zero
                    |> Money.addDigit 1
                    |> Money.addDigit 2
                    |> Money.addDigit 3
                    |> Money.addDigit 4
                    |> Money.addDigit 5
                    |> Money.addDigit 6
                    |> Money.toInt
                    |> Expect.equal 123456
        ]


splitTest : Test
splitTest =
    describe "splitTest"
        [ fuzz splittablePositiveIntFuzzer "splitting evenly" <|
            \int ->
                Money.fromInt (int * 2)
                    |> Money.split 5 5
                    |> Expect.equal ( Money.fromInt int, Money.fromInt int )
        , test "splitting two to one" <|
            \_ ->
                Money.fromInt 1000
                    |> Money.split 200 100
                    |> Expect.equal ( Money.fromInt 667, Money.fromInt 333 )
        , test "splitting an uneven value" <|
            \_ ->
                Money.fromInt 1234
                    |> Money.split 77 42
                    |> Expect.equal ( Money.fromInt 799, Money.fromInt 435 )
        ]


fromIntTest : Test
fromIntTest =
    fuzz positiveIntFuzzer "all ints return to int" <|
        \int ->
            Money.fromInt int
                |> Money.toInt
                |> Expect.equal int
