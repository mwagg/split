module Money exposing
    ( Money
    , addDigit
    , dropDigit
    , fromInt
    , split
    , toInt
    , toString
    , zero
    )


type Currency
    = GBP


type Money
    = Money Currency Int


currencyToString : Currency -> String
currencyToString currency =
    case currency of
        GBP ->
            "£"


toString : Money -> String
toString (Money currency amount) =
    let
        amountAsFloat =
            toFloat amount

        integer =
            amountAsFloat / 100 |> floor

        fractional =
            (amountAsFloat / 100 - toFloat integer)
                * 100
                |> round

        fractionalStr =
            if fractional < 10 then
                "0" ++ String.fromInt fractional

            else
                String.fromInt fractional
    in
    currencyToString currency
        ++ String.fromInt integer
        ++ "."
        ++ fractionalStr


addDigit : Int -> Money -> Money
addDigit digit (Money currency amount) =
    Money currency (amount * 10 + digit)


dropDigit : Money -> Money
dropDigit (Money currency amount) =
    Money currency (amount // 10)


zero : Money
zero =
    Money GBP 0


toInt : Money -> Int
toInt (Money _ amount) =
    amount


fromInt : Int -> Money
fromInt amount =
    Money GBP amount


split : Int -> Int -> Money -> ( Money, Money )
split firstSplit secondSplit (Money currency amount) =
    let
        ratio =
            toFloat firstSplit / toFloat (firstSplit + secondSplit)

        firstAmount =
            toFloat amount * ratio |> ceiling

        secondAmount =
            amount - firstAmount
    in
    ( Money currency firstAmount, Money currency secondAmount )
