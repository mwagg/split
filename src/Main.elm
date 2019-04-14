module Main exposing
    ( Model
    , Msg(..)
    , init
    , main
    , update
    , view
    )

import Browser
import Element
    exposing
        ( Element
        , alignRight
        , centerX
        , column
        , el
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , px
        , rgb
        , row
        , text
        , width
        )
import Element.Border as Border
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes exposing (style)
import Money exposing (Money)



---- MODEL ----


type alias Model =
    { money : Money }


init : ( Model, Cmd Msg )
init =
    ( { money = Money.zero }, Cmd.none )



---- UPDATE ----


type Msg
    = AddDigit Int
    | DropDigit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddDigit digit ->
            ( { model | money = Money.addDigit digit model.money }, Cmd.none )

        DropDigit ->
            ( { model | money = Money.dropDigit model.money }, Cmd.none )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Split", body = [ body model ] }


body : Model -> Html Msg
body model =
    Element.layoutWith { options = [ Element.noHover ] }
        [ width fill
        , htmlAttribute (style "user-select" "none")
        , Font.family
            [ Font.typeface "Roboto"
            , Font.sansSerif
            ]
        ]
        (column [ height fill, width fill ]
            [ totalView model
            , splitView model
            , inputView
            ]
        )


rowStyles : List (Element.Attribute msg) -> List (Element.Attribute msg)
rowStyles =
    List.append
        [ width fill
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        ]


totalView : Model -> Element Msg
totalView model =
    row
        (rowStyles [ height (fillPortion 1) ])
        [ el [ centerX, Font.size 64 ] (text (Money.toString model.money)) ]


mikeSalary =
    11000000


jessicaSalary =
    4300000


splitView : Model -> Element Msg
splitView model =
    let
        ( mike, jessica ) =
            Money.split mikeSalary jessicaSalary model.money
    in
    row
        (rowStyles [ height (fillPortion 1) ])
        [ el [ width fill, Font.center ] <| text (Money.toString mike)
        , el [ width fill, Font.center ] <| text (Money.toString jessica)
        ]


inputView : Element Msg
inputView =
    let
        makeDigitButton digit =
            makeButton (String.fromInt digit) (AddDigit digit)

        makeButton label msg =
            el
                [ onMouseDown msg
                , width fill
                , Font.size 24
                , Font.center
                ]
            <|
                text label

        makeRow els =
            row [ width fill, height fill ] els

        makeDigitRow digits =
            makeRow <|
                List.map makeDigitButton digits
    in
    el (rowStyles [ height (fillPortion 2) ]) <|
        column [ width fill, height fill ]
            [ makeDigitRow [ 1, 2, 3 ]
            , makeDigitRow [ 4, 5, 6 ]
            , makeDigitRow [ 7, 8, 9 ]
            , makeRow [ el [ width fill ] Element.none, makeDigitButton 0, makeButton "<" DropDigit ]
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.document
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
