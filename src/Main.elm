module Main exposing
    ( Msg(..)
    , State
    , init
    , main
    , update
    , view
    )

import Browser
import Element
    exposing
        ( Color
        , Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , padding
        , px
        , rgb
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onMouseDown)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Money exposing (Money)
import Settings exposing (Person, Settings)



---- MODEL ----


initPerson : Person
initPerson =
    { name = "", contribution = 0 }


type State
    = ChangingSettings Settings
    | SplittingAmount Settings Money


init : Json.Value -> ( State, Cmd Msg )
init flags =
    let
        model =
            case Json.decodeValue Settings.decoder flags of
                Err _ ->
                    ChangingSettings
                        { personOne = initPerson
                        , personTwo = initPerson
                        }

                Ok settings ->
                    SplittingAmount settings Money.zero
    in
    ( model, Cmd.none )



---- UPDATE ----


type PersonOrder
    = First
    | Second


type Field
    = Name
    | Contribution


type Msg
    = AddDigit Int
    | DropDigit
    | UpdateSettings PersonOrder Field String
    | SaveSettings


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case state of
        SplittingAmount settings money ->
            case msg of
                AddDigit digit ->
                    ( SplittingAmount settings <| Money.addDigit digit money, Cmd.none )

                DropDigit ->
                    ( SplittingAmount settings <| Money.dropDigit money, Cmd.none )

                _ ->
                    ( state, Cmd.none )

        ChangingSettings settings ->
            updateSettings msg settings


updateSettings : Msg -> Settings -> ( State, Cmd Msg )
updateSettings msg settings =
    case msg of
        UpdateSettings personOrder field value ->
            let
                newSetting =
                    case personOrder of
                        First ->
                            { settings | personOne = updatePerson field value settings.personOne }

                        Second ->
                            { settings | personTwo = updatePerson field value settings.personTwo }
            in
            ( ChangingSettings newSetting, Cmd.none )

        SaveSettings ->
            ( SplittingAmount settings Money.zero, Settings.settingsUpdated settings )

        _ ->
            ( ChangingSettings settings, Cmd.none )


updatePerson : Field -> String -> Person -> Person
updatePerson field value form =
    case field of
        Name ->
            { form | name = value }

        Contribution ->
            case String.toInt value of
                Just contribution ->
                    { form | contribution = contribution }

                Nothing ->
                    if String.length value == 0 then
                        { form | contribution = 0 }

                    else
                        form



---- VIEW ----


view : State -> Browser.Document Msg
view state =
    let
        innerView =
            case state of
                ChangingSettings settings ->
                    settingsView settings

                SplittingAmount settings money ->
                    mainView settings money
    in
    { title = "Split", body = [ body innerView ] }


body : Element Msg -> Html Msg
body inner =
    Element.layoutWith { options = [ Element.noHover ] }
        [ width fill
        , htmlAttribute (style "user-select" "none")
        , Font.family
            [ Font.typeface "Roboto"
            , Font.sansSerif
            ]
        ]
        inner


headerBackgroundColor : Color
headerBackgroundColor =
    rgb255 120 144 156


headerColor : Color
headerColor =
    rgb255 240 240 240


black : Color
black =
    rgb255 0 0 0


header : Element msg
header =
    el
        [ height (px 56)
        , width fill
        , Font.glow black 4
        , Font.bold
        , Font.size 22
        , padding 18
        , Font.center
        , Font.color headerColor
        , Background.color headerBackgroundColor
        ]
        (text "Split")


mainView : Settings -> Money -> Element Msg
mainView settings money =
    column [ height fill, width fill ]
        [ header
        , totalView money
        , splitView settings money
        , inputView
        ]


settingsView : Settings -> Element Msg
settingsView settings =
    let
        saveButton =
            row [ width fill ]
                [ Input.button []
                    { onPress = Just SaveSettings
                    , label = text "Save"
                    }
                ]
    in
    column [ height fill, width fill, padding 10, spacing 10 ]
        [ el [ Font.center, width fill, spacing 20 ] (text "Settings")
        , personSettingsView First settings.personOne
        , personSettingsView Second settings.personTwo
        , saveButton
        ]


personSettingsView : PersonOrder -> Person -> Element Msg
personSettingsView personNumber form =
    let
        number =
            case personNumber of
                First ->
                    "1"

                Second ->
                    "2"

        inputRow opts =
            row [ width fill ]
                [ Input.text []
                    { onChange = UpdateSettings personNumber opts.field
                    , text = opts.value
                    , placeholder = Nothing
                    , label =
                        Input.labelAbove
                            [ height fill
                            , centerY
                            , spacing 10
                            ]
                            (text <| opts.label ++ ":")
                    }
                ]
    in
    column [ width fill, spacing 20 ]
        [ el [ width fill ] (text ("PersonOrder " ++ number))
        , inputRow { label = "Name", field = Name, value = form.name }
        , inputRow { label = "Contribution", field = Contribution, value = String.fromInt form.contribution }
        ]


rowStyles : List (Element.Attribute msg) -> List (Element.Attribute msg)
rowStyles =
    List.append
        [ width fill
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        ]


totalView : Money -> Element Msg
totalView money =
    row
        (rowStyles [ height (fillPortion 1) ])
        [ el [ centerX, Font.size 64 ] (text (Money.toString money)) ]


splitView : Settings -> Money -> Element Msg
splitView settings money =
    let
        ( personOne, personTwo ) =
            Money.split settings.personOne.contribution settings.personTwo.contribution money
    in
    row
        (rowStyles [ height (fillPortion 1) ])
        [ el [ width fill, Font.center ] <| text (Money.toString personOne)
        , el [ width fill, Font.center ] <| text (Money.toString personTwo)
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


main : Program Json.Value State Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
