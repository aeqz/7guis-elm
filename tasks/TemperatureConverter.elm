module TemperatureConverter exposing (main)

{-| The bidirectional dependency can be observed by how when updating any of
the inputs, then the other one gets updated only if the conversion succeeds,
and that's the only possible flow thanks to the model-view-update pattern.

It would be possible to use the type system to add more safety around
temperature units (i.e. not mixing them wrongly), or keeping track of different
validity states (e.g. using a union type), but having both values just as
strings suits better this task requirements, and the parse-convert-display
pipeline with possibility of failure is easy to express in Elm.

-}

import Browser exposing (sandbox)
import Html exposing (Html, div, input, text)
import Html.Attributes exposing (style, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { celsius : String
    , fahrenheit : String
    }


init : Model
init =
    { celsius = ""
    , fahrenheit = ""
    }



-- UPDATE


type Msg
    = SetCelsius String
    | SetFahrenheit String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetCelsius celsius ->
            { celsius = celsius
            , fahrenheit =
                tryConversion celsiusToFahrenheit celsius
                    |> Maybe.withDefault model.fahrenheit
            }

        SetFahrenheit fahrenheit ->
            { fahrenheit = fahrenheit
            , celsius =
                tryConversion fahrenheitToCelsius fahrenheit
                    |> Maybe.withDefault model.celsius
            }


tryConversion : (Float -> Float) -> String -> Maybe String
tryConversion conversion =
    String.toFloat
        >> Maybe.map conversion
        >> Maybe.map String.fromFloat


celsiusToFahrenheit : Float -> Float
celsiusToFahrenheit celsius =
    celsius * (9 / 5) + 32


fahrenheitToCelsius : Float -> Float
fahrenheitToCelsius fahrenheit =
    (fahrenheit - 32) * (5 / 9)



-- VIEW


view : Model -> Html Msg
view { celsius, fahrenheit } =
    div
        [ style "display" "flex"
        , style "gap" "0.5rem"
        , style "padding" "0.5rem"
        ]
        [ input [ value celsius, onInput SetCelsius ] []
        , div [] [ text "Celsius" ]
        , div [] [ text "=" ]
        , input [ value fahrenheit, onInput SetFahrenheit ] []
        , div [] [ text "Fahrenheit" ]
        ]
