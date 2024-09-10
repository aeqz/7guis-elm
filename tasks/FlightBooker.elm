module FlightBooker exposing (main)

{-| I think that the UI as described in the task is not good. Validation
feedback should be shown after the user has tried to submit, and not at any
time (e.g. while the user is editing the input fields for the first time).
Browser validation could be used in that case rather than custom one. But I
will stick to the requirements in that regard and don't use HTML
forms/validation logic.

There's also a constraint that I'm going to ignore: the task statement says
that when any input field contains an invalid date, it must be colored red and
the submit button must be disabled, but this doesn't make any sense for the
return date input when the selected mode is one-way.

As the initial date is arbitrary, I have set a hardcoded one to keep this
simple.

The result is not bad, but there are a few weaknesses:

The default usage of a select element requires dealing with impossible states.
This can be improved by implementing a custom decoder that produces values in a
custom type and can fail on the JavaScript side, but I'm representing the mode
as a string instead to keep it simple, could be considered error prone.

It's also easy to forget not showing the validation state for an input, but
that's related to my first point about this UI not being specified as I think
that it should.

-}

import Browser exposing (sandbox)
import Date exposing (Date)
import Html exposing (Attribute, Html, button, div, input, option, select, text)
import Html.Attributes exposing (disabled, selected, style, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type Model
    = Booking FormData
    | Booked Booking


type Booking
    = OneWay { start : Date }
    | Return { start : Date, return : Date }


init : Model
init =
    Booking defaultForm



-- FORM


type alias FormData =
    { mode : String
    , start : String
    , return : String
    }


defaultForm : FormData
defaultForm =
    { mode = "one-way"
    , start = "27.03.2014"
    , return = "27.03.2014"
    }


validate : FormData -> Maybe Booking
validate { mode, start, return } =
    case ( mode, parseDate start, parseDate return ) of
        ( "one-way", Just startDate, _ ) ->
            Just <|
                OneWay
                    { start = startDate
                    }

        ( "return", Just startDate, Just returnDate ) ->
            case Date.compare startDate returnDate of
                GT ->
                    Nothing

                _ ->
                    Just <|
                        Return
                            { start = startDate
                            , return = returnDate
                            }

        _ ->
            Nothing


parseDate : String -> Maybe Date
parseDate value =
    case String.toList value of
        [ d1, d2, '.', m1, m2, '.', y1, y2, y3, y4 ] ->
            String.fromList [ y1, y2, y3, y4, '-', m1, m2, '-', d1, d2 ]
                |> Date.fromIsoString
                |> Result.toMaybe

        _ ->
            Nothing



-- UPDATE


type Msg
    = SetMode String
    | SetStart String
    | SetReturn String
    | Book Booking


update : Msg -> Model -> Model
update msg model =
    case model of
        Booked _ ->
            model

        Booking formData ->
            case msg of
                SetMode mode ->
                    Booking { formData | mode = mode }

                SetStart start ->
                    Booking { formData | start = start }

                SetReturn return ->
                    Booking { formData | return = return }

                Book booking ->
                    Booked booking



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "padding" "0.5rem"
        ]
    <|
        case model of
            Booked booking ->
                [ text <|
                    case booking of
                        OneWay { start } ->
                            "You have booked a one-way flight on " ++ formatDate start ++ "."

                        Return { start, return } ->
                            "You have booked a flight on " ++ formatDate start ++ " with return on " ++ formatDate return ++ "."
                ]

            Booking ({ mode, start, return } as formData) ->
                [ div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "gap" "0.5rem"
                    ]
                    [ select [ onInput SetMode ]
                        [ option
                            [ value "one-way", selected <| mode == "one-way" ]
                            [ text "one-way flight" ]
                        , option
                            [ value "return", selected <| mode == "return" ]
                            [ text "return flight" ]
                        ]
                    , input
                        [ value start
                        , onInput SetStart
                        , validationFeedback start
                        ]
                        []
                    , input
                        [ value return
                        , onInput SetReturn
                        , if mode == "return" then
                            validationFeedback return

                          else
                            disabled True
                        ]
                        []
                    , button
                        [ case validate formData of
                            Nothing ->
                                disabled True

                            Just booking ->
                                onClick (Book booking)
                        ]
                        [ text "Book" ]
                    ]
                ]


validationFeedback : String -> Attribute msg
validationFeedback date =
    style "background" <|
        if parseDate date == Nothing then
            "red"

        else
            "initial"


formatDate : Date -> String
formatDate =
    Date.format "dd.MM.yyyy"
