module Timer exposing (main)

{-| This was pretty nice to implement in Elm, and feels robust. The different
signals are clear in the update function.

The tick interval can be adjusted in the code. I find responsiveness good at
25ms.

I've represented a tick as the time at which it ticked + elapsed time at that
point, which may look strange, but allows to start ticking again easily when
duration is increased: just set the current tick as now + previous duration.

One detail that is not mentioned in the task is how the elapsed time label
should behave when the duration time was reached and it is reduced by moving
the slider to the left. My implementation reduces the elapsed time to be the
new duration in that case, so if the slider is moved to the right again, it
starts ticking immediately.

Similar to the fight booker's select, the slider input can be made to produce
numeric values by implementing a custom decoder that can fail on the JavaScript
side, rather than having to handle an no-op case in the update function.

-}

import Browser exposing (element)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time exposing (Posix, posixToMillis)


main : Program () Model Msg
main =
    element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { duration : Millis
    , ticking : Maybe Tick
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { duration = maxDuration // 2
      , ticking = Nothing
      }
    , startTicking 0
    )



-- TICK


type alias Millis =
    Int


maxDuration : Millis
maxDuration =
    20000


type alias Tick =
    { elapsed : Millis
    , at : Posix
    }


updateTick : Tick -> Posix -> Tick
updateTick { at, elapsed } now =
    { at = now
    , elapsed = elapsed + posixToMillis now - posixToMillis at
    }



-- UPDATE


type Msg
    = DidTick Tick
    | SetDuration String
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DidTick tick ->
            ( { model
                | ticking =
                    if tick.elapsed < model.duration then
                        Just tick

                    else
                        Nothing
              }
            , Cmd.none
            )

        SetDuration value ->
            case String.toInt value of
                Nothing ->
                    ( model, Cmd.none )

                Just duration ->
                    ( { model | duration = duration }
                    , if model.ticking == Nothing then
                        startTicking model.duration

                      else
                        Cmd.none
                    )

        Reset ->
            ( model, startTicking 0 )


startTicking : Millis -> Cmd Msg
startTicking elapsed =
    Task.perform (Tick elapsed >> DidTick) Time.now



-- SUBSCRIPTIONS


tickInterval : Millis
tickInterval =
    25


subscriptions : Model -> Sub Msg
subscriptions { ticking } =
    case ticking of
        Nothing ->
            Sub.none

        Just tick ->
            Time.every
                (toFloat tickInterval)
                (updateTick tick >> DidTick)



-- VIEW


view : Model -> Html Msg
view { duration, ticking } =
    let
        elapsed =
            Maybe.withDefault duration <|
                Maybe.map .elapsed ticking
    in
    div
        [ style "display" "flex"
        , style "padding" "0.5rem"
        ]
        [ div
            [ style "display" "flex"
            , style "flex-direction" "column"
            , style "gap" "0.5rem"
            ]
            [ div
                [ style "display" "flex"
                , style "gap" "0.5rem"
                ]
                [ div [] [ text "Elapsed Time:" ]
                , div
                    [ style "background" "#e7ecef"
                    , style "flex" "auto"
                    , style "display" "flex"
                    ]
                    [ div
                        [ style "background" "#a3cef1"
                        , style "width" <| percent duration elapsed
                        ]
                        []
                    ]
                ]
            , div []
                [ text <| seconds elapsed ]
            , div
                [ style "display" "flex"
                , style "gap" "0.5rem"
                ]
                [ div [] [ text "Duration:" ]
                , div []
                    [ input
                        [ type_ "range"
                        , onInput SetDuration
                        , Html.Attributes.max <| String.fromInt maxDuration
                        , value <| String.fromInt duration
                        ]
                        []
                    ]
                ]
            , button [ onClick Reset ] [ text "Reset" ]
            ]
        ]


percent : Int -> Int -> String
percent total progress =
    String.fromFloat (100 * toFloat progress / toFloat total) ++ "%"


seconds : Millis -> String
seconds millis =
    String.fromFloat (toFloat (round (toFloat millis / 100)) / 10) ++ "s"
