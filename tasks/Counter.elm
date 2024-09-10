module Counter exposing (main)

{-| Shows the basic model-view-update pattern in Elm UIs. Elm is specifically
focused on making web-based UIs, so scaffolding is pretty small.

By looking at the code, the noisiest parts seem to be the module system and
type signatures. For instance, none of that would be needed if writing this in
plain JavaScript. Type definitions and signatures are not really needed but
make the code easier to understand and maintain.

This code is organised in the usual Elm way, but could be also just inlined
as follows:

    main =
        sandbox
            { init = 0
            , update = always ((+) 1)
            , view = \counter ->
                div [...] [...]
            }

-}

import Browser exposing (sandbox)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    Int


init : Model
init =
    0



-- UPDATE


type Msg
    = Count


update : Msg -> Model -> Model
update Count =
    (+) 1



-- VIEW


view : Model -> Html Msg
view counter =
    div
        [ style "display" "flex"
        , style "gap" "0.5rem"
        , style "padding" "0.5rem"
        ]
        [ div [] [ text <| String.fromInt counter ]
        , button [ onClick Count ] [ text "Count" ]
        ]
