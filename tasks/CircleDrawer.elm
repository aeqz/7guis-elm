module CircleDrawer exposing (main)

{-| Despite of this being a more complex UI with custom drawing, it doesn't
feel too complex to implement in Elm.

I decided the following in regards to behavior that is unspecified by the task
statement:

    - The circle selection is preserved when undoing/redoing.

Built-in CSS hover cannot be used due to the specific task requirements, and a
lot of mouse events handling is required overall. I used HTML elements as
circles rather than using an HTML Canvas because that was easy to do in a
declarative way.

Undo-redo doesn't come for free, but it's easy to implement thank's to Elm's
functional idioms and data immutability.

-}

import Browser exposing (sandbox)
import Html exposing (Attribute, Html, button, div, input, text)
import Html.Attributes exposing (disabled, style, type_, value)
import Html.Events exposing (custom, on, onClick, onInput)
import Json.Decode as Decode


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { nextId : Int
    , overId : Maybe Int
    , editId : Maybe Int
    , editRadius : Maybe Int
    , circles : UndoRedo (List Circle)
    }


init : Model
init =
    { nextId = 0
    , overId = Nothing
    , editId = Nothing
    , editRadius = Nothing
    , circles = UndoRedo [] [] []
    }


type alias Circle =
    { id : Int
    , radius : Int
    , center : Point
    }


contains : Point -> Circle -> Bool
contains point { center, radius } =
    distance point center <= toFloat radius


type alias Point =
    { x : Int
    , y : Int
    }


distance : Point -> Point -> Float
distance a b =
    sqrt <| toFloat <| (b.x - a.x) ^ 2 + (b.y - a.y) ^ 2



-- UNDO-REDO


type alias UndoRedo a =
    { current : a
    , prev : List a
    , next : List a
    }


modify : UndoRedo a -> (a -> a) -> UndoRedo a
modify { current, prev } f =
    { current = f current
    , prev = current :: prev
    , next = []
    }


undo : UndoRedo a -> UndoRedo a
undo undoRedo =
    case undoRedo.prev of
        [] ->
            undoRedo

        current :: prev ->
            { current = current
            , prev = prev
            , next = undoRedo.current :: undoRedo.next
            }


redo : UndoRedo a -> UndoRedo a
redo undoRedo =
    case undoRedo.next of
        [] ->
            undoRedo

        current :: next ->
            { current = current
            , prev = undoRedo.current :: undoRedo.prev
            , next = next
            }



-- UPDATE


type Msg
    = Create Int Point
    | SetOver Point
    | UnsetOver
    | ToggleEdit
    | SetEditRadius String
    | SaveEditRadius
    | Undo
    | Redo


update : Msg -> Model -> Model
update msg model =
    case msg of
        Create radius center ->
            { nextId = model.nextId + 1
            , overId = Just model.nextId
            , editId = Nothing
            , editRadius = Nothing
            , circles =
                modify model.circles <|
                    (::)
                        { id = model.nextId
                        , center = center
                        , radius = radius
                        }
            }

        SetOver point ->
            { model
                | overId =
                    model.circles.current
                        |> List.filter (contains point)
                        |> List.sortBy (.center >> distance point)
                        |> List.head
                        |> Maybe.map .id
            }

        UnsetOver ->
            { model | overId = Nothing }

        ToggleEdit ->
            { model
                | editRadius = Nothing
                , editId =
                    if model.overId == model.editId then
                        Nothing

                    else
                        model.overId
            }

        SetEditRadius value ->
            case String.toFloat value of
                Nothing ->
                    model

                Just radius ->
                    { model | editRadius = Just <| round radius }

        SaveEditRadius ->
            case ( model.editId, model.editRadius ) of
                ( Just editId, Just radius ) ->
                    { model
                        | circles =
                            modify model.circles <|
                                List.map <|
                                    \circle ->
                                        if circle.id == editId then
                                            { circle | radius = radius }

                                        else
                                            circle
                        , editRadius = Nothing
                    }

                _ ->
                    model

        Undo ->
            { model | circles = undo model.circles }

        Redo ->
            { model | circles = redo model.circles }



-- VIEW


view : Model -> Html Msg
view { overId, editId, editRadius, circles } =
    let
        defaultRadius =
            50

        minRadius =
            10

        maxRadius =
            500

        editCircle =
            circles.current
                |> List.filter (.id >> Just >> (==) editId)
                |> List.head

        viewCircle circle =
            let
                radius =
                    if Just circle.id == editId then
                        Maybe.withDefault circle.radius editRadius

                    else
                        circle.radius
            in
            div
                [ style "position" "absolute"
                , style "pointer-events" "none"
                , style "border-radius" "50%"
                , style "border" "1px solid"
                , style "box-sizing" "border-box"
                , style "width" <| String.fromInt (radius * 2) ++ "px"
                , style "height" <| String.fromInt (radius * 2) ++ "px"
                , style "left" <| String.fromInt (circle.center.x - radius) ++ "px"
                , style "top" <| String.fromInt (circle.center.y - radius) ++ "px"
                , style "background" <|
                    if Just circle.id == overId then
                        "silver"

                    else
                        "transparent"
                , style "border-color" <|
                    if Just circle.id == editId then
                        "black"

                    else
                        "silver"
                ]
                []
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "height" "100vh"
        , style "padding" "1rem"
        , style "box-sizing" "border-box"
        ]
        [ div
            [ style "display" "flex"
            , style "justify-content" "center"
            , style "gap" "1rem"
            , style "padding" "0.5rem"
            ]
            [ button [ onClick Undo, disabled <| List.isEmpty circles.prev ] [ text "Undo" ]
            , button [ onClick Redo, disabled <| List.isEmpty circles.next ] [ text "Redo" ]
            ]
        , div
            [ style "flex" "1"
            , style "position" "relative"
            , style "overflow" "hidden"
            , style "border" "2px solid black"
            , onMouseEnter SetOver
            , onMouseMove SetOver
            , onMouseLeave UnsetOver
            , onRightClick ToggleEdit
            , onLeftClick <| Create defaultRadius
            ]
          <|
            List.map viewCircle circles.current
        , case editCircle of
            Nothing ->
                text ""

            Just circle ->
                div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "align-items" "center"
                    , style "padding" "0.5rem"
                    , style "gap" "0.5rem"
                    ]
                    [ text <| "Adjust diameter of circle at (" ++ String.fromInt circle.center.x ++ ", " ++ String.fromInt circle.center.y ++ ")"
                    , input
                        [ type_ "range"
                        , onInput SetEditRadius
                        , onChange SaveEditRadius
                        , Html.Attributes.max <| String.fromInt maxRadius
                        , Html.Attributes.min <| String.fromInt minRadius
                        , value <| String.fromInt <| Maybe.withDefault circle.radius editRadius
                        ]
                        []
                    ]
        ]



-- EVENTS


onChange : msg -> Attribute msg
onChange =
    on "change"
        << Decode.succeed


onMouseEnter : (Point -> msg) -> Attribute msg
onMouseEnter toMsg =
    on "mouseenter" <|
        Decode.map toMsg offsetPoint


onMouseMove : (Point -> msg) -> Attribute msg
onMouseMove toMsg =
    on "mousemove" <|
        Decode.map toMsg offsetPoint


onMouseLeave : msg -> Attribute msg
onMouseLeave =
    on "mouseleave"
        << Decode.succeed


onRightClick : msg -> Attribute msg
onRightClick msg =
    custom "contextmenu" <|
        Decode.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }


onLeftClick : (Point -> msg) -> Attribute msg
onLeftClick toMsg =
    custom "click" <|
        Decode.map
            (\point ->
                { message = toMsg point
                , stopPropagation = True
                , preventDefault = True
                }
            )
            offsetPoint


offsetPoint : Decode.Decoder Point
offsetPoint =
    Decode.map2 Point
        (Decode.field "offsetX" Decode.float |> Decode.map round)
        (Decode.field "offsetY" Decode.float |> Decode.map round)
