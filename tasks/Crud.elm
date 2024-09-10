module Crud exposing (main)

{-| Because of the model-view-update pattern, the data model is clearly defined
and separated from how it is updated and displayed.

I decided the following in regards to behavior that is unspecified by the task
statement:

    - An entry is automatically selected right after creating it.
    - When a selected entry is filtered out, it cannot be updated or deleted.
    - When a selected entry is filtered out, it is still selected if changing
      the filter so it is visible again.
    - The filter is case-insensitive.

Removing an entry from the array results in code that is noisier than it could
be. The Elm core library seems to be minimalistic in the sense that it doesn't
provide functions for some actions that could be easily achieved by combining
other functions that it provides. Helper functions or dependencies could be
used for this.

-}

import Array exposing (Array)
import Browser exposing (sandbox)
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (disabled, style, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { filter : String
    , name : String
    , surname : String
    , selected : Maybe Int
    , names : Array Name
    }


type alias Name =
    { name : String
    , surname : String
    }


init : Model
init =
    { filter = ""
    , name = ""
    , surname = ""
    , selected = Nothing
    , names =
        Array.fromList
            [ { name = "Hans", surname = "Emil" }
            , { name = "Max", surname = "Mustermann" }
            , { name = "Roman", surname = "Tisch" }
            ]
    }



-- UPDATE


type Msg
    = SetFilter String
    | SetName String
    | SetSurname String
    | ToggleSelected Int
    | Create
    | Update
    | Delete


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetFilter filter ->
            { model | filter = filter }

        SetName name ->
            { model | name = name }

        SetSurname surname ->
            { model | surname = surname }

        ToggleSelected i ->
            if model.selected == Just i then
                { model | selected = Nothing }

            else
                case Array.get i model.names of
                    Nothing ->
                        model

                    Just { name, surname } ->
                        { model
                            | selected = Just i
                            , name = name
                            , surname = surname
                        }

        Create ->
            { model
                | selected = Just <| Array.length model.names
                , names =
                    Array.push
                        { name = model.name
                        , surname = model.surname
                        }
                        model.names
            }

        Update ->
            case model.selected of
                Nothing ->
                    model

                Just i ->
                    { model
                        | names =
                            Array.set i
                                { name = model.name
                                , surname = model.surname
                                }
                                model.names
                    }

        Delete ->
            case model.selected of
                Nothing ->
                    model

                Just i ->
                    { model
                        | selected = Nothing
                        , names =
                            Array.append
                                (Array.slice 0 i model.names)
                                (Array.slice (i + 1) (Array.length model.names) model.names)
                    }



-- VIEW


view : Model -> Html Msg
view { filter, name, surname, names, selected } =
    let
        visibleNames =
            Array.toIndexedList names
                |> List.filter
                    (Tuple.second
                        >> .surname
                        >> String.toLower
                        >> String.startsWith (String.toLower filter)
                    )

        anySelected =
            List.any (Tuple.first >> Just >> (==) selected) visibleNames
    in
    div
        [ style "display" "flex"
        , style "height" "100vh"
        , style "padding" "0.5rem"
        , style "box-sizing" "border-box"
        ]
        [ div
            [ style "display" "flex"
            , style "gap" "0.5rem"
            , style "flex-direction" "column"
            , style "flex" "1"
            ]
            [ div
                [ style "display" "flex"
                , style "gap" "0.5rem"
                ]
                [ div [] [ text "Filter prefix:" ]
                , input [ value filter, onInput SetFilter ] []
                ]
            , div
                [ style "display" "flex"
                , style "flex" "1"
                , style "min-height" "0"
                , style "gap" "0.5rem"
                ]
                [ div
                    [ style "display" "flex"
                    , style "flex-direction" "column"
                    , style "flex" "1"
                    , style "overflow-y" "auto"
                    , style "overflow-x" "hidden"
                    , style "border" "2px solid black"
                    ]
                  <|
                    List.map (viewName selected) visibleNames
                , div
                    [ style "display" "flex"
                    , style "gap" "0.5rem"
                    , style "flex-direction" "column"
                    ]
                    [ div
                        [ style "display" "flex"
                        , style "justify-content" "space-between"
                        , style "gap" "0.5rem"
                        ]
                        [ div [] [ text "Name:" ]
                        , input [ value name, onInput SetName ] []
                        ]
                    , div
                        [ style "display" "flex"
                        , style "justify-content" "space-between"
                        , style "gap" "0.5rem"
                        ]
                        [ div [] [ text "Surname:" ]
                        , input [ value surname, onInput SetSurname ] []
                        ]
                    , div
                        [ style "display" "flex"
                        , style "gap" "0.5rem"
                        ]
                        [ button [ onClick Create ] [ text "Create" ]
                        , button [ onClick Update, disabled <| not anySelected ] [ text "Update" ]
                        , button [ onClick Delete, disabled <| not anySelected ] [ text "Delete" ]
                        ]
                    ]
                ]
            ]
        ]


viewName : Maybe Int -> ( Int, Name ) -> Html Msg
viewName selected ( i, { name, surname } ) =
    div
        [ style "cursor" "pointer"
        , style "white-space" "nowrap"
        , style "overflow-x" "hidden"
        , style "flex-shrink" "0"
        , style "padding" "0.2rem"
        , style "text-overflow" "ellipsis"
        , style "background" <|
            if selected == Just i then
                "#a3cef1"

            else
                "initial"
        , onClick <| ToggleSelected i
        ]
        [ text <| surname ++ ", " ++ name ]
