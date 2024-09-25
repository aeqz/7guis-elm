module Cells exposing (main)

{-| I separated both the `Language` and the strategy used for computing the
cell `Outputs` into different modules. This one deals just the UI, which is
simple on the one hand, but on the other hand contains tricky CSS for styling
an HTML table in the expected way. I also added a few simple keyboard commands:
escape to cancel editing a cell, and enter to save it, but note that the
resulting UI is far from being a usable spreadsheet application.

I failed at one of the task goals, which is to efficiently propagate cell
updates rather than recomputing everything, but the reason is that my attempts
felt unnecessarily complex, unreliable, bad at reporting cyclic references and
errors, and not worth it overall in a programming language with a declarative
style, immutable data structures and a model-view-update loop.

Instead, this implementation focuses on being reliable, providing error
messages and avoiding unnecessary computations when recomputing all cells.

-}

import Browser exposing (element)
import Browser.Events exposing (onKeyDown)
import Cells.Language exposing (Position)
import Cells.Outputs as Outputs exposing (Inputs, Outputs)
import Dict
import Html exposing (Html, input, table, td, text, th, tr)
import Html.Attributes exposing (readonly, style, title, value)
import Html.Events exposing (onBlur, onDoubleClick, onInput)
import Json.Decode as Decode


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
    { editCell : Maybe Position
    , editValue : String
    , inputs : Inputs
    , outputs : Outputs
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { editCell = Nothing
      , editValue = ""
      , inputs = example
      , outputs = Outputs.compute example
      }
    , Cmd.none
    )


example : Inputs
example =
    Dict.fromList
        [ ( ( 'B', 2 ), "say Some examples." )
        , ( ( 'F', 2 ), "say Some data." )
        , ( ( 'G', 2 ), "1" )
        , ( ( 'H', 2 ), "1.25" )
        , ( ( 'G', 3 ), "2" )
        , ( ( 'H', 3 ), "2.5" )
        , ( ( 'B', 4 ), "say Number literal." )
        , ( ( 'C', 4 ), "2.5" )
        , ( ( 'G', 4 ), "3" )
        , ( ( 'H', 4 ), "3.75" )
        , ( ( 'B', 5 ), "say Display text." )
        , ( ( 'C', 5 ), "say Hello Cells!." )
        , ( ( 'B', 6 ), "say Sum of column G." )
        , ( ( 'C', 6 ), "sum of column G" )
        , ( ( 'B', 7 ), "say A product of sums." )
        , ( ( 'C', 7 ), "product of sums of column G and column H" )
        , ( ( 'B', 8 ), "say Mirror C7." )
        , ( ( 'C', 8 ), "cell C7" )
        , ( ( 'B', 9 ), "say Conditional." )
        , ( ( 'C', 9 ), "if cell C8 is less than 50 then say C8 < 50. else say C8 >= 50." )
        , ( ( 'B', 10 ), "say Range aggregation." )
        , ( ( 'C', 10 ), "sum of range from cell G2 to cell H4" )
        ]



-- UPDATE


type Msg
    = Edit Position
    | Update String
    | Cancel
    | Save


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Edit position ->
            ( { model
                | editCell = Just position
                , editValue =
                    Dict.get position model.inputs
                        |> Maybe.withDefault ""
              }
            , Cmd.none
            )

        Update input ->
            ( { model | editValue = input }, Cmd.none )

        Cancel ->
            ( { model | editCell = Nothing }, Cmd.none )

        Save ->
            case model.editCell of
                Nothing ->
                    ( model, Cmd.none )

                Just position ->
                    let
                        inputs =
                            case String.trim model.editValue of
                                "" ->
                                    Dict.remove position model.inputs

                                trimmed ->
                                    Dict.insert position trimmed model.inputs
                    in
                    ( { model
                        | inputs = inputs
                        , outputs = Outputs.compute inputs
                        , editCell = Nothing
                      }
                    , Cmd.none
                    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        decodeKey key =
            case key of
                "Enter" ->
                    Decode.succeed Save

                "Escape" ->
                    Decode.succeed Cancel

                _ ->
                    Decode.fail "Not handled"
    in
    Decode.field "key" Decode.string
        |> Decode.andThen decodeKey
        |> onKeyDown



-- VIEW


view : Model -> Html Msg
view { editCell, editValue, outputs } =
    let
        rows =
            List.range 0 99

        columns =
            List.range (Char.toCode 'A') (Char.toCode 'Z')
                |> List.map Char.fromCode

        header =
            columns
                |> List.map columnHeader
                |> (::) (th [] [])
                |> tr
                    [ style "position" "sticky"
                    , style "top" "0"
                    , style "background" "silver"
                    , style "z-index" "2"
                    ]

        columnHeader column =
            th
                []
                [ text <| String.fromChar column
                ]

        rowHeader row =
            th
                [ style "position" "sticky"
                , style "left" "0"
                , style "background" "white"
                , style "z-index" "1"
                , style "padding" "0 0.5rem"
                ]
                [ text <| String.fromInt row
                ]

        cell row column =
            td
                [ style "border" "1px solid black"
                , style "border-collapse" "collapse"
                , style "cursor" <|
                    if editCell == Just ( column, row ) then
                        "text"

                    else
                        "pointer"
                , style "background" <|
                    if editCell == Just ( column, row ) then
                        "initial"

                    else
                        case Dict.get ( column, row ) outputs of
                            Just (Err _) ->
                                "red"

                            _ ->
                                "initial"
                , title <|
                    if editCell == Just ( column, row ) then
                        ""

                    else
                        Dict.get ( column, row ) outputs
                            |> Maybe.map Outputs.outputToString
                            |> Maybe.withDefault ""
                ]
                [ input
                    [ style "border" "0"
                    , style "cursor" "inherit"
                    , style "background" "inherit"
                    , onBlur Save
                    , onInput Update
                    , onDoubleClick <| Edit ( column, row )
                    , readonly <| editCell /= Just ( column, row )
                    , value <|
                        if editCell == Just ( column, row ) then
                            editValue

                        else
                            Dict.get ( column, row ) outputs
                                |> Maybe.map Outputs.outputToString
                                |> Maybe.withDefault ""
                    ]
                    []
                ]
    in
    table
        [ style "border-collapse" "collapse" ]
    <|
        header
            :: List.map
                (\row ->
                    tr [] <|
                        rowHeader row
                            :: List.map (cell row) columns
                )
                rows
