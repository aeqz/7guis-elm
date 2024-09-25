module Cells.Outputs exposing
    ( Error(..), Cycle, errorToString
    , Output, outputToString
    , Inputs, Outputs, compute
    )

{-|


# Error

@docs Error, Cycle, errorToString


# Output

@docs Output, outputToString


# Compute

@docs Inputs, Outputs, compute

-}

import Cells.Language as Language exposing (Context, Position, Value)
import Dict exposing (Dict)
import List.Nonempty as Nonempty exposing (Nonempty)



-- OUTPUT


{-| The `Result` of evaluating an `Expression` all the way from parsing it to
computing its dependency `Value`s.
-}
type alias Output =
    Result Error Value


{-| Display an `Output`.
-}
outputToString : Output -> String
outputToString output =
    case output of
        Ok value ->
            Language.print value

        Err error ->
            errorToString error


{-| An error that may happen when evaluating an `Expression`, from parsing it
to detecting reference `Cycles`, computing dependency `Value`s and evaluating
the parsed `Expression`.
-}
type Error
    = FailedParsing (Nonempty String)
    | CyclicDependencies (Nonempty Cycle)
    | FailedDependencies (Nonempty Position)
    | EvaluationFailed (Nonempty String)


{-| Display an `Error`.
-}
errorToString : Error -> String
errorToString error =
    case error of
        FailedParsing errors ->
            Nonempty.toList errors
                |> String.join "\n"
                |> String.append "Invalid input:\n\n"

        CyclicDependencies cycles ->
            Nonempty.map cycleToString cycles
                |> Nonempty.toList
                |> String.join "\n"
                |> String.append "There are cyclic dependencies:\n\n"

        FailedDependencies dependencies ->
            Nonempty.map positionToString dependencies
                |> Nonempty.toList
                |> String.join ","
                |> String.append "Some dependencies have errors:\n\n"

        EvaluationFailed errors ->
            Nonempty.toList errors
                |> String.join "\n"
                |> String.append "Could not compute the value:\n\n"


positionToString : Position -> String
positionToString ( cell, row ) =
    String.fromChar cell ++ String.fromInt row


cycleToString : Cycle -> String
cycleToString cycle =
    Nonempty.toList cycle
        |> (::) (Nonempty.last cycle)
        |> List.map positionToString
        |> String.join "->"



-- COMPUTE


{-| Raw cell inputs by `Position`.
-}
type alias Inputs =
    Dict Position String


{-| All the computed cell `Output`s by `Position`.
-}
type alias Outputs =
    Dict Position Output


{-| Compute `Outputs` out of the given raw cell `Input`s.

Raw inputs are parsed, checked for reference `Cycles`, and evaluated in an way
that dependency computations are reused to avoid unnecessary work.

-}
compute : Inputs -> Outputs
compute inputs =
    Dict.keys inputs
        |> List.foldl (computePositionOutput inputs []) Dict.empty


computePositionOutput : Inputs -> Trace -> Position -> Outputs -> Outputs
computePositionOutput inputs trace position outputs =
    if Dict.member position outputs then
        outputs

    else
        case Dict.get position inputs of
            Nothing ->
                outputs

            Just input ->
                case checkCycle position trace of
                    Just cycle ->
                        Nonempty.foldl (addCycle cycle) outputs cycle

                    Nothing ->
                        case Language.read input of
                            Err parseErrors ->
                                Dict.insert position (FailedParsing parseErrors |> Err) outputs

                            Ok expression ->
                                let
                                    nextTrace =
                                        position :: trace

                                    dependencies =
                                        Dict.keys inputs
                                            |> List.filter (Language.dependsOn expression)

                                    outputsWithDependencies =
                                        List.foldl (computePositionOutput inputs nextTrace) outputs dependencies
                                in
                                if Dict.member position outputsWithDependencies then
                                    outputsWithDependencies

                                else
                                    let
                                        result =
                                            case
                                                dependencies
                                                    |> List.filter (not << hasValue outputsWithDependencies)
                                                    |> Nonempty.fromList
                                            of
                                                Nothing ->
                                                    context outputsWithDependencies
                                                        |> Language.evaluate expression
                                                        |> Result.mapError EvaluationFailed

                                                Just failedDependencies ->
                                                    FailedDependencies failedDependencies
                                                        |> Err
                                    in
                                    Dict.insert position result outputsWithDependencies


hasValue : Outputs -> Position -> Bool
hasValue outputs position =
    Dict.get position outputs
        |> Maybe.andThen Result.toMaybe
        |> (/=) Nothing


context : Outputs -> Context
context outputs =
    { value =
        \position ->
            Dict.get position outputs
                |> Maybe.andThen Result.toMaybe
    , definedRowsInColumn =
        \fixedColumn ->
            Dict.keys outputs
                |> List.filter (Tuple.first >> (==) fixedColumn)
                |> List.map Tuple.second
    , definedColumnsInRow =
        \fixedRow ->
            Dict.keys outputs
                |> List.filter (Tuple.second >> (==) fixedRow)
                |> List.map Tuple.first
    }



-- CYCLES


{-| A sequence of `Position`s that denote a cycle of cell references.

The last value is implicitly connected to the first one.

-}
type alias Cycle =
    Nonempty Position


type alias Trace =
    List Position


checkCycle : Position -> Trace -> Maybe Cycle
checkCycle position =
    let
        checkCycle_ ecrat trace =
            case trace of
                [] ->
                    Nothing

                current :: next ->
                    if current == position then
                        Just (Nonempty.Nonempty current ecrat)

                    else
                        checkCycle_ (current :: ecrat) next
    in
    checkCycle_ []


addCycle : Cycle -> Position -> Outputs -> Outputs
addCycle cycle position outputs =
    let
        error =
            CyclicDependencies <|
                case Dict.get position outputs of
                    Just (Err (CyclicDependencies cycles)) ->
                        Nonempty.cons cycle cycles

                    _ ->
                        Nonempty.singleton cycle
    in
    Dict.insert position (Err error) outputs
