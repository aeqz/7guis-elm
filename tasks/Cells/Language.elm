module Cells.Language exposing
    ( Expression, read, Position, dependsOn
    , Value, print
    , Context, evaluate
    )

{-|


# Expression

@docs Expression, read, Position, dependsOn


# Value

@docs Value, print


# Evaluate

@docs Context, evaluate

-}

import List.Nonempty as Nonempty exposing (Nonempty(..))
import Parser exposing ((|.), (|=), DeadEnd, Parser, Problem(..))



-- EXPRESSION


{-| Abstract syntax for a simple language with numerical and text values,
operations on cell ranges and conditionals.
-}
type Expression
    = Literal Float
    | Say String
    | Scalar RangeExpression
    | Aggregation Aggregation Aggregation (Nonempty RangeExpression)
    | If ConditionalExpression Expression Expression


{-| Possible value aggregations.
-}
type Aggregation
    = Sum
    | Product


{-| Abstract syntax of cell ranges and cell-wise operations on them.
-}
type RangeExpression
    = Add (Nonempty RangeExpression)
    | Multiply (Nonempty RangeExpression)
    | SubtractTo (Nonempty RangeExpression) RangeExpression
    | DivideBy RangeExpression RangeExpression
    | Range Reference Reference


{-| The index of a cell by a column character and row integer.
-}
type alias Position =
    ( Char, Int )


{-| A reference to either an entire row, an entire column or a single cell.
-}
type Reference
    = Row Int
    | Column Char
    | Cell Position


{-| An expression to be used to branch conditionally depending on another
expression's evaluation.
-}
type ConditionalExpression
    = Compare Expression Comparison Expression


{-| Different possible comparisons.
-}
type Comparison
    = Equal
    | LessThan
    | GreaterThan
    | LessOrEqualThan
    | GreaterOrEqualThan


type alias Errors =
    Nonempty String



-- PROPERTIES


{-| Check whether an `Expression` depends on a specific cell `Position`.
-}
dependsOn : Expression -> Position -> Bool
dependsOn expression position =
    case expression of
        Literal _ ->
            False

        Say _ ->
            False

        Scalar rangeExpression ->
            rangeExpressionDependsOn position rangeExpression

        Aggregation _ _ rangeExpressions ->
            Nonempty.any (rangeExpressionDependsOn position) rangeExpressions

        If conditionalExpression left right ->
            conditionalExpressionDependsOn conditionalExpression position
                || dependsOn left position
                || dependsOn right position


rangeExpressionDependsOn : Position -> RangeExpression -> Bool
rangeExpressionDependsOn position rangeExpression =
    case rangeExpression of
        Add expressions ->
            Nonempty.any (rangeExpressionDependsOn position) expressions

        Multiply expressions ->
            Nonempty.any (rangeExpressionDependsOn position) expressions

        SubtractTo expressions other ->
            Nonempty.any (rangeExpressionDependsOn position) expressions
                || rangeExpressionDependsOn position other

        DivideBy left right ->
            rangeExpressionDependsOn position left
                || rangeExpressionDependsOn position right

        Range fromReference toReference ->
            isLowerBound fromReference position
                && isUpperBound toReference position


conditionalExpressionDependsOn : ConditionalExpression -> Position -> Bool
conditionalExpressionDependsOn conditionalExpression position =
    case conditionalExpression of
        Compare left comparison right ->
            case comparison of
                Equal ->
                    dependsOn left position
                        || dependsOn right position

                LessThan ->
                    dependsOn left position
                        || dependsOn right position

                GreaterThan ->
                    dependsOn left position
                        || dependsOn right position

                LessOrEqualThan ->
                    dependsOn left position
                        || dependsOn right position

                GreaterOrEqualThan ->
                    dependsOn left position
                        || dependsOn right position


isLowerBound : Reference -> Position -> Bool
isLowerBound reference (( column, row ) as position) =
    case reference of
        Row r ->
            r <= row

        Column c ->
            c <= column

        Cell p ->
            p <= position


isUpperBound : Reference -> Position -> Bool
isUpperBound reference (( column, row ) as position) =
    case reference of
        Row r ->
            row <= r

        Column c ->
            column <= c

        Cell p ->
            position <= p



-- PARSING


{-| Parse an `Expression` from a `String`, which may result in parse errors.
-}
read : String -> Result Errors Expression
read =
    Parser.run (expressionParser |. Parser.end)
        >> Result.mapError
            (\deadEnds ->
                case Nonempty.fromList deadEnds of
                    Nothing ->
                        Nonempty.singleton "Could not parse expression"

                    Just someDeadEnds ->
                        Nonempty.map deadEndToString someDeadEnds
            )


expressionParser : Parser Expression
expressionParser =
    Parser.oneOf
        [ Parser.succeed Literal
            |= Parser.float
        , Parser.succeed (String.trim >> Say)
            |. Parser.keyword "say"
            |. Parser.spaces
            |= Parser.getChompedString (Parser.chompUntil ".")
            |. Parser.symbol "."
        , Parser.succeed Scalar
            |. Parser.keyword "scalar"
            |. Parser.spaces
            |. Parser.keyword "from"
            |. Parser.spaces
            |= rangeExpressionParser
        , Parser.succeed identity
            |. Parser.keyword "sum"
            |. Parser.spaces
            |. Parser.keyword "of"
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed (Aggregation Sum Product)
                    |. Parser.keyword "products"
                    |. Parser.spaces
                    |. Parser.keyword "of"
                    |. Parser.spaces
                    |= twoOrMoreParser rangeExpressionParser
                , Parser.succeed (Aggregation Sum Sum)
                    |= oneOrMoreParser rangeExpressionParser
                ]
        , Parser.succeed identity
            |. Parser.keyword "product"
            |. Parser.spaces
            |. Parser.keyword "of"
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed (Aggregation Product Sum)
                    |. Parser.keyword "sums"
                    |. Parser.spaces
                    |. Parser.keyword "of"
                    |. Parser.spaces
                    |= twoOrMoreParser rangeExpressionParser
                , Parser.succeed (Aggregation Product Product)
                    |= oneOrMoreParser rangeExpressionParser
                ]
        , Parser.succeed If
            |. Parser.keyword "if"
            |. Parser.spaces
            |= conditionalExpressionParser
            |. Parser.spaces
            |. Parser.keyword "then"
            |. Parser.spaces
            |= Parser.lazy (\() -> expressionParser)
            |. Parser.spaces
            |. Parser.keyword "else"
            |. Parser.spaces
            |= Parser.lazy (\() -> expressionParser)

        -- Syntax sugar
        , Parser.succeed (\position -> Scalar (Range (Cell position) (Cell position)))
            |= cellParser
        ]


rangeExpressionParser : Parser RangeExpression
rangeExpressionParser =
    Parser.oneOf
        [ Parser.succeed identity
            |. Parser.keyword "result"
            |. Parser.spaces
            |. Parser.keyword "of"
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed Add
                    |. Parser.keyword "adding"
                    |. Parser.spaces
                    |= twoOrMoreParser (Parser.lazy (\() -> rangeExpressionParser))
                , Parser.succeed Multiply
                    |. Parser.keyword "multiplying"
                    |. Parser.spaces
                    |= twoOrMoreParser (Parser.lazy (\() -> rangeExpressionParser))
                , Parser.succeed SubtractTo
                    |. Parser.keyword "subtracting"
                    |. Parser.spaces
                    |= oneOrMoreParser (Parser.lazy (\() -> rangeExpressionParser))
                    |. Parser.spaces
                    |. Parser.keyword "to"
                    |. Parser.spaces
                    |= Parser.lazy (\() -> rangeExpressionParser)
                , Parser.succeed DivideBy
                    |. Parser.keyword "dividing"
                    |. Parser.spaces
                    |= Parser.lazy (\() -> rangeExpressionParser)
                    |. Parser.spaces
                    |. Parser.keyword "by"
                    |. Parser.spaces
                    |= Parser.lazy (\() -> rangeExpressionParser)
                ]
        , Parser.succeed Range
            |. Parser.keyword "range"
            |. Parser.spaces
            |. Parser.keyword "from"
            |. Parser.spaces
            |= referenceParser
            |. Parser.spaces
            |. Parser.keyword "to"
            |. Parser.spaces
            |= referenceParser

        -- Syntax sugar
        , Parser.succeed (\range -> Range range range)
            |= referenceParser
        ]


referenceParser : Parser Reference
referenceParser =
    Parser.oneOf
        [ Parser.succeed Row
            |. Parser.keyword "row"
            |. Parser.spaces
            |= Parser.int
        , Parser.succeed Column
            |. Parser.keyword "column"
            |. Parser.spaces
            |= parseColumn
        , Parser.succeed Cell
            |= cellParser
        ]


cellParser : Parser Position
cellParser =
    Parser.succeed Tuple.pair
        |. Parser.keyword "cell"
        |. Parser.spaces
        |= parseColumn
        |. Parser.spaces
        |= Parser.int


parseColumn : Parser Char
parseColumn =
    List.range (Char.toCode 'A') (Char.toCode 'Z')
        |> List.map
            (\code ->
                Parser.succeed (Char.fromCode code)
                    |. (Char.fromCode code
                            |> String.fromChar
                            |> Parser.token
                       )
            )
        |> Parser.oneOf


conditionalExpressionParser : Parser ConditionalExpression
conditionalExpressionParser =
    Parser.succeed Compare
        |= Parser.lazy (\() -> expressionParser)
        |. Parser.spaces
        |= comparisonParser
        |. Parser.spaces
        |= Parser.lazy (\() -> expressionParser)


comparisonParser : Parser Comparison
comparisonParser =
    Parser.oneOf
        [ Parser.succeed Equal
            |. Parser.keyword "equals"
        , Parser.succeed identity
            |. Parser.keyword "is"
            |. Parser.spaces
            |= Parser.oneOf
                [ Parser.succeed identity
                    |. Parser.keyword "less"
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed LessThan
                            |. Parser.keyword "than"
                        , Parser.succeed LessOrEqualThan
                            |. Parser.keyword "or"
                            |. Parser.spaces
                            |. Parser.keyword "equal"
                            |. Parser.spaces
                            |. Parser.keyword "than"
                        ]
                , Parser.succeed identity
                    |. Parser.keyword "greater"
                    |. Parser.spaces
                    |= Parser.oneOf
                        [ Parser.succeed GreaterThan
                            |. Parser.keyword "than"
                        , Parser.succeed GreaterOrEqualThan
                            |. Parser.keyword "or"
                            |. Parser.spaces
                            |. Parser.keyword "equal"
                            |. Parser.spaces
                            |. Parser.keyword "than"
                        ]
                ]
        ]


oneOrMoreParser : Parser a -> Parser (Nonempty a)
oneOrMoreParser parser =
    Parser.succeed Nonempty
        |= parser
        |. Parser.spaces
        |= Parser.oneOf
            [ Parser.sequence
                { start = "and"
                , separator = "and"
                , end = ""
                , spaces = Parser.spaces
                , item = parser
                , trailing = Parser.Forbidden
                }
            , Parser.succeed []
            ]


twoOrMoreParser : Parser a -> Parser (Nonempty a)
twoOrMoreParser parser =
    Parser.succeed Nonempty
        |= parser
        |. Parser.spaces
        |= Parser.sequence
            { start = "and"
            , separator = "and"
            , end = ""
            , spaces = Parser.spaces
            , item = parser
            , trailing = Parser.Forbidden
            }


deadEndToString : DeadEnd -> String
deadEndToString { problem, col } =
    parseProblemToString problem ++ " at position " ++ String.fromInt col


parseProblemToString : Problem -> String
parseProblemToString problem =
    case problem of
        Expecting expected ->
            "expecting \"" ++ expected ++ "\""

        ExpectingInt ->
            "expecting int"

        ExpectingHex ->
            "expecting hex"

        ExpectingOctal ->
            "expecting octal"

        ExpectingBinary ->
            "expecting binary"

        ExpectingFloat ->
            "expecting float"

        ExpectingNumber ->
            "expecting number"

        ExpectingVariable ->
            "expecting variable"

        ExpectingSymbol symbol ->
            "expecting symbol \"" ++ symbol ++ "\""

        ExpectingKeyword keyword ->
            "expecting keyword \"" ++ keyword ++ "\""

        ExpectingEnd ->
            "expecting end"

        UnexpectedChar ->
            "unexpected char"

        Problem p ->
            "problem " ++ p

        BadRepeat ->
            "bad repeat"



-- VALUE


{-| The result of evaluating an `Expression`, which may be either a numerical
or text value.
-}
type Value
    = Number Float
    | Text String


{-| Display a `Value`.
-}
print : Value -> String
print value =
    case value of
        Number number ->
            String.fromFloat number

        Text text ->
            text



-- EVALUATE


{-| Context with `Value`s associated to cell `Position`s, which may be
referenced by an `Expression` and affect its evaluation.
-}
type alias Context =
    { value : Position -> Maybe Value
    , definedRowsInColumn : Char -> List Int
    , definedColumnsInRow : Int -> List Char
    }


{-| Produce an output `Value` out of an `Expression` under a given `Context`,
which may result in evaluation errors.
-}
evaluate : Expression -> Context -> Result Errors Value
evaluate expression context =
    case expression of
        Literal number ->
            Number number
                |> Ok

        Say text ->
            Text text
                |> Ok

        Scalar range ->
            evaluateRangeExpression range context
                |> Result.andThen scalarRange
                |> withErrorContext "evaluating scalar expression"
                |> Result.map Number

        Aggregation outer inner ranges ->
            evaluateAggregation outer inner ranges context
                |> withErrorContext "evaluating aggregation"
                |> Result.map Number

        If condition left right ->
            evaluateConditionalExpression condition context
                |> withErrorContext "evaluating the if condition"
                |> Result.andThen
                    (\checkResult ->
                        if checkResult then
                            evaluate left context
                                |> withErrorContext "evaluating the if first branch"

                        else
                            evaluate right context
                                |> withErrorContext "evaluating the if second branch"
                    )


scalarRange : List (List Float) -> Result Errors Float
scalarRange rows =
    case rows of
        [ columns ] ->
            case columns of
                [ scalar ] ->
                    Ok scalar

                _ ->
                    ("Expected scalar, got a " ++ String.fromInt (List.length columns) ++ "-columns single-row range")
                        |> Nonempty.singleton
                        |> Err

        _ ->
            ("Expected scalar, got a " ++ String.fromInt (List.length rows) ++ "-rows range")
                |> Nonempty.singleton
                |> Err


evaluateAggregation : Aggregation -> Aggregation -> Nonempty RangeExpression -> Context -> Result Errors Float
evaluateAggregation outer inner ranges context =
    Nonempty.indexedMap
        (\i e ->
            evaluateRangeExpression e context
                |> withErrorContext ("evaluating " ++ combinedAggregationName outer inner ++ " argument at position " ++ String.fromInt i)
                |> Result.map (List.foldl (\row sum -> List.foldl (aggregationOperation inner) sum row) (aggregationDefault inner))
        )
        ranges
        |> Nonempty.foldl1 (map2 (aggregationOperation outer))


aggregationOperation : Aggregation -> Float -> Float -> Float
aggregationOperation aggregation =
    case aggregation of
        Sum ->
            (+)

        Product ->
            (*)


aggregationName : Aggregation -> String
aggregationName aggregation =
    case aggregation of
        Sum ->
            "sum"

        Product ->
            "product"


combinedAggregationName : Aggregation -> Aggregation -> String
combinedAggregationName outer inner =
    aggregationName outer
        ++ (if outer == inner then
                ""

            else
                aggregationName inner ++ "s"
           )


aggregationDefault : Aggregation -> Float
aggregationDefault aggregation =
    case aggregation of
        Sum ->
            0

        Product ->
            1


evaluateRangeExpression : RangeExpression -> Context -> Result Errors (List (List Float))
evaluateRangeExpression expression context =
    case expression of
        Add expressions ->
            evaluateRangeOperation (+) expressions context
                |> withErrorContext "in add"

        Multiply expressions ->
            evaluateRangeOperation (*) expressions context
                |> withErrorContext "in multiply"

        SubtractTo expressions other ->
            map2 (List.map2 (List.map2 (-)))
                (evaluateRangeExpression other context
                    |> withErrorContext "evaluating the argument to subtract from"
                )
                (evaluateRangeOperation (+) expressions context
                    |> withErrorContext "in arguments to subtract"
                )

        DivideBy left right ->
            map2 (List.map2 (List.map2 (/)))
                (evaluateRangeExpression left context
                    |> withErrorContext "evaluating the argument to be divided"
                )
                (evaluateRangeExpression right context
                    |> withErrorContext "evaluating the argument that divides"
                )

        Range from to ->
            evaluateRange from to context


evaluateRange : Reference -> Reference -> Context -> Result Errors (List (List Float))
evaluateRange from to context =
    let
        ( minCol, minRow ) =
            case from of
                Row row ->
                    ( context.definedColumnsInRow row |> List.minimum, Just row )

                Column column ->
                    ( Just column, context.definedRowsInColumn column |> List.minimum )

                Cell ( column, row ) ->
                    ( Just column, Just row )

        ( maxCol, maxRow ) =
            case to of
                Row row ->
                    ( context.definedColumnsInRow row |> List.maximum, Just row )

                Column column ->
                    ( Just column, context.definedRowsInColumn column |> List.maximum )

                Cell ( column, row ) ->
                    ( Just column, Just row )
    in
    Maybe.map2 List.range minRow maxRow
        |> Maybe.withDefault []
        |> List.foldr
            (\row ->
                map2 (::)
                    (Maybe.map2 List.range
                        (minCol |> Maybe.map Char.toCode)
                        (maxCol |> Maybe.map Char.toCode)
                        |> Maybe.withDefault []
                        |> List.map Char.fromCode
                        |> List.foldr
                            (\column ->
                                map2 (::)
                                    (context.value ( column, row )
                                        |> Maybe.map numberValue
                                        |> Maybe.withDefault (Ok 0)
                                        |> withErrorContext ("obtaining the value at cell " ++ String.fromChar column ++ String.fromInt row)
                                    )
                            )
                            (Ok [])
                    )
            )
            (Ok [])


evaluateRangeOperation : (Float -> Float -> Float) -> Nonempty RangeExpression -> Context -> Result Errors (List (List Float))
evaluateRangeOperation operation ranges context =
    Nonempty.indexedMap
        (\i e ->
            evaluateRangeExpression e context
                |> withErrorContext ("evaluating operation at position " ++ String.fromInt i)
        )
        ranges
        |> Nonempty.foldl1 (map2 (List.map2 (List.map2 operation)))


evaluateConditionalExpression : ConditionalExpression -> Context -> Result Errors Bool
evaluateConditionalExpression expression context =
    case expression of
        Compare left comparison right ->
            map2
                (case comparison of
                    Equal ->
                        (==)

                    LessThan ->
                        (<)

                    GreaterThan ->
                        (>)

                    LessOrEqualThan ->
                        (<=)

                    GreaterOrEqualThan ->
                        (>=)
                )
                (evaluate left context
                    |> Result.andThen numberValue
                    |> withErrorContext "evaluating comparison left expression"
                )
                (evaluate right context
                    |> Result.andThen numberValue
                    |> withErrorContext "evaluating comparison right expression"
                )


numberValue : Value -> Result Errors Float
numberValue value =
    case value of
        Number number ->
            Ok number

        Text text ->
            ("Expected number, found text \"" ++ text ++ "\"")
                |> Nonempty.singleton
                |> Err


withErrorContext : String -> Result Errors a -> Result Errors a
withErrorContext context =
    Result.mapError (Nonempty.map (\error -> error ++ " when " ++ context))


map2 : (a -> b -> c) -> Result Errors a -> Result Errors b -> Result Errors c
map2 f left right =
    case ( left, right ) of
        ( Err leftError, Err rightError ) ->
            Nonempty.append leftError rightError
                |> Err

        ( Err leftError, Ok _ ) ->
            Err leftError

        ( Ok _, Err rightError ) ->
            Err rightError

        ( Ok a, Ok b ) ->
            f a b
                |> Ok
