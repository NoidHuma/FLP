module Parser

open FParsec

// Тип выражения
type Expression =
    | Number of int
    | Op of string * Expression * Expression

// Пробелы
let ws = spaces

// Число
let pNumber = pint32 .>> ws |>> Number

// Объявляем выражение
let expr, exprRef = createParserForwardedToRef<Expression, unit>()

// Обработка скобок
let pParenExpr = between (pstring "(" .>> ws) (pstring ")" .>> ws) expr

// Либо число, либо выражение в скобках
let pTerm = pNumber <|> pParenExpr

// Операции с приоритетами
let opp = new OperatorPrecedenceParser<Expression, unit, unit>()
let pExpr = opp.ExpressionParser
opp.TermParser <- pTerm

let binOp sym prio = InfixOperator(sym, ws, prio, Associativity.Left, fun a b -> Op(sym, a, b))

opp.AddOperator(binOp "+" 1)
opp.AddOperator(binOp "-" 1)
opp.AddOperator(binOp "*" 2)
opp.AddOperator(binOp "/" 2)

exprRef := pExpr

// Главный парсер
let pProgram = ws >>. expr .>> eof

let parse input =
    match run pProgram input with
    | Success(result, _, _) ->
        printfn "Разбор успешен: %A" result
        Some result
    | Failure(msg, _, _) ->
        printfn "Ошибка:\n%s" msg
        None