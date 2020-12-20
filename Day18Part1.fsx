#load "./Helper.fsx"
open Helper
open System

type Token = | Num of int64 | Star | Plus | OpenP | CloseP

let rec tokenize' tokens = function
    | [] -> tokens
    | ' '::line -> tokenize' tokens line
    | '*'::line -> tokenize' (Star::tokens) line
    | '+'::line -> tokenize' (Plus::tokens) line
    | '('::line -> tokenize' (OpenP::tokens) line
    | ')'::line -> tokenize' (CloseP::tokens) line
    |   x::line -> tokenize' ((Num (Int64.Parse(x.ToString())))::tokens) line

let tokenize (line : string) = tokenize' [] (line |> Seq.toList) |> List.rev

type Operator = | Mul | Add

type ExpressionTree =
    | Number of int64 // leaf
    | LtoR of Operator * ExpressionTree * ExpressionTree

type BuildItem =
    | BuiltExpr of ExpressionTree
    | BuildLtoR of Operator
    | BuildLtoR2 of Operator * ExpressionTree
    | StartParens
    | PopParens

let rec build b buildStack =
    match b, buildStack with
    | BuiltExpr x, BuildLtoR2 (o, x1)::xs -> build (BuiltExpr (LtoR (o, x1, x))) xs
    | BuiltExpr x, _ -> (BuiltExpr x)::buildStack
    | BuildLtoR o, BuiltExpr x::xs -> BuildLtoR2 (o, x)::xs
    | StartParens, xs -> StartParens::xs
    | PopParens, expr::StartParens::xs -> build expr xs
    | PopParens, _ -> failwithf "Tried to pop parens with an invalid stack: %A" buildStack
    | _, BuildLtoR _::_ -> failwithf "BuildLtoR should never be on the stack itself: %A" buildStack
    | _, PopParens::_ -> failwithf "PopParens should never be on the stack itself: %A" buildStack
    | _, BuildLtoR2 _::xs -> failwithf "BuildLtoR2 should be followed by an expression. Stack: %A" buildStack
    | BuildLtoR2 _, _ -> failwithf "Build should never be called with BuildLtoR2. Stack: %A" buildStack

let getFinalExpr stack = match stack with | BuiltExpr x::[] -> x

let rec buildExpTree stack tokens =
    match tokens with
    | [] -> getFinalExpr stack // close out everything on the stack
    | Num x::tokens -> buildExpTree (build (BuiltExpr (Number x)) stack) tokens
    | Plus::tokens -> buildExpTree (build (BuildLtoR Add) stack) tokens
    | Star::tokens -> buildExpTree (build (BuildLtoR Mul) stack) tokens
    | OpenP::tokens -> buildExpTree (build StartParens stack) tokens
    | CloseP::tokens -> buildExpTree (build PopParens stack) tokens

let rec evaluate exprTree =
    match exprTree with
    | LtoR (Add, expr1, expr2) -> (evaluate expr1) + (evaluate expr2)
    | LtoR (Mul, expr1, expr2) -> (evaluate expr1) * (evaluate expr2)
    | Number x -> x

let evaluateLine line = tokenize line |> buildExpTree [] |> evaluate

readInput "day18.txt" |> Seq.map evaluateLine
|> tee (Seq.iteri (printfn "%d: %A"))
|> Seq.map int64
|> Seq.sum
|> printfn "%d"
