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
    | BuildOp of Operator
    | BuildOp2 of Operator * ExpressionTree
    | StartParens
    | PopParens
    | PushLevel of int

// higher levels should be resolved first
let getPrecLvl = function | Mul -> 1 | Add -> 2

let rec removeNextPushLevel level stack =
    match stack with
    | [] -> []
    | (StartParens)::_-> stack // can't push past parens
    | (PushLevel x)::xs-> if level <= x then xs else stack
    | x::xs -> x::removeNextPushLevel level xs

let rec relaxLevel level stack =
    match removeNextPushLevel level stack with
    | BuildOp o::BuiltExpr x1::xs ->
        build (BuildOp2 (o, x1)) xs
    | BuiltExpr x1::BuildOp o::xs when level <= (getPrecLvl o) ->
        build (BuildOp2 (o, x1)) (relaxLevel level xs)
    | _ -> stack

and build b buildStack =
    match b, buildStack with
    | BuiltExpr x, BuildOp2 (o, x1)::xs ->
        build (BuiltExpr (LtoR (o, x1, x))) (relaxLevel (getPrecLvl o) xs)
    | BuiltExpr x, BuildOp (o)::xs ->
        BuiltExpr x::BuildOp (o)::xs
    | BuildOp o, BuiltExpr x::xs when (getPrecLvl o) = 2 ->
        BuildOp2 (o, x)::xs
    | BuildOp o, xs ->
        if (getPrecLvl o) = 2
        then  BuildOp o::xs
        else BuildOp o::(PushLevel (getPrecLvl o))::xs
    | BuildOp2 (o, x1), BuiltExpr x::xs ->
        build (BuiltExpr (LtoR (o, x1, x))) xs
    | BuildOp2 (o, x1), xs -> BuildOp2 (o, x1)::xs
    | BuiltExpr x, _ -> (BuiltExpr x)::buildStack
    // we can't be sure there is a preceding expression!
    | StartParens, xs -> StartParens::xs
    | PopParens, expr::StartParens::xs -> build expr xs
    | PopParens, xs -> build PopParens (relaxLevel 0 xs)
    | _, _ -> failwithf "Invalid build item %A for stack: %A" b buildStack

let getFinalExpr stack =
    match (relaxLevel 0 stack) with
    | BuiltExpr x::[] -> x
    | xs -> failwithf "Unresolved stack : %A" xs

let rec buildExpTree stack tokens =
    match tokens with
    | [] -> getFinalExpr stack // close out everything on the stack
    | Num x::tokens -> buildExpTree (build (BuiltExpr (Number x)) stack) tokens
    | Plus::tokens -> buildExpTree (build (BuildOp Add) stack) tokens
    | Star::tokens -> buildExpTree (build (BuildOp Mul) stack) tokens
    | OpenP::tokens -> buildExpTree (build StartParens stack) tokens
    | CloseP::tokens -> buildExpTree (build PopParens stack) tokens

let rec evaluate exprTree =
    match exprTree with
    | LtoR (Add, expr1, expr2) -> (evaluate expr1) + (evaluate expr2)
    | LtoR (Mul, expr1, expr2) -> (evaluate expr1) * (evaluate expr2)
    | Number x -> x

let evaluateLine line =
    tokenize line |> buildExpTree [] |> evaluate

readInput "day18.txt" |> Seq.map evaluateLine
|> Seq.map int64
|> tee (Seq.iteri (printfn "%d: %A"))
|> Seq.sum
|> printfn "%d"
