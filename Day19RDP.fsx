#load "./Helper.fsx"
open Helper

// Recursive Decent Parser with Backtracking
// It works, but as it's my first attempt at writing this I don't imagine it's
// all that good. There's probably a much more elegant way of dealing with the
// stacks and organising the code

type Input = | A | B

type RuleCase =
    | SubRules of int list
    | Terminal of char

type Production = { RuleNo: int; Groups: RuleCase list  }

let parseRuleSubGroup (str : string) =
    if str.Contains('"')
    then Terminal (str.Trim().Trim('"').Chars(0))
    else SubRules (str.Split(' ') |> Seq.filter ((<>) "") |> Seq.toList |> List.map int)

let rec parseRules (lines : string list) acc =
    match lines with
    | [] -> (acc, [])
    | ""::xs -> (acc, xs)
    | line::xs ->
        let [| ruleNo; rules |] = line.Split(':')
        let groups = rules.Split('|') |> Seq.toList
        let rule = { RuleNo = int ruleNo
                     Groups = groups |> List.map parseRuleSubGroup }
        parseRules xs (rule::acc)

let parse (lines : string list) =
    let lines = lines |> List.map (fun str -> (str.Trim()))
    let (rules, lines) = parseRules lines []
    (rules, lines)

// Recursive Decent Parser

type AndGroupStackItem =
    { InputPos : int
      Production : int
      CaseNo : int
      CasePos : int }

type NewProductionStackItem =
    { InputPos : int
      Production : int }

type OrGroupStackItem =
    { InputPos : int
      Production : int
      CaseNo : int }

type RDPStackItem =
    | NewProduction of NewProductionStackItem
    | OrGroup of OrGroupStackItem
    | AndGroup of AndGroupStackItem

type BacktrackStackItem =
    { RunStack : RDPStackItem list }

type ProductionResult =
    | Success of int // inputPos
    | Failure
    | ExpandProductionStep of RDPStackItem

let stepProduction rules ruleNo caseNo casePos (input : char list) inputPos =
    let production = rules |> Seq.find (fun x -> x.RuleNo = ruleNo)
    let case = production.Groups |> Seq.item caseNo
    match case with
    | Terminal c ->
        if casePos > 0 then
            failwith "Shouldn't have more than 1 position for a terminal"
        else
            if inputPos >= List.length input
            then Failure // we've run out of input to process!
            else
                if input.[inputPos] = c
                then Success (inputPos + 1)
                else Failure // no match
    | SubRules xs ->
        if casePos >= xs.Length then
            failwith "continueProduction should never try to continue past the end"
        else
            // we need to step inside the production
            ExpandProductionStep
                ( NewProduction
                    { InputPos = inputPos
                      Production = xs.[casePos] })

let prepProduction rules ruleNo inputPos runStack =
    let production = rules |> Seq.find (fun x -> x.RuleNo = ruleNo)

    if List.length production.Groups = 1 then
        // don't bother with an Or group if there's just one subgroup
        [ AndGroup { InputPos = inputPos
                     Production = ruleNo
                     CaseNo = 0
                     CasePos = 0 } ],
        []
    else
        // if there's more than one sub rule, we need to keep track of that
        // and the choice we make
        let newStackItems =
            [ OrGroup { InputPos = inputPos
                        Production = ruleNo
                        CaseNo = 0 } ]
        newStackItems,
        [ { RunStack = newStackItems@runStack } ]

type OnSuccessResult =
    | NewStack of RDPStackItem list
    | CompletedCFG

let rec onSuccess rules inputPos runStack =
    match runStack with
    | [] -> CompletedCFG
    | stackHead::stackTail ->
        match stackHead with
        | NewProduction x ->
            onSuccess rules inputPos stackTail
        | OrGroup x ->
            // if we got a successful OR Group item we continue with the
            // next part of the CFG to process. We don't want to test different
            // or cases. We will do that on failure and backtracking
            onSuccess rules inputPos stackTail
        | AndGroup x ->
            let production = rules |> Seq.find (fun rule -> rule.RuleNo = x.Production)
            match production.Groups.[x.CaseNo] with
            | Terminal _ ->
                onSuccess rules inputPos stackTail
            | SubRules subRules ->
                if x.CasePos = List.length subRules - 1 then
                    // All the rules in the sub-production were successful
                    // continue from the tail
                    onSuccess rules inputPos stackTail
                else
                    // Once of the rules in the And condition was successful,
                    // but we need to continue checking the rest
                    let newHead =
                        AndGroup
                            { InputPos = inputPos
                              Production = x.Production
                              CaseNo = x.CaseNo
                              CasePos = x.CasePos + 1 }
                    NewStack (newHead::stackTail)


type BacktrackResult =
    | NewState of RDPStackItem list * BacktrackStackItem list
    | NothingMoreToTry

let rec failureBacktrack rules backtrackStack =
    match backtrackStack with
    | backtrackHead::backtrackTail ->
        match backtrackHead.RunStack with
        | stackHead::history ->
            // the top of the history stack will be the next item we will
            // try. It must be a OrGroupStackItem, because of the way we add
            // items to the history.
            match stackHead with
            | OrGroup prev ->
                let production = rules |> Seq.find (fun x -> x.RuleNo = prev.Production)
                if prev.CaseNo = List.length production.Groups - 1 then
                    // if we've already tried all the cases, we need to backtrack further
                    failureBacktrack rules backtrackTail
                else
                    // otherwise we need to move the caseNo forward 1 so we try
                    // the next item
                    let newStackItem =
                        OrGroup { InputPos = prev.InputPos
                                  Production = prev.Production
                                  CaseNo = prev.CaseNo + 1 }

                    // and update the backtracking history, so we can continue
                    // from the correct point
                    let updatedBacktrackItem =
                        { RunStack = newStackItem::backtrackHead.RunStack }

                    NewState (newStackItem::history, updatedBacktrackItem::backtrackTail)
            | _ -> failwith "Only OrGroups should be existing for backtrack histories"
        | _ -> failwith "Empty history for backtrack stack isn't valid"
    | [] -> NothingMoreToTry // Nothing to backtrack to, no match

type RDPResult =
    | SuccessfulMatch
    | NoCFGMatch

let rec rdp' rules (input : char list) runStack (backtrackStack : BacktrackStackItem list) =
    match runStack with
    | NewProduction x::xs ->
        let (newStackItems, backtrackAdditions)
            = prepProduction rules x.Production x.InputPos runStack
        rdp' rules input (newStackItems@xs) (backtrackAdditions@backtrackStack)
    | OrGroup x::_ ->
        let newStackHead =
            AndGroup { InputPos = x.InputPos
                       Production = x.Production
                       CaseNo = x.CaseNo
                       CasePos = 0 }
        let newStack = newStackHead::runStack
        rdp' rules input newStack backtrackStack
    | AndGroup x::_ ->
        let result =
            stepProduction rules x.Production x.CaseNo x.CasePos input x.InputPos
        match result with
        | ExpandProductionStep newStackItem ->
            rdp' rules input (newStackItem::runStack) backtrackStack
        | Success inputPos ->
            match onSuccess rules inputPos runStack with
            | NewStack newStack -> rdp' rules input newStack backtrackStack
            | CompletedCFG ->
                // = length because we have moved one when matching the final terminal
                if inputPos = List.length input
                then SuccessfulMatch
                else
                    // We haven't used up all the input, so we failed. We need
                    // to backtrack and try another OR branch
                    let result = failureBacktrack rules backtrackStack
                    match result with
                    | NewState (newStack, backtrackStack) ->
                        rdp' rules input newStack backtrackStack
                    | NothingMoreToTry -> NoCFGMatch
        | Failure ->
            let result = failureBacktrack rules backtrackStack
            match result with
            | NewState (newStack, backtrackStack) ->
                rdp' rules input newStack backtrackStack
            | NothingMoreToTry -> NoCFGMatch
    | [] -> failwith "Should never encounter an empty stack here"


let rdp rules input =
    let startStack =
        [ NewProduction
            { InputPos = 0
              Production = 0 } ]

    rdp' rules input startStack []

let part1 =
    let (rules, messages) = readInput "day19.txt" |> Seq.toList |> parse
    messages
    |> Seq.map (fun m -> rdp rules (m |> Seq.toList))
    |> Seq.filter ((=) SuccessfulMatch)
    |> Seq.length

part1 |> (printfn "Part1: %A")

let part2 =
    let (rules, messages) = readInput "day19-2.txt" |> Seq.toList |> parse
    messages
    |> Seq.map (fun m -> rdp rules (m |> Seq.toList))
    |> Seq.filter ((=) SuccessfulMatch)
    |> Seq.length

part2 |> (printfn "Part2: %A")
