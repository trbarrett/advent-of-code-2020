#load "./Helper.fsx"
open Helper

// A complicated way of doing a RDP

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

let rec recursiveDecentParser ruleNo (rules : Production list) input =
    let rule = rules |> Seq.find (fun x -> x.RuleNo = ruleNo)
    checkGroups rule.Groups rules input

and checkSubRules subRules rules (input : char list) =
    match subRules with
    | r::xs ->
        let (isMatch, remainingInput) =
            recursiveDecentParser r rules input
        if isMatch then checkSubRules xs rules remainingInput
        else (false, input)
    | [] -> (true, input)

and checkGroups groups rules input =
    match input with
    | [] -> (false, input) // no more input to match against
    | input ->
        match groups with
        | x::xs ->
            let (result, remaining) =
                match x with
                | Terminal t -> (t = (input |> List.head), input |> List.tail)
                | SubRules s -> checkSubRules s rules input
            if result then (result, remaining) // the subgroup matched, exit
            else checkGroups xs rules input // subgroup didn't match, try next
        | [] -> (false, input) // no subgroup match

let recursiveDecent rules input =
    let start = rules |> Seq.find (fun x -> x.RuleNo = 0)
    checkGroups start.Groups input

let getMatch rules input =
    recursiveDecentParser 0 rules (input |> Seq.toList)

let (rules, messages) = readInput "day19.txt" |> Seq.toList |> parse

// Doesn't work for part 2
// - The course I watched mentions the issue I'm running into with part 2, but
// doesn't describe how to resolve it. I'm certain there is a way, but I would
// need to do some more research.
let mutable matchCount : int = 0
messages |> Seq.iteri (fun i m ->
    let (result, remaining) = getMatch rules m

    if result && Seq.length remaining = 0 then
        matchCount <- matchCount + 1
        printfn "Matched Input : %d" i
    elif result then
        printfn "Matched %d - But production not complete" i
    else
        printfn "No match %d" i
)

printfn "Matched Total = %d" matchCount
