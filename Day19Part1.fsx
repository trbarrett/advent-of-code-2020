#load "./Helper.fsx"
open Helper
open System

type RuleCase =
    | SubRules of int seq
    | StringMatch of string

type Rule = { RuleNo: int; Groups: RuleCase seq  }

let parseRuleSubGroup (str : string) =
    if str.Contains('"')
    then StringMatch (str.Trim().Trim('"'))
    else SubRules (str.Split(' ') |> Seq.filter ((<>) "")|> Seq.map int)

let rec parseRules (lines : string list) acc =
    match lines with
    | ""::xs -> (acc, xs)
    | line::xs ->
        let [| ruleNo; rules |] = line.Split(':')
        let groups = rules.Split('|')
        let rule = { RuleNo = int ruleNo
                     Groups = groups |> Seq.map parseRuleSubGroup }
        parseRules xs (rule::acc)

let parse (lines : string list) =
    let lines = lines |> List.map (fun str -> (str.Trim()))
    let (rules, lines) = parseRules lines []
    (rules, lines)

let generateAllPossibleMsgs rules =
    let ruleMap = rules |> Seq.map (fun r -> r.RuleNo, r.Groups) |> Map
    let rec generateMatches ruleNo (cache : Map<int, string list>) =
        match cache |> Map.tryFind ruleNo with
        | Some cached -> cached, cache
        | None ->
            let ruleGroups = ruleMap |> Map.find ruleNo
            ruleGroups |> Seq.fold (fun (groupResults, cache) r ->
                match r with
                | StringMatch s -> s::groupResults, cache |> Map.extendListValues ruleNo [s]
                | SubRules subRules ->
                    // each sub-rule could have multiple results
                    let (results, cache) =
                        subRules |> Seq.fold (fun ((results : string list), cache) rNo ->
                            // we can get multiple results from the generate
                            let (subResults, cache) = generateMatches rNo cache
                            // we have to combine them with the previous list
                            // in each possible combination
                            let combinations = List.allPairs results subResults
                            let results = combinations |> List.map (fun (a, b) -> a + b)
                            results, cache
                        ) ([""], cache)
                    // each of the results is combined into a string
                    results@groupResults, cache |> Map.extendListValues ruleNo results
                ) ([], cache)
    generateMatches 0 Map.empty |> fst |> Set


let (rules, messages) = readInput "day19.txt" |> Seq.toList |> parse
let allPossible = rules |> generateAllPossibleMsgs //|> Seq.length |> (printfn "%A")
allPossible |> Set.count |> (printfn "%A")
messages |> Seq.filter (fun m -> allPossible |> Set.contains m)
|> Seq.length |> (printfn "%A")
