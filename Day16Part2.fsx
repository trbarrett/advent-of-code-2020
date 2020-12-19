#load "./Helper.fsx"
open Helper
open System.Text.RegularExpressions

type RuleRange = int * int
module RuleRange =
    let contains value (start, finish) = start <= value && value <= finish

type Rule = string * RuleRange * RuleRange
module Rule =
    let build name start1 end1 start2 end2 =
        name, RuleRange (int start1, int end1), RuleRange (int start2, int end2)
    let isValid value (_, range1, range2) =
        RuleRange.contains value range1 || RuleRange.contains value range2

let parseRulesSection lines =
    let rec parseRulesSection' rules lines =
        match lines with
        | ""::lines -> (rules, lines)
        | line::lines ->
            let m = Regex.Match(line, "^(.+): (\d+)-(\d+) or (\d+)-(\d+)$")
            let vs = m.Groups |> Seq.map (fun x -> x.Value) |> Seq.toArray
            let newRule = Rule.build vs.[1] vs.[2] vs.[3] vs.[4] vs.[5]
            parseRulesSection' (newRule::rules) lines
    parseRulesSection' [] lines

let parseYourTicket lines =
    let rec parseYourTicket' yourTicket lines =
        match lines with
        | ""::lines -> (yourTicket, lines)
        | "your ticket:"::lines -> parseYourTicket' [||] lines
        | line::lines -> parseYourTicket' (line.Split(',') |> Array.map int) lines
    parseYourTicket' [||] lines

let parseNearbyTickets lines =
    let rec parseNearby' nearbyTickets lines  =
        match lines with
        | [] -> (nearbyTickets)
        | ""::lines -> (nearbyTickets)
        | "nearby tickets:"::lines -> parseNearby' [] lines
        | line::lines ->
            parseNearby' ((line.Split(',') |> Array.map int)::nearbyTickets) lines
    parseNearby' [] lines

let parse lines =
    let (fieldRules, lines) = parseRulesSection lines
    let (yourTicket, lines) = parseYourTicket lines
    let (nearbyTickets) = parseNearbyTickets lines
    (fieldRules, yourTicket, nearbyTickets)

let findInvalidField fieldRules =
    Seq.tryFind (fun x -> fieldRules |> Seq.exists (Rule.isValid x) |> not)

let getValidTickets rules = Seq.filter (findInvalidField rules >> Option.isNone)

let findValidRules rules fields =
    let findValid rules field = rules |> Seq.filter (Rule.isValid field)
    fields |> Seq.fold (fun rules field -> findValid rules field ) rules

let rec simplifyMatchings remaingRules (matchings : (int * Rule list) list) =
    // find all the singleRules in matchings
    let singles =
        matchings
        |> List.choose (fun (_, xs) -> match xs with | x::[] -> Some x | _ -> None )
        |> Set
    // remove any singles from the remaining rules and matchings
    let remaingRules = remaingRules |> List.filter ((flip Set.contains) singles >> not)
    let newMatchings =
        matchings
        |> List.map (fun (i, rules) ->
            let newRules =
                if List.length rules = 1
                then rules // don't change where we are matching with just a single rule
                else rules |> List.filter ((flip Set.contains) singles >> not)
            (i, newRules))
    if matchings = newMatchings // we'll recurse only if we've changed the matchings
    then (remaingRules, matchings)
    else simplifyMatchings remaingRules newMatchings

let matchRulesWithTickets rules (tickets : (int array) seq)=
    let rec matcher remaingRules fieldNo matchings =
        let validRules =
            tickets |> Seq.map (fun x -> x.[fieldNo])
            |> findValidRules remaingRules |> Seq.toList
        let matchings = (fieldNo, validRules)::matchings
        let (remaingRules, matchings) = simplifyMatchings remaingRules matchings
        match remaingRules with
        | [] -> matchings // we can do this because there is a unique solution
        | _ -> matcher remaingRules (fieldNo+1) matchings

    matcher rules 0 []
    |> List.map (fun (i, matches) ->
        match matches with // we can do this because there is a unique solution
        | [singleMatch] -> (i, singleMatch)
        | _ -> failwithf "%d Should have a single match. Instead had %A" i matches)

let determinePart2Output yourTicket (matches : (int * Rule) list) =
    matches
    |> List.filter (fun (_, (name, _, _)) -> name.StartsWith("departure"))
    |> List.unzip |> fst
    |> List.map ((flip Array.item) yourTicket )
    |> List.map uint64 |> List.reduce (*)

let (rules, yourTicket, nearby) = readInput "day16.txt" |> Seq.toList |> parse

getValidTickets rules nearby
|> matchRulesWithTickets rules
|> determinePart2Output yourTicket
|> printfn "%A"
