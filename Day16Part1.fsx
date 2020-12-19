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

let findInvalid fieldRules yourTicket nearbyTickets =
    nearbyTickets |> Seq.choose (findInvalidField fieldRules)

readInput "day16.txt" |> Seq.toList |> parse
|||> findInvalid |> Seq.sum |> printfn "%d"
