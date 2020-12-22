#load "./Helper.fsx"
open Helper
open System.Text.RegularExpressions

/// --- Cheeky Version of Day 19 ---
/// This is cheeky because we don't actually do the heavy lifting of matching
/// ourselves, instead we crate a regex pattern that will do it for us.
/// As you can see this "cheeky" version ended up being pretty long, and fairly
/// substantial by itself. The resulting regex is absolutely massive.

type RuleExpression =
    | SubRule of string seq
    | StringMatch of string
    | Group of RuleExpression seq

let tryGetStringMatch = function | StringMatch x -> Some x | _ -> None


type Rule = { RuleNo: string; Expr: RuleExpression }

let parseRuleSubGroup (str : string) =
    str.Split(' ') |> Seq.filter ((<>) "") |> SubRule

let rec parseRules (lines : string list) acc =
    match lines with
    | ""::xs -> (acc, xs)
    | line::xs ->
        let [| ruleNo; ruleExpr |] = line.Split(':')
        let rule =
            if ruleExpr.Contains('"') then
                { RuleNo = ruleNo
                  Expr = StringMatch (ruleExpr.Trim().Trim('"')) }
            else
                { RuleNo = ruleNo
                  Expr = Group (ruleExpr.Split('|') |> Seq.map parseRuleSubGroup) }
        parseRules xs (rule::acc)

let parse (lines : string list) =
    let lines = lines |> List.map (fun str -> (str.Trim()))
    let (rules, lines) = parseRules lines []
    (rules, lines)

let splitRuleTypes rules =
    rules |> List.fold (fun (stringMatches, otherRules) r ->
        match r.Expr with
        | StringMatch str -> (r.RuleNo, str)::stringMatches, otherRules
        | _ -> stringMatches, r::otherRules) ([], [])

let rec createExpandingSeqs left existing right acc =
    // regular expressions have no way to match the same about of repetitions of
    // two different groups, but we can be cheeky and just repeat them up to a
    // certain point
    let n = left + existing + right
    if String.length n > 40000 // magic number where we wouldn't get any more matches
    then "(" + (String.concat "|" acc) + ")"
    else createExpandingSeqs left n right (n::acc)

let replaceLoopNumber loopNo (subRule : string seq) =
    let turnToLoopStr xs =  (xs |> String.concat "") |> (sprintf "(%s)+")
    // we can cheat a bit here knowing what the possible inputs are
    let loopIndex = subRule |> Seq.findIndex ((=) loopNo)
    if loopIndex = (subRule |> Seq.length) - 1
    then subRule |> Seq.take loopIndex |> turnToLoopStr
    else
        let left = subRule |> Seq.take loopIndex |> String.concat ""
        let right = subRule |> Seq.skip (loopIndex + 1) |> String.concat ""
        createExpandingSeqs left "" right []

let rec convertToRegexp (rules : Rule list) =
    match rules with
    | [x] -> splitRuleTypes [x] |> fst |> List.head |> snd |> sprintf "^%s$"
    | _::_ ->
        let (stringRules, others) = splitRuleTypes rules

        let rec replaceExprParts ruleNo (replaceRuleNo, replacement) (ruleExpr : RuleExpression) : RuleExpression =
            match ruleExpr with
            | StringMatch _ -> ruleExpr
            | SubRule subRule ->
                let updated = subRule |> Seq.map (fun r ->
                    if r = replaceRuleNo then replacement else r)
                let numbers = updated |> Seq.choose Int32.tryParse
                if numbers |> Seq.isEmpty
                then StringMatch (updated |> String.concat "")
                else
                    if numbers |> Seq.forall ((=) (int ruleNo))
                    // we have discovered a loop. Replace it with looping regex
                    then StringMatch (replaceLoopNumber ruleNo updated)
                    else SubRule updated
            | Group grp ->
                let grp = grp |> Seq.map (replaceExprParts ruleNo (replaceRuleNo, replacement))
                let strGrp = grp |> Seq.choose tryGetStringMatch
                // if we replaced all the groups, we can just join them
                // and turn them into a string match
                if (grp |> Seq.length) = (strGrp |> Seq.length)
                then StringMatch ("(" + (strGrp |> String.concat "|") + ")")
                else Group grp

        let applyAllStringRules ruleNo (expr : RuleExpression) =
            stringRules |> Seq.fold (flip (replaceExprParts ruleNo)) expr

        others
        |> List.map (fun r -> { r with Expr = applyAllStringRules r.RuleNo r.Expr })
        |> convertToRegexp

let part1 =
    let (rules, messages) = readInput "day19.txt" |> Seq.toList |> parse
    let regexp = convertToRegexp rules |> Regex
    messages |> Seq.filter regexp.IsMatch |> Seq.length

let part2 =
    let (rules, messages) = readInput "day19-2.txt" |> Seq.toList |> parse
    let regexp = convertToRegexp rules |> Regex
    messages |> Seq.filter regexp.IsMatch |> Seq.length

part1 |> (printfn "Part1: %A")
part2 |> (printfn "Part2: %A")
