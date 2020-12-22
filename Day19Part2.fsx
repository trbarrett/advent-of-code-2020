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

// can we generate a state machine that can look at an input and see if it's valid
// first letter comes in, what can that apply to?

//0: 4 1 5
//1: 2 3 | 3 2
//2: 4 4 | 5 5
//3: 4 5 | 5 4
//4: "a"
//5: "b"

// first letter "a", what are our options?
// (0.0), (2.0), (3.0)
// transitions from there?
// another "a"
// -> (2.1) which = (1.0)
// // (for 0: 1 is not something we can bank yet but it might be an option, so
// // can we put it in a holding pattern to see if it can be matched?
// // I guess, but how would we know if the match fails?, and are we sub matching
// // on 1? That's the state machine thing I guess. How many letters are we considering at once?
// next option (b) "aab"
// -> (1.1) ==

// *, 'a', 'b' could also be a '3'


// any letter 'a' or 'b', we can find what that could apply to?
// day
// now a combination of two, 'ab' what options do we have then?

// maps the letters to the rules they are in
//letterMap  = ('a', 1); ('b', 14);

// maps partial or full matches to rules
//allMap =
//   [[1], [Part 9; Part 5; Part 6; Part 2; Full 15; Part 17; Part 4; Part 20; 27; 25; 26; 7])
//   ([14], [9; 19; 16; 31; 6; 2; 13; 17; 20; 27; 21; 22; 26; 7; 14])
//   ([1 1], [Full 4])
//   ([14 14], [Full 19; Full 6; Full 20; ])
//   ([1 14], [5; 6; 21; ])
//   ([14 1], [19; 21; 24])
//   ([15], [5; 16; 18;])
//   ([15 15], [18;])
// etc

// I think we want the Part matches, just to know we can continue matching. I
// don't think we need to do anything with them?

type RuleNo = int

type MatchType = | Full of RuleNo | Part of RuleNo seq

// Not sure we want the ForRule here
type Consideration = { Start: int; Next: int; RuleParts: RuleNo list }
type RuleMatch = { Start: int; Next: int; Rule: RuleNo }

type Intermediary =
    | PartialMatch of Consideration
    | FullMatch of RuleMatch

type Item =
    | Incomplete of Consideration
    | Complete

// We might have to promote full ones multiple times.
// 0 : 1
// 1 : 2
// 2 : 3 3
// 3 : 'a'
// if we get the second 'a' for 2, we need to promote to 1, then promote to 0

let rec tryPromote consideration =
    allMap |> Map.tryFind consideration.ForRule

// promoted rules need to be compared against existing rules as well though...
// if we complete 3 : 4 5 , and we have a 2 : 4 3, and the 4 for that rule 2 has
// already been found, then we need to match against that 3 to complete 2.
// And now if we've completed 2, we need to match again potentially.

// Sounds like we need a loop, and an intermediary state to know about matches

let processNext letter idx partials =
   // If any of the partials was a "Complete Match" (match against rule 0),
   // now that we have another letter we have to throw it away

   // if we have no partials, and idx > 0, then we'll never get a match and
   // we can end now

    let letterRule = letterMap |> Map.find letter

    // could "partials" contain full matches?

    // we probably don't need this, we could just add the the new letter matches
    // to the partials argument
    let newMatches =
        allMap |> Map.tryFind letterRule
        |> List.map (fun k matchingRules ->
            match m with
            | Part _ -> PartialMatch { Start = idx; Next = idx + 1; RuleParts = [letterRule] }
            | Full 0 -> CompleteMatch
            | Full ruleNo -> FullMatch { Start = idx; Next = idx + 1; Rule = ruleNo })

    let matches =
        partials |> List.collect (fun x ->
            // we can only complete partial matches against the next index
            if s.Next <> idx
            then [x] // keep the original for future matches
            else
                let possible = s@[letterRule]
                match allMap |> Map.tryFind possible with
                | None -> [x] // keep the original for future matches
                | Some matches ->
                    matches |> List.map (fun m ->
                        match m with
                        | Part _ -> PartialMatch { Start = idx; Next = idx + 1; RuleParts = possible }
                        | Full 0 -> CompleteMatch
                        | Full ruleNo -> FullMatch { Start = idx; Next = idx + 1; Rule = ruleNo } )
                    @ [x] // I think we might want to keep the original? Not sure. It will fall out later anyway but there may be an optimization here
            )

                    // take the new ones, and set the correct indexes
                    // if any was a full match we need to promote it
    // in a loop:
        // for each full match:
        // Create a new partial and/or combine with an existing at the correct spot
        // keep looping until we run out of full matches to handle

    matches |> Seq.collect (fun x ->
        match x with
        | CompleteMatch -> x
        | PartialMatch x -> [x]
        | FullMatch m ->
            // new rules
            match allMap |> Map.tryFind m.Rule with
            | Part _ -> PartialMatch { Start = m.Start; Next = m.Next; RuleParts = m.Rule }
            | Full 0 -> CompleteMatch
            | Full ruleNo -> FullMatch { Start = m.Start; Next = m.Next; Rule = ruleNo }
            ::
            // extendExistingRules - this is almost identical to what we do when
            // adding a new letter. We should try to reuse the code
            matches
            |> Seq.filter (fun existing -> existing.Next = m.Start)
            |> Seq.collect (fun existing ->
                let possible = existing.RuleParts @ [m.Rule]
                match allMap |> Map.tryFind possible with
                | None -> []
                | Some matches ->
                    matches |> List.map (fun y ->
                        match y with
                        | Part _ -> PartialMatch { Start = y.Start; Next = m.Next; RuleParts = possible }
                        | Full 0 -> CompleteMatch
                        | Full ruleNo -> FullMatch { Start = y.Start; Next = m.Next; Rule = ruleNo } )
                )

    )

    // convert partials to incomplete
    let incompletes =
        matches
        |> Seq.choose (function | Complete -> None | Incomplete x -> Some x)

    // remove any invalid ones
    // that is, any with a next Index (less < idx) that don't match any
    // start indexes (we should work backwards, recusivley (aka foldBack)
    incompletes
    |> Seq.filter (fun x -> x.Next < idx) // only look at ones that should have matched already
    |> Seq.sortBy (fun x -> x.Start)
    |> Seq.fold (fun acc x ->
        // if we have a start, that doesn't match a next then we will never be
        // able to attach that start
        if acc |> Seq.exists (fun y -> y.Next = x.Start)
        then x::acc
        else acc  ) []


let processNext letter partials =
    let matches =
        partials |> List.choose (fun s ->
            let possible = s + letter
            match allMap |> Map.tryFind possible with
            | None -> None
            | Some (PartialMatch partialRule) -> Some (PartialMatch partialRule)
            | Some (FullMatch rule) -> Some (FullMatch rule) )
    // Hmmm. A partial could be looking for different level rules, in which
    // case checking with an extra letter won't work for all of them.
    // how do we know if that fails???
    // 1 : 2 3
    // 3 : 4 4
    // 4 : 'a'
    // if we have a '2', and we get an 'a', we don't want to throw away the
    //possibility of a '1'. We want to keep the '1' possibility around.
    // but how do we know when to throw away options????
    // We'll if we get 4, 4, 4 we know to throw away option 3.
    // If we get a '2', '2' can we throw away option '1'. Do they have different
    // "levels?" and figure it out that way? Is the number equal to a level?
    //   We could possibly use a graph to figure out levels. (maybe, though loops
    //   could throw a spanner into that)
    // If we never throw a rule away is it a big deal?
    //     - we can try that to start with!!!!

    // changing ideas,
    // if we have
    // rules:
    // 1 : 2 5
    // 2 : 4 4
    // 4 : 'a'
    // 5 : 'b'
    // and we don't throw away rules
    // whats to stop '1' from matching 'aaab'? the 'aa' matches 2, and b matches 5
    // we need to ensure the 'a' in the middle is not forgotten. We can't just have
    // '1' skipping items to match the last '5'

    // try to elevate full match rules into partial match rules
    // I think it's ok if we can't??? that means we must have reached the
    // final/root rule and we can treat that differently I think???
    let (fullMatches, partialMatches) = List.partition isFullMatch

    // elevate - A partial could be part of multiple rules
    fullMatches |> List.collect (fun s ->




let (rules, messages) = readInput "day19-2.txt" |> Seq.toList |> parse
let maxLength = messages |> Seq.map (fun m -> m.Length) |> Seq.max
rules |> generateAllPossibleMsgs maxLength |> Seq.length |> (printfn "%A")

let maxMessageLength = messages |> Seq.map (fun m -> m.Length) |> Seq.max
maxMessageLength |> (printfn "maxMessageLength: %A")
//let allPossible = rules |> generateAllPossibleMsgs //|> Seq.length |> (printfn "%A")
//allPossible |> Set.count |> (printfn "%A")
//messages |> Seq.filter (fun m -> allPossible |> Set.contains m)
//|> Seq.length |> (printfn "%A")
