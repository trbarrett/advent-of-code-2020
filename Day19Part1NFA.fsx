open System

#load "./Helper.fsx"
open Helper

type Input = | A | B | Epsilon

type RuleCase =
    | SubRules of int seq
    | Input of Input

type Rule = { RuleNo: int; Groups: RuleCase seq  }

let parseRuleSubGroup (str : string) =
    if str.Contains('"')
    then
        match (str.Trim().Trim('"')) with
        | "a" -> Input A
        | "b" -> Input B
        | input -> failwithf "Unknown input: %s" input
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

type State =
    { Id : Guid
      Name : string
      Transitions : (Input * Guid) list
      Accepting : bool }

let newState name transitions =
    { Id = Guid.NewGuid ()
      Name = name
      Transitions = transitions
      Accepting = false }

let newAcceptingState name =
    { Id = Guid.NewGuid ()
      Name = name
      Transitions = []
      Accepting = true }

/////////////////////////////////
/// Build the NFA
////////////////////////////////

let buildNFAForCase ruleNo caseNo case rules buildRule =
    match case with
    | Input input ->
        // we have a starting state 'S' with a transition to a final state
        let finalState = newAcceptingState (sprintf "%i_%i-F" ruleNo caseNo)
        let transition = input, finalState.Id
        let startState = newState (sprintf "%i_%i-S" ruleNo caseNo) [transition]
        let nfa = [ startState; finalState ]
        nfa
    | SubRules ruleNos ->
        // lookup the rules, see if we have states for them and if not build them
        let nfas =
            ruleNos |> Seq.map (fun ruleNo -> buildRule ruleNo rules)

        let nfa =
            nfas
            |> Seq.reduce (fun nfa1 nfa2 ->
                let nfa2Start = List.head nfa2
                let nfa2StartEpsilonTransition = (Epsilon, nfa2Start.Id)
                let nfa1 =
                    nfa1
                    |> List.map (fun x ->
                        match x.Accepting with
                        | false -> x
                        | true ->
                            { Id = x.Id
                              Name = x.Name
                              Transitions =
                                  nfa2StartEpsilonTransition::x.Transitions
                              Accepting = false } )
                nfa1@nfa2)


        // add an extra start state to the group
        // - more for documentation than anything
        let startState = newState (sprintf "%i_%i-S" ruleNo caseNo)
                                  [Epsilon, (List.head nfa).Id]
        startState::nfa

let rec buildRule ruleNo rules =
    let rule = rules |> Map.find ruleNo
    let nfas =
        rule.Groups
        |> Seq.toList
        |> List.mapi (fun i case -> buildNFAForCase ruleNo i case rules buildRule)

    // we need to combine the rules, in parallel
    // create a new start state, and have it transition to the starts of
    // all the nfas
    let startTransitions =
        nfas
        |> List.map (fun nfa ->
            let nfaStart = List.head nfa
            (Epsilon, nfaStart.Id))

    let startState = newState (sprintf "%i-S" ruleNo) startTransitions

    let nfa = nfas |> List.collect id // combine all the nfas into one nfa
    let nfa = startState::nfa // and add our new start state to the beginning

    // Change all the accepting states to point to the new final state
    let finalState = newAcceptingState (sprintf "%i-F" ruleNo)
    let transitionToFinal = (Epsilon, finalState.Id)
    let nfa = nfa |> List.map (fun state ->
        match state.Accepting with
        | false -> state
        | true -> { Id = state.Id
                    Name = state.Name
                    Transitions = transitionToFinal::state.Transitions
                    Accepting = false })

    // finally add the new final state to the rest of the states
    nfa@[finalState]

let buildNFA rules =
    let rules = rules |> List.map (fun x -> x.RuleNo, x) |> Map
    // we always start with rule 0
    buildRule 0 rules

let convertStringToInput (str : string) =
    str
    |> Seq.map (fun ch ->
        match ch with
        | 'a' | 'A' -> A
        | 'b' | 'B' -> B
        | _ -> failwithf "Unsupported input type %c" ch)
    |> Seq.toList

let printStates nfa =
    let inputToString = function
        | A -> "A"
        | B -> "B"
        | Epsilon -> "ε"

    let printStateTransition state (transitionInput, transitionTarget) =
        match state.Accepting with
        | false ->
            printfn "%A  %s  (%s) -> %s" state.Id state.Name (inputToString transitionInput) (transitionTarget.ToString())
        | true ->
            printfn "%A [%s] (%s) -> %s" state.Id state.Name (inputToString transitionInput) (transitionTarget.ToString())

    let printState state =
        if state.Transitions = List.empty then
            printfn "%A [%s] (No Transitions)" state.Id state.Name
        else
            state.Transitions
            |> List.iter (fun trans -> printStateTransition state trans)

    nfa |> List.iter printState

/////////////////////////////////
/// Run the NFA
////////////////////////////////

let doEpsilonTransitions transitions (states : Map<Guid, State>)  =
    transitions
    |> List.collect (fun (input, target) ->
        match input with
        | Epsilon -> [Map.find target states]
        | _ -> [])
    |> Set

let rec doAllEpsilonTransitions (current : Set<State>) all =
    let next =
        current
        |> Set.collect (fun s -> doEpsilonTransitions s.Transitions all)
        |> Set.union current
    if next = current
    then next
    else doAllEpsilonTransitions next all

let doInputTransitions input transitions (states : Map<Guid, State>) =
    transitions
    |> List.choose (fun (transitionInput, target) ->
        match input = transitionInput with
        | true -> Some (Map.find target states)
        | false -> None)
    |> Set

let rec doAllInputTransitions input (current : Set<State>) all =
    current
    |> Set.collect (fun s -> doInputTransitions input s.Transitions all)

let rec doesNFAAcceptInput' nfa input current =
    // TODO: doing the epsilon transitions is a major candidate for memoization,
    // but converting to a DFA is probably a better option
    let current = doAllEpsilonTransitions current nfa
    match input with
    | [] ->
        // once we reach the end of the input, we accept it if any of the states
        // are an accepting state
        current |> Set.exists (fun x -> x.Accepting)
    | x::xs ->
        let current = doAllInputTransitions x current nfa
        if Set.isEmpty current
        then false // end immediately if there are no valid transitions
        else doesNFAAcceptInput' nfa xs current

let doesNFAAcceptInput nfa input =
    let states = Set [List.head nfa]  // head is always the start state
    let nfa = nfa |> List.map (fun s -> s.Id, s) |> Map
    doesNFAAcceptInput' nfa input states

let (rules, messages) = readInput "day19.txt" |> Seq.toList |> parse

let sw = System.Diagnostics.Stopwatch.StartNew()
let nfa = buildNFA rules
sw.Stop()
//printStates nfa
printfn "Build NFA took %d ms" sw.ElapsedMilliseconds

sw.Reset()
sw.Start()
let acceptingMessages =
    messages
    |> Seq.map convertStringToInput
    |> Seq.filter (fun m -> doesNFAAcceptInput nfa m)

acceptingMessages |> Seq.length |> (printfn "Accept Count: %A")
sw.Stop()
printfn "Run NFA on all msgs took %d ms" sw.ElapsedMilliseconds
