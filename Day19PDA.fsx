open System

#load "./Helper.fsx"
open Helper

// A CFG -> PDA and PDA evaluator to solve both Day19 questions

// Evaluating a PDA with this code is quite slow. Too slow when processing
// several hundred messages. I need to find a way to optimse it. By compiling it
// into a lookup table probably

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

/////////////////////////////////
/// PDA types
////////////////////////////////

type StackSymbol =
    | In of Input
    | BottomOfStack
    | RuleNo of int

type Transition =
    { In : Input
      Pop : StackSymbol option
      Push : StackSymbol option
      Target : Guid }

let symbolPopTransition pop target =
    { In = Epsilon
      Pop =
          if pop = In Epsilon
          then None
          else Some pop
      Push = None
      Target = target }

let symbolPushTransition symbol target =
    { In = Epsilon
      Pop = None
      Push =
          if symbol = In Epsilon
          then None
          else Some symbol
      Target = target }

let inputPopTransition input target =
    { In = input
      Pop =
          if input = Epsilon
          then None
          else Some (In input)
      Push = None
      Target = target }

type State =
    { Id : Guid
      Name : string
      Transitions : Transition list
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
/// Debugging
////////////////////////////////

let inputToString = function
    | A -> "a"
    | B -> "b"
    | Epsilon -> "ε"

let symbolToString = function
    | RuleNo ruleNo -> string ruleNo
    | BottomOfStack -> "$"
    | In input -> inputToString input

let symbolOptToString = function
    | None -> "ε"
    | Some symbol -> symbolToString symbol

let transitionToString trans =
    sprintf "(%s, %s->%s) -> %s"
        (inputToString trans.In)
        (symbolOptToString trans.Pop)
        (symbolOptToString trans.Push)
        (trans.Target.ToString())

let printStateTransition state trans =
    match state.Accepting with
    | false ->
        printfn "%A  %s  %s" state.Id state.Name (transitionToString trans)
    | true ->
        printfn "%A [%s] %s" state.Id state.Name (transitionToString trans)

let printState state =
    if state.Transitions = List.empty then
        printfn "%A [%s] (No Transitions)" state.Id state.Name
    else
        state.Transitions
        |> List.iter (fun trans -> printStateTransition state trans)

let printStates states =
    states |> List.iter printState


/////////////////////////////////
/// Build the PDA
////////////////////////////////

let createPDABase baseRuleNo =
    let finalState = newAcceptingState "Final"
    let finalTransition = symbolPopTransition BottomOfStack finalState.Id
    let mainLoopState = newState "Main" [ finalTransition ]
    let inputPopTransitions =
        [ inputPopTransition A mainLoopState.Id
          inputPopTransition B mainLoopState.Id ]
    let mainLoopState =
        { mainLoopState with
            Transitions = mainLoopState.Transitions @ inputPopTransitions }
    let pushStartingRuleTransition =
        symbolPushTransition (RuleNo baseRuleNo) mainLoopState.Id
    let pushStartingRuleState =
        newState "StartingRule" [ pushStartingRuleTransition ]
    let startPDATransition =
        symbolPushTransition BottomOfStack pushStartingRuleState.Id
    let startPDAState = newState "StartPDA" [ startPDATransition ]
    let states =
        [ finalState ; mainLoopState; pushStartingRuleState; startPDAState ]
    (startPDAState, mainLoopState, states)

let caseToString rule case =
    match case with
    | Input x -> sprintf "%i->%s" rule (inputToString x)
    | SubRules rules ->
        rules
        |> Seq.map (fun r -> r.ToString())
        |> fun strs -> String.Join(".", strs)
        |> sprintf "%i->%s" rule

let addOrUpdateState newState states =
    let found = states |> List.exists (fun s -> s.Id = newState.Id)
    match found with
    | false -> newState::states
    | true ->
        states |> List.map (fun s ->
            if s.Id = newState.Id
            then newState
            else s)

let addOrUpdateStates newStates states =
    (newStates, states)
    ||> List.foldBack addOrUpdateState

let addRuleCaseToPDA ruleNo (case : RuleCase) (mainLoopState, existingStates) =
    let caseStr = caseToString ruleNo case
    let (ruleCaseFirstState, newStates) =
        match case with
        | Input x ->
            // we push the input we want onto the stack so it can be matched
            // on the main loop when reading the input stream
            let transition = symbolPushTransition (In x) mainLoopState.Id
            let inputState =
                newState
                    (sprintf "%s-(%s)" caseStr (inputToString x))
                    [ transition ]
            inputState, [inputState]
        | SubRules rules ->
            let subRuleStateBuilder (nextState, states) ruleNo  =
                let transition = symbolPushTransition (RuleNo ruleNo) nextState.Id
                let ruleState =
                    newState (sprintf "%s-(%d)" caseStr ruleNo) [ transition ]
                ruleState,  ruleState::states
            // fold builds backwards, but that's what we want. We want the first
            // item to be on the top of the stack so we can match it with the
            // next input. So by building backwards we'll match the input as we
            // pop it off the stack
            ((mainLoopState, []), rules)
            ||> Seq.fold subRuleStateBuilder

    // we want to start by popping the rule we are interested in
    let startCaseTransition =
        symbolPopTransition (RuleNo ruleNo) ruleCaseFirstState.Id
    //let startCaseState =
    //    newState (sprintf "%s-(start)" caseStr) [ startCaseTransition ]
    //let newStates = startCaseState::newStates
    //let mainToSTartCaseTransition =
    //    symbolPopTransition (RuleNo ruleNo) ruleCaseFirstState.Id

    let mainLoopState =
        { mainLoopState with
            Transitions = startCaseTransition::mainLoopState.Transitions }
    (mainLoopState,
     existingStates
     |> addOrUpdateStates newStates
     |> addOrUpdateState mainLoopState)


let addRuleToPDA (mainLoopState, states) rule =
   (rule.Groups, (mainLoopState, states))
   ||> Seq.foldBack (addRuleCaseToPDA rule.RuleNo)

let buildPDA rules =
    let (startPDAState, mainLoopState, states) = createPDABase 0
    let _, states =
        ((mainLoopState, states), rules)
        ||> List.fold addRuleToPDA
    (startPDAState, states)

let convertStringToInput (str : string) =
    str
    |> Seq.map (fun ch ->
        match ch with
        | 'a' | 'A' -> A
        | 'b' | 'B' -> B
        | _ -> failwithf "Unsupported input type %c" ch)
    |> Seq.toList

//////////////////////////////////////////
/// Evaluate an input against the PDA
//////////////////////////////////////////

type RunState =
    { State : State
      Stack : StackSymbol list }

type ShouldPopStack =
    | PopStack
    | DontPop

let doNonInputTransitions topOfStack transitions (pdsStates : Map<Guid, State>) =
    transitions
    |> List.collect (fun t ->
        match t.In, t.Pop with
        | Epsilon, None ->
            [Map.find t.Target pdsStates, t.Push, DontPop]
        | Epsilon, Some (In (Epsilon)) ->
            [Map.find t.Target pdsStates, t.Push, DontPop]
        | Epsilon, Some symbol when Some symbol = topOfStack ->
            [Map.find t.Target pdsStates, t.Push, PopStack]
        | _ -> [])
    |> Set

let rec doAllNonInputTransitions pdsStates (runStates : Set<RunState>) =
    let next =
        runStates
        |> Set.collect (fun s ->
            let results =
                doNonInputTransitions
                    (List.tryHead s.Stack)
                    s.State.Transitions
                    pdsStates
            results
            |> Set.map (fun (nextState, push, pop) ->
                let newStack =
                    match pop with
                    | PopStack -> s.Stack |> List.tail
                    | DontPop -> s.Stack
                { State = nextState
                  Stack =
                      match push with
                      | None
                      | Some (In Epsilon) -> newStack
                      | Some (symbol) -> symbol::newStack }))
        |> Set.union runStates
    if next = runStates
    then next
    else doAllNonInputTransitions pdsStates next

let doInputTransitions input (pdaStates : Map<Guid, State>) runState =
    runState.State.Transitions
    |> List.choose (fun t ->
        match input = t.In with
        | true ->
            match runState.Stack, t.Pop with
            | _, None ->
                Some { State = (Map.find t.Target pdaStates)
                       Stack = runState.Stack }
            | (head::xs), Some pop when head = pop ->
                Some { State = (Map.find t.Target pdaStates)
                       Stack = xs }
            | _,_ -> None
        | false -> None )
    |> Set

let rec doAllInputTransitions input pdaStates runStates =
    runStates |> Set.collect (doInputTransitions input pdaStates)

let printRunState runState =
    printfn "State : %s" runState.State.Name
    printfn "Stack : %A" runState.Stack

let rec doesPDAAcceptInput' pdaStates input runStates =
    //printfn "Run Iteration: "
    //runStates |> Set.iter printRunState
    let runStates = doAllNonInputTransitions pdaStates runStates
    //printfn "After NonInput Transitions:"
    //runStates |> Set.iter printRunState
    match input with
    | [] ->
        // once we reach the end of the input, we accept it if any of the states
        // are an accepting state. Note in PDAs the stack does not need to be
        // empty to accept
        runStates |> Set.exists (fun x -> x.State.Accepting)
    | x::xs ->
        let runStates = doAllInputTransitions x pdaStates runStates
        if Set.isEmpty runStates
        then false // end immediately if there are no valid transitions for the input
        else doesPDAAcceptInput' pdaStates xs runStates

let doesPDAAcceptInput startState pdaStates input =
    let runStates = Set [ { State = startState; Stack = [] } ]
    let pdaStates = pdaStates |> List.map (fun s -> s.Id, s) |> Map
    doesPDAAcceptInput' pdaStates input runStates

let (rules, messages) = readInput "day19.txt" |> Seq.toList |> parse

let sw = System.Diagnostics.Stopwatch.StartNew()
let (pdaStartState, pdaStates) = buildPDA rules
sw.Stop()
//printStates pdaStates
printfn "Build PDA took %d ms" sw.ElapsedMilliseconds

sw.Reset()
sw.Start()
let acceptingMessages =
    messages
    |> Seq.map convertStringToInput
    |> Seq.filter (fun m ->
        doesPDAAcceptInput pdaStartState pdaStates m
        |> tee (fun x -> printfn "Message: %A is accepted: %A" m x))

acceptingMessages |> Seq.length |> (printfn "Accept Count: %A")
sw.Stop()
printfn "Run NFA on all msgs took %d ms" sw.ElapsedMilliseconds
