open System

#load "./Helper.fsx"
open Helper

// A CFG -> PDA and PDA evaluator to solve both Day19 questions

// This extends the PDA version we built to add a lookup table. It's still rather
// slow, but much better than it was previously. There's a better way of building
// the lookup table I'm sure.
// - We could compress the PDA first. If there's a single epsilon transition add
// it to the previous transition (allowing multiple stack pushes)
// - We could change the lookup so that an input was required.
// You give it an input and your current state. And it would tell you what
// stack you would need to top pop.
// The builder would need to do smart things like see that:
// Push $ and 0, pop 0, input A can just be coverted to: input A -> push $


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
/// Convert PDA to a lookup table
//////////////////////////////////////////

// For each state in the PDA, determine what top-of-stack/input combinations go
// to what next state, with a pop or not.
// We can epsilon transition, for free, but once we add something to the stack
// we can't take additional input or pop the stack.

type EpsilonBuildState =
    { State : State
      Input : Input
      Popped : StackSymbol option
      PushStack : StackSymbol list }

type RuleBuildState =
    | PureEpsilon of EpsilonBuildState
    // once we've added an input, popped a symbol, or pushed a symbol
    // all we can do is extend the stack
    | StackExtendable of EpsilonBuildState

let getState (buildState : RuleBuildState) =
    match buildState with
    | PureEpsilon x -> x.State
    | StackExtendable x -> x.State

let getBuild (buildState : RuleBuildState) =
    match buildState with
    | PureEpsilon x -> x
    | StackExtendable x -> x

let findBuildTransitions (pdsStates : Map<Guid, State>) buildState =
    let transitions = (getState buildState).Transitions
    match buildState with
    | PureEpsilon _ ->
        transitions
        |> List.collect (fun t ->
            match t.In, t.Pop, t.Push with
            | Epsilon, None, None
            | Epsilon, Some (In (Epsilon)), None ->
                [PureEpsilon
                     { State = Map.find t.Target pdsStates
                       Input = Epsilon
                       Popped = None
                       PushStack = [] }]
            | Epsilon, None, Some symbol
            | Epsilon, Some (In (Epsilon)), Some symbol ->
                [StackExtendable
                     { State = Map.find t.Target pdsStates
                       Input = Epsilon
                       Popped = None
                       PushStack = [symbol] } ]
            | input, popSymbol, _ ->
                [StackExtendable
                     { State = Map.find t.Target pdsStates
                       Input = input
                       Popped = popSymbol
                       PushStack = match t.Push with
                                   | Some s -> [s]
                                   | None -> [] }] )
    | StackExtendable x ->
        transitions
        |> List.collect (fun t ->
            match t.In, t.Pop with
            // we can only do something with no input or pop at this point
            | Epsilon, None
            | Epsilon, Some (In (Epsilon)) ->
                [StackExtendable
                     { State = Map.find t.Target pdsStates
                       Input = x.Input
                       Popped = x.Popped
                       PushStack = match t.Push with
                                   | Some s -> s::x.PushStack
                                   | None -> x.PushStack }]
             | _ -> [])
    |> Set


let rec doAllBuildTransitions
    (pdsStates : Map<Guid, State>)
    (currentBuild : Set<RuleBuildState>)
    =
    let next =
        currentBuild
        |> Set.collect (findBuildTransitions pdsStates)
        |> Set.union currentBuild
        // TODO we need to remove pure subsets of push stacks
    if next = currentBuild
    then next
    else doAllBuildTransitions pdsStates next


type LookupInput =
    { StateId : Guid
      StateName : string
      Input : Input
      TopOfStack : StackSymbol option }

type LookupOutput =
    { StateId : Guid
      StateName : string
      PopStack : bool
      ConsumeInput : bool
      Accepting : bool
      PushStack : StackSymbol list }

type LookupTable = Map<LookupInput, LookupOutput list>

let rec getAllSubsets (xs : 'a list) subsets =
    if List.length xs > 1
    then
        let tail = List.tail xs
        getAllSubsets tail (tail::subsets)
    else subsets

let cleanSubsetInputs baseState (states : Set<EpsilonBuildState>) =
    states
    |> Seq.groupBy (fun x ->
        { StateId = baseState.Id
          StateName = baseState.Name
          Input = x.Input
          TopOfStack = x.Popped })
    |> Seq.map (fun (k, vs) ->
        k,
        vs
        |> Seq.groupBy (fun x -> (x.Input, x.Popped))
        |> Seq.collect(fun (_, buildStates) ->
            if Seq.length buildStates = 1
            then buildStates
            else
                // It isn't useful to have multiple cases that just differ
                // by the pushstack. Remove any matching subsets
                let possibleSubsets =
                    buildStates
                    |> Seq.collect (fun x -> getAllSubsets x.PushStack [])
                    |> Set

                buildStates
                |> Seq.filter (fun buildState ->
                    Set.contains buildState.PushStack possibleSubsets
                    |> not) ) )

let buildLookupForState pdaState (pdaStates : Map<Guid, State>) =
    let starting =
        PureEpsilon { State = pdaState; Input = Epsilon; Popped = None; PushStack = [] }
    //printfn "Build Lookup For State"
    doAllBuildTransitions pdaStates (Set [ starting ])
    //|> tee (Seq.iter (fun x -> printfn "%A" x))
    |> Set.remove starting // starting set isn't a valid transition
    |> Set.map getBuild
    |> cleanSubsetInputs pdaState
    |> Seq.map (fun (input, buildStates) ->
        input,
        buildStates
        |> Seq.toList
        |> List.map (fun buildState ->
            { StateId = buildState.State.Id
              StateName = pdaStates |> Map.find buildState.State.Id |> fun x -> x.Name
              PopStack = Option.isSome buildState.Popped
              ConsumeInput = buildState.Input <> Epsilon
              Accepting = buildState.State.Accepting
              PushStack = buildState.PushStack } ))
    |> Seq.toList

let buildLookupTable (pdsStates : Map<Guid, State>) =
    pdsStates
    //|> Map.filter (fun k v -> v.Name = "StartPDA")
    |> Map.toList
    |> List.collect (fun (_, state) ->
        buildLookupForState state pdsStates)
    |> Map


//////////////////////////////////////////
/// Evaluate an input against the PDA
//////////////////////////////////////////

type RunState =
    { StateId : Guid
      StateName : string
      Accepting : bool
      Stack : StackSymbol list }

let printRunState runState =
    printfn "State : %s" runState.StateName
    printfn "Stack : %A" runState.Stack

let doLookupTransitions
    (input : Input)
    (lookupTable : LookupTable)
    (runState : RunState) =

    // four possible start points for transitions,
    // some of them might be duplicates
    let possibleInputs =
        [ { StateId = runState.StateId
            StateName = runState.StateName
            Input = input
            TopOfStack = runState.Stack |> List.tryHead }
          { StateId = runState.StateId
            StateName = runState.StateName
            Input = Epsilon
            TopOfStack = runState.Stack |> List.tryHead }
          { StateId = runState.StateId
            StateName = runState.StateName
            Input = input
            TopOfStack = None }
          { StateId = runState.StateId
            StateName = runState.StateName
            Input = Epsilon
            TopOfStack = None }]
          |> Set

    possibleInputs
    |> Seq.choose(fun input -> lookupTable |> Map.tryFind input)
    |> Seq.collect id
    |> Seq.map (fun out ->
        let stack =
            if out.PopStack
            then List.tail runState.Stack
            else runState.Stack
        { runState with
            StateId = out.StateId
            StateName = out.StateName
            Accepting = out.Accepting
            Stack = out.PushStack@stack },
        out.ConsumeInput || out.Accepting)
    |> Set

let setPartition predicate set =
    ((Set.empty, Set.empty), set)
    ||> Set.fold (fun (setTrue, setFalse) x ->
        if predicate x
        then Set.add x setTrue, setFalse
        else setTrue, Set.add x setFalse)

let rec doAllInputTransitions input lookupTable runStates completed =
    //printfn "Do All Input Transitions"
    //runStates |> Set.iter printRunState
    let newStates =
        runStates
        |> Set.collect (doLookupTransitions input lookupTable)

    let consuming, incomplete =
        newStates |> setPartition snd

    let consuming = consuming |> Set.map fst
    let incomplete = incomplete |> Set.map fst

    // if a state does not consume the input, we need to recurse to get
    // to one that does
    let completed = Set.union completed consuming
    if Set.isEmpty incomplete
    then completed
    else doAllInputTransitions input lookupTable incomplete completed

let rec doesPDAAcceptInput' lookupTable input (runStates : Set<RunState>) =
    match input with
    | [] ->
        //printfn ""
        //printfn "Check if Accepting"
        let runStates =
            doAllInputTransitions Epsilon lookupTable runStates Set.empty
            |> Set.union runStates
        // DO ALL ACCEPTING TRANSITIONS?
        //runStates |> Set.iter printRunState
        // once we reach the end of the input, we accept it if any of the states
        // are an accepting state. Note in PDAs the stack does not need to be
        // empty to accept
        runStates |> Set.exists (fun x -> x.Accepting)
    | x::xs ->
        //printfn ""
        //printfn "Do next input: %s" (inputToString x)
        let runStates = doAllInputTransitions x lookupTable runStates Set.empty
        if Set.isEmpty runStates
        then false // end immediately if there are no valid transitions for the input
        else doesPDAAcceptInput' lookupTable xs runStates

let doesPDAAcceptInput startState lookupTable input =
    let runStates =
        Set [ { StateId = startState.Id
                StateName = startState.Name
                Stack = []
                Accepting = false } ]
    doesPDAAcceptInput' lookupTable input runStates

let (rules, messages) = readInput "day19-2.txt" |> Seq.toList |> parse

let sw = System.Diagnostics.Stopwatch.StartNew()
let (pdaStartState, pdaStates) = buildPDA rules
sw.Stop()
//printStates pdaStates
printfn "Build PDA took %d ms" sw.ElapsedMilliseconds

sw.Reset()
sw.Start()
let pdaStatesMap = pdaStates |> List.map (fun s -> s.Id, s) |> Map
let pdaLookupTable = buildLookupTable pdaStatesMap
pdaLookupTable
|> Map.toList
|> List.sort
//|> List.iter (fun (k, v) -> printfn "In %A \nOut %A" k v)
printfn "Build PDA lookup table took %d ms" sw.ElapsedMilliseconds

sw.Reset()
sw.Start()
let acceptingMessages =
    messages
    |> Seq.map convertStringToInput
    |> Seq.filter (fun m ->
        doesPDAAcceptInput pdaStartState pdaLookupTable m)
        //|> tee (fun x -> printfn "Message: %A is accepted: %A" m x))

acceptingMessages |> Seq.length |> (printfn "Accept Count: %A")
sw.Stop()
printfn "Run NFA on all msgs took %d ms" sw.ElapsedMilliseconds
