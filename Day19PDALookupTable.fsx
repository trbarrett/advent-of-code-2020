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

type PushPopItem =
    | Push of StackSymbol
    | Pop of StackSymbol

type EpsilonBuildState =
    { State : State
      Input : Input
      PushPopStack : PushPopItem list
      Accepting : bool }

type RuleBuildState =
    | Complete of EpsilonBuildState
    | Building of EpsilonBuildState

let getState (buildState : RuleBuildState) =
    match buildState with
    | Complete x -> x.State
    | Building x -> x.State

let getBuild (buildState : RuleBuildState) =
    match buildState with
    | Complete x -> x
    | Building x -> x

let pushTransitionOnBuildState transitionPush buildState =
   { State = buildState.State
     Input = buildState.Input
     PushPopStack = Push transitionPush::buildState.PushPopStack
     Accepting = buildState.Accepting }

let popTransitionFromBuildState transitionPop buildState =
    match buildState.PushPopStack with
    // if the top of the stack has a push symbol that matches we can
    // cancel it out
    | (Push stackSymbol)::tail when stackSymbol = transitionPop ->
        Some
            { State = buildState.State
              Input = buildState.Input
              PushPopStack = tail
              Accepting = buildState.Accepting }
    // if the top of the stack has a push symbol and it doesn't match,
    // then this transition isn't valid
    | Push _::_ -> None
    // otherwise if the top of the stack doesn't have a pop symbol
    // we can pop this one
    | stack ->
        Some
            { State = buildState.State
              Input = buildState.Input
              PushPopStack = Pop transitionPop::stack
              Accepting = buildState.Accepting }

let doBuildTransitions (pdsStates : Map<Guid, State>) buildState =
    let transitions = (getState buildState).Transitions
    match buildState with
    | Complete x -> Set.empty // can't do anything more with complete states
    | Building x ->
        transitions
        |> List.choose (fun t ->
            let transitionTarget = Map.find t.Target pdsStates
            let buildState = // start by targeting the new target
                { State = transitionTarget
                  Input = Epsilon
                  PushPopStack = x.PushPopStack
                  Accepting = transitionTarget.Accepting }

            // pop if it makes sense
            let buildState =
                match t.Pop with
                | None | Some (In (Epsilon)) -> Some buildState
                | Some symbol ->
                    popTransitionFromBuildState symbol buildState

            // push, if it makes sense
            let buildState =
                buildState
                |> Option.map (fun buildState ->
                    match t.Push with
                    | None | Some (In (Epsilon)) -> buildState
                    | Some symbol ->
                        pushTransitionOnBuildState symbol buildState)

            buildState
            |> Option.map (fun buildState ->
                match t.In with
                | Epsilon -> Building buildState
                | input -> Complete { buildState with Input = input }))
        |> Set

let rec doAllBuildTransitions
    (pdsStates : Map<Guid, State>)
    (currentBuild : Set<RuleBuildState>)
    =
    let next =
        currentBuild
        |> Set.collect (doBuildTransitions pdsStates)
        |> Set.union currentBuild
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
      PopStack : StackSymbol list
      Accepting : bool
      PushStack : StackSymbol list }

type LookupTable = Map<LookupInput, LookupOutput list>

let getInputOrAcceptingRuleBuildState buildState =
    match buildState with
    | Complete buildState -> Some buildState
    | Building buildState when buildState.Accepting = true -> Some buildState
    | _ -> None

let buildLookupForState pdaState (pdaStates : Map<Guid, State>) =
    let starting =
        Building { State = pdaState; Input = Epsilon; PushPopStack = []; Accepting = false }
    //printfn "Build Lookup For State"
    doAllBuildTransitions pdaStates (Set [ starting ])
    //|> tee (Seq.iter (fun x -> printfn "%A" x))
    |> Seq.choose getInputOrAcceptingRuleBuildState
    |> Seq.map (fun buildState ->
        // stack will need to be poped in reverse order to the pop symbols
        // we have on our pushpop stack. We should only have pops at the bottom
        let popStack =
            buildState.PushPopStack
            |> List.choose (function | Pop pop -> Some pop | _ -> None )
            |> List.rev

        let pushStack =
            buildState.PushPopStack
            |> List.choose (function | Push push -> Some push | _ -> None )

        let lookup =
            { StateId = pdaState.Id
              StateName = pdaState.Name
              Input = buildState.Input
              TopOfStack = popStack |> List.tryHead }

        let lookupOut =
            { StateId = buildState.State.Id
              StateName = buildState.State.Name
              PopStack = popStack
              Accepting = buildState.Accepting
              PushStack = pushStack }

        lookup, lookupOut)
    |> Seq.toList

let buildLookupTable (pdsStates : Map<Guid, State>) : LookupTable =
    // This can be quite slow!
    // There's probably a dynamic programming solution to make this much faster
    pdsStates
    //|> Map.filter (fun k v -> v.Name = "StartPDA")
    |> Map.toList
    |> List.collect (fun (_, state) ->
        buildLookupForState state pdsStates)
    |> List.groupBy (fun (lookupIn, _) -> lookupIn)
    |> List.map (fun (lookupIn, xs) -> lookupIn, xs |> List.unzip |> snd)
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

    // two possible start points for transitions,
    // some of them might be duplicates
    let possibleInputs =
        // Epsilon inputs will happen when there's no more input.
        // They only go to final transitions
        [ { StateId = runState.StateId
            StateName = runState.StateName
            Input = input
            TopOfStack = runState.Stack |> List.tryHead }
          { StateId = runState.StateId
            StateName = runState.StateName
            Input = input
            TopOfStack = None } ]
          |> Set

    possibleInputs
    |> Seq.choose(fun input -> lookupTable |> Map.tryFind input)
    |> Seq.collect id
    |> Seq.choose (fun out ->
        // pop then push from the stack
        // we may fail here if the popstack doesn't match what's on the stack
        let stack =
            (Some runState.Stack, out.PopStack)
            ||> List.fold (fun stack pop ->
                match stack with
                | Some (head::tail) when head = pop -> Some tail
                | _ -> None)

        let stack = stack |> Option.map (fun stack -> out.PushStack@stack)

        stack
        |> Option.map (fun stack ->
            { runState with
                StateId = out.StateId
                StateName = out.StateName
                Accepting = out.Accepting
                Stack = stack } ))
    |> Set

let rec doAllInputTransitions input lookupTable runStates =
    runStates |> Set.collect (doLookupTransitions input lookupTable)

let rec doesPDAAcceptInput' lookupTable input (runStates : Set<RunState>) =
    match input with
    | [] ->
        let runStates =
            doAllInputTransitions Epsilon lookupTable runStates
            |> Set.union runStates

        // once we reach the end of the input, we accept it if any of the states
        // are an accepting state. Note in PDAs the stack does not need to be
        // empty to accept
        runStates |> Set.exists (fun x -> x.Accepting)
    | x::xs ->
        let runStates = doAllInputTransitions x lookupTable runStates
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
