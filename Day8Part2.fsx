#load "./Helper.fsx"
open Helper

type Instruction = | Nop of int | Jmp of int | Acc of int
let isNopOrJmp = function
    | Nop _ | Jmp _ -> true
    | _ -> false

let parseInstruction (line : string) =
    let [| command; amount |]  = line.Trim().Split(' ')
    match command with
    | "acc" -> Acc (int amount)
    | "jmp" -> Jmp (int amount)
    | "nop" -> Nop (int amount)
    | _ -> failwithf "Bad Input: %A" line

type StackFrame = { LineNo : int; Input : int; Visited : Set<int>; Flipped : bool }
let mkFrame lineNo input visited =
    { LineNo = lineNo
      Input = input
      Visited = visited
      Flipped = false }

let rewindStack (instructions : Instruction []) stack =
    let stack =
        // if we've already flipped an instruction in our history, we have to abort and try another one
        if stack |> List.exists (fun x -> x.Flipped)
        then stack |> List.skipWhile (fun x -> not x.Flipped) |> List.tail
        else stack

    // Now keep going back until we get to the next instruction we want to try flipping
    let stack = stack |> List.skipWhile (fun x -> not (isNopOrJmp instructions.[x.LineNo]))
    ({ stack.[0] with Flipped = true }, stack |> List.tail)

let rec runWithBacktrack (instructions : Instruction []) stackFrame (stack : StackFrame list) =
    if stackFrame.LineNo >= Array.length instructions then
        stackFrame.Input
    elif stackFrame.Visited |> Set.contains stackFrame.LineNo then
        rewindStack instructions stack ||> runWithBacktrack instructions
    else
        let (lineNo, input) =
            match instructions.[stackFrame.LineNo], stackFrame.Flipped with
            | (Nop _, false) | (Jmp _, true) ->
                (stackFrame.LineNo + 1, stackFrame.Input)
            | (Jmp amt, false) | (Nop amt, true) ->
                (stackFrame.LineNo + amt, stackFrame.Input)
            | (Acc amt, false) ->
                (stackFrame.LineNo + 1, stackFrame.Input + amt)
            | (Acc _, true) -> failwithf "Can't flip a 'acc' instruction. Line %d" stackFrame.LineNo
        let nextFrame = mkFrame lineNo input (stackFrame.Visited |> Set.add stackFrame.LineNo)
        runWithBacktrack instructions nextFrame (stackFrame::stack)

let runFromStart (instructions : Instruction []) =
    runWithBacktrack instructions (mkFrame 0 0 Set.empty) []


readInput "day8.txt"
|> Seq.map parseInstruction |> Seq.toArray
|> runFromStart
|> printfn "%A" //expected: 846, example: 8
