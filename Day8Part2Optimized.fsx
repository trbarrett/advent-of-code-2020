#load "./Helper.fsx"
open Helper

type Instruction = | Nop of int | Jmp of int | Acc of int
let isNopOrJmp = function
    | Nop _ | Jmp _ -> true
    | _ -> false

let parseInstruction (line : string) =
    let [| op; amount |]  = line.Trim().Split(' ')
    match op with
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

type Stack =
    { Stack : StackFrame list
      FlippedIndex : int option
      BadFlippedLines : Set<int> }

let emptyStack = { Stack = []; FlippedIndex = None; BadFlippedLines = Set.empty }

let runFromStart (instructions : Instruction []) =
    let rewindStack (stack : Stack) =
        let stack =
            // if we've already flipped an instruction in our history, we have to abort and try another one
            match stack.FlippedIndex with
            | Some flipFrameIndex ->
                let (badFrames, goodFrames) =
                    stack.Stack
                    |> List.splitAt ((List.length stack.Stack) - flipFrameIndex)
                let badLines = badFrames |> Seq.map (fun x -> x.LineNo) |> Set
                { Stack = goodFrames |> List.tail
                  FlippedIndex = None
                  BadFlippedLines = stack.BadFlippedLines |> Set.union badLines }
            | None -> stack

        // Now keep going back until we get to the next instruction we want to try flipping
        let stackHistory =
            stack.Stack|> List.skipWhile (fun x -> not (isNopOrJmp instructions.[x.LineNo]))
        ({ stack with
            Stack = stackHistory |> List.tail
            FlippedIndex = (stackHistory |> List.length) |> Some },
         { stackHistory.[0] with Flipped = true })

    let rec runWithBacktrack (stack : Stack) stackFrame =
        if stackFrame.LineNo >= Array.length instructions then
            stackFrame.Input
        elif stackFrame.Visited |> Set.contains stackFrame.LineNo then
            rewindStack stack ||> runWithBacktrack
        elif stack.FlippedIndex.IsSome
          && stack.BadFlippedLines |> Set.contains stackFrame.LineNo then
            // we already know this line of processing won't work so abort
            rewindStack stack ||> runWithBacktrack
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
            mkFrame lineNo input (stackFrame.Visited |> Set.add stackFrame.LineNo)
            |> runWithBacktrack { stack with Stack = stackFrame::(stack.Stack) }

    runWithBacktrack emptyStack (mkFrame 0 0 Set.empty)

// This "Optimized" version remembers bad paths it has take with flipped
// operations. It evaluates 683 frames vs. the 1715 frames the unoptimized
// version has to process to get to the correct answer.
//
// Also this remembers if we've flipped an operation somewhere in the stack, so
// that saves quite a few lookups instead of checking the whole stack.
//
// Overall I think this makes the algo in the region of O(n*log n), though it's
// hard to be sure.

readInput "day8.txt"
|> Seq.map parseInstruction |> Seq.toArray
|> runFromStart
|> printfn "%A" //expected: 846, example: 8
