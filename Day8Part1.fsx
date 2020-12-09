#load "./Helper.fsx"
open Helper

type Instruction = | Nop | Jmp of int | Acc of int

let parseInstruction (line : string) =
    let [| command; amount |]  = line.Trim().Split(' ')
    match command with
    | "acc" -> Acc (int amount)
    | "jmp" -> Jmp (int amount)
    | "nop" -> Nop
    | _ -> failwithf "Bad Input: %A" line

let rec run (instructions : Instruction []) lineNo acc visited =
    if visited |> Set.contains lineNo
    then acc
    else
        let visited = visited |> Set.add lineNo
        match instructions.[lineNo] with
        | Nop -> run instructions (lineNo + 1) acc visited
        | Acc amt -> run instructions (lineNo + 1) (acc + amt) visited
        | Jmp amt -> run instructions (lineNo + amt) acc visited

let runFromStart instructions = run instructions 0 0 Set.empty

readInput "day8.txt"
|> Seq.map parseInstruction |> Seq.toArray
|> runFromStart
|> printfn "%A"
