open System

#load "./Helper.fsx"
open Helper

let rotate facing rotate = (facing + rotate + 360) % 360
let facingToInst = function | 0 -> "E" | 90 -> "S" | 180 -> "W" | 270 -> "N"

let rec processInstruction (x,y,f) (instr : string) =
    let (code, amt) = (instr.[0], int instr.[1..(Seq.length instr) - 1])
    match (code, amt) with
    | 'N', amt -> (x, y + amt, f) | 'E', amt -> (x + amt, y, f)
    | 'S', amt -> (x, y - amt, f) | 'W', amt -> (x - amt, y, f)
    | 'R', amt -> (x, y, rotate f amt) | 'L', amt -> (x, y, rotate f -amt)
    | 'F', amt -> processInstruction (x,y,f) (sprintf "%s%d" (facingToInst f) amt)

let processAll xs = Seq.fold processInstruction (0, 0, 0) xs

let manhattanDist ((x : int), (y : int), _) =  Math.Abs x + Math.Abs y

readInput "day12.txt" |> processAll |> manhattanDist |> printfn "%d"
