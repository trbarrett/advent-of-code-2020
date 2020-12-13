open System

#load "./Helper.fsx"
open Helper

let rotate (x, y) amt =
   match (amt + 360) % 360 with
   | 0 -> (x, y) | 90 -> (y, -x) | 180 -> (-x, -y) | 270 -> (-y, x)

let rec processInstruction (x,y,sx,sy) (instr : string) =
   let (code, amt) = (instr.[0], int instr.[1..(Seq.length instr) - 1])
   match (code, amt) with
   | 'N', amt -> (x, y + amt, sx, sy) | 'E', amt -> (x + amt, y, sx, sy)
   | 'S', amt -> (x, y - amt, sx, sy) | 'W', amt -> (x - amt, y, sx, sy)
   | 'R', amt -> let (x, y) = rotate (x, y) amt
                 (x, y, sx, sy)
   | 'L', amt -> let (x, y) = rotate (x, y) -amt
                 (x, y, sx, sy)
   | 'F', amt -> (x, y, sx + x * amt, sy + y * amt)

let processAll xs = Seq.fold processInstruction (10, 1, 0, 0) xs

let manhattanDist (_, _, (x : int), (y : int)) =  Math.Abs x + Math.Abs y

readInput "day12.txt" |> processAll |> manhattanDist |> printfn "%d"
