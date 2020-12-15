open System

#load "./Helper.fsx"
open Helper

let parse (lines : string seq) =
   (lines |> Seq.item 1).Trim().Split(",")
   |> Seq.map (Int64.tryParse)
   |> Seq.indexed
   |> Seq.choose (fun (x, y) -> y |> Option.map (fun y -> int64 x,y))

let firstMatch num offset bNum bOffset =
   Seq.unfold (fun state -> Some(state, state + 1L)) 1L
   |> Seq.filter (fun n -> ((num * n) - offset + bOffset) % bNum = 0L)
   |> Seq.head

// We find when Bus A and Bus B both meet. Bus C must then meet at a
// multiple of that and so on. Our `inc` is that multiple that we update when
// we find when the next bus matches the previous spot.
//
// Except that this is a really messed up way of doing that and I'm surprised it
// actually works at all! I'd really like to clean it up at some point.
let rec calculate' start (offset, num) inc busIds =
   match busIds with
   | [] -> (num * start) - offset
   | (bOffset, bVal)::xs ->
       let items = Seq.unfold (fun state -> Some(state, state + inc)) start
       let nxt = items |> Seq.find (fun n -> ((num * n) - offset + bOffset) % bVal = 0L)
       calculate' nxt (offset, num) (inc * bVal) xs

let rec calculate busIds =
   let busIds = busIds |> (List.sortByDescending snd)
   let (offset, num)::busIds = busIds
   let (bOffset, bNum)::_ =  busIds
   let start = firstMatch num offset bNum bOffset
   calculate' start (offset, num) 1L busIds

readInput "day13.txt"
|> parse |> Seq.toList |> calculate |> printfn "%A"
