#load "./Helper.fsx"
open Helper

let buildSeq curr currIdx m =
   Seq.unfold (fun (curr, currIdx, m) ->
      match Map.tryFind curr m with
      | None -> Some(0, (0, currIdx + 1, (Map.add curr currIdx m)))
      | Some idx ->
         let next = currIdx - idx
         Some(next, (next, currIdx + 1, (Map.add curr currIdx m)))
   ) (curr, currIdx, m)

let run hist upTo =
   buildSeq (hist |> Seq.last)
            ((hist |> Seq.length) - 1)
            (hist |> Seq.indexed |> Seq.map Tuple.flip |> Map)
   |> Seq.skip (upTo - (hist |> Seq.length) - 1)

run [ 0;5;4;1;10;14;7 ] 2020 |> Seq.head |> printfn "Part1: %d"
// Brute-force approach, so takes about 2 minutes for part 2
run [ 0;5;4;1;10;14;7 ] 30_000_000 |> Seq.head |> printfn "Part2: %d"
