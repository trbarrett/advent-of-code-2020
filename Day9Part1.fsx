#load "./Helper.fsx"
open Helper

let hasSumCombination (xs : int64 seq) =
    xs |> Seq.take (Seq.length xs - 1) // get all except the last one
    |> (fun x -> x,x) ||> Seq.allPairs // create all the combinations
    |> Seq.exists (fun (a, b) -> a + b = (xs |> Seq.last)) // match with last

readInput "day9.txt" |> Seq.map int64
|> Seq.windowed (25 + 1)
|> Seq.find (hasSumCombination >> not)
|> Seq.last |> printfn "%d"

