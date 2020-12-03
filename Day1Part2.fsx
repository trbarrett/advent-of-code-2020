#load "./Helper.fsx"
open Helper
let numbers = readInput "day1.txt" |> Seq.map int |> Set

let combinationsOf2 = numbers |> Seq.allPairs numbers

let combinationMap =
    combinationsOf2
    |> Seq.map (fun (x, y) -> x + y)
    |> (flip Seq.zip) combinationsOf2
    |> Map

let result =
    combinationMap
    |> Seq.find (fun (KeyValue (k, _)) -> numbers |> Set.contains (2020 - k))
    |> fun (KeyValue (x, (a, b))) -> a * b * (2020 - x)

printfn "%d" result

