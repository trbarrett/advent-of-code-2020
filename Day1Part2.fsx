#load "./Helper.fsx"
open Helper
let numbers = readInput "day1.txt" |> Seq.map int |> Set
let pairs = numbers |> Seq.allPairs numbers

let result =
    pairs
    |> Seq.find (fun (x, y) -> numbers |> Set.contains (2020 - x - y))
    |> fun (x, y) -> x * y * (2020 - x - y)

printfn "%d" result

