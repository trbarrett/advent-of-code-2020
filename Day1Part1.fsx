#load "./Helper.fsx"
open Helper
let numbers = Helper.readInput "day1.txt" |> Seq.map int |> Set
let result =
    numbers
    |> Seq.find (fun x -> numbers |> Set.contains (2020 - x))
    |> fun x -> x * (2020 - x)

printfn "%d" result