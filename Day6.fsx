#load "./Helper.fsx"
open Helper

let rec groupInput' lines completed incomplete =
    match lines with
    | [] -> (incomplete::completed)
    | ""::xs -> groupInput' xs (incomplete::completed) []
    | line::xs -> groupInput' xs completed (line::incomplete)

let groupInput lines = groupInput' lines [] []

let groups = readInput "day6.txt" |> Seq.toList |> groupInput
let part1result =
    groups
    |> Seq.map (String.concat "" >> Seq.distinct >> Seq.length)
    |> Seq.sum

let part2result =
    groups
    |> Seq.map (Seq.map (Set) >> Set.intersectMany >> Seq.length)
    |> Seq.sum

printfn "Part1: %d" part1result
printfn "Part2: %d" part2result
