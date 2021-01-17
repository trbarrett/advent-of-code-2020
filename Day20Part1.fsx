#load "./Helper.fsx"
open Helper
open System

type Tile =
    { Id : string
      Data : string seq }

let parseIdLine (str : string) = str.Trim(':').Split(' ').[1]

let parseTile (lines : string seq) =
    let rec parseTileLine' xs acc =
        match xs with
        | [] -> ([], acc)
        | ""::xs -> (xs, acc)
        | str::xs -> (xs, str :: acc)

    let id = parseIdLine (Seq.head lines)
    let remaining, tileData = parseTileLine' (Seq.tail lines |> Seq.toList) []

    (remaining, { Id = id; Data = tileData })

let parseInput (lines : string seq) =
    lines

readInput "day20.txt" |> parseInput
