open System

#load "./Helper.fsx"
open Helper

let parse (lines : string seq) =
   let departTimestamp = int64 (lines |> Seq.head)
   let busIds = (lines |> Seq.item 1).Trim().Split(",")
                |> Seq.filter ((<>) "x") |> Seq.map int64
   (departTimestamp, busIds)

let calculate (departTimestamp : int64, busIds) =
   busIds
   |> Seq.map (fun x -> x - departTimestamp % x)
   |> Seq.zip busIds |> Seq.sortBy snd |> Seq.head
   |> fun (busId, mins) ->  busId * mins

readInput "day13.txt"
|> parse |> calculate |> printfn "%A" // 5946
