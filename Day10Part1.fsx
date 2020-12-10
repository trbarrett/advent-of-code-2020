#load "./Helper.fsx"
open Helper

let folder (value, onesCount, threesCount) next =
    match next - value  with
    | 1L -> (next, onesCount + 1, threesCount)
    | 3L -> (next, onesCount, threesCount + 1)
    | _ -> (next, onesCount, threesCount)

readInput "day10.txt" |> Seq.map int64
|> Seq.sort
|> fun xs -> Seq.append xs [ (Seq.last xs) + 3L ] // extra item for the device
|> Seq.fold folder (0L,0,0)
|> fun (_, onesCount, threesCount) -> printfn "%d" (onesCount * threesCount)

