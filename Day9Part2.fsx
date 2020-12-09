#load "./Helper.fsx"
open Helper

let Target = 1212510616L
type State = { Numbers : int64 []; Total : int64 }

let rec solver state (remaining : int64 list) =
    if state.Total = Target then
        Array.min state.Numbers + Array.max state.Numbers

    elif state.Total > Target then
        solver { Numbers = state.Numbers |> Array.skip 1
                 Total = state.Total - state.Numbers.[0]  }
               remaining
    else
        let next::remaining = remaining
        solver { Numbers = Array.append state.Numbers [| next |]
                 Total = state.Total + next  }
               remaining

readInput "day9.txt" |> Seq.map int64
|> Seq.toList
|> solver { Numbers = [||]; Total = 0L }
|> printfn "%d"
