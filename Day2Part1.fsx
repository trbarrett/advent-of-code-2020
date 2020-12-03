#load "./Helper.fsx"
open Helper

type PasswordEntry = { Min : int; Max : int; Letter : char; Pass : string }

let parseInput (str : string) =
    let [| range; letter; pass |] = str.Trim().Split(' ')
    let [| min; max |] = range.Trim().Split('-')
    { Min = int min
      Max = int max
      Letter = letter.[0]
      Pass = pass.Trim() }

let isValid (entry : PasswordEntry) =
    let count = entry.Pass |> Seq.filter ((=) entry.Letter) |> Seq.length
    entry.Min <= count && count <= entry.Max

let result =
    readInput "day2.txt"
    |> Seq.filter (parseInput >> isValid)
    |> Seq.length

printfn "%d" result
