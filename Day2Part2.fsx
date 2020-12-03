#load "./Helper.fsx"
open Helper
open Option

type PasswordEntry = { Pos1 : int; Pos2 : int; Letter : char; Pass : string }

let parseInput (str : string) =
    let [| range; letter; pass |] = str.Trim().Split(' ')
    let [| pos1; pos2 |] = range.Trim().Split('-')
    { Pos1 = (int pos1) - 1
      Pos2 = (int pos2) - 1
      Letter = letter.[0]
      Pass = pass.Trim() }

let isValid (entry : PasswordEntry) =
    let matchLetter = Option.map ((=) entry.Letter) >> defaultValue false
    let check pos = entry.Pass |> Seq.tryItem pos |> matchLetter
    [ entry.Pos1; entry.Pos2 ]
    |> Seq.filter check
    |> Seq.length = 1

let result =
    readInput "day2.txt"
    |> Seq.filter (parseInput >> isValid)
    |> Seq.length

printfn "%A" result
