#load "./Helper.fsx"
open Helper

type Passport = Map<string,string>
module Passport =
    let empty : Passport = Map.empty
    let join (p : Passport) (q : Passport) =
        Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let parseLine (line : string) : Passport =
    let array2Tuple [|a; b|] = (a, b)
    let split (c : char) (x : string) = x.Split c
    line.Split " " |> Seq.map (split ':' >> array2Tuple) |> Map

let rec buildPassports (lines : string list) completed incomplete =
    match lines with
    | [] -> (incomplete::completed)
    | ""::xs -> // empty line indicates a completed passport
        buildPassports xs (incomplete::completed) Passport.empty
    | line::xs -> // otherwise we need to continue to update the incomplete one
        buildPassports xs completed (Passport.join incomplete (parseLine line))

let parseAllPassports lines = buildPassports lines [] Passport.empty

let isValid passport =
    let requiredFields = [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    requiredFields |> Seq.forall ((flip Map.containsKey) passport)

let result =
    readInput "day4.txt"
    |> Seq.toList
    |> parseAllPassports
    |> Seq.filter isValid
    |> Seq.length

printfn "%d" result

