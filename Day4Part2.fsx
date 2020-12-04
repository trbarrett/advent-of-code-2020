open System.Text.RegularExpressions

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

let fieldValidations =
    [ "byr", (fun (x : string) -> int x >= 1920 && int x <= 2002)
      "iyr", (fun (x : string) -> int x >= 2010 && int x <= 2020)
      "eyr", (fun (x : string) -> int x >= 2020 && int x <= 2030)
      "hgt", (fun (x : string) ->
          let m = Regex.Match(x, "^(\d+)(cm|in)$")
          if m.Success then
              let x = int m.Groups.[1].Value
              match m.Groups.[2].Value with
              | "cm" -> x >= 150 && x <= 193
              | "in" -> x >= 59 && x <= 76
              | _ -> false
          else false)
      "hcl", (fun (x : string) -> Regex.IsMatch(x, "^#[0-9abcdef]{6}$"))
      "ecl", (fun (x : string) -> Regex.IsMatch(x, "^(amb|blu|brn|gry|grn|hzl|oth)$"))
      "pid", (fun (x : string) -> Regex.IsMatch(x, "^\d{9}$")) ]

let checkField passport (field, validate)  =
    passport |> Map.containsKey field && validate (Map.find field passport)

let isValid passport = fieldValidations |> Seq.forall (checkField passport)

let result =
    readInput "day4.txt"
    |> Seq.toList
    |> parseAllPassports
    |> Seq.filter isValid
    |> Seq.length

printfn "%d" result
