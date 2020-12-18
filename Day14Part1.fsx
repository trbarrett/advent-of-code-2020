#load "./Helper.fsx"
open Helper
open System
open System.Text.RegularExpressions

type Input =
    | Mask of uint64 * uint64
    | Mem of uint32 * uint64

let strToMask (f : char -> bool) str =
    str |> String.map (fun x -> if f(x) then '1' else '0')
    |> fun str -> Convert.ToUInt64 (str, 2)

let xtraMaskBits = "XXXXXXXXXXXXXXXXXXXXXXXXXXXX" // pad our 36 bit mask out to 64 bits

let parse (line : string) =
    let m = Regex.Match(line, "mask = ([X\d]+)")
    match m.Success with
    | true ->
        let mask = xtraMaskBits + m.Groups.[1].Value
        Mask (mask |> strToMask ((=) '1'), mask |> strToMask ((<>) '0'))
    | false ->
        let m = Regex.Match(line, "mem\[(\d+)\] = (\d+)")
        Mem ((uint32 m.Groups.[1].Value), (uint64 m.Groups.[2].Value))

let rec compute' (Mask (ones, zeros)) mem inputs =
    match inputs with
    | [] -> mem |> Map.toSeq |> Seq.map snd |> Seq.sum
    | Mask (m1, m2)::xs ->
        compute' (Mask (m1, m2)) mem xs
    | Mem (addr, value)::xs ->
        let maskedValue = (value ||| ones)  &&& zeros
        compute' (Mask (ones, zeros)) (Map.add addr maskedValue mem) xs

let compute inputs = compute' (Mask (0UL, 0UL)) Map.empty inputs

readInput "day14.txt"
|> Seq.map parse |> Seq.toList
|> compute |> printfn "%A"
