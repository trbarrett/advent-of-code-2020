#load "./Helper.fsx"
open Helper
open System
open System.Text.RegularExpressions

type Input =
    | Mask of uint64 * uint64 * uint64 seq
    | Mem of uint64 * uint64

let strToMask (f : char -> bool) str =
    str |> String.map (fun x -> if f(x) then '1' else '0')
    |> fun str -> Convert.ToUInt64 (str, 2)

let rec buildCombinations' (items : 'a list) combinations =
    match items with
    | [] -> combinations
    | x::xs ->
        let newCombinations = combinations |> List.map (fun ys -> x::ys)
        buildCombinations' xs (newCombinations@combinations)

let buildCombinations items = buildCombinations' (Seq.toList items) [ [] ]

let xtraMaskBits = "0000000000000000000000000000" // pad our 36 bit mask out to 64 bits

let parse (line : string) =
    let m = Regex.Match(line, "mask = ([X\d]+)")
    match m.Success with
    | true ->
        let mask = xtraMaskBits + m.Groups.[1].Value
        let onesMask = strToMask ((=) '1') mask
        let notFloatingMask = strToMask ((<>) 'X') mask

        let uniqueXValues =
            mask |> Seq.rev |> Seq.indexed
            |> Seq.filter (fun (_, v) -> v = 'X')
            |> Seq.map (fun (i,_) -> uint64 (2.0 ** (float i)))
        let combinations = buildCombinations uniqueXValues |> Seq.map (Seq.sum)

        Mask (onesMask, notFloatingMask, combinations)
    | false ->
        let m = Regex.Match(line, "mem\[(\d+)\] = (\d+)")
        Mem ((uint64 m.Groups.[1].Value), (uint64 m.Groups.[2].Value))

let rec compute' (mask : Input) mem inputs =
    match inputs with
    | [] -> mem |> Map.toSeq |> Seq.map snd |> Seq.sum
    | (Mask _ as mask)::xs ->
        compute' mask mem xs
    | Mem (addr, value)::xs ->
        let (Mask (onesMask, notFloatingMask, combinations)) = mask
        let baseAddress = (addr ||| onesMask) &&& notFloatingMask
        let addresses = combinations |> Seq.map ((+) baseAddress)
        let mem = addresses |> Seq.fold (fun m addr -> Map.add addr value m) mem
        compute' mask mem xs

let compute inputs =
    compute' (Mask (0UL, 0UL, [])) Map.empty inputs

readInput "day14.txt"
|> Seq.map parse |> Seq.toList
|> compute |> printfn "%d"
