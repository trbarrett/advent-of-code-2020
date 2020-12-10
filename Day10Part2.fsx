#load "./Helper.fsx"
open Helper

let rec addToTree (t : Map<int64, int64 list>) frontier newFrontier next =
    match frontier with
    | [] -> (t, newFrontier |> Set.add next)
    | x::xs ->
        if next - x > 3L
        then addToTree t xs (newFrontier |> Set.remove x) next
        else addToTree (t |> Map.extendListValue x next) xs newFrontier next

let rec buildTree (t : Map<int64, int64 list>) frontier unassigned =
    match unassigned with
    | [] -> t
    | x::xs ->
        let (t, newFrontier) = addToTree t frontier (frontier |> Set.ofList) x
        buildTree t (Set.toList newFrontier) xs

let rec countDistinct' (t : Map<int64, int64 list>) (x : int64) mem = // DFS
    match mem |> Map.tryFind x with // yay - dynamic programming
    | Some v -> (v, mem)
    | None ->
        match t |> Map.tryFind x with
        | None -> (1L, mem)
        | Some edges ->
            ((0L, mem), edges)
            ||> List.fold (fun (acc, mem) e ->
                let (count, mem) = countDistinct' t e mem
                (acc + count, Map.add e count mem))

let countDistinct t = countDistinct' t 0L Map.empty |> fst

readInput "day10.txt" |> Seq.map int64 |> Seq.sort
|> fun xs -> Seq.append xs [ (Seq.last xs) + 3L ] // extra item for the device
|> Seq.toList
|> buildTree (Map [0L, []]) [0L]
|> countDistinct |> printfn "%d"
