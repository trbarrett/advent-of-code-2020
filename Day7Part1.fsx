#load "./Helper.fsx"
open Helper
open System.Text.RegularExpressions

let parseRule line =
    let m = Regex.Match(line, "^(.+) bags contain ((\d+ ([^,]+)) bags?,? ?)*")
    m.Groups.[1].Value,
    (m.Groups.[4].Captures |> Seq.map (fun x -> x.Value))

let invertDAG (dag : Map<'a, 'b seq>) : Map<'b, 'a list> =
    dag
    |> Map.flattenOneToMany // flatten
    |> Seq.map Tuple.flip // invert
    |> Map.ofOneToManySeq // reconstitute

let rec traverseDAG' dag frontier visited =
    match frontier with
    | [] -> visited
    | next::newFrontier ->
        if visited |> Set.contains next // don't extend frontier to already visited
        then traverseDAG' dag newFrontier visited
        else
            match dag |> Map.tryFind next with
            | None -> traverseDAG' dag newFrontier (visited |> Set.add next)
            | Some edges -> traverseDAG' dag
                                         (newFrontier @ (Seq.toList edges))
                                         (visited |> Set.add next)

let traverseDAG start dag = traverseDAG' dag [start] Set.empty

readInput "day7.txt"
|> Seq.map parseRule |> Map // Create a DAG
|> invertDAG // we need to invert to see what bags are contained within what
|> traverseDAG "shiny gold"
|> Seq.filter ((<>) "shiny gold") // original item is ignored
|> Seq.length |> printfn "%A"
