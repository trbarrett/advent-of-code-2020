#load "./Helper.fsx"
open Helper
open System.Text.RegularExpressions

type Edge = { Node : string; Cost : int }
type DAG = Map<string, Edge seq>

let parseRule line =
    let m = Regex.Match(line, "^(.+) bags contain (((\d+) ([^,]+)) bags?,? ?)*")
    let edges =
        Seq.zip (m.Groups.[4].Captures |> Seq.map (fun x -> x.Value))
                (m.Groups.[5].Captures |> Seq.map (fun x -> x.Value))
        |> Seq.map (fun (cost, name) -> { Node = name; Cost = int cost })
    m.Groups.[1].Value, edges

let rec traverseDAG' (dag : DAG) (frontier : Edge list) (visited : Edge list) =
    match frontier with
    | [] -> visited
    | next::newFrontier -> // NOTE: We can (and need to) revisit already visited
        match dag |> Map.tryFind next.Node with
        | None -> traverseDAG' dag newFrontier (next::visited)
        | Some edges ->
            let multiplyEdge cost a = { a with Cost = a.Cost * cost }
            let visitCosts = edges |> Seq.map (multiplyEdge next.Cost)
            traverseDAG' dag
                         (newFrontier @ (Seq.toList visitCosts))
                         (next::visited)

let traverseDAG start dag =
    traverseDAG' dag [{ Node = start; Cost = 1 }] List.empty

readInput "day7.txt"
|> Seq.map parseRule |> Map // Create a DAG
|> traverseDAG "shiny gold"
|> Seq.filter (fun e -> e.Node <> "shiny gold") // original item is ignored
|> Seq.map (fun x -> x.Cost) |> Seq.sum
|> printfn "%A"
