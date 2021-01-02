#load "./Helper.fsx"
open Helper

let parseLine (line : string) =
    let rec parse xs acc n =
        match xs with
        | 'n'::ys | 's'::ys -> parse ys acc (Some (string (List.head xs)))
        | 'e'::ys | 'w'::ys ->
            match n with
            | Some pre -> parse ys ((pre+ string (List.head xs))::acc) None
            | None -> parse ys (string (List.head xs)::acc) None
        | _ -> acc

    parse (line |> Seq.toList) [] None
    |> List.rev

let rec getPos dirs (x, y) =
    match dirs with
    | [] -> (x, y)
    |  "e"::xs -> getPos xs (x + 1, y)
    | "ne"::xs -> getPos xs (x + 1, y - 1)
    | "nw"::xs -> getPos xs (x, y - 1)
    |  "w"::xs -> getPos xs (x - 1, y)
    | "sw"::xs -> getPos xs (x - 1, y + 1)
    | "se"::xs -> getPos xs (x, y + 1)

let flipTile floor dir =
    let pos = getPos dir (0, 0)
    if Set.contains pos floor
    then Set.remove pos floor
    else Set.add pos floor

let flipTiles = Seq.fold flipTile Set.empty

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let dirs = [ (1, 0); (1, -1); (0, -1); (-1, 0); (-1, 1); (0, 1) ] |> Set

let surrounding pos = dirs |> Set.map (add pos)

let applyNeighbours tiles newTiles pos =
    let surroundingCount =
        surrounding pos
        |> Seq.filter ((flip Set.contains) tiles)
        |> Seq.length

    match surroundingCount with
    | 1 -> if Set.contains pos tiles then Set.add pos newTiles else newTiles
    | 2 -> Set.add pos newTiles
    | 0 | _ -> newTiles

let conwayStep tiles =
    tiles
    |> Set.collect surrounding
    |> Set.union tiles
    |> Set.fold (applyNeighbours tiles) Set.empty

let part2 () =
    readInput "day24.txt" |> Seq.map parseLine
    |> flipTiles
    |> (Seq.replicate 100 conwayStep |> Seq.reduce (>>))
    |> Set.count |> (printfn "Part2: %A")

let part1 () =
    readInput "day24.txt" |> Seq.map parseLine
    |> flipTiles
    |> Set.count |> (printfn "Part1: %A")

part1 ()
part2 ()
