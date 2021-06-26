#load "./Helper.fsx"
open Helper
open System

type Tile =
    { Id : string
      Data : string list }

let parseIdLine (str : string) =
    str.Trim(':').Split(' ').[1]

let parseTile (lines : string seq) =
    let rec parseTileLine' xs acc =
        match xs with
        | [] -> ([], acc)
        | ""::xs -> (xs, acc)
        | str::xs -> parseTileLine' xs (str::acc)

    match Seq.tryHead lines with
    | Some head ->
        let id = parseIdLine head
        let remaining, tileData = parseTileLine' (Seq.tail lines |> Seq.toList) []
        Some (remaining, { Id = id; Data = tileData })
    | None ->
        None

let parseInput (lines : string seq) =
    let rec parseTites' (lines : string seq) acc =
        match parseTile lines with
        | Some (remainingLines, newTile) ->
            match remainingLines with
            | [] -> newTile::acc
            | _ -> parseTites' remainingLines (newTile::acc)
        | None ->
            acc

    parseTites' lines []

let edgeAsInt str =
    str
    |> String.map (fun c -> if c = '#' then '1' else '0')
    |> (fun str -> Convert.ToUInt64 (str, 2))

let flipEdge str =
    str |> Seq.rev |> Seq.toArray |> System.String

// Edges could be flipped or not, to make things easier to find matches we will
// define one as the "canonical" version
let getCanonicalEdge str =
    let normal = edgeAsInt str
    let flippedStr = flipEdge str
    let flipped = edgeAsInt flippedStr
    if normal <= flipped
    then str
    else flippedStr

let getEdges (tile : Tile) : string * string * string * string =
    // tiles are 10x10, so we can use that to make things easy
    let top = tile.Data |> List.head
    let left = tile.Data |> List.map (fun s -> s.[0]) |> List.toArray |> System.String
    let right = tile.Data |> List.map (fun s -> s.[9]) |> List.toArray |> System.String
    let bottom = tile.Data.[9]
    top, left, right, bottom

let getCanonicalEdges (tile : Tile) =
    let top, left, right, bottom = getEdges tile
    getCanonicalEdge top,
    getCanonicalEdge left,
    getCanonicalEdge right,
    getCanonicalEdge bottom

let addCannonicalEdgeToMap edge tile m =
    match Map.tryFind edge m with
    | Some xs -> Map.add edge (tile::xs) m
    | None -> Map.add edge [tile] m

let findOutsideTiles tiles =
    // create a Map with all the "canonical" versions of edges and the
    // tiles they point at.
    // From the problem statement:
    // "Tiles at the edge of the image also have this border, but the outermost
    // edges won't line up with any other tiles."
    // So if we find canonical edges that aren't shared by tiles, we know they
    // must be an outermost tile.
    let buildEdgeMap acc tile =
        let top, left, right, bottom =
            getCanonicalEdges tile
        acc
        |> addCannonicalEdgeToMap top tile
        |> addCannonicalEdgeToMap left tile
        |> addCannonicalEdgeToMap right tile
        |> addCannonicalEdgeToMap bottom tile

    let edgeMap =
        (Map.empty, tiles)
        ||> List.fold buildEdgeMap

    edgeMap
    |> Map.filter (fun _ tiles -> tiles |> List.length = 1)
    |> Map.map (fun _ tiles -> tiles |> List.head |> fun t -> t.Id)
    |> Map.values

let getCorners (outsideTileIds : int64 seq) =
    outsideTileIds
    |> Seq.groupBy id
    |> Seq.filter (fun (_, tileIds) -> tileIds |> Seq.length > 1)
    |> Seq.map (fun  (key, _) -> key)
    |> Seq.reduce (*)


readInput "day20.txt"
|> parseInput
|> findOutsideTiles
|> Seq.map (fun x -> System.Int64.Parse x)
//|> Seq.sort
//|> Seq.iter (printfn "%A")
|> getCorners
|> printfn "%A"
