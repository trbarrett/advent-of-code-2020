#load "./Helper.fsx"
open Helper

type DimExtents = int * int
type Size = DimExtents * DimExtents * DimExtents
type Box = Map<int, bool> * Size

let parse xs =
    let boxSize = (0, (xs |> Seq.length) - 1), (0, (xs |> Seq.length) - 1), (0, 0)
    let parseChar y x c = (x, y, 0), match c with | '#' -> true | _ -> false
    let parsedLines = xs |> Seq.mapi (fun y line -> line |> Seq.mapi (parseChar y))
    (parsedLines |> Seq.collect id |> Map, boxSize)

let dirs =
    [ for xd in -1 .. +1 do
        for yd in -1 .. +1 do
            for zd in -1 .. +1 do
                if not (xd = 0 && yd = 0 && zd = 0) then yield (xd, yd, zd) ]

let isActive pos =  Map.tryFind pos >> Option.defaultValue false

let add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

let getSurroundingCount pos m =
    dirs |> Seq.filter (fun d -> isActive (add pos d) m) |> Seq.length

let nextActiveState pos m =
   match isActive pos m, getSurroundingCount pos m with
   | _, 3 | true, 2 -> true | _ -> false

let increaseBoxSize ((x0, x1), (y0, y1), (z0, z1)) =
    ((x0 - 1, x1 + 1), (y0 - 1, y1 + 1), (z0 - 1, z1 + 1))

let getExtents ((x0, x1), (y0, y1), (z0, z1)) =
    [ for x in x0..x1 do for y in y0..y1 do for z in z0..z1 do (x, y, z) ]

let cycle (box, size) =
    let size = increaseBoxSize size
    (getExtents size |> Seq.map (fun pos -> (pos, nextActiveState pos box)) |> Map,
     size)

let countActive box =
    box |> Map.toList |> List.unzip |> snd |> List.filter id |> List.length

// helper function for debugging etc
let printBox (box, size) =
    getExtents size
    |> List.sortBy (fun (x, y, z) -> (z,y,x))
    |> List.groupBy (fun (_, _, z) -> z)
    |> List.map (fun (z, xs) -> (z, xs |> List.groupBy (fun (_, y, _) -> y)))
    |> List.iter (fun (z, xs) ->
        printfn "z=%d" z
        xs |> List.iter (fun (_, poses) ->
           poses |> List.iter (fun pos ->
               match box |> Map.find pos with
               | true -> printf "#"
               | false -> printf ".")
           printfn "" ))

let calc input =
    (Seq.replicate 6 cycle |> Seq.reduce (>>)) input
    |> fst |> Map.toList |> List.unzip |> snd |> List.filter id |> List.length

readInput "day17.txt" |> parse
|> calc |> printfn "%A"
