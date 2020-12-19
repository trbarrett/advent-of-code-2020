#load "./Helper.fsx"
open Helper

type Pos = int * int * int * int
type DimExtents = int * int
type Size = DimExtents * DimExtents * DimExtents * DimExtents
type Box = Map<Pos, bool>
type BoxAndSize = Map<Pos, bool> * Size

let parse xs : BoxAndSize =
    let boxSize = (0, (xs |> Seq.length) - 1), (0, (xs |> Seq.length) - 1), (0, 0), (0, 0)
    let parseChar y x c = (x, y, 0, 0), match c with | '#' -> true | _ -> false
    let parsedLines = xs |> Seq.mapi (fun y line -> line |> Seq.mapi (parseChar y))
    (parsedLines |> Seq.collect id |> Map, boxSize)

let dirs : Pos list =
    [ for xd in -1 .. +1 do
        for yd in -1 .. +1 do
            for zd in -1 .. +1 do
                for wd in -1 .. +1 do
                    if not (xd = 0 && yd = 0 && zd = 0 && wd = 0)
                    then yield (xd, yd, zd, wd) ]

let isActive pos = Map.tryFind pos >> Option.defaultValue false

let add (x1, y1, z1, w1) (x2, y2, z2, w2) : Pos =
    (x1 + x2, y1 + y2, z1 + z2, w1 + w2)

let getSurroundingCount (pos : Pos) (m : Box) =
    dirs |> Seq.filter (fun d -> isActive (add pos d) m) |> Seq.length

let nextActiveState (pos : Pos) m =
   match isActive pos m, getSurroundingCount pos m with
   | _, 3 | true, 2 -> true | _ -> false

let increaseBoxSize ((x0, x1), (y0, y1), (z0, z1), (w0, w1)) : Size =
    ((x0 - 1, x1 + 1), (y0 - 1, y1 + 1), (z0 - 1, z1 + 1), (w0 - 1, w1 + 1))

let getExtents ((x0, x1), (y0, y1), (z0, z1), (w0, w1)) : Pos list =
    [ for x in x0..x1 do
        for y in y0..y1 do
            for z in z0..z1 do
                for w in w0..w1 do (x, y, z, w) ]

let cycle ((box, size) : BoxAndSize) : BoxAndSize =
    let size = increaseBoxSize size
    (getExtents size |> Seq.map (fun pos -> (pos, nextActiveState pos box)) |> Map,
     size)

let countActive (box : Box) =
    box |> Map.toList |> List.unzip |> snd |> List.filter id |> List.length

let calc (input : BoxAndSize) =
    (Seq.replicate 6 cycle |> Seq.reduce (>>)) input
    |> fst |> Map.toList |> List.unzip |> snd |> List.filter id |> List.length

readInput "day17.txt" |> parse
|> calc |> printfn "%A"
