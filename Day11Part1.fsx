#load "./Helper.fsx"
open Helper

type Cell = | Ground | EmptySeat | OccupiedSeat
module Cell =
    let toChar = function | Ground -> '.' | EmptySeat -> 'L' | OccupiedSeat -> '#'

let parseCellRow = Seq.map (function | 'L' -> EmptySeat | _ -> Ground)

type Ferry = Cell array array
module Ferry =
    let getSeat (x, y) (ferry : Ferry) =
        ferry |> Seq.tryItem y |> Option.bind (Seq.tryItem x)

    let dirs =
        [ for yd in -1 .. +1 do for xd in -1 .. +1 -> (xd, yd) ]
        |> Seq.filter ((<>) (0,0))

    let getSurroundingSeatCount (x, y) (ferry : Ferry) =
        dirs
        |> Seq.map (fun (dx, dy) -> getSeat (x + dx, y + dy) ferry)
        |> Seq.filter ((=) (Some OccupiedSeat)) |> Seq.length

    let occupiedCount (ferry : Ferry) =
        let isOccupied = function | OccupiedSeat -> 1 | _ -> 0
        ferry |> Array.sumBy (Array.sumBy isOccupied)

    let determineNextSeatState ((x, y) as pos) (ferry : Ferry) =
        match ferry.[y].[x] with
        | Ground -> Ground
        | EmptySeat ->
            if getSurroundingSeatCount pos ferry = 0
            then OccupiedSeat
            else EmptySeat
        | OccupiedSeat ->
            if getSurroundingSeatCount pos ferry >= 4
            then EmptySeat
            else OccupiedSeat

    let print (ferry : Ferry) = // for debugging
        ferry
        |> Array.map (Array.map Cell.toChar)
        |> Array.iter (System.String >> printfn "%s")

let rec updateFerry (ferry : Ferry) counter =
    let width = ferry |> Seq.head |> Seq.length
    let next = [| for y in 0 .. (Seq.length ferry) - 1  ->
                   [| for x in 0 .. width - 1 ->
                      Ferry.determineNextSeatState (x, y) ferry |] |]
    if next = ferry then next
    else updateFerry next (counter + 1)

let solve ferry : Ferry = updateFerry ferry 0

readInput "day11.txt" |> Seq.toArray
|> Array.map (parseCellRow >> Seq.toArray)
|> solve |> Ferry.occupiedCount |> printfn "%d"
