#load "./Helper.fsx"
open Helper

let rec getSeatColumn xs (left, right) =
    let mid = (right - left) / 2 + left
    match xs with
    | [] -> left
    | 'L'::xs -> getSeatColumn xs (left, mid)
    | 'R'::xs -> getSeatColumn xs (mid + 1, right)
    | _ -> failwithf "Bad Input %A" xs

let rec getSeatRow xs (front, back) =
    let mid = (back - front) / 2 + front
    match xs with
    | 'F'::xs -> getSeatRow xs (front, mid)
    | 'B'::xs -> getSeatRow xs (mid + 1, back)
    | xs -> front * 8 + (getSeatColumn xs (0, 7))

let seatIds =
    readInput "day5.txt"
    |> Seq.map (fun x -> getSeatRow (Seq.toList x) (0, 127))

let availableSeats =
    let potentialIds = [ (seatIds |> Seq.min) .. (seatIds |> Seq.max) ] |> Set
    Set.difference potentialIds (seatIds |> Set)

printfn "Part1: %d" (seatIds |> Seq.max)
printfn "Part2: %d" (availableSeats |> Seq.head)
