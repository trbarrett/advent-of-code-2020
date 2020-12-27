// this is a simple recursive pure functional solution. It's too slow for part 2

#load "./Helper.fsx"
open Helper

let rec playGame (cups : int list) moveNo moveLimit =
    if moveNo > moveLimit then
        cups
    else
        let findNext curr remaining =
            let next = remaining |> Seq.filter (fun x -> x < curr) |> Seq.tryMax
            let next =
                match next with
                 | Some x -> x
                 | None -> Seq.max remaining
            remaining |> Seq.findIndex ((=) next)

        let curr = (cups |> List.head)
        let pickedup = cups |> List.skip 1 |> List.take 3
        let remaining = cups |> List.skip 4
        let nextIndex = findNext curr remaining
        let next, tail = remaining |> List.splitAt (nextIndex)
        let newList = next @ [ remaining.[nextIndex] ] @ pickedup @ (tail |> List.tail) @ [curr]
        playGame newList (moveNo + 1) moveLimit

//let cups = [3;8;9;1;2;5;4;6;7] // example
let cups = [8;7;1;3;6;9;4;5;2]
playGame cups 1 100 |> printfn "Part 1: %A"
