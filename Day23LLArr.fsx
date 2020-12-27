
// Solution using an array doing double duty as a linked list. This is the
// fastest solution I could find

let findMaxNode (arr : int []) start =
    let mutable max = start
    let mutable next = arr.[start]
    while next <> start do
        if next > max then
            max <- next
        next <- arr.[next]
    max

let rec findNext (arr : int []) (curr : int) picked (max : int) =
    if curr = 0
    then
        if Array.contains max picked
        then findMaxNode arr arr.[picked.[2]]
        else max
    else
        let mutable next = curr - 1
        while Array.contains next picked && next >= 1 do
            next <- next - 1
        if next = 0 then
            findNext arr 0 picked max
        else
            next

let arrToArrLnk input =
    // use an array as a kind of linked list, with each index being the item, and
    // the value as the next item in the list
    let arr = Array.zeroCreate (Array.length input + 1)
    input |> Array.iteri (fun i v -> arr.[v] <- input.[(i + 1)%(Array.length input)])
    arr

let arrLnkToArr arr start =
    let out = Array.zeroCreate (Array.length arr - 1)
    let mutable next = start
    for i = 1 to (Array.length arr - 1) do
        out.[i-1] <- next
        next <- arr.[next]
    out

let rec playGame (arr : int []) curr moveLimit =
    let mutable curr = curr
    let max = arr |> Array.max
    for move = 1 to moveLimit do
        // pick the next 3
        let p1 = arr.[curr]
        let p2 = arr.[p1]
        let p3 = arr.[p2]
        // remove the picked nodes from the linked list
        arr.[curr] <- arr.[p3]
        // figure out where to drop them
        let dropSpot = findNext arr curr [|p1; p2; p3|] max
        // drop them
        let replace = arr.[dropSpot]
        arr.[dropSpot] <- p1
        arr.[p3] <- replace
        //start the next move
        curr <- arr.[curr]
    arr, curr

let part1 (input : int []) =
    let cups = input
    let moves = 100
    let arr = arrToArrLnk cups
    let (arr, curr) = playGame arr cups.[0] moves
    let result = arrLnkToArr arr curr
    printfn "Part 1: %A" result

let part2 (input : int []) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let cups = Array.append input (Array.init (1_000_000 - Array.length input) ((+) 10))
    let moves = 10_000_000
    let arr = arrToArrLnk cups
    let (arr, curr) = playGame arr cups.[0] moves
    let result = arrLnkToArr arr curr
    let oneIndex = Array.findIndex ((=) 1) result
    let oneAfter = Array.item ((oneIndex + 1) % (Array.length cups)) result
    let twoAfter = Array.item ((oneIndex + 2) % (Array.length cups)) result

    printfn "Part 2:"
    printfn "One After: %d" oneAfter
    printfn "Two After: %d" twoAfter

    printfn "Part 2 took: %d ms" sw.ElapsedMilliseconds

//let input = [|3;8;9;1;2;5;4;6;7|] // example
let input = [|8;7;1;3;6;9;4;5;2|]

part1 input
part2 input
