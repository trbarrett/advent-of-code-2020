
// Solution using a single large buffer we move along, and spans + slices for
// performance

#load "./Helper.fsx"
open Helper
open System

let getMaxIndexOfSpan f (s : inref<Span<int>>) =
    let mutable max = Int32.MinValue
    let mutable idx = -1
    for i = 0 to s.Length - 1 do
        if f(s.[i])
        then
            if s.[i] > max then
                max <- s.[i]
                idx <- i
    (max, idx)


let findNext curr (remaining : inref<Span<int>>) =
    let nextIndex = remaining.IndexOf (curr - 1)
    if nextIndex <> -1 then
        nextIndex
    else
        let (_, nextIndex) = getMaxIndexOfSpan (fun x -> x < curr) &remaining
        if nextIndex <> -1 then
            nextIndex
        else
            let (_, nextIndex) = getMaxIndexOfSpan (fun _ -> true) &remaining
            nextIndex


let rec playGame (cupsBuffer : int []) itemLength bufferIdx moveNo moveLimit =
    let mutable bufferSpan = cupsBuffer.AsSpan()
    let mutable cups = bufferSpan.Slice(bufferIdx, itemLength)
    if moveNo > moveLimit then
        cups.ToArray()
    else
        if moveNo % 50_000 = 0 then
            printfn "Move (%d)" moveNo
        let curr = cups.[0]
        let pickedup = cups.Slice(1,3)
        let mutable remaining = cups.Slice(4)
        let nextIndex = findNext curr &remaining
        let next = remaining.Slice(0, nextIndex + 1)

        // move the current to the end
        cupsBuffer.[bufferIdx + itemLength] <- curr
         // our pickedup items will be overwritten, so we need to copy it
        let pickedupArr = pickedup.ToArray()
        // move the items after the pickup back to where the picked up items started
        next.CopyTo(bufferSpan.Slice(bufferIdx+1))
        // copy the picked up items after the buffer
        pickedupArr.CopyTo(bufferSpan.Slice(bufferIdx+1+next.Length))
        // tail doesn't change, we haven't moved it

        // play next move
        playGame cupsBuffer itemLength (bufferIdx+1) (moveNo + 1) moveLimit

let playGameSpan (input : int []) moves =
    // In each game the start moves one past the end, so for performance create
    // a buffer large enough that we can move one index along for each move.
    // By doing that we'll only have to move the picked items to the correct
    // spot and move down anything before it
    let bufferLength = Array.length input + moves + 2
    let buffer : int [] = Array.zeroCreate bufferLength
    input.CopyTo(buffer, 0)
    playGame buffer (Array.length input) 0 1 moves

let part1 (input : int []) =
    let result = playGameSpan input 100
    printfn "Part 1: %A" result

let part2 (input : int []) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let input = Array.append input (Array.init (1_000_000 - Array.length input) ((+) 10))
    let result = playGameSpan input 10_000_000

    let oneIndex = Array.findIndex ((=) 1) result
    let oneAfter = Array.item ((oneIndex + 1) % (Array.length result)) result
    let twoAfter = Array.item ((oneIndex + 2) % (Array.length result)) result

    printfn "Part 2:"
    printfn "One After: %d" oneAfter
    printfn "Two After: %d" twoAfter

    printfn "Part 2 took: %d ms" sw.ElapsedMilliseconds

//let input = [|3;8;9;1;2;5;4;6;7|] // example
let input = [|8;7;1;3;6;9;4;5;2|]

part1 input

// This will take about 30 minutes depending on processor speed
//part2 input
