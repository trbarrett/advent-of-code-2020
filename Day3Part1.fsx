#load "./Helper.fsx"
open Helper

type Feature = | Empty | Tree
type Altitude = Feature seq
type Mountain = Altitude seq
module Mountain =
    let repeatsAt = Seq.item(0) >> Seq.length
    let getFeature mountain down right =
        mountain |> Seq.item down |> Seq.item (right % (repeatsAt mountain))

type Toboggan = { DistRight : int; DistDown : int; TreeCount : int64 }

let parseAltitude = Seq.map (function | '#' -> Tree | _ -> Empty)

let rec rideDown (mountain : Mountain) (speedRight, speedDown) toboggan =
    if toboggan.DistDown + speedDown >= (Seq.length mountain)
    then toboggan
    else
        let countTrees = function | Tree -> 1L | Empty -> 0L
        let right = toboggan.DistRight + speedRight
        let down = toboggan.DistDown + speedDown
        let feature = Mountain.getFeature mountain down right
        let toboggan = { DistRight = right; DistDown = down
                         TreeCount = toboggan.TreeCount + countTrees feature}
        rideDown mountain (speedRight, speedDown) toboggan

let navigateSlope mountain (speedRight, speedDown) =
    rideDown mountain
             (speedRight, speedDown)
             { DistRight = 0; DistDown = 0; TreeCount = 0L }

let result =
    readInput "day3.txt"
    |> Seq.map parseAltitude
    |> (flip navigateSlope) (3, 1)

printfn "%d" result.TreeCount
