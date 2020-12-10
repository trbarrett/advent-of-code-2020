#I __SOURCE_DIRECTORY__

open System.IO
open System.Collections.Generic


let flip f a b = f b a
let mkTuple x y = x, y
let tee f x = f x; x


let public readInput inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines

let (|KeyValue|) (keyValuePair : KeyValuePair<'k, 'v>) : 'k * 'v =
    let k = keyValuePair.Key
    let v = keyValuePair.Value
    (k, v)

module Tuple =
    let flip (x,y) = y, x

module Map =
    let extendListValue key value m =
        match m |> Map.tryFind key with
        | None -> m |> Map.add key [value]
        | Some existing -> m |> Map.add key (value::existing)

    let ofOneToManySeq xs : Map<'a, 'b list> =
        Seq.fold (fun m (x,y) -> m |> extendListValue x y) Map.empty xs

    let flattenOneToMany m =
        let flatten = (fun (KeyValue(x, ys)) -> ys |> Seq.map (mkTuple x))
        Seq.collect flatten m
