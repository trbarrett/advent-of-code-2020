#I __SOURCE_DIRECTORY__

open System.IO
open System.Collections.Generic


let flip f a b = f b a

let public readInput inputName =
    sprintf "%s/inputdata/%s" __SOURCE_DIRECTORY__ inputName
    |> File.ReadLines

let (|KeyValue|) (keyValuePair : KeyValuePair<'k, 'v>) : 'k * 'v =
    let k = keyValuePair.Key
    let v = keyValuePair.Value
    (k, v)
