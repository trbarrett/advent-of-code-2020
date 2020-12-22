open System

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

module Int64 =
    let tryParse (str : string) =
        match Int64.TryParse str with
        | true, x -> Some x
        | _ -> None

module Int32 =
    let tryParse (str : string) =
        match Int32.TryParse str with
        | true, x -> Some x
        | _ -> None

module Seq =
    let unzip sequence =
        let (lstA, lstB) =
            Seq.foldBack (fun (a,b) (accA, accB) ->
                a::accA, b::accB) sequence ([],[])
        (Seq.ofList lstA, Seq.ofList lstB)

    let tryMax sequence =
        if Seq.isEmpty sequence
        then None
        else Some (Seq.max sequence)

    let tryMin sequence =
        if Seq.isEmpty sequence
        then None
        else Some (Seq.min sequence)

    let tryMaxWithIndex sequence =
        let _, maxValue, maxIndex =
            sequence |> Seq.fold (fun (index, maxSoFar, maxIndex) v ->
                if v > maxSoFar then (index+1, v, index+1)
                else (index+1, maxSoFar, maxIndex)) (-1, System.Int32.MinValue, -1)
        if maxIndex = -1
        then None
        else Some (maxValue, maxIndex)

    let tryMinWithIndex sequence =
        let _, minValue, minIndex =
            sequence |> Seq.fold (fun (index, minSoFar, maxIndex) v ->
                if v < minSoFar then (index+1, v, index+1)
                else (index+1, minSoFar, maxIndex)) (-1, System.Int32.MaxValue, -1)
        if minIndex = -1
        then None
        else Some (minValue, minIndex)

module Set =
    let collect f set = Set.map f set |> Set.unionMany

module Map =

    let keys map =
        map |> Map.toSeq |> Seq.map fst

    let mapValues f map =
        map |> Map.map (fun _ v -> f v)

    /// Similar to List.choose, Seq.choose.
    /// Applies the given function to each key, value pair in the map.
    /// Returns a map comprised of the results for each pair where the function
    /// returns Some(x)
    let choose f map =
        map
        |> Map.toSeq
        |> Seq.choose (fun (k, v) ->
            match f k v with
            | None -> None
            | Some x -> Some (k, x))
        |> Map

    /// Similar to List.choose, Seq.choose.
    /// Applies the given function to each value in the map.
    /// Returns a map comprised of the results for each value of the map
    /// where the function returns Some(x)
    let chooseValues f map =
        map
        |> Map.toSeq
        |> Seq.choose (fun (k, v) ->
            match f v with
            | None -> None
            | Some x -> Some (k, x))
        |> Map

    let findOrDefault key defaultValue map =
        map
        |> Map.tryFind key
        |> Option.defaultValue defaultValue

    let extendListValue key value m =
        match m |> Map.tryFind key with
        | None -> m |> Map.add key [value]
        | Some existing -> m |> Map.add key (value::existing)

    let extendListValues key values m =
        match m |> Map.tryFind key with
        | None -> m |> Map.add key values
        | Some existing -> m |> Map.add key (values@existing)

    let ofOneToManySeq xs : Map<'a, 'b list> =
        Seq.fold (fun m (x,y) -> m |> extendListValue x y) Map.empty xs

    let flattenOneToMany m =
        let flatten = (fun (KeyValue(x, ys)) -> ys |> Seq.map (mkTuple x))
        Seq.collect flatten m

    /// Takes two maps and merges them into a single map, using the given
    /// function to combine the values from the two maps into a single value
    let merge f mapA mapB =
       let allKeys = [mapA; mapB] |> Seq.collect keys |> Seq.distinct

       (Map [], allKeys)
       ||> Seq.fold (fun acc key ->
           let aValue = mapA |> Map.tryFind key
           let bValue = mapB |> Map.tryFind key
           acc |> Map.add key (f aValue bValue)
       )

