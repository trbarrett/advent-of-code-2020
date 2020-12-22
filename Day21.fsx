#load "./Helper.fsx"
open Helper
open System.Text.RegularExpressions

let parse line =
    let m = Regex.Match(line, "^((\w+)\s+)+\(contains ((\w+),?\s?)+\)$")
    m.Groups.[2].Captures |> Seq.map (fun x -> x.Value),
    m.Groups.[4].Captures |> Seq.map (fun x -> x.Value)

let findPossIngredientsForAl items allergen =
    let allWithAllergen =
        items
        |> Seq.filter (fun (_,allergens) -> allergens |> Seq.contains allergen)
        |> Seq.unzip |> fst
    // we can collect them into one list and count them, because we know
    // that each ingredient is distinct for each list
    allWithAllergen |> Seq.collect id |> Seq.countBy id
    |> Seq.filter (snd >> ((=) (allWithAllergen |> Seq.length)))
    |> Seq.map fst

let rec resolveMatching (possibilities : (string * string seq) list) =
    match possibilities with
    | [] -> []
    | _ ->
        // one will just have one possibility, so extract that first
        let sorted = possibilities |> List.sortBy (snd >> Seq.length)
        let (allergen, ingredient) =
            sorted |> List.head
            |> fun (x, allergens) -> (x, allergens |> Seq.item 0)

        // remove the ingredient we just found from the rest of the possibilities
        let remaining =
            sorted.Tail
            |> List.map (fun (a, possIngredients) ->
                a, possIngredients |> Seq.filter (fun i -> i <> ingredient))

        (allergen, ingredient) :: resolveMatching remaining

let countNonAllergens items allergenIngredients =
    let allergenIngredients = allergenIngredients |> Set
    items |> Seq.unzip |> fst |> Seq.collect id
    |> Seq.filter (fun x -> Set.contains x allergenIngredients |> not)
    |> Seq.length

let findAllergenIngredients (items : (string seq * string seq) seq) =
    let allAllergens = items |> Seq.collect snd |> Seq.distinct
    let possibiltiies =
        allAllergens |> Seq.map (fun al -> al, findPossIngredientsForAl items al)
    resolveMatching (possibiltiies |> Seq.toList)

let items = readInput "day21.txt" |> Seq.map parse |> Seq.toList
let matchedAllergens = findAllergenIngredients items

matchedAllergens |> List.unzip |> snd |> countNonAllergens items
|> printfn "Part1: %d"

matchedAllergens |> List.sortBy fst |> List.unzip |> snd |> String.concat ","
|> printfn "Part2: %s"
