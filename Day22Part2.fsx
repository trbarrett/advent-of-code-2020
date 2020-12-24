let calculateScore deck =
   deck |> List.rev |> List.mapi (fun i x -> (i + 1) * x) |> List.sum

type Hand = int list
type Hands = Hand * Hand
type Result = | Player1Win of Hand | Player2Win of Hand

let doesNotNeedToPlaySubgame (player1, player2) =
    if player1 |> List.isEmpty || player2 |> List.isEmpty then
        None
    elif List.max player1 > List.max player2 then
        // player with max cards would always win normally, but the infinite
        // tiebreaker rule means player1 might actually win some games (not sure)
        // but if player1 has the max then there's no need to play
        Some (Player1Win player1)
    else
        None

let rec playGame hands previousRounds : Result =
    if previousRounds |> Set.contains hands then
        Player1Win (hands |> fst) // Abort with player 1 win if we see a result we've already seen
    else
        match hands with
        | xs, [] -> Player1Win xs
        | [], ys -> Player2Win ys
        | x::xs, y::ys ->
            let winner =
                if x <= (xs |> List.length) && y <= (ys |> List.length) then
                    let nextHands = (xs |> List.take x, ys |> List.take y)
                    match doesNotNeedToPlaySubgame nextHands with
                    | Some win -> win
                    | None -> playGame nextHands Set.empty
                elif x > y then
                    Player1Win (hands |> fst)
                else
                    Player2Win (hands |> snd)
            match winner with
            | Player1Win _ -> playGame ((xs@[x;y]), ys) (previousRounds |> Set.add hands)
            | Player2Win _ -> playGame (xs, (ys@[y;x])) (previousRounds |> Set.add hands)

//let hands = ([9;2;6;3;1], [5;8;4;7;10]) // example
let sw = System.Diagnostics.Stopwatch.StartNew()
let hands =
     ([40;26;44;14;3;17;36;43;47;38;39;41;23;28;49;27;18;2;13;32;29;11;25;24;35],
      [19;15;48;37;6;34;8;50;22;46;20;21;10;1;33;30;4;5;7;31;12;9;45;42;16])
playGame hands Set.empty
|> function | Player1Win hand | Player2Win hand -> calculateScore hand
|> printfn "%A"
printfn "took: %Ams" sw.ElapsedMilliseconds
