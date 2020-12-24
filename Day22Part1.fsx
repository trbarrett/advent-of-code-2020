let calculateScore deck =
   deck |> List.rev |> List.mapi (fun i x -> (i + 1) * x) |> List.sum

let rec playGame (player1, player2) =
    match player1, player2 with
    | player1, [] -> calculateScore player1 // player1 win
    | [], player2 -> calculateScore player2 // player2 win
    | x::xs, y::ys ->
        if x > y
        then playGame ((xs@[x;y]), ys)
        else playGame (xs, (ys@[y;x]))

//([9;2;6;3;1], [5;8;4;7;10]) // example
([40;26;44;14;3;17;36;43;47;38;39;41;23;28;49;27;18;2;13;32;29;11;25;24;35],
 [19;15;48;37;6;34;8;50;22;46;20;21;10;1;33;30;4;5;7;31;12;9;45;42;16])
|> playGame |> printfn "%A"
