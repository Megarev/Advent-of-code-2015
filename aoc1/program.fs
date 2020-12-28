let SolveA (input: string) =
    input
    |> Seq.toList
    |> List.sumBy (int >> (fun a -> int ')' - a))
    |> printfn "AnswerA: %i"

let SolveB (input: string) =
    let rec findPos (n: int) (sum: int) (charList: list<int>) =
        if sum = -1 || n > charList.Length then n else findPos (n+1) (sum+charList.[n]) charList

    input
    |> Seq.toList
    |> List.map (int >> (fun a -> int ')' - a))
    |> (findPos 0 0)
    |> printfn "AnswerB: %i"

[<EntryPoint>]
let main argv =
    let input = System.IO.File.ReadAllText("input.txt").Replace(")", "*")
    SolveA input
    SolveB input
    0
