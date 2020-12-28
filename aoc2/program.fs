let SolveA (values: int[][]) =
    let rec iterate (sum: int) (n: int) =
        let area (input: int[]) = 2 * (input.[0]*input.[1] + input.[1]*input.[2] + input.[2]*input.[0]) + input.[0]*input.[1] 
        if n < values.Length-1 then iterate (sum+area values.[n].[0..2]) (n+1)
        else (sum+area values.[n].[0..2])

    printfn "AnswerA: %i" (iterate 0 0)

let SolveB (values: int[][]) =
    let rec iterate (sum: int) (n: int) =
        let perimeter (input: int[]) = 2 * (input.[0] + input.[1])
        let volumePerimter = values.[n].[0..2]
                             |> Array.reduce (fun a b -> a * b)
                             |> (fun a -> a + perimeter values.[n].[0..2])
                             
        if n < values.Length-1 then iterate(sum+volumePerimter) (n+1)
        else (sum+volumePerimter)

    printfn "AnswerB: %i" (iterate 0 0)

[<EntryPoint>]
let main argv =
    let intList = System.IO.File.ReadAllLines("input.txt")
                  |> Array.map ((fun a -> [| yield! (a.Split('x')) |> Array.map (int) |> Array.sort |]))
    SolveA intList
    SolveB intList
    0
