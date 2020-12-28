let CalculatePosition (x, y) (direction: char) = 
    match direction with
    | '>' -> (x+1, y)
    | 'v' -> (x, y+1)
    | '<' -> (x-1, y)
    | '^' -> (x, y-1)
    | _   -> (x, y)

let SolveA (input: string) =
    let mutable positions = [( 0, 0 )]

    let rec processDirections (n: int) =
        positions <- CalculatePosition positions.[0] input.[n] :: positions 

        if n < input.Length-1 then processDirections (n+1)
        else (Set.ofList positions).Count

    printfn "AnswerA: %i" (processDirections 0)

let SolveB (input: string) =
    let mutable positionsA, positionsB = [( 0, 0 )], [( 0, 0 )]

    let rec processAlternateDirections (n: int) (state: bool) =
        match state with
        | true  -> positionsA <- CalculatePosition positionsA.[0] input.[n] :: positionsA
        | false -> positionsB <- CalculatePosition positionsB.[0] input.[n] :: positionsB

        if n < input.Length-1 then processAlternateDirections (n+1) (not state)
        else (Set.ofList (positionsA @ positionsB)).Count
    
    printfn "AnswerB: %i" (processAlternateDirections 0 true)

[<EntryPoint>]
let main argv =
    let directions = System.IO.File.ReadAllText("input.txt")
    SolveA directions
    SolveB directions
    0
