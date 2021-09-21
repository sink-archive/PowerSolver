open System
open System.Diagnostics

// tail recursive hybrid search
type SearchComparisonCheck = Finish | TooHigh | TooLow

type LinearRecResult =
        | Accurate of float
        | Inaccurate of float

let hybridSearch checkFunc =
    let rec linearRec evalFunc accumulator =
        match evalFunc accumulator with
        | Finish -> Accurate(accumulator)
        | TooHigh -> Inaccurate(accumulator)
        | TooLow -> linearRec evalFunc (accumulator + 1.0)

    let rec binaryRec checkFunc current interval =
        let evaluated = checkFunc current
        match evaluated with
        | Finish -> current
        | TooHigh -> binaryRec checkFunc (current - interval) (interval / 2.0)
        | TooLow -> binaryRec checkFunc (current + interval) (interval / 2.0)
    
    let linearRes = linearRec checkFunc 0.0
    match linearRes with
    | Accurate(result) -> result
    | Inaccurate(maxPoint) ->
        let startPoint = maxPoint - 0.5
        let interval = 0.25
        
        binaryRec checkFunc startPoint interval

let solve x z =
    let checkFunc current =
        match x ** current with
        | y when y = z -> Finish
        | y when y < z -> TooLow
        | _            -> TooHigh
    
    hybridSearch checkFunc


let consoleIn (message : string) =
    printf "%s" (message + ": ")
    Console.ReadLine() |> float

[<EntryPoint>]
let main _ =
    printfn "in the equation x^y = z:"
    let x = consoleIn "please enter x"
    let z = consoleIn "please enter z"
    
    let sw = Stopwatch.StartNew()
    let solved = solve x z
    sw.Stop()
    
    printfn $"Solved: %f{x} ^ %s{string solved} = %f{z}, Took %i{sw.ElapsedMilliseconds}ms to compute"
    
    0 // return an integer exit code