open System
open System.Diagnostics

// tail recursive generic binary search
type BinarySearchCheck = Finish | TooHigh | TooLow

let binarySearch evalFunc startPoint =
    let rec searchRec evalFunc current interval =
        let evaluated = evalFunc current
        match evaluated with
        | Finish -> current
        | TooHigh -> searchRec evalFunc (current - interval) (interval / 2.0)
        | TooLow -> searchRec evalFunc (current + interval) (interval / 2.0)
    
    searchRec evalFunc startPoint (startPoint / 2.0)

let solve x z =
    let evalFunc current =
        match x ** current with
        | y when y = z -> Finish
        | y when y < z -> TooLow
        | _            -> TooHigh
    
    binarySearch evalFunc (Math.Max(x, z))


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