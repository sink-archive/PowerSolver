open System
open System.Diagnostics

// dont make me deal with c# janky math funcs in f#
let round (places: int) (num: float) = Math.Round(num,places)

let roundDecimal = round 13 // this number sets how many decimal places we actually care about

let consoleIn (message : string) =
    Console.Write (message + ": ")
    Console.ReadLine() |> float

// should be a recursive binary search
let rec solveRecurse x z interval current =
    if roundDecimal (x ** current) = roundDecimal z then current
    else    
        let next adjCurrent adjInterval = solveRecurse x z adjInterval adjCurrent
        if x ** current < z then
            next (current + interval) interval
        else
            let newInterval = interval / 2.0
            next (current - newInterval) newInterval
            
let solve x z = solveRecurse x z 1.0 1.0

[<EntryPoint>]
let main argv =
    Console.WriteLine "in the equation x^y = z:"
    let x = consoleIn "please enter x"
    let z = consoleIn "please enter z"
    
    let sw = Stopwatch.StartNew()
    let solved = solve x z
    sw.Stop()
    
    printfn $"Solved: %f{x}^%s{string solved} = %f{z}, Took %i{sw.ElapsedMilliseconds}ms to compute"
    
    0 // return an integer exit code