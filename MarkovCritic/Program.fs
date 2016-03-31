open Markov

[<EntryPoint>]
let main argv = 
    Markov.run
    printfn "%A" argv
    0 // return an integer exit code
