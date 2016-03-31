module Markov

open System

type MarkovPair = {
    Word : string;
    Frequency : double;
}

let rec readlines () = 
    seq {         
        let line = Console.ReadLine()
        if not <| String.IsNullOrEmpty(line) then
            yield line
            yield! readlines ()
    }     

let run = 
    let separators = [|","; "."; " "; "!"; "?";|]

    printfn "Enter a text corpus to generate the frequency table."
    let input_corpus = readlines()    
    let tokens = input_corpus |> Seq.collect (fun line -> line.Split(separators, StringSplitOptions.RemoveEmptyEntries))   
    
    printfn "%A" (Seq.toList tokens)

    let freq_table = Map.empty
    let freq_table = tokens 
                    |> Seq.mapi(fun i token -> 
                        let next_token = Seq.item i tokens
                        freq_table.Add(token, {Word = next_token; Frequency = 1.0})
                    )
    
    printfn "%A" freq_table                    

    ()

    
    
