module Main

open ParameterParser
open Loader
open Evaluate
open Opinioner
open Domain
open Player
open CorpusGenerator

let main argv = 
    let tables = Critiquer.getFreqTables
    let rec loop = function
        | x::xs -> x |> evaluate
                     |> Async.RunSynchronously
                     |> makeOpinion
                     |> Critiquer.run tables
                     |> Presenter.present
                     |> play x 
                     |> speak

                   loop xs
        | [] -> ()

    
        

    let parsedArgs = argv |> List.ofArray
                    |> parseCommandLine

    parsedArgs |> (fun x -> 
        match x with
        | Some(input) -> match input.GenerateQuality with
                         | Some(op) -> CorpusGenerator.run op                        
                         | None -> parsedArgs |> Option.map (fun { Path = p } -> p)
                                              |> Option.map Directory.listMp3Files
                                              |> (fun x -> if x.IsSome then loop x.Value)
        | None -> ())
        

    
