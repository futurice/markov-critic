module Main

open ParameterParser
open Loader
open Evaluate
open Opinioner
open Domain

let main argv (pause: Unit -> Unit) = 

    let rec loop = function
        | x::xs -> x |> evaluate
                     |> Async.RunSynchronously
                     |> makeOpinion
                     |> printfn "%A"
                   pause()
                   loop xs
        | [] -> ()

    argv |> List.ofArray
         |> parseCommandLine
         |> Option.map (fun { Path = p } -> p)
         |> Option.map Directory.listMp3Files
         |> Option.map loop
