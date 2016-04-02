module Main

open ParameterParser
open Loader
open Evaluate
open Opinioner
open Domain
open Player

let main argv = 
    let rec loop = function
        | x::xs -> x |> evaluate
                     |> Async.RunSynchronously
                     |> makeOpinion
                     |> Critiquer.run
                     |> Presenter.present
                     |> play x

                   loop xs
        | [] -> ()

    argv |> List.ofArray
         |> parseCommandLine
         |> Option.map (fun { Path = p } -> p)
         |> Option.map Directory.listMp3Files
         |> Option.map loop
