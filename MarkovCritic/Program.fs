﻿// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open Main
open System
open Critiquer
open Domain

[<EntryPoint>]
let main argv = 
    //let result = Main.main argv
    //Critiquer.run Opinion.Ugh
    CorpusGenerator.run
    Console.ReadKey() |> ignore
    0