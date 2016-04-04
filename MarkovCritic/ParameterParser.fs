module ParameterParser

open  Domain

let rec private parseCommandLineRec args optionsSoFar = 
        match args with 
        | [] -> 
            Some optionsSoFar  

        | "-p"::xs | "--path"::xs-> 
            match xs with 
                | [] -> None
                | x::xss -> parseCommandLineRec xss { optionsSoFar with Path = x} 
        | "-g"::xs | "--generate"::xs ->
            match xs with
            | [] -> None
            | x::xss -> match x with
                        | "Wow" -> parseCommandLineRec xss {optionsSoFar with GenerateQuality = Some(Opinion.Wow)}
                        | "Meh" -> parseCommandLineRec xss {optionsSoFar with GenerateQuality = Some(Opinion.Meh)}
                        | "Ugh" -> parseCommandLineRec xss {optionsSoFar with GenerateQuality = Some(Opinion.Ugh)}
                        | _ -> None
        | "-h"::xs | "--help"::xs ->
            printf """ 
Available options:
    -h,--help                Show this help text
    -f,--file file           File name
    -p,--path PATH           Path to file
    -g,--generate Quality    Either "Wow", "Meh", or "Ugh". 
                   """
            None
        | x::xs -> 
            printfn "Option '%s' is unrecognized" x
            parseCommandLineRec xs optionsSoFar 

let parseCommandLine args = 
    let defaultOptions = { Path = "./"; GenerateQuality = None;}

    parseCommandLineRec args defaultOptions