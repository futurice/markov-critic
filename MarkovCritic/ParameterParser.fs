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
        | "-f"::xs | "--file"::xs -> 
            match xs with 
                | [] -> None
                | x::xss -> parseCommandLineRec xss { optionsSoFar with File = x} 

        | "-h"::xs | "--help"::xs ->
            printf """ 
Available options:
    -h,--help                Show this help text
    -f,--file file           File name
    -p,--path PATH           Path to file
                   """
            parseCommandLineRec xs optionsSoFar
        | x::xs -> 
            printfn "Option '%s' is unrecognized" x
            parseCommandLineRec xs optionsSoFar 

let parseCommandLine args = 
    let defaultOptions = { File = "";
                           Path = "./"}

    parseCommandLineRec args defaultOptions