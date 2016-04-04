module CorpusGenerator

open System
open Newtonsoft
open Domain

type MarkovPair = {
    Word : string;
    Frequency : double;
    Cumulative : double;
}

let rec split (values : List<string * string * string>) (list : List<string>) : List<(string * string * string)> =   
    match list with
    | x::y::z::xs -> split ((x,y,z) :: values) (z :: xs)
    | _ -> values

let matched (x : string * string) (triples : List<(string * string * string)>) =
       let filtered = triples |> List.filter ( fun (xx, yy, _) -> x = (xx, yy) ) 
       let mapped = filtered |> List.map (fun (_, __, yy) -> yy)
       mapped

let frequency (value:string) (list:List<string>) : Option<(string * float)> =
    list |> List.filter ((=) value)
    |> List.countBy id
    |> List.tryFind (fun _ -> true)
    |> Option.map (fun (key, count) -> key, 100.0 * ( float count / float list.Length ))

let toMarkovPairs list : List<MarkovPair> =
    list 
    |> List.distinct 
    |> List.map(fun x -> frequency x list)
    |> List.choose id
    |> List.mapFold(fun state x -> 
        let (word, freq) = x                                    
        ({Word = word; Frequency = freq; Cumulative = state + freq},state + freq))
        0.0 
    |> fst    

let getFreqTable (inp : seq<string>) : Map<string * string, List<MarkovPair>> = 
    let tokens = inp 
                |> Seq.collect (fun line -> 
                    line.Replace(".", " ")
                        .Replace("!", " ")
                        .Replace("?", " ")
                        .Replace("\n", " ")
                        .Replace("\r", " ")
                        .Replace("\r\n", " ")
                        .Split([|" "|], StringSplitOptions.RemoveEmptyEntries))   
    let triples = split [] (Seq.toList tokens)            
    triples |> List.map(fun (x, y, _) -> (x,y), matched (x,y) triples)        
            |> List.map(fun (x, y) -> x, (toMarkovPairs y)) 
            |> Map.ofList      


let run opinion =
    let opinionString = sprintf "%A" opinion
    printfn "Generating the %s table..." opinionString

    let freq_table = match opinion with
                     | Wow -> getFreqTable [System.IO.File.ReadAllText("good-reviews.txt")]
                     | Meh -> getFreqTable [System.IO.File.ReadAllText("meh-reviews.txt")]
                     | Ugh -> getFreqTable [System.IO.File.ReadAllText("bad-reviews.txt")]
    
    let outFileName = ".\\" + opinionString + "Table.json"
    printfn "%s table generated! Written out to %s" outFileName (System.IO.Path.GetFullPath outFileName)
    let opinionString = sprintf "%A" opinion
    let serializedString = Newtonsoft.Json.JsonConvert.SerializeObject freq_table 
    System.IO.File.WriteAllLines(outFileName, [serializedString], Text.Encoding.UTF8) 

