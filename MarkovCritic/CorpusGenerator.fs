module CorpusGenerator

open System
open Newtonsoft
open Domain

type MarkovPair = {
    Word : string;
    Frequency : double;
    Cumulative : double;
}

let rec split (values : List<string * string>) (list : List<string>) : List<(string * string)> =
    match list with
    | x::y::xs -> split ((x,y) :: values) (y :: xs)
    | _ -> values

let matched (x : string) (pairs : List<(string * string)>) =
        pairs |> List.filter ( fun (xx, _) -> x = xx ) 
              |> List.map (fun (_, yy) -> yy)

let frequency (value:string) (list:List<string>) : Option<(string * float)> =
    list |> List.filter ((=) value)
    |> List.countBy id
    |> List.tryFind (fun _ -> true)
    |> Option.map (fun (key, count) -> key, 100.0 * ( float count / float list.Length ))

let toMarkovPairs list : List<MarkovPair> =
    let (markovPairs, endFreq) = list 
                                |> List.distinct 
                                |> List.map(fun x -> frequency x list)
                                |> List.choose id
                                |> List.mapFold(fun state x -> 
                                    let (word, freq) = x                                    
                                    ({Word = word; Frequency = freq; Cumulative = state + freq},state + freq)) 0.0 
    markovPairs  

let getFreqTable (input_corpus : seq<string>) : Map<string, List<MarkovPair>> = 
    let tokens = input_corpus |> Seq.collect (fun line ->                                     
                                    line.Replace(".", " ")
                                        .Replace("!", " ")
                                        .Replace("?", " ")
                                        .Replace("\n", " ")
                                        .Replace("\r", " ")
                                        .Replace("\r\n", " ")
                                        .Split([|" "|], StringSplitOptions.RemoveEmptyEntries))   

    let pairs = split [] (Seq.toList tokens)            
    pairs |> List.map(fun (x, _) -> x, matched x pairs)        
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

