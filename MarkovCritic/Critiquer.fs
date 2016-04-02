module Critiquer

open System
open Domain

type MarkovPair = {
    Word : string;
    Frequency : double;
    Cumulative : double;
}

let rand = new System.Random()

let capitalize (str: String) : String = 
    String.Concat(str.[0].ToString().ToUpper(), str.Substring(1))

let getRandomValue (map : Map<string, List<MarkovPair>>) =     
    let size = map.Count    
    let values =  map |> Map.toList |> List.map(fun (key, value) -> value)   
    let randIndex = rand.Next(size - 1)  
    values |> List.item randIndex

let getRandomWord (list : List<MarkovPair>) : string =    
    let randWordVal = rand.NextDouble() * list.[list.Length - 1].Cumulative     
    let lastMatch = list 
                   |> List.find(fun x -> x.Cumulative >= randWordVal )
    lastMatch.Word

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
    

let rec generateMarkovChain (table : Map<string, List<MarkovPair>>) (key : string) (words : List<string>) (limit : int) =     
    let hasKey = table.ContainsKey(key)
    match hasKey with
        | true ->                
            if words.Length < limit then
                let randomWord = getRandomWord table.[key]                
                generateMarkovChain table randomWord (words @ [randomWord]) limit
            else
                words
        | false ->
            //if the table doesn't contain the key, just get a new random and try again
            generateMarkovChain table (getRandomValue table |> getRandomWord) words limit 
                   
let getFreqTable (input_corpus : seq<String>) : Map<string, List<MarkovPair>> = 
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
    
let run opinion fTable =    
    let ughCorpus = [""""""]
    let mehCorpus = seq[""" """]
    let wowCorpus = seq[""" """]

    let punctuation = ["."; "?"; "!";]

    let freq_table = match opinion with
                     | Wow -> fTable wowCorpus
                     | Meh -> fTable mehCorpus
                     | Ugh -> fTable ughCorpus
    
    while true do
        let startingState = getRandomValue freq_table |> getRandomWord        
        let markov_chain = generateMarkovChain freq_table startingState [startingState] 15                                          
                         |> List.reduce(fun state input -> state + " " + input)
//                         |> (fun x -> x.Replace(".", ".")
//                                       .Replace(" ,", ",")
//                                       .Replace(" !", "!")
//                                       .Replace(" ?", "?"))
                         |> capitalize                     
                         |> (fun x -> if not <| (punctuation |> List.contains (x.[x.Length - 1].ToString())) then (x + ".") else x)

        printfn "%A" markov_chain         
    Console.ReadKey() |> ignore              
    

    ()  