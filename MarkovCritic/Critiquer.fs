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
                   
let getFreqTables =
    let wowString = System.IO.File.ReadAllText("WowTable.json")
    let mehString = System.IO.File.ReadAllText("MehTable.json")
    let ughString = System.IO.File.ReadAllText("UghTable.json")

    let wowTable = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, List<MarkovPair>>> wowString
    let mehTable = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, List<MarkovPair>>> mehString
    let ughTable = Newtonsoft.Json.JsonConvert.DeserializeObject<Map<string, List<MarkovPair>>> ughString
    (wowTable, mehTable, ughTable)

let punctuation = ["."; "?"; "!";]

let run opinion (w, m, u) =           

    let freq_table = match opinion with
                     | Wow -> w
                     | Meh -> m
                     | Ugh -> u
        
    let startingState = getRandomValue freq_table |> getRandomWord        
    let markov_chain = generateMarkovChain freq_table startingState [startingState] 15                                          
                        |> List.reduce(fun state input -> state + " " + input)
                        |> capitalize                     
                        |> (fun x -> if not <| (punctuation |> List.contains (x.[x.Length - 1].ToString())) then (x + ".") else x)

    markov_chain                     