module Critiquer

open System

type MarkovPair = {
    Word : string;
    Frequency : double;
    Cumulative : double;
}

let getRandomValue (map : Map<string, List<MarkovPair>>) = 
    let rand = new System.Random()
    let size = map.Count    
    let values =  map |> Map.toList |> List.map(fun (key, value) -> value)   
    let randIndex = rand.Next(size)  
    values |> List.item randIndex

let getRandomWord (list : List<MarkovPair>) : string =
    let rand = new System.Random()       
    let lastMatch = list |> List.findBack(fun x -> x.Cumulative >= rand.NextDouble() )    
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
    |> Option.map (fun (key, count) -> key, ( float count / float list.Length ))

let toMarkovPairs list : List<MarkovPair> =
    let (markovPairs, endFreq) = list 
                                |> List.distinct 
                                |> List.map(fun x -> frequency x list)
                                |> List.choose id
                                |> List.mapFold(fun state x -> 
                                    let (word, freq) = x
                                    let newFreq = if state + freq < 1.0 then state + freq else 1.0
                                    ({Word = word; Frequency = freq; Cumulative = state + freq},state + freq)) 0.0 
    markovPairs       
    

let rec generateMarkovChain (table : Map<string, List<MarkovPair>>) (key : string) (words : List<string>) (limit : int) =         
    if words.Length < limit then
        let randomWord = getRandomWord table.[key]        
        generateMarkovChain table randomWord (words @ [randomWord]) limit
    else
        words    
                   
let getFreqTable (input_corpus : seq<String>) : Map<string, List<MarkovPair>> = 
    let tokens = input_corpus |> Seq.collect (fun line -> 
                                    line.Replace(",", " ,")
                                        .Replace(".", " .")
                                        .Replace("!", " !")
                                        .Replace("?", " ?")
                                        .Replace("\n", " ")
                                        .Split([|" "|], StringSplitOptions.RemoveEmptyEntries))         

    let pairs = split [] (Seq.toList tokens)            

    pairs |> List.map(fun (x, _) -> x, matched x pairs)        
                    |> List.map(fun (x, y) -> x, (toMarkovPairs y)) 
                    |> Map.ofList
    
let run =    
    let input_corpus = seq[ """It doesn't necessarily do it in chronological order, though. 
The history of the Galaxy has got a little muddled, for a
number of reasons: partly because those who are trying to keep
track of it have got a little muddled, but also because some very
muddling things have been happening anyway."""; 
"""It doesn't necessarily do it in chronological order, though. 
The history of the Galaxy has got a little muddled, for a
number of reasons: partly because those who are trying to keep
track of it have got a little muddled, but also because some very
muddling things have been happening anyway. for for for for"""]

    let freq_table = getFreqTable input_corpus                       
    
    //just printing  
    printfn "Frequency table:"              
    freq_table |> Map.iter (fun key value -> printfn "\tKey: %s, Value: %A" key value) 

    let startingState = getRandomValue freq_table |> getRandomWord
    let markov_chain = generateMarkovChain freq_table startingState [] 10

    printfn "Markov chain: %A" markov_chain

    ()  