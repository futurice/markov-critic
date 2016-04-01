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
    let randomWord = getRandomWord table.[key]    
    if words.Length < limit then
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
    let input_corpus = seq[ """Loaded with dazzling action but bereft of purpose, the Point Break remake will be remembered as the first film to make audiences pine for the simultaneous presences of Keanu Reeves and Gary Busey.
ove sees writer-director Gaspar Noé delivering some of his warmest and most personal work; unfortunately, it's also among his most undeveloped and least compelling.
In some respects, Alvin and the Chipmunks: The Road Chip is a marginal improvement over prior installments, although this in no way qualifies as a recommendation.
Kristen Wiig's vibrant performance is almost worth the price of admission -- and it has to be, because Hateship Loveship doesn't have much else going for it. 
Despite its impressive cast and some sharp observations, A.C.O.D. is neither funny enough nor poignant enough to work as a potent comedy or incisive satire. 
Colour Me Kubrick has a fascinating premise, but provides little insight into Kubrick and the man who impersonated him. 
Shallow script, weak direction. 
 Hathaway is a charming heroine, but the simple storyline gets overwhelmed by silly gimmickry. 
 Jurassic Park III is darker and faster than its predecessors, but that doesn't quite compensate for the franchise's continuing creative decline. 
 Gruesome, explicit and highly controversial; Lars Von Triers arthouse-horror, though beautifully shot, is no easy ride. 
 A too-short script and a romance lacking in heat detracts from an otherwise haughty charmer. 
 It's colorful and amiable enough, and Depp's heart is clearly in the right place, but The Rum Diary fails to add sufficient focus to its rambling source material. 
 John Huston proves an odd choice to direct, miring Annie in a sluggish, stagebound mess of an adaptation, but the kids are cute and the songs are memorable. """]

    let freq_table = getFreqTable input_corpus                                   

    
    let startingState = getRandomValue freq_table |> getRandomWord
    let markov_chain = generateMarkovChain freq_table startingState [] 10
    printfn "Markov chain: %A" markov_chain
    
    Console.ReadKey() |> ignore

    ()  