open System

type MarkovPair = {
    Word : string;
    Frequency : double;
}

let rec split (values : List<string * string>) list =
    match list with
    | x::y::xs -> split ((x,y) :: values) (y :: xs)
    | _ -> values

let matched x pairs =
        pairs |> List.filter ( fun (xx, _) -> x = xx ) 
              |> List.map (fun (_, yy) -> yy)

let frequency value list =
    list |> List.filter ((=) value)
    |> List.countBy id
    |> List.tryFind (fun _ -> true)
    |> Option.map (fun (key, count) -> key, 1.00  / ( float count / float list.Length ))

let markoved list : List<MarkovPair> =
    list |> List.map(fun x -> frequency x list)
        |> List.filter(fun x -> x.IsSome)
        |> List.map(fun x -> 
                            let (word, freq) = x.Value
                            {Word = word; Frequency = freq})
    
let run =
    let input_corpus = seq[ """It doesn't necessarily do it in chronological order, though. 
The history of the Galaxy has got a little muddled, for a
number of reasons: partly because those who are trying to keep
track of it have got a little muddled, but also because some very
muddling things have been happening anyway."""; """It doesn't necessarily do it in chronological order, though. 
The history of the Galaxy has got a little muddled, for a
number of reasons: partly because those who are trying to keep
track of it have got a little muddled, but also because some very
muddling things have been happening anyway."""]
     
    let tokens = input_corpus |> Seq.collect (fun line -> 
                                    line.Replace(",", " ,")
                                        .Replace(".", " .")
                                        .Replace("!", " !")
                                        .Replace("?", " ?")
                                        .Replace("\n", "")
                                        .Split([|" "|], StringSplitOptions.RemoveEmptyEntries))         

    let pairs = split [] (Seq.toList tokens)                                                    

    let freq_table = pairs 
                    |> List.map(fun (x, _) -> x, matched x pairs) 
                    |> List.map(fun (x, y) -> x, (markoved y))
                    |> Map.ofList                
    
    //just printing                
    freq_table |> Map.iter (fun key value -> printfn "Key: %s, Value: %A" key value) 

    ()