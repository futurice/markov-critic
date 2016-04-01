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
        |> List.filter(fun x -> x = None)
        |> List.map(fun x -> 
                            let (word, freq) = x.Value
                            {Word = word; Frequency = freq})
    
let run =         //todo make this accept a seq of strings, or open a file, or something
    let input_corpus = seq[ """Anything that happens, happens. 
 Anything that, in happening, causes something else to  
 happen, causes something else to happen.  
 Anything that, in happening, causes itself to happen again,                       
 happens again. 

    It doesn't necessarily do it in chronological order, though. 
The history of the Galaxy has got a little muddled, for a
number of reasons: partly because those who are trying to keep
track of it have got a little muddled, but also because some very
muddling things have been happening anyway.

One of the problems has to do with the speed of light and
the difficulties involved in trying to exceed it. You can't. Nothing
travels faster than the speed of light with the possible exception
of bad news, which obeys its own special laws. The Hingefreel
people of Arkintoofle Minor did try to build spaceships that were
powered by bad news but they didn't work particularly well and
were so extremely unwelcome whenever they arrived anywhere
that there wasn't really any point in being there.

So, by and large, the peoples of the Galaxy tended to languish
in their own local muddles and the history of the Galaxy itself
was, for a long time, largely cosmological.
  
Which is not to say that people weren't trying. They tried
sending off fleets of spaceships to do battle or business in
distant parts, but these usually took thousands of years to get
anywhere. By the time they eventually arrived, other forms of
travel had been discovered which made use of hyperspace to
circumvent the speed of light, so that whatever battles it was
that the slower-than-light fleets had been sent to fight had already
been taken care of centuries earlier by the time they actually got
there .""";

"""Anything that happens, happens. 
 Anything that, in happening, causes something else to  
 happen, causes something else to happen.  
 Anything that, in happening, causes itself to happen again,                       
 happens again. 

    It doesn't necessarily do it in chronological order, though. 
The history of the Galaxy has got a little muddled, for a
number of reasons: partly because those who are trying to keep
track of it have got a little muddled, but also because some very
muddling things have been happening anyway.

One of the problems has to do with the speed of light and
the difficulties involved in trying to exceed it. You can't. Nothing
travels faster than the speed of light with the possible exception
of bad news, which obeys its own special laws. The Hingefreel
people of Arkintoofle Minor did try to build spaceships that were
powered by bad news but they didn't work particularly well and
were so extremely unwelcome whenever they arrived anywhere
that there wasn't really any point in being there.

So, by and large, the peoples of the Galaxy tended to languish
in their own local muddles and the history of the Galaxy itself
was, for a long time, largely cosmological.
  
Which is not to say that people weren't trying. They tried
sending off fleets of spaceships to do battle or business in
distant parts, but these usually took thousands of years to get
anywhere. By the time they eventually arrived, other forms of
travel had been discovered which made use of hyperspace to
circumvent the speed of light, so that whatever battles it was
that the slower-than-light fleets had been sent to fight had already
been taken care of centuries earlier by the time they actually got
there ."""
                          ]
     
    let tokens = input_corpus |> Seq.collect (fun line -> 
                                    line.Replace(",", " ,")
                                        .Replace(".", " .")
                                        .Replace("!", " !")
                                        .Replace("?", " ?")
                                        .Replace("\n", "")
                                        .Split([|" "|], StringSplitOptions.RemoveEmptyEntries))         

    let pairs = split [] (Seq.toList tokens)                                                    

    let freq_table = pairs 
                    |> List.map(fun (x, _) -> x, matched x) 
                    |> List.map(fun (x, y) -> x, (markoved y))
                    |> Map.ofList                
    
    //just printing                
    freq_table |> Map.iter (fun key value -> printfn "Key: %s, Value: %A" key value) 

    ()