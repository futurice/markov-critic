module Opinioner

open Domain

let makeOpinion = function
    | Some { Popularity = popularity } when popularity >= 40 -> 
        Wow
    | Some _ ->
        Ugh
    | None -> 
        Meh
        