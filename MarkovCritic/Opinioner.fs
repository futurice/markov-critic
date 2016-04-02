module Opinioner

open Domain

let makeOpinion = function
    | Some { Popularity = popularity } when popularity >= 50 -> 
        Wow
    | Some _ ->
        Ughhh
    | None -> 
        Meh
        