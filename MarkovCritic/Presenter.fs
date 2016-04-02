module Presenter

open Domain

let present critique = 
    let { Opinion = opinion ; Critique = markov_chain } = critique
    sprintf "%A: %s" opinion markov_chain