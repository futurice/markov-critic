﻿module Player

open NAudio;
open NAudio.Wave;
open Domain

let play mp3File description = 
    use wo = new NAudio.Wave.WaveOut()
    let (Mp3File name) = mp3File
    let fileName = name.Split [|'\\'|] |> Array.last
    printfn "%s %A" description fileName 
    use audioFileReader = new AudioFileReader(name);
    wo.Init(audioFileReader);
    wo.Play();
    System.Console.ReadKey() |> ignore
