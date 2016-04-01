module Loader

open TagLib; 
open Domain

let interpret (mp3File: Mp3File) = 
    let (Mp3File name) = mp3File
    let file = TagLib.File.Create(name);
    let titlet = match (file.Tag.Title |> Option.ofObj) with
                 | Some s -> s | None -> name.Split [|'\\'|] 
                                            |> Array.last
                                            |> (fun str -> str.Replace(".mp3", ""))

    let id3 = { Title = titlet;
                Performer = Array.tryHead file.Tag.Performers }

    {Id3 = id3; Duration = file.Properties.Duration}

