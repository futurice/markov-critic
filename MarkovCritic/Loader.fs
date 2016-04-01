module Loader

open TagLib; 
open Domain

let interpret (mp3File: Input) = 
    let path = mp3File.Path + mp3File.File
    printfn "%A" path
    let file = TagLib.File.Create(path);
    let id3 = { Title = file.Tag.Title;
                Album = file.Tag.Album;
                Year = file.Tag.Year;
                Performers = Array.tryHead file.Tag.Performers }

    {Id3 = id3; Duration = file.Properties.Duration}