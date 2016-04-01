module Loader

open TagLib; 
open Domain

let interpret (mp3File: Mp3File) = 
    let (Mp3File name) = mp3File
    let file = TagLib.File.Create(name);
    let id3 = { Title = file.Tag.Title;
                Album = file.Tag.Album;
                Year = file.Tag.Year;
                Performers = Array.tryHead file.Tag.Performers }

    {Id3 = id3; Duration = file.Properties.Duration}