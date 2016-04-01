module Loader

open TagLib; 
open Domain

let interpret (mp3File: string) = 
    let file = TagLib.File.Create(mp3File);
    let id3 = { Title = file.Tag.Title;
                Album = file.Tag.Album;
                Year = file.Tag.Year;
                Performers = Array.toList file.Tag.Performers}
    {Id3 = id3; Duration = file.Properties.Duration}