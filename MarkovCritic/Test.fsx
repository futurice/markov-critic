#r "../packages/NAudio.1.7.3/lib/net35/NAudio.dll"
#r "../packages/taglib.2.1.0.0/lib/taglib-sharp.dll"
#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
#load "Domain.fs"
#load "Loader.fs"
#load "Spotify.fs"
#load "Evaluate.fs"

open NAudio;
open NAudio.Wave;
open FSharp.Data;
open TagLib;
open Domain
open Spotify

let list = [@"C:\Users\Tomasz\OneDrive\Music\Linkin Park - Roads Untraveled.mp3";
       //     @"C:\Users\Tomasz\OneDrive\Music\My Demons - Starset (lyrics).mp3";
        //    @"C:\Users\Tomasz\OneDrive\Music\Ram Jam Black Betty.mp3";
            @"D:\temp\01 Once Upon a Time.mp3" ]

list |> List.map Evaluate.evaluate
     |> Async.Parallel
     |> Async.RunSynchronously
     |> Array.choose id



