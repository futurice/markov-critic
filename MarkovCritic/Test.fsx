#r "../packages/NAudio.1.7.3/lib/net35/NAudio.dll"
#r "../packages/taglib.2.1.0.0/lib/taglib-sharp.dll"
#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"
#load "Domain.fs"
#load "Loader.fs"

open NAudio;
open NAudio.Wave;
open FSharp.Data;
open TagLib;
open Domain

let test = query "baby" |> Async.RunSynchronously 


