module Directory

open Domain

let listMp3Files dir = 
    let folder = new System.IO.DirectoryInfo(dir)
    folder.GetFiles() 
        |> Array.filter (fun it -> it.Extension = ".mp3")
        |> List.ofArray
        |> List.map (fun it -> Mp3File it.FullName)