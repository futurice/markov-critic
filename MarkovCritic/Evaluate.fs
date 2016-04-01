module Evaluate

let evaluate file =
     file |> Loader.interpret
          |> (fun it -> Spotify.search it.Id3.Title it.Id3.Performer)