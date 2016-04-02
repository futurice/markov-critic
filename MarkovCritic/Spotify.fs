module Spotify

open Domain
open FSharp.Data

[<Literal>]
let private format =  """
{
"tracks": {
    "items": [ {
        "artists": [ {
            "name":"Justin Biber"
            } ],
        "popularity": -100,
        "name":"Baby"
        } ]
    }
}
"""
type private Provider = JsonProvider<format>

let search title performerOption = 
    async { 
            let! response =
             Http.AsyncRequestString ("https://api.spotify.com/v1/search",
                                      query = [ "q", title; "type", "track"; "limit", "20" ])

            let getPairs json = 
                let test = Provider.Parse(json)
                seq { for record in test.Tracks.Items do 
                        yield { Title = record.Name;
                                Artist = record.Artists.[0].Name;
                                Popularity = record.Popularity }  }

            let pri x =
                printfn "%A %A" performerOption x
                x

            let sameAuthor author =
                let toUpperCase (str: string) = str.ToUpper();
                performerOption |> Option.map toUpperCase
                                |> Option.filter ((toUpperCase >> (=)) author)
                                |> Option.isSome

            return response |> getPairs  
                            |> Seq.toList 
                            |> List.tryFind (fun it -> sameAuthor it.Artist )
        } 