module Spotify

open Domain
open FSharp.Data;

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

let search title = 
    async { let! response =
             Http.AsyncRequestString ("https://api.spotify.com/v1/search",
                                      query = [ "q", title; "type", "track"; "limit", "5" ])

            let getPairs json = 
                let test = Provider.Parse(json)
                seq { for record in test.Tracks.Items do 
                        yield { Title = record.Name;
                                Artist = record.Artists.[0].Name;
                                Popularity = record.Popularity }  }

            return getPairs response |> Seq.toList
        } 