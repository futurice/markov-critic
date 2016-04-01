#r "../packages/FSharp.Data.2.2.5/lib/net40/FSharp.Data.dll"

open FSharp.Data

let results = FSharp.Data.HtmlDocument.Load("http://www.rottentomatoes.com/browse/dvd-all/?maxTomato=50&services=amazon;amazon_prime;flixster;hbo_go;itunes;netflix_iw;vudu#")
