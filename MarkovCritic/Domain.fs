module Domain

type Id3 = { Title: string;
             Album: string;
             Year: uint32;
             Performers: string option}

type SpotifyMetadata = { Title: string; 
                         Artist: string;
                         Popularity: int }

type Mp3 = { Id3: Id3;
             Duration: System.TimeSpan }

type Input = { Path: string; }

type Opinion = 
         | Wow
         | Meh
         | Ugh

type Mp3File = Mp3File of string