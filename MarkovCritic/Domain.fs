module Domain

type Id3 = { Title: string;
             Album: string;
             Year: uint32;
             Performers: string list}

type Mp3 = { Id3: Id3;
             Duration: System.TimeSpan }