#Markov Critic
An F# program that uses Markov Chains and Spotify's API to rate to deliver music reviews.

##Building
Currently only builds on Windows .NET, due to a dependency on `System.Speech` in  for reading the reviews allowed. If that functionality is removed, building on Mono should work.

##Usage
###Running

Just invoke `MarkovCritic.exe`. It will look in the `.exe`'s directory for any MP3 files, and issue judgments on them.

###Customizing
By default, the program uses three pre-generated frequency tables to generate its reviews. They are `WowTable.json`, `MehTable.json` and `UghTable.json`. These are included with the project (in the `/resources` folder), and placed in the output `bin` directory when the project is built. 

At runtime, the program will look for these tables in the same directory as the executable. 

If you'd like to generate new tables of your own, you can override the defaults by running `MarkovCritic.exe -g <Wow|Meh|Ugh>`. `Wow` will generate a frequency table by looking for a `good-reviews.txt` file. `Meh` and `Ugh` look for a `meh-reviews.txt` and `bad-reviews.txt` respectively. Each of these text files are expected to be found in the same directory as the executable. 

 
 

