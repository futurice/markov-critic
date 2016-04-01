#r "../packages/NAudio.1.7.3/lib/net35/NAudio.dll"

open NAudio;
open NAudio.Wave;

let test (mp3File: string) = 
    use file1 = new Mp3FileReader(mp3File)
    printf "%A" file1
    WaveFileWriter.CreateWaveFile(@"C:\Users\Tomasz\OneDrive\Music\test.wav", file1);
    
test @"C:\Users\Tomasz\OneDrive\Music\moby flower.mp3" 