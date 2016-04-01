﻿module Critiquer

open System
open Domain

type MarkovPair = {
    Word : string;
    Frequency : double;
    Cumulative : double;
}

let rand = new System.Random()

let capitalize (str: String) : String = 
    String.Concat(str.[0].ToString().ToUpper(), str.Substring(1))

let getRandomValue (map : Map<string, List<MarkovPair>>) =     
    let size = map.Count    
    let values =  map |> Map.toList |> List.map(fun (key, value) -> value)   
    let randIndex = rand.Next(size - 1)  
    values |> List.item randIndex

let getRandomWord (list : List<MarkovPair>) : string =    
    let randWordVal = rand.NextDouble() * list.[list.Length - 1].Cumulative     
    let lastMatch = list 
                   |> List.find(fun x -> x.Cumulative >= randWordVal )
    lastMatch.Word

let rec split (values : List<string * string>) (list : List<string>) : List<(string * string)> =
    match list with
    | x::y::xs -> split ((x,y) :: values) (y :: xs)
    | _ -> values

let matched (x : string) (pairs : List<(string * string)>) =
        pairs |> List.filter ( fun (xx, _) -> x = xx ) 
              |> List.map (fun (_, yy) -> yy)

let frequency (value:string) (list:List<string>) : Option<(string * float)> =
    list |> List.filter ((=) value)
    |> List.countBy id
    |> List.tryFind (fun _ -> true)
    |> Option.map (fun (key, count) -> key, 100.0 * ( float count / float list.Length ))

let toMarkovPairs list : List<MarkovPair> =
    let (markovPairs, endFreq) = list 
                                |> List.distinct 
                                |> List.map(fun x -> frequency x list)
                                |> List.choose id
                                |> List.mapFold(fun state x -> 
                                    let (word, freq) = x                                    
                                    ({Word = word; Frequency = freq; Cumulative = state + freq},state + freq)) 0.0 
    markovPairs       
    

let rec generateMarkovChain (table : Map<string, List<MarkovPair>>) (key : string) (words : List<string>) (limit : int) =                
    let randomWord = getRandomWord table.[key]        
    if words.Length < limit then
        //todo: if the key is the last element in any of the corpora, pick a new key. otherwise, we crash. oops.        
        generateMarkovChain table randomWord (words @ [randomWord]) limit
    else
        words
                   
let getFreqTable (input_corpus : seq<String>) : Map<string, List<MarkovPair>> = 
    let tokens = input_corpus |> Seq.collect (fun line ->                                     
                                    line.Replace(".", " ")
                                        .Replace("!", " ")
                                        .Replace("?", " ")
                                        .Replace("\n", " ")
                                        .Replace("\r", " ")
                                        .Replace("\r\n", " ")
                                        .Split([|" "|], StringSplitOptions.RemoveEmptyEntries))         

    let pairs = split [] (Seq.toList tokens)            

    pairs |> List.map(fun (x, _) -> x, matched x pairs)        
                    |> List.map(fun (x, y) -> x, (toMarkovPairs y)) 
                    |> Map.ofList
    
let run opinion =    
    let ughCorpus = seq[ """ How did they get it so wrong? How could they remake the beloved 1990s action classic Point Break with such ineptitude and so little affection?
 It fully deserves to be played on a loop, silently, in extreme-sports goods shops everywhere.
 Whereas the original was filled with humour and gung-ho machismo, this Fast and Furious-style update is a joyless experience.
 We have no emotional connection with these characters, nor do we care whether they survive or not.
 Steer well clear of this flatly disappointing remake, as it seems hell-bent on stripping out everything that made the original movie so much fun.
 Pointless. Heartbreaking.
 It's so unbelievably self-important, with occasionally genuine and hazardous stunt work badly compromised by the annoying ponderousness of it all, meaning that we wind up simply not caring who's risking their lives or who wants to get into whose pants.
 [An] adrenaline-fuelled but empty rehash that flies close enough to Bigelow's movie to remind you how much you'd much rather be watching the original.
 Director Ericson Core has ditched the fun of the Patrick Swayze/Keanu Reeves original and replaced it with soulless action scenes.
 The stunts are impressive. The rest is a wash out.
 This is a high-grade action thriller that -- dare I suggest it? -- redeems the Point Break brand. And, yes, there's definitely room for a sequel.
 Point Break? More like Point Miss.
 Bracey's emotionally scarred Johnny Utah is so wooden he makes Keanu look like Kenneth Branagh.
 We're left scratching our head, wondering what it was all for. 
 Core revels in the beauty of nature and the danger of taming it, but showing such is meaningless if the plot is so weak it barely strings the destinations together.
 Relentless action cannot dispel the toxic levels of claptrap that stink up this po-faced, slow-witted remake.
 This iteration of Point Break effectively serves as a blueprint of exactly how not to reboot an action film.
 A complete failure. Who would've thought? [Full review in Spanish]
 Point Break aspires to be a decent remake, but winds up being one of the most boring action films ever made.
 If you like to see people pushing themselves to the physical brink, parts of the movie will absolutely hold your attention. If only the plot was that compelling.
 [Director] Carpignano has discovered a genuine performer with charisma and confidence in Koudous Seihon.
 Both topically and dramatically, this is a film that matters more than most when immigration has reached an explosive status with people fleeing misery being blamed for the Paris terrorist attack. 
 The film is unwaveringly attentive to problematizing the dividing line between predator and prey.
 A piercing character study whose narrow view frustrates complete empathy.
 There's a specificity to Mediterranea that at times makes it feel like an actual documentary.
 Too much directorial obfuscation keeps this powerful refugee tale from being as potent as it could be.
 This calm, hardheaded film never sacrifices its toughness for a swooning, misty-eyed moment of hope.
 Carpignano glosses over much of the sociopolitical context in his depictions of the chain of events.
 Offers a deliberately muted, finely textured account of the ordeals many Africans endure both before and after voyages to Europe in search of better lives.
 "Mediterranea" does not solely owe its topicality to recent events, like the tragic sinking of an immigrant boat bound for Italy off the coast of Libya that resulted in 400 deaths, it will induce a shiver of recognition for American viewers too.
 If you're the sort to wonder what it is that drives people to leave their homes to find work in other countries, or even other continents, then think of the film as an educational experience.
 Sad story about the struggles of two brothers from Burkina Faso who journey to Italy in hopes of finding a better life.
 This is a story packed with the sort of heartbreak and worry that would be hard to sit through were it not for American director Jonas Carpignano's touching, tender narrative skill.
 It's a slow-burn study in feeling powerless and unwelcome, anchored by Seihon's performance as a man patient and adept at sussing out and adapting to what others need until he just can't do it any longer.
 This debut feature from director Jonas Carpignano is often both harrowing and moving though this film festival favorite...falls short of some of the similar, and even more tragic, stories that can be heard just by turning on the news.
 Though realized with great empathy and tact, the film fails to convey the catastrophic extent of the situation it addresses by keeping its narrative too tightly focused.
 A timely humanistic immigration film.
 This is as accessible and as lovable as avant-garde filmmaking ever gets.
 Offers a loving and unvarnished look at one of Oklahoma's more eccentric stars.
 A poetic exploration of a moment, a place and an artist.
 Whiskery and restless, grooving and grotesque, the documentarian Les Blank's long-suppressed film A Poem Is a Naked Person plays like your memories of some mad, stoned last-century summer.
 A Poem Is A Naked Person is littered with striking moments that fit casually into Blank's study of fame and aspiration.
 Les Blank's long-lost Leon Russell documentary is a beguiling snapshot of a lost era.
 And this is A Poem is a Naked Person's accomplishment: composing a symphonic harmony of seemingly dissociated activities and subjects into one comprehensive, elegantly organized and, yes, even poetic whole.
 An inside look at the 1970's Leon Russell that is not much of an inside look at all.
 A perfect example of the down-home informality Blank made famous.
 "A Poem Is a Naked Person" is a tuneful peculiarity, capturing singer-songwriter Leon Russell and his bandmates, his friends, guest musicians and Oklahoma eccentrics.
 In essence, the real audience for this are Leon Russell aficionados, of which there are fewer today than in 1974.
 Russell embodies history of Southern rock, with roots proudly showing. . .Cultural heritage [as] living, breathing, rocking and rolling inspiration, is wonderfully preserved.
 In the '70s, this would've been an unusually intimate tour portrait. Now, it's a newly unearthed time capsule, the remarkable clarity of Blank's portrait compounded by the distance from which we're looking at it.
 I like Russell's music, but I learned absolutely nothing about him.
 A Poem is a Naked Person is a trippy time-capsule celebration of Russell and his music and of Oklahoma and its people.
 You begin to understand the eccentric characters and amazing weirdness of a (now lost) America that Blank was celebrating.
 Blank isn't afraid of the artistic wrath that he may incur and rather than making a Leon Russell film, has made what is defiantly a Les Blank film full of all of the idiosyncrasies that one would expect. 
 This film was never released in theaters - it was obviously way too far out and ragged for its own good. But it works nicely as a warped time capsule harkening back to strange days.
 Shot while hanging out with Russell across two years, "Poem" wriggles with weirdness and smells to high heaven of its 1970s roots. 
 Shot in the early 70s but shelved for 40 years, this portrait of R&B great Leon Russell (1974) immediately takes its place among the best rock docs.
 Therein lies Berardini's critique: using naught but his subjects' own words, he paints a portrait of men who have been dazzled by profits at the expense of their ethics.
 Though at times not for the fainthearted, Tom Swift and His Electric Rifle takes subject matter many of us probably give little thought to at all in day to day life and asks us to consider it deeply.
 An important account of shifts in policing tactics and the problematic interaction between law enforcement and private companies.
 Footage of the now-wealthy Smiths being deposed is damning, the brothers' legal jiujitsu is appalling, and the stories of deaths are heartbreaking.
 Relying entirely on flawed cases and weak lawsuits from the mid-2000s, "Killing Them Safely" is a very poor indictment of TASER International, the manufacturer of the widely used and mostly safe electronic weapon.
 A dark comedy about humanity's knack for quickie solutions and our ability to lie to ourselves, especially when our noble deeds go awry.
 Berardini's doc vigorously proves that despite their marketing, Tasers are not the answer. They are only means to a bigger, deadlier problem.
 Mr. Berardini's packed documentary makes its case early and often, perhaps too often, but it's more chilling than your average issue film.
 With the intensified focus on use of force in police departments, the unsettling documentary "Killing Them Safely" couldn't be timelier.
 Killing Them Safely is, above all, an example of excellent, ethical, fair, and balanced journalism allowing both sides to state their case.
 With adjusted expectations, the movie plays with surprising depth, inspecting the redemption of a ruined life with care and attention to thespian detail. 
 Solid and dependable, "Forsaken" shouldn't be forgotten. But I wish so much of it didn't feel half-remembered.
 It has a solid story to tell, and tells it with no winks and few, if any, frills. It's involving and ultimately exciting.
 The movie is too visually lovely to forsake, but the predictable story and basic plot holes remain unforgiven.
 [Screenwriter Brad] Mirman follows the Western template as if it were an exact science.
 Attempts to be a slow burn thriller building to a climactic shoot-out are more 'Back to the Future III' than 'High Noon,' but smaller character moments are deeply, genuinely felt by the actors. 
 It's hard to be overtly opinionated regarding Forsaken, because the plodding, uneventful little movie never asks more of you than basic consciousness; it's like the cinematic equivalent of cleaning out dryer lint.
 True, we have seen almost all the elements in "Forsaken" in a hundred Westerns of the past. But they're assembled with such care that there's room for version 101.
 ... conveys a gritty style and features solid performances, but most of the characters and themes seem recycled.
 So generic are these tribulations and story beats that they could have entered the realm of self-aware camp if they weren't played so straight. 
 Old-school western fans won't find a lot of originality here, but if you're looking for a well-executed, straight genre exercise, give it a shot.
 Forsaken greatly benefits from the poignant teaming of its father-and-son stars -- as well as Michael Wincott as an especially elegant and eloquent gunfighter who has great respect for John.
 Though it borrows liberally from the classic Shane and doesn't really offer anything new, this lowkey Western still works, thanks to patient storytelling and a batch of strong performances.
 Father and son team up for a Western in the classic style.
 In the end, there is nothing horribly wrong with Forsaken, but there isn't all that much right about it either. 
 Forsaken offers sufficient gun battles, standoffs, and shot-him-dead moments to keep fans of superficial Westerns entertained, but its revenge story is unfalteringly by-the-books.
 Because of its intriguing emphasis on the strained father/son relationship, 'Forsaken' ends up being a compelling drama as well as a wonderful addition to the Western genre. 
 Forsaken is still a film to be enjoyed. The performances themselves justify that.
 It should please those who like their westerns the old-fashioned way.
 In a standard cowering-town-needs-a-gunfighter drama, typical themes (redemption, forgiveness) are laid out with little imagination.
 "Exposed" is a mess and completely forgettable, but perhaps it's more interesting as a lost movie, with its true form caught between the demands of its financier and the vision of its original creator.
 In certain mutilated pictures, you can detect the lineaments of greatness: Consider Orson Welles's "The Magnificent Ambersons." Here, that's not the case.
 'Exposed' is confusing and very difficult to follow. It ends up being a downer mess -- despite interesting turns by Keanu Reeves and Ana de Armas. 
 Awfully silly - and just plain awful ...
 The confused, heel-dragging mystery drama Exposed suggests an especially dour, arty episode of Law & Order: SVU, minus any reasons to keep watching.
 Cliched thriller mixes cop story, supernatural elements.
 ... feels like two disparate ideas scrambled together in a way that's never convincing.
 Has a grimy, off-kilter charm not seen since the heyday of '70s exploitation.
 What a strange and frustrating mess this is.
 Exposed is a film suffering from blunt force trauma to the head. 
 With Reeves in energy-saving mode, the troubled cop and corruption routines simmer but never come to the boil.
 [Reeves] appears to only have two expressions: sad face and need-a-pee face.
 What is being released is a baffling hodge-podge of a movie, full of contradictory elements. Bits of it, seen in isolation, are effective and atmospheric, but the plotting is tangled and confused.
 It's clearly intended as a magical-realist take on very genuine social conflicts, but the result, in this edit at least, is a trial to sit through.
 To call it disjointed is an understatement: Exposed is unintelligible. It feels like two completely different movies inelegantly Frankensteined together.
 Incomprehensibly disjointed and stunningly dull ...
 A grim-faced Keanu Reeves looks bewildered throughout Exposed and you can only sympathise.
 Carries a complex, convoluted narrative that attempts to cover too much ground, balancing a myriad of themes.
 It's fascinating to watch -- not once was I bored -- and superior to most other offerings on the big screen at the moment.
 It has all the Tarantino's characteristics and that's the reason this is a must for the filmmaker fans. [Full Review in Spanish]
 There's politically incorrect humor and violence, as well as an attractive cast, but it's too long to be perfect. [Full Review in Spanish]
 'The Hateful Eight', is a confirmation of an exaggerated and fuzzy Tarantino, so determined to surprise us with his wit and shock us with his audacity and ends up sabotaging his interesting argument with excessive characters. [Full Review in Spanish
 Tarantino brings a very bold political film disguised as a western that seems deep but at the same time can be enjoyed without any problems. [Full review in Spanish]
 Even though I don't think this is Tarantino's best film, the director delivers and satisfies his fans. [Full Review in Spanish]
 Tarantino is a slicker and just cares about giving The Hateful Eight an air of quality. [Full review in Spanish]
 All these great actors, the majestic landscapes and picturesque and mysterious characters wasted... A Tarantino's mistake. [Full review in Spanish]
 The Hateful Eight doesn't try to make a point about race or the status quo in America today, it doesn't even try to do it. It's just interested in telling us a good, violent and emotional story. [Full review in Spanish]
 As he becomes even wiser an orchestrator of scenarios, characters, and conclusions, Tarantino's work turns more vicious in its time-bomb-like destruction. 
 While not a quantum leap forward in his directing, The Hateful Eight finds a new corner for the filmmaker to explore as both a writer and a director.
 Scenes do not gel into a whole, nor do characters compel; suspense is absent, and early parody and irony have given way to out-and-out torture porn. 
 The Hateful Eight is easily Tarantino's most fierce and contemporary film to date.
 The Hateful Eight sees Tarantino stake a claim as his own most passionate auteurist: the film is a melting chamber-pot of self-referentiality and re-creation.
 Its lenght and lack of that characteristical 'Tarantino shine' of his more famous body of work make a rewatch of this film a necessary to develop a taste for it. [Full Review in Spanish]
 It's good and bad, trashy and brilliant, flimsy and substantial all at the same time. And as for Tarantino? He's maddening, frustrating, totally unique.
 Bloated and hilariously self-indulgent.
 Tarantino is making his points and although it's too long, it is a spectacle to enjoy, a mix between The Canterbury Tales, Cluedo and spaghetti westerns.
 Just when you think you know what Quentin Tarantino will do next, you realize just how wrong you are as evidenced by this great addition to the director's oeuvre. 
 It's not exactly a culmination of Tarantino's work to date but it echoes themes and storytelling ideas we've seen him play with since Reservoir Dogs.
 The expose of brain damage risks to footballers is powerful stuff, but the film never flies, due to a messy screenplay that loses its focus and awkwardly interweaves an immigrant love story into the mix
 I am a sucker for cause films that champion right against might, but even allowing for that bias, I can recommend Concussion as engaging cinematic grit
 It's become all too easy for heavyweights like the NFL to forget the essence of sport: win or lose, it's just a game but one which can physically and mentally scar the real people who play them.
 Feels like an outline spun by an ambitious Hollywood executive rather than anything resembling real life.
 Tthe events depicted are fascinating but feel inconclusive, so it's easy to leave feeling a little unfulfilled at the close of play.
 Relying in no small part on Smith's reliably first-rate performance, Concussion is a thoughtful and interesting story neglectfully executed.
 The science shines brightest in this confronting true story that brings shocking new meaning to the phrase 'contact sport'.
 Concussion takes a serious subject - brain damage caused to professional American football players, and the collusion of the medical establishment in covering it up - and renders it in full-bore, crashing-bore, Oscar-courting prestige drama mode.
 Viewers, and not just retired footballers, can all too easily forget what the movie's even about.
 Smith still does a fine job of communicating the stubborn resilience of Omalu, an admirably unapologetic whistleblower so committed to the CTE cause he paid for his lifesaving research out of his own pocket.
 In terms of leading some discourse about the safety of America's most popular sport, at least 'Concussion' is in the stadium. But Concussion doesn't really enter the game. 
 Omalu's dedication to his profession and his struggle to make himself heard and understood make for an inspirational story, and Landesman effectively lays out the facts and invites the audience to judge both Omalu and his opponents.
 Smith's quietly tough performance gives Concussion its heart.
 The NFL may well feel gratified that the movie is as vague and weak as it is.
 Will Smith's performance is solid, but Concussion's plodding, procedural nature turns the drama into a bit of a do-gooder dirge.
 The film is worth for two reasons: its theme and its main actor. [Full Review in Spanish]
 To see Will Smith seriously acting is refreshing, he should try it more often. [Full Review in Spanish]
 The script also knows how to pull back when sentimentality threatens. The film's salutary ending shows just how it's done.
 Told in fits and starts, the film does convey a fair bit of information, but it never clarifies what needs to be done about the continuing danger.
 I'm not sure I would ever forbid a son of mine to play football. But I would definitely insist he see Concussion before strapping on those pads.
 A fascinating and entertaining attempt to answer baseball's Big Question -- Who was the fastest ever?
 [Stands] as further testament to baseball's status as our most chess-like sport, and one that, even when broken down to its tiniest component parts, never loses its magic.
 For baseball fans, it delivers the high heat. For the non-fan, there may be a little too much inside baseball.
 Fastball will change the way you watch the game, without ever diminishing the sport's mystery and grandeur.
 There's nothing here that wouldn't have fit comfortably into an hour-long TV special, and it starts to drag after a while.
 The irony of the push to speed up the game of baseball - the amount of time between pitches, between commercial breaks, between anything that might actually appeal to the iGeneration - is that speed has always been at the heart of the game.
 Filmmaker Jonathan Hock goes beyond anecdotes to the larger role of the fastball in baseball, the way the game changed when the pursuit of velocities exceeding a batter's reaction time became a Holy Grail.
 This appealing documentary makes you understand why aficionados regard baseball as a form of poetry.
 With Kevin Costner narrating, Hock illuminates and entertains as he sketches portraits of the greats, from original fireballer Walter Johnson to the indomitable Nolan Ryan.
 You don't have to be a baseball fanatic or for that matter a historian or a physicist to appreciate "Fastball" ...
 Hock's documentary has a thrilling pop that should help it strike a competitive chord with anyone even remotely enchanted by our national pastime.
 Suitably solemn yet irresistibly lively documentary about the myth, lore and logistics of baseball's classic pitch.
 The lively MLB-produced doc's choice archival material and full roster of new interviews will delight fans, both as a warm-up to the 2016 season and beyond.
 The sunniness of Fastball leaves out a lot, but watching it can be as pleasurable as an afternoon at the ballpark.
 A fascinating and downright lovable documentary feature by Jonathan Hock.
 If you are not familiar with this level of baseball nerdiness, then Fastball will be a revelation, and hopefully an entertaining one. If you are familiar, then Fastball will satisfy on a deep and extremely specific level.
 One of the worst films to play Midnight Madness in ages...
 Baskin is Turkey's answer to a Rob Zombie movie, which is meant as a compliment in this case.
 Take it or leave it, but I was very happy to have partaken. So, I suspect, will more than a few other pervy geeks.
 ...horror-hounds will relish the orgy of the damned that unfolds.
 surreal, uncompromising, bestial and eerily beautiful... even if it is not, despite what some unsuspecting viewers may believe, named for a certain popular brand of ice cream, Evrenol's film sure is one haunting Turkish delight.
 Baskin excels beyond its vile, visceral aesthetic into a puzzle of keyholes, doors, and fate.
 Visually, Baskin is like a twisted sibling of Fulci and Argento by way of Cronenberg and Winding Refn.
 Baskin is the classic example of a film that fails because it can't support its carnage. I'm all for extreme blood and guts, but it needs to have purpose.
 [Baskin] offers little in the way of narrative involvement or scares but doesn't stint on sustained, stylized revulsion.
 Its initial promise dissipates in a muddle of repetitious phantasmagoria and too little narrative or character development.
 Baffles and bedazzles before blistering into a masterpiece.
 The film mostly functions as a tour of familiar horror tropes for much of its running time.
 Evrenol shoots all of this artfully, imbuing everything from a police van broken down by the side of a road to a man having his intestines slowly pulled out with a certain eerie beauty.
 It's a testament to the power of Baskin's imagery and atmosphere (and the solid performances), then, that the shortcomings fall by the wayside for long stretches as we absorb this freaky world in which the protagonists find themselves.
 Tickling the mind even as it lurches the gut, "Baskin," a stylish, shape-shifting horror film from Turkey, pulls a bait-and-switch.
 "Baskin" is a perfectly imbalanced mix of chilly atmosphere, heavy-handed symbolism, and familiar horror-movie tropes.
 A torture-gore blowout that rises above pure nausea with an intriguing blur of possible realities.
 The pacing is slack and the splatter excessive, but this twisted cross-genre exercise should be red meat to gore-hounds.
 The filmmakers handle their material efficiently, but it's hard to imagine anyone familiar with the genre finding Bleed fresh or as vividly scary as its predecessors.
 Bleed is familiar, by-the-numbers horror filmmaking, but it's also short and sweet enough to capitalize on cheap thrills.
 Too derivative and not stylish enough to merit any special championing, as indie B-horror movies go, it's nonetheless nicely crafted enough to rate a cut above the low-expectation median.
 Reflecting influences ranging from The Texas Chainsaw Massacre to Rosemary's Baby to -- well, you name it -- Bleed doesn't exactly break any new ground, stylistically or otherwise.
 Bleed is really nothing you haven't seen before, but there's still some pretty interesting elements that result from its mixtape approach.
 A comedy that's neither clever enough nor sufficiently over the top.
 Despite its flagrant attempts to mimic what works in the comedies by Seth Rogen and Evan Goldberg, it's never particularly funny.
 The problem is that a movie like this - limp, lazy, generic - just doesn't cut it at a time when American indies and television offer sharp, witty, satisfyingly complex takes on the "Facebook generation."
 Shot like a typically ugly Adam Sandler effort and so clumsily stitched together that it feels as if large plot chunks litter the editing-room floor ...
 Not only is this ensemble unemployment comedy labored and witless, but it bears the unmistakable scars of a protracted and ultimately failed struggle in the editing room.
 It's hard work finding work these days, is it not? Not according to "Get a Job."
 At its sloppy heart, this is meant to be an affirming movie, but the filmmakers could have taken a cue from one line of dialogue: "Don't just feel special. Be special."
 Clearly, the economy has given Get a Job a reason to be sour. But there's no excuse for being so sexist. 
 The imperfect work mired in storage all this time gets a well-deserved spin.
 "Get a Job" is nothing special.
 Not only is this a movie without any guts, it doesn't have much of a brain either.
 Predictable workplace comedy has drugs, sex, swearing.
 After a viewing, it's obvious why the producers lost interest in releasing it. 
 Get A Job points to either massive studio compromise or a filmmaker who has somehow lost the mastery of his once-auspicious occupation
 Get A Job's primary problem is that it doesn't know if it wants to be a realistic look at millennials and the current economy, or go for the cheap gag about the jive-talking pimp renting out a sleazy motel.
 This long-shelved comedy proves a disappointing mix of onscreen talent, uneven social satire and juvenile humor.
 What makes Get A Job so infuriatingly bad, rather than the kind of film you hate and then completely forget about, is the all-star cast that it has at its disposal and disgracefully wastes.
 A brutally cynical, largely unfunny film fueled by muddled social commentary.
 Writer-directors Micah Wright and Jay Lender are kids'-cartoon vets and show a facility for comedy on a more human level here - as does the nimble cast, which ably handles the tonal shift from travel nightmare to actual nightmare.
 Is the film savaging cable TV? Ethically challenged filmmakers? Found-footage horror itself? If the intent was to finish off this subgenre by exposing the rote mechanics behind it, the mission is only halfway accomplished.
 Wait, you mean you won't be begging for They're Watching's cast to be sliced and diced? How ... refreshing.
 What we have here is a horror movie with all the expected trimmings: Rumors of witchcraft. An isolated house way out in the middle of spooky woods where no one can hear you ... well, you know how that goes.
 The storytelling becomes muddled in the middle, and the suspense doesn't build as well as it ought to, but the winking undercurrent keeps the film watchable.
 Viewers looking for the minimal amount of horror in their horror comedies will be appeased with They're Watching, but the focus here is on laughs, not chills - and that won't work for audiences seeking an equal balance between genres.
 Here comes one weird, cracked-out, trippy horror tale that kicks into high screams at the 62-minute mark.
 The movie doesn't do justice to a promising premise.
 Tired jokes and uninspired gore abound in what amounts to an unbearable experience, and that's before the climactic bloodbath has a chance to disappoint with its sub-amateur special effects.
 Here's a 94-minute found-footage horror movie that would have worked better as an 8-minute found-footage Funny or Die comedy sketch.
 An hour and fifteen minutes of tedium is too long to wait for two or three minutes of pleasantly cheesy.
 A contemporary Blair Witch Project knock-off that feels significantly more dated than the film it's inspired by.
 It makes some progress, but it unsurprisingly stalls where so many found footage films have failed before.
 Neither very plausible nor scary, this found-footage exercise is nonetheless entertaining enough for a spell ...
 So much better than last year's plodding, aimless Part 1.
 Part 2 is supposed to be the emotional crescendo to an epic love triangle, but due a complete lack of chemistry between Lawrence and her callow male co-stars, it's all hollow, and the gold-embossed Super Happy Ending feels fraudulent. D.B.
 Affirms Katniss Everdeen's status as the most significant of the reluctant teen heroes who have battled adult tyranny in the fantasy movie series of the past few decades (and no, I'm not forgetting Luke Skywalker or Harry Potter).
 action and theme don't always cohere, and what's good in the film too often has to be dug out from under a lot of underwhelming excess
 Well, the Hunger Games finally grew up at the end, after all that silliness about the earth-shattering importance of fashion statements. We finally got one convincing romance, to go with a real battle and real politics.
 This 137 minute conclusion results heavy and drags considerably due to its lack of what it promised: Action, emotion, unrestrained rage. [Full Review in Spanish]
 It reminds us that every "happy" ending can also be a tragedy. [Full review in Spanish]
 While there's lots to recommend the movie, the silly premise is the only thing I could focus on for the entire 2 hours and 17 minutes -- which is inexcusably long for a movie that is actually just half of a movie.
 Relentlessly bleak from the first frame, this final outing pulls no punches as white knuckle action sequences see the casualty rate rise alongside the tension.
 the last hour of Hunger Games 3.2 is good enough that deeper readings of this text would actually hold water in a way that previous attempts to politicize it have failed.
 Part 2 feels like it has a sense of direction that was lost after the first movie, and it's enjoyably more action-heavy. 
 It's frustrating to watch the Jennifer Lawrence be marched from plot point to plot point, just getting things over with. 
 I understand that YA books succeed by transposing real teen concerns into life-and-death fantasy situations, but any connection to the characters that existed by the end of 2013's Catching Fire has been stripped away by these plodding Mockingjay movies.
 I remember struggling with Suzanne Collins' final book. It didn't have the same sense of urgency and excitement. My thoughts on this movie are essentially the same.
 In this final film, fans will get a satisfying resolution. It's an emotional and dark journey illuminated by an otherworldly performance from Jennifer Lawrence.
 A satisfying conclusion to a brilliant series of films that show us the dangers of the oligarchy.
 Kudos to Lawrence for helping bring a satisfactory conclusion
 [The] rare blockbuster that finds a compelling middle ground between thoughtfulness and big, splashy spectacle. 
 A fittingly serviceable end to a series that always had more potential than impact.
 ... provides a satisfying end to the story rather than an inspiring one. It preserves the dark turn of the book's finale, the character of Katniss, and the journey of Peeta to recover the compassion destroyed by Snow.
 First with the telephone, then early cinema, the magic of wireless radio and, finally, television, Dreams Rewired bombards the senses with a thorough and clever montage of found footage from the 1890s to the pre-war era.
 The documentary isn't advancing an argument so much as simply restating a European socialistic breed of fact. 
 An extended look, sans nostalgia, at how we used to envision ourselves and our future - and at how those of us alive now, at what seems the apex of communication technology, will look to everyone watching us in the future.
 Compilation of silent and sound films attempts to address fears about technology, but proves excruciating instead.
 A lively, visually enthralling attempt to gaze into the future by remembering the past.
 The ethereal essay provides a bounty of poetry, in the form of a measured narration by international treasure Tilda Swinton, and an extensively labored assembly of 200 black-and-white film clips.
 The film really serves as a tribute to the then newfangled phenomenon of moving pictures ... Its most persuasive (and unspoken) revelation is that filmmaking evolved concurrently in the Old World as it did in the United States.
 It's a feast for fans of early film, but alas, the narration is inescapable.
 As a documentary, "Dreams Rewired" spends a long time circling various points without ever landing on one.
 The entertaining Tilda Swinton narrates, from a high-falutin' academic script, but it's the images that provide the fascination.
 Science fiction becomes reality in this funny and disturbing collection of filmed techno-history
 Dreams Rewired isn't in the business of recovery or even analysis. Instead, it gestures, it implies, it signifies.
 "Dreams Rewired" is scattered by necessity and intent, and it throws off enough sparks to set your brain reeling.
 The effect is more somnambulistic than stimulating, and eventually you're less concerned about Big Brother peering into your home than you are about getting Tilda Swinton out of your head.
 Solid messages and mild scares are fun; more toys for sale.
 a series of predictable and pedestrian scenarios, including a morbidly obvious comparison between the two gentlemen's organs of generation that will color your thinking about Patrick Stewart forever. 
 While the pair of them look like they might just perhaps be able to make this highly improbable and dubiously conservative nonsense vaguely amusing, they don't.
 The movie won't earn Ferrell any new fans, but it's a safe bet if you like his type of humor. [Full review in Spanish]
 Watch if you enjoy watching talented people waste their time, and also enjoy wasting yours.
 That's one problematic comedy.
 An odd mix of sentimental family warmth and gross-out antics, this comedy doesn't have the courage of its own convictions, which means that it's not quite funny enough to keep the audience fully entertained. 
 The second pairing of Will Ferrell and Mark Wahlberg isn't as funny as their first offering, The Other Guys, but it's still funny enough to warrant a look.
 It has its share of laughs, even though they may not be memorable ones.
 What keeps Daddy's Home watchable is Wahlberg's checkmate machismo, as the intimidating foil necessary for Ferrell's namby-pambyism to register. It's like watching Andy Samberg's SNL impersonation of tough guy Mark Wahlberg, a self-parody of a spoof.
 Slapstick comedy that really doesn't do anything new for the genre. [Full review in Spanish]
 Predictably stupid and hopelessly obnoxious...
 There is no doubt that Will Ferrell and Mark Wahlberg connect on screen. See them interact together has its charm but not enough to make us burst into laughter. [Full review in Spanish]
 The usual formula is used to make this a generic but fun comedy. [Full review in Spanish]
 Daddy's Home insists on staying in common ground and to make matters worse, his argument is unable to build paths that culminate in moments of laughter. [Full review in Spanish]
 Sadly, not even the great performances can't save its awful script. [Full review in Spanish]
 What's the target audience? It's really innapropriate for a family film. [Full review in Spanish]
 Daddy's Home manages to succeed thanks to Will Ferrell, experienced comedian that once again stays in his comfort zone to do what he does best: amuse others with his characteristic jokes, comments and gestures. [Full Review in Spanish]
 Somewhere hidden in this film there is a good comedy. [Full Review in Spanish]
 This movie feel extremely long and repetitive despite its hour-and-a-half lenght, something deeply worrying given that comedies are supposed to make you laugh, which Daddy's home fails at. [Full Review in Spanish]
 It's symptomatic of the production's limpness that the movie was shot, for tax-credit reasons, in New Orleans, yet the locations have been scrubbed to a funk-free suburban anonymity.
 "James White" gets up close and personal in often discomfiting ways, but it's never exploitative or glib. It hits the highs, and the rock bottoms, and all the damnable stuff in between.
 Filmmaker Josh Mond is as interested in his hero's valleys as his...well, not peaks, but the moments when he's less agitated, even peaceful.
 Abbott contributes a smart, soulful performance, but Nixon keeps threatening to walk away with the movie as the mother, who can't get enough of life and whose physical decay is colored by rage, defiance, and terror.
 Both Nixon and Mond invest James White... with a raw honesty that makes James White a compelling drama about the demands and rewards of family.
 An accomplished and compelling film by writer/director Josh Mond, James White is also pretty much a bummer.
 Wisely, James White leaves the unresolved troubles and feelings of its eponymous character unresolved.
 The acting is good in this film, but watching it is like a depressing, exhausting journey to nowhere.
 It's Abbott who's the revelation, showing off all sorts of previously unseen leading-man potential.
 Veteran producer Josh Mond makes his feature-directing debut here. It elides as much as it shows, but his sketches are mostly deft and always deeply physical.
 The experience of watching "James White" is like being shut up in a small, dark, airless room ... a sickroom.
 Abbott's performance is serious and committed; James White is constantly fuming in an interior dispute with the rest of the world.
 Most films dealing with illness and carer relationships follow the same emotional paths and miss the same points. This film, by contrast, feels fresh and real even to those of us who have been there.
 Whether you want to spend time with "James White" depends on your tolerance for yet another film about how hard it is for guys who just feel too much.
 "James White" can be a chore to sit through, but it's never completely without merit.
 The movie... is so engaging there are times when it becomes almost overwhelming. But that is a glowing sign of how well the movie is acted and put together.
 Death does not become him
 A low-key but devastating drama with more raw authenticity than a hundred examples of Sundance landfill.
 This courageous film gets right up in the face of suffering and it doesn't flinch.
 It's not easy viewing by any means. But it is strangely refreshing for a movie to show us that terminal illness involves agony and vomit and terror, despite what Beaches might have told us.
 One will never wish to go through precisely what White does, but losing one's parents is inevitable, and the film is a searingly authentic portrait of the process.
 Writer-director William Riead offers a highly simplified version of his subject's life.
 If Mother Teresa were here to see the film, she would probably say, 'You made this piece of garbage about me?'
 This Mother Teresa biopic offers Hallmark Channel-grade inspiration of the most sluggish sort.
 A biopic about Mother Teresa could have easily been a self-important slog, yet William Riead's "The Letters" proves a stirring and absorbing if not quite definitive drama.
 Struggles aren't ignored here; they're just surmounted with patience and devotion. That may be a good strategy in life, but it can be static to watch on screen.
 Her accomplishments are even more impressive once we learn how fiercely she wrestled with God.
 Even Mother Teresa's harshest detractors might say she deserves a better biopic than The Letters
 A drama in which belief is reduced to well-meaning but inert treacle.
 Bound to disappoint, especially considering the absolutely wasted and bungled potential. Quite possibly the most boring and dramatically inert movie you'll see all year. (Full Content Review for Parents -- Violence, etc. -- also available)
 Underwhelming biopic about the extraordinary Mother Teresa.
 Her work made her one of the most celebrated figures of the 20th century, but The Letters is far too preoccupied with the bureaucratic minutiae of her journey.
 Good intentions alone can't salvage this aggressively heavy-handed Mother Teresa biopic that perhaps could use some divine filmmaking intervention.
 This is a revealing study of a woman who was one thing to the world but something completely different to herself, with a fine performance by Juliet Stevenson
 Teresa is simply portrayed as a dedicated servant of God, while whatever internal struggle she dealt with remains told, not shown.
 Ms. Stevenson is effective and credible in the role. And "The Letters" is worth viewing for people of all faiths.
 "The Letters" is a beautiful and deeply moving tribute to the self-sacrificing Mother Teresa. 
 A warts-and-all biopic which reveals Mother Teresa as a tortured soul who felt abandoned by the same God she served so selflessly.
 The movie's writing, direction and acting are not alone in blowing. Ciaran Hope's overwrought score sounds like a mashup of music from hokey old biblical pictures. If a duller, less inspired film hits the cineplex this season, it will be a miracle.
 It's refreshing to encounter the occasional movie character who is blessedly devoid of guile. For that matter, it's also a relief to attend a film where we're not expected to hiss and throw our popcorn at the screen the moment a Catholic priest appears.
 A tale of two Mother Teresa's: On the brink of Vatican sainthood just announced, holy healer or 'hell's angel'? Expect an essentially nuance-free infomercial embrace of the former, despite ironic inklings of nun feminism challenging male church authority.
 Not without its heated confrontations, but it feels unnecessary, working to depict the downfall of a man who's beaten them to the punch in terms of addressing his own self-destructive tendencies. 
 While perhaps not as deep as it could have been, the movie is nonetheless a compelling examination of an unrepentant cheater.
 Despite a committed lead performance from Ben Foster, Frears' drama is an obvious and frustrating depiction of ambition and obsession.
 The Program feels ultimately more akin to box-ticking than character-powered drama.
 I had a hard time buying so much concentrated bile, and for me, the film loses an objectivity that might have otherwise scored points for the destructive nature of competition that can wreck a decent man's corrupted psyche.
 There is nothing remotely likable, or even relatable, about Lance Armstrong in The Program.
 We're left with the well-acted and well-told, if familiar, story of a man who knew better but couldn't stop himself.
 The Program is a highly watchable, very enjoyable film, but never truly manages to get to the heart of the issue.
 Frears squeezes tension out of these interstitial moments, letting personalities rather than facts collide.
 Foster nails Armstrong, right down to his final, clenched-jaw TV confession to Oprah Winfrey but The Program's glancing narrative feels less secure.
 "The Program," much to its detriment, concentrates almost exclusively on the history of the doping effort . There is no mention of his childhood or adolescence or any attempt to analyze his character.
 The most surprising thing about 'The Program'...is how narratively pedestrian it is...feels like territory that's already been well-trod (or ridden).
 It never goes deep on what it was that produced the awfulness that is Lance Armstrong.
 "The Program" fails to add anything new, or penetrate the soul of so crafty a cheater.
 For a story about incredibly focused determination - and if nothing else, Lance Armstrong had that - the film remains strangely uninvolved.
 The Program reveals the tiniest details about Lance Armstrong's doping regimen. Just don't expect to learn as much about the cyclist himself.
 The flashily photographed enterprise too often becomes a blur of sound bites and slick aesthetics akin to a Nike commercial.
 The Program is a solid primer to this fascinating rise-and-fall saga -- one offering far-reaching implications about corruption in sports and celebrity culture.
 Much like Frears' other films, The Program shines best in its performances. Ben Foster disappears into his role as Armstrong...
 Flat Lance Armstrong biopic has drugs, strong language.
 Toby and co. come across as a pale, anti-intellectual imitation of the college-aged friends in Donna Tartt's The Secret History - a novel that explores the roots of its characters' moral recklessness rather than just chalking it up to teenage feelings.
 Toby is so un-self-aware that his journey seems like mere obtuseness; what the film has to say about youthful degeneracy is less than zero.
 Nearly everything about "The Preppie Connection" - from the high-school class war to the flat, explanatory narration - has been cribbed from other, better films.
 This film fails even to evoke the '80s in costumes, soundtrack or other atmospherics.
 [A] shallow and profoundly unexciting "true-life account" of one student's brief reign as a campus drug lord.
 As written, Toby is somewhat of an empty slate, and Mann doesn't do much to fill in the blanks.
 A dramatized real-life scandal of 1980s prep-school drug dealing plays like a tepid compilation of fictive cliches in The Preppie Connection.
 By the time quotes from Joseph Goebbels are used to explain Fox's methods, The Brainwashing of My Dad has resorted to the tactics of its targets ...
 A timely and thought-provoking portrait of the soulscape of America with its rage, lack of civility, and right-wing media machinations.
 Points fingers in all the right directions but fails to dig deep enough.
 Although Senko ultimately overplays Hillary Clinton's "vast right-wing conspiracy" card, her film, with its timely "Trumpian" reverberations, nevertheless serves up some compelling food for thought.
 Right-wing outlets may be exploiting humanity's ugly side, but Ms. Senko's frail-looking father (who died in January at 93) isn't so much the face of the phenomenon as he is a small and not especially representative sample of it.
 Jen Senko's workmanlike (Kickstarter-funded) documentary examines American media's propaganda-led phenomenon of extreme rightwing bigotry with a fine-tooth comb.
 Through anecdotal and social science research, Senko's film also provides much-needed insight as to why Donald Trump's caustic discourse and demagoguery is catnip for so many people.
 An entertaining look at why Fox News is setting the agenda for what passes for journalism in the U.S, and a tool, perhaps, for deprogramming its adherents.
 A thought-provoking examination of the media's effect on Boobus Americana. 
 Impressively tracks a family phenomenon via media mind controlled ditto-heads, brainwashed staged interventions, angry white men, and drive time as the geography of destiny. But lacks further scrutiny into shadow corporate complicity and a tanked economy.
 Maybe I could say that if Christopher Guest rounded up his usual troupe and did a verbatim, shot-for-shot remake, it would be one of the funniest films of the year.
 In addition to an overreliance on Skype and silly effects such as laugh tracks - the film's examination feels shallow.
 Paula Pell's script is like a distended Saturday Night Live skit, overworking one lame joke: what if a bunch of forty-somethings threw the kind of party you only see in movies about hormonal teenagers?
 Fresh, funny, and heartfelt, Sisters makes up for what it lacks in plot with a rolling succession of tear-inducing jokes and a gaggle of hard-partying characters that you would actually want to buy a drink for.
 There's a darker undertone to the story that is touched upon but not given the gravitas it requires. 
 More wit would be welcome, but Fey and Poehler keep things humming
 Sisters is an uneven mix of raunchy farce and sincerity that's elevated by the undeniable chemistry between Tina Fey and Amy Poehler.
 Sisters is the Fey-Poehler feature-length team-up many have been waiting for.
 Though Sisters does briefly achieve the right levels of relentlessly reckless humour at the height of the aforementioned party, there are protracted spells either side of the main event where even a mild chuckle is hard to find.
 It's a bit like Animal House for girls, but not as freewheeling.
 Is this the collabo between our brightest, savviest comedy players that we've been dreaming of? Not quite. It's easy to want more from Sisters, if only because Fey and Poehler have already given us so much.
 Sisters is a knucklehead comedy - sporadically amusing and always happy to resort to dick jokes - saved a little by the appeal of its leads.
 In what can be taken as an extended and belated apology for their woeful 2008 comedy Baby Mama, funny femmes Tina Fey (30 Rock) and Amy Poehler (Parks and Recreation) team up as sisters for another girls-behaving-badly slapstick comedy. 
 Tina Fey and Amy Poehler are an unbeatable comedy team. That doesn't make them infallible.
 There's no shortage of comedic talent on screen, yet Sisters plays a bit like a raunchy stand up routine, loosely stitched together by a narrative that begs for more than crass, out-to-shock verbal torrent
 Okay, there are some solid one-liners (including two - count 'em! - tampon jokes), and Maya Rudolph is reliably excellent as an uppity spoilsport, Brinda. But the best gags are few and far between, and the movie starts to lag like a lengthy sitcom.
 While it won't be immortalised in the enduring annals of cinema, it did trigger some unexpectedly visceral reactions from me.
 One of the most disappointing films I've seen in a while.
 Agent One: "Hey, we just signed Tina Fey and Amy Poehler to a movie about sisters who visit their childhood home, only to find that their parents have sold it out from under them." Agent Two: "Sounds great. Who's doing the script?" Agent One: "Script?"
 You feel like you're at that party that refuses to die, and all the really interesting people are on the other side of the room doing funny things you strain to overhear.
 Even when Paula Pell's screenplay falters, the likeability of the central pairing is more than enough to carry the picture.
 This movie would have been better with a heavier focus on just these two rather than bringing in their SNL entourage
 Despite the at times amateurish acting, the film is, in the end, surprisingly touching and a reminder of the fragility of love.
 No�'s film may not lack squelchy spectacle, but when it comes to anything deeper it is oddly anticlimactic.
 After a slow start No�'s self-referential soap proves more-ish.
 Everything is muted in Love. When not shooting raunchy sex scenes, No�'s preferred framing is the medium-close-up. He keeps his camera near his characters but seems unable to elicit the true emotional essence of sexuality he claims to court.
 It's bold, fleshy and audacious, at least in theory. But it is also numb.
 Despite being interesting, it's inconsistent and is both a demonstration of the courage of its director, as well as its shortcomings. [Full Review in Spanish]
 A great movie, but deffinately not for those who get offended easily. [Full review in Spanish]
 For a woman that know nothing about what's going on inside a guy's head, this can be the perfect guide to understanding the opposite sex. [Full review in Spanish]
 The blunt eroticism is going backwards, always structurally and narratively backwards, as in all films of No�. [Full reviw in Spanish]
 Gaspar No� took the fun in sex. [Full review in Spanish]
 Is interesting that Gaspar No� still in the search to make audiences feel uncomfortable, but curiously Love is may be one of his least transgressive works. [Full Review in Spanish]
 In theaters it has found rather indifferent response. [Full review in Spanish] 
 You can watch Love in 3D, but it won't add any dimensions to the characters or this script.
 An interesting way to talk about the different aspects of being in a relationship, including sexuality. [Full review in Spanish]
 One of those films you have to see to believe but only for those not easily offended. [Full review in Spanish]
 Exciting, sexy, moving and painful... [Full review in Spanish]
 The problem here isn't really with the porn, but with everything else, which features the weakest dramaturgy this side of Deep Throat. 
 The production values may be appreciably higher than your average porno but in terms of the quality of its plot, dialogue and performances, Love is not that far off.
 Karl Glusman has a future in Hollywood.
 Although masterfully acted by its three main actors, the only new offering in this film responds to the natural need of Noe to cause a shock in the audience. [Full Review in Spanish]
 An informative and infectiously danceable history Latin pop history lesson. 
 A joyous mix of social history and musicology.
 This is a documentary not just for the latin community, or the Bronx community, but for lovers of music, artists of all kinds, and folks who gain some sense of release from music they love.
 Slick and mildly provocative, but overlong with excessive expository information. Steve Jobs makes for a very captivating subject.
 The more interesting question [Gibney poses, and poses well, is why we need to create a hero of someone who no doubt changed the world, but who was more iconoclast that saint.
 If you're expecting solid answers, you'll be severely disappointed. Ditto, if you come expecting a hagiography. In fact, I'm betting worshipers at the church of Jobs will be livid.
 For those who don't yet know of Jobs' dark side, Gibney's documentary will be a useful eye-opener, but those looking to understand what made Jobs great in almost equal proportion to his nastiness will remain in the dark.
 Steve Jobs: The Man In The Machine is a conversation starter loaded with controversial data about the lesser-known life of genius/tyrant Steve Jobs, but it asks a simple question of "why" that it can't even answer itself.
 Alex Gibney's twisty, engrossing documentary Steve Jobs: The Man in the Machine approaches its subject from an oblique but highly productive angle.
 While too long and with little new light on Jobs' failings as a human being, the questions it raises about his cult status and the way he helped change the world make it worthwhile.
 An attempt to understand the cult around a figure that's virtuous and revolting at the same time. [Full review in Spanish]
 Despite the movie's journalistic substance, the pleasure-free banality of its style gets in the way of a view of Jobs himself, whose work is as much aesthetic as it is industrial.
 Coolly performs vivisection on a man, a company, an industry, and a way of life.
 It simply isn't that easy to write off the Jobs' extraordinary drive to succeed at all costs. The difference between Gibney's documentary and other posthumous profiles of Jobs is that it doesn't necessarily maintain a reverential tone toward its subject.
 A thought-provoking and ethically-charged documentary about the Apple computer entrepreneur.
 If GIbney's questions are not precisely yours, his premise - that documentaries must ask questions rather than presume answers - is bracing, reimagining not only what documentaries can do, but also what belief can do.
 The picture is made with the consummate skill of the predominant documentarian of our age, getting as close as conceivably possible to the essence of Jobs.
 This isn't a love letter or a takedown; it's a procedural, just-the-facts biography of a man who made a big impact on the world.
 Thought-provoking documentary on the iconic legendary Apple co-founder.
 Gibney doesn't offer a big, final Sorkin-style statement. Instead, he grapples with his own ambivalence about Jobs and his enormous social and technological legacy.
 Whether or not you care about Apple products or Steve Jobs, Gibney's documentary offers a story well worth watching.
 A chilling portrait of an icon who remains revered for spearheading so many technological innovations despite his general contempt for humanity and his utter lack of people skills. 
 Gibney is never able to join, or understand, the choir of millions singing the praises of Steve Jobs. Perhaps because of this, the documentary he has created seems a lot closer to the truth than anything else I've seen about Jobs.
 Right from the start, this involving documentary asks much of its audience and poses questions that are unnerving yet engrossing.
 "What Our Fathers Did: A Nazi Legacy" wields a power that towers above many other small movies. It may not be the large definition of cinematic, but it is still a true film.
 ...the sensation of seeing men responsible for so much death relaxing with their families is unsettling, while at the same time highlighting some small vestige of the humanity of both fathers.
 A troubling study of denial, wartime responsibility and the challenge of dealing with a monster in the family.
 The men meet, visit historical sites with Evans, begin to battle: "I like you, but I don't like your brains and the thoughts in your brains." The contrast is bracing. 
 Most troublingly, images of Nazis in modern Ukraine in What Our Fathers Did: A Nazi Legacy suggests that not being able to acknowledge that history paves the way for it to happen again.
 What starts out as a genial documentary about two sons of high officials of the Nazi Party soon turns chilling in the gripping and compelling "What Our Fathers Did: A Nazi Legacy."
 The film is not just about a very specific and difficult conversation. Ultimately, it is also about the failure of conversation itself.
 How do you cope with knowing your father was an architect of evil? In this anguish-inducing documentary, two men struggle with that question in very different ways.
 What Our Fathers Did is a movie about historical and filial responsibility, about repudiation, about acceptance, about the pain we inherit, and the pain that continues to be doled out.
 My Nazi Legacy becomes horribly gripping.
 [W�chter is] a frightening example of how the denial common to fascist regimes can endure long afterward.
 It's a film that attempts the impossible: to make us understand what it's like to confront the fact that your father was responsible for the deaths of thousands upon thousands of innocent Jews during the Holocaust.
 A hard, unsparing watch, but rewarding in the way it pushes each man to find common ground in the darkest corner of history.
 Essential documentary viewing.
 A horribly gripping film.
 It entirely upends what I confess were my own preconceptions about what such a film would be: that is, a placid, consensual study, ruefully brooding on the sins of the fathers. This is far more challenging - and more disturbing.
 What is it like to grow up as the son of a senior Nazi, with atrocities on your family conscience? In this powerful documentary, the British lawyer Philippe Sands meets two men living in the shadow of the Third Reich in very different ways.
 A valuable examination of personal responsibility v familial loyalty set against events that should never be taken for granted.
 A fascinating doc about a couple sons of Nazi war criminals, one ashamed of his father's legacy, the other stubbornly proud. 
 Too much of Noma is composed of gorgeous pillow shots, which grow static and fussy, appearing to exist almost apart from the subject matter. 
 Deschamps never ventures below the surface of Redzepi's wildly successful experiment, and while the pictures are pretty, no one judges food on appearance alone.
 Slick but always sharp in the best contemporary European nonfiction fashion.
 I've never seen a restaurant documentary that seemed less interested in showing the joy of food.
 Most scenes feel stagey, with Redzepi feigning intimacy but sounding like he's auditioning to be the next Gordon Ramsay. 
 The story of Ren� Redzepi and his award-winning restaurant, Noma, succeeds in spite of any directorial missteps.
 Less a documentary than a glittering souvenir, but it's still a record of a legend.
 "Noma: My Perfect Storm" is crafted with exquisite care in the vein of its subject, though it occasionally feels overly precious.
 Despite some mouth-watering shots of the presumably delicious menu items, the approach combines a strange mix of ingredients that isn't for all tastes.
 The film forgoes narration for a naturalistic style, putting viewers in the place they're most curious about -- the kitchen.
 The end result is a revealing portrait of an artist wholly dedicated to his art.
 Food is not an inherently cinematic subject, being fundamentally about the sense of taste rather than that of sight. But, in its own terms, this doco isn't too bad at all.
 Unfortunately, you learn more about his struggles, and his food, in multiple episodes of Anthony Bourdain's TV series No Reservations and The Mind of a Chef than you do in the 100 mostly flaccid minutes of Noma: My Perfect Storm. 
 There's no reason this should run almost two hours.
 Ignore, for a moment, the generic title and the laborious first 20 minutes, and you can appreciate this mostly heartfelt, mostly sincere comedy-drama about trying to outrun your small-town past.
 Enjoyable, deceptively simple story.
 Brooklyn lacks any real conflict. The ingredients are what make it atractive, however, outstanding manufacturing and notable acting work by the it's leads are not enough to save it from Crowley's bland directorial work. [Full review in Spanish]
 Saoirse Ronan creates an unforgettable character. [Full review in Spanish]
 A beautiful story about love and self-discovery, a great production with great direction. [Full review in Spanish]
 It tells one woman's tale beautifully. The rest of her new country's story, however, is left unfinished... and unseen.
 Brooklyn is an excellent film for many reasons. While it may take its title from one small part of the world, it works so well because of the universal nature of its themes.
 Part of the film's charm is the way that Irish director John Crowley manages the mood, without compromising the momentum. His direction is impeccable.
 [Brooklyn] doesn't just look back wistfully at the past, it also transcends the period setting with powerfully timeless questions: Where do I belong? What can I make of my life?
 A subtle drama that work because of Saoirse Ronan's acting work. [Full review in Spanish]
 Sincere, emotional and even touching, but it's far from being a great film. [Full review in Spanish]
 While she's surrounded by a name cast, this is Ronan's film, and while she's made big impressions before (from Atonement to The Grand Budapest Hotel), this is her most assured and moving characterisation yet.
 The immigrant story is one that has been told countless times over, but director John Crowley's moving and funny Brooklyn brings a degree of empathy to this particular tale that is rare. 
 A soft movie when it comes to immigration themes that allows her leading lady to shine. [Full review in Spanish]
 An impeccable, romantic, and realistic film. [Full review in Spanish]
 The film is certainly lovely and well-acted, but the nerve it has obviously hit is not immediately obvious. Perhaps it's the film's very modesty - its lack of pretense, grandeur and histrionics - that accounts for its appeal.
 Everything seems to be in place, without being memorable even once. [Full Review in Spanish]
 "Brooklyn" is a well acted and visually competent drama ... but also an "old school" movie. [Full Review in Spanish]
 A film as understated but radiant and layered as its protagonist, Brooklyn succeeds with a simple storyline whose emotional impact aptly hits home. 
 Brooklyn is a superfluous romantic film that's entertaining but nothing more; its dramatic parts aren't captured with enough intensity to break down the audience. [Full Review in Spanish]
 This is Saoirse Ronan's film. She's given a huge amount to do, and asked to transform before our eyes from a callow, floundering girl into an assured and confident woman. She does so brilliantly, and talk will turn to Oscars.
 It doesn't sound exciting but it is really well done, with a great cast, understated but powerful, it's genuinely affecting, atmospheric, often funny, and very accessible.
 I don't know that McKay should go on being Hollywood's fiduciary moralist, but he's clearly on the side of the angels and of the entertainers.
 [Adam McKay] gets that a lot of celebrities overact to simplify a embroiled story. [Full Review in Spanish]
 Smart and snappy, this comedy is one of the scariest films of the year, using humour to outline the 2008 economic collapse from the inside.
 Wonderfully played (although our heroes are far from noble) and directed with great energy, this is dreadfully enjoyable film.
 There's no doubt that it's the actors' performances that carry the film to the award-nominated heights its been reaching as of late. But despite all the praise for Bale, who does indeed do a stellar job, it's Carell who shines brightest. 
 A pseudo-documentary dressed up like a human interest ensemble when it should have felt more like a supervillain heist movie.
 At the end of the film, I was hardly any the wiser as to the reasons eight million jobs and six million homes were lost in the US, even though the skill of the actors and the film's ambition remain impressive.
 McKay's adaptation of Michael Lewis' book presents the subprime loan crisis as a screwball comedy... But perhaps the only same response to the loathsome skulduggery behind the 2008 financial crash is to laugh at it?
 Adam McKay is passionate about the subject and The Big Short is exciting, passionate filmmaking.
 An immensely entertaining and worthwhile document of America's modern horror story.
 At first, the to-camera segments can be discombobulating, but over two hours, the film coalesces into a brilliantly accessible, scathing account of the financial crisis, and its continuing aftermath.
 It'll make you angry, but in a good way.
 It exposes the vast degree of lies and manipulation of the financial world in a far more entertaining way than any drama would.
 The film tries to take the socialist political stance of exposing the greed of the big American banks, yet it has such a difficult time taking a measured stance towards its leading characters.
 Even if it is funny, as it should be in parts, it also has the capacity for drama and seriousness when dealing with the consequences of the crisis, which makes this a smart piece of work. [Full Review in Spanish]
 In some ways, it's a flashier version of another Lewis adaptation Moneyball, which made another dull topic - baseball statistics - surprisingly interesting. 
 The very best that a satire can hope to achieve.
 The Big Short is the film that we needed at this time.
 It's a disaster movie - where the impending and continuous boom-bust cycle of capitalism is the oncoming disaster.
 Hanks, as a stranger in a strange land, gives us equal portions of laughs and insights into the worlds of both adults and adolescents. Big also offers up a very funny satire of corporate ladder climbing.
 This elegant and perhaps very restrained presentation does just that, presenting the case. Discussing the rest is up to us. [Full review in Spanish]
 'Carol' is a meticulous low-key reward for audiences.
 Focusing on the details that go from set design to excellent performances, Carol is an exquisite film. [Full review in Spanish]
 Brilliant adaptation, revised and refined, from the romantic novel by Patricia Highsmith. One of the best films of the year. [Full review in Spanish]
 'Carol' is a declaration of love to another film era but from a thoroughly modern perspective. [Full review in Spanish]
 Every piece fits perfectly to tell a story about love and passion, but ultimately, it feels like you've already seen this film. [Full review in Spanish]
 'Carol' is the most romantic movie of the year, and through Rooney Mara's performance-and Haynes' careful direction-it might also be the most cathartic.
 It's a delight to scrutinize every inch of Lachman's deep focus compositions and try and take in the abundance of exquisite detail.
 A love story told with looks and subtleties, hand with hand with an extraordinary cinematography work. [Full review in Spanish]
 An elegant, profound, meassured and extraordinary crafted film. Blanchett and Mara are hypnotic. [Full review in Spanish]
 In spite of being Hannes' less ambitious cinematic film, Carol offers the experience of putting you in the shoes of it's characters and feel what they feel. [Full review in Spanish]
 Carol is a n excellent film, full of warmth and romance.
 This is pure cinema, which through its artistry opens a window into the souls of its characters and admittedly opens the closed windows of its viewers as well.
 What Carol captures more specifically than the thrill of a romantic encounter is the act of remembering only to forget.
 Carol is a movie worth watching to understand its main theme without fear of censorship, and most of all, to realize and understand that love is love regardless of genre. [Full Review in Spanish]
 Carol is a truly striking cinematic achievement, artfully exploring themes that resonate still with powerful force today.
 The rhythm is slow but the film is never boring, this is a story worthy of seeing in the big screen. [Full review in Spanish]
 As he did with his gloriously realized 2002 domestic drama Far From Heaven, director Todd Haynes once more brings a story of how, in the pursuit of dreams, even happy endings can come with collateral damage.
 The film still makes the viewer swoon, its heady mood of love and longing generated by the briefest of glances and gestures. 
 A perfectly acted, perfectly sculpted, perfectly rounded exploration of love and strife in the not-so-perfect '50s.
 Sometimes, I can almost hear the studio executives talking before a film gets the green light. 
 Delivers precisely what is expected to its fans...The super-high quality of the animation, which places Alvin, Theodore and Simon in the real world, remains a real highlight in these innocuous adventures - it really is an unrecognised art. 
 It's harmless enough, but harmless garbage is still garbage.
 The Road Chip hardly qualifies as great cinema. And really, no adult goes to these films because they want to. But it is a perfectly serviceable accompaniment to a choc top and bag of popcorn.
 Tediously silly, slapstick re-visit with the rollicking rodents...
 A movie that kids would love and adults can watch without getting bored. [Full review in Spanish]
 Even though the film is marketed as a road movie for kids, it barely has any road in it and that's just the least of its flaws. [Full review in Spanish]
 A good movie for kids that adults can also enjoy. [Full review in Spanish]
 Fun time at the movies with lots of color and music, totally forgettable but enjoyable. [Full review in Spanish[
 A sugar-sweet, nutty kind of chip-muck, but cinematic excrement all the same.
 You might want to stick sharpened knitting needles in your ears.
 Instantly forgettable but inoffensive fluff... you know, for kids. And 'inoffensive' is better than can be said for many movies aimed at children.
 The franchise squeaks past with a so-so sequel that barely improves on what came before. Our only hope is that at some point they'll have to hibernate.
 Alvin and the Chipmunks: The Road Chip is the fourth, agonising instalment in the franchise that charts the adventures of three enormously-punchable helium-voiced rodents.
 If you're looking for something to amuse the kids on a cold, windy afternoon then Road Chip is bearable enough -- just. But a fifth film would really be a stretch.
 One amusing airport security sketch aside, this series must surely have had its chips.
 Ultimately, fans of the franchise won't be disappointed while the rest of us will be quietly hoping that Homeland Security step in a finish the job.
 The absence of long-standing series stalwart David Cross is a shame (Tony Hale can't quite fill the gap) but Jason Lee is back, making it business as usual.
 The final product quite amiable nonsense.
 Normally dependable comic actors are wasted in roles.
 An engrossing rebuttal to the sense that only Hollywood can do big, kinetic entertainment.
 Writer-director-star Paul Gross spent a lot of time over in Afghanistan as he prepared to make Hyena Road, and the verisimilitude of its combat sequences, both on the ground and back at command centre, is one of the payoffs.
 The film acknowledges political and moral quandaries in Canada's peacekeeping mission in Afghanistan but then applies the most disingenuous tropes from old-school combat films and westerns to gloss over the grey areas.
 The conflict between acting strategically from the comfort of a military base, and acting morally on the ground, plays well in Hyena Road.
 Gross' script realistically depicts the uncertain and frequently shifting alliances that characterized the Afghan conflict's security situation.
 "Hyena Road" has an agreeable modesty to it that almost makes up for what can sometimes be a very pedestrian treatment of contemporary warfare.
 The goal of the movie is truth-telling rather than flag-waving, but it also succeeds as impactful storytelling.
 The war story featuring good people making the best of a bad situation may feel very familiar, but it is a better kind of familiarity that also characterizes Hyena Road.
 Gross' combat scenes are the appropriate kind of chaotic, explosive rushes that tend to feature the soldiers trying to figure what's going on as much as actually fighting.
 The strategy was sound, but the script proves clunky on screen because it's clad in a heavy armor of thought and doesn't leave enough gaps for the subtle nuances and offhand gestures that make characters feel entirely real.
 Canada's own war-on-terror film can't get beyond embedding us among the troops (GoPro-style shots, military code and jargon) to strike at any deep, dark truths about the recent war in Afghanistan.
 It's one of the better films about the Afghan conflicts but it struggles to strike a balance between connecting with real soldiers' experiences and contributing something interesting to the genre.
 While it's not American Sniper, it still seems like a missed opportunity.
 Paul Gross situates the film's events somewhere between violent, militaristic fantasy and gentler, anti-war lament.
 Viewers are graced with a few disappointingly subdued action scenes, two humdrum romantic subplots, and a virtual museum of bad acting.
 The explosions are so wonderfully photographed; if only we actually cared about the characters exploding.
 Gross ... is ultimately unable to satisfy the rules of dramatic engagement.
 "Hyena Road" is a skillfully filmed reminder, if one is needed, about why Afghanistan earned its status as the "graveyard of empires."
 Hyena Road runs two hours long, but its entire narrative could be wedged into a half-hour documentary.
 Gross manages to craft a feature that's horrifying and strangely inviting at the same time, delivering solid characterization to go with all the chaos. 
 An entertaining story with an agile script, easy to understand, full of innocence and lots of warmth, as well as comical situations that will surely make you laugh. [Full Review in Spanish]
 A film that attempts to show us the importance of being kind and noble embodied in Charlie Brown. [Full review in Spanish]
 An ideal movie for this time of year, pleasing fans and also manages to present these beloved characters to a new audience. [Full Review in Spanish]
 It's mercifully "un-modernised" and pretty faithful to Charles M. Schulz' original comic strip, but it's also rather flat and not especially funny. And there's too much padding; it could just as well have been a short rather than feature-length.
 The Peanuts Movie is both modern and traditional, pleasing on all fronts, which must have been hard to achieve.
 Kids will be able to follow the simple storyline and will enjoy the misadventures of the good-natured, well-intentioned Charlie Brown. 
 Snoopy, Charlie Brown and friends finally arrive on the big screen in a movie that sticks close to the gently comical tone of the comic strip that launched in 1950 and the vintage TV shows from the 1960s and 1970s.
 Can be a bit frantic and antic; the ending's more schmaltzy than Schulzy. But the sad-sack, what's-the-point-of-it-all? adult humour sneaks in. Pretty much the least disappointing feature-film adaptation America's most famous comic-strip could get. 
 Anyone who possesses even a passing familiarity with Charlie Brown and the gang from Peanuts should find some appreciable measure of delight with this super-faithful feature film adaptation...a perfectly fine, perfectly safe big-screen translation. 
 An uplifting kids film without any of the cinisism found in most movies nowadays. [Full review in Spanish]
 What other films for children teach in an hour - about life, the universe and everything - Schulz could teach in a line, and this film reflects that. It's undeniably about decency, goodness, and love. And Snoopy.
 Emotional and lots of fun, a great blast from the past and a new discovery for kids today. [Full review in Spanish]
 A charming film, made especially for those who grew up in the comics. [Full review in Spanish]
 A film that works better as an homage rather than recreate the greatness of the original cartoon. [Full review in Spanish]
 If you are looking for an agreeable and entertaining school holidays film for younger children, this is it. Fans of the comic strip should get a kick out of it, too.
 It's a sincere film for children, squarely in the tradition of the sweet, simply drawn Peanuts cartoons from the 1960s.
 The Peanuts Movie manages to maintain the soul of the comic strip even as it upgrades the visuals.
 A flawless, noble and tender family film. [Full Review in Spanish]
 True to the essence of its characters, and animated in a more tridimensional way, Peanuts is funny and has a great message for every age group, and despite not having the most original plot, it will satisfy long-time fans. [Full Review in Spanish]
 The movie knows where it lives: as a precious piece of nostalgia in the minds of grown-ups who cuddled up to Snoopy as kids.
 This is dreary stuff.
 A flat sea shanty.
 A host of pictorially arresting, even painterly images can't make a satisfying whole out of In the Heart of the Sea, Ron Howard's film that doesn't dig very deep, its penetrating title notwitstanding.
 Howard's film is predictable, yet that's not to say it's not enjoyable. If you can get past Hemsworth's mish-mash of an accent (part New England local, part booming Thunder God), there are plenty of impressive turns to be seen.
 The film delves into issues that discards little too fast or invents too late. [Full review in Spanish]
 In the Heart of the Sea is a technical marvel, demonstrating that Howard is still a master of making movies look and sound stunning. Unfortunately, the screenplay was lost at sea.
 It has the basis of the whole tradition of sea adventure films. [Full review in Spanish]
 The story of the ship at the mercy of the great white whale, told with amazing special effects, grandiloquence, and some common places. [Full review in Spanish]
 Unfortunately, the film's script... is far too creaky to make the most of the men's run-ins, while the all too obviously fake computer-generated effects suck the tension out of the showdowns between ship and beast.
 You'll be better off re-reading Moby-Dick than watching this soggy adaptation of the source material
 In The Heart of Sea show us the mastery of narrative Ron Howard has achieved through the real story that inspired Moby Dick. [Full review in Spanish]
 ... a clumsy behemoth, inspiring a kind of dull awe at all the resources mounted for its realization.
 The images here lash and last; the story creaks and groans. Anthony Dod Mantle's cinematography is brilliant; it's as if we're looking back at this past through a water-sloshed spyglass, darkly. The simpler story's too rote and modern, though.
 Visually the film is great, but the premise is forgettable and doesn't offer much. [Full review in Spanish]
 The whale moments are great, but outside of them, the film uses too much unnecessary CGI, which takes away a lot of the film's more real moments.
 Lacks in base thrills, devoid of human drama, and frustratingly crafted to wash away any inherent interest that might somehow have slipped through unmolested.
 The screenplay goes around in circles without creating an enriching experience, the characters have no arc, and what few moments could have been dramatic end up boring and full of cliches. [Full Review in Spanish]
 Ron Howard's In the Heart of the Sea is a beautifully-made film that features stunning visual effects, but it's ultimately let down by a bland screenplay that's unable to bring out the exciting and epic nature of the story.
 There's hardly a genuine moment, and a framing device only adds another layer of dramatization to something that already plays like a big-budget History Channel re-enactment.
 The film's most affecting scenes (are of) Herman Melville interviewing a survivor of the wreck.They sit in a small living room, huddled as close as the survivors in a lifeboat - and as bound by the story of the Essex as was its ill-fated crew.
 You say "goes completely off the rails" like it's a bad thing.
 A riff on the Hollywood conventions of a story we know very well already, with little new to say. James McAvoy's mad scientist is fun to watch, though.
 Unfortunately, - just like the monster in question - the latest offering from director Paul McGuigan (Lucky Number Slevin) is a bit of a lumbering mess.
 This is not your dad's "Frankenstein," more is the pity, and time would be better spent watching the marvelous 1931 original.
 Like the spark of life itself, it's hard to identify the elusive missing ingredient that prevents this ragbag of potentially likable body parts ever earning the accolade "It's alive!"
 It is a strange mix of intentional and unintentional laughs.
 The film repeatedly loses its charge, falling back on dull, franchise world-building for sequels that will probably never be made. What a shame.
 This new version adds little to nothing to the story we already know and have seen so many times. [Full review in Spanish]
 Victor Frankenstein lumbers and lurches rather like the monstrosity that the title character jolts to life with a bolt of lightning from the heavens.
 Old Meat.
 Add the film to the disturbingly long list of dreadful adaptations of a source that deserves better.
 The only time the movie comes to life (so to speak) is during its climax, when Victor succeeds in bringing his creation to life and must deal with the subsequent rampage... After slogging through 100 minutes of tedium to get there, it's hard to care.
 The screenplay for this movie is tedious, boring and hard to believe. [Full review in Spanish]
 Victor Frankenstein is a spectacular movie. [Full review in Spanish]
 Unfortunately, the end result has neither the subtlety of Sherlock nor the intellectual rigor of Mary Shelley's novel - it's a hammy, clunky misfire which largely squanders a strong core concept.
 In spite of being a great production wih a great cast, the movie falls appart due to a bad script. [Full review in Spanish]
 It has so many illogical elements that it becomes an involuntary comedy. [Full review in Spanish]
 Any time you watch a "reimagining" of a story in the public domain, you do so at your own peril.
 [This] schizophrenic lump of stitched-together cinematic remains hardly deserves the moniker 'alive.' 
 What slightly elevates the movie (but doesn't quite redeem due to its almost 2 hour leght) are its costumes and overall production design. [Full Review in Spanish]
 Tyler Labine carries a lot of the film against his straight man Crawford, like the grizzled spawn of Seth Rogen and Jack Black -- a jovial head in parkas and earflap toques.
 In Mountain Men, Labine and his cast achieve a hard-to-strike balance of comedy and heart-felt drama while maintaining a subtleness that respects your adult attention span.
 The script has plenty of salty language and dialogue that rings authentic. The story is well-constructed with a conclusion that will leave you smiling. And the scenery in and around Revelstoke, B.C. is awesome.
 Mountain Men delivers big heart, some big laughs and authentic character development observed through sharp, but always natural, dialogue.
 Fleet and likeable.
 Though Topher forges ahead bravely, it's all too clear that he's headed toward an underwhelming conclusion.
 Writer/director Cameron Labine finds a trail that feels reasonably fresh, due in no small measure to the warm comedic talent of co-star Tyler Labine.
 There are three good laughs in "Mountain Men" and two modestly dramatic sections. That averages out to a decent moment every 18 minutes.
 While some of the interpersonal revelations and inner character struggle feel decidedly familiar, treading the waters of the male coming-of-age tale, the setting is novel and the added dramatics of their adventure freshen the story.
 A refreshingly heartfelt, warm and tender film.
 ... hints at an intriguing character study, yet despite some solid performances, the script derails its momentum with generic and predictable third-act twists.
 Richard Gere gives an annoying performance in 'The Benefactor' -- and it works! 
 An unconvincing melodrama indicting money's power to manipulate as the root of all evil.
 As a memorable work of cinema, it misses every important mark by a mile.
 A run-of-the-mill story of a junkie.
 If the conclusion is a little too sunny, that's a small flaw in an otherwise compelling film about the hazards of trying to buy emotional connection.
 "A compelling portrait of a man on the verge.... Suffers from a sense of rigidity that somehow both fits its themes and stymies a greater sense of realism."
 Despite great cast, melodrama has little to say; drug use.
 Has Richard Gere been hitting the bong?
 There's a richness to the cinematography that, along with the central performance, hearkens back to a character study from a few decades prior.
 The Benefactor is both a bad film and a thoroughly inexplicable one.
 Gere is committed but his character becomes increasingly annoying, with a one-note tortured past, and the script leaves him and his underwritten co-stars stranded with a pat ending.
 The Benefactor is a character portrait in search of a movie.
 Renzi provides a platform for Gere to act in barnstorming fashion but can't work out what to do with any of the other characters. They tend just to stand embarrassed on the sidelines as Gere holds forth and steals every scene in which he appears.
 The price of Gere's good looks is his acting skills don't quite match them.
 Fanning (criminally underused) and James have so little meat to their roles, they end up as ciphers while Gere has a ball as a drug-addicted tycoon on the verge of a nervous breakdown.
 The Benefactor loses whatever anarchic spark it may have had, leaving us with an increasingly empty symphony of its star's repertoire of heavy breathing, blinking and out-of-context laughing.
 Gere is very good as the larger-than-life Franny but the film is too slight to do his performance justice.
 Gere is watchable but constrained by a rushed screenplay that never gives us a proper handle on the man. The potentially intriguing character dynamics fail to splutter into life.
 Presented in a rather unique way, without feeling contrived when doing so.
 After this production, every high school in America is going to want to take a shot at staging this. Grease is the word.
 The ambitious $16 million production of the Rydell High School musical was as impressive as it was fun.
 The three-hour production (almost a third of it, seemingly, commercials) was filled with marvelous moments like this: theatrical reveals, cinematic dissolves, television tempos.
 The show wasn't perfect, but it was great fun with touches of film nostalgia and musical theater earnestness.
 Regardless of its flaws, Grease is a reason to look forward to the next round of live musicals on TV.
 This... was a show that was more about individual moments than about building a story. 
 It had all the infectiousness and emoji-big smiles you need from three hours of live musical entertainment. 
 Thanks to exceptional work from director Thomas Kail and several sterling supporting performances, much of Grease Live! was as sweet and tasty as a root beer float.
 An ambitious, exuberant and eye-boggling adaptation of Grease.
 With this hectic, ambitious and hormonal Grease, Fox proved it could up the ante in the new mini-industry of musicals on TV.
 Fox raised the bar on TV musicals Sunday night with its ambitious, wildly energetic and mostly entertaining Grease: Live.
 It was a seamless, vibrant, energizing hit.
 Despite its innovative direction and talented cast, Grease: Live fell victim to its bland source material - and equally bland leads - leaving it unable to truly top other iterations of the modern TV musical. 
 The show successfully managed to combine all the nostalgic elements of Grease that everyone expected while creating a new experience.
 Grease: Live opted for a more cinematic approach, connecting the dots between the live-to-tape soap operas of the 1940s and the "live music videos" you see at contemporary MTV awards shows.
 Even within the constrains of a live, televised event-which, let's be clear, Fox DOES know how to blow out, when it wants to-I couldn't help think that Grease: Live was just a dud.
 High school itself is the nineteen-fifties of our imagination, always easier in our memories than it was in reality. With Grease, we can pretend we still want to go back.
 If Grease Live had more moments to work with like "There Are Worse Things I Could Do," it might have been truly special - but there was only so much it could do when the musical itself didn't have anything else to give.
 If you hate musicals and hate Grease, Grease Live! sure wasn't going to change your mind all of the sudden. But for the multi-generational fanbase who admire this show, there was plenty to love about Grease Live!
 Thrilling live musical is fun, has iffy messages galore.
 Griffin relishes working in this lurid, sanguinary Italian style, and his enthusiasm is contagious.
 Funny animated sequel has cartoon jeopardy, potty humor.
 Working in a surreally inflected v�rit� style - with few title cards or identifications other than what is spoken on screen - Mr. Sauper also has a knack for catching his subjects in unguarded moments.
 "We Come as Friends" is a travelogue through hell, one that we've come to know far too well wherever it is on the globe. 
 A deliberately vague portrait of Sudan as a country beset by self-interested neocolonialist outsiders.
 A a collection of stolen sights, often dripping in gallows humor, that builds to a subjective portrayal of a true, dense rot.
 Sees Sudan as the epicenter of neocolonial competition between the US and China, and can already see it's about to lead to more war. . .in his pointed political travelogue.
 A riveting documentary that will bring your blood to a boil when you see how Africans are being exploited once again.
 Prepare to be emotionally engrossed and enraged by this alarming and searing expos� on neocolonialism in South Sudan.
 "We Come as Friends" has been beautifully filmed, with Sauper's God's-eye views of his plane's wing and the African landscape below resembling something Saint-Exup�ry might have conjured.
 We Come as Friends aims a cin�ma v�rit� lens on a place where many promises are made, but few are kept.
 Hubert Sauper goes where other people don't go, sees what the crowd doesn't see and creates unsettling, provocative political documentaries that are unlike anyone else's.
 The ongoing tragedy in Africa is too nefarious, too complicated, for any one film to do it justice, but We Come as Friends opens a wide window into this mansion of horrors.
 The filmmaker uses his little plane to give us a bird's eye view of a country struggling with keeping its independence against the odds.
 This beautifully shot documentary is an in depth examination of Sudan from myriad points of view.
 We Come As Friends, Hubert Sauper's teeming, Breughel-and-Bosch-pursuing documentary portrait of chaos after colonialism in battle-torn South Sudan is more eye-widening, surreal, sorrowful and anarchic than his earlier Darwin's Nightmare.
 In case you thought that bad people weren't still doing bad things in Africa, Hubert Sauper's disturbing documentary We Come as Friends will disabuse you of that notion tout de suite. 
 The action always feels as if it's unfolding in present tense, the avant-garde score and disorienting extreme close-ups conveying a sense of nervous, spontaneous energy.
 A fascination with serendipity, irony, and absurdity like that in Werner Herzog's documentaries propels "Friends" into unexpected territory.
 Light on its feet yet dead-serious in tone, this excellent doc alternates micro to macro, ground to air.
 Deeply unsettling and saddening, a brief glimpse of a tanned George Clooney only solidifying the tragedy. This is a pretty powerful work of non-fiction.
 Sauper and his two-man crew fly over a land that's becoming as alien to its indigenous population as it was (and still is) to those who fancy exploiting it.
 There's a lot to like on a conceptual level, but the execution is one-note and monotonous."""]

    let mehCorpus = seq[""" """]
    let wowCorpus = seq[""" """]

    let punctuation = ["."; "?"; "!";]

    let freq_table = match opinion with
                     | Wow -> getFreqTable wowCorpus
                     | Meh -> getFreqTable mehCorpus
                     | Ugh -> getFreqTable ughCorpus
    
    while true do
        let startingState = getRandomValue freq_table |> getRandomWord        
        let markov_chain = generateMarkovChain freq_table startingState [startingState] 15                                          
                         |> List.reduce(fun state input -> state + " " + input)
                         |> (fun x -> x.Replace(" .", ".")
                                       .Replace(" ,", ",")
                                       .Replace(" !", "!")
                                       .Replace(" ?", "?"))
                         |> capitalize                     
                         |> (fun x -> if not <| (punctuation |> List.contains (x.[x.Length - 1].ToString())) then (x + ".") else x)

        printfn "%A" markov_chain         
    Console.ReadKey() |> ignore              
    

    ()  