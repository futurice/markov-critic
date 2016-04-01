module Critiquer

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
    let randIndex = rand.Next(size)  
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
 There's a lot to like on a conceptual level, but the execution is one-note and monotonous.
 Few would argue how powerful and effective this last adaptation turned out to be. [Full Review in Spanish]
 Too bad it also transmits a certain narrative tiredness because it follows too closely the original play. [Full Review in Spanish]
 Kurzel has put together an adaptation worthy of 'The Scottish Play', but less powerful than the sum of its parts suggests. [Full Review in Spanish]
 Fassbender and Cotillard deliver mesmerising lead performances which are both compelling and realistic.
 Blunt, dreamy and visceral new adaptation of the immortal work of William Shakespeare, technically very lucid and brilliantly defended by Michael Fassbender and Marion Cotillard. [Full review in Spanish]
 It capture the spirit and the poetic greatness with remarkable eloquence. [Full revies in Spanish]
 This film is heavy into style and it was a style that will have selective appeal. But the story is strong enough to hold the audience captive with what is, as comes as no surprise, a good
 Fassbender and company do justice to the Bard's 'Double, Double Toil and Trouble' classic .
 This is a powerful, well acted and directed (Justin Kurzel) adaptation, which gives unusual depth to the character of Lady Macbeth. It gives additional force to the play's message about the evil and corruption which can result from great ambition.
 Striking visuals guide the way more breathlessly than any footnote could.
 Not all of your favorite scenes may be here, but the ones that are, will satisfy.
 Bold, insightful and bracingly cinematic.
 The two-hour film is full of synthesized sound and fury, and what it signifies is not worth watching.
 A worthy 'Macbeth' refresher course, highlighted by Michael Fassbender's feverish Macbeth and Marion Cotillard's simmering Lady Macbeth. 
 'Macbeth' lives valiantly and dies soddenly by its decision to substitute so much of the Bard's poetry with beautiful but empty visuals.
 Perhaps the fiercest cinematic translation of Shakespeare to date.
 The present edition, in spite of an impressive cast, is a blood bath that is more violent than any before, but to no avail. 
 Live by the sword, die by the sword, as the old saying goes, and what a fantastic homage this is to one of history's best dramaturges. [Full Review in Spanish]
 Michael Fassbender's grave Macbeth is immaculate from the start; he is forged on the battlefield and never seems to leave it.
 A bleak, daring drama from Ukraine.
 If there's one place where love has no place, it's here.
 All of it makes for a unique cinematic experience, an ambitious work that relies on its audience taking a leap of faith. Those that do will be rewarded.
 There is nothing else like The Tribe, at once a searing, singular vision of a particular time and place and a brutal metaphor for the wounded human condition.
 The lack of spoken words or score also heightens the natural sound to a magnified impact in a film that's hardly lacking impact. 
 An intriguing, unique story done a disservice by some frustrating directorial choices. Leaving it untranslated means we only get the broad strokes where I want details. 
 Despite the...longueurs 'The Tribe' is certainly a dark and powerful portrait of the grim goings-on at a school where violence plays a far greater role than education, told in a fashion that can't help but fascinate.
 It must overcome its gimmick, or become a footnote. Slaboshpitsky succeeds by using a language that crosses all social and cultural barriers: violence. Also, sex.
 This is a challenging film that's not for everyone. Yet there's no denying its brilliant concept and its raw cinematic power.
 The Tribe is constantly riveting, and even if there is one other movie without spoken dialogue this year, Shaun the Sheep Movie, you still won't see anything else like it. 
 What sets The Tribe from any other youth crime film is the way it's told, challenging the viewer every step of the way. [Full review in Spanish]
 The end result is like nothing else out there and while the violence that accompanies the characters' fates requires a strong stomach, this is a compelling experiment in pure cinema that's worth experiencing.
 The Tribe revels in the distance it leaves between its audience and its characters, but in placing viewers on the outside, it also creates an experience that's almost perversely empathetic.
 It's a movie Samuel Beckett would have loved, exploding with language but existentially acknowledging both how little is communicated and how much humanity we share regardless.
 The Tribe shows that you don't need dialogue to communicate.
 While The Tribe often makes for troubling, confrontational viewing, it is ferociously engaging. Stripped of dialogue, this is cinema of high-wire purity: muscular, precise, emotionally complex -- and surprisingly easy to follow.
 The movie features no music, and no words, yet some moments are so powerful and visceral that I still caught myself covering my ears.
 Whether [Slaboshpytskiy] can achieve anything so innovative after The Tribe may be an open question, but on the basis of this debut his ability to handle powerful drama looks extremely assured, even when it's excruciating to watch.
 Easily the most intense movie experience of the the year, The Tribe is an unsettling examination of how the oppressed can become the oppressor.
 This is pure cinema where words are not necessary; the actions of the characters speak for themselves. And they speak oh so loudly... there is no way for the audience to cover their ears.
 Thanks to witty writing and excellent performances, Break Point turns out to be one of the funniest sports comedies that we've seen in years.
 Jeremy Sisto and David Walton are well matched in this breezy, likable tennis comedy.
 A mostly laugh-free, paint-by-numbers approach to a pair of former pros vying for relevance as they enter, kicking and screaming, into their mid 30s.
 Easygoing and always likeable but hardly packed with laughs.
 The script plays like a random automatic serving machine. Ideas bounce all over the place, only to be quickly followed by more.
 Karas doesn't exactly reinvent the wheel as he puts this odd couple through the paces of getting in shape and reconciling old wounds, but he's helped by some laugh-out-loud quirk ...
 "Break Point" has its difficulties with storytelling and tone, but it carries most of the way, finding a rich sense of humor on and off the court.
 It's the kind of amiable but predictable trifle that nobody ever seeks out, but that will mildly amuse everyone who happens to stumble onto it when it hits cable and the streaming services.
 There's just too much d�ja vu at play on this court.
 Always nice to enjoy a little comfort-food movie in which almost nothing surprising or particularly fresh happens, but we're happy to spend time with the characters and we wish them the best as the credits roll.
 The affable, shaggy-dog tennis comedy Break Point is bristly and charming, just like its star and producer, Jeremy Sisto, who plays the affable shaggy-dog Jimmy Price, a washed-up semi-pro tennis player.
 It's a mild-mannered film, strange considering the ferocious nature of tennis itself, the sport that supposedly holds "Break Point" together.
 Quietly moving comedy about tennis, brotherhood; language.
 "Break Point" looks good, and it has a big serve, but it's hard to love wholeheartedly.
 Break Point may not be a great film, but it certainly has a lot of heart. This is a sweet flick.
 As far as tennis comedies go, this one sits somewhere between WIMBLEDON and BALLS OUT. Make of that what you will.
 Break Point is light and bouncy, not unlike a tennis ball being lobbed back and forth. You always know where it's going, yet there's a distinct pleasure in the fast-paced way it gets there. 
 Break Point has its moments. It's passable, light entertainment, but ultimately comes up short when reaching for deeper comedic or dramatic flair.
 Even if you manage to disentangle the many twists relatively early on, there is still pleasure to be had here in witnessing how Pastoll's bilingual screenplay allows crucial giveaways to get lost in translation. 
 Interesting yet over-long, melodramatic and fatally predictable, Road Games is a backwoods horror film with a sheen of class, respectability and Barbara Crampton. These things go some way to save the day.
 Writer-director Abner Pastoll delivers an accomplished, tense and unpredictable thriller that makes the most of a strong cast.
 Nothing is quite what is seems on this psycho road trip, and crazy revelations are eventually spilled.
 It's reasonably entertaining if not a tad by the numbers.
 Stripped-down thriller benefits from its setting.
 Fueled by strong performances and some genuinely riveting twists and turns, it's a solid genre film that delivers the goods in a largely fresh way.
 [A] well-acted but formulaic thriller.
 The cast doesn't quite succeed in keeping the suspense fresh throughout the story's left turns.
 Yes, you can enjoy Road Games even if you aren't a gorehound. I wish I could recommend Road Games beyond that though, since all roads lead to a twist ending that you can kinda-sorta make out from a mile off.
 Road Games is far too slight and simple for a premise that starts with a twisted promise. It's more a board game than a chess match.
 A twisty little ride through the French countryside where everyone is suspect of playing hide the cheese knife.
 Though the inspiration here is clearly Hitchcockian, the movie's vibe is so tongue-in-cheek as to be weightless.
 Despite that late inning stumble, Road Games still provides enough suspense, thrills and dark humor to leave genre buffs more than satisfied.
 Rather like watching a car wreck on the opposite side of a motorway.
 For all the amusingly fatuous remarks heard here -- and Miss Spheeris has a great ear for these -- the overriding dimness of most of the fans and musicians is frightening.
 This is a well-made, observant documentary, with attitude to spare and plenty of justifiable laughs at the expense of its subjects.
 There's so little respect for the music that we never see or hear a number from beginning to end, and we rarely hear any of the musicians speak more than a few seconds at a time.
 Fascinating.
 ...benefits substantially from the periodic inclusion of electrifying moments...
 [Tommy] Oliver hits all the familiar notes in unfamiliar ways, focusing on the impact of violence and exploitation.
 1982 is a ham-fisted morality tale about love, marriage and the fallout of the '80s crack epidemic as though told by someone whose intel on all three came primarily from pulp sources.
 The film feels coolly detached because the story and characters are underdeveloped.
 Suitably low-key but sometimes under-realized, this drama is fueled by its working-class milieu and a heart-wrenching performance by Hill Harper.
 Meandering and inconsequential.
 Sorrentino wants to say something profound about illnesses that bury loved ones alive. But the pompous lines kill the mood.
 Caine and Keitel are great together and Sorrentino delivers some typically gasp-inducing visual flourishes. But it's also unmistakably indulgent and, save for a few scenes, doesn't quite deliver the insightful meditation on ageing it promises.
 Youth largely consists of a bunch of people rambling around a resort doing nothing. But I can't think of a better bunch of people to ramble around and do nothing with.
 Youth is as psychologically savvy as it is beautiful.
 Sorrentino's Youth is sublime.
 It manages to be both pithy and pretentious.
 Paolo Sorrentino's script and direction are indulgent and extravagant, his words and pictures thoughtfully considered for comedy, character and quiet provocation.
 Caine is terrific -- inscrutable and distant, but evidently there are depths behind his oversized horn-rimmed glasses.
 While Youth may not be his worst film, it is his most pretentious and bombastic.
 Does Sorrentino attempt to tackle too much in this film? Possibly, though I would rather see a director experimenting with too many ideas than scraping the barrel with too few. Youth is a rich and rewarding experience.
 Absurd but at the same time profound, this is a rare movie find. I have seen it twice and plan to see it again.
 Overwrought nine-tenth-life crisis drama; not even a great cast can create sympathy for the artistic and existential turning points on arty display.
 Youth might not be the most bedazzling of Sorrentino's films -- then again, how could one ever outdo The Great Beauty? But it is indeed the director's most compassionate and affecting film to date.
 This new attempt by Italian director Sorrentino gets lost in translation. [Full review in Spanish]
 Sorrentino's lush cinematography captures the decadent beauty of wealth at the Swiss Alps resort, but also the claustrophobic, disconnected quality of spaces occupied by the hyper-privileged.
 How the hell did Paolo Sorrentino's latest not dominate awards season? Spotlight and The Big Short are timely films. Youth is timeless. 
 Sorrentino's work isn't to everyone's taste, his films can be a bit langorous and obtuse, so it depends on whether you find the pay-off worth it. If you do, this is excellent.
 A decidedly mixed bag of a film, varying wildly in quality on almost a scene-by-scene basis.
 Artfully dreamlike and boasting towering performances by three legendary actors, Youth is a mesmerizing meditation on age, beauty and friendship
 Despite the surface sheen, and some enterprising plot twists, it doesn't entirely convince.
 This British sci-fi punches well above its budget visually, it's a shame the drama can't match it.
 This doesn't come together, but Trefgarne has clearly got talent.
 There's simply too much going on to establish characters. More upsettingly - being that this is a sci-fi film - it's impossible to tell what the cool parts are supposed to be.
 It's obvious a lot of care, time and creative passion went into it. It's a relatively low budget project that does a great job pretending it's not. But behind the aesthetics, there isn't much else.
 A strong, if not genre-changing, feature debut for actor-turned-writer/director Justin Trefgarne.
 "Narcopolis" starts off intriguingly and ends solidly. It's everything else in between that isn't particularly compelling.
 even if it doesn't ever rise to legendary status itself, Legend is a fitting portrait of the twin gangsters who, while they had their moment in the '60s, have already begun to fade from view
 What could have been a blazing entry into the pantheon of great British mob dramas is instead a dish of stale pudding.
 It's a movie that clearly states its goals but that doesn't accomplish them.
 As Reggie's emotionally fragile wife, Emily Browning is an oasis of sympathy amid the squalor, the only person in the movie we really give a damn about.
 Hardy is an ensemble all by himself.
 Tom Hardy impresses mightily as he"splits up with himself." Yet even with his skills, Legend often misses the mark.
 While "Legend" definitely has its drawbacks, you basically overlook or ignore them because of Hardy. His characterizations are superb, showing the contradictions and nuances of brotherly sociopaths who are laws unto themselves.
 A stupendous exercise of the criminal tale, of period reconstruction, of giving new life to a whole tradition. [Full Review in Spanish]
 Hardy is [Legend's] saving grace, valiantly dual acting in the roles of the very different twin brothers.
 Hardy and Emily Browning give excellent performances in this film, co-written and directed by Brian Helgeland ('42'). The story is compelling and the characters are interesting. It is a solid historical drama.
 Tom Hardy's double performance is worth the admission price. [Full review in Spanish]
 An average movie with an amazing performance by Tom Hardy. [Full review in Spanish]
 Even though the efforts to make the twins likeable and endearing characters are there, we can't really empathize with them and the film feels just like another glorification of violence for violence sake. [Full review in Spanish]
 A dynamic tale where Tom Hardy's acting chops become the center of attention. [Full review in Spanish]
 What stands out are the good performances that possibly just would like to fans of the gangster movies. [Full review in Spanish]
 Stretches a little too long, but it's a pretty good gangster film. [Full review in Spanish]
 Tom Hardy's excellent performance is sadly not enough to pick up the work of director Brian Helgeland, who is better suited writing great stories behind cameras. [Full Review in Spanish]
 The perfect setting and the outstanding work of Tom Hardy are the reasons to see the film. [Full review in Spanish]
 Tom Hardy sustain the film playing the notorious Kray twins. [Full review in Spanish]
 Legend is entertaining, has good rhythm and makes good use of period music. The problem is that it could have been so much more, considering the material it was based on. [Full Review in Spanish]
 It's hard to imagine a world where Eddie Redmayne won't nab the Best Actor gong for the second year running.
 Eddie Redmayne's performance as Einar Wegener, 'The Danish Girl,' is revealing, heartbreaking and believable.
 Fresh-faced British actor Eddie Redmayne here provides another sterling example of just how deeply he can immerse himself into a role.
 The film's story is unique and brave and the two commanding performances give it a gripping emotional weight that is very affecting.
 That we never really glean who "the Danish girl" is is quite fitting since the filmmakers also haven't the slightest idea who their characters really are.
 The acting is what makes this film.
 Numbing translation to film of a vital drama about the first transexual in history. [Full Review in Spanish]
 I wish the filmmakers had dwelled in the strange nature of Gerda and Lili's sexuality as they came to understand it together. Instead, it devolves into something a lot more sentimental and easy.
 Vikander delves beneath the surface to explore a truly fascinating woman. And Redmayne is simply wonderful, totally convincing both as Einar and as the "Danish girl" of the film's title.
 Helped considerably by Vikander's strong playing and a lovely turn by Belgian actor Matthias Schoenaerts (late of the remake of Far From The Madding Crowd) as Einar's old friend Hans Axgil, this nevertheless hangs upon Redmayne.
 It's a subject that isn't often explored in mainstream cinema and so hopefully this film makes, at the very least, a small difference.
 The Danish Girl is a tale that, however delicate, humane and elegant, is also excessively hackeneyed and ends up boring an audience that looks for more. [Full Review in Spanish]
 [An] earnestly told, splendidly visualized film that could please the masses but will likely electrify few.
 It's commendable that Hooper (The King's Speech) has brought Einar Wegener to a multiplex near you... But this restrained, elegant drama seems at odds with its pioneering subject.
 Tom Hooper still doesn't show any sign of evolution as a director. [Full review in Portuguese.]
 Both Eddie Redmayne and Alicia Vikander shine on their own, but their interaction and relationship on screen is the real enjoyment for the audience. [Full Review in Spanish]
 Loved The Danish Girl. I hated Lili...
 Because of the simplicity with which The Danish Girl treats such a controversial subject matter, the movie feels like a wasted opportunity to portray a character the deserves admiration for their courage. [Full Review in Spanish]
 "The Danish Girl" fails to give its full potential, but it is good enough to recommend it with confidence. [Full review in Spanish] 
 Vikander is excellent, Redmayne is tremendous and Hooper does a great job harnessing the performers to get to the emotional truth of the story.
 A memory lane roach motel melancholy lapse into Greed Decade doom not unlike now, as this bad boy veers in a slowly simmering, psychologically dense portrait, between victim and villain. While toying with audience senses, and his chosen targets as well.
 A 9-year-old sociopath stubbornly fails to become scary in this stillborn thriller.
 By the time the film goes out in a blazing inferno of hell-raising loneliness and tops itself off with the best final line-to-credit song combo since Killer Joe, The Boy has reached a point of stupid fun.
 The scariest aspect of "The Boy" is the extent to which Macneill makes it possible to sympathize with the troubled protagonist -- even as its haunting final shot hints at the horrors yet to come.
 The Boy's most disturbing facet is the possibilities it imagines.
 The director tries to generate a pace that his dramatic efforts fail to match.
 While it's admirable that director Macneill and his co-scripter Clay McLeod Chapman opted to emphasize mood and psychology over the story's more exploitable elements, it nonetheless results in a listless tedium.
 While the score goes out of its way to make his every action feel sinister, the picture doesn't fulfill its horrific potential until the third act.
 Craig William Macneill's film is a sporadically frightening slow burn with a fatally overlong fuse.
 The film feels overly long, and while lingering shots of the mountain scenery do help convey the isolation of the deserted motel, too many of them feel repetitive.
 Mr. Macneill and his co-screenwriter, Clay McLeod Chapman, have developed a feature stunning to behold if somewhat unpersuasive in narrative.
 Chapman and Macneill have a truly chilling character whose evolution will be both fascinating and frightening to watch unfold.
 The Boy is a very slow burn, one that successfully works to the narrative at hand, but isn't particularly enjoyable to watch.
 The Boy is a title that makes this movie sound innocuous. A more fitting header would be Portrait of the Serial Killer as a Young Boy.
 Not since Henry: Portrait of a Serial Killer has a movie gotten inside the head of a killer with such cold-blooded artistry.
 It makes no bones about the fact that there is something critically broken in Ted. The question for the audience is whether it is nature, nurture, or a mixture of both.
 It succeeds in conveying the dark edge of an effective thriller, but it lacks the human sentiment -- the poignancy, the devastation -- that would've made it soar above less heady genre fare.
 An austere and chilling portrait of America's abandoned margins, The Boy is a slow-burner that builds and builds to its climactic conflagration, and offers a dark, disturbing flipside to Richard Linklater's Boyhood (2014).
 In the march to the end, "Miss You Already" grinds its gears through the late stages of cancer, following a predictable and cliche path. Each scene is a yank on the heartstrings, a poke in the tear ducts, its intents and machinations plainly obvious. 
 A whimsical dramedy that induces the same feeling as eating a cup of Pinkberry or sharing a bottle of chardonnay with a friend and watching E!
 Miss You Already possesses a chemistry that's worthy enough for those in the mood for a downish-note friendship drama, but it's an experience I'll probably never subject myself to again - a strange sign of approval, but approval nonetheless.
 It winningly reflects how to utilize quiet understandings and, yes, very loud laughter. 
 Despite the earnest approach from its two stars (who also serve, with Christopher Smith, as producers), there isn't anything fresh in the story.
 The script from first-time feature writer Morwenna Banks lacks grit and complexity, attributes Hardwicke also disappointingly eschews in playing it safe with trite scenes.
 This heartfelt tribute to female bonding is steered in predictable directions, more eager to jerk tears than explore new ground.
 Is this really an honest portrait of a beautiful and intense friendship between women? What's intense about it, or beautiful, or honest?
 There's a jagged energy and a rawness to it. Miss You Already jerks its tears honestly.
 [The] script has the excruciating familiarity and predictability of a gal-pal tearjerker we've seen a hundred times already; all that's missing is to have Barrymore burst into a chorus of "Wind Beneath My Wings."
 ...The movie has the tone of a live-action Cosmopolitan magazine, pushing the viewer to make judgments about people and behaviours, urging you to invest in the drama from an ego point of view first.
 There's a tough-minded drama struggling to break through the movie's glossy veneer-a contemplation of the black hole of death that, sooner or later, becomes the center of life.
 A jarringly jolly weeper whose save it from the saccharine. 
 [Collette and Barrymore] capture the bond between two women who love each other unconditionally, for better or worse.
 It's a proposal about feelings and finds its audience in every person who want to be moved, have a good time, and even reflect a little. [Full Review in Spanish]
 Screenwriter Banks redeems herself with a well-judged final act that tugs our heartstrings without feeling like we're being shamelessly manipulated into reaching for another tissue.
 It's the gallows humour delivered with warmth by Collette, Drew Barrymore, Dominic Cooper and Paddy Considine that prevents this becoming unbearably maudlin.
 An intelligent script that mixes drama and comedy in a subtle way, making this a visual and emotional delight. [Full review in Spanish]
 A film that doesn't have a perfect story but achieves its goal of showing real life in a moving way without being preachy. [Full review in Spanish]
 Miss You Already recontextualizes beats of the cancer movie sub-genre into a story about the bulletproof power of female friendship.
 A fascinating portrait of a man some would call driven, and others may call psychotic.
 Fighting bulls. It's a tough job, but does someone *really* have to do it??
 This documentary captures what may be the last of Antonio Barrera's bullfighting days at a time when the sport itself seems about to fade into history.
 It's rather hard to feel wholly sympathetic towards the quixotic bullfighter who at one point declares, "I've never had a relationship, even with a woman, as intimate as the one I have with a bull."
 Fascinating and oddly inspiring, Gored is too focused on Barrera the myth to ever really focus on Barrera the man.
 Trivedi and Naqvi put together this multi-tentacled story using a daunting variety of footage, news reports and archival film mixed with interviews. It paints an extremely grim picture.
 You can't help but be impressed (if aghast) at the lengths some matadors will go to please an audience.
 even at a mere 80 minutes, this feature feels padded and overlong - and the ingenious way that the writer/director contrives to stage events before Emma's various devices also sadly palls through repetition. In short, needs more slashing.
 Sucker is likable, geeky and gently funny rather than hilarious.
 Spall is the film's greatest asset but Luc's performance is a more harmonious fit tonally. Like the film, he is pleasant, unprepossessing and nothing to write home about.
 Sucker errs by not spending enough screen-time devising and carrying off creative cons; instead, it invests too much in sub-stories that are never fleshed out, nor as engrossing as the cons these promising characters could have pulled off.
 Undemanding and ultimately irritating but gentle con-man comedy from Australia.
 Whereas "Lost in Thailand" felt like a homage to Stephen Chow's brand of slapstick, "Lost in Hong Kong" looks to be an all-encompassing love letter to Hong Kong filmmaking.
 Perhaps because it tries too hard to be too many things, the movie loses its punch.
 Sappy crowd pleasing Hong Kong comedy.
 Sturdy, well-acted, but a mostly dull and unsurprising "You can't go home" melodrama. 
 A Country Called Home never goes all-in on the implied cornpone of its title-but it never really goes all-in on anything else, either.
 There's heart here for the taking, but Axster turns a complicated domestic and emotional crisis into an episode of a network television drama.
 Lacks the clear vision to know how to distinguish itself from a hundred other similar movies.
 There's exactly one fascinating, original character in Anna Axster's well-meaning but bland debut feature, "A Country Called Home." Unfortunately, the movie's not about him.
 The rigorously dull A Country Called Home hates flyover country more than Bill Maher.
 The third act in particular becomes an awkward mixture of broad comedy and pathos.
 It's refreshing to see Poots in a role such as this, one that lets her easygoing charm shine through. 
 The gentle drama A Country Called Home is in sync with its small-town Texas setting. Unfortunately, much of it is also as flat as the terrain, despite the efforts of an engaging cast led by Imogen Poots.
 While I found it sincere, I felt like it meandered too much and suffered from anxiety over what kind of a film it wanted to be. 
 A Country Called Home is a decent enough trip through the back roads of Texas, connecting with a partly dysfunctional family. It's just not something that most viewers will want to write home about.
 There is not a lot in the plot that is surprising or even original. There is some wit and there is a not too unrealistic look at strained family relationships. 
 Parents and grandparents shouldn't be surprised if, afterward, youngsters are sufficiently curious to Google info about the real-life Apollo moon missions that the movie playfully references.
 Works hard in an effort to recapture the fizzy thrills of action-centric Pixar flicks like 'The Incredibles'.
 For some Capture the Flag will just about scrape by on its honourable intentions and brisk delivery; however, what seems on paper like a vaguely educational distraction, is merely another exercise in colourful chaos.
 Several of the characters are so grating you long for a lunar disaster to strand them as far away from planet Earth as possible.
 Capture The Flag has enough visual energy to get to the moon and back. Alas, the script blows harder than a solar wind.
 It's a feeble storyline and the film is only partially lifted by some lively action sequences and one or two Gravity-like scenes of the astronauts drifting in space.
 It may lack the finesse and charm of animation from Pixar, Disney or DreamWorks, but it's a decent yarn for younger kids who want something to watch on a rainy day or school holiday.
 Aside from a fleeting post-Minions Kubrick gag, there's little here for the oldies and nothing that the Pixar generation won't have seen done before and better.
 So mind-numbingly stupid that any investment in the characters is next to impossible.
 Capture the Flag sits comfortably at the classier end of children's fare for the start of 2016.
 There is an inkling of a better film here but something appears to have got lost in translation, making this quite the forgettable voyage.
 This colourful animation tries a little too hard to get down with the kids.
 A good family film with a positive message. [Full review in Spanish]
 One of those films kids can see over and over again and adults won't have to suffer when seeing it. [Full review in Spanish]
 Great animation, too bad the story isn't that great or original. Still pretty decent. [Full review in Spanish]
 It touches on the subject of keeping families together successfully. [Full review in Spanish]
 The film entertains and will certainly be enjoyed by children and adults with knowledge in films and pop culture. [Full review in Spanish]
 A good effort that achieves it's low goals. [Full review in Spanish]
 They just don't make this kind of romantic trifle anymore-and for good reason.
 A combination romance, farce and road movie that whiffs in all three departments.
 Lackluster mother-daughter comedy has teen drug use.
 The film ultimately boils down to people bludgeoning one another in unimaginative close-ups.
 Like the house, the movie is something of a shifting puzzle box; if only the backstory that turns its gears weren't so rotten.
 As for the home-invasion angle, there isn't quite enough to set it apart from the pack. Much like the title, "Intruders" is just a little too familiar. "You're Next," this is decidedly not.
 Intruders blends a few horror subgenres to create something complex and engrossing, all while wearing influences from Adam Wingard like a badge of pride.
 Intruders ultimately comes across like basic-cable schlock (or is it Netflix schlock now?), slightly redeemed by the germ of a great idea, even if said idea never truly germinates.
 "Intruders" doesn't provide a pounding viewing experience, but it's sharp where it counts, holding to a rhythm of torment and confusion that keeps the picture engrossing and repeatedly unnerving.
 Intruders is tautly directed by first-time feature filmmaker Adam Schindler.
 "Intruders," a distasteful thriller with a bludgeoning sensibility and little common sense, turns a cozy family home into a clockwork house of horrors.
 The actors alone can't sustain "Intruders" for its full 90 minutes, but for the most part they follow Starr's lead, carrying a film that's both menacing and magnetic.
 The result is a promising film that leaves a bad taste in your mouth, like a meal well-presented on the plate that just doesn't fill you up.
 An efficiently engineered suspenser, with solid performances and a tight pace.
 Well-acted and initially suspenseful before it turns increasingly implausible, lazy and uninspired as it piles on the twists in unsatisfying ways.
 A highly competent, suspenseful, and fun thriller that never overstays its welcome. Do it a favor, and open the door.
 The film doesn't know if it wants to be an over-the-top, sensationalized shocker with cartoonishly evil villains... or a more realistic, gritty thriller with human characters, and the end result lies in a muddied, thrill-less, exasperating middle ground.
 Intruders at least tries to do something different, and it does manage to keep things ticking over smoothly enough.
 Of the numerous DC LEGO films, Cosmic Clash is one of the better entries in the franchise. Just be prepared to make a trip to the closest LEGO retailer afterwards.
 Heroes time-travel and fight evil in funny, inventive tale.
 An engaging and thoughtful snapshot of Dean's life, enlivened by a pair of superb performances and stunning production design.
 Beautifully written and directed, this fact-based drama is an odd mixture of excellent acting and not-quite-right casting.
 As a portrait of James Dean, Life only manages to capture his soft, stolid side.
 It's odd that Corbijn, a gifted still photographer in his own right, has so little to say about the relationship between shooter and subject, or the impermanence of celebrity.
 While the script is sometimes too heavy footed, on the whole Life has an unassuming quality that wears well over the course of its two hours.
 Life misses the mark by perhaps a sixtieth of a second, but that's enough.
 It ends up demystifying Dean, perhaps by accident but no less regrettably.
 I loved Ben Kingsley's over-the-top work as studio head Jack Warner, who in one scene explains the lay of the land to Mr. Dean in a manner that would inspire envy from Don Corleone. This guy isn't messing around.
 Dig, if you will, the pictures, but you don't need "Life" as a stargazing aid.
 The actors and their exchanges ring true, and by the time the film reaches its lonesome conclusion, the resonances are eerie.
 A moody, leisurely and occasionally frustrating piece of work ...
 Flat James Dean biopic has swearing, some nudity.
 Both Pattinson and DeHaan could have used more to do, but both actors put in performances that elevate the proceedings.
 Corbijn is great at taking real-life flesh and blood people and alchemically rendering them in striking 2-dimensional images that transcend and mythologize the reality, but "Life" shows him fall some way short of achieving the reverse.
 There's not much to go on here.
 Pattinson and DeHaan are both strong, portraying real people with a mix of imitation and individuality.
 It really is the work of Pattinson, and even more so DeHaan, that makes "Life" a success.
 As the film's emotional anchor, Stock isn't nearly as fascinating by comparison.
 For Dean aficionados, this is a wonderful way to get the backstory on some of the most iconic images we have of the star.
 Life turned out to be as bland as its title.
 It's an entertaining romp and one of the funniest Christmas comedies we've seen for years.
 As with the appalling Pyongyang-set The Interview of last year, The Night Before relies on swearing, stereotypes, causing offence and adolescent sexual jokes for its laughs.
 Like your old Christmas jumper, Rogen is feeling a little worn.
 I'd go so far as to say that The Night Before might turn into one of those yuletide favorites the whole family will go on to enjoy.
 Loaded with party drugs and with Rogen putting in yet another of his rote man-child performances - really, Seth, enough already - it's a pleasant enough time-killer though all the improvised scenes with over-lapping dialogue are a little hard to make out
 Top marks to Jillian Bell and Michael Shannon for briefly raising the overall tone; brickbats to James Franco for bringing it right back down again.
 There isn't enough story to sustain the running time and it lacks howlingly funny set-pieces.
 It's A Wonderful Life meets Pineapple Express in this stoner Christmas comedy.
 It's bogged down by too many derailing tangents, but the three appealing leads have a wonderful chemistry, and it gets close to the spirit of the season.
 In The Night Before, which Levine directed and co-wrote, sweetness and crudity mingle from the outset. 
 A film about how time changes friendships with hilarious jokes and humor to along with it. [Full review in Spanish]
 Unfortunately, the moments designed as comic relief become the main focus of the film, giving us cheap laughs that stray away from the main plot. [Full review in Spanish]
 Michael Shannon's name isn't on the movie poster, but "The Night Before" wouldn't be as memorable or funny without him. 
 This blending of the stoner bromance with the Christmas comedy works surprisingly well, layering gross-out humour with holiday sentimentality.
 A hilarious Christmas comedy that deftly mixes drug-fueled humor with holiday sentimentality.
 There's a glimmer of genuine sweetness beneath the tomfoolery in The Night Before, which occasionally casts a warm glow over characters as they learn valuable lessons about the power of friendship to overcome every obstacle.
 Such a ragged, staggering, let-it-all-hang-out bro-comedy that the women feel parachuted in. Strains to be a Christmas movie for '80s babies now all grown up but umbilically attached to their smartphones. 
 There's small pockets of convincing dialogue between the friends, but this clashes with random, chaotic set pieces.
 The Night Before might well have called Christmas Carol on drugs. [Full Review in Spanish]
 In the midst of an otherwise by-the-book, boys-will-be-boys tale, it's one hell of Christmas cracker surprise.
 Guy Maddin delivers another of his wild and whimsical fantasies, tinged with camp and couched in the film grammar of silent cinema.
 The Forbidden Room is an absolutely manic, joyous romp through a hilariously warped revision of seemingly the entirety of silent film, an endlessly inventive celebration of the limitlessness and sheer dexterity of cinema from the first frame to the last.
 The tinted, corroded images ripple, pulsate and disintegrate. They burn up, then reconfigure as something else entirely. The surface of the movie is liquid; the actors seem to be drifting in an ocean of photographic developer fluid.
 It is sometimes brilliant and sometimes boring, but even the boring parts have an eccentric sparkle.
 The Forbidden Room is a fun ride for cineastes and was much f�ted at festivals. Whether it will appeal to mainstream taste with its insane mash-up of B movie-style parody, silent-movie intertitles and jungle vampires is anyone's guess.
 Maddin remains a unique treasure, and The Forbidden Room is one hell of a trove.
 Exquisitely designed, this cornucopia of melodramatic fragments and movie pastiches will enchant Guy Maddin fans.
 A visual phantasmagoria with a built-in propulsive energy.
 A grueling marathon of cinematic masturbation; a mind-numbingly empty exercise in self-conscious style with absolutely nothing to say.
 It's an exercise in high kitsch which enraptures at first, but becomes increasingly enervating the longer it lasts.
 If you're up for The Forbidden Room, it fulfills it's oneiric promise by leaving you with the vague impression that you dreamed it - and dreamed its dreams within dreams - even if you know you didn't.
 The screen pulsates like infernal internal organs, or bubbles and mutates like melting celluloid jammed in a hot projector.
 Once again, Maddin draws his inspiration from cinema's transitional phase between silent and sound, and he is abetted wonderfully by production designer Galen Johnson and cinematographer St�phanie Weber-Biron.
 An exhausting and overwhelming film to experience, but there's also something exhilarating about its mad energy and boundless invention.
 What Maddin & Co. have invented here ranges from Freudian horror to childish naughtiness.
 There's a strange comfort in knowing that Freud's fascinating topography of the mind will never be completely discredited while Maddin is making deliriously absurd movies.
 Dense and lacking the playful quality of his more straightforward work, this represents a new multi-narrative direction for Maddin.
 Whilst The Forbidden Room is overlong and messy, it is still a lightning bolt of creativity from one of the most enigmatic and compelling filmmakers working today.
 The bad part is that at over two hours long the film is not that good.
 Canadian iconoclast Guy Maddin has been making strange, surreal films that evoke the images and storytelling traditions of silent movies for decades. The Forbidden Room (2015)... is like a compendium of his obsessions and cinematic fetishes.
 Some generally witty lines are trampled by many more that are neither clever nor funny. 
 Amy Ryan has a nice turn as Don's naive assistant, but her humanity is more than this movie can handle.
 A joyless celebration of well-trod stereotypes in an even more joyless madcap lampoon.
 Sam Rockwell applies his usual deft touch to the title character, whose born-again ministry is founded on his dubious excavation of religious relics in Israel, but whose charlatanism serves a sincere and abiding faith.
 The humor is very hit-and-miss, both because it's wielding a rather blunt satirical weapon and because the sheer number of people Don fools becomes hard to swallow. But when Don Verdean's barbs land, they work ...
 Hess' eccentric characters are neither likable enough to root for nor ridiculous enough to earn big laughs.
 There is an absence of meanness in the Hesses' comic worldview that makes their films almost impossible to dislike.
 There is a redemptive message in Verdean's eventual downfall but it's mostly lost in a storyline that tries way too hard to be outlandish.
 Even with the likes of Amy Ryan and Will Forte providing capable backup, it all grows ancient, fast.
 The film employs and immediately squanders the talents of legitimate comedy all-stars Amy Ryan, Danny McBride, Jemaine Clement and Will Forte.
 There is little that is worse than satire that tiptoes around its subject, as if risking offense were a sin.
 Don Verdean is a pleasingly entertaining, sometimes hilarious, quietly satirical take on what makes us tick as a subculture.
 Parts of "Don Verdean" are executed with real imagination. The rest of it falls asleep.
 Don Verdean is the sort of comedy which presumes its own hilarity long before it gets around to telling any actual jokes, or staging anything that might otherwise be considered funny...
 I think I chuckled about three times ...
 A poor attempt to spoof evangelicals and their mindless, desperate followers.
 Don Verdean may only appeal to hardcore Hess fans-anyone else will likely never once crack even a smile.
 "Don Verdean" could be more sharp, more focused, and it's easy to be disappointed by the film's low ambitions.
 There are some scattered laughs, but considering its broad targets, the film is too detached from reality to consistently hit the mark.
 Should have been broader, should have been funnier. Did they lose their nerve?
 It has enough original (and strange) elements to feel like its own entity and stand out. [Full review in Spanish]
 The most beloved fighter in cinema does not stand anymore in the ring. [Full Review in Spanish]
 If Rocky was a K.O, Creed is a T.K.O.
 Sylvester Stallone reminds us why we fell in love with Rocky in the first place. [Full review in Spanish]
 A powerfull, emotional and well told story. [Full review in Spanish]
 The plot is pretty simple, but it has a good start and a clear end, also has moments that easily remind us to Rocky(1976). [Full Review in Spanish]
 While this film is basically Rocky VII, it's also much more than that, and perhaps the best in the series as it tells a standalone story with energy and skill.
 Coogler manages to make one of the best films in the series showing a Rocky full of courage and elegance. [Full Review in Spanish]
 In this revival, there is not only talent, but also affection and intelligence. [Full Review in Spanish]
 Creed reimagined a classic, while paying tribute to a character that remains after forty years. [Full Review in Spanish]
 Creed may not reunite generations like Star Wars did, but has fiber and muscle to keep fighting. [Full Review in Spanish]
 The most romantic story in sports films the industry has ever contributed to pop culture. [Full Review in Spanish]
 A powerful drama with great fight sequences and a superb camera work. [Full Review in Spanish]
 Stallone delivers a touching and wonderfully understated performance. 
 Creed is able to succeed as an exceptionally entertaining sports story, and the groundwork for building an identity free from the past.
 This is everything we could have hoped for from a Rocky spinoff and more. It's a story about legacy, race, purpose, trust, and friendship. 
 with Creed, the series really has returned to its roots... there's an earthiness and street quality that we haven't witnessed since the 1976 original.
 Director Ryan Coogler proves to be a filmmaker with a great capacity for narrative, and just like his character, he's trying to leave more than a good impression - a legacy. [Full Review in Spanish]
 An honest film the will thrill the thoughest and that can be enjoyed even by those who haven't seen the originals. [Full review in Spanish]
 Like a palooka charging from his corner, Creed plunges ahead with only a few sidesteps. (Director) Coogler takes his time with each development, endearing these already likable characters to the viewer even more.
 Panahi may make his points from inside a cab, but he's no hack. Just a farer of honesty and truth in a country where truth no longer has meaning. 
 Panahi has made a work of invention and brio that remains visually lively throughout, despite its formal restrictions."""]

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