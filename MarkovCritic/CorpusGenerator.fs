module CorpusGenerator

open System
open Newtonsoft

type MarkovPair = {
    Word : string;
    Frequency : double;
    Cumulative : double;
}

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

let getFreqTable (input_corpus : seq<string>) : Map<string, List<MarkovPair>> = 
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

let ughCorpus = seq[""" How did they get it so wrong How could they remake the beloved s action classic Point Break with such ineptitude and so little affection
It fully deserves to be played on a loop silently in extremesports goods shops everywhere
Whereas the original was filled with humour and gungho machismo this Fast and Furiousstyle update is a joyless experience
We have no emotional connection with these characters nor do we care whether they survive or not
Steer well clear of this flatly disappointing remake as it seems hellbent on stripping out everything that made the original movie so much fun
Pointless Heartbreaking
Its so unbelievably selfimportant with occasionally genuine and hazardous stunt work badly compromised by the annoying ponderousness of it all meaning that we wind up simply not caring whos risking their lives or who wants to get into whose pants
An adrenalinefuelled but empty rehash that flies close enough to Bigelows movie to remind you how much youd much rather be watching the original
Director Ericson Core has ditched the fun of the Patrick SwayzeKeanu Reeves original and replaced it with soulless action scenes
The stunts are impressive The rest is a wash out
This is a highgrade action thriller that  dare I suggest it  redeems the Point Break brand And yes theres definitely room for a sequel
Point Break More like Point Miss
Braceys emotionally scarred Johnny Utah is so wooden he makes Keanu look like Kenneth Branagh
Were left scratching our head wondering what it was all for
Core revels in the beauty of nature and the danger of taming it but showing such is meaningless if the plot is so weak it barely strings the destinations together
Relentless action cannot dispel the toxic levels of claptrap that stink up this pofaced slowwitted remake
This iteration of Point Break effectively serves as a blueprint of exactly how not to reboot an action film
A complete failure Who wouldve thought Full review in Spanish
Point Break aspires to be a decent remake but winds up being one of the most boring action films ever made
If you like to see people pushing themselves to the physical brink parts of the movie will absolutely hold your attention If only the plot was that compelling
Director Carpignano has discovered a genuine performer with charisma and confidence in Koudous Seihon
Both topically and dramatically this is a film that matters more than most when immigration has reached an explosive status with people fleeing misery being blamed for the Paris terrorist attack
The film is unwaveringly attentive to problematizing the dividing line between predator and prey
A piercing character study whose narrow view frustrates complete empathy
Theres a specificity to Mediterranea that at times makes it feel like an actual documentary
Too much directorial obfuscation keeps this powerful refugee tale from being as potent as it could be
This calm hardheaded film never sacrifices its toughness for a swooning mistyeyed moment of hope
Carpignano glosses over much of the sociopolitical context in his depictions of the chain of events
Offers a deliberately muted finely textured account of the ordeals many Africans endure both before and after voyages to Europe in search of better lives
Mediterranea does not solely owe its topicality to recent events like the tragic sinking of an immigrant boat bound for Italy off the coast of Libya that resulted in  deaths it will induce a shiver of recognition for American viewers too
If youre the sort to wonder what it is that drives people to leave their homes to find work in other countries or even other continents then think of the film as an educational experience
Sad story about the struggles of two brothers from Burkina Faso who journey to Italy in hopes of finding a better life
This is a story packed with the sort of heartbreak and worry that would be hard to sit through were it not for American director Jonas Carpignanos touching tender narrative skill
Its a slowburn study in feeling powerless and unwelcome anchored by Seihons performance as a man patient and adept at sussing out and adapting to what others need until he just cant do it any longer
This debut feature from director Jonas Carpignano is often both harrowing and moving though this film festival favoritefalls short of some of the similar and even more tragic stories that can be heard just by turning on the news
Though realized with great empathy and tact the film fails to convey the catastrophic extent of the situation it addresses by keeping its narrative too tightly focused
A timely humanistic immigration film
This is as accessible and as lovable as avantgarde filmmaking ever gets
Offers a loving and unvarnished look at one of Oklahomas more eccentric stars
A poetic exploration of a moment a place and an artist
Whiskery and restless grooving and grotesque the documentarian Les Blanks longsuppressed film A Poem Is a Naked Person plays like your memories of some mad stoned lastcentury summer
A Poem Is A Naked Person is littered with striking moments that fit casually into Blanks study of fame and aspiration
Les Blanks longlost Leon Russell documentary is a beguiling snapshot of a lost era
And this is A Poem is a Naked Persons accomplishment composing a symphonic harmony of seemingly dissociated activities and subjects into one comprehensive elegantly organized and yes even poetic whole
An inside look at the s Leon Russell that is not much of an inside look at all
A perfect example of the downhome informality Blank made famous
A Poem Is a Naked Person is a tuneful peculiarity capturing singersongwriter Leon Russell and his bandmates his friends guest musicians and Oklahoma eccentrics
In essence the real audience for this are Leon Russell aficionados of which there are fewer today than in
Russell embodies history of Southern rock with roots proudly showing  Cultural heritage as living breathing rocking and rolling inspiration is wonderfully preserved
In the s this wouldve been an unusually intimate tour portrait Now its a newly unearthed time capsule the remarkable clarity of Blanks portrait compounded by the distance from which were looking at it
I like Russells music but I learned absolutely nothing about him
A Poem is a Naked Person is a trippy timecapsule celebration of Russell and his music and of Oklahoma and its people
You begin to understand the eccentric characters and amazing weirdness of a now lost America that Blank was celebrating
Blank isnt afraid of the artistic wrath that he may incur and rather than making a Leon Russell film has made what is defiantly a Les Blank film full of all of the idiosyncrasies that one would expect
This film was never released in theaters  it was obviously way too far out and ragged for its own good But it works nicely as a warped time capsule harkening back to strange days
Shot while hanging out with Russell across two years Poem wriggles with weirdness and smells to high heaven of its s roots
Shot in the early s but shelved for  years this portrait of RB great Leon Russell  immediately takes its place among the best rock docs
Therein lies Berardinis critique using naught but his subjects own words he paints a portrait of men who have been dazzled by profits at the expense of their ethics
Though at times not for the fainthearted Tom Swift and His Electric Rifle takes subject matter many of us probably give little thought to at all in day to day life and asks us to consider it deeply
An important account of shifts in policing tactics and the problematic interaction between law enforcement and private companies
Footage of the nowwealthy Smiths being deposed is damning the brothers legal jiujitsu is appalling and the stories of deaths are heartbreaking
Relying entirely on flawed cases and weak lawsuits from the mids Killing Them Safely is a very poor indictment of TASER International the manufacturer of the widely used and mostly safe electronic weapon
A dark comedy about humanitys knack for quickie solutions and our ability to lie to ourselves especially when our noble deeds go awry
Berardinis doc vigorously proves that despite their marketing Tasers are not the answer They are only means to a bigger deadlier problem
Mr Berardinis packed documentary makes its case early and often perhaps too often but its more chilling than your average issue film
With the intensified focus on use of force in police departments the unsettling documentary Killing Them Safely couldnt be timelier
Killing Them Safely is above all an example of excellent ethical fair and balanced journalism allowing both sides to state their case
With adjusted expectations the movie plays with surprising depth inspecting the redemption of a ruined life with care and attention to thespian detail
Solid and dependable Forsaken shouldnt be forgotten But I wish so much of it didnt feel halfremembered
It has a solid story to tell and tells it with no winks and few if any frills Its involving and ultimately exciting
The movie is too visually lovely to forsake but the predictable story and basic plot holes remain unforgiven
Screenwriter Brad Mirman follows the Western template as if it were an exact science
Attempts to be a slow burn thriller building to a climactic shootout are more Back to the Future III than High Noon but smaller character moments are deeply genuinely felt by the actors
Its hard to be overtly opinionated regarding Forsaken because the plodding uneventful little movie never asks more of you than basic consciousness its like the cinematic equivalent of cleaning out dryer lint
True we have seen almost all the elements in Forsaken in a hundred Westerns of the past But theyre assembled with such care that theres room for version
conveys a gritty style and features solid performances but most of the characters and themes seem recycled
So generic are these tribulations and story beats that they could have entered the realm of selfaware camp if they werent played so straight
Oldschool western fans wont find a lot of originality here but if youre looking for a wellexecuted straight genre exercise give it a shot
Forsaken greatly benefits from the poignant teaming of its fatherandson stars  as well as Michael Wincott as an especially elegant and eloquent gunfighter who has great respect for John
Though it borrows liberally from the classic Shane and doesnt really offer anything new this lowkey Western still works thanks to patient storytelling and a batch of strong performances
Father and son team up for a Western in the classic style
In the end there is nothing horribly wrong with Forsaken but there isnt all that much right about it either
Forsaken offers sufficient gun battles standoffs and shothimdead moments to keep fans of superficial Westerns entertained but its revenge story is unfalteringly bythebooks
Because of its intriguing emphasis on the strained fatherson relationship Forsaken ends up being a compelling drama as well as a wonderful addition to the Western genre
Forsaken is still a film to be enjoyed The performances themselves justify that
It should please those who like their westerns the oldfashioned way
In a standard coweringtownneedsagunfighter drama typical themes redemption forgiveness are laid out with little imagination
Exposed is a mess and completely forgettable but perhaps its more interesting as a lost movie with its true form caught between the demands of its financier and the vision of its original creator
In certain mutilated pictures you can detect the lineaments of greatness Consider Orson Welless The Magnificent Ambersons Here thats not the case
Exposed is confusing and very difficult to follow It ends up being a downer mess  despite interesting turns by Keanu Reeves and Ana de Armas
Awfully silly  and just plain awful
The confused heeldragging mystery drama Exposed suggests an especially dour arty episode of Law  Order SVU minus any reasons to keep watching
Cliched thriller mixes cop story supernatural elements
feels like two disparate ideas scrambled together in a way thats never convincing
Has a grimy offkilter charm not seen since the heyday of s exploitation
What a strange and frustrating mess this is
Exposed is a film suffering from blunt force trauma to the head
With Reeves in energysaving mode the troubled cop and corruption routines simmer but never come to the boil
Reeves appears to only have two expressions sad face and needapee face
What is being released is a baffling hodgepodge of a movie full of contradictory elements Bits of it seen in isolation are effective and atmospheric but the plotting is tangled and confused
Its clearly intended as a magicalrealist take on very genuine social conflicts but the result in this edit at least is a trial to sit through
To call it disjointed is an understatement Exposed is unintelligible It feels like two completely different movies inelegantly Frankensteined together
Incomprehensibly disjointed and stunningly dull
A grimfaced Keanu Reeves looks bewildered throughout Exposed and you can only sympathise
Carries a complex convoluted narrative that attempts to cover too much ground balancing a myriad of themes
Its fascinating to watch  not once was I bored  and superior to most other offerings on the big screen at the moment
It has all the Tarantinos characteristics and thats the reason this is a must for the filmmaker fans Full Review in Spanish
Theres politically incorrect humor and violence as well as an attractive cast but its too long to be perfect Full Review in Spanish
The Hateful Eight is a confirmation of an exaggerated and fuzzy Tarantino so determined to surprise us with his wit and shock us with his audacity and ends up sabotaging his interesting argument with excessive characters Full Review in Spanish
Tarantino brings a very bold political film disguised as a western that seems deep but at the same time can be enjoyed without any problems Full review in Spanish
Even though I dont think this is Tarantinos best film the director delivers and satisfies his fans Full Review in Spanish
Tarantino is a slicker and just cares about giving The Hateful Eight an air of quality Full review in Spanish
All these great actors the majestic landscapes and picturesque and mysterious characters wasted A Tarantinos mistake Full review in Spanish
The Hateful Eight doesnt try to make a point about race or the status quo in America today it doesnt even try to do it Its just interested in telling us a good violent and emotional story Full review in Spanish
As he becomes even wiser an orchestrator of scenarios characters and conclusions Tarantinos work turns more vicious in its timebomblike destruction
While not a quantum leap forward in his directing The Hateful Eight finds a new corner for the filmmaker to explore as both a writer and a director
Scenes do not gel into a whole nor do characters compel suspense is absent and early parody and irony have given way to outandout torture porn
The Hateful Eight is easily Tarantinos most fierce and contemporary film to date
The Hateful Eight sees Tarantino stake a claim as his own most passionate auteurist the film is a melting chamberpot of selfreferentiality and recreation
Its lenght and lack of that characteristical Tarantino shine of his more famous body of work make a rewatch of this film a necessary to develop a taste for it Full Review in Spanish
Its good and bad trashy and brilliant flimsy and substantial all at the same time And as for Tarantino Hes maddening frustrating totally unique
Bloated and hilariously selfindulgent
Tarantino is making his points and although its too long it is a spectacle to enjoy a mix between The Canterbury Tales Cluedo and spaghetti westerns
Just when you think you know what Quentin Tarantino will do next you realize just how wrong you are as evidenced by this great addition to the directors oeuvre
Its not exactly a culmination of Tarantinos work to date but it echoes themes and storytelling ideas weve seen him play with since Reservoir Dogs
The expose of brain damage risks to footballers is powerful stuff but the film never flies due to a messy screenplay that loses its focus and awkwardly interweaves an immigrant love story into the mix
I am a sucker for cause films that champion right against might but even allowing for that bias I can recommend Concussion as engaging cinematic grit
Its become all too easy for heavyweights like the NFL to forget the essence of sport win or lose its just a game but one which can physically and mentally scar the real people who play them
Feels like an outline spun by an ambitious Hollywood executive rather than anything resembling real life
Tthe events depicted are fascinating but feel inconclusive so its easy to leave feeling a little unfulfilled at the close of play
Relying in no small part on Smiths reliably firstrate performance Concussion is a thoughtful and interesting story neglectfully executed
The science shines brightest in this confronting true story that brings shocking new meaning to the phrase contact sport
Concussion takes a serious subject  brain damage caused to professional American football players and the collusion of the medical establishment in covering it up  and renders it in fullbore crashingbore Oscarcourting prestige drama mode
Viewers and not just retired footballers can all too easily forget what the movies even about
Smith still does a fine job of communicating the stubborn resilience of Omalu an admirably unapologetic whistleblower so committed to the CTE cause he paid for his lifesaving research out of his own pocket
In terms of leading some discourse about the safety of Americas most popular sport at least Concussion is in the stadium But Concussion doesnt really enter the game
Omalus dedication to his profession and his struggle to make himself heard and understood make for an inspirational story and Landesman effectively lays out the facts and invites the audience to judge both Omalu and his opponents
Smiths quietly tough performance gives Concussion its heart
The NFL may well feel gratified that the movie is as vague and weak as it is
Will Smiths performance is solid but Concussions plodding procedural nature turns the drama into a bit of a dogooder dirge
The film is worth for two reasons its theme and its main actor Full Review in Spanish
To see Will Smith seriously acting is refreshing he should try it more often Full Review in Spanish
The script also knows how to pull back when sentimentality threatens The films salutary ending shows just how its done
Told in fits and starts the film does convey a fair bit of information but it never clarifies what needs to be done about the continuing danger
Im not sure I would ever forbid a son of mine to play football But I would definitely insist he see Concussion before strapping on those pads
A fascinating and entertaining attempt to answer baseballs Big Question  Who was the fastest ever
Stands as further testament to baseballs status as our most chesslike sport and one that even when broken down to its tiniest component parts never loses its magic
For baseball fans it delivers the high heat For the nonfan there may be a little too much inside baseball
Fastball will change the way you watch the game without ever diminishing the sports mystery and grandeur
Theres nothing here that wouldnt have fit comfortably into an hourlong TV special and it starts to drag after a while
The irony of the push to speed up the game of baseball  the amount of time between pitches between commercial breaks between anything that might actually appeal to the iGeneration  is that speed has always been at the heart of the game
Filmmaker Jonathan Hock goes beyond anecdotes to the larger role of the fastball in baseball the way the game changed when the pursuit of velocities exceeding a batters reaction time became a Holy Grail
This appealing documentary makes you understand why aficionados regard baseball as a form of poetry
With Kevin Costner narrating Hock illuminates and entertains as he sketches portraits of the greats from original fireballer Walter Johnson to the indomitable Nolan Ryan
You dont have to be a baseball fanatic or for that matter a historian or a physicist to appreciate Fastball
Hocks documentary has a thrilling pop that should help it strike a competitive chord with anyone even remotely enchanted by our national pastime
Suitably solemn yet irresistibly lively documentary about the myth lore and logistics of baseballs classic pitch
The lively MLBproduced docs choice archival material and full roster of new interviews will delight fans both as a warmup to the  season and beyond
The sunniness of Fastball leaves out a lot but watching it can be as pleasurable as an afternoon at the ballpark
A fascinating and downright lovable documentary feature by Jonathan Hock
If you are not familiar with this level of baseball nerdiness then Fastball will be a revelation and hopefully an entertaining one If you are familiar then Fastball will satisfy on a deep and extremely specific level
One of the worst films to play Midnight Madness in ages
Baskin is Turkeys answer to a Rob Zombie movie which is meant as a compliment in this case
Take it or leave it but I was very happy to have partaken So I suspect will more than a few other pervy geeks
horrorhounds will relish the orgy of the damned that unfolds
surreal uncompromising bestial and eerily beautiful even if it is not despite what some unsuspecting viewers may believe named for a certain popular brand of ice cream Evrenols film sure is one haunting Turkish delight
Baskin excels beyond its vile visceral aesthetic into a puzzle of keyholes doors and fate
Visually Baskin is like a twisted sibling of Fulci and Argento by way of Cronenberg and Winding Refn
Baskin is the classic example of a film that fails because it cant support its carnage Im all for extreme blood and guts but it needs to have purpose
Baskin offers little in the way of narrative involvement or scares but doesnt stint on sustained stylized revulsion
Its initial promise dissipates in a muddle of repetitious phantasmagoria and too little narrative or character development
Baffles and bedazzles before blistering into a masterpiece
The film mostly functions as a tour of familiar horror tropes for much of its running time
Evrenol shoots all of this artfully imbuing everything from a police van broken down by the side of a road to a man having his intestines slowly pulled out with a certain eerie beauty
Its a testament to the power of Baskins imagery and atmosphere and the solid performances then that the shortcomings fall by the wayside for long stretches as we absorb this freaky world in which the protagonists find themselves
Tickling the mind even as it lurches the gut Baskin a stylish shapeshifting horror film from Turkey pulls a baitandswitch
Baskin is a perfectly imbalanced mix of chilly atmosphere heavyhanded symbolism and familiar horrormovie tropes
A torturegore blowout that rises above pure nausea with an intriguing blur of possible realities
The pacing is slack and the splatter excessive but this twisted crossgenre exercise should be red meat to gorehounds
The filmmakers handle their material efficiently but its hard to imagine anyone familiar with the genre finding Bleed fresh or as vividly scary as its predecessors
Bleed is familiar bythenumbers horror filmmaking but its also short and sweet enough to capitalize on cheap thrills
Too derivative and not stylish enough to merit any special championing as indie Bhorror movies go its nonetheless nicely crafted enough to rate a cut above the lowexpectation median
Reflecting influences ranging from The Texas Chainsaw Massacre to Rosemarys Baby to  well you name it  Bleed doesnt exactly break any new ground stylistically or otherwise
Bleed is really nothing you havent seen before but theres still some pretty interesting elements that result from its mixtape approach
A comedy thats neither clever enough nor sufficiently over the top
Despite its flagrant attempts to mimic what works in the comedies by Seth Rogen and Evan Goldberg its never particularly funny
The problem is that a movie like this  limp lazy generic  just doesnt cut it at a time when American indies and television offer sharp witty satisfyingly complex takes on the Facebook generation
Shot like a typically ugly Adam Sandler effort and so clumsily stitched together that it feels as if large plot chunks litter the editingroom floor
Not only is this ensemble unemployment comedy labored and witless but it bears the unmistakable scars of a protracted and ultimately failed struggle in the editing room
Its hard work finding work these days is it not Not according to Get a Job
At its sloppy heart this is meant to be an affirming movie but the filmmakers could have taken a cue from one line of dialogue Dont just feel special Be special
Clearly the economy has given Get a Job a reason to be sour But theres no excuse for being so sexist
The imperfect work mired in storage all this time gets a welldeserved spin
Get a Job is nothing special
Not only is this a movie without any guts it doesnt have much of a brain either
Predictable workplace comedy has drugs sex swearing
After a viewing its obvious why the producers lost interest in releasing it
Get A Job points to either massive studio compromise or a filmmaker who has somehow lost the mastery of his onceauspicious occupation
Get A Jobs primary problem is that it doesnt know if it wants to be a realistic look at millennials and the current economy or go for the cheap gag about the jivetalking pimp renting out a sleazy motel
This longshelved comedy proves a disappointing mix of onscreen talent uneven social satire and juvenile humor
What makes Get A Job so infuriatingly bad rather than the kind of film you hate and then completely forget about is the allstar cast that it has at its disposal and disgracefully wastes
A brutally cynical largely unfunny film fueled by muddled social commentary
Writerdirectors Micah Wright and Jay Lender are kidscartoon vets and show a facility for comedy on a more human level here  as does the nimble cast which ably handles the tonal shift from travel nightmare to actual nightmare
Is the film savaging cable TV Ethically challenged filmmakers Foundfootage horror itself If the intent was to finish off this subgenre by exposing the rote mechanics behind it the mission is only halfway accomplished
Wait you mean you wont be begging for Theyre Watchings cast to be sliced and diced How  refreshing
What we have here is a horror movie with all the expected trimmings Rumors of witchcraft An isolated house way out in the middle of spooky woods where no one can hear you  well you know how that goes
The storytelling becomes muddled in the middle and the suspense doesnt build as well as it ought to but the winking undercurrent keeps the film watchable
Viewers looking for the minimal amount of horror in their horror comedies will be appeased with Theyre Watching but the focus here is on laughs not chills  and that wont work for audiences seeking an equal balance between genres
Here comes one weird crackedout trippy horror tale that kicks into high screams at the minute mark
The movie doesnt do justice to a promising premise
Tired jokes and uninspired gore abound in what amounts to an unbearable experience and thats before the climactic bloodbath has a chance to disappoint with its subamateur special effects
Heres a minute foundfootage horror movie that would have worked better as an minute foundfootage Funny or Die comedy sketch
An hour and fifteen minutes of tedium is too long to wait for two or three minutes of pleasantly cheesy
A contemporary Blair Witch Project knockoff that feels significantly more dated than the film its inspired by
It makes some progress but it unsurprisingly stalls where so many found footage films have failed before
Neither very plausible nor scary this foundfootage exercise is nonetheless entertaining enough for a spell
So much better than last years plodding aimless Part
Part  is supposed to be the emotional crescendo to an epic love triangle but due a complete lack of chemistry between Lawrence and her callow male costars its all hollow and the goldembossed Super Happy Ending feels fraudulent DB
Affirms Katniss Everdeens status as the most significant of the reluctant teen heroes who have battled adult tyranny in the fantasy movie series of the past few decades and no Im not forgetting Luke Skywalker or Harry Potter
action and theme dont always cohere and whats good in the film too often has to be dug out from under a lot of underwhelming excess
Well the Hunger Games finally grew up at the end after all that silliness about the earthshattering importance of fashion statements We finally got one convincing romance to go with a real battle and real politics
This  minute conclusion results heavy and drags considerably due to its lack of what it promised Action emotion unrestrained rage Full Review in Spanish
It reminds us that every happy ending can also be a tragedy Full review in Spanish
While theres lots to recommend the movie the silly premise is the only thing I could focus on for the entire  hours and  minutes  which is inexcusably long for a movie that is actually just half of a movie
Relentlessly bleak from the first frame this final outing pulls no punches as white knuckle action sequences see the casualty rate rise alongside the tension
the last hour of Hunger Games  is good enough that deeper readings of this text would actually hold water in a way that previous attempts to politicize it have failed
Part  feels like it has a sense of direction that was lost after the first movie and its enjoyably more actionheavy
Its frustrating to watch the Jennifer Lawrence be marched from plot point to plot point just getting things over with
I understand that YA books succeed by transposing real teen concerns into lifeanddeath fantasy situations but any connection to the characters that existed by the end of s Catching Fire has been stripped away by these plodding Mockingjay movies
I remember struggling with Suzanne Collins final book It didnt have the same sense of urgency and excitement My thoughts on this movie are essentially the same
In this final film fans will get a satisfying resolution Its an emotional and dark journey illuminated by an otherworldly performance from Jennifer Lawrence
A satisfying conclusion to a brilliant series of films that show us the dangers of the oligarchy
Kudos to Lawrence for helping bring a satisfactory conclusion
The rare blockbuster that finds a compelling middle ground between thoughtfulness and big splashy spectacle
A fittingly serviceable end to a series that always had more potential than impact
provides a satisfying end to the story rather than an inspiring one It preserves the dark turn of the books finale the character of Katniss and the journey of Peeta to recover the compassion destroyed by Snow
First with the telephone then early cinema the magic of wireless radio and finally television Dreams Rewired bombards the senses with a thorough and clever montage of found footage from the s to the prewar era
The documentary isnt advancing an argument so much as simply restating a European socialistic breed of fact
An extended look sans nostalgia at how we used to envision ourselves and our future  and at how those of us alive now at what seems the apex of communication technology will look to everyone watching us in the future
Compilation of silent and sound films attempts to address fears about technology but proves excruciating instead
A lively visually enthralling attempt to gaze into the future by remembering the past
The ethereal essay provides a bounty of poetry in the form of a measured narration by international treasure Tilda Swinton and an extensively labored assembly of  blackandwhite film clips
The film really serves as a tribute to the then newfangled phenomenon of moving pictures  Its most persuasive and unspoken revelation is that filmmaking evolved concurrently in the Old World as it did in the United States
Its a feast for fans of early film but alas the narration is inescapable
As a documentary Dreams Rewired spends a long time circling various points without ever landing on one
The entertaining Tilda Swinton narrates from a highfalutin academic script but its the images that provide the fascination
Science fiction becomes reality in this funny and disturbing collection of filmed technohistory
Dreams Rewired isnt in the business of recovery or even analysis Instead it gestures it implies it signifies
Dreams Rewired is scattered by necessity and intent and it throws off enough sparks to set your brain reeling
The effect is more somnambulistic than stimulating and eventually youre less concerned about Big Brother peering into your home than you are about getting Tilda Swinton out of your head
Solid messages and mild scares are fun more toys for sale
a series of predictable and pedestrian scenarios including a morbidly obvious comparison between the two gentlemens organs of generation that will color your thinking about Patrick Stewart forever
While the pair of them look like they might just perhaps be able to make this highly improbable and dubiously conservative nonsense vaguely amusing they dont
The movie wont earn Ferrell any new fans but its a safe bet if you like his type of humor Full review in Spanish
Watch if you enjoy watching talented people waste their time and also enjoy wasting yours
Thats one problematic comedy
An odd mix of sentimental family warmth and grossout antics this comedy doesnt have the courage of its own convictions which means that its not quite funny enough to keep the audience fully entertained
The second pairing of Will Ferrell and Mark Wahlberg isnt as funny as their first offering The Other Guys but its still funny enough to warrant a look
It has its share of laughs even though they may not be memorable ones
What keeps Daddys Home watchable is Wahlbergs checkmate machismo as the intimidating foil necessary for Ferrells nambypambyism to register Its like watching Andy Sambergs SNL impersonation of tough guy Mark Wahlberg a selfparody of a spoof
Slapstick comedy that really doesnt do anything new for the genre Full review in Spanish
Predictably stupid and hopelessly obnoxious
There is no doubt that Will Ferrell and Mark Wahlberg connect on screen See them interact together has its charm but not enough to make us burst into laughter Full review in Spanish
The usual formula is used to make this a generic but fun comedy Full review in Spanish
Daddys Home insists on staying in common ground and to make matters worse his argument is unable to build paths that culminate in moments of laughter Full review in Spanish
Sadly not even the great performances cant save its awful script Full review in Spanish
Whats the target audience Its really innapropriate for a family film Full review in Spanish
Daddys Home manages to succeed thanks to Will Ferrell experienced comedian that once again stays in his comfort zone to do what he does best amuse others with his characteristic jokes comments and gestures Full Review in Spanish
Somewhere hidden in this film there is a good comedy Full Review in Spanish
This movie feel extremely long and repetitive despite its hourandahalf lenght something deeply worrying given that comedies are supposed to make you laugh which Daddys home fails at Full Review in Spanish
Its symptomatic of the productions limpness that the movie was shot for taxcredit reasons in New Orleans yet the locations have been scrubbed to a funkfree suburban anonymity
James White gets up close and personal in often discomfiting ways but its never exploitative or glib It hits the highs and the rock bottoms and all the damnable stuff in between
Filmmaker Josh Mond is as interested in his heros valleys as hiswell not peaks but the moments when hes less agitated even peaceful
Abbott contributes a smart soulful performance but Nixon keeps threatening to walk away with the movie as the mother who cant get enough of life and whose physical decay is colored by rage defiance and terror
Both Nixon and Mond invest James White with a raw honesty that makes James White a compelling drama about the demands and rewards of family
An accomplished and compelling film by writerdirector Josh Mond James White is also pretty much a bummer
Wisely James White leaves the unresolved troubles and feelings of its eponymous character unresolved
The acting is good in this film but watching it is like a depressing exhausting journey to nowhere
Its Abbott whos the revelation showing off all sorts of previously unseen leadingman potential
Veteran producer Josh Mond makes his featuredirecting debut here It elides as much as it shows but his sketches are mostly deft and always deeply physical
The experience of watching James White is like being shut up in a small dark airless room  a sickroom
Abbotts performance is serious and committed James White is constantly fuming in an interior dispute with the rest of the world
Most films dealing with illness and carer relationships follow the same emotional paths and miss the same points This film by contrast feels fresh and real even to those of us who have been there
Whether you want to spend time with James White depends on your tolerance for yet another film about how hard it is for guys who just feel too much
James White can be a chore to sit through but its never completely without merit
The movie is so engaging there are times when it becomes almost overwhelming But that is a glowing sign of how well the movie is acted and put together
Death does not become him
A lowkey but devastating drama with more raw authenticity than a hundred examples of Sundance landfill
This courageous film gets right up in the face of suffering and it doesnt flinch
Its not easy viewing by any means But it is strangely refreshing for a movie to show us that terminal illness involves agony and vomit and terror despite what Beaches might have told us
One will never wish to go through precisely what White does but losing ones parents is inevitable and the film is a searingly authentic portrait of the process
Writerdirector William Riead offers a highly simplified version of his subjects life
If Mother Teresa were here to see the film she would probably say You made this piece of garbage about me
This Mother Teresa biopic offers Hallmark Channelgrade inspiration of the most sluggish sort
A biopic about Mother Teresa could have easily been a selfimportant slog yet William Rieads The Letters proves a stirring and absorbing if not quite definitive drama
Struggles arent ignored here theyre just surmounted with patience and devotion That may be a good strategy in life but it can be static to watch on screen
Her accomplishments are even more impressive once we learn how fiercely she wrestled with God
Even Mother Teresas harshest detractors might say she deserves a better biopic than The Letters
A drama in which belief is reduced to wellmeaning but inert treacle
Bound to disappoint especially considering the absolutely wasted and bungled potential Quite possibly the most boring and dramatically inert movie youll see all year Full Content Review for Parents  Violence etc  also available
Underwhelming biopic about the extraordinary Mother Teresa
Her work made her one of the most celebrated figures of the th century but The Letters is far too preoccupied with the bureaucratic minutiae of her journey
Good intentions alone cant salvage this aggressively heavyhanded Mother Teresa biopic that perhaps could use some divine filmmaking intervention
This is a revealing study of a woman who was one thing to the world but something completely different to herself with a fine performance by Juliet Stevenson
Teresa is simply portrayed as a dedicated servant of God while whatever internal struggle she dealt with remains told not shown
Ms Stevenson is effective and credible in the role And The Letters is worth viewing for people of all faiths
The Letters is a beautiful and deeply moving tribute to the selfsacrificing Mother Teresa
A wartsandall biopic which reveals Mother Teresa as a tortured soul who felt abandoned by the same God she served so selflessly
The movies writing direction and acting are not alone in blowing Ciaran Hopes overwrought score sounds like a mashup of music from hokey old biblical pictures If a duller less inspired film hits the cineplex this season it will be a miracle
Its refreshing to encounter the occasional movie character who is blessedly devoid of guile For that matter its also a relief to attend a film where were not expected to hiss and throw our popcorn at the screen the moment a Catholic priest appears
A tale of two Mother Teresas On the brink of Vatican sainthood just announced holy healer or hells angel Expect an essentially nuancefree infomercial embrace of the former despite ironic inklings of nun feminism challenging male church authority
Not without its heated confrontations but it feels unnecessary working to depict the downfall of a man whos beaten them to the punch in terms of addressing his own selfdestructive tendencies
While perhaps not as deep as it could have been the movie is nonetheless a compelling examination of an unrepentant cheater
Despite a committed lead performance from Ben Foster Frears drama is an obvious and frustrating depiction of ambition and obsession
The Program feels ultimately more akin to boxticking than characterpowered drama
I had a hard time buying so much concentrated bile and for me the film loses an objectivity that might have otherwise scored points for the destructive nature of competition that can wreck a decent mans corrupted psyche
There is nothing remotely likable or even relatable about Lance Armstrong in The Program
Were left with the wellacted and welltold if familiar story of a man who knew better but couldnt stop himself
The Program is a highly watchable very enjoyable film but never truly manages to get to the heart of the issue
Frears squeezes tension out of these interstitial moments letting personalities rather than facts collide
Foster nails Armstrong right down to his final clenchedjaw TV confession to Oprah Winfrey but The Programs glancing narrative feels less secure
The Program much to its detriment concentrates almost exclusively on the history of the doping effort  There is no mention of his childhood or adolescence or any attempt to analyze his character
The most surprising thing about The Programis how narratively pedestrian it isfeels like territory thats already been welltrod or ridden
It never goes deep on what it was that produced the awfulness that is Lance Armstrong
The Program fails to add anything new or penetrate the soul of so crafty a cheater
For a story about incredibly focused determination  and if nothing else Lance Armstrong had that  the film remains strangely uninvolved
The Program reveals the tiniest details about Lance Armstrongs doping regimen Just dont expect to learn as much about the cyclist himself
The flashily photographed enterprise too often becomes a blur of sound bites and slick aesthetics akin to a Nike commercial
The Program is a solid primer to this fascinating riseandfall saga  one offering farreaching implications about corruption in sports and celebrity culture
Much like Frears other films The Program shines best in its performances Ben Foster disappears into his role as Armstrong
Flat Lance Armstrong biopic has drugs strong language
Toby and co come across as a pale antiintellectual imitation of the collegeaged friends in Donna Tartts The Secret History  a novel that explores the roots of its characters moral recklessness rather than just chalking it up to teenage feelings
Toby is so unselfaware that his journey seems like mere obtuseness what the film has to say about youthful degeneracy is less than zero
Nearly everything about The Preppie Connection  from the highschool class war to the flat explanatory narration  has been cribbed from other better films
This film fails even to evoke the s in costumes soundtrack or other atmospherics
A shallow and profoundly unexciting truelife account of one students brief reign as a campus drug lord
As written Toby is somewhat of an empty slate and Mann doesnt do much to fill in the blanks
A dramatized reallife scandal of s prepschool drug dealing plays like a tepid compilation of fictive cliches in The Preppie Connection
By the time quotes from Joseph Goebbels are used to explain Foxs methods The Brainwashing of My Dad has resorted to the tactics of its targets
A timely and thoughtprovoking portrait of the soulscape of America with its rage lack of civility and rightwing media machinations
Points fingers in all the right directions but fails to dig deep enough
Although Senko ultimately overplays Hillary Clintons vast rightwing conspiracy card her film with its timely Trumpian reverberations nevertheless serves up some compelling food for thought
Rightwing outlets may be exploiting humanitys ugly side but Ms Senkos fraillooking father who died in January at  isnt so much the face of the phenomenon as he is a small and not especially representative sample of it
Jen Senkos workmanlike Kickstarterfunded documentary examines American medias propagandaled phenomenon of extreme rightwing bigotry with a finetooth comb
Through anecdotal and social science research Senkos film also provides muchneeded insight as to why Donald Trumps caustic discourse and demagoguery is catnip for so many people
An entertaining look at why Fox News is setting the agenda for what passes for journalism in the US and a tool perhaps for deprogramming its adherents
A thoughtprovoking examination of the medias effect on Boobus Americana
Impressively tracks a family phenomenon via media mind controlled dittoheads brainwashed staged interventions angry white men and drive time as the geography of destiny But lacks further scrutiny into shadow corporate complicity and a tanked economy
Maybe I could say that if Christopher Guest rounded up his usual troupe and did a verbatim shotforshot remake it would be one of the funniest films of the year
In addition to an overreliance on Skype and silly effects such as laugh tracks  the films examination feels shallow
Paula Pells script is like a distended Saturday Night Live skit overworking one lame joke what if a bunch of fortysomethings threw the kind of party you only see in movies about hormonal teenagers
Fresh funny and heartfelt Sisters makes up for what it lacks in plot with a rolling succession of tearinducing jokes and a gaggle of hardpartying characters that you would actually want to buy a drink for
Theres a darker undertone to the story that is touched upon but not given the gravitas it requires
More wit would be welcome but Fey and Poehler keep things humming
Sisters is an uneven mix of raunchy farce and sincerity thats elevated by the undeniable chemistry between Tina Fey and Amy Poehler
Sisters is the FeyPoehler featurelength teamup many have been waiting for
Though Sisters does briefly achieve the right levels of relentlessly reckless humour at the height of the aforementioned party there are protracted spells either side of the main event where even a mild chuckle is hard to find
Its a bit like Animal House for girls but not as freewheeling
Is this the collabo between our brightest savviest comedy players that weve been dreaming of Not quite Its easy to want more from Sisters if only because Fey and Poehler have already given us so much
Sisters is a knucklehead comedy  sporadically amusing and always happy to resort to dick jokes  saved a little by the appeal of its leads
In what can be taken as an extended and belated apology for their woeful  comedy Baby Mama funny femmes Tina Fey  Rock and Amy Poehler Parks and Recreation team up as sisters for another girlsbehavingbadly slapstick comedy
Tina Fey and Amy Poehler are an unbeatable comedy team That doesnt make them infallible
Theres no shortage of comedic talent on screen yet Sisters plays a bit like a raunchy stand up routine loosely stitched together by a narrative that begs for more than crass outtoshock verbal torrent
Okay there are some solid oneliners including two  count em  tampon jokes and Maya Rudolph is reliably excellent as an uppity spoilsport Brinda But the best gags are few and far between and the movie starts to lag like a lengthy sitcom
While it wont be immortalised in the enduring annals of cinema it did trigger some unexpectedly visceral reactions from me
One of the most disappointing films Ive seen in a while
Agent One Hey we just signed Tina Fey and Amy Poehler to a movie about sisters who visit their childhood home only to find that their parents have sold it out from under them Agent Two Sounds great Whos doing the script Agent One Script
You feel like youre at that party that refuses to die and all the really interesting people are on the other side of the room doing funny things you strain to overhear
Even when Paula Pells screenplay falters the likeability of the central pairing is more than enough to carry the picture
This movie would have been better with a heavier focus on just these two rather than bringing in their SNL entourage
Despite the at times amateurish acting the film is in the end surprisingly touching and a reminder of the fragility of love
Nos film may not lack squelchy spectacle but when it comes to anything deeper it is oddly anticlimactic
After a slow start Nos selfreferential soap proves moreish
Everything is muted in Love When not shooting raunchy sex scenes Nos preferred framing is the mediumcloseup He keeps his camera near his characters but seems unable to elicit the true emotional essence of sexuality he claims to court
Its bold fleshy and audacious at least in theory But it is also numb
Despite being interesting its inconsistent and is both a demonstration of the courage of its director as well as its shortcomings Full Review in Spanish
A great movie but deffinately not for those who get offended easily Full review in Spanish
For a woman that know nothing about whats going on inside a guys head this can be the perfect guide to understanding the opposite sex Full review in Spanish
The blunt eroticism is going backwards always structurally and narratively backwards as in all films of No Full reviw in Spanish
Gaspar No took the fun in sex Full review in Spanish
Is interesting that Gaspar No still in the search to make audiences feel uncomfortable but curiously Love is may be one of his least transgressive works Full Review in Spanish
In theaters it has found rather indifferent response Full review in Spanish
You can watch Love in D but it wont add any dimensions to the characters or this script
An interesting way to talk about the different aspects of being in a relationship including sexuality Full review in Spanish
One of those films you have to see to believe but only for those not easily offended Full review in Spanish
Exciting sexy moving and painful Full review in Spanish
The problem here isnt really with the porn but with everything else which features the weakest dramaturgy this side of Deep Throat
The production values may be appreciably higher than your average porno but in terms of the quality of its plot dialogue and performances Love is not that far off
Karl Glusman has a future in Hollywood
Although masterfully acted by its three main actors the only new offering in this film responds to the natural need of Noe to cause a shock in the audience Full Review in Spanish
An informative and infectiously danceable history Latin pop history lesson
A joyous mix of social history and musicology
This is a documentary not just for the latin community or the Bronx community but for lovers of music artists of all kinds and folks who gain some sense of release from music they love
Slick and mildly provocative but overlong with excessive expository information Steve Jobs makes for a very captivating subject
The more interesting question Gibney poses and poses well is why we need to create a hero of someone who no doubt changed the world but who was more iconoclast that saint
If youre expecting solid answers youll be severely disappointed Ditto if you come expecting a hagiography In fact Im betting worshipers at the church of Jobs will be livid
For those who dont yet know of Jobs dark side Gibneys documentary will be a useful eyeopener but those looking to understand what made Jobs great in almost equal proportion to his nastiness will remain in the dark
Steve Jobs The Man In The Machine is a conversation starter loaded with controversial data about the lesserknown life of geniustyrant Steve Jobs but it asks a simple question of why that it cant even answer itself
Alex Gibneys twisty engrossing documentary Steve Jobs The Man in the Machine approaches its subject from an oblique but highly productive angle
While too long and with little new light on Jobs failings as a human being the questions it raises about his cult status and the way he helped change the world make it worthwhile
An attempt to understand the cult around a figure thats virtuous and revolting at the same time Full review in Spanish
Despite the movies journalistic substance the pleasurefree banality of its style gets in the way of a view of Jobs himself whose work is as much aesthetic as it is industrial
Coolly performs vivisection on a man a company an industry and a way of life
It simply isnt that easy to write off the Jobs extraordinary drive to succeed at all costs The difference between Gibneys documentary and other posthumous profiles of Jobs is that it doesnt necessarily maintain a reverential tone toward its subject
A thoughtprovoking and ethicallycharged documentary about the Apple computer entrepreneur
If GIbneys questions are not precisely yours his premise  that documentaries must ask questions rather than presume answers  is bracing reimagining not only what documentaries can do but also what belief can do
The picture is made with the consummate skill of the predominant documentarian of our age getting as close as conceivably possible to the essence of Jobs
This isnt a love letter or a takedown its a procedural justthefacts biography of a man who made a big impact on the world
Thoughtprovoking documentary on the iconic legendary Apple cofounder
Gibney doesnt offer a big final Sorkinstyle statement Instead he grapples with his own ambivalence about Jobs and his enormous social and technological legacy
Whether or not you care about Apple products or Steve Jobs Gibneys documentary offers a story well worth watching
A chilling portrait of an icon who remains revered for spearheading so many technological innovations despite his general contempt for humanity and his utter lack of people skills
Gibney is never able to join or understand the choir of millions singing the praises of Steve Jobs Perhaps because of this the documentary he has created seems a lot closer to the truth than anything else Ive seen about Jobs
Right from the start this involving documentary asks much of its audience and poses questions that are unnerving yet engrossing
What Our Fathers Did A Nazi Legacy wields a power that towers above many other small movies It may not be the large definition of cinematic but it is still a true film
the sensation of seeing men responsible for so much death relaxing with their families is unsettling while at the same time highlighting some small vestige of the humanity of both fathers
A troubling study of denial wartime responsibility and the challenge of dealing with a monster in the family
The men meet visit historical sites with Evans begin to battle I like you but I dont like your brains and the thoughts in your brains The contrast is bracing
Most troublingly images of Nazis in modern Ukraine in What Our Fathers Did A Nazi Legacy suggests that not being able to acknowledge that history paves the way for it to happen again
What starts out as a genial documentary about two sons of high officials of the Nazi Party soon turns chilling in the gripping and compelling What Our Fathers Did A Nazi Legacy
The film is not just about a very specific and difficult conversation Ultimately it is also about the failure of conversation itself
How do you cope with knowing your father was an architect of evil In this anguishinducing documentary two men struggle with that question in very different ways
What Our Fathers Did is a movie about historical and filial responsibility about repudiation about acceptance about the pain we inherit and the pain that continues to be doled out
My Nazi Legacy becomes horribly gripping
Wchter is a frightening example of how the denial common to fascist regimes can endure long afterward
Its a film that attempts the impossible to make us understand what its like to confront the fact that your father was responsible for the deaths of thousands upon thousands of innocent Jews during the Holocaust
A hard unsparing watch but rewarding in the way it pushes each man to find common ground in the darkest corner of history
Essential documentary viewing
A horribly gripping film
It entirely upends what I confess were my own preconceptions about what such a film would be that is a placid consensual study ruefully brooding on the sins of the fathers This is far more challenging  and more disturbing
What is it like to grow up as the son of a senior Nazi with atrocities on your family conscience In this powerful documentary the British lawyer Philippe Sands meets two men living in the shadow of the Third Reich in very different ways
A valuable examination of personal responsibility v familial loyalty set against events that should never be taken for granted
A fascinating doc about a couple sons of Nazi war criminals one ashamed of his fathers legacy the other stubbornly proud
Too much of Noma is composed of gorgeous pillow shots which grow static and fussy appearing to exist almost apart from the subject matter
Deschamps never ventures below the surface of Redzepis wildly successful experiment and while the pictures are pretty no one judges food on appearance alone
Slick but always sharp in the best contemporary European nonfiction fashion
Ive never seen a restaurant documentary that seemed less interested in showing the joy of food
Most scenes feel stagey with Redzepi feigning intimacy but sounding like hes auditioning to be the next Gordon Ramsay
The story of Ren Redzepi and his awardwinning restaurant Noma succeeds in spite of any directorial missteps
Less a documentary than a glittering souvenir but its still a record of a legend
Noma My Perfect Storm is crafted with exquisite care in the vein of its subject though it occasionally feels overly precious
Despite some mouthwatering shots of the presumably delicious menu items the approach combines a strange mix of ingredients that isnt for all tastes
The film forgoes narration for a naturalistic style putting viewers in the place theyre most curious about  the kitchen
The end result is a revealing portrait of an artist wholly dedicated to his art
Food is not an inherently cinematic subject being fundamentally about the sense of taste rather than that of sight But in its own terms this doco isnt too bad at all
Unfortunately you learn more about his struggles and his food in multiple episodes of Anthony Bourdains TV series No Reservations and The Mind of a Chef than you do in the  mostly flaccid minutes of Noma My Perfect Storm
Theres no reason this should run almost two hours
Ignore for a moment the generic title and the laborious first  minutes and you can appreciate this mostly heartfelt mostly sincere comedydrama about trying to outrun your smalltown past
Enjoyable deceptively simple story
Brooklyn lacks any real conflict The ingredients are what make it atractive however outstanding manufacturing and notable acting work by the its leads are not enough to save it from Crowleys bland directorial work Full review in Spanish
Saoirse Ronan creates an unforgettable character Full review in Spanish
A beautiful story about love and selfdiscovery a great production with great direction Full review in Spanish
It tells one womans tale beautifully The rest of her new countrys story however is left unfinished and unseen
Brooklyn is an excellent film for many reasons While it may take its title from one small part of the world it works so well because of the universal nature of its themes
Part of the films charm is the way that Irish director John Crowley manages the mood without compromising the momentum His direction is impeccable
Brooklyn doesnt just look back wistfully at the past it also transcends the period setting with powerfully timeless questions Where do I belong What can I make of my life
A subtle drama that work because of Saoirse Ronans acting work Full review in Spanish
Sincere emotional and even touching but its far from being a great film Full review in Spanish
While shes surrounded by a name cast this is Ronans film and while shes made big impressions before from Atonement to The Grand Budapest Hotel this is her most assured and moving characterisation yet
The immigrant story is one that has been told countless times over but director John Crowleys moving and funny Brooklyn brings a degree of empathy to this particular tale that is rare
A soft movie when it comes to immigration themes that allows her leading lady to shine Full review in Spanish
An impeccable romantic and realistic film Full review in Spanish
The film is certainly lovely and wellacted but the nerve it has obviously hit is not immediately obvious Perhaps its the films very modesty  its lack of pretense grandeur and histrionics  that accounts for its appeal
Everything seems to be in place without being memorable even once Full Review in Spanish
Brooklyn is a well acted and visually competent drama  but also an old school movie Full Review in Spanish
A film as understated but radiant and layered as its protagonist Brooklyn succeeds with a simple storyline whose emotional impact aptly hits home
Brooklyn is a superfluous romantic film thats entertaining but nothing more its dramatic parts arent captured with enough intensity to break down the audience Full Review in Spanish
This is Saoirse Ronans film Shes given a huge amount to do and asked to transform before our eyes from a callow floundering girl into an assured and confident woman She does so brilliantly and talk will turn to Oscars
It doesnt sound exciting but it is really well done with a great cast understated but powerful its genuinely affecting atmospheric often funny and very accessible
I dont know that McKay should go on being Hollywoods fiduciary moralist but hes clearly on the side of the angels and of the entertainers
Adam McKay gets that a lot of celebrities overact to simplify a embroiled story Full Review in Spanish
Smart and snappy this comedy is one of the scariest films of the year using humour to outline the  economic collapse from the inside
Wonderfully played although our heroes are far from noble and directed with great energy this is dreadfully enjoyable film
Theres no doubt that its the actors performances that carry the film to the awardnominated heights its been reaching as of late But despite all the praise for Bale who does indeed do a stellar job its Carell who shines brightest
A pseudodocumentary dressed up like a human interest ensemble when it should have felt more like a supervillain heist movie
At the end of the film I was hardly any the wiser as to the reasons eight million jobs and six million homes were lost in the US even though the skill of the actors and the films ambition remain impressive
McKays adaptation of Michael Lewis book presents the subprime loan crisis as a screwball comedy But perhaps the only same response to the loathsome skulduggery behind the  financial crash is to laugh at it
Adam McKay is passionate about the subject and The Big Short is exciting passionate filmmaking
An immensely entertaining and worthwhile document of Americas modern horror story
At first the tocamera segments can be discombobulating but over two hours the film coalesces into a brilliantly accessible scathing account of the financial crisis and its continuing aftermath
Itll make you angry but in a good way
It exposes the vast degree of lies and manipulation of the financial world in a far more entertaining way than any drama would
The film tries to take the socialist political stance of exposing the greed of the big American banks yet it has such a difficult time taking a measured stance towards its leading characters
Even if it is funny as it should be in parts it also has the capacity for drama and seriousness when dealing with the consequences of the crisis which makes this a smart piece of work Full Review in Spanish
In some ways its a flashier version of another Lewis adaptation Moneyball which made another dull topic  baseball statistics  surprisingly interesting
The very best that a satire can hope to achieve
The Big Short is the film that we needed at this time
Its a disaster movie  where the impending and continuous boombust cycle of capitalism is the oncoming disaster
Hanks as a stranger in a strange land gives us equal portions of laughs and insights into the worlds of both adults and adolescents Big also offers up a very funny satire of corporate ladder climbing
This elegant and perhaps very restrained presentation does just that presenting the case Discussing the rest is up to us Full review in Spanish
Carol is a meticulous lowkey reward for audiences
Focusing on the details that go from set design to excellent performances Carol is an exquisite film Full review in Spanish
Brilliant adaptation revised and refined from the romantic novel by Patricia Highsmith One of the best films of the year Full review in Spanish
Carol is a declaration of love to another film era but from a thoroughly modern perspective Full review in Spanish
Every piece fits perfectly to tell a story about love and passion but ultimately it feels like youve already seen this film Full review in Spanish
Carol is the most romantic movie of the year and through Rooney Maras performanceand Haynes careful directionit might also be the most cathartic
Its a delight to scrutinize every inch of Lachmans deep focus compositions and try and take in the abundance of exquisite detail
A love story told with looks and subtleties hand with hand with an extraordinary cinematography work Full review in Spanish
An elegant profound meassured and extraordinary crafted film Blanchett and Mara are hypnotic Full review in Spanish
In spite of being Hannes less ambitious cinematic film Carol offers the experience of putting you in the shoes of its characters and feel what they feel Full review in Spanish
Carol is a n excellent film full of warmth and romance
This is pure cinema which through its artistry opens a window into the souls of its characters and admittedly opens the closed windows of its viewers as well
What Carol captures more specifically than the thrill of a romantic encounter is the act of remembering only to forget
Carol is a movie worth watching to understand its main theme without fear of censorship and most of all to realize and understand that love is love regardless of genre Full Review in Spanish
Carol is a truly striking cinematic achievement artfully exploring themes that resonate still with powerful force today
The rhythm is slow but the film is never boring this is a story worthy of seeing in the big screen Full review in Spanish
As he did with his gloriously realized  domestic drama Far From Heaven director Todd Haynes once more brings a story of how in the pursuit of dreams even happy endings can come with collateral damage
The film still makes the viewer swoon its heady mood of love and longing generated by the briefest of glances and gestures
A perfectly acted perfectly sculpted perfectly rounded exploration of love and strife in the notsoperfect s
Sometimes I can almost hear the studio executives talking before a film gets the green light
Delivers precisely what is expected to its fansThe superhigh quality of the animation which places Alvin Theodore and Simon in the real world remains a real highlight in these innocuous adventures  it really is an unrecognised art
Its harmless enough but harmless garbage is still garbage
The Road Chip hardly qualifies as great cinema And really no adult goes to these films because they want to But it is a perfectly serviceable accompaniment to a choc top and bag of popcorn
Tediously silly slapstick revisit with the rollicking rodents
A movie that kids would love and adults can watch without getting bored Full review in Spanish
Even though the film is marketed as a road movie for kids it barely has any road in it and thats just the least of its flaws Full review in Spanish
A good movie for kids that adults can also enjoy Full review in Spanish
Fun time at the movies with lots of color and music totally forgettable but enjoyable Full review in Spanish
A sugarsweet nutty kind of chipmuck but cinematic excrement all the same
You might want to stick sharpened knitting needles in your ears
Instantly forgettable but inoffensive fluff you know for kids And inoffensive is better than can be said for many movies aimed at children
The franchise squeaks past with a soso sequel that barely improves on what came before Our only hope is that at some point theyll have to hibernate
Alvin and the Chipmunks The Road Chip is the fourth agonising instalment in the franchise that charts the adventures of three enormouslypunchable heliumvoiced rodents
If youre looking for something to amuse the kids on a cold windy afternoon then Road Chip is bearable enough  just But a fifth film would really be a stretch
One amusing airport security sketch aside this series must surely have had its chips
Ultimately fans of the franchise wont be disappointed while the rest of us will be quietly hoping that Homeland Security step in a finish the job
The absence of longstanding series stalwart David Cross is a shame Tony Hale cant quite fill the gap but Jason Lee is back making it business as usual
The final product quite amiable nonsense
Normally dependable comic actors are wasted in roles
An engrossing rebuttal to the sense that only Hollywood can do big kinetic entertainment
Writerdirectorstar Paul Gross spent a lot of time over in Afghanistan as he prepared to make Hyena Road and the verisimilitude of its combat sequences both on the ground and back at command centre is one of the payoffs
The film acknowledges political and moral quandaries in Canadas peacekeeping mission in Afghanistan but then applies the most disingenuous tropes from oldschool combat films and westerns to gloss over the grey areas
The conflict between acting strategically from the comfort of a military base and acting morally on the ground plays well in Hyena Road
Gross script realistically depicts the uncertain and frequently shifting alliances that characterized the Afghan conflicts security situation
Hyena Road has an agreeable modesty to it that almost makes up for what can sometimes be a very pedestrian treatment of contemporary warfare
The goal of the movie is truthtelling rather than flagwaving but it also succeeds as impactful storytelling
The war story featuring good people making the best of a bad situation may feel very familiar but it is a better kind of familiarity that also characterizes Hyena Road
Gross combat scenes are the appropriate kind of chaotic explosive rushes that tend to feature the soldiers trying to figure whats going on as much as actually fighting
The strategy was sound but the script proves clunky on screen because its clad in a heavy armor of thought and doesnt leave enough gaps for the subtle nuances and offhand gestures that make characters feel entirely real
Canadas own waronterror film cant get beyond embedding us among the troops GoProstyle shots military code and jargon to strike at any deep dark truths about the recent war in Afghanistan
Its one of the better films about the Afghan conflicts but it struggles to strike a balance between connecting with real soldiers experiences and contributing something interesting to the genre
While its not American Sniper it still seems like a missed opportunity
Paul Gross situates the films events somewhere between violent militaristic fantasy and gentler antiwar lament
Viewers are graced with a few disappointingly subdued action scenes two humdrum romantic subplots and a virtual museum of bad acting
The explosions are so wonderfully photographed if only we actually cared about the characters exploding
Gross  is ultimately unable to satisfy the rules of dramatic engagement
Hyena Road is a skillfully filmed reminder if one is needed about why Afghanistan earned its status as the graveyard of empires
Hyena Road runs two hours long but its entire narrative could be wedged into a halfhour documentary
Gross manages to craft a feature thats horrifying and strangely inviting at the same time delivering solid characterization to go with all the chaos
An entertaining story with an agile script easy to understand full of innocence and lots of warmth as well as comical situations that will surely make you laugh Full Review in Spanish
A film that attempts to show us the importance of being kind and noble embodied in Charlie Brown Full review in Spanish
An ideal movie for this time of year pleasing fans and also manages to present these beloved characters to a new audience Full Review in Spanish
Its mercifully unmodernised and pretty faithful to Charles M Schulz original comic strip but its also rather flat and not especially funny And theres too much padding it could just as well have been a short rather than featurelength
The Peanuts Movie is both modern and traditional pleasing on all fronts which must have been hard to achieve
Kids will be able to follow the simple storyline and will enjoy the misadventures of the goodnatured wellintentioned Charlie Brown
Snoopy Charlie Brown and friends finally arrive on the big screen in a movie that sticks close to the gently comical tone of the comic strip that launched in  and the vintage TV shows from the s and s
Can be a bit frantic and antic the endings more schmaltzy than Schulzy But the sadsack whatsthepointofitall adult humour sneaks in Pretty much the least disappointing featurefilm adaptation Americas most famous comicstrip could get
Anyone who possesses even a passing familiarity with Charlie Brown and the gang from Peanuts should find some appreciable measure of delight with this superfaithful feature film adaptationa perfectly fine perfectly safe bigscreen translation
An uplifting kids film without any of the cinisism found in most movies nowadays Full review in Spanish
What other films for children teach in an hour  about life the universe and everything  Schulz could teach in a line and this film reflects that Its undeniably about decency goodness and love And Snoopy
Emotional and lots of fun a great blast from the past and a new discovery for kids today Full review in Spanish
A charming film made especially for those who grew up in the comics Full review in Spanish
A film that works better as an homage rather than recreate the greatness of the original cartoon Full review in Spanish
If you are looking for an agreeable and entertaining school holidays film for younger children this is it Fans of the comic strip should get a kick out of it too
Its a sincere film for children squarely in the tradition of the sweet simply drawn Peanuts cartoons from the s
The Peanuts Movie manages to maintain the soul of the comic strip even as it upgrades the visuals
A flawless noble and tender family film Full Review in Spanish
True to the essence of its characters and animated in a more tridimensional way Peanuts is funny and has a great message for every age group and despite not having the most original plot it will satisfy longtime fans Full Review in Spanish
The movie knows where it lives as a precious piece of nostalgia in the minds of grownups who cuddled up to Snoopy as kids
This is dreary stuff
A flat sea shanty
A host of pictorially arresting even painterly images cant make a satisfying whole out of In the Heart of the Sea Ron Howards film that doesnt dig very deep its penetrating title notwitstanding
Howards film is predictable yet thats not to say its not enjoyable If you can get past Hemsworths mishmash of an accent part New England local part booming Thunder God there are plenty of impressive turns to be seen
The film delves into issues that discards little too fast or invents too late Full review in Spanish
In the Heart of the Sea is a technical marvel demonstrating that Howard is still a master of making movies look and sound stunning Unfortunately the screenplay was lost at sea
It has the basis of the whole tradition of sea adventure films Full review in Spanish
The story of the ship at the mercy of the great white whale told with amazing special effects grandiloquence and some common places Full review in Spanish
Unfortunately the films script is far too creaky to make the most of the mens runins while the all too obviously fake computergenerated effects suck the tension out of the showdowns between ship and beast
Youll be better off rereading MobyDick than watching this soggy adaptation of the source material
In The Heart of Sea show us the mastery of narrative Ron Howard has achieved through the real story that inspired Moby Dick Full review in Spanish
a clumsy behemoth inspiring a kind of dull awe at all the resources mounted for its realization
The images here lash and last the story creaks and groans Anthony Dod Mantles cinematography is brilliant its as if were looking back at this past through a watersloshed spyglass darkly The simpler storys too rote and modern though
Visually the film is great but the premise is forgettable and doesnt offer much Full review in Spanish
The whale moments are great but outside of them the film uses too much unnecessary CGI which takes away a lot of the films more real moments
Lacks in base thrills devoid of human drama and frustratingly crafted to wash away any inherent interest that might somehow have slipped through unmolested
The screenplay goes around in circles without creating an enriching experience the characters have no arc and what few moments could have been dramatic end up boring and full of cliches Full Review in Spanish
Ron Howards In the Heart of the Sea is a beautifullymade film that features stunning visual effects but its ultimately let down by a bland screenplay thats unable to bring out the exciting and epic nature of the story
Theres hardly a genuine moment and a framing device only adds another layer of dramatization to something that already plays like a bigbudget History Channel reenactment
The films most affecting scenes are of Herman Melville interviewing a survivor of the wreckThey sit in a small living room huddled as close as the survivors in a lifeboat  and as bound by the story of the Essex as was its illfated crew
You say goes completely off the rails like its a bad thing
A riff on the Hollywood conventions of a story we know very well already with little new to say James McAvoys mad scientist is fun to watch though
Unfortunately  just like the monster in question  the latest offering from director Paul McGuigan Lucky Number Slevin is a bit of a lumbering mess
This is not your dads Frankenstein more is the pity and time would be better spent watching the marvelous  original
Like the spark of life itself its hard to identify the elusive missing ingredient that prevents this ragbag of potentially likable body parts ever earning the accolade Its alive
It is a strange mix of intentional and unintentional laughs
The film repeatedly loses its charge falling back on dull franchise worldbuilding for sequels that will probably never be made What a shame
This new version adds little to nothing to the story we already know and have seen so many times Full review in Spanish
Victor Frankenstein lumbers and lurches rather like the monstrosity that the title character jolts to life with a bolt of lightning from the heavens
Old Meat
Add the film to the disturbingly long list of dreadful adaptations of a source that deserves better
The only time the movie comes to life so to speak is during its climax when Victor succeeds in bringing his creation to life and must deal with the subsequent rampage After slogging through  minutes of tedium to get there its hard to care
The screenplay for this movie is tedious boring and hard to believe Full review in Spanish
Victor Frankenstein is a spectacular movie Full review in Spanish
Unfortunately the end result has neither the subtlety of Sherlock nor the intellectual rigor of Mary Shelleys novel  its a hammy clunky misfire which largely squanders a strong core concept
In spite of being a great production wih a great cast the movie falls appart due to a bad script Full review in Spanish
It has so many illogical elements that it becomes an involuntary comedy Full review in Spanish
Any time you watch a reimagining of a story in the public domain you do so at your own peril
This schizophrenic lump of stitchedtogether cinematic remains hardly deserves the moniker alive
What slightly elevates the movie but doesnt quite redeem due to its almost  hour leght are its costumes and overall production design Full Review in Spanish
Tyler Labine carries a lot of the film against his straight man Crawford like the grizzled spawn of Seth Rogen and Jack Black  a jovial head in parkas and earflap toques
In Mountain Men Labine and his cast achieve a hardtostrike balance of comedy and heartfelt drama while maintaining a subtleness that respects your adult attention span
The script has plenty of salty language and dialogue that rings authentic The story is wellconstructed with a conclusion that will leave you smiling And the scenery in and around Revelstoke BC is awesome
Mountain Men delivers big heart some big laughs and authentic character development observed through sharp but always natural dialogue
Fleet and likeable
Though Topher forges ahead bravely its all too clear that hes headed toward an underwhelming conclusion
Writerdirector Cameron Labine finds a trail that feels reasonably fresh due in no small measure to the warm comedic talent of costar Tyler Labine
There are three good laughs in Mountain Men and two modestly dramatic sections That averages out to a decent moment every  minutes
While some of the interpersonal revelations and inner character struggle feel decidedly familiar treading the waters of the male comingofage tale the setting is novel and the added dramatics of their adventure freshen the story
A refreshingly heartfelt warm and tender film
hints at an intriguing character study yet despite some solid performances the script derails its momentum with generic and predictable thirdact twists
Richard Gere gives an annoying performance in The Benefactor  and it works
An unconvincing melodrama indicting moneys power to manipulate as the root of all evil
As a memorable work of cinema it misses every important mark by a mile
A runofthemill story of a junkie
If the conclusion is a little too sunny thats a small flaw in an otherwise compelling film about the hazards of trying to buy emotional connection
A compelling portrait of a man on the verge Suffers from a sense of rigidity that somehow both fits its themes and stymies a greater sense of realism
Despite great cast melodrama has little to say drug use
Has Richard Gere been hitting the bong
Theres a richness to the cinematography that along with the central performance hearkens back to a character study from a few decades prior
The Benefactor is both a bad film and a thoroughly inexplicable one
Gere is committed but his character becomes increasingly annoying with a onenote tortured past and the script leaves him and his underwritten costars stranded with a pat ending
The Benefactor is a character portrait in search of a movie
Renzi provides a platform for Gere to act in barnstorming fashion but cant work out what to do with any of the other characters They tend just to stand embarrassed on the sidelines as Gere holds forth and steals every scene in which he appears
The price of Geres good looks is his acting skills dont quite match them
Fanning criminally underused and James have so little meat to their roles they end up as ciphers while Gere has a ball as a drugaddicted tycoon on the verge of a nervous breakdown
The Benefactor loses whatever anarchic spark it may have had leaving us with an increasingly empty symphony of its stars repertoire of heavy breathing blinking and outofcontext laughing
Gere is very good as the largerthanlife Franny but the film is too slight to do his performance justice
Gere is watchable but constrained by a rushed screenplay that never gives us a proper handle on the man The potentially intriguing character dynamics fail to splutter into life
Presented in a rather unique way without feeling contrived when doing so
After this production every high school in America is going to want to take a shot at staging this Grease is the word
The ambitious  million production of the Rydell High School musical was as impressive as it was fun
The threehour production almost a third of it seemingly commercials was filled with marvelous moments like this theatrical reveals cinematic dissolves television tempos
The show wasnt perfect but it was great fun with touches of film nostalgia and musical theater earnestness
Regardless of its flaws Grease is a reason to look forward to the next round of live musicals on TV
This was a show that was more about individual moments than about building a story
It had all the infectiousness and emojibig smiles you need from three hours of live musical entertainment
Thanks to exceptional work from director Thomas Kail and several sterling supporting performances much of Grease Live was as sweet and tasty as a root beer float
An ambitious exuberant and eyeboggling adaptation of Grease
With this hectic ambitious and hormonal Grease Fox proved it could up the ante in the new miniindustry of musicals on TV
Fox raised the bar on TV musicals Sunday night with its ambitious wildly energetic and mostly entertaining Grease Live
It was a seamless vibrant energizing hit
Despite its innovative direction and talented cast Grease Live fell victim to its bland source material  and equally bland leads  leaving it unable to truly top other iterations of the modern TV musical
The show successfully managed to combine all the nostalgic elements of Grease that everyone expected while creating a new experience
Grease Live opted for a more cinematic approach connecting the dots between the livetotape soap operas of the s and the live music videos you see at contemporary MTV awards shows
Even within the constrains of a live televised eventwhich lets be clear Fox DOES know how to blow out when it wants toI couldnt help think that Grease Live was just a dud
High school itself is the nineteenfifties of our imagination always easier in our memories than it was in reality With Grease we can pretend we still want to go back
If Grease Live had more moments to work with like There Are Worse Things I Could Do it might have been truly special  but there was only so much it could do when the musical itself didnt have anything else to give
If you hate musicals and hate Grease Grease Live sure wasnt going to change your mind all of the sudden But for the multigenerational fanbase who admire this show there was plenty to love about Grease Live
Thrilling live musical is fun has iffy messages galore
Griffin relishes working in this lurid sanguinary Italian style and his enthusiasm is contagious
Funny animated sequel has cartoon jeopardy potty humor
Working in a surreally inflected vrit style  with few title cards or identifications other than what is spoken on screen  Mr Sauper also has a knack for catching his subjects in unguarded moments
We Come as Friends is a travelogue through hell one that weve come to know far too well wherever it is on the globe
A deliberately vague portrait of Sudan as a country beset by selfinterested neocolonialist outsiders
A a collection of stolen sights often dripping in gallows humor that builds to a subjective portrayal of a true dense rot
Sees Sudan as the epicenter of neocolonial competition between the US and China and can already see its about to lead to more war  in his pointed political travelogue
A riveting documentary that will bring your blood to a boil when you see how Africans are being exploited once again
Prepare to be emotionally engrossed and enraged by this alarming and searing expos on neocolonialism in South Sudan
We Come as Friends has been beautifully filmed with Saupers Godseye views of his planes wing and the African landscape below resembling something SaintExupry might have conjured
We Come as Friends aims a cinma vrit lens on a place where many promises are made but few are kept
Hubert Sauper goes where other people dont go sees what the crowd doesnt see and creates unsettling provocative political documentaries that are unlike anyone elses
The ongoing tragedy in Africa is too nefarious too complicated for any one film to do it justice but We Come as Friends opens a wide window into this mansion of horrors
The filmmaker uses his little plane to give us a birds eye view of a country struggling with keeping its independence against the odds
This beautifully shot documentary is an in depth examination of Sudan from myriad points of view
We Come As Friends Hubert Saupers teeming BreughelandBoschpursuing documentary portrait of chaos after colonialism in battletorn South Sudan is more eyewidening surreal sorrowful and anarchic than his earlier Darwins Nightmare
In case you thought that bad people werent still doing bad things in Africa Hubert Saupers disturbing documentary We Come as Friends will disabuse you of that notion tout de suite
The action always feels as if its unfolding in present tense the avantgarde score and disorienting extreme closeups conveying a sense of nervous spontaneous energy
A fascination with serendipity irony and absurdity like that in Werner Herzogs documentaries propels Friends into unexpected territory
Light on its feet yet deadserious in tone this excellent doc alternates micro to macro ground to air
Deeply unsettling and saddening a brief glimpse of a tanned George Clooney only solidifying the tragedy This is a pretty powerful work of nonfiction
Sauper and his twoman crew fly over a land thats becoming as alien to its indigenous population as it was and still is to those who fancy exploiting it
Theres a lot to like on a conceptual level but the execution is onenote and monotonous
Few would argue how powerful and effective this last adaptation turned out to be Full Review in Spanish
Too bad it also transmits a certain narrative tiredness because it follows too closely the original play Full Review in Spanish
Kurzel has put together an adaptation worthy of The Scottish Play but less powerful than the sum of its parts suggests Full Review in Spanish
Fassbender and Cotillard deliver mesmerising lead performances which are both compelling and realistic
Blunt dreamy and visceral new adaptation of the immortal work of William Shakespeare technically very lucid and brilliantly defended by Michael Fassbender and Marion Cotillard Full review in Spanish
It capture the spirit and the poetic greatness with remarkable eloquence Full revies in Spanish
This film is heavy into style and it was a style that will have selective appeal But the story is strong enough to hold the audience captive with what is as comes as no surprise a good
Fassbender and company do justice to the Bards Double Double Toil and Trouble classic
This is a powerful well acted and directed Justin Kurzel adaptation which gives unusual depth to the character of Lady Macbeth It gives additional force to the plays message about the evil and corruption which can result from great ambition
Striking visuals guide the way more breathlessly than any footnote could
Not all of your favorite scenes may be here but the ones that are will satisfy
Bold insightful and bracingly cinematic
The twohour film is full of synthesized sound and fury and what it signifies is not worth watching
A worthy Macbeth refresher course highlighted by Michael Fassbenders feverish Macbeth and Marion Cotillards simmering Lady Macbeth
Macbeth lives valiantly and dies soddenly by its decision to substitute so much of the Bards poetry with beautiful but empty visuals
Perhaps the fiercest cinematic translation of Shakespeare to date
The present edition in spite of an impressive cast is a blood bath that is more violent than any before but to no avail
Live by the sword die by the sword as the old saying goes and what a fantastic homage this is to one of historys best dramaturges Full Review in Spanish
Michael Fassbenders grave Macbeth is immaculate from the start he is forged on the battlefield and never seems to leave it
A bleak daring drama from Ukraine
If theres one place where love has no place its here
All of it makes for a unique cinematic experience an ambitious work that relies on its audience taking a leap of faith Those that do will be rewarded
There is nothing else like The Tribe at once a searing singular vision of a particular time and place and a brutal metaphor for the wounded human condition
The lack of spoken words or score also heightens the natural sound to a magnified impact in a film thats hardly lacking impact
An intriguing unique story done a disservice by some frustrating directorial choices Leaving it untranslated means we only get the broad strokes where I want details
Despite thelongueurs The Tribe is certainly a dark and powerful portrait of the grim goingson at a school where violence plays a far greater role than education told in a fashion that cant help but fascinate
It must overcome its gimmick or become a footnote Slaboshpitsky succeeds by using a language that crosses all social and cultural barriers violence Also sex
This is a challenging film thats not for everyone Yet theres no denying its brilliant concept and its raw cinematic power
The Tribe is constantly riveting and even if there is one other movie without spoken dialogue this year Shaun the Sheep Movie you still wont see anything else like it
What sets The Tribe from any other youth crime film is the way its told challenging the viewer every step of the way Full review in Spanish
The end result is like nothing else out there and while the violence that accompanies the characters fates requires a strong stomach this is a compelling experiment in pure cinema thats worth experiencing
The Tribe revels in the distance it leaves between its audience and its characters but in placing viewers on the outside it also creates an experience thats almost perversely empathetic
Its a movie Samuel Beckett would have loved exploding with language but existentially acknowledging both how little is communicated and how much humanity we share regardless
The Tribe shows that you dont need dialogue to communicate
While The Tribe often makes for troubling confrontational viewing it is ferociously engaging Stripped of dialogue this is cinema of highwire purity muscular precise emotionally complex  and surprisingly easy to follow
The movie features no music and no words yet some moments are so powerful and visceral that I still caught myself covering my ears
Whether Slaboshpytskiy can achieve anything so innovative after The Tribe may be an open question but on the basis of this debut his ability to handle powerful drama looks extremely assured even when its excruciating to watch
Easily the most intense movie experience of the the year The Tribe is an unsettling examination of how the oppressed can become the oppressor
This is pure cinema where words are not necessary the actions of the characters speak for themselves And they speak oh so loudly there is no way for the audience to cover their ears
Thanks to witty writing and excellent performances Break Point turns out to be one of the funniest sports comedies that weve seen in years
Jeremy Sisto and David Walton are well matched in this breezy likable tennis comedy
A mostly laughfree paintbynumbers approach to a pair of former pros vying for relevance as they enter kicking and screaming into their mid s
Easygoing and always likeable but hardly packed with laughs
The script plays like a random automatic serving machine Ideas bounce all over the place only to be quickly followed by more
Karas doesnt exactly reinvent the wheel as he puts this odd couple through the paces of getting in shape and reconciling old wounds but hes helped by some laughoutloud quirk
Break Point has its difficulties with storytelling and tone but it carries most of the way finding a rich sense of humor on and off the court
Its the kind of amiable but predictable trifle that nobody ever seeks out but that will mildly amuse everyone who happens to stumble onto it when it hits cable and the streaming services
Theres just too much dja vu at play on this court
Always nice to enjoy a little comfortfood movie in which almost nothing surprising or particularly fresh happens but were happy to spend time with the characters and we wish them the best as the credits roll
The affable shaggydog tennis comedy Break Point is bristly and charming just like its star and producer Jeremy Sisto who plays the affable shaggydog Jimmy Price a washedup semipro tennis player
Its a mildmannered film strange considering the ferocious nature of tennis itself the sport that supposedly holds Break Point together
Quietly moving comedy about tennis brotherhood language
Break Point looks good and it has a big serve but its hard to love wholeheartedly
Break Point may not be a great film but it certainly has a lot of heart This is a sweet flick
As far as tennis comedies go this one sits somewhere between WIMBLEDON and BALLS OUT Make of that what you will
Break Point is light and bouncy not unlike a tennis ball being lobbed back and forth You always know where its going yet theres a distinct pleasure in the fastpaced way it gets there
Break Point has its moments Its passable light entertainment but ultimately comes up short when reaching for deeper comedic or dramatic flair
Even if you manage to disentangle the many twists relatively early on there is still pleasure to be had here in witnessing how Pastolls bilingual screenplay allows crucial giveaways to get lost in translation
Interesting yet overlong melodramatic and fatally predictable Road Games is a backwoods horror film with a sheen of class respectability and Barbara Crampton These things go some way to save the day
Writerdirector Abner Pastoll delivers an accomplished tense and unpredictable thriller that makes the most of a strong cast
Nothing is quite what is seems on this psycho road trip and crazy revelations are eventually spilled
Its reasonably entertaining if not a tad by the numbers
Strippeddown thriller benefits from its setting
Fueled by strong performances and some genuinely riveting twists and turns its a solid genre film that delivers the goods in a largely fresh way
A wellacted but formulaic thriller
The cast doesnt quite succeed in keeping the suspense fresh throughout the storys left turns
Yes you can enjoy Road Games even if you arent a gorehound I wish I could recommend Road Games beyond that though since all roads lead to a twist ending that you can kindasorta make out from a mile off
Road Games is far too slight and simple for a premise that starts with a twisted promise Its more a board game than a chess match
A twisty little ride through the French countryside where everyone is suspect of playing hide the cheese knife
Though the inspiration here is clearly Hitchcockian the movies vibe is so tongueincheek as to be weightless
Despite that late inning stumble Road Games still provides enough suspense thrills and dark humor to leave genre buffs more than satisfied
Rather like watching a car wreck on the opposite side of a motorway
For all the amusingly fatuous remarks heard here  and Miss Spheeris has a great ear for these  the overriding dimness of most of the fans and musicians is frightening
This is a wellmade observant documentary with attitude to spare and plenty of justifiable laughs at the expense of its subjects
Theres so little respect for the music that we never see or hear a number from beginning to end and we rarely hear any of the musicians speak more than a few seconds at a time
Fascinating
benefits substantially from the periodic inclusion of electrifying moments
Tommy Oliver hits all the familiar notes in unfamiliar ways focusing on the impact of violence and exploitation
is a hamfisted morality tale about love marriage and the fallout of the s crack epidemic as though told by someone whose intel on all three came primarily from pulp sources
The film feels coolly detached because the story and characters are underdeveloped
Suitably lowkey but sometimes underrealized this drama is fueled by its workingclass milieu and a heartwrenching performance by Hill Harper
Meandering and inconsequential
Sorrentino wants to say something profound about illnesses that bury loved ones alive But the pompous lines kill the mood
Caine and Keitel are great together and Sorrentino delivers some typically gaspinducing visual flourishes But its also unmistakably indulgent and save for a few scenes doesnt quite deliver the insightful meditation on ageing it promises
Youth largely consists of a bunch of people rambling around a resort doing nothing But I cant think of a better bunch of people to ramble around and do nothing with
Youth is as psychologically savvy as it is beautiful
Sorrentinos Youth is sublime
It manages to be both pithy and pretentious
Paolo Sorrentinos script and direction are indulgent and extravagant his words and pictures thoughtfully considered for comedy character and quiet provocation
Caine is terrific  inscrutable and distant but evidently there are depths behind his oversized hornrimmed glasses
While Youth may not be his worst film it is his most pretentious and bombastic
Does Sorrentino attempt to tackle too much in this film Possibly though I would rather see a director experimenting with too many ideas than scraping the barrel with too few Youth is a rich and rewarding experience
Absurd but at the same time profound this is a rare movie find I have seen it twice and plan to see it again
Overwrought ninetenthlife crisis drama not even a great cast can create sympathy for the artistic and existential turning points on arty display
Youth might not be the most bedazzling of Sorrentinos films  then again how could one ever outdo The Great Beauty But it is indeed the directors most compassionate and affecting film to date
This new attempt by Italian director Sorrentino gets lost in translation Full review in Spanish
Sorrentinos lush cinematography captures the decadent beauty of wealth at the Swiss Alps resort but also the claustrophobic disconnected quality of spaces occupied by the hyperprivileged
How the hell did Paolo Sorrentinos latest not dominate awards season Spotlight and The Big Short are timely films Youth is timeless
Sorrentinos work isnt to everyones taste his films can be a bit langorous and obtuse so it depends on whether you find the payoff worth it If you do this is excellent
A decidedly mixed bag of a film varying wildly in quality on almost a scenebyscene basis
Artfully dreamlike and boasting towering performances by three legendary actors Youth is a mesmerizing meditation on age beauty and friendship
Despite the surface sheen and some enterprising plot twists it doesnt entirely convince
This British scifi punches well above its budget visually its a shame the drama cant match it
This doesnt come together but Trefgarne has clearly got talent
Theres simply too much going on to establish characters More upsettingly  being that this is a scifi film  its impossible to tell what the cool parts are supposed to be
Its obvious a lot of care time and creative passion went into it Its a relatively low budget project that does a great job pretending its not But behind the aesthetics there isnt much else
A strong if not genrechanging feature debut for actorturnedwriterdirector Justin Trefgarne
Narcopolis starts off intriguingly and ends solidly Its everything else in between that isnt particularly compelling
even if it doesnt ever rise to legendary status itself Legend is a fitting portrait of the twin gangsters who while they had their moment in the s have already begun to fade from view
What could have been a blazing entry into the pantheon of great British mob dramas is instead a dish of stale pudding
Its a movie that clearly states its goals but that doesnt accomplish them
As Reggies emotionally fragile wife Emily Browning is an oasis of sympathy amid the squalor the only person in the movie we really give a damn about
Hardy is an ensemble all by himself
Tom Hardy impresses mightily as hesplits up with himself Yet even with his skills Legend often misses the mark
While Legend definitely has its drawbacks you basically overlook or ignore them because of Hardy His characterizations are superb showing the contradictions and nuances of brotherly sociopaths who are laws unto themselves
A stupendous exercise of the criminal tale of period reconstruction of giving new life to a whole tradition Full Review in Spanish
Hardy is Legends saving grace valiantly dual acting in the roles of the very different twin brothers
Hardy and Emily Browning give excellent performances in this film cowritten and directed by Brian Helgeland  The story is compelling and the characters are interesting It is a solid historical drama
Tom Hardys double performance is worth the admission price Full review in Spanish
An average movie with an amazing performance by Tom Hardy Full review in Spanish
Even though the efforts to make the twins likeable and endearing characters are there we cant really empathize with them and the film feels just like another glorification of violence for violence sake Full review in Spanish
A dynamic tale where Tom Hardys acting chops become the center of attention Full review in Spanish
What stands out are the good performances that possibly just would like to fans of the gangster movies Full review in Spanish
Stretches a little too long but its a pretty good gangster film Full review in Spanish
Tom Hardys excellent performance is sadly not enough to pick up the work of director Brian Helgeland who is better suited writing great stories behind cameras Full Review in Spanish
The perfect setting and the outstanding work of Tom Hardy are the reasons to see the film Full review in Spanish
Tom Hardy sustain the film playing the notorious Kray twins Full review in Spanish
Legend is entertaining has good rhythm and makes good use of period music The problem is that it could have been so much more considering the material it was based on Full Review in Spanish
Its hard to imagine a world where Eddie Redmayne wont nab the Best Actor gong for the second year running
Eddie Redmaynes performance as Einar Wegener The Danish Girl is revealing heartbreaking and believable
Freshfaced British actor Eddie Redmayne here provides another sterling example of just how deeply he can immerse himself into a role
The films story is unique and brave and the two commanding performances give it a gripping emotional weight that is very affecting
That we never really glean who the Danish girl is is quite fitting since the filmmakers also havent the slightest idea who their characters really are
The acting is what makes this film
Numbing translation to film of a vital drama about the first transexual in history Full Review in Spanish
I wish the filmmakers had dwelled in the strange nature of Gerda and Lilis sexuality as they came to understand it together Instead it devolves into something a lot more sentimental and easy
Vikander delves beneath the surface to explore a truly fascinating woman And Redmayne is simply wonderful totally convincing both as Einar and as the Danish girl of the films title
Helped considerably by Vikanders strong playing and a lovely turn by Belgian actor Matthias Schoenaerts late of the remake of Far From The Madding Crowd as Einars old friend Hans Axgil this nevertheless hangs upon Redmayne
Its a subject that isnt often explored in mainstream cinema and so hopefully this film makes at the very least a small difference
The Danish Girl is a tale that however delicate humane and elegant is also excessively hackeneyed and ends up boring an audience that looks for more Full Review in Spanish
An earnestly told splendidly visualized film that could please the masses but will likely electrify few
Its commendable that Hooper The Kings Speech has brought Einar Wegener to a multiplex near you But this restrained elegant drama seems at odds with its pioneering subject
Tom Hooper still doesnt show any sign of evolution as a director Full review in Portuguese
Both Eddie Redmayne and Alicia Vikander shine on their own but their interaction and relationship on screen is the real enjoyment for the audience Full Review in Spanish
Loved The Danish Girl I hated Lili
Because of the simplicity with which The Danish Girl treats such a controversial subject matter the movie feels like a wasted opportunity to portray a character the deserves admiration for their courage Full Review in Spanish
The Danish Girl fails to give its full potential but it is good enough to recommend it with confidence Full review in Spanish
Vikander is excellent Redmayne is tremendous and Hooper does a great job harnessing the performers to get to the emotional truth of the story
A memory lane roach motel melancholy lapse into Greed Decade doom not unlike now as this bad boy veers in a slowly simmering psychologically dense portrait between victim and villain While toying with audience senses and his chosen targets as well
A yearold sociopath stubbornly fails to become scary in this stillborn thriller
By the time the film goes out in a blazing inferno of hellraising loneliness and tops itself off with the best final linetocredit song combo since Killer Joe The Boy has reached a point of stupid fun
The scariest aspect of The Boy is the extent to which Macneill makes it possible to sympathize with the troubled protagonist  even as its haunting final shot hints at the horrors yet to come
The Boys most disturbing facet is the possibilities it imagines
The director tries to generate a pace that his dramatic efforts fail to match
While its admirable that director Macneill and his coscripter Clay McLeod Chapman opted to emphasize mood and psychology over the storys more exploitable elements it nonetheless results in a listless tedium
While the score goes out of its way to make his every action feel sinister the picture doesnt fulfill its horrific potential until the third act
Craig William Macneills film is a sporadically frightening slow burn with a fatally overlong fuse
The film feels overly long and while lingering shots of the mountain scenery do help convey the isolation of the deserted motel too many of them feel repetitive
Mr Macneill and his coscreenwriter Clay McLeod Chapman have developed a feature stunning to behold if somewhat unpersuasive in narrative
Chapman and Macneill have a truly chilling character whose evolution will be both fascinating and frightening to watch unfold
The Boy is a very slow burn one that successfully works to the narrative at hand but isnt particularly enjoyable to watch
The Boy is a title that makes this movie sound innocuous A more fitting header would be Portrait of the Serial Killer as a Young Boy
Not since Henry Portrait of a Serial Killer has a movie gotten inside the head of a killer with such coldblooded artistry
It makes no bones about the fact that there is something critically broken in Ted The question for the audience is whether it is nature nurture or a mixture of both
It succeeds in conveying the dark edge of an effective thriller but it lacks the human sentiment  the poignancy the devastation  that wouldve made it soar above less heady genre fare
An austere and chilling portrait of Americas abandoned margins The Boy is a slowburner that builds and builds to its climactic conflagration and offers a dark disturbing flipside to Richard Linklaters Boyhood
In the march to the end Miss You Already grinds its gears through the late stages of cancer following a predictable and cliche path Each scene is a yank on the heartstrings a poke in the tear ducts its intents and machinations plainly obvious
A whimsical dramedy that induces the same feeling as eating a cup of Pinkberry or sharing a bottle of chardonnay with a friend and watching E
Miss You Already possesses a chemistry thats worthy enough for those in the mood for a downishnote friendship drama but its an experience Ill probably never subject myself to again  a strange sign of approval but approval nonetheless
It winningly reflects how to utilize quiet understandings and yes very loud laughter
Despite the earnest approach from its two stars who also serve with Christopher Smith as producers there isnt anything fresh in the story
The script from firsttime feature writer Morwenna Banks lacks grit and complexity attributes Hardwicke also disappointingly eschews in playing it safe with trite scenes
This heartfelt tribute to female bonding is steered in predictable directions more eager to jerk tears than explore new ground
Is this really an honest portrait of a beautiful and intense friendship between women Whats intense about it or beautiful or honest
Theres a jagged energy and a rawness to it Miss You Already jerks its tears honestly
The script has the excruciating familiarity and predictability of a galpal tearjerker weve seen a hundred times already all thats missing is to have Barrymore burst into a chorus of Wind Beneath My Wings
The movie has the tone of a liveaction Cosmopolitan magazine pushing the viewer to make judgments about people and behaviours urging you to invest in the drama from an ego point of view first
Theres a toughminded drama struggling to break through the movies glossy veneera contemplation of the black hole of death that sooner or later becomes the center of life
A jarringly jolly weeper whose save it from the saccharine
Collette and Barrymore capture the bond between two women who love each other unconditionally for better or worse
Its a proposal about feelings and finds its audience in every person who want to be moved have a good time and even reflect a little Full Review in Spanish
Screenwriter Banks redeems herself with a welljudged final act that tugs our heartstrings without feeling like were being shamelessly manipulated into reaching for another tissue
Its the gallows humour delivered with warmth by Collette Drew Barrymore Dominic Cooper and Paddy Considine that prevents this becoming unbearably maudlin
An intelligent script that mixes drama and comedy in a subtle way making this a visual and emotional delight Full review in Spanish
A film that doesnt have a perfect story but achieves its goal of showing real life in a moving way without being preachy Full review in Spanish
Miss You Already recontextualizes beats of the cancer movie subgenre into a story about the bulletproof power of female friendship
A fascinating portrait of a man some would call driven and others may call psychotic
Fighting bulls Its a tough job but does someone really have to do it
This documentary captures what may be the last of Antonio Barreras bullfighting days at a time when the sport itself seems about to fade into history
Its rather hard to feel wholly sympathetic towards the quixotic bullfighter who at one point declares Ive never had a relationship even with a woman as intimate as the one I have with a bull
Fascinating and oddly inspiring Gored is too focused on Barrera the myth to ever really focus on Barrera the man
Trivedi and Naqvi put together this multitentacled story using a daunting variety of footage news reports and archival film mixed with interviews It paints an extremely grim picture
You cant help but be impressed if aghast at the lengths some matadors will go to please an audience
even at a mere  minutes this feature feels padded and overlong  and the ingenious way that the writerdirector contrives to stage events before Emmas various devices also sadly palls through repetition In short needs more slashing
Sucker is likable geeky and gently funny rather than hilarious
Spall is the films greatest asset but Lucs performance is a more harmonious fit tonally Like the film he is pleasant unprepossessing and nothing to write home about
Sucker errs by not spending enough screentime devising and carrying off creative cons instead it invests too much in substories that are never fleshed out nor as engrossing as the cons these promising characters could have pulled off
Undemanding and ultimately irritating but gentle conman comedy from Australia
Whereas Lost in Thailand felt like a homage to Stephen Chows brand of slapstick Lost in Hong Kong looks to be an allencompassing love letter to Hong Kong filmmaking
Perhaps because it tries too hard to be too many things the movie loses its punch
Sappy crowd pleasing Hong Kong comedy
Sturdy wellacted but a mostly dull and unsurprising You cant go home melodrama
A Country Called Home never goes allin on the implied cornpone of its titlebut it never really goes allin on anything else either
Theres heart here for the taking but Axster turns a complicated domestic and emotional crisis into an episode of a network television drama
Lacks the clear vision to know how to distinguish itself from a hundred other similar movies
Theres exactly one fascinating original character in Anna Axsters wellmeaning but bland debut feature A Country Called Home Unfortunately the movies not about him
The rigorously dull A Country Called Home hates flyover country more than Bill Maher
The third act in particular becomes an awkward mixture of broad comedy and pathos
Its refreshing to see Poots in a role such as this one that lets her easygoing charm shine through
The gentle drama A Country Called Home is in sync with its smalltown Texas setting Unfortunately much of it is also as flat as the terrain despite the efforts of an engaging cast led by Imogen Poots
While I found it sincere I felt like it meandered too much and suffered from anxiety over what kind of a film it wanted to be
A Country Called Home is a decent enough trip through the back roads of Texas connecting with a partly dysfunctional family Its just not something that most viewers will want to write home about
There is not a lot in the plot that is surprising or even original There is some wit and there is a not too unrealistic look at strained family relationships
Parents and grandparents shouldnt be surprised if afterward youngsters are sufficiently curious to Google info about the reallife Apollo moon missions that the movie playfully references
Works hard in an effort to recapture the fizzy thrills of actioncentric Pixar flicks like The Incredibles
For some Capture the Flag will just about scrape by on its honourable intentions and brisk delivery however what seems on paper like a vaguely educational distraction is merely another exercise in colourful chaos
Several of the characters are so grating you long for a lunar disaster to strand them as far away from planet Earth as possible
Capture The Flag has enough visual energy to get to the moon and back Alas the script blows harder than a solar wind
Its a feeble storyline and the film is only partially lifted by some lively action sequences and one or two Gravitylike scenes of the astronauts drifting in space
It may lack the finesse and charm of animation from Pixar Disney or DreamWorks but its a decent yarn for younger kids who want something to watch on a rainy day or school holiday
Aside from a fleeting postMinions Kubrick gag theres little here for the oldies and nothing that the Pixar generation wont have seen done before and better
So mindnumbingly stupid that any investment in the characters is next to impossible
Capture the Flag sits comfortably at the classier end of childrens fare for the start of
There is an inkling of a better film here but something appears to have got lost in translation making this quite the forgettable voyage
This colourful animation tries a little too hard to get down with the kids
A good family film with a positive message Full review in Spanish
One of those films kids can see over and over again and adults wont have to suffer when seeing it Full review in Spanish
Great animation too bad the story isnt that great or original Still pretty decent Full review in Spanish
It touches on the subject of keeping families together successfully Full review in Spanish
The film entertains and will certainly be enjoyed by children and adults with knowledge in films and pop culture Full review in Spanish
A good effort that achieves its low goals Full review in Spanish
They just dont make this kind of romantic trifle anymoreand for good reason
A combination romance farce and road movie that whiffs in all three departments
Lackluster motherdaughter comedy has teen drug use
The film ultimately boils down to people bludgeoning one another in unimaginative closeups
Like the house the movie is something of a shifting puzzle box if only the backstory that turns its gears werent so rotten
As for the homeinvasion angle there isnt quite enough to set it apart from the pack Much like the title Intruders is just a little too familiar Youre Next this is decidedly not
Intruders blends a few horror subgenres to create something complex and engrossing all while wearing influences from Adam Wingard like a badge of pride
Intruders ultimately comes across like basiccable schlock or is it Netflix schlock now slightly redeemed by the germ of a great idea even if said idea never truly germinates
Intruders doesnt provide a pounding viewing experience but its sharp where it counts holding to a rhythm of torment and confusion that keeps the picture engrossing and repeatedly unnerving
Intruders is tautly directed by firsttime feature filmmaker Adam Schindler
Intruders a distasteful thriller with a bludgeoning sensibility and little common sense turns a cozy family home into a clockwork house of horrors
The actors alone cant sustain Intruders for its full  minutes but for the most part they follow Starrs lead carrying a film thats both menacing and magnetic
The result is a promising film that leaves a bad taste in your mouth like a meal wellpresented on the plate that just doesnt fill you up
An efficiently engineered suspenser with solid performances and a tight pace
Wellacted and initially suspenseful before it turns increasingly implausible lazy and uninspired as it piles on the twists in unsatisfying ways
A highly competent suspenseful and fun thriller that never overstays its welcome Do it a favor and open the door
The film doesnt know if it wants to be an overthetop sensationalized shocker with cartoonishly evil villains or a more realistic gritty thriller with human characters and the end result lies in a muddied thrillless exasperating middle ground
Intruders at least tries to do something different and it does manage to keep things ticking over smoothly enough
Of the numerous DC LEGO films Cosmic Clash is one of the better entries in the franchise Just be prepared to make a trip to the closest LEGO retailer afterwards
Heroes timetravel and fight evil in funny inventive tale
An engaging and thoughtful snapshot of Deans life enlivened by a pair of superb performances and stunning production design
Beautifully written and directed this factbased drama is an odd mixture of excellent acting and notquiteright casting
As a portrait of James Dean Life only manages to capture his soft stolid side
Its odd that Corbijn a gifted still photographer in his own right has so little to say about the relationship between shooter and subject or the impermanence of celebrity
While the script is sometimes too heavy footed on the whole Life has an unassuming quality that wears well over the course of its two hours
Life misses the mark by perhaps a sixtieth of a second but thats enough
It ends up demystifying Dean perhaps by accident but no less regrettably
I loved Ben Kingsleys overthetop work as studio head Jack Warner who in one scene explains the lay of the land to Mr Dean in a manner that would inspire envy from Don Corleone This guy isnt messing around
Dig if you will the pictures but you dont need Life as a stargazing aid
The actors and their exchanges ring true and by the time the film reaches its lonesome conclusion the resonances are eerie
A moody leisurely and occasionally frustrating piece of work
Flat James Dean biopic has swearing some nudity
Both Pattinson and DeHaan could have used more to do but both actors put in performances that elevate the proceedings
Corbijn is great at taking reallife flesh and blood people and alchemically rendering them in striking dimensional images that transcend and mythologize the reality but Life shows him fall some way short of achieving the reverse
Theres not much to go on here
Pattinson and DeHaan are both strong portraying real people with a mix of imitation and individuality
It really is the work of Pattinson and even more so DeHaan that makes Life a success
As the films emotional anchor Stock isnt nearly as fascinating by comparison
For Dean aficionados this is a wonderful way to get the backstory on some of the most iconic images we have of the star
Life turned out to be as bland as its title
Its an entertaining romp and one of the funniest Christmas comedies weve seen for years
As with the appalling Pyongyangset The Interview of last year The Night Before relies on swearing stereotypes causing offence and adolescent sexual jokes for its laughs
Like your old Christmas jumper Rogen is feeling a little worn
Id go so far as to say that The Night Before might turn into one of those yuletide favorites the whole family will go on to enjoy
Loaded with party drugs and with Rogen putting in yet another of his rote manchild performances  really Seth enough already  its a pleasant enough timekiller though all the improvised scenes with overlapping dialogue are a little hard to make out
Top marks to Jillian Bell and Michael Shannon for briefly raising the overall tone brickbats to James Franco for bringing it right back down again
There isnt enough story to sustain the running time and it lacks howlingly funny setpieces
Its A Wonderful Life meets Pineapple Express in this stoner Christmas comedy
Its bogged down by too many derailing tangents but the three appealing leads have a wonderful chemistry and it gets close to the spirit of the season
In The Night Before which Levine directed and cowrote sweetness and crudity mingle from the outset
A film about how time changes friendships with hilarious jokes and humor to along with it Full review in Spanish
Unfortunately the moments designed as comic relief become the main focus of the film giving us cheap laughs that stray away from the main plot Full review in Spanish
Michael Shannons name isnt on the movie poster but The Night Before wouldnt be as memorable or funny without him
This blending of the stoner bromance with the Christmas comedy works surprisingly well layering grossout humour with holiday sentimentality
A hilarious Christmas comedy that deftly mixes drugfueled humor with holiday sentimentality
Theres a glimmer of genuine sweetness beneath the tomfoolery in The Night Before which occasionally casts a warm glow over characters as they learn valuable lessons about the power of friendship to overcome every obstacle
Such a ragged staggering letitallhangout brocomedy that the women feel parachuted in Strains to be a Christmas movie for s babies now all grown up but umbilically attached to their smartphones
Theres small pockets of convincing dialogue between the friends but this clashes with random chaotic set pieces
The Night Before might well have called Christmas Carol on drugs Full Review in Spanish
In the midst of an otherwise bythebook boyswillbeboys tale its one hell of Christmas cracker surprise
Guy Maddin delivers another of his wild and whimsical fantasies tinged with camp and couched in the film grammar of silent cinema
The Forbidden Room is an absolutely manic joyous romp through a hilariously warped revision of seemingly the entirety of silent film an endlessly inventive celebration of the limitlessness and sheer dexterity of cinema from the first frame to the last
The tinted corroded images ripple pulsate and disintegrate They burn up then reconfigure as something else entirely The surface of the movie is liquid the actors seem to be drifting in an ocean of photographic developer fluid
It is sometimes brilliant and sometimes boring but even the boring parts have an eccentric sparkle
The Forbidden Room is a fun ride for cineastes and was much fted at festivals Whether it will appeal to mainstream taste with its insane mashup of B moviestyle parody silentmovie intertitles and jungle vampires is anyones guess
Maddin remains a unique treasure and The Forbidden Room is one hell of a trove
Exquisitely designed this cornucopia of melodramatic fragments and movie pastiches will enchant Guy Maddin fans
A visual phantasmagoria with a builtin propulsive energy
A grueling marathon of cinematic masturbation a mindnumbingly empty exercise in selfconscious style with absolutely nothing to say
Its an exercise in high kitsch which enraptures at first but becomes increasingly enervating the longer it lasts
If youre up for The Forbidden Room it fulfills its oneiric promise by leaving you with the vague impression that you dreamed it  and dreamed its dreams within dreams  even if you know you didnt
The screen pulsates like infernal internal organs or bubbles and mutates like melting celluloid jammed in a hot projector
Once again Maddin draws his inspiration from cinemas transitional phase between silent and sound and he is abetted wonderfully by production designer Galen Johnson and cinematographer Stphanie WeberBiron
An exhausting and overwhelming film to experience but theres also something exhilarating about its mad energy and boundless invention
What Maddin  Co have invented here ranges from Freudian horror to childish naughtiness
Theres a strange comfort in knowing that Freuds fascinating topography of the mind will never be completely discredited while Maddin is making deliriously absurd movies
Dense and lacking the playful quality of his more straightforward work this represents a new multinarrative direction for Maddin
Whilst The Forbidden Room is overlong and messy it is still a lightning bolt of creativity from one of the most enigmatic and compelling filmmakers working today
The bad part is that at over two hours long the film is not that good
Canadian iconoclast Guy Maddin has been making strange surreal films that evoke the images and storytelling traditions of silent movies for decades The Forbidden Room  is like a compendium of his obsessions and cinematic fetishes
Some generally witty lines are trampled by many more that are neither clever nor funny
Amy Ryan has a nice turn as Dons naive assistant but her humanity is more than this movie can handle
A joyless celebration of welltrod stereotypes in an even more joyless madcap lampoon
Sam Rockwell applies his usual deft touch to the title character whose bornagain ministry is founded on his dubious excavation of religious relics in Israel but whose charlatanism serves a sincere and abiding faith
The humor is very hitandmiss both because its wielding a rather blunt satirical weapon and because the sheer number of people Don fools becomes hard to swallow But when Don Verdeans barbs land they work
Hess eccentric characters are neither likable enough to root for nor ridiculous enough to earn big laughs
There is an absence of meanness in the Hesses comic worldview that makes their films almost impossible to dislike
There is a redemptive message in Verdeans eventual downfall but its mostly lost in a storyline that tries way too hard to be outlandish
Even with the likes of Amy Ryan and Will Forte providing capable backup it all grows ancient fast
The film employs and immediately squanders the talents of legitimate comedy allstars Amy Ryan Danny McBride Jemaine Clement and Will Forte
There is little that is worse than satire that tiptoes around its subject as if risking offense were a sin
Don Verdean is a pleasingly entertaining sometimes hilarious quietly satirical take on what makes us tick as a subculture
Parts of Don Verdean are executed with real imagination The rest of it falls asleep
Don Verdean is the sort of comedy which presumes its own hilarity long before it gets around to telling any actual jokes or staging anything that might otherwise be considered funny
I think I chuckled about three times
A poor attempt to spoof evangelicals and their mindless desperate followers
Don Verdean may only appeal to hardcore Hess fansanyone else will likely never once crack even a smile
Don Verdean could be more sharp more focused and its easy to be disappointed by the films low ambitions
There are some scattered laughs but considering its broad targets the film is too detached from reality to consistently hit the mark
Should have been broader should have been funnier Did they lose their nerve
It has enough original and strange elements to feel like its own entity and stand out Full review in Spanish
The most beloved fighter in cinema does not stand anymore in the ring Full Review in Spanish
If Rocky was a KO Creed is a TKO
Sylvester Stallone reminds us why we fell in love with Rocky in the first place Full review in Spanish
A powerfull emotional and well told story Full review in Spanish
The plot is pretty simple but it has a good start and a clear end also has moments that easily remind us to Rocky Full Review in Spanish
While this film is basically Rocky VII its also much more than that and perhaps the best in the series as it tells a standalone story with energy and skill
Coogler manages to make one of the best films in the series showing a Rocky full of courage and elegance Full Review in Spanish
In this revival there is not only talent but also affection and intelligence Full Review in Spanish
Creed reimagined a classic while paying tribute to a character that remains after forty years Full Review in Spanish
Creed may not reunite generations like Star Wars did but has fiber and muscle to keep fighting Full Review in Spanish
The most romantic story in sports films the industry has ever contributed to pop culture Full Review in Spanish
A powerful drama with great fight sequences and a superb camera work Full Review in Spanish
Stallone delivers a touching and wonderfully understated performance
Creed is able to succeed as an exceptionally entertaining sports story and the groundwork for building an identity free from the past
This is everything we could have hoped for from a Rocky spinoff and more Its a story about legacy race purpose trust and friendship
with Creed the series really has returned to its roots theres an earthiness and street quality that we havent witnessed since the  original
Director Ryan Coogler proves to be a filmmaker with a great capacity for narrative and just like his character hes trying to leave more than a good impression  a legacy Full Review in Spanish
An honest film the will thrill the thoughest and that can be enjoyed even by those who havent seen the originals Full review in Spanish
Like a palooka charging from his corner Creed plunges ahead with only a few sidesteps Director Coogler takes his time with each development endearing these already likable characters to the viewer even more
Panahi may make his points from inside a cab but hes no hack Just a farer of honesty and truth in a country where truth no longer has meaning
Panahi has made a work of invention and brio that remains visually lively throughout despite its formal restrictions
Panahis lighthearted banter and his interactions with his niece and his multiple fares  whether real or staged  give a valuable insight into life lived under a constant veil of political and religious oppression
Taxi is mustsee feelgood cinma vrit of the narrative variety
Jafar Panahis Taxi looks onto a world where the social order and the spiritual order are at odds in flux where the conversations are sometimes cutting sometimes comic sometimes troubled sometimes profound
Taxi is perhaps the most ingeniously optimistic movie Ive seen this year It also reminds me of the current Room another instance of extreme captivity where the only response is imagination Both films make our world a bigger place
Here is an auteur able to swallow the urge to shake his fist and bellow to the heavens about the injustice of his situation Instead he continues to explore with humor and humanity the themes that have always fascinated him
Taxi is easily the directors most accessible work to date
Panahis status as a martyr for his art could have gulled him into loftiness and pride and yet by some miracle Taxi stays as modest as his smile the point being not to recruit us to his cause but to put us on the side of his compatriots
This is not just a powerful lesson in creative determination and censorship defiance but an absorbing look at humanity full of poignancy and humor  Hope lives
iven the option Panahi might prefer to move onto fresh material  but as man and artist he seems to be surviving as well as could be hoped
Mixes the meta elements of his recent output with the shrewd social commentary of his prearrest work all buoyed by a cheekiness that makes it accessible to audiences beyond the arthouse
The sequences go from lightly absurd and funny to troubling and political from Panahis enjoyment of the rich fabric of secular life in Tehran to his anxiety of residing in a society controlled by the thoughtpolice of Irans religious oligarchy
Taxi is certainly the most entertaining of Panahis forbidden works but it is also a rich experience full of human drama comedy and ideas The filmmaker has turned civil disobedience into its own genre
At one point a lawyer lays a rose near the dashboard camera with the comment This is for the people of cinema For audiences Jafar Panahis Taxi is just such a rose
The spirit of Taxi is similar to Richard Linklaters Slackerpicking up new characters for discussions of current lifeexcept its funnier more polemic and a champion of every film ever made
Despite the laughs Taxi never loses touch with the desperate reality at the heart of its formal experiment
Beautifully balanced cheerful but incisive Taxi Tehran is playful but also deadly serious
It all feels spontaneous yet knowing filled with wry humor and revealing asides slipping in political and social commentary with a smile
His most recent work is an eloquent example of their strategies to pursue his film activity Full review in Spanish
What do developing countries need Trade not aid this provocative documentary argues
An easytounderstand docuessay with a toughtoaccept message
Boenishs wife Jean who trained to jump with him is interviewed extensively and although Strauch doesnt provide much backstory for her she emerges as that rarity  a perfect matchup to a seemingly unmatchable man
A generally splendid documentary by Marah Strauch about people throwing themselves off cliffs Literally
Long before there were GoPros there was Boenish a camera mounted on his helmet jumping from Yosemites El Capitan or LA skyscrapers under construction
Boenishs enthusiasm and lifeforce energize Sunshine Superman whenever hes an onscreen presence his absence proves to be a much harder thing to turn into compelling cinema
The two sides of the documentary more often exist alongside each other rather than supporting them Still especially for people who arent going to be throwing out chutes any time soon its a thrilling ride
Daring Thrilling Surprising  a well told true story
A spectacular and affecting portrait
While the doc takes a conventional and even pedestrian approach  with multiple talkinghead interviews and some reenactments with actors  there is also plenty of spectacular archival footage showing jumps dating back to the s
Engrossing tensionfilled and utterly fascinating
Sunshine Superman is at times a bit too reverent with the material and there are distracting reenactments  la The Jinx and Man on Wire some of which seem extraneous but overall the film is a sunkissed tribute to a singular individual
You might walk away from the film saying not That was cool or That was sad but That soundtrack was really fantastic
Boenish convinced plenty of jumpers to strap cameras to their heads which resulted in some spectacular footage
Firsttime filmmaker Marah Strauch skillfully interweaves the Boenish story  full of copious vintage footage shot by Carl  with dramatizations and interviews
You may find Sunshine Superman exhilarating or you may find Sunshine Superman terrifying but you almost surely will not find it boring
It will dropkick you right in the vertigo
Its a routine documentary about a fascinating man albeit one with a great classic rock soundtrack
Heartstopping cliffjumping doc
This is still a wonderful portrait of a man who chose to embrace life to its fullest whilst biting his thumb at gravity
This documentary about Carl Boenish founder of modern BASE jumping is a captivating portrait of a man unlike any other Not just an adrenaline junkie but a superb cinematographer technical wizard philosopher filmmaker and promoter of his sport
Ever watched an extreme sports enthusiast leap off a mountain to his death No Then you havent seen this bittersweet biopic about a compulsive base jumper with a death wish
The true story of charity founder Christina Nobles empathy in action
Noble largely works due to its clenchedfist approach tending to the particulars of Christinas war against suffering while maintaining its message of hope making it the rare faithbased film thats more show than tell
Writerdirector Stephen Bradley may make some missteps but he capitalizes on this underdog storys inherent thrills
The real Noble accomplished a lot but the movie insists on giving her achievements a mystical and mythical dimension  without the imagination to carry it off
A smart touching tragic goofy and surprisingly captivating dramatic biography of Irish childrens rights activist Christina Noble
Her Dickensian childhood provides real intrigue but writerdirector Stephen Bradley portrays the grownup Noble as a messianic figure making her philanthropy seem like acts of hubris rather than selflessness
Nobles biggest flop may be its dialogue
I left Noble wanting to know more about Christinas story than could be shown in a twohour film Can anyones life be sufficiently distilled in such a short span No But glimpses of such lives can serve other purposes
Uneven but nonetheless emotionally gratifying
Her indomitable spirit and merry heart make this remarkable story vivid true and touching
A feisty passionate performance by the Irish actress Deirdre OKane gives the inspirational biopic Noble a serrated edge of defiance and gumption
An earnest tribute to a strong and ambitious woman
Intelligent sincere and unabashedly goodhearted
As its title indicates this is a straightup inspirational tale but its sincerity and aboveaverage execution set it apart from many other similar movies
Portrays Noble as a person rather than a middleclass moviegoers moral pinup
Uplifting docudrama set in Vietnam recounting an Irish womans selfless efforts on behalf of the wartorn countrys homeless orphans
Even if you dont quite buy the philosophy this is selling the three main actors are impressive and director Stephen Bradley does a fair job of slipping between the different timelines
Noble is a solid straightfaced biopic that is raised to surprising heights by a flinty central performance from the redoubtable Deirdre OKane
Often desperately sad but ultimately triumphant Noble is a sincere and wellexecuted portrayal of good triumphing over evil
Bradley has structured the tale well as when we do move between eras its done so in a seamless manner and with a minimum contrivance
Erika Frankels documentary is finally revealed to be a story of prolonged adjustment to retirement and a poignant illustration of sublimated redemption
Frankel has a fine eye for telling detail and the result while sentimental is as irresistible as the dessert cart
King Georges feels stretched into feature length but its ending neatly portrays a man with a fierce personal code who seems to have accepted change
A poignant funny and wellseasoned portrait of autumnal fervor
The relationship between the young American and the old Frenchman is as rich as one of Perriers sauces the pupil and the teacher the son and the father the keen protg and the stubborn classicist
King Georges reminds us that a singular dining experience can often be the expression of a very singular personality singular temperament singular discipline
The vividly detailed behindthescenes glimpse of one of Americas best kitchens along with a holistic view of the title character make King Georges a flavorful treat worth savoring
Fun fare for foodies especially those with a taste for classic French cuisine and nostalgia for the posh French restaurants that once dominated the culinary scene
By patiently letting events play out past the easy and expected endnote Frankel has fashioned a fine story out of what might have been merely a cautionary tale
It finds its filmmaker completely lost between impulses to pay homage play it safe or offer somethinganythingnew
The widescreen intimacy of small moments  the flush of a rainsoaked cheek  humanizes Donzellis grand folly and the couple who challenge the parameters of morality
Marguerite  Juliens romance is a nonitem but Donzelli sprinkles it with faux naivet which has the aftertaste of an artificial sweetener Who would ever want to see this movie
The wan narrative at hand fails on several levels including an annoying amount of intentional anachronistic details that detract rather enhance
For a premise as provocative as an incestuous French fairy tale this sort of toothlessness is tantamount to a death knell
The excruciating experience of Marguerite  Julien need only be endured by viewers with an obsessive interest in the least constructive aesthetic currents in contemporary French cinema
An overheated but haunting strangely involving tale inspired by the doomed romance between reallife aristocratic French siblings circa
Whats missing is any sense of unified purpose or vision
It plays a little long but the visuals are spectacular in Pixars holiday film about family and overcoming fear Its a curious blend of coming of age story buddy movie and mid west adventure
Just nice basic fun really
The latest PixarDisney feature is a somewhat headscratching affair with some sweet charms and cute laughs but nothing like the complexity and profundity of Inside Out
As bright and fastmoving as it is The Good Dinosaur is very much a colourbynumbers effort Pixar has covered this sort of territory so many times now that recurring themes about being yourself have become rote to all but the youngest Pixar fans
Good enough even if its not as Pixar perfect as weve come to expect
Very good indeed
In  for the first time in the companys history Pixar released two films One of them was wonderful The other was The Good Dinosaur
An awesome technical feat The Good Dinosaur is a very good Pixar film but not quite a great one even if it is highly likeable all the same
While not up there with Toy Story  and The Incredibles as Pixars best this is still a warm and wonderfully told tale Arlo may be The Good Dinosaur but this is a great movie
Hyper cartoon design in the foreground and IMAX realism in the background makes an odd dichotomy and its hard to see the point
One of Pixars more entertaining recent efforts
By turns breathtaking to look at and emotionally engaging the film has to rate as a triumph for the way in which it can appeal to audiences of every age while perhaps relying more on classic storytelling elements
A deeply affecting and impossibly beautiful film that is steeped in genuine emotion colorful characters and a moving moral message
While The Good Dinosaur does have things to admire about it including beautiful animation and some intriguing characters its ultimately one of Pixars weaker efforts thanks to an overlyfamiliar story and themes
It certainly has its charms and is several notches better than a lot of the computeranimated mayhem passed off as childrens entertainment these days but its impact is almost entirely in the moment
The Good Dinosaur is a much better film than Cars but the newer movies propensity for sentimentality and Americana does indeed recall the earlier unlovely Pixar adventure
Its a beautiful touching and exciting tale of learning to grow up in a world where loss and death are a part of life
As sublime as D character animation gets
While the humour and supporting menagerie of characters feel slightly stale Arlo and Spots relationship is a remarkable feat of emotional evolution
You dont often chastise a film for not being sufficiently manipulative but My All American is an outandproud tearjerker and it cant even do that
Fumbles the opening kickoff and never recovers Manages a field goal at the end but no touchdown
For those looking for a sports film of basic quality and genuine narrative My All American is definitively useless
Heartfelt if bland football biopic is great for game fans
Dont get me wrong this film accurately shares a wonderful guys alltooshort life with us I only wish it had been delivered in a more wellrounded way
American football is played on a rectangle but movies about it can sure be square
My All American really has only one goal putting a lump in your throat at watching a kid refuse to stop believing that every one of his dreams could come true Lump delivered
This is the kind of movie they make you watch in grade school Its meant to be good for you and the entrylevel script has every clich in the playbook
Instead of genuine inspiration the movie provides only flimsy superficial positivity
Even those who love inspirational sports films might have to flinch from My All American is a movie so square conservative and humorless that it winds up playing like a brutally straightfaced South Park parody of gridiron schmaltz
My All American scores additional points for its emotional impact
an easily digestible slice of Texas football nostalgia that stumbles short of the goal line
Everything in the film may be true but every second feels contrived and thats a momentum killer that leaves My All American wide open to an allout blitz of rolling eyeballs
Almost unwatchable outside of Texas
Commits a major gaffe by overpolishing Freddie Steinmark undermining the impact of his reallife story
My AllAmerican is a redblooded Godfearing cavalcade of walltowall inspirational earnestness
Michael Reilly Burke and Robin Tunney are good as Steinmarks parents while as Steinmarks devoted highschool sweetheart Sarah Bolger makes a mark for herself as someone to watch
Other than a moving conclusion this is a mediocre sports drama
Were never invested in the sanitary whitebread story that renounces drama and entertainment for the sake of accuracy and a squeakyclean PG rating
Not an inspirational football movie but the highlights reel from one with a golden boy who is his own manic pixie dreamboat The worst sort of hagiography
Its an intricate web but when its finally untangled a remarkable and striking picture emerges Spotlight is that picture
the kind of film that takes on the Big Subject with modesty and selfreflection drawing the viewer into the world of the reporters and culture of Boston in  to better understand the scope of everything thats at stake
Succeeds because it puts the audience alongside the reporters allowing us to share the incredulity disgust and frustration
Nearbreathless hightension performances from a fully immersed ensemble each member conveying an unflagging determination to unveil the breadth of degeneracy
One of the years most noteworthy Oscar contenders and a brilliant ensemble piece
Spotlight is an advertorial for journalistic objectivity and integrity in the face of abject perversion
Spotlight a brilliant new film will shake you out of your complacency and renew your faith in the power of investigative journalism
The screenplay by Tom McCarthy and Josh Singer is one of the films greatest strengths imbued with as much wit as it is sobering truths
For such an intense story Spotlight ultimately fails because its so caught up in the process of news gathering that it stops being a compelling movie
Spotlight is a great movie that shows us the extreme amount of hipocresy at work within our world Full Review in Spanish
Spotlight is more than just a chronicle of horrendous events its a portrait of the pain and suffering millions around the world have experienced Full review in Spanish
Spotlight has increased my admiration for journalists who work diligently to find the truth behind serious problems even if it involves powerful institutions like the Catholic Church
McCarthys clear simple direction allows the performances to shine
A stirring assessment of the right and wrong of our humanity in the volatile world of a changing newsroom
The effort and teamwork highlighted in this film is absorbing intelligent and it remarks how exciting investigative journalism is perhaps it lacks adrenaline but its cinematic importance is undeniable Full review in Spanish
The cast is an impressive ensemble but Spotlights genius is in its calmly urgent take on events In doing so it makes them all the more sobering and gravid
There are fine ensemble performances from McAdams John Slattery Paul Guilfoyle and Stanley Tucci but Michael Keaton is Spotlights secret weapon
Spotlight is an indispensable film for moviegoers and for every believer Full review in Spanish
Like the crusading journalists it honors Spotlight forges ahead with a gripping story and doesnt stop until it gets it right
It restores my faith in the concept of redemption to see McCarthy bouncing from The Cobbler straight to Spotlight
The remake adapted and directed by veteran Hollywood screenwriter Billy Ray messes with and messes up the central romance and changes the triangular dynamic
This is a story of longings obsession and the inability to move on from events unaccounted for by justice
Why then does it fall so flat Ray hasnt nailed the structure dug deeply enough into the characterisations or finessed the big twist nearly well enough  thats why
An intriguing timeshifting tale of obsession and one of Hollywoods better remakes
This is a lethargic Alist remake of an ingenious Oscarwinning Argentine thriller from  called The Secret in Their Eyes Im not sure why they dropped the definite article
This isnt so much a remake of Juan Jos Campanellas  Oscar winner for Best Foreign Language Film as an utter travesty of it
This is a pale imitation of the Oscarwinning Argentinian original that cant breathe any new life into familiar territory
Two great performances squandered in a silly misfire
The film itself doesnt generate much suspense but it does engage as a morally ambiguous tale about loss and obsession
Tme and again the drama spirals into cliched foolishness and rulebending procedural pastiche from which only a typically engrossing turn by Ejiofor can save it
As a decent involving thriller it exercises the little grey cells and passes muster
Sacrifices atmosphere and character interest for the intrigues of a murder plot that simply isnt very compelling or consequential
Chiwetel Ejiofors new movie is a mess Even the title  why are there more eyes than secrets  induces brain ache
Appropriately enough a bogus Hollywood prestige picture is exactly what we get with US remake Secret in Their Eyes
Its rare for an American remake to be scruffier than the original but this film is an intriguingly messier take on the superslick hugely engaging  Oscar winner from Argentina
The overwhelming sense of unoriginality lingers over proceedings like a dark menacing cloud
Sure it is still an entertaining film and offers some great moments of acting and suspense but those who have the time to compare both films will be disappointed
The original pictures melodramatic excesses have been augmented with fresh indulgences to deliver a film that is not quite peculiar enough to be interesting
The new film fails to recapture the atmosphere of fear and menace that made the first Secret so gripping
The stars all bring their A games but beyond their convincingly moody performances this dour remake has little to recommend it  unless youre in the market for a circuitous year ordeal of loss regret and missed opportunities
A lowkey drama whose modesty is its own reward
James Francos Boyhoodrandom scenes sketching out boys callowness cruelty compassion and comic books And curiosity Bicuriosity
Yosemite mines Francos fiction for its most vital quality his unsentimental depiction of youthful insecurity this time among fifthgraders
Where STAND BY ME pressed its characters faces into the stink of mortality the boys here have yet to even hear the news about a dead body down by the train tracks
The material aspires to powerful terseness but its seemingly conscientious refusal to ramp up too much drama ultimately renders it thinly anecdotal
Its an interesting effort with a clear portrait of preadolescent curiosity but its not something to be viewed casually as the features patience with tone takes some getting used to
A sobering evocative drama
Franco gives one of his most subtle performances yet as a recoveringalcoholic father and the three young newcomers performances are honest and affecting
The movie succeeds on its own quiet terms binding the three parts together with assurance and tonal consistency to cast a lingering spell
While many may find its slow pace and lack of action a bit frustrating Yosemite is a thoughtful and wellexecuted coming of age indie
An earnest examination of the loss of innocence this critic might have appreciated even more if the subjectmatter hadnt be so relentlessly dark and disturbing
Although evocative and nicely observed the comingofage drama Yosemite ultimately proves too lowkey and elliptical to make much of an impression Stand by Me it aint
Extraction constantly tries to score a flashy TKO  but never lands a decent body blow
Very lowrent with a phonedin Bruce Willis performance However director Steven C Miller and game stars Gina Carano and Kellan Lutz contribute a few solid action scenes
There are a couple of passable fight scenes including one in a biker bar But the plot is ridiculous the bad guys are uninteresting and the script is so dull youll want to check your emails during the screening
Extractions  not by any stretch of the imagination good But at least it doesnt waste everybodys time
An unexceptional action picture with oldfashioned ideas about what it means to be a man
There are I guess some decently staged fight scenes including clever use of some pool balls but they in no way make up for having to sit there and watch Lutz garble out warmedover tough guy dialogue
The actionthriller Extraction may not be about tooth removal but for all the fun it proves it might as well be
Its a rare movie that cant be saved by Bruce Willis singlehandedly killing all the bad guys And yet here it is Extraction
It takes confidence to put out a movie whose singleword title is also the procedure by which a dentist gets rid of a rotten tooth
Extraction is an exhaustively paintbynumbers affair and nobody is more bored with it than Willis
Extraction Id rather have my teeth pulled by diseased evil monkeys
Some stylish touches cant rescue a thin concept thats riddled with cliches
Bad acting clunky cinematography and editing heavy violence and a lessthangenius screenplay make this a totally disposable action movie not even recommendable to diehard Willis ans
An obvious twist wrecks the story and a good supporting cast upstages the Younger Less Charismatic Eastwood in every scene
It inelegantly attempts to infuse a standard revenge western with the gravitas of a war veterans cominghome odyssey
Diablo is the first new film Ive seen in  Ill be surprised if I see one thats worse all year
Diablo is an underminute movie that feels doubly long bringing you facetoface with the Devil but taking its sweet time to get there unfortunately
Roeck aims for an homage to Eastwood Srs revisionist Westerns think The Outlaw Josey Wales Unforgiven etc but the result is pretentious and pointless
Powerful and beautifully shot if emotionally distant
Theres an Eastwood back in the saddle and maybe for some viewers that will be enough but once Diablo gets to a full gallop it really has no idea where to go
An early candidate for the  worst movies list
A relatively ordinary oater
Diablo boasts the skeleton of an interesting allegorical oater  but Carlos de los Rios screenplay never manages to provide flesh or a beating heart leaving the whole endeavor feeling more like a rough draft
Hes got the DNA for westerns but Scott Eastwood still needs some seasoning if Diablo is any indication
Theres not much beneath the surface of this modest vigilante saga
As far as high concept westernhorror flicks are concerned Diablo is nowhere near the excellence of the recent VOD release Bone Tomahawk but it should be satisfying for audiences who are in store for a moody and intriguing genre exercise
Ill give Lawrence Roeck credit at least he didnt reveal the twist to his new film Diablo in its last few minutes
Its impossible not to think of Clint Eastwood in his great Westerns when watching his youngest son Scott in this poor one and the comparison isnt favorable Both the movie and its star seem empty
Subverts the cultural contradictions saturating westerns historically while probing the postCivil War PTSD pathology beyond the officially mythologized dark side wild west frontier And another corrective to the portrayal of Native Americans on screen
The kind of movie your stepdad would mention having seen on Netflix a few days back but have trouble remembering the name of Hed probably think it was pretty good though
Diablo is not a great film by any stretch but it is stylish
Absurdist comedy with a level of violence so extreme and so jarring it makes the ultraviolence of Kubricks A Clockwork Orange look tame
While Moonwalkers is not a great comedy it delivers its laughs through the course of the film and leaves you with a smile
Houston we have landed in an overtly silly modandLSD rendition of  London that seems on loan from the Austin Powers sets
The misguided Moonwalkers invests too much in the comedy potential of things that havent been funny for a long while
Director Antoine BardouJacquet who devised the story is dazzled by period style and puerile jokes but nothing lands as especially funny merely tired
a sweet film that isnt afraid to show a mans head being blown off And to make it sort of kind of funny
Forget a fake moon landing Moonwalkers is a fake comedy
Its a setup rife with possibilities yet BardouJacquet manages to squander all of it veering wildly between farce and abject violence
There are too many footinmouth moments rather than clever writing and imaginative directing The films narcissistic attitude overplays its welcome
Graphic violence lots of drugs in soso black comedy
Moonwalkers starts off with an interesting idea and an attractive visual style but winds up falling out of orbit
Its all good English fun full of drugs brutality and general lates decadence Its also weirdly slack for such an insane ride as if director Antoine BardouJacquet was afraid he might get in the way of Dean Craigs splendid story
A drugfueled hyperviolent swingingsixties story
Moonwalkers is bloody vulgar and strangely mesmerizing with a unique cast that is essential to executing its ridiculous concept Ron Perlman is gloriously violent and is able to showcase the best side of dry humor
Moonwalkers takes a weird clumsy turn
An amusing concept becomes overloaded with quirks and psychedelic nonsense
While it brings a great deal of energy and promise to the table the film ultimately succumbs to its own bombastic tendencies
Has all the makings of a cult classic
Tarantino on acid subversive sixties stoner satire But a combo ballsy big screen intersection of politics publicity and propaganda that couldnt be more provocatively in the here and now concerning truth in movies and the media  if there ever was any
The whole thing has a retro swinging s vibe that I enjoyed The actors all commit to both the lunacy on screen and its premise and the result is a good natured and entertaining film
The Girl in the Book is an auspicious debut for Cohn a showcase for VanCamps true acting abilities and a fascinating feminine story
Cohn displays deep sympathy with her protagonists intersecting emotional crises scripting a narrative thats intensely perceptive without becoming mired in mawkishness
The filmmakers lend the films overall impact the alert observational intelligence of a firstrate short story
So unpretentious that it could be accused of lacking style or vigor writerdirector Marya Cohns maturely conceived Kickstarterbudgeted debut swaps genders on the more traditionally maledriven story of a stunted comingofage
As Alice VanCamp is exceptional eliciting our sympathy even when the character is making maddeningly selfdestructive decisions
Though its resolution is a bit pat most of The Girl in the Book is a smart and pointed look at abuses of power and roles women too often play in the literary world
VanCamp gives a layered memorable performance while writerdirector Marya Cohn making her feature debut has crafted a nonlinear story that artfully tiptoes between cliche and truth
The director Ms Cohn making her feature debut wrote the script and handily keeps the storys many elements in motion
From time to time Cohn allows a few rays of light to penetrate the gloom and suggest that Alice may find a way transcend her despair Its a small measure of relief from the overall mood of hopelessness
The Girl in the Book proves to be anything but a pageturner
Cohn  adeptly balances elements of contemporary romantic comedy and scathing cultural criticism in a film that is obviously deeply personal and simultaneously universal
Perhaps We are Twisted Fing Sister doesnt satisfy in the expected manner but its a terrific summary of hard work and dedication to a dream presenting casual fans with a PhD in SMFology
Its all steak no sizzle  the opposite of Twisted Sister
Its a very entertaining yarn made all the more enjoyable by the fact the bands two leading members guitarist Jay Jay French and his arch rival lead singer Dee Snider have a very welldeveloped sense of their own absolute ridiculousness
This persuasive paean to sheer bloodyminded persistence flies by over the course of  engrossing often hilarious minutes
Dee Snyder is charming and the movie is crammed with great archival footage of the band ranging from their early days as a David Bowie cover act to their jamming onstage with Lemmy Kilmister of Motorhead on British TV
A comprehensive and sometimes harrowing portrayal of the grind a working bar band in the s had to endure to get by
Using a trove of archival footage and memorabilia the story is told by the band managers and its devout fans over the course of  wildly entertaining minutes
Silverman proves that she is a legit actress not just pretty good for a comedian trying to get serious
Silvermans scarily good in this role  sickjokefunny when the behavior supports it raw yet subtle at Laneys most reckless junctures
Silverman the tarttongued standup comic known for treading treacherous social waters confirms herself as a serious actress If only shed been given a character supported by a stronger arc
A tough unbending sometimes brutally truthful profile of one womans addiction and the havoc it wreaks on herself and just about everyone who matters to her
Silverman is far and away the best part of I Smile Back a strained entry in the Mad Housewife genre
Silvermans performance while good is by no means great and she is not able to transcend what amounts to a little too much sweetness and light in this cinematic Smile
This is a dark picture but it has an exceptional focal point in Silverman who fully embodies the drain of depression
Sarah Silverman uses her standup persona to bring her fragile damaged addict to life giving one of the most startling performances of
I Smile Back has all of the makings of an intense emotional drama but the execution is messy and by the end didnt seem to have a clear story that it was trying tell
Holding the film together is Silvermans tourdeforce performance Involved in every scene her serious acting holds the film together
Silvermans shtick may be getting tired but her bravura turn in I Smile Back suggests a career resurgence
Silverman takes a tremendous leap of faith outside of her comfort zone and gives a nuanced and emotionally draining performance
A terrible script by Paige Dylan takes all the fun out of standup comedian Sarah Silverman playing a serious part as a depressed addict
I Smile Back pummels with nastiness then moves from one shocking event to the next without a backward glance
It is a brave performance by Sarah Silverman as a woman slipping into a private hell of her own making but  well that probably says enough
I Smile Back isnt a candycoated noble depiction of addiction and the toll it takes on families but rather a bleak yet honest look at how regular people lose themselves and how difficult it can be to find the way back
Silverman delivers a knockout performance  any memories of her scatalogical standup act are washed away in an instant But her intense commitment to the depths of depression belongs in a better more focused less derivative film
The sort of reductionism one finds in cautionary videos made for schools with equally weak logic
It would be nice if this film offered more hope for Laney but it isnt that kind of film It is just a slice of life and a pretty depressing slice at that with a disturbing beginning a dismal end and false hope in the middle
At a running time of  minutes we learn too little about Laney and too much about her addiction
Entertainment offers some genuine insight into its subject matter Coupled with an astonishing central performance it is a work that continually fascinates
I left this barbed portrait of a crackingup comic with more than a little respect for its fearless director Rick Alverson and his trusting star Gregg Turkington You cant deny that theyre a match made in heaven
Get with the extreme deadpan tone and long takes though and the film feels as brave as it is potentially exasperating
Weirdly compelling if studenty and unfocused
Like Hamburgers metahacky comedy routine the film confronts and challenges in order to produce something increasingly rare in American cinema an active engaged experience
The Comedian is an ugly man with an ugly soul and an ugly sense of comedy that at one point literally includes making fart noises for  full seconds as he pretends to gun down his silent audience with a soccer trophy
Gregg Turkington delivers a spoton performance as a downinthedumps comic in this gloomy offering Too bad the film left me me in a deep funk too
A Greek tragedy told through the vessel of a smalltime comedian Entertainment is a bleak look into a broken man swallowedup by secondrate showbusiness
Its the closest emulation of a waking nightmare in an American movie in a very long time The anxiety is generalized but its also as specific as Hell
A daring and mainly successful black comedy about a dour and depressive comedian on a deadend tour of California
It is not fun but its confrontational style yields dividends
Theres a chic emptiness to Entertainment undoubtedly and anticomedy constructs that may rub the wrong way but theres also a spiky intelligence at work too one that engages through the artifice of disengagement and the illusion of performance
Conjures an elegant portrait of a nation lost in a surrealist slipstream drawing on the iconography of its past and the shell of its present Its one of the best American movies of the year
The Frankensteinesque final scenes of Entertainment make it painfully clear As much as we want to read them as social critique Rick Alverson makes monster movies
An unsparing road trip through the barrenness of American pop culture and by extension the human soul
A weird road flick about a weirdo standup comic with a weird repertoire
This is one of those inyourface angry confrontational movies It wasnt just a chore to sit through it was more like a homework assignment given to you by a teacher who hates your guts
Its difficult to give Alverson and his star too much credit for depth insight or having a point Because Im not certain they have one
Alverson continues to zero in on the most brutal tendencies of contemporary comedy
Entertainment is a fully realised and extremely accomplished translation of Gregg Turkingtons Neil Hamburger character to the big screen
An empathetic snapshot of a country that is almost never depicted in such an accessible light
North Korean culture is lensed in part through a South Korean perspective with the final chapter asking Is reunification possible
The fact that Yoo isnt regarded as an outsider by the people shes recording allows her to capture a number of revealing unrehearsed moments that startle in their raw emotion
Scattering history lessons and ambiguous imagery amid Ms Yoos engagement with North Koreans her film implicitly asks What must they think of us
Yoos film is a necessary correction of how we should view or think about what life is really like in North Korea even if the work feels somewhat limited by its own necessity
Though repetitious this muckraking documentary exposes the dangers of chemicals that corporations put into some of their products
Though Whelans debut filmmaking effort wears some of its homemade characteristics proudly it wrangles more than enough credible interviewees to make its points
Like any of these documentaries this ones all over the map but Whelan grounds it in his family
A rather slipshod approach to the science of the subject undermines what might otherwise be a very strong film
Breezy Michael Mooreish environmental documentary by a nontreehugging venture capitalist advocates for disclosure of carcinogenic chemicals in everyday products
A heartfelt documentary about the chemical industrys aggressive efforts to conceal the thousands of potentially toxic ingredients contained in everyday products
A chilling exploration of the toxic underpinnings of the word fragrance that appears on detergents and colognes alike
No matter its cinematic derivativeness Stinks outcry against continuing to use the American citizenry as chemistry experiment guinea pigs carries with it the unassailable whiff of common sense
While Whelan repeats his points too much it remains gripping and maddening throughout to watch him run into stone walls
Even if you know a lot about the period Trumbo is still full of telling detail and Bryan Cranstons heartfelt performance as the remarkably driven and engagingly human Trumbo makes it well worth catching
Despite the dark subject matter theres much levity courtesy of Trumbos droll wit plus a roll call of Hollywood legends including Kirk Douglas Otto Preminger and an ebullient Bmovie producer Frank King played with gusto by John Goodman
Bryan Cranston does a remarkably effective job of impersonating the man who played such a key role in the era of the McCarthy blacklist from his raspy voice to his distinctive cigarette holder and his pungent sense of humour
With a topnotch cast and a compelling story Trumbo delivers a fascinating piece of cinema history that easily ranks as one of the best films of
With elements that include politics star power glamour and money there is plenty of bite in Trumbo Its a crackling good tale
Its full of both gravitas and titillation as perhaps any true story is there is the destruction of lives and careers on one hand and the spectacle of the players on the other  with Hollywood stars of the s and s as more or less support players
Some have quibbled over the fact that Elliott and Stuhlbarg dont look much like Wayne and Robinson here but it hardly matters as their performances are so strong and theyre matched by a large and luminous cast
The end result is a nobleintentioned attempt to pay tribute to the man responsible for writing Spartacus Roman Holiday The Brave One and Papillion but one that feels all too familiar in the way that it has been put together
Bryan Cranston shines in this remarkable true story about one of the darkest periods in Hollywoods history
Louis C K delivers a gentle and touching performance as fictitious screenwriter Arlen Hird that gives the film its emotional centre though why Roach and McNamara needed to introduce a fictional character isnt immediately clear
The film is a labour of love for Bryan Cranston as Trumbo As Hollywoods greatest screenwriter embattled but not beaten he embodies the old Hemingway definition of courage grace under pressure
If you are vaguely aware of his peerless reputation  or of the noble stand Trumbo took during a dark time in American history  then youll be content enough with this basic biopic of the man Even if it isnt a helluva movie
Bryan Cranston is irresistible as Dalton Trumbo in this sparkling period drama surrounding the Hollywood Ten
Absolutely sterling compelling drama about the notorious Hollywood blacklist of the s and sBryan Cranston Breaking Bad puts in a careerhigh turn as screenwriter Dalton Trumbo
Its repetitive in places but Trumbo is a moderately interesting character study Its not putting Dalton Trumbo on a pedestal and asking for him to be declared a saint
The fabric that holds Trumbo together is the love story of the writer and his devoted longsuffering wife Cleo As played by Diane Lane Cleo emerges as Trumbos most ardent supporter and most honest critic
A film so well built it also allows to explore the social and economic state of things at the time and how it affects Trumbo and his personal life Full review in Spanish
A story so incredible as the fact that Hollywwod was out for a with hunt Full review in Spanish
In his st project on the serious side after movies like The Campaign Dinner for Schmucks  Meet the Parents Jay Roach delivers a rather fine job
A film that does justice to screenwriter Dalton Trumbo Full review in Spanish
The actors have the showmanship to chew the lurid shopworn material up to bits savoring it like a Royale with cheese
Directorcostar Jackie Earle Haley and his cast serve up a solid slice of Tarantinolite But the script unravels with weak third act and overexplained finale
The script veers from comic narrated episodes to surprising violence planting early narrative seeds that yield some effective surprises
Theres an adequate conclusion waiting for viewers willing to work through a movie theyve seen before though Haley throttles repetition with some welcome screen energy and a love for the genre
Its a twisty kinetic and satisfying thriller with a didntseethatcoming climax
The thirdact twists that sink Jackie Earle Haleys directorial debut are downright Criminal
It wont win any awards but its not a bad evening out
An eleventh hour twist adds a bit of intrigue and everyone  including Haley himself as a henchman  seems to be having a pretty good time
It owes as much to Quentin Tarantino as the four bunglers owe to Eddie Yet it doesnt feel like a mere imitation it has too much wit and too many striking performances for that
A profane thriller that so closely resembles the Bmovies that followed The Usual Suspects Pulp Fiction and Get Shorty it could be mistaken for an archeological discovery
The diverting  if derivative  crime drama directed by Jackie Earle Haley comes laced with equal parts comedy and lurid violence a la Quentin Tarantino
Haley whips it into something reasonably entertaining even as you start thinking about how truly great Get Shorty and Travoltas Chili Palmer were midway through those doublecrossing criminal activities
Criminal Activities is a sneaky snaky little crime thriller with some pretty impressive plot twists
Its combined cast has an allages appeal although Robert Lowells screenplay struggles to drive through  minutes
If you dont expect much in terms of originality there are some good times to be had here
All sound and fury signifying nothing  just clichs begetting clichs
I almost wish that the film would have centered on Travolta and Haleys characters since the plot threads involving the four friends get wrapped up in about halfadozen The Usual Suspects wannabe twists
Some stylish touches and committed performances enliven this otherwise generic crime thriller
An exciting revenge thriller enhanced by Pulp Fiction vibes and a fascinating  but quirky  performance by Dan Stevens as youve never seen him before
a family home is also a prison of perverse history aspirational envy and twisted revenge Estranged shows Englands class structure to be a closed system its bricks and mortar staying essentially fixed no matter how deep the genetic pool may run
The transfixing directorial debut of Adam Levins Estranged creates an encroaching sense of imbalance and peril very early on and doesnt let up until the end credits
A welldelivered dark thriller helmed by firsttimer Adam Levins
Based on true events and well balanced the story of The  may be a surprise to many who only know the basics of the event
Despite dramatic physical effects during the initial structural collapse The  lacks the claustrophobia expected playing out like a s disaster movie minus the narrative tension
Strong drama and inherent humanity prevail to ensure that even the most hardhearted of viewers will crack and blubber with a lump in their throats
A missed opportunity to tell what should be a captivating reallife disaster tale that is instead plodding scattershot and lacking in dramatic impetus
How a film with this many faults can still deliver the emotive release its angling for is not perhaps as mysterious as it seems  you can hack away at it with a pickaxe but this storys indestructible
The astonishing true life story of The  deserves a better than movie than this Trite above and below ground it is not suitable for miners Or anyone else really
Its pretty hokey but likable and the fantasy last supper scene is tearjerking stuff
Riggens account is more thoughtful than the average disaster movie
What it does have is a gripping madeforthemovies combination of horror and heart as the families of the men gather at the surface first in protest at the lack of help then in celebration as the government gets behind the miners plight
A conventional but gripping dramatisation
Once seen instantly forgotten
In the end The  only grabs on a superficial level
The notion of a brownedup Juliette Binoche playing a humble Chilean streetfood seller deserves the snorting it will undoubtedly trigger
Riggen methodically juxtaposes crises above and below ground level the only stylistic surprise being a scene that recalls a hallucinatory moment from Oliver Stones wholly superior World Trade Center
A decent absorbing film but it all felt more urgent and compelling when it was unfolding on a daily basis on the news
The  lacks in emotion construction and staging Full Review in Spanish
The mens ordeal remains powerfully affecting although there are long stretches when it is touch and go whether their story will end up buried beneath a mountain of Hollywood cheese and clich
Sanitized for Hollywood a harrowing reallife story does not always make a good film
The film is forgettable but the story it tells us is a touching one and worth remembering
It focuses on painting emotion that was there anyway missing a chance to exploit richer plot elements As a result it is not as powerful as it might have been
Stanley Nelsons documentary tracing the rise and fall of black Americas most militant movement has a little bit of everything good mercurial personalities passion and plenty of drama
An evenhanded overview of a tumultuous time in US history
Exploring activisms role in the s struggle for civil rights this is a powerful look at a contentiously iconic movement
The film doesnt shake our suspicion that the stories being told have calcified into legend
Why are the Black Panthers not a force today Has radicalism collapsed An engrossing return to a forgotten past
It might have benefited from a more critical polemic some of those Panther cats I believe werent all good but for an analysis of US racial dynamics as seen from the post ICantBreathe era it makes for sobering viewing
Conventional as a documentary but speaks about much more than the just the subject at hand
Both educational and entertaining the rise and fall of the most alluring and controversial black organisation of Americas late s comes to life via facts vivid images and unique statements
Combining archive footage and presentday interviews and using music to drive the story Nelson tells a complicated and often violent story in lithe inventive fashion
A pulsing soulpower soundtrack extensive and rare archive footage and fiercely honest contemporary interviews drive Stanley Nelsons blistering account of the rise and fall of the Black Panther party
Vanguard of the Revolution is at its strongest when it entertains female voices Elaine Brown Kathleen Cleaver Ericka Huggins Their fire is vigorously undimmed
Nelsons film mixes rare archival footage with penetrating interviews to explore the successes failures myths and realities of the Black Panthers
This film is an awakening even for people who watched the original coverage firsthand
Theres still a crisis in blackwhite relations which makes this film timely
The thing that struck me watching this was how little things have changed with white privilege and police killing blacks since this group started out as a self defense organization in Oakland California in
Nelsons work feeling more fragmented than kaleidoscopic falls short of sactivist docs The Weather Underground  or   But sobering images and snippets of poetic insight linger
Obviously theres more about Americas postslavery institutions and their resisters than can fit in a twohour movie But with the aid of an extrafunky soundtrack Vanguard of the Revolution makes a good fistpumping start
Nelsons documentary shows that the story wasnt as simple as most people remember today
Despite the title not at all a fan tribute to a group that like so many others in the mad s imploded through a combination of state repression and its own ineptitude
The Panthers confused manifesto included shades of Marxism Leninism and the black nationalist theories of Malcolm X But the movements descent into violence and corruption was swift and fatal as Mr Nelsons fine film painstakingly demonstrates
Aaron Sorkins screenplay and Danny Boyles direction give us a Steve Jobs profile that does not idolise or flatter him As a result the film gains respect and we gain insight
When you have time to absorb all the positives here from the performances to the scripts details it impresses and lingers
Sleek efficient and spunky Aaron Sorkins attempt to distil Steve Jobs life into one pocketsized movie reflects Apple products in all but one way its ambition is alienating enough to render it userunfriendly
A balanced and often critical perspective of the flawed visionary behind Apple Pixar and everything Mac
Featuring fine performances from the whole cast as they make Sorkins sometimestricky dialogue sound real Boyles film makes you wonder how Steves friends and colleagues would feel about how impossible and appalling hes made to appear here
This is one small corner of a portrait magnified to the extreme If you do happen to find this product userfriendly it will be due to the aptly intuitive design of Fassbenders excellent performance in the title role
All respect to Leo but Fassbender deserves an award just for getting through the dialogue here alone
Jobs insistence on a closed system completely incompatible with anything else is a convenient metaphor for the character we see on screen here
Sorkin has created something that feels staged and unrealistic Its as if every key person in Steve Jobs life wants to complain in the halfhour leading up to each launch
Fassbender in a superb performance portrays Jobs as a genius  but also as a painfully flawed human being
This screenplay is staggeringly lazy Its factually incorrect in almost every way consisting of selfsatisfied expository patter and cringingly simplistic characterisation
Steve Jobs a study of the late cofounder of Apple that takes place in the hours and minutes prior to three separate product launches is a powerful film with little subtlety
Though it will undoubtedly mean more to people somewhat intimately familiar with Apples history Steve Jobs is a nevertheless affecting piece of cinema that never attempts to canonise its subject matter
the drive the philosophy the ruthlessness the prescience of Jobs a man who redefined the way we interact with the world
Steve Jobs is a dreamy pop song that repeats the chorus far too often But it sure does play a mean hook
Boyle  and maybe Sorkin though the domineering Sorkinese makes it hard to surmise  is doing something smarter even outright subversive
Though it is a decidedly dark portrait that shimmers into shape before us Michael Fassbender infuses a comic bounce to his performance that neatly counters his regular outbursts of despotism and bad fatherhood
If you loved The Social Network youll also love this film but if you hated it this one will feel as an insufferable drag despite the great effort of everyone involved Full Review in Spanish
Clever funny and utterly engaging this is both excellent and accessible
Thank you Danny Boyle thank you Aaron Sorkin thank you linesmen and ball boys the Ashton Kutcher certified piece of excrement Jobs has been successfully eclipsed by this inventive and kinetic glimpse into the life of tech icon Steve Jobs
Though JP Sniadecki doesnt elucidate any broad structural motive his film gradually adopts an engrossing rhythm among its clatter of steel and ambient chatter
By the end the real focus of The Iron Ministry isnt the train but the world zipping past it
Designed as a broadly impressionistic vision of the ways the countrys vast railroad system is used the pic is nonideological and intermittently engrossing catering to viewers especially drawn to this type of nonnarrative docu filmmaking
The parallel tracks of railways and cinema profitably converge yet again in JPSniadeckis outstanding semiexperimental documentary The Iron Ministry a pungently immersive evocation of traveling on Chinese trains
What emerges is a sense of an optimistic people well aware of how hard times can be but convinced they might be getting better
Coolly formal yet ceaselessly tactile works from lovely visual abstraction to the most material of physical concerns immaculate sound design a song for ears that crave the sound of rail travel and the insistent buzz of human commerce
The overheard conversations touch on social issues eg Chinas rapid industrialization and rampant unemployment addressed more thoroughly in numerous recent Chinese films
The Iron Ministry is neither boring nor confining which is just to say that its not a long trip through a faraway country Its a work of art  vivid and mysterious and full of life
Filmmaker JP Sniadecki withholds judgment and resists editorializing but the result is frustratingly nebulous and devoid of context
Seamlessly rides many rails through China to intimately experience sounds sights and even smells alongside restless people on the move through space and economic change
Amid the rumble Sniadeckis camera spies such a variety of life that it soon seems as though these trains provide a stage for the full spectrum of human activity
A subtly political film about the hopes and frustrations of ordinary Chinese citizens that is as dramatic in its own odd way as a kungfu costume drama
The Iron Ministry is a rather odd work for many reasons even if its depiction of class and ethnicity on various trains across modern China captures an essential moment
Grief unleashes the possibility of change in this wrenching drama allowing for an unexpected emotional thaw that rewards both stubborn optimism and traumatic resilience
An explosive family drama whose intense performances cant always compensate for such a heavyhanded scenario Bad Hurt nonetheless marks a promising directorial debut from playwright Mark Kemble
Bad Hurt wallows in heartbreak dashed dreams and death So why is it uplifting all the same
The material remains startlingly sincere leading with secure profound characterizations and a sensational understanding of toxic environments
An artfully drawn and beautifully acted film about a workingclass family grappling with a drugaddicted exvet son and a mentally and physically challenged daughter
The film hinges on a powerful central performance by Karen Allen as Elaine the wife and mother trying to hold it all together
Exhaustion of mind and body is the primary sentiment in this sensitively observed family drama drawn with an intimacy that is palpable and uncompromising
Throughout the filmmakers live up to the movies title But as the story comes to a close they opt to wrap it in comforting clich and they turn a miserable but credible viewing experience into a confounding one
A bittersweet drama about a family struggling to maintain dignity in the face of a variety of life challenges
Although affecting and well acted the family drama Bad Hurt is too airless and depressing to fully engage
Although Bad Hurt traffics in tough material it is filled with little moments of heart
Despite some powerful moments as the characters work through various demons and secrets the film feels more contrived than authentic compromising the emotional impact
In spite of the harrowing details the film builds compassion for even the most disturbing characters and scenes
Bad Hurt shows the truth is loud violent and unpredictable
Zariwny lacks Roths love for peeling flesh and tarcolored bloodgeysers making this all feel weirdly pointless
Scene for scene line for line gag for gag its basically the same movie And the original was no masterpiece to begin with
Travis Zariwny predictably scrubs all the edges and eccentricities down however fashioning another impersonally polished cover jam
A remakesorry rebootof the  movie of the same title in which attractive young people contract a super flesheating virus is not surprisingly more of the same
Who benefits from the existence of this film
Roth isnt exactly known for being critically defensible or for exercising directorial restraint but Travis Z somehow manages to up the gore quotient
The Cabin Fever remake is pointless primarily concerned with serving the financial interests of its producers not meeting the needs of franchise fans
Ive youve seen Eli Roths Cabin Fever theres no need to watch this  remake If you HAVENT seen Roths Cabin Fever start there and reread my previous sentence
This dud sets a new standard for the term pointless remake
Roths charcoal sense of humor is missing the cruel irony lacking its hellish zing
Unfortunately director Travis Z doesnt quite have Roths sense of humor about the whole thing and takes it all very seriously
A superior remake even if it is still abysmal nonsense
It feels flat throughout the bloodletting disappoints and perhaps most damning of all it neuters pancakes
There just is no point whatsoever to any of this childish witless nonsense
Unlike many horror fans Im usually open to remakes but Cabin Fevers humorless overly reverential redo might set a new standard for pointlessness
The whole thing is a likability vacuum
The symptoms are familiar Nausea fatigue and impatience accompanied by excessive eye roll and exasperation Yup Must be Cabin Fever a mindnumbing disease once believed eradicated
Crimson Peak is a disappointment because it doesnt have a particularly interesting plot nor does its characters end up being too charismatic even if it does show the visual inventivness of Del Toro Full Review in Spanish
Crimson Peak supersedes homage and becomes something new a reoriginal
Del Toro understands that sex and violence are what movies are for He wants to literally show you his characters insides
When Guillermo del Toro set out to cowrite and direct Crimson Peak a work of Gothic horror as gorgeous as it is preposterous the word restraint must have been missing from his moviemaking mission statement
Gifted Mexican filmmaker Guillermo Del Toro Pans Labyrinth makes an odd misstep with this overwrought gothic horror thriller which is so bloated that its more silly than scary
Crimson Peak is the perfect film for that small subgroup of moviegoers that doesnt see shivers and manic giggling as incompatible
The story quickly grew predictable and bland and the design of the ghosts and the role they played in the story left me feeling disappointed
Theres a point at which someone who is an expert in something can go from teacher to pedant
Yes the script is a tad silly at times but a game cast  Wasikowska was born to wander dark corridors  and del Toros eye for detail make for a stylish and scary shocker
Remarkable in every aspect and the classic tone is appreciated given the current tendencies in ghost movies Full Review in Spanish
This wannabe classic oldfashioned grand Hollywood production in the Gothic romance genre becomes ridiculously graphic and violent
Sumptuously stylized and soundscaped there are a few Victorian moments of sublimated psychosexuality But the sensationnovel plot and ghoststory atmosphere are too steamedout here chugging on the ending gets too stabby brutal and modern
As lush and atmospheric a film as the American cinema has created in years
The movies real star is the Sharpes decaying mansion a chilling living presence in its own right
It is fascinating for the myriad ways it veers right and wrong and it speaks to the stronger passages that their weaker brethren can be overlooked within reason
The set is spectacular the cast is stellar but Guillermo del Toros haunted costume drama is short on drama  and scares
Crimson Peak is a film more concerned with style than characters and the mysteryromancehorror plot despite an interesting theme the characters are thin and the plot is predictable
A film that duly fulfills its ambitions it chills and excites with its revisionist visuals and reminds us that the best stories with ghosts are allusions to deepseated societal fears and repressed emotions This is one film not to miss
You can feel Del Toros seal and personality but it unfortunately misses the level of his better works and you can feel the studios pressure all around Full Review in Spanish
With a reasonable smattering of gore and some absolutely breathtaking set and costume design del Toros final product is a truly unique beast
Keeping the audience off balance is key to any supernatural horror movie In that sense Drew Halls Convergence is pretty successful because for its first half I had no idea what the hell was going on
Convergence is a haunting tale of redemption  punctuated by Hostellike torture porn
A gripping police procedural thriller from writerdirector Alberto Rodrguez with understated character and political depth as Spains fascist past looms over all the characters
Rodrguez captures this suffocating and inescapably corrupt world through a noirish gothic cinematography
Allegory washes up against mystery in this superb period piece
Marshland is steeped in ominous atmosphere but the intrigue packs few surprises  its murky political undertow never quite pays off
With each new discovery the film becomes more intriguing and doubts are raised about almost everyone barring the director who has done a firstclass job
A film that keeps asking questions right up until its final haunting image The nighttime car chase is pretty great too
A visually striking noir thriller set against a backdrop of sex drugs and political intrigue
This gritty very intense Spanish thriller has the feel of an Iberian True Detective
How far you enjoy Marshland will depend on your enthusiasm for the ritual of genre  and in particular for the type of story where violence against women is mainly an excuse to probe the troubled souls of men
WOW Seriously just WOW This multiawardwinning drama from Spain ranks as one of the best films of  from a multitude of angles
The swampy lowlands at the mouth of Spains Guadalquivir River as lensed by cinematographer Alex Catalan help liberate the film from its genre moorings to produce a striking new form of Southern Gothic
A couple of mismatched cops in the immediate postFranco era investigate the brutal murders of two teenage girls in Alberto Rodriguezs satisfyingly atmospheric neonoir
Visually and atmospherically Marshland is suffused with an eerie oppressiveness entirely at odds with the regions reputation for lighthearted alegria
The novel settings  rural Southern postFranco Spain  sets this convoluted Spanish serial killer thriller apart
The elegant widescreen compositions and use of light and shadows are strongly reminiscent of Seven and Zodiac and the films eerie disconcerting mood brings to mind HBOs True Detective
Marshland has superb performances and a heady atmosphere but its greatest strength is finding resolution while letting the mystery be
Um filme que sob a superfcie de suspense policial constri um painel complexo de um pas lidando com feridas que jamais fecharam
While were still pretending that True Detective season  didnt happen we have Alberto Rodrguezs thriller Marshland to satisfy our yearning for moody detectives investigating a disturbing case that shakes their faith in humanity
Whilst its broad plot might be a touch too neat Marshland manages to say quite a lot about entrenched neglect and ignorance both within and without systems of power
Even if it stays true to the conventions of the crime drama it dares to explore complicated themes of the political and social issues of the time its set on Full review in Spanish
The most debilitating thing about this whole soggy mince pie of a picture is that its narrated by a dog
The Coopers are quite simply bad company
There are moments of genuine inspiration to be found but such is the intent to tick every Christmas box the good is outweighed rather heavily by the bad
Love the Coopers sets itself up to explore some interesting if not original questions but it baulks at probing too deep and instead retreats into schmaltz
Despite solid performances all round this isnt as good as the sum of its parts  its a series of cleverly constructed sketches culminating in a final act that never quite delivers the payoff
Love The Coopers may not be everyones cup of tea at the holidays but if you have ever had to prepare a special dinner or know that some people just dont get along in the same room you can relate
A bunch of Alisters plays out a string of intertwining plot threads all in search of a heavily eggnogged family hug
This is for Christmas movie masochists only
After spending time with them you wont know whether to hang the mistletoe or yourself
Full of clichs and dumb humor the film is barely entertaining even with Steve Martin as the narrator Full review in Spanish
This may look like its going to be a zany Christmas romp but its really a warm exploration of family connections essentially an American take on Love Actuallys multistrand comedydrama
With moments of unique lucidity this movie occupies its place among the classics that you can infinitely rewatch during the holidays bringing its acid outlook about these kind of gatherings Full Review in Spanish
Christmas With The Coopers employs an episodic structure that recalls Love Actually but falls woefully short of Richard Curtis festive romance
Wasted the grace of his talented comedians in a script that is nothing more than a sum of scattered sketches Full review in Spanish
Lots of photogenic folks serve up white whine usually weak laughs or suddenly candid chats with strangers The Hollywood contrivances are cringing Dundering in its efforts to melt your heart to slush
Love the Coopers isnt immune to a few overthetop moments and it never met a welltrodden plot device it didnt like but its unabashed heart and hopefulness is akin to a warm cozy knitted throw
It may take more risks than your standard Hallmark Channel Christmas movie but for a holiday film theres such little faith placed in our characters to live and breath
Its a terrible waste of talent Bah humbug
Full of embarrasing scenes that make you cringe its a shame that its director couldnt do best with the films talented cast Full Review in Spanish
Only occasionally does a moment ring true thanks largely to talented actors subduing a balky script like parents wrestling a petulant kid onto Santas lap
This one will leave you terrified of your own home jumping at every creak of the floorboard or bump in the night And God help you if you happen to live in a house with an attic
Very scary normal activity
Another quality horror film damaged by the increasingly dopey found footage format
Hangman doesnt rewrite found footage history but it plays to enough of the genres strengths in this creepy little home invasion tale
Doesnt offer connective tissue bellylaughs and scares Its just a shapeless creation that didnt survive the production process rendering a promising premise forgettable
Theres a whole lot of arid downtime in between the ostensibly colorful set pieces and on the whole the movie seems like it was edited with a blender
Freaks of Nature proves a lifeless combination of alien invasion saga zombie thriller vampire romance and highschool drama
Jack is in love but Maria and her family are driving him crazy Its almost like hes living in a sitcom which is great for him professionally He gives up writing a pilot for Space Bar Cheers in space and pens a sitcom based on his life How meta
The comedy is essentially sweetnatured the conflict that will keep our lovebirds apart so crucial to the genre does not go down quite so well
Amindblowing revelation from the silver screen Life is affected by the lifechanging decisions we make
Maybe there is some evolution to the Bond character in this movie from the old lowbrow misogynistic testosteronefueled Bond of the past into something more rounded grounded and thoughtful
Either a return to form a winking homage to the whole series or a dumbing down of the angsty introspective Bond of the Daniel Craig era Choose up sides
feels like a Best of Bond compilation  Bluenoses who once complained of Bonds indiscriminate bedhopping need not worry Its all Craig can do to suppress a yawn midseduction
For his latest adventure James Bond mixes the personal drama of Skyfall with the vintage globehopping action of the previous  movies
Its the end of an era but not the end of James Bond
feels slightly disjointed as if all the parts arent quite lining up and the progression of action that is supposed to build emotionally instead feels more like little more than a series of plot points
Spectre dances in the gate like an antsy thoroughbred from its very first frames as if it just cant wait to be a James Bond film
A film that takes from the classic bond movies but lacks a fundamental element for it to work a time period Full review in Spanish
Thats right In a film that feels about  minutes too long you dont get to the money shot for almost two hours And with that much time to kill you may find yourself dreaming of a few martinis  be they shaken or stirred
Instead of propelling Spectre over the top where it belongs Waltz instead becomes its unhappy avatar the thinly grinning face of sadly diminished returns
Its a solid serious spy film that still has a playful glint in its eye
plotting is convoluted maybe even tortured though I do wonder if all its conspiracies and turns dont constitute a MacGuffin
Most of the film plays like a game of Connect Four piling up and grouping together characters and schemes from Craigs last three films in vaguely related ways in the hopes that the connections to Bonds past will bring them some emotional heft
Essentially a wellacted Fast and Furious movie with more British accents Its good but not great
Spectre features all you would expect from a great Bond film amazing gadgets exotic locations stunts a new Aston Martin and a sprinkling of humour
With a perfect blend of a compelling narrative and breathtaking action sequences Spectre marks a return to greatness for the James Bond franchise whose past two entries had struggled to find the right mix of these two vital elements
A fairly muddled entry into the Bond canon
A film best enjoyed in theaters with your family and youll maybe have a good time but those looking for more than popcorn entertainment should stay away Full Review in Spanish
In their second  collaboration Mendes and Craig lighten the tone considerably Here at last we sense the two are actually having fun with a franchise that originally found success after all as equal parts adventure romance and spoof
Spectre is both modern and cutting edge as well as classic vintage James Bond
It doesnt exactly soar and the lack of levity grates yet the Spooks movie still delivers some appealingly oldschool mayhem
In the end it does not feel much different from an abovepar television episode but then that is probably no bad thing and it does leave you wishing that British filmmakers would make better use of an old reliable such as Peter Firth
Undone by sentimentality grumbles a senior secret agent in Spooks The Greater Good having been foiled when a longfavored rendezvous location proves a trap He might as well be talking about the film itself
Its nonsense but theres fun to be had in the endless doublecrosses and fans of the TV shows trademark gruff faceoffs wont feel disappointed
A solidly entertaining espionage thriller not up there in the first rank of spy movies but a decent enough placeholder to keep fans of the genre happy until the next James Bond
The film passes the time perfectly tolerably but it is no more comfortable in this less intimate medium than were ancient movie versions of The Sweeney and Callan
The big screen proves an unforgiving canvas for both the shows hitherto highend production values and its topical urgency
This may be bereft of the bombast that Americans expect from their spy adventures but as a modern spin on a John Le Carrstyle thriller its a stylish and smart action film
If you can overlook the films weaker elements however then Spooks still entertains consistently enough to make it a genre entry worth investing time in whether or not youve previously been a fan of the series
Led by the honorably dour Firth and the charismafree Harington MI is convoluted and dull
Written shot and cut to the demands of TV Dull and cluttered with overthetop intrigues
A dreary directtovideograde adventure that should have been terminated before it ever began
Nalluri confronts the familiarity of it all with commitment to speed and a general awareness that while his effort isnt going to look like a blockbuster it can periodically play like one
A film that like most of its ilk these days is more interested in action than motivation
Theres a surplus of doubledealings plot twists and international locales to justify the theatrical treatment
MI is no action bmovie classic but it manages to weave a complex and compelling narrative knot mix in some absorbing musings about the nature of doing right and following orders and pack in some nailbiting shoot outs
Spooks The Greater Good though nothing entirely new is a pacy wellcrafted spythriller that certainly matches the competition
Its difficult to call it great more like Spooks The Quite Good
Can an enduring hardy British television show make it on the big screen
Ultimately its Firths film and he ensures its never less than thoroughly watchable
A story about intergenerational bonding in a nonicky way its not for the whole family but it is one hell of a ride for anyone with a lot of miles on the biological clock
Given the film comes in at a shade under  minutes Weitz deserves credit for packing a lot in its an economical movie in every way but one rich in life
The fabulous Lily Tomlin finally gets the lead role she deserves in this smart engaging comedydrama
With not a wasted frame in its taut  minutes this is deserving of your attention as one of the years best movies even if its being released in the middle of August
This is the sort of accomplished indie film that we have been missing the last few years one that couldnt be made in the mainstream but which tells a recognisable important human story without feeling preachy
A bracing very entertaining little road movie with more grit and gumption than the subject matter might have suggested
a lot more conventional than it pretends
A fresh and intelligent film about scorned women but also really strong A comedy to enjoy with your family Full review in Spanish
A comedy with hints of drama and a simple plot but with a strong message Full review in Spanish
Tomlin is undone by much of the dialogue even when you get a sense shes putting her own brash spin on it
Lily Tomlin is excellent as Elle Full Review in spanish
Grandma is a transgressive and innovative option for the audience and not just a film about unwanted pregnancies abortion homosexualtiy or feminism Full Review in Spanish
Its a movie that invites reflection but is also entertaining Full Review in Spanish
A modest but effective feminist dramedy Full Review in Spanish
Lily Tomlin is a tridimensional and unforgettable character you cant keep your eyes off the screen Full review in Spanish
Paul Weitz the cocreator of American Pie wrote the clever consistently funny surprisingly affecting script especially for Tomlin who hearts back with one of the best performances of the year
Grandma is a crowdpleaser potent with Tomlins DNA from the razorsharp wit in dialogue to the way the movie itself speaks to sometimes pointed but necessary ends
Grandma never intended to be more than the feel good movie it is Full Review in Spanish
Wonderfully aggressively feminist a rare crossgenerational portrait of two women getting to know each other amidst a crisis Smart and acerbically funny
Grandma may be a short film but unlike many other films in the genre does not let go at any time thanks to its beautiful story and great performances from actresses Full review in Spanish
Bahranis most accomplished film to date
Its a mesmerizing and disturbing portrait elevated by two strong performances
Audiences will no doubt notice the powerful and important role played by the score from Australian screen composers Antony Partos and Matteo Zingales important and remarkably effective
Worried about how youre going to pay that next clump of bills Then  Homes will bring on long and lasting nightmares
Homes isnt just about chasing the American dream its titular dwellings represent after all but about its destruction
The two powerful lead performances in this tough emotional film are strong recommendations on their own
The rackets and scams it exposes are all real the result of extensive research Its a gripping thriller with good guys and bad guys but everyone in it is a victim including Rick Carver Bahranis achievement in this film is breathtaking
It is smartly if predictably told with a flair for capturing moments of heightened emotions regularly involving people we meet only once or twice
It is perhaps the most compelling film yet made about the global economic downturn and the everyday people whose lives it tore apart
A powerfully affecting modern tragedy
a dramatic thriller about predatory capitalism that cuts deep to the economic bone in the way it reminds us how one persons attaining his dream usually comes at the expense of someone elses
A brutally honest compelling drama about Americas housing crisis
A breathless thriller with expertly shot you are there camerawork and a rising score but it also comes from moments of conflict that are pitched at a very high level Emotions are high lives are at stake and something is very very wrong
Homes is a timely topical drama that packs real punch the deeper it goes into the rabbit hole of lawless capitalization on destroyed lives
A sizzling righton morality story pleading for justice for the vulnerable workers who lose their homes
An electric melodrama  with Andrew Garfield in his best role so far supported by an outstanding supporting cast
Michael Shannon gives an exceptional performance as a real estate broker who is absolutely determined to succeed in a business where most are failing by any means necessary This film effectively shows the human tragedy of home foreclosures
Shannons coldblooded villain is compelling too snarling a string of heartless maxims and giving us a chilling glimpse of American capitalism red in tooth and claw
By the way Shannon Boardwalk Empire is brilliant in the role I truly admire this actor He is going to win the Oscar some day His performance in  Homes should have him a nomination
Sustained rhythm urgent framing and a perniciously overbearing score ensure this second venture into the darkness of a systemic failure will not be forgotten so quickly
Tucker Green certainly isnt shy about testing her audiences patience and while she can sometimes get a bit too enamored with her own moody elliptical atmospherics theres clearly a unique imagination at work here
The main body of the movie is a slow repetitious study of the familys daily life
An intimate earnest and astute depiction of a family dynamic picking up on all of the idiosyncrasies and nuances of a somewhat relatable household full of repressed feelings
However willfully obscured the narrative may be the emotional truth of every moment in the moment is always piercingly clear and punishingly accessible
Its nicely shot superbly acted and extremely frustrating
An impressive sliceoflife drama thats tinged with unease and strangeness
While some might find its conclusion faintly ridiculous buy into the premise and youll experience an unexpectedly joyous finale
A soulful drama that heralds the arrival of a new voice in British cinema
The title and central concept may be ripe with religious allusions but theres no heavyhanded allegorising here Nor does the film ever veer off into soapy drama forget about any shock reveals or intrigues
A mysterious and intimate fable in the guise of gritty social realism simply and powerfully acted
Second Coming is an enigmatic and quietly impressive piece of storytelling
Overall this is a British film of rare ambition and imagination which builds to a final image of heartstopping grace
Heralds the emergence of a major new filmmaking talent in Debbie Tucker Green
The pace will be too slow for some but still if you make it to the end youll come away chuffed
Leaves no doubt that its central supernatural event is  real yet it makes absolutely no case for it whatsoever and refuses to even engage with it
In a highly nuanced central performance Marshall is both engaging and evasive perfectly matching the fluid tone of Tucker Greens enigmatic urban parable
We see a family at its best and at its worst with the grumpy mealtime silences and the playful banter Much of this is down to the firstrate performances from Elba and Marshall
Its an effective thriller that sets out to scare the living daylights out of even the most skeptical viewer and delivers in spades
The Pack wont make anyone forget Jaws  or even Cujo
We learn nothing much about the family members except that outsmarting dogs doesnt seem to be their forte Also not a forte of anyone involved in this project originality in picking a name
Despite the interesting openings the film punctures in its formula the detours from the expected course of events are short and return us firmly to known genre territory
The attractiveness of the package and the steady pace at which it doles out the action more than make up for the character shortcomings in one of the best animalsrunamok horror movies in recent memory
Ultimately these feral dogs pose no danger that couldnt be solved by staying inside boarding the windows and barricading the doors Its not a bad approach to the film itself
Take Me to the River falls woefully short on offering a serious contribution to the history of African Americaninspired music
An often vibrant documentary about the making of an album of the same title seeking to connect generations
Take Me to the River is at its most interesting when zeroing in on the backandforth between musicians of different eras
Not so much a traditional documentary as a musical celebration of Memphis status as what host Terrence Howard calls one of the special places on this Earth
Theres magic in the music and the men and women who make it Maybe they put Mavis Staples at the end because nobody could follow her
Take Me to the River includes just enough history of the civil rights era to lend it gravitas The colorblind recording practices of studios like Stax were an anomaly at the time and are well worth noting
An overdue homage to a city that for close to a decade was home to the second largest black business in America
Theres some talk about the collaborations significance and theres some racially charged history to relate but the real point is to hear these oldsters one last time to remember and maybe even discover what they gave us
With better direction and execution and a more substantial budget and judicious editing it could have been so much more
A must for music lovers
While the work is joyous and respectful the movie is poorly directed frequently decimating pure musicianship to spotlight banal conversations that add little to the overall flow of the feature
It feels like a mishmash effort overall more a home movie than a theatrical release Thats fine If you approach it on those terms you cant help but feel the love too
A moving tribute to a grand piece of Americana
The premise for this documentary couldnt be more stilted and some of the matchups are enough to make you wince But there are a few striking intergenerational moments
While those sessions result in full songs some of the most memorable iconic tunes in music history this film never coalesces into something greater than a collection of mildly interesting pieces
Three of the guitar marvels shown here  Hubert Sumlin Teenie Hodges and Skip Pitts  have died since filming This is a marginal but worthwhile footnote to their legacy
Flawed as it is River reminds us where all the great music came from
Even if the filmmaking loses focus at times the personalities onscreen are consistently engaging and the experimental collaborations yield some toetapping tracks
Take Me to the River is a goodspirited but patchy documentary less about dropping beats and more about dropping names
While you may stick around for the stories the films true draw is the music
In form and content plotting and politics the film is egregiously disguised as a smug inspirational piece but in reality it is the cinematic equivalent of being offered free hugs from Donald Trump
A strange and relaxed film that feels like the Levinson of the good old days Full review in Spanish
Rock the Kasbah hides a much more predictable and tedious story of redemption Full review in Spanish
We stand before a lazy piece of work that includes well established actors and avoids totally ridiculing itself because of its protagonist Bruce Willis being tough Kate Hudson showing off her beauty and an excellent soundtrack Full Review in Spanish
An interesting cast and premise that feels like a missed opportunity Full review in Spanish
Bill Murray is everything in this movie sadly its not enough A weird and failed movie Full review in Spanish
Leaving aside the galloping misogyny Rock the Kasbah just isnt remotely funny or smart
Turns out its ruinously hard to raise a smile at IEDs and arms deals gone awry especially in a conflict thats still raging No doubt its meant to be satire but it feels like profiteering
Rock the Kasbah feels likes the pathetic last wheezes of the Baby Boomer Entitlement Project Bro Division
On paper this could have been excellent as it stands its painful and futile for all involved
Murray has rarely been so charmless in a movie
Singularly fails to charm  and even Murrays on faltering form
A resounding misfire
Its neither funny nor feelgood but its biggest crime is wasting Murray left trying to mine laughs from unfunny circumstances
Unfunny and also casually offensive
Its a strangely lacklustre film thats happy to plonk Murray in a vaguely interesting crisis situation  in this case wartorn Afghanistan  and just hope for the best
Barry Levinsons film loosely inspired by the true story of reality TV show Afghan Star comes at its subject matter from entirely the wrong direction
A dogs dinner of a movie which not even the usually reliable Bill Murray can raise above the level of confused cliched claptrap
Murray is entertaining but the plot seems entirely arbitrary with characters such as Kate Hudsons happy hooker Merci popping up without much rhyme or reason A real disappointment
Murrays performance really is one of very few saving graces in this truly underwhelming piece of cinema
Its a fascinating look at modern journalism  but perhaps not always for the reasons its makers intended
Its endeavor in capturing the global downfall of the media and this downfalls continuing eradication of one of the most respected professions in human history is every inch the hard truth
Mapes and Rather resent the way that the debate gets bogged down in the documents without focusing on the larger issue but the film gets bogged down in just the same way
The casting cannot save a slightly hohum story that crosses the Atlantic badly
The movie is wordy worthy and often bombastic in dialogue with Blanchett the backbone of the story while Redfords careful nuanced portrayal of the nations most trusted news anchor is a study in understatement
Solid but uninspiring
Redford is marvellous but even Blanchett cant do anything with the screenplays messy rendition of Mapes who too often plays like a mushy romcom heroine
Blanchett is terrific as Mapes juggling hard journalism and home life serving breakfast to her son while fielding toughasnails phone calls
That generic title obscures a surprisingly complex exploration of the reallife events surrounding the fall of iconic American newscaster Dan Rather in
Anchored by a blazing performance by Cate Blanchett as the resolute Mapes and a studiedly cooler one from Robert Redford as legendary US newscaster Dan Rather Truth delivers a gripping account
As a case study in journalism and ethics the film is intriguing As drama it stutters
Truth is well made but a little too pleased with itself
A captivating real life drama thats up there with other great films about journalism Full review in Spanish
A well built script that balances the professional work aspect of journalism while also showing how this affects their personal lives Full review in Spanish
A good drama about how to handle an editorial department when there are interest conflicts with the government Full review in Spanish
The movie fails by its own premise why do we need to feel empathy for two journalists that screwed up no matter how you feel about Bush Full review in Spanish
Predicatable old and conservative using the old strategy of manequism of a true story Full review in Spanish
Sadly Vanderbilt isnt Fincher and neither is Pakula his work might be good but it lacks personality Full review in Spanish
A great film that surprisingly didnt make a lot of noise hopefully time will give it a place that praises all its virtues and great performances Full review in Spanish
Based on true events the film stands out because of the great performances by Blanchett and Redford Full review in Spanish
A heartfelt and earnest homespun comedy about the family ties that bind us all together
A novel setting surrenders to a seriously corny rural romance with not nearly enough laughs to put it over
Good nature counts for something but its all so comfy you may fall asleep
Apparently too close to the story to recognize how ill suited she was to translating its charms to the screen Trigiani has emerged with nothing but corny stilted Americana like something Garrison Keillor might burp out on a really off day
The warmth of spirit behind this project allows its missteps to be mostly forgiven
Director Adriana Trigiani adapting her bestselling novel delivers the hackneyed material with good cheer eliciting bright performances from an excellent ensemble cast
Nothing feels believable in Big Stone Gap a bungled charmfree look at smalltown life in the South in the late s
Trigiani doesnt push the material into any energetic directions but she does capture the sway of the town better with atmosphere than dramatics
The plot itself has little momentum and what should feel dramatic instead feels inert
Its tempting to say Big Stone Gap is greetingcard pretty and sweet but that doesnt quite fit because these days greeting cards seem to have developed a bit of an edge Big Stone Gap is edgefree
That the creaky romantic comedy Big Stone Gap attracted such a large ensemble of talented name actors only makes this countryfried turkey feel like an even more notable miss
Unpretentious and it goes down easy like sweet tea brewed by sunshine
Sure the script can be simpler than a diner menu And at times the nostalgia seems manufactured like the goodies at a Cracker Barrel gift shop butBig Stone Gap proves to bea nice change of pace from the summer popcornmovie season
The movie ambles along amiably enough for a while its better if you are a fan of one or more members of the cast
Brutally mediocre
By the time we get to the final borderline ludicrous moments in Big Stone Gap I wasnt even surprised by the overwhelming sappiness of it all
Flat comedy about smalltown s life lacks laughs
Bleeding Heart may deliver some much needed catharsis but its ultimately a hollow film that isnt concerned with consequences or the echoing cycle of violence just vanquishing the bad guy reclaiming a dime store sense of freedom and not much more
Bleeding Heart is a fascinating film though a tad predictable If nothing else it recognizes the underappreciated work of Jessica Biel and Zosia Mamet and places writerdirector Diane Bell at the forefront of exciting indie filmmakers
Ironies are sliced thick and violence offers little surprise but the feature manages matters of the heart quite well even with little narrative to explore
Good movie bad title
Bleeding Heart shows that all the asanas in the world cant change a man with hate in his heart
Biel turns in a fine performance as the unlikely violent hero but Mamet excels in a role far different than her Shoshanna on HBOs Girls
Despite its nonexploitative approach to its subject the film is too schematic and obvious to have the desired impact
Theres a core of a good idea here but the film itself is pale and anemic
It pales in comparison to the recent epic saga tweeted by Aziah King about two strippers unapologetic road trip from Detroit to Tampa Fla that went viral on the Internet
Though violent and thrilling Bleeding Heart is first and foremost a film about healing
There may not be much surprising here but this is a smartly sensitive depiction of abuse and redemption that never descends into caricature
A potentially intriguing examination of family bonds and abusive relationships is relegated to the background in this generic revenge thriller
If anything this reminds me how much Id like to see both Anderson and Biel  hell even Mamet  in a better film than this
Bleeding Heart takes subject matter deserving of mature thoughtful treatment and distorts reality into a series of soap opera clichs
Why does Michael Shannon get the best written part in a lesbian drama
May be a familiar DavidversusGoliath tale it is also an inspiring and hugely emotional experience due in large part to the powerful performances
Freeheld has a very worthwhile subject and its Alist liberal actors do their best
Worth catching
Sollett mounts a fullscale attack on the tear ducts as he rolls out scene after scene of inspiring oratory heartrending illness and fullon liberal dogooding And it works like gangbusters
Moore gets to wear a striking wig but has no dialogue that makes you look twice And the more Stacie cries over her saintly lover the more guiltily conscious you become that your own eyes are waterfree
Freeheld is earnest and wellintentioned but often feels like it is trying to teach us all a lesson It works best in the tender emotions of one woman facing the awful reality of losing the woman she loves
Perfunctory direction renders dull what could have been a compelling story about the need for equality in a job where decades worth of gender discrimination has been compounded by systemic homophobia
Flat and lacking the archly deadened attributes of Todd Haynes Carol a quartet of Worlds Greatest Actors work hard to animate a timid movie
An acceptable drama that had more potential in its noble story Full review in Spanish
It falls in the same cliches about tolerance and doesnt really add anything new or original to the subject Full review in Spanish
The movie manages to put its ideas out there maybe not in the most subtle way but it gets the discussion going and thats the idea in the end Full review in Spanish
Una pequea joya bien actuada bien contada con un elenco extraordinario aunque muy modestamente producida
Even if its a small film the performances and the relevance of its story save it Full review in Spanish
The performances are top notch and the case is a worthy portraying example of human rights addressing issues of equality tolerance and homophobia Full Review in Spanish
A manipulative drama with good intentions that ends up being a cartoon of what it wants to say Full review in Spanish
Thank God there are good performances that work but dont save in this movie Full review in Spanish
Great themes and story and a spectacular cast however its predictable and it takes the easy way out Full review in Spanish
In the quest to make An Important Film director Peter Sollet Nick and Norahs Infinite Playlist Raising Victor Vargas has produced Teachable Moments instead of complexity or authentic emotion
Freeheld is ultimately a film without a soul without character full of clichs to a genre that simply does not need them Full review in Spanish
It offers a CliffsNotes encapsulation of Edgar Allen Poes most enduring works for viewers unacquainted with them
It has its moments and will be a welcome respite for any middle schooler sitting through a boring lecture But if we were ever asked if we wanted a second viewing wed have to quoth the raven nevermore
In spite of the common source material and tone of oppressive psychological horror these shorts feel like they could be the work of five different people
Its no substitute for Poes atmospheric writing but its a pretty good introduction to it
Despite working from extraordinary subject matterEdgar Allan Poes various tales of the macabretoo many entries in this animated anthology are disappointingly ordinary
The voices of the late Christopher Lee and Bela Lugosi reading from the text isnt quite the same But Garcia compensates with a different animation style for each of the five stories
The best sections reverberate with the power of Poes words As such they serve as apt tributes to yarns that will always be more formidable on the page than on the screen
Just in time for Halloween comes this singular tribute to Edgar Allan Poe from veteran animator Raul Garcia
Extraordinary Tales reminds viewers that animation can enable an artist to realize an individual vision even on a limited budget
With its weak framing story and hastily assembled formatting Extraordinary Tales simply is the sum of partsnot a whole
Poes storytelling gift is so timeless and the voice actors assembled so captivating that Extraordinary Tales cant help but work on some level It just never quite rises above that faint praise
Garcia brings these classic Poe tales to life with the notable aid of atmospheric score by Sergio de la Puente
Extraordinary Tales leaves us wishing for more more running time more of Poes material more of Garcias macabre animated magic
The fact that an author who died  years ago continues to be a draw for moviemakers says something about Edgar Allan Poes imagination And heres a feverish new animated Poe feature  released just in time for Halloween
the definitive works of Edgar Allan Poe on the big screen
Five stories by Edgar Allan Poe get varied treatment in this striking but inert animated compendium
Unsure of its targeted demographic Tales straddles the tonal line flipflopping like a fish left on the dock
Poes creepy unevenly adapted tales arent for young kids
So much is made of the Marvel Cinematic Universe but what DC is doing with these animated features is much more organic
Batman Bad Blood assures everything remains status quo in terms of another outstanding actionpacked and entertaining effort starring the Dark Knight
Superviolent superhero mayhem with a complex story
highlypolished garbage from almost the beginning with no relief from its elderly ministrations all the way through to the end
An impeccable proposal with a nostalgic tone that almost touches ingenuity Almost Highly recommendable Full review in Spanish
As gripping and as smart a piece of entertainment as youll find this year
For the most part Bridge of Spies is a sober reflection on America actually living its values and living up to its promise
Steven Spielberg takes on the Cold War with a stately sentimental thriller that gurgles along with quiet intensity only occasionally finding a real spark of energy
Bridge of Spies is a warm portrait of a friendship an excitingly intricate story of realistically scaled suspense and a visually ravishing and lovingly crafted rendering of midcentury America where how you look matters much less than what you do
A fascinating story where the interactions between smart and unpredictable characters is the main focus Full review in Spanish
With Hanks on superb form Spielberg delivering a visually stunning and surprisingly feelgood thriller Bridge of Spies absolutely satisfies
Simply the best film Spielberg has made in decades Avoids the sentimentality of mostperhaps because of the inclusion of the Coen brothers in the screenwriting team Their best work in a decade as well for that matter
The strength of the film lies in the bond developed between two men A bond built on mutual respect and common understanding in a time when trusting the enemy could get you killed
With a mostlycompelling story and a particularly excellent performance from Oscar nominee Mark Rylance Bridge of Spies is an enjoyable Cold War tale that manages to bring enough tension to its narrative to make it a worthwhile journey
Steven Spielberg goes into Stanley Kramer mode for Bridge of Spies a socially conscious tale of touchandgo diplomacy at home at the office and on the global stage Bluray
A lesson in how to direct movies and proves why Spielberg is matchless in his ability to place the camera as a narrator who tells a story through images Full review in Spanish
The master of multiplexes certainly found a fantastic tale for his st feature
With an overriding penchant for nobility above all Bridge of Spies has a welcome straightforward charm despite its ultimately understated impact
Bridge of Spies is a quality film featuring loads of commendable work but Spielbergs glossy and exceedingly upbeat take on the material might make it more of a crowdpleaser than an Oscar contender
Locates the sweet spot for so much superficially disparate recent Spielberg morally engaged and reflective yet playful and funny and loose  and with the formal beauty of one of American cinemas greatest mainstream artists
Bridge of Spies is an inspiring drama about a man like any of us who struggled to do the right thing Full Review in Spanish
Its reminiscent of a Clint Eastwood film solemn with no provocative scenes filled with images that remind us of what America was built on Full Review in Spanish
Any doubt that Steven Spielberg is a latterday Frank Capra can be put to rest Bridge of Spies is a celebration of an Everyman empowered by Americas cando spirit of justice and decency in this case doing nothing less than saving the world
Dr Cabbie does something interesting It takes a rather serious subject  the fact that immigrants bring vital skills to Canada that are cruelly wasted  and milks the issue for its comic possibilities
A prime example of a movie with a meaningful message that fails to deliver on its potential
Its still a distinctly Canadian movie delivered entirely in English and lacking the lengthy and wellchoreographed dance performances that have become a trademark of Indian cinema
Functioning as light satire Dr Cabbie never quite digs deep enough
The Prophet gets a Disney makeover thats ultimately even more disturbing than it is cloying
If you are a fan of Gibrans work this film is recommended for those sections just be prepared for some schmaltz to go along with the transcendentalist philosophy
Simply put this movie is gorgeous from start to finish
What makes The Prophet worth watching is the animation
Neesons gentle sonorous voice perfectly underpins these visual extravaganzas
an acquired taste but a frequently powerful examination of spirituality and relationships for those in the right mood
As a visual experience many of the animated sequences are beautiful But without the focus of a traditional screenplay the story drags a bit and even feels tedious
The animators are uniformly talented but wildly divergent in styles
Gibran encouraged his readers to savor every drop of life even the bitter ones and viewers should relax and savor every panel of this film
The simple Disneystyle look of the framing story merely feels like a delivery system for those dazzling depictions of wisdom and the most memorable lesson is that there are so many more glorious ways to tell an animated story than Disneystyle
The real star of The Prophet is the animation The visuals make this a film a craft of art that also has spiritual uplifting of words
Too childdirected and broadly characterized the commercialminded Disneystyle highstakes is a padding out of ponderingness to ponderousness This adaptations more posey and cutesy than questionposing or poetically profound
Theres no room in theatres right now for movies that just want to ilustrate text no matter how deep and poetic it is Full review in Spanish
A film with a strong message about achieving what seems imposible Full review in Spanish
An interesting cinematic experience that mixes poetry music animated sequences and relatable characters Full review in Spanish
An affectionate and endearing proposal that dignifies the human spirit Full review in Spanish
The fact that the film abuses some narrative elements make this a tiring tedious and at moments a boring film Full review in Spanish
As it often happens in these type of films the end result is uneven but there are a couple of outstanding segments Full review in Spanish
An interesting project but it misses its target completely Full review in Spanish
An interesting experiment that compiles different styles of animation in a single film Full review in Spanish
An obsessive friendship between two teenage girls unfolds with equal amounts of tenderness and terror in Breathe a modest but acutely observed and affecting adolescent portrait
Theres no doubting Laurents skills as an actors director
Director Mlanie Laurent conducts this minute drama at a leisurely pace getting terrific performances from the two leads
Its only the second feature for actressturnedfilmmaker Laurent whos probably still best known in the US for her avenging cinephile in Inglourious Basterds yet Breathe crackles with the intuitive control of a seasoned auteur
Breathe is also about the great intimacy between girls that is an exclusively female domain the closest boys can achieve is a bromance but dont even think about cuddling and stuff bro
I can tell you that Ms Laurents direction is astute and economical that both of the films young stars give fine performances and that Breathe is a very good title for a film that ever so gradually takes your breath away
Josphine Japy and Lou de Lage are painfully genuine as two very different but equally lost young women
The entire piece is precisely woven together from script to performance to execution and the result is a chilling study of emotional annihilation and its aftermath
These are extraordinary performances and considering Frances way of nurturing female talent Japy and de Laage could be at the dawn of year careers Theyre that good
Nuanced sensitive and unflinching
The subject is the kind of intense frenemy relationship only possible between teen girls and Laurent guides her young stars to naturalistic performances that convey the joys and miseries of adolescence
A harrowing examination of teen angst as one girl navigates the highs and lows of an unstable friendship
If you think the emotional volatility of adolescent female friendships is a uniquely American phenomenon Mlanie Laurents potent psychological drama is evidence to the contrary
In some ways it plays like a horror movie in other ways its almost a documentary
Laurent has an excellent eye for shot composition and cinematographer Arnaud Potiers crisp photography aids the director in creating an enviable set of gorgeous and memorable shots and the film is consistently visually compelling
Breathe offers ample evidence of the growing confidence and skill of Melanie Laurent as a director
With boys  and men  on the side the females of the film anchor it with ease once again proving that a good drama is dependent on everything except the actors genders
Breathe is a deftly observed look at teen power games and how closeness can turn to animosity in one miserable night out
A modest but keenly observed comingofage drama about an average suburban teenager
The examination of the unknowable and undeniable power and pain of a broken friendship on display in Breathe is truly something to behold
Its Mulligan to whom the biggest plaudits belong But the rest of the film cant quite live up to her
Some of the most interesting aspects of the suffragette movement are glossed over without examination and you get the feeling that while the film is quite decent an opportunity for something outstanding has been missed
A maximum of four minutes on screen by Meryl Streep give the story an even more special meaning Full review in Spanish
Mulligan transmits all the emotions of her character and the audience  especially women  achieve to connect with her Full Review in Spanish
An astonishingly tedious drama
A very conventional and safe film about a very singular and brave event in the postvictorian era Full review in Spanish
With a plot and characters based significantly on historical events Suffragette does succeed in highlighting the or at least a systemic oppression of women through manipulation humiliation and violence
Its Mulligans picture all the way though and once again she shows the depth and range that makes her one of the leading British actors of her generation
Suffragette highlights how much the world has changed for the better over the past century However it also highlights there is still much room for improvement
Suffragette is vividly made enough that even if it glosses the full breadth of the suffrage movement it still introduces viewers to the history of womens activism and gets them intrigued enough to want to learn more
The problem is that it is simply a politically correct film and could be much more Full review in Spanish
An earnest melodramatic and probably necessary history lesson
An interesting story sadly we have too many loose ends at the end Full review in Spanish
An important film serious well crafted and relevant Full review in Spanish
An overly straightforward plot ditching any complexity for an awkward simplicity but thankfully the cast is topnotch and the characters are compelling
One of its most appreciated accomplishments is its ability to paint the heroic events of a social movement that happened more than a hundred years ago with a modernday relevance sure to resonate with audiences of today Sadly no one will see it
Sarah Gavrons focus on one character helps lift the film above slogans and marches and makes it more personal But when the fight becomes more of a war we lack the attachment to the movements biggest losses
This is an urgent persuasive if cloyingly conventional history lesson with a story that Hollywoods barely touched on before
This is a beautiful film not just in its narrative but also in its astounding cinematography and production design
Suffragette barely delves into the inner workings of its political movement opting instead for an admittedly sympathetic everywoman protagonist
A lot more fun than many heftier supposed romcoms thanks to the timing and chemistry of its leads
There are enough twists turns and strange little bumps in the road to make this welltraveled journey to romantic comedy bliss surprisingly worthwhile
Larkish and nicely confident Brit romantic comedy enlivened by sparkling performances and a tangy quite original and observant script
Even though you know how things will turn out its amusing along the way
MAN UP is that rare romance fueled comedy that will ably entertain both men and women
Perhaps the whole thing is intended as postmodern  But its not funny ironic its not funny straight its not funny anything Its embarrassing from start to finish and Pegg looks deeply uncomfortable in it
Im a sucker for movies where smart people fall in love Smart wellwritten people anyway And about  per cent of Man Up is that sort of movie This is enough
Its definitely not Peggs finest professional hour but as fizzy romantic comedies go Man Up has its share of surprises and enthusiasm for the material
Bell and Pegg both terrific comic actors mine small gestures and reactions for laughs But theyre at their best when theyre talking and they talk a lot in Man Up
Aside from the silly title and disappointingly pat ending even the romcomaverse will find something to love about Man Up
Man Up has a couple of bits that dont quite work and the ending is just silly but for the most part Pegg and Bell carry this thing through sheer force of personality
Honestly it would be nice if on occasion a film like this at least tried harder than making the main character somewhat dissatisfied and assuming that would be enough to get everyone identifying with her
Both actors stay sharp through some pretty degrading moments and if Palmer and screenwriter Tess Morris are bent on serious buttonpushing in the closing scenes at least they garnish it with playfulness and wit
Bell and Pegg are utterly defeated by a screenplay that favours plot over characterization to an almost surreal parodic degree
Formulaic romcom has lots of sex talk drinking
The kind of romantic comedy that reminds you why the genre continues to wheeze and sputter and die
Lake Bell and Simon Pegg make for an amiable pairing in a hugely enjoyable and fastpaced comedy
Man Up benefits largely from the hilarious dynamic between Pegg and Bell and the humor alone is strong enough to recommend the film
Its all very gener ic and predictable but not in an entirely bad way Man Up has its charms and itll raise a smile perhaps even a chuckle or two if youre looking for a nofrills romcom
For all the romcom conventions of the film and there many it has a lowkey charm and an adult take on expectations disappointments and missed opportunities
If the purpose of a horror movie is to be horrific then Hellions gets the job done
An unpleasant muddle of the visceral and the abstract
A nifty way to get freaked out for eightytwo minutes Hellions is Alice in Wonderland if Alice in Wonderland was about unwanted teen pregnancy
Bruce McDonalds latest is a genrefreakonly affair that even at  minutes feels like a joke that takes much too long reaching its punch line
The thrills diminish very quickly as Pascal Trottiers script runs out of ideas collapsing into incoherence in the final reel
Hellions is unsettling but in all the wrong ways
McDonald does manage to craft a few striking visuals that take advantage of holiday elements like pumpkin patches and costumes but other shots border on cheesy and overblown
Even at  minutes Hellions feels padded out running out of plot after an unsatisfying expository scene halfway through and relying on visual gimmickry to make up the rest
When it comes to the visual scheme of Hellions less would have equaled more There is enough good left to savor however to prove more treat than trick
As the terror shifts from suggested to explicit the eeriness wanes and this supernatural story becomes muddled
Hellions is art book horror something to flip through but never truly eerie or scary
A halfcocked fusion of Rosemarys Baby and a home invasion flick
A perfect light midnight movie that should go great with light drinks good company and a warm blanket
Hellions is a bigger Halloween bust than the house that gives out dental floss to trick r treaters
Some of the horror tropes being played with here  Rosemarys Baby is a clear touchstone  are staying up way past their bedtime
In its best moments Hellions is interesting and perplexing and worth soaking in in the sense that one would admire a Hieronymus Boschinspired painting But without at least one foot on the ground it doesnt inspire fear
This creepfest initially serves up some good horrorflick fun but seems to lose the plot as the wind kicks up and the sky changes colour and the chase goes on and on
The scare factor wears down pretty quickly
Its all fairly preachy nonsense and never as cerebral as McDonald thinks it is
No matter how good these aesthetic tropes are at cultivating fear however the content inevitably falls prey to nervous laughter before sadly devolving further into eyerolls
Apparently this is a sequel to Chiens Zombie  which according to reviews is even nastier Count me out
a problematic cavalcade of uncomfortable male fantasies and their punishment but there is something to its anythinggoes insanity that proves irresistibly infectious especially at the midnight hour
Its unbelievably awful
Well manufactured but no strokes of genius here the best thing of it is that Vin Diesel and Micahel Cane are in it Full review in Spanish
As one random and chaotic supernatural brawl follows another the decision to keep the lead villain on the sidelines becomes increasingly puzzling Why make a movie about a witch hunter without a memorable evil witch
If youre willing to put the faults in the script aside youll have an entertaining movie with good action sequences and humor Full review in Spanish
Not only has Diesel plopped the mantle of Cage on his shoulders he seems to have borrowed one of Cages justly celebrated hairpieces
Esentially consists of everyone involved hoping this is successful enough to make sequels
The plot is a bewildering blunder expecting you to jump right into it even though youre never sure you understand the storys progression
Vin Diesel delivers on his leading man role in this action packed film Full review in Spanish
Its not even a letdown because you could see from the start it was going to fail Full review in Spanish
It is terrible It is fabulously entertaining
This is all too bland too serious The characters needed a significant injection of charisma
An abortion and one we can blame on the success of the dimwitted Fast and Furious hits
Hohum except that its all set up for a sequel and I wouldnt bet against it Vin Diesel is a hard man to kill
Vin Diesel makes a bid for yet another franchise with a supernatural action romp thats both deeply ridiculous and enjoyably entertaining
The special effects are poorly designed and the laughs are readily found though probably not intentionally
A yarn that is just entertaining enough for you to walk out of the theater satisfied but not enough to have you begging for more
The Last Witch Hunter has some great production design and digital effects but the movie directed by Breck Eisner is an interminable bore which foolishly places the weight of the film on Vin Diesels broad shoulders
The greatest disappointment of all is that the ending promises an unfortunate sequel That comes off like a threat
Exposition backstory and actiondialogue clich are all tossed into the pot and stirred up real bad A hocuspocus hodgepodge of balderdash and humbuggery The only smells wafting from this cauldron are the odour of silliness and stench of stupidity
Picture Harry Potter for Emo kids No wonder theres already a sequel in the works Brood on Vin
It may not be a good film but its made  or at least acted  with a bizarre affection that goes a long way toward being endearing
An engaging provocative and pertinent piece of cinema
Too light too silly
A smartly written cheekily engaged glibly enjoyable political satire
Theres nothing new about the political smokeandmirrors the BullockThornton relationship fails to convince and the inevitable idealistic resolution feels unsatisfying and unearned
Even the title is unwieldy and that sets the tone
With characters speaking almost exclusively in soundbites anecdotes proverbs and quotations its like being cornered at a convention of fortune cookie fillers
Billed as a comedy drama Our Brand Is Crisis sits on the fence of both genres without really committing to either
Bullocks usual audience might be turned off by her characters dark cynicism but as meaty political satire this stands tall in a nearempty field
In the end though in spite of its hardhearted cynical veneer David Gordon Greens comedydrama turns into a strangely softcentred and manipulative affair
Bullock is hilarious as well as unsettling in the lead
The movie loses its cynical nerve with an utterly phoney finale that ties itself up in conscientious knots
The plot is dull and a few illjudged moments of comedy just seem weird
Bullock is great fun as the brash bossy empress of spin doctors and has a feisty chemistry with the snakelike Thornton
A protagonist who revels in the sheer cynicism of her job gets a sentimental redemption out of nowhere Sandra Bullocks comedic chops are undercut by it
Chaotic storytelling and uneven tone get in the way of an outcome that should really enrage an audience
The set up induces hopes that a screwball comedy with real buzz will be in store something along the lines of Preston Sturges hilarious  movie The Great McGinty Sadly these hopes are soon dashed
A regular dramedy that doesnt boast great performances or a great story Full review in Spanish
Bullock does her best to render the emotional turmoil authentic but the script doesnt do any favors by moving from laughoutloud to heartfelt soulsearch
Still there are important lessons about political manipulation and theres fun to be had watching Bullock and Billy Bob seething at one another
Promising to be a biting satire but succumbing to the juvenility it loathes this film feels like its still teething unable to chew on a political jawbreaker
This teen horror fantasy based on RL Stines bestselling childrens books is made with an anarchic glee that in its better moments rekindles memories of Joe Dantes zany tongueincheek movies of the s
Its great fun combining live action with topnotch computer animation and I enjoyed it enormously
Goosebumps is a wicked delight packed full of spooks and scares that should have adults jumping out of their seats almost as often as little ones
The film is pleasantly enjoyable throughout combining likeable characters with knowing gags and lively set pieces
While only providing sporadic entertainment for the overs itll keep the tween set amused to the bottom of their popcorn buckets
Mixing the action comedy and horror from novelist RL Steins books into a familyfriendly package this lively romp is entertaining enough to amuse the audience even when it veers off the rails
Goosebumps speeds up helterskelter in the second act and never loses its momentum providing an eyepopping feast Cool visuals aside its also full of intertextual content
At times as lumbering as the mantis director Rob Lettermans semifamilyflick has a few giggles but ultimately feels strained
A pacy quirky and thoroughly entertaining family adventure
Its a little scrappy but theres more than enough carnivalesque mayhem to keep monster fans young and old diverted
Goosebumps combines cynicism with sweetness If you can get past the slow start woefully blank hero Zach Dylan Minnette and hitandmiss special effects youll be charmed
Goosebumps has a clever mixture of monsters mayhem comedy and romance that will remind parents of childhood favourites such as The Goonies Gremlins or Jumanji
The picture doesnt have the satirical edge of a Gremlins but it combines a sharp funny script with some emotional substance
The film is set up as a great entertainment for children who do not avoid the early fears Full Review in Spanish
Jack Black gives one of his more enjoyable performances playing the writer as a Frankenstein figure  the creator of creatures that he cant control
Black is in good form as the enigmatic Stine but it is Minnette as Zach who impresses most
A goofily entertaining horror romp full of tweenfriendly chuckles and scares
Makes the Jumanji reboot superfluous
Goosebumps focuses on keeping the dialogue snappy the action plentiful and the CGI convincing as it quickly moves through scenario after scenario
The triumvirate of young actors are charming but inevitably its Mr Black who steals the show with his patented hilarious bellowing Will todays creepypasta kids still enjoy such silly fireside supernaturalism We do hope so
Its steadily paced with more longheld contemplative shots of natural beauty than bursts of impressive action and an elastic dreamlike sense of passing time
A breathtaking work of art which revolves around a haunting female lead
Its cinematic night nurse
Episodic scenes of violence ensue but fans of martial arts should bed in for something more slow spare and mysterious than the likes of Crouching Tiger Hidden Dragon
Its a wonderful thing to behold Its also a frustrating beast to absorb
A film of surpassingly exquisite visual beauty
The director pays exhaustive attention to sound editing and miseenscne
May just be the most tedious and enervating kungfu movie ever conceived
A film that gains greatly from second or third viewings
The Assassin unfortunately is more still life than cinema
Magical and utterly mesmerising
A measured elegantly choreographed tale with Shu Qi convincingly capturing the steely determination and conflicted heart of the lethal blackgarbed assassin
The Assassin targets cinematic pleasure and kills it stone dead
When you spend most of your time either admiring the look of the film or trying to work out what the bloody hell is going on theres precious little time left for enjoying it
Still waters often run deep but here you cant see past the glossy surface
I gather there is something to this film that I lack the patience to appreciate in its storytelling but I cannot for the life of me meditate on the occurrences like the film seems like it wants me to
As beguilingly elusive as it is exquisite
The Assassin is a singular vision realized with absolute mastery of style and a lightness of touch thats to die for
Its not an easy film to follow not because it is especially complicated or labyrinthine but because it is abstracted by Hou into a flow of gentle beautiful moments punctuated by bursts of violence
Intriguing at times because of a maliciously dosed ambiguity The Assassin is a really novel film even among HsiaoHsiens own body of work Full Review in Spanish
Ches mission to lift his father up  intimately demonstrates how health care education and supportive housing  help a motivated man gain confidence and his life
In My Fathers House doesnt spend much time on politics examining why rates of fatherlessness are so high among black American families but it doesnt need to
The movies message if any If youre a successful rap star you might want to think twice before returning to the ghetto to track down the deadbeat dad you never knew
The result is a film whose exploration of responsibility and addiction will interest viewers whove never heard of Smith
In My Fathers House could not have come at a better time
Has vulnerability and a few laughs But a story about reconnecting shouldnt seem so uninterested in filling in the gaps
In My Fathers House offers lots of interesting raw material but it could use a disinterested observers remix
Though it brings limited insight to the problem of absent fathers in the black community In My Fathers House tells a story of loneliness abandonment anger and joy that all can relate to
Whats powerful and interesting about In My Fathers House is the way so much goodwill and urging are tested in Ches personal life
Watching Mr Tillman as he cheerfully embraces sobriety to please his son and is rebuffed by his former street pals for doing so we see a poignant meditation on how our expectations of loved ones can become a burden that not everyone can carry
Its neither as hopeful nor heartbreaking as it might first appear
A brave personal introspective exploration of a phenomenon that has no easy solution
Thomas and Rose ricochet from one boneheaded depressing escapade to another and by the end you may start to feel as eager as the mobsters theyre tormenting to get rid already of these babbos
Few films are better titled than The Wannabe a portrait of a Bronx kid who would have done anything to be part of the mob world he knew from the movies
Arquette shines but this unmademan mob movie lacks the energy and brutally dark humor of Rob the Mob which was about exactly the same story
The film is less a revisionist take on the circumstances of John Gottis  indictment than a tedious love child of Bonnie and Clyde and Goodfellas
The mix of callous humor and romantic doom doesnt always hold up but in its best moments The Wannabe finds real spikiness in the pitfalls of antihero worship
Mr Piazza offers a persuasive portrait of decline but it is the crumbling beauty and flailing hopes of Rose that resonate Ms Arquette comprehends the character inside and out and her aim is true
Its taste in antiheroes doesnt inspire fascination just a desire to see a different movie
The Wannabe is too derivative for any insight offering little more than tired tropes and bad accents
Theres frippery and flummery a la Almodvar and Hitchcock two of Ozons favorite directors and he also cites Wilders understanding of drag And not to forget Ozons love for Fassbinder the late rapidfire genregobbler
Its fun to watch The New Girlfriend the way its fun to drink a glass of Champagne and about as memorable
An oddly sweet film gentle and genuine but its also aware of murky psychological spaces pushing focus on clearing confusion not sensationalizing the obvious
The New Girlfriend is a funny delicate subtly flavored dish deliciously prepared by Franois Ozon  idiosyncratic and unforgettable
Duris in drag looks like a refugee from the New York Dolls but his character aspires to nothing more than life as a bourgeois pearlclad mom
Its less than it shouldve been and a little mild But Ozon makes it glide with confidence in or out of heels
Duris creates a sensitive and moving character a confused soul lost without his wife who was the anchor of his life
What is surprising is how conventionally the screenplay begins to wrap up this story and how blatantly manipulative its methods are in order to do so
Ozon is on typically slippery teasing form with this sly and playful tale of female friendship with a twist
A swirl of gender confusion The New Girlfriend tosses all the heshewe question marks it can find in a tale of contemporary confused sexuality
Its never clear what the directors intentions are the premise looses strenght and it falls apart thowards the end Full review in Spanish
The acting is remarkable in a movie that does not pretend to be something else than what it truly is approaching the subject of confusion between genders with originality Full Review in Spanish
The movie is a revision of both the limitations of the upper classes and their completely outdated morals Full Review in Spanish
Simply magnificent Full review in Spanish
A solid script full of surprises and great performances Full review in Spanish
A strong twisted but perfectly written directed and acted film Full review in Spanish
While many gay and identity films wallow in weightiness Ozon has always rejected categorizing cinema in the same way that he has embraced sexual fluidity
Arguably reminiscent of camp Ozon entries like Angel and  Women this has good enough work from Duris but is truly saved by the lovely and lively Demoustier
an intelligent investigation of hearts that dont know quite what they want
A strong thematic undercurrent pits bourgeois social conventions against authentic selfdefinition and ultimate freedom to live without shame or undue social limitations
Director John Wells deftly navigates the trajectory of the story helped by Rob Simonsens frothy score
This drama about a worldrenowned chef seeking redemption in his personal life features a fiery performance by Bradley Cooper compromised by a predictable script that doesnt have the right mix of ingredients
Burnt should have been a spicy treat Instead it comes and goes like so much fast food leaving you hungry for a latenight DVD snack
It is hard to find the characters preoccupations anything other than ridiculous however divine the food looks
Anyone who makes an informed decision to watch Burnt might want to bring a bingocard of dramatic cliches to tick off during the runtime
Its intended to be a tale of redemption but Adam doesnt have a lot of redeeming qualities
This halfbaked dramedy about cooking feels more like a lackluster television pilot rather than a featurelength film
Cooper delivers the goods right enough with help from Daniel Bruhl as his business partner and Sienna Miller as his souschefcumloveinterest
It makes for a pretty good show Cooper cooks up sweet and juicy in just about every role he plays so watching the him sizzle on the griddle of shortorder Shakespeare set against the backdrop of Europes fine cuisine is a ready pleasure
a missed opportunity that ultimately feels like two movies ungainly stitched together at the middle ie its both an engrossing cooking drama and alltooslick character study
Burnt is plated beautifully but lacks the complexity of flavor it so desperately wants to achieve
Strong characters help hold the attention as this overcooked drama develops but in the end it feels so concocted that its difficult to believe
This story of how a troubled exile achieves healing through a creative partnership with a woman equally devoted to their shared craft is familiar ground    Burnt could be retitled Silver Linings Cookbook
Built around a strong performance by Cooper Burnt is a story of redemption
Coopers performance helps lift the character but as is often the case with a mediocre script and story its not clear what exactly this movie wants to be Youll enjoy the greatlooking movie star The comeback story Not so much
Halfbaked tepid underseasoned  all these culinary putdowns are deserved for a film that asks us to root for a broken man with nothing to lose then hands him redemption on a silver platter
A light entertaining and well told film you can enjoy without any complications especially for those who enjoy great food Full review in Spanish
What is most noticeable in this film is that in the  minutes there is nothing that appeals to the viewer Full review in Spanish
Bradley Cooper is a chef looking for redemption in this little successful dramedy Full Review in Spanish
Despite the amount of delectably photographed dishes the drama in Burnt unfortunately feels undercooked and watered down
Long after youve seen the film youll remember the wonderfully nuanced work of the cast particularly Ms Hawkins
Parents and teachers of all the Nathans out there may be inclined to give A Brilliant Young Mind the full four stars for the rest of us its small smart and satisfying
A Brilliant Young Mind is not only warmhearted but offers surprises in a symmetrically crafted script and a handy rebuke to the compounded hearttugs of another recent tale of genius in the world The Theory of Everything
A poignant story with an accomplished cast
A beautiful movie wrapped around a moving story  one that should not be missed
Though billed as a boygenius story A Brilliant Young Mind is really a tougher thing a movie about the complex emotional lives of autistic children
stock characters are part of the toolkit for writing melodramas but they can be more or less fully realized and in this case Matthews and writer James Graham have gone for less
The path may be predictable but the priorities of the screenplay by James Graham are unexpected
Yes this movie is as sugary sweet as it sounds but director Morgan Matthews uses his documentarian eye and does a good job of showing us the world through the eyes of a remarkable young man
Bungled ending but a great stylish brilliantly cast film about math prodigy
well acted but tugs too eagerly at the heartstrings and relies too heavily on manipulative melodrama that tends to trivialize its protagonists plight
It is a fully realized love story of rare spirit the sort of deeply affecting film that can make you laugh until you are near tears then drive you close to bawling
Im sort of amazed that movies like this keep getting made which makes me the stupid one
A Brilliant Young Mind is an inoffensive film looking to please a broad audience and while it achieves its goals theres always a sense that it could have done more
Hardly breaks new ground but its a watchable entry in the tortured young genius genre
It does for Aspergers syndrome what A Beautiful Mind did for schizophrenia lots of drama but not much science
Witnessing Nathans special powers as his dad called them may give the film its spectacle but its soul is in the relationships Nathan struggles to build
Given that the film is concerned with the rigid certainties of algebraic formulation its winning formula  mixing charm lowkey humour onscreen chemistry and emotional delicacy  is altogether more ineffable
Matthews direction is perfect Yes he manipulates you throughout but nothing is milked there is great subtlety and his settings in Taiwan and at Cambridge are deftly shot by Les Miserables and The Kings Speech cinematographer Danny Cohen
A confidently directed version of an underdog story that weve seen before
ChiRaq isnt nearly as polished or cohesive as his earlyperiod worksbut its delivered with a sense of fervor and immediacy that hasnt been felt from Lee in years
Its messy in places as Lees movies tend to be But there isnt a moment that ChiRaq isnt alive This is a deeply serious biting picture that also has joy in its heart
If theres a hotbutton modern American issue to be pressed Lee jabs it The common thread is vitality energy and urgency
Lee who birthed such memorable films asShes Gotta Have It and School Daze is back at the top of his form withChiRaq Lets hope he stays there
Submitting his finest work since the s Lee is inspired and alert for a change displaying renewed interest in the world around him The mischief and outrage presented here is outstanding
Savage optimism
In short its a Spike Lee film and one of his best in a long time  earnest flawed idiosyncratic and unforgettable
When Lee cooks up a stew this heady one best recognizethe right film at the right time Lees most creatively fertile and socially immediate narrative feature in years
Youve got the superbly selfpossessed Teyonah Parris as Lysistrata a stern and witty Angela Bassett  and Nick Cannon looking very comfortable in the role of the rap artist and gang leader known as ChiRaq They are served both chilled and hot
This is a Spike Lee film through and through It features jarring shifts in tone and temperament It stridently proclaims its message throughout at times like a revival meeting seeking a callback at others a history lesson in verse
A mesmerising indelible and important piece of contemporary cinema
Deliriously satirical bouncing between comedy romance and tragedy yet it primarily serves as a wake up call for the disenfranchised people suffering in this country
Theres so much swagger in ChiRaq that its a little uneven this movie is both small and immense the same way that violence within a community can feel isolated to an area but is also reveals a systemic worldwide problem
There is no story beyond this simplest standoff set up no character development no sign of any other life
There are some good acting performances by Nick Cannon who also sings a couple of songs Jennifer Hudson who also sings a song on the soundtrack and Teyonah Parris The soundtrack of this film should have gotten an Academy Award nomination
This is a movie that manages to be both brash and earnest hilarious and deadly serious bluntly rhetorical and poetic at the same time
Never subtle always strident and absolutely necessary
Lees film is worth seeing for its bombastic excess and camoclad dance scenes But if youre looking for tactful visual responses to the Black Lives Matter movement and the effects of police brutality this isnt it
Lee returns to engaging enraged form with ChiRaq combining social commentary anger humour dramatics and overthetop style in a sometimes messy mix that uses every trick necessary to put a spotlight on Americas poisonous love affair with guns
Ultimately this is neither tragedy nor farce Anybody know the Greek word for boredom
Its real honest funny and downright predictable but the Patel family is so engaging and delightful that you wish theyd adopt you
Meet the Patels ends up being much more than a movie about a guy going out on dates Its about shifting identities parental expectations and trying to hold on to a life raft of tradition in a swirling sea of change And its pretty funny too
Their attempt to stitch home movies goofily animated family conversations and wry crosscultural observations into a fullfledged movie has a few amusing moments but more often falls flat
Meet the Patels isnt an event but it makes for an engrossing story with a specific cultural perspective
We enjoy Ravis search for suitable mates and we like his family In the end they just want each other to be happy  and thats all that matters
Engaging but overlong and more than a mite unsettling
Ultimately a touching funny documentary about family and cultural forces putting pressure on a firstgeneration IndianAmerican man to do what should come naturally find love and a life partner
This story of a guy looking for love in many of the wrong places turns out to be one of the happiest surprises of the movie year
Its a film that pokes goodnatured fun while maintaining a real respect both for traditions that have survived for generations and for the circumstances when its healthier to set some of those traditions aside
Meet the Patels is a funny lightweight look at the weight of cultural traditions
Ravis journey for an American happy ending is eclipsed by the fascinating intricacies of Indian matchmaking
A charming if completely obvious documentary
Its nothing new under the sun but this is a charmingly unassuming and often very funny little movie
A gently humourous doc
Theres a touch of vanity project about the enterprise but Ravi makes an entertaining narrator imaginative use is made of animation and the film sheds light on timeless themes like love family and honesty
The documentary works as well as it does because Ravi while exhibiting a wry dry sense of humour is also something of an introvert Hes not always looking for a punchline so the ones he makes have more punch
Rather than taking a firm position for or against arranged marriages Meet the Patels is a bighearted poignant and truly funny documentary that shows people will try anything to find love  and sometimes anything works
This highly personal and lighthearted documentary gives some social context but never dives deep on the potentially devastating cultural divide it is exploring
One of the funniest romcoms of the year
Patels parents are the real personalities of the movie so goodhumored and spirited and so wonderful in sticking to their oldfashioned view of family life
Theres no reason for Rabid Dogs to exist as even character identity and motivation receives little attention
This remake of Mario Bavas  thriller is a serviceable timekiller that benefits from star Lambert Wilsons slowburn performance
If you can endure the messy slaughter with a body count in double digits the plot is not without its rewards
French filmmaker Eric Hannezos new Rabid Dogs never betters its predecessor but its a smoother ride
What this French redo of Mario Bavas  heist thriller lacks in plot it more than makes up for in flashy camerawork energy  Hitchcock would have cued us to the surprise twist earlier
Straight Outta Compton is not a movie that will change your life but its a very entertaining and strong approach to NWA Full review in Spanish
It has equal parts biopic movie and a lively representation of a cultural phenomenon thats still relevant today Full review in Spanish
An initially valiant attempt to do justice to a sprawling complex story that eventually succumbs to the pitfalls of a conventional music biopic
It follows some of the tropes of the biopic genre however what sets it appart is the human and raw emotion in the performances of the main cast Full review in Spanish
Despite some forced sentimentality it does its job as a biopic and that is the way it chooses to be but it could have offered more Full Review in Spanish
While the story might seem convoluted to rap neophytes Straight Outta Compton remains compelling due to its brilliant cast
the movie would have benefited from a much much shorter runtime ie this is not material that needs or deserves such an epic length
When it lets its music talk its rage has street cred
Straight Outta Compton essentially picks one narrative and sticks to it but the story it chooses to tell is undeniably powerful
One of the better musical biopics of the last  years Straight Outta Compton is a real ragstoriches showbiz story
As with most musical biopics its the rise of the artist that is the most entertaining
The first hour flies by in a haze of frenetic energy and exciting performances from all the main players
The movie has inescapable verve capturing why its subjects mean every bit as much as The Sex Pistols or Rolling Stones
Its all pretty entertaining and a testament to how much they accomplished in such a short space of time
Its just all a little too much for a twoandahalf hour movie
The opening act has a level of insight and exciting energy that the rest of the movie bogged down in plotting comes nowhere near replicating
A short history of rap
Stylish yet all posturing with no humanity an exercise in brand control by those who have no interest in exposing their faults
It portrays incredibly well the elements that made the creation of NWA inevitable the social political and economical conditions in which its founders were brought up Full review in Portuguese
F Gary Gray tells the story efficiently and leaving no loose ends on both script and plot Full Review in Spanish
What the film lacks in logic and scientific plausibility it makes up for in the philosophical questions it raises
The ideas McClean puts on screen are creative and original And his technical skill is outstanding He shows a lot of promise in other words and the films worth checking out
This is the first feature from writerdirector Joe McClean and he has made a debut that is admittedly uneven but not without promise
The turmoil in the friendship of the trio grounds the film in humanity when things get too theoretical
Joe McLean makes a memorable directorial debut with this mindblowing scifi reminiscent of such inscrutable screen classics as Memento and The Matrix
An inspiring film that shows the power of faith and forgiveness on and off the field
The Erwins latest film leans heavily on a genre formula but it is better than most Christian films
Enough of the flick worked for me to give it a slight recommendation Full Content Review  Violence etc  for Parents also available
Sure its an evangelical Remember the Titans but at least the Erwin brothers have made an effort parochially speaking to go outside the lines
Entertaining Inspiring Gripping True Story
Until the balance tips rather too blatantly toward the latter during the final minutes the overall narrative mix of history lesson gridiron action and spiritual uplift is effectively and satisfyingly sustained
Sports and religion are a potent combination one that siblings Jon and Andrew Erwin October Baby Moms Night Out exploit to canny effect in their new film based on the reallife Woodlawn High School football team
Remember the Titans  Facing the Giants  Rudy  Woodlawn
Its hard to buy that this brand of Christianity is fighting for the rights of the minority while so clearly throwing their own weight around as the majority religion
Heartwarming factbased drama about faith race football
Woodlawn hits spiritual pay dirt
The unintentional message here What integrated Alabama wasnt Christian brotherhood but Alabamas REAL state religion  football
A film taken with the singular American delusion that Jesus loves football though it also throws in a new delusion Jesus hates the US Constitution
Although handsomely crafted wellacted and made with transparently noble intentions Roland Emmerichs Stonewall is a movie that seems destined to please almost no one
Isnt a perfect movie by any means but the good intentions are there
Lends all new meaning to the phrase Roland Emmerich disaster movie
The Stonewall Riots deserved a better movie and unfortunately Emmerich was not the guy who was going to deliver it
Its a selffinanced passion project from a man who might be the most financially successful out gay filmmaker ever We should be celebrating this but man oh man does he make it difficult
Irvine is with that tousled blonde hair and milky skin with its apple cheeks a beautiful ephebe with no more emotional resonance than if he were actually carved from a pillar of marble
This cant be the kind of equality that those Stonewall activists were fighting for the right to have their story turned into formulaic historical fiction as tedious as the kind about straight people
If someone set out to make an afterschool special about street hustlers this would be it
The worst Quantum Leap ever
more than bad and worse than disappointing a tragic distortion of a vitally important story that insults the people it tries to honor and insults its audience as well
Where the social impact of the riots is lasting the impact of the movie is negligible
Almost everything that could go wrong does
Its a high school playready version of the riots a version more focused on outside characters and related inspiring lessons than even the riots themselves and wears the rosestcolored glasses about the dynamics of the era in which it took place
spends an excruciatingly long time on a happily ever after coda  minutes about Danny and not enough of what followed after the riots
The Stonewall Riots were a triumph for a marginalized community but Emmerich fails to convey the significance of the event in any meaningful fashion The subject matter deserves better and so do we
Stonewall is such a cataclysmic disaster of a film that Im surprised nobody has called FEMA yet to help with all the damage its done to the GLBTQ community
Despite the volatile subject matter Stonewall is a rather bland rendering
Disappointing gayrights drama has sex language violence
Stonewall is a movie about a pivotal moment in LGBT history as filtered through the perspective of a fictional hunk of Wonder Bread named Danny who steps off a bus from Indiana and right into a central role in the Christopher Street scene
Compared to a genuinely rousing gayrights historical drama like Milk  Stonewall falls considerably short
While its hard to shed too many tears over a corporation taking its customers for granted what Hanks movie does so well is appreciate the people who built Tower and in doing so transmit that in this case they did care
But Hanks wisely limits the celebrity talking heads in this riseandfall story Instead he focuses on the people who built the company from a Sacramento drugstore annex to a global brand creating a ragtag family in the process
Hanks does a strong job with his documentary debut here giving All Things Must Pass a lot of energy and an endearing quality It helps that the interview subjects are so engaging and likable
The film never lapses into hagiography partly because Solomon is too authentic a presence
A warm wistful documentary that fondly recalls a time when buying music was a communal activity and not just clicking on things
Overall this is an assured effort informative bittersweet and appealing for both the young and the not so young
Hanks and Leckart tap into a latent nostalgia for Towers freeandeasy heyday and the movie is an intensely bittersweet experience for anyone who grew up with Tower Records as a home away from home
In Colin Hanks admiring and tragic corporate biography Tower Records wasnt just a rock n roll mecca but a family operation that got high on its own supply
Solomons skills as a raconteur the employees unabashed love for their work and the constant stream of rock music playing in the background advance the film into something much more than a talkingheads documentary
The real beating heart of the film is its collection of wild war tales told by the companys former employees who regarded Tower as more than just a paycheck gig or a commercial proposition
Lively and loving
Colin Hanks makes his feature directing debut with this irresistible documentary about the evolution of the music business
Its loving and lovely but goes too easy on the hubris and greed
Hanks makes the rookie mistake of covering the same points too thoroughly  the film could be  to  minutes shorter  but you can see why he lets entertaining interviewees ramble a bit
This is Towers story and Hanks tells in a way that will resonate with both grizzled music veterans who have hung onto their physical collections and millennials wondering what all the fuss was about
As Bruce Springsteen says in the film Everybody in a record store is a little bit of your friend for  minutes or so And hes right  including all the ups and downs that friendship entails
Hanks found an amiable raconteur in Solomon now  but sharp and focused on the business that was his life A collection of Solomons confidants sing his praises and get misty about how much fun they had in the old days
We learn of the partyhearty environment and familylike vibe of a world where it was cool to write off cocaine as a business expense And we see the hubris and myopia that doomed the industry
Director Colin Hanks lets his affection for his subject run over The film probably is for record aficionados only
A love letter to the store and Solomon  but also to the bygone era of music consumption before iPods and Spotify
If only acting was as easy as hitting his patented RKO
A belated barelyrelated sequel generic enough to make the eminently forgettable  original look like an oasis of cinematic personality
The story fails to embrace the humor in the premise resulting in a sappy melodrama that tries too hard to bring you to tears Full review in Spanish
A light comedy that offers great entertainment and also touches on womens rights and the elderly at the workplace Full review in Spanish
The chemistry between Hathaway and De Niro feels sincere and is what keeps the story up and going Full review in Spanish
Thanks largely to performances by De Niro and Hathaway The Intern is a gentle enjoyable fantasyand certainly Meyerss best film in more than a decade
It is strange seeing De Niro playing a nice normal old dude and not insane edgy enraged andor emotionally constipated and you also cant help but think that Meyers movie would have worked better if he had shot someone
Compared to the dreck now synonymous with DeNiros name this soso comedy a pleasant but forgettable fish out of water story is actually one of his best films in years
Its a work of unwavering optimism with an uncompromised procareerwoman message and I found it refreshing
A typically breezy and entertaining Nancy Meyers comedy
Theres a lot less going on in The Intern  the story of an old guy who goes to work for one of those fancy new ecommerce places whatever that is  than meets the eye or the funny bone
DeNiro is his usual charming best and Hathaway takes what could have been a onedimensional character and breathes life into her
Its a strange one The Intern Just as youre wincing at a tired gag about how older people dont know Facebook along comes sentimental melodrama and youre suddenly grabbing for a tissue
Its the job of movies to fulfill wishes and the desire for an understanding parental figure is as basic as the desire to feel needed and useful in ones old age
De Niro and Hathaways very real chemistry is enjoyable and transcends The Interns many shortcomings
Fauxfeminist and whitewashily liberal A movie that wants us to feel bad for a female executivewhos actually highpowerprivilegedonly for a great white fathersubstitute to be her perpetual personal cheerer is far from feminist and damn near stupid
The Intern is a smart funny and surprisingly touching film anchored by firstrate performances from DeNiro and Hathaway
Some films have grace notes this one has what might be more appropriately termed graceless notes
Hathaway plays much better against DeNiros pinstriped persona when shes biting into her role with a Miranda Priestly boorishness
Its sad too because De Niro brings an amiable twinkling charm to the role Instead were stuck with misogyny dramedy and snivelling
The Intern doesnt have a huge amount to say about the issues it touches on its generally happy to let its two charismatic leads simply hang out together Its a li ttle cutesy at times and overlong but its not without its charms
Those of us who were never cool young guys live with an enduring hope that someday well be cool old guys Its a fantasy embodied in the flesh  specifically in the gloriously creased proudly aging flesh of yearold Robert De Niro
A predictable drawnout and cheesy story about remaining true to yourself in the face of pressures to sell out
The music is peppy enough but the spoken dialogue feels like it was written with emoticons
As silly and sometimes nonsensical as it is the movie is surprisingly sweet and wellintentioned
In the end this awful movie is an adaptation of the cartoon about as much as Steven Spielbergs Jaws was an adaptation of Emily Brontes Wuthering Heights
Jem has less in common with its neondrenched s source material than with the reallife Internettoredcarpet trajectory of Justin Bieber  a similarly generic teen idol with moves dully modeled on superior artistic predecessors
Though there are some light scifi elements the story remains largely grounded and based in the real world focusing on the characters rather than the spectacle
Everything about Jem and the Holograms seems specifically designed to be annoying The film is deeply misguided and not half as clever as it thinks it is
How can a dressup party with this much glitter makeup and hair dye preach the importance of being the real you
Jon M Chus weaksauce adaptation of the s Hasbro toy turned cartoon not only fails resoundingly as a film it also fails as a nostalgia piece  which honestly might be the greater sin in todays pop cultureverse
Sure Peeples has a nice if unmemorable voice but the vapid storyline with fantastic overtones transports Jem and the Holograms into another dimension one thats utterly flat
The movie is trite cheap and shoddy designed to be watched on an iPhone instead of in a theaterplus theres no way this assortment of bland nicegirls would ever become superstars Not even on YouTube
a shallow glittery girlygirl fantasy that unless you have a high tolerance of cute and the ability to laugh at how stupid this whole thing is may make you put your head through a wall
For a movie about someone learning to come out of their shell Jem and the Holograms is utterly confounding about what it even means to be authentic
Its hard to imagine a movie this year more sadistically boring and bland
Utterly implausible on every level and ultimately rather insulting a bit of glitter and lots of hugs are the sum total of its girl power
Chu was responsible for the shambles that was GI Joe Retaliation and proves that was no fluke through his uncertain handling of the conflicting material
Every line every twist and every note of music feels painstakingly focusgrouped
A highly flawed occasionally dumb but fascinating blast of neon revisionist girl power and social media wanderlust
This live action dud is basically what you imagine a bargainbasement Star is Born would be like if it was directed by the chap who did a Step Up sequel a horror producer and Justin Biebers manager
An unrealistic representation of the music industry
Gainsbourg and Sy play off each other wonderfully emphasizing how these characters relate to each other as people their scenes together feel emotionally honest even though one can barely imagine them happening in real life
Deserves credit for raising important issues at all but the trite romcom packaging compromises the good intentions
Samba manages to be hugely entertaining featuring a superb cast and expert direction without abandoning its important message about a universal problem
Though the film doesnt provide any answers it does give voice to the millions who suffer these same situations daily
Samba tries to be too many things to too many people although you cant say it doesnt have heart
Going for frivolity the endeavor abandons authenticity adding more confusion and disorder to an already scrambled film
The filmmakers mix touching social realism feelgood romantic comedy and workingclass farce into a patronizing ragout of flavors that never successfully blend together
Unfortunately the material flounders from the broadly farcical to the bombastically melodramatic
So affecting is the performance by Omar Sy Jurassic World The Intouchables as Samba like the dance an undocumented Senegalese chef you almost forgive the shortcomings
becomes caught between a breezy romantic comedy and a provocative drama about social justice
A comedy that was sold to appeal to a sensitive and inteligent side but that never happens Full review in Spanish
A refreshing romantic comedy that shares a lot with the directors previous film Full review in Spanish
Although the performances are memorable the plot does not know how to combine drama and humor Full review in Spanish
A small comedy that bases its success on its leading couple Omar Sy and the always gracefull Charlotte Gainsbourg Full review in Spanish
A highly enjoyable movie Full review in Spanish
Olivier Nakache and Eric Toledano return with one of The Untouchables stars Omar Sy in the title role Its a darker sadder and less audiencepleasingly contrived tale than their previous effort
Samba finds a much stronger rhythm when it stops contriving and simply shines a light on the joy and pain and musical interludes of lives lived in the margins
The editing errs on the side of longueurs but likable people and the miseenscne draw you in Somehow even the artificiality feels heartwarming
Nakache and Toledano have another crowdpleaser with international appeal on their hands
Samba is still an entertaining and enjoyable movie due in large part to the charm of its leads Omar Sy and Charlotte Gainsbourg
An adventure film of big scale that focuses on human drama Full review in Spanish
Unfortunately Everests technical feats arent matched by a subpar script from William Nicholson and Simon Beaufoy
The film only seems to come to life when showing how challenging is the climb and how unpredictable and harsh mother nature can be
For a movie about the worlds biggest mountain Everest feels small
The script emphasises the dangers but fails to underscore that with a truly treacherous aesthetic environment
Everest has a couple of good moments however nothing that moves you the way its advertised Full review in Spanish
Everest splits the difference between documentary reenactment and hypedup Hollywood drama
Worth seeing for its terrifying action sequences and its stunning visuals but it never delivers the expected emotional gut punch
Kormkurs film excells because it never romanticizes the mountain and he doesnt drag the last moments of those who died in it Full review in Spanish
Its a solid disaster film that certainly isnt the feelgood movie of the year
Thrilling exhausting and ultimately devastating
Everest remakes the experience as entertainment invites you to observe but not feel the suffering and invites you to pay for the privilege
Spectacle filmmaking with a purpose Everest is a welcome throwback to the testoroneheavy adventure flicks of past decades
With visually stunning imagery and a solid Alist cast this film just about transcends its oddly uninvolving story
The picture utilizes every bit of the expanded D frame to provide an immersive experience But thats combined with a real sense of brutality
The movies first hour is a bit slow precisely because the filmmakers refuse to inject conventional dramatic elements But it all pays off in the devastating finale
The stunning location photography stays with you much longer than the scripts shortcomings The mountain as ever has the last word
This is an impressive filmmaking achievement but those expecting to be moved by the story may well be left cold
Everest the movie in showing how the mountain conquers man never explains the torturous treks appeal and that monstrous Himalayan peak ends up dwarfing the human stories here struggling for air
Over the course of a fragmented  minutes we find ourselves lost in just as much of a snowy fog as those of the actors who wander in and out of the frame as if uncertain as to whether the story even requires them
Inevitably and immeasurably affecting but conventional in terms of what it says
The footage  discoveries made by the Allies in the liberated Nazi camps during   is graphic terrible unforgettable
This wise sober new documentary is a reminder of historical horrors but its also a tale of censorship
Though undoubtedly an underwhelming documentary this remains an important piece of cinema as it documents a barbaric brutality we have a duty to explore and study
Night Will Fall isnt simply a film about the war it documents the power of emerging technologies to reveal and publicise war crimes  something that also feels acutely relevant today
We wouldnt watch it again but were glad we watched it once
As startling and bleakly compelling as youd expect from this rare combination of director and subject
This is an extraordinary record But be warned Once seen these images cannot be unseen
This is a shocking and moving account of how the Bernstein documentary was shot edited and shelved
The survivors and liberators interviewed by Singer have never forgotten their experiences and this crucial account of a dark episode in history stands as a chillingly eloquent memorial
It is difficult to imagine that therell be a more important film this year Dont miss it
Impressively sober thoughtful
After  years in the can some of the most visceral detailed and shocking footage of the liberation of Nazi concentration camps is being released
A masterfully constructed eyeopening and at times eyesaverting look at an obscure chapter of Holocaust history
As well as footage of Bernstein Singer has some fascinating interviews with English Soviet and American soldiers who did the filming
Horrifying Fascinating
As Night Will Fall shows even in the darkest hour sometimes the greatest heroes are those willing to stare bravely into humanitys worst depths and tell the world what happened
The liberation footage itself is the centerpiece of Night Will Fall It is mesmerizing sickening disturbing and essential
It may not do full justice to all of its subjects in its tight  minutes but its not a film youre likely to forget
Night Will Fall is an unsparing yet unfortunately necessary reminder of the atrocities committed seventy plus years ago
Hellers grasp of the material is as firm as a more experienced directors Still the primary attraction is Powley who offers up the best acting performance Ive seen all year
Boldly honest and frank comingofage film that is at once daring in its subject matter yet very much part of its genre Good but not quite great
Frankness candor and honesty served hot A mustsee
Playing a character trying to reconcile her hormones heart and head Powley seems genuine in a way that actresses playing adolescents rarely do and helps ensure that the heady rush of a movie that shes in does the same
This powerfully arresting and affecting adaptation of the book by Phoebe Gloeckner is only getting started with the many and varied provocations it has in store
Powley offers a bold performance that mixes rabid hormones spiritual agonising scary selfdebasement and extreme teenage dramaqueening
Most crucially Heller is more interested in bearing witness to adolescent experience than passing judgment  and this approach extends to her handling of the adults around Minnie
Powley is a yearold actress but shes a convincing teen in every sense which will lead to the discomfort of some Its a breakout performance
Whether adulthood is measured by age or experience or by something less quantifiable is one of the key questions the movie asks
Prepare for the most fked up love triangle ever
From her breathless confessions of true love to her sweaty accounts of coitus Heller Powley and Gloeckner pull us onto the mental merrygoround of the female teen
An irregular coming of age tale that having a story thats grounded on reality flirts with improbable situations taken out of a very used mold Full review in Spanish
The film is quietly radical not because it dares to rattle cages so much but because it doesnt This is a story of huge emotions and big moments told via intimate gestures and tiny power shifts Its a gem
The Diary of a Teenage Girl is honest frank and doesnt hold back So if you dont mind a swear word or two and you want a realistic approach from a film this movie could be for you
Bel Powley Alexander Skarsgrd as the boyfriend and Kristen Wiig as the mum all display the subtlety not in the script
Multiple stars are born in The Diary of a Teenage Girl the conventionally titled film premiered earlier this year at Sundance that turns out to be unconventional in every way that matters
Writerdirector Marielle Heller never passes judgement on her characters giving us an honestly messy account of her heroines sexual and artistic awakening
The Diary of a Teenage Girl is a rare gem because it strips the male gaze and sees Minnies sexuality as her initial link to a deeper self discovery
The Diary of a Teenage Girl is surely destined to become a tentpole comingofage movie and I imagine a significant vehicle for the careers of everyone involved
It abuses the unconvincing drama and dramatic twists the director submits to the young protagonist Full Review in Spanish
Striking the right goodnatured tone between admiration and amusement I Am Thor isnt particularly slickly or imaginatively packaged but its straightforward DIY presentation feels apt
Jon Mikl Thor is a showman and a stalwart a flatout legend and I AM THOR is the often uproarious and sometimes quite poignant love letter he deserves
A charming subject goes a long way in characterstudy doc I Am Thor an imperfect but energizing portrait of selfstyled metal god Jon Mikl Thor
Highly entertainingThankfully Thors charisma is undeniable and its fun to watch him interact with fans and dominate the stage
I Am Thor is an amusing show business tale that is all the more endearing for the respect it shows towards the eccentric and indomitable figure at its center
Eden is proof that you need more than a good beat and stylized visuals to be memorable Full review in Spanish
An honest portrait of the hardships that come from chasing ones dream Full review in Spanish
A failed attempt by director HansenLove to create a portrait of a young DJ Full review in Spanish
Even with the nostalgia factor behing it the movies falls short of what it couldve been Full review in Spanish
A fair portrait of the French music scene of the s that captures the escence of the music that began to feed from recicled nostalgia Full review in Spanish
The cruel chronic of an aspiring DJ that never quite gets there Full review in Spanish
If not for Daft Punk this would be another generic movie about a musician that grasps glory only to loose it all at the end Full review in Spanish
HansenLove focuses on creating an atmosphere with rhythm and rich charachters to tell her story Full review in Spanish
Edens best quality is its bittersweetness a story about postponing adulthood and an honest portrait of fun confusing and good times as a teen Full review in Spanish
A swan song to the life of a teenager living free of consequence Full review in Spanish
Eden perfectly captures the spirit of the s and boasts a great soundtrack Full review in Spanish
A complex and elegant film Full review in Spanish
A tribute and acknowledgement of the intelligence behind the DJs craft Full review in Spanish
Like going to a club sober
Paul describes a song he likes as existing between euphoria and melancholia which is the balance the movie hes in strikes as well as interested in joy as it is in loss Which may be the best thing these stories of not making it bring to the table
Where the early scenes of Eden feel on the nose the back half is filled with precisely the sorts of glancingyetwounding blows that are HansenLoves specialty
The drama occurs only fleetingly and Greta Gerwigs cameo as one of Pauls flames is stilted
Eden does however offer an intriguing insiders view of an important musical subculture an insiders view that crucially remains rooted in the alltoorelatable reality from which the music itself is designed to offer temporary escape
Only in retrospect could I understand Edens careful delineation of temporal emotional and geographic properties
An impressive and suprisingly melancholy look at repetition in life soundtracked by the recursive music of the French Touch scene
Learning to Drive is a story of companionship loneliness resilience Its a small artfully crafted thing but it resonates in big ways
Charming and often very funny
Learning to Drive is precisely the sort of adultthemed intelligent and heartfelt film it wants to be with Clarkson and Kingsley wonderfully on point
Despite Patricia Clarksons performance and screenwriter Sarah Kernochans expansion of Katha Pollitts  New Yorker essay the film still feels undercooked
Kingsley acquits himself in the role with considerable grace He also makes a solid anchor to Clarkson who relinquishes her own elegant sometimes frosty screen persona for something a little wilder and decidedly sexier
Small in scale and largely unassuming both of which may be in the movies favor since it never tries too hard The film seems content to just be
Empathetic drama treats mature themes with warmth
A perfectly pleasant minute diversion with little conflict and no major drama You could watch these two act all day
Patricia Clarkson and Ben Kingsley are such likeable performers that theyre watchable in the worst tripe but its hard to imagine a weaker vehicle for their considerable talents
A light duty and passably entertaining exposition by Ben Kingsley and Patricia Clarkson
Its an engaging diversion but falls short of anything more
Driving a car is a useful cinematic metaphor for taking control of ones life and there is a gentle determination behind this film which makes it more engaging than it sounds
Directed by Isabel Coixet and based on a magazine article by the feminist writer Katha Pollitt this is a little film but there is a lot of craft in it To make a movie about human connection that is funny without being mawkish is no small achievement
Visually Learning to Drive is mostly flat  but then there are only so many ways you can shoot two people sitting next to each other in a car
Clarkson and a compelling Kingsley make for very easy company as their gentle exchange of age and culture enables them to embrace a challenging future
There are glimpses of intrigue but we never get to know the characters well enough to care for them
Director Isabel Coixets films have of late been a dark and sometimes dreary bunch so its a relief that her latest Learning To Drive is wryer sweeter and far less pretentious
there is a sense of wornout familiarity that attaches to this wellintended but ultimately featherweight indie drama about a privileged Manhattan lady and her relationship with her exotic and presumably wise driving instructor
This isnt just a good film because of the great acting however it has a solid romantic story and it touches on some important social issues as well
Somewhat lucid at times with flourishes of comedy as enjoyable as individual moments are it unfolds rather predictably
A Perfect Day is rarely actively bad but more often inspires a shrug
This compact little satire  set in s Balkans  is a small personal story about huge unfairnesses and injustices Bleakly bitterly blackly funny
The refusal to sugarcoat or overpraise its fearless dogooders is itself noble and useful and at its best A Perfect Day relishes in portraying characters we rarely see on film in a darkly comic fashion
Its frustrating how close this is to being a good movie The pieces are there The follow through isnt
The characters are too thinly sketched to generate much of an emotional connection leaving some compelling ideas unfulfilled in the process
The film may be scattershot and odd but it needs to be odder still
Although caught between Mash and Hurt Locker the film packs a powerful lesson
Gritbomb comedy dripdripdrip absurdism Altmans MASH Richard Shepards The Hunting Party and the grandiosely mad Emir Kusturica can be glimpsed in the rearview mirror of floundering influence
With an eclectic soundtrack welltimed editing and crisp cinematography  and of course that terrific cast led by the great Del Toro  A Perfect Day is a roughedged gem
A project thats intriguing funny emotional and sensitive one that raises issues abundantly important today and makes us consider whether were truly considering what else is going on in the world around us
There is a subtle tonal complexity to A Perfect Day where even the title has an air of sarcasm There is a lingering note of melancholy throughout the film and it earns every bit of it
By focusing on a small story the sarcastically titled A Perfect Day becomes more relatable and in the process tells us more about the big picture in this case the Bosnian conflict"""]

let run =
    let freq_table = getFreqTable ughCorpus 
    let ughString = Newtonsoft.Json.JsonConvert.SerializeObject freq_table 
    System.IO.File.WriteAllLines(".\ughCorpus.json", [ughString], Text.Encoding.UTF8) 