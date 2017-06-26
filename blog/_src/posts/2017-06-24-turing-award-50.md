    Title: Quotes and Stories from "Turing 50"
    Date: 2017-06-24T20:00:52
    Tags: dear diary, by Ben Greenman

The ACM recently hosted [a celebration of 50 years of the A.M. Turing award](https://www.acm.org/turing-award-50).
These are some notes and thoughts from the event,
including how Fred Brooks once rented a bus,
Don Knuth's outrageous implementation of batch processing,
and Judea Pearl's theory of homo sapiens.

<!-- more -->

**Conventions / Disclaimers:**

- Italic text is paraphrased, may be incorrect, and may be incorrectly attributed.
  Make sure to watch the ACM's live stream before quoting anything here!!!

- Section-breaks are labeled as "panel", "talk", "question", etc.

- This is intentionally "bad writing" in the Peter Lee sense (see below)
  --- primarily "what I saw" and very little about "what I thought and felt".
  A summary in my own words just wouldn't do justice to the panelists.

- The "Augmented Reality" session was my favorite.

### Opening Remarks

#### _Alan Turing is with us today_

At the start of the event, the [emcee](http://users.ecs.soton.ac.uk/wh/) unveiled a bronze bust of Alan Turing.
This statue was on display at center stage during the whole event.

It's a good sculpture and it's good we remember Alan Turing, but I'm sad
 that the ACM would encourage this kind of idol-worship.


### talk: Impact of Turing Recipients' Work

Speaker: Barbara Liskov

- _the first awards recognized achievements in the standard fields of theory, AI, and systems_
- _hostile environment around the first awards, trepidation about future awards_
- _with Unix, Ritchie and Thompson got the design right_
- _Niklaus Wirth said, 'If I understood how important Pcode was, I would have
   spent more time designing it'_

#### thoughts

What is "systems" --- does that even have a definition?
And Unix is definitely NOT an example of a "right design"; rather it's a
 landmark of [worse is better](https://www.dreamsongs.com/WorseIsBetter.html) design.

### panel: Advances in Deep Neural Networks

Panelists: Judea Pearl (JP) Michael Jordan (MJ) Fei-Fei Li (FL) Stuart Russell (SR)
  Ilya Sutskever (IS) Raquel Urtasun (RU).

- _please welcome Judea Pearl and his panel_
- SR: _I work in all areas of AI except for deep learning_
- JP: _I am a foreigner in this field ... left because human beings are not
       good at handling information ... people are very good with causal inference,
       not with statistical inference ... deep learning is statistical ...
       correlation not causation_
- JP: _... there is a very old existence proof, homo sapiens took over the planet
       ... I believe because they had an internal model of their environment
       ... a drawing of a lion with wings is evidence of this model, you have
           to have such a model before you can experiment with it and imagine
       ... snakes have superb optics, result of a long
           evolution process
       ... very specific but they cannot build eyeglasses
       ... humans have an internal model, can build a market based on promises
           and build large communities based on promises_
- JP: _... I see four levels
       ... second level is predicting events, if I do X then what?
       ... third level is counterfactual, if I did things differently then how
           would the outcome change
       ... very hard to advance between levels, are we working to help
           machine learning 'level up'?_
- JP: _data science is about the relation between data and reality
       ... data alone is not data science_

- MJ: _today we can't think without holding a piece of metal_
- MJ: _machine learning is part of computer science rather than AI
       ... AI is about how to make human
       ... machine learning is about allocating resources
       ... matrices are not all of human intelligence
       ... neural nets are part of a wider toolbox
       ... too much hype in NLP_
- MJ: _huge gap between syntax and semantics ... chat bots are just syntax, don't learn
       ... faking intelligence with neural nets, so well that you can build a company ..._
- MJ: _real metric is task completion_
- MJ: _if I say 'a GLEEB walked across the airport' then true intelligence
       can make a lot of educated guesses about a 'GLEEB' without any other context_

- FL: _I disagree, ML is part of AI
       ... understanding intelligence and making intelligent methods for solving AI problems_
- FL: _to quote Churchhill 'its not beginning of end, not end, not beginning of end, probably end of beginning'_
- FL: _todays AI powered by hardware and data_
- FL: _AI cannot yet find our keys_
- FL: _quote: 'todays AI is making a perfect chess move while the world is on fire' ... ignores context_

- SR: _Turing ... a program is a mathematical object ... math community did not recognize this_
- SR: _lots of grad student descent ... tuning to get performance ...
       deep learning is neglecting the problem of exponential data
       ... deep learning is just circuits, circuits lack expressive power ...
       a human can process data from CERN but a neural net cannot, need to know physics_
- SR: _probabilistic programming, somewhat under the radar, maybe on the right track
       ... 10-line program running/generating/updating a large network of possibilities
       ... more composable and flexible_

- IS: _why I like deep learning ... philosophically satisfying ... the hypothesis
       class is a circuit ... powerful hypothesis class not too many parameters
       ... can actually find circuits ... 'violates all theory' ... really amazing
       ... humans can see and hear pretty fast, even though our neurons are
           pretty slow, perhaps because we do a massively parallel process that doesn't
           take many steps ... works well enough to be useful_
- IS: _models e.g. for vision are very hard to understand ... fight fire with fire
       ... incomprehensible solution to incomprehensible problem_


- RU: _the breakthrough in neural nets is not algorithms ... it is tricks,
       hardware, and grad students_
- RU: _with neural nets we forget about modeling, uncertainty, and prior
       knowledge ... perception is a canonical example_

#### question: boundaries

- JP: _glad to see people in deep learning understand its limitations ...
       is there a clearer definition of the boundaries? Are you
       worried about bridging the levels factual/inferential/counterfactural?_
- MJ: _the big problem is decision making under uncertainty_
- FL: _cognition is a hard problem_
- JP: _do you have a clear idea of the boundaries?_
- MJ: _neural nets use back-propagation ... its non-modular, sad fact ...
       performance and explainability is the tradeoff ... then again
       people are non-modular_
- SR: _AlphaGo is not deep learning ... basically an improved version of
       the machines Arthur Samuel made in the late 1950s ... the interesting
       code is in C++ ... rules of go, next moves, searching future states ...
       depends on transitive closure_
- JP: _can AlphaGo take advice from a human?_
- MJ: _not currently, but that would be a new policy to add to the toolbox
       ... just as neural nets are one tool within AlphaGo_
- RU: _no reason to ask if deep learning is going to solve all problems_

#### question: education?

- JP: _what DO you teach in your neural networks classes?_
- FL: _.......... chain rule, Taylor expansion_
- JP: _teaching is communicating truths ... what is true about neural nets?
       what are some things that will definitely not happen?_
- SR: _Peter Norvig and I have a problem with our AI book ... chapter on vision,
       chapter on speech, will probably post just point to the neural
       nets chapter ... we don't really understand! ... really selling students
       short_
- FL: _in labs we talk about what we cannot do ... we all have open problems_
- MJ: _CS cirriculum needs more statistics, inferential thinking ... revise
       the whole cirriculum bottom-up to weave this in_

#### question: could a neural net fix my phone without breaking it?

- JP: _right! big problem that neural nets have no internal model to manipulate_


#### question: generalizability?

- IS: _special-purpose vs. general purpose solution depends on the problem ...
       most things we give special-purpose solutions ... I guess if you
       wanted to automate a mathematician that would need to be general_
- SR: _always argue with your self ... try to break what you've built
       ... there's a system that plays video games just using the pixels on
       screen as hints ... it's very good at mazes; if a newborn
       baby learned to play maze games in 2 hours that would be amazing! ...
       does the system scale? absolutely not_

#### thoughts

When MJ said "people are non-modular", I think he means that people are able
 to break abstraction barriers when needed.

### panel: Restoring Personal Privacy without Compromising National Security

Panelists: Joan Feigenbaum (JF) Whifield Diffie (WD) Bryan Ford (BF) Nadia Heninger (NH) Paul Syverson (PS)

- JF: _... wikileaks ... russian hackers ... social emergency ..._

- WD: _everything I say today is copyleft_
- WD: _its a misunderstanding to talk about a conflict between security and
       privacy ... two aspects ... problem goes back to feudalism ...
       the right to build a castle was granted by the king ...
       on one hand a castle improves national security ... on the other
       hand a castle can be used to attack the king ... technology is
       upsetting the basic notion of private vs. public security ...
       governments cannot protect citizens and cannot protect themselves ...
       extremely difficult to prove that a small process is secure_
- WD: _exceptional access makes it more complex_

- PS: _major concern are national security threats and ability of authorities
      to confound threats ... analogy to printing press ... proclimation of
      1635 that only state messengers can carry letters ... 1663 treatise
      by the national censor, no printing house can have a back door ...
      the general topic is very old ... title of this session isn't very good,
      the real dilemma is investigation vs privacy_

- BF: _code is law for better or worse, tech is not a tool like a watch ...
      tech can monitor us and decide when it works ... tech is government,
      not obedient tools ... the mind is a warrant-proof space ... 5th
      amendment rights should extend to wearables_

- NH: _cannot divorce the security/privacy issues from the current political
      context ... the serious vulnerabilities are not in math ... they are
      in users and implementors_

#### question: back doors

- JF: _perhaps we should explain what a back door is_
- BF: _agency keeps a master key in escrow_
- BF: _non-lawyers can and should take a stand on basic issues_
- BF: _there are legitimate warrant-proof spaces ... electronic extensions
      of the mind need to be recognized as warrant-proof spaces_
- BF: _the set of authorities with backdoor access should change as I travel
      between countries ... but this will lead to a global race to the bottom_
- WD: _germany has a law against sex tourism (committed by German citizens
      visiting other countries) ... neither government will be willing to
      lose backdoor access_
- NH: _technical reasons against backdoors ...
      (1) 'weak crypto' was implemented, nobody turned it off, is now
      breakable by anyone in 2015 ...
      (2) Juniper used non-default crypto parameters, someone (inside?) changed
      the parameters ... (3) attackers exploit back doors_
- PS: _quote 'you can put a man on the moon, surely you can put a man on the sun'_
- WD: _trouble is getting him back safely_
- BF: _I think back doors are okay, but not for personal devices ...
      need public lab and transparent processes, need separation of powers ...
      prosecutors are getting cases thrown out because courts do not accept
      their backdoors ... there is a place for transparent back door tools_
- NH: _politicians are rarely technical people_
- BF: _tech is not a set of policy-neutral tools, need to address gap of understanding_

#### question: ???

- WD: _we don't know how to build good crypto programs ... opponents are debugging
      our programs with different goals ... we're trying for-all-paths safety (universal)
      ... they're trying exists-bad-path (existential)_
- BF: _cybersecurity market is a lemon market_

#### question: how to advise

- JF: _question from audience 'I am an advisor to a company working with nuclear
      energy, they are terrified of being attacked, how should I advise them?'_
- WD: _a network like that is probably separated enough to be safe ... the problem
      is being safe AND connected to the web_
- BF: _because IOT_


#### question: what should the ACM do?

- NH: _maybe we need increased regulation, the ACM could help bring experts together_

#### question: what is true security

- PS: _it's all the same thing ... gets labeled differently ... just trying to
      control which bits can go where and who gets to read them_
- NH: _security is the absense of being violated_
- PS: _no true security, need to consider context_
- JF: _problem of our community, have strict standards, may be unrealistic ...
      maybe a lot more tolerance in practice than our model accepts_
- PS: _security and privacy are environmental problems_

#### question: can we stop the needle-in-haystack search for vulnerabilities?

- PS: _need to build in security from the start_
- BF: _need rule of law, transparency, separation of powers_
- WD: _stop delaying, instead of spending $$$ on fixing problems, we should
      invest in solving the fundamental issues_


### panel: Preserving our Past for the Future

Panelists: Craig Partridge (CP) Vint Cerf (VC) Brewster Kahle (BK) Natasa Milic-Frayling (NM) Mahadev Satyanarayanan (MS) Brent Sales (BS)

Note: I was volunteering during this session; quotes are sparse

- MS: _the running system is the total documentation ... there are too many
      details for prose to capture_
- ??: _running old code has a danger of running old bugs_
- ??: _question: what not to save? ... it's very hard to tell in advance_
- MS: _there is no absolute censor in a world with caching_
- BK: _asking UNESCO to solve the problem is unrealistic ... need to empower
      the fanatics, given them tools to preserve data_

#### thoughts

I totally agree with the "empower the fanatics" sentiment.
Today, because of fanatics, I think we're doing pretty well about preserving
 the past.
Suppose I found an old PowerPoint file.
I'm sure I could find a way to read it with help from the internet --- either
 by searching Google, pirating an old version of PowerPoint, or asking online forums.
So personally I'm not worried about losing data we have currently; I'm more
 worried about the future, the internet becoming "less chaotic".

The panel raised a good question about how to preserve research and encourage reproducibility.
A `.pdf` or `.tex` document is not enough; a virtual machine is okay.
Really I think we need a stronger cultural emphasis on literate programming
 and a mature library like TeX to help authors store and share their work.
[The Gamma](https://thegamma.net/) seems on the right track.

I was surprised that the panel did not discuss search, version control, and
 the ACM's open-access policy.


### panel: Moore's Law is Really Dead: What's Next?

Panelists: John Hennesy (JH) Doug Berger (DB) Norm Jouppi (NJ) Butler Lampson (BL) Margaret Martonosi (MM)

Note: I was volunteering during this session; quotes are sparse

- BL: _there's plenty of room at the top ... with Moore's Law we got
      improvements at the bottom of the software stack, everything above
      got to benefit and it was easy to integrate the changes ...
      there's lots of opportunities to trim fat in the middle/top of the software
      stack ... these improvements will be harder to integrate, but there's
      lots of opportunities_
- MM: _By the way, don't believe the brochure that says I'm at Google.
      My affiliation is Princeton, Google and I are just friends._
- BL: _important to distinguish approximate vs. precise software ...
      precise software has a specification and the customer cares about that
      specification ... approximate software doesn't have a hard spec, just needs
      to approximately work ... the web is approximate, it doesn't work and it
      doesn't need to! ... windows is precise, definitely has a spec and users
      definitely care_

#### thoughts

The recording of this panel should be good; it was very lively, very practical.
And the first audience question (by [David Patterson](https://people.eecs.berkeley.edu/~pattrsn/)) was "an A+ question".

The panel reminded me of a talk by [Yale Patt](http://users.ece.utexas.edu/~patt/)
 about "the end" of the Von Neumann architecture.
His opinion is future computers will be Von Neumann machines that rely
 on "accelerators" like a GPU --- computer organization is not going to change,
 but will expand to have a bigger toolbox.
So sure, Moore's Law is dead, but there are many opportunities to make
 computers faster at places other than the bottom of the software stack.


### panel: Challenges in Ethics and Computing

Panelists: Dierdre Mulligan (DM) Jennifer Chaynes (JC) Helen Nissenbaum (HN) Raj Reddy (RR) Noel Sharkey (NS)

Note: I was volunteering during this session; quotes are sparse

- RR: _there are more slaves in the world currently than there were in the
      US during the civil war ... here is one way technology could help,
      by giving everone a device to record their location ... if 
      someone's time and location is constant, they may be held against their
      will_
- ??: _do you believe every problem has a technological solution?_

- NS: _yes the training set may be biased against people similar to me,
      but I want you to consider my case as an individual_

- JC: _a very nice Washington Post article_

- ??: _whether to encrypt the back hall_

- RR: _we can sit here and wring our hands, but nothing will come of it unless
      there are rules in the US constitution_

#### thoughts

I did not enjoy this panel.
This is an ACM event, not a United Nations event.
An ACM-sponsored panel about social and political problems should
 look for constructive ways that computer science can address these problems.
Raj Reddy tried to give constructive solutions, but the panel seemed more
 interested in complaining about how hopeless things are.

The comment by Noel Sharkey about "consider me as an individual" was something
 I hadn't thought about.
Instead of worrying about biased datasets, let's use technology to
 collect data on an individual instead of abstracting a person by their
 race, age, or neighborhood.


### talk: Computer Science as a Major Body of Accumulated Knowledge

Speaker Donald Knuth (DEK)

- DEK: _don't applaud me, just read my books_
- DEK: _at the time, computer science was AI, numerical analysis, and programming languages_
- DEK: _a colleague said 'I will believe that computer science is a science when
       it has 1000 deep theorems' ... I am not sure what a deep theorem is but
       I think its different from what's proven by deep learning_
- DEK: _great privilege that we can invent the problems we work on ... imagination ...
       physicists can only guess the size of the sun_
- DEK: _I've always believed computer science and math are two parallel subjects ...
       sometimes you hear people wondering if one subsumes the other_
- DEK: _when I won the Turing Award, the prize money was about $1,000,000 less
       than it is today ... I did get a nice Tiffany bowl that my wife and I
       use to serve strawberries ... strawberries actually taste better ..._
- DEK: _very fortunate in this field ... I'm completely worthless as an economic
       advisor ... it's a game I've been able to take advantage of_

#### question: how could you offer to pay for TeX bug reports?

- DEK: _well there were many, many bugs ... I stopped doubling at 32768 ...
       brought people out of nowhere ... next time I check the bug reports
       will be 2021 ... someone is queueing the bugs reports ... I believe
       strongly in batch rather than swap-in/swap-out ... last time
       I checked reports was 2014 so 2021 will be next_
- DEK: _TeX was a literate program, and it helped that I wrote 'The Errors of TeX'
       about the first N bugs_

#### question: do you think computers will become good composers of music? do you see a role for computer-assisted proving?

- DEK: _Yes in part, assisted is the key word ... I have a program
       running now that I hope will help me prove a theorem_

#### question: favorite algorithm?

- DEK: _Tarjan's strong components ... short deep useful_

#### question: thoughts on AI, computers taking over?

- DEK: _I get scared when I see Stuart Russell making assumptions based
       on people acting rationally ... then you look at election results_

#### question: if you could start over and do things differently, what would you change?

- DEK: _I think I would use decimal internally in TeX instead of binary_

#### question: how to record history?

- DEK: _a video 'Lets not dumb down the history of CS' ... used to be history
       of algorithms ... trouble with funding ... the history is nothing that a non-CS person
       could not understand ... the whole field of history changed from internal
       to external ... historians need to be external to get published in journals
       ... no CS department supports a historian ... recently read a dissertation
       about the ALGOL 60 copmiler ... very careful, describes data structures
       and organization ... this kind of thing is what deserves to be called history_

#### question: teachers

- DEK: _hardest thing for me is choosing between two hypotheses (1) could
       teach this to anyone (2) only 2% of the world is geeks ... suppose
       the second is true then you can't talk about how to teach if the
       teacher is not in the 2% ..._

- DEK: _the newest issue of CACM has a fun typo, 'deep earning'_


### panel: Quantum Computing: Far Away? Around the Corner? Or Maybe Both at the Same Time?

Panelists: Umesh Vazirani (UV) Dorit Aharonov (DA) Jay Gambetta (JG) John Martinis (JM) Andy Yao (AY)

- JM: _goal to have a 45-50 qbit machine ... 1 error per 1000 operations ...
      to test, run sample algorithm, chart output vs. a classical supercomputer
      ... got to be a supercomputer to finish the computation in time_
- AY: _I'm a believer ... one suggested benchmark is to factor 1000-digit numbers ...
      impossible to attain ... need to expore new possibilities, take physics
      attitute_
- AY: _CS did well attracting attention to quantum ... science
      should be more open ... share results between physics chemistry CS ...
      don't just stick to your specialized conferences_
- AY: _CS departments reception to quantum is less than satisfactory ...
      15 years ago, maybe 4 or 5 universities ... now, maybe 7 or 8 .. China
      doing much better in this regard_
- JG: _not useful to make analogy to anything classical ... universal
      fault tolerance? or computation in the presence of error ... either
      would be excellent, still a long way off_
- JG: _IBM put quantum on the cloud ... picked an instruction set that
      tries to abstract away ... have been 19 published papers on the behavior
      of this quantum hardware_
- DA: _two paths ... finding algorithms, besides Shor's algorithm ...
      make quantum computer to realize the algorithms ... finding algorithms
      is very difficult ... information-processing point-of-view_
- DA: _error correction still small scale ... can we use entanglement
      between probes to improve accuracy?_

- UV: _different goals ... maybe you want perfect Qbits for a perfect Hilbert
      space ... reality is a noisy space ... short run, how to compute with
      noise ... how to correct errors ... 
- JG: _those 2 paths are the same to me ... we want larger devices with fidelity
      ... lets build hardware see where goes ... exciting prospect, computer
      scientists will explore what they can do with these erroneous qbits
      ... that's why IBM has the instruction set open to the community_

#### question: why isn't adding 10 qbits only 10x harder?

- JM: _building infrastructure to scale ... not just grad student code ...
      we're all good coders using standard industry practices for coding_
- JG: _fidelity is hard_

#### question: both IBM and Google use superconducting storage?

- JM: _superconducting scales ... ion traps harder to scale, but we still
      watch, keep eye on data_

#### question: education

- JM: _I like talking to engineering colleges ... physics and engineering
      need to work together_

#### question: is quantum going to change programing languages?

- JG: _yes very different to handle errors ... current challenge is
      building an abstraction over the superconducting hardware_
- JM: _hoping to first expose hardware, then get a model, eventually a language_
- DA: _need to start with more algorithms_

#### question: what would Feynman do?

- JM: _experiments!_
- DA: _yes he'd tell us to keep exploring, and explore with us_


### panel: Augmented Reality: From Gaming to Cognitive Aids and Beyond

Panelists: Blair MacIntyre (BM) Fred Brooks (FB) Peter Lee (PL) Yvonne Rogers (YR) Ivan Sutherland (IS)

Peter Lee is wearing a headset.

- BM: _I like the last session's title better than ours_

- IS: _I can tell you how VR started. Bell Helicopter company wanted to land
      at night ... put an infrared camera on the landing site and a display
      in the cockpit ... to test they used the roof of their building ...
      one day an observer in a Bell office is watching, though the camera,
      two men playing catch on the roof ... one player threw the ball at
      the camera and the observer ducked ... he had identified his position
      with the camera ... my observation was that you didn't need a camera,
      could substitute a computer ... the rest is history_
- YR: _my goal is to augment people ... [Englebart](https://en.wikipedia.org/wiki/The_Mother_of_All_Demos)
      very inspiring ... ok 2 stories ... (1) a student of mine wanted to help
      picky eaters ... computer vision for when they tried to hide peas under
      the plate ... projected colors onto the peas, called them 'disco peas',
      kids were distracted enough to get over their aversion ...  children and
      parents got involved, new social interaction ... (2) opera makeup for
      schoolchildren, virtually getting into character ... teenage boys in the
      classes got to try makeup for the first time ... singers found it useful
      for rehearsals_
- PL: _I feel socially awkward wearing this headset, but I have some of my
      slides here ... making a wearable headset requires huge effort ...
      research prototypes can be uncomfortable ... a product needs to be
      perfect and its very hard to do perfect ... one goal, give Lowe's VR
      to demo a virtual kitchen ... Case Western anatomy class used virtual
      cadaver, great collective experience_
- FB: _two stories ... (1) Henry Fuchs 1998, working with breast surgeon,
      try augmented reality to improve the precision of biopsy probe insertion
      ... 2 years to a working prototype, hard to track surgeon's eyes, display
      where probe is, where ultrasound is, provide low latency ... one day
      trying on live patient, worked 100% perfect probe right on the mark,
      jubilation ...  then the doctor had to tell the patient 'yes it is really
      cancer' ... (2) a challenge, augmented reality EMT training ... real
      teams, virtual patient, virtual surround ... track real tools, 8 eyes, 8
      images, team needs to interact_

#### question: what are current uses of augmented reality?

- IS: _the pilot of a jumbo jet typically has 1 hour flight experience before
      he flies for the first time, but extensive training in a flight simulator_
- FB: _the **best** AR_
- IS: _once I was in a flight simulator with the chief pilot ... and he turned
      to me and asked 'have you ever experienced a slow roll in a 747?' ... a
      slow roll is a twisting motion, a very benign maneuver, constant 1G
      pressure the plane doesn't know its upside down ... 'here we go' and
      suddenly the world inverted ... I remarked that it was certainly impressive,
      but didn't you treat the simulator as a real experience, and never attempted
      anything you would not do in reality? ... 'that is true, but I am the chief pilot'_
- PL: _construction, architecture_
- FB: _where's the 'augmented'?_
- PL: _whether augmented or virtual_
- FB: _yes we did my kitchen that way, made my wife sick when she tried it_
- PL: _surgical_
- FB: _still sounds virtual_
- YR: _displays on a car, superimposed directions on the tarmac ... one
      of the users took a vacation and had to use the old GPS technology
      ... found it very difficult to go back_

#### question: AR tools for developers?

- BM: _can developers write apps for the Microsoft [Hololens](https://www.microsoft.com/en-us/hololens)?_
- PL: _we belive in experience, anything we can do to foster experiences is good_
- FB: _faking things ... subtle and important ... I remember using a flight
      simulator, navigating the runway, and I turned my head to see if my wing
      was going to clip a plane ... turned and there was nothing there ...
      emotional shock to leave the simulation, I had been flying for 1 hour_
- IS: _pilot training is an early adopter because the cost of real planes
      is so high, impossible to train for emergency situations_
- IS: _the ultimate goal, you can sit in a virtual chair ... and if the chair
      has handcuffs you cannot get up ... a virtual bullet is lethal ...
      probably impossible because bits don't weigh anything ...
      you know Ben Franklin invented augmented reality, eyeglasses ... the
      desire outweighs cost ... I cannot see the audience here, maybe it
      would be good if I had a headset! but Peter took his off_
- PL: _because of my slides I couldn't see the audience, but then without
      the headset I couldn't see them either_

#### question: any challenges to suggest to the audience?

- PL: _if we had holographic transport, we wouldn't need planes!_
- YR: _maybe, but you need to give attendees a physical presence ...
      smell, touch_
- IS: _what makes us willing to work together? I had a collaboration with three
      people ... all in different locations .. communicated with a phone
      ... worked perfectly, because we had worked in the same location first
      and knew one another so well ... how to get to that point, where a
      simulation could be a useful tool ... another good observation by FB,
      given a domain X ask how good does the simulation need to be for X ...
      Licklider told me, you'd need damn good fiction to land someone on the
      moon, the simulation would need to provide every detail ... for flight
      simulation the user's imagination can fill some gaps, a pilot can recognize
      an aircraft carrier from a rougher picture_
- FB: _at IBM I once hired buses to bring the Poughkeepsie secretaries to the
      main office ... the secretaries at the two offices only knew one another
      from the phone ... this one lunch did so much good ... only $75 to rent
      a bus_
- PL: _how important is it to shake hands, to bump into a table?_
- YR: _for this conference, I think the live stream is getting a better
      experience because the cameras zoom in on us, the panelists ... the
      audience in the back cannot see us, only a picture of us on the monitors_
- PL: _one excellent video game, starts in the dark, you hear a voice ... turn
      around and there's a character sitting on a chair ... if you rearrange
      your furniture he finds a new seat ...
- YR: _games are a great example ... Pokemon Go ... Apple jusr released an
      app toolkit ... need to get those in schools, in the hands of kids who
      can build with them_

#### question: Ivan, about your 'ultimate display' paper, what has since surprised or frustrated you?

- IS: _I wasn't surprised because I never had any expectations ... of course
      sticks are not real ... no assumptions so no strong feelings_

#### question: people already distracted by cell phones, how to manage all this input?

- YR: _good question, how much data you can present to people ... and then
      the problem with google glass, your companions don't know what you
      are looking at ... at least with snapchat glasses, you can trust the
      device is simpler_
- PL: _good writing defines reality, bad writing reports it ... with the printing
      press, quickly went from 30,000 books to over 13,000,000 ... novels
      evolved shortly after, a new form of expression_

#### question: Peter, how long do your people wear the hololens?

- PL: _hard to say ... but often longer than the battery lasts_
- FB: _how long does it last?_
- PL: _depends what you're doing, 3 hours_
- FB: _that's encouraging, we had a 30-minute cutoff because participants had enough_

#### question: nausea

- PL: _I get nauseous in our minecraft VR ... but there's a pop-out feature
      where you keep playing, but the game world is in a TV set instead of
      around you ... can pop back in when you're feeling better_
- YR: _we've seen about 20% of the population gets nauseous_
- audience member: _Dana Boyd conducted an experiment, found the nausea was worse for wemon_
- YR: _oculus makes me feel sick, but the hololens has never given me trouble_
- PL: _have models to predict head motion, to keep the VR world steadier_
- BM: _I remember reading papers that measured framerate ... would be interesting
      to revisit_
- FB: _framerate not important, its the latency that gets you ... one colleague
      of mine, we call her 'the canary' because she's so sensitive, in fact ..._
- PL: _talking about nausea is part of the problem, people experience it more
      ... every time I talk about it in public my co-workers tell me to stop!_

- PL: _another cool application, there's a hololens app to give blind people
      a tour of the Redmond office ... you say a building and it takes you there_

- FB: _one challenge, the relative brightness of the real and virtual worlds_

#### question: any last remarks

- IS: _I hoped from the beginning that AR would be a teaching tool ... I learned
      that `F = MA` not from a book but from a large flywheel in the school's
      basement ... very substantial inertia ... the greatest value for AR
      would be to show people things in a way that makes the underlying meaning
      clear ... what color should the hydrogen atoms in a benzene ring be? the
      color will be fiction, but the quality of learning will depend on that
      fiction ... challenge for content makers ... what is the haptic experience
      of feeling bits?

### Small Group Session

After the last panel, I got to attend a small group session with other students,
 Dick Karp, and Don Knuth. It doesn't feel right to summarize or quote from the
 session, but there's one thing I want to write about.

During the group session, I said something that I now regret.
There was a brief silence as the group changed subjects, and someone
suggested that we do a round of introductions.
I objected, _this will take so long_,
 but in fact the introductions were a very good idea.

Normally, I don't like introductions because they focus on names, backgrounds,
 and credentials.
I don't care about any of these when I'm meeting someone!
Rather, I prefer to just talk and by-the-way learn about the other person(s).
There's an anaology to double-blind reviewing --- the focus should be content
 and not credentials.

These introductions were successful for two reasons.
First, they gave everyone in the room a turn to speak, and this seemed
 to help people join the actual discussion sooner.
That was strange to me.
I always feel a little nervous the first time I speak up in front
 of a group, but if I really feel like speaking then I can always get over this
 little barrier.
I guess it's not right to assume the nervousness is "little" for everyone.
Second, the introductions format was "say your name and a funny fact".
This prompt by itself led to some nice conversation topics:

- Could a computer program decide whether a statement was funny or not funny?
- What kind of humor works in a classroom? In a textbook?
- Would this kind of introduction be acceptable in another era or culture,
  for instance Victorian England?

"Nice" in the sense that everyone could contribute, which was a real challenge.
Even the question "does anyone have a favorite algorithm?" didn't have much
 success fostering discussion.

Related: a useful greeting at the event was "what SIG are you?".
The answer was a good hint about what level of abstraction you two could best communicate at.

<!-- ### Misc. -->
<!-- I noticed that some of the young people who served on panels and also gave -->
<!-- long-and-not-very-insightful answers to questions were later on their laptops -->
<!-- as other panels discussed things. I noticed some of the older people who -->
<!-- served on panels falling asleep during other panels -->

