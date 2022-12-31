## RECOMMENDATIONS ##

Rumbling, bumbling, stumbling...

- QB mobility
	- is only used to determine likelihood of scramble
	- Should also impact ability to avoid a rush / blitz (I did not check if response to blitz calls rollout code)

- Incomplete passes are sometimes "throw aways"
	- Pressure situations are the primary cause
	- Account for this in game, especially with PBP
	- Also account for blocked passes as part of this (it does happen!)

- Penalties
	- We have penalty frequency for each team
	- But how frequent are certain types of penalties?
	- How is the game determining which penalties are called
	- We need to consider timing of certain penalties (pass interference)
	- also consider when in the play they happen (how is the clock affected?)

- Punting / returns
	- be sure older teams (without punter) don't punt
	- be sure teams since "fair catch" option have the choice
	- also factor in for automated games to generate this (how often does it happen?)
	- how often do certain types of results happen from punts?
		- out of bounds
		- bounces past return man
		- coffin corner (and how often is it succesful?)
	- do we account for touchback (also with kicks?)
	- time for the play = time for punt (more with longer punts) + time for return

- Alternate rule types
	- Candian football rules
	- 3 downs
	- XFL rulesets
	- custom option for kickoffs

- Turnovers
	- do we account for turnovers in the end zone?
	- how are interceptions in the end zone handled?
	- we could look for pass lengths longer than "to goal" and process those???

- On-the-fly recommendation for QB mobility rating
	- with current code, base this on rushing average
	- with a database, this could be based on attempts / yardage

- Find reference on football formations
	- make as database
		- include what years they were used
		- include percentage of times they were used
	- data available per team or coach???

- Use information on formations to improve AI play calling

- Use infromation on formations to improve availability of plays

- Use historic PBP data to go beyond rush/pass pct
	- determine likelihood of calling plays on situations
	- consider situations like the following:
		- down # and long
		- down # and short
		- score deficit and clock time

- Allow choosing which receiver play is intended for
	- Account for % chance he's covered
	- Coverage impacts ability to complete
		- stat completion % wouldn't be the only thing driving a pass play
		- in other words, account for normal distribution of stats

- Consider Audibles
	- figure a % chance of it being called
	- at a minimum, include in the PBP based

- Address HOW weather affects play calling
	- no evidence of statistical impact on completion % / etc
		- in reality, you're more likely to run than pass (what %?)
		- you're also less likely to try for long kicks

	- impact starts with rain, wind over 15 MPH
		- fewer medium --> long passes
		- fewer FG attempts
	
	- in other words, teams are more conservative than risk the known impact


- Sack % of QB 
	- use this as factor in whether a play is a sack 
	- perhaps team sacks should accounted for vs. other teams in the league

- If pass play is called and runner is the QB, let PBP reflect that it's a trick play

- Consider implementing an aggression level for the AI
	- this can take form in a "scoring focus" - how likely the team is to go for a score
	- this can also affect how many long passes, out of bounds attempts, timeout calls, etc

- How often do plays go out-of-bounds normally?


## Helpful Links ##

[Play Tendencies](https://sites.northwestern.edu/msia/2020/01/31/nfl-tendency-analysis-and-basic-play-type-prediction/)

[NFLFastr](https://www.nflfastr.com/)

[Overview of Formations](https://en.m.wikipedia.org/wiki/List_of_formations_in_American_football)

[Early Formations](https://www.thefootballodyssey.com/anatomy-of-a-game-1/the-pioneer-years-the-birth-of-formation-football)

[College Play Calls](https://rpubs.com/Kazink/CFB_PlayCall)

[NFL Savant](http://nflsavant.com/about.php)

[Play Call Analysis](https://github.com/msf894/nfl-playcall-analysis-and-predictions)

[Weather Effects](https://www.covers.com/nfl/how-weather-affects-betting)

[Sports and Normal Distribution](https://sports.stackexchange.com/questions/8676/normal-distribution-and-sports)

[Stats and Regression to mean](http://statscenter.org/videos/)
