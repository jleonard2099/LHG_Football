
BRC = Best Record (Individual)
TRC = Team Record

___

## Game82 File Format ##
FILE.SCD
startGame
endGame
currentGame

___

## MISC CODE NOTES ##

#### Named Sub for 3900? ####
If so, need to rewrite GoTo 3360
- rewrite as a sub?
- EndOfQuarter
	-goes to 700 / 850 / 8500
	- use a parameter to track where to go after exit?
		
#### Menu Choices #### 
- OFFMENU is a mess
- DEFMENU loop ???
- Neither is a "menu", but rather an AI choice

#### Kickoff Subroutine #### 
- starts from 330
- it just gets called at the beginning
- but also elsewhere
	
#### Misc #### 
- FumblePBP + 650 (always together?)
- Make subroutines from 8530?
- InputKey loops: evaluate 3814, 4801
___

## Standards ##
- DEFAULTS = GameDefaults.cfg
- DEV.BAS = Paths.cfg
___

#### "DEFAULTS" file #### 

literally, in order, populates these variables

J6(0)		3		opponent type; 3 = PC vs PC; user options are in U5
G9			1		type of game (ruleset); 0 = college, 1 = NFL
H%			2		related to LO (location STRING)
N7			0		display pause
R5%			0		(if college , pre 1968...this gets set to 1?) 
					(all I can gather is this would make the clock not stopped after 1st down)
					(i don't see where this gets used in code)
Y8			3		pro rule year opt 0-3; 3 = after 1993
Y9			5		college rule year opt 0-5; 5 = after 1991
NG%(14)		2		sudden death?; 2 = to win!

___

### COMP CHEAT function ### 
- basically it's picking D based on play?
- D2 = line play
- D3 = pass coverage
- P2 = player 2 choice???

___

### Stadiums ### 
If not STADIUMS file:
- call it Memorial Stadium
- generate attendance between 10k-15k

___

#### What are I1,I2,J? ####
I1 = Offense Choice
     LOCATE ,14:PRINT "0 - ATTEMPT FIELD GOAL"
     LOCATE ,14:PRINT "1 - FAKE FIELD GOAL (RUN)"
     LOCATE ,14:PRINT "2 - FAKE FIELD GOAL (PASS)"

I2 = Defense Choice
     LOCATE ,14:PRINT "1 - GO FOR THE BLOCK"
     LOCATE ,14:PRINT "2 - CALL FIELD GOAL-SAFE"

___

## Documenting Changes ##
(1) Longer / more useful variable names
(2) Longer / more useful function names
(3) Documented random # generations
(4) Multiple IFs --> CASE/SELECT
(5) Updated formatting of FOR/NEXT, breakup lines for easy reading
(6) FOR/NEXT loops easier to read
(7) Updated logic of input statements; tranistion from GOTO to looping
(8) Removed unecessary labels / REM statements

___

## P2 Play Selection ##

### Overview ###
1- 8	Running Play => line 3400
9-18	Passing Play => Line 3500
19		Punt => Line 3800
20 		Field Goal => Line 3600
21 		Scouting Report => Line 6000
22 		Change Quarterback => Line 8299
23 		Show Stats => Line 900
24 		Select Kicker => Line 950

So, for example: anywhere you see "P2 = 20" as an assignment (not a comparison), the game is setting up a field goal try.

#### Breakdown ####

### Runs ###
1	??
2	??
3	??
4	??
5	??
6	??
7	??
8	??

### Passes ###
9	??
10	??
11	??
12	??
13	??
14	??
15	??
16	??
17	??
18	??

19	Punt
20	FG