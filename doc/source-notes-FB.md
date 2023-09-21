## P2 Play Selection ##

### Overview ###
|Value	| Play Type         |
|-------|-------------------|
|1- 8	|Running Plays 	    |
|9-18	|Passing Plays       |
|19	|Punt               |
|20 	|Field Goal 	    |
|21 	|Scouting Report    |
|22 	|Change Quarterback |BRC = Best Record (Individual)
|23 	|Show Stats	    |
|24 	|Select Kicker	    |
|??     |QB Kneel Down      |

So, for example: anywhere you see "P2 = 20" as an assignment (not a comparison), the game is setting up a field goal try.

### Breakdown ###

#### Runs ####
|Value	|Play	                                         |
|-------|------------------------------------------------|
|1	|Blast	                                         |
|2	|Dive / Counter	                                 |
|3	|Off Tackle / Isolation                          |
|4	|Toss Sweep / Power Sweep / Quick Pitch          |
|5	|Option	                                         |
|6	|Draw / Sprint Draw	                         |
|7	|Trap / Misdirection / Quick Trap / Over-the-Top |
|8	|QB Sneak                                        |


#### Passes ####
|Value	|Play	                             |
|-------|------------------------------------|
|9	|3-Step Drop	                     |
|10	|Short Pass	                     |
|11	|Medium Pass / Cross / Curl          |
|12	|Long Pass / Deep Post / Deep Corner |
|13	|Screen Pass	                     |
|14	|Med Play Act Pass	             |
|15	|Long Play Act Pass	             |
|16	|Sideline Pass	                     |
|17	|Rollout Pass	                     |
|18	|Bootleg Pass	                     |

#### Special ####
|Value	|Play	|
|-------|-------|
|19	|Punt	|
|20	|FG	|


## Game Play ##

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
	
#### COMP CHEAT function ####
- basically it's picking D based on play chosen?
- D2 = line play
- D3 = pass coverage
- P2 = player 2 choice???

#### Others #### 
- FumblePBP + 650 (always together?)
- Make subroutines from 8530?
- InputKey loops: evaluate 3814, 4801

## Misc / Menus ##

#### What are I1,I2,J? ####
I1 = Offense Choice

	LOCATE ,14:PRINT "0 - ATTEMPT FIELD GOAL
	LOCATE ,14:PRINT "1 - FAKE FIELD GOAL (RUN)
	LOCATE ,14:PRINT "2 - FAKE FIELD GOAL (PASS)

I2 = Defense Choice

	LOCATE ,14:PRINT "1 - GO FOR THE BLOCK"
	LOCATE ,14:PRINT "2 - CALL FIELD GOAL-SAFE"

## New File Standards ##
- DEFAULTS = GameDefaults.cfg
- DEV.BAS = Paths.cfg
