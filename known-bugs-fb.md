
## Leaders / Standings --> View / Print Reports
- Uses stat file name, not team name
	- read in from .LDR file --> TM$()
	- this is needed in the file to identify the stats to load
	- my guess is we'd need to add the team to the compiled stats (restructuring)
- Happens in the original game
- Three separate modules
	- SEEPRO
	- SEECOLL
	- SEENATL

## Ticker Scores 
Scores are 0's too much
- SX%(), SX$() are related variables
- assume it's because we only look at games to 32
- If so we should increase upper bound to cover max games in a schedule
- Need to test behavior against original game (is it a BUG?)

___

## Menu Redundancy ##

#### Team Menu ####
- Input A Team --> Input a Team
- Trade Player --> Trade Player
- Align Statistical Files --> Align Statistical Files

### Game Play ###

#### Halftime Stats ####
- First page of halftime stats is not full screen
- Also, "display stats again?" is on grey background
	- We can keep the "grey background" as a standard for all games
	- We can remove it from this screen only, as the grey background is really for the "boxes" being drawn on the screen

#### END OF GAME ####
- doesn't wait for a key press before prompting about boxscores
- would need to call "DECIDE"
	
#### DECIDE function ####
- It relies on GetSpeed but that's probably unecessary
- GetSpeed is called by BoxScreen without DECIDE
		
#### Scheduler ####
- Doesn't look to see if files exist (Box scores, etc.)
- ACCESS USER SAVED BOXSCORES ---> no prompt (CsMsgBox) if none are found!!!!
- no warnings when files are deleted!!!

#### SEECOLL ####
- View / Print Conference Reports
- Error Line 100
	- when choosing Pro League for College Conference
	- needs to be some file validation

#### Randomize Timer ####
NOT SO MUCH A BUG, BUT...
(1) Before game starts
(2) Immediately when a TOUCHDOWN is scored
(3) At HalfTime

___

## "THE FIELD GOAL BUG" ##

### College ###
- Newer teams are definitely taking FG's regularly
- However it still seems normal without the fix

### Pro ###
- This is the one I'm worried about
- It seems to be consistent with trying FG's only in Q2/Q4
- This logic has seemingly been ignored as a factor

_In other words, it seems the real issue is using Pro rules_

### AI Field Goal Choice Situations  ###

#### 4th Quarter ####
I haven't considered yet if I agree with these choices
- 4th Down, Opponent's 32 to 38, Score = 3 Pt. Margin, Pro Rules or College Rules + Team after 1960
- 4th Down, Opponent's 32 to 38, Score <= 3 Pt. Lead, Pro Rules or College Rules + Team after 1960
- 4th Down, <= 2 Yds to Go, Opponent's 25 - 35, Score - Down by 10 or less, 4 Minutes Remaining, Pro Rules or College Rules + Team after 1960
- 4th Down, More Than 2 Minutes, Score - Down by 4 to 10, Inside Red Zone
- 4th Down, Down by 4 or More, Inside Red Zone

#### Any Quarter ####
I haven't considered yet if I agree with these choices
- 4th Down, 3+ Yds To Go, Inside Opponent's 32, Pro Rules or College Rules + Team after 1960
- 4th Down, Any Time, Inside Opponent's 35, Pro Rules or College Rules + Team after 1960
- Any Down, Less than 22 seconds to go, 0 Timeouts Offense, Inside Opponent's 35
- Any Down, Less than 12 seconds to go, 0 Timeouts Offense, Inside Opponent's 35

#### 2nd Quarter ####
I'm in the middle on these choices
- 4th Down, Inside Opponent's 35, Pro Rules, or College Rules + Team after 1960
- 4th Down, Any Time, Inside Opponent's 35, Pro Rules or College Rules + Team after 1960

*RECOMMEND*: based on score, you may want to go for TD
*EVALUATE*: does this still happen based on the "Any Quarter" conditions?

#### 2nd Quarter | Overtime | 4th Quarter and Tied Score ####
I fully agree with this choice
- Any down, inside opponent's 35, Less than 22 seconds to go
