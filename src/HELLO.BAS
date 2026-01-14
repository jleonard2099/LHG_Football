$Debug
$Color:32

Const GAME_VERSION$ = "7.15"
Const MAX_FILES = 10240
Const MAX_CAREER_YEARS = 15
Const MAX_CONFERENCES = 40
Const MAX_SCHED_STATS = 120
Const MAX_SCHEDULE_GAMES = 2000
Const MAX_TOURNAMENT_GAMES = 200
Const MAX_TEAMS = 1000
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV_COL = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

Const TEAMS_PER_DIV_PRO = 10
Const DIVS_PER_LEAGUE = 4
Const TEAMS_PER_LEAGUE = 40

Const SCHEDULE_SIZE_BYTES = 56
Const LAST_COL = 80
Const LAST_ROW = 30
Const SCHED_ENTRIES_PER_SCREEN = 25
Const MAX_PBP_LINES = 4

'Adjust these to change the frequency
'of penalties being called on special teams
Const PK_PENALTY_THRESHOLD = 3
Const KO_PENALTY_THRESHOLD = 775

DefInt A-Z

' General purpose
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'StringManip.bi'
'$Include: 'QBWRITE.bi'

' For Utils
'$Include: 'BoxRoutines.bi'
'$Include: 'FileTeamSelection.bi'
'$Include: 'Variables.bi'
'$Include: 'MainRoutines.bi'
'$Include: 'TITLE.bi'

' For Game
'$Include: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
'Defaults for Event Settings
Data 5,9,3,99,3,99,3,299,4,3,0,0,0
For X = 1 To 13:
    Read eventSettings(X)
Next

Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING TOUCHDOWNS,RUSHING AVERAGE,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING TOUCHDOWNS,RECEIVING AVERAGE,LONGEST CATCH
Data PASS ATTEMPTS,PASS COMPLETIONS,PASSING TOUCHDOWNS,HAD INTERCEPTED,PASSING YARDS
Data COMPLETION PCT.,PASSER RATING,LONGEST PASS,TOTAL YARDS,SCORING (NON-KICKER)
Data SCORING (KICKERS),TOTAL SCORING,KICKOFF RETURNS,KICKOFF YARDS,KICKOFF TOUCHDOWNS
Data KICKOFF RETURN AVERAGE,LONGEST KO RETURN,PUNT RETURNS,PUNT RETURN YARDS
Data PUNT RETURN TOUCHDOWNS,PUNT RETURN AVERAGE,LONGEST PUNT RETURN,PUNTS
Data PUNTING YARDS,PUNTING AVERAGE,FIELD GOALS MADE,FIELD GOALS ATTEMPTED
Data FIELD GOAL %,LONGEST FIELD GOAL,POINT AFTER MADE,POINT AFTER ATTEMPTS,PAT%
Data INTERCEPTIONS,INTERCEPTION YARDS,INTERCEPTION TD'S,LONGEST INTERCEPTION
For I = 0 To 45:
    Read indRecCategory$(I)
Next

Data YARDS/GAME,TOTAL YARDS,GAIN/PLAY,RUSHING YARDS/GAME,TOTAL RUSHING
Data GAIN/RUSH,PASSING YARDS/GAME,PASSING YARDS,YARDS/ATTEMPT
Data YARDS/COMPLETION,COMPLETION PCT.,INTERCEPTION PCT.,POINTS SCORED
Data POINTS/GAME,MARGIN OF VICTORY,PUNTING AVERAGE,PUNT RETURN AVG
Data KICKOFF RETURN AVG,FIRST DOWNS,FIRST DOWNS/GAME,MOST FUMBLES
Data MOST FUMBLES LOST,FEWEST FUMBLES LOST,PENALTIES/GAME,PENALTY YARDS/GAME
Data YARDS/GAME,YARDS ALLOWED,YARDS/PLAY,MOST YARDS/GAME
Data RUSHING YARDS/GAME,RUSHING YARDS,GAIN/RUSH,PASSING YARDS/GAME
Data PASSING YARDS,YARDS/ATTEMPT,YARDS/COMPLETION,COMPLETION PCT.
Data INTERCEPTION PCT.,POINTS ALLOWED,POINTS/GAME,MOST POINTS ALLOWED
Data MOST PTS ALLOWED/GAME,PUNT RET AVERAGE,KICKOFF RET AVERAGE
For I = 0 To 43:
    Read teamRecCategory$(I)
Next


Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,RUSING TOUCHDOWNS,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING AVERAGE,RECEIVING TOUCHDOWNS,LONGEST RECEPTION
Data PASS ATTEMPTS,PASS COMPLETIONS,INTERCEPTIONS,PASSING YARDS,TOUCHDOWN PASSES,COMPLETION PERCENTAGE,AVERAGE YDS/COMP.,QUARTERBACK RATING,LONGEST PASS
Data KICKOFF RETURNS,KICKOFF RETURN YARDS,AVERAGE GAIN/KO RETURN,KICKOFF RETURN TD'S,LONGEST KICKOFF RETURN
Data PUNT RETURNS,PUNT RETURN YARDS,AVERAGE GAIN/PUNT RETURN,PUNT RETURN TD'S,LONGEST PUNT RETURN
Data PUNTS,PUNTING YARDS,AVERAGE PER PUNT
Data F.G.'s MADE,F.G. ATTEMPTS,F.G. PERCENTAGE,E.P.'s MADE,E.P. ATTEMPTS,E.P. PERCENTAGE,LONGEST FIELD GOAL
Data INTERCEPTIONS,SACKS
Data NON-KICKERS,KICKERS,ALL PLAYERS
Data FIRST DOWNS,RUSH ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,PASSING YARDS,TOTAL YARDS,PASS COMPLETION PCT.,FUMBLES,FUMBLES LOST,INTERCEPTIONS THROWN,TURNOVER RATIO,PENALTIES,PENALTY YARDAGE,SACKS ALLOWED,SACK YARDAGE
Data OPP. FIRST DOWNS,OPP. RUSH ATTEMPTS,OPP. RUSHING YARDS,OPP. RUSHING AVERAGE,OPP. PASSING YARDS,OPP. TOTAL YARDS,OPP. PASS COMPLETION PCT.,OPP. FUMBLES,OPP. FUMBLES RECOVERED,INTERCEPTIONS,OPP. PENALTIES,OPP. PENALTY YARDAGE,SACKS
Data SACK YARDAGE LOST,ALL-PURPOSE YARDS
For I = 0 To 73:
    Read expCategories$(I)
Next


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
'Indicators for ticker periods
'L = Later; H = Half; F = Final
Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F

For I = 0 To 14:
    Read tickerPeriod$(I)
Next

Data 4-3 READ,5-2 READ,4-3 PINCH,5-2 PINCH
Data 4-3 PASS RUSH,5-2 PASS RUSH
Data 4-3 INSIDE STUNT,5-2 INSIDE STUNT,4-3 OUTSIDE STUNT,5-2 OUTSIDE STUNT

Data STANDARD ZONE,STANDARD M-M,SHORT ZONE,MAN UNDER,NICKEL,PREVENT
Data BLITZ (1-ILB),BLITZ (1-OLB),BLITZ (2-ILB),BLITZ (2-OLB),BLITZ ENGAGE

For I = 1 To 10:
    Read defFormation$(I)
Next I

For I = 1 To 11:
    Read passCov$(I)
Next I

'Below values are for defYdAdj
'This impacts rushing yards (modern eras)
Data 0,3,-6,0,-3,-3,-3,0,0,3,3,6: 'Blast
Data 0,3,-3,0,0,0,-6,0,0,6,3,6: 'Dive
Data -3,0,0,3,0,0,3,0,0,-3,3,6: 'Off Tackle
Data 0,-3,3,0,0,0,3,3,-3,-3,0,3: 'Sweep
Data 0,-3,6,0,0,0,6,3,-3,-6,-3,-3: 'Option
Data -6,-6,-3,-3,6,6,-3,-3,-3,-3,-3,-6: 'Draw
Data 3,3,-3,-3,-3,-3,-6,-6,-6,-6,3,6: 'Trap

For I = 1 To 7:
    For J = 1 To 12:
        'Rush Yd Adjustments (Based on DEF)
        Read defYdAdj(I, J)
    Next J
Next I

'FIELD GOAL %'s / INTERVALS
Data 74,88,48,78,26,68,14,62,9,52:

'Values for passing adjustments
Data 0,3,-4,3,0,3,0,0,0,0,3: ' Int % Hitch
Data 0,0,-4,0,0,3,0,0,0,0,3: ' Int % Short Pass
Data -4,0,3,-4,-4,-6,3,3,4,4,4: ' Int % Med Pass
Data -4,0,4,-4,-4,-6,3,3,4,4,4: ' Int % Long Pass
Data 3,4,0,4,3,4,0,0,-4,-4,0: ' Int % Screen
Data -4,0,3,-4,-4,-4,3,3,4,4,4: ' Int % Med Play Act
Data -4,0,4,-4,-4,-6,4,4,4,4,4: ' Int % Long Play Act
Data 0,-4,3,-4,-4,-6,3,3,4,4,4: ' Int % Sideline
Data 3,0,3,0,3,3,0,0,0,0,3: ' Int % Rollout
Data 3,0,3,0,3,3,0,0,0,0,3: ' Int % Bootleg

Data 31,21,21,11,40,50,8,8,-5,-5,-13: ' Comp% Hitch
Data 21,11,11,1,30,40,-10,-10,-20,-20,-23: ' Comp% Short Pass
Data 1,-9,6,-4,-5,-10,-19,-19,-17,-17,-13: ' Comp% Med Pass
Data -19,-29,-9,-19,-30,-40,-25,-25,-25,-25,-17: ' Comp% Long Pass
Data 40,40,40,40,50,50,20,20,-24,-24,-33: ' Comp% Screen
Data 6,-19,-9,-19,-10,-10,-17,-17,-13,-13,-6: ' Comp% Med Play Act
Data -14,-29,-19,-29,-30,-40,-25,-25,-17,-17,-6: ' Comp% Long Play Act
Data 6,-14,1,-4,10,10,-13,-13,-19,-19,-17: ' Comp% Sideline
Data -5,5,-10,10,0,0,-19,-17,-19,-13,-13: ' Comp% RolloutOUT
Data -5,5,-10,10,0,0,-19,-17,-19,-13,-13: ' Comp% Bootleg

Data 0,1,-1,0,1,2,1,1,2,2,3: ' Y/C Hitch
Data 0,1,-1,0,1,2,3,3,21,21,25: ' Y/C Short Pass
Data 0,1,0,0,1,2,21,21,25,25,28: ' Y/C MED
Data 0,1,1,2,-1,-1,33,33,42,42,42: ' Y/C Long Pass
Data 0,1,-1,0,1,2,1,1,25,25,36: ' Y/C Screen
Data 1,1,-1,-2,-1,-1,25,25,28,28,31: ' Y/C Med Play Act
Data 1,1,1,2,-1,-1,42,42,42,42,42: ' Y/C Long Play Act
Data 0,-1,1,2,1,2,18,18,28,28,33: ' Y/C Sideline
Data 0,1,-1,0,1,2,21,25,21,28,28: ' Y/C Rollout
Data 0,1,-1,0,1,2,21,25,21,28,28: ' Y/C Bootleg

Data -8,-8,-4,-4,-8,-8,-6,-8,-4,-8,-4: ' Sack Hitch
Data -4,-4,0,0,-8,-8,-2,-2,0,0,2: ' Sack Short Pass
Data 0,0,-2,-2,2,-6,3,3,13,13,23: ' Sack Med Pass
Data 4,4,0,0,8,-4,13,13,23,23,33: ' Sack Long Pass
Data -8,-8,-8,-4,-8,-8,-4,-4,0,0,3: ' Sack SCRN
Data 0,0,-2,-2,2,-4,13,13,23,23,33: ' Sack Med Play Act
Data 4,4,0,0,8,-2,23,23,33,33,43: ' Sack Long Play Act
Data -4,-4,-2,-2,2,-6,0,0,3,3,13: ' Sack Sideline
Data -6,-6,-4,-4,-8,-8,3,13,3,23,23: ' Sack Rollout
Data -6,-6,-4,-4,-8,-8,3,13,3,23,23: ' Sack Bootleg

Data 0,0,1,4,11,21,31,49,62,74,81,85,88,90,92: ' brkawayMod(0,X)
Data 3,5,8,10,15,23,32,47,58,68,75,80,85,88,92: ' brkawayMod(1,X)

Data 69,80,77,61,62,77,64,45,36,69,49,11,2,15,48,45,31,21,29,78,55,48,65,70,58,61,78,70,68,65,70,63,65,70,76,81,87,79: ' runYdAdj(0,X)
Data -4,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,6,7: ' runYdAdj(1,X)

'Breakaway Yards
Data 11,-5,11,0
Data 11,-5,11,0
Data 11,0,21,0
Data 11,0,21,0
Data 11,0,11,10
Data 11,0,11,10
Data 11,0,21,10
Data 11,0,21,10
Data 11,0,21,10
Data 11,10,21,10
Data 11,10,31,10
Data 11,10,31,10
Data 21,10,31,10
Data 21,10,51,10
Data 21,10,51,10
Data 21,10,41,20
Data 21,10,51,20
Data 21,10,81,20
Data 21,10,71,30
Data 21,10,61,40
Data 21,10,31,70
Data 31,10,51,50
Data 31,10,31,70
Data 41,10,31,70
Data 51,10,31,70
Data 41,20,11,90
Data 41,20,11,90
Data 21,40,11,90
Data 21,40,11,90
Data 71,30,11,90
Data 61,40,11,90
Data 51,50,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90

'GOAL LINE
Data 72,-5,83,-4,75,-4,68,-4,80,-3,70,-3,60,-3,75,-2,60,-2,45,-2,60,-1
Data 30,-1,100,0,30,1,60,1,45,2,60,2,75,2,60,3,70,3,80,3,68,4,75,4,83,4
Data 72,5,78,5,84,5,90,5,96,5,96,5

'Blitz Coverage Adj
Data 28,89,99,100,0,0,0,0,0,0,0,0,0,0: 'Hitch 1
Data 10,53,96,97,98,99,100,0,0,0,0,0,0,0: 'Hitch 2
Data 2,37,83,96,97,98,99,100,0,0,0,0,0,0: ' Hitch 3
Data 2,32,71,92,94,96,97,98,99,100,0,0,0,0: ' Hitch 4
Data 2,22,59,92,93,94,100,0,0,0,0,0,0,0: ' Hitch 5
Data 2,12,63,88,90,92,93,94,95,96,97,98,99,100: ' Hitch 6
Data 2,6,38,86,88,90,92,94,95,96,97,98,99,100: ' Hitch 7
Data 2,6,25,74,82,90,92,94,95,96,97,98,99,100: ' Hitch 8
Data 2,6,25,69,77,85,87,90,92,95,97,98,99,100: ' Hitch 9
Data 2,6,25,66,74,82,84,87,89,92,94,96,98,100: ' Hitch 10
Data 34,72,98,99,100,0,0,0,0,0,0,0,0,0: ' Screen 1
Data 14,54,97,98,99,100,0,0,0,0,0,0,0,0: ' Screen 2
Data 10,55,80,95,96,97,98,99,100,0,0,0,0,0: ' Screen 3
Data 10,51,75,91,93,95,96,97,98,99,100,0,0,0: ' Screen 4
Data 10,30,70,90,92,94,95,96,97,98,99,100,0,0: ' Screen 5
Data 10,35,71,86,88,90,92,94,95,96,97,98,99,100: ' Screen 6
Data 10,32,68,83,85,87,89,91,93,95,96,98,99,100: ' Screen 7
Data 10,29,65,80,82,85,87,89,91,93,95,97,99,100: ' Screen 8
Data 10,21,57,72,77,82,86,89,91,93,95,97,99,100: ' Screen 9
Data 10,13,49,64,72,79,85,89,91,93,95,97,99,100: ' Screen 10
Data 0,97,99,100,0,0,0,0,0,0,0,0,0,0: ' Short Pass 1
Data 0,45,96,98,99,100,0,0,0,0,0,0,0,0: ' Short Pass 2
Data 0,10,82,97,98,99,100,0,0,0,0,0,0,0: ' Short Pass 3
Data 0,0,63,94,95,96,97,98,99,100,0,0,0,0: ' Short Pass 4
Data 0,0,45,91,93,95,96,97,98,99,100,0,0,0: ' Short Pass 5
Data 0,0,45,80,86,92,94,95,96,97,98,99,100,0: ' Short Pass 6
Data 0,0,40,70,80,90,93,94,95,96,97,98,99,100: ' Short Pass 7
Data 0,0,40,65,75,85,88,90,93,94,96,98,99,100: ' Short Pass 8
Data 0,0,30,51,66,81,88,90,92,94,96,98,99,100: ' Short Pass 9
Data 0,0,20,37,57,77,88,90,92,94,96,98,99,100: ' Short Pass 10
Data 17,77,94,99,100,0,0,0,0,0,0,0,0,0: ' Med Pass 1
Data 6,35,76,96,97,98,99,100,0,0,0,0,0,0: ' Med Pass 2
Data 0,0,52,92,96,98,100,0,0,0,0,0,0,0: ' Med Pass 3
Data 0,0,40,80,87,93,95,97,99,100,0,0,0,0: ' Med Pass 4
Data 0,0,4,69,81,89,94,96,98,100,0,0,0,0: ' Med Pass 5
Data 0,0,5,58,70,78,83,87,91,95,98,100,0,0: ' Med Pass 6
Data 0,0,0,41,57,70,78,83,88,92,95,98,100,0: ' Med Pass 7
Data 0,0,0,19,36,51,66,78,88,92,94,96,98,100: ' Med Pass 8
Data 0,0,0,13,26,43,61,76,86,90,93,96,98,100: ' Med Pass 9
Data 0,0,0,11,22,33,51,66,76,82,87,92,96,100: ' Med Pass 10
Data 1,56,98,100,0,0,0,0,0,0,0,0,0,0: ' Long Pass 1
Data 0,0,53,95,96,100,0,0,0,0,0,0,0,0: ' Long Pass 2
Data 0,0,0,67,87,97,98,99,100,0,0,0,0,0: ' Long Pass 3
Data 0,0,0,13,65,97,98,99,100,0,0,0,0,0: ' Long Pass 4
Data 0,0,0,0,32,64,96,97,98,99,100,0,0,0: ' Long Pass 5
Data 0,0,0,0,12,40,82,86,90,93,96,98,100,0: ' Long Pass 6
Data 0,0,0,0,0,26,69,75,81,86,91,94,97,100: ' Long Pass 7
Data 0,0,0,0,0,0,17,40,64,72,80,88,94,100: ' Long Pass 8
Data 0,0,0,0,0,0,6,29,54,64,74,84,92,100: ' Long Pass 9
Data 0,0,0,0,0,0,3,23,44,56,68,80,90,100: ' Long Pass 10

'playActAdj
Data 0,10,20,30,40,50,60,70,80,90,50,55,60,65,70,75,80,85,90,95,100
Data 0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2

'QB Arm Rating ADJs
Data 0,10,20,30,40,50,60,70,40,45,50,55,60,65,70,50,53,57,60,63,67
Data 0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3

For I = 0 To 4:
    'Field Goal adjustments?
    Read adjFGPct(I)
    Read adjFGLine(I)
Next I

For I = 0 To 3:
    '0 = INT%
    '1 = COMP%
    '2 = Y/C
    '3 = Sack
    For I1 = 1 To 10:
        '1 = Hitch
        '2 = Short Pass
        '3 = Medium Pass
        '4 = Long Pass
        '5 = Screen
        '6 = Med Play Action
        '7 = Long Play Action
        '8 = Sideline
        '9 = Rollout
        '10 = Bootleg
        For J = 1 To 11:
            'Pass Adjustments per coverage
            Read passCovAdj(I, I1, J)
        Next J
    Next I1
Next I

For I = 0 To 1:
    For J = 0 To 14:
        'Run yard adjustments?
        Read brkawayMod(I, J)
    Next
Next

For I = 0 To 1:
    For J = 1 To 38:
        'Run yard adjustments?
        Read runYdAdj(I, J)
    Next
Next

For I = 1 To 38:
    For J = 1 To 4:
        'Breakaway yardage???
        Read brkawayYds(I, J)
    Next
Next

For I = 1 To 30:
    For J = 1 To 2:
        'Goalline yard adjustments
        Read goalLnYdAdj(I, J)
    Next
Next

For I = 1 To 5:
    '1 = Hitch
    '2 = Screen
    '3 = Short PASS
    '4 = Medium Pass
    '5 = Long Pass
    For I1 = 1 To 10:
        For J = 1 To 14:
            'What type of passing adjustment is this?
            'It doesn't cover all types of pass plays
            Read blitzCovAdj(I, I1, J)
        Next
    Next
Next

For I = 1 To 2:
    For J = 1 To 21:
        'Play Action pass adjustment???
        Read playActAdj(I, J)
    Next
Next

For I = 1 To 2:
    For J = 0 To 20:
        'Arm Rating Adjustments
        Read ydsPerCompPctAdj(J, I)
    Next
Next


Data BLAST,DIVE,OFF TACKLE,TOSS SWEEP,OPTION RUN,DRAW PLAY,MISDIRECTION
Data QB SNEAK,POWER SWEEP,QUICK PITCH,QUICK TRAP,ISOLATION,SPRINT DRAW
Data OVER THE TOP,COUNTER
Data 3 STEP DROP,SHORT PASS,MEDIUM PASS,LONG PASS,SCREEN PASS
Data MED PLAY ACT PASS,LONG PLAY ACT PASS,SIDELINE PASS,ROLLOUT PASS
Data BOOTLEG PASS,CROSS,CURL PASS,DEEP POST,DEEP CORNER

For I = 1 To 29:
    Read playDesc$(I)
Next

Data 7-2 BASE,7-2 INSIDE STUNT,7-2 OUTSIDE STUNT
Data 6-2 BASE,6-2 INSIDE STUNT,6-2 OUTSIDE STUNT
Data 5-3 BASE,5-3 PINCH,5-3 INSIDE STUNT,5-3 OUTSIDE STUNT

For I = 1 To 10:
    Read defFormEraA$(I)
Next I

'Below values are for rush yard adjustments
'These apply to eras before 1945
Data -3,-3,0,-3,-3,0,0,-3,0,3,3,6
Data -3,-3,0,-3,-3,0,-3,-3,-3,0,3,6
Data 0,3,0,3,3,0,0,0,0,-3,3,6
Data 6,6,-3,6,6,-3,3,6,6,0,0,3
Data 6,6,3,6,6,3,3,3,3,0,-3,-3
Data 0,3,6,0,3,6,0,0,-3,0,-3,-6
Data -3,-3,0,0,-3,3,0,-3,0,3,3,6

For I = 1 To 7:
    For J = 1 To 12:
        'Rush Yd Adjustments (Based on DEF)
        Read defYdAdjEraA(I, J)
    Next J
Next I

Data 6-2 BASE,6-2 INSIDE STUNT,6-2 OUTSIDE STUNT
Data 5-3 BASE,5-3 INSIDE STUNT,5-3 OUTSIDE STUNT
Data 5-2 READ,5-2 PINCH,5-2 INSIDE STUNT,5-2 OUTSIDE STUNT

For I = 1 To 10:
    Read defFormEraB$(I)
Next I

'Below values are for rush yard adjustments
'These apply to 1945 - 1956
Data -3,-3,0,0,0,3,-3,-6,-3,0,3,6
Data -3,-3,0,-3,-3,0,-3,-3,-3,0,3,6
Data 3,3,0,0,0,-3,0,0,0,-3,3,6
Data 6,6,-3,3,6,0,0,0,0,-6,0,3
Data 6,6,3,3,3,0,0,3,3,-3,-3,-3
Data 0,3,6,0,-3,0,0,0,0,0,-3,-6
Data 0,-3,3,0,0,3,-3,-3,-6,0,3,6

For I = 1 To 7:
    For J = 1 To 12:
        'Rush Yd Adjustments (Based on DEF)
        Read defYdAdjEraB(I, J)
    Next J
Next I


' Game options
'location$(0) = "@ VISITING TEAM"
location$(1) = "HOME FIELD  "
location$(2) = "NEUTRAL SITE"

overtime$(0) = "NO OVERTIME                    "
overtime$(1) = "1 QUARTER (1974-2011)          "
overtime$(2) = "COMPETE TO WIN (1974-2009)     "
overtime$(3) = "NCAA DIVISION I-AA (1996-2018) "
overtime$(4) = "NCAA DIVISION I-AA (2019-2020) "
overtime$(5) = "NCAA DIVISION I-AA (2021+)     "
overtime$(6) = "MOD. REGULAR SEASON (2012-2016)"
overtime$(7) = "MOD. REGULAR SEASON (2017+)    "
overtime$(8) = "MOD. POSTSEASON (2010+)        "

modeAbbrev$(0) = "HH"
modeAbbrev$(1) = "CV"
modeAbbrev$(2) = "CH"
modeAbbrev$(3) = "CC"

playMode$(0) = "HUMAN OPPONENT           "
playMode$(1) = "COMPUTER IS VISITING TEAM"
playMode$(2) = "COMPUTER IS HOME TEAM    "
playMode$(3) = "COMPUTER VS COMPUTER     "

rulesType$(0) = "PLAY USING COLLEGE RULES   "
rulesType$(1) = "PLAY USING NFL RULES       "
rulesType$(2) = "PLAY USING USFL (80s) RULES"
rulesType$(3) = "PLAY USING AFL RULES       "
rulesType$(4) = "PLAY USING WFL RULES       "
rulesType$(5) = "PLAY USING WLAF RULES      "
'rulesType$(6) = "PLAY USING XFL RULES       "

ruleYrPro$(0) = "PRE-1962"
ruleYrPro$(1) = "1962-1973"
ruleYrPro$(2) = "1974-1977"
ruleYrPro$(3) = "1978-1993"
ruleYrPro$(4) = "1994-2007"
ruleYrPro$(5) = "2008-2010"
ruleYrPro$(6) = "2011-2015"
ruleYrPro$(7) = "2016-2017"
ruleYrPro$(8) = "2018-2022"
ruleYrPro$(9) = "2023"
ruleYrPro$(10) = "2024"
ruleYrPro$(11) = "2025+"

ruleYrColl$(0) = "PRE-1968 "
ruleYrColl$(1) = "1968-1980"
ruleYrColl$(2) = "1981-1983"
ruleYrColl$(3) = "1984"
ruleYrColl$(4) = "1985"
ruleYrColl$(5) = "1986-1990"
ruleYrColl$(6) = "1991     "
ruleYrColl$(7) = "1992-2006"
ruleYrColl$(8) = "2007"
ruleYrColl$(9) = "2008-2011"
ruleYrColl$(10) = "2012-2017"
ruleYrColl$(11) = "2018-PRESENT"

weather$(0) = "GOOD                          "
weather$(1) = "BAD (RAIN/SNOW)               "
weather$(2) = "TERRIBLE (BLIZZARD/HEAVY RAIN)"
weather$(3) = "COMPUTER SELECT               "

windSetting$(0) = "NONE             "
windSetting$(1) = "LIGHT TO MODERATE"
windSetting$(2) = "STRONG AND GUSTY "
windSetting$(3) = "COMPUTER SELECT  "

yesNoText$(0) = "NO "
yesNoText$(1) = "YES"

yesNo$(0) = "N"
yesNo$(1) = "Y"

'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

If _FileExists(cfgPath$ + "errlog") Then
    Kill cfgPath$ + "errlog"
End If

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

Select Case opSysType$
    Case "[WINDOWS]":
        dirSeparator$ = "\"
    Case "[MAC]", "[MACOSX]", "[LINUX]":
        dirSeparator$ = "/"
End Select

'Default new folders to current path
For I = 0 To 3
    diskPaths$(I) = _CWD$ + dirSeparator$
Next

' Test if we can write locally
CanWriteLocally = 0 ' Assume FALSE
On Error GoTo WriteTestFailed
Open "WRITETEST.TMP" For Output As #1
Close #1
Kill "WRITETEST.TMP"
CanWriteLocally = 1 ' SUCCESS - TRUE
WriteTestFailed:
On Error GoTo Errhandler

' Determine base path for config files
If (CanWriteLocally = 1) Or dirSeparator$ = "/" Then
    ' Can write locally - use local folders (portable/Mac/Linux scenario)
    cfgPath$ = _CWD$
Else
    ' Can't write locally - use AppData (Windows restricted scenario)
    cfgPath$ = Environ$("LOCALAPPDATA") + "\LH Games\3-in-1 Football\"
End If

'Read-in existing DEV.BAS
If _FileExists(cfgPath$ + "DEV.BAS") Then

    Open cfgPath$ + "DEV.BAS" For Input As #1

    'DEV.BAS will not have the "Schedule" folder configured
    For I = 0 To 3
        Input #1, diskPaths$(I)
    Next

    Close #1

    Kill cfgPath$ + "DEV.BAS"

End If

If Not _FileExists(cfgPath$ + "FOLDERS.CFG") Then

    ' Create FOLDERS.CFG file
    Open cfgPath$ + "FOLDERS.CFG" For Output As #2

    For I = 0 To 3
        Print #2, Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    Close #2

End If

'Read in path values
Open cfgPath$ + "FOLDERS.CFG" For Input As #3

For I = 0 To 3
    Input #3, diskPaths$(I)
Next

Close #3

'If _FileExists(cfgPath$ + "errlog") Then
'    Kill cfgPath$ + "errlog"
'End If

'Run main menu
SELECTION% = 1

menu& = _NewImage(640, 480, 256)
Screen menu&

Do

    Call Main_Menu(SELECTION%)

Loop Until SELECTION% = 7

System


'----------------------------------------

Errhandler:

Open cfgPath$ + "errlog" For Append As #10
Print #10, "An error occurred. Please contact support with the following information:"
Print #10, "Error #"; Err; "on program file line"; _ErrorLine

If _InclErrorLine Then
    Print #10, "    in module "; _InclErrorFile$; " line "; _InclErrorLine
End If

If _FileExists(cfgPath$ + "GAME82") Then
    Print #10, "Schedule game # "; Using "####"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If

Print #10, "K1(I, 1) = "; test1
Print #10, "K1(I, 2) = "; test2
Print #10,
Close #10
Resume Next ' moves program to code following the error.


'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection)

    Do

        _Title "3-in-1 Football - Menu"

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "PLAY SINGLE GAME       "
        menuChoices$(2) = "TEAM MANAGEMENT        "
        menuChoices$(3) = "REPLAY ASSESSMENT      "
        menuChoices$(4) = "LEADERS AND RECORDS    "
        menuChoices$(5) = "SCHEDULES AND BOXSCORES"
        'menuChoices$(6) = "HARD DRIVE INSTALLATION  "
        menuChoices$(6) = "CONFIGURE GAME FOLDERS "
        menuChoices$(7) = "EXIT PROGRAM           "

        menuSelection = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection

                Case 1:
                    If schedGame(0) + schedGame(2) <= schedGame(1) Then
                        activeSchedule = 1
                    Else
                        activeSchedule = 0
                    End If

                    Do
                        Call FOOTBALL
                    Loop Until activeSchedule = 0 Or earlyExit = 1

                Case 2:
                    Call TeamMgt_Menu
                    'Call CountGames

                Case 3:
                    Call Replay_Menu

                Case 4:
                    Call Leaders_Menu

                Case 5:
                    Call SCHEDULE

                Case 6:
                    Call ConfigureDrives

            End Select

        End If

    Loop Until menuSelection = 7 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection = 7

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing a team and
'it's roster.
Sub TeamMgt_Menu ()

    Do

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "INPUT A NEW TEAM          "
        menuChoices$(2) = "EDIT AN EXISTING TEAM     "
        menuChoices$(3) = "PRINT TEAM ROSTER         "
        menuChoices$(4) = "VIEW TEAM ROSTER          "
        menuChoices$(5) = "DRAFT PLAYERS             "
        menuChoices$(6) = "TRADE PLAYERS             "
        menuChoices$(7) = "RETURN TO MAIN MENU       "

        menuSelection = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection

                Case 1:
                    Call InputTeam

                Case 2:
                    Call CNGTM(1)

                Case 3:
                    Call CNGTM(2)

                Case 4:
                    Call CNGTM(3)

                Case 5:
                    Call DRAFT

                Case 6:
                    Call TRADE

            End Select

        End If

    Loop Until menuSelection = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing replay
'data, from statistics to reports and
'team careers
Sub Replay_Menu ()

    Do

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 5)

        menuChoices$(1) = "REVIEW COMPILED STATS           "
        'menuChoices$(2) = "COMPARE REPLAY STATS TO ACTUAL  "
        menuChoices$(2) = "MODIFY REPLAY STATS             "
        menuChoices$(3) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(4) = "TEAM CAREER OPTIONS             "
        menuChoices$(5) = "RETURN TO MAIN MENU             "

        menuSelection = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection

                Case 1:
                    Call LOOKY

                Case 2:
                    Call EditStats

                Case 3
                    Call MERGE

                Case 4:
                    Call CAREER

                Case 5:
                    '

            End Select

        End If

    Loop Until menuSelection = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'This subroutine provodes the user with
'several options for managing team
'leaders, stats and standings
Sub Leaders_Menu ()

    Do

        Call TITLE

        ReDim menuChoices$(1 To 21)

        menuChoices$(1) = "MANAGE LEAGUES         "
        menuChoices$(2) = "UPDATE STATS/ STANDINGS"
        menuChoices$(3) = "ACCESS LEAGUE REPORTS  "
        menuChoices$(4) = "HEAD-TO-HEAD RESULTS   "
        menuChoices$(5) = "LEAGUE RECORDS         "
        menuChoices$(6) = "EXPANDED LEAGUE LEADERS"
        menuChoices$(7) = "DELETE LEAGUE STATS    "

        menuChoices$(8) = "MANAGE CONFERENCES     "
        menuChoices$(9) = "UPDATE STATS/ STANDINGS"
        menuChoices$(10) = "ACCESS CONF. REPORTS   "
        menuChoices$(11) = "HEAD-TO-HEAD STATS     "
        menuChoices$(12) = "CONFERENCE RECORDS     "
        menuChoices$(13) = "EXPANDED CONF. LEADERS "
        menuChoices$(14) = "DELETE CONFERENCE STATS"

        menuChoices$(15) = "MANAGE ORGANIZATIONS   "
        menuChoices$(16) = "UPDATE POLL / STATS    "
        menuChoices$(17) = "ACCESS NAT'L. REPORTS  "
        menuChoices$(18) = "NATIONAL RECORDS       "
        menuChoices$(19) = "DELETE NATIONAL STATS  "
        menuChoices$(20) = "RETURN TO MAIN MENU    "
        menuChoices$(21) = "EXIT PROGRAM           "

        menuSelection = 1
        Count% = 21
        Start% = 1
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 7
        Columns% = 3
        Spacing% = 3
        Row% = 9
        Column% = 2

        Call box0(7, 1, 16, 25, 2, 15, 1)
        Call box0(7, 27, 16, 51, 2, 15, 1)
        Call box0(7, 53, 16, 77, 2, 15, 1)

        Color 15, 0
        Locate 8, 4: Print "PROFESSIONAL LEAGUE"
        Locate 8, 30: Print "COLLEGE CONFERENCE"
        Locate 8, 55: Print "NATIONAL ORGANIZATION"

        Locate 18
        Print "PROFESSIONAL LEAGUE IS USED TO DEFINE A GROUP OF PRO TEAMS AND DIVISIONS"
        Print
        Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A SINGLE CONFERENCE"
        Print
        Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE COLLEGE CONFERENCES TO"
        Print "TRACK AND UPDATE TO GENERATE A NATIONAL POLL AND LEADERS."

        _MouseShow "DEFAULT"
        MAMenu menuChoices$(), menuSelection, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% = 27 Then Exit Sub

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection

                Case 1: Call CREATLEAG
                Case 2: Call CompileLeague
                Case 3: Call SEELEAG
                Case 4: Call HD2HDLEAG
                Case 5: Call Records_Menu(1)
                Case 6: Call NEWLDR(1)
                Case 7: Call BLANKPRO
                Case 8: Call CREATCON
                Case 9: Call CompileCollege
                Case 10: Call SEECON
                Case 11: Call HD2HDCON
                Case 12: Call Records_Menu(2)
                Case 13: Call NEWLDR(0)
                Case 14: Call BLANKCON
                Case 15: Call CREATENATL
                Case 16: Call CompileNatlOrg
                Case 17: Call SEENATL
                Case 18: Call Records_Menu(3)
                Case 19: Call BLANKORG

            End Select

        End If

    Loop Until menuSelection >= 20 Or ExitCode% = 27

    If menuSelection = 21 Then
        System
    End If

End Sub


' General purpose
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'StringManip.bm'
'$Include: 'QBWRITE.bm'

' For Utils
'$Include: 'BoxRoutines.bm'
'$Include: 'ConfigureDrives.bm'
'$Include: 'FileTeamSelection.bm'
'$Include: 'TITLE.bm'
'$Include: 'ALIGN.BAS'
'$Include: 'BLANKCON.BAS'
'$Include: 'BLANKORG.BAS'
'$Include: 'BLANKPRO.BAS'
'$Include: 'CAREDIT.BAS'
'$Include: 'CAREER.BAS'
'$Include: 'CARLDR.BAS'
'$Include: 'CARVIEW.BAS'
'$Include: 'CNGTM.BAS'
'$Include: 'COMPCON.BAS'
'$Include: 'COMPLEAG.BAS'
'$Include: 'COMPNATL.BAS'
'$Include: 'CONVERT.BAS'
'$Include: 'CREATCON.BAS'
'$Include: 'CREATNATL.BAS'
'$Include: 'CREATLEAG.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'HD2HDCON.BAS'
'$Include: 'HD2HDLEAG.BAS'
'$Include: 'LOOKY.BAS'
'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'$Include: 'NEWLDRST.BAS'
'$Include: 'REC.BAS'
'$Include: 'SCHEDULE.BAS'
'$Include: 'SEECON.BAS'
'$Include: 'SEELEAG.BAS'
'$Include: 'SEENATL.BAS'
'$Include: 'SINPUT.BAS'
'$Include: 'TINPUT.BAS'
'$Include: 'TRADE.BAS'
'$Include: 'QBFunctions.bm'

' For Game
'INCLUDE: 'FOOTBALL.BAS'


'----------------------------------------
'           FOOTBALL Subroutine
'----------------------------------------
'This subroutine serves as the main "source"
'for the action in the game. It steps the
'user through choosing game parameters and
'teams, as well as determining weather and
'other game time settings.
Sub FOOTBALL ()

    Shared actualAttendance&, avgAttendance&

    _Title "3-in-1 Football v" + GAME_VERSION$ + " - GAME TIME!"

    Shared scheduleFile$
    Shared tickerStart

    Shared location$(), overtime$()
    Shared playMode$()
    Shared rulesType$(), ruleYrPro$(), ruleYrColl$()
    Shared yesNoText$()

    Color 7, 0
    Cls

    Randomize Timer

    '----------------------------------------
    '        INITIALIZE ALL VARIABLES
    '----------------------------------------
    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------
    If Not _FileExists(cfgPath$ + "DEFAULT") Then

        Open cfgPath$ + "DEFAULT" For Output As #3

        'Player option
        Print #3, 3

        'Rules Type
        Print #3, 1

        'Home Field
        Print #3, 1

        'PBP Delay
        Print #3, 1.2

        'Stop clock on 1st down?
        Print #3, 0

        'Pro Rules Options
        Print #3, 9

        'College Rules Options
        Print #3, 11

        'Overtime setting
        Print #3, 0

        'Sound option
        Print #3, 1

        Close #3

    End If

    Open cfgPath$ + "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, ruleOptType
    Input #4, gameLoc ' Home Field Advantage
    Input #4, pbpDelay!
    Input #4, firstDownClockOpt
    Input #4, ruleOptPro
    Input #4, ruleOptColl
    Input #4, overtimeOpt 'Overtime setting
    If Not EOF(4) Then
        Input #4, sndOpt
    End If
    Close #4

    If _FileExists(cfgPath$ + "TICKER") Then

        Open cfgPath$ + "TICKER" For Input As #5

        For I = 1 To 32:
            For I1 = 0 To 1:
                For I2 = 1 To 14
                    Input #5, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32
            For I1 = 0 To 1
                Input #5, SX$(I, I1)
            Next
        Next

        For I = 1 To 32
            Input #5, ST%(I)
        Next

        Close #5

        tickerGames = 1

    Else

        tickerGames = 0

    End If

    'Load game in-progress, if there is one
    If _FileExists(cfgPath$ + "GAME82") Then

        Open cfgPath$ + "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        'Open cfgPath$ + "pbplog" + Str$(currGame) For Append As #11
        Open cfgPath$ + "pbplog" For Append As #11

        If Not _FileExists(scheduleFile$) Then

            'Notify user that game won't execute
            MsgText$ = "CAN'T FIND OR LOAD TARGET SCHEDULE.  PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        Else

            'Read in schedule options
            Open scheduleFile$ For Random As #7 Len = SCHEDULE_SIZE_BYTES

            For X = 0 To 20
                Field #7, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
            Next

            Field #7, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

            Get #7, schedGame(0) + schedGame(2)

            For X = 0 To 20:
                schedOptions(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 1:
                diskIDs$(X) = Q$(22 + X)
            Next

            Close #7

            autoPlay = 1

        End If

    Else

        Open cfgPath$ + "pbplog" For Append As #11

    End If


    If autoPlay = 1 Then

        Call AUTOSETUP(goodTeam)

        If goodTeam = 0 Then

            'Notify user that game won't execute
            MsgText$ = "ATTEMPTED TO LOAD AN INVALID TEAM. PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        End If

    Else

        goalPostAdj = 0

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 3, 2: Color 14
            Print "(1) ";
            Color 15: Print playMode$(playerOpt)
            Locate 4, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 2), teamRat_GAME(0, 1): Print gameTeams$(0)
            Locate 5, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 2), teamRat_GAME(1, 1): Print gameTeams$(1)
            Locate 6, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 7, 2: Color 14
            Print "(5) ";
            Color 15: Print "TYPE OF GAME: "; rulesType$(ruleOptType)
            Locate 8, 2: Color 14
            Print "(6) ";
            Color 15: Print "PRO RULES: "; ruleYrPro$(ruleOptPro)
            Locate 9, 2: Color 14
            Print "(7) ";
            Color 15: Print "COLLEGE RULES: "; ruleYrColl$(ruleOptColl)
            Locate 10, 2: Color 14
            Print "(8) ";
            Color 15: Print "SUDDEN DEATH: "; overtime$(overtimeOpt)
            Locate 11, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 12, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS"
            Locate 13, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!
            Locate 14, 2: Color 14
            Print "(B) ";
            Color 15: Print "SOUND ENABLED: "; yesNoText$(sndOpt)

            Locate 16, 2: Color 14
            Print "NOTE:: "
            Color 15

            Locate , 2: Print "*NCAA*  1968+, CLOCK STOPPED AFTER A 1ST DOWN"
            Locate , 3: Print "        PRE-1984, PASS INTERFERENCE SPOTTED AT INFRACTION"
            Locate , 3: Print "        1992+, ADVANCING FUMBLE ALLOWED"
            Locate , 3: Print "        HOLDING: PRE-1984, 15 YDS; 1984+ 10 YDS @ PT OF INFRACTION"
            Locate , 2: Print "*PRO*   PRE-1974, GOAL POSTS ON GOAL, BALL ON 20 AFTER MISS FG"
            Locate , 3: Print "        1994+, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"
            Locate , 3: Print "        2017+, 10 MINUTE OT, 2 TOs, EACH TEAM POSSESS + FIRST SCORE WINS"
            Locate , 3: Print "        HOLDING: PRE-1974, 15 YDS; 1974-77, 10 YDS RUN / 15 YDS PASS;"
            Locate , 3: Print "                  1978+, 10 YDS ON ALL";
            Locate , 3: Print "        KO: PRE-1974 @ 40 YL; 1974-1993 @ 35 YL";
            Locate , 3: Print "            1994-2010 @ 30 YL; 2011+ @ 35 YL"

            Do

                Color 7

                I$ = GetKeyPress$

                Call LetterToNumber(I$, setting)

            Loop Until setting >= 0 And setting <= 11 Or I$ = Chr$(27) Or UCase$(I$) = Chr$(17) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" And UCase$(I$) <> Chr$(17) Then

                Select Case setting

                    Case 0:
                        Open cfgPath$ + "DEFAULT" For Output As #8
                        Print #8, playerOpt
                        Print #8, ruleOptType
                        Print #8, gameLoc ' Home Field advantage
                        Print #8, pbpDelay!
                        Print #8, firstDownClockOpt
                        Print #8, ruleOptPro
                        Print #8, ruleOptColl
                        Print #8, overtimeOpt
                        Print #8, sndOpt
                        Close #8

                    Case 1
                        playerOpt = playerOpt + 1
                        If playerOpt > 3 Then playerOpt = 0

                    Case 2:
                        Call ChooseGameTeam(diskIDs$(0), NTMS, teamNbr, gameTeams$(0), 0)
                        teamIdx_GAME(0) = teamNbr

                    Case 3:
                        Call ChooseGameTeam(diskIDs$(1), NTMS, teamNbr, gameTeams$(1), 1)
                        teamIdx_GAME(1) = teamNbr

                    Case 4:
                        gameLoc = gameLoc + 1
                        If gameLoc = 3 Then gameLoc = 1

                    Case 5:
                        ruleOptType = ruleOptType + 1
                        If ruleOptType = 5 Then ruleOptType = 0

                    Case 6:
                        ruleOptPro = ruleOptPro + 1
                        If ruleOptPro > 11 Then ruleOptPro = 0

                    Case 7
                        ruleOptColl = ruleOptColl + 1
                        If ruleOptColl > 11 Then ruleOptColl = 0

                    Case 8:
                        overtimeOpt = overtimeOpt + 1
                        If overtimeOpt > 8 Then overtimeOpt = 0

                    Case 9:
                        '

                    Case 10
                        Do
                            Locate 13, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

                    Case 11
                        sndOpt = 1 - sndOpt

                End Select

                If setting >= 4 Then

                    goalPostAdj = 0
                    firstDownClockOpt = 0

                    If ruleOptType > 0 And ruleOptPro <= 1 Then
                        'Pro, Pre-1974
                        goalPostAdj = -10
                    End If

                    If ruleOptType = 4 Then
                        'WFL
                        goalPostAdj = 0
                    End If

                    If ruleOptType = 0 And ruleOptColl = 0 Then
                        'College, Pre-1868
                        firstDownClockOpt = 1
                    End If

                End If

            End If

        Loop Until (setting = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or I$ = Chr$(17) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> Chr$(17) And UCase$(I$) <> "Q" Then

            'TICKER PROMPT
            Cls
            Color 3

            Call TEAMBOX(7, 1)

            Locate 7, 11

            Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
            Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
            Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER"
            Print
            Locate , 11: Input "START TIME: "; startTime$

            L = Len(startTime$)

            If L < 4 Then
                startTime$ = "1:00P"
            Else
                If L = 5 Then HR = Val(Left$(startTime$, 1)): MIN = Val(Mid$(startTime$, 3, 2))
                If L = 6 Then HR = Val(Left$(startTime$, 2)): MIN = Val(Mid$(startTime$, 4, 2))
            End If

            tickerMeridian$ = Right$(startTime$, 1)

            If UCase$(tickerMeridian$) = "P" And Left$(startTime$, 2) <> "12" Then HR = HR + 12

            TM = HR * 60 + MIN
            tickerStart = Int(TM / 30) - 18

        Else

            earlyExit = 1

        End If

    End If


    'If we've run into issues so far...
    If earlyExit = 1 Then
        Close #11
        Exit Sub
    End If


    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case playerOpt

        Case 1:
            playerMode = 1
            compTeam = 0

        Case 2:
            playerMode = 1
            compTeam = 1

        Case 3:
            playerMode = 2

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    For I = 0 To 1:
        'If disk ID has alpha-char, or is only 3 characters,
        'then assume the year is the first 2 chars
        If CheckString%(diskIDs$(I)) = 1 Or Len(diskIDs$(I)) <= 3 Then
            teamYears(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
        Else
            'If we have 4 or more characters,
            'then Val will return the first 4 digits
            teamYears(I) = Val(RTrim$(diskIDs$(I)))
        End If

    Next I

    'If (leagRat_GAME(P1, 7)/100 > 65)
    If teamYears(impactTeam) <= 1956 Then
        If teamYears(impactTeam) <= 1944 Then
            'Before 1944
            rushEra = 1
        Else
            '1945 - 1956
            rushEra = 2
        End If

    Else
        '1956+
        rushEra = 0
    End If

    If rushEra > 0 Then
        Call GetDefensiveEra(defenseEra)
    End If

    'Populate menu choices for defense formation
    'This is based on eras before and after
    'passing was more common
    Select Case rushEra

        Case 0
            'Modern era defense
            If ruleOptType > 0 Then

                'Pro game: change from 5-2 to 3-4
                For I = 1 To 10

                    'Update def. formations
                    If Left$(defFormation$(I), 3) = "5-2" Then
                        defFormation$(I) = "3-4" + Right$(defFormation$(I), Len(defFormation$(I)) - 3)
                    End If

                Next

            End If

            For J1 = 1 To 10:
                defPlay$(J1) = defFormation$(J1)
            Next

        Case 1
            'Heavy rushing defense
            For J1 = 1 To 10:
                defPlay$(J1) = defFormEraA$(J1)
            Next

        Case 2
            'Heavy rushing defense
            If defenseEra = 3 And ruleOptType > 0 Then

                'Pro teams would likely still run a 7-2
                'Efficiency is close to 6-2 so here we
                'are just changing the name
                For I = 1 To 10

                    'Update def. formations
                    If Left$(defFormEraB$(I), 3) = "6-2" Then
                        defFormEraB$(I) = "7-2" + Right$(defFormEraB$(I), Len(defFormEraB$(I)) - 3)
                    End If

                Next

            End If

            For J1 = 1 To 10:
                defPlay$(J1) = defFormEraB$(J1)
            Next

    End Select


    'Determine attendance
    ' ** In the future, we could break this up
    ' ** by averages over each decade, based on
    ' ** available data (specifics not often recorded)
    If gameLoc = 2 Then

        'Neutral stadium and attendance
        gameStadium$ = "NEUTRAL FIELD"

        'These averages should be pretty accurate for
        'at least years 1980 +
        If ruleOptType = 0 Then
            'Random # 5,000 - 35,000 for College
            actualAttendance& = Int(Rnd(1) * 30000) + 5000
        Else
            'Random # 50,000 - 80,000 for Pro
            actualAttendance& = Int(Rnd(1) * 30000) + 50000
        End If

    Else

        If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + diskIDs$(gameLoc)) Then

            'Generic stadium and attendance
            gameStadium$ = "MEMORIAL STADIUM"

            If ruleOptType = 0 Then
                'College: assume medium-sized stadium
                estimatedCapacity& = 50000
                'Random attendance: 60% to 95% of capacity
                fillRate = 0.60 + (Rnd(1) * 0.35)
            Else
                'Pro: assume larger stadium
                estimatedCapacity& = 65000
                'Random attendance: 75% to 98% of capacity
                fillRate = 0.75 + (Rnd(1) * 0.23)
            End If
            
            actualAttendance& = Int(estimatedCapacity& * fillRate)

        Else

            Open diskPaths$(0) + DATA_FILE_NAME$ + "." + diskIDs$(gameLoc) For Random As #9 Len = DATA_SIZE_BYTES

            Field #9, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)

            Get #9, teamIdx_GAME(gameLoc)

            gameStadium$ = RTrim$(Q$(1))
            avgAttendance& = CVL(Q$(3))
            Z$ = Q$(4)

            Close #9

            'Determine attendance randomly
            If Z$ <> "x" Or avgAttendance& = 0 Then

                If Z$ <> "x" Then
                    'Generic stadium and attendance
                    gameStadium$ = "MEMORIAL STADIUM"
                End If

                If ruleOptType = 0 Then
                    'College: assume medium-sized stadium
                    estimatedCapacity& = 50000
                    'Random attendance: 60% to 95% of capacity
                    fillRate = 0.60 + (Rnd(1) * 0.35)
                Else
                    'Pro: assume larger stadium
                    estimatedCapacity& = 65000
                    'Random attendance: 75% to 98% of capacity
                    fillRate = 0.75 + (Rnd(1) * 0.23)
                End If
                
                actualAttendance& = Int(estimatedCapacity& * fillRate)

            Else
                'Average attendance +/ 5%
                If ruleOptType = 0 Then
                    'College attendance ranges 75% to 100% of max capacity
                    margin = avgAttendance& * .12
                Else
                    'Pro attendance ranges 90% to 100% of max capacity
                    margin = avgAttendance& * .05
                End If
                actualAttendance& = avgAttendance& + Int(Rnd(1) * margin * 2) - margin
            End If

        End If

    End If

    goalPostAdj = 0
    firstDownClockOpt = 0

    If teamRat_GAME(0, 2) = teamRat_GAME(1, 2) And teamRat_GAME(0, 1) = teamRat_GAME(1, 1) Then
        Call ColorChange
    End If

    If ruleOptType > 0 And ruleOptPro <= 1 Then
        'Pro, Pre-1974
        goalPostAdj = -10
    End If

    If ruleOptType = 4 Then
        'WFL
        goalPostAdj = 0
    End If

    If ruleOptType = 0 And ruleOptColl = 0 Then
        'College, Pre-1868
        firstDownClockOpt = 1
    End If

    Color 15, 0

    'Adjust Run/Pass distribution
    For team = 0 To 1

        'Run more often in inclement weather
        If climate(4) = -1 Then
            '5%-10%
            leagRat_GAME(team, 7) = leagRat_GAME(team, 7) + 5
        End If

        If climate(4) = -2 Then
            '10%-20%
            leagRat_GAME(team, 7) = leagRat_GAME(team, 7) + 10
        End If

    Next team

    Call NAMEEDIT

    For currTeam = 0 To 1

        If autoPlay = 1 Then
            'Autoamtic determine QB
            Erase activeKicker

            qbNumber(0) = schedOptions(5)

            If qbNumber(0) = 4 Then
                useRandomQB(0) = 1
                qbNumber(0) = 0
            Else
                qbMobility(0) = schedOptions(6)
            End If

            qbNumber(1) = schedOptions(8)

            If qbNumber(1) = 4 Then
                useRandomQB(1) = 1
                qbNumber(1) = 0
            Else
                qbMobility(1) = schedOptions(9)
            End If

            For I = 0 To 3
                qbRushIdx(currTeam, I) = -1
            Next I

            If useRandomQB(currTeam) = 0 Then

                'Store the index (from running backs)
                'of each QB that has rushing contributions
                'We leave out the current QB because this
                'is only useded to check that the QB doesn't
                'handoff to another QB
                For currBack = 0 To 9:
                    For currQB = 0 To 3
                        If rbacks$(currTeam, currBack) = qbacks$(currTeam, currQB) And currQB <> qbNumber(currTeam) Then
                            player$ = rbacks$(currTeam, currBack)
                            qbRushIdx(currTeam, currQB) = currBack
                        End If
                    Next currQB
                Next currBack

            Else

                'We need to assign a mobility rating
                'based on the QB contribution.
                'We'll start with the starter.
                qbContrib = GetQBRushPctGame%(qbacks$(currTeam, qbNumber(currTeam)), currTeam)
                qbMobility(currTeam) = GetQBMobility%(qbContrib)

            End If

        Else

            'Manual prompt for QB
            '-- we could probably update the routine
            '-- to handle autoPlay = 1 by itself
            Call CHOOSEQB(currTeam)

        End If

    Next currTeam

    'Set weather for non-dome stadium
    If teamRat_GAME(gameLoc, 3) <> 1 Then
        Call SETWEATHER
    End If

    Call SCOUT

    quarter = 1
    gameClock! = 900

    Call HALFSTART

    Call SETSCREEN

    '--- JUMP TO OT FOR TESTING
    'quarter = 5
    'GoTo 3364

    If autoPlay <> 1 And playerMode <> 2 Then
        whistle& = _SndOpen("whistle.mp3")
    End If

    '----------------------------------------
    '           START OF GAME
    '----------------------------------------
    Call CoinToss

    '-- Start of a "kickoff" routine

    ' ** Kickoff Choices **
    330 '
    If ruleOptType > 0 And ruleOptPro < 8 Then
        'Pro rules before 2008
        'Not able to defer
        Call KickoffChoices(0)
    Else
        If quarter = 1 Then
            Call KickoffChoices(1)
        Else
            Call KickoffChoices(0)
        End If
    End If

    ' ** Ready for Kickoff **
    340 '
    Call SetChainMarker

    'Update scoreboard with kicking team
    Call StatusBoard

    'New possession
    U3 = 0

    scoreX0 = 0

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    'Determine 1st returner
    For I = 0 To 2
        If krNumRet(D, I) = 100 Then
            KN = I
            Exit For
        End If
    Next

    'Find 2nd returner by contribution
    Call Rolld100(krChance)

    For I = 2 To 0 Step -1
        If krChance <= krNumRet(D, I) Then
            I1 = I
        End If
    Next I

    Call WINDEX
    Call CLEARPBP

    Select Case KN

        Case 0
            pbpString$ = kickRet$(D, I1) + " (" + LTrim$(Str$(Int(krYdsPerRet(D, I1) * 3))) + " AVG) IS DEEP TO RECEIVE THE KICK"
            Call PBP

        Case 1
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

        Case 2
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

    End Select

    If KN > 0 Then
        If d100% <= 50 Then
            pbpString$ = kickRet$(D, I1) + " (" + LTrim$(Str$(Int(krYdsPerRet(D, I1) * 3))) + " AVG) AND " + kickRet$(D, KN2) + " (" + LTrim$(Str$(Int(krYdsPerRet(D, KN2) * 3))) + " AVG) ARE DEEP"
        Else
            pbpString$ = kickRet$(D, KN2) + " (" + LTrim$(Str$(Int(krYdsPerRet(D, KN2) * 3))) + " AVG) AND " + kickRet$(D, I1) + " (" + LTrim$(Str$(Int(krYdsPerRet(D, I1) * 3))) + " AVG) ARE DEEP"
        End If
    End If

    Call PBP

    'After a TD is scored
    'Change possession back to where it should be
    'P1 = team that kicks off
    'D = team that receives

    'If human scores, P1 will be compTeam
    If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

        'Computer Choice
        kickChoice = 0

        '4th Quarter, 3 Mins Left, Team is behind?
        '-- we need better logic than that
        If gameClock! <= 180 And quarter = 4 And score(P1, 0) - score(D, 0) < 0 Then
            kickChoice = 2
        End If

    Else

        pbpString$ = "0-REGULAR  1-SQUIBB  2-ONSIDE KICK " + gameTeams$(P1)
        Call PBP

        Do
            I$ = GetKeyPress$
            kickChoice = Val(I$)

            If kickChoice = 2 And ruleOptType <> 0 And ruleOptPro >= 10 And quarter < 4 And score(P1, 0) > score(D1, 0) Then
                'Onside kick NOT ALLOWED
                'Pro Rules Year 2024+, Onside Kick before 4th Quarter
                pbpString$ = "CAN ONLY USE IN 4TH QUARTER IF YOU ARE BEHIND IN SCORE"
                Call PBP

                kickChoice = -1
            End If

        Loop Until kickChoice >= 0 And kickChoice <= 2

    End If

    notReturned = 0

    Select Case kickChoice

        Case 0
            'Regular Kickoff

            oobRoll = Int(Rnd(1) * 10000) + 1

            If oobRoll >= 998 And oobRoll <= 1000 Then

                'Kickoff out-of-bounds
                pbpString$ = "OH NO! THE KICK SAILS OUT-OF-BOUNDS!"
                Call PBP

                '-- add charge of a penalty to the team ?!?!
                '"Kickoff Out of Bounds"

                'Swap defense and offense
                Call CHANGEPOSS

                ydLine = 40
                ydsToScore = 100 - ydLine

            Else

                Call KickoffReturn(notReturned, penOnPlay)

                'Determine return yards based on returner's average
                If notReturned = 1 Then

                    If penOnPlay = 1 Then

                        totalSTPenalties = totalSTPenalties + 1

                        'Fair Catch Interference
                        pbpString$ = "*** PENALTY - " + gameTeams$(D) + " - FAIR CATCH INTERFERENCE ***"

                        If kickYL! <= 0 Then
                            'End Zone - penalty takes to the Touchback line
                            'This has already been done
                            'by the KickoffReturn routine
                            penaltyYds! = 0
                        Else
                            penaltyYds! = 15
                        End If

                        GoTo HANDLEPENALTY

                    End If

                Else

                    'Normal kick return
                    RN0 = Int(Rnd(1) * 1000) + 1

                    Select Case krYdsPerRet(P1, I1)

                        Case Is <= 8

                            Select Case RN0
                                Case 1 To 5
                                    'Kick return for TD - 0.5% chance
                                    ydsGained = 110

                                Case 6 To 20 '1.5% chance
                                    '45 to 65 yds + (-21 to to 3 yds); based on KR's avg - 7
                                    ydsGained = Int((Rnd(1) * 21) + 45) + (krYdsPerRet(P1, I1) - 7) * 3

                                Case 21 To 70 '5% chance
                                    '25 to 45 ys + (-21 to to 3 yds); based on KR's avg - 7
                                    ydsGained = Int((Rnd(1) * 21) + 25) + (krYdsPerRet(P1, I1) - 7) * 3

                                Case Is >= 71 '93% chance
                                    '15 to 25 yds  + (-21 to to 3 yds); based on KR's avg - 7
                                    ydsGained = Int((Rnd(1) * 10) + 15) + (krYdsPerRet(P1, I1) - 7) * 3

                            End Select

                        Case 9
                            If RN0 <= 12 Then
                                'Kick return for TD - 1.2% chance
                                ydsGained = 110
                            Else
                                '15 to 33 yds
                                ydsGained = Int((Rnd(1) * 19) + 15)
                            End If

                        Case Is >= 10
                            If RN0 <= 20 Then
                                'Kick return for TD - 2% chance
                                ydsGained = 110
                            Else
                                '15 to 33 yds + (3 yds / int value of return average > 10)
                                'So a returner with 12 yard average would be 21 to 39 yards
                                ydsGained = Int((Rnd(1) * 19) + 15) + (krYdsPerRet(P1, I1) - 10) * 3
                            End If

                    End Select

                    'Normal KR yardage
                    ydsToScore = 100 - ydLine
                    Call StatusBoard

                    startYds = 100 - ydLine
                    endYds = startYds - ydsGained
                    Call MOVEBALL

                    scoreX1 = 3
                    gameStatsPlayer(P1, I1, 14) = gameStatsPlayer(P1, I1, 14) + 1

                    If ydsGained + kickYL! > 89.99 Then

                        gameStatsPlayer(P1, I1, 15) = gameStatsPlayer(P1, I1, 15) + 100 - kickYL!
                        gameStatsPlayer(P1, I1, 16) = gameStatsPlayer(P1, I1, 16) + 1

                        If 100 - kickYL! > gameLongKRYds(P1, I1) Then
                            gameLongKRTD$(P1, I1) = "t"
                            gameLongKRYds(P1, I1) = 100 - kickYL!
                        End If

                        'Touchdown!
                        GoTo 3200

                    End If

                    returnYL! = ydsGained + kickYL!

                    If returnYL! <= -.05 Then

                        'Returner didn't make it out of the endzone
                        'P1 = Return Team
                        Call Touchback(1)

                    Else

                        ydLine = returnYL!

                        If ydsGained > gameLongKRYds(P1, I1) Then
                            gameLongKRTD$(P1, I1) = ""
                            gameLongKRYds(P1, I1) = ydsGained
                        End If

                        gameStatsPlayer(P1, I1, 15) = gameStatsPlayer(P1, I1, 15) + ydsGained

                        'Check for fumble - kick return
                        fumbPlayType = 3
                        Call CheckFumble_1
                        If ballFumbled = 1 Then
                            Call FumblePBP
                            GoTo 650
                        End If

                        Call AddPossessionTime(4, 7)

                        RN1 = Int(Rnd(1) * 1000) + 1

                        If RN1 >= KO_PENALTY_THRESHOLD - teamRat_GAME(P1, 0) Then
                            Call PenaltyOnKickoff(autoFirst)
                            GoTo HANDLEPENALTY
                        End If

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                    End If

                End If

            End If

        Case 1
            'Squib Kick
            Call SQUIB

            'Check for fumble - Squib
            Call CheckFumble_1
            If ballFumbled = 1 Then
                Call FumblePBP
                GoTo 650
            End If

            Call AddPossessionTime(4, 7)

            '3% chance of penalty on squib kick
            If d100% >= 97 Then
                Call PenaltyOnKickoff(autoFirst)
            End If

            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

        Case 2
            'Onside Kick
            Call ONSIDE

            currDown = 1
            ydsToFirst = 10

            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

    End Select

    GoTo 700

    '----------------------------------------
    '           END OF GAME!!!
    '----------------------------------------

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================


    '----- NOODLE -----

    650 '
    'Team Fumbles
    gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

    '50-60% chance of offense recovering
    offRecovChance = Int(Rnd(1) * 11) + 50

    If d100% <= offRecovChance Then

        pbpString$ = gameTeams$(P1) + " RECOVERS!!"
        Call PBP

        I = 50
        playSegment = 1

        GoTo 3303

    Else

        Call FumbleRecoverPBP

        pbpString$ = pbpString$ + gameTeams$(D) + " RECOVERS!!"
        Call PBP

        'Team Fumbles Lost
        gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1

        If isOT = 1 And overtimeOpt >= 3 Then
            nbrPossOT(D) = 1
        End If

        currDown = 1
        ydsToFirst = 10
        ydLine = 100 - ydLine

        Call CHANGEPOSS
        Call SetChainMarker

        If ruleOptType > 0 Or ruleOptColl >= 7 Then
            'Pro Rules or College after 1991

            RN1 = Int(Rnd(1) * 3) + 1

            If RN1 > 2 Then
                '1% chance of a return with the fumble

                scoreX1 = 10

                '5% chance of return for TD
                'This is a bit higher than the 4.75% actual
                If d100% > 95 Then
                    fumbRetYards = 100
                Else
                    fumbRetYards = Int(Rnd(1) * 18)
                End If

                fumbRetYards = fumbRetYards + (Int(Rnd(1) * 11) - 5) / 10
                fumbRetYdLine! = CInt((ydLine + fumbRetYards) * 10) / 10

                startYds = 100 - ydLine
                endYds = 100 - fumbRetYdLine!
                Call MOVEBALL

                pbpString$ = "RETURNED FOR"

                If fumbRetYdLine! >= 99.99 Then

                    pbpString$ = pbpString$ + yardsToDecimalStr$(100 - ydLine) + " YDS "
                    Call PBP

                    'Touchdown!
                    GoTo 3200

                Else

                    'retYdLine < 99.99
                    If Int(fumbRetYards) = 0 Then
                        pbpString$ = "THEY'LL HAVE TO SORT THIS ONE OUT FROM THE PILE."
                    Else
                        pbpString$ = pbpString$ + yardsToDecimalStr$(fumbRetYards) + " YDS"
                    End If

                    Call PBP

                    If fumbRetYdLine! <= .01 Then
                        'Safety!
                        GoTo 3250
                    End If

                    ydLine = fumbRetYdLine!

                End If

            End If

        End If

        If ydLine >= 90 Then
            ydsToFirst = 100 - ydLine
        End If

    End If

    playSegment = 5

    GoSub 3900

    CheckPoss_OT:
    'Change of possession
    'College - Setup next possession
    'Pro - Evaluate score if other team has had the ball
    If isOT = 1 And (overtimeOpt >= 3 And overtimeOpt <= 5) Or (overtimeOpt >= 6 And nbrPossOT(D) = 1) Or (overtimeOpt = 8 And otTDs(D) = 1) Then
        GoTo 3364
    End If

    700 '

    Call SetChainMarker

    Call DISPWEATHER

    playSegment = 0
    ballFumbled = 0
    ydsGained = 0
    W5 = 0

    If U3 = 9 Then
        'Poss End / Kickoff
        U3 = 0
        Call CHANGEPOSS
        Call KickoffLine
        GoTo 340
    End If

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    'Track the type of play for the previous call.
    'This helps us with decision making later,
    'and happens of course before we choose our next play
    Call UpdatePlayHistory(playCall, ydsGained)

    Do

        Call StatusBoard
        Call DEFMENU(defenseEra)

        'Offense Play Selection
        '-- Default to user-select
        autoPlaySelect = 0

        Do

            validPlay = 1

            'If computer is on Offense
            If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

                userSelectBC = 0
                autoPlaySelect = 1

                'This chooses the call but not the intended player
                Call ComputerOffense

                chosenPlay = playCall

                'If a run play was not chosen, shift chosenPlay because
                'It should be a unique value over 14
                If playCall > 8 Then
                    chosenPlay = playCall + 6
                End If

            Else

                userSelectBC = 1
                autoPlaySelect = 0

                Do
                    3118 '
                    'Determine play call

                    Call FRAMEOPTION(10, P1)
                    Print "SELECT OFFENSE "; gameTeams$(P1)

                    For playCall = 0 To 4:
                        Locate 11 + playCall, 14
                        Print playCall; "- "; playSelect$(playCall); Tab(41); playCall + 5; "- "; playSelect$(playCall + 5)
                    Next playCall

                    Locate , 15
                    Print "G - GADGET PLAY"; Tab(42); "T - CALL TIMEOUT"

                    Do

                        I$ = GetKeyPress$

                        If I$ = "8" Then
                            'Ten minute ticker
                            I$ = "K"
                        End If

                        If UCase$(I$) = Chr$(17) Then
                            'System
                            Exit Sub
                        End If

                        ' *** Select Gadget Play ***
                        If UCase$(I$) = "G" Then

                            Call FRAMEOPTION(8, P1)
                            Print "SELECT GADGET PLAY "; gameTeams$(P1)

                            For G2 = 1 To 3:
                                Locate 11 + G2, 14
                                Print G2; "- "; gadget$(G2)
                            Next G2

                            Do
                                I$ = GetKeyPress$

                                If I$ <> Chr$(27) Then
                                    G2 = Val(I$)
                                End If

                            Loop Until G2 >= 1 And G2 <= 3 Or I$ = Chr$(27)

                            Screen , , 0, 0


                            If I$ = Chr$(27) Then
                                GoTo 3118
                            End If

                            Select Case G2

                                Case 1
                                    ' *** FLEA FLICKER ***
                                    Call FLEA(ffSuccess)

                                    If ffSuccess <= ffPctSuccess Then
                                        'Not able to get the pass off
                                        playType = 1
                                        W5 = 1
                                        ydsGained = (Int(Rnd(1) * 6) + 10)

                                        'Sacked
                                        GoTo 3541

                                    Else

                                        playType = 2

                                        If d100% <= intChance Then

                                            ydsGained = catchYds
                                            fleaFlicker = 2
                                            'Intercepted!
                                            GoTo 3552

                                        Else
                                            'Determine target WR for flea flicker
                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If d100% <= catchPctSuccess Then
                                                fleaFlicker = 2
                                                GoTo 3586
                                            Else
                                                ydsGained = 0
                                                GoTo 3535
                                            End If

                                        End If

                                    End If

                                Case 2
                                    ' *** REVERSE, REVERSE! ***
                                    isReverse = 1

                                    GoSub 3128

                                    Call QBLINE

                                    playType = 1

                                    If userSelectBC = 1 Then

                                        userSelectBC = 0

                                    Else

                                        3181 '
                                        'Random ball carrier
                                        Call Rolld100(carrContrib)

                                        For I = 9 To 0 Step -1:
                                            If carrContrib <= rbRushContrib(P1, I) Then
                                                I1 = I
                                            End If
                                        Next I

                                        '8335
                                        'If the ball carrier chosen is a benched QB
                                        If I1 = (qbRushIdx(P1, 0) Or I1 = qbRushIdx(P1, 1) Or I1 = qbRushIdx(P1, 2) Or I1 = qbRushIdx(P1, 3)) Then

                                            'Check if they have any receptions
                                            If rbNumRec(P1, I1) > 0 Then
                                                GoTo 3403

                                            Else
                                                'Check if active QB is a runner
                                                For currBack = 0 To 9

                                                    If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                                                        I1 = currBack
                                                        'Regular gain
                                                        GoTo 3403
                                                    End If

                                                Next

                                                If isReverse = 1 Then
                                                    'QB can't reverse to himself
                                                    'Choose another carrier
                                                    GoTo 3181
                                                Else
                                                    'Regular rush gain (can't do a reverse)
                                                    GoTo 3401
                                                End If

                                            End If

                                        End If

                                    End If

                                    '8336
                                    'Adjust yardage based on league rating and weather
                                    rushBase = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                                    If isReverse = 1 Then
                                        'Reverse play?
                                        Call REVGAIN
                                        GoTo 3416
                                    Else
                                        'Regular gain
                                        GoTo 3403
                                    End If

                                Case 3
                                    ' *** HB PASS ***
                                    halfbackPass = 1
                                    playType = 2

                                    Call FRAMEOPTION(11, P1)
                                    Print "SELECT PASSER":
                                    For KK = 0 To 4
                                        Locate 12 + KK, 14
                                        Print KK; "-"; rbacks$(P1, KK); Tab(41); KK + 5; "-"; rbacks$(P1, KK + 5)
                                    Next KK

                                    Do
                                        I$ = GetKeyPress$
                                        I1 = Val(I$)
                                    Loop Until I1 >= 0 And I1 <= 9

                                    Screen , , 0, 0

                                    catchPctSuccess = 5

                                    If (coverage = 1 Or coverage = 3) And hasRunRBPass(P9) = 0 Then
                                        'Increase success if they haven't tried
                                        'the play before
                                        catchPctSuccess = 30
                                    End If

                                    '5-10% chance of interception
                                    intChance = Int(Rnd(1) * 6) + 5
                                    ydsGained = Int(Rnd(1) * 71) + 30
                                    catchYds = Int(Rnd(1) * 11) + 30

                                    hasRunRBPass(P9) = 1

                                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND HANDS OFF TO " + rbacks$(P1, I1)
                                    Call PBP

                                    pbpString$ = rbacks$(P1, I1) + " HAS THE BALL AND WANTS TO THROW!!!"
                                    Call PBP

                                    I3 = Int(Rnd(1) * 3)
                                    pbpString$ = "HE HAS " + wdRec$(P1, I3) + " OPEN DOWNFIELD"
                                    Call PBP

                                    If coverage = 10 Or coverage = 11 Or coverage = 8 And d100% <= 35 Then
                                        ydsGained = (Int(Rnd(1) * 4) + 4)
                                        'Sacked
                                        GoTo 3541
                                    Else

                                        If d100% <= intChance Then
                                            'Interception
                                            ydsGained = catchYds
                                            fleaFlicker = 1
                                            halfbackPass = 0
                                            'Intercepted!
                                            GoTo 3552
                                        Else
                                            'Determine target WR for HB pass
                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If d100% <= catchPctSuccess Then
                                                fleaFlicker = 1
                                                GoTo 3586
                                            Else
                                                ydsGained = 0
                                                GoTo 3535
                                            End If
                                        End If
                                    End If

                            End Select

                        End If

                        If UCase$(I$) = "K" Then
                            GoTo 850
                        End If

                        If UCase$(I$) = "T" Then
                            Call TimeoutCalled(P1, periodOver)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360
                            I$ = "10"
                            'If periodOver = 1 Then GoTo 3360
                        End If

                        XX = Val(I$)

                    Loop Until XX >= 0 And XX <= 9

                    Screen , , 0, 0

                    ' *** Play Type Selection
                    Select Case XX

                        Case 0

                            Call RUNMENU

                            Do
                                Locate 20, 14
                                Print "             SELECT PLAY ";
                                Input playCall
                            Loop Until playCall >= 0 And playCall <= 15

                            If playCall = 0 Then
                                Screen , , 0, 0
                                GoTo 3118
                            End If

                            chosenPlay = playCall

                            'Here we are categorizing plays that are
                            'simply alternate names for the same thing

                            'Power Sweep / Quick Pitch = Toss Sweep
                            If playCall = 9 Or playCall = 10 Then playCall = 4

                            'Quick Trap = Trap / Misdirection
                            If playCall = 11 Then playCall = 7

                            'Isolation = Off Tackle
                            If playCall = 12 Then playCall = 3

                            'Sprint Draw = Draw
                            If playCall = 13 Then playCall = 6

                            'Over the Top = Trap / Misdirection
                            If playCall = 14 Then playCall = 7

                            'Counter = Dive
                            If playCall = 15 Then playCall = 2

                        Case 1

                            Call PASSMENU

                            Do
                                Locate 19, 14
                                Print "             SELECT PLAY ";
                                Input playCall
                            Loop Until playCall >= 0 And playCall <= 14

                            Screen , , 0, 0

                            If playCall = 0 Then GoTo 3118

                            'We have 15 run plays, so chosenPlay needs
                            'a unique value up to 29
                            chosenPlay = playCall + 15

                            'Here we are categorizing plays that are
                            'simply alternate names for the same thing

                            'Cross / Curl = Medium Pass
                            If playCall = 11 Or playCall = 12 Then playCall = 3

                            'Deep Post / Deep Corner = Long Pass
                            If playCall = 13 Or playCall = 14 Then playCall = 4

                            'playCall needs a unique value as well
                            'that represents a chosen play,
                            'not just a number on the menu
                            'So here we account for the
                            'presence of 8 run play types
                            playCall = playCall + 8

                        Case Is >= 2
                            'playCall needs a unique value; values of XX from 3+
                            'will translate to 20+
                            playCall = XX + 17

                    End Select

                Loop Until playCall <> 19 Or ydLine < 70

                If playCall = 26 Then playCall = 24

                'QB Sneak (playCall = 25)
                If XX = 8 Then

                    pbpString$ = gameTeams$(P1) + " IS LETTING THE PLAY CLOCK WIND DOWN"
                    Call PBP

                    pbpString$ = "PENALTY - " + gameTeams$(P1) + " - DELAY OF GAME"
                    Call PBP

                    playSegment = 8
                    penaltyYds! = 5
                    loseDown = 0

                    'Reset ball carrier
                    rushBase = 0

                    GoTo 3326

                End If

                If XX = 7 Then
                    playCall = 9
                    playType = 1
                    GoTo 3403
                End If

                If (quarter = 3 Or quarter = 1 Or gameClock! > 120) And playCall = 16 Then
                    pbpString$ = "CAN ONLY USE LAST 2 MIN. OF EACH HALF"
                    Call PBP
                    validPlay = 0
                End If

            End If

        Loop Until validPlay = 1

        'Determine intended ball carrier
        If autoPlaySelect = 0 Then

            If playCall = 8 Then
                Screen , , 0, 0
            End If

            If playCall >= 1 And playCall <= 8 Then

                3128 '

                Call FRAMEOPTION(11, P1)

                Print "SELECT BALL CARRIER":

                For KK = 0 To 4

                    Locate 11 + KK, 14
                    Print KK; "- "; rbacks$(P1, KK);

                    If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, KK, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, KK) / 100 + .1 Then
                        Print " ###"
                    Else
                        Print
                    End If

                    If rbRushContrib(P1, KK) = 100 Then GoTo CSELECT

                Next

                For KK = 0 To 4

                    Locate 11 + KK, 41
                    Print KK + 5; "- "; rbacks$(P1, KK + 5);

                    If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, KK + 5, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, KK + 5) / 100 + .1 Then
                        Print " ###"
                    Else
                        Print
                    End If

                    If rbRushContrib(P1, KK + 5) = 100 Then GoTo CSELECT

                Next KK

                CSELECT:
                Locate 17, 28: Print "C-COMPUTER SELECT"

                Do
                    I$ = GetKeyPress$

                    If UCase$(I$) = "C" Then
                        userSelectBC = 0
                    Else
                        userSelectBC = 1
                        I1 = Val(I$)
                    End If

                Loop Until I1 >= 0 And I1 <= 9 Or userSelectBC = 0

                Screen , , 0, 0

            End If

        End If

        ResolvePlay:
        'Computer Offense - Long Pass Inside Opp 15 --> Change to Medium Pass
        If ((playerMode = 1 And P1 = compTeam Or playerMode = 2) And _ 
            (playCall = 12 Or playCall = 15) And ydLine >= 85) Then
            playCall = 11
        End If

        confusion = 0

        'If there are timeouts and we're in the red zone, there's a 4% chance of confusion
        If timeouts(P1) > 0 And ydLine > 80 And d100% <= 4 Then

            pbpString$ = "THERE'S SOME CONFUSION AT THE LINE OF SCRIMMAGE"
            Call PBP

            pbpString$ = qbacks$(P1, qbNumber(P1)) + " HAS TO CALL TIME OUT TO AVOID DELAY OF GAME"
            Call PBP

            timeouts(P1) = timeouts(P1) - 1
            confusion = 1

        End If

    Loop While confusion = 1

    Call StatusBoard
    Call SetChainMarker

    If playerMode = 1 And D = compTeam Then
        Call COMPCHEAT(defenseEra)
    End If

    If qbTakeKnee = 1 Then
        qbTakeKnee = 0
        GoTo 3403
    End If

    ' *** PRE-SNAP PENALTIES ***
    penaltyOnPlay = 0

    If penaltyOnPlay = 1 Then
        
        Call PenaltyPreSnap(penaltyOnPlay)
        playSegment = 8
        GoTo HANDLEPENALTY

    End If

    'At this point the ball carrier has been chosen
    'Now we have to resolve the play outcome and yardage
    Select Case playCall

        Case 1 To 8
            ' ****  RUNNING PLAYS  ****
            Call WINDEX
            Call CLEARPBP

            playType = 1
            Call QBLINE

            'If user picked the ball carrier, work out yardage / fumbles
            If userSelectBC = 1 Then

                userSelectBC = 0

            Else

                3401 '
                'Pick random ball carrier based on contribution %
                Call Rolld100(carrContrib)

                For I = 9 To 0 Step -1:
                    If carrContrib <= rbRushContrib(P1, I) Then
                        I1 = I
                    End If
                Next I

                CheckQBRunner:
                'If the computer chose a QB for a run play,
                'we will instead go with a pass play.
                'The game handles all QB rushes as scrambles / etc.
                'Without this, rush attempts for a team run
                'way high and pass attempts run too low
                If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 And _ 
                    rbacks$(P1, I1) = qbacks$(P1, qbNumber(P1)) Then
                    'Basically we're picking a pass play
                    'Chances here are based on calculations of
                    'how often the AI would call these types
                    'of passes during the regular game.
                    'Currently, very little context is going into this.
                    Call Rolld100(RN0)

                    Select Case RN0

                        Case 0 To 12
                            playCall = 10

                        Case 13 To 25
                            playCall = 13

                        Case 26 To 85
                            playCall = 11

                        Case 86 To 100
                            playCall = 12

                    End Select

                    GoTo ResolvePlay

                End If

                'See if chosen runner is a benched QB
                If I1 = (qbRushIdx(P1, 0) Or I1 = qbRushIdx(P1, 1) Or I1 = qbRushIdx(P1, 2) Or I1 = qbRushIdx(P1, 3)) Then

                    'Check if runner has receptions
                    If rbNumRec(P1, I1) <= 0 Then

                        'Assign active QB instead
                        For currBack = 0 To 9
                            If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                                I1 = currBack
                                GoTo CheckQBRunner
                            End If
                        Next

                        If isReverse = 1 Then
                            'Reverse play?
                            GoTo 3181
                        Else
                            'Look for new ball carrier
                            GoTo 3401
                        End If

                    End If

                Else

                    'Adjust rush yds based on league rating and weather
                    rushBase = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                    If isReverse = 1 Then
                        'Reverse play?
                        Call REVGAIN
                        GoTo 3416
                    End If

                End If

            End If 'Done checking userSelectBC

            ' ** Normal run play **

            3403 '
            'Adjust yardage based on DEF league ratings and weather
            rushBase = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

            If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, I1, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, I1) / 100 + .1 Then
                'If team has more than 20 attempts and current runner's contribution > his average?
                rushBase = Int(rushBase * .4)
            End If

            RN0 = Int(Rnd(1) * 200) + 1

            'Fumble on snap - 0.5 % chance
            If RN0 = 97 And ydLine <= 98.899 And ydLine >= 1.101 Then

                pbpString$ = qbacks$(P1, qbNumber(P1)) + " FUMBLES THE SNAP!"
                Call PBP

                'impacts play by +/- 0.9 yds
                ydsGained = Int((Rnd(1) * 20) - 10) / 10

                '-- No fatigue yards for fumbled snap

                scoreX0 = 4
                scoreX1 = 9
                I1 = qbNumber(P1)

                If ydLine + ydsGained < 99.94999 Then
                    'If we're within a foot of the first down
                    If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then
                        Call MEASUREMENT
                    End If
                Else
                    'Touchdown is only worth yards left on the field
                    ydsGained = ydsToScore
                End If

                playYds! = CInt((ydsGained) * 10) / 10

                If Int(playYds!) = 0 Then
                    pbpString$ = "HE RECOVERS IT NEAR THE LINE OF SCRIMMAGE"
                Else
                    If playYds! < 0 Then
                        pbpString$ = "HE LOSES" + Str$(0 - Int(playYds!)) + " YARDS"
                    Else
                        pbpString$ = "HE GAINS" + Str$(Int(playYds!)) + " YARDS"
                    End If
                End If

                Call PBP

                'Play results - fumbled snap
                If endZone = 1 Then

                    Call TryForTwoResult
                    GoTo CheckPoss_2PtConv

                Else

                    ballFumbled = 1

                    Call COMPILER

                    ydLine = ydLine + ydsGained

                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1
                    If currDown = 3 Then
                        thirdDownAtt(P1) = thirdDownAtt(P1) + 1
                    End If

                    GoTo 650

                End If

            End If

            'QB Sneak
            If playCall = 8 Then

                Call QBSNEAK

                'Play results - QB sneak
                If endZone = 1 Then

                    Call TryForTwoResult
                    GoTo CheckPoss_2PtConv

                Else

                    playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                    If playYdLine! < .05 Then
                        'Safety!
                        GoTo 3250
                    End If

                    If playYdLine! >= 99.9499 Then
                        'Touchdown!
                        GoTo 3200
                    End If

                End If

                Call CheckFumble_1

                GoTo 3309

            End If

            'Take Knee
            If playCall = 9 Then

                pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND KNEELS DOWN FOR "
                Call PBP
                ydsGained = -1.0
                playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                qbTakeKnee = 0

                GoTo 3546

            End If

            'Option play = 2-3% cance of fumble on pitch
            'Overall, option plays have fumbles 3-5% of the time
            'If not fumbled here, it will be checked later on the run
            RN0 = Int(Rnd(1) * 1000) + 1

            If playCall = 5 And (RN0 >= 50 And RN0 <= 75) Then

                pbpString$ = qbacks$(P1, qbNumber(P1)) + " MAKES A BAD PITCH ON THE OPTION!!"
                Call PBP

                'Loss of 6 - 12 yds
                ydsGained = Int((Rnd(1) * 7) + 6) * -1

                '-- No fatigue yards for bad pitch

                scoreX0 = 4
                scoreX1 = 9
                I1 = qbNumber(P1)

                pbpString$ = "HE LOSES" + yardsToDecimalStr$(ydsGained * -1) + " YARDS"
                Call PBP

                returnYL! = CInt((ydsGained + ydLine) * 10) / 10

                If returnYL! < .05 Then

                    ballFumbled = 0

                    'Safety!
                    GoTo 3250

                Else

                    'Play results - fumbled pitch
                    If endZone = 1 Then

                        Call TryForTwoResult
                        GoTo CheckPoss_2PtConv

                    Else

                        ballFumbled = 1

                        Call COMPILER

                        ydLine = ydLine + ydsGained

                        ydsToFirst = ydsToFirst - ydsGained
                        currDown = currDown + 1
                        If currDown = 3 Then
                            thirdDownAtt(P1) = thirdDownAtt(P1) + 1
                        End If

                        GoTo 650

                    End If

                End If

            End If

            Call HANDOFF

            If dLine = 13 Then

                Call GOALLINE

            Else

                'Adjust rush yds for Def Formation
                Select Case rushEra

                    Case 0
                        rushBase = rushBase + defYdAdj(playCall, dLine)

                    Case 1
                        rushBase = rushBase + defYdAdjEraA(playCall, dLine)

                    Case 2
                        rushBase = rushBase + defYdAdjEraB(playCall, dLine)

                End Select

                overuseAdj = 0

                '  Rush Att + Pass Att
                If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then
                    'If rush % > team average
                    If (gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3))) > (leagRat_GAME(P1, 7) / 100) And _ 
                        K3(P1, 0) - (leagRat_GAME(D, 0) * .3) > 4.5 And _
                        (leagRat_GAME(D, 0) * .3) > 4.5 And _ 
                        leagRat_GAME(P1, 7) < 50 Then
                        overuseAdj = CInt(((K3(P1, 0) - 3.6) / .3)) * -1
                    End If
                End If

                rushBase = rushBase + defYdAdj(playCall, dLine)
                rushBase = rushBase + overuseAdj

                'Home Field Advantage - Rushing Yds
                If P1 = gameLoc Then
                    'Home: +0.3
                    rushBase = rushBase + 1
                Else
                    'Away (not Neutral): -0.3
                    If gameLoc <> 2 Then rushBase = rushBase - 1
                End If

                If rushBase < 1 Then rushBase = 1

                'Artificially adjust
                '--- rushBase = rushBase + 3

                'rushBase = 1 to 38
                If rushBase <= 38 Then

                    'If ((playCall <= 2 And dLine = 5 Or playCall <= 2 And dLine = 6) '
                    'Or (playCall >= 6 And dLine = 5 Or playCall >= 6 And dLine = 6)
                    'Or (playCall > 2 And playCall < 5 And dLine = 7)
                    'Or (playCall > 2 And playCall < 5 And dLine = 8)) Then
                    If (((playCall <= 2 Or playCall >= 6) And (dLine = 7 Or dLine = 8)) Or ((playCall = 3 Or playCall = 4) And (dLine = 9 Or dLine = 10))) Then

                        ' *** STUNTS ***
                        Call Rolld100(RN4)

                        Select Case rushBase

                            Case Is <= 15
                                Select Case RN4
                                    Case 1 To 15:
                                        ydsGained = Int(Rnd(1) * 3) - 2
                                    Case 16 To 60:
                                        ydsGained = Int(Rnd(1) * 2) + 1
                                    Case 61 To 100:
                                        ydsGained = Int(Rnd(1) * (rushBase * 2.13)) + 1
                                End Select

                            Case 16, 17
                                Select Case RN4
                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 3) - 2
                                    Case 11 To 50:
                                        ydsGained = Int(Rnd(1) * 2) + 1
                                    Case 51 To 100:
                                        ydsGained = Int(Rnd(1) * (rushBase * 1.73)) + 1
                                End Select

                            Case 18, 19
                                Select Case RN4
                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 3) - 2
                                    Case 11 To 40:
                                        ydsGained = Int(Rnd(1) * 2) + 1
                                    Case 41 To 100:
                                        ydsGained = Int(Rnd(1) * (rushBase * 1.5)) + 1
                                End Select

                            Case 20, 21
                                Select Case RN4
                                    Case 1 To 5:
                                        ydsGained = Int(Rnd(1) * 3) - 2
                                    Case 6 To 30:
                                        ydsGained = Int(Rnd(1) * 2) + 1
                                    Case 31 To 100:
                                        ydsGained = Int(Rnd(1) * (rushBase * 1.3)) + 1
                                End Select

                            Case Is >= 22
                                Select Case RN4
                                    Case 1 To 5:
                                        ydsGained = Int(Rnd(1) * 3) - 2
                                    Case 6 To 20:
                                        ydsGained = Int(Rnd(1) * 2) + 1
                                    Case 21 To 100:
                                        ydsGained = Int(Rnd(1) * (rushBase * 1.2)) + 1
                                End Select

                        End Select

                        GoTo 3416

                    End If

                    Call Rolld100(RN4)

                    If ((playCall <= 2 Or playCall >= 6) And RN4 <= 50 And (coverage = 7 Or coverage = 9)) Or ((playCall >= 3 And playCall <= 5) And (coverage = 8 And RN4 <= 50) Or coverage = 10) Or coverage = 11 Then

                        Call BLITZ2

                    Else

                        If rushBase > 17 Then

                            ydsGained = Int(Rnd(1) * 48) + 1

                            Select Case ydsGained

                                Case 1 To 44
                                    ydsGained = (Int(Rnd(1) * 11) - 1) - (((18 - rushBase) * 2) / 10)

                                Case 45
                                    ydsGained = Int(Rnd(1) * 2 * rushBase) + 1

                                Case 46 To 47
                                    ydsGained = Int(Rnd(1) * rushBase) + 1

                                Case 48
                                    ydsGained = Int(Rnd(1) * 5 * rushBase) + 1

                            End Select

                        Else

                            '10% of runs
                            baseBrkawayChance = 10

                            '-- FATIGUE NOT IMPLEMENTED
                            'Increase chance based on defensive fatigue
                            fatigueBonus = 0 'CInt((1.0 - fatigueLevel%) * chanceScale)

                            adjBrkawayChance = baseBrkawayChance + fatigueBonus

                            If d100% > (100 - Int(Rnd(1) * adjBrkawayChance)) Then

                                ' *** BREAKAWAY ***
                                If d100% <= 50 Then
                                    ydsGained = Int(Rnd(1) * brkawayYds(rushBase, 0)) + brkawayYds(rushBase, 1)
                                Else
                                    ydsGained = Int(Rnd(1) * brkawayYds(rushBase, 3)) + brkawayYds(rushBase, 4)
                                End If

                            Else

                                If playCall = 4 Or playCall = 5 Then
                                    'Sweep / Option
                                    I5 = 1
                                Else
                                    I5 = 0
                                End If

                                'This is a % chance of reducing
                                'the yardage ???
                                For I6 = 0 To 14:
                                    If d100% <= brkawayMod(I5, I6) Then
                                        ydsGained = I6 - 5
                                        Exit For
                                    End If
                                Next I6

                                'runYdAdj(0, X) = chance for smaller gain
                                If d100% <= runYdAdj(0, rushBase) Then
                                    'runYdAdj(1, X) = small gain amount
                                    ydsGained = ydsGained + runYdAdj(1, rushBase)
                                End If

                            End If

                        End If

                    End If 'Done checking coverage

                Else
                    'rushBase = 39 +
                    ydsGained = Int(Rnd(1) * 48) + 1

                    Select Case ydsGained

                        Case 1 To 44
                            ydsGained = (Int(Rnd(1) * 11) - 1) - (((18 - rushBase) * 2) / 10)

                        Case 45
                            ydsGained = Int(Rnd(1) * 2 * rushBase) + 1

                        Case 46 To 47
                            ydsGained = Int(Rnd(1) * rushBase) + 1

                        Case 48
                            ydsGained = Int(Rnd(1) * 5 * rushBase) + 1

                    End Select

                End If

                'End Select

            End If 'Done checking / adjusting for position

            3416 '
            'Determine play yardage
            Call YARDAGE

            '-- FATIGUE NOT IMPLEMENTED
            'Fatigue yards - run play (increase)
            If endZone = 1 Then
                'ydsGained = ydsGained * fatigueLevel%
            End If

            'Call PBP based on special scenarios
            '"Stuffed", Long Play or Run for TD
            If ydsGained <= 0 Then
                Call STUFFED
            End If

            If ydsGained > 10 And ydLine + ydsGained < 99.94999 Then
                Call LongGainPBP
            End If

            If ydLine + ydsGained >= 99.94999 Then
                Call RunForTD_PBP
            End If

            startYds = 100 - ydLine
            endYds = startYds - ydsGained
            Call MOVEBALL

            If ydLine + ydsGained < 99.94999 Then
                'Not a score

                'Check for fumble - run
                fumbPlayType = 0
                Call CheckFumble_1

                If ballFumbled = 0 Then
                    'If we're within a foot of the first down
                    If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then
                        Call MEASUREMENT
                    End If
                End If
            Else
                'Touchdown is only worth yards left on the field
                ydsGained = ydsToScore
            End If

            If Int(ydsGained) = 0 Then
                pbpString$ = "HE'LL GAIN JUST A LITTLE BIT"
            Else
                'Print / call PBP for actual yardage
                If ydsGained < 0 Then

                    pbpString$ = rbacks$(P1, I1) + " LOST"

                    scoreX0 = 1
                    scoreX1 = 1
                    'pbpString$ = pbpString$ + Str$(CInt((ydsGained * -1) * 10) / 10)
                    pbpString$ = pbpString$ + yardsToDecimalStr$(ydsGained * -1)

                Else

                    Call RUNPBP

                    scoreX0 = 1
                    scoreX1 = 1
                    newYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                    If newYdLine! >= 99.94999 Then
                        'pbpString$ = pbpString$ + Str$(CInt((100 - ydLine) * 10) / 10)
                        pbpString$ = pbpString$ + yardsToDecimalStr$(100 - ydLine)
                    Else
                        'pbpString$ = pbpString$ + Str$(CInt((ydsGained) * 10) / 10)
                        pbpString$ = pbpString$ + yardsToDecimalStr$(ydsGained)
                    End If

                End If

                pbpString$ = pbpString$ + " YARDS"

            End If

            Call PBP

            If isReverse = 1 Then
                isReverse = 0
            End If

            'Play results - run play
            If endZone = 1 Then

                ballFumbled = 0

                Call TryForTwoResult
                GoTo CheckPoss_2PtConv

            Else

                newYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                If newYdLine! < .05 Then
                    ballFumbled = 0
                    'Safety!
                    GoTo 3250
                End If

                If newYdLine! >= 99.94999 Then
                    ballFumbled = 0
                    'Touchdown!
                    GoTo 3200
                End If

            End If

            'Resolve play (penalty / fumble)
            GoTo 3309

        Case 9 To 18

            Call WINDEX
            Call CLEARPBP

            playType = 2
            Call QBLINE

            Call PASSBALL(playDir)

            Call Rolld100(sackRoll)
            sackChance = passCovAdj(3, playCall - 8, coverage) + leagRat_GAME(P1, 5) + leagRat_GAME(D, 6) + 1

            'SACKED
            'Sack chance (based on coverage) + sack ratings
            If sackRoll < sackChance Then

                ' *** SACKED ***
                'Treated as a run play for penalties
                playType = 1
                W5 = 1

                If playCall = 12 Or playCall = 15 Then
                    ydsGained = (Int(Rnd(1) * 9) + 5)
                End If

                If playCall = 10 Or playCall = 9 Or playCall = 13 Then
                    ydsGained = (Int(Rnd(1) * 5) + 3)
                End If

                If playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17 Or playCall = 18 Then
                    ydsGained = (Int(Rnd(1) * 7) + 4)
                End If

                3541 '
                Call YARDAGE
                Call SACK

                '-- No fatigue yards for sack

                'Yardage for sack is negative
                ydsGained = ydsGained * -1

                If defSacks(D) <> 0 Then

                    M5 = 0
                    RN0 = Int(Rnd(1) * teamSacks(D)) + 1

                    For currIdx = 0 To 14

                        'Find player for sack based on contribution
                        M5 = M5 + playerSacks(D, currIdx)

                        If RN0 <= M5 Then
                            pbpString$ = pbpString$ + " BY " + defSacks$(D, currIdx)
                            Exit For
                        End If

                    Next

                End If

                Call PBP

                scoreX0 = 4
                playType = 1

                'Play results - sack
                If endZone = 1 Then

                    Call TryForTwoResult
                    GoTo CheckPoss_2PtConv

                Else

                    sackYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                    If sackYdLine! < .05 Then

                        'Safety!
                        GoTo 3250

                    Else

                        'Check for fumble - Sack
                        fumbPlayType = 1
                        Call CheckFumble_1

                    End If

                End If

                GoTo 3309

            End If

            'FORCED TO SCRAMBLE / RUN
            willScramble = 0
            qbContrib = GetQBRushPct%(qbacks$(P1, qbNumber(P1)))

            If qbMobility(P1) = 9 Then

                Select Case qbContrib

                    Case 27 To 33
                        mobileBonus = 0

                    Case 34 To 39
                        mobileBonus = 6

                    Case 40 To 49
                        mobileBonus = 5

                    Case 50 To 99
                        mobileBonus = 4

                End Select

            Else

                mobileBonus = 0

            End If

            'Short Pass
            If (playCall = 9 Or playCall = 10 Or playCall = 13) Then

                Select Case qbMobility(P1)

                    Case 1 To 6
                        scrambleChance = Int(qbMobility(P1))

                    Case 7
                        scrambleChance = 13

                    Case 8
                        scrambleChance = 19

                    Case 9
                        '21% - 27%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 21

                End Select

            End If

            'Medium Pass / Sideline Pass / Rollout
            If (playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17) Then

                Select Case qbMobility(P1)

                    Case 1 To 2
                        '2%, 4%
                        scrambleChance = qbMobility(P1) + 1

                    Case 3 To 4
                        scrambleChance = 5 'was 4

                    Case 5 To 6
                        '7%, 8%
                        scrambleChance = qbMobility(P1) + 2

                    Case 7 To 8
                        '14%, 16%
                        scrambleChance = qbMobility(P1) * 2

                    Case 9
                        '27% - 33%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 27

                End Select

            End If

            'Long Pass
            If (playCall = 12 Or playCall = 15) Then

                Select Case qbMobility(P1)

                    Case 1 To 6
                        '4%, 5%, 6%, 7%, 8%, 9%
                        scrambleChance = Int(qbMobility(P1)) + 3

                    Case 7
                        scrambleChance = 12

                    Case 8
                        '18%
                        scrambleChance = Int(qbMobility(P1) * 2) + 3

                    Case 9
                        '35% - 41%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 35

                End Select


            End If 'was 4

            'Pass on Fake Kick
            If playCall = 18 Then

                Select Case qbMobility(P1)

                    Case 1 To 4
                        '4%, 5%, 6%, 7%
                        scrambleChance = Int(qbMobility(P1)) + 3

                    Case 5
                        scrambleChance = 9

                    Case 6 To 7
                        scrambleChance = 14

                    Case 8
                        scrambleChance = 23

                    Case 9
                        '43% - 49%
                        scrambleChance = Int(Rnd(1) * mobileBonus) + 43

                End Select

            End If

            'Increase chances for College, since stats
            'are recorded as reduced by 15%
            If ruleOptType = 0 Then
                scrambleChance = scrambleChance + 15
            End If

            Call Rolld100(scrambleRoll)

            'Determine chance of QB scramble
            If scrambleRoll <= scrambleChance Then
                willScramble = 1
            End If

            If willScramble = 1 Then

                'QB RUN
                qbRushAvg = 0

                For currBack = 0 To 9
                    If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                        'Def Rush Adj doesn't apply
                        qbRushAvg = rbRushAvg(P1, currBack)
                        Exit For
                    End If
                Next currBack

                If qbRushAvg = 0 And qbMobility(P1) < 2 Then

                    'QB is not a rusher, or mobility is low
                    'Essentially, we're treating these as the expected behavior.
                    '(Low mobility QB's will often have low or negative averages)
                    '(This will translate to larger negatives below)
                    If specialGain < 10 Then
                        'average gains are 5-7 yds
                        qbRushMin = 2
                        qbRushMax = 7
                    Else
                        'longer gain
                        qbRushMin = 3
                        qbRushMax = 16
                    End If

                Else

                    'QB is a rusher
                    Call Rolld100(RN0)

                    Select Case RN0

                        Case 1 To 35
                            '0 - 2 yards
                            qbRushMin = 0
                            qbRushMax = 3

                        Case 36 To 75
                            '3 - 10 yards
                            qbRushMin = 2
                            qbRushMax = (qbRushAvg * 3) / 4

                        Case 76 To 90
                            '11 - 20 yards
                            qbRushMin = 7
                            qbRushMax = (qbRushAvg * 3) / 2

                        Case 91 To 100
                            '20+ yards
                            qbRushMin = 17
                            qbRushMax = qbRushAvg + 9

                    End Select

                End If

                ydsGained = Int(Rnd(1) * qbRushMax) + qbRushMin

                '-- FATIGUE NOT IMPLEMENTED
                'Fatigue yards - QB scramble (increase)
                If endZone = 1 Then
                    'ydsGained = ydsGained * fatigueLevel%
                End If

                Call YARDAGE

                startYds = 100 - ydLine
                endYds = startYds - ydsGained
                Call MOVEBALL

                RN0 = Int(Rnd(1) * 2) + 1

                If playCall = 17 Or playCall = 18 Then

                    If RN0 = 1 Then
                        pbpString$ = qbacks$(P1, I1) + " ROLLS OUT TO HIS RIGHT"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " ROLLS OUT TO HIS LEFT"
                    End If
                    Call PBP

                    If playCall = 17 Then
                        pbpString$ = qbacks$(P1, I1) + " TUCKS IT IN AND TAKES OFF!"
                        Call PBP
                        If ydsGained > 0 Then
                            pbpString$ = "HE PICKS UP ABOUT"
                        Else
                            pbpString$ = "HE LOSES ABOUT"
                        End If
                    Else
                        pbpString$ = qbacks$(P1, I1) + " KEEPS THE BALL AND AND RUNS FOR"
                    End If

                Else

                    If RN0 = 1 Then
                        pbpString$ = qbacks$(P1, I1) + " IS CHASED FROM THE POCKET"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " CAN'T FIND AN OPEN TARGET"
                    End If

                    Call PBP
                    pbpString$ = "HE SCRAMBLES FOR"

                End If

                3546 '
                playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                scoreX0 = 4
                scoreX1 = 9
                playType = 1

                If playYdLine! >= 99.94999 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
                Else
                    If Int(ydsGained) > 0 Then
                        pbpString$ = pbpString$ + Str$(ydsGained)
                    Else
                        If Int(ydsGained) = 0 Then
                            pbpString$ = "HE JUST MAKES IT BACK TO SCRIMMAGE"
                        Else
                            If playCall <> 17 Then
                                pbpString$ = pbpString$ + " A LOSS OF" + yardsToDecimalStr$(0 - ydsGained)
                            Else
                                pbpString$ = pbpString$ + yardsToDecimalStr$(0 - ydsGained)
                            End If
                        End If
                    End If
                End If

                If Int(ydsGained) > 0 Then
                    pbpString$ = pbpString$ + " YARDS"
                End If
                Call PBP

                'Play results - scramble
                If endZone = 1 Then

                    Call TryForTwoResult
                    GoTo CheckPoss_2PtConv

                Else

                    If playYdLine! < .05 Then
                        'Safety!
                        GoTo 3250
                    End If

                    If playYdLine! >= 99.9499 Then
                        'Increase first downs
                        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1
                        'Touchdown!
                        GoTo 3200
                    End If

                End If

                'Check for fumble - QB sneak / rollout
                fumbPlayType = 2
                Call CheckFumble_1

                GoTo 3309

            Else

                'Determine pass outcome
                Call Rolld100(passChance)

                'Compare value to expected interception %
                '               QB's Int % - Pass type adjustment + league DEF Int %
                intChance = qbPctInt(P1, I1) - passCovAdj(0, playCall - 8, coverage) + leagRat_GAME(D, 2)

                'Home Field Advantage: Interceptions
                '12% difference in takeaways
                'translates to ~0.2% chance per play
                'we will process this is as 1/5 chance of adjusting by 1
                If P1 = gameLoc Then
                    'Home: more INTs
                    If d100% <= 20 Then intChance = intChance - 1
                Else
                    'Away: fewer INTs
                    If d100% <= 20 Then intChance = intChance + 1
                End If

                If passChance > (100 - intChance) Then

                    'Intercepted!
                    fleaFlicker = 0
                    GoTo 3552

                Else

                    'Determine % chance of completion
                    Call AdjustCompPct

                    '-- FATIGUE NOT IMPLEMENTED
                    'Fatigue: 0-10% bonus
                    fatigueBonus = 0 'CInt((1.0 - fatigueLevel%) * 20

                    If passChance <= expCompPct + passTypeCmpAdj + ballPosCmpAdj + windCmpAdj + fatigueBonus Then
                        'Pass completed

                        'Determine yards based on player's average
                        If I3 > 9 Then
                            I4 = wrYdsPerC(P1, I3 - 10)
                            avgRecYds = I4
                        Else
                            I4 = rbYdsPerC(P1, I3)
                            avgRecYds = I4
                        End If

                        'Adjust yards for weather
                        If climate(4) = -1 Then
                            I4 = I4 - 0.5
                        End If
                        If climate(4) = -2 Then
                            I4 = I4 - 1
                        End If

                        If I4 < 11 Then
                            'Determine impact of blitz
                            Call BLITZ

                        Else

                            I4 = I4 + passCovAdj(2, playCall - 8, coverage)

                            If ydsPerComp(D) / 100 > 0.5 Then
                                I4 = I4 - (1 - ydsPerComp(D) / 100) * 10
                            End If
                            If ydsPerComp(D) / 100 < 0.5 Then
                                I4 = I4 + ydsPerComp(D) / 100 * 10
                            End If

                            'Check if the play is overused
                            Call OffPlayOveruse

                            'Short Pass
                            If playCall = 9 Or playCall = 10 Or playCall = 13 Then
                                I4 = I4 * ((((expCompPct + overuseAdj) * K3(P1, 3)) / (expCompPct + overuseAdj + 18)) / K3(P1, 3))
                            End If

                            'Long Pass
                            If playCall = 12 Or playCall = 15 Then
                                I4 = I4 * ((((expCompPct + overuseAdj) * K3(P1, 3)) / (expCompPct + overuseAdj - 18)) / K3(P1, 3))
                            End If

                            '-- FATIGUE NOT IMPLEMENTED
                            bigGainBonus = 0 'CInt((1.0 - fatigueLevel) * 2)
                            bigGainThreshold = 25 - bigGainBonus

                            bigGainRoll = Int(Rnd(1) * 25) + 1

                            'Big gain check: 4% for short/medium, better odds for deep passes
                            If (playCall <> 12 And playCall <> 15 And bigGainRoll = bigGainThreshold) Or _
                                ((playCall = 12 Or playCall = 15) And ((bigGainRoll = bigGainThreshold And ruleOptType = 0) Or (ruleOptType <> 0 And bigGainRoll >= bigGainThreshold - 1))) Then
                                'Longer gains - ~3% chance of TD when winning big already
                                If bigGainRoll = 25 And Int(Rnd(1) * 32) = 11 And score(P1, 0) > 13 And XD(D) > .6 Then
                                    ydsGained = 102
                                Else
                                    ydsGained = Int(Rnd(1) * I4 * 12) + 1 + (I4 * 6)
                                End If
                            Else

                                If bigGainRoll <= 19 Then
                                    'Standard gain: 76% chance
                                    ydsGained = Int(Rnd(1) * 13) + 1 + ((I4 * 2.3) - 7.2)
                                Else
                                    'Medium gain: 20% chance
                                    ydsGained = (Int(Rnd(1) * (5 + (I4 * 2))) + 1) + (3 * (I4 - 1))
                                End If
                            End If

                        End If

                        3586 '
                        'ADDED AFTER REPLAYS OF 5-24-93 <-- ???
                        '1-in-3 Chance And Pro Game = adjust by 1
                        If Int(Rnd(1) * 3) + 1 = 1 And ruleOptType <> 0 Then
                            ydsGained = ydsGained - 1
                        End If

                        '-- FATIGUE NOT IMPLEMENTED
                        'Fatigue yards - pass play (increase)
                        If endZone = 1 Then
                            'ydsGained = ydsGained * fatigueLevel%
                        End If

                        Call YARDAGE

                        scoreX0 = 3
                        scoreX1 = 2

                        If ydLine + ydsGained > 99.9499 Then
                            'I don't undertand why this is here
                            'ydsGained = 109 - ydLine
                        End If

                        newYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                        'Play-by-Play for Completion %
                        Call PassCompletePBP(playDir)

                        If fleaFlicker <> 0 Then fleaFlicker = 0

                        'If newYdLine! < 99.9499 Then
                        If ydLine + ydsGained < 99.9499 Then

                            'Check for fumble - normal pass
                            fumbPlayType = 0
                            Call CheckFumble_1

                            If ballFumbled = 0 Then
                                'If we're within a foot of the first down
                                If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then
                                    Call MEASUREMENT
                                End If
                            End If

                        End If

                        '...
                        'Else
                        If newYdLine! >= 99.9499 Then

                            ydsGained = 100 - ydLine

                            'End zone
                            Call PassYardagePBP(yardsToDecimalStr$(ydsGained))

                            'Longest catch
                            If ydsGained > gameLongRec(P1, I3) Then
                                gameLongRecTD$(P1, I3) = "t"
                                gameLongRec(P1, I3) = ydsGained
                            End If

                            'Longest pass?
                            If ydsGained > gameLongPassYds(P1, qbNumber(P1)) Then
                                gameLongPassTD$(P1, qbNumber(P1)) = "t"
                                gameLongPassYds(P1, qbNumber(P1)) = ydsGained
                            End If

                        Else

                            If Int(ydsGained) = 0 Then
                                pbpString$ = "HE'LL JUST GET TO THE LINE OF SCRIMMAGE"
                                Call PBP
                            Else
                                If ydsGained < 0 Then
                                    'Loss on the play
                                    Call ReceiverLossPBP(yardsToDecimalStr$(ydsGained * -1))
                                Else
                                    'Gain on the play
                                    Call PassYardagePBP(yardsToDecimalStr$(ydsGained))
                                End If
                            End If

                            'Longest catch?
                            If ydsGained > gameLongRec(P1, I3) Then
                                gameLongRecTD$(P1, I3) = ""
                                gameLongRec(P1, I3) = ydsGained
                            End If

                            'Longest pass?
                            If ydsGained > gameLongPassYds(P1, qbNumber(P1)) Then
                                gameLongPassTD$(P1, qbNumber(P1)) = ""
                                gameLongPassYds(P1, qbNumber(P1)) = ydsGained
                            End If

                        End If

                        'Play results - pass play
                        If endZone = 1 Then

                            ballFumbled = 0

                            Call TryForTwoResult
                            GoTo CheckPoss_2PtConv

                        Else

                            newYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                            If newYdLine! < .05 Then

                                ballFumbled = 0

                                'Safety!
                                GoTo 3250

                            End If

                            If newYdLine! >= 99.9499 Then

                                ballFumbled = 0

                                'Increase first downs
                                gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

                                'Touchdown!
                                GoTo 3200

                            End If

                        End If

                        If playCall = 16 Then

                            obChance = Int(Rnd(1) * 10) + 1

                            If obChance <= 7 Then

                                playSegment = 5

                                RN0 = Int(Rnd(1) * 5) + 1
                                Select Case RN0
                                    Case 1: pbpString$ = "AND HE GETS OUT OF BOUNDS"
                                    Case 2: pbpString$ = "AND HE'S KNOCKED OUT OF BOUNDS"
                                    Case 3: pbpString$ = "AND HE STEPS OUT OF BOUNDS"
                                    Case 4: pbpString$ = "AND HE'S PUSHED OUT OF BOUNDS"
                                    Case 5: pbpString$ = "AND HE'S SHOVED OUT OF BOUNDS"
                                End Select

                                Call PBP
                                ballFumbled = 0

                            End If

                        End If

                        'Resolve play (fumble / penalty) and record
                        GoTo 3309

                    Else

                        'Incomplete pass
                        GoTo 3535

                    End If 'Done checking for pass complete

                End If

            End If 'Done checking if rollout

        Case 19

            Call Rolld100(prChance)

            For J = 2 To 0 Step -1
                If prChance <= prNumRet(D, J) Then
                    I1 = J
                End If
            Next J

            puntRetNbr = I1

            Call WINDEX
            Call CLEARPBP

            pbpString$ = pret$(D, puntRetNbr) + " (" + LTrim$(Str$(Int(prYdsPerRet(D, puntRetNbr) * 3))) + " AVG) IS DEEP TO RECEIVE"
            Call PBP

            If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then
                'Punt away from return man if:
                'Wind is not AGAINST us AND
                '  Average >= 10   --OR--
                '  Team is close to midfield
                If climate(0) <> D And ((prYdsPerRet(D, puntRetNbr) * 3) >= 12 Or ydLine >= 50) Then
                    puntChoice = 1
                Else
                    puntChoice = 0
                End If

            Else

                Call FRAMEOPTION(9, P1)
                Print "PUNTING OPTIONS "; gameTeams$(P1)
                Locate 11, 14
                Locate , 14: Print "0 - PUNT NORMAL"
                Locate , 14: Print "1 - PUNT AWAY FROM THE RETURN MAN"
                Locate , 14: Print "2 - FAKE PUNT (RUN)"
                Locate , 14: Print "3 - FAKE PUNT (PASS)"
                Locate , 14: Print "4 - TAKE INTENTIONAL SAFETY (INSIDE OWN 20)"
                Locate , 14: Print "5 - QUICK KICK (1ST-3RD DOWN)"

                Do
                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        Screen , , 0, 0
                        GoTo 3118
                    Else
                        puntChoice = Val(I$)
                    End If

                    '-- Shouldn't these be "And Not" ??
                Loop Until puntChoice >= 0 And puntChoice <= 5 Or Not (ydLine > 20 And puntChoice = 4) Or Not (currDown = 4 And puntChoice = 5) Or Not (currDown < 4 And puntChoice < 5)

                Screen , , 0, 0

                If puntChoice = 5 Then

                    If d100% <= 50 Then
                        pbpString$ = gameTeams$(P1) + " IS QUICK KICKING!!"
                    Else
                        pbpString$ = gameMascots$(P1) + " ARE QUICK KICKING!!"
                    End If

                    Call PBP

                    Call Rolld100(kickChance)
                    scoreX0 = 0
                    ydsGained = ydsPerPunt(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * ydsPerPunt(P1, 0)))) + 11

                    Select Case climate(0)

                        Case P1, 2
                            ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - climate(1)

                        Case D
                            ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + climate(1)

                    End Select

                    If (coverage > 6 And kickChance >= 60 And kickChance <= 72) Or (coverage <= 6 And kickChance >= 50 And kickChance <= 56) Then
                        'Blocked punt
                        GoTo 3840
                    End If

                    pbpString$ = "THE KICK IS UP AND ON ITS WAY"
                    Call PBP

                    ydsGained = ydsGained + 2

                    Call YARDAGE
                    'Gets us a value for ydLineTeam

                    kickDistYds = CInt((ydsGained) * 10) / 10

                    pbpString$ = "KICK BY " + gameTeams$(P1) + " TRAVELS" + Str$(Int(kickDistYds)) + " YARDS"
                    Call PBP

                    'Punt Att
                    gameStatsTeam(P1, 7) = gameStatsTeam(P1, 7) + 1

                    puntYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                    If puntYdLine! >= 99.94999 Then

                        'Punt travels to end zone

                        'Track punt yards
                        gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + 100 - ydLine

                        'Track touchback for punter
                        gameStatsTeam(P1, 27) = gameStatsTeam(P1, 27) + 1

                        If 100 - ydLine > gameStatsTeam(P1, 26) Then
                            'Longest punt?
                            gameStatsTeam(P1, 26) = 100 - ydLine
                        End If

                        'Hangtime of 4 - 5 seconds
                        '-- Average is 4.2-4.4 seconds
                        '-- Range is 3.7 - 4.7 seconds
                        Call AddPossessionTime(4, 5)
                        Call CHANGEPOSS
                        Call Touchback(0)

                    Else

                        'Punt yardage - quick kick
                        gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + ydsGained

                        fairCatchZone = 0

                        Select Case ruleOptType

                            Case 0
                                'College
                                If ruleOptColl <= 10 Then
                                    fairCatchLine = 80
                                Else
                                    fairCatchLine = 75
                                End If

                            Case Is >= 1
                                'Pro
                                If ruleOptPro <= 8 Then
                                    fairCatchLine = 80
                                Else
                                    fairCatchLine = 75
                                End If

                        End Select

                        If ydsGained + ydLine > fairCatchLine Then
                            'Inside the fair catch area (20/25 yd line)
                            fairCatchZone = 1
                            gameStatsTeam(P1, 29) = gameStatsTeam(P1, 29) + 1
                        End If

                        If ydsGained > gameStatsTeam(P1, 26) Then
                            'Longest punt?
                            gameStatsTeam(P1, 26) = ydsGained
                        End If

                        Call PuntNotReturned(fairCatchZone, puntChoice, penOnPlay)

                        If penOnPlay = 1 Then

                            totalSTPenalties = totalSTPenalties + 1

                            'Fair Catch Interference
                            pbpString$ = "*** PENALTY - " + gameTeams$(D) + " - FAIR CATCH INTERFERENCE - 15 YDS ***"

                            penaltyYds! = 15

                            GoTo HANDLEPENALTY

                        Else

                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Call DELAY

                        End If

                    End If

                    GoTo CheckPoss_OT

                End If

            End If

            scoreX0 = 0

            'Punt yardage - normal punt
            ydsGained = ydsPerPunt(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * ydsPerPunt(P1, 0)))) + 3

            If (100 - ydLine) <= ((ydsPerPunt(P1, 0) * 2) + 28) Then
                ydsGained = ydsGained + (100 - ydLine) - (31 + ydsPerPunt(P1, 0) * 2)
            End If

            Select Case climate(0)

                Case P1, 2
                    ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - climate(1)

                Case D
                    ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + climate(1)

            End Select

            Call YARDAGE

            If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then

                'Computer will not call Punt Safe
                If d100% <= 10 Then
                    puntDefChoice = 1
                Else
                    puntDefChoice = 0
                End If

            Else

                Call FRAMEOPTION(9, D)
                Print "THE PUNTING TEAM FOR "; gameTeams$(P1); " IS ON THE FIELD"
                Locate 11, 14
                Locate , 14: Print "0 - SET UP PUNT RETURN"
                Locate , 14: Print "1 - GO FOR THE BLOCK"
                Locate , 14: Print "2 - CALL PUNT-SAFE"
                Print
                Locate , 14: Print "SELECT OPTION "; gameTeams$(D)

                Do
                    I$ = GetKeyPress$
                    puntDefChoice = Val(I$)
                Loop Until puntDefChoice >= 0 And puntDefChoice <= 2

                Screen , , 0, 0

            End If

            'PUNTSNAP
            Select Case puntChoice

                Case 2, 3

                    ' *** FAKE PUNT - RUN/PASS ***
                    If puntDefChoice = 2 Then

                        pbpString$ = gameTeams$(D) + " IS IN PUNT SAFE SET...FAKE PUNT? (Y/N)"
                        Call PBP

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
                    End If

                    If UCase$(I$) = "Y" Or puntDefChoice <> 2 Then

                        If puntChoice = 3 Then

                            ' *** FAKE W/PASS ***
                            playType = 2
                            ydsGained = 0
                            pbpString$ = "IT'S A FAKE!!! " + punter$(P1, 0) + " IS LOOKING TO PASS"
                            Call PBP

                            Call Rolld100(RN0)

                            If puntDefChoice = 0 Then

                                Select Case RN0

                                    'Fake punts convert about 50% of the time
                                    Case 1 To 50:
                                        ydsGained = Int(Rnd(1) * 27)

                                    Case 37 To 90:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        GoTo 3309

                                    Case 91 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                End Select

                            End If

                            If puntDefChoice = 1 Then

                                Select Case RN0

                                    Case 1 To 26:
                                        ydsGained = Int(Rnd(1) * 20)

                                    Case 27 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        GoTo 3309

                                    Case 81 To 90:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                    Case 91 To 100:
                                        ydsGained = 100 - ydLine

                                End Select

                            End If

                            If puntDefChoice = 2 Then

                                Select Case RN0

                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 7) + 6

                                    Case 11 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        GoTo 3309

                                    Case 81 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                End Select

                            End If

                            pbpString$ = "PASS IS COMPLETE FOR"

                            If CInt((ydsGained + ydLine) * 10) / 10 >= 99.9499 Then
                                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Call PBP
                            Else
                                pbpString$ = pbpString$ + yardsToDecimalStr$(ydsGained) + " YARDS"
                                Call PBP
                            End If

                            playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                            If playYdLine! < .05 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If playYdLine! >= 99.9499 Then
                                'Touchdown!
                                GoTo 3200
                            End If

                            fumbPlayType = 2
                            Call CheckFumble_1

                        Else

                            playType = 1
                            Call PuntFakeRunYards(puntDefChoice)

                            playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                            If playYdLine! < .05 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If playYdLine! >= 99.9499 Then
                                'Touchdown!
                                GoTo 3200
                            End If

                            fumbPlayType = 2
                            Call CheckFumble_1

                        End If

                        GoTo 3309

                    End If 'Done processing fake punt

                Case 4
                    Call TAKESAFETY
                    'Offensive Safety
                    GoTo 3251

            End Select

            PSNAP:
            '-----------------------------
            '** Normal punt successful **
            '-----------------------------
            pbpString$ = punter$(P1, 0) + " WAITS FOR THE SNAP"
            Call PBP

            RN5 = Int(Rnd(1) * 200) + 1

            If RN5 >= 12 And RN5 <= 14 Then

                '1.5% chance of bad snap - punt
                pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
                Call PBP

                If ruleOptType = 0 Then
                    '10-25 yd loss
                    ydsGained = (Int(Rnd(1) * 16) + 10) * -1
                Else
                    '0-10 yd loss
                    ydsGained = (Int(Rnd(1) * 11) + 5) * -1
                End If

                Call AddPossessionTime(4, 7)

                'Team Fumble
                gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1
                'Team Fumbles lost
                gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1

                fumbRetYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                If fumbRetYdLine! < -9.999 Then

                    'Offensive Safety
                    score(D, quarter) = score(D, quarter) + 2
                    score(D, 0) = score(D, 0) + 2
                    GoTo 3251

                End If

                If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

                    pbpString$ = "IT'S BLOCKED IN THE END ZONE!"
                    Call PBP

                    If d100% <= 50 Then

                        'Offensive Safety
                        score(D, quarter) = score(D, quarter) + 2
                        score(D, 0) = score(D, 0) + 2
                        pbpString$ = "THE OFFENSE RESCUES IT!"
                        Call PBP
                        GoTo 3251

                    Else

                        scoreX1 = 10
                        pbpString$ = "THE DEFENSE SCOOPS IT UP!"
                        Call PBP

                        Call CHANGEPOSS

                        'Touchdown!
                        GoTo 3200

                    End If

                End If

                'Update possession for DEF (blocked punt)
                If isOT = 1 And overtimeOpt >= 6 Then
                    nbrPossOT(D) = 1
                End If

                Call CHANGEPOSS

                currDown = 1
                ydsToFirst = 10
                ydLine = 100 - ydLine

                If ydsToFirst >= 90 Then
                    ydsToFirst = 100 - ydLine
                End If

                If d100% <= 50 Then
                    pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
                Else
                    pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
                End If

                If ydLine <= 1 Then
                    pbpString$ = pbpString$ + " 1 YARD LINE"
                Else
                    If ydLine > 50 Then
                        pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
                    Else
                        pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
                    End If
                End If

                Call PBP

                playSegment = 5

                GoSub 3900

            Else

                'Snap gets off fine
                Call Rolld100(RN6)

                If puntDefChoice = 1 Then

                    'Check for block
                    pbpString$ = "THERE'S A HEAVY RUSH!!!"
                    Call PBP

                    Select Case RN6

                        Case 1 To 8
                            'Blocked punt
                            GoTo 3840

                        Case 9 To 11
                            Call PenaltyOnPunt(autoFirst)

                            GoTo HANDLEPENALTY

                    End Select

                End If

                pbpString$ = punter$(P1, 0) + " TAKES THE SNAP"
                Call PBP

                If puntChoice = 1 Then
                    'Punt away from the return man
                    '-- should this do more???
                    If d100% <= 10 Then
                        ydsGained = ydsGained - (Int(Rnd(1) * 11) + 25)
                        pbpString$ = "HE SHANKS IT!"
                    Else
                        ydsGained = ydsGained - (Int(Rnd(1) * 9) + 4)
                        pbpString$ = "HE KICKS IT TOWARDS THE CORNER"
                    End If

                End If

                If ydsGained < 40 Then
                    pbpString$ = "HE GETS THE PUNT AWAY"
                    Call PBP
                Else
                    Call PUNTPBP
                End If

                puntDistYds! = CInt((ydsGained) * 10) / 10

                pbpString$ = "PUNT BY " + punter$(P1, 0) + " TRAVELS" + Str$(Int(puntDistYds!)) + " YARDS"
                Call PBP

                '** Regular Punt attempt + return **

                'Increase punt attempt
                gameStatsTeam(P1, 7) = gameStatsTeam(P1, 7) + 1

                puntYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                If puntYdLine! >= 99.94999 Then

                    'Track punt yards
                    gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + 100 - ydLine

                    'Track touchback
                    gameStatsTeam(P1, 27) = gameStatsTeam(P1, 27) + 1

                    'Hangtime of 4 - 5 seconds
                    '-- Average is 4.2-4.4 seconds
                    '-- Range is 3.7 - 4.7 seconds
                    Call AddPossessionTime(4, 5)
                    Call CHANGEPOSS
                    Call Touchback(0)

                Else

                    'Track punt yards
                    gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + ydsGained

                    Call Rolld100(puntChance)

                    Select Case ruleOptType

                        Case 0
                            'College
                            If ruleOptColl <= 10 Then
                                fairCatchLine = 80
                            Else
                                fairCatchLine = 75
                            End If

                        Case Is >= 1
                            'Pro
                            If ruleOptPro <= 8 Then
                                fairCatchLine = 80
                            Else
                                fairCatchLine = 75
                            End If

                    End Select

                    resultLine = ydsGained + ydLine
                    If resultLine > 80 Then
                        'Kicks inside the 20
                        gameStatsTeam(P1, 29) = gameStatsTeam(P1, 29) + 1
                    End If

                    fairCatchZone = 0

                    If resultLine > fairCatchLine Then
                        'We're in the fair catch zone, but let's ensure it makes sense to call

                        'For older rulesets that didn't auto-grant a touchback,
                        'only call fair catch outside the 10 yard line.
                        If ((ruleOptType > 0 And ruleOptPro < 9) Or (ruleOptType = 0 And ruleOptColl < 11)) And resultLine <= 90 Then
                            fairCatchZone = 1
                        End If

                        'For newer rule sets, we would take the fair catch
                        'anywhere in the allowed zone because it gives us touchback
                        If ((ruleOptType > 0 And ruleOptPro >= 9) Or (ruleOptType = 0 And ruleOptColl >= 11)) Then
                            fairCatchZone = 1
                        End If

                    End If

                    If ydsGained > gameStatsTeam(P1, 26) Then
                        'Longest punt?
                        gameStatsTeam(P1, 26) = ydsGained
                    End If

                    'Update possession for DEF (receiving team) - punt
                    If isOT = 1 And overtimeOpt >= 6 Then
                        nbrPossOT(D) = 1
                    End If

                    'See if punt can be returned
                    If (puntChoice = 1 And d100% < 50) Or (fairCatchZone = 1 And d100% < 75) Or (ydsGained + ydLine >= 90 And puntChance <= 5) Or (ydsGained + ydLine < fairCatchLine And puntChance > 65) Or puntDefChoice = 2 Then
                        ' ** DO NOT RETURN:
                        ' - 50% of pnnts away from return man
                        ' - 75% of punts inside the fair catch zone
                        ' - 5% of punts inside the 10
                        ' - 35% of punts outside fair catch zone
                        ' - Punt Safe formation
                        Call PuntNotReturned(fairCatchZone, puntChoice, penOnPlay)

                        If penOnPlay = 1 Then

                            totalSTPenalties = totalSTPenalties + 1

                            'Fair Catch Interference
                            pbpString$ = "*** PENALTY - " + gameTeams$(D) + " - FAIR CATCH INTERFERENCE - 15 YDS ***"

                            penaltyYds! = 15

                            GoTo HANDLEPENALTY

                        Else

                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Call DELAY

                        End If

                    Else

                        'Normal PR yardage
                        Call PuntReturnYards

                        If ydsGained + ydLine >= 99.99 Then

                            'Track Punt Return
                            gameStatsPlayer(P1, puntRetNbr + 3, 14) = gameStatsPlayer(P1, puntRetNbr + 3, 14) + 1

                            'Track Return Yards
                            gameStatsPlayer(P1, puntRetNbr + 3, 15) = gameStatsPlayer(P1, puntRetNbr + 3, 15) + 100 - ydLine

                            'Track Return TD
                            gameStatsPlayer(P1, puntRetNbr + 3, 16) = gameStatsPlayer(P1, puntRetNbr + 3, 16) + 1

                            returnGain = 100 - ydLine

                            If returnGain > gameLongPRYds(P1, puntRetNbr) Then
                                gameLongPRTD$(P1, puntRetNbr) = "t"
                                gameLongPRYds(P1, puntRetNbr) = returnGain
                            End If

                            I1 = puntRetNbr

                            'Touchdown!
                            GoTo 3200

                        End If

                        gameStatsPlayer(P1, puntRetNbr + 3, 14) = gameStatsPlayer(P1, puntRetNbr + 3, 14) + 1
                        gameStatsPlayer(P1, puntRetNbr + 3, 15) = gameStatsPlayer(P1, puntRetNbr + 3, 15) + ydsGained

                        If ydsGained > gameLongPRYds(P1, puntRetNbr) Then
                            gameLongPRTD$(P1, puntRetNbr) = ""
                            gameLongPRYds(P1, puntRetNbr) = ydsGained
                        End If

                        If ydsGained + ydLine <= .01 Then
                            'Safety!
                            GoTo 3250
                        End If

                        currDown = 1
                        ydsToFirst = 10

                        ydLine = ydLine + ydsGained

                        If ydLine >= 89.99 Then
                            ydsToFirst = 100 - ydLine
                        End If

                        RN1 = Int(Rnd(1) * 1000) + 1

                        If RN1 >= KO_PENALTY_THRESHOLD - teamRat_GAME(P1, 0) Then

                            Call PenaltyOnPuntReturn(autoFirst)
                            GoTo HANDLEPENALTY

                        Else

                            'Check for fumble - punt return
                            fumbPlayType = 4
                            Call CheckFumble_1
                            If ballFumbled = 1 Then
                                Call FumblePBP
                                GoTo 650
                            End If

                        End If

                        'Hangtime of 4 - 5 seconds
                        '-- Average is 4.2-4.4 seconds
                        '-- Range is 3.7 - 4.7 seconds
                        Call AddPossessionTime(4, 5)

                        If ydsGained <= 10 Then
                            'Shorter returns are 4-8 seconds
                            Call AddPossessionTime(4, 8)
                        Else
                            'Longer returns are 10-12 seconds
                            Call AddPossessionTime(10, 12)
                        End If

                        Call DELAY

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                    End If

                End If

            End If

            GoTo CheckPoss_OT

        Case 20

            Call Rolld100(fgChance)

            For J = 1 To 0 Step -1
                If fgChance <= kickerFGPct(P1, J) Then
                    I1 = J
                End If
            Next J

            activeKicker(P1) = I1

            'FG range is considered up to 67 yards
            '-- 48 yard line = 65 yard attempt
            If ydLine < 48 + goalPostAdj Then

                pbpString$ = "NOT IN FIELD GOAL RANGE"
                Call PBP
                GoTo 700

            Else

                ' *** FIELD GOAL DECISION ***
                If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

                    fgChoice = 0

                Else

                    Call FRAMEOPTION(6, P1)
                    Print "FIELD GOAL OPTIONS "; gameTeams$(P1)
                    Locate 11, 14
                    Locate , 14: Print "0 - ATTEMPT FIELD GOAL"
                    Locate , 14: Print "1 - FAKE FIELD GOAL (RUN)"
                    Locate , 14: Print "2 - FAKE FIELD GOAL (PASS)"

                    Do
                        I$ = GetKeyPress$
                        fgChoice = Val(I$)
                    Loop Until fgChoice >= 0 And fgChoice <= 2 Or I$ = Chr$(27)

                    Screen , , 0, 0

                End If

                If I$ = Chr$(27) Then GoTo 3118

                Call Rolld100(RN0)

                If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then

                    fgDefChoice = 1

                Else

                    Call FRAMEOPTION(9, D)

                    Print "THE FIELD GOAL UNIT FOR "; gameTeams$(P1); " IS ON THE FIELD"
                    Locate 12, 14
                    Locate , 14: Print "1 - GO FOR THE BLOCK"
                    Locate , 14: Print "2 - CALL FIELD GOAL-SAFE"
                    Print
                    Locate , 14: Print "SELECT OPTION "; gameTeams$(D)

                    Do
                        I$ = GetKeyPress$
                        fgDefChoice = Val(I$)
                    Loop Until fgDefChoice >= 1 And fgDefChoice <= 2

                End If

                'Screen , , 0, 0
                Call WINDEX
                Call CLEARPBP

                'Fake Field Goal Was Chosen
                If fgChoice > 0 Then

                    Select Case ydsToFirst

                        Case 1 To 5
                            'Short Yardage
                            passFakeSuccess(1) = 55
                            passFakeSuccess(2) = 30

                        Case 6 To 15
                            'Medium Yardage
                            passFakeSuccess(1) = 40
                            passFakeSuccess(2) = 20

                        Case Is >= 16
                            'Long Yardage
                            passFakeSuccess(1) = 15
                            passFakeSuccess(2) = 5

                    End Select

                    ' *** FAKE FIELD GOAL ***
                    If fgDefChoice = 2 Then

                        pbpString$ = gameTeams$(D) + " IS IN FIELD GOAL SAFE SET...FAKE FIELD GOAL? (Y/N)"
                        Call PBP

                    Else

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "N" Then GoTo 3602

                    End If

                    If fgChoice = 1 Then

                        ' *** FAKE W/RUN ***
                        Call FGFakeRunYards(fgDefChoice)

                        playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                        If playYdLine! < .05 Then
                            'Safety!
                            GoTo 3250
                        End If

                        If playYdLine! >= 99.9499 Then
                            'Touchdown!
                            GoTo 3200
                        End If

                        'Check for fumble - Fake FG Run
                        fumbPlayType = 2
                        Call CheckFumble_1

                    Else

                        ' *** FAKE W/PASS ***
                        pbpString$ = "IT'S A FAKE!!! THE HOLDER IS LOOKING TO PASS"
                        Call PBP

                        ydsGained = 0

                        passIncomplete = 0
                        passIntercepted = 0

                        Call Rolld100(RN0)

                        Select Case fgDefChoice

                            Case 1
                                'FG Block

                                Select Case RN0

                                    Case 1 To passFakeSuccess(1):
                                        ydsGained = Int(Rnd(1) * 20)

                                    Case passFakeSuccess(1) + 1 To 85:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        passIncomplete = 1

                                    Case 86 To 91:
                                        pbpString$ = "THE PASS IS INTERCEPTED"
                                        Call PBP
                                        passIntercepted = 1

                                    Case 92 To 100:
                                        ydsGained = 100 - ydLine

                                End Select

                            Case 2
                                'FG Safe

                                Select Case RN0

                                    Case 1 To passFakeSuccess(2):
                                        ydsGained = Int(Rnd(1) * 7) + 6

                                    Case passFakeSuccess(2) + 1 To 95:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        passIncomplete = 1
                                        GoTo 3309

                                    Case 96 To 98:
                                        pbpString$ = "THE PASS IS INTERCEPTED"
                                        Call PBP
                                        passIntercepted = 1

                                    Case 99 To 100:
                                        ydsGained = 100 - ydLine

                                End Select

                        End Select

                        'Fake FG Pass
                        If passIntercepted = 1 Then

                            scoreX0 = 0

                            'Team Pass Att
                            gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                            'Team Int
                            gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1

                            If fleaFlicker <> 1 Then
                                'QB Pass Att
                                gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                                'QB Interceptions
                                gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1
                            End If

                            'Record INT's for Defense
                            gameInts(D, defPlayer, 0) = gameInts(D, defPlayer, 0) + 1

                            'Update possession for DEF (interception)
                            If isOT = 1 And overtimeOpt >= 6 Then
                                nbrPossOT(D) = 1
                            End If

                            Call AddPossessionTime(5, 8)

                            GoTo InterceptionReturn

                        Else

                            If passIncomplete <> 1 Then

                                pbpString$ = "PASS IS COMPLETE FOR"

                                catchYdLine! = CInt((ydsGained + ydLine) * 10) / 10
                                catchYds! = CInt((ydsGained) * 10) / 10

                                If catchYdLine! >= 99.9499 Then
                                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Else
                                    pbpString$ = pbpString$ + Str$(Int(catchYds!)) + " YARDS"
                                End If

                                Call PBP

                                If catchYdLine! < .05 Then
                                    'Safety!
                                    GoTo 3250
                                End If

                                If catchYdLine! >= 99.9499 Then
                                    'Touchdown!
                                    GoTo 3200
                                End If

                                'Check for fumble - Fake FG Pass
                                fumbPlayType = 2
                                Call CheckFumble_1

                            End If

                        End If

                    End If

                    GoTo 3309

                Else

                    3602 'FIELD GOAL

                    If ydLine >= 97 + goalPostAdj Then
                        fgSuccessChance = kickerIdx(P1, activeKicker(P1))
                    Else
                        Call FieldGoalChance
                    End If

                    Call Rolld100(RN9)

                    If RN9 < 50 Then
                        pbpString$ = kicker$(P1, activeKicker(P1)) + " TROTS ON TO TRY A FIELD GOAL"
                    Else
                        pbpString$ = "THE FIELD GOAL UNIT FOR " + gameTeams$(P1) + " COMES ON THE FIELD"
                    End If

                    Call PBP

                    pbpString$ = "THIS WILL BE A" + Str$(CInt(117 + goalPostAdj - ydLine)) + " YARD ATTEMPT"
                    Call PBP

                    If RN9 < 50 Then
                        pbpString$ = "HE WAITS FOR THE SNAP"
                    Else
                        pbpString$ = kicker$(P1, activeKicker(P1)) + " WAITS FOR THE SNAP"
                    End If

                    RN0 = Int(Rnd(1) * 200) + 1

                    If RN0 = 12 Or RN0 = 13 Then

                        '0.5% chance of bad snap - field goal
                        pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
                        Call PBP

                        Call AddPossessionTime(4, 7)

                        'Team Fumbles
                        gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1
                        'Team Fumbles Lost
                        gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1

                        ydsGained = (Int(Rnd(1) * 9) + 6) * -1
                        retYdLine! = CInt((ydsGained + ydLine) * 10) / 10

                        If ydLine + ydsGained < -9.999 Then
                            score(D, quarter) = score(D, quarter) + 2
                            score(D, 0) = score(D, 0) + 2
                            GoTo 3251
                        End If

                        If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

                            If d100% <= 50 Then

                                score(D, quarter) = score(D, quarter) + 2
                                score(D, 0) = score(D, 0) + 2

                                'Safety!
                                GoTo 3251

                            Else

                                scoreX1 = 10
                                ydsToScore = ydLine

                                Call CHANGEPOSS
                                Call ShowChainMarker
                                startYds = 100 - ydLine
                                endYds = retYdLine!
                                Call MOVEBALL

                                'Touchdown!
                                GoTo 3200

                            End If

                        End If

                        'Update possession for DEF (blocked FG)
                        If isOT = 1 And overtimeOpt >= 6 Then
                            nbrPossOT(D) = 1
                        End If

                        Call CHANGEPOSS
                        currDown = 1
                        ydsToFirst = 10
                        ydLine = 100 - ydLine

                        If ydsToFirst >= 90 Then
                            ydsToFirst = 100 - ydLine
                        End If

                        If d100% <= 50 Then
                            pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
                        Else
                            pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
                        End If

                        If ydLine <= 1 Then
                            pbpString$ = pbpString$ + " 1 YARD LINE"
                        Else
                            If ydLine > 50 Then
                                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
                            Else
                                pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
                            End If
                        End If

                        playSegment = 5

                        GoSub 3900

                        GoTo CheckPoss_OT

                    Else

                        Call Rolld100(blockChance)

                        'Increase FGA
                        gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) + 1

                        If fgDefChoice = 1 And ((ydLine >= 68 + goalPostAdj And blockChance >= 97) Or (ydLine < 68 + goalPostAdj And blockChance >= 99)) Then

                            'Blocked FG
                            pbpString$ = "KICK IS BLOCKED!!!"
                            Call PBP

                            Call AddPossessionTime(4, 7)

                            ydsGained = Int(Rnd(1) * 22) * -1

                            Call YARDAGE
                            ydLine = 100 - ydLine - ydsGained

                            If ydLine >= 109.949 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If ydLine >= 100 Then

                                'Update possession for DEF (blocked FG)
                                If isOT = 1 And overtimeOpt >= 6 Then
                                    nbrPossOT(D) = 1
                                End If

                                ydsToScore = 100 - ydLine
                                Call CHANGEPOSS
                                Call ShowChainMarker

                                'Block FG - Recover in end zone
                                If d100% <= 50 Then
                                    pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
                                Else
                                    pbpString$ = gameMascots$(P1) + " RECOVER IT IN THE END ZONE!!"
                                End If

                                Call PBP

                                scoreX1 = 6

                            Else

                                If d100% <= 94 Then

                                    'Update possession for DEF (fumble recovery)
                                    If isOT = 1 And overtimeOpt >= 6 Then
                                        nbrPossOT(D) = 1
                                    End If

                                    Call CHANGEPOSS

                                    currDown = 1
                                    ydsToFirst = 10

                                    If ydLine >= 90 Then
                                        ydsToFirst = 100 - ydLine
                                    End If

                                    Call DELAY

                                    Call CheckGameClock
                                    If gameClock! = 0 Then GoTo 3360

                                    GoTo CheckPoss_OT

                                Else

                                    '6% chance of being recovered for a touchdown

                                    'Update possession for DEF (fumble recovery)
                                    If isOT = 1 And overtimeOpt >= 6 Then
                                        nbrPossOT(D) = 1
                                    End If

                                    ydsGained = 100 - ydLine
                                    ydsToScore = ydsGained

                                    Call CHANGEPOSS
                                    Call ShowChainMarker

                                    startYds = 100 - ydLine
                                    endYds = startYds - ydsGained
                                    Call MOVEBALL

                                    If d100% <= 50 Then
                                        pbpString$ = gameTeams$(P1) + " TAKES IT INTO THE END ZONE!!"
                                    Else
                                        pbpString$ = gameMascots$(P1) + " TAKE IT INTO THE END ZONE!!"
                                    End If

                                    Call PBP

                                    scoreX1 = 6

                                End If

                            End If

                            'Touchdown!
                            GoTo 3200

                        Else

                            If d100% > PK_PENALTY_THRESHOLD Then

                                'Field Goal Attempt
                                pbpString$ = "THE KICK IS UP AND..."
                                Call PBP

                                Select Case climate(0)

                                    Case D
                                        fgSuccessChance = fgSuccessChance - Int(Rnd(1) * 7) + 3 - climate(1)

                                    Case P1, 2
                                        fgSuccessChance = fgSuccessChance + Int(Rnd(1) * 7) - 3 + climate(1)

                                End Select

                                Call FGATT

                                If fgChance <= fgSuccessChance Then

                                    'Increase FGM
                                    gameStatsPlayer(P1, 3 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 3 + activeKicker(P1) * 4, 17) + 1
                                    scoreX1 = 8
                                    I1 = activeKicker(P1)

                                    Call SCOREUPD

                                    currDown = 1
                                    ydsToFirst = 10
                                    score(P1, 0) = score(P1, 0) + 3
                                    score(P1, quarter) = score(P1, quarter) + 3

                                    Call FGMADE

                                    Call AddPossessionTime(4, 7)
                                    Call ITSGOOD
                                    Call SETSCREEN

                                    'Overtime Situations
                                    'College - New Possession
                                    'Reg SD - Game Over (first score)
                                    If isOT = 1 And (overtimeOpt >= 1 And overtimeOpt <= 5) And score(P1, 0) <> score(D, 0) Then
                                        GoTo 3364
                                    End If

                                    'College
                                    'Mod SD Reg
                                    If isOT = 1 And overtimeOpt >= 3 And overtimeOpt <= 7 Then

                                        If nbrPossOT(D) = 1 Then
                                            'Opp has had ball
                                            'Check score
                                            GoTo 3364
                                        Else
                                            'Opp has NOT possessed the ball;
                                            'They're about to get it for a kickoff
                                            nbrPossOT(D) = 1
                                        End If

                                    End If

                                    'Mod Sudden Death Postseason
                                    'If opp has had the ball or has TD,
                                    'then we need to evaluate score
                                    If isOT = 1 And overtimeOpt = 8 Then
                                        If nbrPossOT(D) = 1 Or otTDs(D) = 1 Then
                                            GoTo 3364
                                        Else
                                            nbrPossOT(D) = 1
                                        End If
                                    End If

                                    'Change possession to place the *
                                    'next to the team on Def when we
                                    'update the screen / status board
                                    Call CHANGEPOSS

                                    Call KickoffLine
                                    Call SetChainMarker
                                    Call StatusBoard

                                    Call CheckGameClock
                                    If gameClock! = 0 Then
                                        'End of quarter
                                        U3 = 9
                                        GoTo 3360
                                    End If

                                    'Change possession back to where it should be
                                    'Off always kicks off to Def
                                    Call CHANGEPOSS

                                    GoTo 340

                                Else

                                    Call MISSEDFG
                                    currDown = 1
                                    ydsToFirst = 10

                                    'Reg SD - Game Over (first score)
                                    If isOT = 1 And (overtimeOpt = 1 Or overtimeOpt = 2) And score(P1, 0) <> score(D, 0) Then
                                        GoTo 3364
                                    End If

                                    'College
                                    'Mod SD Reg
                                    If isOT = 1 And overtimeOpt >= 3 And overtimeOpt <= 7 Then

                                        If nbrPossOT(D) = 1 Then
                                            'Opp has had ball
                                            'Check score
                                            GoTo 3364
                                        Else
                                            'Opp has NOT possessed the ball;
                                            'Pro - they're about to get a kickoff
                                            If overtimeOpt >= 6 Then
                                                nbrPossOT(D) = 1
                                            End If
                                        End If

                                    End If

                                    'Mod Sudden Death Postseason
                                    If isOT = 1 And overtimeOpt = 8 Then
                                        If nbrPossOT(D) = 0 Then
                                            nbrPossOT(D) = 1
                                        End If
                                    End If

                                    'Change possession back to where it should be
                                    'Off always kicks off to Def
                                    Call CHANGEPOSS

                                    If ydLine > 40 And ydLine <= 79.99 And ruleOptType = 1 And ruleOptPro >= 4 Then
                                        'NFL, after 1993
                                        ydLine = ydLine - 7
                                        ydLine = 100 - ydLine
                                    Else
                                        If ydLine > 79.99 Or goalPostAdj < 0 Then
                                            ydLine = 20
                                        End If
                                        If ydLine > 40 And ydLine <= 79.99 And goalPostAdj = 0 Then
                                            ydLine = 100 - ydLine
                                        End If
                                    End If

                                    Call AddPossessionTime(4, 7)
                                    Call CheckGameClock
                                    If gameClock! = 0 Then GoTo 3360

                                    GoTo CheckPoss_OT

                                End If

                            Else

                                'Foul on the play
                                Call PenaltyOnPlaceKick(autoFirst)
                                GoTo HANDLEPENALTY

                            End If 'Done checking FG success

                        End If ' Done handling successful FG

                    End If ' Done checking for bad snap

                    GoTo 700

                End If

            End If

        Case 21
            Call SCOUTREPT
            GoTo 700

        Case 22
            Call CHOOSEQB(P1)
            GoTo 700

        Case 23
            Call BOXOPTIONS(0)
            GoTo 850

        Case 24
            Call CHOOSEKICKER(P1)
            GoTo 700

    End Select

    '----------

    3535 ' PASS NOT COMPLETE
    'Incomplete / Interference Penalty
    scoreX0 = 2
    RN1 = Int(Rnd(1) * 1000) + 1

    loseDown = 0
    playSegment = 4

    If RN1 <= 75 + teamRat_GAME(D, 0) And playCall <> 13 And endZone = 0 Then

        Call Rolld100(passPenalty)

        ' *** PASSING PENALTIES ***
        Select Case passPenalty

            Case 1 To 48

                If teamYears(impactTeam) >= 1938 Then

                    pbpString$ = "*** PENALTY - DEF - PASS INTERF."

                    'Penatly yards based on pass type
                    Select Case playCall

                        Case 9, 10, 13
                            penaltyYds! = Int(Rnd(1) * 10) + 1

                        Case 12, 15
                            penaltyYds! = Int(Rnd(1) * 20) + 21

                        Case 11, 14, 16, 17, 18
                            penaltyYds! = Int(Rnd(1) * 10) + 11

                    End Select

                    'In NCAA, or NFL before 1974
                    'Pass intereference not a spot foul
                    If ruleOptType = 0 And ruleOptColl >= 3 And penaltyYds! > 15 Or (ruleOptType >= 1 And ruleOptPro <= 1) Then
                        penaltyYds! = 15
                    End If

                    Call YARDAGE

                    If penaltyYds! + ydLine >= 99.99 Then

                        If ruleOptType = 0 Then
                            pbpString$ = pbpString$ + " IN THE END ZONE ***"

                            ydLine = 98
                            ydsToFirst = 2
                        Else
                            pbpString$ = pbpString$ + " IN THE END ZONE ***"

                            ydLine = 99
                            ydsToFirst = 1
                        End If

                    Else

                        pbpString$ = pbpString$ + " AT THE"

                        If penaltyYds! + ydLine < 50 Then
                            pbpString$ = pbpString$ + Str$(CInt(penaltyYds! + ydLine))
                        Else
                            pbpString$ = pbpString$ + Str$(CInt(100 - ydLine - penaltyYds!))
                        End If

                        pbpString$ = pbpString$ + " YD LINE ***"

                    End If

                    'Automatic first down
                    autoFirst = 1

                Else

                    'This was much more common in early days
                    pbpString$ = "*** PENALTY - OFF - ILLEGAL FORWARD PASS - 5 YDS ***"

                    '-5 yard penalty
                    penaltyYds! = -5

                    'Loss of down
                    loseDown = 1

                End If

            Case 49 To 50
                pbpString$ = "*** PENALTY - OFF - ILLEGAL FORWARD PASS - 5 YDS ***"

                '-5 yard penalty
                penaltyYds! = -5

                'Loss of down
                loseDown = 1

            Case 51

                If teamYears(impactTeam) >= 1982 Then

                    pbpString$ = "*** PENALTY - OFF - ILLEGAL TOUCH PASS - 5 YDS ***"

                    '-5 yard penalty
                    penaltyYds! = -5

                    'Loss of down - Pro only
                    If ruleOptType > 0 Then
                        loseDown = 1
                    End If

                Else

                    pbpString$ = "*** PENALTY - OFF - INELIGIBLE MAN DOWNFIELD - 5 YDS ***"

                    '-5 yard penalty
                    penaltyYds! = -5

                End If

            Case 52 To 56
                pbpString$ = "*** PENALTY - OFF - INELIGIBLE MAN DOWNFIELD - 5 YDS ***"

                '-5 yard penalty
                penaltyYds! = -5

            Case 57 To 64
                pbpString$ = "*** PENALTY - OFF - INTENTIONAL GROUNDING - 10 YDS ***"

                '10 yard penalty
                penaltyYds! = -10

                'Loss of down
                loseDown = 1

            Case 65 To 80

                If teamYears(impactTeam) >= 1938 Then

                    pbpString$ = "*** PENALTY - OFF - PASS INTERFERENCE - 10 YDS ***"

                    penaltyYds! = -10

                Else

                    'This was much more common in early days
                    pbpString$ = "*** PENALTY - OFF - ILLEGAL FORWARD PASS - 5 YDS ***"

                    '-5 yard penalty
                    penaltyYds! = -5

                    'Loss of down
                    loseDown = 1

                End If

            Case 81 To 100
                pbpString$ = "*** PENALTY - DEF - ROUGHING PASSER - 15 YDS / 1ST D! ***"

                penaltyYds! = 15

                'Automatic first down
                autoFirst = 1

        End Select

        GoTo HANDLEPENALTY

    Else

        Call INCOMPLETE

        'Play results - incomplete pass
        If endZone = 1 Then

            Call TryForTwoResult
            GoTo CheckPoss_2PtConv

        End If

    End If


    GoTo 3309


    '----- NOODLE -----

    3200 '
    Randomize Timer

    'So essentially we have a 50/50 chance
    'of seeing the team name or the mascot name
    'IE, TOUCHDOWN! 89 SAN FRANCISCO
    '    TOUCHDOWN! 49ERS

    If d100% <= 50 Then
        pbpString$ = "T O U C H D O W N !!! " + gameTeams$(P1)
    Else
        pbpString$ = "T O U C H D O W N !!! " + gameMascots$(P1)
    End If

    Call PBP

    Call SCOREUPD
    Call COMPILER

    If ruleOptType <> 4 Then
        'Not WFL; Touchdowns worth 6
        score(P1, quarter) = score(P1, quarter) + 6
        score(P1, 0) = score(P1, 0) + 6
    Else
        'WFL; Touchdowns worth 7
        score(P1, quarter) = score(P1, quarter) + 7
        score(P1, 0) = score(P1, 0) + 7
    End If

    Call SETSCREEN

    Call KickoffLine
    currDown = 1
    ydsToFirst = 10

    Call StatusBoard

    'Overtime - Single Q / Sudden Death
    If isOT = 1 And (overtimeOpt = 1 Or overtimeOpt = 2) Then
        GoTo 3364
    End If

    'Overtime - College / Mod. Sudden Death
    If isOT = 1 And (overtimeOpt >= 3) Then

        otTDs(P1) = 1

        'Mod SD Reg
        If (overtimeOpt = 6 Or overtimeOpt = 7) Then
            'Assign other team possession so that
            'code evaluates score correctly
            nbrPossOT(D) = 1
            'First TD wins
            'Game over
            GoTo 3364

        Else

            'College | Mod SD Post
            'If Opp has had ball and hasn't scored,
            'then the game should beover
            If nbrPossOT(D) = 1 And otTDs(D) = 0 Then
                GoTo 3364
            End If

        End If

    End If

    PointsAfterTD:
    Call GetPATChoice(convChoice)

    If convChoice = 2 Then

        ydsGained = 0
        endZone = 1

        If ruleOptType = 1 And ((ruleOptPro > -4 Or (ruleOptColl >= 0 And ((quarter >= 7 And overtimeOpt = 4) Or (quarter >= 6 And overtimeOpt = 5))))) Then
            'NFL 1994+ Or College Rules + OT + alternating 2-pt conversion
            ydLine = 98
        End If

        If ruleOptType = 0 Then
            'College rules
            '2 point conversion from the 3
            ydLine = 97
        End If

        convChoice = 0

        If quarter >= 9 Then
            test = 1
        End If

        'Choose a play for 2-point conversion
        GoTo 700

    Else

        ' ******************************
        ' **** POINT AFTER ATTEMPT *****
        ' ******************************
        'Adjust distance for goal posts
        K1(nbrScores, 4) = K1(nbrScores, 4) + 10

        'Determine kicker
        For J = 1 To 0 Step -1:
            'Auto-determine target kicker, I1
            If d100% <= kickerPATPct(P1, J) Then
                activeKicker(P1) = J
            End If
        Next J

        gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) + 1

        pbpString$ = kicker$(P1, activeKicker(P1)) + " IS ON TO TRY THE EXTRA POINT"
        Call PBP

        'Determine success
        If d100% <= kickerIdx(P1, activeKicker(P1)) Then

            gameStatsPlayer(P1, 5 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 5 + activeKicker(P1) * 4, 17) + 1

            pbpString$ = "THE KICK IS UP..."
            Call PBP
            Call MADEPAT

            'Increase score
            score(P1, quarter) = score(P1, quarter) + 1
            score(P1, 0) = score(P1, 0) + 1

            'Track the attempt for the kicker
            K1(nbrScores, 6) = activeKicker(P1)
            K1(nbrScores, 4) = K1(nbrScores, 4) + 1

            Call SETSCREEN

        Else

            'PAT blocked (9% chance)
            If d100% <= 9 Then

                pbpString$ = "KICK IS BLOCKED!!"
                Call PBP

                If isOT = 1 And overtimeOpt >= 6 Then
                    nbrPossOT(D) = 1
                End If

                If ruleOptType <> 1 Then
                    'All but NFL
                    Call Turnover_PAT
                    U3 = 9
                    GoTo CheckPoss_OT

                End If

            Else

                If d100% > 1 Then
                    Call MISSPAT
                Else
                    'Foul on the play
                    Call PenaltyOnPlaceKick(autoFirst)
                    GoTo HANDLEPENALTY
                End If

            End If

        End If

    End If

    'Overtime - After TD + PAT
    If isOT = 1 And ((overtimeOpt >= 3 And overtimeOpt <= 5) Or overtimeOpt = 8) Then
        GoTo 3364
    End If

    'Change possession to place the *
    'next to the team on Def when we
    'update the screen / status board
    Call CHANGEPOSS
    Call StatusBoard

    playSegment = 4

    U3 = 9

    ydsToFirst = 10

    GoSub 3900

    U3 = 0

    'Lineup the kickoff
    Call KickoffLine

    Call StatusBoard

    If playType > 0 And playType < 3 And 100 - ydLine > 10 And ruleOptType <> 0 Then
        'First Downs
        gameStatsTeam(D, 0) = gameStatsTeam(D, 0) + 1
    End If

    'Change possession back to where it should be
    'Off always kicks off to Def
    Call CHANGEPOSS

    GoTo 340


    '----- NOODLE -----


    3250 '
    ' *** SAFETY ***
    score(D, quarter) = score(D, quarter) + 2
    score(D, 0) = score(D, 0) + 2

    gameStatsTeam(D, 19) = gameStatsTeam(D, 19) + 1

    Call COMPILER

    3251 '
    If ruleOptPro < 2 And ruleOptType = 1 Then
        'Kickoff from 40 after Safety (Prior to 1974)
        ydLine = 40
    Else
        ydLine = 20
    End If

    currDown = 1
    ydsToFirst = 10

    playSegment = 5
    scoreX1 = 7

    Call SCOREUPD

    'Update possession for DEF (safety)
    If isOT = 1 And overtimeOpt >= 6 Then
        nbrPossOT(D) = 1
    End If

    'Change possession to place the *
    'next to the team on Def when we
    'update the screen / status board
    Call CHANGEPOSS

    I6 = 99

    U3 = 9
    GoSub 3900
    U3 = 0

    pbpString$ = "S A F E T Y  !!!"
    Call PBP

    Call SETSCREEN

    'Overtime - Evaluate Score / Change Poss
    If isOT = 1 And (overtimeOpt >= 1 And overtimeOpt <= 5) Or (overtimeOpt >= 6 And nbrPossOT(D) = 1) Then
        GoTo 3364
    Else
        'Change possession back to where it should be
        'Off always kicks off to Def
        Call CHANGEPOSS
        Call KickoffLine
        GoTo 340
    End If


    '----- NOODLE -----

    3303 '
    If CInt(ydsToFirst * 10) / 10 < .05 Then
        Call FirstDownAfterPlay
        '-- should we skip assessing penalties?
        '-- would it happen? if on OFF, then it
        '-- might actually be accepted?
    End If

    If currDown = 3 Then
        thirdDownAtt(P1) = thirdDownAtt(P1) + 1
    End If

    If currDown = 4 Then
        thirdDownFail(P1) = thirdDownFail(P1) + 1
    End If

    If currDown > 4 Then

        'Update possession for DEF
        If isOT = 1 And overtimeOpt >= 6 Then
            nbrPossOT(D) = 1
        End If

        Call TurnoverOnDowns
        GoSub 3900
        GoTo CheckPoss_OT

    End If

    RN0 = Int(Rnd(1) * 1000) + 1

    ' *** PERSONAL FOULS ***
    If RN0 <= 20 + teamRat_GAME(D, 0) Then

        pbpString$ = "*** PENALTY - "

        Call Rolld100(RN1)

        If (ruleOptType > 0 And ruleOptPro = 0) Or (ruleOptType = 0 And ruleOptColl = 0 And teamYears(impactTeam) <= 1957) Then
            'No facemask penalties before 1962! (1957 for NCAA)
            'It was simply illegal from 1955-1961 but not penalized
            'Unsportsmanlike conduct was more common
            If RN1 >= 80 Then RN1 = 75
        End If

        If (ruleOptType > 0 And ruleOptPro >= 5) Or (ruleOptType = 0 And ruleOptColl >= 9) Then
            'Starting 2008, all facemasks are flagrant
            If (RN1 >= 80 And RN1 <= 84) Then RN1 = 85
        End If

        Select Case RN1

            Case 1 To 4
                pbpString$ = pbpString$ + "OFF - ILLEGAL USE OF HANDS -"
                penaltyYds! = -15

            Case 5
                pbpString$ = pbpString$ + "OFF - FACEMASK -"
                penaltyYds! = -15

            Case 6
                pbpString$ = pbpString$ + "OFF - PERSONAL FOUL -"
                penaltyYds! = -15

            Case 7 To 8
                pbpString$ = pbpString$ + "OFF - TAUNTING -"
                penaltyYds! = -15

            Case 9
                pbpString$ = pbpString$ + "OFF - TRIPPING -"
                penaltyYds! = -15

            Case 10 To 16
                pbpString$ = pbpString$ + "OFF - UNNECESSARY ROUGH. -"
                penaltyYds! = -15

            Case 17 To 20
                pbpString$ = pbpString$ + "OFF - UNSPORTS. CONDUCT -"
                penaltyYds! = -15

            Case 21 To 22
                pbpString$ = pbpString$ + "OFF - CHOP BLOCK -"
                penaltyYds! = -15

            Case 23 To 24
                If (ruleOptType = 0 And teamYears(impactTeam) >= 1993) Or (ruleOptType = 1 And teamYears(impactTeam) >= 2005) Then
                    'Horse Collar tackle penalty introduced
                    'NCAA - 1993; NFL - 2005
                    pbpString$ = pbpString$ + "DEF - HORSE COLLAR -"
                Else
                    pbpString$ = pbpString$ + "DEF - PERSONAL FOUL -"
                End If

                penaltyYds! = 15

            Case 25 To 36
                pbpString$ = pbpString$ + "DEF - ILLEGAL USE HANDS -"
                penaltyYds! = 15

            Case 37 To 42
                pbpString$ = pbpString$ + "DEF - PERSONAL FOUL -"
                penaltyYds! = 15

            Case 43 To 44
                pbpString$ = pbpString$ + "DEF - TAUNTING -"
                penaltyYds! = 15

            Case 45 To 46
                pbpString$ = pbpString$ + "DEF - TRIPPING -"
                penaltyYds! = 15

            Case 47 To 74
                pbpString$ = pbpString$ + "DEF - UNNECESSARY ROUGHNESS -"
                penaltyYds! = 15

            Case 75 To 79
                pbpString$ = pbpString$ + "DEF - UNSPORTSMANLIKE CONDUCT -"
                penaltyYds! = 15

            Case 80 To 84
                pbpString$ = pbpString$ + "DEF - INCIDENTAL FACEMASK -"
                penaltyYds! = 15

            Case 85 To 100
                pbpString$ = pbpString$ + "DEF - FACEMASK -"
                penaltyYds! = 15

        End Select

        If penaltyYds! < 0 Then
            'Penalty on Offense
            pbpPenYds = 0 - penaltyYds!
        Else
            pbpPenYds = penaltyYds!
        End If

        If ydLine + penaltyYds! > 100 Then
            'Half the distance to the goal
            pbpString$ = pbpString$ + " 1/2 DISTANCE ***"
        Else
            pbpString$ = pbpString$ + Str$(pbpPenYds) + " YDS ***"
        End If

        If RN1 >= 23 Then

            'Can't end on defensive penalty
            If gameClock! <= 0 Then gameClock! = 1

            'Most defensive penalties result in 1st down
            'except Incidental Facemask | Unsportsmanlike Conduct
            If Not (RN1 >= 75 And RN1 >= 84) Then
                autoFirst = 1
            End If

        End If

        playSegment = 6

        GoTo HANDLEPENALTY

    Else

        GoSub 3900

        GoTo 700

    End If


    '----- NOODLE -----

    3309 '
    'This checks for the chance of penalties during a play
    'If there is no penalty on the play, then it resolves the
    'outcome of the play.
    'In short, all plays come here that aren't either
    'blocked / intercepted / penalized / etc.

    ' *** PENALTIES DURING PLAY ***
    RN4 = Int(Rnd(1) * 1000) + 1

    Select Case playType

        Case 1
            'Run Play
            Select Case RN4

                Case 1 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - OFF - ILLEGAL MOTION - 5 YDS ***"
                    penaltyYds! = -5

                Case 2 + teamRat_GAME(P1, 0) To 14 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - "

                    'Pro Rules before 1974 | College Rules before 1984
                    If (ruleOptType > 0 And ruleOptPro <= 1) Or (ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF - HOLDING - 15 YDS ***"
                        penaltyYds! = -15
                    Else
                        pbpString$ = pbpString$ + "OFF - HOLDING - 10 YDS ***"
                        penaltyYds! = -10
                    End If

                    isOffHolding = 1

                Case 15 + teamRat_GAME(P1, 0) To 16 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - OFF - TOO MANY MEN ON FIELD - 5 YDS ***"
                    penaltyYds! = -5

                Case 17 + teamRat_GAME(P1, 0) To 25 + teamRat_GAME(P1, 0)
                    If teamYears(impactTeam) >= 1940 Then
                        pbpString$ = "*** PENALTY - OFF - ILLEGAL BLOCK - 5 YDS ***"
                        penaltyYds! = -5
                    Else
                        pbpString$ = "*** PENALTY - OFF - ILLEGAL MOTION - 5 YDS ***"
                        penaltyYds! = -5
                    End If

                Case 26 + teamRat_GAME(P1, 0) To 27 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - DEF - TOO MANY MEN ON FIELD - 5 YDS ***"
                    penaltyYds! = 5

                Case 28 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0) To 30 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    If ruleOptType = 0 Then
                        'Penalty for NCAA is 10 yards / no auto-first
                        pbpString$ = "*** PENALTY - DEF - HOLDING - 10 YDS ***"
                        penaltyYds! = 10
                    Else
                        pbpString$ = "*** PENALTY - DEF - HOLDING - 5 YDS / 1ST D! ***"
                        autoFirst = 1
                        penaltyYds! = 5
                    End If

                Case Else

                    Call COMPILER

                    ydLine = ydLine + ydsGained

                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then
                        Call FumblePBP
                        GoTo 650
                    Else
                        'Evaluate outcome of play for penalties
                        GoTo 3303
                    End If

            End Select

        Case 2
            'Pass Play
            Select Case RN4

                Case 1 To 16 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - "

                    'Pro Rules before 1978 | College Rules before 1984
                    If (ruleOptType > 0 And ruleOptPro < 3) Or (ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF - HOLDING - 15 YDS *** "
                        penaltyYds! = -15
                        isOffHolding = 1
                    Else
                        pbpString$ = pbpString$ + "OFF - HOLDING - 10 YDS ***"
                        penaltyYds! = -10
                        isOffHolding = 1
                    End If

                Case 17 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - OFF - ILLEGAL MOTION - 5 YDS ***"
                    penaltyYds! = -5

                Case 18 + teamRat_GAME(P1, 0) To 21 + teamRat_GAME(P1, 0)
                    If teamYears(impactTeam) >= 1940 Then
                        pbpString$ = "*** PENALTY - OFF - ILLEGAL BLOCK - 15 YDS ***"
                        penaltyYds! = -15
                    Else
                        pbpString$ = "*** PENALTY - OFF - ILLEGAL MOTION - 5 YDS ***"
                        penaltyYds! = -5
                    End If

                Case 22 + teamRat_GAME(P1, 0)
                    pbpString$ = "*** PENALTY - OFF - TOO MANY MEN ON FIELD - 5 YDS ***"
                    penaltyYds! = -5

                Case (23 + teamRat_GAME(P1, 0)) To (23 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0) - 1)
                    pbpString$ = "*** PENALTY - UNKNOWN ***"
                    penaltyYds! = 0

                Case (23 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)) To (26 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0))
                    If teamYears(impactTeam) >= 1933 Then
                        pbpString$ = "*** PENALTY - DEF - ILLEGAL CONTACT - 5 YDS / 1ST D! ***"
                        autoFirst = 1
                        penaltyYds! = 5
                    Else
                        pbpString$ = "*** PENALTY - OFF - ILLEGAL MOTION - 5 YDS ***"
                        penaltyYds! = -5
                    End If

                Case 27 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = "*** PENALTY - DEF - TOO MANY MEN ON FIELD - 5 YDS ***"
                    penaltyYds! = 5

                Case 28 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0) To 30 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    If ruleOptType = 0 Then
                        'Penalty for NCAA is 10 yards / no auto-first
                        pbpString$ = "*** PENALTY - DEF - HOLDING - 10 YDS ***"
                        penaltyYds! = 10
                    Else
                        pbpString$ = "*** PENALTY - DEF - HOLDING - 5 YDS / 1ST DOWN! ***"
                        autoFirst = 1
                        penaltyYds! = 5
                    End If

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained

                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then
                        Call FumblePBP
                        GoTo 650
                    Else
                        'Evaluate outcome of play for penalties
                        GoTo 3303
                    End If

            End Select

    End Select

    ' *** HOLDING PENALTY ***
    'College 1991+
    'Randomly determine the "infraction point" for enforcing the penalty
    If ruleOptType = 0 And ruleOptColl >= 6 And isOffHolding = 1 Then

        If playType = 1 Then
            'Run Play - past scrimmage
            penaltyYds! = penaltyYds! + Int(Rnd(1) * 5) + 1
        End If

        If playType = 2 Then
            'Pass Play - behind scrimmage
            penaltyYds! = penaltyYds! - Int(Rnd(1) * 8) + 1
        End If

    End If

    'Reset for next penalty
    isOffHolding = 0

    HANDLEPENALTY:
    If autoPlay <> 1 Or playerMode <> 2 And whistle& And sndOpt = 1 Then
        _SndPlay whistle&
    End If

    penAccepted = 0

    'Choice for Penalty
    Call PenaltyChoice(penAccepted, autoFirst)

    If penAccepted = 1 Then

        'Accept the Penalty
        If ruleOptType = 0 And ruleOptColl = 0 Then
            'College Pre-1968
            playSegment = 1
        Else
            If playSegment <> 4 Then
                playSegment = 5
            End If
        End If

        pbpString$ = "PENALTY ACCEPTED"
        Call PBP

        If penaltyYds! > 0 Then
            3330 '
            'Penalty on Defensive
            Call PenaltyCharge(D, penaltyYds!)

            If ydsToFirst < .01 Or autoFirst = 1 Then
                3335 '
                Call FirstDownAfterPlay
                autoFirst = 0
            End If

        Else
            3326 '
            'Penalty on Offense
            Call PenaltyCharge(P1, penaltyYds!)

            If CInt(ydsToFirst * 10) / 10 < .05 Then
                Call FirstDownAfterPlay
            End If

        End If

        If loseDown = 1 Then
            currDown = currDown + 1
            loseDown = 0
            If currDown > 4 Then
                'Loss of down on 4th = turnover
                If isOT = 1 And overtimeOpt >= 6 Then
                    nbrPossOT(D) = 1
                End If
                Call TurnoverOnDowns
                GoSub 3900
                GoTo CheckPoss_OT

            End If
        End If

    Else

        pbpString$ = "PENALTY DECLINED"

        Call PBP
        Call COMPILER

        autoFirst = 0
        ydLine = ydLine + ydsGained

        ydsToFirst = ydsToFirst - ydsGained
        currDown = currDown + 1

        If currDown = 3 Then
            thirdDownAtt(P1) = thirdDownAtt(P1) + 1
        End If

        If playSegment < 4 Or playSegment > 5 Then
            playSegment = 1
        End If

        If CInt((ydsToFirst) * 10) / 10 < .05 Then

            Call FirstDownAfterPlay

        Else

            If currDown > 4 Then

                'Update possession for DEF
                If isOT = 1 And overtimeOpt >= 6 Then
                    nbrPossOT(D) = 1
                End If
                Call TurnoverOnDowns
                GoSub 3900
                GoTo CheckPoss_OT

            End If

        End If

    End If

    If playSegment = 4 Then
        'End of Pass Play / Down
        'Assess for personal fouls
        GoTo 3303
    Else
        GoSub 3900

        GoTo 700
    End If


    '--------------------
    ' End Of Quarter
    '--------------------

    3360 '
    gameClock! = 0
    Call StatusBoard

    quarter = quarter + 1

    If quarter = 3 Or quarter >= 5 Then
        'End of half = new possession
        U3 = 0
    End If

    'Crosswind 10-30% chance
    xwindChance = Int(Rnd(1) * 21) + 10

    If d100% <= xwindChance Then
        climate(0) = 2
    Else
        climate(0) = 1 - climate(0)
    End If

    gameOver = 0

    'quarter - 1 = previous quarter
    'We've already incremented it
    Select Case quarter - 1

        Case 1, 3
            'quarter = 2 / 4
            'End of 1st / 3rd Quarter
            gameClock! = 900

            Call StatusBoard
            pbpString$ = "END OF QUARTER"
            Call PBP

            GoTo 700

        Case 2
            'quarter = 2 / 4
            'Half Time
            gameClock! = 900

            Call StatusBoard
            pbpString$ = "HALF TIME"
            Call PBP

            winTeam = 1 - winTeam

            Call HALFSTART
            Call GetKeyWhenNoDelay
            halfTime = 1

            Do
                Color 15, 0
                Call BOXOPTIONS(0)

                'Reset option for next call
                If X <> 0 Then X = 0

                Color 15, 0
                Cls

                If Not (autoPlay = 1 And playerMode = 2) Then

                    Do
                        'Using Home Team's color for the box
                        Call TEAMBOX(1, 1)
                        pbpString$ = "DISPLAY STATS AGAIN? (Y/N)"
                        Locate 7, (80 - Len(pbpString$)) / 2
                        Print pbpString$
                        I$ = GetKeyPress$
                        Color 15, 0
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                End If

            Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

            GoTo 850

        Case Else
            'End of 4th Quarter / Overtime

            3364 '
            'Check overtime score
            gameOver = 0

            If isOT = 0 And quarter = 5 And score(0, 0) <> score(1, 0) Then

                'Regular Game, Not Tied
                gameOver = 1

            Else

                'Sudden Death, single quarter over
                If (overtimeOpt = 1 And quarter = 6 And gameClock! = 0) Then
                    gameOver = 1
                End If

                'Sudden Death, Score Not Tied
                If ((overtimeOpt = 1 Or overtimeOpt = 2) And score(0, 0) <> score(1, 0)) Then
                    gameOver = 1
                End If

                'NCAA | Modified Sudden Death (Reg): Both Teams Have Possession, Not Tied
                If ((overtimeOpt >= 3 And overtimeOpt <= 7) And score(0, 0) <> score(1, 0) And (nbrPossOT(0) = 1 And nbrPossOT(1) = 1 Or nbrPossOT(0) = 0 And nbrPossOT(1) = 0)) Then
                    gameOver = 1
                End If

                'Modified Sudden Death (Postseason): Both Teams Have Possession And At Least 1 Has TD Or Score
                If overtimeOpt <= 8 And score(0, 0) <> score(1, 0) And ((otTDs(P1) And nbrPossOT(0) = 1 And nbrPossOT(1) = 1) Or (otTDs(P1) = 0 And nbrPossOT(0) = 1 And nbrPossOT(1) = 1)) Then
                    gameOver = 1
                End If

            End If

            If gameOver = 1 Then

                'Game ends after quarter;
                If isOT = 0 Or overtimeOpt = 1 Then
                    'restore actual quarter #
                    quarter = quarter - 1
                End If

            Else

                Select Case overtimeOpt

                    Case 1
                        'Sudden Death 1 Quarter
                        'Overtime with quarters
                        isOT = 1
                        I$ = "Y"
                        gameClock! = 900

                    Case 2
                        'Sudden Death to Win
                        'Overtime with quarters
                        isOT = 1
                        I$ = "Y"

                        'We have to keep the game clock going
                        'It isn't perfect, but it works
                        'I've seen the clock down to 2 or 3 mins
                        'before getting reset
                        If gameClock! = 0 Then
                            gameClock! = 900
                        End If

                    Case 3 To 5
                        'College Rules
                        'Overtime with quarters
                        isOT = 1
                        I$ = "N"

                        'We won't print this but if it's a 0 value,
                        'we'll end up right back here.
                        gameClock! = 900

                        '1st and 10 from the 25
                        ydLine = 75
                        ydsToScore = 25

                        ydsToFirst = 10
                        currDown = 1

                        'If nobody has had the ball yet
                        If quarter = 5 And (nbrPossOT(0) = 0 And nbrPossOT(1) = 0) Then

                            timeouts(0) = 1
                            timeouts(1) = 1

                            Call CoinToss

                            If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

                                otChoice = 2

                            Else

                                Call OPTIONBOX(9)
                                Print gameTeams$(winTeam); " DO YOU ELECT TO:"
                                Locate 12, 14
                                Locate , 14: Print "1 - ATTACK"
                                Locate , 14: Print "2 - DEFEND"

                                Do
                                    I$ = GetKeyPress$
                                    otChoice = Val(I$)
                                Loop Until otChoice >= 1 And otChoice <= 2

                            End If

                            'If defend was chosen
                            If otChoice = 2 Then
                                'Opposite team is on OFF
                                P1 = 1 - winTeam
                            Else
                                '
                                P1 = winTeam
                            End If

                            D = 1 - P1

                        End If

                        'Overtime periods are defined by each team having possession
                        If nbrPossOT(0) = 1 And nbrPossOT(1) = 1 Then
                            quarter = quarter + 1
                        End If

                        If (nbrPossOT(0) = 0 And nbrPossOT(1) = 0) Or (nbrPossOT(0) = 1 And nbrPossOT(1) = 1) Then

                            Erase nbrPossOT
                            nbrPossOT(P1) = 1

                        ElseIf nbrPossOT(0) = 1 Then

                            nbrPossOT(1) = 1
                            P1 = 1
                            D = 0
                            If score(P1, 0) > score(D, 0) Then 3364

                        ElseIf nbrPossOT(1) = 1 Then

                            nbrPossOT(0) = 1
                            P1 = 0
                            D = 1
                            If score(P1, 0) > score(D, 0) Then 3364

                        End If

                        Call SETSCREEN

                        'College Overtime, 2019+
                        If (quarter >= 9 And overtimeOpt = 4) Or (quarter >= 7 And overtimeOpt = 5) Then
                            'No more drives from the 25
                            'Alternating 2-point conversion
                            GoTo PointsAfterTD
                        Else
                            GoTo 700
                        End If

                    Case 6, 7
                        'Sudden Death Modified - Regular Season

                        'Overtime with quarters
                        isOT = 1
                        I$ = "Y"

                        'If this is the start of overtime
                        If nbrPossOT(0) = 0 And nbrPossOT(1) = 0 Then
                            gameClock! = 900
                        End If

                        'If other team has had possession,
                        'but we (P1/Off) are ahead
                        If nbrPossOT(D) = 1 And score(P1, 0) > score(D, 0) Then
                            GoTo 3364
                        End If

                        If gameClock! = 0 Then
                            'End after 1 period
                            I$ = "N"
                        Else
                            'Keep on playing!
                            If nbrPossOT(0) = 1 Or nbrPossOT(1) = 1 Then
                                GoTo 700
                            End If
                        End If

                    Case 8
                        'Sudden Death Modified - Post Season

                        'Overtime with quarters
                        isOT = 1
                        I$ = "Y"

                        If gameClock! = 0 Then

                            'More periods!
                            gameClock! = 900

                        Else

                            'If opponent has had the ball and are ahead
                            'Then the game is over
                            If nbrPossOT(D) = 1 And (score(D, 0) > score(P, 0)) Then
                                GoTo 3364
                            End If

                            'If we have a TD
                            If otTDs(P1) = 1 Then

                                'If opp hasn't had the ball, but we
                                'just scored, they get a kickoff
                                If nbrPossOT(D) = 0 And score(P1, 0) <> score(D, 0) Then

                                    'They're likely going to get the ball
                                    nbrPossOT(D) = 1

                                    currDown = 1
                                    ydsToFirst = 10
                                    Call KickoffLine
                                    GoTo 340

                                End If

                            Else

                                'Turnover on Downs

                                'We have had the ball, but haven't scored a TD
                                'If opponent hasn't had the ball,
                                'they are about to get it
                                If nbrPossOT(P1) = 1 Then 'And nbrPossOT(D) = 0 Then
                                    nbrPossOT(D) = 1
                                    GoTo 700
                                End If

                            End If

                        End If

                End Select

                'Overtime with Quarters
                If UCase$(I$) = "Y" Then

                    If quarter = 5 Then

                        isOT = 1

                        Call HALFSTART

                        If overtimeOpt = 7 Then

                            '2 timeout for each team
                            timeouts(0) = 2
                            timeouts(1) = 2

                            'Set clock to 10 minutes, not 15
                            gameClock! = 600
                        Else
                            gameClock! = 900
                        End If

                        Call StatusBoard

                        Call CoinToss

                        P1 = winTeam
                        D = 1 - winTeam

                        Call KickoffChoices(0)

                        'Assign possession for receiving team
                        If overtimeOpt >= 1 Then
                            nbrPossOT(D) = 1
                        End If

                        Call KickoffLine
                        GoTo 340

                    Else

                        'Multiple Quarters
                        isOT = 1

                        'Sudden Death Postseason
                        'EVERY Quarter
                        If overtimeOpt = 2 Then
                            'Just switch sides of the field
                            winTeam = 1 - winTeam

                            GoTo 700

                        End If

                        'Mod SD Postseason - "3rd Quarter" OT
                        If overtimeOpt = 8 And (quarter Mod 4 = 3) Then

                            'Modified Sudden Death
                            Call CHANGEPOSS
                            Call HALFSTART
                            Call SETSCREEN
                            GoTo 340

                        End If

                        'NCAA - Alternating Possessions
                        'Each such alternate is an "OT" quarter
                        If overtimeOpt >= 3 And overtimeOpt <= 5 Then

                            timeouts(0) = 1
                            timeouts(1) = 1

                            GoTo 700

                        End If

                    End If

                Else

                    'Since this increases at end of quarter,
                    'we need to decrease
                    quarter = quarter - 1
                    gameOver = 1

                End If

            End If

    End Select

    'Check if game is over
    If gameOver = 1 Then

        gameClock! = 0
        Call StatusBoard

        pbpString$ = "END OF GAME"
        Call PBP

        Call GetKeyWhenNoDelay

    Else

        Call StatusBoard
        Call SetChainMarker

    End If

    Call TICKERUPDATE

    850 '
    Call TICKER

    If halfTime = 1 Then

        halfTime = 0

        If autoPlay <> 1 Then
            For I3 = 0 To 1:
                Call CHOOSEQB(I3)
            Next I3
        End If

        Randomize Timer

        Call StatusBoard

        GoTo 330

    Else

        If gameOver = 1 Then
            Call EndOfGame
            Exit Sub
        Else
            GoTo 700
        End If

    End If


    '----- NOODLE -----

    3552 '

    If defInts(D) > 0 Then
        Call QBTHROW
        Call FINDINT(defPlayer)
        pbpString$ = "PASS IS INTERCEPTED BY " + defTeam$
    Else
        pbpString$ = "PASS IS INTERCEPTED"
    End If

    scoreX0 = 0

    'Record INT's for Defense
    gameInts(D, defPlayer, 0) = gameInts(D, defPlayer, 0) + 1

    'Team Pass Att
    gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
    'Team Pass INT
    gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1

    If fleaFlicker <> 1 Then
        'QB Pass Att
        gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
        'QB Pass INT
        gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1
    End If

    'Update possession for DEF (interception)
    If isOT = 1 And overtimeOpt >= 6 Then
        nbrPossOT(D) = 1
    End If

    If endZone = 1 Then

        'Intercepted - Endzone / 2 Pt Conv
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        Call Turnover_PAT

        'Goes to a kickoff
        GoTo CheckPoss_2PtConv

    End If

    Call AddPossessionTime(5, 8)

    If fleaFlicker <> 0 Then
        fleaFlicker = 0
    Else
        If playCall = 12 Or playCall = 15 Then
            ydsGained = Int(Rnd(1) * 20) + 21
        End If
        If playCall <> 12 And playCall <> 15 Then
            ydsGained = Int(Rnd(1) * 10) + 1
        End If
        If playCall = 13 Then
            ydsGained = ydsGained - 5
        End If
        If playCall = 11 Or playCall = 14 Or playCall = 17 Or playCall = 16 Or playCall = 18 Then
            ydsGained = ydsGained + 10
        End If
    End If

    GoTo InterceptionReturn


    '----- NOODLE -----


    3900 '
    'Adjust possession time, check for special scenarios
    '(ie, hurry up, 2-minute warning, etc.)

    '--> Convert to a subroutine
    '--> takes a parameter regarding gameClock
    '--> then returns that so we can issue the following afterwards
    '       If gameClock! = 0 Then GoTo 3360
    twoMinWarn = 0

    If playSegment <> 8 Then

        'Pro Rules or College, 2024+
        If ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(impactTeam) >= 2024) Then

            If gameClock! > 120.01 And (quarter = 2 Or quarter >= 4) And Not (gameClock! > 159.99 And playSegment = 0) And Not (gameClock! > 147 And playSegment = 1) And Not (gameClock! > 142 And playSegment = 2) And Not (gameClock! > 137 And playSegment = 3) And Not (gameClock! > 126 And playSegment = 4) And Not (gameClock! > 128 And playSegment = 5) And Not (gameClock! > 130 And playSegment = 6) And Not (gameClock! > 132 And playSegment = 7) Then

                pbpString$ = "* * * T W O   M I N U T E   W A R N I N G * * *"
                Call PBP

                If gameClock! <= 126.01 And gameClock! >= 120.99 Then
                    gameClock! = gameClock! - 6
                    timePoss(P1) = timePoss(P1) + 6
                End If

                If gameClock! > 125.99 Then
                    timePoss(P1) = timePoss(P1) + (gameClock! - 120)
                    gameClock! = 120
                End If

                playSegment = 4
                twoMinWarn = 1

            End If

            Call DisplayClockDown

        End If

        If gameClock! <= 120 And (quarter = 2 Or quarter >= 4) Then

            If playSegment <= 4 Then

                Call CheckGameClock
                If gameClock! = 0 Then GoTo 3360
                choice = 0
                Call TwoMinMenu(choice)

                Select Case choice

                    Case 1:
                        pbpString$ = "HUDDLE"
                        Call PBP

                    Case 2:
                        If timeouts(P1) = 0 Then
                            'If no timeouts
                            pbpString$ = "HUDDLE"
                            Call PBP
                        Else
                            pbpString$ = "TIME OUT " + gameTeams$(P1) + " - THEY HAVE" + Str$(timeouts(P1) - 1) + " REMAINING"
                            Call PBP
                            timeouts(P1) = timeouts(P1) - 1
                            playSegment = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 3
                        If timeouts(D) = 0 Then
                            'If no timeouts
                            pbpString$ = "HUDDLE"
                            Call PBP
                        Else
                            pbpString$ = "TIME OUT " + gameTeams$(D) + " - THEY HAVE" + Str$(timeouts(D) - 1) + " REMAINING"
                            Call PBP
                            timeouts(D) = timeouts(D) - 1
                            playSegment = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 4
                        If playSegment = 0 Or playSegment = 1 Then
                            pbpString$ = "HURRY UP"
                            Call PBP
                            playSegment = playSegment + 2
                        End If

                    Case 5
                        pbpString$ = qbacks$(P1, qbNumber(P1)) + " IS TRYING TO GET THE TEAM LINED UP"
                        Call PBP

                        playSegment = 3

                        If teamYears(P1) < 1990 Then
                            'Throwaway pass
                            pbpString$ = "HE STEPS BACK AND THROWS IT OUT OF BOUNDS"
                            Call PBP
                        Else
                            'Spiking was legal in 1990
                            pbpString$ = "HE LINES UP QUICKLY AND SPIKES THE BALL"
                            Call PBP
                        End If

                        'Increase pass attempts (team + player)
                        gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                        gameStatsPlayer(P1, qbNumber(P1), 9) = gameStatsPlayer(P1, qbNumber(P1), 9) + 1

                        currDown = currDown + 1
                        If currDown = 3 Then
                            thirdDownAtt(P1) = thirdDownAtt(P1) + 1
                        End If

                        If teamYears(P1) < 1990 Then
                            Call AddPossessionTime(3, 4)
                        Else
                            Call AddPossessionTime(2, 3)
                        End If

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                End Select

            End If

        End If

        If playSegment = 0 Then

            Call AddPossessionTime(33, 38)

            If ruleOptType <> 0 Then
                'Pro Rules
                If gameClock! > 40 Then
                    gameClock! = gameClock! - 4
                    timePoss(P1) = timePoss(P1) + 4
                End If

                If gameClock! > 43 And teamYears(0) >= 1990 And teamYears(1) >= 1990 Then
                    gameClock! = gameClock! - 3
                    timePoss(P1) = timePoss(P1) + 3
                End If

                If gameClock! > 43 And teamYears(0) >= 1993 And teamYears(1) >= 1993 Then
                    gameClock! = gameClock! + 3
                    timePoss(P1) = timePoss(P1) - 3
                End If

            Else
                'College Rules
                If gameClock! > 43 Then

                    If teamYears(0) = 1956 And teamYears(1) = 1956 Then
                        gameClock! = gameClock! + 3
                        timePoss(P1) = timePoss(P1) - 3
                    End If

                    If teamYears(0) = 1957 And teamYears(1) = 1957 Then
                        gameClock! = gameClock! + 3
                        timePoss(P1) = timePoss(P1) - 3
                    End If

                End If

            End If

        Else
            'playSegment 1 to 3
            If playSegment >= 1 And playSegment <= 3 Then
                timeElapsed! = 27 - (playSegment * 5) + (Int(Rnd(1) * 5) + 1)
            Else
                'playSegment >= 4
                'Only subtract time if we're not AT the 2-min warning
                If twoMinWarn = 1 Then
                    timeElapsed! = 0
                Else
                    timeElapsed! = (2 * playSegment) - 5 + (Int(Rnd(1) * 3) + 1)
                End If
            End If

            gameClock! = gameClock! - timeElapsed!
            timePoss(P1) = timePoss(P1) + timeElapsed!

        End If

        Call CheckGameClock
        If gameClock! = 0 Then GoTo 3360

    End If

    Return


    '----- NOODLE -----


    3840 '
    'Blocked Punt
    gameStatsTeam(P1, 28) = gameStatsTeam(P1, 28) + 1
    pbpString$ = "KICK IS BLOCKED!!!"

    Call PBP

    Call AddPossessionTime(4, 7)

    ydsGained = Int(Rnd(1) * 30) * -1

    Call YARDAGE
    ydLine = 100 - ydLine - ydsGained

    If ydLine >= 109.949 Then

        pbpString$ = "THE BALL ROLLS OUT OF THE END ZONE!!"
        Call PBP

        'Safety!
        GoTo 3250

    End If

    If ydLine >= 100 Then
        'Ball in the end zone
        'Defense assumed to recover
        'Technically, the offense could recover
        'but that would also result in a safety
        scoreX1 = 6

        pbpString$ = gameTeams$(D) + " RECOVERS IT IN THE END ZONE!!"
        Call PBP

        Call CHANGEPOSS

        'Touchdown!
        GoTo 3200

    End If

    If d100% > 60 Then

        '60% chance of it being a touchdown regardless
        'of the original gain that was calculated
        scoreX1 = 6

        Call CHANGEPOSS

        ydsGained = 100 - ydLine
        ydsToScore = ydsGained
        Call ShowChainMarker
        Call StatusBoard

        startYds = 100 - ydLine
        endYds = startYds - ydsGained
        Call MOVEBALL

        pbpString$ = gameTeams$(P1) + " TAKES THE BALL INTO THE END ZONE!!"
        Call PBP

        'Touchdown!
        GoTo 3200

    Else

        'Update possession for DEF (blocked punt)
        If isOT = 1 And overtimeOpt >= 6 Then
            nbrPossOT(D) = 1
        End If

        Call CHANGEPOSS

        currDown = 1
        ydsToFirst = 10
        If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        Call DELAY

        If gameClock! = 0 Then GoTo 3360

    End If

    GoTo CheckPoss_OT


    '----- NOODLE -----


    'Punt Fake - Pass Intercepted

    3876 '
    scoreX0 = 0

    'Team Pass Att
    gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
    'Team Pass INT
    gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1

    'QB Pass Att
    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
    'QB Pass INT
    gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1

    'Update possession for DEF (interception)
    If isOT = 1 And overtimeOpt >= 6 Then
        nbrPossOT(D) = 1
    End If

    Call AddPossessionTime(5, 8)

    GoTo InterceptionReturn

    '----- NOODLE -----

    CheckPoss_2PtConv:
    'NCAA Alternating "2-pt" Conversion | SD Mod PostSeason
    '2-pt conversion ends possession
    'In college, if other team has had the ball then we check the score
    'If (isOT = 1 And (quarter < 7 And overtimeOpt = 4) Or (quarter < 6 And overtimeOpt >= 3 And overtimeOpt <= 5)) Or (quarter >= 7 And overtimeOpt = 4) Or (quarter >= 6 And overtimeOpt = 5) Or (isOT = 1 And overtimeOpt = 8) Then
    If (isOT = 1 And nbrPossOT(D) = 1 And overtimeOpt >= 3 And overtimeOpt <= 5) Or (quarter >= 7 And overtimeOpt = 4) Or (quarter >= 6 And overtimeOpt = 5) Or (isOT = 1 And overtimeOpt = 8) Then
        GoTo 3364
    Else
        Call KickoffLine
        GoTo 340
    End If

    '----- NOODLE -----

    InterceptionReturn:
    'Determine yardage from an interception,
    'output the corresponding PBP and finally
    'track all associated stats

    ydsGained = Int(Rnd(1) * 10) + 1

    Call YARDAGE

    'Spot of the interception
    intYdLine! = CInt((ydsGained + ydLine) * 10) / 10

    If intYdLine! >= 99.9499 Then

        'Interception in the end zone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP
        Call CHANGEPOSS
        Call Touchback(0)

    Else

        newYdLine! = ydsGained + ydLine
        If newYdLine! <= 1 Then
            pbpString$ = pbpString$ + " AT THE 1 YARD LINE"
        Else
            If newYdLine! > 49.99 Then
                pbpString$ = pbpString$ + " AT THE" + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
            End If
            If newYdLine! < 49.99 Then
                pbpString$ = pbpString$ + " AT THE" + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
            End If
        End If

        Call PBP

        Call CHANGEPOSS

        currDown = 1
        ydLine = 100 - ydsGained - ydLine

        ydsToScore = 100 - ydLine
        Call ShowChainMarker
        Call StatusBoard
        Call YARDLINE

        '8% chance of longer RETURN
        Call Rolld100(RN1)
        If ruleOptType = 0 Then
            gainAdj = 5
            tdChance = 13
        Else
            gainAdj = 0
            tdChance = 11
        End If

        ydsToEndZone = 100 - ydLine

        Select Case RN1

            Case 1 To 35 - gainAdj
                'No Return
                ydsGained = 0

            Case 35 - gainAdj To 50 - gainAdj
                '1 - 9 yards
                ydsGained = Int(Rnd(1) * 9) + 1

            Case 50 - gainAdj To 75
                '10 - 19 yards
                ydsGained = Int(Rnd(1) * 9) + 10

            Case 75 To 75 + tdChance
                'Touchdown
                ydsGained = ydsToEndZone

            Case 75 + tdChance To 100
                '20+ yards
                'Given that this is 12% of INT returns,
                'and about the same % of returns make
                'it inside the 10, this will not result
                'in enough making it there.
                inside10 = ydsToEndZone - 10 - (Int(Rnd(1) * 6) + 5)
                ydsGained = Int(Rnd(1) * inside10) + 20

        End Select

        Call YARDAGE

        scoreX1 = 5

        startYds = 100 - ydLine
        endYds = startYds - ydsGained
        Call MOVEBALL

        pbpString$ = "RETURNED FOR"

        If ydsGained + ydLine > 99.99 Then

            pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"

            'Record INT Yds, TDs
            gameInts(P1, defPlayer, 1) = gameInts(P1, defPlayer, 1) + 100 - ydLine
            gameInts(P1, defPlayer, 2) = gameInts(P1, defPlayer, 2) + 1

            If 100 - ydLine > gameLongInt(P1, defPlayer) Then
                gameLongIntTD$(P1, defPlayer) = "t"
                gameLongInt(P1, defPlayer) = 100 - ydLine
            End If

            Call PBP

            'Touchdown!
            GoTo 3200

        Else

            returnYds! = CInt((ydsGained) * 10) / 10

            If returnYds! >= 0 Then
                pbpString$ = pbpString$ + Str$(Int(returnYds!)) + " YARDS"
            Else
                pbpString$ = pbpString$ + " " + Str$(Int(returnYds!)) + " YARDS"
            End If
            Call PBP

            playSegment = 5
            ydLine = ydLine + ydsGained

            ydsToFirst = 10

            If ydLine >= 90 Then
                ydsToFirst = 100 - ydLine
            End If

            'Record INT Yds
            gameInts(P1, defPlayer, 1) = gameInts(P1, defPlayer, 1) + ydsGained

            If ydsGained > gameLongInt(P1, defPlayer) Then
                gameLongIntTD$(P1, defPlayer) = ""
                gameLongInt(P1, defPlayer) = ydsGained
            End If

            fumbPlayType = 5
            Call CheckFumble_1
            If ballFumbled = 1 Then
                Call FumblePBP
                GoTo 650
            End If

            RN1 = Int(Rnd(1) * 1000) + 1

            'Interception return
            If RN1 > 950 - teamRat_GAME(P1, 0) Then

                pbpString$ = "*** PENALTY - ILLEGAL BLOCK - 10 YDS ***"
                penaltyYds! = 10
                'Print #11, "(INT RET) "; pbpString$
                GoTo HANDLEPENALTY

            End If

            Call DELAY

        End If

    End If 'Done checking for endzone

    GoSub 3900
    GoTo CheckPoss_OT

End Sub

'INCLUDE: 'GameRoutines.bm'

'----------------------------------------
'            INFO Subroutine
'----------------------------------------
'This subroutine displays a simple header
'and footer for the game options menu.
Sub INFO ()

    Cls

    Color , 1
    Locate 1, 1
    Print Tab(80); " ";

    Locate , 33
    Color 14
    Print "GAME OPTIONS MENU"

    Locate LAST_ROW, 1
    Print Tab(80); " ";
    Locate , 1
    Print "    LANCE HAFFNER / JASON LEONARD  1986-2025"; Tab(70); Left$(Time$, 5);
    Locate 3, 1
    Color , 0

End Sub


'----------------------------------------
'           InitVar Subroutine
'----------------------------------------
' Initializes variables for the game
' Does not process any that will be read
' in from file or assigned elsewhere
Sub InitVar ()

    Shared scheduleFile$
    Shared actualAttendance&, avgAttendance&

    Shared recLongFGTxt$(), recLongIntTxt$(), recLongKRTxt$()
    Shared recLongPassTxt$(), recLongPRTxt$()
    Shared recLongRecTxt$(), recLongRushTxt$()

    Shared oppName$(), locIndicator$(), gameSite$()
    Shared teamIndicator$()
    'Shared eventSettings()
    Shared oppScore(), teamScore(), powerRating()

    Shared gameAttendance&()

    teamIndicator$(0) = "VISITING TEAM"
    teamIndicator$(1) = "HOME TEAM"

    direction$(0) = "RIGHT"
    direction$(1) = "LEFT"

    gadget$(1) = "FLEA FLICKER"
    gadget$(2) = "REVERSE"
    gadget$(3) = "HB PASS"

    playSelect$(0) = "RUN"
    playSelect$(1) = "PASS"
    playSelect$(3) = "FIELD GOAL ATTEMPT"
    playSelect$(2) = "PUNT"
    playSelect$(4) = "SCOUTING REPORT"
    playSelect$(5) = "CHANGE/SELECT QB"
    playSelect$(6) = "SHOW GAME STATS"
    playSelect$(7) = "QB KNEEL DOWN"
    playSelect$(8) = "TEN MINUTE TICKER "
    playSelect$(9) = "CHANGE/SELECT KICKER"

    downDesc$(0) = "FIRST"
    downDesc$(1) = downDesc$(0)
    downDesc$(2) = "SECOND"
    downDesc$(3) = "THIRD"
    downDesc$(4) = "FOURTH"

    totalSTPenalties = 0

    '-- For future releases, let's not initialize
    '-- anything that WILL get assigned by the interface
    autoFirst = 0
    autoPlay = 0
    ballFumbled = 0
    catchPctSuccess = 0
    catchYds = 0
    checkFumbRev = 0
    chosenPlay = 0
    compTeam = 0
    coverage = 0
    currDown = 0
    dLine = 1
    endAllGames = 0
    endYds = 0
    endZone = 0
    expCompPct = 0
    ffPctSuccess = 0
    fgAttYds = 0
    firstDownClockOpt = 0
    fleaFlicker = 0
    fumbPlayType = 0
    gameClock! = 0
    gameLoc = 0
    gameOver = 0
    gameOver = 0
    goalLnYdAdj = 0
    goalPostAdj = 0
    halfTime = 0
    halfbackPass = 0
    indRushPct = 0
    intChance = 0
    isOT = 0
    kickYL! = 0
    last2PlayType = 2
    last3PlayType = 2
    lastPlayType = 2 ' Special teams (kickoff)
    lastPlayYards = 0
    nbrLines = 1
    nbrScores = 0
    overtimeOpt = 0
    overuseAdj = 0
    pbpDelay! = 0
    periodNbr = 0
    playCall = 0
    playSegment = 0
    playType = 0
    playerMode = 0
    playerOpt = 0
    puntRetNbr = 0
    qbTakeKnee = 0
    qback = 0
    quarter = 0
    ruleOptColl = 0
    ruleOptPro = 0
    ruleOptType = 0
    rushYds = 0
    scoreX0 = 0
    scoreX1 = 0
    sndOpt = 1
    startYds = 0
    tickerGames = 0
    timeElapsed! = 0
    winTeam = 0
    ydLine = 0
    ydLineTeam = 0
    ydsGained = 0
    ydsToFirst = 0
    ydsToScore = 0

    'Set this to 1 for a black/white experience
    BW = 0

    D = 0
    P1 = 0
    W5 = 0
    XD = 0
    XE = 0
    YF = 0

    actualAttendance& = 0
    avgAttendance& = 0

    defTeam$ = ""
    gameStadium$ = ""
    markers$ = ""
    pbpString$ = ""
    targetWRName$ = ""
    YN$ = ""

    'These below are for reading in game data
    'Erase defInts$, defSacks$, gameMascots$
    'Erase kicker$, kickRet$
    'Erase pret$, punter$, qbacks$, rbacks$
    'Erase wdRec$
    Erase activeKicker, climate
    Erase hasRunRBPass, hasRunRev, indRushPct
    Erase kickerFGA, kickerFGPct, kickerIdx, kickerPATPct
    Erase krNumRet, krYdsPerRet, leagRat_GAME
    Erase otTDs
    Erase playerInts, playerSacks, prNumRet, prYdsPerRet
    Erase qbArmRat, qbCompPct, qbNumAtt, qbPctInt
    Erase rbNumRec, rbRushContrib, rbRushAvg, rbYdsPerC
    Erase teamInts, teamSacks ', teamRat_GAME
    Erase wrNumRec, wrYdsPerC, ydsPerComp, ydsPerPunt

    Erase defInts, defSacks
    'Erase eventSettings
    Erase playerFGA, playerFGM
    Erase sackStatsOpp, gameStatsTeam, gameStatsPlayer
    Erase K1, K3
    Erase gameFGA!, gameFGM!
    Erase gameInts, hasRunFF, playerInts
    Erase gameLongFGAtt, gameLongInt, gameLongKRYds
    Erase gameLongPassYds, gameLongPRYds
    Erase gameLongRec, gameLongRunYds
    Erase gameTeamStat!, gameOppStat!, gamePlayerStat!

    Erase nbrPossOT
    Erase oppScore, powerRating, teamScore
    Erase qbNumber, qbMobility

    Erase passCovAdj, ST%, SX%
    Erase qbRushIdx
    Erase recLongRec!, recLongFG!, recLongInt!, recLongKR!, recLongPass!, recLongRush!
    Erase recLongPR!

    Erase sackStatsTeam, score, scoreQuarters, scoreTimes
    Erase schedGame, schedOptions
    Erase teamIdx_GAME, timeouts, timePoss
    Erase teamYears, thirdDownAtt, thirdDownFail
    Erase totalInts, totalSacks
    Erase useRandomQB, XD

    Erase diskIDs$, gameTeams$
    Erase gameSite$
    Erase gameLongIntTD$, gameLongKRTD$
    Erase gameLongPRTD$, gameLongPassTD$
    Erase gameLongRecTD$, gameLongRunTD$
    Erase locIndicator$, oppName$
    Erase SX$

    Erase recLongFGTxt$, recLongIntTxt$, recLongKRTxt$
    Erase recLongRecTxt$, recLongRushTxt$
    Erase recLongPassTxt$, recLongPRTxt$

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP (validTeam)

    validTeam = 0

    For tgtTeam = 0 To 1

        tgtId = schedOptions(tgtTeam)
        tgtYear$ = RTrim$(diskIDs$(tgtTeam))

        Call ReadGameTeam(tgtYear$, gameTeams$(tgtTeam), tgtId, tgtTeam, validTeam)

        teamIdx_GAME(tgtTeam) = schedOptions(tgtTeam)

        If tgtId = 0 Then
            validTeam = 0
        End If

    Next tgtTeam

    If validTeam = 1 Then

        'Assign settings from schedule / etc.
        gameLoc = schedOptions(2)
        ruleOptType = schedOptions(3)
        pbpDelay! = schedOptions(4) / 10
        ruleOptColl = schedOptions(7)
        playerOpt = schedOptions(10)
        ruleOptPro = schedOptions(11)
        tickerStart = schedOptions(12)
        overtimeOpt = schedOptions(14)

        'Pro Rules, pre-1974
        If ruleOptType > 0 And ruleOptPro < 1 Then
            'adjust kicks based on goal posts
            'placed at front of endzone
            goalPostAdj = -10
        End If

        'WFL rules
        If ruleOptType = 4 Then
            goalPostAdj = 0
        End If

    End If

End Sub

'----------------------------------------
'       LetterToNumber Subroutine
'----------------------------------------
'This subroutine converts an alphabetic
'character to a numeric equivalent,
'where A = 10, B = 11, etc.
'This is useful for long menus that have
'more than 10 options (0 - 9) and a single
'key is desired for a choice.
Sub LetterToNumber (promptVal$, J)

    char$ = UCase$(promptVal$)
    code% = Asc(char$)

    ' Digits 0-9
    If code% >= 48 And code% <= 57 Then
        J = code% - 48
        ' Letters A-Z
    ElseIf code% >= 65 And code% <= 90 Then
        ' A=10, B=11, ..., M=22
        J = code% - 55
    Else
        ' default / invalid input
        J = 0
    End If

End Sub


'----------------------------------------
'           TEAMBOX Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 6 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub TEAMBOX (boxHeight, teamIdx)

    Locate 6, 8
    Color teamRat_GAME(teamIdx, 2), teamRat_GAME(teamIdx, 1)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 7 + boxHeight, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 7, 11

End Sub


'----------------------------------------
'         FRAMEOPTION Subroutine
'----------------------------------------
'This subroutine draws an ASCII box using
'team colors. The box starts at row 8 and
'is approx. 60 char's wide. When done, the
'cursor is set at the first printable row
'and column inside the box.
Sub FRAMEOPTION (boxHeight, teamIdx)

    PCopy 0, 1
    Screen , , 1, 1

    Locate 8, 8
    Color teamRat_GAME(teamIdx, 2), teamRat_GAME(teamIdx, 1)

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To boxHeight
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + boxHeight, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 9, 12

End Sub


'----------------------------------------
'           Rolld100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Sub Rolld100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub


'----------------------------------------
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Function d100% ()

    d100% = Int(Rnd(1) * 100) + 1

End Function


'----------------------------------------
'            DELAY Subroutine
'----------------------------------------
'This subroutine simply serves as a place-
'holder for the command to initiate the
'pbp delay.
Sub DELAY ()

    _Delay pbpDelay!

End Sub


'----------------------------------------
'            CoinToss Subroutine
'----------------------------------------
' Literally handles only 1 thing
' winTeam = 0 | 1 (Visitor or Home)
Sub CoinToss

    winTeam = Int(Rnd(1) * 2)

    If d100% <= 50 Then
        pbpString$ = gameTeams$(winTeam) + " WON THE COIN TOSS"
    Else
        pbpString$ = gameMascots$(winTeam) + " WON THE COIN TOSS"
    End If

    Call PBP

End Sub


'----------------------------------------
'             PBP Subroutine
'----------------------------------------
'This is the classic Haffner Play-by-play
'subroutine. Given the target text, it will
'place it in the correct spot in the pbp
'text window. No text is placed if the
'delay is 0.
Sub PBP ()

    M$ = InKey$

    Select Case UCase$(M$)

        Case "B"
            Call BOXOPTIONS(0)
            Call Scoreboard

        Case "O"
            Call OPTIONS

        Case "T"
            Call TICKER
            Call Scoreboard

        Case Chr$(17)
            Cls
            End

    End Select

    If pbpDelay! > 0 Then

        If nbrLines <= MAX_PBP_LINES Then
            pbpText$(nbrLines) = pbpString$
        Else
            For I = 1 To MAX_PBP_LINES - 1
                pbpText$(I) = pbpText$(I + 1)
            Next
            pbpText$(MAX_PBP_LINES) = pbpString$
        End If

        nbrLines = nbrLines + 1

        Call PLACEPBP
        Call DELAY

    End If

End Sub


'----------------------------------------
'           WINDEX Subroutine
'----------------------------------------
'This subroutine cleans the "window" used
'for the play-by-play text.
Sub WINDEX ()

    pbpRow = 25

    Color 14 - 7 * BW, 1 - BW

    For I = pbpRow To (pbpRow + MAX_PBP_LINES - 1)
        Locate I, 3
        Call BLANKLINE
    Next I

    Locate pbpRow, 3

End Sub


'----------------------------------------
'       BLANKLINE Subroutine
'----------------------------------------
'This subroutine literally prints blank
'spaces to clear a line of the PBP box.
Sub BLANKLINE ()

    Print String$(74, 46);

End Sub


'----------------------------------------
'          CLEARPBP Subroutine
'----------------------------------------
'This subroutine clears all the strings
'used for each line of PBP display
Sub CLEARPBP ()

    If pbpDelay! > 0 And pbpOpt <> 1 Then
        nbrLines = 1
        Erase pbpText$
    End If

End Sub


'----------------------------------------
'           TICKER Subroutine
'----------------------------------------
'This subroutine displays the score of all
'current games being tracked by the ticker.
'For each game it will find the score that
'matches the current game time / period.
Sub TICKER ()

    If Not (autoPlay = 1 And playerMode = 2) Then

        Color 14, 0
        Cls

        If halfTime = 1 Then

            Select Case RN0

                Case 1 To 12:
                    'ESPN
                    halftimeString$ = "ALLSTATE HALFTIME REPORT"

                Case 13 To 25:
                    'NBC
                    halftimeString$ = "PROGRESSIVE HALFTIME REPORT"

                Case 26 To 37:
                    'Amazon
                    halftimeString$ = "TOYOTA HALFTIME REPORT"

                Case 38 To 50:
                    'ABC
                    halftimeString$ = "USAA HALFTIME REPORT"

                Case 51 To 62:
                    'CBS
                    halftimeString$ = "STATE FARM HALFTIME REPORT"
                    
                Case 63 To 75
                    'Fox
                    halftimeString$ = "VERIZON HALFTIME REPORT"
                                
                Case 76 To 87
                    'CBS / College
                    halftimeString$ = "GEICO HALFTIME REPORT"
                
                Case 88 To 100
                    'ESPN / College
                    halftimeString$ = "MERCEDES BENZ HALFTIME REPORT"

            End Select

        Else

            If gameOver = 1 Then

                Select Case RN0

                    Case 1 To 25
                        'ABC
                        halftimeString$ = "CHEVROLET FINAL SCOREBOARD"

                    Case 26 To 50:
                        'Fox / CBS
                        halftimeString$ = "STATE FARM FINAL SCOREBOARD"

                    Case 51 To 75:
                        'NBC
                        halftimeString$ = "FEDERAL EXPRESS FINAL SCOREBOARD"

                    Case 76 To 100:
                        'ESPN
                        halftimeString$ = "ESPN SPORTSCENTER FINAL SCOREBOARD"

                End Select

            Else

                If halfTime = 0 Then
                    halftimeString$ = "LANCE HAFFNER GAMES SCOREBOARD"
                End If

            End If

        End If

        Locate , ((80 - Len(halftimeString$)) / 2)
        Print halftimeString$

        Color 15

        If tickerGames = 0 Then

            Locate 10, 28: Print "NO OTHER GAMES SCHEDULED"

        Else

            If quarter = 1 Then

                Select Case gameClock!

                    Case 600 To 899
                        timeInterval = 1

                    Case 300 To 599
                        timeInterval = 2

                    Case Is < 300
                        timeInterval = 3

                End Select

            End If

            If quarter = 2 Then

                Select Case gameClock!

                    Case 600 To 899
                        timeInterval = 4

                    Case 300 To 599
                        timeInterval = 5

                    Case Is < 300
                        timeInterval = 6

                End Select

            End If

            If quarter = 3 Then

                Select Case gameClock!

                    Case 600 To 899
                        timeInterval = 9

                    Case 300 To 599
                        timeInterval = 10

                    Case Is < 300
                        timeInterval = 11

                End Select

            End If

            If quarter = 4 Then

                Select Case gameClock!

                    Case 600 To 899
                        timeInterval = 12

                    Case 300 To 599
                        timeInterval = 13

                    Case Is < 300
                        timeInterval = 14

                End Select

            End If

            If quarter = 5 Then

                Select Case gameClock!

                    Case 600 To 899
                        timeInterval = 15

                    Case 300 To 599
                        timeInterval = 16

                    Case Is < 300
                        timeInterval = 17

                End Select

            End If

            If halfTime = 1 Then
                timeInterval = 8
            End If

            If Left$(SX$(1, 0), 3) <> "XXX" Then
                Locate 2, 1
                tickerX = 1
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(1, 0), 13);
                Locate , 15
                Print Using "## "; SX%(1, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(1, 1), 13);
                Locate , 15: Print Using "## #"; SX%(1, 1, tickInterval)
            End If

            If Left$(SX$(2, 0), 3) <> "XXX" Then
                Locate 5, 1
                tickerX = 2
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(2, 0), 13);
                Locate , 15: Print Using "## "; SX%(2, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(2, 1), 13);
                Locate , 15: Print Using "## #"; SX%(2, 1, tickInterval)
            End If

            If Left$(SX$(3, 0), 3) <> "XXX" Then
                Locate 8, 1
                tickerX = 3
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(3, 0), 13);
                Locate , 15: Print Using "## "; SX%(3, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(3, 1), 13);
                Locate , 15: Print Using "## #"; SX%(3, 1, tickInterval)
            End If

            If Left$(SX$(4, 0), 3) <> "XXX" Then
                Locate 11, 1
                tickerX = 4
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(4, 0), 13);
                Locate , 15: Print Using "## "; SX%(4, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(4, 1), 13);
                Locate , 15: Print Using "## #"; SX%(4, 1, tickInterval)
            End If

            If Left$(SX$(5, 0), 3) <> "XXX" Then
                Locate 14, 1
                tickerX = 5
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(5, 0), 13);
                Locate , 15: Print Using "## "; SX%(5, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(5, 1), 13);
                Locate , 15: Print Using "## #"; SX%(5, 1, tickInterval)
            End If

            If Left$(SX$(6, 0), 3) <> "XXX" Then
                Locate 17, 1
                tickerX = 6
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(6, 0), 13);
                Locate , 15: Print Using "## "; SX%(6, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(6, 1), 13);
                Locate , 15: Print Using "## #"; SX%(6, 1, tickInterval)
            End If

            If Left$(SX$(7, 0), 3) <> "XXX" Then
                Locate 20, 1
                tickerX = 7
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(7, 0), 13);
                Locate , 15: Print Using "## "; SX%(7, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(7, 1), 13);
                Locate , 15: Print Using "## #"; SX%(7, 1, tickInterval)
            End If

            If Left$(SX$(8, 0), 3) <> "XXX" Then
                Locate 23, 1
                tickerX = 8
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(8, 0), 13);
                Locate , 15: Print Using "## "; SX%(8, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(8, 1), 13);
                Locate , 15: Print Using "## #"; SX%(8, 1, tickInterval);
            End If

            If Left$(SX$(9, 0), 3) <> "XXX" Then
                Locate 2, 21
                tickerX = 9
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(9, 0), 13);
                Locate , 35: Print Using "## "; SX%(9, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(9, 1), 13);
                Locate , 35: Print Using "## #"; SX%(9, 1, tickInterval)
            End If

            If Left$(SX$(10, 0), 3) <> "XXX" Then
                Locate 5, 21
                tickerX = 10
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(10, 0), 13);
                Locate , 35: Print Using "## "; SX%(10, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(10, 1), 13);
                Locate , 35: Print Using "## #"; SX%(10, 1, tickInterval)
            End If

            If Left$(SX$(11, 0), 3) <> "XXX" Then
                Locate 8, 21
                tickerX = 11
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(11, 0), 13);
                Locate , 35: Print Using "## "; SX%(11, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(11, 1), 13);
                Locate , 35: Print Using "## #"; SX%(11, 1, tickInterval)
            End If

            If Left$(SX$(12, 0), 3) <> "XXX" Then
                Locate 11, 21
                tickerX = 12
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(12, 0), 13);
                Locate , 35: Print Using "## "; SX%(12, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(12, 1), 13);
                Locate , 35: Print Using "## #"; SX%(12, 1, tickInterval)
            End If

            If Left$(SX$(13, 0), 3) <> "XXX" Then
                Locate 14, 21
                tickerX = 13
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(13, 0), 13);
                Locate , 35: Print Using "## "; SX%(13, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(13, 1), 13);
                Locate , 35: Print Using "## #"; SX%(13, 1, tickInterval)
            End If

            If Left$(SX$(14, 0), 3) <> "XXX" Then
                Locate 17, 21
                tickerX = 14
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(14, 0), 13);
                Locate , 35: Print Using "## "; SX%(14, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(14, 1), 13);
                Locate , 35: Print Using "## #"; SX%(14, 1, tickInterval)
            End If

            If Left$(SX$(15, 0), 3) <> "XXX" Then
                Locate 20, 21
                tickerX = 15
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(15, 0), 13);
                Locate , 35: Print Using "## "; SX%(15, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(15, 1), 13);
                Locate , 35: Print Using "## #"; SX%(15, 1, tickInterval)
            End If

            If Left$(SX$(16, 0), 3) <> "XXX" Then
                Locate 23, 21
                tickerX = 16
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(16, 0), 13);
                Locate , 35: Print Using "## "; SX%(16, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(16, 1), 13);
                Locate , 35: Print Using "## #"; SX%(16, 1, tickInterval);
            End If

            If Left$(SX$(17, 0), 3) <> "XXX" Then
                Locate 2, 41
                tickerX = 17
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(17, 0), 13);
                Locate , 55: Print Using "## "; SX%(17, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(17, 1), 13);
                Locate , 55: Print Using "## #"; SX%(17, 1, tickInterval)
            End If

            If Left$(SX$(18, 0), 3) <> "XXX" Then
                Locate 5, 41
                tickerX = 18
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(18, 0), 13);
                Locate , 55: Print Using "## "; SX%(18, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(18, 1), 13);
                Locate , 55: Print Using "## #"; SX%(18, 1, tickInterval)
            End If

            If Left$(SX$(19, 0), 3) <> "XXX" Then
                Locate 8, 41
                tickerX = 19
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(19, 0), 13);
                Locate , 55: Print Using "## "; SX%(19, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(19, 1), 13);
                Locate , 55: Print Using "## #"; SX%(19, 1, tickInterval)
            End If

            If Left$(SX$(20, 0), 3) <> "XXX" Then
                Locate 11, 41
                tickerX = 20
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(20, 0), 13);
                Locate , 55: Print Using "## "; SX%(20, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(20, 1), 13);
                Locate , 55: Print Using "## #"; SX%(20, 1, tickInterval)
            End If

            If Left$(SX$(21, 0), 3) <> "XXX" Then
                Locate 14, 41
                tickerX = 21
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(21, 0), 13);
                Locate , 55: Print Using "## "; SX%(21, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(21, 1), 13);
                Locate , 55: Print Using "## #"; SX%(21, 1, tickInterval)
            End If

            If Left$(SX$(22, 0), 3) <> "XXX" Then
                Locate 17, 41
                tickerX = 22
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(22, 0), 13);
                Locate , 55: Print Using "## "; SX%(22, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(22, 1), 13);
                Locate , 55: Print Using "## #"; SX%(22, 1, tickInterval)
            End If

            If Left$(SX$(23, 0), 3) <> "XXX" Then
                Locate 20, 41
                tickerX = 23
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(23, 0), 13);
                Locate , 55: Print Using "## "; SX%(23, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(23, 1), 13);
                Locate , 55: Print Using "## #"; SX%(23, 1, tickInterval)
            End If

            If Left$(SX$(24, 0), 3) <> "XXX" Then
                Locate 23, 41
                tickerX = 24
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(24, 0), 13);
                Locate , 55: Print Using "## "; SX%(24, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(24, 1), 13);
                Locate , 55: Print Using "## #"; SX%(24, 1, tickInterval);
            End If

            If Left$(SX$(25, 0), 3) <> "XXX" Then
                Locate 2, 61
                tickerX = 25
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(25, 0), 13);
                Locate , 75: Print Using "## "; SX%(25, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(25, 1), 13);
                Locate , 75: Print Using "## #"; SX%(25, 1, tickInterval)
            End If

            If Left$(SX$(26, 0), 3) <> "XXX" Then
                Locate 5, 61
                tickerX = 26
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(26, 0), 13);
                Locate , 75: Print Using "## "; SX%(26, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(26, 1), 13);
                Locate , 75: Print Using "## #"; SX%(26, 1, tickInterval)
            End If

            If Left$(SX$(27, 0), 3) <> "XXX" Then
                Locate 8, 61
                tickerX = 27
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(27, 0), 13);
                Locate , 75: Print Using "## "; SX%(27, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(27, 1), 13);
                Locate , 75: Print Using "## #"; SX%(27, 1, tickInterval)
            End If

            If Left$(SX$(28, 0), 3) <> "XXX" Then
                Locate 11, 61
                tickerX = 28
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(28, 0), 13);
                Locate , 75: Print Using "## "; SX%(28, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(28, 1), 13);
                Locate , 75: Print Using "## #"; SX%(28, 1, tickInterval)
            End If

            If Left$(SX$(29, 0), 3) <> "XXX" Then
                Locate 14, 61
                tickerX = 29
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(29, 0), 13);
                Locate , 75: Print Using "## "; SX%(29, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(29, 1), 13);
                Locate , 75: Print Using "## #"; SX%(29, 1, tickInterval)
            End If

            If Left$(SX$(30, 0), 3) <> "XXX" Then
                Locate 17, 61
                tickerX = 30
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(30, 0), 13);
                Locate , 75: Print Using "## "; SX%(30, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(30, 1), 13);
                Locate , 75: Print Using "## #"; SX%(30, 1, tickInterval)
            End If

            If Left$(SX$(31, 0), 3) <> "XXX" Then
                Locate 20, 61
                tickerX = 31
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(31, 0), 13);
                Locate , 75: Print Using "## "; SX%(31, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(31, 1), 13);
                Locate , 75: Print Using "## #"; SX%(31, 1, tickInterval)
            End If

            If Left$(SX$(32, 0), 3) <> "XXX" Then
                Locate 23, 61
                tickerX = 32
                tickInterval = TickerTime%(timeInterval, tickerX)

                Print Left$(SX$(32, 0), 13);
                Locate , 75: Print Using "## "; SX%(32, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(32, 1), 13);
                Locate , 75: Print Using "## #"; SX%(32, 1, tickInterval);
            End If

        End If

        I$ = GetKeyPress$

    End If

    Screen , , 0, 0
    Cls

End Sub


'----------------------------------------
'          TickerTime Function
'----------------------------------------
'This subroutine determines which of the
'pre-determined time intervals the ticker
'display should be using.
Function TickerTime% (timeInterval, X)

    Shared tickerStart

    tickerIdx = timeInterval + (tickerStart - ST%(X)) * 3

    If tickerIdx < 0 Then tickerIdx = 0
    If tickerIdx > 14 Then tickerIdx = 14
    If gameOver = 1 Then tickerIdx = 14

    TickerTime% = tickerIdx

End Function


'----------------------------------------
'        TICKERUPDATE Subroutine
'----------------------------------------
'This subroutine calculates the updated
'scores for the ticker games.
Sub TICKERUPDATE ()

    'Should this be <= 31???
    If autoPlay = 1 And schedGame(2) <= 32 Then

        If quarter = 1 Then
            If gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 1) = -1 Then
                SX%(schedGame(2) + 1, 0, 1) = score(0, 0): SX%(schedGame(2) + 1, 1, 1) = score(1, 0)
            End If
            If gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 2) = -1 Then
                SX%(schedGame(2) + 1, 0, 2) = score(0, 0): SX%(schedGame(2) + 1, 1, 2) = score(1, 0)
            End If
        End If

        If quarter = 2 Then
            If gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 4) = -1 Then
                SX%(schedGame(2) + 1, 0, 4) = score(0, 0)
                SX%(schedGame(2) + 1, 1, 4) = score(1, 0)
            End If
            If gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 5) = -1 Then
                SX%(schedGame(2) + 1, 0, 5) = score(0, 0)
                SX%(schedGame(2) + 1, 1, 5) = score(1, 0)
            End If
        End If

        If quarter > 2 Then
            If SX%(schedGame(2) + 1, 0, 3) = -1 Then
                SX%(schedGame(2) + 1, 0, 3) = score(0, 1)
                SX%(schedGame(2) + 1, 1, 3) = score(1, 1)
            End If
            If SX%(schedGame(2) + 1, 0, 6) = -1 Then
                For I2 = 6 To 8:
                    SX%(schedGame(2) + 1, 0, I2) = score(0, 2) + score(0, 1)
                    SX%(schedGame(2) + 1, 1, I2) = score(1, 2) + score(1, 1)
                Next
            End If
        End If

        If quarter = 3 Then
            If gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 9) = -1 Then
                SX%(schedGame(2) + 1, 0, 9) = score(0, 0)
                SX%(schedGame(2) + 1, 1, 9) = score(1, 0)
            End If
            If gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 10) = -1 Then
                SX%(schedGame(2) + 1, 0, 10) = score(0, 0)
                SX%(schedGame(2) + 1, 1, 10) = score(1, 0)
            End If
        End If

        If quarter > 3 And SX%(schedGame(2) + 1, 0, 11) = -1 Then
            SX%(schedGame(2) + 1, 0, 11) = score(0, 3) + score(0, 2) + score(0, 1)
            SX%(schedGame(2) + 1, 1, 11) = score(1, 3) + score(1, 2) + score(1, 1)
        End If

        If quarter = 4 Then
            If gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 12) = -1 Then
                SX%(schedGame(2) + 1, 0, 12) = score(0, 0)
                SX%(schedGame(2) + 1, 1, 12) = score(1, 0)
            End If
            If gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 13) = -1 Then
                SX%(schedGame(2) + 1, 0, 13) = score(0, 0)
                SX%(schedGame(2) + 1, 1, 13) = score(1, 0)
            End If
        End If

        SX%(schedGame(2) + 1, 0, 14) = score(0, 0)
        SX%(schedGame(2) + 1, 1, 14) = score(1, 0)

    End If

End Sub


'----------------------------------------
'       UpdateSchedule Subroutine
'----------------------------------------
'This subroutine updates the schedule
'file for a completed game. It will first
'update any relevant ticker scores, then
'update the scores for the current game in
'the corresponding schedule file.
Sub UpdateSchedule ()

    Shared scheduleFile$

    If schedGame(2) <= 32 Then

        For X = 0 To 1:
            SX$(schedGame(2) + 1, X) = gameTeams$(X)
        Next

        Open cfgPath$ + "TICKER" For Output As #1

        For I = 1 To 32:
            For I1 = 0 To 1:
                For I2 = 1 To 14
                    Print #1, SX%(I, I1, I2)
                Next
            Next
        Next

        For I = 1 To 32:
            For I1 = 0 To 1:
                Print #1, SX$(I, I1)
            Next
        Next

        For I = 1 To 32:
            Print #1, ST%(I)
        Next

        Close #1

    End If

    Open scheduleFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    schedOptions(13) = 1

    For X = 0 To 20:
        LSet Q$(X + 1) = MKI$(schedOptions(X))
    Next

    For X = 0 To 1:
        LSet Q$(22 + X) = diskIDs$(X)
    Next

    LSet Q$(24) = MKI$(score(0, 0))
    LSet Q$(25) = MKI$(score(1, 0))

    Put #1, schedGame(0) + schedGame(2)

    Close #1

    schedGame(2) = schedGame(2) + 1

    autoPlay = 1

End Sub


'----------------------------------------
'            SCOUT Subroutine
'----------------------------------------
'This subroutine makes some sort of adj-
'ustments to each team, but it's still not
'clear why it's called "SCOUT"
Sub SCOUT ()

    For scoutTeam = 0 To 1

        I2 = 0
        I3 = 0
        I4 = 0
        I5 = 0
        I6 = 0

        For I1 = 0 To 9
            If rbNumRec(scoutTeam, I1) <> 0 Then
                I2 = rbNumRec(scoutTeam, I1)
                I3 = 0
            End If
        Next I1

        For I1 = 0 To 9
            If I1 = 0 Then
                K3(scoutTeam, 3) = K3(scoutTeam, 3) + rbNumRec(scoutTeam, I1) * rbYdsPerC(scoutTeam, I1)
                K3(scoutTeam, 3) = K3(scoutTeam, 3) + wrYdsPerC(scoutTeam, I1) * (wrNumRec(scoutTeam, I1) - I2)
            End If

            If I1 > 0 And I1 < 10 And rbNumRec(scoutTeam, I1) <> 0 Then
                K3(scoutTeam, 3) = K3(scoutTeam, 3) + rbYdsPerC(scoutTeam, I1) * (rbNumRec(scoutTeam, I1) - I3)
            End If

            If I1 > 0 And I1 < 6 And wrNumRec(scoutTeam, I1) <> 0 Then
                K3(scoutTeam, 3) = K3(scoutTeam, 3) + wrYdsPerC(scoutTeam, I1) * (wrNumRec(scoutTeam, I1) - I6)
            End If

            If rbNumRec(scoutTeam, I1) <> 0 Then
                I3 = rbNumRec(scoutTeam, I1)
            End If

            If wrNumRec(scoutTeam, I1) <> 0 Then
                I6 = wrNumRec(scoutTeam, I1)
            End If

        Next I1

        K3(scoutTeam, 3) = Int((K3(scoutTeam, 3) * 3) / 100)

        I2 = 0
        I3 = 0
        I6 = 0

        For I1 = 0 To 9
            If rbRushContrib(scoutTeam, I1) <> 0 Then
                I4 = rbRushContrib(scoutTeam, I1)
                I5 = 0
            End If
        Next I1

        For I1 = 0 To 9
            If I1 = 0 Then
                K3(scoutTeam, 0) = K3(scoutTeam, 0) + rbRushContrib(scoutTeam, I1) * rbRushAvg(scoutTeam, I1)
                I2 = I2 + qbNumAtt(scoutTeam, I1) * qbCompPct(scoutTeam, I1)
                I3 = I3 + qbNumAtt(scoutTeam, I1) * qbPctInt(scoutTeam, I1)
            End If

            If I1 > 0 And rbRushContrib(scoutTeam, I1) <> 0 Then
                K3(scoutTeam, 0) = K3(scoutTeam, 0) + rbRushAvg(scoutTeam, I1) * (rbRushContrib(scoutTeam, I1) - I5)
            End If

            If I1 > 0 And I1 < 4 And qbNumAtt(scoutTeam, I1) <> 0 Then
                I2 = I2 + qbCompPct(scoutTeam, I1) * (qbNumAtt(scoutTeam, I1) - I6)
                I3 = I3 + qbPctInt(scoutTeam, I1) * (qbNumAtt(scoutTeam, I1) - I6)
            End If

            If rbRushContrib(scoutTeam, I1) <> 0 Then
                I5 = rbRushContrib(scoutTeam, I1)
            End If

            If qbNumAtt(scoutTeam, I1) <> 0 Then
                I6 = qbNumAtt(scoutTeam, I1)
            End If

        Next I1

        I1 = 1 - scoutTeam

        'Defensive Ratings
        K3(scoutTeam, 0) = ((K3(scoutTeam, 0) * 3) / 100) + ((leagRat_GAME(I1, 0)) * 3)
        K3(scoutTeam, 0) = CInt(K3(scoutTeam, 0)) / 10

        K3(scoutTeam, 1) = CInt((I2 / 100) + leagRat_GAME(I1, 1))
        K3(scoutTeam, 2) = CInt((I3 / 100) - leagRat_GAME(I1, 2))
        K3(scoutTeam, 4) = (ydsPerPunt(scoutTeam, 0) * 2) + 28
        K3(scoutTeam, 5) = (kickerFGA(scoutTeam, activeKicker(scoutTeam)) * 5) + 25
        K3(scoutTeam, 6) = kickerIdx(scoutTeam, activeKicker(scoutTeam))

        XE = ydsPerComp(I1) / 100

        If XE < 0.5 Then
            XD(I1) = Int(XE * 30)
        End If
        If XE > 0.5 Then
            XD(I1) = Int(((1 - XE) * 30) * -1)
        End If

        If scoutTeam = gameLoc Then
            'Home field
            K3(scoutTeam, 0) = K3(scoutTeam, 0) + .3
            K3(scoutTeam, 1) = K3(scoutTeam, 1) + 3

        Else
            'Away (not neutral)
            If gameLoc <> 2 Then
                K3(scoutTeam, 0) = K3(scoutTeam, 0) - .3
                K3(scoutTeam, 1) = K3(scoutTeam, 1) - 3
            End If

        End If

    Next scoutTeam

End Sub


'----------------------------------------
'          HALFSTART Subroutine
'----------------------------------------
'This subroutine resets the game parameters
'for a new half: yard line, possession,
'new set of downs, timeouts, etc.
Sub HALFSTART ()

    'Team to Kick/Receive
    P1 = winTeam
    D = 1 - winTeam

    'Timeouts
    timeouts(0) = 3
    timeouts(1) = 3

    '1st and 10
    currDown = 1
    ydsToFirst = 10
    YF = -ydsToFirst

    Call KickoffLine

End Sub


'----------------------------------------
'         CHOOSEKICKER Subroutine
'----------------------------------------
'This subroutine gives the user a choice
'of kicker for a kicking attempt.
Sub CHOOSEKICKER (currTeam)

    Color , 1
    Cls

    Call OPTIONBOX(10)

    Locate , 31: Print "SELECT KICKER": Print
    Locate , 18
    Print " # NAME            FG%  %ATT  EP%  %ATT"

    For I = 0 To 1

        If kicker$(currTeam, I) <> "XXX" And kicker$(currTeam, I) <> "END" And kicker$(currTeam, I) <> "" Then
            Locate , 18
            Print I; "-"; kicker$(currTeam, I);
            Locate , 36
            Print Using " ###   ###  ###   ###"; kickerFGA(currTeam, I) * 5 + 25, kickerFGPct(currTeam, I), kickerIdx(currTeam, I), kickerPATPct(currTeam, I)
        End If

        Locate , 1

    Next I

    Print

    Locate , 19: Print "SELECT (0-1) ";

    Do
        I$ = GetKeyPress$
        choice = Val(I$)
    Loop Until choice = 0 Or choice = 1

    activeKicker(currTeam) = choice

End Sub


'----------------------------------------
'           PLACEPBP Subroutine
'----------------------------------------
'This subroutine is used to position the
'Play-by-Play text in the screen, taking
'care to center the text as well.
Sub PLACEPBP ()

    Color 14 - 7 * BW, 1 - BW

    pbpRow = 25

    For I = 1 To MAX_PBP_LINES

        stringOutput$ = _Trim$(Left$(pbpText$(I), 74))

        Locate pbpRow + I - 1, 3
        Print String$(74, 46);

        Locate pbpRow + I - 1, 3
        Print " "; stringOutput$; " "

    Next

End Sub


'----------------------------------------
'         DISPWEATHER Subroutine
'----------------------------------------
'This subroutine displays the current
'weather and wind conditions
Sub DISPWEATHER ()

    Color 15 * 1 - BW, 2 + 5 * BW

    Locate 7, 20

    Print "WIND: ";

    If climate(1) = 0 Then
        Print "NONE ";
    End If
    If climate(1) = -7 Then
        Print "LIGHT ";
    End If
    If climate(1) = -15 Then
        Print "STRONG ";
    End If

    If climate(1) <> 0 Then

        Select Case climate(0)
            Case P1
                Print "+";
            Case D
                Print "-";
            Case 2
                Print "X";
        End Select

    End If

    Locate , 40
    Print "WEATHER: ";

    If climate(4) = 0 Then
        Print "GOOD"
    End If
    If climate(4) = -1 Then
        Print "RAIN/SNOW"
    End If
    If climate(4) = -2 Then
        Print "DOWNPOUR/BLIZZARD"
    End If

End Sub


'----------------------------------------
'         CHANGEPOSS Subroutine
'----------------------------------------
'This subroutine literally just swaps the
'indicators as to which team in the arrays
'is defense and which is on offense.
Sub CHANGEPOSS ()

    Swap P1, D

End Sub


'----------------------------------------
'          SETSCREEN Subroutine
'----------------------------------------
'This subroutine essentially draws the
'football field and scoreboard on a fresh
'graphic screen. It has been gutted to
'create the DisplayField routine as one
'that is separate.
Sub SETSCREEN ()

    'Screen , , 0, 0

    Color 0, 0

    Cls

    Call DisplayField

End Sub


'----------------------------------------
'          Scoreboard Subroutine
'----------------------------------------
' Draws the latest score on the screen
Sub Scoreboard

    'Converted these Chr$ statements so the graphics
    'are preserved in any text editor
    Print Chr$(219); String$(38, 223)
    Print Chr$(219)
    Print Chr$(219)
    Print Chr$(219)

    Locate 2, 40
    Color 1 + BW * 6, 0
    Print Chr$(220)

    Locate 3, 2, 0
    Color 15 * (1 - BW), 1 + BW * 6

    'Quarter Headings
    Print String$(19, 32);

    If quarter <= 4 Then

        Print "1  2  3  4      ";

    Else

        If quarter = 5 Then
            Print "1  2  3  4  OT  ";
        Else

            Locate 3, 2
            Print String$(39, 32);
            Locate , 19

            Select Case quarter

                Case 5 To 8
                    minQtr = 5

                Case 9 To 12
                    minQtr = 9

            End Select

            For J = minQtr To quarter
                Print Using "OT# "; J - 4;
            Next J
        End If

    End If

    Locate , 37: Print "T   ";

    'Score
    For I = 0 To 1

        Locate I + 4, 2
        Print "  ";

        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1)
        Print gameTeams$(I);

        Color 15 * (1 - BW), 1 + BW * 6
        Print Tab(36);

        Locate I + 4, 20

        If quarter <= 5 Then

            For J = 1 To quarter
                Print Using "## "; score(I, J);
            Next J

        Else

            Select Case quarter

                Case 5 To 8
                    minQtr = 5

                Case 9 To 12
                    minQtr = 9

            End Select

            For J = minQtr To quarter
                If I > 1 Or K > 20 Then
                    Print #10, "I = "; I; "; J = "; J
                End If
                Print Using "##  "; score(I, J);
            Next J

        End If

        Locate , 35
        Print Using "###   "; score(I, 0);

    Next I

    Locate 4 + P1, 3
    Print "*";

    Locate 5 - P1, 3
    Print " ";

    Locate 6, 1
    Color 0, 1 + BW * 6
    Print Chr$(219); String$(39, 220);

    Locate 2, 43
    Color 15, 4 + BW * 3
    Print Chr$(219); String$(36, 223);

    Locate 3, 43
    Print Chr$(219);

    Locate 4, 43
    Print Chr$(219);

    Locate 5, 43
    Print Chr$(219);

    Locate 2, 80
    Color 0: Print Chr$(223);

End Sub


'----------------------------------------
'        DisplayField Subroutine
'----------------------------------------
'This subroutine draws the football field
'and scoreboard.
Sub DisplayField ()

    Color 15, 1 + BW * 6

    Print

    Call Scoreboard

    'Display Status (Clock/Down/Yardage)
    Call YARDLINE
    Call DisplayClockDown

    Locate 6, 44

    Color 0, 4 + BW * 3: Print String$(37, 220);

    Color 15 * (1 - BW), 2 + 5 * BW

    'Display field background
    fieldHeight = 16 '12

    For I = 7 To (7 + fieldHeight):
        Locate I, 1
        Print String$(80, 32);
    Next I

    Call DISPWEATHER

    'Replaced special characters with Chr$ references
    'This keeps modern text editors from messing things up

    'Print top row yard markers
    Locate 8, 4
    Color 15 * (1 - BW), 2 + 5 * BW
    Print Chr$(218) + String$(3, 196);

    For II = 1 To 21
        Print String$(2, 196) + Chr$(194);
    Next

    Print String$(5, 196) + Chr$(191);

    'Print end zones and major yard markers
    markers$ = Chr$(179) + String$(5, 176)

    For II = 1 To 20
        markers$ = markers$ + Chr$(179) + "  "
    Next

    markers$ = markers$ + Chr$(179) + String$(5, 176) + Chr$(179)

    '9 to 17
    For II = 9 To (5 + fieldHeight)
        Locate II, 4
        Print markers$
    Next

    'Print bottom row yard markers
    'Locate 18, 4
    Locate 22, 4
    Print Chr$(192) + String$(5, 196);

    For II = 1 To 21
        Print Chr$(193) + String$(2, 196);
    Next

    Print String$(3, 196) + Chr$(217);

    'Print minor yard markers
    For II = 10 To 67 Step 3
        For J = 1 To 2
            'Locate 11, II + J
            Locate 13, II + J
            Print Chr$(34);
            'Locate 16, II + J
            Locate 18, II + J
            Print Chr$(34);
        Next
    Next

    'Print major yard numbers
    For II = 1 To 9
        'For J = 9 To 17 Step 8
        For J = 9 To (5 + fieldHeight) Step 12
            Locate J, II * 6 + 9
            Print Using "#" + Chr$(179) + "0"; II + 2 * (II - 5) * (II > 5);
        Next
    Next

    'Print play-by-play box
    pbpRow = 24
    'Locate pbpRow, 1: Print Tab(79)

    Color 14 - 7 * BW, 1 - BW
    'Locate pbpRow, 1: Print String$(80, 32);
    Locate pbpRow, 1: Print Chr$(219); String$(78, 223); Chr$(219);
    Locate pbpRow + 1, 1: Print Chr$(219) + " "; String$(76, 46); " " + Chr$(219);
    Locate pbpRow + 2, 1: Print Chr$(219) + " "; String$(76, 46); " " + Chr$(219);
    Locate pbpRow + 3, 1: Print Chr$(219) + " "; String$(76, 46); " " + Chr$(219);
    Locate pbpRow + 4, 1: Print Chr$(219) + " "; String$(76, 46); " " + Chr$(219);
    Locate pbpRow + 5, 1: Print Chr$(219); String$(78, 220); Chr$(219);

End Sub


'----------------------------------------
'          YARDLINE Subroutine
'----------------------------------------
'This subroutine calculates the yardline
'for the ball. This is needed because our
'variables for tracking position only use
'yards to endzone.
Sub YARDLINE ()

    ydLineTeam = ydsToScore - (ydsToScore > 50) * (100 - ydsToScore - ydsToScore)

End Sub


'----------------------------------------
'          GOALLINE Subroutine
'----------------------------------------
'This subroutine calculates the yardage
'gained / lost from plays when close to
'the goal line.
Sub GOALLINE ()

    ' *** GOAL LINE DEFENSE ***
    Call Rolld100(RN6)

    If (rushEra = 0 And (dLine = 4 Or dLine = 7)) Then
        'This is the code as originally written
        'The game, as currently written, will never execute the Below
        'This routine only gets called if goal line is explicitly chosen

        Select Case ydLine

            Case 94.999 To 95.998

                Select Case RN6
                    Case 1 To 14: ydsGained = 6
                    Case 15 To 17: ydsGained = 4
                    Case 18 To 29: ydsGained = 3
                    Case 30 To 43: ydsGained = 2
                    Case 44 To 63: ydsGained = 1
                    Case 64 To 77: ydsGained = 0
                    Case 78 To 84: ydsGained = -1
                    Case 85 To 91: ydsGained = -2
                    Case 92 To 98: ydsGained = -3
                    Case 99 To 100: ydsGained = -4
                End Select

            Case 95.999 To 96.998

                Select Case RN6
                    Case 1 To 29: ydsGained = 5
                    Case 30 To 31: ydsGained = 3
                    Case 32 To 45: ydsGained = 2
                    Case 46 To 72: ydsGained = 1
                    Case 73 To 86: ydsGained = 0
                    Case 87 To 90: ydsGained = -1
                    Case 91: ydsGained = -2
                    Case 92 To 94: ydsGained = -3
                    Case 95 To 97: ydsGained = -4
                    Case 98 To 100: ydsGained = -5
                End Select

            Case 96.999 To 97.998

                Select Case RN6
                    Case 1 To 24: ydsGained = 4
                    Case 25 To 28: ydsGained = 2
                    Case 29 To 69: ydsGained = 1
                    Case 70 To 85: ydsGained = 0
                    Case 86 To 88: ydsGained = -1
                    Case 89 To 93: ydsGained = -2
                    Case 94 To 100: ydsGained = -4
                End Select

            Case 97.999 To 98.998

                Select Case RN6
                    Case 1 To 43: ydsGained = 3
                    Case 44 To 65: ydsGained = 1
                    Case 66 To 83: ydsGained = 0
                    Case 84 To 92: ydsGained = -1
                    Case 93 To 94: ydsGained = -2
                    Case 95 To 100: ydsGained = -4
                End Select

            Case Is >= 98.999
                Select Case RN6
                    Case 1 To 57: ydsGained = 2
                    Case 58 To 74: ydsGained = 0
                    Case 75 To 83: ydsGained = -1
                    Case 84 To 90: ydsGained = -2
                    Case 91 To 97: ydsGained = -3
                    Case 98 To 100: ydsGained = -4
                End Select

        End Select

    Else
        'These impacts are typically covered by
        'an explicit goal line defense
        Select Case ydLine

            Case 94.999 To 95.998

                Select Case RN6
                    Case 1 To 17: ydsGained = 6
                    Case 18 To 22: ydsGained = 4
                    Case 23 To 35: ydsGained = 3
                    Case 36 To 52: ydsGained = 2
                    Case 53 To 76: ydsGained = 1
                    Case 77 To 93: ydsGained = 0
                    Case 94 To 100: ydsGained = -1
                End Select

            Case 95.999 To 96.998

                Select Case RN6
                    Case 1 To 26: ydsGained = 5
                    Case 27 To 29: ydsGained = 3
                    Case 30 To 49: ydsGained = 2
                    Case 50 To 78: ydsGained = 1
                    Case 79 To 94: ydsGained = 0
                    Case 95 To 100: ydsGained = -1
                End Select

            Case 96.999 To 97.998

                Select Case RN6
                    Case 1 To 27: ydsGained = 4
                    Case 28 To 32: ydsGained = 2
                    Case 33 To 80: ydsGained = 1
                    Case 81 To 98: ydsGained = 0
                    Case 99 To 100: ydsGained = -1
                End Select

            Case 97.999 To 98.998

                Select Case RN6
                    Case 1 To 46: ydsGained = 3
                    Case 47 To 72: ydsGained = 1
                    Case 73 To 91: ydsGained = 0
                    Case 92 To 100: ydsGained = -1
                End Select

            Case Is >= 98.999

                Select Case RN6
                    Case 1 To 61: ydsGained = 2
                    Case 62 To 96: ydsGained = 0
                    Case 97 To 100: ydsGained = -1
                End Select

        End Select

    End If

    Call Rolld100(RN7)

    If rushBase <= 30 Then
        'Added this to avoid scenarios
        'where the rushing average from
        'OTHERS was negative, therefore
        'leading to a negative index
        If rushBase <= 0 Then
            rushBase = 1
        End If

        If RN7 <= goalLnYdAdj(rushBase, 1) Then
            ydsGained = ydsGained + goalLnYdAdj(rushBase, 2)
        End If

    Else
        If RN7 <= goalLnYdAdj(30, 1) Then
            ydsGained = ydsGained + goalLnYdAdj(30, 2)
        End If

    End If

End Sub


'----------------------------------------
'          NAMEEDIT Subroutine
'----------------------------------------
'This subroutine trims all the spaces from
'team names, mascot names and player names.
Sub NAMEEDIT ()

    For I = 0 To 1

        gameTeams$(I) = RTrim$(gameTeams$(I))
        gameMascots$(I) = RTrim$(gameMascots$(I))

        For I1 = 0 To 9:
            rbacks$(I, I1) = RTrim$(rbacks$(I, I1))
        Next I1

        For I1 = 0 To 5:
            wdRec$(I, I1) = RTrim$(wdRec$(I, I1))
        Next I1

        For I1 = 0 To 3:
            qbacks$(I, I1) = RTrim$(qbacks$(I, I1))
        Next I1

        For I1 = 0 To 2:
            kickRet$(I, I1) = RTrim$(kickRet$(I, I1))
        Next I1

        For I1 = 0 To 2:
            pret$(I, I1) = RTrim$(pret$(I, I1))
        Next I1

        punter$(I, 0) = RTrim$(punter$(I, 0))

        For I1 = 0 To 1:
            kicker$(I, I1) = RTrim$(kicker$(I, I1))
        Next I1

        For I1 = 0 To 14:
            defSacks$(I, I1) = RTrim$(defSacks$(I, I1))
        Next I1

        For I1 = 0 To 9
            defInts$(I, I1) = RTrim$(defInts$(I, I1))
        Next I1

    Next I

End Sub


'----------------------------------------
'          SCOUTREPT Subroutine
'----------------------------------------
'This subroutine clears the screen so it
'can print a quick scouting report
Sub SCOUTREPT ()

    Call OPTIONBOX(13)
    Locate 8, 30: Print "SCOUTING REPORT"

    Locate 10, 30
    Color teamRat_GAME(0, 2), teamRat_GAME(0, 1): Print gameTeams$(0);

    Color 0, 7
    Locate , 52
    Color teamRat_GAME(1, 2), teamRat_GAME(1, 1): Print gameTeams$(1)

    Color 0, 7
    Print
    Locate , 12
    Print "RUSH AVG.";
    Locate , 34: Print Using "#.#"; K3(0, 0);
    Locate , 56: Print Using "#.#"; K3(1, 0)

    Locate , 12: Print "FUM %";
    Locate , 34: Print leagRat_GAME(0, 3) + leagRat_GAME(1, 4);
    Locate , 56: Print leagRat_GAME(1, 3) + leagRat_GAME(0, 4)

    Locate , 12: Print "COMP %";
    Locate , 34: Print K3(0, 1);
    Locate , 56: Print K3(1, 1)

    Locate , 12: Print "INT %";
    Locate , 34: Print K3(0, 2);
    Locate , 56: Print K3(1, 2)

    Locate , 12: Print "AVG/COMP";
    Locate , 34: Print Using "##.#"; K3(0, 3) + XD(1);
    Locate , 56: Print Using "##.#"; K3(1, 3) + XD(0)

    Locate , 12: Print "PUNT AVG";
    Locate , 34: Print K3(0, 4);
    Locate , 56: Print K3(1, 4)

    Locate , 12: Print "FG % ";
    Locate , 34: Print K3(0, 5);
    Locate , 56: Print K3(1, 5)

    Locate , 12: Print "EP % ";
    Locate , 34: Print K3(0, 6);
    Locate , 56: Print K3(1, 6)

    Locate , 12: Print "SACKED %";
    Locate , 34: Print leagRat_GAME(0, 5) + leagRat_GAME(1, 6);
    Locate , 56: Print leagRat_GAME(1, 5) + leagRat_GAME(0, 6)

    Locate , 12: Print "RUN/PASS %";
    Locate , 34: Print leagRat_GAME(0, 7);
    Locate , 56: Print leagRat_GAME(1, 7)

    Locate 12, 38: If K3(0, 0) - (leagRat_GAME(1, 0) * .3) > 4.5 And leagRat_GAME(0, 7) < 50 Then Print "*";
    Locate 12, 60: If K3(1, 0) - (leagRat_GAME(0, 0) * .3) > 4.5 And leagRat_GAME(1, 7) < 50 Then Print "*";

    Locate 16, 38
    If K3(0, 3) >= 18 Then Print "*";

    Locate 16, 60
    If K3(1, 3) >= 18 Then Print "*";

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'          ADDBACKS Subroutine
'----------------------------------------
'This subroutine populates running backs
'to the list of receivers.
Sub ADDBACKS ()

    For I = 0 To 1

        For I1 = 0 To 9

            For I2 = 0 To 3

                If rbacks$(I, I1) = qbacks$(I, I2) Then
                    gameStatsPlayer(I, I1, 0) = gameStatsPlayer(I, I1, 0) + gameStatsPlayer(I, I2 + 10, 0)
                    gameStatsPlayer(I, I1, 1) = gameStatsPlayer(I, I1, 1) + gameStatsPlayer(I, I2 + 10, 1)
                    gameStatsPlayer(I, I1, 2) = gameStatsPlayer(I, I1, 2) + gameStatsPlayer(I, I2 + 10, 2)
                    gameStatsPlayer(I, I2 + 10, 0) = 0
                    gameStatsPlayer(I, I2 + 10, 1) = 0
                    gameStatsPlayer(I, I2 + 10, 2) = 0
                End If

            Next I2

        Next I1

    Next I

End Sub


'----------------------------------------
'           ONSIDE Subroutine
'----------------------------------------
'This subroutine handles the resolution of
'an onside kick and the associated text in
'the Play-by-Play
Sub ONSIDE ()

    pbpString$ = kicker$(P1, activeKicker(P1)) + " IS PLACING THE BALL ON THE TEE"
    Call PBP

    If d100% <= 50 Then
        pbpString$ = gameTeams$(P1) + " LINE UP FOR THE KICKOFF"
    Else
        pbpString$ = gameMascots$(P1) + " LINE UP FOR THE KICKOFF"
    End If

    Call PBP

    pbpString$ = "IT'S AN ONSIDE KICK!!"
    Call PBP

    okDist = (Rnd(1) * 21) + 5
    ydLine = ydLine + okDist
    ydsGained = okDist

    Call AddPossessionTime(4, 7)

    If ruleYrPro >= 8 And ruleOptType <> 0 Then
        'Pro Football, Years 2018+
        If teamYears(D) >= 2018 And teamYears(D) <= 2019 Then
            onsideSuccess = 7
        Else
            onsideSuccess = 11
        End If
    Else
        onsideSuccess = 21
    End If

    If CInt(ydsGained * 10) / 10 < 9.95 Then

        pbpString$ = "BALL DID NOT TRAVEL 10 YDS"
        Call PBP
        ydLine = 100 - ydLine

    Else

        If d100% <= onsideSuccess Then

            'Kicking team recovers
            pbpString$ = gameTeams$(P1) + " RECOVERS !!!"
            Call PBP

            'Update possession for OFF (kicking team) - onside kick
            If isOT = 1 And (overtimeOpt = 1 Or overtimeOpt = 2 Or overtimeOpt >= 6) Then
                nbrPossOT(P1) = 1
            End If

        Else

            'Receiving team gets it
            pbpString$ = gameTeams$(D) + " RECOVERS"
            Call PBP
            ydLine = 100 - ydLine

            Call CHANGEPOSS

        End If

    End If

End Sub


'----------------------------------------
'         INCOMPLETE Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'text string for an incomplete pass.
Sub INCOMPLETE ()

    Call Rolld100(RN1)

    Select Case RN1

        Case 1 To 10
            ' ***PASS BROKEN UP ***
            Call QBTHROW

            RN1 = Int(Rnd(1) * 6) + 1

            Select Case RN1
                Case 1: pbpString$ = "PASS IS BROKEN UP"
                Case 2: pbpString$ = "PASS IS TIPPED AWAY"
                Case 3: pbpString$ = "PASS IS KNOCKED AWAY"
                Case 4: pbpString$ = "PASS IS KNOCKED DOWN"
                Case 5: pbpString$ = "PASS IS DEFLECTED"
                Case 6: pbpString$ = "PASS IS BATTED AWAY"
            End Select

            'Uses players with INTs as names for the work
            If defInts(D) > 0 Then
                Call FINDINT(defPlayer)
                pbpString$ = pbpString$ + " BY " + defTeam$
            End If

        Case 11 To 20

            RN2 = Int(Rnd(1) * 4) + 1

            Select Case RN2
                Case 1: pbpString$ = "HE THROWS THIS ONE AWAY"
                Case 2: pbpString$ = "HE JUST GETS IT OUT OF HIS HAND"
                Case 3: pbpString$ = "HE HAS TO THROW IT AWAY"
                Case 4: pbpString$ = "THIS ONE WILL GO NOWHERE"
            End Select

        Case 21 To 100

            Call QBTHROW

            If d100% > 60 Then

                Call Rolld100(RN2)

                Select Case playCall

                    Case 13
                        'Screen
                        Select Case RN2
                            Case 1 To 40:
                                pbpString$ = "BUT THE SCREEN FALLS INCOMPLETE"
                            Case 41 To 55:
                                pbpString$ = "BUT THE SCREEN IS BROKEN UP"
                            Case 56 To 60:
                                pbpString$ = "BUT " + targetWRName$ + " CAN'T HANDLE THE PASS"
                            Case 61 To 100:
                                pbpString$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE"
                        End Select

                    Case 12
                        'Long Pass
                        Select Case RN2
                            Case 1 To 15:
                                pbpString$ = "BUT THE PASS IS BROKEN UP"
                            Case 16 To 44:
                                pbpString$ = "BUT THE PASS IS OVERTHROWN"
                            Case 45 To 74:
                                pbpString$ = "BUT " + targetWRName$ + " CAN'T GET TO THE BALL...INCOMPLETE"
                            Case 75 To 80:
                                pbpString$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE"
                            Case 81 To 100:
                                pbpString$ = "BUT " + targetWRName$ + " IS COVERED...INCOMPLETE"
                        End Select

                    Case Else
                        Select Case RN2
                            Case 1 To 23:
                                pbpString$ = "BUT THE PASS FALLS INCOMPLETE"
                            Case 24 To 47:
                                pbpString$ = "BUT THE PASS IS THROWN BEHIND " + targetWRName$ + "...INCOMPLETE"
                            Case 48 To 70:
                                pbpString$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE"
                            Case 71 To 75:
                                pbpString$ = targetWRName$ + " CAN'T HANDLE THE PASS...INCOMPLETE"
                            Case 76 To 81:
                                pbpString$ = "BUT THE PASS IS BATTED DOWN AT THE LINE OF SCRIMMAGE"
                            Case 82 To 96:
                                pbpString$ = "BUT THE PASS TO " + targetWRName$ + " IS BROKEN UP"
                            Case 97 To 199:
                                pbpString$ = "THE PASS IS BROKEN UP AND ALMOST PICKED OFF!!"
                        End Select

                End Select

            Else

                '40% chance of reaching here
                RN2 = Int(Rnd(1) * 26) + 1

                Select Case RN2
                    Case 1:
                        pbpString$ = targetWRName$ + " DROPS A PASS RIGHT TO HIM"
                    Case 2:
                        pbpString$ = targetWRName$ + " IS HIT AND CAN'T HANG ON....INCOMPLETE PASS"
                    Case 3:
                        pbpString$ = targetWRName$ + " BROKE THE WRONG WAY...INCOMPLETE"
                    Case 4:
                        pbpString$ = targetWRName$ + " DROPS IT!"
                    Case 5:
                        pbpString$ = "DROPPED BY " + targetWRName$
                    Case 6:
                        pbpString$ = "BUT " + targetWRName$ + " CAN'T REACH IT"
                    Case 7:
                        pbpString$ = targetWRName$ + " CAN'T HAUL IT IN...INCOMPLETE"
                    Case 8:
                        pbpString$ = targetWRName$ + " MISHANDLES IT....INCOMPLETE"
                    Case 9:
                        pbpString$ = targetWRName$ + " JUGGLES IT....INCOMPLETE"
                    Case 10:
                        pbpString$ = targetWRName$ + " CAN'T FIND THE HANDLE....INCOMPLETE"
                    Case 11:
                        pbpString$ = targetWRName$ + " CAN'T PULL IT IN....INCOMPLETE"
                    Case 12:
                        pbpString$ = targetWRName$ + " BOBBLES IT....INCOMPLETE"
                    Case 13:
                        pbpString$ = targetWRName$ + " WAS OUT OF BOUNDS....INCOMPLETE"
                    Case 14:
                        pbpString$ = targetWRName$ + " CAME DOWN OUT OF BOUNDS....INCOMPLETE"
                    Case 15:
                        pbpString$ = "THE PASS IS BROKEN UP"
                    Case 16:
                        pbpString$ = "BUT " + targetWRName$ + " CAN'T HANDLE THE PASS"
                    Case 17:
                        pbpString$ = "BUT THE PASS IS OVERTHROWN"
                    Case 18:
                        pbpString$ = "BUT " + targetWRName$ + " CAN'T GET TO THE BALL...INCOMPLETE"
                    Case 19:
                        pbpString$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE"
                    Case 20:
                        pbpString$ = "BUT " + targetWRName$ + " IS COVERED...INCOMPLETE"
                    Case 21:
                        pbpString$ = "BUT THE PASS FALLS INCOMPLETE"
                    Case 22:
                        pbpString$ = "BUT THE PASS IS THROWN BEHIND " + targetWRName$ + "...INCOMPLETE"
                    Case 23:
                        pbpString$ = targetWRName$ + " CAN'T HANDLE THE PASS...INCOMPLETE"
                    Case 24:
                        pbpString$ = "BUT THE PASS IS BATTED DOWN AT THE LINE OF SCRIMMAGE"
                    Case 25:
                        pbpString$ = "BUT THE PASS TO " + targetWRName$ + " IS BROKEN UP"
                    Case 26:
                        pbpString$ = "THE PASS IS BROKEN UP AND ALMOST PICKED OFF!!"
                End Select

            End If

    End Select

    Call PBP

End Sub

'----------------------------------------
'           QBSNEAK Subroutine
'----------------------------------------
'This subroutine handles the yard gain and
'Play-by-Play text for a QB sneak.
Sub QBSNEAK ()

    ' *** QB SNEAK ***
    brotherlyShove = 0

    teamName$ = Right$(gameTeams$(P1), Len(gameTeams$(P1)) - 3)

    If (teamName$ = "PHILADELPHIA" Or teamName$ = "BUFFALO") And (teamYears(P1) >= 2022 And teamYears(P1) <= 2025) Then

        brotherlyShove = 1

        RN0 = Int(Rnd(1) * 3) + 1

        If teamName$ = "PHILADELPHIA" Then
            If RN0 = 1 Then
                pbpString$ = gameTeams$(P1) + " LINES UP FOR THE BROTHERLY SHOVE!"
            Else
                pbpString$ = gameTeams$(P1) + " LINES UP FOR THE TUSH PUSH!"
            End If
        End If

        If teamName$ = "BUFFALO" Then
            If RN0 = 1 Then
                pbpString$ = gameTeams$(P1) + " LINES UP FOR THE SNOW PLOW!"
            Else
                pbpString$ = gameTeams$(P1) + " LINES UP FOR THE TUSH PUSH!"
            End If
        End If

        Call PBP

    End If

    If brotherlyShove = 1 Then
        pbpString$ = qbacks$(P1, qbNumber(P1)) + " PUSHES FORWARD WITH HELP FROM HIS TEAM."
    Else
        pbpString$ = qbacks$(P1, qbNumber(P1)) + " KEEPS IT ON THE SNEAK"
    End If

    Call PBP

    'Inside Linebacker is blitzing
    If (coverage = 7 Or coverage = 9 Or coverage = 11) Then

        'QB sneaks succeed 80% of the time
        If d100% > 80 Then
            '0.3 to 0.9
            ydsGained = (Int(Rnd(1) * 7) + 3) / 10
        Else
            '-.3 to .2
            ydsGained = (Int(Rnd(1) * 6) - 3) / 10
        End If

    Else

        'Use a variable to store each of the 4 values below
        'Then use separate logic (based on rush era) to
        'map dLine values to the variable below
        defStrategy = 1

        If rushEra > 1 Then

            Select Case dLine

                Case 1, 4
                    defStrategy = 1

                Case 2, 5, 9
                    defStrategy = 3

                Case 3, 6, 10
                    defStrategy = 4

                Case 11
                    defStrategy = 5

            End Select

        Else

            Select Case dLine

                Case 1, 2
                    defStrategy = 1

                Case 3, 4
                    defStrategy = 2

                Case 5, 6
                    defStrategy = 3

                Case 7 To 10
                    defStrategy = 4

                Case 11
                    defStrategy = 5

            End Select

        End If

        Select Case defStrategy

            Case 1
                'Read
                Select Case RN4
                    Case 1 To 10
                        '-.3 to .2
                        ydsGained = (Int(Rnd(1) * 6) - 3) / 10

                    Case 11 To 80
                        '0.3 to 0.9
                        ydsGained = (Int(Rnd(1) * 7) + 3) / 10

                    Case 81 To 100
                        '1 to 2
                        ydsGained = Int(Rnd(1) * 2) + 1

                End Select

            Case 2
                'Pinch
                If RN4 <= 30 Then
                    '-.3 to .2
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    '0.3 to 0.9
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 3
                'Inside Stunt
                If RN4 <= 15 Then
                    '-.3 to .2
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    '0.3 to 0.9
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 4
                'Outside Stunt / Pass Rush
                Select Case RN4
                    Case 1 To 10
                        '-.3 to .2
                        ydsGained = (Int(Rnd(1) * 6) - 3) / 10

                    Case 11 To 80
                        '0.3 to 0.9
                        ydsGained = (Int(Rnd(1) * 7) + 3) / 10

                    Case 81 To 100
                        '1 to 2
                        ydsGained = Int(Rnd(1) * 2) + 1

                End Select

            Case 5
                'Goal line
                If RN4 <= 40 Then
                    '-.3 to .2
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    '0.3 to 0.9
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

        End Select

    End If

    'Philly converted 94% of attempts in 2022,
    'but are about 85% successful overall
    'Buffalo has converted 77% overall
    If brotherlyShove = 1 Then
        If (teamName$ = "PHILADELPHIA" And RN4 <= 85) Or (teamName$ = "BUFFALO" And RN4 <= 77) Then
            '1.3 - 2.4 yards
            ydsGained = 1 + (Int(Rnd(1) * 15) + 3) / 10
        End If
    End If

    '-- FATIGUE NOT IMPLEMENTED
    'Fatigue yards - QB Sneak (increase)
    If endZone = 1 Then
        'ydsGained = ydsGained * fatigueLevel%
    End If

    sneakYds! = CInt((ydsGained) * 10) / 10
    newYdLine! = CInt((ydsGained + ydLine) * 10) / 10

    If sneakYds! = 0 Then
        pbpString$ = "HE ONLY MAKES IT TO THE LINE OF SCRIMMAGE"
    Else

        If sneakYds! < 0 Then
            pbpString$ = "HE LOSES" + Str$(0 - Int(sneakYds!))
        Else
            pbpString$ = "HE GAINS"
            If newYdLine! >= 99.94999 Then
                pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
            Else
                pbpString$ = pbpString$ + Str$(Int(sneakYds!))
            End If
        End If

        pbpString$ = pbpString$ + " YARDS ON THE PLAY"

    End If

    Call PBP

    I1 = qback

    scoreX0 = 4
    scoreX1 = 9
    playType = 1

End Sub


'----------------------------------------
'        OffPlayOveruse Subroutine
'----------------------------------------
'This subroutine adjusts the efficiency
'of a rushing play if the offense is using
'it more than normal
Sub OffPlayOveruse ()

    '  Team Rush Att + Team Pass Att
    If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then

        'If (team rush % < league rush %) and Y/C % > 18
        If (gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3))) < (leagRat_GAME(P1, 7) / 100) And K3(P1, 3) >= 18 Then
            'Play yardage potential is ~65%)
            I4 = I4 * .65
        End If

        If I4 <= 0 Then I4 = 1

    End If

End Sub


'----------------------------------------
'        DefPlayOveruse Subroutine
'----------------------------------------
'This subroutine attempts to adjust the
'effectiveness of a play if the defense
'has been overusing a particular formation.
Sub DefPlayOveruse ()

End Sub


'----------------------------------------
'           BLITZ Subroutine
'----------------------------------------
'This subroutine determines the yardage
'gains for a blitz defense for when a
'pass will be completed.
Sub BLITZ ()

    Shared blitzCovAdj(), playActAdj()

    XE = ydsPerComp(D) / 100

    If XE <= 0.5 Then
        XE = CInt(XE * 100)
    Else
        XE = CInt((1 - XE) * 100)
    End If

    If XE > 21 Then XE = 21
    If XE < 1 Then XE = 1

    If d100% <= playActAdj(1, XE) Then
        I4 = I4 + playActAdj(2, XE)
    End If

    'Cap "Arm Rating" at -20/20
    '(there are only 20 values in the array)
    If qbArmRat(P1, qbNumber(P1)) > 20 Then
        qbArmRat(P1, qbNumber(P1)) = 20
    End If
    If qbArmRat(P1, qbNumber(P1)) < -20 Then
        qbArmRat(P1, qbNumber(P1)) = -20
    End If

    armRatIdx = qbArmRat(P1, qbNumber(P1))

    If armRatIdx <> 0 Then

        If armRatIdx < 0 Then
            'Negative values
            armRatIdx = armRatIdx * -1
        End If

        If d100% <= ydsPerCompPctAdj(armRatIdx, 1) Then

            If qbArmRat(P1, qbNumber(P1)) > 0 Then
                'Positive arm rating - add to average
                I4 = I4 + ydsPerCompPctAdj(armRatIdx, 2)
            Else
                'Negative arm rating - add to average
                I4 = I4 - ydsPerCompPctAdj(armRatIdx, 2)
            End If

        End If

    End If

    Call OffPlayOveruse
    I7 = 4

    altYdg = 0

    Select Case playCall
        Case 9
            I7 = 1

        Case 10
            I7 = 3
            If coverage = 9 Or coverage = 10 Then
                Call BlitzYardageLong(21)
                altYdg = 1
            End If

            If coverage = 11 Then
                Call BlitzYardageLong(25)
                altYdg = 1
            End If

        Case 11
            If coverage = 7 Or coverage = 8 Then
                Call BlitzYardageLong(21)
                altYdg = 1
            End If

            If coverage = 9 Or coverage = 10 Then
                Call BlitzYardageLong(25)
                altYdg = 1
            End If

            If coverage = 11 Then
                Call BlitzYardageLong(28)
                altYdg = 1
            End If

        Case 12
            I7 = 5
            If coverage = 7 Or coverage = 8 Then
                Call BlitzYardageLong(33)
                altYdg = 1
            End If

            If coverage >= 9 Then
                Call BlitzYardageLong(42)
                altYdg = 1
            End If

        Case 13
            I7 = 2
            If coverage = 9 Or coverage = 10 Then
                Call BlitzYardageLong(25)
                altYdg = 1
            End If

            If coverage = 11 Then
                Call BlitzYardageLong(36)
                altYdg = 1
            End If

        Case 14
            If coverage = 7 Or coverage = 8 Then
                Call BlitzYardageLong(25)
                altYdg = 1
            End If

            If coverage = 9 Or coverage = 10 Then
                Call BlitzYardageLong(28)
                altYdg = 1
            End If

            If coverage = 11 Then
                Call BlitzYardageLong(31)
                altYdg = 1
            End If

        Case 15
            I7 = 5
            If coverage >= 7 Then
                Call BlitzYardageLong(42)
                altYdg = 1
            End If

        Case 16
            If coverage = 7 Or coverage = 8 Then
                Call BlitzYardageLong(18)
                altYdg = 1
            End If

            If coverage = 9 Or coverage = 10 Then
                Call BlitzYardageLong(28)
                altYdg = 1
            End If

            If coverage = 11 Then
                Call BlitzYardageLong(33)
                altYdg = 1
            End If

        Case 17, 18
            If coverage = 7 Or coverage = 9 Then
                Call BlitzYardageLong(21)
                altYdg = 1
            End If

            If coverage = 8 Then
                Call BlitzYardageLong(25)
                altYdg = 1
            End If

            If coverage >= 10 Then
                Call BlitzYardageLong(28)
                altYdg = 1
            End If

    End Select

    'If coverage >= 7 Then
    'Call BlitzYardageLong(I8)
    'altYdg = 1
    'End If

    If altYdg <> 1 Then

        I4 = I4 + passCovAdj(2, playCall - 8, coverage)

        If I4 > 10 Then I4 = 10
        If I4 < 1 Then I4 = 1

        Call Rolld100(RN9)

        For I = 1 To 14
            If RN9 <= blitzCovAdj(I7, I4, I) Then Exit For
        Next I

        'This calculates shorter gains after
        'an unsuccessful blitz attempt.
        ' Yardage ranges from -4 to 101
        ' 50% chance of gains under 30
        Select Case I

            Case 1:
                ydsGained = Int(Rnd(1) * 5) - 4

            Case 2:
                ydsGained = Int(Rnd(1) * 3) + 1

            Case 3:
                ydsGained = Int(Rnd(1) * 4) + 4

            Case 4:
                ydsGained = Int(Rnd(1) * 8) + 8

            Case 5:
                ydsGained = Int(Rnd(1) * 5) + 16

            Case 6:
                ydsGained = Int(Rnd(1) * 5) + 21

            Case 7:
                ydsGained = Int(Rnd(1) * 5) + 26

            Case 8:
                ydsGained = Int(Rnd(1) * 11) + 30

            Case 9:
                ydsGained = Int(Rnd(1) * 11) + 40

            Case 10:
                ydsGained = Int(Rnd(1) * 11) + 50

            Case 11:
                ydsGained = Int(Rnd(1) * 11) + 60

            Case 12:
                ydsGained = Int(Rnd(1) * 11) + 70

            Case 13:
                ydsGained = Int(Rnd(1) * 11) + 80

            Case 14:
                ydsGained = Int(Rnd(1) * 11) + 90

        End Select

    End If

End Sub


'----------------------------------------
'      BlitzYardageLong Subroutine
'----------------------------------------
'This subroutine calculates longer gains
'from a play after an unsuccessful blitz
'attempt.
'Yardage ranges from 31 to 101
Sub BlitzYardageLong (I8)

    ' *** BLITZ ***
    Select Case I8

        Case 18:
            ydsGained = Int(Rnd(1) * 21) + 10

        Case 21:
            ydsGained = Int(Rnd(1) * 31) + 10

        Case 25:
            ydsGained = Int(Rnd(1) * 41) + 10

        Case 28:
            ydsGained = Int(Rnd(1) * 51) + 10

        Case 31:
            ydsGained = Int(Rnd(1) * 41) + 20

        Case 33:
            ydsGained = Int(Rnd(1) * 71) + 10

        Case 36:
            ydsGained = Int(Rnd(1) * 91) + 10

        Case 42:
            ydsGained = Int(Rnd(1) * 71) + 30

        Case Else:
            ydsGained = ydsGained + (I4 - 4) * 3

    End Select

End Sub


'----------------------------------------
'           BLITZ2 Subroutine
'----------------------------------------
'This is another routine for calculating
'blitz yardage gains.
Sub BLITZ2 ()

    Call Rolld100(RN4)

    Select Case rushBase

        Case Is <= 15

            Select Case RN4

                Case 1 To 15
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 16 To 60
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 61 To 100
                    ydsGained = Int(Rnd(1) * (rushBase * 2.22)) + 1

            End Select

        Case 16, 17

            Select Case RN4

                Case 1 To 10
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 11 To 50
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 51 To 100
                    ydsGained = Int(Rnd(1) * (rushBase * 1.78)) + 1

            End Select

        Case 18, 19

            Select Case RN4

                Case 1 To 10
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 11 To 40
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 41 To 100
                    ydsGained = Int(Rnd(1) * (rushBase * 1.54)) + 1

            End Select

        Case 20, 21

            Select Case RN4

                Case 1 To 5
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 6 To 30
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 31 To 100
                    ydsGained = Int(Rnd(1) * (rushBase * 1.32)) + 1

            End Select

        Case Else

            Select Case RN4

                Case 1 To 5
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 6 To 20
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 21 To 100
                    ydsGained = Int(Rnd(1) * (rushBase * 1.2)) + 1

            End Select

    End Select

End Sub


'----------------------------------------
'          COMPCHEAT Subroutine
'----------------------------------------
'This subroutine appears to help the comp-
'uter team pick a more useful defense that
'is baseds on the chosen offense. But that
'is somewhat a guess.
Sub COMPCHEAT (defEra)

    coverage = 1

    RN0 = Int(Rnd(1) * 4) + 1

    If RN0 = 2 Then

        Select Case playCall

            Case 1, 2, 6, 8
                'Shot runs up middle / QB sneak
                Select Case rushEra

                    Case 1

                        Select Case defEra

                            Case 1
                                dLine = 1 '7-2 Base

                            Case 2, 3
                                If ydLine > 70 Then
                                    dLine = 4 '6-2 Base
                                Else
                                    dLine = 4 '6-2 Base
                                End If

                        End Select

                    Case 2

                        Select Case defEra

                            Case 1
                                dLine = 4 '6-2 Base

                            Case 2
                                dLine = 8 '5-2 Pinch

                            Case 3
                                If (leagRat_GAME(P1, 7) / 100 > 65) Then
                                    dLine = 4 '6-2 Base
                                Else
                                    dLine = 8 '5-2 Pinch
                                End If

                        End Select

                    Case 0
                        If d100% <= 50 Then
                            dLine = 3 '4-3 Pinch
                        Else
                            dLine = 4 '5-2 Pinch
                        End If

                End Select


            Case 3, 4, 5, 7
                'Wider runs / off tackle
                Select Case rushEra

                    Case 1

                        Select Case defEra

                            Case 1
                                dLine = 2 '7-2 Outside Stunt

                            Case 2, 3
                                If ydLine > 70 Then
                                    dLine = 2 '7-2 Outside Stunt
                                Else
                                    dLine = 6 '6-2 Outside Stunt
                                End If

                        End Select

                    Case 2
                        Select Case defEra

                            Case 1
                                dLine = 6 '6-2 Outside Stunt

                            Case 2
                                dLine = 10 '5-2 Outside Stunt

                        End Select

                    Case 0
                        'dLine = 2   '5-2 Read
                        dLine = 3 '4-3 Pinch

                End Select

            Case 9, 10, 13
                'Short passes
                coverage = 3

            Case 11, 14
                'Medium passes
                If d100% <= 50 Then
                    coverage = 3
                Else
                    coverage = 5
                End If

            Case 12, 15
                'Long Passes
                coverage = 1

            Case 16
                'Sideline pass
                coverage = 2

            Case Is >= 17
                'Rollout / fake
                coverage = 3

        End Select

    End If

End Sub


'----------------------------------------
'       FumbleRecoverPBP Subroutine
'----------------------------------------
'This subroutine handles the Play-by-play
'text for a fumble recovery
Sub FumbleRecoverPBP ()

    pbpString$ = ""

    If defSacks(D) <> 0 Then

        M5 = 0
        RN0 = Int(Rnd(1) * teamSacks(D)) + 1

        For currIdx = 0 To 14

            'Find player for fumble based on contribution
            M5 = M5 + playerSacks(D, currIdx)

            If RN0 <= M5 Then
                pbpString$ = defSacks$(D, currIdx) + " OF "
                Exit For
            End If

        Next

    End If

End Sub


'----------------------------------------
'        CheckGameClock Subroutine
'----------------------------------------
'This subroutine looks to see if the clock
'has expired and adjust the play time and
'possession based on time that was left on
'the clock.
Sub CheckGameClock ()

    If gameClock! <= 0 Then

        TE = gameClock!
        timePoss(P1) = timePoss(P1) + TE
        gameClock! = 0

    End If

End Sub


'----------------------------------------
'       GetDefensiveEra Subroutine
'----------------------------------------
'This subroutine is used to determine what
'general era of football defense we are in
'to make it easier to select formations
Sub GetDefensiveEra (defEra)

    defEra = 0

    If teamYears(impactTeam) <= 1944 Then

        If teamYears(impactTeam) <= 1936 Then
            '7-2 used prior to 1936
            defEra = 1
        Else
            '1937 - 1944
            If (leagRat_GAME(P1, 7) / 100) > 65 Then
                '7-2 for heaviest rushing teams
                defEra = 2
            Else
                '6-2 against run; 5-3 against pass
                defEra = 3
            End If

        End If

    Else

        If ruleOptType = 0 Then
            'College 1945 - 1949
            If teamYears(impactTeam) <= 1949 Then
                defEra = 1
            Else
                'College 1950 - 1956
                defEra = 2
            End If
        Else
            'Pro 1945-1956
            defEra = 3
        End If

    End If

End Sub


'----------------------------------------
'       ComputerDefense Subroutine
'----------------------------------------
'This subroutine determines the defense
'to call based on siutational context, such
'as down, yardage and game clock
Sub ComputerDefense (defEra)

    skipY90 = 0
    scoreDiff = score(P1, 0) - score(D, 0)

    If quarter = 4 And ((scoreDiff >= 10 And gameClock! <= 300) Or scoreDiff >= 17) Then
        'Protect lead late in the game
        coverage = 6
    Else

        If teamYears(impactTeam) >= 1942 And ((quarter = 2 Or quarter >= 4) And gameClock! <= 120) Then

            '2-minute warning: implemented 1942 for NFL
            'Even without it in college, strategy will be about the same
            Call TwoMinDefense(scoreDiff)

        Else

            Select Case currDown

                Case 1

                    If ydsToFirst > 10 Then

                        Call Rolld100(RN0)

                        dLine = 1
                        coverage = 2

                        Select Case RN0

                            Case 1 To 5
                                dLine = 5
                                Call Rolld100(RN1)

                                Select Case RN1

                                    Case 1 To 45: coverage = 7

                                    Case 46 To 90: coverage = 8

                                    Case 91 To 95: coverage = 9

                                    Case 96 To 100: coverage = 10

                                End Select

                            Case 6 To 20
                                dLine = 6
                                coverage = 3

                            Case 21 To 55
                                dLine = 1
                                coverage = 5

                            Case 56 To 90
                                dLine = 2

                            Case 91 To 95
                                coverage = 1

                        End Select

                    End If

                    If ydsToFirst < 10 Then
                        '8867
                        dLine = 1
                        coverage = 1
                        If d100% <= 50 Then
                            dLine = 2
                            coverage = 3
                        Else
                            dLine = 4
                        End If

                        skipY90 = 1

                    End If

                Case 2

                    If ydsToFirst > 8 Then

                        Call Rolld100(RN0)
                        dLine = 1
                        coverage = 2

                        Select Case RN0

                            Case 1 To 5
                                dLine = 5
                                Call Rolld100(RN1)

                                Select Case RN1

                                    Case 1 To 45: coverage = 7

                                    Case 46 To 90: coverage = 8

                                    Case 91 To 95: coverage = 9

                                    Case 96 To 100: coverage = 10

                                End Select

                            Case 6 To 20
                                dLine = 6
                                coverage = 3

                            Case 21 To 55
                                dLine = 1
                                coverage = 5

                            Case 56 To 90
                                dLine = 2

                            Case 91 To 95
                                coverage = 1

                        End Select

                    End If

                    If ydsToFirst < 5 Then
                        '8867
                        dLine = 1
                        coverage = 1
                        If d100% <= 50 Then
                            dLine = 2
                            coverage = 3
                        Else
                            dLine = 4
                        End If

                        skipY90 = 1

                    End If

                Case 3

                    If ydsToFirst >= 3 Then

                        Call Rolld100(RN0)
                        dLine = 1
                        coverage = 2

                        Select Case RN0
                            Case 1 To 5
                                dLine = 5
                                Call Rolld100(RN1)

                                Select Case RN1

                                    Case 1 To 45: coverage = 7

                                    Case 46 To 90: coverage = 8

                                    Case 91 To 95: coverage = 9

                                    Case 96 To 100: coverage = 10

                                End Select

                            Case 6 To 20
                                dLine = 6
                                coverage = 3

                            Case 21 To 55
                                dLine = 1
                                coverage = 5

                            Case 56 To 90
                                dLine = 2

                            Case 91 To 95
                                coverage = 1

                        End Select

                    Else
                        '8867
                        dLine = 1
                        coverage = 1
                        If d100% <= 50 Then
                            dLine = 2
                            coverage = 3
                        Else
                            dLine = 4
                        End If

                        skipY90 = 1

                    End If

                Case 4

                    If ydLine >= 68 Then

                        If (quarter = 4 And score(D, 0) - score(P1, 0) > 10) Or (quarter >= 3 And score(D, 0) - score(P1, 0) > 21) Then

                            Call Rolld100(RN0)
                            dLine = 1
                            coverage = 2

                            Select Case RN0

                                Case 1 To 5
                                    dLine = 5
                                    Call Rolld100(RN1)

                                    Select Case RN1

                                        Case 1 To 45: coverage = 7

                                        Case 46 To 90: coverage = 8

                                        Case 91 To 95: coverage = 9

                                        Case 96 To 100: coverage = 10

                                    End Select

                                Case 6 To 20
                                    dLine = 6
                                    coverage = 3

                                Case 21 To 55
                                    dLine = 1
                                    coverage = 5

                                Case 56 To 90
                                    dLine = 2

                                Case 91 To 95
                                    coverage = 1

                            End Select

                        End If

                    End If

                    If ydsToFirst >= 3 Then

                        Call Rolld100(RN0)
                        dLine = 1
                        coverage = 2

                        Select Case RN0

                            Case 1 To 5
                                dLine = 5
                                Call Rolld100(RN1)

                                Select Case RN1

                                    Case 1 To 45:
                                        coverage = 7

                                    Case 46 To 90:
                                        coverage = 8

                                    Case 91 To 95:
                                        coverage = 9

                                    Case 96 To 100:
                                        coverage = 10

                                End Select

                            Case 6 To 20
                                dLine = 6
                                coverage = 3

                            Case 21 To 55
                                dLine = 1
                                coverage = 5

                            Case 56 To 90
                                dLine = 2

                            Case 91 To 95
                                coverage = 1

                        End Select

                    Else
                        '8867
                        dLine = 1
                        coverage = 1
                        If d100% <= 50 Then
                            dLine = 2
                            coverage = 3
                        Else
                            dLine = 4
                        End If

                        skipY90 = 1

                    End If

                Case Else

                    If d100% <= leagRat_GAME(P1, 7) Then

                        dLine = 1
                        coverage = 1
                        If d100% <= 50 Then
                            dLine = 2
                            coverage = 3
                        Else
                            dLine = 4
                        End If

                    Else

                        Call Rolld100(RN1)
                        dLine = 1
                        coverage = 2

                        Select Case RN1

                            Case 1 To 5
                                dLine = 5
                                Call Rolld100(RN2)

                                Select Case RN2

                                    Case 1 To 45:
                                        coverage = 7

                                    Case 46 To 90:
                                        coverage = 8

                                    Case 91 To 95:
                                        coverage = 9

                                    Case 96 To 100:
                                        coverage = 10

                                End Select

                            Case 6 To 20
                                dLine = 6
                                coverage = 3

                            Case 21 To 55
                                dLine = 1
                                coverage = 5

                            Case 56 To 90
                                dLine = 2

                            Case 91 To 95
                                coverage = 1

                        End Select

                    End If

            End Select

            If skipY90 <> 1 Then
                If ydLine >= 90 And coverage = 5 Or ydsToFirst < 7 And coverage = 5 Then
                    coverage = 3
                End If
            End If

        End If

    End If

    If rushEra > 0 Then
        Call HISTDEFENSE(defEra)
    End If

End Sub


'----------------------------------------
'        TwoMinDefense Subroutine
'----------------------------------------
'This subroutine determines the defense
'that might be used during the last 2
'minutes of a half
Sub TwoMinDefense (scoreDiff)

    'Offense is behind
    If scoreDiff <= 0 Then

        If d100% <= 50 Then

            Select Case Int(Rnd(1) * 3)
                Case 0
                    dLine = 4

                Case 1
                    dLine = 7

                Case 2
                    dLine = 8
            End Select

        Else

            dLine = 3

        End If

        Call Rolld100(RN0)

        Select Case RN0

            Case 1 To 33:
                coverage = 1

            Case 34 To 67:
                coverage = 2

            Case 68 To 100:
                coverage = 5

        End Select

    Else

        'Offense is ahead
        If quarter = 2 And scoreDiff >= 4 Then
            'Protect a lead late in 1st half
            coverage = 6
        Else

            Call Rolld100(RN1)

            Select Case RN1

                Case 1 To 50:
                    dLine = 1
                    If d100% <= 90 Then
                        coverage = 1
                    Else
                        coverage = 2
                    End If

                Case 51 To 70
                    dLine = 2
                    If d100% <= 90 Then
                        coverage = 1
                    Else
                        coverage = 2
                    End If

                Case 71 To 94
                    dLine = 3
                    If d100% <= 90 Then
                        coverage = 1
                    Else
                        coverage = 2
                    End If

                Case 95
                    dLine = 5
                    coverage = 10

                Case 96
                    dLine = 6
                    coverage = 9

                Case 97, 98
                    dLine = 5

                Case 99, 100
                    dLine = 6
                    coverage = 7

            End Select

        End If

    End If

End Sub


'----------------------------------------
'         HISTDEFENSE Subroutine
'----------------------------------------
'This subroutine determines the defense
'to call based on what was common for
'older eras of football
Sub HISTDEFENSE (defEra)

    'Map choices from modern defense era
    'to best equivalents for historic era
    Select Case dLine

        Case 1 To 4
            'Read / Pinch
            Select Case rushEra

                Case 1

                    Select Case defEra

                        Case 1
                            dLine = 1

                        Case 2, 3
                            If ydLine > 70 Then
                                dLine = 1
                            Else
                                dLine = 4
                            End If

                    End Select

                Case 2
                    Select Case defEra

                        Case 1
                            dLine = 1

                        Case 2
                            dLine = 7

                        Case 3
                            If (leagRat_GAME(P1, 7) / 100 > 65) Then
                                dLine = 1
                            Else
                                If dLine = 1 Then
                                    dLine = 7
                                Else
                                    dLine = 8
                                End If
                            End If

                    End Select

            End Select

        Case 5 To 8
            'Inside Stunt | Pass Rush
            Select Case rushEra

                Case 1

                    Select Case defEra

                        Case 1
                            dLine = 2

                        Case 2, 3
                            If ydLine > 70 Then
                                dLine = 2
                            Else
                                dLine = 5
                            End If

                        Case 3
                            dLine = 9

                    End Select

                Case 2
                    Select Case defEra

                        Case 1, 2
                            dLine = 5

                        Case 3
                            If (leagRat_GAME(P1, 7) / 100 > 65) Then
                                dLine = 1
                            Else
                                dLine = 5
                            End If

                    End Select

            End Select

        Case 9, 10
            'Outside Stunt
            Select Case rushEra

                Case 1

                    Select Case defEra

                        Case 1
                            dLine = 3

                        Case 2, 3
                            If ydLine > 70 Then
                                dLine = 3
                            Else
                                dLine = 6
                            End If

                    End Select

                Case 2
                    Select Case defEra

                        Case 1
                            dLine = 3

                        Case 2
                            dLine = 10

                        Case 3
                            If (leagRat_GAME(P1, 7) / 100 > 65) Then
                                dLine = 3
                            Else
                                dLine = 10
                            End If

                    End Select

            End Select

    End Select

End Sub


'----------------------------------------
'       ShowChainMarker Subroutine
'----------------------------------------
'This subroutine draws the "chain" / yard
'marker on the screen
Sub ShowChainMarker ()

    'Determine physical direction based on team and quarter
    'Home team (P1=1): goes right in Q1&Q4, left in Q2&Q3
    'Visiting team (P1=0): goes left in Q1&Q4, right in Q2&Q3
    If P1 = 1 Then
        'Home team
        If quarter Mod 2 = 1 Then
            'Odd quarters - moving right
            markerDir = 1
            J = FindMarkerColumn(ydsToScore) - 3
        Else
            'Even quarters - moving left
            markerDir = 0
            J = FindMarkerColumn(100 - ydsToScore) - 3
        End If
    Else
        'Visiting team
        If quarter Mod 2 = 1 Then
            'Odd quarters - moving left
            markerDir = 0
            J = FindMarkerColumn(100 - ydsToScore) - 3
        Else
            'Even quarters - moving right
            markerDir = 1
            J = FindMarkerColumn(ydsToScore) - 3
        End If
    End If

    midFieldRow = 15
    Locate midFieldRow, 4

    Color 15 * (1 - BW), 2 + 5 * BW

    If J < chainStart Then
        'Change in Possession
        'Arrow goes where team takes over

        'Print / restore yard lines
        Print Left$(markers$, J - 1);

        '10-yard "chains" are not moved / printed

        'Print "ball" possession arrow
        Color 0: Print Chr$(16 + markerDir);

        'Print / restore yard lines
        Color 15 * (1 - BW): Print Mid$(markers$, J + 1, chainStart - J - 1);
        Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, chainStart, 7);
        Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(markers$, 67 - chainStart);

    Else

        If J > chainEnd Then

            'Moving the ball forward
            'Arrow goes in front of the "chains"

            'Print / restore yard lines
            Print Left$(markers$, chainStart - 1);

            'Clear "ball" possession arrow
            Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, chainStart, 7);

            'Print / restore yard lines
            Color 15 * (1 - BW), 2 + 5 * BW: Print Mid$(markers$, chainEnd + 1, J - chainEnd - 1);

            'Print "ball" possession arrow
            Color 0: Print Chr$(16 + markerDir);

            'Print / restore yard lines
            Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(markers$, 73 - J);

        Else
            'Start of a play
            'Arrow goes at beginning of "chains"

            'Print / restore yard lines
            Print Left$(markers$, chainStart - 1);

            'Print 10 yard "chains"
            Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, chainStart, J - chainStart);

            'Print "ball" possession arrow - LEFT
            Color BW * 15: Print Chr$(16 + markerDir);

            'Print / restore yard lines
            Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, J + 1, chainEnd - J);
            Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(markers$, 73 - chainEnd);

        End If

    End If

End Sub


'----------------------------------------
'        SetChainMarker Subroutine
'----------------------------------------
'This subroutine determines where each of
'the elements of the "chain" / yard marker
'should be placed
Sub SetChainMarker ()

    ydsToScore = 100 - ydLine
    YF = -ydsToFirst

    'Calculate chain positions based on team and quarter
    If P1 = 1 Then
        'Home Team
        If quarter Mod 2 = 1 Then
            'Odd quarters - moving right
            chainStart = FindMarkerColumn(ydsToScore + YF) - 3
            chainEnd = chainStart + 6
        Else
            'Even quarters - moving left
            chainEnd = FindMarkerColumn(100 - ydsToScore - YF) - 3
            chainStart = chainEnd - 6
        End If
    Else
        'Visiting Team
        If quarter Mod 2 = 1 Then
            'Odd quarters - moving left
            chainEnd = FindMarkerColumn(100 - ydsToScore - YF) - 3
            chainStart = chainEnd - 6
        Else
            'Even quarters - moving right
            chainStart = FindMarkerColumn(ydsToScore + YF) - 3
            chainEnd = chainStart + 6
        End If
    End If

    Call ShowChainMarker

End Sub


'----------------------------------------
'          MOVEBALL Subroutine
'----------------------------------------
'This subroutine handles the play-by-play
'text for after the ball has moved. With
'that also comes Play-by-Play for the
'tackle / stop.
Sub MOVEBALL ()

    Dim ydLineTemp

    If pbpDelay! <> 0 Then

        I$ = "HE'S AT "

        If endYds <= startYds Then

            For ydsToScore = startYds To endYds Step -1

                Call ShowChainMarker
                _Delay pbpDelay! / 20
                Call YARDLINE
                ydLineTemp = ydLineTeam

                If ydsToScore <= 0 Then
                    ydsToScore = endYds
                Else
                    'We need to be careful not to say someone is at the 0 yard line
                    If Int(ydLineTemp / 10) * 10 = ydLineTemp And ydLineTemp > 0 Then
                        pbpString$ = I$ + "THE" + Str$(ydLineTemp) + " YARD LINE"
                        Call PLACEPBP
                        I$ = ""
                    End If
                End If

            Next ydsToScore

        Else

            For ydsToScore = startYds To endYds

                Call ShowChainMarker
                _Delay pbpDelay! / 20
                Call YARDLINE
                ydLineTemp = ydLineTeam

                If ydsToScore <= 0 Then
                    ydsToScore = endYds
                Else
                    'We need to be careful not to say someone is at the 0 yard line
                    If Int(ydLineTemp / 10) * 10 = ydLineTemp And ydLineTemp > 0 Then
                        pbpString$ = I$ + "THE" + Str$(ydLineTemp) + " YARD LINE"
                        Call PLACEPBP
                        I$ = ""
                    End If
                End If

            Next ydsToScore

        End If

        If ydsToScore <= 0 Then
            ydsToScore = 0
        Else
            ydsToScore = ydsToScore + 1

            Call YARDLINE

            RN0 = Int(Rnd(1) * 22) + 1

            Select Case RN0
                Case 1: I$ = " IS TACKLED"
                Case 2: I$ = " GOES OUT OF BOUNDS"
                Case 3: I$ = " IS HIT"
                Case 4: I$ = " GOES DOWN"
                Case 4: I$ = " IS GANG TACKLED"
                Case 5: I$ = " IS STOPPED"
                Case 6: I$ = " IS TRIPPED UP"
                Case 7: I$ = " IS WRAPPED UP"
                Case 8: I$ = " IS RIDDEN DOWN"
                Case 9: I$ = " IS LEVELED"
                Case 10: I$ = " IS PULLED DOWN"
                Case 11: I$ = " IS HAULED DOWN"
                Case 12: I$ = " IS FORCED OUT"
                Case 13: I$ = " IS SNOWED UNDER"
                Case 14: I$ = " IS THROWN DOWN"
                Case 15: I$ = " IS POPPED"
                Case 16: I$ = " IS SMACKED"
                Case 17: I$ = " IS SLAMMED"
                Case 18: I$ = " IS STOPPED"
                Case 19: I$ = " IS POPPED"
                Case 19: I$ = " STEPS OUT"
                Case 20: I$ = " IS PUSHED OUT"
                Case 21: I$ = " IS KNOCKED OUT OF BOUNDS"
                Case 22: I$ = " IS BROUGHT DOWN"
            End Select

            If ydsToScore >= 100 Then

                pbpString$ = "THE BALL CARRIER" + I$ + " FOR A" + Str$(Int(ydLine)) + " YARD LOSS"

            Else

                If ydsToScore > 0 Then
                    Call ShowChainMarker
                    If ydLineTeam <= 1 Then
                        pbpString$ = "AND HE" + I$ + " AT THE 1 YARD LINE"
                    Else
                        pbpString$ = "AND HE" + I$ + " AT THE" + Str$(Int(ydLineTemp)) + " YARD LINE"
                    End If
                End If

            End If

        End If

        If ydsToScore <> 0 Then
            Call PBP
        End If

    End If

End Sub


'----------------------------------------
'         StatusBoard Subroutine
'----------------------------------------
'This subroutine updates the scoreboard
'with the latest values for the clock,
'and possession location.
Sub StatusBoard ()

    'Screen , , 0, 0

    If Screen(2, 1) <> 219 Then

        Call SETSCREEN
        Call SetChainMarker

    Else

        YF = -ydsToFirst
        ydsToScore = 100 - ydLine

        '-- The value of BW is always 0.
        Color 15 * (1 - BW), 1 + BW * 6
        Locate 4 + P1, 3

        'Print indicator of team with possession
        Print "*";: Locate 5 - P1, 3: Print " ";

        Call YARDLINE
        Call DisplayClockDown

    End If

End Sub


'----------------------------------------
'         ColorChange Subroutine
'----------------------------------------
'This routine checks to see if both teams
'have conflicting primary colors.
'If so, it will substitute the colors
'for the home team by either reversing
'them (with a slight color change) or
'otherwise using White/Grey/Black as
'an alternate color.
'In essence, it's the digital equivalent
'of assigning an alternate jersey color.
Sub ColorChange ()

    ' ** Home Team Colors **
    fg_color = teamRat_GAME(1, 2)
    bg_color = teamRat_GAME(1, 1)

    '-- Black Foreground --
    '-- Blue / Green / Cyan / Red / Magenta Background --
    If fg_color = 0 And ((bg_color >= 1 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to alternate version of background color
        'Change background to Black
        If bg_color < 9 Then
            teamRat_GAME(1, 2) = bg_color + 8
        Else
            teamRat_GAME(1, 2) = bg_color - 8
        End If
        teamRat_GAME(1, 1) = 0
    End If


    '-- Black Foreground --
    '-- Orange / Yellow Background --
    If (fg_color = 0) And (bg_color = 14 Or bg_color = 6) Then
        'Change foreground to background color
        'Change background to foreground color
        teamRat_GAME(1, 2) = bg_color
        teamRat_GAME(1, 1) = fg_color
    End If


    '-- Grey / White Foreground --
    '-- Blue / Green / Cyan / Red / Magenta Background --
    If (fg_color = 7 Or fg_color = 15) And ((bg_color >= 1 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to alternate version of background color
        'Change background to Grey | White
        If bg_color < 9 Then
            teamRat_GAME(1, 2) = bg_color + 8
        Else
            teamRat_GAME(1, 2) = bg_color - 8
        End If
        If fg_color = 7 Then
            teamRat_GAME(1, 1) = 15
        Else
            teamRat_GAME(1, 1) = 7
        End If
    End If


    '-- Grey / White Foreground --
    '-- Black Background --
    If (fg_color = 7 Or fg_color = 15) And (bg_color = 0) Then
        'Change foreground to Black
        'Change background to Grey | White
        teamRat_GAME(1, 2) = 0
        If fg_color = 7 Then
            teamRat_GAME(1, 1) = 15
        Else
            teamRat_GAME(1, 1) = 7
        End If
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- Black Background --
    If ((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And bg_color = 0 Then
        'Change foreground to Black
        'Change background to alternate version of foreground color
        teamRat_GAME(1, 2) = 0
        If fg_color < 9 Then
            teamRat_GAME(1, 1) = fg_color + 8
        Else
            teamRat_GAME(1, 1) = fg_color - 8
        End If
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- White | Grey Background --
    If ((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And (bg_color = 7 Or bg_color = 15) Then
        'Change foreground to Grey | White
        'Change background to foreground color
        If bg_color = 7 Then
            teamRat_GAME(1, 2) = 15
        Else
            teamRat_GAME(1, 2) = 7
        End If

        teamRat_GAME(1, 1) = fg_color
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- Blue / Green / Cyan / Red / Magenta Background --
    If ((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And ((bg_color >= 1 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to alternate version of background color
        'Change background to alternate version of foreground color
        If bg_color < 9 Then
            teamRat_GAME(1, 2) = bg_color + 8
        Else
            teamRat_GAME(1, 2) = bg_color - 8
        End If
        If fg_color < 9 Then
            teamRat_GAME(1, 1) = fg_color + 8
        Else
            teamRat_GAME(1, 1) = fg_color - 8
        End If

    End If


    '-- Orange / Yellow Foreground
    '-- Black / Blue / Green / Cyan / Red / Magenta Background --
    If (fg_color = 14 Or fg_color = 6) And ((bg_color >= 0 And bg_color <= 5) Or (bg_color >= 9 And bg_color <= 13)) Then
        'Change foreground to background color
        'Change background to foreground color
        teamRat_GAME(1, 2) = bg_color
        teamRat_GAME(1, 1) = fg_color
    End If


    '-- Orange / Yellow Foreground
    '-- Grey / White Background --
    If (fg_color = 14 Or fg_color = 6) And (bg_color = 7 Or bg_color = 15) Then
        'Change foregroundto background color
        'Change background to Grey | White
        teamRat_GAME(1, 2) = bg_color
        If fg_color = 7 Then
            teamRat_GAME(1, 1) = 15
        Else
            teamRat_GAME(1, 1) = 7
        End If
    End If


    '-- Blue / Green / Cyan / Red / Magenta Foreground --
    '-- Orange / Yellow Background
    If (((fg_color >= 1 And fg_color <= 5) Or (fg_color >= 9 And fg_color <= 13)) And (bg_color = 14 Or bg_color = 6)) Then
        'Change foregroundto background color
        'Change background to foreground color
        teamRat_GAME(1, 2) = bg_color
        teamRat_GAME(1, 1) = fg_color
    End If


    '-- Grey / White Foreground --
    '-- Orange / Yellow Background
    If (fg_color = 7 Or fg_color = 15) And (bg_color = 14 Or bg_color = 6) Then
        'Change foregroundto background color
        'Change background to Grey | White
        teamRat_GAME(1, 2) = bg_color

        'teamRat_GAME(1, 1) = 0
        If fg_color = 7 Then
            teamRat_GAME(1, 1) = 15
        Else
            teamRat_GAME(1, 1) = 7
        End If
    End If


End Sub


'----------------------------------------
'       DisplayClockDown Subroutine
'----------------------------------------
'Outputs current clock time and quarter,
'as well as down and yardage on the StatusBoard
Sub DisplayClockDown

    Color 15 * (1 - BW), 4 + BW * 3

    TI = gameClock!
    TI = TI + TI * (TI < 0)
    I = Int(TI / 60)

    Call TICKERUPDATE

    J = TI - I * 60

    qtrTime! = I + J / 100

    Locate 3, 44:
    If quarter <= 4 Then
        Print Using " TIME: ##.##     QUARTER: #  "; qtrTime!, quarter;
    Else
        'Quarter >= 5 (Aka OVERTIME)
        'Sudden Death To Win, NCAA, Mod SD Postseason
        If (overtimeOpt >= 2 And overtimeOpt <= 5) Or overtimeOpt = 8 Then
            'No Time Limit (Don't Track Clock)
            Print " TIME:  -.--     QUARTER: OT ";
        Else
            Print Using " TIME: ##.##     QUARTER: OT "; qtrTime!;
        End If

    End If

    Locate 3, 53: Print ":";
    Locate , 73: Print Tab(80); " ";

    Locate 4, 44: Print Using " DOWN: #"; currDown;
    Print Tab(60);

    If ydsToFirst >= 0.7 Then
        Print Using " TO GO: ##"; CInt(ydsToFirst);
    Else
        Print " TO GO: INCHES";
    End If

    Print Tab(80); " ";

    Locate 5, 44: Print " BALL ON ";
    If ydLine <= 50 Then
        Print "OWN";
    Else
        Print "OPP";
    End If

    Print Using " ## YD LINE "; CInt(ydLineTeam);
    Print Tab(80); " ";

End Sub


'----------------------------------------
'           DEFMENU Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'selecting their defensive choices.
Sub DEFMENU (defEra)

    If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then
        'Computer on Defense or PC vs PC

        Call ComputerDefense(defEra)
        Call DEF_CheckGoalLine

    Else

        Do

            goodSelection = 1

            'Determine Line Formation
            Call FRAMEOPTION(10, D)
            Print "SELECT DEFENSIVE LINE PLAY "; gameTeams$(D)

            'If teamYears(impactTeam) <= 1945 Then
            'Historic menu

            'The problem with this is dLine doesn't align
            'in any way with the current menu
            '    For dLine = 1 To 4:
            '        Locate 10 + dLine, 14
            '        Print dLine - 1; "- "; defFormEraA$(dLine); Tab(41); dLine - 1; "- "; defFormEraA$(dLine)
            '    Next dLine

            'Else

            For dLine = 1 To 5:
                Locate 10 + dLine, 14
                Print dLine - 1; "- "; defPlay$(dLine); Tab(41); dLine + 4; "- "; defPlay$(dLine + 5)
            Next dLine

            Print
            Locate , 27: Print "G - GOAL LINE (5 YL AND IN)"

            If timeouts(D) > 0 Then
                Locate , 27: Print "T - CALL TIMEOUT"
            End If

            Do
                I$ = GetKeyPress$

                If UCase$(I$) = Chr$(17) Then
                    Cls
                    End
                End If

                If UCase$(I$) = "G" And ydLine >= 94.999 Then
                    dLine = 13
                    Screen , , 0, 0
                    Exit Sub
                End If

                If UCase$(I$) = "T" Then
                    Call TimeoutCalled(D, periodOver)
                    Call CheckGameClock
                    If gameClock! = 0 Then Exit Sub
                    'If periodOver = 1 Then GoTo 3360
                End If

                dLine = Val(I$)

            Loop Until dLine >= 0 And dLine <= 9 And UCase$(I$) <> "G" And UCase$(I$) <> "T"

            'Adjust dLine to match
            'plays lined out from menu
            dLine = dLine + 1

            Screen , , 0, 0

            'End If

            'Determine Pass Coverage
            Call FRAMEOPTION(10, D)
            Print "SELECT PASS COVERAGE "; gameTeams$(D)

            For covChoice = 1 To 5
                Locate 10 + covChoice, 14
                Print covChoice - 1; "- "; passCov$(covChoice); Tab(41); covChoice + 4; "- "; passCov$(covChoice + 5)
            Next covChoice

            Locate , 41: Print " B - BLITZ ENGAGE"

            goodChoice = 0

            Do

                J$ = GetKeyPress$

                'Main loop condition
                If J$ <> Chr$(27) Then
                    If UCase$(I$) = "B" Then coverage = 10: Exit Do
                    coverage = Val(J$)
                End If

                If coverage >= 0 And coverage <= 9 Or I$ = Chr$(27) Then
                    goodChoice = 1
                End If

                If teamYears(D) <= 1960 And (coverage = 5 Or coverage = 6) Then
                    'These coverages would have only been used in the 1960's and on
                    pbpString$ = "THESE WERE FIRST USED BY TEAMS IN THE 1960's; CHOOSE AGAIN"
                    Call PBP
                    goodChoice = 0
                End If

            Loop Until goodChoice = 1

            If J$ <> Chr$(27) Then

                ' 3065
                coverage = coverage + 1
                Screen , , 0, 0

                'More loop conditions
                '(5-2 Pass Rush Or Inside Stunt) AND (Blitz ILB or Blitz Engage)
                If (dLine = 7 Or dLine = 8) And (coverage = 7 Or coverage = 9 Or coverage = 11) Then
                    goodSelection = 0
                End If

                '(5-2 Inside Stunt Or 4-3 Outside Stunt) AND (Blitz OLB or Blitz Engage)
                If (dLine = 9 Or dLine = 10) And (coverage = 8 Or coverage = 10 Or coverage = 11) Then
                    goodSelection = 0
                End If

            End If

        Loop Until goodSelection = 1

    End If

    'Nickel coverage has it's
    'own rush effectiveness
    If coverage = 5 Then
        dLine = 11
    End If

    'Prevent coverage has it's
    'own rush effectiveness
    If coverage = 6 Then
        dLine = 12
    End If

End Sub


'----------------------------------------
'         DEGFOALLINE Subroutine
'----------------------------------------
'This subroutine automatically picks a
'goal line defense when 5 yards are left.
Sub DEF_CheckGoalLine ()

    If (quarter >= 9 And overtimeOpt = 4) Then
        'Confirm that we are here correctly
        'and that goal line is being chosen
        'If so, then we just need to make it
        'more effective, or need to make the
        'offense less effective as time drags
        test = 1
    End If

    If ydLine > 94.999 Then
        dLine = 13
        coverage = 1
    End If

End Sub


'----------------------------------------
'         Turnover_PAT Subroutine
'----------------------------------------
'Handle return of turnover in PAT attempt
'This could either be one of the two:
'- An INT in the Endzone (2 Pt Conv)
'- A blocked kick (PAT)
Sub Turnover_PAT

    ydLine = 1
    ydsToScore = 99

    Call CHANGEPOSS

    Call ShowChainMarker
    Call YARDLINE

    If d100% <= 18 Then
        ydsGained = 110
    Else
        ydsGained = Int(Rnd(1) * 21) + 50
    End If

    Call YARDAGE

    scoreX1 = 12

    startYds = 100 - ydLine
    endYds = startYds - ydsGained
    Call MOVEBALL

    pbpString$ = "RETURNED FOR"

    If ydsGained + ydLine > 99.99 Then

        pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
        Call PBP

        score(P1, quarter) = score(P1, quarter) + 2
        score(P1, 0) = score(P1, 0) + 2

        pbpString$ = "GOOD FOR 2 POINTS!!"
        Call PBP

        Call SETSCREEN
        Call SCOREUPD

    Else

        convYds! = CInt((ydsGained) * 10) / 10

        pbpString$ = pbpString$ + Str$(Int(convYds)) + " YARDS"
        Call PBP

    End If

    ydLine = ydsGained
    ydsToScore = 100 - ydLine

    endZone = 0

End Sub


'----------------------------------------
'         GetPATChoice Subroutine
'----------------------------------------
'This subroutine handles the computer's
'choice for a TD conversion, or lets
'a human user make their choice as applicable.
Sub GetPATChoice (userChoice)

    'Default value (no choice made)
    userChoice = 0

    If ruleOptType = 4 Then
        'WFL = No PAT
        userChoice = 2
    Else

        'NFL - Prior to 1994 OR College Rules
        If ruleOptType = 1 And (ruleOptPro < 4 Or ruleOptColl >= 0) Then

            'If the user has chosen modern NCAA overtime,
            'respect their choice and alternate 2-pt conversion
            If (quarter >= 7 And overtimeOpt = 4) Or (quarter >= 6 And overtimeOpt = 5) Then
                userChoice = 2
            Else
                '2 PT conversion not a rule
                userChoice = 1
            End If

        Else

            If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

                ' College Overtime
                ' Force 2-point conversion based on year
                ' 2019-2020 = 3rd OT (Q = 7)
                ' 2021+ = 2nd OT (Q = 6)
                If (quarter >= 7 And overtimeOpt = 4) Or (quarter >= 6 And overtimeOpt = 5) Then

                    userChoice = 2

                Else

                    pbpString$ = "1-KICK EP 2-GO FOR TWO"
                    Call PBP

                    ' *** Computer on Offense, or PC vs. PC ***

                    'By default, go for one
                    userChoice = 1

                    'Determine score deficit
                    scoreDiff = score(P1, 0) - score(D, 0)

                    ' ***** When to "Go for TWO" *****

                    If ruleOptType = 1 And ruleOptPro >= 4 Then
                        'NFL, Post 1994 (when 2 pt conversion was implemented)
                        ' 4th quarter, down by 2 and 2 minutes left
                        ' 4th quarter, down by 9 and 5 minutes left
                        ' 4th quarter, down by 6 and 7 minutes left
                        If quarter = 4 And (scoreDiff = -9 And gameClock! < 600 Or scoreDiff = -2 And gameClock! < 120 Or scoreDiff = -5 And gameClock! < 420) Then
                            userChoice = 2
                        End If

                        'Mod Sudden Death, Down by 1
                        'That is, 1st team scored; a 2-pt conversion is the only way to win
                        If isOT = 1 And overtimeOpt = 8 And scoreDiff = -1 Then
                            userChoice = 2
                        End If

                    Else

                        'College / AFL / WLAF / USFL
                        ' Decide based on down / score

                        ' 3rd / 4th quarter, down by 18
                        ' 4th quarter, down by 16
                        If (quarter >= 3 And scoreDiff = -18) Or (quarter = 4 And scoreDiff = -16) Then
                            userChoice = 2
                        Else
                            '1) down by 2
                            '2) ahead by 5 | 12
                            If scoreDiff = -2 Or scoreDiff = 1 Or scoreDiff = 5 Or scoreDiff = 12 Then
                                userChoice = 2
                            Else
                                'Anytime outside the final 2-min warning
                                If Not (quarter = 4 And gameClock! > 240 Or quarter < 4) Then
                                    'down by 1 | 4 | 5 | 8 | 10 | 13
                                    If scoreDiff = -1 Or scoreDiff = -4 Or scoreDiff = -5 Or scoreDiff = -8 Or scoreDiff = -10 Or scoreDiff = -13 Then
                                        userChoice = 2
                                    End If
                                End If

                            End If

                        End If

                    End If

                End If

            Else

                ' *** Human player on Offense ***
                pbpString$ = "1-KICK EP 2-GO FOR TWO"

                Do
                    Call PBP
                    I$ = GetKeyPress$
                    userChoice = Val(I$)
                Loop Until userChoice = 1 Or userChoice = 2

            End If

        End If

    End If

End Sub


'----------------------------------------
'          LongGainPBP Subroutine
'----------------------------------------
'This subroutine chooses the play-by-play
'text for the start of a play where any
'long gain has been calculated.
Sub LongGainPBP ()

    RN0 = Int(Rnd(1) * 20) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE BREAKS FREE"
        Case 2: pbpString$ = "HE DODGES A TACKLER"
        Case 3: pbpString$ = "HE STIFF ARMS A TACKLER"
        Case 4: pbpString$ = "HE SPRINTS DOWN THE SIDELINE"
        Case 5: pbpString$ = "HE'S GOT DAYLIGHT"
        Case 6: pbpString$ = "HE CUTS BACK"
        Case 7: pbpString$ = "HE CUTS UPFIELD"
        Case 8: pbpString$ = "HE DARTS THROUGH A GAPING HOLE"
        Case 9: pbpString$ = "HE HAS LOTS OF ROOM"
        Case 10: pbpString$ = "HE JUKES PAST A DEFENDER"
        Case 11: pbpString$ = "HE GOES INTO TURBO MODE"
        Case 12: pbpString$ = "HE SHIFTS INTO HIGH GEAR"
        Case 13: pbpString$ = "HE BREAKS IT BIG!!"
        Case 14: pbpString$ = "HE BURSTS INTO OPEN FIELD"
        Case 15: pbpString$ = "HE RUNS WILD"
        Case 16: pbpString$ = "HE POPS A BIG ONE"
        Case 17: pbpString$ = "HE BUSTS ONE"
        Case 18: pbpString$ = "HE FIGHTS THROUGH THE LINE"
        Case 19: pbpString$ = "HE SHAKES OFF THE DEFENSE"
        Case 20: pbpString$ = "HE SEES A BREAK"
    End Select

    Call PBP

End Sub


'----------------------------------------
'          RunForTD_PBP Subroutine
'----------------------------------------
'This subroutine generates the Play-by-Play
'for a rushing TD.
Sub RunForTD_PBP ()

    RN0 = Int(Rnd(1) * 29) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE BREAKS FREE"
        Case 2: pbpString$ = "HE DODGES A TACKLER"
        Case 3: pbpString$ = "HE STIFF ARMS A TACKLER"
        Case 4: pbpString$ = "HE SPRINTS DOWN THE SIDELINE"
        Case 5: pbpString$ = "HE SEES DAYLIGHT"
        Case 6: pbpString$ = "HE'S GOT DAYLIGHT"
        Case 7: pbpString$ = "HE CUTS UPFIELD"
        Case 8: pbpString$ = "HE DARTS THROUGH A GAPING HOLE"
        Case 9: pbpString$ = "HE HAS LOTS OF ROOM"
        Case 10: pbpString$ = "HE BREAKS IT"
        Case 11: pbpString$ = "HE TURNS ON THE JETS"
        Case 12: pbpString$ = "HE SHIFTS INTO HIGH GEAR"
        Case 13: pbpString$ = "HE BREAKS IT BIG!!"
        Case 14: pbpString$ = "HE BURSTS INTO OPEN FIELD"
        Case 15: pbpString$ = "HE RUNS WILD"
        Case 16: pbpString$ = "HE POPS A BIG ONE"
        Case 17: pbpString$ = "HE BUSTS ONE"
        Case 18: pbpString$ = "HE FIGHTS THROUGH THE LINE"
        Case 19: pbpString$ = "HE SHAKES OFF THE DEFENSE"
        Case 20: pbpString$ = "HE SEES A BREAK"
        Case 21: pbpString$ = "HE'S GONE!"
        Case 22: pbpString$ = "NO ONE IS CLOSE TO CATCHING HIM"
        Case 23: pbpString$ = "SEE YOU LATER!!"
        Case 24: pbpString$ = "HE GETS THE CORNER"
        Case 25: pbpString$ = "HE COULD WALK THAT ONE IN"
        Case 26: pbpString$ = "HE'LL GO THE DISTANCE"
        Case 27: pbpString$ = "THERE HE GOES!!"
        Case 28: pbpString$ = "HE COULD..GO..ALL..THE..WAY!"
        Case 29: pbpString$ = "HE'S PAST THE SECONDARY"
    End Select

    Call PBP

End Sub


'----------------------------------------
'         SETWEATHER Subroutine
'----------------------------------------
'This subroutine presents the user a menu
'that helps set the wind and weather options
Sub SETWEATHER ()

    Call SETSCREEN

    Call OPTIONBOX(9)

    Print "WEATHER OPTIONS "
    Locate 12, 14
    Locate , 14: Print "0 - GOOD"
    Locate , 14: Print "1 - BAD (RAIN/SNOW)"
    Locate , 14: Print "2 - TERRIBLE (BLIZZARD/HEAVY RAIN)"
    Locate , 14: Print "3 - COMPUTER SELECT"

    If autoPlay = 1 Then
        'Read weather from schedule options
        choice = schedOptions(20)
    Else

        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3 Or I$ = "0"

    End If

    'Screen , , 0, 0

    'Figure out which team to favor
    'for trends happening over decades.
    'Currently this is only for the
    'impact weather has on pass comp %.
    If gameLoc = 2 Then
        impactTeam = 0
    Else
        impactTeam = gameLoc
    End If

    lightPrecip = 0
    heavyPrecip = 0

    Select Case choice

        Case 1
            lightPrecip = 1
            heavyPrecip = 0

        Case 2
            lightPrecip = 0
            heavyPrecip = 1

        Case 3

            Call Rolld100(RN1)

            Select Case RN1

                Case 20 To 38
                    'Snow affects 7% of games
                    'Light rain affects 12% of games
                    lightPrecip = 1
                    heavyPrecip = 0

                Case 7 To 12
                    'Blizzards affect 1% of games
                    'Heavy rain affects 5% of games
                    lightPrecip = 0
                    heavyPrecip = 1

            End Select

    End Select

    If lightPrecip = 1 Then

        'Bad Weather
        climate(4) = -1
        climate(5) = 12
        Select Case teamYears(impactTeam)

            Case Is <= 1989
                climate(6) = 6

            Case 1990 To 1999
                climate(6) = 5

            Case 2000 To 2009
                climate(6) = 4

            Case Is >= 2010
                climate(6) = 3

        End Select

    End If

    If heavyPrecip = 1 Then

        climate(4) = -2
        climate(5) = 2
        Select Case teamYears(impactTeam)

            Case Is <= 1989
                climate(6) = 11

            Case 1990 To 1999
                climate(6) = 9

            Case 2000 To 2009
                climate(6) = 8

            Case Is >= 2010
                climate(6) = 6

        End Select

    End If

    Call OPTIONBOX(9)

    strongWind = 0
    lightWind = 0

    Print "WIND OPTIONS "
    Locate 12, 14
    Locate , 14: Print "0 - NONE                        "
    Locate , 14: Print "1 - LIGHT TO MODERATE (< 15 MPH)"
    Locate , 14: Print "2 - STRONG AND GUSTY (>= 15 MPH)"
    Locate , 14: Print "3 - COMPUTER SELECT             "

    If autoPlay = 1 Then
        choice = schedOptions(19)
    Else
        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 0 And choice <= 3
    End If

    'Screen , , 0, 0

    Select Case choice

        Case 1
            lightWind = 1
            strongWind = 0

        Case 2
            lightWind = 0
            strongWind = 1

        Case 3

            Call Rolld100(RN1)

            Select Case RN1

                Case 1 To 26
                    'Light wind impacts 27% of games
                    lightWind = 1
                    strongWind = 0

                Case 45 To 60
                    'Strong wind impacts 15% of games
                    lightWind = 0
                    strongWind = 1

            End Select

    End Select

    If lightWind = 1 Then

        'Light Wind
        'Distance impacted by 5-10 yards
        climate(1) = 0 - Int(Rnd(1) * 5) + 5

        Select Case teamYears(impactTeam)

            Case Is <= 1989
                climate(2) = -3
                '5%-8%
                climate(3) = 0 - Int(Rnd(1) * 4) + 5

            Case 1990 To 1999
                climate(2) = -3
                '5%-7%
                climate(3) = 0 - Int(Rnd(1) * 3) + 5

            Case 2000 To 2009
                climate(2) = -3
                '4%-6%
                climate(3) = -5

            Case Is >= 2010
                climate(2) = -2
                climate(3) = -4

        End Select

    End If

    If strongWind = 1 Then

        'Strong Wind
        'Distance impacted by 10-15 yards
        climate(1) = 0 - Int(Rnd(1) * 5) + 10
        Select Case teamYears(impactTeam)

            Case Is <= 1989
                '7%-10%
                climate(2) = 0 - Int(Rnd(1) * 4) + 7
                '15%-20%
                climate(3) = 0 - Int(Rnd(1) * 6) + 15

            Case 1990 To 1999
                '6%-8%%
                climate(2) = -7
                '12%-18%
                climate(3) = 0 - Int(Rnd(1) * 7) + 12

            Case 2000 To 2009
                '5%-7%
                climate(2) = 0 - Int(Rnd(1) * 3) + 5
                '10-15%
                climate(3) = 0 - Int(Rnd(1) * 6) + 10

            Case Is >= 2010
                '4%-6%
                climate(2) = -5
                '8%-12%
                climate(3) = 0 - Int(Rnd(1) * 5) + 8

        End Select

    End If

End Sub


'----------------------------------------
'            FGATT Subroutine
'----------------------------------------
'This subroutine tracks and increments FG
'attempts based on yardage increments for
'the current kicker.
Sub FGATT ()

    fgAttYds = CInt(117 + goalPostAdj - ydLine)

    Select Case fgAttYds

        Case 1 To 19
            playerFGA(P1, activeKicker(P1), 0) = playerFGA(P1, activeKicker(P1), 0) + 1

        Case 20 To 29
            playerFGA(P1, activeKicker(P1), 1) = playerFGA(P1, activeKicker(P1), 1) + 1

        Case 30 To 39
            playerFGA(P1, activeKicker(P1), 2) = playerFGA(P1, activeKicker(P1), 2) + 1

        Case 40 To 49
            playerFGA(P1, activeKicker(P1), 3) = playerFGA(P1, activeKicker(P1), 3) + 1

        Case Is >= 50
            playerFGA(P1, activeKicker(P1), 4) = playerFGA(P1, activeKicker(P1), 4) + 1

    End Select

End Sub


'----------------------------------------
'            FGMADE Subroutine
'----------------------------------------
'This subroutine tracks and increments FG
'made based on yardage increments for
'the current kicker.
Sub FGMADE ()

    If fgAttYds > gameLongFGAtt(P1, activeKicker(P1)) Then
        gameLongFGAtt(P1, activeKicker(P1)) = fgAttYds
    End If

    Select Case fgAttYds

        Case 1 To 19
            playerFGM(P1, activeKicker(P1), 0) = playerFGM(P1, activeKicker(P1), 0) + 1

        Case 20 To 29
            playerFGM(P1, activeKicker(P1), 1) = playerFGM(P1, activeKicker(P1), 1) + 1

        Case 30 To 39
            playerFGM(P1, activeKicker(P1), 2) = playerFGM(P1, activeKicker(P1), 2) + 1

        Case 40 To 49
            playerFGM(P1, activeKicker(P1), 3) = playerFGM(P1, activeKicker(P1), 3) + 1

        Case Is >= 50
            playerFGM(P1, activeKicker(P1), 4) = playerFGM(P1, activeKicker(P1), 4) + 1

    End Select

End Sub


'----------------------------------------
'       ComputerOffense Subroutine
'----------------------------------------
'This subroutine determines the offensive
'plays to call for the computer. Decisions
'are carefully made based on several factors.
Sub ComputerOffense ()

    ' *** INITIALIZATION & CONSTANTS ***
    Dim inFGRange, inLongFGRange

    'So 50 yarder is attempted from Opp 33 (67)
    fgAttLine = (117 - fgLimit% + goalPostAdj)
    inFGRange = fgAttLine - 3
    inLongFGRange = fgAttLine + 10

    scoreDiff = score(P1, 0) - score(D, 0)
    teamRushPct = leagRat_GAME(P1, 7)

    Print #11, , "DEBUG: Computer is choosing offense. Situation: RuleType="; ruleOptType; "; Down="; currDown; "; Yards="; ydsToFirst; "; YardLine="; ydLine; "; YdsToScore="; ydsToScore

    'Increase run % based on AI tendency for
    'special scenarios favoring a pass choice
    If ruleOptType = 0 Then

        Select Case teamRushPct

            Case Is <= 44
                rushAdjAI = 10

            Case 45 To 54
                rushAdjAI = 20

            Case 55 To 57
                rushAdjAI = 29

            Case Is >= 58
                rushAdjAI = 18

        End Select

    Else

        Select Case teamRushPct

            Case Is <= 44
                rushAdjAI = 20

            Case 45 To 54
                rushAdjAI = 30

            Case 55 To 57
                rushAdjAI = 40

            Case Is >= 58
                rushAdjAI = 24

        End Select

    End If

    'Adjust rush % to account for how QB sneaks
    'are handled in the code
    teamRushPct = CInt((teamRushPct * 0.86))

    '--------------------
    ' 2-POINT CONVERSION
    '--------------------
    If endZone = 1 Then

        Call Rolld100(RN0)

        Select Case RN0

            'Rollout Pass
            Case 1 To 25:
                playCall = 17

                ' Medium Pass
            Case 26 To 50:
                playCall = 11

                ' Short Pass
            Case 51 To 70:
                playCall = 10

                ' Option
            Case 71 To 80:
                playCall = 5

                ' Sweep
            Case 81 To 90:
                playCall = 4

                ' Off Tackle
            Case 91 To 100:
                playCall = 3

        End Select

        Exit Sub

        Print #11, , "DEBUG: Computer chose offense. 2-pt Conversion. PlayCall="; playCall

    End If


    '--------------------
    ' 4th QUARTER
    '--------------------

    'Offense leading, Defense no timeouts,
    'Under 2 minutes; time left to take knee each down
    'Teams didn't use victory formation until the 1980s
    If quarter = 4 And timeouts(D) = 0 And score(P1, 0) > score(D, 0) And gameClock! <= 35 * (4 - currDown) And teamYears(P1) >= 1979 Then

        ' *** VICTORY FORMATION ***
        playCall = 9
        playType = 1
        qbTakeKnee = 1

        Exit Sub

        Print #11, , "DEBUG: Computer chose offense. Victory Formation. PlayCall="; playCall

    End If

    If ((quarter = 2 And ydLine > 40) Or quarter >= 4) And gameClock! <= 120 Then

        ' *** TWO-MINUTE WARNING ***
        '---------------------------

        'Down by >= 4
        If score(D, 0) - score(P1, 0) > 3 Then
            If quarter >= 4 Then
                GoTo 8880
            End If
            If quarter = 2 Then
                GoTo 8885
            End If
        End If

        'Score Tied or Down
        If score(D, 0) - score(P1, 0) >= 0 Then
            GoTo 8885
        End If

        '2nd Qtr, Past Own 40 Yd Line
        If ydLine > 40 And quarter = 2 Then
            GoTo 8885
        End If

        If d100% <= 60 Then
            playCall = 3
        Else
            playCall = 4
        End If

        'In FG range, < 22 secs to go, 2nd Qtr | 4th Qtr Tie | Overtime
        If ydLine >= inFGRange And gameClock! <= 22 And (quarter = 2 Or quarter > 4 Or (quarter = 4 And scoreDiff = 0)) Then
            'Field Goal
            Print #11, , "DECISION: Field Goal, late in Q2/Q4"
            playCall = 20
        End If

        '4th Down, >= 3 Yds to go, In FG range, Modern Era (FGs uncommon before 1960)
        If currDown = 4 And ydsToFirst >= 3 And ydLine >= inFGRange And ydLine < 87 And _ 
            (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
            'Field Goal
            Print #11, , "DECISION: Field Goal, 4th down, >=3 Yds"
            playCall = 20
        End If

        'Outside LONG FG range, 4th Down
        If currDown = 4 And ydLine < inFGRange Then
            'Punt
            playCall = 19
        End If

    Else

        ' *** REGULAR STRATEGY ***
        '---------------------------
        'Safety Prevention (Inside Own 10)
        If currDown <= 3 And ydLine <= 10 Then
            Call Offense_PreventSafety
            GoTo 8880
        End If 'Done checking if in danger of safety

        '-------------------------
        ' DOWN AND DISTANCE LOGIC
        '-------------------------
        If ruleOptType = 0 Then
            'Balanced College teams run about 50%
            rushPctThreshold = 65
        Else
            'Balanced Pro teams run about 55%
            rushPctThreshold = 55
        End If

        '----------------
        '### 4TH DOWN ###
        '----------------
        If currDown = 4 Then

            'Determine if we should go for it

            ' *** Long Yardage ***
            '---------------------
            '4th Qtr+, Inside Opponent's field, Down by 11+
            If quarter >= 4 And ydLine >= 50 And score(D, 0) - score(P1, 0) > 10 Then
                'Average teams pass on 4th when down
                If teamRushPct < rushPctThreshold Then
                    GoTo 8833
                End If
            End If

            '3rd Qtr+, Inside Opponent's 45, Down by 15+
            If quarter >= 3 And ydLine >= 55 And score(D, 0) - score(P1, 0) > 14 Then
                'Average teams run with only 4 yds to go
                If teamRushPct >= rushPctThreshold And ydsToFirst <= 4 Then
                    GoTo 8827
                Else
                    GoTo 8833
                End If
            End If

            'Down by 4 or More, Inside Red Zone
            If currDown = 4 And quarter = 4 And score(D, 0) - score(P1, 0) >= 4 And ydLine >= 80 + goalPostAdj Then
                'Average teams run with only 3 yds to go
                If teamRushPct >= rushPctThreshold And ydsToFirst <= 3 Then
                    GoTo 8827
                Else
                    GoTo 8833
                End If
            End If

            ' *** Short Yardage ***
            '----------------------
            '(Everyone runs)

            '1 Yd To Go, In FG range, 3 Pt. Lead
            If ydsToFirst <= 1 And ydLine >= inFGRange And ydLine < 87 And scoreDiff <= 3 And scoreDiff > 0 Then
                'Go for it!
                GoTo 8827
            End If

            '<= 2 Yds to Go, Long FG range, Down by 11+
            If quarter >= 4 And ydLine >= inLongFGRange And ydLine < inFGRange + goalPostAdj And ydsToFirst <= 2 And score(D, 0) - score(P1, 0) >= 11 Then
                GoTo 8827
            End If

            '< 3 Yds to Go, Inside Opponent's 40
            If currDown = 4 And (ydsToFirst < 3 And ydLine >= 60) Or (ydsToFirst < 2 And ydLine >= 50) Then
                GoTo 8827
            End If


            ' *** Field Goal Decisions ***
            '------------------------------

            '4th Qtr | OT
            '---------------
            'Modern Era (FGs uncommon before 1960)
            If (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then

                'In FG range, Score = 3 Pt. Margin,
                If quarter >= 4 And ydLine >= inFGRange And scoreDiff <= 3 And scoreDiff >= -3 Then
                    'Field Goal
                    Print #11, , "DECISION: Field Goal, 4th Quarter, 4th Down, Within 3 Pts"
                    playCall = 20
                    GoTo 8800
                End If

                'In FG range, Score = 3 Pt. Deficit
                If quarter >= 4 And ydLine >= inFGRange And score(D, 0) - score(P1, 0) <= 3 Then
                    'Field Goal
                    Print #11, , "DECISION: Field Goal, 4th Quarter, 4th Down, Down 3 Pts"
                    playCall = 20
                    GoTo 8800
                End If

                '<5 Minutes, Long FG range, Down by 1-10
                If quarter >= 4 And gameClock! < 300 And ydLine >= inLongFGRange And ydLine < inFGRange + goalPostAdj And _
                    score(D, 0) - score(P1, 0) <= 10 And score(D, 0) - score(P1, 0) > 0 Then

                    If ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960 Then
                        Print #11, , "DECISION: Field Goal, 4th Quarter, <5 Mins, Down by 1-10"
                        playCall = 20
                    Else
                        playCall = 19
                    End If
                    GoTo 8800
                End If

                '>= 3 Yds To Go, <= 49 Yd Att
                If currDown = 4 And ydsToFirst >= 3 Then
                    'Take all attempts < 49 yards; 40% of attempts 50-59 yds
                    If (ydLine >= 68) Or (ydLine >= 67 And ydLine <= 58 And d100% < 40) Then
                        'Field Goal
                        Print #11, , "DECISION: Field Goal, 4th Quarter, >= 3 Yds, Normal Attempt"
                        '-- Most field goals are done here
                        playCall = 20
                        GoTo 8800
                    End If
                End If

            End If

            '<5 Minutes, Down by 4 to 11, Inside Red Zone
            If quarter = 4 And gameClock! > 240 And score(D, 0) - score(P1, 0) >= 4 And score(D, 0) - score(P1, 0) <= 11 And ydLine > 80 + goalPostAdj Then
                'Field Goal
                Print #11, , "DECISION: Field Goal, 4th Quarter, 4th Down, <5 Mins, Down by 4-11, Red Zone"
                playCall = 20
                GoTo 8800
            End If

            ' *** Default to Punt ***
            '------------------------
            'Outside FG range
            If currDown = 4 And ydLine < fgAttLine + 3 Then
                'Punt
                '-- Most punts are done here
                playCall = 19
                GoTo 8800
            End If

            ' *** Go-for-it Conditions ***
            '-----------------------------
            '4th Down
            If currDown = 4 Then
                If ydsToFirst < 2 Then
                    GoTo 8827
                End If
                If ydsToFirst >= 4 Then
                    GoTo 8833
                End If
            End If

        End If


        '----------------------
        '### 1ST - 3RD DOWN ###
        '----------------------
        'Extreme run-heavy team (70%+)
        d1Thresh = 70
        d2Thresh = 80

        If (currDown = 1 And teamRushPct >= d1Thresh) Or (currDown <= 2 And teamRushPct >= d2Thresh) Then
            GoTo 8827
        End If

        ' *** Short Yardage ***
        '----------------------
        '(Everyone runs)

        'All Team's - Inside 8 Yd Line
        If ydLine <= 8 Then
            GoTo 8827
        End If

        ' 2nd/3rd Down, Very Short Yardage
        d2Thresh = 3

        If (currDown = 2 And ydsToFirst < d2Thresh) Or (currDown = 3 And ydsToFirst < 2) Then
            GoTo 8827
        End If

        'Consolidated check for heavier rush teams
        If teamRushPct >= rushPctThreshold Then

            If ruleOptType = 0 Then
                ' COLLEGE: Tighter windows (more plays fall through)
                If (currDown = 2 And ydsToFirst >= 3 And ydsToFirst <= 8) Or _
                   (currDown = 3 And ydsToFirst >= 2 And ydsToFirst <= 3) Then
                    GoTo 8827
                End If
                ' 2nd-and-9+: Falls through
                ' 3rd-and-4+: Falls through
            Else
                ' NFL: Wider windows (current behavior)
                If (currDown = 2 And ydsToFirst >= 3 And ydsToFirst <= 12) Or _
                   (currDown = 3 And ydsToFirst >= 2 And ydsToFirst <= 5) Then
                    GoTo 8827
                End If
                ' 2nd-and-13+: Falls through
                ' 3rd-and-6+: Falls through
            End If
            
        End If


        ' *** Long Yardage ***
        '----------------------
        '1st Down, >= 11 Yds
        If (currDown = 1 And ydsToFirst > 10) And teamRushPct <= rushPctThreshold Then
            'Balanced / passing team will mostly pass
            GoTo 8833
        End If

        'Very run-heavy teams (60+) caught by earlier check
        d2Thresh = 10
        
        If currDown = 2 And ydsToFirst >= d2Thresh Then
            If teamRushPct > 60 And ydsToFirst <= 12 Then
                'Run on 2nd 10-12 Yds
                GoTo 8827
            ElseIf teamRushPct < 50 Then
                'Pass-oriented teams forced to pass
                GoTo 8833
            End If
            '50-60% teams fall through
        End If

        '3rd Down, Medium/Long (4+)
        If currDown = 3 And ydsToFirst >= 4 Then
            'Run-heavy teams (55+) run 3rd-and-short-medium
            If teamRushPct >= rushPctThreshold And ydsToFirst <= 5 Then
                GoTo 8827
            ElseIf teamRushPct < 50 Then
                'Pass-oriented teams forced to pass
                GoTo 8833
            End If
            ' 50-55% teams on 3rd-and-4/5 fall through
            ' Everyone on 3rd-and-6+ falls through
        End If

        ' *** Decided Based on Rush % ***
        '--------------------------------
        '2nd-and-3 to 9
        '3rd-and-2 to 3
        'All remaining situations

        'Adjust rush % based on the recent success of
        'plays called against the opponent
        sequencingMod = GetPlaySequencingModifier%
        adjustedRushPct = teamRushPct + sequencingMod

        'Ensure adjustment stays within reasonable bounds
        If adjustedRushPct < 15 Then
            adjustedRushPct = 15
        End If
        If adjustedRushPct > 90 Then
            adjustedRushPct = 90
        End If

        'Determine rush / pass decision
        baseAdj! = adjustedRushPct + rushAdjAI
        lateGameBoost! = leagRat_GAME(P1, 7) / 2

        Call Rolld100(RN0)

        If (RN0 > baseAdj!) Or _
           (RN0 >= baseAdj! - lateGameBoost! And (quarter = 4 And gameClock! <= 480 And score(D,0) - score(P1,0) >= 9)) Then
            GoTo 8833
        Else
            GoTo 8827
        End If

    End If


    '=============================
    '   PLAY SELECTION BRANCHES
    '=============================
    8827 '
    If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then
        If gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3)) > (teamRushPct + rushAdjAI) / 100 And (teamRushPct + rushAdjAI) < (50 + rushAdjAI) Then
            'Team has rushed more than normal - PASS!
            GoTo 8833
        End If
    End If

    Call Offense_ShortYardage

    GoTo 8800

    8833 '
    Call Offense_LongYardage

    ' Only adjust if we should use quick passing
    If ShouldUseQuickPassing% = 1 Then
        Call AdjustPassPlayForSequencing
    End If

    GoTo 8800


    '-------------------------------------
    ' *** DECISION MADE - FINAL CHECKS ***
    '-------------------------------------
    8880 '
    '2nd Qtr, 4th Down
    If quarter = 2 And currDown = 4 Then

        'Outside FG range
        If ydLine < inFGRange Then
            'Punt
            playCall = 19
        Else
            'In FG range, Modern Era (FGs uncommon before 1960)
            If (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                'Field Goal
                Print #11, , "DECISION: Field Goal, 2nd Quarter, 4th Down"
                playCall = 20
            End If
        End If

        GoTo 8800

    End If

    'Any Qtr
    '---------------
    '< 22 secs to go, No TOs (Off) | < 12 secs to go, TOs Remaining (Off)
    If (gameClock! <= 22 And timeouts(P1) = 0) Or _ 
        (gameClock! <= 12 And timeouts(P1) > 0) Then
        'Pass
        playCall = 12
        GoTo 8800
    End If

    If ydLine > 10 Then
        Call AdjustForSituation
    End If

    GoTo 8800


    '---------------------------------------
    ' 2-Min Warning Special Teams Decisions
    '---------------------------------------
    8885 '
    'Outside FG range, 4th Down, 2nd Qtr --OR-- 4th Qtr + Ahead or Tied
    If ydLine < inFGRange And currDown = 4 And (quarter = 2 Or (quarter >= 4 And score(P1, 0) >= score(D, 0))) Then
        'Punt
        playCall = 19
        GoTo 8800
    End If

    '2nd Qtr Only
    '4th Down, In FG range, Modern Era (FGs uncommon before 1960)
    If ydLine >= inFGRange And quarter = 2 And currDown = 4 And _
        (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
        'Field Goal
        Print #11, , "DECISION: Field Goal, 2-Min Warning, 4th Down, In Range"
        playCall = 20
        GoTo 8800
    End If

    'Any Qtr
    '---------------
    'Outside FG range, < 37 secs to go + No TOs (Off)
    'Outside FG range, < 22 secs to go + TOs Remaining (Off)
    If ydLine < inFGRange And _
        (gameClock! <= 37 And timeouts(P1) = 0) Or _
        (gameClock! <= 12 And timeouts(P1)) > 0 Then
        'Pass
        playCall = 12
        GoTo 8800
    End If

    'In FG range, < 22 secs to go, No TOs (Off)
    If ydLine >= inFGRange And gameClock! <= 22 And timeouts(P1) = 0 Then
        'Field Goal
        Print #11, , "DECISION: Field Goal, Late Game, No TOs, In Range"
        playCall = 20
        GoTo 8800
    End If

    'In FG range, < 12 secs to go, TOs Remaining (Off)
    If ydLine >= inFGRange And gameClock! <= 12 And timeouts(P1) > 0 Then
        'Field Goal
        Print #11, , "DECISION: Field Goal, Late Game, TOs, In Range"
        playCall = 20
        GoTo 8800
    End If

    'In FG range, 4th Down, Modern Era (FGs uncommon before 1960)
    If ydLine >= inFGRange And currDown = 4 And _ 
        (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
        'Field Goal
        Print #11, , "DECISION: Field Goal, 4th Down, In Range"
        playCall = 20
        GoTo 8800
    End If

    'If we haven't set a play by now, use two-minute drill logic
    If currDown < 4 Then
        Call AdjustForSituation ' Your two-minute play selection routine
        GoTo 8800
    End If


    '=============================
    '      FINAL ADJUSTMENTS
    '=============================
    8800 '
    If ydLine >= 80 Then
        Call AdjustForRedZone
    End If

    If coverage = 6 Then
        Call AdjustForPreventDefense
    End If

    If Not (climate(1) = 0) And Not (climate(4) = 0) Then
        Call AdjustForWeather
    End If

    Call AdjustForEra(teamYears(P1))

    '--> Make a routine here
    ' these are special teams adjustments ?!?!

    'Sudden Death OT, <= 49 yard FG attempt
    If isOT = 1 And overtimeOpt = 2 And (ydLine >= 68) And currDown <= 3 Then
        'Do run plays to setup the FG
        playCall = 1
    End If

    'College | Sudden Death Modified
    '4th Down = Prioritize Score over Punts / FG
    If isOT = 1 And overtimeOpt >= 3 And currDown = 4 Then

        'Punt, College OT - just go for it!
        If playCall = 19 And overtimeOpt >= 3 And overtimeOpt <= 5 Then
            playCall = 12
        End If

        'Punt chosen but score tied
        If playCall = 19 And score(D, 0) = score(P1, 0) Then
            playCall = 12
        End If

        'Punt chosen, Ahead 3 or less (Off)
        'Opponent could win by a FG on their possession.
        If playCall = 19 And score(D, 0) - score(P1, 0) <= 3 Then
            'If turnover on downs won't give opponent
            'a 50 yd FG attempt, we should go for it.
            'In Mod SD, just go for it because it will be
            'our last possession.
            If ((overtimeOpt = 1 Or overtimeOpt = 2) And ydLine >= 32) Or (overtimeOpt >= 6) Then
                playCall = 12
            End If
        End If

        'Punt or FG chosen, Down by 4 or more (Off)
        '(ie, we have to try and get a 1st or score)
        If (playCall = 19 Or playCall = 20) And score(D, 0) - score(P1, 0) > 3 Then
            playCall = 12
        End If

        'Sudden Death Postseason, <= 39 Yd FG
        'Prefer going for the score
        If overtimeOpt = 8 And (ydLine >= 78) Then
            playCall = 12
        End If

    End If

    'FG Over 40 yards, heavy winds, kicker accuracy < 80%
    kickerAvgPct = (kickerFGA(P1, activeKicker(P1)) * 5) + 5
    If playCall = 20 And ydLine > 78 And climate(1) = -15 And kickerAvgPct < 80 Then
        playCall = 12
    End If

    Print #11, , "DEBUG: Computer chose offense. Normal Exit. PlayCall="; playCall

    '--- Uncomment to see what the play call %'s are
    '-- Call pbplog(12977)

End Sub


'----------------------------------------
'    Offense_PreventSafety Subroutine
'----------------------------------------
'This subroutine determines the best plays
'to call on Offense to get some breathing
'room when in danger of a safety
Sub Offense_PreventSafety

    'If team has run the ball more than average...
    If gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3)) > ((teamRushPct + rushAdjAI) / 100) And (teamRushPct + rushAdjAI) < (50 + rushAdjAI) Then

        'Check for pass
        Call Rolld100(RN1)

        Select Case RN1

            '3-Step Drop
            Case 1 To 25:
                playCall = 9

                'Short Pass
            Case 26 To 60:
                playCall = 10

                'Screen
            Case 61 To 100:
                playCall = 13

        End Select

    Else

        Call Rolld100(RN1)

        Select Case RN1

            Case 1 To 20
                'QBSneak
                playCall = 8

            Case 21 To 60
                'Blast
                playCall = 1

            Case 61 To 100
                'Dive
                playCall = 2

        End Select

    End If

End Sub


'----------------------------------------
'     Offense_ShortYardage Subroutine
'----------------------------------------
'This subroutine determines the best plays
'to call on Offense for average situations.
'The decisions are a blend of running and
'passing to keep teams guessing.
Sub Offense_ShortYardage

    If ydsToFirst <= 1.32 Then

        'QB Sneak
        If d100% <= GetSneakChance% Then
            playCall = 8
        End If

    Else

        Call Rolld100(RN1)

        'Check for play-action opportunity
        If lastPlayType = 0 And lastPlayYards! >= 5 And teamRushPct >= 45 Then

            'Last play was successful run - favor play-action
            Select Case RN1

                'Long PA Pass
                Case 1 To 10:
                    playCall = 15
                    
                    'Medium PA Pass
                Case 11 To 25:
                    playCall = 14
                    
                    'Option
                Case 26 To 35:
                    playCall = 5
                    
                    'Sweep
                Case 36 To 45:
                    playCall = 4
                    
                    'Trap
                Case 46 To 55:
                    playCall = 7
                    
                    'Off Tackle
                Case 56 To 70:
                    playCall = 3
                    
                    'Dive
                Case 71 To 85:
                    playCall = 2
                    
                    'Blast
                Case 86 To 100:
                    playCall = 1

            End Select
                
        Else

            ' Normal play selection
            Select Case RN1

                'Long PA Pass
                Case 1 To 2:
                    If teamRushPct >= 45 Then
                        playCall = 15
                    Else
                        playCall = 12
                    End If

                    'Medium PA Pass
                Case 3 To 5:
                    If teamRushPct >= 45 Then
                        playCall = 14
                    Else
                        playCall = 11
                    End If

                    'Option
                Case 6 To 10:
                    playCall = 5

                    'Sweep
                Case 11 To 20:
                    playCall = 4

                    'Trap
                Case 21 To 30:
                    playCall = 7

                    'Off Tackle
                Case 31 To 50:
                    playCall = 3

                    'Dive
                Case 51 To 70:
                    playCall = 2

                    'Blast
                Case 71 To 100:
                    playCall = 1

            End Select

        End If

    End If

    'Tush Push sneak for the Eagles has a chance of sometimes getting closer to 2 yards
    'Even if the team doesn't get the 2 yards, they will often try it again!
    If qbacks$(P1, qbNumber(P1)) = "J.HURTS" And _ 
        (gameTeams$(P1) = "22 PHILADELPHIA" Or gameTeams$(P1) = "23 PHILADELPHIA" Or gameTeams$(P1) = "24 PHILADELPHIA" Or gameTeams$(P1) = "25 PHILADELPHIA") And _
        ydsToFirst <= 2 Then
        playCall = 8
    End If

End Sub


'----------------------------------------
'     Offense_LongYardage Subroutine
'----------------------------------------
'This subroutine determines the best plays
'to call on Offense for more desparate
'scenarios. The decisions are mostly pass
'plays, with only a draw play to try and
'reset the defense at times.
Sub Offense_LongYardage

    Call Rolld100(RN1)

    Select Case RN1

        ' Draw
        Case 1 To 5:
            playCall = 6

            ' Rollout
        Case 6 To 30:
            playCall = 17

            ' 3-Step Drop
        Case 31 To 36:
            playCall = 9

            ' Screen
        Case 37 To 44:
            playCall = 13

            ' Long Pass
        Case 45 To 63:
            playCall = 12

            ' Medium Pass
        Case 64 To 89:
            playCall = 11

            ' Short Pass
        Case 90 To 100:
            playCall = 10

    End Select

    '3rd / 4th Down, >= 5 Yds To Go
    If currDown >= 3 And ydsToFirst >= 5 Then

        '-- If Draw was selected, change to Long Pass
        If playCall = 6 Then playCall = 12

        '-- If Short Pass was selected, change to Medium Pass
        If playCall = 10 Then playCall = 11

        '-- If 3-Step Drop or screen was selected, change to Rollout
        If (playCall = 13 Or playCall = 9) Then playCall = 17

    End If

    'Inside Opponent's 10 or < 7 Yds To Go
    '-- If Long Pass was selected, change to Medium Pass
    If (playCall = 12 Or playCall = 15) And (ydLine > 90 Or ydsToFirst < 7) Then
        playCall = 11
    End If

End Sub


'----------------------------------------
'      AdjustForSituation Subroutine
'----------------------------------------
'This focuses on offensive play calling
Sub AdjustForSituation ()

    Call Rolld100(randPlay)

    ' Adjust strategy based on timeouts remaining
    Select Case timeouts(P1)

        Case 0
            ' NO TIMEOUTS - Heavily favor sideline passes (stops clock)
            Select Case randPlay

                ' Sideline Pass - 55% (most important!)
                Case 1 To 55:
                    playCall = 16
                
                    ' Long Pass - 15% (incompletion stops clock)
                Case 56 To 70:
                    playCall = 12
                
                    ' Screen - 10% (safe, quick play)
                Case 71 To 80:
                    playCall = 13
                
                    ' Medium Pass - 10%
                Case 81 To 90:
                    playCall = 11
                
                    ' Draw - 10% (defense expects pass, safe yards)
                Case 91 To 100:
                    playCall = 6
            End Select

        Case 1
            ' 1 TIMEOUT - Balanced approach
            Select Case randPlay

                ' Sideline Pass - 45%
                Case 1 To 45:
                    playCall = 16
                
                    ' Medium Pass - 20%
                Case 46 To 65:
                    playCall = 11
                
                    ' Long Pass - 15%
                Case 66 To 80:
                    playCall = 12
                
                    ' Screen - 12%
                Case 81 To 92:
                    playCall = 13
                
                    ' Draw - 8%
                Case 93 To 100:
                    playCall = 6

            End Select

        Case Is >= 2
            ' 2+ TIMEOUTS - Can use middle of field more
            Select Case randPlay

                ' Sideline Pass - 35%
                Case 1 To 35:
                    playCall = 16
                
                    ' Medium Pass - 25% (middle okay with TOs)
                Case 36 To 60:
                    playCall = 11
                
                    ' Screen - 15% (great opening play for 2-min drill)
                Case 61 To 75:
                    playCall = 13
                
                    ' Long Pass - 15%
                Case 76 To 90:
                    playCall = 12
                
                    ' Draw - 10% (keep defense honest)
                Case 91 To 100:
                    playCall = 6

            End Select
        
    End Select

    ' Situational Adjustments
    ' -----------------------
    'Inside Opponent's 10 = Medium Pass
    If ydLine >= 90 Then
        playCall = 11
    End If

    ' Deep in own territory (< 30 yd line) - favor safer plays
    If ydLine < 30 Then
        ' Change long passes to medium
        If playCall = 12 Then playCall = 11
        ' Change draws to screens (safer)
        If playCall = 6 Then playCall = 13
    End If
    
    ' Inside Red Zone, close score - can run to set up scoring play
    If ydLine > 80 And Abs(score(D, 0) - score(P1, 0)) <= 3 Then
        ' 25% chance of running play (Blast/Dive/Off Tackle/Sweep)
        If d100% <= 25 Then
            playCall = Int(Rnd(1) * 4) + 1
        End If
    End If
    
    ' Very low clock situations (<15 seconds, no timeouts)
    ' Force sideline or incompletion-friendly plays
    If gameClock! < 15 And timeouts(P1) = 0 Then
        If playCall <> 16 And playCall <> 12 Then
            ' Switch to sideline pass
            playCall = 16
        End If
    End If

End Sub


'----------------------------------------
'       AdjustForRedZone Subroutine
'----------------------------------------
'This subroutine adjusts play calls when
'in the red zone to reflect real team
'tendencies.
Sub AdjustForRedZone ()

    Dim zoneDepth As Integer
    Dim adjustRoll As Integer

    ' Calculate how deep in red zone (0-19 yards from goal)
    zoneDepth = ydLine - 80

    ' Red Zone adjustments based on play type
    Select Case playCall

        'Power runs work BETTER in red zone - increase chance slightly
        Case 1, 2, 3, 7
            ' If close to goal (inside 5) and in obvious run situation
            ' Keep the run, but maybe change type for variety
            If ydLine >= 95 And currDown >= 3 And ydsToFirst <= 3 Then
                ' 30% chance to change to QB Sneak
                If d100% <= 30 And ydsToFirst <= 1.32 Then
                    'QB Sneak
                    playCall = 8
                End If
            End If


            'Outside runs: Less room to work on perimeter
        Case 4, 5
            ' Inside the 10: 50% chance to change to power run
            If ydLine >= 90 And d100% <= 50 Then

                adjustRoll = d100%

                Select Case adjustRoll

                    Case 1 To 40
                        'Off Tackle
                        playCall = 3

                    Case 41 To 70
                        'Blast
                        playCall = 1

                    Case 71 To 100
                        'Dive
                        playCall = 2
                End Select

            End If

        Case 11, 14
            'Inside the 5: Change to short pass or rollout
            If ydLine >= 95 Then
                If d100% <= 50 Then
                    'Short Pass
                    playCall = 10
                Else
                    'Rollout
                    playCall = 17
                End If
            End If

            'Deep shots don't work in compressed field
        Case 12, 15
            'Inside the 10: Always change to medium
            If ydLine >= 90 Then
                'Medium Pass
                playCall = 11

            Else
                ' 10-20 yard line: 70% chance to change to medium
                If zoneDepth < 10 And d100% <= 70 Then
                    'Medium Pass
                    playCall = 11
                End If
            End If

            'Screens less effective in compressed field
        Case 13
            'Inside the 10: 60% chance to change
            If ydLine >= 90 And d100% <= 60 Then
                If d100% <= 50 Then
                    'Short Pass
                    playCall = 10
                Else
                    'Draw
                    playCall = 6
                End If
            End If


            'Sideline routes harder near goal line (less room)
        Case 16
            'Inside the 5: 40% chance to change to rollout or short pass
            If ydLine >= 95 And d100% <= 40 Then
                If d100% <= 50 Then
                    'Rollout
                    playCall = 17
                Else
                    'Short Pass
                    playCall = 10
                End If
            End If
    
    End Select
    
    'Additional situational red zone adjustments
    
    ' Goal-to-go situations: favor power football
    If ydsToFirst >= zoneDepth And playCall >= 9 And playCall <= 17 Then

        ' On goal-to-go, 25% chance to change pass to run
        If d100% <= 25 Then

            adjustRoll = d100%

            Select Case adjustRoll

                Case 1 To 30
                    'QB Sneak (if close)
                    playCall = 8

                Case 31 To 55
                    'Blast
                    playCall = 1

                Case 56 To 80
                    'Dive
                    playCall = 2

                Case 81 To 100
                    'Off Tackle
                    playCall = 3

            End Select

        End If

    End If
    
    ' 3rd/4th down and goal from inside the 3
    ' Favor QB sneak even more
    If currDown >= 3 And ydLine >= 97 And _ 
        ydsToFirst <= 3 And d100% <= 40 Then
        'QB Sneak
        playCall = 8
    End If

End Sub


'----------------------------------------
'   AdjustForPreventDefense Subroutine
'----------------------------------------
'This subroutine adjusts offensive play calls
'when facing prevent defense (soft coverage).
Sub AdjustForPreventDefense ()

    Dim adjustRoll As Integer
    
    ' Prevent defense strategy:
    ' - Safeties playing deep (long passes won't work)
    ' - Linebackers dropping back (draws are effective)
    ' - Cornerbacks playing soft (intermediate routes open)
    ' - Goal: Take what defense gives, methodical drives
    
    Select Case playCall
        
        ' ====================================
        ' LONG PASSES - Convert to Medium
        ' ====================================
        'Long Pass
        Case 12
            '80% of time, change to medium pass
            '20% of time, keep (sometimes you catch prevent sleeping)
            If d100% <= 80 Then
                'Medium Pass
                playCall = 11
            End If
        
            'Long PA Pass
        Case 15
            'Play-action less effective vs prevent (no one biting)
            'Change to medium pass or rollout
            If d100% <= 60 Then
                'Medium Pass
                playCall = 11
            Else
                'Rollout
                playCall = 17
            End If
        
            ' ====================================
            ' SCREENS - Less Effective
            ' ====================================
            'Screen
        Case 13
            'Prevent defense plays soft, screens need hard rush
            '50% convert to draw or short pass
            If d100% <= 50 Then

                If d100% <= 60 Then
                    'Draw (better vs prevent)
                    playCall = 6
                Else
                    'Short Pass
                    playCall = 10
                End If

            End If

            ' ====================================
            ' OUTSIDE RUNS - Convert Some to Draws
            ' ====================================
            'Sweep
        Case 4
            'Outside runs okay, but draws better vs soft defense
            '30% convert to draw
            If d100% <= 30 Then
                'Draw
                playCall = 6
            End If
        
            'Option
        Case 5
            'Option less effective when LBs dropping back
            '40% convert to draw or dive
            If d100% <= 40 Then

                If d100% <= 60 Then
                    'Draw
                    playCall = 6
                Else
                    'Dive
                    playCall = 2
                End If

            End If
        
            ' ====================================
            ' POWER RUNS - Keep As-Is
            ' ====================================
        Case 1, 2, 3, 7, 8
            'Blast, Dive, Off Tackle, Trap, QB Sneak
            'These work fine vs prevent - no adjustment
        
            ' ====================================
            ' MEDIUM PASSES - Ideal vs Prevent
            ' ====================================
        Case 11, 14
            'Medium Pass, Medium PA Pass
            'Perfect for prevent - keep as-is
            'These are your bread and butter
        
            ' ====================================
            ' SHORT PASSES - Good vs Prevent
            ' ====================================
        Case 9, 10
            '3-Step Drop, Short Pass
            'Work well vs prevent - keep as-is
        
            ' ====================================
            ' ROLLOUT - Effective vs Prevent
            ' ====================================
        Case 17 ' Rollout
            'QB can read soft coverage, find holes
            'Keep as-is, very effective
        
            ' ====================================
            ' SIDELINE PASS - Situational
            ' ====================================
        Case 16 ' Sideline Pass
            'In two-minute drill, keep as-is (clock management)
            'Otherwise, 30% chance to convert to medium pass
            If gameClock! > 120 Or quarter < 2 Then

                If d100% <= 30 Then
                    'Medium Pass
                    playCall = 11
                End If

            End If
        
            ' ====================================
            ' DRAW - Perfect vs Prevent
            ' ====================================
        Case 6 ' Draw
            'Draw is ideal vs prevent - always keep
        
            ' ====================================
            ' SPECIAL TEAMS - No Adjustment
            ' ====================================
        Case 19, 20
            'Punt, Field Goal - no change
    
    End Select
    
    ' Additional strategic consideration:
    ' If in red zone vs prevent, favor runs more
    ' (Defense giving up yards but protecting end zone)
    If ydLine >= 80 Then
        'In red zone, convert some passes to runs
        Select Case playCall

            Case 10, 11 ' Short/Medium Pass
                '25% chance to run instead
                If d100% <= 25 Then

                    adjustRoll = d100%

                    Select Case adjustRoll

                        'Blast
                        Case 1 To 40: playCall = 1

                            'Dive
                        Case 41 To 70: playCall = 2

                            'Off Tackle
                        Case 71 To 90: playCall = 3

                            'Draw
                        Case 91 To 100: playCall = 6

                    End Select

                End If

        End Select

    End If

End Sub


'----------------------------------------
'       AdjustForWeather Subroutine
'----------------------------------------
'This subroutine adjusts play calls based on
'weather conditions.
Sub AdjustForWeather ()
    
    ' Determine weather conditions
    If climate(1) = -15 Then hasStrongWind = 1
    If climate(1) = -7 Then hasLightWind = 1
    If climate(4) = -1 Then hasModPrecip = 1
    If climate(4) = -2 Then hasHeavyPrecip = 1
    
    ' Calculate weather severity (0-3 scale)
    ' This determines strength of adjustments
    weatherSeverity = 0
    If hasLightWind = 1 Then
        weatherSeverity = weatherSeverity + 1
    End If
    If hasStrongWind = 1 Then
        weatherSeverity = weatherSeverity + 2
    End If
    If hasModPrecip = 1 Then
        weatherSeverity = weatherSeverity + 1
    End If
    If hasHeavyPrecip = 1 Then
        weatherSeverity = weatherSeverity + 2
    End If
    
    ' Cap worst conditions at 3 (strong wind + heavy precip = 4)
    If weatherSeverity > 3 Then weatherSeverity = 3
    
    ' ========================================
    ' LONG PASSES - Most Affected by Wind
    ' ========================================
    ' Wind makes deep balls unpredictable
    
    Select Case playCall

        'Long Pass
        Case 12

            Select Case weatherSeverity

                Case 1
                    changeChance = 30

                Case 2
                    changeChance = 55

                Case 3
                    changeChance = 80

            End Select

            If d100% <= changeChance Then playCall = 11
        
            'Long PA Pass
        Case 15

            Select Case weatherSeverity

                Case 1
                    changeChance = 35

                Case 2
                    changeChance = 60

                Case 3
                    changeChance = 85

            End Select

            If d100% <= changeChance Then playCall = 14
            
    End Select
    
    ' ========================================
    ' MEDIUM PASSES - Moderately Affected
    ' ========================================
    'Challenging, but more manageable

    Select Case playCall

        'Medium Pass
        Case 11

            'Primarily affected by heavy conditions
            If weatherSeverity >= 2 Then

                If d100% <= (weatherSeverity * 12) Then

                    Call Rolld100(adjustRoll)

                    If adjustRoll <= 60 Then
                        playCall = 10 ' Short Pass
                    Else
                        playCall = 2 ' Dive
                    End If

                End If

            End If
        
            'Medium PA Pass
        Case 14

            If weatherSeverity >= 2 Then

                If d100% <= (weatherSeverity * 15) Then

                    Call Rolld100(adjustRoll)

                    If adjustRoll <= 50 Then
                        'Short Pass
                        playCall = 10
                    Else
                        'Blast
                        playCall = 1
                    End If

                End If

            End If

    End Select
    
    ' ========================================
    ' SCREENS - Affected by Precipitation
    ' ========================================
    ' Timing/catching issues in rain/snow
    
    'Screen
    If playCall = 13 Then

        'More affected by precipitation than wind
        If hasModPrecip = 1 Or hasHeavyPrecip = 1 Then

            If hasHeavyPrecip = 1 Then
                changeChance = 50
            Else
                changeChance = 30
            End If
            
            If d100% <= changeChance Then

                Call Rolld100(adjustRoll)

                If adjustRoll <= 50 Then
                    'Draw
                    playCall = 6
                Else
                    'Short Pass
                    playCall = 10
                End If

            End If

        End If

    End If
    
    ' ========================================
    ' SIDELINE PASSES - Very Affected by Wind
    ' ========================================
    ' Wind affects throws to boundary
    
    'Sideline Pass
    If playCall = 16 Then

        'Primarily a wind issue
        If hasStrongWind = 1 Then

            If d100% <= 45 Then

                Call Rolld100(adjustRoll)

                If adjustRoll <= 60 Then
                    'Short Pass
                    playCall = 10
                Else
                    'Rollout
                    playCall = 17
                End If

            End If

        ElseIf hasLightWind = 1 Then

            If d100% <= 25 Then
                'Short Pass
                playCall = 10
            End If

        End If

    End If
    
    ' ========================================
    ' SHORT PASSES - Slight Risk
    ' ========================================
    ' Quick passes still work but some risk

    Select Case playCall

        '3-Step Drop, Short Pass
        Case 9, 10

            'Only affected by severe conditions
            If weatherSeverity >= 3 Then

                If d100% <= 20 Then

                    Call Rolld100(adjustRoll)

                    Select Case adjustRoll

                        Case 1 To 35:
                            'Dive
                            playCall = 2

                        Case 36 To 70:
                            'Blast
                            playCall = 1

                        Case 71 To 100:
                            'Off Tackle
                            playCall = 3

                    End Select

                End If

            End If

    End Select

    ' ========================================
    ' ROLLOUT - Still Effective
    ' ========================================
    ' QB can adjust to conditions

    'Rollout
    If playCall = 17 Then
        ' No change - QB mobility helps adjust to weather
    End If
    
    ' ========================================
    ' OUTSIDE RUNS - Affected by Precipitation
    ' ========================================
    ' Footing issues on perimeter
    
    Select Case playCall

        'Sweep, Option
        Case 4, 5

            'Precipitation makes footing difficult
            If hasModPrecip = 1 Or hasHeavyPrecip = 1 Then

                If hasHeavyPrecip = 1 Then
                    changeChance = 35
                Else
                    changeChance = 20
                End If

                If d100% <= changeChance Then

                    adjustRoll = d100%

                    Select Case adjustRoll

                        'Blast
                        Case 1 To 40: playCall = 1

                            'Dive
                        Case 41 To 70: playCall = 2

                            'Off Tackle
                        Case 71 To 100: playCall = 3

                    End Select

                End If

            End If

    End Select

    ' ========================================
    ' POWER RUNS - Unaffected or Better
    ' ========================================
    ' Inside runs work well in any weather
    Select Case playCall

        'Blast, Dive, Off Tackle, Trap, Draw
        Case 1, 2, 3, 7, 6
            'Keep as-is - these are ideal
            'Defense has same footing issues

    End Select
    
    ' ========================================
    ' QB SNEAK - Unaffected
    ' ========================================
    If playCall = 8 Then
        ' Weather doesn't matter for sneaks
    End If

End Sub


'----------------------------------------
'         AdjustForEra Subroutine
'----------------------------------------
'This subroutine adjusts play calls based on
'the era/year of the team. Different decades had
'vastly different offensive philosophies.
Sub AdjustForEra (teamYear)
    
    ' ========================================
    ' PRE-1960: VERY RUN-HEAVY ERA
    ' ========================================
    ' Limited passing concepts, simple plays
    ' T-Formation, Single Wing dominance
    If teamYear < 1960 Then
        
        ' Complex passing plays didn't exist yet
        Select Case playCall
            
            'Screen, Rollout
            Case 13, 17
                'These plays weren't common, change to simpler passes
                If d100% <= 60 Then
                    'Short Pass
                    playCall = 10
                End If
            
                'Long Pass
            Case 12
                'Deep passing rare, change to medium
                If d100% <= 50 Then
                    'Medium Pass
                    playCall = 11
                End If
            
                'Medium PA, Long PA
            Case 14, 15
                'Play-action less developed
                If d100% <= 40 Then
                    If playCall = 15 Then
                        'Long PA → Medium Pass
                        playCall = 11
                    Else
                        'Medium PA → Short Pass
                        playCall = 10
                    End If
                End If

        End Select
        
    End If
    
    ' ========================================
    ' 1960-1977: TRANSITION ERA
    ' ========================================
    ' Passing game developing, more balanced
    ' AFL influence, vertical passing emerges
    
    If teamYear >= 1960 And teamYear < 1978 Then
        
        'Screens still relatively uncommon
        If playCall = 13 Then
            If d100% <= 35 Then
                'Change screen to short pass
                playCall = 10
            End If
        End If
        
        'Rollouts more common but not as prevalent as modern
        'No adjustment needed - they existed
        
    End If
    
    ' ========================================
    ' 1978-1994: WEST COAST OFFENSE ERA
    ' ========================================
    ' Bill Walsh revolution, short passing as extension of run
    ' Timing routes, precision passing
    
    If teamYear >= 1978 And teamYear < 1995 Then
        
        ' Short passes and screens become MORE effective
        ' No adjustments needed - this is baseline era
        
    End If
    
    ' ========================================
    ' 1995-2010: SPREAD OFFENSE EMERGENCE
    ' ========================================
    ' Multiple receiver sets, more complex routes
    ' Shotgun formation becomes common
    
    If teamYear >= 1995 And teamYear < 2011 Then
        
        ' Screens become more sophisticated
        ' No major adjustments - still within normal range
        
        'Rollouts slightly more common
        'Short Pass
        If playCall = 10 Then
            If d100% <= 8 Then
                'Some short passes become rollouts
                playCall = 17
            End If
        End If
        
    End If

    ' ========================================
    ' 2011-2019: RPO & MOBILE QB ERA
    ' ========================================
    ' Read-option concepts, mobile quarterbacks
    ' Faster pace, more pre-snap motion
    If teamYear >= 2011 And teamYear < 2020 Then
        
        'Option plays become more viable again

        'Sweep
        If playCall = 4 Then
            'Some sweeps become option plays
            If d100% <= 15 And qbMobility(P1) >= 6 Then
                'Option
                playCall = 5
            End If
        End If
        
        'QB sneaks more aggressive with mobile QBs
        '(Already handled by custom sneak logic)
        
        'Rollouts and bootlegs more common
        'Medium Pass
        If playCall = 11 Then
            If d100% <= 10 And qbMobility(P1) >= 5 Then
                'Change to rollout for mobile QB
                playCall = 17
            End If
        End If
        
    End If
    
    ' ========================================
    ' 2020+: MODERN SPREAD & ANALYTICS ERA
    ' ========================================
    ' Heavy analytics influence, aggressive 4th downs
    ' Wide zone runs, quick game passing
    
    If teamYear >= 2020 Then
        
        'More aggressive with mobile QBs
        If qbMobility(P1) >= 7 Then
            
            ' Designed QB runs more common
            'Short Pass
            If playCall = 10 Then
                If d100% <= 12 Then
                    'Some short passes become option/designed run
                    playCall = 5
                End If
            End If
            
            'Rollouts very common for mobile QBs
            'Medium Pass
            If playCall = 11 Then
                If d100% <= 15 Then
                    'Rollout
                    playCall = 17
                End If
            End If
            
        End If
        
        'Screens more sophisticated and common
        'Draw
        If playCall = 6 Then
            If d100% <= 10 Then
                'Some draws become screens
                playCall = 13
            End If
        End If
        
    End If

    ' ========================================
    ' COLLEGE-SPECIFIC ADJUSTMENTS
    ' ========================================
    ' College offenses historically more diverse

    'NCAA rules
    If ruleOptType = 0 Then

        'Pre-1980: College ran even more than pros
        If teamYear < 1980 Then

            'Power running more dominant
            If (playCall = 13 Or playCall = 17) And d100% <= 40 Then

                adjustRoll = d100%

                Select Case adjustRoll

                    'Blast
                    Case 1 To 50:
                        playCall = 1
                        'Dive
                    Case 51 To 100:
                        playCall = 2

                End Select

            End If
        End If
        
        '1980-2000: Option offense golden age (Nebraska, Oklahoma, etc.)
        If teamYear >= 1980 And teamYear < 2000 Then
            'Outside runs and options more common
            'Off Tackle
            If playCall = 3 And d100% <= 20 Then
                'Change to option
                playCall = 5
            End If
        End If
        
        '2000-2010: Spread offense explosion (Urban Meyer, etc.)
        If teamYear >= 2000 And teamYear < 2011 Then
            'More spread concepts, quick passes
            'Power runs
            If (playCall = 1 Or playCall = 2) And d100% <= 15 Then
                'Change some power runs to quick passes
                playCall = 10
            End If
        End If
        
        '2011+: Modern college spread/RPO dominance
        If teamYear >= 2011 Then
            'Tempo, mobile QBs, read concepts
            'Much more option/read concepts
            If (playCall = 1 Or playCall = 2) And d100% <= 20 And qbMobility(P1) >= 6 Then
                'Power runs become options
                playCall = 5
            End If
        End If
        
    End If
    
End Sub


'----------------------------------------
' AdjustPassPlayForSequencing Subroutine
'----------------------------------------
'Adjusts the pass play type, based on
'the sequencing / histort that we have
'tracked. Call this AFTER a pass play
'is selected, to evaluate and modify it.
Sub AdjustPassPlayForSequencing ()
    
    Dim adjustRoll As Integer
    
    ' Force quick/safe passes
    Select Case playCall

        'Draw
        Case 6
            'Draw is fine for pressure situations
            'Keep as-is

            'Already quick passes
        Case 9, 10, 13
            'Keep as-is - these are perfect

            'Long Pass, Long PA
        Case 12, 15
            ' Change to quick passes
            adjustRoll = d100%
            Select Case adjustRoll
                '3-Step Drop
                Case 1 To 40: playCall = 9
                    'Screen
                Case 41 To 70: playCall = 13
                    'Short Pass
                Case 71 To 100: playCall = 10
            End Select

            'Medium Pass, Medium PA
        Case 11, 14
            'Change to quick passes
            adjustRoll = d100%
            Select Case adjustRoll
                'Short Pass
                Case 1 To 50: playCall = 10
                    '3-Step Drop
                Case 51 To 80: playCall = 9
                    'Screen
                Case 81 To 100: playCall = 13
            End Select

            'Sideline Pass
        Case 16
            ' Keep sideline if in two-minute drill (clock management)
            If gameClock! > 120 Or quarter < 2 Then
                'Not two-minute, change to quick pass
                'Short Pass
                playCall = 10
            End If

            'Rollout
        Case 17
            'Change to quick pass (50% of time)
            If d100% <= 50 Then
                adjustRoll = d100%
                If adjustRoll <= 60 Then
                    '3-Step Drop
                    playCall = 9
                Else
                    'Short Pass
                    playCall = 10
                End If
            End If

    End Select

End Sub

'----------------------------------------
'      UpdatePlayHistory Subroutine
'----------------------------------------
'Track history of last few plays so that
'we can get a sense of what's working and
'what's not with the offense
'
'Parameters:
'  playCallExecuted - The playCall value that was run
'  yardsGained - Net yards gained/lost on the play
Sub UpdatePlayHistory (playCallExecuted, yardsGained)
    
    ' Shift history back one play
    last3PlayType = last2PlayType
    last2PlayType = lastPlayType
    
    ' Determine play type from playCall
    Select Case playCallExecuted
        Case 1 To 8 ' Runs (Blast through QB Sneak)
            lastPlayType = 0
        
        Case 9 To 17 ' Passes (3-Step through Rollout)
            lastPlayType = 1
        
        Case 19, 20 ' Special teams (Punt, FG)
            lastPlayType = 2
        
        Case Else
            ' Unknown play type, keep previous value
            ' Don't shift history
            Exit Sub
    End Select
    
    ' Store yards gained
    lastPlayYards = yardsGained
    
End Sub


'----------------------------------------
'         MEASUREMENT Subroutine
'----------------------------------------
'This subroutine handles the play-by-play
'for when a measurement is done to check
'for first down.
Sub MEASUREMENT ()

    pbpString$ = "HE'S CLOSE TO A FIRST DOWN"
    Call PBP

    pbpString$ = "THE OFFICIAL SIGNALS FOR A MEASUREMENT"
    Call PBP
    Call DELAY

    'Calculate yards to first
    measure! = ydsGained - ydsToFirst

    'Round to 2 decimals
    measure! = Fix(measure! * 100 + Sgn(measure!) * .5) / 100

    Select Case measure!

        Case -0.33 To -.22
            pbpString$ = "HE'S ABOUT A FOOT SHY"

        Case -0.21 To -0.16
            pbpString$ = "HE'S ABOUT 6 INCHES SHORT"

        Case -0.15 To -0.01
            pbpString$ = "HE'S SHORT BY INCHES"

        Case 0 To 0.33
            pbpString$ = "THE OFFICIAL SIGNALS FIRST DOWN!"

    End Select

    Call PBP

End Sub


'----------------------------------------
'       ReceiverLossPBP Subroutine
'----------------------------------------
' Generates a PBP string for when a pass
' play loses yardage
Sub ReceiverLossPBP (yardString$)

    Call Rolld100(yardPBP)

    pbpString$ = ""

    Select Case yardPBP

        Case 1 To 20
            If d100% <= 50 Then
                pbpString$ = gameTeams$(P1) + " LOSES"
            Else
                pbpString$ = "THE " + gameMascots$(P1) + " LOSES"
            End If

            pbpString$ = pbpString$ + yardString$ + " YARDS ON THE PLAY"

        Case 21 To 40
            pbpString$ = "THAT'S A LOSS OF" + yardString$ + " FOR "
            Call WHICHRECEIVER

        Case 41 To 60
            Call WHICHRECEIVER
            pbpString$ = pbpString$ + " WITH A" + yardString$ + " YARD LOSS."

        Case 61 To 80
            pbpString$ = "THAT'S A" + yardString$ + " YARD LOSS FOR"

            If d100% <= 50 Then
                pbpString$ = pbpString$ + gameTeams$(P1)
            Else
                pbpString$ = pbpString$ + " THE " + gameMascots$(P1)
            End If

        Case 81 To 100
            pbpString$ = "THAT CATCH BY "
            Call WHICHRECEIVER
            pbpString$ = pbpString$ + " LOST " + yardString$ + " YARDS"

    End Select

    Call PBP

End Sub


'----------------------------------------
'        WHICHRECEIVER Subroutine
'----------------------------------------
'This subroutine determines which receiver
'to include in the Play-by-Play text
Sub WHICHRECEIVER

    If I3 > 9 Then
        pbpString$ = pbpString$ + wdRec$(P1, I3 - 10)
    Else
        pbpString$ = pbpString$ + rbacks$(P1, I3)
    End If

End Sub


'----------------------------------------
'       TurnoverOnDowns Subroutine
'----------------------------------------
'This subroutine handles assignment of
'the variables needed for a new set of downs
Sub TurnoverOnDowns

    currDown = 1
    ydsToFirst = 10
    ydLine = 100 - ydLine

    If ydLine > 90 Then ydsToFirst = 100 - ydLine

    Call CHANGEPOSS

    playSegment = 5

End Sub


'----------------------------------------
'       PenaltyOnKickoff Subroutine
'----------------------------------------
'This subroutine handles the yardage and
'Play-by-Play for a penalty on the kick.
Sub PenaltyOnKickoff (autoFD)

    totalSTPenalties = totalSTPenalties + 1

    pbpString$ = "THERE'S A FLAG ON THE PLAY!!"
    Call PBP

    Call Rolld100(kickPenalty)

    Select Case kickPenalty

        Case 1 To 54
            'Illegal Block (Kicking team)
            pbpString$ = "*** PENALTY - ILLEGAL BLOCK - OFF - 15 YDS ***"
            penaltyYds! = -15

        Case 55 To 56
            pbpString$ = "*** PENALTY - PLAYER OUT-OF-BOUNDS - OFF - 5 YDS ***"
            penaltyYds! = -5

        Case 57 To 58
            'Holding (Kicking team)
            pbpString$ = "*** PENALTY - " + gameTeams$(D) + " - HOLDING - 5 YDS / 1ST D! ***"
            penaltyYds! = -5
            autoFD = 1

        Case 59 To 100
            'Holding (Return team)
            pbpString$ = "*** PENALTY - " + gameTeams$(P1) + " - HOLDING - 15 YDS! ***"
            penaltyYds! = 15

    End Select

    'Print #11, "(KICKOFF) "; pbpString$

    playSegment = 4

End Sub


'----------------------------------------
'      PenaltyOnPlaceKick Subroutine
'----------------------------------------
'This subroutine handles the yardage and
'Play-by-Play for a penalty on the placekick.
Sub PenaltyOnPlaceKick (autoFD)

    totalSTPenalties = totalSTPenalties + 1

    pbpString$ = "THERE'S A FLAG ON THE PLAY!!"
    Call PBP

    Call Rolld100(kickPenalty)

    Select Case kickPenalty

        Case 1 To 4
            pbpString$ = "*** PENALTY - LEAPING - DEF - 15 YDS / 1ST D! ***"
            penaltyYds! = 15
            autoFD = 1

        Case 4 To 6
            pbpString$ = "*** PENALTY - LEVERAGE - DEF - 15 YDS / 1ST D! ***"
            penaltyYds! = 15
            autoFD = 1

        Case 7 To 20
            pbpString$ = "*** PENALTY - ROUGHING KICKER - DEF - 15 YDS / 1ST D! ***"
            penaltyYds! = 15
            autoFD = 1

        Case 21 To 29
            pbpString$ = "*** PENALTY - RUNNING INTO KICKER - DEF - 5 YARDS ***"
            penaltyYds! = 5

        Case 30 To 100
            If ruleOptType = 0 Then
                pbpString$ = "*** PENALTY - DEF - HOLDING - 10 YDS ***"
                penaltyYds! = 10
            Else
                pbpString$ = "*** PENALTY - DEF - HOLDING - 5 YDS / 1ST D! ***"
                penaltyYds! = 5
            End If

            autoFD = 1

    End Select

    'Print #11, "(PLACE KICK) "; pbpString$

    playSegment = 4

End Sub


'----------------------------------------
'        PenaltyOnPunt Subroutine
'----------------------------------------
'This subroutine handles the yardage and
'Play-by-Play for a penalty on the punt.
Sub PenaltyOnPunt (autoFD)

    totalSTPenalties = totalSTPenalties + 1

    pbpString$ = "THERE'S A FLAG ON THE PLAY!!"
    Call PBP

    Call Rolld100(puntPenalty)

    'OFF = Punting Team
    'DEF = Return Team

    Select Case puntPenalty

        Case 1 To 7
            pbpString$ = "*** PENALTY - LEVERAGE - DEF - 15 YDS / 1ST D! ***"
            penaltyYds! = 15
            autoFD = 1

        Case 8 To 20
            pbpString$ = "*** PENALTY - ILLEGAL BLOCK - DEF - 15 YDS / 1ST D! ***"
            penaltyYds! = 15
            autoFD = 1

        Case 21 To 60
            pbpString$ = "*** PENALTY - ROUGHING KICKER - DEF - 15 YDS / 1ST D! ***"
            penaltyYds! = 15
            autoFD = 1

        Case 61 To 94
            pbpString$ = "*** PENALTY - INELIG. DOWNFLD. KICK - OFF - 15 YDS ***"
            penaltyYds! = -15

        Case 95 To 100
            pbpString$ = "*** PENALTY - PLAYER OUT-OF-BOUNDS - OFF - 5 YDS ***"
            penaltyYds! = -5

    End Select

    'Print #11, "(PUNT) "; pbpString$

    Call PBP

    playSegment = 4

End Sub


'----------------------------------------
'     PenaltyOnPuntReturn Subroutine
'----------------------------------------
'This subroutine determines the outcome
'for a penalty on a punt return.
Sub PenaltyOnPuntReturn (autoFD)

    totalSTPenalties = totalSTPenalties + 1

    pbpString$ = "THERE'S A FLAG ON THE PLAY!!"
    Call PBP

    Call Rolld100(puntPenalty)

    Select Case puntPenalty

        Case 1 To 41
            'Holding - Return team
            pbpString$ = "*** PENALTY - " + gameTeams$(P1) + " - HOLDING - 15 YDS! ***"
            penaltyYds! = -15

        Case 42 To 44
            'Holding - Kicking team
            pbpString$ = "*** PENALTY - " + gameTeams$(D) + " - HOLDING - 5 YDS / 1ST D! ***"
            penaltyYds! = -5
            autoFD = 1

        Case 45 To 100
            'Illegal Block (Kicking team)
            pbpString$ = "*** PENALTY - " + gameTeams$(D) + " - ILLEGAL BLOCK ON THE RETURN - 10 YDS ***"
            penaltyYds! = 10

    End Select

    'Print #11, "(PUNT RETURN) "; pbpString$

End Sub
    

'----------------------------------------
'          PenaltyChoice Function
'----------------------------------------
'Determine how to handle PENALTIES
'Both penalty yardage and yards gained
'from the play have been determined
Sub PenaltyChoice (decision, autoFD)

    '0 yards = unhandled penalty
    If penaltyYds! <> 0 Then

        pbpString$ = pbpString$ + " 1-ACCEPT 2-DECLINE"
        Call PBP

        If playerMode = 0 Then

            I$ = GetKeyPress$

        Else

            'Computer Player - default to DECLINE
            decision = 2

            'Computer Player
            If penaltyYds! > 0 Then

                'Penalty on Defense
                'Offense gets to choose
                If (playerMode = 1 And D = compTeam) Then
                    'Human Offense
                    Do
                        I$ = GetKeyPress$
                        decision = Val(I$)
                    Loop Until decision = 1 Or decision = 2
                Else
                    'Computer Offense
                    '  If we didn't get the first down but would be granted it  --OR--
                    '  We would get a first down  --OR--
                    '  We gained <= 5 yards [penalties give us 5+ yards]
                    If (autoFD = 1 And (penaltyYds! > ydsToFirst Or (ydsToFirst - ydsGained > 0))) Or ydsGained <= 5 Then
                        decision = 1
                    End If
                End If

            Else

                'Penalty on Offense
                'Defense gets to choose
                If (playerMode = 1 And P1 = compTeam) Then
                    'Human Defense
                    Do
                        I$ = GetKeyPress$
                        decision = Val(I$)
                    Loop Until decision = 1 Or decision = 2
                Else
                    'Computer Defense
                    '   4th Down and offense got a 1st  --OR--
                    '   3rd Down and offnese has <=3 yds for 1st --OR--
                    '   Offense gained yards on the play (any down)
                    If ((currDown > 3 And ydsGained >= ydsToFirst) Or (currDown = 3 And ydsToFirst - ydsGained <= 3 Or ydsGained >= 0)) Then
                        decision = 1
                    End If
                End If

            End If

        End If

    End If 'Done checking for unhandled penalty

End Sub


'----------------------------------------
'     PenaltyCharge Subroutine
'----------------------------------------
'This handles tracking of penalties and
'yardage for most fouls
Sub PenaltyCharge (teamIdx, ydsCharged!)

    If ydLine + ydsCharged! > 100 Then

        'Half the distance to the goal - AHEAD
        statYds! = CInt(100 - ydLine) / 2
        ydsCharged! = statYds!

    Else

        If ydLine + ydsCharged! < 1 Then

            'Half the distance to the goal - BEHIND
            statYds! = 0 - CInt(ydLine) / 2
            ydsCharged! = statYds!

        Else
            If ydsCharged! < 0 Then
                statYds! = 0 - ydsCharged!
            Else
                statYds! = ydsCharged!
            End If
        End If

    End If

    ydsToFirst = ydsToFirst - ydsCharged!
    ydLine = ydLine + ydsCharged!

    ydsToScore = 100 - ydLine

    'Increase team penalties
    gameStatsTeam(teamIdx, 11) = gameStatsTeam(teamIdx, 11) + 1

    'Increase team penalty yardage
    gameStatsTeam(teamIdx, 12) = gameStatsTeam(teamIdx, 12) + statYds!

End Sub


'----------------------------------------
'       PenaltyPreSnap Subroutine
'----------------------------------------
'This handles tracking of penalties and
'yardage for pre-snap fouls
Sub PenaltyPreSnap (flagOnPlay)

    RN1 = Int(Rnd(1) * 1000) + 1

    If RN1 <= 30 And endZone <> 1 And playCall < 21 Then

        pbpString$ = "*** PENALTY - "
        penaltyType = Int(Rnd(1) * 45) + 1

        If penaltyType <= 24 Then
            penaltyTeam = P1
        Else
            penaltyTeam = D
        End If

        'Home Field Advantage - Penalties
        If penaltyTeam = gameLoc Then
            'Home: 7.5% less chance of penalty
            If RN1 + 75 <= 30 Then
                penaltyType = 0
            End If
        Else
            'Away: 7.5% more chance of penalty
            If RN1 - 75 <= 30 Then
                penaltyType = 0
            End If
        End If

        unhandled = 0

        Select Case penaltyType

            Case 1 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFF - OFFSIDES - 5 YDS ***"
                penaltyYds! = -5

            Case 2 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFF - NEUTRAL ZONE INFRACTION - 5 YDS ***"
                penaltyYds! = -5

            Case 3 + teamRat_GAME(P1, 0) To 5 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFF - DELAY OF GAME - 5 YDS ***"
                penaltyYds! = -5

            Case 6 + teamRat_GAME(P1, 0) To 34 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFF - FALSE START - 5 YDS ***"
                penaltyYds! = -5

            Case 35 + teamRat_GAME(P1, 0) To 39 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFF - ILLEGAL FORMATION - 5 YDS ***"
                penaltyYds! = -5

            Case 40 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFF - ILLEGAL SHIFT - 5 YDS ***"
                penaltyYds! = -5

            Case 41 + teamRat_GAME(D, 0)
                pbpString$ = pbpString$ + "DEF - DELAY OF GAME - 5 YDS ***"
                penaltyYds! = 5

            Case 42 + teamRat_GAME(D, 0) To 50 + teamRat_GAME(D, 0)
                pbpString$ = pbpString$ + "DEF - ENCROACHMENT - 5 YDS ***"
                penaltyYds! = 5

            Case Else
                pbpString$ = pbpString$ + "DEF - OFFSIDES - 5 YDS ***"
                penaltyYds! = 5

        End Select

        flagOnPlay = 1

    End If

End Sub


'----------------------------------------
'     FirstDownAfterPlay Subroutine
'----------------------------------------
'This handles updating variables for play
'when a penalty has resulted in a first down
Sub FirstDownAfterPlay

    currDown = 1
    ydsToFirst = 10

    If ydLine > 90 Then ydsToFirst = 100 - ydLine

    If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then
        playSegment = 1
    End If

    'Increase first downs
    gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

    If gameClock! <= 0 Then gameClock! = 1

End Sub


'----------------------------------------
'          CHOOSEQB Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'choosing a quarterback and sets up the
'corresponding arrays for the game.
Sub CHOOSEQB (currTeam)

    Color 15, 0
    Cls

    Call TEAMBOX(12, currTeam)

    useRandomQB(currTeam) = 0

    Locate , 31
    Print "SELECT QUARTERBACK"
    Print
    Locate , 18: Print " # QB NAME     % OF  COMP  INT"
    Locate , 18: Print "               ATTS  PCT   PCT  ARM"

    For currQB = 0 To 3

        If qbacks$(currTeam, currQB) <> "XXX" And qbacks$(currTeam, currQB) <> "END" And qbacks$(currTeam, currQB) <> "" Then

            Locate , 18: Print currQB; "-             ";
            Locate , 18: Print currQB; "- "; qbacks$(currTeam, currQB);
            Locate , 33

            If currQB = 0 Then

                Print qbNumAtt(currTeam, currQB);

                Locate , 39: Print qbCompPct(currTeam, currQB);
                Locate , 45: Print qbPctInt(currTeam, currQB);
                Locate , 50: Print qbArmRat(currTeam, currQB)

            Else

                'Only print extra QB's if they
                'don't have 100% contribution
                If qbNumAtt(currTeam, currQB - 1) < 100 And qbNumAtt(currTeam, currQB - 1) > 0 Then

                    Print Int(qbNumAtt(currTeam, currQB)) - qbNumAtt(currTeam, currQB - 1);

                    Locate , 39: Print qbCompPct(currTeam, currQB);
                    Locate , 45: Print qbPctInt(currTeam, currQB);
                    Locate , 50: Print qbArmRat(currTeam, currQB)

                End If

                Locate , 1

            End If

        End If

    Next currQB

    Locate , 19: Print "4 - COMPUTER SELECT PASSER EACH PLAY"
    Print
    Locate , 19: Print "SELECT (0-4) ";
    Color 15, 0

    Do
        I$ = GetKeyPress$
        choice = Val(I$)
    Loop Until choice >= 0 And choice <= 4

    If choice <= 3 Then
        qbNumber(currTeam) = choice
    End If

    If choice = 4 Then
        useRandomQB(currTeam) = 1
        'qbNumber(currTeam) = choice
        qbContrib = GetQBRushPctGame%(qbacks$(currTeam, qbNumber(currTeam)), currTeam)
        qbMobility(currTeam) = GetQBMobility%(qbContrib)
    End If

    If choice <= 3 And useRandomQB(currTeam) = 0 Then

        qbContrib = GetQBRushPctGame%(qbacks$(currTeam, qbNumber(currTeam)), currTeam)
        suggestedMobility = GetQBMobility%(qbContrib)

        Call TEAMBOX(12, currTeam)

        Locate 8, 16: Print "ENTER "; qbacks$(currTeam, choice); " MOBILITY RATING (1-9)"
        Locate , 16: Print "(1-LOW, 9-HIGH)"
        Print
        Locate , 16: Print "THIS IS HOW OFTEN HE RUNS OUT OF THE POCKET"
        Locate , 16: Print "SOME QB'S LIKE MARINO & BRADY ALMOST NEVER DO"
        Locate , 16: Print "OTHERS LIKE MAHOMES & VICK WILL TAKE OFF"
        Locate , 16: Print "FOR AN AVERAGE QB, SIMPLY ENTER 5"
        Print
        Locate , 16: Print "THIS QB CONTRIBUTED"; qbContrib; "% OF TEAM RUNS"
        Locate , 16: Print "A SUGGESTED VALUE FOR THEM IS"; suggestedMobility

        Color 15, 0

        Do
            I$ = GetKeyPress$
            qbMobility(currTeam) = Val(I$)
        Loop Until qbMobility(currTeam) >= 1 And qbMobility(currTeam) <= 9

    End If

    'Update rusher index of each benched QB
    For I = 0 To 3:
        qbRushIdx(currTeam, I) = -1
    Next I

    'If we selected a QB
    If useRandomQB(currTeam) = 0 Then

        For currBack = 0 To 9:

            For currQB = 0 To 3
                'Store the rushing index of other QB's
                If rbacks$(currTeam, currBack) = qbacks$(currTeam, currQB) And I5 <> qbNumber(currTeam) Then
                    qbRushIdx(currTeam, currQB) = currBack
                End If
            Next currQB

        Next currBack

    End If

End Sub


'----------------------------------------
'        TimeoutCalled Subroutine
'----------------------------------------
Sub TimeoutCalled (teamIdx, periodOver)

    pbpString$ = "TIME OUT " + gameTeams$(teamIdx) + " - THEY HAVE" + Str$(timeouts(teamIdx) - 1) + " REMAINING"
    Call PBP

    timeouts(teamIdx) = timeouts(teamIdx) - 1
    playSegment = 9

    Call AddPossessionTime(7, 10)
    'Call CheckGameClock(periodOver)

End Sub


'----------------------------------------
'      KickoffChoices Subroutine
'----------------------------------------
'This subroutine gives the user choices
'to handle a pro kick off.
Sub KickoffChoices (deferOpt)

    'In all situations here, winTeam is the coin toss winner
    Call StatusBoard

    If (playerMode = 1 And winTeam = compTeam) Or playerMode = 2 Then

        If overtimeOpt >= 6 Then
            'Modified Sudden Death
            'Teams prefer to kick so they know how many
            'points they need after the other team's possession
            koChoice = 1
        Else
            'Default to receiving the kick
            koChoice = 2
        End If

    Else
        Call OPTIONBOX(9)
        Print gameTeams$(winTeam); " DO YOU ELECT TO:"
        Locate 12, 14
        Locate , 14: Print "1 - KICK OFF"
        Locate , 14: Print "2 - RECEIVE"
        Locate , 14: Print "3 - DEFEND GOAL WITH WIND AT BACK"
        Locate , 14: Print "4 - DEFEND GOAL WITH WIND IN FACE"
        If deferOpt = 1 Then
            Locate , 14: Print "5 - DEFER CHOICE TO 2ND HALF"
        End If

        Do
            I$ = GetKeyPress$
            koChoice = Val(I$)
        Loop Until koChoice >= 1 And koChoice <= (4 + deferOpt)

    End If

    Screen , , 0, 0

    Select Case koChoice

        Case 1, 2

            'Kicking team is always P1
            'Receiving team is always D
            If koChoice = 1 Then
                D = 1 - winTeam
                P1 = winTeam
            Else
                D = winTeam
                P1 = 1 - winTeam
            End If

            'Computer Player
            If (playerMode = 1 And D = compTeam) Or playerMode = 2 Then

                defChoice = 1

            Else

                Call OPTIONBOX(7)

                'winTeam must kickoff
                'The other team chooses how to defend
                Print gameTeams$(D); " DO YOU ELECT TO:"
                Locate 12, 14
                Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
                Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

                Do
                    I$ = GetKeyPress$
                    defChoice = Val(I$)
                Loop Until defChoice >= 1 And defChoice <= 2

            End If

            Screen , , 0, 0

            If defChoice = 1 Then
                climate(0) = 1 - winTeam
            Else
                climate(0) = winTeam
            End If

        Case 3, 4

            If koChoice = 3 Then
                'Having the wind in your favor
                climate(0) = winTeam
            Else
                'Working against the wind
                climate(0) = 1 - winTeam
            End If

            If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

                koChoice = 2

            Else

                Call OPTIONBOX(7)
                Print gameTeams$(1 - winTeam); " DO YOU ELECT TO:"
                Locate 12, 14
                Locate , 14: Print "1 - KICK OFF"
                Locate , 14: Print "2 - RECEIVE"

                Do
                    I$ = GetKeyPress$
                    koChoice = Val(I$)
                Loop Until koChoice >= 1 And koChoice <= 2

            End If

            Screen , , 0, 0

            'Kicking team is always P1
            'Receiving team is always D
            If koChoice = 1 Then
                D = winTeam
                P1 = 1 - winTeam
            Else
                D = 1 - winTeam
                P1 = winTeam
            End If

        Case 5
            winTeam = 1 - winTeam
            Call KickoffChoices(0)

    End Select

End Sub


'----------------------------------------
'       PuntNotReturned Subroutine
'----------------------------------------
'This subroutine factors in for all the
'possible reasons a kick would not be
'returned, and handles PBP for each.
Sub PuntNotReturned (fairCatchPossible, puntType, isPenalty)

    isTouchback = 0
    isPenalty = 0

    'Let's see where the ball landed
    ydLine = ydLine + ydsGained
    ydsToScore = ydLine
    Call YARDLINE

    'Calculating yard line for return team
    ydLine = 100 - ydLine

    'Hangtime of 4 - 5 seconds
    '-- Average is 4.2-4.4 seconds
    '-- Range is 3.7-4.7 seconds
    Call AddPossessionTime(4, 5)

    'RN0 = Int(Rnd(1) * 47) + 1
    Call Rolld100(RN0)

    '8% of regular punts | Intentionally punt away from return man
    If RN0 <= 8 Or puntType = 1 Then

        'Out-of-bounds
        If ydLineTeam < 0.1 Then

            If d100% <= 50 Then
                pbpString$ = "THE BALL GOES OUT OF BOUNDS AT THE END ZONE"
            Else
                pbpString$ = "THE KICK SOARS OUT OF BOUNDS AT THE END ZONE"
            End If

        Else

            If ydLineTeam <= 1 Then
                yardLineString$ = Str$(1)
            Else
                yardLineString$ = Str$(Int(ydLineTeam))
            End If

            If d100% <= 50 Then
                pbpString$ = "THE BALL GOES OUT OF BOUNDS AT THE" + yardLineString$ + " YARD LINE"
            Else
                pbpString$ = "THE KICK SOARS OUT OF BOUNDS AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
            End If

        End If

        Call PBP

    Else

        'Downed inside the team's 10
        If ydLineTeam <= 10 Then

            If RN0 <= 1 Then

                pbpBall = Int(Rnd(1) * 2) + 1
                downTeam = Int(Rnd(1) * 2)
                teamName = Int(Rnd(1) * 2) + 1

                If downTeam = P1 Then
                    If teamName = 1 Then
                        recoverTeam$ = gameTeams$(P1)
                    Else
                        recoverTeam$ = gameMascots$(P1)
                    End If
                Else
                    If teamName = 1 Then
                        recoverTeam$ = gameTeams$(D)
                    Else
                        recoverTeam$ = gameMascots$(D)
                    End If
                End If

                If pbpBall = 1 Then
                    pbpString$ = "THE BALL ROLLS PAST THE RETURNER!"
                    Call PBP
                End If

                If ydLineTeam <= 1 Then
                    yardLineString$ = Str$(1)
                Else
                    yardLineString$ = Str$(Int(ydLineTeam))
                End If

                RN1 = Int(Rnd(1) * 4) + 1

                Select Case RN1

                    Case 1
                        pbpString$ = "IT IS DOWNED AT THE" + yardLineString$ + " YARD LINE"

                    Case 2
                        pbpString$ = recoverTeam$ + " DOWNS IT AT THE" + yardLineString$ + " YARD LINE"

                    Case 3
                        pbpString$ = "THE BALL IS DOWNED AT THE" + yardLineString$ + " YARD LINE"

                    Case 4
                        pbpString$ = "DOWNED BY" + recoverTeam$ + " ON THE" + yardLineString$ + " YARD LINE"

                End Select

                Call PBP

                fairCatchPossible = 0

            Else

                'Inside the 10 - Fair Catch
                fairCatchPossible = 1

            End If

        Else

            'Outside the fair catch zone
            fairCatchPossible = 0

        End If

    End If

    If ydLineTeam <= 1 Then
        yardLineString$ = Str$(1)
    Else
        yardLineString$ = Str$(Int(ydLineTeam))
    End If

    If fairCatchPossible = 1 And currDown = 4 Then

        pbpString$ = "A FAIR CATCH IS CALLED AT THE" + yardLineString$ + " YARD LINE"
        Call PBP

    End If

    Call CHANGEPOSS

    If d100% > 1 Then

        If isTouchback = 1 Then
            'Punt ended up in end zone
            Call Touchback(1)
        Else
            'Ball downed instead
            currDown = 1
            ydsToFirst = 10
        End If

    Else

        isPenalty = 1

    End If

    'No return = No yards
    'Important for calculating penalties
    ydsGained = 0

End Sub


'----------------------------------------
'         TAKESAFETY Subroutine
'----------------------------------------
'This subroutine handles ball position,
'variables and Play-by-play for when a
'punter takes a safety voluntarily.
Sub TAKESAFETY ()

    Call AddPossessionTime(4, 7)

    ydsGained = (ydLine * -1) - 1

    'Rush Att
    gameStatsTeam(P1, 1) = gameStatsTeam(P1, 1) + 1
    'Rush Yds
    gameStatsTeam(P1, 2) = gameStatsTeam(P1, 2) + ydsGained

    rndPbp = Int(Rnd(1) * 3) + 1

    Select Case rndPbp

        Case 1
            pbpString$ = punter$(P1, 0) + " IS HEADED OUT OF THE END ZONE"

        Case 2
            pbpString$ = punter$(P1, 0) + " MAKES HIS WAY TOWARD THE END ZONE"

        Case 3
            pbpString$ = punter$(P1, 0) + " IS GOING TO TAKE A SAFETY ON THIS ONE"

    End Select

    Call PBP

    score(D, quarter) = score(D, quarter) + 2
    score(D, 0) = score(D, 0) + 2

End Sub


'----------------------------------------
'           FINDINT Subroutine
'----------------------------------------
'This subroutine appears to determine
'something related to interceptions.
'Perhaps the player to associate with
'an interception attempt?
Sub FINDINT (defPlayIdx)

    sumInts = 0
    RN0 = Int(Rnd(1) * teamInts(D)) + 1
    currIdx = 0

    'Find player for interception based on contribution
    Do While currIdx <= 9 And RN0 > sumInts

        sumInts = sumInts + playerInts(D, currIdx)

        If RN0 > sumInts Then
            currIdx = currIdx + 1
        End If

    Loop

    'Safety check in case no player was found
    If currIdx > 9 Then

        'Pick first player with interceptions as fallback
        i = 0
        Do While i <= 9 And playerInts(D, i) = 0
            i = i + 1
        Loop

        If i <= 9 Then
            currIdx = i
        Else
            'Ultimate fallback if somehow no one has interceptions
            currIdx = 0
        End If

        Open cfgPath$ + "errlog" For Append As #10
        Print #10, "DATA MISMATCH: teamInts="; teamInts(D); " but sum of playerInts="; sumInts
        Print #10, "Choosing player at index "; currIdx
        Close #10

    End If

    If currIdx <= 9 Then
        defTeam$ = defInts$(D, currIdx)
    End If

    defPlayIdx = currIdx

End Sub


'----------------------------------------
'           YARDAGE Subroutine
'----------------------------------------
'This subroutine I believe is used to
'round off the yardage gained on a play
Sub YARDAGE ()

    ydsGained = ydsGained + (Int(Rnd(1) * 11) - 5) / 10

End Sub


'----------------------------------------
'       PuntReturnYards Subroutine
'----------------------------------------
'This subroutine handles the calculations
'for punt yardage and who receives the
'punt.
Sub PuntReturnYards ()

    ydLine = ydLine + ydsGained

    Call CHANGEPOSS

    ydsToScore = ydLine
    ydLine = 100 - ydLine

    Call YARDLINE

    RN0 = Int(Rnd(1) * 1000) + 1

    Select Case prYdsPerRet(P1, puntRetNbr)

        Case 1 To 4

            Select Case RN0

                Case 1 To 50
                    '-5 to 0
                    ydsGained = Int((Rnd(1) * 6) - 5)

                Case 51 To 899
                    '-6 to 16
                    ydsGained = Int((Rnd(1) * 14)) + (prYdsPerRet(P1, puntRetNbr) - 3) * 3

                Case 900 To 995
                    If rushBase <> 1 Then
                        '6 to 53
                        ydsGained = Int((Rnd(1) * 39) + 12) + (prYdsPerRet(P1, puntRetNbr) - 3) * 3
                    End If

                Case 993 To 1000
                    '0.8% chance of TD
                    If rushBase <> 1 Then
                        ydsGained = 110
                    End If

            End Select

        Case 5

            Select Case RN0

                Case 1 To 900
                    '90% chance
                    '0 - 24
                    ydsGained = Int((Rnd(1) * 25) + 0)

                Case 901 To 960
                    '7% chance
                    '12 - 50
                    ydsGained = Int((Rnd(1) * 39) + 12)

                Case 961 To 1000
                    '4% chance of TD
                    If rushBase <> 1 Then
                        ydsGained = 110
                    End If

            End Select

        Case Is >= 6

            If RN0 > 925 And rushBase <> 1 Then
                '7.5% chance of TD; was 10%
                ydsGained = 110
            Else
                '95% chance
                '0 to 34
                ydsGained = Int((Rnd(1) * 26)) + (prYdsPerRet(P1, puntRetNbr) - 6) * 3
            End If

    End Select

    scoreX1 = 4
    Call YARDAGE

    Call ShowChainMarker

    pbpString$ = "THE BALL IS TAKEN BY " + pret$(P1, puntRetNbr) + " AT THE" + Str$(Int(ydLineTeam))
    Call PBP

    startYds = 100 - ydLine
    endYds = startYds - ydsGained
    Call MOVEBALL

End Sub


'----------------------------------------
'       PuntFakeRunYards Subroutine
'----------------------------------------
'This subroutine handles yardage gains and
'Play-by-Play text for a fake play.
Sub PuntFakeRunYards (choice)

    Dim successRate(0 To 2)

    'choice
    '0 - SET UP PUNT RETURN"
    '1 - GO FOR THE BLOCK"
    '2 - CALL PUNT-SAFE"

    'Short: 1-5 Yards
    'Medium: 6-15 Yards
    'Long: 16+ Yards

    Select Case ydsToFirst

        Case 1 To 5
            'Short Yardage
            successRate(0) = 55
            successRate(1) = 75
            successRate(2) = 45

        Case 6 To 15
            'Medium Yardage
            successRate(0) = 45
            successRate(1) = 55
            successRate(2) = 35

        Case Is >= 16
            'Long Yardage
            successRate(0) = 20
            successRate(1) = 25
            successRate(2) = 15

    End Select

    pbpString$ = "IT'S A FAKE!!! THE UPBACK IS RUNNING THE BALL"
    Call PBP

    Call Rolld100(RN0)

    Select Case choice

        Case 0
            'Should be 7-12 yards average
            '-- Current values should average 7 yards overall.
            '   7 yards (normal), 6 yards (block), 3 yards (safe)
            Select Case RN0

                Case 1 To successRate(0)
                    ydsGained = Int(Rnd(1) * 27)

                Case successRate(0) + 1 To 98
                    ydsGained = Int(Rnd(1) * 6) - 2

                Case 99 To 100
                    ydsGained = 100 - ydLine

            End Select

        Case 1
            'Should be 10-15+ yards average
            '-- Current values should average 17 yards overall.
            '   17 yards (normal), 13 yards (block), 8 yards (punt safe)
            Select Case RN0

                Case 1 To successRate(1)
                    ydsGained = Int(Rnd(1) * 21)

                Case successRate(1) + 1 To 95
                    ydsGained = Int(Rnd(1) * 11) - 4

                Case 96 To 100
                    ydsGained = 100 - ydLine

            End Select

        Case 2
            'Should be 5-10 yards average
            '-- Current values should average 3 yards overall.
            '   3 yards (normal), 1.5 yards (block), -1 yards (punt safe)
            Select Case RN0

                Case 1 To successRate(2)
                    ydsGained = Int(Rnd(1) * 21)

                Case successRate(2) + 1 To 99
                    ydsGained = Int(Rnd(1) * 7) - 7

                Case 100
                    ydsGained = 100 - ydLine

            End Select

    End Select

    Call YARDAGE

    playSegment = 1

    startYds = 100 - ydLine
    endYds = startYds - ydsGained
    Call MOVEBALL

    pbpString$ = "THE UPBACK GAINED"

    scoreX0 = 10
    scoreX1 = 11

    fakeYdLine! = CInt((ydsGained + ydLine) * 10) / 10

    fakeYds! = CInt((ydsGained) * 10) / 10

    If fakeYdLine! >= 99.94999 Then
        pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
    Else
        pbpString$ = pbpString$ + Str$(fakeYds!)
    End If

    If fakeYds! = 0 Then
        pbpString$ = pbpString$ + " NOTHING ON THE RUN"
    Else
        pbpString$ = pbpString$ + " YARDS ON THE FAKE RUN"
    End If

    Call PBP

End Sub


'----------------------------------------
'         CheckFumble_1 Subroutine
'----------------------------------------
'This subroutine calculates if a player
'has fumbled the ball.
Sub CheckFumble_1 ()

    Call Rolld100(fumbleRoll)

    If checkFumbRev = 1 Then
        checkFumbRev = 0
    End If

    'Home Field Advantage: Fumbles
    '12% difference in takeaways
    'translates to ~0.2% chance per play
    'we will process this is as 1/5 chance of adjusting by 1
    If P1 = gameLoc Then
        'Home: fewer fumbles
        If d100% <= 20 Then fumbleRoll = fumbleRoll + 1
    Else
        'Away: more fumbles
        If d100% <= 20 Then fumbleRoll = fumbleRoll - 1
    End If

    Select Case fumbPlayType

        Case 0:
            'Run
            fumbleRoll = fumbleRoll - 2

        Case 1:
            'Sack
            '15-20% chance (10X as often)
            fumbleRoll = fumbleRoll - 20

        Case 2:
            'Pass
            fumbleRoll = fumbleRoll

        Case 3, 4:
            'Kick / Punt Return
            '3-5% chance (3X as oten)
            fumbleRoll = fumbleRoll - 7

        Case 5:
            'Interception Return
            '5-10% chance (5X as often)
            fumbleRoll = fumbleRoll - 7

    End Select

    'Blast = slightly more secure run
    '1-2% of calls (2X as often)
    If playCall = 1 Then
        fumbleRoll = fumbleRoll + 1
    End If

    'Option
    '3% of calls (3X as often)
    'We've previously checked for fumble on the pitch
    If playCall = 5 Then
        'fumbleRoll = fumbleRoll
    End If

    fumbleRoll = fumbleRoll - climate(5)

    'Off Fumble % + Def Fumble%
    fumbleRating = (leagRat_GAME(P1, 3) + leagRat_GAME(D, 4))

    Select Case fumbleRating

        Case -3, -2:
            fumbleChance = -3

        Case -1 To 0:
            'Value = -2, -1
            fumbleChance = fumbleRating - 1

        Case 1:
            fumbleChance = -3

        Case 2:
            fumbleChance = -4

        Case 3:
            fumbleChance = 1

        Case 4:
            fumbleChance = 2

        Case 5:
            fumbleChance = 0

        Case Is >= 6:
            fumbleChance = -2

    End Select

    Select Case teamYears(P1)

        Case Is <= 1949
            fumbleChance = fumbleChance - 2

        Case 1949 To 1960
            fumbleChance = fumbleChance - 1

        Case 1961 To 1969
            fumbleChance = fumbleChance - 2

        Case 1970 To 1990
            fumbleChance = fumbleChance - 1

        Case 1991 To 1994
            '1991 may not be the earliest year
            'need to test 1981-1990
            fumbleChance = fumbleChance - 3

    End Select

    If fumbleRoll <= fumbleChance Then
        ballFumbled = 1
    End If

    'Reset for standard play type
    fumbPlayType = 0

End Sub


'----------------------------------------
'         CheckFumble_2 Subroutine
'----------------------------------------
'This subroutine calculates if a player
'has fumbled the ball.
Sub CheckFumble_2 ()

    Call Rolld100(fumbleRoll)

    If checkFumbRev = 1 Then
        checkFumbRev = 0
    End If

    'Off Fumble % + Def Fumble%
    fumbleRating = leagRat_GAME(P1, 3) + leagRat_GAME(D, 4)

    'Real world results don't play out with
    'more fumbles past a certain point
    If fumbleRating >= 6 Then fumbleRating = 5

    Select Case fumbPlayType

        Case 0:
            'Run
            fumbleChance = fumbleRating

        Case 1:
            'Sack
            '15-20% chance (10X as often)
            fumbleChance = fumbleRating + 10

        Case 2:
            'Pass
            fumbleChance = fumbleRating

        Case 3, 4:
            'Kick / Punt Return
            '3-5% chance (3X as oten)
            fumbleChance = fumbleRating + 3

        Case 5:
            'Interception Return
            '5-10% chance (5X as often)
            fumbleChance = fumbleRating + 5

    End Select

    'Blast = slightly more secure run
    '1-2% of calls (2X as often)
    If playCall = 1 Then
        fumbleChance = fumbleChance + 1
    End If

    'Option
    '3% of calls (3X as often)
    'We've previously checked for fumble on the pitch
    If playCall = 5 Then
        fumbleChance = fumbleChance + 3
    End If

    fumbleChance = fumbleChance - climate(5)

    If fumbleRoll <= fumbleChance Then
        ballFumbled = 1
    End If

    'Reset for standard play type
    fumbPlayType = 0

End Sub


'----------------------------------------
'            FLEA Subroutine
'----------------------------------------
'This subroutine handles yardage and the
'Play-by-Play text for a flea flicker.
Sub FLEA (ffChance)

    I1 = qbNumber(P1)

    catchPctSuccess = qbCompPct(P1, qbNumber(P1)) + Int(leagRat_GAME(D, 1)) - 4

    intChance = qbPctInt(P1, qbNumber(P1)) + leagRat_GAME(D, 2) + 5

    ffPctSuccess = 5
    If coverage = 7 Or coverage = 8 Then ffPctSuccess = 30
    If coverage = 9 Or coverage = 10 Then ffPctSuccess = 50
    If coverage = 11 Then ffPctSuccess = 70

    If d100% <= 25 Then
        ydsGained = Int(Rnd(1) * 50) + 51
    Else
        ydsGained = Int(Rnd(1) * 15) + 35
    End If

    catchYds = Int(Rnd(1) * 16) + 35

    If hasRunFF(P1) > 0 Then

        catchPctSuccess = 3

        'increase chance of INT
        intChance = intChance + 10

        If ffPctSuccess = 5 Then
            ffPctSuccess = 30
        End If

    End If

    hasRunFF(P1) = 1

    pbpString$ = rbacks$(P1, 0) + " TAKES THE HANDOFF"
    Call PBP

    pbpString$ = "HE TOSSES IT BACK TO " + qbacks$(P1, qbNumber(P1)) + " FOR A FLEA FLICKER!!!"
    Call PBP

    I3 = Int(Rnd(1) * 3)

    pbpString$ = wdRec$(P1, I3) + " IS OPEN DOWNFIELD"
    Call PBP

    Call Rolld100(ffChance)

End Sub


'----------------------------------------
'           REVGAIN Subroutine
'----------------------------------------
'This subroutine handles yardage and the
'Play-by-Play text for a reverse play.
Sub REVGAIN ()

    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP"

    pbpString$ = pbpString$ + " AND HANDS OFF TO " + rbacks$(P1, I1) + " ON THE REVERSE!!"
    Call PBP

    Call Rolld100(RN0)

    If hasRunRev(P9) = 0 Then

        Select Case RN0

            Case 1 To 20
                ydsGained = (Int(Rnd(1) * 8) + 5) * -1
                checkFumbRev = 1

            Case 21 To 80
                ydsGained = Int(Rnd(1) * 11)

            Case 81 To 100
                ydsGained = Int(Rnd(1) * 81)

        End Select

    Else
        'Team has already run a reverse
        'Play will be less effective
        Select Case RN0

            Case 1 To 5:
                ydsGained = Int(Rnd(1) * 81)

            Case 6 To 55:
                ydsGained = Int(Rnd(1) * 11)

            Case 56 To 100:
                ydsGained = (Int(Rnd(1) * 8) + 5) * -1
                checkFumbRev = 1

        End Select

    End If

    hasRunRev(P9) = 1

End Sub


'----------------------------------------
'       FGFakeRunYards Subroutine
'----------------------------------------
'This subroutine handles yardage and the
'Play-by-Play text for a fake run on a
'kicking lineup.
Sub FGFakeRunYards (choice)

    Select Case ydsToFirst

        Case 1 To 5
            'Short Yardage
            passFakeSuccess(1) = 50
            passFakeSuccess(2) = 25

        Case 6 To 15
            'Medium Yardage
            passFakeSuccess(1) = 35
            passFakeSuccess(2) = 15

        Case Is >= 16
            'Long Yardage
            passFakeSuccess(1) = 5
            passFakeSuccess(2) = 1

    End Select

    pbpString$ = "IT'S A FAKE!!! THE HOLDER IS RUNNING THE BALL"
    Call PBP

    Call Rolld100(RN0)

    Select Case choice

        Case 1
            'Block

            Select Case RN0

                Case 1 To passFakeSuccess(1):
                    ydsGained = Int(Rnd(1) * 21)

                Case passFakeSuccess(1) + 1 To 95:
                    ydsGained = Int(Rnd(1) * 11) - 4

                Case 96 To 100:
                    ydsGained = 100 - ydLine

            End Select

        Case 2
            'FG Safe

            Select Case RN0

                Case 1 To passFakeSuccess(2):
                    ydsGained = Int(Rnd(1) * 21)

                Case passFakeSuccess(2) + 1 To 99:
                    ydsGained = Int(Rnd(1) * 7) - 7

                Case 100:
                    ydsGained = 100 - ydLine

            End Select

    End Select

    Call YARDAGE

    playSegment = 1

    startYds = 100 - ydLine
    endYds = startYds - ydsGained
    Call MOVEBALL

    pbpString$ = "THE HOLDER GAINED"

    scoreX0 = 10
    scoreX1 = 11

    fakeYdLine! = CInt((ydsGained + ydLine) * 10) / 10

    fakeYds! = CInt((ydsGained) * 10) / 10

    If fakeYds! = 0 Then

        pbpString$ = pbpString$ + " NOTHING ON THE FAKE RUN"

    Else

        If fakeYdLine! >= 99.94999 Then
            pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
        Else
            pbpString$ = pbpString$ + Str$(fakeYds!)
        End If

        pbpString$ = pbpString$ + " YARDS ON THE FAKE RUN"

    End If

    Call PBP

End Sub


'----------------------------------------
'           HANDOFF Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'text for a run play.
Sub HANDOFF ()

    Call DisplayDefense

    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP"

    If playCall = 4 Or playCall = 5 Then

        Call Rolld100(randSide)
        If randSide <= 58 Then
            '58% left
            fieldSide = 1
        Else
            '42% Right
            fieldSide = 0
        End If

        If rbacks$(P1, I1) = qbacks$(P1, qbNumber(P1)) Then

            pbpString$ = pbpString$ + " AND KEEPS IT ON THE " + playDesc$(chosenPlay)
            Call PBP

        Else
            RN0 = Int(Rnd(1) * 2) + 1

            If RN0 = 1 Then
                pbpString$ = pbpString$ + " AND TOSSES OUT TO " + rbacks$(P1, I1)
            Else
                'RN0 = 2
                pbpString$ = pbpString$ + " AND PITCHES OUT TO " + rbacks$(P1, I1)
            End If

            Call PBP

            pbpString$ = rbacks$(P1, I1) + " GOES AROUND THE " + direction$(fieldSide) + " END"
            Call PBP

        End If

    Else

        If rbacks$(P1, I1) = qbacks$(P1, qbNumber(P1)) Then
            pbpString$ = pbpString$ + " AND KEEPS IT ON THE " + playDesc$(chosenPlay)
        Else
            pbpString$ = pbpString$ + " AND HANDS OFF TO " + rbacks$(P1, I1) + " ON THE " + playDesc$(chosenPlay)
        End If

        Call PBP

    End If

End Sub


'----------------------------------------
'        DisplayDefense Subroutine
'----------------------------------------
'Outputs current line and coverage that
'was selected by the defense
Sub DisplayDefense

    formationString = Int(Rnd(1) * 3) + 1

    Select Case formationString

        Case 1
            pbpString$ = "THE DEFENSE LINES UP IN "

        Case 2
            pbpString$ = gameTeams$(D) + " LINE UP IN "

        Case 3
            pbpString$ = gameMascots$(D) + " LOOK TO BE IN "

    End Select

    'Display defensive coverage
    If dLine < 11 Then

        Select Case rushEra

            Case 0
                pbpString$ = pbpString$ + defFormation$(dLine)

            Case 1
                pbpString$ = pbpString$ + defFormEraA$(dLine)

            Case 2
                pbpString$ = pbpString$ + defFormEraB$(dLine)

        End Select

        pbpString$ = pbpString$ + " " + passCov$(coverage)

    Else

        If dLine = 11 Then
            pbpString$ = pbpString$ + "NICKEL"
        End If

        If dLine = 12 Then
            pbpString$ = pbpString$ + "PREVENT"
        End If

        If dLine = 13 Then
            pbpString$ = pbpString$ + "JUMBO FORMATION"
        End If

    End If

    Call PBP

End Sub


'----------------------------------------
'          PASSBALL Subroutine
'----------------------------------------
'This subroutine helps setup the stage for
'a pass play, including Play-by-Play text.
'This includes determining who the ball is
'going to and where they might be.
Sub PASSBALL (direction)

    overuseAdj = 0

    'Pick QB based on contribution
    Call Rolld100(qbChance)

    For J = 3 To 0 Step -1
        If qbChance <= qbNumAtt(P1, J) Then
            I1 = J
        End If
    Next J

    If useRandomQB(P1) = 1 Then
        'Update selected QB from random choice
        If I1 > 0 Then
            qbNumber(P1) = I1
        End If
        qbNumber(P1) = I1
        qbContrib = GetQBRushPctGame%(qbacks$(P1, qbNumber(P1)), currTeam)
        qbMobility(P1) = GetQBMobility%(qbContrib)
    Else
        'Assign seleted QB
        I1 = qbNumber(P1)
    End If

    'Determine target WR
    Do

        Call Rolld100(recChance)

        If playCall = 13 Then
            'Bias the roll to favor running backs more
            '(0 - 85% cumulative recept contribs)
            recChance = recChance * 0.85
        End If

        If playCall = 12 Or playCall = 15 Then
            'Bias the roll to favor wide receivers more
            '(50+% cumulative recept contribs)
            recChance = recChance * 0.5 + 50
        End If

        qbChosen = 0

        For J = 15 To 10 Step -1
            If recChance <= wrNumRec(P1, J - 10) Then
                I3 = J
            End If
        Next J

        For J = 9 To 0 Step -1
            If recChance <= rbNumRec(P1, J) Then
                I3 = J
            End If
        Next J

        'Look to see if a QB was chosen
        If I3 > 9 Then
            If wdRec$(P1, I3 - 10) = qbacks$(P1, qbNumber(P1)) Then
                qbChosen = 1
            End If
        End If

        If I3 < 10 Then
            If rbacks$(P1, I3) = qbacks$(P1, qbNumber(P1)) Then
                qbChosen = 1
            End If
        End If

    Loop Until qbChosen = 0

    Call DisplayDefense

    Call PASSPBP

    If I3 > 9 Then
        'Target WR = wide receiver / tight end
        targetWRName$ = wdRec$(P1, I3 - 10)
    Else
        'Target WR = running back
        targetWRName$ = rbacks$(P1, I3)
    End If

    RN0 = Int(Rnd(1) * 6) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE'S GOT "
        Case 2: pbpString$ = "HE SPOTS "
        Case 3: pbpString$ = "HE'S LOOKING FOR "
        Case 4: pbpString$ = "HE SEES "
        Case 5: pbpString$ = "HE'S SEARCHING FOR "
        Case 6: pbpString$ = "HE FINDS "
    End Select

    Call WHICHRECEIVER

    Call Rolld100(randDir)

    Select Case randDir

        Case 1 To 22:
            'Middle (22%)
            direction = 3

        Case 23 To 60
            'Left/Sideline (38%)
            direction = 2

        Case 61 To 100
            'Right/Downfield (40%)
            direction = 4

    End Select

    ' Add depth variation for passes
    Call Rolld100(depthRoll)
    If depthRoll <= 15 Then
        'Deep (15% of passes)
        direction = 5
    End If
    If depthRoll >= 16 And depthRoll <= 23 Then
        'Wide open (8% of passes)
        direction = 1
    End If

    Select Case direction

        Case 1, 6
            pbpString$ = pbpString$ + " WIDE OPEN"

        Case 2
            pbpString$ = pbpString$ + " ON THE SIDELINE"

        Case 3
            If playCall <> 13 Then
                pbpString$ = pbpString$ + " OVER THE MIDDLE"
            End If

        Case 4
            If playCall <> 13 Then
                pbpString$ = pbpString$ + " DOWNFIELD"
            End If

        Case 5
            If playCall = 12 Or playCall = 15 Then
                pbpString$ = pbpString$ + " DEEP"
            End If

    End Select

    Call PBP

End Sub


'----------------------------------------
'      AdjustCompPct Subroutine
'----------------------------------------
'This subroutine performs adjustments to
'the QB's completion %. It shifts it based
'on the defense's overall rating), ball
'position, play type and wind (windCmpAdj)

'Passes are completed if the random # generated
' is below the sum of all these ratings

Sub AdjustCompPct ()

    'Comp %, adjusted for DEF team's rating
    expCompPct = qbCompPct(P1, I1) + Int(leagRat_GAME(D, 1))

    'Home Field Advantage for Pass Comp %
    If P1 = gameLoc Then
        'Home: +3%
        expCompPct = expCompPct + 3
    Else
        'Away (not Neutral): -3%
        If gameLoc <> 2 Then expCompPct = expCompPct - 3
    End If

    'Adjust ?? based on position
    Select Case ydLine

        'Goal Line to Own 0
        Case 0 To 40.071:
            ballPosCmpAdj = 3

            'Own 40 to Opp 20
        Case 40.072 To 80.071:
            ballPosCmpAdj = -2

            'Opp 20 to Opp 15
        Case 80.072 To 85.071:
            ballPosCmpAdj = -4

            'Opp 15 to Opp 10
        Case 85.072 To 90.071:
            ballPosCmpAdj = -10

            'Opp 10 to Opp 5
        Case 90.072 To 95.071:
            ballPosCmpAdj = -26

            'Opp 5 to  Goal
        Case Is > 95.071:
            ballPosCmpAdj = -13

    End Select

    'Adjust for pass type
    passTypeCmpAdj = passCovAdj(1, playCall - 8, coverage)

    windCmpAdj = 0

    'Wind direction NOT in offense's favor
    If climate(0) = D Then

        If (playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17 Or playCall = 18) Then
            'Medium Pass
            windCmpAdj = climate(2)
        End If

        If (playCall = 12 Or playCall = 15) Then
            'Long Pass
            windCmpAdj = climate(3) + climate(6)
        End If

    End If

End Sub


'----------------------------------------
'          SCOREUPD Subroutine
'----------------------------------------
'This subroutine tracks variables related
'to team score and also updates the current
'score in the game.
Sub SCOREUPD ()

    'current "score" of the game
    nbrScores = nbrScores + 1

    scoreTimes(nbrScores) = gameClock!
    scoreQuarters(nbrScores) = quarter

    'If quarter = 4 Then
    '
    'End If

    If scoreX1 <> 7 Then
        K1(nbrScores, 1) = P1
    Else
        K1(nbrScores, 1) = D
    End If

    K1(nbrScores, 0) = scoreX1
    K1(nbrScores, 5) = 100 - ydLine

    If scoreX1 = 3 Then
        K1(nbrScores, 5) = 100 - kickYL!
    End If

    If scoreX1 = 8 Then
        K1(nbrScores, 5) = 117 + goalPostAdj - ydLine
    End If

    If scoreX1 = 5 Or scoreX1 = 12 Then
        K1(nbrScores, 2) = defPlayer
    Else
        K1(nbrScores, 2) = I1
    End If

    K1(nbrScores, 3) = I3

    If K1(nbrScores, 5) < 1 Then
        K1(nbrScores, 5) = 1
    End If

End Sub


'----------------------------------------
'          COMPILER Subroutine
'----------------------------------------
'This subroutine is used to increase team
'and player stats. It appears to be code
'that was not used fully but in transition
'It seems to be designed to simply let YOU
'send the right parameters and it will do
'the work to increase pass att, yards, etc.
Sub COMPILER ()

    If scoreX0 >= 1 And scoreX0 <= 4 Then

        Select Case scoreX0

            Case 1, 4
                'Team Rush Att
                gameStatsTeam(P1, 1) = gameStatsTeam(P1, 1) + 1

                'Team Rush Yds
                If ydsGained + ydLine < 99.95 Then
                    gameStatsTeam(P1, 2) = gameStatsTeam(P1, 2) + ydsGained
                Else
                    gameStatsTeam(P1, 2) = gameStatsTeam(P1, 2) + 100 - ydLine
                End If

            Case 2
                'Team Pass Att
                gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1

            Case 3
                'Team Pass Att
                gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                'Team Pass Comp
                gameStatsTeam(P1, 4) = gameStatsTeam(P1, 4) + 1

                'Team Pass Yds
                '-- Do we never substract passing yards for a sack?
                '-- Or does that happen elsewhere?
                If ydsGained + ydLine < 99.95 Then
                    gameStatsTeam(P1, 6) = gameStatsTeam(P1, 6) + ydsGained
                Else
                    gameStatsTeam(P1, 6) = gameStatsTeam(P1, 6) + 100 - ydLine
                End If

        End Select


        Select Case scoreX0

            Case 1

                'RB Rush Att
                gameStatsPlayer(P1, I1, 0) = gameStatsPlayer(P1, I1, 0) + 1

                If ydsGained + ydLine >= 99.95 Then
                    'RB Rush Yards
                    gameStatsPlayer(P1, I1, 1) = gameStatsPlayer(P1, I1, 1) + 100 - ydLine
                    'RB Rush TDs
                    gameStatsPlayer(P1, I1, 2) = gameStatsPlayer(P1, I1, 2) + 1
                    BG = 100 - ydLine
                    If BG > gameLongRunYds(P1, I1) Then
                        gameLongRunTD$(P1, I1) = "t"
                        gameLongRunYds(P1, I1) = BG
                    End If
                Else
                    'RB Rush Yards
                    gameStatsPlayer(P1, I1, 1) = gameStatsPlayer(P1, I1, 1) + ydsGained
                    If ydsGained > gameLongRunYds(P1, I1) Then
                        gameLongRunTD$(P1, I1) = ""
                        gameLongRunYds(P1, I1) = ydsGained
                    End If
                End If

            Case 2

                If halfbackPass = 0 Then
                    'Pass Att
                    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                End If

            Case 3

                If halfbackPass <> 1 Then

                    'Pass Att
                    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                    'Pass Comp
                    gameStatsPlayer(P1, I1, 10) = gameStatsPlayer(P1, I1, 10) + 1

                    If ydsGained + ydLine >= 99.95 Then
                        'Pass Yards
                        gameStatsPlayer(P1, I1, 12) = gameStatsPlayer(P1, I1, 12) + 100 - ydLine
                        'Pass TDs
                        gameStatsPlayer(P1, I1, 13) = gameStatsPlayer(P1, I1, 13) + 1
                    Else
                        'Pass Yards
                        gameStatsPlayer(P1, I1, 12) = gameStatsPlayer(P1, I1, 12) + ydsGained
                    End If

                End If

                If I3 > 9 Then

                    If ydsGained + ydLine >= 99.95 Then
                        'WR Rec Yds
                        gameStatsPlayer(P1, I3 - 10, 7) = gameStatsPlayer(P1, I3 - 10, 7) + 100 - ydLine
                        'WR Rec TDs
                        gameStatsPlayer(P1, I3 - 10, 8) = gameStatsPlayer(P1, I3 - 10, 8) + 1
                    Else
                        'WR Rec Yds
                        gameStatsPlayer(P1, I3 - 10, 7) = gameStatsPlayer(P1, I3 - 10, 7) + ydsGained
                    End If

                    'WR Recepts
                    gameStatsPlayer(P1, I3 - 10, 6) = gameStatsPlayer(P1, I3 - 10, 6) + 1

                Else

                    If ydsGained + ydLine >= 99.95 Then
                        'RB Rec Yds
                        gameStatsPlayer(P1, I3, 4) = gameStatsPlayer(P1, I3, 4) + 100 - ydLine
                        'RB Rec TDs
                        gameStatsPlayer(P1, I3, 5) = gameStatsPlayer(P1, I3, 5) + 1
                    Else
                        'RB Rec Yds
                        gameStatsPlayer(P1, I3, 4) = gameStatsPlayer(P1, I3, 4) + ydsGained
                    End If

                    'RB Recepts
                    gameStatsPlayer(P1, I3, 3) = gameStatsPlayer(P1, I3, 3) + 1

                End If

            Case 4

                If W5 = 0 Or W5 = 1 And ruleOptType = 0 Then
                    'Rush Att
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 0) = gameStatsPlayer(P1, qbNumber(P1) + 10, 0) + 1
                End If

                If W5 = 0 And ydsGained + ydLine < 99.95 Or W5 = 1 And ruleOptType = 0 And ydsGained + ydLine < 99.95 Then
                    'Rush Yards
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 1) = gameStatsPlayer(P1, qbNumber(P1) + 10, 1) + ydsGained
                End If

                If W5 = 0 And ydsGained + ydLine >= 99.95 Or W5 = 1 And ruleOptType = 0 And ydsGained + ydLine >= 99.95 Then
                    'Rush Yds
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 1) = gameStatsPlayer(P1, qbNumber(P1) + 10, 1) + 100 - ydLine
                    'Rush TDs
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 2) = gameStatsPlayer(P1, qbNumber(P1) + 10, 2) + 1
                End If

                If W5 = 1 Then
                    'Team Sacks
                    sackStatsTeam(P1, 0) = sackStatsTeam(P1, 0) + 1
                    'Team Sack Yds
                    sackStatsTeam(P1, 1) = sackStatsTeam(P1, 1) + ydsGained * -1
                    'Opp Sacks
                    sackStatsOpp(D, defPlayer, 0) = sackStatsOpp(D, defPlayer, 0) + 1
                    'Opp Sack Yds
                    sackStatsOpp(D, defPlayer, 1) = sackStatsOpp(D, defPlayer, 1) + ydsGained * -1
                End If

        End Select

    End If

    If halfbackPass = 1 Then halfbackPass = 0
    scoreX0 = 0

End Sub


'----------------------------------------
'           TwoMinMenu Subroutine
'----------------------------------------
'This subroutine gives the user a menu of
'options for the last 2 minutes of the
'game and handles the appropriate changes
'to team variables
Sub TwoMinMenu (menuChoice)

    teamRushPct = leagRat_GAME(P1, 7)

    'If Not (playerMode = 2 And pbpDelay > 0) Then
    If pbpDelay! > 0 Then

        Call OPTIONBOX(6)

        Locate 9, 14: Print "1 - HUDDLE"
        Locate , 14: Print "2 - TIME OUT "; gameTeams$(P1); " ";
        Locate , 43: Print timeouts(P1)
        Locate , 14: Print "3 - TIME OUT "; gameTeams$(D); " ";
        Locate , 43: Print timeouts(D)
        Locate , 14: Print "4 - HURRY UP OFFENSE"
        If teamYears(P1) < 1990 Then
            Locate , 14: Print "5 - THROW AWAY PASS"
        Else
            Locate , 14: Print "5 - SPIKE THE BALL"
        End If

    End If

    Do

        'We'll get here even at 2 minutes
        'So we need to check if < 2 mins before
        'taking any timeouts
        If playerMode > 0 Then

            scoreDiff = score(P1, 0) - score(D, 0)

            'If computer is on Defense
            If (D = compTeam Or playerMode = 2) Then

                'If DEF is behind, take TO to get extra time
                If playSegment <> 4 And (scoreDiff > 0 And timeouts(D) > 0 And gameClock! < 120) Then
                    Print #11, "DEF behind - take TO"
                    menuChoice = 3
                End If

            End If

            'Defense gets priority for TO
            'This is because we're only checking
            'when they are behind.
            If (P1 = compTeam Or playerMode = 2) And menuChoice <> 3 Then
                'If computer is on Offense

                If quarter = 2 Then

                    '2nd quarter
                    '-----------

                    'Offense ahead, outside the 40 - Modern era
                    'Offense is behind outside the 15 (easy FG range or med/long pass)
                    If (scoreDiff >= 0 And ydLine > 40 And teamRushPct < 65) Or _ 
                        (scoreDiff <= 0 And ydLine > 15) Then
                        'Try to score but take timeouts as needed
                        If timeouts(P1) > 0 Then
                            If gameClock! < 120 Then menuChoice = 2
                        Else
                            menuChoice = 4
                        End If
                        Exit Do
                    End If

                Else

                    '4th quarter
                    '-----------

                    'Offense is behind by 2 scores
                    'They NEED to score fast
                    '-- UPDATE to only be if clock isn't stopped
                    '-- playSegment <> 4
                    If scoreDiff <= 0 And scoreDiff >= -14 Then
                        If timeouts(P1) > 0 Then
                            If gameClock! < 120 Then menuChoice = 2
                        Else
                            menuChoice = 4
                        End If
                        Exit Do
                    End If

                End If

            End If

        End If

        'Check that the computer didn't already call timeout)
        If playerMode = 0 Or (playerMode = 1 And (menuChoice <> 2 And menuChoice <> 3)) Then
            Do
                I$ = GetKeyPress$
                menuChoice = Val(I$)
            Loop Until menuChoice >= 1 And menuChoice <= 5
        End If

    Loop Until Not ((menuChoice = 2 Or menuChoice = 4 Or menuChoice = 5) And playerMode = 1 And P1 = compTeam) Or (menuChoice = 3 And playerMode = 1 And D = compTeam)

    Color 0, 5 * (1 - BW)

    Call DisplayField

End Sub


'----------------------------------------
'       FieldGoalChance Subroutine
'----------------------------------------
'This subroutine calculcates the distance
'for a FG
Sub FieldGoalChance ()

    windAdj = 0

    Select Case climate(0)

        Case P1

            If climate(1) = -7 Then
                '5-10% impact
                windAdj = 0 - Int(Rnd(1) * 5) + 5
            End If
            If climate(1) = -15 Then
                '15-20% impact
                windAdj = 0 - Int(Rnd(1) * 5) + 15
            End If

        Case D

            If climate(1) = -7 Then
                '5-10% impact
                windAdj = Int(Rnd(1) * 5) + 5
            End If
            If climate(1) = -15 Then
                '15-20% impact
                windAdj = Int(Rnd(1) * 5) + 15
            End If

        Case 2

            If climate(1) = -7 Then
                '5% impact
                windAdj = -5
            End If
            If climate(1) = -15 Then
                '10% impact
                windAdj = -10
            End If
            '40+ yard field goals impacted more
            If ydLine <= 77 + goalPostAdj Then
                windAdj = windAdj - 5
            End If

    End Select

    'Increase average FG% chance for newer eras
    Select Case teamYears(P1)

        Case 1970 To 1978
            adjFGPct(0) = 85
            adjFGPct(1) = 63
            adjFGPct(2) = 43
            '20% for 50+
            adjFGPct(3) = 23
            adjFGPct(4) = 18

        Case 1979 To 1991
            adjFGPct(0) = 92
            adjFGPct(1) = 75
            adjFGPct(2) = 56
            '36% for 50+
            adjFGPct(3) = 39
            adjFGPct(4) = 34

        Case 1992 To 2011
            adjFGPct(0) = 97
            adjFGPct(1) = 84
            adjFGPct(2) = 68
            '52% for 50+
            adjFGPct(3) = 55
            adjFGPct(4) = 50

        Case Is >= 2012
            adjFGPct(0) = 99
            adjFGPct(1) = 92
            adjFGPct(2) = 78
            '65% for 50+
            adjFGPct(3) = 68
            adjFGPct(4) = 62

    End Select

    For I = 4 To 0 Step -1:

        If ydLine >= adjFGLine(I) + goalPostAdj + windAdj Then

            'Why 9 - here??
            fgSuccessChance = adjFGPct(I) - ((9 - kickerFGA(P1, activeKicker(P1))) * 5)

            If ydLine >= 68 + goalPostAdj Then
                'Less than 50 yard attempt
                'Adjust success chance based on distance from midpoint of range
                'For example, for 30-39 yd attempts, 35 yd is the midpoint
                'So success for 35 is the average; 34 yd attempt is +1%; 36 yd is -1%
                fgSuccessChance = fgSuccessChance + (ydLine - adjFGLine(I)) + goalPostAdj - 4 + windAdj
            End If

        End If

    Next I

    'Home Field Advantage - Field Goal Chance
    If P1 = gameLoc Then
        'Home: +1%
        fgSuccessChance = fgSuccessChance + 1
    Else
        'Away (not Neutral): -1%
        fgSuccessChance = fgSuccessChance - 1
    End If

    If fgSuccessChance <= 0 Then
        'Always a 5% chance success
        'Data backs this up since even the worst league years,
        'kickers made 5% of kicks over 50+
        fgSuccessChance = 5
    End If

    If fgSuccessChance >= 100 Then
        'Always a 5% chance failure
        fgSuccessChance = 95
    End If

End Sub


'----------------------------------------
'           RUNMENU Subroutine
'----------------------------------------
'This subroutine prints the different
'options for a run play
Sub RUNMENU ()

    Call FRAMEOPTION(12, P1)

    Print "SELECT RUNNING PLAY "; gameTeams$(P1)
    Print
    Locate , 14: Print "1 - BLAST                 9 - POWER SWEEP "
    Locate , 14: Print "2 - DIVE                 10 - QUICK PITCH "
    Locate , 14: Print "3 - OFF TACKLE           11 - QUICK TRAP  "
    Locate , 14: Print "4 - TOSS SWEEP           12 - ISOLATION   "
    Locate , 14: Print "5 - OPTION RUN           13 - SPRINT DRAW "
    Locate , 14: Print "6 - DRAW PLAY            14 - OVER THE TOP"
    Locate , 14: Print "7 - TRAP/MISDIRECTION    15 - COUNTER     "
    Locate , 14: Print "8 - QB SNEAK                              "

End Sub


'----------------------------------------
'          PASSMENU Subroutine
'----------------------------------------
'This subroutine prints the different
'options for a pass play
Sub PASSMENU ()

    Call FRAMEOPTION(11, P1)

    Print "SELECT PASS PLAY "; gameTeams$(P1)
    Print
    Locate , 14: Print "1 - 3 STEP DROP           8 - SIDELINE PASS"
    Locate , 14: Print "2 - SHORT PASS            9 - ROLLOUT PASS "
    Locate , 14: Print "3 - MEDIUM PASS          10 - BOOTLEG PASS "
    Locate , 14: Print "4 - LONG PASS            11 - CROSS        "
    Locate , 14: Print "5 - SCREEN PASS          12 - CURL PASS    "
    Locate , 14: Print "6 - MED PLAY ACT PASS    13 - DEEP POST    "
    Locate , 14: Print "7 - LONG PLAY ACT PASS   14 - DEEP CORNER  "

End Sub


'----------------------------------------
'        SHORTYARDAGE Subroutine
'----------------------------------------
'This subroutine calculates yardage gains
'for Goal Line situations.
Sub SHORTYARDAGE ()

    Call Rolld100(RN6)

    'All Except Pinch / Pass Rush
    If (rushEra = 0 And dLine > 3 And dLine < 6) Or (rushEra >= 1 And dLine <> 8) Then

        Select Case ydLine

            Case 94.999 To 95.998

                Select Case RN6
                    Case 1 To 17: ydsGained = 6
                    Case 18 To 22: ydsGained = 4
                    Case 23 To 35: ydsGained = 3
                    Case 36 To 52: ydsGained = 2
                    Case 53 To 76: ydsGained = 1
                    Case 77 To 93: ydsGained = 0
                    Case 94 To 100: ydsGained = -1
                End Select

            Case 95.999 To 96.998

                Select Case RN6
                    Case 1 To 26: ydsGained = 5
                    Case 27 To 29: ydsGained = 3
                    Case 30 To 49: ydsGained = 2
                    Case 50 To 78: ydsGained = 1
                    Case 79 To 94: ydsGained = 0
                    Case 95 To 100: ydsGained = -1
                End Select

            Case 96.999 To 97.998

                Select Case RN6
                    Case 1 To 27: ydsGained = 4
                    Case 28 To 32: ydsGained = 2
                    Case 33 To 80: ydsGained = 1
                    Case 81 To 98: ydsGained = 0
                    Case 99 To 100: ydsGained = -1
                End Select

            Case 97.999 To 98.998

                Select Case RN6
                    Case 1 To 46: ydsGained = 3
                    Case 47 To 72: ydsGained = 1
                    Case 73 To 91: ydsGained = 0
                    Case 92 To 100: ydsGained = -1
                End Select

            Case Is >= 98.999

                Select Case RN6
                    Case 1 To 61: ydsGained = 2
                    Case 62 To 96: ydsGained = 0
                    Case 97 To 100: ydsGained = -1
                End Select

        End Select

    Else

        'Pinch / Pass Rush
        Select Case ydLine

            Case 94.999 To 95.998

                Select Case RN6
                    Case 1 To 14: ydsGained = 6
                    Case 15 To 17: ydsGained = 4
                    Case 18 To 29: ydsGained = 3
                    Case 30 To 43: ydsGained = 2
                    Case 44 To 63: ydsGained = 1
                    Case 64 To 77: ydsGained = 0
                    Case 78 To 84: ydsGained = -1
                    Case 85 To 91: ydsGained = -2
                    Case 92 To 98: ydsGained = -3
                    Case 99 To 100: ydsGained = -4
                End Select

            Case 95.999 To 96.998

                Select Case RN6
                    Case 1 To 29: ydsGained = 5
                    Case 30 To 31: ydsGained = 3
                    Case 32 To 45: ydsGained = 2
                    Case 46 To 72: ydsGained = 1
                    Case 73 To 86: ydsGained = 0
                    Case 87 To 90: ydsGained = -1
                    Case 91: ydsGained = -2
                    Case 92 To 94: ydsGained = -3
                    Case 95 To 97: ydsGained = -4
                    Case 98 To 100: ydsGained = -5
                End Select

            Case 96.999 To 97.998

                Select Case RN6
                    Case 1 To 24: ydsGained = 4
                    Case 25 To 28: ydsGained = 2
                    Case 29 To 69: ydsGained = 1
                    Case 70 To 85: ydsGained = 0
                    Case 86 To 88: ydsGained = -1
                    Case 89 To 93: ydsGained = -2
                    Case 94 To 100: ydsGained = -4
                End Select

            Case 97.999 To 98.998

                Select Case RN6
                    Case 1 To 43: ydsGained = 3
                    Case 44 To 65: ydsGained = 1
                    Case 66 To 83: ydsGained = 0
                    Case 84 To 92: ydsGained = -1
                    Case 93 To 94: ydsGained = -2
                    Case 95 To 100: ydsGained = -4
                End Select

            Case Is >= 98.999

                Select Case RN6
                    Case 1 To 57: ydsGained = 2
                    Case 58 To 74: ydsGained = 0
                    Case 75 To 83: ydsGained = -1
                    Case 84 To 90: ydsGained = -2
                    Case 91 To 97: ydsGained = -3
                    Case 98 To 100: ydsGained = -4
                End Select

        End Select

    End If

    Call Rolld100(RN7)

    If rushBase <= 30 Then
        If RN7 <= goalLnYdAdj(rushBase, 1) Then
            ydsGained = ydsGained + goalLnYdAdj(rushBase, 2)
        End If
    Else
        'rushBase < 30
        If RN7 <= goalLnYdAdj(30, 1) Then
            ydsGained = ydsGained + goalLnYdAdj(30, 2)
        End If
    End If

End Sub


'----------------------------------------
'           PASSPBP Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'string for pass play.
Sub PASSPBP ()

    defTeam$ = "THE DEFENSE"

    Call Rolld100(RN0)

    If coverage >= 7 Then
        'Defense is blitzing

        Select Case RN0

            Case 1 To 33:
                pbpString$ = "BLITZ IS COMING...!!"
                Call PBP

            Case 34 To 66:
                pbpString$ = "HERE COMES THE BLITZ...!!"
                Call PBP

            Case 67 To 109:
                pbpString$ = "BLITZ IS ON...!!"
                Call PBP

                If defSacks(D) = 0 Then
                    defTeam$ = gameTeams$(D)
                Else

                    M5 = 0
                    RN1 = Int(Rnd(1) * teamSacks(D)) + 1

                    'Determine player for sacks based on contribution
                    For defPlayer = 0 To 14

                        M5 = M5 + playerSacks(D, defPlayer)

                        If RN1 <= M5 Then
                            defTeam$ = defSacks$(D, defPlayer)
                            Exit For

                        End If

                    Next

                End If

        End Select

    End If

    RN0 = Int(Rnd(1) * 15) + 1
    If (coverage >= 7 Or dLine >= 9) Then

        Select Case RN0

            Case 1: pbpString$ = qbacks$(P1, I1) + " HAS TO HURRY"
            Case 2: pbpString$ = qbacks$(P1, I1) + " IS UNDER A HEAVY PASS RUSH"
            Case 3: pbpString$ = qbacks$(P1, I1) + " IS BEING RUSHED"
            Case 4: pbpString$ = qbacks$(P1, I1) + " IS BEING PRESSURED"
            Case 5: pbpString$ = qbacks$(P1, I1) + " FEELS THE HEAT"
            Case 6: pbpString$ = qbacks$(P1, I1) + " IS BEING CHASED"
            Case 7: pbpString$ = qbacks$(P1, I1) + " IS BEING HURRIED"
            Case 8: pbpString$ = qbacks$(P1, I1) + " IS FORCED OUT OF THE POCKET"
            Case 9: pbpString$ = qbacks$(P1, I1) + " IS SCRAMBLING"
            Case 10: pbpString$ = qbacks$(P1, I1) + " IS UNDER THE GUN"
            Case 11: pbpString$ = qbacks$(P1, I1) + "'S POCKET IS COLLAPSING"
            Case 12: pbpString$ = qbacks$(P1, I1) + "'S PROTECTION IS BREAKING DOWN"
            Case 13: pbpString$ = "HERE COMES " + defTeam$
            Case 14: pbpString$ = defTeam$ + " BREAKS THROUGH"
            Case 15: pbpString$ = defTeam$ + " IS COMING THROUGH"

        End Select

    Else

        Select Case playCall

            Case 9, 10
                If RN0 <= 50 Then
                    pbpString$ = qbacks$(P1, I1) + " TAKES THE SNAP AND DROPS BACK...LOOKING SHORT..."
                Else
                    pbpString$ = qbacks$(P1, I1) + " DROPS QUICKLY AND SETS..."
                End If

            Case 11, 16

                Select Case RN0

                    Case 1 To 30: pbpString$ = qbacks$(P1, I1) + " DROPS BACK AND LOOKS..."
                    Case 31 To 40: pbpString$ = qbacks$(P1, I1) + " DROPS BACK...HE'S UNDER PRESSURE..."
                    Case 41 To 60: pbpString$ = qbacks$(P1, I1) + " TO THROW...HAS TIME..."
                    Case 61 To 70: pbpString$ = qbacks$(P1, I1) + " BACK TO PASS...HAS PROTECTION..."
                    Case 71 To 80: pbpString$ = qbacks$(P1, I1) + " DROPS BACK...IS BEING CHASED!!"
                    Case 81 To 100: pbpString$ = qbacks$(P1, I1) + " HAS PLENTY OF TIME TO THROW..."

                End Select

            Case 12
                If RN0 <= 50 Then
                    pbpString$ = qbacks$(P1, I1) + " DROPS BACK...HE WANTS TO GO DEEP"
                Else
                    pbpString$ = qbacks$(P1, I1) + " TAKES A DEEP DROP...HE'S LOOKING LONG"
                End If

            Case 13
                If RN0 <= 50 Then
                    pbpString$ = qbacks$(P1, I1) + " DROPPING...BEING CHASED..."
                Else
                    pbpString$ = qbacks$(P1, I1) + " DROPS BACK...HE'S UNDER PRESSURE..."
                End If

            Case 14, 15
                If RN0 <= 50 Then
                    pbpString$ = qbacks$(P1, I1) + " MAKES A GOOD PLAY FAKE..."
                Else
                    pbpString$ = qbacks$(P1, I1) + " FAKES THE GIVE AND WANTS TO THROW IT..."
                End If

            Case 17
                pbpString$ = qbacks$(P1, I1) + " ROLLS OUT..."

            Case 18
                pbpString$ = qbacks$(P1, I1) + " HAS IT ON THE BOOTLEG"

        End Select

    End If

    Call PBP

End Sub


'----------------------------------------
'            SACK Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'text for a sack.
Sub SACK ()

    pbpString$ = "BUT HE WON'T BE ABLE TO GET A PASS OFF!"
    Call PBP

    If halfbackPass = 1 Then
        pbpString$ = rbacks$(P1, I1)
    Else
        pbpString$ = qbacks$(P1, I1)
    End If

    RN0 = Int(Rnd(1) * 12) + 1
    Call Rolld100(RN1)

    Select Case RN0
        Case 1: pbpString$ = pbpString$ + " IS SACKED"
        Case 2: If RN1 <= 20 Then pbpString$ = pbpString$ + " IS NAILED"
        Case 3: If RN1 <= 30 Then pbpString$ = pbpString$ + " IS BURIED"
        Case 4: If RN1 <= 40 Then pbpString$ = pbpString$ + " IS DRILLED"
        Case 5: If RN1 <= 50 Then pbpString$ = pbpString$ + " IS PANCAKED"
        Case 6: If RN1 <= 60 Then pbpString$ = pbpString$ + " IS THROWN"
        Case 7: If RN1 <= 70 Then pbpString$ = pbpString$ + " IS HAMMERED"
        Case 8: If RN1 <= 80 Then pbpString$ = pbpString$ + " IS TACKLED"
        Case 9: If RN1 <= 90 Then pbpString$ = pbpString$ + " IS SWARMED UNDER"
        Case 10: pbpString$ = pbpString$ + " IS LEVELED"
        Case 11: pbpString$ = pbpString$ + " IS TACKLED"
        Case 12: pbpString$ = pbpString$ + " IS WRAPPED UP"
    End Select

    pbpString$ = pbpString$ + " FOR A" + Str$(CInt(ydsGained)) + " YARD LOSS"

End Sub


'----------------------------------------
'           QBLINE Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play for
'when the QB approaches the o-line.
Sub QBLINE ()

    test1 = 1
    qback = 0

    If useRandomQB(P1) = 1 Then
        'Assign the starting QB
        qbNumber(P1) = 0
        qbContrib = GetQBRushPctGame%(qbacks$(P1, qbNumber(P1)), currTeam)
        qbMobility(P1) = GetQBMobility%(qbContrib)
    Else
        'use the randomly assigned QB
        qback = qbNumber(P1)
    End If

    Call Rolld100(RN1)

    'If Not (currDown <= 2 Or currDown = 3 And d100% <= 80) Then
    If (currDown = 3 And d100% > 80) Or currDown = 4 Then

        RN0 = Int(Rnd(1) * 10) + 1

        Select Case RN0
            Case 1: pbpString$ = gameTeams$(P1) + " COMES UP TO THE LINE"
            Case 2: pbpString$ = gameTeams$(P1) + " BREAKS THE HUDDLE TO LINE UP"
            Case 3: pbpString$ = "THE " + gameMascots$(P1) + " APPROACH THE LINE OF SCRIMMAGE"
            Case 4: pbpString$ = "THE " + gameMascots$(P1) + " LEAVE THE HUDDLE AND LINE UP"
            Case 5: pbpString$ = qbacks$(P1, qback) + " CALLS OUT THE SIGNALS"
            Case 6: pbpString$ = qbacks$(P1, qback) + " SURVEYS THE DEFENSE"
            Case 7: pbpString$ = "HERE IS THE " + downDesc$(currDown) + " DOWN AND" + Str$(Int(ydsToFirst)) + " YARD PLAY FOR " + gameTeams$(P1)
            Case 8: pbpString$ = "HERE IS THE " + downDesc$(currDown) + " DOWN AND" + Str$(Int(ydsToFirst)) + " YARD PLAY FOR THE " + gameMascots$(P1)
            Case 9: pbpString$ = qbacks$(P1, qback) + " SETTLES AT THE LINE OF SCRIMMAGE"
            Case 10: pbpString$ = gameTeams$(P1) + " LINES UP IN FORMATION"
        End Select

        Call PBP

    End If

End Sub


'----------------------------------------
'            SQUIB Subroutine
'----------------------------------------
'This subroutine handles the yardage AND
'Play-by-Play text for a squib kick
Sub SQUIB ()

    pbpString$ = kicker$(P1, activeKicker(P1)) + " IS PLACING THE BALL ON THE TEE"
    Call PBP

    pbpString$ = gameTeams$(P1) + " LINES UP FOR THE KICKOFF"
    Call PBP

    pbpString$ = "IT'S A SQUIBBER DOWN THE FIELD!"
    Call PBP

    Call CHANGEPOSS

    scoreX0 = 0

    kickYL! = 100 - (ydLine + Int(Rnd(1) * 16) + 29)
    kickYL! = kickYL! + (Int(Rnd(1) * 11) - 5) / 10
    ydLine = kickYL!

    ydsToScore = 100 - ydLine
    Call ShowChainMarker
    Call YARDLINE

    pbpString$ = "KICK TAKEN BY AN UPMAN AT THE" + Str$(CInt(kickYL!)) + " YD LINE "
    Call PBP

    ydsGained = Int(Rnd(1) * 7) + 9

    startYds = 100 - ydLine
    endYds = startYds - ydsGained
    Call MOVEBALL

    ydLine = kickYL! + ydsGained

    fumbPlayType = 3

End Sub


'----------------------------------------
'           PUNTPBP Subroutine
'----------------------------------------
'This subroutine creates the Play-by-play
'text for a punt.
Sub PUNTPBP ()

    RN0 = Int(Rnd(1) * 12) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE HITS IT"
        Case 2: pbpString$ = "HE NAILS IT"
        Case 3: pbpString$ = "HE KICKS IT"
        Case 4: pbpString$ = "HE LAUNCHES IT"
        Case 5: pbpString$ = "HE BOOTS IT"
        Case 6: pbpString$ = "HE BOOMS IT"
        Case 7: pbpString$ = "HE HITS ONE"
        Case 8: pbpString$ = "HE NAILS ONE"
        Case 9: pbpString$ = "HE KICKS ONE"
        Case 10: pbpString$ = "HE LAUNCHES ONE"
        Case 11: pbpString$ = "HE BOOTS ONE"
        Case 12: pbpString$ = "HE BOOMS ONE"
    End Select

    Call PBP

    RN0 = Int(Rnd(1) * 6) + 1

    Select Case RN0
        Case 1: pbpString$ = "IT'S A HIGH, SPIRALING KICK"
        Case 2: pbpString$ = "IT'S A HIGH FLOATER"
        Case 3: pbpString$ = "IT'S A HIGH END OVER END KICK"
        Case 4: pbpString$ = "IT'S A LOW, LINE DRIVE"
        Case 5: pbpString$ = "HE GETS IT TO TURN OVER"
        Case 6: pbpString$ = "HE DOESN'T GET IT TO TURN OVER"
    End Select

    Call PBP

End Sub


'----------------------------------------
'           STUFFED Subroutine
'----------------------------------------
'This subroutine generates a Play-by-Play
'for "stuffed" run plays / losses
Sub STUFFED ()

    RN0 = Int(Rnd(1) * 12) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE'S SNOWED UNDER BY A GANG OF TACKLERS"
        Case 2: pbpString$ = "HE MEETS A WALL OF DEFENDERS"
        Case 3: pbpString$ = "HE BOUNCES OFF ONE TACKLER"
        Case 4: pbpString$ = "HE SLIPS AND FALLS"
        Case 5: pbpString$ = "THE HOLE IS CLOSED"
        Case 6: pbpString$ = "HE FIGHTS FOR EXTRA YARDAGE"
        Case 7: pbpString$ = "HE'S STOOD UP!"
        Case 8: pbpString$ = "HIS FEET GO OUT FROM UNDER HIM"
        Case 9: pbpString$ = "HE SLIPS TO THE TURF"
        Case 10: pbpString$ = "DOWN HE GOES!!"
        Case 11: pbpString$ = "HE CAN'T MAKE HIS CUT"
        Case 12: pbpString$ = "HE CAN'T KEEP HIS FOOTING"
    End Select

    Call PBP

End Sub


'----------------------------------------
'          MISSEDFG Subroutine
'----------------------------------------
'This subroutine builds the Play-by-Play
'string for a missed Field Goal.
Sub MISSEDFG

    'direction of kick (right/left)
    fieldSide = Int(Rnd(1) * 2)

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 10: pbpString$ = "BAD SNAP! THE KICK IS WAY OFF!"

        Case 11 To 25: pbpString$ = "NO GOOD"

        Case 26 To 50: pbpString$ = "IT SAILS WIDE...NO GOOD"

        Case 51 To 70: pbpString$ = "HE MISSED IT!"

        Case 71 To 85: pbpString$ = "IT HITS THE UPRIGHTS....NO GOOD!"

        Case 86 To 90: pbpString$ = "HE HOOKS IT...NO GOOD!"

        Case 91 To 95:
            If 117 + goalPostAdj - ydLine > 45 Then
                pbpString$ = "IT FALLS SHORT....NO GOOD!"
            Else
                pbpString$ = "IT'S NO GOOD ... OFF TO THE " + direction$(fieldSide)
            End If

        Case 96 To 100: pbpString$ = "IT'S NO GOOD!"

    End Select

    Call PBP

End Sub


'----------------------------------------
'           ITSGOOD Subroutine
'----------------------------------------
'This subroutine builds the Play-by-Play
'string for a made Field Goal.
Sub ITSGOOD

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 10:
            pbpString$ = "IT'S GOOD!!!"

        Case 11 To 20:
            pbpString$ = "THE KICK IS GOOD!!!"

        Case 21 To 30:
            pbpString$ = "HE PUTS IT THROUGH THE UPRIGHTS!!!"

        Case 31 To 40:
            pbpString$ = "HE SPLITS THE UPRIGHTS!!!"

        Case 41 To 50:
            pbpString$ = "IT'S GOOD!!!"

        Case 51 To 60:
            pbpString$ = "HE HAMMERS IT THROUGH!!!"

        Case 61 To 70:
            pbpString$ = "RIGHT DOWN THE MIDDLE!!!"

        Case 71 To 80:
            pbpString$ = "IT HAS THE DISTANCE...IT'S GOOD!!!"

        Case 81 To 90:
            pbpString$ = "HE NAILS IT...IT'S GOOD!!!"

        Case 91 To 100:
            pbpString$ = "IT'S GOOD!"

    End Select

    Call PBP

End Sub


'----------------------------------------
'           MISSPAT Subroutine
'----------------------------------------
'This subroutine builds the Play-by-Play
'string for a missed PAT.
Sub MISSPAT

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 30:
            pbpString$ = "NO GOOD"

        Case 31 To 50:
            pbpString$ = "HE MISSED IT!"

        Case 51 To 65:
            pbpString$ = "THE KICK SAILS WIDE!!...NO GOOD!"

        Case 66 To 85:
            pbpString$ = "BAD SNAP FROM CENTER!! NO KICK"
            gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) - 1

        Case 86 To 100:
            pbpString$ = "IT HITS THE UPRIGHTS!!...NO GOOD!"

    End Select

    Call PBP

End Sub


'----------------------------------------
'           MADEPATSubroutine
'----------------------------------------
'This subroutine builds the Play-by-Play
'string for a made PAT.
Sub MADEPAT

    Call Rolld100(RN0)

    Select Case RN0

        Case 1 To 25:
            pbpString$ = "IT'S GOOD!!!"

        Case 26 To 50:
            pbpString$ = "THE KICK IS GOOD!!!"

        Case 51 To 60:
            pbpString$ = "HE PUTS IT THROUGH THE UPRIGHTS!!!"

        Case 61 To 70:
            pbpString$ = "HE SPLITS THE UPRIGHTS!!!"

        Case 71 To 80:
            pbpString$ = "IT'S GOOD!!!"

        Case 81 To 90:
            pbpString$ = "HE HAMMERS IT THROUGH!!!"

        Case 91 To 100:
            pbpString$ = "RIGHT DOWN THE MIDDLE!!!"

    End Select

    Call PBP

End Sub


'----------------------------------------
'        KickoffReturn Subroutine
'----------------------------------------
'This subroutine handles the outcome of
'a regular kickoff, including whether
'or not the kick is returnable
Sub KickoffReturn (kickNotReturned, isPenalty)

    isPenalty = 0

    Call KickoffDistance
    'kickYL! represents yard line where kick lands

    'Swap defense and offense
    Call CHANGEPOSS

    fairCatchZone = 0

    Select Case ruleOptType

        Case 0
            'College
            If ruleOptColl <= 10 Then
                fairCatchLine = 20
            Else
                fairCatchLine = 25
            End If

        Case Is >= 1
            'Pro
            If ruleOptPro <= 7 Then
                fairCatchLine = 20
            Else
                fairCatchLine = 25
            End If

    End Select

    If kickYL! <= fairCatchLine Then
        fairCatchZone = 1
    End If

    'Newer rules - fair catch allowed
    If ((ruleOptType > 0 And ruleOptPro >= 8) Or (ruleOptType = 0 And ruleOptColl >= 11)) And fairCatchZone = 1 Then

        fcChance = 0

        Select Case ruleOptType

            Case 0
                'College, 2018+
                'Touchback rewarded
                'Take it if inside the 20
                If kickYL! <= 20 Then
                    fcChance = 7
                Else
                    If teamYears(D) >= 2018 And teamYears <= 2020 Then
                        fcChance = 27
                    Else
                        fcChance = 33
                    End If
                End If

            Case Is >= 1
                'Pro, 2018+

                '-- These may need adjusted
                If ruleOptPro = 8 Then
                    fcChance = 12
                Else
                    'Pro, 2023+
                    'When touchback isn't rewarded, fair catch is not likely inside the 5
                    If kickYL! <= 5 And teamYears(impactTeam) = 2023 Then
                        fcChance = 0
                    Else
                        If kickYL! <= 20 And teamYears(impactTeam) >= 2024 Then
                            'Touchback rewarded
                            'Take it if inside the 20
                            fcChance = 7
                        Else
                            fcChance = 18
                        End If

                    End If

                End If

        End Select

        If d100% <= fcChance Then
            fairCatch = 1
        End If

    End If

    If fairCatch = 1 Then

        ydsToScore = 100 - kickYL!
        Call YARDLINE

        If kickYL! < .01 Then

            pbpString$ = kickRet$(P1, I1) + " SIGNALS FOR FAIR CATCH IN THE END ZONE"

        Else

            If ydLineTeam <= 1 Then
                yardLineString$ = Str$(1)
            Else
                yardLineString$ = Str$(Int(ydLineTeam))
            End If

            RN0 = Int(Rnd(1) * 2) + 1

            If RN0 = 1 Then
                pbpString$ = kickRet$(P1, I1) + " SIGNALS FOR FAIR CATCH AT THE" + yardLineString$ + " YARD LINE"
            Else
                pbpString$ = kickRet$(P1, I1) + " CALLS FOR A FAIR CATCH ON THE" + yardLineString$ + " YARD LINE"
            End If

        End If

        Call PBP

        kickNotReturned = 1

        If d100% <= 1 Then
            isPenalty = 1
        End If

        If kickYL! <= 0 Then

            'Kick in end zone
            If isPenalty = 1 Then
                'FC Interf. in End Zone is equivalent to Touchback
                'Actual penalty will be handled later
                ydLine = fairCatchLine
            Else
                Call Touchback(1)
            End If

        Else

            'Newer NFL / NCAA rules reward touchback on fair catch
            If kickYL! <= fairCatchLine And ((ruleOptType = 0 And ruleOptColl >= 11) Or (ruleOptType > 0 And ruleOptPro >= 10)) Then
                Call Touchback(0)
            Else
                ydLine = kickYL!
            End If

        End If

    Else

        If kickYL! <= -9.99 Then

            'Kicked out of endzone - automatic touchback
            Call Touchback(1)
            kickNotReturned = 1

        Else

            scoreX0 = 0

            If kickYL! < .01 Then

                'Kick taken in end zone
                ydLine = -(kickYL! - 2 * kickYL!)

                ydsToScore = 100 - ydLine
                Call ShowChainMarker
                Call YARDLINE

                pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + Str$(CInt((kickYL! - 2 * kickYL!))) + " YDS DEEP IN THE END ZONE"
                Call PBP

                'Computer is receiving?
                'Return it within first 2 yards of endzone
                If (playerMode = 1 And P1 = compTeam) Or playerMode = 2 Then

                    If kickYL! < -2 Then
                        returnChoice = 1
                    Else
                        returnChoice = 2
                    End If

                Else

                    pbpString$ = "1-DOWN IT 2-RETURN IT"
                    Call PBP

                    Do
                        I$ = GetKeyPress$
                        returnChoice = Val(I$)
                    Loop Until returnChoice = 1 Or returnChoice = 2

                End If

                If returnChoice = 1 Then

                    'Received in end zone, kneel for touchback
                    Call Touchback(1)
                    kickNotReturned = 1

                Else

                    kickNotReturned = 0

                End If

            Else

                'Kick outside end zone - adjust position
                kickYL! = kickYL! + (Int(Rnd(1) * 11) - 5) / 10
                ydLine = kickYL!

                ydsToScore = 100 - ydLine
                Call ShowChainMarker
                Call YARDLINE

                pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + " AT THE" + Str$(CInt(kickYL!)) + " YD LINE "
                Call PBP

            End If

        End If

    End If 'Done checking if fair catch

    If kickNotReturned = 1 Then
        'No return = No yards
        'Important for calculating penalties
        ydsGained = 0
    End If

End Sub


'----------------------------------------
'         KickoffLine Subroutine
'----------------------------------------
'This subroutine calculates where to
'place the tee for the kickoff
Sub KickoffLine

    Select Case ruleOptType

        Case 0
            'College

            Select Case ruleOptColl

                Case 0 To 4
                    'Pre-1968
                    '1968 - 1980
                    '1981 - 1983
                    '1984 - 1985
                    ydLine = 40
                    ydsToScore = 60

                Case 5 To 7
                    '1986 - 1990
                    '1991
                    '1992 - 2006
                    ydLine = 35
                    ydsToScore = 65

                Case 8 To 9
                    '2007
                    '2008 - 2011
                    ydLine = 30
                    ydsToScore = 70

                Case 10 To 11
                    '2012 - PRESENT
                    ydLine = 35
                    ydsToScore = 65

            End Select

        Case 1
            'NFL
            Select Case ruleOptPro

                Case 0, 1
                    'Before 1974
                    ydLine = 40
                    ydsToScore = 60

                Case 2 To 3
                    '1974-1993
                    ydLine = 35
                    ydsToScore = 65

                Case 4 To 5
                    '1993-2010
                    ydLine = 30
                    ydsToScore = 70

                Case Is >= 6
                    '2011+
                    ydLine = 35
                    ydsToScore = 65

            End Select

        Case 2, 3, 5
            'AFL, USFL, WLAF
            If ruleOptPro <= 1 Then
                'Before 1974...AFL
                ydLine = 40
                ydsToScore = 60
            Else
                'USFL / WLAF
                ydLine = 35
                ydsToScore = 65
            End If

        Case 4
            'WFL
            ydLine = 30
            ydsToScore = 70

    End Select

End Sub


'----------------------------------------
'       KickoffDistance Subroutine
'----------------------------------------
'This subroutine handles the setup for a
'kick off by handling Play-by-Play and
'calculating the kick distance
Sub KickoffDistance ()

    pbpString$ = kicker$(P1, activeKicker(P1)) + " IS PLACING THE BALL ON THE TEE"
    Call PBP

    If d100% <= 50 Then
        pbpString$ = gameTeams$(P1) + " IS LINING UP FOR THE KICKOFF"
    Else
        pbpString$ = "THE " + gameMascots$(P1) + " ARE LINING UP FOR THE KICKOFF"
    End If

    Call PBP

    pbpString$ = "AND THE KICK IS ON ITS WAY"
    Call PBP

    Call Rolld100(RN0)

    'Return rates sourced from internet
    'https://theathletic.com/4552820/2023/05/25/nfl-kickoff-rules-changes/
    'https://fivethirtyeight.com/features/why-are-we-still-returning-kicks-in-college-football/

    'Determine kickoff yardage based on game "era"
    If ruleOptType = 1 Then

        Select Case ruleOptPro

            Case 0 To 1
                'Prior to 1974, the NFL had a kick line at the 40
                'They had a return rate of 75%
                returnRate = 75

            Case 3
                'In 1974, the NFL moved the kick line to the 35.
                'They had a return rate of 92%
                returnRate = 92

            Case 3
                'Rates from 1978 - 1990 are unknown
                'From 1991-1993, rates were about 76%
                returnRate = 76

            Case 4
                'In 1994, the NFL moved the kick line to the 30.
                'They had a return rate up to 88%
                returnRate = 90

            Case 5
                returnRate = 84

            Case 6
                'In 2011, the NFL moved the kick line to the 35.
                'They also eliminated running starts.
                'Return rates fell to nearly 50%
                returnRate = 51

            Case 7
                returnRate = 42

            Case 8
                returnRate = 40

            Case 9
                '2023 - Last year before dynamic kickoff
                returnRate = 27

            Case 10
                '2024 - First year of dynamic kickoff
                returnRate = 36

            Case 11
                '2025+ - Dynamic kickoff finally taken serious
                returnRate = 79

        End Select

    End If

    If ruleOptType = 0 Then

        Select Case ruleOptColl

            Case 0 To 1
                'Return rates based on NFL,
                'since there is no public data
                'that I have found
                returnRate = 75

            Case 2 To 6
                'Return rates based on NFL,
                'since there is no public data
                'that I have found
                returnRate = 88

            Case 7 To 8
                '1992 - 2007
                returnRate = 60

            Case 9
                '2008 - 2011
                returnRate = 76

            Case 10 To 11
                '2012 - 2017
                returnRate = 58

                'In between here the return rate fell to 51%

            Case 11
                '2018+
                returnRate = 40

        End Select

    End If

    If RN0 <= returnRate Then

        If ruleOptType > 0 And ruleOptPro >= 11 Then
            'Dynamic kickoff - most travel 59-61 yards (return zone)
            'Outliers range from 56-64 yards
            'So here we weight our results around a bell curve
            distWeight = 0
            For i = 1 To 12
                distWeight = distWeight + Rnd(1)
            Next i
            kickDistance = Int(distWeight * 4) + 56
        Else
            'Kick travels 55 to 67 yds (returnable)
            kickDistance = Int(Rnd(1) * 13) + 55
        End If
    Else
        'that is, inside the end zone (not returnable)
        'Kick will travel 68 to 75 yds
        kickDistance = Int(Rnd(1) * 8) + 68
    End If

    'Determine where resulting yard line is after kick
    kickYL! = 100 - (ydLine + kickDistance + goalPostAdj)

    If ruleOptType = 0 And ruleOptColl = 0 Then
        'College before 1968
        kickYL! = kickYL! + 10
    End If

    'Adjust kick distance if factoring for wind
    Select Case climate(0)

        Case D
            'Wind direction favors return team for 3-6 yards
            kickYL! = kickYL! - Int(Rnd(1) * 7) + 3 - climate(1)

        Case P1, 2
            'Wind direction favors kicking team for -3 to 3 yards
            kickYL! = kickYL! + Int(Rnd(1) * 7) - 3 + climate(1)

    End Select

    kickDist = 100 - ydLine - kickYL!

End Sub


'----------------------------------------
'           QBTHROW Subroutine
'----------------------------------------
'This subroutine creates a play-by-play
'string for a QB pass.
Sub QBTHROW ()

    If playCall <> 13 Then

        throwPBP = Int(Rnd(1) * 23) + 1

        Select Case throwPBP
            Case 1: pbpString$ = "HE PASSES IT"
            Case 2: pbpString$ = "HE FIRES IT"
            Case 3: pbpString$ = "HE FIRES THE BALL"
            Case 4: pbpString$ = "HE FIRES ONE"
            Case 5: pbpString$ = "HE FIRES A PASS"
            Case 6: pbpString$ = "HE RIFLES IT"
            Case 7: pbpString$ = "HE RIFLES THE BALL"
            Case 8: pbpString$ = "HE RIFLES ONE"
            Case 9: pbpString$ = "HE RIFLES A PASS"
            Case 10: pbpString$ = "HE THROWS"
            Case 11: pbpString$ = "HE TOSSES"
            Case 12: pbpString$ = "HE DRILLS A PASS"
            Case 13: pbpString$ = "HE FLOATS A PASS"
            Case 14: pbpString$ = "HE THREADS IT"
            Case 15: pbpString$ = "HE DRILLS THE BALL"
            Case 16: pbpString$ = "HE UNLOADS THE BALL"
            Case 17: pbpString$ = "HE UNLOADS IT"
            Case 18: pbpString$ = "HE RIPS THE BALL"
            Case 19: pbpString$ = "HE RIPS IT"
            Case 20: pbpString$ = "THROWN ON A LINE"
            Case 21: pbpString$ = "HE DRILLS IT"
            Case 22: pbpString$ = "HE FLINGS IT"
            Case 23: pbpString$ = "HE FIRES A BULLET"
        End Select

    End If

    Call PBP

End Sub


'----------------------------------------
'           RUNPBP Subroutine
'----------------------------------------
'This subroutine creates a play-by-play
'string for a run.
Sub RUNPBP ()

    If playCall <> 4 And playCall <> 5 Then

        RN0 = Int(Rnd(1) * 62) + 1

        Select Case RN0
            Case 1: pbpString$ = rbacks$(P1, I1) + " GAINS"
            Case 2: pbpString$ = rbacks$(P1, I1) + " BLASTS AHEAD FOR"
            Case 2: pbpString$ = rbacks$(P1, I1) + " BLASTS THROUGH FOR"
            Case 3: pbpString$ = rbacks$(P1, I1) + " SCOOTS OUT FOR"
            Case 4: pbpString$ = rbacks$(P1, I1) + " BURSTS AHEAD FOR"
            Case 5: pbpString$ = rbacks$(P1, I1) + " BURSTS THROUGH FOR"
            Case 6: pbpString$ = rbacks$(P1, I1) + " PLOWS AHEAD FOR"
            Case 7: pbpString$ = rbacks$(P1, I1) + " PLOWS THROUGH FOR"
            Case 8: pbpString$ = rbacks$(P1, I1) + " DASHES AHEAD FOR"
            Case 9: pbpString$ = rbacks$(P1, I1) + " DASHES OUT FOR"
            Case 10: pbpString$ = rbacks$(P1, I1) + " DRIVES AHEAD FOR"
            Case 11: pbpString$ = rbacks$(P1, I1) + " DRIVES THROUGH FOR"
            Case 12: pbpString$ = rbacks$(P1, I1) + " HAMMERS OUT"
            Case 13: pbpString$ = rbacks$(P1, I1) + " HAMMERS AHEAD FOR"
            Case 14: pbpString$ = rbacks$(P1, I1) + " HAMMERS THROUGH FOR"
            Case 15: pbpString$ = rbacks$(P1, I1) + " PUSHES AHEAD FOR"
            Case 16: pbpString$ = rbacks$(P1, I1) + " PUSHES THROUGH FOR"
            Case 17: pbpString$ = rbacks$(P1, I1) + " DASHES FOR"
            Case 18: pbpString$ = rbacks$(P1, I1) + " DASHES AHEAD FOR"
            Case 19: pbpString$ = rbacks$(P1, I1) + " DASHES THROUGH FOR"
            Case 20: pbpString$ = rbacks$(P1, I1) + " BURSTS AHEAD FOR"
            Case 21: pbpString$ = rbacks$(P1, I1) + " BURSTS THROUGH FOR"
            Case 22: pbpString$ = rbacks$(P1, I1) + " RUMBLES FOR"
            Case 23: pbpString$ = rbacks$(P1, I1) + " RUMBLES AHEAD FOR"
            Case 24: pbpString$ = rbacks$(P1, I1) + " RUMBLES THROUGH FOR"
            Case 25: pbpString$ = rbacks$(P1, I1) + " SHOVES AHEAD FOR"
            Case 26: pbpString$ = rbacks$(P1, I1) + " SHOVES THROUGH FOR"
            Case 27: pbpString$ = rbacks$(P1, I1) + " DARTS FOR"
            Case 28: pbpString$ = rbacks$(P1, I1) + " DARTS AHEAD FOR"
            Case 29: pbpString$ = rbacks$(P1, I1) + " DARTS THROUGH FOR"
            Case 30: pbpString$ = rbacks$(P1, I1) + " KNIFES AHEAD FOR"
            Case 31: pbpString$ = rbacks$(P1, I1) + " KNIFES THROUGH FOR"
            Case 32: pbpString$ = rbacks$(P1, I1) + " BULLS HIS WAY FOR"
            Case 33: pbpString$ = rbacks$(P1, I1) + " BULLS AHEAD FOR"
            Case 34: pbpString$ = rbacks$(P1, I1) + " BULLS THROUGH FOR"
            Case 35: pbpString$ = rbacks$(P1, I1) + " MUSCLES AHEAD FOR"
            Case 36: pbpString$ = rbacks$(P1, I1) + " MUSCLES THROUGH FOR"
            Case 37: pbpString$ = rbacks$(P1, I1) + " BLASTS AHEAD FOR"
            Case 38: pbpString$ = rbacks$(P1, I1) + " BLASTS THROUGH FOR"
            Case 39: pbpString$ = rbacks$(P1, I1) + " POWERS AHEAD FOR"
            Case 40: pbpString$ = rbacks$(P1, I1) + " POWERS THROUGH FOR"
            Case 41: pbpString$ = rbacks$(P1, I1) + " SPRINTS FOR"
            Case 42: pbpString$ = rbacks$(P1, I1) + " SPRINTS THROUGH FOR"
            Case 43: pbpString$ = rbacks$(P1, I1) + " SPRINTS AHEAD FOR"
            Case 44: pbpString$ = rbacks$(P1, I1) + " DIVES THROUGH FOR"
            Case 45: pbpString$ = rbacks$(P1, I1) + " CHARGES AHEAD FOR"
            Case 46: pbpString$ = rbacks$(P1, I1) + " CHARGES HIS WAY FOR"
            Case 47: pbpString$ = rbacks$(P1, I1) + " CHARGES THROUGH FOR"
            Case 48: pbpString$ = rbacks$(P1, I1) + " SMASHES AHEAD FOR"
            Case 49: pbpString$ = rbacks$(P1, I1) + " SMASHES THROUGH FOR"
            Case 50: pbpString$ = rbacks$(P1, I1) + " BOLTS FOR"
            Case 51: pbpString$ = rbacks$(P1, I1) + " BOLTS AHEAD FOR"
            Case 52: pbpString$ = rbacks$(P1, I1) + " BOLTS THROUGH FOR"
            Case 53: pbpString$ = rbacks$(P1, I1) + " CUTS THROUGH FOR"
            Case 54: pbpString$ = rbacks$(P1, I1) + " WRESTLES FOR"
            Case 55: pbpString$ = rbacks$(P1, I1) + " WRESTLES AHEAD FOR"
            Case 56: pbpString$ = rbacks$(P1, I1) + " WRESTLES THROUGH FOR"
            Case 57: pbpString$ = rbacks$(P1, I1) + " SLICES THROUGH FOR"
            Case 58: pbpString$ = rbacks$(P1, I1) + " SURGES FORWARD FOR"
            Case 59: pbpString$ = rbacks$(P1, I1) + " POWERS FORWARD FOR"
            Case 69: pbpString$ = rbacks$(P1, I1) + " BURSTS FORWARD FOR"
            Case 61: pbpString$ = rbacks$(P1, I1) + " FINDS A HOLE FOR"
            Case 62: pbpString$ = rbacks$(P1, I1) + " HITS THE HOLE FOR"
        End Select

    Else

        'Sweep / Option
        RN0 = Int(Rnd(1) * 23) + 1

        Select Case RN0
            Case 1: pbpString$ = rbacks$(P1, I1) + " GALLOPS AHEAD FOR"
            Case 2: pbpString$ = rbacks$(P1, I1) + " GALLOPS FOR"
            Case 3: pbpString$ = rbacks$(P1, I1) + " BREAKS FREE FOR"
            Case 4: pbpString$ = rbacks$(P1, I1) + " DASHES AHEAD FOR"
            Case 5: pbpString$ = rbacks$(P1, I1) + " DASHES OUT FOR"
            Case 5: pbpString$ = rbacks$(P1, I1) + " DASHES TO THE EDGE FOR"
            Case 6: pbpString$ = rbacks$(P1, I1) + " SCOOTS AHEAD FOR"
            Case 7: pbpString$ = rbacks$(P1, I1) + " SCOOTS OUT FOR"
            Case 8: pbpString$ = rbacks$(P1, I1) + " SCAMPERS FOR"
            Case 9: pbpString$ = rbacks$(P1, I1) + " SCAMPERS AHEAD FOR"
            Case 10: pbpString$ = rbacks$(P1, I1) + " SLIPS OUT FOR"
            Case 11: pbpString$ = rbacks$(P1, I1) + " SLIPS AHEAD FOR"
            Case 12: pbpString$ = rbacks$(P1, I1) + " SLIPS THROUGH FOR"
            Case 13: pbpString$ = rbacks$(P1, I1) + " SKIRTS OUTSIDE FOR"
            Case 14: pbpString$ = rbacks$(P1, I1) + " SKIRTS AHEAD FOR"
            Case 15: pbpString$ = rbacks$(P1, I1) + " SKIRTS TO THE EDGE FOR"
            Case 16: pbpString$ = rbacks$(P1, I1) + " TURNS THE CORNER FOR"
            Case 17: pbpString$ = rbacks$(P1, I1) + " TURNS UPFIELD FOR"
            Case 18: pbpString$ = rbacks$(P1, I1) + " GETS THE CORNER FOR"
            Case 19: pbpString$ = rbacks$(P1, I1) + " GETS UPFIELD FOR"
            Case 20: pbpString$ = rbacks$(P1, I1) + " GETS TO THE EDGE FOR"
            Case 21: pbpString$ = rbacks$(P1, I1) + " TAKES OFF FOR"
            Case 22: pbpString$ = rbacks$(P1, I1) + " SHUFFLES OUT FOR"
            Case 23: pbpString$ = rbacks$(P1, I1) + " SHUFFLES TO THE EDGE FOR"

        End Select

    End If

End Sub


'----------------------------------------
'       PassCompletePBP Subroutine
'----------------------------------------
'This subroutine builds the play-by-play
'text for a pass completion, along with
'udpating variables for position.
Sub PassCompletePBP (direction)

    Call QBTHROW

    If fleaFlicker = 0 Then
        'Adjust Comp %
        Select Case playCall

            Case 9, 10
                If avgRecYds < 3 Then
                    catchYds = Int(Rnd(1) * 7) - 2
                Else
                    catchYds = Int(Rnd(1) * 6) + 2
                End If

            Case 13:
                If avgRecYds < 3 Then
                    catchYds = Int(Rnd(1) * 4) - 7
                Else
                    catchYds = (Int(Rnd(1) * 4) + 1) * -1
                End If

            Case 11, 14, 16, 17, 18:
                If avgRecYds < 4 Then
                    catchYds = Int(Rnd(1) * 5) + 3
                Else
                    If avgRecYds = 4 Or avgRecYds = 5 Then
                        catchYds = Int(Rnd(1) * 9) + 6
                    Else
                        catchYds = Int(Rnd(1) * 9) + 8
                    End If
                End If

            Case 12, 15
                If avgRecYds < 3 Then
                    catchYds = Int(Rnd(1) * 5) + 3
                Else
                    If avgRecYds = 3 Or avgRecYds = 4 Then
                        catchYds = Int(Rnd(1) * 10) + 10
                    Else
                        catchYds = Int(Rnd(1) * 36) + 20
                    End If
                End If
        End Select

    End If

    If catchYds > ydsGained Then
        catchYds = ydsGained
        itFlag = 1
    End If

    ydsToScore = 100 - ydLine - catchYds
    Call ShowChainMarker
    Call YARDLINE

    If ydsToScore <= 0 Then

        pbpString$ = targetWRName$ + " MAKES THE CATCH IN THE END ZONE!!!"
        Call PBP

    Else

        Call Rolld100(catchContext)

        Select Case playCall

            Case 9, 10
                'Short Pass
                Select Case catchContext
                    Case 1 To 33
                        pbpString$ = targetWRName$ + " HAS THE TOSS OVER THE MIDDLE"
                    Case 34 To 66
                        pbpString$ = targetWRName$ + " CATCHES IT IN THE FLAT"
                    Case 67 To 100
                        pbpString$ = "HE DUMPS IT TO " + targetWRName$
                End Select

            Case 13
                'Screen pass
                Select Case catchContext
                    Case 1 To 33
                        pbpString$ = "HE SCREENS IT TO " + targetWRName$
                    Case 34 To 66
                        pbpString$ = "HE DUMPS THE SCREEN TO " + targetWRName$
                    Case 67 To 199
                        pbpString$ = "HE HAS " + targetWRName$ + " ON THE SCREEN"
                End Select

            Case 11, 14
                'Medium Pass
                Select Case catchContext
                    Case 1 To 33
                        pbpString$ = targetWRName$ + " TAKES IT OVER THE MIDDLE"
                    Case 34 To 66
                        pbpString$ = "HE HAS " + targetWRName$ + " OPEN"
                    Case 67 To 100
                        pbpString$ = "HE COMPLETES IT TO " + targetWRName$
                End Select

            Case 12, 15
                'Long Pass
                Select Case catchContext

                    Case 1 To 20
                        pbpString$ = "HE THROWS IT DEEP TO " + targetWRName$

                    Case 21 To 40
                        pbpString$ = targetWRName$ + " MAKES A LEAPING CATCH"

                    Case 41 To 60
                        pbpString$ = "HE FLOATS IT TO " + targetWRName$

                    Case 61 To 80
                        pbpString$ = targetWRName$ + " SNATCHES IT"

                    Case 81 To 100
                        pbpString$ = targetWRName$ + " CRADLES THE DEEP BALL"

                End Select

            Case 16
                'Sideline pass
                If catchContext <= 50 Then
                    pbpString$ = "HE HAS " + targetWRName$ + " NEAR THE SIDELINE"
                Else
                    pbpString$ = "HE SPOTS " + targetWRName$
                End If

            Case 17, 18
                'Rollout / Fake Pass
                Select Case catchContext
                    Case 1 To 40
                        pbpString$ = "HE HITS " + targetWRName$

                    Case 41 To 80
                        pbpString$ = "HE FIRES IT TO " + targetWRName$

                    Case 81 To 90
                        pbpString$ = "HE LAUNCHES IT TO " + targetWRName$

                    Case 91 To 100
                        pbpString$ = targetWRName$ + " MAKES A GREAT CATCH"
                End Select

        End Select

        If fleaFlicker <> 0 Then
            pbpString$ = targetWRName$ + " MAKES A GREAT CATCH"
        End If

        If ydLineTeam <= 1 Then
            yardLineString$ = Str$(1)
        Else
            yardLineString$ = Str$(Int(ydLineTeam))
        End If

        pbpString$ = pbpString$ + " AT THE" + yardLineString$ + " YARD LINE"

        If itFlag = 1 Then
            itFlag = 0
            Call PBP
        Else
            'Adjust team position
            Call PBP

            startYds = 100 - ydLine - catchYds
            endYds = 100 - ydLine - ydsGained
            Call MOVEBALL

        End If

    End If

End Sub

'----------------------------------------
'       PassYardagePBP Subroutine
'----------------------------------------
'This subroutine builds the play-by-play
'text for reporting the yardage after
'a passing player
Sub PassYardagePBP (yardString$)

    Call Rolld100(yardPBP)

    pbpString$ = ""

    Select Case yardPBP

        Case 1 To 20
            If d100% <= 50 Then
                pbpString$ = gameTeams$(P1) + " PICKS UP"
            Else
                pbpString$ = "THE " + gameMascots$(P1) + " PICK UP"
            End If

            pbpString$ = pbpString$ + yardString$ + " YARDS ON THE PLAY"

        Case 21 To 40
            pbpString$ = "THAT'S A" + yardString$ + " YARDER FOR "
            Call WHICHRECEIVER

        Case 41 To 60
            Call WHICHRECEIVER
            pbpString$ = pbpString$ + " WITH THE" + yardString$ + " YARD GAIN."

        Case 61 To 80
            pbpString$ = "THAT WILL BE A" + yardString$ + " YARD GAIN FOR "

            If d100% <= 50 Then
                pbpString$ = pbpString$ + gameTeams$(P1)
            Else
                pbpString$ = pbpString$ + "THE " + gameMascots$(P1)
            End If

        Case 81 To 100
            pbpString$ = "THAT CATCH BY "
            Call WHICHRECEIVER

            playYdLine! = CInt((ydsGained + ydLine) * 10) / 10

            If playYdLine! >= 99.9499 Then
                pbpString$ = pbpString$ + " GAINED" + Str$(Int(100 - ydLine)) + " YARDS"
            Else
                pbpString$ = pbpString$ + " GAINED" + yardString$ + " YARDS"
            End If

    End Select

    Call PBP

End Sub


'----------------------------------------
'          Touchback Subroutine
'----------------------------------------
'This subroutine handles a touchback BY
'update the variables for down, YARDLINE
'Also calls Play-by-Play and sets the
'ball position marker.
Sub Touchback (endZone)

    Call CHANGEPOSS

    Select Case ruleOptType

        Case 0
            'College

            Select Case ruleOptColl

                Case 0 To 2, 4 To 9
                    'Before 1984
                    '1985 - 2011
                    ydLine = 20
                    ydsToScore = 80

                Case 3
                    '1984 only
                    ydLine = 30
                    ydsToScore = 70

                Case 10 To 11
                    '2012 - PRESENT
                    ydLine = 25
                    ydsToScore = 75

            End Select

        Case 1
            'NFL
            Select Case ruleOptPro

                Case 0 To 7
                    'Before 2018
                    ydLine = 20
                    ydsToScore = 80

                Case 8 To 9
                    '2018 - 2023
                    ydLine = 25
                    ydsToScore = 75

                Case Is >= 10
                    '2024 - Present
                    If endZone = 1 Then
                        ydLine = 30
                    Else
                        ydLine = 25
                    End If

                    ydsToScore = 100 - ydLine

            End Select

        Case 2, 3, 5
            'USFL, AFL, WLAF
            If ruleOptPro < 1 Then
                'Before 1974...AFL
                ydLine = 20
                ydsToScore = 80
            Else
                'USFL / WLAF
                ydLine = 20
                ydsToScore = 80
            End If

        Case 4
            'WFL
            ydLine = 20
            ydsToScore = 80

    End Select

    currDown = 1
    ydsToFirst = 10

    Call CHANGEPOSS

    pbpString$ = "TOUCHBACK"
    Call PBP

    Call WINDEX
    Call CLEARPBP

    Call ShowChainMarker
    Call YARDLINE

    Call StatusBoard

End Sub


'----------------------------------------
'      AddPossessionTime Subroutine
'----------------------------------------
'This subroutine updates the team's time
'of possession for the most recent play.
'The time added is between minSeconds
'and maxSeconds, determined randomly.
Sub AddPossessionTime (minSeconds, maxSeconds)

    secsDelta = maxSeconds - minSeconds
    timeElapsed! = Int(Rnd(1) * (secsDelta + 1)) + minSeconds
    gameClock! = gameClock! - timeElapsed!
    timePoss(P1) = timePoss(P1) + timeElapsed!

End Sub


'----------------------------------------
'          FumblePBP Subroutine
'----------------------------------------
'This subroutine creates the play-by-play
'text for a fumnle
Sub FumblePBP ()

    RN0 = Int(Rnd(1) * 10) + 1

    Select Case RN0

        Case 1:
            pbpString$ = "*** FUMBLE ON THE PLAY!! ***"

        Case 2:
            pbpString$ = "*** LOOSE BALL!! ***"

        Case 3:
            pbpString$ = "*** BALL IS ON THE GROUND!! ***"

        Case 4:
            pbpString$ = "*** BALL IS KNOCKED LOOSE!! ***"

        Case 5 To 8:
            pbpString$ = "*** BALL IS STRIPPED!! ***"

        Case 9:
            pbpString$ = "*** IT'S ANYONE'S BALL !! ***"

        Case 10:
            pbpString$ = "*** FREE BALL !! ***"

    End Select

    Call PBP

End Sub


'----------------------------------------
'       TryForTwoResult Subroutine
'----------------------------------------
'This subroutine makes a generic attempt
'to determine if the attempt at 2PC is
'successfull. It also gives Play-by-Play
'text for the attempt.
Sub TryForTwoResult ()

    If playType = 1 Then
        K1(nbrScores, 4) = 20
    End If
    If playType = 2 Then
        K1(nbrScores, 4) = 30
    End If

    'No longer dealing with endzone situation
    pbpString$ = "TRY FOR TWO IS "
    gameStatsTeam(P1, 35) = gameStatsTeam(P1, 35) + 1

    If ruleOptType = 1 Then
        ydsForConv = 2
    Else
        ydsForConv = 3
    End If

    If ydsGained < ydsForConv Then
        pbpString$ = pbpString$ + "NO GOOD"
        Call PBP
    Else
        pbpString$ = pbpString$ + "GOOD"
        Call PBP

        score(P1, 0) = score(P1, 0) + 2
        score(P1, quarter) = score(P1, quarter) + 2

        K1(nbrScores, 4) = K1(nbrScores, 4) + 1
        gameStatsTeam(P1, 36) = gameStatsTeam(P1, 36) + 1

        Call SETSCREEN

    End If

    endZone = 0

End Sub


'----------------------------------------
'           OPTIONS Subroutine
'----------------------------------------
'This subroutine gives you a small menu
'for changing some of the game options
'(such as pbp delay, coaching mode) in
'the middle of game play.
Sub OPTIONS ()

    Shared playMode$()

    Color 15, 0

    Screen , , 0, 0

    Call INFO

    Do

        Color , 0
        Locate 3, 1
        Color 14: Print "(1) ";
        Color 15: Print playMode$(playerOpt)
        Color 14: Print "(2) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY: #.# "; pbpDelay!
        Print
        Color 14: Print "<ENTER> ";
        Color 15: Print "RETURN TO GAME PLAY"

        Do
            Color 7, 0
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 0 And I <= 2

        Select Case I
            Case 1
                playerOpt = playerOpt + 1
                If playerOpt > 3 Then playerOpt = 0
            Case 2
                Locate 4, 23: Input pbpDelay!
        End Select

        ' Determine computer team
        Select Case playerOpt

            Case 1:
                playerMode = 1
                compTeam = 0

            Case 2:
                playerMode = 1
                compTeam = 1

            Case 3:
                playerMode = 2

        End Select

    Loop Until I$ = Chr$(13)

    Screen , , 0, 0
    'Call DisplayField

End Sub


'----------------------------------------
'           EVENTS Subroutine
'----------------------------------------
'This subroutine works with another to
'look for any events that meet the limits
'for "special". If found, it will create
'a corresponding "special" box score
Sub EVENTS (specialCount, specialStat)

    Shared eventSettings()

    'Read in existing parameters if there are any
    If _FileExists(cfgPath$ + "EVENTS.BOX") Then

        Open cfgPath$ + "EVENTS.BOX" For Input As #1

        For X = 1 To 13
            Input #1, eventSettings(X)
        Next

        Close #1

        checkSpecial% = 0

        'Score / period conditions for special box score
        If quarter > eventSettings(1) Or _ 
            score(0, 0) > eventSettings(2) Or _ 
            score(1, 0) > eventSettings(2) Or _ 
            score(0, 0) < eventSettings(3) Or _
            score(1, 0) < eventSettings(3) Then
            checkSpecial% = 1
        Else

            I = 0

            Do While I <= 1 And checkSpecial% = 0

                I1 = 0

                Do While I1 <= 9 And checkSpecial% = 0

                    'Player Stat conditions for special box score
                    If gameStatsPlayer(I, I1, 1) > eventSettings(4) Or _ 
                        gameStatsPlayer(I, I1, 2) > eventSettings(5) Or _ 
                        gameStatsPlayer(I, I1, 4) > eventSettings(6) Or _
                        gameStatsPlayer(I, I1, 5) > eventSettings(7) Then
                        checkSpecial% = 1
                    End If

                    I1 = I1 + 1

                Loop

                I = I + 1

            Loop

            If checkSpecial% <> 1 Then

                I = 0

                Do While I <= 1 And checkSpecial% = 0

                    I1 = 0

                    Do While I1 <= 5 And checkSpecial% = 0

                        'Player Stat conditions for special box score
                        If gameStatsPlayer(I, I1, 7) > eventSettings(6) Or _
                            gameStatsPlayer(I, I1, 8) > eventSettings(7) Then
                            checkSpecial% = 1
                        End If

                        I1 = I1 + 1

                    Loop

                    I = I + 1

                Loop

            End If

            If checkSpecial% <> 1 Then

                I = 0

                Do While I <= 1 And checkSpecial% = 0

                    I1 = 0

                    Do While I1 <= 3 And checkSpecial% = 0

                        'Player Stat conditions for special box score
                        If gameStatsPlayer(I, I1, 12) > eventSettings(8) Or _
                            gameStatsPlayer(I, I1, 13) > eventSettings(9) Then
                            checkSpecial% = 1
                        End If

                        I1 = I1 + 1

                    Loop

                    I = I + 1

                Loop

            End If

            If checkSpecial% <> 1 Then

                I = 0

                Do While I <= 1 And checkSpecial% = 0

                    'Stat conditions for special box score
                    If gameStatsPlayer(I, 3, 17) > eventSettings(10) Or _ 
                        gameStatsPlayer(I, 7, 17) > eventSettings(10)Then
                        checkSpecial% = 1
                    End If

                    I = I + 1

                Loop

            End If

        End If

    End If 'Done checking EVENTS.BOX file

    If checkSpecial% = 1 Then
        Call FoundSpecial(specialCount, specialStat)
    End If

End Sub


'----------------------------------------
'        FoundSpecial Subroutine
'----------------------------------------
'This subroutine simply counts how many
'special boxscore files already exist, so
'that it an assign a unique number to the
'boxscore about to be created.
Sub FoundSpecial (specialCount, specialStat)

    'Get count of SPC files to determine
    'number to assign next one
    Spec$ = diskPaths$(1) + "*.BSS"
    'Spec$ = diskPaths$(1) + "*.SPC"
    specialCount = FileCount%(Spec$)
    specialCount = specialCount + 1
    specialStat = 1

End Sub


'----------------------------------------
'          POSTGAME Subroutine
'----------------------------------------
'This subroutine processes everything that
'is necessary after a game is complete.
'It checks for special events, handles a
'prompt for stats, and saving of stats.
'Records in Football are only calculated
'when stats are compiled.
Sub POSTGAME ()

    Shared scheduleFile$

    'Call CheckRecords
    Call EVENTS(specialCount, specialStat)
    gameOver = 1

    Do

        Color 15, 0
        Cls

        Locate 2

        Color 14: Print "(0) ";
        Color 15: Print "STATS TO SCREEN"
        Print

        Color 14: Print "(1) ";
        Color 15: Print "STATS TO PRINTER"
        Print

        Color 14: Print "(2) ";
        Color 15: Print "STATS TO TEXT FILE"

        If (autoPlay = 1 And playerMode = 2) Then
            choice = 2
        Else
            Do
                I$ = GetKeyPress$
                choice = Val(I$)
            Loop Until choice >= 0 And choice <= 2
        End If

        Call BOXOPTIONS(choice)
        Call TICKER

        'Reset option for next call
        If choice <> 0 Then choice = 0

        Color 15, 0
        Cls

        If Not (autoPlay = 1 And playerMode = 2) Then

            Do
                'Using Home Team's color for the box
                Call TEAMBOX(1, 1)
                pbpString$ = "DISPLAY STATS AGAIN? (Y/N)"
                Locate 7, (80 - Len(pbpString$)) / 2
                Print pbpString$
                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

    'If we had special events
    If specialStat = 1 Then

        boxName$ = "BOXSCORE" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".BSS"

        'Append special events
        Call BOX2SPC(boxName$)

        Call BOX2FILE(2, boxName$, specialStat)

    End If

    Call SAVESTATS

    gameOver = 0
    specialStat = 0

End Sub


'----------------------------------------
'         BOXOPTIONS Subroutine
'----------------------------------------
'This subroutine takes appropriate action
'for the output of the box file, which
'depends on the choice given to it.

' PARAMETER outputChoice
'    represents menu choice
'       0 = Display on screen
'       1 = Send to printer
'       2 = Save to file

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOXOPTIONS (outputChoice)

    Shared scheduleFile$

    Select Case outputChoice

        Case 0
            Call BOX2SCREEN

        Case 1, 2
            If outputChoice = 1 Then
                'Printer = temporary file
                boxName$ = "TEMPBOX"
            Else
                'File = prompt | auto-create
                If autoPlay <> 1 Then
                    boxName$ = _SaveFileDialog$("Save Boxscore File", diskPaths$(1) + "\Boxscore", "*.BSS", "Special Boxscore File")

                    'Cls
                    'Print "ENTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";
                    'Input boxName$
                    'boxName$ = boxName$ + ".BSS"
                Else
                    'ScheduleName_GameXXX.BS
                    baseFile$ = GetFileBaseName$(scheduleFile$)
                    boxName$ = diskPaths$(1) + baseFile$ + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
                End If

            End If

            'Includes a special option to send to printer when done
            If boxName$ <> "" Then
                Call BOX2FILE(outputChoice, boxName$, 0)
            End If

    End Select

End Sub


'----------------------------------------
'          BOX2FILE Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'an output file.

' PARAMETER printChoice
'    choice to print file vs. saving it

'The option to send to printer will first
'create a temporary file, then loop THROUGH
'it to print each line.
Sub BOX2FILE (printChoice, boxName$, hasSpecial)

    Shared actualAttendance&

    If hasSpecial = 1 Then
        Open (boxName$) For Append As #1
    Else
        Open (boxName$) For Output As #1
    End If

    If gameLoc = 2 Then
        Print #1, "SITE: NEUTRAL"
    Else
        Print #1, "SITE: "; gameStadium$
    End If

    Print #1, Using "ATTENDANCE: ###,###"; actualAttendance&
    Print #1,

    Print #1, Tab(20); "Q1 Q2 Q3 Q4 ";

    If quarter = 5 Then
        Print #1, "OT ";
    Else
        If quarter > 5 Then
            For X = 5 To quarter
                'OT1, OT2, OT3
                Print #1, Using "OT# "; (X - 4);
            Next X
        End If
    End If

    Print #1, "- GAME"

    'Score summary
    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(20);
        Print #1, Using "## ## ## ## "; score(I, 1), score(I, 2), score(I, 3), score(I, 4);

        If quarter > 4 Then
            For I1 = 5 To quarter
                Print #1, Using "## "; score(I, I1);
            Next
        End If

        Print #1, "- ";
        Print #1, Using "###"; score(I, 0)

    Next I

    Print #1,

    If nbrScores <> 0 Then

        For I = 1 To nbrScores

            Print #1, gameTeams$(K1(I, 1)); "-";

            TI = scoreTimes(I)
            TI = TI + TI * (TI < 0)
            IR% = Int(TI / 60)
            J = TI - IR% * 60

            Print #1, Tab(17);

            'Minutes
            Print #1, Using "##"; IR%;
            Print #1, ":";

            'Seconds
            If J >= 10 Then
                Print #1, Using "## "; J;
            Else
                Print #1, Using "0#"; J;
            End If

            Print #1, Tab(23);

            If scoreQuarters(I) = 1 Then Print #1, "OF 1ST -";
            If scoreQuarters(I) = 2 Then Print #1, "OF 2ND -";
            If scoreQuarters(I) = 3 Then Print #1, "OF 3RD -";
            If scoreQuarters(I) = 4 Then Print #1, "OF 4TH -";
            If scoreQuarters(I) > 4 Then Print #1, "OF OT"; scoreQuarters(I) - 4; "-";

            '# of yards for score
            If K1(I, 0) <> 7 Then Print #1, CInt(K1(I, 5));

            tryPAT = 1
            test1 = K1(I, 1)
            test2 = K1(I, 2)

            Select Case K1(I, 0)

                Case 1
                    Print #1, "RUN BY "; rbacks$(K1(I, 1), K1(I, 2))

                Case 2
                    Print #1, "PASS BY "; qbacks$(K1(I, 1), K1(I, 2));
                    Print #1, " TO ";
                    If K1(I, 3) > 9 Then
                        Print #1, wdRec$(K1(I, 1), K1(I, 3) - 10)
                    Else
                        Print #1, rbacks$(K1(I, 1), K1(I, 3))
                    End If

                Case 3
                    Print #1, "K.O. RETURN BY "; kickRet$(K1(I, 1), K1(I, 2))

                Case 4
                    Print #1, "PUNT RETURN BY "; pret$(K1(I, 1), K1(I, 2))

                Case 5
                    Print #1, "INTERCEPTION RETURN BY "; defInts$(K1(I, 1), K1(I, 2))

                Case 6
                    Print #1, " BLOCKED KICK RECOVERED IN END ZONE"

                Case 7
                    Print #1, " SAFETY"
                    tryPAT = 0

                Case 8
                    Print #1, "FG BY "; kicker$(K1(I, 1), K1(I, 2))
                    tryPAT = 0

                Case 9
                    Print #1, "RUN BY "; qbacks$(K1(I, 1), K1(I, 2))

                Case 10
                    Print #1, " FUMBLE RETURN "

                Case 11
                    Print #1, "FAKE PUNT", K1(I, 2)

                Case 12
                    Print #1, "2-PT DEFENSE RETURN BY "; defInts$(K1(I, 1), K1(I, 2))
                    tryPAT = 0

            End Select

            If tryPAT = 1 Then

                Print #1, Tab(17);
                If K1(I, 4) = 10 Then Print #1, " (KICK FAILED)"
                If K1(I, 4) = 11 Then Print #1, " (KICK BY "; kicker$(K1(I, 1), K1(I, 6));: Print #1, ")"
                If K1(I, 4) = 20 Then Print #1, " (RUN FAILED)"
                If K1(I, 4) = 21 Then Print #1, " (RUN GOOD)"
                If K1(I, 4) = 30 Then Print #1, " (PASS FAILED)"
                If K1(I, 4) = 31 Then Print #1, " (PASS GOOD)"

            End If

        Next I

    End If

    Print #1,
    Print #1,

    Print #1, Tab(27); "TEAM STATISTICS"
    hSpace1 = (15 - Len(gameTeams$(0))) / 2
    hSpace2 = (15 - Len(gameTeams$(1))) / 2

    'gameStatsTeam() = Offense Stats
    Print #1, Tab(10 + hSpace1); gameTeams$(0); Tab(45 + hSpace2); gameTeams$(1)

    Print #1, Tab(17); gameStatsTeam(0, 0); Tab(29); "FIRST DOWNS"; Tab(51); gameStatsTeam(1, 0)

    Print #1, Tab(14);

    If ruleOptType = 0 Then
        Print #1, gameStatsTeam(0, 1); "-"; CInt(gameStatsTeam(0, 2)); Tab(29); "RUSHING YDS"; Tab(48); gameStatsTeam(1, 1); "-"; CInt(gameStatsTeam(1, 2))
    Else
        Print #1, gameStatsTeam(0, 1) - sackStatsTeam(0, 0); "-"; CInt(gameStatsTeam(0, 2) + sackStatsTeam(0, 1)); Tab(29); "RUSHING YDS"; Tab(48); gameStatsTeam(1, 1) - sackStatsTeam(1, 0); "-"; CInt(gameStatsTeam(1, 2) + sackStatsTeam(1, 1))
    End If

    Print #1, Tab(16); CInt(gameStatsTeam(0, 6)); Tab(29); "PASSING YDS"; Tab(51); CInt(gameStatsTeam(1, 6))

    Print #1, Tab(16); CInt(gameStatsTeam(0, 6)) + CInt(gameStatsTeam(0, 2)); Tab(30); "TOTAL YDS"; Tab(51); CInt(gameStatsTeam(1, 6)) + CInt(gameStatsTeam(1, 2))

    Print #1, Tab(13); gameStatsTeam(0, 4); "-"; gameStatsTeam(0, 3); "-"; gameStatsTeam(0, 5); Tab(31); "PASSES"; Tab(47); gameStatsTeam(1, 4); "-"; gameStatsTeam(1, 3); "-"; gameStatsTeam(1, 5)

    If gameStatsTeam(0, 7) <> 0 And gameStatsTeam(1, 7) <> 0 Then
        Print #1, Tab(15); gameStatsTeam(0, 7); "-"; CInt((gameStatsTeam(0, 8) / gameStatsTeam(0, 7)) * 10) / 10; Tab(32); "PUNTS"; Tab(49); gameStatsTeam(1, 7); "-"; CInt((gameStatsTeam(1, 8) / gameStatsTeam(1, 7)) * 10) / 10
    End If

    Print #1, Tab(15); gameStatsTeam(0, 9); "-"; gameStatsTeam(0, 10); Tab(31); "FUM/LOST"; Tab(50); gameStatsTeam(1, 9); "-"; gameStatsTeam(1, 10)

    Print #1, Tab(15); gameStatsTeam(0, 11); "-"; CInt(gameStatsTeam(0, 12)); Tab(31); "PENALTIES"; Tab(50); gameStatsTeam(1, 11); "-"; CInt(gameStatsTeam(1, 12))

    Print #1, Tab(15); sackStatsTeam(0, 0); "-"; Int(sackStatsTeam(0, 1)); Tab(32); "SACKED"; Tab(50); sackStatsTeam(1, 0); "-"; CInt(sackStatsTeam(1, 1))

    Print #1, Tab(16);
    Print #1, Using "##"; Int(timePoss(0) / 60);
    Print #1, ":";
    Print #1, Using "##"; timePoss(0) - Int(timePoss(0) / 60) * 60;

    Print #1, Tab(26); "TIME OF POSSESSION";

    Print #1, Tab(51);
    Print #1, Using "##"; Int(timePoss(1) / 60);
    Print #1, ":";
    Print #1, Using "##"; timePoss(1) - Int(timePoss(1) / 60) * 60

    Print #1, Tab(15); thirdDownAtt(0) - thirdDownFail(0); "-"; thirdDownAtt(0); Tab(27); "THIRD DOWN EFF"; Tab(49); thirdDownAtt(1) - thirdDownFail(1); "-"; thirdDownAtt(1)
    Print #1,
    Print #1,

    Print #1, Tab(17); "RUSHING STATISTICS"
    Call ADDBACKS

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "TC   YDS   AVG   LG   TD"

        For I1 = 0 To 13

            If gameStatsPlayer(I, I1, 0) <> 0 Then

                If I1 <= 9 Then
                    Print #1, rbacks$(I, I1);
                Else
                    Print #1, qbacks$(I, I1 - 10);
                End If

                Print #1, Tab(18); Using "###  #### ###.#  "; gameStatsPlayer(I, I1, 0), CInt(gameStatsPlayer(I, I1, 1)), CInt(gameStatsPlayer(I, I1, 1)) / gameStatsPlayer(I, I1, 0);

                If I1 <= 9 Then
                    'Long   TD
                    Print #1, Using "###   ##"; gameLongRunYds(I, I1), gameStatsPlayer(I, I1, 2)
                Else
                    'TD
                    Print #1, Using "      ##"; gameStatsPlayer(I, I1, 2)
                End If
            End If

        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "RECEIVING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "TC    YDS    AVG   LG    TD"

        For I1 = 0 To 5

            If gameStatsPlayer(I, I1, 6) <> 0 Then
                Print #1, wdRec$(I, I1); Tab(19);
                Print #1, Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 6), CInt(gameStatsPlayer(I, I1, 7)), CInt(gameStatsPlayer(I, I1, 7)) / gameStatsPlayer(I, I1, 6), gameLongRec(I, I1 + 10), gameStatsPlayer(I, I1, 8)
            End If

        Next I1

        For I1 = 0 To 9

            If gameStatsPlayer(I, I1, 3) <> 0 Then
                Print #1, rbacks$(I, I1); Tab(19);
                Print #1, Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 3), CInt(gameStatsPlayer(I, I1, 4)), CInt(gameStatsPlayer(I, I1, 4)) / gameStatsPlayer(I, I1, 3), gameLongRec(I, I1), gameStatsPlayer(I, I1, 5)
            End If

        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "PASSING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "ATT  COM COM %  YDS  Y/A  Y/C  TD  TD%  I    I%  LG"

        For I1 = 0 To 3

            If gameStatsPlayer(I, I1, 9) <> 0 Then

                Print #1, qbacks$(I, I1); Tab(19);
                Print #1, Using "###  ### ###.#  ### ##.# "; gameStatsPlayer(I, I1, 9), gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 10) / gameStatsPlayer(I, I1, 9) * 100, CInt(gameStatsPlayer(I, I1, 12)), CInt(gameStatsPlayer(I, I1, 12)) / gameStatsPlayer(I, I1, 9);

                If gameStatsPlayer(I, I1, 10) <> 0 Then
                    'Y/C, TD
                    Print #1, Using "##.#  ##"; CInt(gameStatsPlayer(I, I1, 12)) / gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 13);
                Else
                    'TDs
                    Print #1, Using "      ##"; gameStatsPlayer(I, I1, 13);
                End If

                'TD%, Ints, Int%, Longest
                Print #1, Using "###.# ## ###.# ###"; gameStatsPlayer(I, I1, 13) / gameStatsPlayer(I, I1, 9) * 100, gameStatsPlayer(I, I1, 11), gameStatsPlayer(I, I1, 11) / gameStatsPlayer(I, I1, 9) * 100, gameLongPassYds(I, I1)

            End If

        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "KICKOFF RETURNS"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "KR   YDS   AVG   LG  TD"

        For I1 = 0 To 2
            If gameStatsPlayer(I, I1, 14) <> 0 Then
                Print #1, kickRet$(I, I1); Tab(19);
                Print #1, Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), CInt(gameStatsPlayer(I, I1, 15)), CInt(gameStatsPlayer(I, I1, 15)) / gameStatsPlayer(I, I1, 14), gameLongKRYds(I, I1), gameStatsPlayer(I, I1, 16)
            End If
        Next I1

        Print #1,

    Next I

    Print #1,
    Print #1, Tab(17); "PUNT RETURNS"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "PR   YDS   AVG   LG  TD"

        For I1 = 3 To 5
            If gameStatsPlayer(I, I1, 14) <> 0 Then
                Print #1, pret$(I, I1 - 3); Tab(19);
                Print #1, Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), CInt(gameStatsPlayer(I, I1, 15)), CInt(gameStatsPlayer(I, I1, 15)) / gameStatsPlayer(I, I1, 14), gameLongPRYds(I, I1 - 3), gameStatsPlayer(I, I1, 16)
            End If
        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "PUNTING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "PNT   AVG  LG  TB  BLK  20"

        If gameStatsTeam(I, 7) <> 0 Then
            Print #1, punter$(I, 0); Tab(19);
            Print #1, Using "###  ##.#  ##  ##   ##  ##  ##  ##.#  ##.#"; gameStatsTeam(I, 7), gameStatsTeam(I, 8) / gameStatsTeam(I, 7), gameStatsTeam(I, 26), gameStatsTeam(I, 27), gameStatsTeam(I, 28), gameStatsTeam(I, 29)
        End If

        Print #1,

    Next I

    Print #1, Tab(17); "KICKING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

        If gameStatsPlayer(I, 4, 17) <> 0 Or gameStatsPlayer(I, 6, 17) <> 0 Then

            Print #1, kicker$(I, 0); Tab(19);
            If gameStatsPlayer(I, 6, 17) <> 0 Then Print #1, Using "### ### ###.# "; gameStatsPlayer(I, 5, 17), gameStatsPlayer(I, 6, 17), gameStatsPlayer(I, 5, 17) / gameStatsPlayer(I, 6, 17) * 100;

            If gameStatsPlayer(I, 4, 17) = 0 Then
                Print #1,
            Else
                Print #1, Tab(33); Using "### ### ###.# ##  #"; gameStatsPlayer(I, 3, 17), gameStatsPlayer(I, 4, 17), gameStatsPlayer(I, 3, 17) / gameStatsPlayer(I, 4, 17) * 100, gameLongFGAtt(I, 0), playerFGM(I, 0, 0);

                'FG's 0 - 19
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 0, 0); playerFGM(I, 0, 1);
                'FG's 20 - 29
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 0, 1); playerFGM(I, 0, 2);
                'FG's 30 - 39
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 0, 2); playerFGM(I, 0, 3);
                'FG's 40 - 49
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 0, 3); playerFGM(I, 0, 4);
                'FG's 50+
                Print #1, "- ";: Print #1, Using "#"; playerFGA(I, 0, 4)
            End If

        End If

        If gameStatsPlayer(I, 8, 17) <> 0 Or gameStatsPlayer(I, 10, 17) <> 0 Then

            Print #1, kicker$(I, 1); Tab(19);
            If gameStatsPlayer(I, 10, 17) <> 0 Then Print #1, Using "### ### ###.# "; gameStatsPlayer(I, 9, 17), gameStatsPlayer(I, 10, 17), gameStatsPlayer(I, 9, 17) / gameStatsPlayer(I, 10, 17) * 100;

            If gameStatsPlayer(I, 8, 17) = 0 Then
                Print #1,
            Else
                Print #1, Tab(33); Using "### ### ###.# ##  #"; gameStatsPlayer(I, 7, 17), gameStatsPlayer(I, 8, 17), gameStatsPlayer(I, 7, 17) / gameStatsPlayer(I, 8, 17) * 100, gameLongFGAtt(I, 1), playerFGM(I, 1, 0);

                'FG's 0 - 19
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 1, 0); playerFGM(I, 1, 1);
                'FG's 20 - 29
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 1, 1); playerFGM(I, 1, 2);
                'FG's 30 - 39
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 1, 2); playerFGM(I, 1, 3);
                'FG's 40 - 49
                Print #1, "- ";: Print #1, Using "#  #"; playerFGA(I, 1, 3); playerFGM(I, 1, 4);
                'FG's 50+
                Print #1, "- ";: Print #1, Using "#"; playerFGA(I, 1, 4)
            End If

        End If

        Print #1,

    Next I

    Print #1,
    Print #1, "INTERCEPTIONS"

    For I = 0 To 1:

        Print #1, gameTeams$(I); Tab(16);
        Print #1, "IR   YDS   AVG   LG  TD"

        For I1 = 0 To 9
            If gameInts(I, I1, 0) <> 0 Then
                Print #1, defInts$(I, I1); Tab(16);
                Print #1, Using "##  ####  ##.#  ###  ##"; gameInts(I, I1, 0), CInt(gameInts(I, I1, 1)), CInt(gameInts(I, I1, 1)) / gameInts(I, I1, 0), gameLongInt(I, I1), gameInts(I, I1, 2)
            End If
        Next I1

        Print #1,

    Next I

    Print #1, "SACKS"

    For I = 0 To 1:
        Print #1, gameTeams$(I); Tab(19);
        Print #1, "NO.   YDS LOST"
        For I1 = 0 To 14
            If sackStatsOpp(I, I1, 0) <> 0 Then
                Print #1, defSacks$(I, I1); Tab(19);
                Print #1, Using " ##       ##"; sackStatsOpp(I, I1, 0), CInt(sackStatsOpp(I, I1, 1))
            End If
        Next I1
        Locate , 1: Print #1,
    Next I

    Close #1

    If printChoice = 1 Then

        Open boxName$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop
        
        Close #2

        LPrint Chr$(12)

        Kill boxName$

    End If

End Sub


'----------------------------------------
'           BOX2SPC Subroutine
'----------------------------------------
'This subroutine adds any special event
'information to a boxscore file
Sub BOX2SPC (boxName$)

    Open diskPaths$(1) + boxName$ For Output As #1

    Print #1,

    'Output any special events
    Print #1,

    Close #1

End Sub


'----------------------------------------
'         BOX2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the boxscore for
'output to the end user display.
Sub BOX2SCREEN ()

    Shared actualAttendance&

    Color 15, 0

    Cls

    Color 14
    Locate 2, 1
    Print "SITE: ";
    Color 15

    If gameLoc = 2 Then
        Print "NEUTRAL"
    Else
        Print gameStadium$
    End If

    Color 14
    Print "ATTENDANCE: ";
    Color 15
    Print Using "###,###"; actualAttendance&
    Print

    Call ScoreSummary

    If Not (autoPlay = 1 And playerMode = 2) Then
        Locate LAST_ROW, 27
        Color 14
        Print "PRESS ANY KEY TO CONTINUE";
        I$ = GetKeyPress$
    End If

    Call STATS2SCREEN

End Sub


'----------------------------------------
'        ScoreSummary Subroutine
'----------------------------------------
'This subroutine prints a score by period
'for each team.
Sub ScoreSummary

    Locate 5, 20
    Color 14

    'quarter has been increased prior to this call
    'So quarter = 3 represents halftime

    If quarter = 3 Then
        Print "Q1 Q2 ";
    Else
        Print "Q1 Q2 Q3 Q4 ";

        If quarter = 5 Then
            Print "OT ";
        Else
            If quarter > 5 Then
                For X = 5 To quarter
                    'OT1, OT2, OT3
                    Print Using "OT# "; (X - 4);
                Next X
            End If
        End If

    End If

    Print "- GAME"

    For I = 0 To 1:

        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1)
        Print gameTeams$(I);
        Color 15, 0
        Locate , 20

        If quarter = 3 Then
            Print Using "## ## "; score(I, 1), score(I, 2);
        Else

            Print Using "## ## ## ## "; score(I, 1), score(I, 2), score(I, 3), score(I, 4);

            If quarter > 5 Then
                For I1 = 5 To quarter
                    Print Using "## "; score(I, I1);
                Next
            End If

        End If

        Print "- ";

        Print Using "###"; score(I, 0)

    Next I

    Print
    Color 15, 0

    scoreLines = 0
    maxScoreLines = 13 'for page 1

    If nbrScores <> 0 Then

        For I = 1 To nbrScores

            Locate , 1
            Print gameTeams$(K1(I, 1)); " - ";

            TI = scoreTimes(I)
            TI = TI + TI * (TI < 0)
            IR% = Int(TI / 60)
            J = TI - IR% * 60

            Locate , 17
            Print Using "##.##"; IR% + J / 100;
            Locate , 19: Print ":";

            Locate , 23

            If scoreQuarters(I) = 1 Then Print "OF 1ST -";
            If scoreQuarters(I) = 2 Then Print "OF 2ND -";
            If scoreQuarters(I) = 3 Then Print "OF 3RD -";
            If scoreQuarters(I) = 4 Then Print "OF 4TH -";
            If scoreQuarters(I) = 5 Then Print "OF OVERTIME -";

            If K1(I, 0) <> 7 Then Print CInt(K1(I, 5));

            tryPAT = 1

            Select Case K1(I, 0)

                Case 1
                    Print "RUN BY "; rbacks$(K1(I, 1), K1(I, 2))

                Case 2
                    Print "PASS BY "; qbacks$(K1(I, 1), K1(I, 2)); " TO ";
                    If K1(I, 3) > 9 Then
                        Print wdRec$(K1(I, 1), K1(I, 3) - 10)
                    Else
                        Print rbacks$(K1(I, 1), K1(I, 3))
                    End If

                Case 3
                    Print "K.O. RETURN BY "; kickRet$(K1(I, 1), K1(I, 2))

                Case 4
                    Print "PUNT RETURN BY "; pret$(K1(I, 1), K1(I, 2))

                Case 5
                    Print "INTERCEPTION RETURN BY "; defInts$(K1(I, 1), K1(I, 2))

                Case 6
                    Print " BLOCKED KICK RECOVERED IN END ZONE"

                Case 7
                    Print " SAFETY"
                    tryPAT = 0

                Case 8
                    Print "FG BY "; kicker$(K1(I, 1), K1(I, 2))
                    tryPAT = 0

                Case 9
                    Print "RUN BY "; qbacks$(K1(I, 1), K1(I, 2))

                Case 10
                    Print " FUMBLE RETURN "

                Case 11
                    Print "FAKE PUNT ", K1(I, 2)

                Case 12
                    Print "2-PT DEFENSE RETURN BY "; defInts$(K1(I, 1), K1(I, 2))
                    tryPAT = 0

            End Select

            scoreLines = scoreLines + 1

            If tryPAT = 1 Then

                Locate , 17
                If K1(I, 4) = 10 Then Print " (KICK FAILED)"
                If K1(I, 4) = 11 Then Print " (KICK BY "; kicker$(K1(I, 1), K1(I, 6));: Print ")"
                If K1(I, 4) = 20 Then Print " (RUN FAILED)"
                If K1(I, 4) = 21 Then Print " (RUN GOOD)"
                If K1(I, 4) = 30 Then Print " (PASS FAILED)"
                If K1(I, 4) = 31 Then Print " (PASS GOOD)"
                scoreLines = scoreLines + 1

            End If

            If Not (autoPlay = 1 And playerMode = 2) And scoreLines >= maxScoreLines Then
                Locate LAST_ROW, 20
                Color 14
                Print "PRESS ANY KEY FOR MORE SCORE INFORMATION";
                Call GetKeyWhenNoDelay

                Cls
                Locate 2, 27
                Print "SCORE SUMMARY (CONTINUED)"
                Color 15
                Locate 4, 1

                'We can show more lines of scores on pages 2/3
                scoreLines = 0
                maxScoreLines = 18
            End If

        Next I

    End If

End Sub


'----------------------------------------
'        STATS2SCREEN Subroutine
'----------------------------------------
'This subroutine formats the game stats
'for output to the end user display
Sub STATS2SCREEN ()

    Screen , , 0, 0

    Color , 1
    Cls

    Call OPTIONBOX(15)
    Locate LAST_ROW, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 10, 32

    Print "TEAM STATISTICS"

    Locate , 15
    Color teamRat_GAME(0, 2), teamRat_GAME(0, 1)
    Print gameTeams$(0);

    Color 0, 7
    Locate , 50
    Color teamRat_GAME(1, 2), teamRat_GAME(1, 1)
    Print gameTeams$(1)

    Color 0, 7
    Locate , 20: Print gameStatsTeam(0, 0);
    Locate , 34: Print "FIRST DOWNS";
    Locate , 55: Print gameStatsTeam(1, 0)

    If ruleOptType = 0 Then
        Locate , 18: Print gameStatsTeam(0, 1); "-"; CInt(gameStatsTeam(0, 2));
        Locate , 34: Print "RUSHING YDS";
        Locate , 53: Print gameStatsTeam(1, 1); "-"; CInt(gameStatsTeam(1, 2))
    Else
        Locate , 18: Print gameStatsTeam(0, 1) - sackStatsTeam(0, 0); "-"; CInt(gameStatsTeam(0, 2) + sackStatsTeam(0, 1));
        Locate , 34: Print "RUSHING YDS";
        Locate , 53: Print gameStatsTeam(1, 1) - sackStatsTeam(1, 0); "-"; CInt(gameStatsTeam(1, 2) + sackStatsTeam(1, 1))
    End If

    Locate , 20: Print CInt(gameStatsTeam(0, 6));
    Locate , 34: Print "PASSING YDS";
    Locate , 55: Print CInt(gameStatsTeam(1, 6))

    Locate , 35: Print "TOTAL YDS";
    Locate , 20: Print CInt(gameStatsTeam(0, 6)) + CInt(gameStatsTeam(0, 2));
    Locate , 55: Print CInt(gameStatsTeam(1, 6)) + CInt(gameStatsTeam(1, 2))

    Locate , 37: Print "PASSES";
    Locate , 16: Print gameStatsTeam(0, 4); "-"; gameStatsTeam(0, 3); "-"; gameStatsTeam(0, 5);
    Locate , 51: Print gameStatsTeam(1, 4); "-"; gameStatsTeam(1, 3); "-"; gameStatsTeam(1, 5)

    If gameStatsTeam(0, 7) <> 0 And gameStatsTeam(1, 7) <> 0 Then
        Locate , 38: Print "PUNTS";
        'was 15 / 50
        Locate , 17: Print gameStatsTeam(0, 7); "-"; CInt((gameStatsTeam(0, 8) / gameStatsTeam(0, 7)) * 10) / 10;
        Locate , 52: Print gameStatsTeam(1, 7); "-"; CInt((gameStatsTeam(1, 8) / gameStatsTeam(1, 7)) * 10) / 10
    End If

    Locate , 36: Print "FUM/LOST";
    Locate , 18: Print gameStatsTeam(0, 9); "-"; gameStatsTeam(0, 10);
    Locate , 53: Print gameStatsTeam(1, 9); "-"; gameStatsTeam(1, 10)

    Locate , 35: Print "PENALTIES";
    Locate , 18: Print gameStatsTeam(0, 11); "-"; CInt(gameStatsTeam(0, 12));
    Locate , 53: Print gameStatsTeam(1, 11); "-"; CInt(gameStatsTeam(1, 12))

    Locate , 37: Print "SACKED";
    Locate , 18: Print sackStatsTeam(0, 0); "-"; CInt(sackStatsTeam(0, 1));
    Locate , 53: Print sackStatsTeam(1, 0); "-"; CInt(sackStatsTeam(1, 1))

    Locate , 18: Print Using "##"; Int(timePoss(0) / 60);
    Print ":";
    Print Using "##"; timePoss(0) - Int(timePoss(0) / 60) * 60;
    Locate , 31: Print "TIME OF POSSESSION";
    Locate , 53: Print Using "##"; Int(timePoss(1) / 60);
    Print ":";
    Print Using "##"; timePoss(1) - Int(timePoss(1) / 60) * 60

    Locate , 32: Print "THIRD DOWN EFF";
    'was 16, 51
    Locate , 17: Print thirdDownAtt(0) - thirdDownFail(0); "-"; thirdDownAtt(0);
    Locate , 52: Print thirdDownAtt(1) - thirdDownFail(1); "-"; thirdDownAtt(1)

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 32
    Print "RUSHING STATISTICS"
    Print
    Call ADDBACKS

    For I = 0 To 1

        Locate , 20
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36
        Print "TC   YDS   AVG   LG   TD"

        For I1 = 0 To 13

            If gameStatsPlayer(I, I1, 0) <> 0 Then

                Locate , 20
                If I1 <= 9 Then
                    Print rbacks$(I, I1);
                Else
                    Print qbacks$(I, I1 - 10);
                End If

                Locate , 35: Print Using "###  #### ###.#  "; gameStatsPlayer(I, I1, 0), CInt(gameStatsPlayer(I, I1, 1)), CInt(gameStatsPlayer(I, I1, 1)) / gameStatsPlayer(I, I1, 0);

                If I1 <= 9 Then
                    Print Using "###   ##"; gameLongRunYds(I, I1), gameStatsPlayer(I, I1, 2)
                Else
                    Print Using "      ##"; gameStatsPlayer(I, I1, 2)
                End If

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 32
    Print "RECEIVING STATISTICS"
    Print
    nbrPlayers = 0

    For I = 0 To 1

        Locate , 18
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);: Color 0, 7
        Locate , 36: Print "TC    YDS    AVG   LG    TD"

        For I1 = 0 To 5

            If gameStatsPlayer(I, I1, 6) <> 0 Then
                Locate , 18
                Print wdRec$(I, I1);
                Locate , 36: Print Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 6), CInt(gameStatsPlayer(I, I1, 7)), CInt(gameStatsPlayer(I, I1, 7)) / gameStatsPlayer(I, I1, 6), gameLongRec(I, I1 + 10), gameStatsPlayer(I, I1, 8)
                nbrPlayers = nbrPlayers + 1
            End If

            If (nbrPlayers > 0 And nbrPlayers Mod 16 = 0) Then
                'Create a 2nd page if there are enough players
                'to spam out past the page
                Call GetKeyWhenNoDelay

                Call FRAMESTATS(22)
                Locate LAST_ROW, 27
                Print "PRESS ANY KEY TO CONTINUE";

                Locate 2, 32
                Print "RECEIVING STATISTICS"
                Print

                Locate , 18
                Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
                Color 0, 7
                Locate , 36: Print "TC    YDS    AVG   LG    TD"

            End If

        Next I1

        For I1 = 0 To 9

            If gameStatsPlayer(I, I1, 3) <> 0 Then
                Locate , 18
                Print rbacks$(I, I1);
                Locate , 36: Print Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 3), CInt(gameStatsPlayer(I, I1, 4)), CInt(gameStatsPlayer(I, I1, 4)) / gameStatsPlayer(I, I1, 3), gameLongRec(I, I1), gameStatsPlayer(I, I1, 5)
                nbrPlayers = nbrPlayers + 1
            End If

            If (nbrPlayers > 0 And nbrPlayers Mod 16 = 0) Then
                'Create a 2nd page if there are enough players
                'to spam out past the page
                Call GetKeyWhenNoDelay

                Call FRAMESTATS(22)
                Locate LAST_ROW, 27: Print "PRESS ANY KEY TO CONTINUE";

                Locate 2, 32: Print "RECEIVING STATISTICS"
                Print

                Locate , 18
                Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
                Color 0, 7
                Locate , 36: Print "TC    YDS    AVG   LG    TD"

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 32
    Print "PASSING STATISTICS"
    Print

    For I = 0 To 1
        Locate , 6
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 22: Print "ATT  COM COM %  YDS  Y/A  Y/C  TD  TD%  I    I%  LG"

        For I1 = 0 To 3

            If gameStatsPlayer(I, I1, 9) <> 0 Then

                Locate , 6
                Print qbacks$(I, I1);
                Locate , 22: Print Using "###  ### ###.#  ### ##.# "; gameStatsPlayer(I, I1, 9), gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 10) / gameStatsPlayer(I, I1, 9) * 100, CInt(gameStatsPlayer(I, I1, 12)), CInt(gameStatsPlayer(I, I1, 12)) / gameStatsPlayer(I, I1, 9);

                If gameStatsPlayer(I, I1, 10) <> 0 Then
                    'Y/C, TD
                    Print Using "##.#  ##"; CInt(gameStatsPlayer(I, I1, 12)) / gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 13);
                Else
                    Print Using "      ##"; gameStatsPlayer(I, I1, 13);
                End If

                '             TD%, I, I%,   LG
                Print Using "###.# ## ###.# ###"; gameStatsPlayer(I, I1, 13) / gameStatsPlayer(I, I1, 9) * 100, gameStatsPlayer(I, I1, 11), gameStatsPlayer(I, I1, 11) / gameStatsPlayer(I, I1, 9) * 100, gameLongPassYds(I, I1)

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 25
    Print "KICKOFF RETURN STATISTICS"
    Print

    For I = 0 To 1
        Locate , 20
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36: Print "KR   YDS   AVG   LG  TD"

        For I1 = 0 To 2

            If gameStatsPlayer(I, I1, 14) <> 0 Then
                Locate , 20
                Print kickRet$(I, I1);
                Locate , 36
                Print Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), CInt(gameStatsPlayer(I, I1, 15)), CInt(gameStatsPlayer(I, I1, 15)) / gameStatsPlayer(I, I1, 14), gameLongKRYds(I, I1), gameStatsPlayer(I, I1, 16)

            End If

        Next I1

        Print
    Next I

    Print
    Locate , 25
    Print "PUNT RETURN STATISTICS"
    Print

    For I = 0 To 1
        Locate , 20
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36: Print "PR   YDS   AVG   LG  TD"

        For I1 = 3 To 5
            If gameStatsPlayer(I, I1, 14) <> 0 Then
                Locate , 20
                Print pret$(I, I1 - 3);
                Locate , 36
                Print Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), CInt(gameStatsPlayer(I, I1, 15)), CInt(gameStatsPlayer(I, I1, 15)) / gameStatsPlayer(I, I1, 14), gameLongPRYds(I, I1 - 3), gameStatsPlayer(I, I1, 16)
            End If
        Next I1

        Print
    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 29
    Print "PUNTING STATISTICS"
    Print

    For I = 0 To 1
        Locate , 10
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1)
        Print gameTeams$(I);: Color 0, 7
        Locate , 26: Print "PNT   AVG  LG  TB  BLK  20"

        If gameStatsTeam(I, 7) <> 0 Then
            Locate , 10
            Print punter$(I, 0);
            Locate , 26
            Print Using "###  ##.#  ##  ##   ##  ##  ##  ##.#  ##.#"; gameStatsTeam(I, 7), gameStatsTeam(I, 8) / gameStatsTeam(I, 7), gameStatsTeam(I, 26), gameStatsTeam(I, 27), gameStatsTeam(I, 28), gameStatsTeam(I, 29)
        End If

        Print
    Next I

    Print
    Locate , 29
    Print "KICKING STATISTICS"
    Print

    For I = 0 To 1

        Locate , 3
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);: Color 0, 7
        Locate , 18: Print "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

        If gameStatsPlayer(I, 4, 17) <> 0 Or gameStatsPlayer(I, 6, 17) <> 0 Then
            Locate , 3
            Print kicker$(I, 0);
            Locate , 18

            If gameStatsPlayer(I, 6, 17) <> 0 Then
                Print Using "### ### ###.# "; gameStatsPlayer(I, 5, 17), gameStatsPlayer(I, 6, 17), gameStatsPlayer(I, 5, 17) / gameStatsPlayer(I, 6, 17) * 100;
            End If

            If gameStatsPlayer(I, 4, 17) = 0 Then
                Print
            Else
                Locate , 32
                Print Using "### ### ###.# ##  #"; gameStatsPlayer(I, 3, 17), gameStatsPlayer(I, 4, 17), gameStatsPlayer(I, 3, 17) / gameStatsPlayer(I, 4, 17) * 100, gameLongFGAtt(I, 0), playerFGM(I, 0, 0);

                'FG's 0 - 19
                Print "- ";: Print Using "#  #"; playerFGA(I, 0, 0); playerFGM(I, 0, 1);
                'FG's 20 - 29
                Print "- ";: Print Using "#  #"; playerFGA(I, 0, 1); playerFGM(I, 0, 2);
                'FG's 30 - 39
                Print "- ";: Print Using "#  #"; playerFGA(I, 0, 2); playerFGM(I, 0, 3);
                'FG's 40 - 49
                Print "- ";: Print Using "#  #"; playerFGA(I, 0, 3); playerFGM(I, 0, 4);
                'FG's 50+
                Print "- ";: Print Using "#"; playerFGA(I, 0, 4)
            End If

        End If

        If gameStatsPlayer(I, 8, 17) <> 0 Or gameStatsPlayer(I, 10, 17) <> 0 Then

            Locate , 3
            Print kicker$(I, 1);
            Locate , 18

            If gameStatsPlayer(I, 10, 17) <> 0 Then
                Print Using "### ### ###.# "; gameStatsPlayer(I, 9, 17), gameStatsPlayer(I, 10, 17), gameStatsPlayer(I, 9, 17) / gameStatsPlayer(I, 10, 17) * 100;
            End If

            If gameStatsPlayer(I, 8, 17) = 0 Then
                Print
            Else

                Locate , 32
                Print Using "### ### ###.# ##  #"; gameStatsPlayer(I, 7, 17), gameStatsPlayer(I, 8, 17), gameStatsPlayer(I, 7, 17) / gameStatsPlayer(I, 8, 17) * 100, gameLongFGAtt(I, 1), playerFGM(I, 1, 0);

                'FG's 0 - 19
                Print "- ";: Print Using "#  #"; playerFGA(I, 1, 0); playerFGM(I, 1, 1);
                'FG's 20 - 29
                Print "- ";: Print Using "#  #"; playerFGA(I, 1, 1); playerFGM(I, 1, 2);
                'FG's 30 - 39
                Print "- ";: Print Using "#  #"; playerFGA(I, 1, 2); playerFGM(I, 1, 3);
                'FG's 40 - 49
                Print "- ";: Print Using "#  #"; playerFGA(I, 1, 3); playerFGM(I, 1, 4);
                'FG's 50+
                Print "- ";: Print Using "#"; playerFGA(I, 1, 4)

            End If

        End If

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 35
    Print "INTERCEPTIONS"
    Print

    For I = 0 To 1:
        Locate , 20
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36
        Print "IR   YDS   AVG   LG  TD"

        For I1 = 0 To 9
            If gameInts(I, I1, 0) <> 0 Then
                Locate , 20: Print defInts$(I, I1);
                Locate , 36: Print Using "##  ####  ##.#  ###  ##"; gameInts(I, I1, 0), CInt(gameInts(I, I1, 1)), CInt(gameInts(I, I1, 1)) / gameInts(I, I1, 0), gameLongInt(I, I1), gameInts(I, I1, 2)
            End If
        Next I1

        Locate , 1: Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate LAST_ROW, 27
    Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 39
    Print "SACKS"
    Print

    For I = 0 To 1
        Locate , 25
        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1): Print gameTeams$(I);: Color 0, 7

        Locate , 41
        Print "NO.   YDS LOST"

        For I1 = 0 To 14
            If sackStatsOpp(I, I1, 0) <> 0 Then
                Locate , 25: Print defSacks$(I, I1);
                Locate , 41: Print Using " ##       ##"; sackStatsOpp(I, I1, 0), CInt(sackStatsOpp(I, I1, 1))
            End If
        Next I1

        Locate , 1
        Print

    Next I

    Call GetKeyWhenNoDelay

End Sub


'----------------------------------------
'          SAVESTATS Subroutine
'----------------------------------------
'This subroutine prompts the user in ORDER
'to determine which teams to save satts For
'(autoplay will always save both teams).
'It also updates the GAME82 progress file,
'since at this point only can the game be
'considered fully complete.
Sub SAVESTATS ()

    Shared scheduleFile$

    Cls

    If autoPlay <> 1 Then

        Do
            Call TEAMBOX(1, 1)
            pbpString$ = "SAVE STATISTICS? (Y/N)"
            Locate , (80 - Len(pbpString$)) / 2
            Print pbpString$
            Color 15, 0

            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or autoPlay = 1 Then

        Cls

        ' Update schedule, ticker, etc.
        'If autoPlay = 1 Then
        If _FileExists(cfgPath$ + "GAME82") Then

            Call UpdateSchedule

            If schedGame(1) < schedGame(0) + schedGame(2) Then
                Kill cfgPath$ + "GAME82"
                endAllGames = 1
            Else

                Open cfgPath$ + "GAME82" For Output As #1

                Print #1, scheduleFile$

                For I = 0 To 2:
                    Print #1, schedGame(I)
                Next

                Close #1

            End If

        End If

        Cls

        periodNbr = quarter

        For idx = 0 To 1

            skipCompile% = 0

            Color 15, 0
            Cls

            Call FRAMEOPTION(1, idx)

            pbpString$ = "COMPILE STATS FOR " + gameTeams$(idx) + "? (Y/N)"
            Locate , (80 - Len(pbpString$)) / 2
            Print pbpString$
            Color 15, 0

            If autoPlay <> 1 Then

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If I$ = "N" Then skipCompile% = 1

            End If

            If skipCompile% <> 1 Then

                Call COMPILESTATFILES(idx)

            End If

        Next idx

    End If

End Sub


'----------------------------------------
'      COMPILESTATFILES Subroutine
'----------------------------------------
'This subroutine updates the various stat
'files for the corresponding team based on
'the stats generated for a completed game.
Sub COMPILESTATFILES (teamIdx)

    Shared actualAttendance&

    Shared oppScore(), powerRating()
    Shared teamScore()

    Shared gameAttendance&()

    Shared gameSite$()
    Shared locIndicator$(), oppName$()

    Shared recLongRecTxt$(), recLongRushTxt$()
    Shared recLongFGTxt$(), recLongIntTxt$(), recLongKRTxt$()
    Shared recLongPassTxt$(), recLongPRTxt$()

    'Determine stat file name
    statFile$ = nameStatFile$(gameTeams$(teamIdx))

    'Read existing stats from the file
    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, gameTeams$(teamIdx)

        For I = 0 To 9:
            Input #1, rbacks$(teamIdx, I)
        Next

        For I = 0 To 5:
            Input #1, wdRec$(teamIdx, I)
        Next

        For I = 0 To 3:
            Input #1, qbacks$(teamIdx, I)
        Next

        For I = 0 To 2:
            Input #1, kickRet$(teamIdx, I)
        Next

        For I = 0 To 2:
            Input #1, pret$(teamIdx, I)
        Next

        Input #1, punter$(teamIdx, 0)

        For I = 0 To 1:
            Input #1, kicker$(teamIdx, I)
        Next

        For J = 0 To 13:
            For K = 0 To 17:
                Input #1, gamePlayerStat!(J, K)
            Next
        Next

        For L = 0 To 38:
            Input #1, gameTeamStat!(L)
        Next

        For L = 0 To 38:
            Input #1, gameOppStat!(L)
        Next

        For J = 0 To 9
            Input #1, defInts$(teamIdx, J)
            For J1 = 0 To 2
                Input #1, totalInts(teamIdx, J, J1)
            Next
        Next

        For J = 0 To 14
            Input #1, defSacks$(teamIdx, J)
            For J1 = 0 To 1
                Input #1, totalSacks(teamIdx, J, J1)
            Next
        Next

        For I = 0 To 9:
            Input #1, recLongRush!(teamIdx, I)
            Input #1, recLongRushTxt$(teamIdx, I, 0)
            Input #1, recLongRushTxt$(teamIdx, I, 1)
        Next

        For I = 0 To 15:
            Input #1, recLongRec!(teamIdx, I)
            Input #1, recLongRecTxt$(teamIdx, I, 0)
            Input #1, recLongRecTxt$(teamIdx, I, 1)
        Next

        For I = 0 To 3:
            Input #1, recLongPass!(teamIdx, I)
            Input #1, recLongPassTxt$(teamIdx, I, 0)
            Input #1, recLongPassTxt$(teamIdx, I, 1)
        Next

        For I = 0 To 2:
            Input #1, recLongKR!(teamIdx, I)
            Input #1, recLongKRTxt$(teamIdx, I, 0)
            Input #1, recLongKRTxt$(teamIdx, I, 1)
        Next

        For I = 0 To 2:
            Input #1, recLongPR!(teamIdx, I)
            Input #1, recLongPRTxt$(teamIdx, I, 0)
            Input #1, recLongPRTxt$(teamIdx, I, 1)
        Next

        For I = 0 To 1:
            Input #1, recLongFG!(teamIdx, I)
            Input #1, recLongFGTxt$(teamIdx, I)
            For I1 = 0 To 4:
                Input #1, gameFGM!(teamIdx, I, I1)
                Input #1, gameFGA!(teamIdx, I, I1)
            Next
        Next

        For I = 0 To 9:
            Input #1, recLongInt!(teamIdx, I)
            Input #1, recLongIntTxt$(teamIdx, I, 0)
            Input #1, recLongIntTxt$(teamIdx, I, 1)
        Next

        Close #1

    End If

    ' Look for records
    For I = 0 To 9
        If gameLongRunYds(teamIdx, I) > recLongRush!(teamIdx, I) Then
            recLongRush!(teamIdx, I) = gameLongRunYds(teamIdx, I)
            recLongRushTxt$(teamIdx, I, 0) = gameLongRunTD$(teamIdx, I)
            recLongRushTxt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If

        If gameLongInt(teamIdx, I) > recLongInt!(teamIdx, I) Then
            recLongInt!(teamIdx, I) = gameLongInt(teamIdx, I)
            recLongIntTxt$(teamIdx, I, 0) = gameLongIntTD$(teamIdx, I)
            recLongIntTxt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 15
        If gameLongRec(teamIdx, I) > recLongRec!(teamIdx, I) Then
            recLongRec!(teamIdx, I) = gameLongRec(teamIdx, I)
            recLongRecTxt$(teamIdx, I, 0) = gameLongRecTD$(teamIdx, I)
            recLongRecTxt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 3
        If gameLongPassYds(teamIdx, I) > recLongPass!(teamIdx, I) Then
            recLongPass!(teamIdx, I) = gameLongPassYds(teamIdx, I)
            recLongPassTxt$(teamIdx, I, 0) = gameLongPassTD$(teamIdx, I)
            recLongPassTxt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 2
        If gameLongKRYds(teamIdx, I) > recLongKR!(teamIdx, I) Then
            recLongKR!(teamIdx, I) = gameLongKRYds(teamIdx, I)
            recLongKRTxt$(teamIdx, I, 0) = gameLongKRTD$(teamIdx, I)
            recLongKRTxt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If

        If gameLongPRYds(teamIdx, I) > recLongPR!(teamIdx, I) Then
            recLongPR!(teamIdx, I) = gameLongPRYds(teamIdx, I)
            recLongPRTxt$(teamIdx, I, 0) = gameLongPRTD$(teamIdx, I)
            recLongPRTxt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    If gameStatsTeam(teamIdx, 26) > gameTeamStat!(28) Then
        gameTeamStat!(28) = gameStatsTeam(teamIdx, 26)
    End If

    For I = 0 To 1

        If gameLongFGAtt(teamIdx, I) > recLongFG!(teamIdx, I) Then
            recLongFG!(teamIdx, I) = gameLongFGAtt(teamIdx, I)
            recLongFGTxt$(teamIdx, 1) = gameTeams$(1 - teamIdx)
        End If

        For I1 = 0 To 4
            gameFGM!(teamIdx, I, I1) = gameFGM!(teamIdx, I, I1) + playerFGM(teamIdx, I, I1)
            gameFGA!(teamIdx, I, I1) = gameFGA!(teamIdx, I, I1) + playerFGA(teamIdx, I, I1)
        Next

    Next

    For J = 0 To 13
        For K = 0 To 17:
            gamePlayerStat!(J, K) = gamePlayerStat!(J, K) + CInt(gameStatsPlayer(teamIdx, J, K))
        Next
    Next

    gameTeamStat!(32) = gameTeamStat!(32) + score(teamIdx, 1)
    gameTeamStat!(33) = gameTeamStat!(33) + score(teamIdx, 2)
    gameTeamStat!(34) = gameTeamStat!(34) + score(teamIdx, 3)
    gameTeamStat!(35) = gameTeamStat!(35) + score(teamIdx, 4)

    For K = 5 To periodNbr
        gameTeamStat!(36) = gameTeamStat!(36) + score(teamIdx, K)
    Next

    gameOppStat!(32) = gameOppStat!(32) + score(1 - teamIdx, 1)
    gameOppStat!(33) = gameOppStat!(33) + score(1 - teamIdx, 2)
    gameOppStat!(34) = gameOppStat!(34) + score(1 - teamIdx, 3)
    gameOppStat!(35) = gameOppStat!(35) + score(1 - teamIdx, 4)

    For K = 5 To periodNbr
        gameOppStat!(36) = gameOppStat!(36) + score(1 - teamIdx, K)
    Next

    For J = 0 To 3
        gameTeamStat!(17) = gameTeamStat!(17) + gameStatsPlayer(teamIdx, J, 13)
        gameOppStat!(17) = gameOppStat!(17) + gameStatsPlayer(1 - teamIdx, J, 13)
    Next

    For J = 0 To 13
        gameTeamStat!(15) = gameTeamStat!(15) + gameStatsPlayer(teamIdx, J, 2)
        gameOppStat!(15) = gameOppStat!(15) + gameStatsPlayer(1 - teamIdx, J, 2)
    Next

    For J = 0 To 2
        gameTeamStat!(18) = gameTeamStat!(18) + gameStatsPlayer(teamIdx, J, 16)
        gameOppStat!(18) = gameOppStat!(18) + gameStatsPlayer(1 - teamIdx, J, 16)
        gameTeamStat!(22) = gameTeamStat!(22) + gameStatsPlayer(teamIdx, J, 14)
        gameOppStat!(22) = gameOppStat!(22) + gameStatsPlayer(1 - teamIdx, J, 14)
        gameTeamStat!(23) = gameTeamStat!(23) + gameStatsPlayer(teamIdx, J, 15)
        gameOppStat!(23) = gameOppStat!(23) + gameStatsPlayer(1 - teamIdx, J, 15)
        gameTeamStat!(19) = gameTeamStat!(19) + gameStatsPlayer(teamIdx, J + 3, 16)
        gameOppStat!(18) = gameOppStat!(19) + gameStatsPlayer(1 - teamIdx, J + 3, 16)
        gameTeamStat!(24) = gameTeamStat!(24) + gameStatsPlayer(teamIdx, J + 3, 14)
        gameOppStat!(24) = gameOppStat!(24) + gameStatsPlayer(1 - teamIdx, J + 3, 14)
        gameTeamStat!(25) = gameTeamStat!(25) + gameStatsPlayer(teamIdx, J + 3, 15)
        gameOppStat!(25) = gameOppStat!(25) + gameStatsPlayer(1 - teamIdx, J + 3, 15)
    Next

    For J = 0 To 9
        gameTeamStat!(20) = gameTeamStat!(20) + gameInts(teamIdx, J, 2)
        gameOppStat!(20) = gameOppStat!(20) + gameInts(1 - teamIdx, J, 2)
        gameTeamStat!(26) = gameTeamStat!(26) + gameInts(teamIdx, J, 0)
        gameOppStat!(26) = gameOppStat!(26) + gameInts(1 - teamIdx, J, 0)
        gameTeamStat!(27) = gameTeamStat!(27) + gameInts(teamIdx, J, 1)
        gameOppStat!(27) = gameOppStat!(27) + gameInts(1 - teamIdx, J, 1)
    Next

    gameTeamStat!(21) = gameTeamStat!(21) + gameStatsTeam(teamIdx, 19)
    gameOppStat!(21) = gameOppStat!(21) + gameStatsTeam(1 - teamIdx, 19)
    gameTeamStat!(37) = gameTeamStat!(37) + gameStatsTeam(teamIdx, 35)
    gameOppStat!(37) = gameOppStat!(37) + gameStatsTeam(1 - teamIdx, 35)
    gameTeamStat!(38) = gameTeamStat!(38) + gameStatsTeam(teamIdx, 36)
    gameOppStat!(38) = gameOppStat!(38) + gameStatsTeam(1 - teamIdx, 36)
    gameTeamStat!(30) = gameTeamStat!(30) + gameStatsTeam(teamIdx, 28)
    gameOppStat!(30) = gameOppStat!(30) + gameStatsTeam(1 - teamIdx, 28)
    gameTeamStat!(29) = gameTeamStat!(29) + gameStatsTeam(teamIdx, 27)
    gameOppStat!(29) = gameOppStat!(29) + gameStatsTeam(1 - teamIdx, 27)
    gameTeamStat!(31) = gameTeamStat!(31) + gameStatsTeam(teamIdx, 29)
    gameOppStat!(31) = gameOppStat!(31) + gameStatsTeam(1 - teamIdx, 29)

    For L = 0 To 12:
        gameTeamStat!(L) = gameTeamStat!(L) + gameStatsTeam(teamIdx, L)
        gameOppStat!(L) = gameOppStat!(L) + gameStatsTeam(1 - teamIdx, L)
    Next

    gameTeamStat!(13) = gameTeamStat!(13) + sackStatsTeam(teamIdx, 0)
    gameTeamStat!(14) = gameTeamStat!(14) + sackStatsTeam(teamIdx, 1)
    gameOppStat!(13) = gameOppStat!(13) + sackStatsTeam(1 - teamIdx, 0)
    gameOppStat!(14) = gameOppStat!(14) + sackStatsTeam(1 - teamIdx, 1)

    For J = 0 To 9
        For J1 = 0 To 2
            totalInts(teamIdx, J, J1) = totalInts(teamIdx, J, J1) + gameInts(teamIdx, J, J1)
        Next
    Next

    For J = 0 To 14
        For J1 = 0 To 1
            totalSacks(teamIdx, J, J1) = totalSacks(teamIdx, J, J1) + sackStatsOpp(teamIdx, J, J1)
        Next
    Next

    Cls

    Print ">>>>>"
    Print " THE STATS ARE BEING SAVED UNDER"
    Print "THE FOLLOWING NAME::: ";
    Color teamRat_GAME(teamIdx, 2), teamRat_GAME(teamIdx, 1): Print statFile$
    Color 15, 0

    Open diskPaths$(1) + statFile$ For Output As #2

    Print #2, gameTeams$(teamIdx)

    For I = 0 To 9:
        Print #2, rbacks$(teamIdx, I)
    Next

    For I = 0 To 5:
        Print #2, wdRec$(teamIdx, I)
    Next

    For I = 0 To 3:
        Print #2, qbacks$(teamIdx, I)
    Next

    For I = 0 To 2:
        Print #2, kickRet$(teamIdx, I)
    Next

    For I = 0 To 2:
        Print #2, pret$(teamIdx, I)
    Next

    Print #2, punter$(teamIdx, 0)

    For I = 0 To 1:
        Print #2, kicker$(teamIdx, I)
    Next

    For J = 0 To 13
        For K = 0 To 17:
            Print #2, gamePlayerStat!(J, K)
        Next
    Next

    For L = 0 To 38:
        Print #2, gameTeamStat!(L)
    Next

    For L = 0 To 38:
        Print #2, gameOppStat!(L)
    Next

    For J = 0 To 9
        Print #2, defInts$(teamIdx, J)
        For J1 = 0 To 2
            Print #2, totalInts(teamIdx, J, J1)
        Next
    Next

    For J = 0 To 14
        Print #2, defSacks$(teamIdx, J)
        For J1 = 0 To 1
            Print #2, totalSacks(teamIdx, J, J1)
        Next
    Next

    For I = 0 To 9:
        Print #2, recLongRush!(teamIdx, I)
        Print #2, recLongRushTxt$(teamIdx, I, 0)
        Print #2, recLongRushTxt$(teamIdx, I, 1)
    Next

    For I = 0 To 15:
        Print #2, recLongRec!(teamIdx, I)
        Print #2, recLongRecTxt$(teamIdx, I, 0)
        Print #2, recLongRecTxt$(teamIdx, I, 1)
    Next

    For I = 0 To 3:
        Print #2, recLongPass!(teamIdx, I)
        Print #2, recLongPassTxt$(teamIdx, I, 0)
        Print #2, recLongPassTxt$(teamIdx, I, 1)
    Next

    For I = 0 To 2:
        Print #2, recLongKR!(teamIdx, I)
        Print #2, recLongKRTxt$(teamIdx, I, 0)
        Print #2, recLongKRTxt$(teamIdx, I, 1)
    Next

    For I = 0 To 2:
        Print #2, recLongPR!(teamIdx, I)
        Print #2, recLongPRTxt$(teamIdx, I, 0)
        Print #2, recLongPRTxt$(teamIdx, I, 1)
    Next

    For I = 0 To 1:
        Print #2, recLongFG!(teamIdx, I)
        Print #2, recLongFGTxt$(teamIdx, I)
        For I1 = 0 To 4:
            Print #2, gameFGM!(teamIdx, I, I1)
            Print #2, gameFGA!(teamIdx, I, I1)
        Next
    Next

    For I = 0 To 9:
        Print #2, recLongInt!(teamIdx, I)
        Print #2, recLongIntTxt$(teamIdx, I, 0)
        Print #2, recLongIntTxt$(teamIdx, I, 1)
    Next

    Close #2

    Print "<<<<<"
    Print
    Print

    numberGames = 0

    'Read stats in from file
    If _FileExists(diskPaths$(1) + statFile$ + "S") Then
        Call ReadScheduleStats(statFile$, null, numberGames)
    End If

    If numberGames < MAX_SCHED_STATS Then
        numberGames = numberGames + 1
    End If

    Select Case gameLoc

        Case 2
            If periodNbr > 4 Then
                locIndicator$(numberGames) = "X-" + Str$(periodNbr - 4) + " OT"
            Else
                locIndicator$(numberGames) = "X"
            End If

        Case teamIdx
            If periodNbr > 4 Then
                locIndicator$(numberGames) = "H-" + Str$(periodNbr - 4) + " OT"
            Else
                locIndicator$(numberGames) = "H"
            End If

        Case (1 - teamIdx)
            If periodNbr > 4 Then
                locIndicator$(numberGames) = "A-" + Str$(periodNbr - 4) + " OT"
            Else
                locIndicator$(numberGames) = "A"
            End If

    End Select

    teamScore(numberGames) = score(teamIdx, 0)
    oppName$(numberGames) = gameTeams$(1 - teamIdx)
    oppScore(numberGames) = score(1 - teamIdx, 0)
    gameAttendance&(numberGames) = actualAttendance&
    gameSite$(numberGames) = gameStadium$
    powerRating(0, numberGames) = teamRat_GAME(teamIdx, 4)
    powerRating(1, numberGames) = teamRat_GAME(1 - teamIdx, 4)

    Call SaveScheduleStats(statFile$, numberGames)

    Erase oppScore, powerRating
    Erase teamScore
    Erase gameAttendance&
    Erase locIndicator$, oppName$, gameSite$
    Erase gameTeamStat!, gameOppStat!, gamePlayerStat!

End Sub


'----------------------------------------
'            pbplog Routine
'----------------------------------------
'This subroutine is designed to help me
'print information about where the game
'is at during the code. The general idea
'was originally to be called everywhere
'there is PBP. This helps me debug where
'issues are happening in a game.
Sub pbplog (lineNumber)

    'Print #11, "Called from line "; lineNumber
    'Print #11, gameTeams$(0); " vs "; gameTeams$(1)
    'Print #11, lineNumber, ydLine, ydsToFirst, fgAttLine, gameClock!, score(P1, 0), score(D, 0)
    'Print #11, gameTeams$(P1), playCall
    'Print #11, penaltyYds!
    'Print #11, gameTeams$(P1)
    'Print #11, qbacks$(P1, qbNumber(P1)), qbMobility(P1)
    'Print #11, pbpString$
    'Print #11,

End Sub


'----------------------------------------
'       ChooseGameTeam Subroutine
'----------------------------------------
'This subroutine provides interface for
'choosing a team for the game
Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)

    Shared previousYear$

    Shared teamIndicator$()

    Do

        chosenIdx = 0

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 4)

        Locate 7, 32

        Locate , 12: Print teamIndicator$(gameIdx)
        Locate , 12: Print "ENTER DISK ID: "
        Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 8,

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ <> Chr$(13) Then

                Locate 8, 27: Print I$;

                NY$ = ""

                Do Until NY$ = Chr$(13)

                    NY$ = GetKeyPress$

                    If NY$ = Chr$(8) And Len(I$) >= 1 Then
                        col% = Pos(0)
                        I$ = Left$(I$, (Len(I$) - 1))
                        Locate , col% - 1
                        Print " ";
                        Locate , col% - 1
                    Else
                        I$ = I$ + NY$
                        Print NY$;
                    End If

                Loop

                I$ = Left$(I$, (Len(I$) - 1))

                If Not (I$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(I$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                        fileLength& = LOF(1)

                        Field #1, 15 As Q$(0)

                        numberTeams = fileLength& / TEAM_SIZE_BYTES

                        For teamIdx = 1 To numberTeams
                            Get #1, teamIdx
                            'numberTeams = numberTeams + 1
                            teamNames$(teamIdx) = Q$(0)
                            teamIndex%(teamIdx) = teamIdx
                        Next teamIdx

                        Close #1

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

                previousYear$ = yearNumber$

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, gameIdx)
                'teamGender$(gameIdx) = GetTeamGender$(yearNumber$)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do

                        YCoord = Int((80 - Len(teamName$)) / 2)
                        Locate 2, YCoord
                        Color teamRat_GAME(gameIdx, 2), teamRat_GAME(gameIdx, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

                'chosenIdx = -1 means there was invalid data
            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    'If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor = 0: HiLiteBGColor = 7
        Rows% = LAST_ROW - 6
        Columns% = 4
        Spacing% = 6
        Row% = 5
        Column% = 1

        Color , 4
        Cls

        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate LAST_ROW, 1
        Color 14
        Print Tab(3); "PG DN";
        Print Tab(18); "ENTER";
        Color 15: Print "-SELECTS TEAM";
        Color 14
        Print Tab(39); "ESC";
        Color 15: Print "-SELECT NEW ID";
        Color 14
        Print Tab(72); "PG UP";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, gameIdx, validData)

        End If

    End If

    If validData = 0 And ExitCode% <> 27 Then

        selectedIdx = -1
        Locate 9,
        Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA.... PRESS ANY KEY AND TRY ANOTHER DISK ID", 20, PASS)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, PASS)

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx, dataOK)

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES

    Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

    Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$

    Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$

    Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$

    Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$

    Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$

    Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$

    Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$

    Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)

    Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)

    Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)

    Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)

    Field #1, 200 As Q$, 200 As Q$, 128 As Q$, 12 As Q$(122), 2 As Q$(123), 12 As Q$(124), 2 As Q$(125), 2 As Q$(126), 2 As Q$(127), 2 As Q$(128), 12 As Q$(129), 2 As Q$(130), 2 As Q$(131), 2 As Q$(132), 2 As Q$(133), 2 As Q$(134), 200 As Q$, 150 As Q$

    Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)

    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)

    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)

    Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)

    Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

    Get #1, teamIdx

    targetTeam$ = RTrim$(Q$(1))
    gameMascots$(gameIdx) = RTrim$(Q$(2))

    For I1 = 0 To 7
        leagRat_GAME(gameIdx, I1) = CVI(Q$(I1 + 3))
    Next I1

    For I1 = 0 To 4
        teamRat_GAME(gameIdx, I1) = CVI(Q$(I1 + 11))
    Next I1

    For I1 = 0 To 9
        rbacks$(gameIdx, I1) = RTrim$(Q$(I1 * 5 + 16))
        rbRushContrib(gameIdx, I1) = CVI(Q$(I1 * 5 + 17))
        rbRushAvg(gameIdx, I1) = CVI(Q$(I1 * 5 + 18))
        rbNumRec(gameIdx, I1) = CVI(Q$(I1 * 5 + 19))
        rbYdsPerC(gameIdx, I1) = CVI(Q$(I1 * 5 + 20))
    Next I1

    For I1 = 0 To 5
        wdRec$(gameIdx, I1) = RTrim$(Q$(I1 * 3 + 66))
        wrNumRec(gameIdx, I1) = CVI(Q$(I1 * 3 + 67))
        wrYdsPerC(gameIdx, I1) = CVI(Q$(I1 * 3 + 68))
    Next I1

    For I1 = 0 To 3
        qbacks$(gameIdx, I1) = RTrim$(Q$(I1 * 5 + 84))
        qbNumAtt(gameIdx, I1) = CVI(Q$(I1 * 5 + 85))
        qbCompPct(gameIdx, I1) = CVI(Q$(I1 * 5 + 86))
        qbPctInt(gameIdx, I1) = CVI(Q$(I1 * 5 + 87))
        qbArmRat(gameIdx, I1) = CVI(Q$(I1 * 5 + 88))
    Next I1

    For I1 = 0 To 2
        kickRet$(gameIdx, I1) = RTrim$(Q$(I1 * 3 + 104))
        krNumRet(gameIdx, I1) = CVI(Q$(I1 * 3 + 105))
        krYdsPerRet(gameIdx, I1) = CVI(Q$(I1 * 3 + 106))
    Next I1

    For I1 = 0 To 2
        pret$(gameIdx, I1) = RTrim$(Q$(I1 * 3 + 113))
        prNumRet(gameIdx, I1) = CVI(Q$(I1 * 3 + 114))
        prYdsPerRet(gameIdx, I1) = CVI(Q$(I1 * 3 + 115))
    Next I1

    punter$(gameIdx, 0) = RTrim$(Q$(122))
    ydsPerPunt(gameIdx, 0) = CVI(Q$(123))

    kicker$(gameIdx, 0) = RTrim$(Q$(124))
    kickerFGA(gameIdx, 0) = CVI(Q$(125))
    kickerFGPct(gameIdx, 0) = CVI(Q$(126))
    kickerIdx(gameIdx, 0) = CVI(Q$(127))
    kickerPATPct(gameIdx, 0) = CVI(Q$(128))

    kicker$(gameIdx, 1) = Q$(129)
    kickerFGA(gameIdx, 1) = CVI(Q$(130))
    kickerFGPct(gameIdx, 1) = CVI(Q$(131))
    kickerIdx(gameIdx, 1) = CVI(Q$(132))
    kickerPATPct(gameIdx, 1) = CVI(Q$(133))

    ydsPerComp(gameIdx) = CVI(Q$(134))

    For I1 = 0 To 14
        defSacks$(gameIdx, I1) = Q$(I1 * 2 + 135)
        playerSacks(gameIdx, I1) = CVI(Q$(I1 * 2 + 136))
    Next I1

    For I1 = 0 To 9
        defInts$(gameIdx, I1) = Q$(I1 * 2 + 165)
        playerInts(gameIdx, I1) = CVI(Q$(I1 * 2 + 166))
    Next I1

    Close #1

    If CheckString%(targetTeam$) = 1 Then

        dataOK = 0

    Else

        If Left$(gameMascots$(gameIdx), 1) = " " Or gameMascots$(gameIdx) = "" Then
            gameMascots$(gameIdx) = gameTeams$(gameIdx)
        End If

        indRushPct(gameIdx, 0) = rbRushContrib(gameIdx, 0)

        For currBack = 9 To 1 Step -1
            If rbRushContrib(gameIdx, currBack) <> 0 Then
                indRushPct(gameIdx, currBack) = rbRushContrib(gameIdx, currBack) - rbRushContrib(gameIdx, currBack - 1)
            End If
        Next

        teamSacks(gameIdx) = 0
        teamInts(gameIdx) = 0

        For I1 = 0 To 14
            teamSacks(gameIdx) = teamSacks(gameIdx) + playerSacks(gameIdx, I1)
            If playerSacks(gameIdx, 0) > 0 Then
                defSacks(gameIdx) = 1
            End If
        Next I1

        For I1 = 0 To 9
            teamInts(gameIdx) = teamInts(gameIdx) + playerInts(gameIdx, I1)
            If playerInts(gameIdx, 0) > 0 Then
                defInts(gameIdx) = 1
            End If
        Next I1

        dataOK = 1

    End If

End Sub


'----------------------------------------
'      GetKeyWhenNoDelay Subroutine
'----------------------------------------
'Allows user to silently press a key
'(no prompt) when the game is not autoplay
Sub GetKeyWhenNoDelay ()

    'If we're not in autoplay
    If Not (autoPlay = 1 And playerMode = 2) Then
        I$ = GetKeyPress$
    End If

End Sub


'----------------------------------------
'          EndOfGame Subroutine
'----------------------------------------
'This subroutine serves as the final portion
'of a post game routine. At this point,
'stats have been saved and we are simply
'looking to determine whether a new game
'should be started.
Sub EndOfGame

    For X = 0 To 1
        'Print #11, gameTeams$(X); ";"; gameStatsTeam(X, 1); ";"; gameStatsTeam(X, 3)
    Next X

    'Print #11, "ST PENALTIES:"; totalSTPenalties
    'Print #11, "TOTAL PENALTIES:"; gameStatsTeam(0, 11) + gameStatsTeam(1, 11)

    Close #11
    Call POSTGAME

    Cls

    ' Prompt to play another game
    If autoPlay <> 1 Then

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        pbpString$ = "PLAY ANOTHER GAME? (Y/N)"
        Locate 7, (80 - Len(pbpString$)) / 2
        Print pbpString$

        Color 15, 0

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
        Cls
        Locate LAST_ROW / 2, 33
        Print "PLEASE WAIT ..."
    End If

End Sub


'----------------------------------------
'         fatigueLevel% Function
'----------------------------------------
'This routine determines a team fatigue
'level that can be used to impact
'plays the later a game goes on. The level
'currently is used as an integer multiplier
'to increase yardage gains on offense due
'to a fatigued defense.

'Considering several impac such as:
'- Increase Q4 chance of explosive play (run/pass):
'  chanceScale = 10
'- Increase fumble chance:  fumbleChance = fumbleChance * (2.0 - fatigueLevel)

Function fatigueLevel%

    ' Calculate fatigue (7% per OT period)
    fatigue = 1.0 + ((quarter - 4) * 0.07)

    ' Floor at 50% performance
    If fatigue < 0.5 Then
        fatigue = 0.5
    End If

    fatigueLevel% = fatigue

End Function


'----------------------------------------
'           fgLimit Function
'----------------------------------------
'This function returns the distance at
'which the average kicker is likely to
'limit their attempts
Function fgLimit%

    Dim kickLimit As Integer

    If ruleOptType = 0 Then

        Select Case ruleOptColl

            Case 0 To 1
                'Up to 1980's
                kickLimit = 40

            Case 2 To 6
                '1980's - 1990's
                kickLimit = 50

            Case 7 To 8
                '2000's
                kickLimit = 55

            Case 9 To 11
                '2008+
                kickLimit = 60

        End Select

    Else

        Select Case ruleOptPro

            Case 0 To 1
                'Up to 1970's
                kickLimit = 39

            Case 2 To 3
                '1970's - 1990's
                kickLimit = 49

            Case 4
                '2000's
                kickLimit = 55

            Case Is >= 5
                '2008+
                kickLimit = 60

        End Select

    End If

    fgLimit% = kickLimit

End Function


'----------------------------------------
'   GetPlaySequencingModifier Function
'----------------------------------------
'Returns an adjustment value for a team's
'run/pass balance, based on the success of
'the most recent plays executed.
'
'Returns:
'  Positive number = Favor runs more
'  Negative number = Favor passes more
'  0 = No adjustment
'
'Call this EARLY in ComputerOffense to modify
'your run/pass decision logic.
Function GetPlaySequencingModifier% ()

    Dim modifier As Integer
    modifier = 0

    ' ========================================
    ' RULE 1: STAY HOT
    ' ========================================
    ' If last play had big success, favor that type

    If lastPlayType = 0 And lastPlayYards >= 8 Then
        ' Last RUN gained 8+ yards - stay with running
        modifier = modifier + 15
    End If

    If lastPlayType = 0 And lastPlayYards >= 12 Then
        ' Last RUN gained 12+ yards - REALLY stay with it
        modifier = modifier + 10 ' Total +25
    End If

    If lastPlayType = 1 And lastPlayYards >= 12 Then
        ' Last PASS gained 12+ yards - keep passing
        modifier = modifier - 15
    End If

    If lastPlayType = 1 And lastPlayYards >= 20 Then
        ' Last PASS gained 20+ yards - REALLY keep passing
        modifier = modifier - 10 ' Total -25
    End If

    ' ========================================
    ' RULE 2: MIX IT UP
    ' ========================================
    ' If running same play type repeatedly, switch it up

    ' Two plays in a row of same type
    If lastPlayType = last2PlayType And lastPlayType <> 2 Then
        ' Been running/passing twice, defense adjusts
        If lastPlayType = 0 Then
            ' Two runs in a row, favor pass
            modifier = modifier - 18
        Else
            ' Two passes in a row, favor run
            modifier = modifier + 18
        End If
    End If

    ' Three plays in a row of same type (very predictable!)
    If lastPlayType = last2PlayType And lastPlayType = last3PlayType And lastPlayType <> 2 Then
        ' Been doing same thing 3 times, REALLY switch it up
        If lastPlayType = 0 Then
            ' Three runs, strongly favor pass
            modifier = modifier - 15 ' Total -33 with above
        Else
            ' Three passes, strongly favor run
            modifier = modifier + 15 ' Total +33 with above
        End If
    End If

    ' ========================================
    ' RULE 3: STOP THE BLEEDING
    ' ========================================
    ' After losses, get conservative

    If lastPlayYards < -5 Then
        ' Big loss (sack, TFL), get conservative
        ' Handled separately below for pass-specific logic
        ' But slightly favor runs for safety
        modifier = modifier + 8
    End If

    ' Multiple bad plays in a row - very conservative
    If lastPlayYards < 0 And lastPlayType = 1 And last2PlayType = 1 Then
        ' Two incomplete/negative passes in a row
        ' Get the ball in playmakers' hands safely
        modifier = modifier + 12
    End If

    'Reduce likelihood for heavier run teams
    Select Case teamRushPct
        
        Case Is >= 65
            'Run-heavy team
            modifier = CInt(modifier * 0.6)

        Case 55 To 64
            'Slightly run heavy
            modifier = CInt(modifier * 0.8)

    End Select

    ' ========================================
    ' RULE 4: MOMENTUM SHIFT
    ' ========================================
    ' *** Not implemented here - needs additional tracking ***
    ' After turnover or change of possession, teams often run first
    ' (This would require tracking "just got ball back")


    GetPlaySequencingModifier% = modifier

End Function


'----------------------------------------
'        GetSneakChance% Function
'----------------------------------------
'This function returns the chance that
'the active team QB is going to run a
'sneak play (based on mobility)
Function GetSneakChance%

    Select Case qbMobility(P1)

        Case 1 To 2
            '16%, 22%
            sneakChance = (qbMobility(P1) * 6) + 10

        Case 3 To 4
            '36%, 43
            sneakChance = (qbMobility(P1) * 7) + 15

        Case 5 To 6
            '50%, 58%
            sneakChance = (qbMobility(P1) * 8) + 10

        Case 7 To 8
            '66%, 74%
            sneakChance = (qbMobility(P1) * 8) + 10

        Case 9
            '86%
            sneakChance = (qbMobility(P1) * 9) + 5

    End Select


    ' Situational adjustments
    situationBonus = 0
    
    '--- DETERMINE HOW OFTEN SNEAKS HAPPEN FIRST
    '--- THEN RUN WITH THE CODE BELOW AND COMPARE
    '--- I CAN ASSUME I'M GOING TO HAVE TO DECREASE
    '--- THE CHANCES ABOVE

    ' 4th down - no second chance, more likely to sneak
    'If currDown = 4 Then
    '    situationBonus = situationBonus + 15
    'End If
    
    ' Very short distance (inches) - favor sneak
    'If ydsToFirst <= 0.5 Then
    '    situationBonus = situationBonus + 12
    'ElseIf ydsToFirst <= 0.75 Then
    '    situationBonus = situationBonus + 6
    'End If
    
    ' Goal line (inside 2 yard line) - sneak more effective
    'If ydLine >= 98 Then
    '    situationBonus = situationBonus + 8
    'End If
    
    ' Modern era - QBs more willing/trained to sneak
    'If teamYears(P1) >= 2010 Then
    '    situationBonus = situationBonus + 5
    'ElseIf teamYears(P1) >= 1990 Then
    '    situationBonus = situationBonus + 3
    'End If
    
    ' Apply bonus and cap at 95% (never 100% certain)
    sneakChance = sneakChance + situationBonus
    'If sneakChance > 95 Then sneakChance = 95
    
    GetSneakChance% = sneakChance

End Function


'----------------------------------------
'        FindMarkerColumn Function
'----------------------------------------
'This function determines which column
'to place the ball/arrow marker in
Function FindMarkerColumn (X)

    FindMarkerColumn = 11 + Int((X + Int(X / 5) - 1) / 2)

End Function


'----------------------------------------
'          GetTeamYear Function
'----------------------------------------
'This subroutine determines the 4-digit
'team year from the team loaded by the
'file. It uses the 2 digit year to make
'assumptions. (00 to 24 are year 2000.)
'The earliest years of pro football began
'in 1925, so years 00 to 24 are safe to
'be considered as 2000's
Function GetTeamYear% (yearFromName$)

    teamYear% = Val(RTrim$(yearFromName$))

    'For 2-digit years
    If teamYear% >= 0 And teamYear% < 25 Then
        GetTeamYear% = 2000 + teamYear%
    End If

    If teamYear% >= 25 And teamYear% <= 99 Then
        GetTeamYear% = 1900 + teamYear%
    End If

End Function


'----------------------------------------
'     ShouldUseQuickPassing Function
'----------------------------------------
'Returns TRUE if recent plays suggest we should
'use quick/safe passing (screens, 3-step drops).
'
'Call this AFTER determining we're passing, to
'decide WHAT TYPE of pass to call.
Function ShouldUseQuickPassing% ()
    
    ShouldUseQuickPassing% = 0
    
    ' ========================================
    ' POST-SACK RECOVERY
    ' ========================================
    ' Just got sacked - get ball out quick
    
    If lastPlayType = 1 And lastPlayYards <= -5 Then
        ' Sacked on last play, go quick
        ShouldUseQuickPassing% = True
        Exit Function
    End If
    
    ' ========================================
    ' PRESSURE BUILDING
    ' ========================================
    ' Multiple passes with minimal/negative yards
    
    If lastPlayType = 1 And last2PlayType = 1 Then
        If lastPlayYards <= 2 Then
            ' Last two plays were passes with little success
            ' Simplify and get ball out quick
            ShouldUseQuickPassing% = 1
            Exit Function
        End If
    End If
    
    ' ========================================
    ' THIRD-AND-LONG AFTER SACK
    ' ========================================
    ' Got sacked, now facing 3rd-and-long
    ' Sometimes better to throw quick and punt than risk another sack
    
    If currDown = 3 And ydsToFirst >= 12 And lastPlayYards < -3 Then
        ' 3rd-and-long after loss, 40% chance of quick pass
        If d100% <= 40 Then
            ShouldUseQuickPassing% = 1
            Exit Function
        End If
    End If
    
End Function


'----------------------------------------
'         yardsToString Function
'----------------------------------------
'This functions conerts a yardage value
'(ie, from a play gain / loss) to a string
'value that is printable.
'The key purpose of this is to force output
'to 1 decimal place.
Function yardsToDecimalStr$ (sourceYds!)

    If (sourceYds! * 10 = 83) Or (sourceYds! * 10) = 88 Or (sourceYds! * 10 = 93) Or (sourceYds! * 10 = 98) Then
        'For some reason, when fixing these to 1 decimal place,
        'will result in values like .300000000000001
        yardsToDecimalStr$ = FUsing$(Str$(sourceYds!), "##.#", 1)
    Else
        yardsToDecimalStr$ = Str$(CInt((sourceYds!) * 10) / 10)
    End If

End Function





Sub CountGames ()

    Shared teamRatings()

    Shared previousYear$

    Shared scheduleAP%(), scheduleNG%()

    Shared scheduleYN$()

    'Read Schedule Data
    scheduleFile$ = _OpenFileDialog$("Choose Schedule file", diskPaths$(3), "*.SCD", "Schedule files [.SCD]")

    'Choose Team File
    Color 15, 0
    Cls

    Call MenuBox(6, 8, 61, 7, 15, 4)

    Locate 7, 32

    Locate , 12: Print "ENTER DISK ID: "
    Print
    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    Print
    Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"

    I$ = GetKeyPress$

    If I$ <> Chr$(27) Then

        If I$ <> Chr$(13) Then

            Locate 7, 27: Print I$;

            NY$ = ""

            Do Until NY$ = Chr$(13)

                NY$ = GetKeyPress$

                If NY$ = Chr$(8) And Len(I$) >= 1 Then
                    col% = Pos(0)
                    I$ = Left$(I$, (Len(I$) - 1))
                    Locate , col% - 1
                    Print " ";
                    Locate , col% - 1
                Else
                    I$ = I$ + NY$
                    Print NY$;
                End If

            Loop

            I$ = Left$(I$, (Len(I$) - 1))

            If Not (I$ = "" And yearNumber$ <> "") Then

                yearNumber$ = RTrim$(I$)
                Erase teamNames$
                numberTeams = 0

                Cls

                If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                    MsgText$ = "NO TEAMS WERE FOUND MATCHING THAT ID... PRESS ANY KEY TO SELECT ANOTHER ID"
                    BoxWidth = 20
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)
                    Exit Sub

                Else

                    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$ For Random As #1 Len = TEAM_SIZE_BYTES

                    fileLength& = LOF(1)

                    Field #1, 15 As Q$(0)

                    numberTeams = fileLength& / TEAM_SIZE_BYTES

                    For teamIdx = 1 To numberTeams
                        Get #1, teamIdx
                        'numberTeams = numberTeams + 1
                        teamNames$(teamIdx) = Q$(0)
                        teamIndex%(teamIdx) = teamIdx
                    Next teamIdx

                    Close #1

                End If 'Done checking if file exists

            End If 'Done checking for blank year number

            previousYear$ = yearNumber$

        Else

            yearNumber$ = previousYear$

        End If 'Done checking for ENTER key

    End If 'Done checking for ESC

    'Process schedule file
    If scheduleFile$ <> "" And numberTeams > 0 Then

        test = scheduleAP%(0)

        Call LoadScheduleInfo(scheduleFile$)

        test = scheduleAP%(0)

        Open "GamesCount" For Append As #3
        Print #3, "TeamName,NumberGames"

        'Loop through each team
        For currTeam = 1 To numberTeams

            totalGames = 0

            'Process each game in schedule
            For I = 1 To scheduleAP%(0)

                scheduleYN$(I, 0) = RTrim$(scheduleYN$(I, 0))
                scheduleYN$(I, 1) = RTrim$(scheduleYN$(I, 1))

                If currTeam = scheduleNG%(I, 0) And yearNumber$ = scheduleYN$(I, 0) Or currTeam = scheduleNG%(I, 1) And yearNumber$ = scheduleYN$(I, 1) Then
                    totalGames = totalGames + 1
                End If

            Next 'Done looping through games

            'Write matching games
            Print #3, teamNames$(currTeam); ","; totalGames

        Next

        Close #3

    End If


End Sub

