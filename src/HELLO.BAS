$Debug
$Color:32

Const GAME_VERSION$ = "7.00"
Const MAX_SCHEDULE_GAMES = 1500
Const MAX_TOURNAMENT_GAMES = 200
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const TEAMS_PER_DIV_COL = 15
Const TEAMS_PER_DIV_PRO = 10
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30
Const DIVS_PER_LEAGUE = 3
Const TEAMS_PER_LEAGUE = 30
Const MAX_CAREER_YEARS = 15
Const MAX_CONFERENCES = 40

Const MAX_SCHED_STATS = 120
Const SCHEDULE_SIZE_BYTES = 56

DefInt A-Z

' General purpose
'$Include: 'KeyInput.bi'
'$Include: 'QPProEqu.bi'
'$Include: 'PadRight.bi'
'$Include: 'QBWRITE.bi'

' For Utils
'$Include: 'BoxRoutines.bi'
'$Include: 'FileTeamSelection.bi'
'$Include: 'Variables.bi'
Dim Shared fgAttLine
Dim Shared gameTeamStat!(0 To 38), gameOppStat!(0 To 38), gamePlayerStat!(0 To 13, 0 To 17)
Dim Shared gameInts(1, 9, 2), gameLongFG(1, 1), gameStatsPlayer(1, 13, 17), gameStatsTeam(1, 36)
Dim statsFGA!(0 To 1, 0 To 4), statsFGM!(1, 4)
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

Data 4-3 READ,5-2 READ,4-3 PINCH,5-2 PINCH,4-3 INSIDE STUNT
Data 5-2 INSIDE STUNT,4-3 OUTSIDE STUNT,5-2 OUTSIDE STUNT
Data 4-3 PASS RUSH,5-2 PASS RUSH,GOAL LINE

Data STANDARD ZONE,STANDARD M-M,SHORT ZONE,MAN UNDER,NICKEL,PREVENT
Data BLITZ (1-ILB),BLITZ (1-OLB),BLITZ (2-ILB),BLITZ (2-OLB),BLITZ ENGAGE

Data BLAST,DIVE,OFF TACKLE,END SWEEP,OPTION RUN
Data DRAW PLAY,TRAP,3 STEP DROP,SHORT PASS,MED PASS
Data LONG PASS,SHORT SCREEN,MED-PLAY ACT PASS,LONG-PLAY ACT PASS,SIDELINE PASS
Data ROLLOUT PASS,BOOTLEG PASS,PUNT,FG ATT,SCOUT REPORT

For I = 0 To 14:
    Read tickerPeriod$(I)
Next

For I = 1 To 11:
    Read defFormation$(I)
Next I

For I = 1 To 11:
    Read passCov$(I)
Next I

'-- this has been retired in favor of RP$()
For I = 1 To 20:
    Read offensePlay$(I)
Next I

Data 0,3,-6,0,-3,0,0,3,-3,-3: 'BLAST DATA
Data 0,3,-3,0,-6,0,0,6,0,0: 'DIVE
Data -3,0,0,3,3,0,0,-3,0,0: 'OFF TACKLE
Data 0,-3,3,0,3,3,-3,-3,0,0: 'SWEEP
Data 0,-3,6,0,6,3,-3,-6,0,0: 'OPTION
Data -6,-6,-3,-3,-3,-3,-3,-3,6,6: 'DRAW
Data 3,3,-3,-3,-6,-6,-6,-6,-3,-3: 'TRAP

Data OFF,5,DEF,5,OFF,10: 'PENALTY INFO

Data 74,88,52,78,33,68,16,62,10,52: 'FIELD GOAL %'s / INTERVALS

Data -3,-4,0,3,-6,-6,0,0,0,0,0: ' INT HITCH
Data -3,-4,0,-1,-6,-6,0,0,0,0,0: ' INT SP
Data 0,-1,0,-1,0,-1,0,0,0,0,0: ' INT MP
Data 3,2,0,-1,4,5,0,0,0,0,0: ' INT LP
Data -6,-6,-6,-6,-6,-6,0,0,0,0,0: ' INT SCREEN
Data 0,-1,0,0,2,0,0,0,0,0,0: ' INT MED PA
Data 3,2,3,2,4,5,0,0,0,0,0: ' INT L PA
Data 0,1,-1,-1,0,0,0,0,0,0,0: ' INT SL
Data 0,0,0,0,0,0,0,0,0,0,0: ' INT ROLLOUT
Data 0,0,0,0,0,0,0,0,0,0,0: ' INT BOOTLEG

Data 31,21,21,11,40,50,8,8,-5,-5,-13: ' C% HITCH
Data 21,11,11,1,30,40,-10,-10,-20,-20,-23: ' C% SP
Data 1,-9,6,-4,-5,-10,-19,-19,-17,-17,-13: ' C% MP
Data -19,-29,-9,-19,-30,-40,-25,-25,-25,-25,-17: ' C% LP
Data 40,40,40,40,50,50,20,20,-24,-24,-33: ' C% SCREEN
Data 6,-19,-9,-19,-10,-10,-17,-17,-13,-13,-6: ' C% MED PA
Data -14,-29,-19,-29,-30,-40,-25,-25,-17,-17,-6: ' C% L PA
Data 6,-14,1,-4,10,10,-13,-13,-19,-19,-17: ' C% SL
Data -5,5,-10,10,0,0,-19,-17,-19,-13,-13: ' C% ROLLOUT
Data -5,5,-10,10,0,0,-19,-17,-19,-13,-13: ' C% BOOTLEG

Data 0,1,-1,0,1,2,1,1,2,2,3: ' Y/C HITCH
Data 0,1,-1,0,1,2,3,3,21,21,25: ' Y/C SP
Data 0,1,0,0,1,2,21,21,25,25,28: ' Y/C MED
Data 0,1,1,2,-1,-1,33,33,42,42,42: ' Y/C LP
Data 0,1,-1,0,1,2,1,1,25,25,36: ' Y/C SCREEN
Data 1,1,-1,-2,-1,-1,25,25,28,28,31: ' Y/C MED PA
Data 1,1,1,2,-1,-1,42,42,42,42,42: ' Y/C L PA
Data 0,-1,1,2,1,2,18,18,28,28,33: ' Y/C SL
Data 0,1,-1,0,1,2,21,25,21,28,28: ' Y/C ROLL
Data 0,1,-1,0,1,2,21,25,21,28,28: ' Y/C BOOTLEG

Data -8,-8,-4,-4,-8,-8,-6,-8,-4,-8,-4: ' SCK HITCH
Data -4,-4,0,0,-8,-8,-2,-2,0,0,2: ' SCK SP
Data 0,0,-2,-2,2,-6,3,3,13,13,23: ' SCK MP
Data 4,4,0,0,8,-4,13,13,23,23,33: ' SCK LP
Data -8,-8,-8,-4,-8,-8,-4,-4,0,0,3: ' SCK SCRN
Data 0,0,-2,-2,2,-4,13,13,23,23,33: ' SCK MED PA
Data 4,4,0,0,8,-2,23,23,33,33,43: ' SCK L PA
Data -4,-4,-2,-2,2,-6,0,0,3,3,13: ' SCK SL
Data -6,-6,-4,-4,-8,-8,3,13,3,23,23: ' SCK ROLL
Data -6,-6,-4,-4,-8,-8,3,13,3,23,23: ' SCK BOOTLEG

Data 0,0,1,4,11,21,31,49,62,74,81,85,88,90,92: ' RM%(0,X)
Data 3,5,8,10,15,23,32,47,58,68,75,80,85,88,92: ' RM%(1,X)

Data 69,80,77,61,62,77,64,45,36,69,49,11,2,15,48,45,31,21,29,78,55,48,65,70,58,61,78,70,68,65,70,63,65,70,76,81,87,79: ' runYdAdj(0,X)
Data -4,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,6,7: ' runYdAdj(1,X)

'brkawayYds
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

Data 28,89,99,100,0,0,0,0,0,0,0,0,0,0: 'HITCH 1
Data 10,53,96,97,98,99,100,0,0,0,0,0,0,0: 'HITCH 2
Data 2,37,83,96,97,98,99,100,0,0,0,0,0,0: ' HITCH 3
Data 2,32,71,92,94,96,97,98,99,100,0,0,0,0: ' HITCH 4
Data 2,22,59,92,93,94,100,0,0,0,0,0,0,0: ' HITCH 5
Data 2,12,63,88,90,92,93,94,95,96,97,98,99,100: ' HITCH 6
Data 2,6,38,86,88,90,92,94,95,96,97,98,99,100: ' HITCH 7
Data 2,6,25,74,82,90,92,94,95,96,97,98,99,100: ' HITCH 8
Data 2,6,25,69,77,85,87,90,92,95,97,98,99,100: ' HITCH 9
Data 2,6,25,66,74,82,84,87,89,92,94,96,98,100: ' HITCH 10
Data 34,72,98,99,100,0,0,0,0,0,0,0,0,0: ' SCREEN 1
Data 14,54,97,98,99,100,0,0,0,0,0,0,0,0: ' SCREEN 2
Data 10,55,80,95,96,97,98,99,100,0,0,0,0,0: ' SCREEN 3
Data 10,51,75,91,93,95,96,97,98,99,100,0,0,0: ' SCREEN 4
Data 10,30,70,90,92,94,95,96,97,98,99,100,0,0: ' SCREEN 5
Data 10,35,71,86,88,90,92,94,95,96,97,98,99,100: ' SCREEN 6
Data 10,32,68,83,85,87,89,91,93,95,96,98,99,100: ' SCREEN 7
Data 10,29,65,80,82,85,87,89,91,93,95,97,99,100: ' SCREEN 8
Data 10,21,57,72,77,82,86,89,91,93,95,97,99,100: ' SCREEN 9
Data 10,13,49,64,72,79,85,89,91,93,95,97,99,100: ' SCREEN 10
Data 0,97,99,100,0,0,0,0,0,0,0,0,0,0: ' SP 1
Data 0,45,96,98,99,100,0,0,0,0,0,0,0,0: ' SP 2
Data 0,10,82,97,98,99,100,0,0,0,0,0,0,0: ' SP 3
Data 0,0,63,94,95,96,97,98,99,100,0,0,0,0: ' SP 4
Data 0,0,45,91,93,95,96,97,98,99,100,0,0,0: ' SP 5
Data 0,0,45,80,86,92,94,95,96,97,98,99,100,0: ' SP 6
Data 0,0,40,70,80,90,93,94,95,96,97,98,99,100: ' SP 7
Data 0,0,40,65,75,85,88,90,93,94,96,98,99,100: ' SP 8
Data 0,0,30,51,66,81,88,90,92,94,96,98,99,100: ' SP 9
Data 0,0,20,37,57,77,88,90,92,94,96,98,99,100: ' SP 10
Data 17,77,94,99,100,0,0,0,0,0,0,0,0,0: ' MP 1
Data 6,35,76,96,97,98,99,100,0,0,0,0,0,0: ' MP 2
Data 0,0,52,92,96,98,100,0,0,0,0,0,0,0: ' MP 3
Data 0,0,40,80,87,93,95,97,99,100,0,0,0,0: ' MP 4
Data 0,0,4,69,81,89,94,96,98,100,0,0,0,0: ' MP 5
Data 0,0,5,58,70,78,83,87,91,95,98,100,0,0: ' MP 6
Data 0,0,0,41,57,70,78,83,88,92,95,98,100,0: ' MP 7
Data 0,0,0,19,36,51,66,78,88,92,94,96,98,100: ' MP 8
Data 0,0,0,13,26,43,61,76,86,90,93,96,98,100: ' MP 9
Data 0,0,0,11,22,33,51,66,76,82,87,92,96,100: ' MP 10
Data 1,56,98,100,0,0,0,0,0,0,0,0,0,0: ' LP 1
Data 0,0,53,95,96,100,0,0,0,0,0,0,0,0: ' LP 2
Data 0,0,0,67,87,97,98,99,100,0,0,0,0,0: ' LP 3
Data 0,0,0,13,65,97,98,99,100,0,0,0,0,0: ' LP 4
Data 0,0,0,0,32,64,96,97,98,99,100,0,0,0: ' LP 5
Data 0,0,0,0,12,40,82,86,90,93,96,98,100,0: ' LP 6
Data 0,0,0,0,0,26,69,75,81,86,91,94,97,100: ' LP 7
Data 0,0,0,0,0,0,17,40,64,72,80,88,94,100: ' LP 8
Data 0,0,0,0,0,0,6,29,54,64,74,84,92,100: ' LP 9
Data 0,0,0,0,0,0,3,23,44,56,68,80,90,100: ' LP 10


Data 0,10,20,30,40,50,60,70,80,90,50,55,60,65,70,75,80,85,90,95,100
Data 0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2

'QB Y/C ADJ
Data 0,10,20,30,40,50,60,70,40,45,50,55,60,65,70,50,53,57,60,63,67
Data 0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3

For I = 1 To 7:
    For I1 = 1 To 10:
        'Run Ydg Adjustments (Based on DEF?)
        Read defYdAdj(I, I1)
    Next I1
Next I

For I = 0 To 2:
    'Penalty Info
    Read penaltyDesc$(I) 'OFF/DEF
    Read penaltyYds(I) '# Yds
Next I

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
        '8 = SL
        '9 = Rollout
        '10 = Bootleg
        For J = 1 To 11:
            'Pass Adjustments per coverage
            Read passCovAdj(I, I1, J)
        Next J
    Next I1
Next I

For I = 0 To 1:
    For I1 = 0 To 14:
        'Run yard adjustments?
        Read RM%(I, I1)
    Next
Next

For I = 0 To 1:
    For I1 = 1 To 38:
        'Run yard adjustments?
        Read runYdAdj(I, I1)
    Next
Next

For I = 1 To 38:
    For I1 = 1 To 4:
        'Breakaway yardage???
        Read brkawayYds(I, I1)
    Next
Next

For I = 1 To 30:
    For I1 = 1 To 2:
        'Goalline yard adjustments
        Read goalLnYdAdj(I, I1)
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
            Read S2%(I, I1, J)
        Next
    Next
Next

For I = 1 To 2:
    For I1 = 1 To 21:
        'Play Action pass adjustment???
        Read PS%(I, I1)
    Next
Next

For I = 1 To 2:
    For I1 = 0 To 20:
        'Y/C% Adjustment
        Read ydsPerCompPctAdj(I1, I)
    Next
Next


Data BLAST,DIVE,OFF TACKLE,TOSS SWEEP,OPTION RUN,DRAW PLAY,MISDIRECTION
Data QB SNEAK,POWER SWEEP,QUICK PITCH,QUICK TRAP,ISOLATION,SPRINT DRAW
Data OVER THE TOP,COUNTER
Data 3 STEP DROP,SHORT PASS,MEDIUM PASS,LONG PASS,SCREEN PASS
Data MED PLAY ACT PASS,LONG PLAY ACT PASS,SIDELINE PASS,ROLLOUT PASS
Data BOOTLEG PASS,CROSS,CURL PASS,DEEP POST,DEEP CORNER

For I = 1 To 29: Read playDesc$(I): Next

' Game options
'location$(0) = "@ VISITING TEAM"
location$(1) = "HOME FIELD  "
location$(2) = "NEUTRAL SITE"

overtime$(0) = "NO OVERTIME          "
overtime$(1) = "ONE 15-MINUTE QUARTER"
overtime$(2) = "SUDDEN DEATH TO WIN  "
overtime$(3) = "NCAA DIVISION I-AA   "

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
ruleYrPro$(9) = "2023-PRESENT"

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

yesNo$(0) = "Y"
yesNo$(1) = "N"



'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

'Default new folders to current path
For I = 0 To 3

    Select Case opSysType$
        Case "[WINDOWS]":
            diskPaths$(I) = _CWD$ + "\"
        Case "[MAC]", "[MACOSX]", "[LINUX]":
            diskPaths$(I) = _CWD$ + "/"
    End Select

Next

'Read-in existing DEV.BAS
If _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Input As #1

    'DEV.BAS will not have the "Schedule" folder configured
    For I = 0 To 2
        Input #1, diskPaths$(I)
    Next

    Close #1

    Kill "DEV.BAS"

End If

'Write FOLDERS.CFG
If Not _FileExists("FOLDERS.CFG") Then

    Open "FOLDERS.CFG" For Output As #2

    For I = 0 To 3
        Print #2, diskPaths$(I)
    Next

    Close #2

End If

'Read in path values
Open "FOLDERS.CFG" For Input As #3

For I = 0 To 3
    Input #3, diskPaths$(I)
Next

Close #3

'Run main menu
SELECTION% = 1

Do

    Call Main_Menu(SELECTION%)

Loop Until SELECTION% = 7

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
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
                    Call FOOTBALL

                Case 2:
                    Call TeamMgt_Menu

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
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT"
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT "
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

        ReDim menuChoices$(1 To 4)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS     "
        menuChoices$(2) = "TEAM CAREER OPTIONS             "
        'menuChoices$(3) = "COMPARE REPLAY STATS TO ACTUAL  "
        'menuChoices$(4) = "MODIFY REPLAY STATS             "
        menuChoices$(3) = "MERGE SEPARATE STATISTICAL FILES"
        menuChoices$(4) = "RETURN TO MAIN MENU             "
        SELECTION% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call MERGE

                Case 4:
                    '

            End Select

        End If

    Loop Until SELECTION% = 4 Or ExitCode% = 27

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
        SELECTION% = 1
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
        MAMenu menuChoices$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% = 27 Then Exit Sub

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

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

    Loop Until SELECTION% >= 20 Or ExitCode% = 27

    If SELECTION% = 21 Then
        System
    End If

End Sub


' General purpose
'$Include: 'KeyInput.bm'
'$Include: 'QPProEqu.bm'
'$Include: 'PadRight.bm'
'$Include: 'QBWRITE.bm'

' For Utils
'$Include: 'BoxRoutines.bm'
'$Include: 'ConfigureDrives.bm'
'Include: 'FileTeamSelection.bm'


'----------------------------------------
'          ChooseTeam Subroutine
'----------------------------------------
'Provides dialog for user to select teams
' from a previously populated team file.
'The focus of this routine is to accept
' a disk ID from the user. It then relies
' on separate subroutines to assist with
' actually choosing the team, and also
' reading in data for the chosen team
Sub ChooseTeam (yearNumber$, numberTeams, chosenIdx, teamName$)

    Shared teamRatings()

    'Shared teamNames$()

    Shared previousYear$

    Do

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
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
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

                Call SelectTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%)
                'teamGender$(gameIdx) = GetTeamGender$(yearNumber$)

                If ExitCode% <> 27 And numberTeams <> 0 And chosenIdx > 0 Then

                    Do

                        YCoord = Int((80 - Len(teamName$)) / 2)
                        Locate 2, YCoord
                        Color teamRatings(2), teamRatings(1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

                'chosenIdx = -1 means there was invalid data
            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N" Or chosenIdx = -1

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'          SelectTeam Subroutine
'----------------------------------------
'Presents selection menu of all teams
' found in the target "disk" file.
'The focus of this routine is confirming
' a team choice. It then relies on
' another subroutine to read in data
' from the selected team.
Sub SelectTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%)

    ExitCode% = -1
    validData = 0

    If numberTeams > 0 And CheckString%(teamNames$(numberTeams)) = 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor = 0: HiLiteBGColor = 7
        Rows = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";
        Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";
        Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadTeam(yearNumber$, teamName$, selectedIdx)
            Call ReadExtraData(yearNumber$, selectedIdx, validData)

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
'           ReadTeam Subroutine
'----------------------------------------
'Reads in all data for the selected team,
' given it's index in the team file.
'This routine is highly customized TO
' the specific game for which it has
' been included for.
Sub ReadTeam (teamYear$, targetTeam$, teamIdx)

    Shared ydsCompAdj

    Shared mascot$, teamNick$

    Shared leagueRatings(), teamRatings()

    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks(), armRating()

    Shared intName$(), krName$()
    Shared pkName$(), prName$(), punterName$()
    Shared qbName$(), rbName$()
    Shared sackName$(), wrName$()

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
    mascot$ = RTrim$(Q$(2))

    For I = 0 To 7:
        leagueRatings(I) = CVI(Q$(I + 3))
    Next I

    For I = 0 To 4:
        teamRatings(I) = CVI(Q$(I + 11))
    Next I

    'Running Backs
    For I = 0 To 9
        'Name
        rbName$(I) = Q$(I * 5 + 16)
        'Rush Att
        rushContrib(I) = CVI(Q$(I * 5 + 17))
        'Rush Avg
        rushAvg(I) = CVI(Q$(I * 5 + 18))
        'Receptions
        rbRecepts(I) = CVI(Q$(I * 5 + 19))
        'Rec Avg
        rbRecAvg(I) = CVI(Q$(I * 5 + 20))
    Next I

    'Wide Receivers
    For I = 0 To 5
        'Name
        wrName$(I) = Q$(I * 3 + 66)
        'Receptions
        recContrib(I) = CVI(Q$(I * 3 + 67))
        'Rec Avg
        wrRecAvg(I) = CVI(Q$(I * 3 + 68))
    Next I

    'Quarterbacks
    For I = 0 To 3
        'Name
        qbName$(I) = Q$(I * 5 + 84)
        'Attempts
        passContrib(I) = CVI(Q$(I * 5 + 85))
        'Completion %
        compPct(I) = CVI(Q$(I * 5 + 86))
        'Interception %
        qbIntPct(I) = CVI(Q$(I * 5 + 87))
        'Arm Rating | Yds/Comp Adj
        armRating(I) = CVI(Q$(I * 5 + 88))
    Next I

    'Kickoff Returners
    For I = 0 To 2
        'Name
        krName$(I) = Q$(I * 3 + 104)
        'Returns
        krContrib(I) = CVI(Q$(I * 3 + 105))
        'Return Avg
        krRetAvg(I) = CVI(Q$(I * 3 + 106))
    Next I

    'Punt Returners
    For I = 0 To 2
        'Name
        prName$(I) = RTrim$(Q$(I * 3 + 113))
        'Returns
        prContrib(I) = CVI(Q$(I * 3 + 114))
        'Return Avg
        prRetAvg(I) = CVI(Q$(I * 3 + 115))
    Next I

    'Punter Name
    punterName$(0) = RTrim$(Q$(122))
    'Punt Average
    puntAvg(0) = CVI(Q$(123))

    'Place Kickers
    For I = 0 To 1
        'Name
        pkName$(I) = RTrim$(Q$(I * 5 + 124))
        'FGA
        pkFGA(I) = CVI(Q$(I * 5 + 125))
        'FG%
        pkFGContrib(I) = CVI(Q$(I * 5 + 126))
        'PAT
        pkPAT(I) = CVI(Q$(I * 5 + 127))
        'PAT %
        pkPATContrib(I) = CVI(Q$(I * 5 + 128))

    Next I

    ydsCompAdj = CVI(Q$(134))

    'Sacks
    For I = 0 To 14
        'Name
        sackName$(I) = Q$(I * 2 + 135)
        '# Sacks
        numSacks(I) = CVI(Q$(I * 2 + 136))
    Next I

    'Interceptions
    For I = 0 To 9
        'Name
        intName$(I) = Q$(I * 2 + 165)
        '# Ints
        numInts(I) = CVI(Q$(I * 2 + 166))
    Next I

    'Close team file
    Close #1

End Sub


'----------------------------------------
'        ReadExtraData Subroutine
'----------------------------------------
'Reads in extra data for the selected team,
' given it's index in the team file.
'This extra file usually includes info.
' related to stadiums, coaches, etc.
'This routine is highly customized TO
' the specific game for which it has
' been included for.
Sub ReadExtraData (teamYear$, teamIdx, dataOK)

    Shared teamAttendance&

    Shared stadium$, Z$

    If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$) Then

        MsgText$ = "WARNING!!! NO DATA FILE WAS FOUND FOR THIS TEAM. PRESS ANY KEY TO CONTINUE"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = DATA_SIZE_BYTES

        Field #2, 15 As X$, 30 As QQ$(1), 4 As QQ$(3), 1 As QQ$(4)

        Get #2, teamIdx

        stadium$ = RTrim$(QQ$(1))
        teamAttendance& = CVL(QQ$(3))
        Z$ = QQ$(4)

        Close #2

        T& = teamAttendance&

        If Z$ <> "x" Or teamAttendance& > 200000 Then

            dataOK = 0
            stadium$ = ""
            teamAttendance& = 0

        Else

            dataOK = 1

        End If

    End If

End Sub


'----------------------------------------
'         nameStatFile$ Function
'----------------------------------------
'Helps create uniquely named file to
' store a team's stats.
'
'There are 2 main changes done in order
' to accomodate this:
' 1) Replaces invalid characters with _
' 2) If "ST" is the name, shortens the
'    prefix. This helps with college
'    teams (ARKANSAS vs ARKANSAS ST)
Function nameStatFile$ (teamName$)

    teamPrefix$ = teamName$

    For XX = 4 To 11

        If Mid$(teamPrefix$, XX, 1) = " " Or Mid$(teamPrefix$, XX, 1) = "." Or Mid$(teamPrefix$, XX, 1) = "/" Or Mid$(teamPrefix$, XX, 1) = "_" Then
            Mid$(teamPrefix$, XX, 1) = "_"
        End If

    Next XX

    For XX = 12 To 14

        If Mid$(teamPrefix$, XX, 2) = "ST" Then
            Mid$(teamPrefix$, 9, 1) = "_"
            Mid$(teamPrefix$, 10, 2) = "ST"
        End If

    Next XX

    'Length of 'converted' name for file
    '-- update this if we want to support long files
    QQ = 8

    For XX = 11 To 4 Step -1
        If Mid$(teamPrefix$, XX, 1) = "_" Then
            QQ = XX - 4
        Else
            Exit For
        End If
    Next XX

    nameStatFile$ = Mid$(teamPrefix$, 4, QQ) + "." + Left$(teamPrefix$, 2)

End Function


'----------------------------------------
'          CheckString Function
'----------------------------------------
'This function checks the given string
'to determine if any characters are not
'alpha-numeric or printable.
'Return Values
'1 = non-printable characters were found
'0 = all characters in string are alpha-numeric.
Function CheckString% (targetString$)

    badString = 0
    stringPosition% = 1

    Do

        test = Asc(Mid$(targetString$, stringPosition%, 1))

        'If it is a non-printable character (corrupt data)
        If test > 122 Or test < 32 Then
            stringPosition% = 1
            badString = 1
        Else
            stringPosition% = stringPosition% + 1
        End If

    Loop Until stringPosition% = Len(targetString$) Or badString = 1

    CheckString% = badString

End Function


'----------------------------------------
'        ChooseFile$ Function
'----------------------------------------
'Shows user a file selection menu, based
' on the file spec parameter passed.
'
'fileSpec$ - file specification to load
'            should include base path.
'
' EXAMPLE - C:\Temp\*.tmp
'
'fileDesc$ - brief description of the
'            type of file
'
' RETURNS - base name of chosen file
'            (that is, without extension)
Function ChooseFile$ (fileSpec$, fileDesc$, ExitCode%)

    descLength = Len(fileDesc$)

    Do
        'Display list of files...

        Count% = FileCount%(fileSpec$)

        If Count% = 0 Then

            Locate 10, 8
            zeroCountMsg$ = "NO " + fileDesc$ + " FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU"

            Call CSMsgBox(zeroCountMsg$, 20, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            ReDim foundFiles$(0 To Count%)

            'For X = 1 To Count%
            '8 char's filename + 4 char's (. + ext)
            'foundFiles$(X) = Space$(12)
            'Next

            foundFiles$(0) = fileSpec$
            Call ReadFile(foundFiles$())

            Do

                XCoord = Int((80 - (descLength + 12)) / 2)

                Color , 1
                Cls
                Locate , XCoord
                Color 14
                Print "SELECT "; fileDesc$; " FILE"
                Color 15
                SELECTION% = 1
                START% = 1
                FileFGColor = 15: FileBGColor = 1
                HiLiteFGColor% = 0: HiLiteBGColor% = 7
                Rows% = 20
                Columns% = 3
                Spacing% = 5
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";
                Color 15
                Print "-SELECTS "; fileDesc$;
                Color 14
                Locate , 55
                Print "HIT ESCAPE KEY TO EXIT";
                Color 15

                _MouseShow "DEFAULT"
                MAMenu foundFiles$(), SELECTION%, START%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = RTrim$(foundFiles$(SELECTION%))
                    XCoord = Int((80 - Len(targetFile$)) / 2)
                    Locate 2, XCoord
                    Print targetFile$

                    Do
                        Locate , 31
                        Print "ANY CHANGE? (Y/N)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "N"

    If ExitCode% <> 27 Then
        ChooseFile$ = Left$(RTrim$(targetFile$), Len(targetFile$) - 4)
    End If

End Function


'----------------------------------------
'     ReportFilePrompt_Pro Subroutine
'----------------------------------------
'This subroutine prompts the user for
'the name of the report file to be saved
Sub UserFilePrompt (reportFile$)

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Locate 7, 32

        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
        Locate 7, 37: Input reportFile$

        If reportFile$ <> "" Then

            Do
                Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

End Sub


'----------------------------------------
'     ScheduleFilePrompt Subroutine
'----------------------------------------
'This subroutine prompts the user for
'the name of the schedule file to be saved
Sub ScheduleFilePrompt (scheduleFile$)

    Color 15, 0
    Cls

    Call MenuBox(6, 8, 61, 8, 15, 1)

    Do

        Locate 7, 32

        Locate , 12: Print "ENTER NAME FOR SCHEDULE: "
        Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
        Locate , 12: Print "SAMPLE NAMES ARE INDIANA, BOS68, CHI-76, ETC      "
        Locate 7, 40: Input scheduleFile$

        If scheduleFile$ <> "" Then

            Do

                Locate 14, 12
                Print "ANY CHANGE? (Y/N) "
                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

        End If

    Loop Until (UCase$(I$) = "N" And scheduleFile$ <> "") Or I$ = Chr$(27)


End Sub


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
'$Include: 'CREATCON.BAS'
'$Include: 'CREATNATL.BAS'
'$Include: 'CREATLEAG.BAS'
'$Include: 'DRAFT.BAS'
'$Include: 'HD2HDCON.BAS'
'$Include: 'HD2HDLEAG.BAS'
'Include: 'LOOKY.BAS'


'----------------------------------------
'            LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "3-in-1 Football - View Stats"

    Do

        Color 15, 0
        Cls

        Call MenuBox(8, 8, 61, 6, 15, 1)

        Locate 10, 15

        Locate , 30: Color 3: Print "VIEW/PRINT STATS IN:"
        Locate , 30: Color 15: Print "C";
        Color 7: Print "OLLEGE FORMAT"
        Locate , 30: Color 15: Print "P";
        Color 7: Print "RO FORMAT"
        Locate , 30: Color 15: Print "E";
        Color 7: Print "XIT TO MAIN MENU"

        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "C": statFormat = 0
            Case "P": statFormat = 1
            Case "E", Chr$(27): '
        End Select

        If UCase$(I$) = "C" Or UCase$(I$) = "P" Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                Color 15, 0

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$) And teamNbr > 0 Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Call LoadTeamStats(statFile$, team$, totalGames%, teamPower, SS)
                    Call ViewStatsSummary(team$, totalGames%, statFormat, teamPower, SS)

                End If

            End If

        End If

    Loop Until UCase$(I$) = "E" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, totalGames%, teamPower, SS)

    Shared oppScore(), powerRating()
    Shared statsGI(), statsGS()
    Shared teamRatings(), teamScore()

    Shared gameAttendance&()

    Shared playerStats!(), teamStats!()
    Shared scoreTotals()

    Shared statsFGA!(), statsFGM!()
    Shared statsLPR!(), sackStats!()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsTeam!(), statsOpp!(), statsPlayers!()

    Shared gameSite$(), locIndicator$(), oppName$()

    Shared intName$(), sackName$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()

    Shared statsLFG$(), statsLRec$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, totalGames%

    'Opponent Score
    For I = 1 To MAX_SCHED_STATS:
        Input #1, teamScore(I)
    Next I

    'Opponent
    For I = 1 To MAX_SCHED_STATS:
        Input #1, oppName$(I)
    Next I

    'Team Score
    For I = 1 To MAX_SCHED_STATS:
        Input #1, oppScore(I)
    Next I

    'Home / Away
    For I = 1 To MAX_SCHED_STATS:
        Input #1, locIndicator$(I)
    Next I

    '???
    For I = 0 To 1
        For I1 = 1 To MAX_SCHED_STATS
            Input #1, powerRating(I, I1)
        Next
    Next

    'Attendance
    For I = 1 To MAX_SCHED_STATS:
        Input #1, gameAttendance&(I)
    Next I

    'Stadium
    For I = 1 To MAX_SCHED_STATS:
        Input #1, gameSite$(I)
    Next I

    Close #1

    'STRENGTH OF SCHEDULE
    SS = 0

    For I = 1 To totalGames%
        SS = powerRating(1, I) + SS
    Next

    SS = SS / totalGames%

    'INDIVIDUAL GAME POWER RATINGS
    teamPower = 0

    For I = 1 To totalGames%
        pwrRating = teamScore(I) - oppScore(I) + powerRating(1, I)
        If Left$(locIndicator$(I), 1) = "H" Then
            pwrRating = pwrRating - 3
        End If
        If Left$(locIndicator$(I), 1) = "A" Then
            pwrRating = pwrRating + 3
        End If
        teamPower = teamPower + pwrRating
    Next

    GL = 0

    If totalGames% <= 4 Then
        GL = 5 - totalGames%
        teamPower = teamPower + GL * powerRating(0, 1)
    End If

    teamPower = teamPower / (totalGames% + GL)

    totalLosses = 0
    totalWins = 0
    tiedGames = 0

    D% = 0
    scoreTotals(0) = 0
    scoreTotals(1) = 0

    'Load player stats
    Open diskPaths$(1) + statFile$ For Input As #2

    'We are passed teamName$ as a variable from the disk routine
    'However the format of the files for football is such that
    'it's also contained here.
    'To be consistent with other games, it needs evaluated
    'if this belongs here or somewhere else.
    Input #2, teamName$

    For I = 0 To 9:
        Input #2, rbName$(I)
    Next I

    For I = 0 To 5:
        Input #2, wrName$(I)
    Next I

    For I = 0 To 3:
        Input #2, qbName$(I)
    Next I

    For I = 0 To 2:
        Input #2, krName$(I)
    Next I

    For I = 0 To 2:
        Input #2, prName$(I)
    Next I

    Input #2, punterName$(0)

    For I = 0 To 1:
        Input #2, pkName$(I)
    Next I

    For J = 0 To 13
        For K = 0 To 17:
            Input #2, statsPlayers!(J, K)
        Next K
    Next J

    For L = 0 To 38:
        Input #2, statsTeam!(L)
    Next L

    For L = 0 To 38:
        Input #2, statsOpp!(L)
    Next L

    For J = 0 To 9:
        Input #2, intName$(J)
        For J1 = 0 To 2:
            Input #2, statsGI(J, J1)
        Next
    Next

    For J = 0 To 14:
        Input #2, sackName$(J)
        For J1 = 0 To 1:
            Input #2, statsGS(J, J1)
        Next
    Next

    For I = 0 To 9:
        Input #2, statsLRush!(I)
        Input #2, statsLRush$(I, 0)
        Input #2, statsLRush$(I, 1)
    Next

    For I = 0 To 15:
        Input #2, statsLRec!(I)
        Input #2, statsLRec$(I, 0)
        Input #2, statsLRec$(I, 1)
    Next

    For I = 0 To 3:
        Input #2, statsLPass!(I)
        Input #2, statsLPass$(I, 0)
        Input #2, statsLPass$(I, 1)
    Next

    For I = 0 To 2:
        Input #2, statsLKR!(I)
        Input #2, statsLKR$(I, 0)
        Input #2, statsLKR$(I, 1)
    Next

    For I = 0 To 2:
        Input #2, statsLPR!(I)
        Input #2, statsLPR$(I, 0)
        Input #2, statsLPR$(I, 1)
    Next

    For I = 0 To 1:
        Input #2, statsLFG!(I)
        Input #2, statsLFG$(I)
        For I1 = 0 To 4:
            Input #2, statsFGM!(I, I1)
            Input #2, statsFGA!(I, I1)
        Next
    Next

    For I = 0 To 9:
        Input #2, statsLInt!(I)
        Input #2, statsLInt$(I, 0)
        Input #2, statsLInt$(I, 1)
    Next

    Close #2

    For J = 0 To 13
        For K = 0 To 17:
            playerStats!(J, K) = statsPlayers!(J, K)
        Next K
    Next J

    test = playerStats

    For M = 0 To 12:
        teamStats!(0, M) = statsTeam!(M)
    Next M

    For M = 0 To 12:
        teamStats!(1, M) = statsOpp!(M)
    Next M

    sackStats!(0, 0) = statsTeam!(13)
    sackStats!(0, 1) = statsTeam!(14)
    sackStats!(1, 0) = statsOpp!(13)
    sackStats!(1, 1) = statsOpp!(14)

End Sub


'----------------------------------------
'       ViewStatsSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatsSummary (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared teamRatings()

    Do

        DM% = 0

        Color 15, 0
        Cls

        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Stats Report", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call StatsToReport(0, userFile$, teamName$, totalGames%, statFormat, teamPower, SS)
                    End If

                Case "P"
                    Call StatsToReport(1, "REPORT", teamName$, totalGames%, statFormat, teamPower, SS)

                Case "S"
                    Call StatsToScreen(teamName$, totalGames%, statFormat, teamPower, SS)

            End Select

            Color 15, 0
            Cls

            Call MenuBox(6, 8, 61, 1, 15, 1)

            Do
                Locate 7
                U$ = "DISPLAY STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

            'Call ResetStatsPrompt (statFile$)

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'      StatsToScreen Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's stats to the display
Sub StatsToScreen (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared statsFGA!(), statsFGM!()
    Shared teamStats!(), playerStats!()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsLPR!(), sackStats!(), statsTeam!(), statsOpp!(), statsPlayers!()

    Shared gameAttendance&()

    Shared statsGI(), statsGS()

    Shared teamScore(), teamRatings(), oppScore(), scoreTotals()

    Shared intName$(), locIndicator$(), oppName$(), sackName$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared statsLRec$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$()
    Shared statsLPR$(), gameSite$()

    Color 15, 0
    Cls

    Call TITLE_STATS

    Locate 1, 1
    Color 14, 1
    Print Tab(81);

    header$ = "GAME-BY-GAME REPORT FOR " + teamName$
    Xcoord = Len(header$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord

    Print "GAME-BY-GAME REPORT FOR ";
    Color teamRatings(2), teamRatings(1)
    Print teamName$

    Color 14, 1
    Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD    ";
    Print
    Color 15, 0

    For I = 1 To totalGames%

        Color 7

        If (I Mod 2) = 0 Then Color 15

        Print oppName$(I);
        Locate , 19: Print Using "###"; teamScore(I);
        Print "-";
        Print Using "###   "; oppScore(I);

        If teamScore(I) > oppScore(I) Then
            totalWins = totalWins + 1
            Print "W";
        End If

        If teamScore(I) < oppScore(I) Then
            totalLosses = totalLosses + 1
            Print "L";
        End If

        If teamScore(I) = oppScore(I) Then
            tiedGames = tiedGames + 1
            Print "T";
        End If

        Locate , 31: Print Using "####,###"; gameAttendance&(I);
        Locate , 43: Print locIndicator$(I); "  "; Left$(gameSite$(I), 24);

        Locate , 69
        Print "(";
        Print Using "##"; totalWins;: Print "-";: Print Using "##"; totalLosses;: Print "-";: Print Using "##"; tiedGames;
        Print ")"

        scoreTotals(0) = scoreTotals(0) + teamScore(I)
        scoreTotals(1) = scoreTotals(1) + oppScore(I)

        If (I Mod 20 = 0) Then

            Locate 24, 1
            Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
            I$ = GetKeyPress$
            ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***
            Cls
            Color , 0
            Locate 1, 1: Print Tab(80); " ";
            Locate , Xcoord

            Color 14
            Print "GAME-BY-GAME REPORT FOR ";
            Color teamRatings(2), teamRatings(1)
            Print teamName$

            Color 14
            Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD   "
            Color 15, 0

        End If

    Next I

    If statFormat = 0 Then
        Locate 23, 31
        Print Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS;
    End If

    scoreTotals(0) = 0
    scoreTotals(1) = 0
    totalWins = 0
    totalLosses = 0
    tiedGames = 0

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 2
    Color 12
    Print teamName$; " STATISTICS REPORT"
    Print

    Color 14
    Print "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
    Color 15
    Print teamName$;
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; statsTeam!(32); statsTeam!(33); statsTeam!(34); statsTeam!(35); statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)
    Print "OPPONENTS";
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; statsOpp!(32); statsOpp!(33); statsOpp!(34); statsOpp!(35); statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)
    Print

    Color 14
    Print "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Color 15
    Print teamName$;
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsTeam!(32) / totalGames%; statsTeam!(33) / totalGames%; statsTeam!(34) / totalGames%; statsTeam!(35) / totalGames%; (statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)) / totalGames%
    Print "OPPONENTS";
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsOpp!(32) / totalGames%; statsOpp!(33) / totalGames%; statsOpp!(34) / totalGames%; statsOpp!(35) / totalGames%; (statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)) / totalGames%
    Print

    'H = Home; A = Away; N = Neutral; T = Total
    'D = Dates; T = Total; A = Average
    HD& = 0: AD& = 0: ND& = 0: TD& = 0
    HT& = 0: CT& = 0: NT& = 0: TT& = 0
    HA& = 0: AA& = 0: NA& = 0: TA& = 0

    For I = 1 To totalGames%

        If Left$(locIndicator$(I), 1) = "H" Then
            HD& = HD& + 1
            HT& = HT& + gameAttendance&(I)
        End If

        If Left$(locIndicator$(I), 1) = "A" Then
            AD& = AD& + 1
            CT& = CT& + gameAttendance&(I)
        End If

        If Left$(locIndicator$(I), 1) = "X" Then
            ND& = ND& + 1
            NT& = NT& + gameAttendance&(I)
        End If

        TD& = TD& + 1
        TT& = TT& + gameAttendance&(I)

    Next

    If HD& <> 0 Then HA& = HT& / HD&
    If AD& <> 0 Then AA& = CT& / AD&
    If ND& <> 0 Then NA& = NT& / ND&
    If TD& <> 0 Then TA& = TT& / TD&

    Color 14: Print "ATTENDANCE    DATES      TOTAL    AVERAGE"
    Color 15: Print Using "HOME            ##    #####,###   ###,###"; HD&; HT&; HA&
    Color 15: Print Using "AWAY            ##    #####,###   ###,###"; AD&; CT&; AA&
    Color 15: Print Using "NEUTRAL         ##    #####,###   ###,###"; ND&; NT&; NA&
    Color 15: Print Using "TOTAL           ##    #####,###   ###,###"; TD&; TT&; TA&

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 2
    Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Color 7
    Print "FIRST DOWNS"; Tab(45); Using "####"; teamStats!(0, 0);: Print Tab(65); Using "####"; teamStats!(1, 0)
    Print "RUSHING ATTEMPTS"; Tab(45);
    'Team
    If statFormat = 0 Then
        Print Using "####"; teamStats!(0, 1);
    Else
        Print Using "####"; teamStats!(0, 1) - sackStats!(0, 0);
    End If
    Print Tab(65);
    'Opponent
    If statFormat = 0 Then
        Print Using "####"; teamStats!(1, 1)
    Else
        Print Using "####"; teamStats!(1, 1) - sackStats!(1, 0)
    End If

    Color 15
    Print "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then
        Print Using "####"; teamStats!(0, 2);
    Else
        Print Using "####"; teamStats!(0, 2) + sackStats!(0, 1);
    End If
    Print Tab(65);
    If statFormat = 0 Then
        Print Using "####"; teamStats!(1, 2)
    Else
        Print Using "####"; teamStats!(1, 2) + sackStats!(1, 1)
    End If

    Color 7
    Print "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And teamStats!(0, 1) <> 0 Then
        Print Using "##.#"; teamStats!(0, 2) / teamStats!(0, 1);
    End If
    If statFormat <> 0 And teamStats!(0, 1) - sackStats!(0, 0) <> 0 Then
        Print Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0));
    End If
    Print Tab(65);
    If statFormat = 0 And teamStats!(1, 1) <> 0 Then
        Print Using "##.#"; teamStats!(1, 2) / teamStats!(1, 1)
    End If
    If statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) <> 0 Then
        Print Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0))
    End If
    If statFormat = 0 And teamStats!(1, 1) = 0 Or statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) = 0 Then
        Print
    End If

    Print "RUSHING YARDS PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print Using "###.#"; teamStats!(0, 2) / totalGames%;
    Else
        Print Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%;
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "###.#"; teamStats!(1, 2) / totalGames%
    Else
        Print Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%
    End If

    Print
    Print "PASSES ATTEMPTED"; Tab(45); Using "####"; teamStats!(0, 3);
    Print Tab(65); Using "####"; teamStats!(1, 3)

    Print "PASSES COMPLETED"; Tab(45); Using "####"; teamStats!(0, 4);
    Print Tab(65); Using "####"; teamStats!(1, 4)

    Print "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; teamStats!(0, 5);
    Print Tab(65); Using "####"; teamStats!(1, 5)

    Print "PASS COMPLETION PERCENTAGE"; Tab(44);
    If teamStats!(0, 3) <> 0 Then
        Print Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;
    End If
    If teamStats!(1, 3) <> 0 Then
        Print Tab(64); Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100
    End If
    If teamStats!(1, 3) = 0 Then
        Print
    End If

    Color 15
    Print "NET YARDS PASSING"; Tab(45); Using "####"; teamStats!(0, 6);
    Print Tab(65); Using "####"; teamStats!(1, 6)

    Color 7
    Print "YARDS PER PASS ATTEMPT"; Tab(45);
    If teamStats!(0, 3) <> 0 Then
        Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    End If
    If teamStats!(1, 3) <> 0 Then
        Print Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3)
    End If
    If teamStats!(1, 3) = 0 Then
        Print
    End If

    Print "YARDS PER PASS COMPLETION"; Tab(45);
    If teamStats!(0, 4) <> 0 Then
        Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    End If
    If teamStats!(1, 4) <> 0 Then
        Print Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4)
    End If
    If teamStats!(1, 4) = 0 Then
        Print
    End If

    Print "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; teamStats!(0, 6) / totalGames%;
    Print Tab(64); Using "###.#"; teamStats!(1, 6) / totalGames%
    Print

    Print "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then
        Print Using "####"; teamStats!(0, 1) + teamStats!(0, 3);
    Else
        Print Using "####"; teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3);
    End If
    Print Tab(65);
    If statFormat = 0 Then
        Print Using "####"; teamStats!(1, 1) + teamStats!(1, 3)
    Else
        Print Using "####"; teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)
    End If

    Print "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print Using "###.#"; (teamStats!(0, 1) + teamStats!(0, 3)) / totalGames%;
    Else
        Print Using "###.#"; (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3)) / totalGames%;
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "###.#"; (teamStats!(1, 1) + teamStats!(1, 3)) / totalGames%
    Else
        Print Using "###.#"; (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)) / totalGames%
    End If

    Color 15
    Print "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then
        Print Using "####"; teamStats!(0, 2) + teamStats!(0, 6);
    Else
        Print Using "####"; teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6);
    End If
    Print Tab(65);
    If statFormat = 0 Then
        Print Using "####"; teamStats!(1, 2) + teamStats!(1, 6)
    Else
        Print Using "####"; teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)
    End If

    Color 7
    Print "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then
        Print Using "##.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / (teamStats!(0, 1) + teamStats!(0, 3));
    Else
        Print Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3));
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "##.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / (teamStats!(1, 1) + teamStats!(1, 3))
    Else
        Print Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3))
    End If

    Print "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print Using "###.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / totalGames%;
    Else
        Print Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / totalGames%;
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "###.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / totalGames%
    Else
        Print Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / totalGames%
    End If

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 2: Color 12
    Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Color 7

    Print "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print Using "###"; statsTeam!(22);: Print "/";: Print Using "####"; statsTeam!(23);
    Print Tab(61); Using "###"; statsOpp!(22);: Print "/";: Print Using "####"; statsOpp!(23)

    Print "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsTeam!(22) <> 0 Then
        Print Using "##.#"; statsTeam!(23) / statsTeam!(22);
    End If
    If statsOpp!(22) <> 0 Then
        Print Tab(65); Using "##.#"; statsOpp!(23) / statsOpp!(22)
    End If
    If statsOpp!(22) = 0 Then
        Print
    End If

    Print "KICKOFF RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsTeam!(22) / totalGames%;: Print Tab(65); Using "##.#"; statsOpp!(22) / totalGames%

    Print "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print Using "###"; statsTeam!(24);: Print "/";: Print Using "####"; statsTeam!(25);
    Print Tab(61); Using "###"; statsOpp!(24);: Print "/";: Print Using "####"; statsOpp!(25)

    Print "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsTeam!(24) <> 0 Then
        Print Using "##.#"; statsTeam!(25) / statsTeam!(24);
    End If
    If statsOpp!(24) <> 0 Then
        Print Tab(65); Using "##.#"; statsOpp!(25) / statsOpp!(24)
    End If
    If statsOpp!(24) = 0 Then
        Print
    End If

    Print "PUNT RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsTeam!(24) / totalGames%;: Print Tab(65); Using "##.#"; statsOpp!(24) / totalGames%

    Print "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print Using "###"; statsTeam!(26);: Print "/";: Print Using "####"; statsTeam!(27);
    Print Tab(61); Using "###"; statsOpp!(26);: Print "/";: Print Using "####"; statsOpp!(27)

    Print "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsTeam!(26) <> 0 Then
        Print Using "##.#"; statsTeam!(27) / statsTeam!(26);
    End If
    If statsOpp!(26) <> 0 Then
        Print Tab(65); Using "##.#"; statsOpp!(27) / statsOpp!(26)
    End If
    If statsOpp!(26) = 0 Then
        Print
    End If

    Print "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsTeam!(26) / totalGames%;
    Print Tab(65); Using "##.#"; statsOpp!(26) / totalGames%
    Print

    Print "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print Using "###"; teamStats!(0, 7);: Print "/";: Print Using "####"; teamStats!(0, 8);
    Print Tab(61); Using "###"; teamStats!(1, 7);: Print "/";: Print Using "####"; teamStats!(1, 8)

    Print "AVERAGE YARDS PER PUNT"; Tab(45);
    If teamStats!(0, 7) <> 0 Then
        Print Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    End If
    If teamStats!(1, 7) <> 0 Then
        Print Tab(65); Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7)
    End If
    If teamStats!(1, 7) = 0 Then
        Print
    End If

    Print "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print Using "##.#"; teamStats!(0, 7) / totalGames%;: Print Tab(65); Using "##.#"; teamStats!(1, 7) / totalGames%
    Print

    Print "FUMBLES / FUMBLES LOST"; Tab(41);
    Print Using "###"; teamStats!(0, 9);: Print "/";: Print Using "####"; teamStats!(0, 10);
    Print Tab(61); Using "###"; teamStats!(1, 9);: Print "/";: Print Using "####"; teamStats!(1, 10)

    Print "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print Using "###"; teamStats!(0, 11);: Print "/";: Print Using "####"; teamStats!(0, 12);
    Print Tab(61); Using "###"; teamStats!(1, 11);: Print "/";: Print Using "####"; teamStats!(1, 12)

    Print "AVERAGE YARDS PER PENALTY"; Tab(45);
    If teamStats!(0, 11) <> 0 Then
        Print Using "##.#"; teamStats!(0, 12) / teamStats!(0, 11);
    End If
    If teamStats!(1, 11) <> 0 Then
        Print Tab(65); Using "##.#"; teamStats!(1, 12) / teamStats!(1, 11)
    End If
    If teamStats!(1, 11) = 0 Then
        Print
    End If

    Print "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print Using "##.#"; teamStats!(0, 11) / totalGames%;: Print "/";: Print Using "##.#"; teamStats!(0, 12) / totalGames%;
    Print Tab(60); Using "##.#"; teamStats!(1, 11) / totalGames%;: Print "/";: Print Using "##.#"; teamStats!(1, 12) / totalGames%

    Print "SAFETIES"; Tab(45);: Print Using "####"; statsTeam!(21);: Print Tab(65); Using "####"; statsOpp!(21)

    Print "TIMES SACKED / YARDS LOST"; Tab(41);
    Print Using "###"; sackStats!(0, 0);: Print "/";: Print Using "####"; sackStats!(0, 1);
    Print Tab(61); Using "###"; sackStats!(1, 0);: Print "/";: Print Using "####"; sackStats!(1, 1)

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"

    If D% <> 1 Then

        For I1 = 0 To 9

            For I2 = 0 To 3

                If rbName$(I1) = qbName$(I2) Then
                    playerStats!(I1, 0) = playerStats!(I1, 0) + playerStats!(I2 + 10, 0)
                    playerStats!(I1, 1) = playerStats!(I1, 1) + playerStats!(I2 + 10, 1)
                    playerStats!(I1, 2) = playerStats!(I1, 2) + playerStats!(I2 + 10, 2)
                    playerStats!(I2 + 10, 0) = 0
                    playerStats!(I2 + 10, 1) = 0
                    playerStats!(I2 + 10, 2) = 0
                End If

            Next I2

        Next I1

    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13

        If playerStats!(I1, 0) <> 0 Then

            DM% = DM% + 1
            Color 7

            If DM% / 2 = Int(DM% / 2) Then Color 15
            If I1 <= 9 Then Print rbName$(I1);
            If I1 > 9 Then Print qbName$(I1 - 10);

            Print Tab(18); Using "####  ####   ##.#      ###.#    ##    "; playerStats!(I1, 0); playerStats!(I1, 1); playerStats!(I1, 1) / playerStats!(I1, 0); playerStats!(I1, 1) / totalGames%; playerStats!(I1, 2);

            If I1 > 9 Then
                Print
            Else
                Print Using "###"; statsLRush!(I1);: Print statsLRush$(I1, 0); Tab(64); statsLRush$(I1, 1)
            End If

        End If

    Next I1

    Color 14
    Print
    Print teamName$; Tab(18);

    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1); teamStats!(0, 2); teamStats!(0, 2) / teamStats!(0, 1); teamStats!(0, 2) / totalGames%; statsTeam!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1) - sackStats!(0, 0); teamStats!(0, 2) + sackStats!(0, 1); (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0)); (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%; statsTeam!(15)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1); teamStats!(1, 2); teamStats!(1, 2) / teamStats!(1, 1); teamStats!(1, 2) / totalGames%; statsOpp!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1) - sackStats!(1, 0); teamStats!(1, 2) + sackStats!(1, 1); (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0)); (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%; statsOpp!(15)
    End If

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5

        If playerStats!(I1, 6) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print wrName$(I1); Tab(18);
            Print Using "####  ####   "; playerStats!(I1, 6); playerStats!(I1, 7);

            If playerStats!(I1, 6) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; playerStats!(I1, 7) / playerStats!(I1, 6);
            End If

            Print Using "      ###.#    ##    "; playerStats!(I1, 7) / totalGames%; playerStats!(I1, 8);
            Print Using "###"; statsLRec!(I1 + 10);: Print statsLRec$(I1 + 10, 0); Tab(64); statsLRec$(I1 + 10, 1)

        End If

    Next I1

    For I1 = 0 To 9

        If playerStats!(I1, 3) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print rbName$(I1); Tab(18);
            Print Using "####  ####   "; playerStats!(I1, 3); playerStats!(I1, 4);

            If playerStats!(I1, 3) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; playerStats!(I1, 4) / playerStats!(I1, 3);
            End If

            Print Using "      ###.#    ##    "; playerStats!(I1, 4) / totalGames%; playerStats!(I1, 5);
            Print Using "###"; statsLRec!(I1);: Print statsLRec$(I1, 0); Tab(64); statsLRec$(I1, 1)

        End If

    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);

    Print Using "####  ####   "; teamStats!(0, 4); teamStats!(0, 6);
    If teamStats!(0, 4) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    End If
    Print Using "      ###.#    ##"; teamStats!(0, 6) / totalGames%; statsTeam!(17)

    Print "OPPONENT TOTALS"; Tab(18);
    Print Using "####  ####   "; teamStats!(1, 4); teamStats!(1, 6);
    If teamStats!(1, 4) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4);
    End If
    Print Using "      ###.#    ##"; teamStats!(1, 6) / totalGames%; statsOpp!(17)

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3

        If playerStats!(I1, 9) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            'Att / Comp / Comp % / Yds
            Print qbName$(I1); Tab(18); Using "####  ### ###.# #### "; playerStats!(I1, 9); playerStats!(I1, 10); playerStats!(I1, 10) / playerStats!(I1, 9) * 100; playerStats!(I1, 12);

            'Yds / Att
            If playerStats!(I1, 9) = 0 Then
                Print "   --";
            Else
                Print Using "##.#"; playerStats!(I1, 12) / playerStats!(I1, 9);
            End If

            ' Yds / Catch
            If playerStats!(I1, 10) = 0 Then
                Print "   --";
            Else
                Print Using " ##.#"; playerStats!(I1, 12) / playerStats!(I1, 10);
            End If

            'TD / INT / INT %
            Print Using " ###.# ### ### ###.#     "; playerStats!(I1, 12) / totalGames%; playerStats!(I1, 13); playerStats!(I1, 11); playerStats!(I1, 11) / playerStats!(I1, 9) * 100;

            'QB Rating
            awayTies! = QBRating!(I1, statFormat)
            Print Using "###.#"; awayTies!

        End If

    Next I1

    Print

    Color 14
    'Team Totals
    Print teamName$; Tab(18);

    'Att / Comp
    Print Using "####  ### "; teamStats!(0, 3); teamStats!(0, 4);

    'Comp %
    If teamStats!(0, 3) = 0 Then
        Print "   --";
    Else
        Print Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;
    End If

    'Yds
    Print Using " #### "; teamStats!(0, 6);

    'Yds / Att
    If teamStats!(0, 3) = 0 Then
        Print " --";
    Else
        Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    End If

    'Yds / Catch
    If teamStats!(0, 4) = 0 Then
        Print "   --";
    Else
        Print Using " ##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    End If

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; teamStats!(0, 6) / totalGames%; statsTeam!(17); teamStats!(0, 5);

    'INT %
    If teamStats!(0, 3) = 0 Then
        Print "   --"
    Else
        Print Using "  #.#"; teamStats!(0, 5) / teamStats!(0, 3) * 100
    End If

    Print "OPPONENT TOTALS"; Tab(18);

    'Att / Comp
    Print Using "####  ### "; teamStats!(1, 3); teamStats!(1, 4);

    'Comp %
    If teamStats!(1, 3) = 0 Then
        Print "   --"
    Else
        Print Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100;
    End If

    'Yds
    Print Using " #### "; teamStats!(1, 6);

    'Yds / Att
    If teamStats!(1, 3) = 0 Then
        Print " --";
    Else
        Print Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3);
    End If

    'Yds / Catch
    If teamStats!(1, 4) = 0 Then
        Print "   --";
    Else
        Print Using " ##.#"; teamStats!(1, 6) / teamStats!(1, 4);
    End If

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; teamStats!(1, 6) / totalGames%; statsOpp!(17); teamStats!(1, 5);

    'INT %
    If teamStats!(1, 3) = 0 Then
        Print "   --"
    Else
        Print Using "  #.#"; teamStats!(1, 5) / teamStats!(1, 3) * 100
    End If

    Print
    Print
    Color 12

    Print "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If playerStats!(I1, 9) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print qbName$(I1); Tab(22); Using "###"; statsLPass!(I1);: Print statsLPass$(I1, 0); Tab(32); statsLPass$(I1, 1)
        End If
    Next

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If playerStats!(I1, 14) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print krName$(I1); Tab(18);
            Print Using "####  ####   "; playerStats!(I1, 14); playerStats!(I1, 15);
            If playerStats!(I1, 14) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; playerStats!(I1, 15) / playerStats!(I1, 14);
            End If
            Print Using "      ###.#    ##    "; playerStats!(I1, 15) / totalGames%; playerStats!(I1, 16);
            Print Using "###"; statsLKR!(I1);: Print statsLKR$(I1, 0); Tab(64); statsLKR$(I1, 1)
        End If
    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);
    If statsTeam!(22) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsTeam!(22); statsTeam!(23); statsTeam!(23) / statsTeam!(22); statsTeam!(23) / totalGames%; statsTeam!(18)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statsOpp!(22) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsOpp!(22); statsOpp!(23); statsOpp!(23) / statsOpp!(22); statsOpp!(23) / totalGames%; statsOpp!(18)
    End If

    Print
    Print
    Print
    Color 12
    Print "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 3 To 5

        If playerStats!(I1, 14) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print prName$(I1 - 3); Tab(18);
            Print Using "####  ####   "; playerStats!(I1, 14); playerStats!(I1, 15);

            If playerStats!(I1, 14) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; playerStats!(I1, 15) / playerStats!(I1, 14);
            End If

            Print Using "      ###.#    ##    "; playerStats!(I1, 15) / totalGames%; playerStats!(I1, 16);
            Print Using "###"; statsLPR!(I1 - 3);: Print statsLPR$(I1 - 3, 0); Tab(64); statsLPR$(I1 - 3, 1)

        End If

    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);
    If statsTeam!(24) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsTeam!(24); statsTeam!(25); statsTeam!(25) / statsTeam!(24); statsTeam!(25) / totalGames%; statsTeam!(19)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statsOpp!(24) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); statsOpp!(25) / totalGames%; statsOpp!(19)
    End If

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Color 15
    Print punterName$(0); Tab(18); Using "####  ####    "; teamStats!(0, 7); teamStats!(0, 8);
    If teamStats!(0, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    End If
    Print Using "    ##.#      ##     ##      ##"; teamStats!(0, 7) / totalGames%; statsTeam!(31); statsTeam!(30); statsTeam!(28)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; teamStats!(1, 7); teamStats!(1, 8);
    If teamStats!(1, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7);
    End If
    Print Using "    ##.#      ##     ##"; teamStats!(1, 7) / totalGames%; statsOpp!(31); statsOpp!(30)

    Print
    Print
    Print
    Color 12
    Print "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Color 15
    Print teamName$; Tab(18); Using "####  ####    "; teamStats!(0, 7); teamStats!(0, 8);
    If teamStats!(0, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    End If

    Print Using "    ###       ####        ##.#     ##.#"; statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); (teamStats!(0, 8) - statsOpp!(25)) / teamStats!(0, 7)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; teamStats!(1, 7); teamStats!(1, 8);
    If teamStats!(1, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7);
    End If

    '                Ret       Yds         Avg      Net Avg
    If statsTeam!(24) > 0 Then
        oppPuntAvg! = statsTeam!(25) / statsTeam!(24)
    Else
        oppPuntAvg! = 0
    End If
    Print Using "    ###       ####        ##.#     ##.#"; statsTeam!(24); statsTeam!(25); oppPuntAvg!; (teamStats!(1, 8) - statsTeam!(25)) / teamStats!(1, 7)

    Print
    Print
    Print
    Color 12
    Print "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
    Color 15

    testFGA = playerStats!(4, 17)
    testFGM = playerStats!(3, 17)
    testEPA = playerStats!(6, 17)
    testEPM = playerStats!(5, 17)

    If playerStats!(4, 17) <> 0 And playerStats!(6, 17) <> 0 Then

        Print pkName$(0);
        Print Tab(16);

        If playerStats!(6, 17) <> 0 Then
            'Extra Points
            Print Using "### ### ###.# "; playerStats!(5, 17); playerStats!(6, 17); playerStats!(5, 17) / playerStats!(6, 17) * 100;
        End If

        'Field Goals
        If playerStats!(4, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; playerStats!(3, 17); playerStats!(4, 17); playerStats!(3, 17) / playerStats!(4, 17) * 100; statsLFG!(0); statsFGM!(0, 0);
            Print "-";: Print Using "## ##"; statsFGA!(0, 0); statsFGM!(0, 1);
            Print "-";: Print Using "## ##"; statsFGA!(0, 1); statsFGM!(0, 2);
            Print "-";: Print Using "## ##"; statsFGA!(0, 2); statsFGM!(0, 3);
            Print "-";: Print Using "## ##"; statsFGA!(0, 3); statsFGM!(0, 4);
            Print "-";: Print Using "##"; statsFGA!(0, 4)
        End If

    End If

    If playerStats!(8, 17) <> 0 And playerStats!(10, 17) <> 0 Then

        Print pkName$(1); Tab(16);

        If playerStats!(10, 17) <> 0 Then
            Print Using "### ### ###.# "; playerStats!(9, 17); playerStats!(10, 17); playerStats!(9, 17) / playerStats!(10, 17) * 100;
        End If

        If playerStats!(8, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; playerStats!(7, 17); playerStats!(8, 17); playerStats!(7, 17) / playerStats!(8, 17) * 100; statsLFG!(1); statsFGM!(1, 0);
            Print "-";: Print Using "## ##"; statsFGA!(1, 0); statsFGM!(1, 1);
            Print "-";: Print Using "## ##"; statsFGA!(1, 1); statsFGM!(1, 2);
            Print "-";: Print Using "## ##"; statsFGA!(1, 2); statsFGM!(1, 3);
            Print "-";: Print Using "## ##"; statsFGA!(1, 3); statsFGM!(1, 4);
            Print "-";: Print Using "##"; statsFGA!(1, 4)
        End If

    End If

    Print

    NOONE:
    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    For I1 = 0 To 9

        If statsGI(I1, 0) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print intName$(I1); Tab(18);

            '            Ret   Yds
            Print Using "####  ####   "; statsGI(I1, 0); statsGI(I1, 1);

            'Yds / Ret
            If statsGI(I1, 0) = 0 Then
                Print " --";
            Else
                Print Using "##.#"; statsGI(I1, 1) / statsGI(I1, 0);
            End If

            'Yds / Gm, TD
            Print Using "      ###.#    ##    "; statsGI(I1, 1) / totalGames%; statsGI(I1, 2);

            'Long Ret / Against
            Print Using "###"; statsLInt!(I1);: Print statsLInt$(I1, 0); Tab(64); statsLInt$(I1, 1)

        End If

    Next I1

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If statsGS(I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print sackName$(I1); Tab(20); Using "###            ###"; statsGS(I1, 0); statsGS(I1, 1)
        End If
    Next

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress

End Sub

'----------------------------------------
'       StatsToReport Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's stats to the default printer
Sub StatsToReport (printDest, destFile$, teamName$, totalGames%, statFormat, teamPower, SS)

    Shared statsFGA!(), statsFGM!(), gameAttendance&(), statsGI(), statsGS()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsLPR!(), sackStats!(), statsTeam!(), statsOpp!(), statsPlayers!()

    Shared playerStats!(), teamStats!()

    Shared teamScore(), oppScore(), scoreTotals()

    Shared gameSite$(), oppName$()
    Shared intName$(), sackName$(), locIndicator$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()

    Shared statsLRec$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, "GAME-BY-GAME REPORT FOR "; teamName$
    Print #1,
    Print #1, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"

    For I = 1 To totalGames%

        Print #1, oppName$(I); Tab(19); Using "###"; teamScore(I);
        Print #1, "-";: Print #1, Using "###   "; oppScore(I);

        If teamScore(I) > oppScore(I) Then
            totalWins = totalWins + 1
            Print #1, "W";
        End If

        If teamScore(I) < oppScore(I) Then
            totalLosses = totalLosses + 1
            Print #1, "L";
        End If

        If teamScore(I) = oppScore(I) Then
            tiedGames = tiedGames + 1
            Print #1, "T";
        End If

        Print #1, Tab(31);
        Print #1, Using "####,###   "; gameAttendance&(I);
        Print #1, locIndicator$(I); "  "; Left$(gameSite$(I), 24);

        Print #1, Tab(69);
        Print #1, "(";
        Print #1, Using "##"; totalWins;: Print #1, "-";: Print #1, Using "##"; totalLosses;: Print #1, "-";: Print #1, Using "##"; tiedGames;
        Print #1, ")"

        scoreTotals(0) = scoreTotals(0) + teamScore(I)
        scoreTotals(1) = scoreTotals(1) + oppScore(I)

    Next I

    Print #1,
    If statFormat = 0 Then
        Print #1, Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS
    End If

    scoreTotals(0) = 0
    scoreTotals(1) = 0
    totalWins = 0
    totalLosses = 0
    tiedGames = 0

    Print #1,
    Print #1,
    Print #1, "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
    Print #1, teamName$; Tab(30); Using " ###  ###  ###  ###   ###"; statsTeam!(32); statsTeam!(33); statsTeam!(34); statsTeam!(35); statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)
    Print #1, "OPPONENTS"; Tab(30); Using " ###  ###  ###  ###   ###"; statsOpp!(32); statsOpp!(33); statsOpp!(34); statsOpp!(35); statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)
    Print #1,

    '-- Would totalGames% ever be 0?
    Print #1, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Print #1, teamName$; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsTeam!(32) / totalGames%; statsTeam!(33) / totalGames%; statsTeam!(34) / totalGames%; statsTeam!(35) / totalGames%; (statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)) / totalGames%
    Print #1, "OPPONENTS"; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsOpp!(32) / totalGames%; statsOpp!(33) / totalGames%; statsOpp!(34) / totalGames%; statsOpp!(35) / totalGames%; (statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)) / totalGames%
    Print #1,

    HD = 0: AD = 0: ND = 0: TD = 0
    HT = 0: CT = 0: NT = 0: TT = 0
    HA = 0: AA = 0: NA = 0: TA = 0

    For I = 1 To totalGames%
        If Left$(locIndicator$(I), 1) = "H" Then
            HD = HD + 1
            HT = HT + gameAttendance&(I)
        End If
        If Left$(locIndicator$(I), 1) = "A" Then
            AD = AD + 1
            CT = CT + gameAttendance&(I)
        End If
        If Left$(locIndicator$(I), 1) = "X" Then
            ND = ND + 1
            NT = NT + gameAttendance&(I)
        End If
        TD = TD + 1
        TT = TT + gameAttendance&(I)
    Next

    If HD <> 0 Then HA = HT / HD
    If AD <> 0 Then AA = CT / AD
    If ND <> 0 Then NA = NT / ND
    If TD <> 0 Then TA = TT / TD

    Print #1, "ATTENDANCE    DATES      TOTAL    AVERAGE"
    Print #1, Using "HOME            ##    #####,###   ###,###"; HD; HT; HA
    Print #1, Using "AWAY            ##    #####,###   ###,###"; AD; CT; AA
    Print #1, Using "NEUTRAL         ##    #####,###   ###,###"; ND; NT; NA
    Print #1, Using "TOTAL           ##    #####,###   ###,###"; TD; TT; TA
    Print #1, Chr$(12)

    Print #1, "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Print #1, "FIRST DOWNS"; Tab(45); Using "####"; teamStats!(0, 0);
    Print #1, Tab(65); Using "####"; teamStats!(1, 0)

    Print #1, "RUSHING ATTEMPTS"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(0, 1);
    Else
        Print #1, Using "####"; teamStats!(0, 1) - sackStats!(0, 0);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(1, 1)
    Else
        Print #1, Using "####"; teamStats!(1, 1) - sackStats!(1, 0)
    End If

    Print #1, "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(0, 2);
    Else
        Print #1, Using "####"; teamStats!(0, 2) + sackStats!(0, 1);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(1, 2)
    Else
        Print #1, Using "####"; teamStats!(1, 2) + sackStats!(1, 1)
    End If

    Print #1, "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And teamStats!(0, 1) <> 0 Then
        Print #1, Using "##.#"; teamStats!(0, 2) / teamStats!(0, 1);
    End If
    If statFormat <> 0 And teamStats!(0, 1) - sackStats!(0, 0) <> 0 Then
        Print #1, Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0));
    End If

    Print #1, Tab(65);

    If statFormat = 0 And teamStats!(1, 1) <> 0 Then
        Print #1, Using "##.#"; teamStats!(1, 2) / teamStats!(1, 1)
    End If
    If statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) <> 0 Then
        Print #1, Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0))
    End If
    If statFormat = 0 And teamStats!(1, 1) = 0 Or statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) = 0 Then
        Print #1,
    End If

    Print #1, "RUSHING YARDS PER GAME"; Tab(44);

    If statFormat = 0 Then
        Print #1, Using "###.#"; teamStats!(0, 2) / totalGames%;
    Else
        Print #1, Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%;
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "###.#"; teamStats!(1, 2) / totalGames%
    Else
        Print #1, Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%
    End If

    Print #1,
    Print #1, "PASSES ATTEMPTED"; Tab(45); Using "####"; teamStats!(0, 3);: Print #1, Tab(65); Using "####"; teamStats!(1, 3)
    Print #1, "PASSES COMPLETED"; Tab(45); Using "####"; teamStats!(0, 4);: Print #1, Tab(65); Using "####"; teamStats!(1, 4)
    Print #1, "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; teamStats!(0, 5);: Print #1, Tab(65); Using "####"; teamStats!(1, 5)
    Print #1, "PASS COMPLETION PERCENTAGE"; Tab(44);

    If teamStats!(0, 3) <> 0 Then
        Print #1, Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;
    End If
    If teamStats!(1, 3) <> 0 Then
        Print #1, Tab(64); Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100
    Else
        Print #1,
    End If

    Print #1, "NET YARDS PASSING"; Tab(45); Using "####"; teamStats!(0, 6);
    Print #1, Tab(65); Using "####"; teamStats!(1, 6)

    Print #1, "YARDS PER PASS ATTEMPT"; Tab(45);
    If teamStats!(0, 3) <> 0 Then
        Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    End If
    If teamStats!(1, 3) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3)
    Else
        Print #1,
    End If

    Print #1, "YARDS PER PASS COMPLETION"; Tab(45);
    If teamStats!(0, 4) <> 0 Then
        Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    End If
    If teamStats!(1, 4) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4)
    Else
        Print #1,
    End If

    Print #1, "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; teamStats!(0, 6) / totalGames%;
    Print #1, Tab(64); Using "###.#"; teamStats!(1, 6) / totalGames%
    Print #1,

    Print #1, "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(0, 1) + teamStats!(0, 3);
    Else
        Print #1, Using "####"; teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(1, 1) + teamStats!(1, 3)
    Else
        Print #1, Using "####"; teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)
    End If

    Print #1, "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (teamStats!(0, 1) + teamStats!(0, 3)) / totalGames%;
    Else
        Print #1, Using "###.#"; (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3)) / totalGames%;
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (teamStats!(1, 1) + teamStats!(1, 3)) / totalGames%
    Else
        Print #1, Using "###.#"; (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)) / totalGames%
    End If

    Print #1, "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(0, 2) + teamStats!(0, 6);
    Else
        Print #1, Using "####"; teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; teamStats!(1, 2) + teamStats!(1, 6)
    Else
        Print #1, Using "####"; teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)
    End If

    Print #1, "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "##.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / (teamStats!(0, 1) + teamStats!(0, 3));
    Else
        Print #1, Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3));
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "##.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / (teamStats!(1, 1) + teamStats!(1, 3))
    Else
        Print #1, Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3))
    End If

    Print #1, "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / totalGames%;
    Else
        Print #1, Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / totalGames%;
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / totalGames%
    Else
        Print #1, Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / totalGames%
    End If

    Print #1,
    Print #1, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsTeam!(22);: Print #1, "/";: Print #1, Using "####"; statsTeam!(23);
    Print #1, Tab(61); Using "###"; statsOpp!(22);: Print #1, "/";: Print #1, Using "####"; statsOpp!(23)

    Print #1, "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsTeam!(22) <> 0 Then
        Print #1, Using "##.#"; statsTeam!(23) / statsTeam!(22);
    End If
    If statsOpp!(22) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsOpp!(23) / statsOpp!(22)
    Else
        Print #1,
    End If

    Print #1, "KICKOFF RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsTeam!(22) / totalGames%;
    Print #1, Tab(65); Using "##.#"; statsOpp!(22) / totalGames%

    Print #1, "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsTeam!(24);: Print #1, "/";: Print #1, Using "####"; statsTeam!(25);
    Print #1, Tab(61); Using "###"; statsOpp!(24);: Print #1, "/";: Print #1, Using "####"; statsOpp!(25)

    Print #1, "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsTeam!(24) <> 0 Then
        Print #1, Using "##.#"; statsTeam!(25) / statsTeam!(24);
    End If
    If statsOpp!(24) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsOpp!(25) / statsOpp!(24)
    Else
        Print #1,
    End If

    Print #1, "PUNT RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsTeam!(24) / totalGames%;
    Print #1, Tab(65); Using "##.#"; statsOpp!(24) / totalGames%

    Print #1, "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsTeam!(26);: Print #1, "/";: Print #1, Using "####"; statsTeam!(27);
    Print #1, Tab(61); Using "###"; statsOpp!(26);: Print #1, "/";: Print #1, Using "####"; statsOpp!(27)

    Print #1, "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsTeam!(26) <> 0 Then
        Print #1, Using "##.#"; statsTeam!(27) / statsTeam!(26);
    End If
    If statsOpp!(26) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsOpp!(27) / statsOpp!(26)
    Else
        Print #1,
    End If

    Print #1, "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsTeam!(26) / totalGames%;
    Print #1, Tab(65); Using "##.#"; statsOpp!(26) / totalGames%

    Print #1,

    Print #1, "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print #1, Using "###"; teamStats!(0, 7);: Print #1, "/";: Print #1, Using "####"; teamStats!(0, 8);
    Print #1, Tab(61); Using "###"; teamStats!(1, 7);: Print #1, "/";: Print #1, Using "####"; teamStats!(1, 8)

    Print #1, "AVERAGE YARDS PER PUNT"; Tab(45);
    If teamStats!(0, 7) <> 0 Then
        Print #1, Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    End If
    If teamStats!(1, 7) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7)
    Else
        Print #1,
    End If

    Print #1, "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);
    Print #1, Using "##.#"; teamStats!(0, 7) / totalGames%;
    Print #1, Tab(65); Using "##.#"; teamStats!(1, 7) / totalGames%
    Print #1,

    Print #1, "FUMBLES / FUMBLES LOST"; Tab(41);
    Print #1, Using "###"; teamStats!(0, 9);: Print #1, "/";: Print #1, Using "####"; teamStats!(0, 10);
    Print #1, Tab(61); Using "###"; teamStats!(1, 9);: Print #1, "/";: Print #1, Using "####"; teamStats!(1, 10)

    Print #1, "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print #1, Using "###"; teamStats!(0, 11);: Print #1, "/";: Print #1, Using "####"; teamStats!(0, 12);
    Print #1, Tab(61); Using "###"; teamStats!(1, 11);: Print #1, "/";: Print #1, Using "####"; teamStats!(1, 12)

    Print #1, "AVERAGE YARDS PER PENALTY"; Tab(45);
    If teamStats!(0, 11) <> 0 Then
        Print #1, Using "##.#"; teamStats!(0, 12) / teamStats!(0, 11);
    End If
    If teamStats!(1, 11) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 12) / teamStats!(1, 11)
    Else
        Print #1,
    End If

    Print #1, "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print #1, Using "##.#"; teamStats!(0, 11) / totalGames%;: Print #1, "/";: Print #1, Using "##.#"; teamStats!(0, 12) / totalGames%;
    Print #1, Tab(60); Using "##.#"; teamStats!(1, 11) / totalGames%;: Print #1, "/";: Print #1, Using "##.#"; teamStats!(1, 12) / totalGames%

    Print #1, "SAFETIES"; Tab(45);
    Print #1, Using "####"; statsTeam!(21);
    Print #1, Tab(65); Using "####"; statsOpp!(21)

    Print #1, "TIMES SACKED / YARDS LOST"; Tab(41);
    Print #1, Using "###"; sackStats!(0, 0);: Print #1, "/";: Print #1, Using "####"; sackStats!(0, 1);
    Print #1, Tab(61); Using "###"; sackStats!(1, 0);: Print #1, "/";: Print #1, Using "####"; sackStats!(1, 1)
    Print #1, Chr$(12)

    Print #1, "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"

    I = 0

    If D% <> 1 Then
        For I1 = 0 To 9
            For I2 = 0 To 3
                If rbName$(I1) = qbName$(I2) Then
                    playerStats!(I1, 0) = playerStats!(I1, 0) + playerStats!(I2 + 10, 0)
                    playerStats!(I1, 1) = playerStats!(I1, 1) + playerStats!(I2 + 10, 1)
                    playerStats!(I1, 2) = playerStats!(I1, 2) + playerStats!(I2 + 10, 2)
                    playerStats!(I2 + 10, 0) = 0
                    playerStats!(I2 + 10, 1) = 0
                    playerStats!(I2 + 10, 2) = 0
                End If
            Next I2
        Next I1
    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13
        If playerStats!(I1, 0) <> 0 Then
            If I1 <= 9 Then Print #1, rbName$(I1);
            If I1 > 9 Then Print #1, qbName$(I1 - 10);
            Print #1, Tab(18); Using "####  ####   ##.#      ###.#    ##    "; playerStats!(I1, 0); playerStats!(I1, 1); playerStats!(I1, 1) / playerStats!(I1, 0); playerStats!(I1, 1) / totalGames%; playerStats!(I1, 2);
            If I1 > 9 Then
                Print #1,
            Else
                Print #1, Using "###"; statsLRush!(I1);
                Print #1, statsLRush$(I1, 0); Tab(64); statsLRush$(I1, 1)
            End If
        End If
    Next I1

    Print #1, teamName$; Tab(18);
    If statFormat = 0 Then
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1); teamStats!(0, 2); teamStats!(0, 2) / teamStats!(0, 1); teamStats!(0, 2) / totalGames%; statsTeam!(15)
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1) - sackStats!(0, 0); teamStats!(0, 2) + sackStats!(0, 1); (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0)); (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%; statsTeam!(15)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1); teamStats!(1, 2); teamStats!(1, 2) / teamStats!(1, 1); teamStats!(1, 2) / totalGames%; statsOpp!(15)
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1) - sackStats!(1, 0); teamStats!(1, 2) + sackStats!(1, 1); (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0)); (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%; statsOpp!(15)
    End If

    Print #1,
    Print #1, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5
        If playerStats!(I1, 6) <> 0 Then
            Print #1, wrName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; playerStats!(I1, 6); playerStats!(I1, 7); playerStats!(I1, 7) / playerStats!(I1, 6); playerStats!(I1, 7) / totalGames%; playerStats!(I1, 8);
            Print #1, Using "###"; statsLRec!(I1 + 10);
            Print #1, statsLRec$(I1 + 10, 0); Tab(64); statsLRec$(I1 + 10, 1)
        End If
    Next I1

    For I1 = 0 To 9
        If playerStats!(I1, 3) <> 0 Then
            Print #1, rbName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; playerStats!(I1, 3); playerStats!(I1, 4); playerStats!(I1, 4) / playerStats!(I1, 3); playerStats!(I1, 4) / totalGames%; playerStats!(I1, 5);
            Print #1, Using "###"; statsLRec!(I1);
            Print #1, statsLRec$(I1, 0); Tab(64); statsLRec$(I1, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);
    Print #1, Using "####  ####   "; teamStats!(0, 4); teamStats!(0, 6);

    If teamStats!(0, 4) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    End If

    Print #1, Using "      ###.#    ##"; teamStats!(0, 6) / totalGames%; statsTeam!(17)
    Print #1, "OPPONENT TOTALS"; Tab(18);
    Print #1, Using "####  ####   "; teamStats!(1, 4); teamStats!(1, 6);

    If teamStats!(1, 4) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4);
    End If

    Print #1, Using "      ###.#    ##"; teamStats!(1, 6) / totalGames%; statsOpp!(17)

    Print #1,
    Print #1, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3

        If playerStats!(I1, 9) <> 0 Then

            Print #1, qbName$(I1); Tab(18); Using "####  ### ###.# #### ##.#"; playerStats!(I1, 9); playerStats!(I1, 10); playerStats!(I1, 10) / playerStats!(I1, 9) * 100; playerStats!(I1, 12); playerStats!(I1, 12) / playerStats!(I1, 9);

            If playerStats!(I1, 10) = 0 Then
                Print #1, "   --";
            Else
                Print #1, Using " ##.#"; playerStats!(I1, 12) / playerStats!(I1, 10);
            End If

            Print #1, Using " ###.# ### ### ###.#     "; playerStats!(I1, 12) / totalGames%; playerStats!(I1, 13); playerStats!(I1, 11); playerStats!(I1, 11) / playerStats!(I1, 9) * 100;

            awayTies! = QBRating!(I1, statFormat)

            Print #1, Using "###.#"; awayTies!

        End If

    Next I1

    Print #1, teamName$; Tab(18);
    Print #1, Using "####  ### "; teamStats!(0, 3); teamStats!(0, 4);

    If teamStats!(0, 3) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;
    End If

    Print #1, Using "#### "; teamStats!(0, 6);

    If teamStats!(0, 3) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    End If
    If teamStats!(0, 4) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using " ##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    End If

    Print #1, Using " ###.# ### ### "; teamStats!(0, 6) / totalGames%; statsTeam!(17); teamStats!(0, 5);

    If teamStats!(0, 3) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using "###.#     "; teamStats!(0, 5) / teamStats!(0, 3) * 100
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);
    Print #1, Using "####  ### "; teamStats!(1, 3); teamStats!(1, 4);

    If teamStats!(1, 3) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100;
    End If

    Print #1, Using "#### "; teamStats!(1, 6);

    If teamStats!(1, 3) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3);
    End If
    If teamStats!(1, 4) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using " ##.#"; teamStats!(1, 6) / teamStats!(1, 4);
    End If

    Print #1, Using " ###.# ### ### "; teamStats!(1, 6) / totalGames%; statsOpp!(17); teamStats!(1, 5);

    If teamStats!(1, 3) = 0 Then
        Print #1, "   --"
    Else
        Print #1, Using "###.#     "; teamStats!(1, 5) / teamStats!(1, 3) * 100
    End If

    Print #1,
    Print #1,
    Print #1, "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If playerStats!(I1, 9) <> 0 Then
            Print #1, qbName$(I1); Tab(22); Using "###"; statsLPass!(I1);
            Print #1, statsLPass$(I1, 0); Tab(32); statsLPass$(I1, 1)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If playerStats!(I1, 14) <> 0 Then
            Print #1, krName$(I1); Tab(18);
            Print #1, Using "####  ####   "; playerStats!(I1, 14); playerStats!(I1, 15);
            If playerStats!(I1, 14) = 0 Then
                Print #1, "  --";
            Else
                Print #1, Using "##.#"; playerStats!(I1, 15) / playerStats!(I1, 14);
            End If
            Print #1, Using "      ###.#    ##    "; playerStats!(I1, 15) / totalGames%; playerStats!(I1, 16);
            Print #1, Using "###"; statsLKR!(I1);: Print #1, statsLKR$(I1, 0); Tab(64); statsLKR$(I1, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);

    If statsTeam!(22) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsTeam!(22); statsTeam!(23); statsTeam!(23) / statsTeam!(22); statsTeam!(23) / totalGames%; statsTeam!(18)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);

    If statsOpp!(22) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsOpp!(22); statsOpp!(23); statsOpp!(23) / statsOpp!(22); statsOpp!(23) / totalGames%; statsOpp!(18)
    End If

    Print #1,
    Print #1,
    Print #1,
    Print #1, "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 3 To 5
        If playerStats!(I1, 14) <> 0 Then
            Print #1, prName$(I1 - 3); Tab(18);
            Print #1, Using "####  ####   "; playerStats!(I1, 14); playerStats!(I1, 15);
            If playerStats!(I1, 14) = 0 Then
                Print #1, "  --";
            Else
                Print #1, Using "##.#"; playerStats!(I1, 15) / playerStats!(I1, 14);
            End If
            Print #1, Using "      ###.#    ##    "; playerStats!(I1, 15) / totalGames%; playerStats!(I1, 16);
            Print #1, Using "###"; statsLPR!(I1 - 3);: Print #1, statsLPR$(I1 - 3, 0); Tab(64); statsLPR$(I1 - 3, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);

    If statsTeam!(24) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsTeam!(24); statsTeam!(25); statsTeam!(25) / statsTeam!(24); statsTeam!(25) / totalGames%; statsTeam!(19)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);

    If statsOpp!(24) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); statsOpp!(25) / totalGames%; statsOpp!(19)
    End If

    Print #1,

    I = 0

    Print #1, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Print #1, punterName$(0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; teamStats!(0, 7); teamStats!(0, 8); teamStats!(0, 8) / teamStats!(0, 7); teamStats!(0, 7) / totalGames%; statsTeam!(31); statsTeam!(30); statsTeam!(28)
    Print #1, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; teamStats!(1, 7); teamStats!(1, 8); teamStats!(1, 8) / teamStats!(1, 7); teamStats!(1, 7) / totalGames%; statsOpp!(31); statsOpp!(30)
    Print #1,
    Print #1,
    Print #1,
    Print #1, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Print #1, teamName$; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; teamStats!(0, 7); teamStats!(0, 8); teamStats!(0, 8) / teamStats!(0, 7); statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); (teamStats!(0, 8) - statsOpp!(25)) / teamStats!(0, 7)
    Print #1, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; teamStats!(1, 7); teamStats!(1, 8); teamStats!(1, 8) / teamStats!(1, 7); statsTeam!(24); statsTeam!(25); statsTeam!(25) / statsTeam!(24); (teamStats!(1, 8) - statsTeam!(25)) / teamStats!(1, 7)
    Print #1,
    Print #1,
    Print #1,
    Print #1, "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

    If playerStats!(4, 17) <> 0 Or playerStats!(6, 17) <> 0 Then
        Print #1, pkName$(0);
        Print #1, Tab(16);
        If playerStats!(6, 17) <> 0 Then
            Print #1, Using "### ### ###.# "; playerStats!(5, 17); playerStats!(6, 17); playerStats!(5, 17) / playerStats!(6, 17) * 100;
        End If
        If playerStats!(4, 17) = 0 Then
            Print #1,
        Else
            Print #1, Tab(30);
            Print #1, Using "### ### ###.# ## ##"; playerStats!(3, 17); playerStats!(4, 17); playerStats!(3, 17) / playerStats!(4, 17) * 100; statsLFG!(0); statsFGM!(0, 0);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 0); statsFGM!(0, 1);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 1); statsFGM!(0, 2);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 2); statsFGM!(0, 3);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 3); statsFGM!(0, 4);
            Print #1, "-";: Print #1, Using "##"; statsFGA!(0, 4)
        End If
    End If

    If playerStats!(8, 17) <> 0 Or playerStats!(10, 17) <> 0 Then
        Print #1, pkName$(1); Tab(16);
        If playerStats!(10, 17) <> 0 Then
            Print #1, Using "### ### ###.# "; playerStats!(9, 17); playerStats!(10, 17); playerStats!(9, 17) / playerStats!(10, 17) * 100;
        End If
        If playerStats!(8, 17) = 0 Then
            Print #1,
        Else
            Print #1, Tab(30);
            Print #1, Using "### ### ###.# ## ##"; playerStats!(7, 17); playerStats!(8, 17); playerStats!(7, 17) / playerStats!(8, 17) * 100; statsLFG!(1); statsFGM!(1, 0);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 0); statsFGM!(1, 1);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 1); statsFGM!(1, 2);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 2); statsFGM!(1, 3);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 3); statsFGM!(1, 4);
            Print #1, "-";: Print #1, Using "##"; statsFGA!(1, 4)
        End If
    End If

    Print #1,
    Print #1,
    Print #1, "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 0 To 9
        If statsGI(I1, 0) <> 0 Then
            Print #1, intName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; statsGI(I1, 0); statsGI(I1, 1); statsGI(I1, 1) / statsGI(I1, 0); statsGI(I1, 1) / totalGames%; statsGI(I1, 2);
            Print #1, Using "###"; statsLInt!(I1);: Print #1, statsLInt$(I1, 0); Tab(64); statsLInt$(I1, 1)
        End If
    Next I1

    Print #1,
    Print #1, "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If statsGS(I1, 0) <> 0 Then
            Print #1, sackName$(I1); Tab(20); Using "###            ###"; statsGS(I1, 0); statsGS(I1, 1)
        End If
    Next

    Close #1

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with
'a choice to reset stats. If they chose,
'and confirm, it will then proceed to
'delete the files.
Sub ResetStatPrompt (statFile$)

    Cls

    Call MenuBox(6, 8, 61, 1, 15, 1)

    Do
        Locate 7
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        Call MenuBox(6, 8, 61, 1, 15, 1)

        Do

            Locate 7
            U$ = "ARE YOU SURE? (Y/N)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$

        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then
            Kill diskPaths$(1) + statFile$
            Kill diskPaths$(1) + statFile$ + "S"
            If _FileExists(diskPaths$(1) + statFile$ + "R") Then Kill diskPaths$(1) + statFile$ + "R"
        End If

        'Loop Until UCase$(I$) = "N"

    End If 'Done checking if reset stats

End Sub


'$Include: 'MERGE.BAS'
'$Include: 'NEWLDR.BAS'
'$Include: 'REC.BAS'
'Include: 'SCHEDULE.BAS'


'----------------------------------------
'          SCHEDULE Subroutine
'----------------------------------------
'This subroutine gives the user several
'options for interfacing with the Scheduler
'and also user boxscores generated by
'auto-played games
Sub SCHEDULE ()

    Dim scheduleFile$
    ReDim menuChoices$(1 To 8)

    Shared NS%

    Shared scheduleAP%(), scheduleNG%()

    _Title "3-in-1 Football - Scheduler"

    'For I = 1 To 1500
    '    scheduleNG%(I, 13) = 0
    'Next

    Do

        Color 7, 0
        Cls

        Call TITLE_SCHEDULER

        menuChoices$(1) = "ENTER A NEW SCHEDULE              "
        menuChoices$(2) = "PLAY / EDIT AN EXISTING SCHEDULE  "
        menuChoices$(3) = "EXTRACT TEAM SCHEDULE             "
        menuChoices$(4) = "EXTRACT CONFERENCE SCHEDULE       "
        menuChoices$(5) = "CHANGE TEAM CONTROL FOR A SCHEDULE"
        menuChoices$(6) = "MAKE GLOBAL CHANGES TO A SCHEDULE "
        menuChoices$(7) = "BOXSCORE MAINTENANCE / PARAMETERS "
        menuChoices$(8) = "RETURN TO 3 IN 1 FB MENU          "
        SELECTION% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            'Assign full array sizes for schedule games
            ReDim homeScores(MAX_SCHEDULE_GAMES)
            ReDim visitorScores(MAX_SCHEDULE_GAMES)
            ReDim scheduleH$(MAX_SCHEDULE_GAMES)
            ReDim scheduleV$(MAX_SCHEDULE_GAMES)
            ReDim scheduleQB_V$(MAX_SCHEDULE_GAMES)
            ReDim scheduleQB_H$(MAX_SCHEDULE_GAMES)

            scheduleNG%(0, 2) = 1
            scheduleNG%(0, 3) = 1
            scheduleNG%(0, 7) = 11
            scheduleNG%(0, 10) = 3
            scheduleNG%(0, 11) = 8
            scheduleNG%(0, 14) = 1
            scheduleNG%(0, 19) = 3
            scheduleNG%(0, 20) = 3

            Select Case SELECTION%

                Case 1:
                    'number of games in schedule
                    scheduleAP%(0) = 0
                    NS% = 0

                    scheduleFile$ = _InputBox$("Schedule File Name", "Enter name for schedule file")

                    If scheduleFile$ <> "" Then
                        Call NewSchedule(scheduleFile$)
                    End If

                Case 2 To 6:

                    Do

                        'Have the user pick a schedule
                        scheduleFile$ = ChooseFile$(diskPaths$(3) + "*.SCD", "SCHEDULE", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then

                            Call LoadScheduleInfo(scheduleFile$)

                            Cls

                            'Extract / Change / Global
                            If SELECTION% >= 3 And SELECTION% <= 6 Then
                                Call ExtractSchedule(SELECTION%, scheduleFile$)
                                If SELECTION% >= 4 Then Call SaveScheduleFile(scheduleFile$)
                            Else
                                'Retrieve / Edit / Play Schedule
                                Color 15, 0
                                Call NewSchedule(scheduleFile$)
                            End If

                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 7:
                    Call BoxscoreMaintenance

                Case 8:
                    Cls

            End Select

        End If

    Loop Until SELECTION% = 8 Or ExitCode% = 27

    'Free up memory for rest of program
    ReDim homeScores(1)
    ReDim visitorScores(1)
    ReDim scheduleH$(1)
    ReDim scheduleV$(1)

End Sub


'----------------------------------------
'       NewSchedule SubRoutine
'----------------------------------------
'This subroutine facilitates the user
'creation of a schedule. It provides all
'options required and displays the schedule
'while editing
Sub NewSchedule (scheduleFile$)

    Shared BS%, NS%

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleH$(), scheduleV$()
    Shared scheduleQB_V$(), scheduleQB_H$()
    Shared scheduleYN$()

    Shared modeAbbrev$()
    Shared qbName$()

    qbName$(4) = "RANDOM"

    modeAbbrev$(0) = "HH"
    modeAbbrev$(1) = "C1"
    modeAbbrev$(2) = "C2"
    modeAbbrev$(3) = "CC"

    scheduleYN$(0, 0) = ""
    scheduleYN$(0, 1) = ""

    Do

        Color 15, 0
        Cls

        Locate 23, 1
        Color , 1: Print Tab(80); " ";
        Locate 23, 1
        Color 13: Print "HF";
        Color 14: Print "-HOME FIELD ADV  ";
        Color 13: Print "MA";
        Color 14: Print "-MODE OF PLAY  ";
        Color 11: Print " G";
        Color 14: Print "-GOTO GAME";
        Color 11: Print " B";
        Color 14: Print "-BOXSCORE OPTIONS"

        Locate 24, 1
        Color , 1: Print Tab(80); " ";
        Locate 24, 1
        Color 11: Print "C";
        Color 14: Print "-SAVE AND CONTINUE  ";
        Color 11: Print "A";
        Color 14: Print "-ADD A GAME  ";
        Color 11: Print "D";
        Color 14: Print "-DELETE A GAME  ";
        Color 11: Print "E";
        Color 14: Print "-EDIT A GAME  ";
        Color 11: Print "PGUP  PGDN";

        Locate 25, 1
        Color , 1: Print Tab(80); " ";
        Locate 25, 1
        Color 11: Print "S";
        Color 14: Print "-SORT GAMES  ";
        Color 11: Print "P";
        Color 14: Print "-PLAY GAMES   ";
        Color 11: Print "X";
        Color 14: Print "-SAVE AND EXIT  ";
        Color 11: Print "Q";
        Color 14: Print "-QUIT  ";
        Color 11: Print "R";
        Color 14: Print "-RESET SCORES";

        Locate 1, 1: Print Tab(80); " ";
        Locate 1, 1: Print scheduleFile$
        Locate 1, 33

        Color 14: Print "AUTO SCHEDULER"

        Color 15, 0

        Locate 2, 1
        Color 14
        Print "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  HF"
        Color 15

        'Show 20 schedule entries on the screen at once
        For X = 1 To 20

            Locate X + 2, 1

            If X + NS% * 20 <= scheduleAP%(0) Then

                T1 = scheduleNG%(X + NS% * 20, 0)
                YN$ = scheduleYN$(X + NS% * 20, 0)
                Call ReadTeam(YN$, team$, T1)
                Call ReadExtraData(YN$, T1, validData)

                If validData = 1 Then

                    scheduleV$(X + NS% * 20) = team$
                    scheduleQB_V$(X + NS% * 20) = qbName$(scheduleNG%(X + NS% * 20, 5))

                    T1 = scheduleNG%(X + NS% * 20, 1)
                    YN$ = scheduleYN$(X + NS% * 20, 1)
                    Call ReadTeam(YN$, team$, T1)
                    Call ReadExtraData(YN$, T1, validData)

                    If validData = 1 Then

                        scheduleH$(X + NS% * 20) = team$
                        scheduleQB_H$(X + NS% * 20) = qbName$(scheduleNG%(X + NS% * 20, 8))

                        'Game number
                        Print Using "####  "; X + NS% * 20;

                        'Date
                        Print Using "##/##/##"; scheduleNG%(X + NS% * 20, 17); scheduleNG%(X + NS% * 20, 16); scheduleNG%(X + NS% * 20, 18);

                        'Time
                        Locate , 17
                        I$ = "A"
                        HR% = scheduleNG%(X + NS% * 20, 15) / 100
                        MIN% = scheduleNG%(X + NS% * 20, 15) - HR% * 100

                        If HR% = 12 Then I$ = "P"
                        If HR% > 12 Then HR% = HR% - 12: I$ = "P"

                        leagLdrTeams$ = Right$(Str$(HR%), 2) + ":"

                        If MIN% < 10 Then leagLdrTeams$ = leagLdrTeams$ + "0" + Right$(Str$(MIN%), 1)
                        If MIN% >= 10 Then leagLdrTeams$ = leagLdrTeams$ + Right$(Str$(MIN%), 2)

                        'Time
                        leagLdrTeams$ = leagLdrTeams$ + I$
                        Print leagLdrTeams$;

                        'Visiting Team
                        Locate , 25: Print scheduleV$(X + NS% * 20); " ";
                        If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; visitorScores(X + NS% * 20);

                        'Home Team
                        Locate , 46: Print scheduleH$(X + NS% * 20); " ";
                        If scheduleNG%(X + NS% * 20, 13) = 1 Then Print Using "###"; homeScores(X + NS% * 20);

                        'Mode of Play
                        Locate , 67: Print modeAbbrev$(scheduleNG%(X + NS% * 20, 10));

                        'Home Field
                        Locate , 72: If scheduleNG%(X + NS% * 20, 2) = 1 Then Print "Y" Else Print "N"

                    End If

                End If

                If validData = 0 Then
                    'Invalid team file
                    Locate 9,
                    Call CSMsgBox("THERE WAS AN ISSUE READING TEAM DATA. CHECK THE TEAM FILE FOR ANY ERRORS... PRESS ANY KEY AND TRY ANOTHER SCHEDULE", 40, PASS)
                    K$ = GetKeyPress$
                    Call CSMsgBox("", 0, PASS)
                    Exit Sub
                End If

            End If

        Next

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "A":
                ' *** ADD A GAME TO THE SCHEDULE ***
                V$ = ""
                H$ = ""
                QV$ = ""
                QH$ = ""

                'number of games in schedule
                scheduleAP%(0) = scheduleAP%(0) + 1
                targetGame% = scheduleAP%(0)

                BS% = Int((targetGame% - 1) / 20)
                If NS% < BS% Then NS% = BS%

                For X = 0 To 20
                    If X <> 13 Then
                        scheduleNG%(targetGame%, X) = scheduleNG%(targetGame% - 1, X)
                    End If
                Next

                For X = 0 To 1:
                    scheduleYN$(targetGame%, X) = scheduleYN$(targetGame% - 1, X)
                Next

                DT$ = "10-01-91"
                leagLdrTeams$ = "07:30P"
                Call NewGame(V$, QV$, H$, QH$, YN$, DT$, leagLdrTeams$, NTMS, targetGame%)

            Case "E":
                Call EditGame(DT$, leagLdrTeams$)

            Case "B":
                Do
                    Locate 23, 1: Print "ENTER GAME # TO GO TO        "
                    Locate 23, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)
                NS% = Int((targetGame% - 1) / 20)

                'boxName$ = diskPaths$(1) + "BOX" + Mid$(Str$(targetGame%), 2, Len(Str$(targetGame%)))
                boxName$ = diskPaths$(1) + scheduleFile$ + "_GAME" + Mid$(Str$(targetGame%), 2, Len(Str$(targetGame%))) + ".BS"

                Call EditBoxscore(boxName$)
                Color 15, 0

            Case "D":
                ' *** DELETE A GAME FROM THE SCHEDULE ***
                Do
                    Locate 24, 1: Input "ENTER GAME # TO DELETE "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

                For X = targetGame% To scheduleAP%(0) - 1
                    For X1 = 0 To 20
                        scheduleNG%(X, X1) = scheduleNG%(X + 1, X1)
                    Next

                    visitorScores(X) = visitorScores(X + 1)
                    homeScores(X) = homeScores(X + 1)

                    For X1 = 0 To 1
                        scheduleYN$(X, X1) = scheduleYN$(X + 1, X1)
                    Next
                Next

                scheduleAP%(0) = scheduleAP%(0) - 1
                BS% = Int((scheduleAP%(0) - 1) / 20)
                If NS% > BS% Then NS% = BS%

            Case "G":
                'GOTO A SPECIFIC PAGE
                Do
                    Locate 23, 1: Print "ENTER GAME # TO GO TO        "
                    Locate 23, 1: Input "ENTER GAME # TO GO TO "; targetGame%
                Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

                NS% = Int((targetGame% - 1) / 20)

            Case "S":
                Call SortSchedule

            Case "C", "X":
                Call SaveScheduleFile(scheduleFile$)

            Case "Q", Chr$(27): '

            Case "P":
                Call SaveScheduleFile(scheduleFile$)

                ' *** PLAY GAMES ***
                Color , 0
                Cls

                Call MenuBox(6, 8, 61, 8, 15, 4)

                Do

                    Locate 8, 12: Print "TOTAL GAMES ="; scheduleAP%(0)
                    Locate 9, 12: Print "SELECT GAME # TO START AND END WITH": Print
                    Locate 10, 12: Input "ENTER START GAME #: "; startGame: Print
                    Locate 11, 12: Input "ENTER END GAME #:"; endGame: Print

                    Do
                        Locate 13, 12: Print "ANY CHANGE? (Y/N)"
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

                Loop Until UCase$(I$) = "N"

                If startGame <= 0 Then startGame = 1
                If endGame > scheduleAP%(0) Then endGame = scheduleAP%(0)

                Open "GAME82" For Output As #1

                Print #1, scheduleFile$ + ".SCD"
                Print #1, startGame 'Start Game
                Print #1, endGame 'End Game
                Print #1, 0

                Close #1

                For I = 1 To scheduleAP%(0)
                    HR% = Int(scheduleNG%(I, 15) / 100)
                    MIN% = scheduleNG%(I, 15) - HR% * 100
                    TM% = HR% * 60 + MIN%
                    scheduleNG%(I, 12) = Int(TM% / 30) - 18
                Next

                Open "TICKER" For Output As #2

                For I = 1 To 32:
                    For I1 = 0 To 1:
                        For I2 = 1 To 14
                            Print #2, -1
                        Next
                    Next
                Next

                For I = 1 To 32:
                    For I1 = 0 To 1:
                        Print #2, "XXX"
                    Next
                Next

                For I = startGame To startGame + 31
                    Print #2, scheduleNG%(I, 12)
                Next

                Close #2

                'Free up used memory so games can load faster
                ReDim scheduleNG%(1, 20)
                ReDim scheduleYN$(1, 1)
                ReDim homeScores(1)
                ReDim visitorScores(1)
                ReDim scheduleH$(1)
                ReDim scheduleV$(1)
                ReDim scheduleQB_V$(1)
                ReDim scheduleQB_H$(1)

                Call FOOTBALL

                'Restore memory so we can populate again
                ReDim scheduleNG%(MAX_SCHEDULE_GAMES, 20)
                ReDim scheduleYN$(MAX_SCHEDULE_GAMES, 1)
                ReDim homeScores(MAX_SCHEDULE_GAMES)
                ReDim visitorScores(MAX_SCHEDULE_GAMES)
                ReDim scheduleH$(MAX_SCHEDULE_GAMES)
                ReDim scheduleV$(MAX_SCHEDULE_GAMES)
                ReDim scheduleQB_V$(MAX_SCHEDULE_GAMES)
                ReDim scheduleQB_H$(MAX_SCHEDULE_GAMES)

                Call LoadScheduleInfo(scheduleFile$)

            Case "R":
                ' *** RESET GAMES ***
                For I = 1 To scheduleAP%(0)
                    scheduleNG%(I, 13) = 0
                Next

            Case Chr$(0) + Chr$(81), Chr$(0) + Chr$(73)
                ' *** PAGE DOWN | RIGHT ARROW ***
                If NS% >= BS% Then
                    NS% = BS%
                Else
                    NS% = NS% + 1
                End If

            Case Chr$(0) + Chr$(73), Chr$(0) + Chr$(75)
                ' *** PAGE UP | LEFT ARROW ***
                If NS% <> 0 Then
                    NS% = NS% - 1
                End If

        End Select

    Loop Until UCase$(I$) = "Q" Or UCase$(I$) = "X"

End Sub


'----------------------------------------
'          EditGame SubRoutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for choosing games to edit, then
'calls the NewGame routine to essentially
'display the existing parameters and allow
'them to be modified
Sub EditGame (DT$, leagLdrTeams$)

    Shared scheduleAP%()
    Shared scheduleH$(), scheduleV$()
    Shared scheduleQB_H$(), scheduleQB_V$()
    Shared scheduleYN$()
    Shared scheduleNG%()

    ' *** EDIT A GAME ***
    Do
        Locate 23, 1: Print "ENTER GAME # TO EDIT        "
        Locate 23, 1: Input "ENTER GAME # TO EDIT "; targetGame%
    Loop Until targetGame% >= 1 And targetGame% <= scheduleAP%(0)

    V$ = scheduleV$(targetGame%)
    H$ = scheduleH$(targetGame%)
    YN$ = scheduleYN$(targetGame%, 0)
    QV$ = scheduleQB_V$(targetGame%)
    QH$ = scheduleQB_H$(targetGame%)

    HR% = Int(scheduleNG%(targetGame%, 15) / 100)
    MIN% = scheduleNG%(targetGame%, 15) - HR% * 100

    If HR% >= 12 Then
        I$ = "P"
        If HR% > 12 Then HR% = HR% - 12
    Else
        I$ = "A"
    End If

    leagLdrTeams$ = Right$(Str$(HR%), 2) + ":"

    If MIN% < 10 Then
        leagLdrTeams$ = leagLdrTeams$ + "0" + Right$(Str$(MIN%), 1)
    Else
        leagLdrTeams$ = leagLdrTeams$ + Right$(Str$(MIN%), 2)
    End If

    leagLdrTeams$ = leagLdrTeams$ + I$
    DT$ = Right$(Str$(scheduleNG%(targetGame%, 17)), 2) + "/"
    DYMO% = Val(Right$(Str$(scheduleNG%(targetGame%, 16)), 2))

    If DYMO% < 10 Then
        DYMO$ = "0" + Right$(Str$(scheduleNG%(targetGame%, 16)), 1)
    Else
        DYMO$ = Right$(Str$(scheduleNG%(targetGame%, 16)), 2)
    End If

    DT$ = DT$ + DYMO$ + "/" + Right$(Str$(scheduleNG%(targetGame%, 18)), 2)

    Call NewGame(V$, QBV$, H$, QBH$, YN$, DT$, leagLdrTeams$, NTMS, targetGame%)

End Sub

'----------------------------------------
'           NewGame SubRoutine
'----------------------------------------
'This subroutine gives the user an interface
'for editing and choosing options for the
'specified game in the schedule
Sub NewGame (visitor$, visitorQB$, home$, homeQB$, YN$, DT$, leagLdrTeams$, NTMS, gameIndex%)

    Shared scheduleNG%(), teamRatings%()

    Shared scheduleH$(), scheduleV$()
    Shared scheduleQB_V$(), scheduleQB_H$()
    Shared scheduleYN$()

    Shared location$(), modeAbbrev$(), overtime$()
    Shared playMode$(), qbName$()
    Shared rulesType$(), ruleYrPro$(), ruleYrColl$()
    Shared weather$(), windSetting$(), yesNoText$()

    Dim displayPause!

    scheduleYN$(0, 0) = ""
    scheduleYN$(0, 1) = ""

    qbName$(4) = "RANDOM"

    Do

        ' ** Enter New Game **
        Color 15, 0
        Cls

        Color 2
        Print Tab(29); "SELECT OPENING OPTIONS"

        Locate 2, 2: Color 14
        Print "(1) ";
        Color 15: Print playMode$(scheduleNG%(gameIndex%, 10))
        Locate 3, 2: Color 14
        Print "(2) ";
        Color 15: Print "VISITING TEAM / QB: "; visitor$; " / "; visitorQB$
        Locate 4, 2: Color 14
        Print "(3) ";
        Color 15: Print "HOME TEAM / QB: "; home$; " / "; homeQB$
        Locate 5, 2: Color 14
        Print "(4) ";
        Color 15: Print "GAME DATE (XX-XX-XX)  "; DT$
        Locate 6, 2: Color 14
        Print "(5) ";
        Color 15: Print "GAME TIME (XX:XXP)    "; leagLdrTeams$
        Locate 7, 2: Color 14
        Print "(6) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; scheduleNG%(gameIndex%, 4) / 10
        Locate 8, 2: Color 14
        Print "(7) ";
        Color 15: Print "GAME LOCATION: "; location$(scheduleNG%(gameIndex%, 2))
        Locate 9, 2: Color 14
        Print "(8) ";
        Color 15: Print "TYPE OF GAME: "; rulesType$(scheduleNG%(gameIndex%, 3))
        Locate 10, 2: Color 14
        Print "(9) ";
        Color 15: Print "PRO RULES: "; ruleYrPro$(scheduleNG%(gameIndex%, 11))
        Locate 11, 2: Color 14
        Print "(A) ";
        Color 15: Print "COLLEGE RULES: "; ruleYrColl$(scheduleNG%(gameIndex%, 7))
        Locate 12, 2: Color 14
        Print "(B) ";
        Color 15: Print "OVERTIME: "; overtime$(scheduleNG%(gameIndex%, 14))
        Locate 13, 2: Color 14
        Print "(C) ";
        Color 15: Print "WIND: "; windSetting$(scheduleNG%(gameIndex%, 19))
        Locate 14, 2: Color 14
        Print "(D) ";
        Color 15: Print "WEATHER: "; weather$(scheduleNG%(gameIndex%, 20))
        Locate 16, 2: Color 14
        Print "(0) ";
        Color 15: Print "ACCEPT SETTINGS"

        Locate 1, 1

        Do

            I$ = GetKeyPress$

            Call LetterToNumber(I$, setting)

            Select Case setting

                Case 1:
                    scheduleNG%(gameIndex%, 10) = scheduleNG%(gameIndex%, 10) + 1
                    If scheduleNG%(gameIndex%, 10) > 3 Then scheduleNG%(gameIndex%, 10) = 0

                Case 2, 3:

                    teamNbr = 0
                    Call ChooseTeam(YN$, NTMS, teamNbr, team$)

                    If teamNbr <> 0 Then

                        Color 15, 0
                        scheduleNG%(gameIndex%, setting - 2) = teamNbr
                        scheduleYN$(gameIndex%, setting - 2) = YN$

                        If setting = 2 Then
                            visitor$ = team$
                            scheduleV$(gameIndex%) = team$
                            visitorQB$ = GetQBName$(gameIndex%, setting)
                            scheduleQB_V$(gameIndex%) = visitorQB$
                        End If

                        If setting = 3 Then
                            home$ = team$
                            scheduleH$(gameIndex%) = team$
                            homeQB$ = GetQBName$(gameIndex%, setting)
                            scheduleQB_H$(gameIndex%) = homeQB$
                        End If

                    End If

                Case 4:
                    Locate 5, 26: Input DT$

                Case 5:
                    Locate 6, 26: Print "        "
                    Locate 6, 26: Input leagLdrTeams$

                Case 6:
                    Locate 7, 32: Input displayPause!
                    scheduleNG%(gameIndex%, 4) = displayPause! * 10

                Case 7:
                    'N = 1 - (scheduleNG%(gameIndex%, 2))
                    'scheduleNG%(gameIndex%, 2) = N
                    N = 1 - (scheduleNG%(gameIndex%, 2) - 1)
                    scheduleNG%(gameIndex%, 2) = N + 1

                Case 8:
                    scheduleNG%(gameIndex%, 3) = scheduleNG%(gameIndex%, 3) + 1
                    If scheduleNG%(gameIndex%, 3) > 5 Then scheduleNG%(gameIndex%, 3) = 0

                Case 9:
                    scheduleNG%(gameIndex%, 11) = 1 + scheduleNG%(gameIndex%, 11)
                    If scheduleNG%(gameIndex%, 11) > 3 Then scheduleNG%(gameIndex%, 11) = 0

                Case 10:
                    scheduleNG%(gameIndex%, 7) = 1 + scheduleNG%(gameIndex%, 7)
                    If scheduleNG%(gameIndex%, 7) > 5 Then scheduleNG%(gameIndex%, 7) = 0

                Case 11:
                    scheduleNG%(gameIndex%, 14) = scheduleNG%(gameIndex%, 14) + 1
                    If scheduleNG%(gameIndex%, 14) > 3 Then scheduleNG%(gameIndex%, 14) = 0

                Case 12:
                    scheduleNG%(gameIndex%, 19) = scheduleNG%(gameIndex%, 19) + 1
                    If scheduleNG%(gameIndex%, 19) > 3 Then scheduleNG%(gameIndex%, 19) = 0

                Case 13:
                    scheduleNG%(gameIndex%, 20) = scheduleNG%(gameIndex%, 20) + 1
                    If scheduleNG%(gameIndex%, 20) > 3 Then scheduleNG%(gameIndex%, 20) = 0

            End Select

        Loop Until (setting >= 0 And setting <= 13)

        If visitor$ <> "" And home$ <> "" And leagLdrTeams$ <> "" And DT$ <> "" Then

            meridian$ = Right$(leagLdrTeams$, 1)
            fullTime$ = Left$(leagLdrTeams$, Len(leagLdrTeams$) - 1)
            MIN% = Val(Right$(fullTime$, 2))
            hourString$ = Left$(fullTime$, Len(fullTime$) - 3)
            HR% = Val(hourString$)

            If UCase$(meridian$) = "P" And HR% < 12 Then
                'If user typed in PM, we are storing as military time
                'We don't check that military time may have been used
                HR% = HR% + 12
            End If

            'Gane Time
            scheduleNG%(gameIndex%, 15) = HR% * 100 + MIN%

            'Month
            scheduleNG%(gameIndex%, 17) = Val(Left$(DT$, 2))

            'Day
            scheduleNG%(gameIndex%, 16) = Val(Mid$((DT$), 4, 2))

            'Year
            scheduleNG%(gameIndex%, 18) = Val(Mid$((DT$), 7, 2))

        End If

    Loop Until setting = 0

End Sub

'----------------------------------------
'       ExtractSchedule SubRoutine
'----------------------------------------
'This is the core subroutine to leverage
'when mass changes are needing to be made
'to a schedule. It essentially reads in
'the existing schedule parameters, prompts
'the user for the appropriate changes, and
'then writes the changes back to the orig-
'inal schedule file.
Sub ExtractSchedule (scheduleAction%, scheduleFile$)
    'scheduleAction% = 3: EXTRACT TEAM'S SCHEDULE FROM LEAGUE SCHEDULE
    'scheduleAction% = 4: EXTRACT CONFERENCE SCHEDULE
    'scheduleAction% = 5: CHANGE THE MODE OF PLAY FOR A TEAM
    'scheduleAction% = 6: MAKE GLOBAL CHANGES TO A SCHEDULE

    'EXTRACT A SCHEDULE FROM ANOTHER
    Dim displayPause!

    Shared homeScores(), visitorScores()

    Shared scheduleAP%()
    Shared scheduleNG%(), teamRatings%()
    Shared scheduleYN$()

    Shared memberIdx()
    Shared memberName$(), memberYear$()

    Select Case scheduleAction%

        Case 6
            Call ChangeGlobalSettings
            Call SaveScheduleFile(scheduleFile$)
            Call NewSchedule(scheduleFile$)

        Case 3, 4, 5

            If scheduleAction% = 3 Or scheduleAction% = 4 Then
                'Pick name of target file
                subSchedFile$ = _InputBox$("Schedule File Name", "Enter name for schedule file")
            End If

            If scheduleAction% = 4 And subSchedFile$ <> "" Then
                'Pick conference to extract
                conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)

                If ExitCode% <> 27 And ExitCode% <> 0 Then

                    Open diskPaths$(2) + conferenceFile$ + ".TMI" For Input As #1

                    Input #1, statType

                    If statType = 0 Then

                        Input #1, LG$
                        Input #1, X$
                        Input #1, X$

                        For X = 1 To 30
                            Input #1, memberName$(X)
                            Input #1, memberIdx(X)
                            Input #1, memberYear$(X)
                        Next

                        numberTeams = 30

                        Close #1

                    End If

                    If statType = 1 Then

                        Input #1, LG$

                        For X = 1 To 30:
                            Input #1, memberName$(X)
                            Input #1, memberIdx(X)
                            Input #1, memberYear$(X)
                        Next

                        numberTeams = 30

                        Close #1

                    End If

                    For I = 1 To scheduleAP%(0)

                        scheduleYN$(I, 0) = RTrim$(scheduleYN$(I, 0))
                        scheduleYN$(I, 1) = RTrim$(scheduleYN$(I, 1))

                        For I1 = 1 To numberTeams

                            If memberName$(I1) <> "XXX" Then

                                If memberIdx(I1) = scheduleNG%(I, 0) And memberYear$(I1) = scheduleYN$(I, 0) Or memberIdx(I1) = scheduleNG%(I, 1) And memberYear$(I1) = scheduleYN$(I, 1) Then

                                    Print
                                    Print "FOUND A MATCH"

                                    Open diskPaths$(3) + subSchedFile$ + ".SCD" For Random As #2 Len = SCHEDULE_SIZE_BYTES

                                    fileLength& = LOF(2)
                                    numberGames = fileLength& / SCHEDULE_SIZE_BYTES

                                    For X = 0 To 20
                                        Field #2, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
                                    Next

                                    Field #2, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

                                    For X = 0 To 20:
                                        LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
                                    Next

                                    For X = 0 To 1:
                                        LSet Q$(22 + X) = scheduleYN$(I, X)
                                    Next

                                    LSet Q$(24) = MKI$(visitorScores(I))
                                    LSet Q$(25) = MKI$(homeScores(I))

                                    Put #2, numberGames + 1

                                    Close #2

                                    Exit For

                                End If

                            End If 'Done checking for "XXX"

                        Next I1

                    Next I

                End If 'Done checking COUNT

            Else

                If (scheduleAction% = 3 And subSchedFile$ = "") Then
                    'If user cancelled from name entry
                    teamNbr = 0
                Else
                    teamNbr = 0
                    Call ChooseTeam(YN$, numberTeams, teamNbr, teamName$)
                End If

                If teamNbr <> 0 Then

                    Color 15, 0
                    Cls

                    'Change mode of play
                    If scheduleAction% = 5 Then

                        Color teamRatings%(2), teamRatings%(1): Print teamName$;
                        Color 15, 0
                        Print " IS TO BE:"
                        Print
                        Color 12
                        Print "(0)";
                        Color 15
                        Print " MANUALLY CONTROLLED"
                        Print
                        Color 12
                        Print "(1)";
                        Color 15
                        Print " COMPUTER CONTROLLED"

                        Do
                            C$ = GetKeyPress$
                            I1 = Val(C$)
                        Loop Until C$ = "0" Or C$ = "1"

                        Print
                        Input "ENTER PLAY-BY-PLAY DELAY FOR THESE GAMES "; displayPause!

                    End If

                    For I = 1 To scheduleAP%(0)

                        scheduleYN$(I, 0) = RTrim$(scheduleYN$(I, 0))
                        scheduleYN$(I, 1) = RTrim$(scheduleYN$(I, 1))

                        If teamNbr = scheduleNG%(I, 0) And YN$ = scheduleYN$(I, 0) Or teamNbr = scheduleNG%(I, 1) And YN$ = scheduleYN$(I, 1) Then

                            'CHANGE SETTINGS FOR A TEAM
                            If teamNbr = scheduleNG%(I, 0) And YN$ = scheduleYN$(I, 0) Then
                                scheduleNG%(I, 10) = 2
                            End If

                            If teamNbr = scheduleNG%(I, 1) And YN$ = scheduleYN$(I, 1) Then
                                scheduleNG%(I, 10) = 1
                            End If

                            If (teamNbr = scheduleNG%(I, 0) And YN$ = scheduleYN$(I, 0) Or teamNbr = scheduleNG%(I, 1) And YN$ = scheduleYN$(I, 1)) And I1 = 1 And scheduleAction% = 5 Then
                                scheduleNG%(I, 10) = 3
                            End If

                            scheduleNG%(I, 4) = displayPause! * 10

                            'Extract only
                            If Not (scheduleAction% = 5) Then

                                Open diskPaths$(3) + subSchedFile$ + ".SCD" For Random As #3 Len = SCHEDULE_SIZE_BYTES
                                fileLength& = LOF(3)
                                totalGames = fileLength& / SCHEDULE_SIZE_BYTES

                                'Write matching games
                                Locate 1, 1: Print totalGames + 1; " MATCHES"

                                For X = 0 To 20
                                    Field #3, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
                                Next

                                Field #3, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

                                For X = 0 To 20:
                                    LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
                                Next

                                For X = 0 To 1:
                                    LSet Q$(22 + X) = scheduleYN$(I, X)
                                Next

                                LSet Q$(24) = MKI$(visitorScores(I))
                                LSet Q$(25) = MKI$(homeScores(I))

                                Put #3, totalGames + 1

                                Close #3

                            End If 'Done checking schedule action

                        End If 'Done checking for matches

                    Next 'Done looping through games

                End If

            End If

    End Select

End Sub


'----------------------------------------
'       EditParameters SubRoutine
'----------------------------------------
'This subroutine gives the user an inter-
'face for editing the parameters by which
'"Special" boxscores are created / saved.
Sub EditParameters ()

    Shared eventSettings()

    'Read in existing parameters if there are any
    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1

        For X = 1 To 13
            Input #1, eventSettings(X)
        Next

        Close #1

    End If

    Cls
    Color 14

    Print "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES"

    Color 15

    Print " 1. IF QUARTERS ARE OVER "; eventSettings(1)
    Print " 2. A TEAM'S POINTS MORE THAN "; eventSettings(2)
    Print " 3. A TEAM'S POINTS LESS THAN "; eventSettings(3)
    Print " 4. RUSHER'S YARDS MORE THAN "; eventSettings(4)
    Print " 5. RUSHER'S TDS MORE THAN "; eventSettings(5)
    Print " 6. RECEIVER'S YARDS MORE THAN "; eventSettings(6)
    Print " 7. RECEIVER'S TDS MORE THAN "; eventSettings(7)
    Print " 8. PASSER'S YARDS MORE THAN "; eventSettings(8)
    Print " 9. PASSER'S TDS MORE THAN "; eventSettings(9)
    Print "10. KICKER'S FGM MORE THAN "; eventSettings(10)

    C1 = 1

    Do

        Locate 23, 1: Print "SELECT # TO CHANGE 1-10 (0 TO CONTINUE) ";
        Input B9

        Select Case B9

            Case 1: Locate 2: Input " 1. IF QUARTERS ARE OVER "; eventSettings(1)
            Case 2: Locate 3: Input " 2. A TEAM'S POINTS MORE THAN "; eventSettings(2)
            Case 3: Locate 4: Input " 3. A TEAM'S POINTS LESS THAN "; eventSettings(3)
            Case 4: Locate 5: Input " 4. RUSHER'S YARDS MORE THAN "; eventSettings(4)
            Case 5: Locate 6: Input " 5. RUSHER'S TDS MORE THAN "; eventSettings(5)
            Case 6: Locate 7: Input " 6. RECEIVER'S YARDS MORE THAN "; eventSettings(6)
            Case 7: Locate 8: Input " 7. RECEIVER'S TDS MORE THAN "; eventSettings(7)
            Case 8: Locate 9: Input " 8. PASSER'S YARDS MORE THAN "; eventSettings(8)
            Case 9: Locate 10: Input " 9. PASSER'S TDS MORE THAN "; eventSettings(9)
            Case 10: Locate 11: Input "10. KICKER'S FGM MORE THAN "; eventSettings(10)

        End Select

    Loop Until B9 = 0 'ENTER also counts as 0

    'Save all changes
    Open "EVENTS.BOX" For Output As #2

    For X = 1 To 13
        Print #2, eventSettings(X)
    Next

    Close #2

End Sub


'----------------------------------------
'    BoxscoreMaintenance SubRoutine
'----------------------------------------
'This subroutine gives the user a menu
'for several of the options available to
'manage and maintain boxscores.
Sub BoxscoreMaintenance ()

    ReDim menuChoices$(1 To 6)

    menuChoices$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
    menuChoices$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
    menuChoices$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
    menuChoices$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
    menuChoices$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
    menuChoices$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "
    SELECTION% = 1
    Count% = 6
    Start% = 1
    ExitCode% = 6
    FileFGColor = 15: FileBGColor = 1
    HiLiteFGColor% = 0: HiLiteBGColor% = 7
    Spacing% = 1
    Row% = 8
    menuWidth% = Len(menuChoices$(1))
    Column% = (80 - menuWidth% - 2) / 2

    Do

        Color 15, 0

        Call TITLE_SCHEDULER

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call EditParameters

                Case 2:
                    boxscoreFile$ = ChooseFile$(diskPaths$(1) + "*.BSS", "BOXSCORE", ExitCode%)
                    'boxscoreFile$ = ChooseFile$(diskPaths$(1) + "*.SPC", "BOXSCORE", ExitCode%)

                    If ExitCode% <> 27 And ExitCode% <> 0 And boxscoreFile$ <> "" Then
                        boxName$ = diskPaths$(1) + boxscoreFile$
                        Call EditBoxscore(boxName$ + ".BSS")
                        'Call EditBoxscore(boxName$ + ".SPC")
                    End If

                Case 3
                    Spec$ = diskPaths$(1) + "*.BS"
                    'Spec$ = diskPaths$(1) + "BOX*."
                    Call DeleteBoxscores(SELECTION%, Spec$)

                Case 4
                    Spec$ = diskPaths$(1) + "*.BSS"
                    'Spec$ = diskPaths$(1) + "*.SPC"
                    Call DeleteBoxscores(SELECTION%, Spec$)

                Case 5:
                    Spec$ = diskPaths$(1) + "*.BS"
                    'Spec$ = diskPaths$(1) + "BOX*."
                    Call DeleteBoxscores(SELECTION%, Spec$)

                    Spec$ = diskPaths$(1) + "*.BSS"
                    'Spec$ = diskPaths$(1) + "*.SPC"
                    Call DeleteBoxscores(SELECTION%, Spec$)

            End Select

        End If

    Loop Until SELECTION% = 6 Or ExitCode% = 27

End Sub


'----------------------------------------
'       SaveScheduleFile SubRoutine
'----------------------------------------
'This subroutine writes all scheduled data,
'as stored in the relevant arrays, back to
'the target schedule file.
Sub SaveScheduleFile (saveFile$)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(diskPaths$(3) + saveFile$ + ".SCD") Then Kill diskPaths$(3) + saveFile$ + ".SCD"

    Open diskPaths$(3) + saveFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 20:
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1:
            LSet Q$(22 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(24) = MKI$(visitorScores(I))
        LSet Q$(25) = MKI$(homeScores(I))

        Put #1, I

    Next

    Close #1

End Sub


'----------------------------------------
'       SortSchedule SubRoutine
'----------------------------------------
'This subroutine is used to automatically
'sort all games in the schedule by their
'date / time.
Sub SortSchedule ()

    Dim schedDates&(MAX_SCHEDULE_GAMES)
    Dim D&, M&, Y&

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    For I = 1 To scheduleAP%(0)
        HR% = Int(scheduleNG%(I, 15) / 100)
        MIN% = scheduleNG%(I, 15) - HR% * 100
        TM% = HR% * 60 + MIN%
        scheduleNG%(I, 12) = Int(TM% / 30 - 18)
        D& = (scheduleNG%(I, 18) - 1)
        D& = D& * 365
        M& = (scheduleNG%(I, 17) - 1)
        M& = M& * 31
        Y& = scheduleNG%(I, 16)
        schedDates&(I) = (D& + M& + Y&) * 24 * 60 + TM%
    Next

    For Z = 1 To scheduleAP%(0) - 1

        For Z1 = Z + 1 To scheduleAP%(0)

            Locate 1, 1: Print Z, Z1

            If scheduleNG%(Z, 12) > scheduleNG%(Z1, 12) Then

                For I = 0 To 20:
                    Swap scheduleNG%(Z, I), scheduleNG%(Z1, I)
                Next

                For I = 0 To 1:
                    Swap scheduleYN$(Z, I), scheduleYN$(Z1, I)
                Next

                Swap schedDates&(Z), schedDates&(Z1)
                Swap visitorScores(Z), visitorScores(Z1)
                Swap homeScores(Z), homeScores(Z1)

            End If

        Next Z1

    Next Z

    For Z = 1 To scheduleAP%(0) - 1

        For Z1 = Z + 1 To scheduleAP%(0)

            If schedDates&(Z) > schedDates&(Z1) Then

                For I = 0 To 20:
                    Swap scheduleNG%(Z, I), scheduleNG%(Z1, I)
                Next

                For I = 0 To 1:
                    Swap scheduleYN$(Z, I), scheduleYN$(Z1, I)
                Next

                Swap schedDates&(Z), schedDates&(Z1)
                Swap visitorScores(Z), visitorScores(Z1)
                Swap homeScores(Z), homeScores(Z1)

            End If

        Next Z1

    Next Z

End Sub


'----------------------------------------
'    ChangeGlobalSettings SubRoutine
'----------------------------------------
'This subroutine presents the end user a
'variety of game options that can be changed
'or applied to all games in the schedule file.
'These would be game-specific options, as
'opposed to teams / dates / times.
Sub ChangeGlobalSettings ()

    Shared scheduleAP%(), scheduleNG%()

    Shared modeAbbrev$()
    Shared qbName$(), scheduleYN$()

    Shared location$(), overtime$(), playMode$()
    Shared rulesType$(), ruleYrColl$(), ruleYrPro$()
    Shared windSetting$(), weather$()

    Dim displayPause!

    scheduleYN$(0, 0) = ""
    scheduleYN$(0, 1) = ""

    ' GLOBAL SETTINGS FOR A SCHEDULE
    ' The code below would use default values based on the most recent rules
    'ruleOptType = 1
    'ruleOptColl = 11
    'ruleOptPro = 8
    'overtimeOpt = 1
    'windOpt = 3
    'weatherOpt = 3

    'QB selections
    qbName$(4) = "RANDOM"

    'This code will assign defaults based on the first game in the schedule.
    'Usually when I want to change global settings, I want to keep most
    'settings in the schedule the same.
    playerOpt = scheduleNG%(1, 10)
    gameLoc = scheduleNG%(1, 2)
    ruleOptType = scheduleNG%(1, 3)
    ruleOptPro = scheduleNG%(1, 11)
    ruleOptColl = scheduleNG%(1, 7)
    overtimeOpt = scheduleNG%(1, 14)
    displayPause! = scheduleNG%(1, 4) / 10
    windOpt = scheduleNG%(1, 19)
    weatherOpt = scheduleNG%(1, 20)

    DT$ = "09-01-23"
    leagLdrTeams$ = "07:30P"

    Do
        Color 15, 0
        Cls
        Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"

        Locate 3, 2: Color 14: Print "(1) ";
        Color 15: Print "GAME PLAY: "; playMode$(playerOpt)
        Locate 5, 2: Color 14: Print "(2) ";
        Color 15: Print "TYPE OF GAME: "; rulesType$(ruleOptType)
        Locate 6, 2: Color 14: Print "(3) ";
        Color 15: Print "PRO RULES: "; ruleYrPro$(ruleOptPro)
        Locate 7, 2: Color 14: Print "(4) ";
        Color 15: Print "COLLEGE RULES: "; ruleYrColl$(ruleOptColl)
        Locate 8, 2: Color 14: Print "(5) ";
        Color 15: Print "OVERTIME: "; overtime$(overtimeOpt)
        Locate 9, 2: Color 14: Print "(6) ";
        Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#  "; displayPause!
        Locate 10, 2: Color 14: Print "(7) ";
        Color 15: Print "WIND OPTION: "; windSetting$(windOpt)
        Locate 11, 2: Color 14: Print "(8) ";
        Color 15: Print "WEATHER OPTION: "; weather$(weatherOpt)

        Locate 13, 2: Color 14: Print "(0) ";
        Color 15: Print "ACCEPT GLOBAL SETTINGS"

        Do

            I$ = GetKeyPress$

            Call LetterToNumber(I$, iVal)

        Loop Until iVal >= 0 And iVal <= 10

        Select Case iVal

            Case 1:
                playerOpt = playerOpt + 1
                If playerOpt > 3 Then playerOpt = 0

            Case 2:
                ruleOptType = ruleOptType + 1
                If ruleOptType > 5 Then ruleOptType = 0

            Case 3:
                ruleOptPro = ruleOptPro + 1
                If ruleOptPro > 9 Then ruleOptPro = 0

            Case 4:
                ruleOptColl = ruleOptColl + 1
                If ruleOptColl > 11 Then ruleOptColl = 0

            Case 5:
                overtimeOpt = overtimeOpt + 1
                If overtimeOpt > 3 Then overtimeOpt = 0

            Case 6:
                Locate 9, 32: Input displayPause!
                'Input "DISPLAY VARIABLE "; displayPause!

            Case 7:
                windOpt = windOpt + 1
                If windOpt > 3 Then windOpt = 0

            Case 8:
                weatherOpt = weatherOpt + 1
                If weatherOpt > 3 Then weatherOpt = 0

        End Select

    Loop Until iVal = 0

    For I = 1 To scheduleAP%(0)
        scheduleNG%(I, 2) = gameLoc
        scheduleNG%(I, 3) = ruleOptType
        scheduleNG%(I, 4) = displayPause! * 10
        scheduleNG%(I, 7) = ruleOptColl
        scheduleNG%(I, 10) = playerOpt
        scheduleNG%(I, 11) = ruleOptPro
        scheduleNG%(I, 14) = overtimeOpt
        scheduleNG%(I, 19) = windOpt
        scheduleNG%(I, 20) = weatherOpt
    Next

End Sub


'----------------------------------------
'       DeleteBoxscores Subroutine
'----------------------------------------
'This subroutine prompts for confirmation
'of and proceeds to delete the specified
'types of boxscores. This is called by
'BoxScore maintenance routine.
Sub DeleteBoxscores (fileChoice%, fileSpec$)

    Spec$ = diskPaths$(0) + fileSpec$
    Count% = FileCount%(Spec$)

    If Count% = 0 Then

        Select Case fileChoice%

            Case 3:
                MsgText$ = "NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU"

            Case 4:
                MsgText$ = "NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU"

            Case 5:
                ' Exit Sub

        End Select

        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        ReDim boxFiles$(0 To Count%)

        For X = 1 To Count%
            boxFiles$(X) = Space$(12)
        Next

        boxFiles$(0) = fileSpec$
        Call ReadFile(boxFiles$())

        For X = 1 To Count%
            Kill (diskPaths$(1) + boxFiles$(X))
            Locate 12, 34: Print "DELETING "; boxFiles$(X)
        Next

        MsgText$ = "ALL BOXSCORE FILES HAVE BEEN DELETED. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    End If

End Sub


'----------------------------------------
'      LoadScheduleInfo SubRoutine
'----------------------------------------
'This subroutine opens the target SCHEDULE
'file and reads the data into corresponding
'arrays for use in the scheduler
Sub LoadScheduleInfo (targetFile$)

    Shared BS%, NS%

    Shared scheduleAP%(), scheduleNG%()
    Shared homeScores(), visitorScores()
    Shared scheduleYN$()

    Open diskPaths$(3) + targetFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES

    fileLength& = LOF(1)
    scheduleAP%(0) = fileLength& / SCHEDULE_SIZE_BYTES
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    For I = 1 To scheduleAP%(0)

        Get #1, I

        For X = 0 To 20:
            scheduleNG%(I, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            scheduleYN$(I, X) = RTrim$(Q$(22 + X))
        Next

        visitorScores(I) = CVI(Q$(24))
        homeScores(I) = CVI(Q$(25))

    Next

    Close #1

    NS% = 0

End Sub


'----------------------------------------
'       EditBoxscore SubRoutine
'----------------------------------------
'This subroutine is designed to facilitate
'the user's ability to edit, in-realtime,
'a boxscore that has been previously created
'by the game. It is essentially a DOS-based
'text editing routine, which was written in
'x86 asm. Therefore, the corresponding code
'is no longer available or useful. It is
'recommended that users simply edit their
'boxscores in their favorite text editor.
Sub EditBoxscore (boxName$)

    If Not _FileExists(boxName$) Then

        MsgText$ = "BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call QBWRITE(boxName$)

    End If

End Sub


'$Include: 'SEECON.BAS'
'$Include: 'SEENATL.BAS'
'$Include: 'SEELEAG.BAS'
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

    _Title "3-in-1 Football v" + GAME_VERSION$ + " - GRIDIRON!"

    Shared scheduleFile$
    Shared tickerStart

    Shared location$(), overtime$()
    Shared playMode$()
    Shared rulesType$(), ruleYrPro$(), ruleYrColl$()


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
    If Not _FileExists("DEFAULT") Then

        Open "DEFAULT" For Output As #3

        For I = 1 To 8:
            Print #3, 0
        Next

        Close #3

    End If

    Open "DEFAULT" For Input As #4
    Input #4, playerOpt
    Input #4, ruleOptType
    Input #4, gameLoc ' Home Field Advantage
    Input #4, pbpDelay!
    Input #4, firstDownClockOpt
    Input #4, ruleOptPro
    Input #4, ruleOptColl
    Input #4, NG%(14) 'Overtime setting
    Close #4

    If _FileExists("TICKER") Then

        Open "TICKER" For Input As #5

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
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #6

        Input #6, scheduleFile$

        For I = 0 To 2:
            Input #6, schedGame(I)
        Next

        Close #6

        currGame = schedGame(0) + schedGame(2)

        'Open "pbplog" + Str$(currGame) For Append As #11
        Open "pbplog" For Append As #11
        If Not _FileExists(diskPaths$(3) + scheduleFile$) Then

            'Notify user that game won't execute
            MsgText$ = "CAN'T FIND OR LOAD TARGET SCHEDULE.  PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, PASS)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, PASS)

            earlyExit = 1

        Else

            'Read in schedule options
            Open diskPaths$(3) + scheduleFile$ As #7 Len = SCHEDULE_SIZE_BYTES

            For X = 0 To 20
                Field #7, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
            Next

            Field #7, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

            Get #7, schedGame(0) + schedGame(2)

            For X = 0 To 20:
                NG%(X) = CVI(Q$(X + 1))
            Next

            For X = 0 To 1:
                diskIDs$(X) = Q$(22 + X)
            Next

            Close #7

            autoPlay = 1
            
        End If

    Else

        Open "pbplog" For Append As #11

    End If


    If autoPlay = 1 Then

        Call AUTOSETUP(goodTeam)

        If goodTeam = 0 Then

            'Notify user that game won't execute
            MsgText$ = "ATTEMPTED TO LOAD AN INVALID TEAM. PRESS ANY KEY TO RETURN TO MENU"
            BoxWidth = 18
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
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

            Locate 2, 2: Color 14
            Print "(1) ";
            Color 15: Print playMode$(playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 2), teamRat_GAME(0, 1): Print gameTeams$(0)
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 2), teamRat_GAME(1, 1): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "GAME LOCATION: "; location$(gameLoc)
            Locate 6, 2: Color 14
            Print "(5) ";
            Color 15: Print "TYPE OF GAME: "; rulesType$(ruleOptType)
            Locate 7, 2: Color 14
            Print "(6) ";
            Color 15: Print "PRO RULES: "; ruleYrPro$(ruleOptPro)
            Locate 8, 2: Color 14
            Print "(7) ";
            Color 15: Print "COLLEGE RULES: "; ruleYrColl$(ruleOptColl)
            Locate 9, 2: Color 14
            Print "(8) ";
            Color 15: Print "OVERTIME: "; overtime$(NG%(14))
            Locate 10, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS"
            Locate 12, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.#"; pbpDelay!

            'Locate 14, 2: Print "NOTES: CLOCK IS STOPPED AFTER A 1ST DOWN IN COLLEGE BEGINNING IN 1968"
            'Locate , 2: Print "       HOLDING IS A 15 YARD PENALTY FOR COLLEGE PRE-1981 AND PRO PRE-1974"
            'Locate , 2: Print "       1991-PRESENT, COLLEGE HOLDING MARKED FROM POINT OF INFRACTION"
            'Locate , 2: Print "       1974-77, PRO HOLDING ON A RUN IS 10 YDS AND 15 ON A PASS"
            'Locate , 2: Print "       1978-1994, ALL PRO HOLDING PENALTIES ARE 10 YARDS"
            'Locate , 2: Print "       PRE-1974, IN THE PROS, GOAL POSTS WERE ON THE GOAL LINE, THE BALL WAS"
            'Locate , 2: Print "       PLACED ON THE 20 AFTER MISSED FIELD GOALS, KICKOFFS WERE FROM THE 40"
            'Locate , 2: Print "       1992-PRESENT, COLLEGE RULES ALLOW ADVANCEMENT OF A FUMBLE"
            'Locate , 2: Print "       PRE-1984, IN COLLEGE PASS INTERFERENCE AT SPOT OF INFRACTION"
            'Locate , 2: Print "       PRO 1994-, KO FROM 35 TO 30, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"

            Locate 14, 2: Color 14
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
            Locate , 3: Print "        KO: PRE-1974, 40 YL; 1974-1993, 35 YL; 1994-2010, 30 YL; 2011+, 35 YL"

            Do

                Color 7

                I$ = GetKeyPress$

                Call LetterToNumber(I$, setting)

            Loop Until setting >= 0 And setting <= 10 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case setting

                    Case 0:
                        Open "DEFAULT" For Output As #8
                        Print #8, playerOpt '
                        Print #8, ruleOptType '
                        Print #8, gameLoc ' Home Field advantage
                        Print #8, pbpDelay! '
                        Print #8, firstDownClockOpt '
                        Print #8, ruleOptPro '
                        Print #8, ruleOptColl '
                        Print #8, NG%(14) 'overtime setting
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
                        If ruleOptType = 6 Then ruleOptType = 0

                    Case 6:
                        ruleOptPro = ruleOptPro + 1
                        If ruleOptPro > 9 Then ruleOptPro = 0

                    Case 7
                        ruleOptColl = ruleOptColl + 1
                        If ruleOptColl > 11 Then ruleOptColl = 0

                    Case 8:
                        NG%(14) = NG%(14) + 1
                        If NG%(14) > 3 Then NG%(14) = 0

                    Case 9:
                        '

                    Case 10
                        Do
                            Locate 12, 32
                            Input pbpDelay!
                        Loop Until pbpDelay! >= 0

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

        Loop Until (setting = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

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
    If earlyExit = 1 Then Exit Sub

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
        teamYears(I) = GetTeamYear%(Left$(gameTeams$(I), 2))
    Next I

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

            'Random # 45,000 - 55,000
            actualAttendance& = Int(Rnd(1) * 10000) + 45000

        Else

            Open diskPaths$(0) + DATA_FILE_NAME$ + "." + diskIDs$(gameLoc) For Random As #9 Len = DATA_SIZE_BYTES

            Field #9, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)

            Get #9, teamIdx_GAME(gameLoc)

            gameStadium$ = RTrim$(Q$(1))
            avgAttendance& = CVL(Q$(3))
            Z$ = Q$(4)

            Close #9

            'Determine attendance randomly
            If Z$ <> "x" Then
                'Generic stadium and attendance
                gameStadium$ = "MEMORIAL STADIUM"

                'Random # 45,000 - 55,000
                actualAttendance& = Int(Rnd(1) * 10000) + 45000
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

                'Average attendance +/- 5,000
                'actualAttendance& = avgAttendance& + Int(Rnd(1) * 10000) - 5000
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

    If ruleOptType > 0 Then
        leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 2
        leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 2
    End If

    leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 4
    leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 4

    Call NAMEEDIT

    For currTeam = 0 To 1

        If autoPlay = 1 Then
            'Autoamtic determine QB
            Erase activeKicker

            qbNumber(0) = NG%(5)

            If qbNumber(0) = 4 Then
                useRandomQB(0) = 1
                qbNumber(0) = 0
            Else
                qbMobility(0) = NG%(6)
            End If

            qbNumber(1) = NG%(8)

            If qbNumber(1) = 4 Then
                useRandomQB(1) = 1
                qbNumber(1) = 0
            Else
                qbMobility(1) = NG%(9)
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

    '----------
    'Coin Toss
    '----------
    'Print #11, gameTeams$(0); " vs "; gameTeams$(1)

    winTeam = Int(Rnd(1) * 2)

    Call SCOUT
    Call HALFSTART

    quarter = 1
    gameClock! = 900

    For I = 0 To 1
        For I1 = 0 To 4
            score(I, I1) = 0
        Next I1
    Next I

    Call SETSCREEN

    If d100% <= 50 Then
        pbpString$ = gameTeams$(winTeam) + " WON THE COIN TOSS"
    Else
        pbpString$ = gameMascots$(winTeam) + " WON THE COIN TOSS"
    End If

    Call PBP


    '----------------------------------------
    '           START OF GAME
    '----------------------------------------

    '-- Start of a "kickoff" routine

    330 '
    If ruleOptType > 0 And ruleOptPro < 8 Then
        'Pro rules before 2008
        Call KOChoices_NoDefer
    Else
        Call KOChoices_Defer
    End If

    340 '
    Swap P1, D

    Call MARKER1

    X = 0
    U3 = 0

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
    krChance = Int(100 * Rnd(1)) + 1

    For I = 2 To 0 Step -1
        If krChance <= krNumRet(D, I) Then
            I1 = I
        End If
    Next I

    Select Case KN

        Case 0
            pbpString$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) IS DEEP TO RECEIVE THE KICK"
            Call PBP

        Case 1
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

        Case 2
            If I1 = 1 Then KN2 = 0
            If I1 = 0 Then KN2 = 1

    End Select

    If KN > 0 And d100% <= 50 Then
        pbpString$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) AND " + kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) ARE DEEP"
    Else
        pbpString$ = kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) AND " + kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) ARE DEEP"
    End If

    Call PBP

    If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

        kickChoice = 0
        If gameClock! <= 180 And quarter = 4 And score(P1, 0) - score(D, 0) < 0 Then
            kickChoice = 2
        End If

    Else

        pbpString$ = "0-REGULAR  1-SQUIBB  2-ONSIDE KICK " + gameTeams$(P1)
        Call PBP

        Do
            I$ = GetKeyPress$
            kickChoice = Val(I$)
        Loop Until kickChoice >= 0 And kickChoice <= 2

    End If

    kickNotReturned = 0
    'Call pbplog(3964)

    Select Case kickChoice

        Case 0
            Call KickoffDistance
            'kickYL represents yard line where kick lands

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

            If kickYL <= fairCatchLine Then
                fairCatchZone = 1
                'Call pbplog(3998)
            End If

            If ((ruleOptType > 0 And ruleOptPro >= 8) Or (ruleOptType = 0 And ruleOptColl >= 11)) And fairCatchZone = 1 Then

                'Newer rules - fair Catch Allowed
                Call Rolld100(RN0)

                fcChance = 0

                Select Case ruleOptType

                    Case 0
                        'College, 2018+
                        If ruleOptColl >= 11 Then
                            If teamYears(D) >= 2018 And teamYears <= 2020 Then
                                fcChance = 27
                            Else
                                fcChance = 33
                            End If
                        End If

                    Case Is >= 1
                        'Pro, 2018+
                        If ruleOptPro = 8 Then fcChance = 12
                        If ruleOptPro = 9 Then fcChance = 18

                End Select

                If RN0 <= fcChance Then
                    fairCatch = 1
                End If

            End If

            If fairCatch = 1 Then

                ydsToScore = 100 - kickYL
                Call YARDLINE

                RN0 = Int(Rnd(1) * 2) + 1

                If RN0 = 1 Then
                    pbpString$ = kickRet$(P1, I1) + " SIGNALS FOR FAIR CATCH AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                Else
                    pbpString$ = kickRet$(P1, I1) + " CALLS FOR A FAIR CATCH ON THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                End If

                Call PBP

                'Call pbplog(4048)

                'Touchback will auto-change possession
                Call CHANGEPOSS
                Call Touchback
                kickNotReturned = 1

            Else


                If kickYL <= -9.99 Then

                    'Kicked out of endzone
                    'Automatic touchback
                    'Touchback will auto-change possession
                    'Call pbplog(4063)
                    Call CHANGEPOSS
                    Call Touchback
                    kickNotReturned = 1

                Else

                    X = 0


                    If kickYL < .01 Then

                        'Kick taken in end zone
                        ydLine = -(kickYL - 2 * kickYL)
                        ydsToScore = 100 - ydLine

                        Call MARKER
                        Call YARDLINE

                        pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + Str$(Int((kickYL - 2 * kickYL) + .5)) + " YDS DEEP IN THE END ZONE"

                        Call PBP

                        'Computer is receiving?
                        'Return it within first 2 yards of endzone
                        If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                            If kickYL < -2 Then
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

                            'Received in end zone,
                            'kneel for touchback
                            'Touchback will auto-change possession
                            Call CHANGEPOSS
                            Call Touchback
                            'Print #11, "TOUCHBACK, Kickoff, Downed"
                            kickNotReturned = 1

                        Else

                            kickNotReturned = 0

                        End If

                    Else

                        'Kick outside end zone - adjust position
                        kickYL = kickYL + (Int(Rnd(1) * 11) - 5) / 10

                        ydLine = kickYL
                        ydsToScore = 100 - ydLine

                        Call MARKER
                        Call YARDLINE

                        pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + " AT THE" + Str$(Int(kickYL + .5)) + " YD LINE "
                        Call PBP

                    End If

                End If

            End If 'Done checking if fair catch


            'Determine return yards based on returner's average
            If kickNotReturned = 0 Then

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

                        'If RN0 <= 50 Then
                        If RN0 <= 12 Then
                            'Kick return for TD - 1.2% chance
                            ydsGained = 110
                        Else
                            '15 to 33 yds
                            ydsGained = Int((Rnd(1) * 19) + 15)
                        End If

                    Case Is >= 10

                        'If RN0 <= 80 Then
                        If RN0 <= 20 Then
                            'Kick return for TD - 2% chance
                            ydsGained = 110
                        Else
                            '15 to 33 yds + (3 yds / int value of return average > 10)
                            'So a returner with 12 yard average would be 21 to 39 yards
                            ydsGained = Int((Rnd(1) * 19) + 15) + (krYdsPerRet(P1, I1) - 10) * 3
                        End If

                End Select

                startYds = 100 - ydLine
                endYds = startYds - ydsGained

                Call MOVEBALL

                X1 = 3
                gameStatsPlayer(P1, I1, 14) = gameStatsPlayer(P1, I1, 14) + 1

                If ydsGained + kickYL > 89.99 Then

                    gameStatsPlayer(P1, I1, 15) = gameStatsPlayer(P1, I1, 15) + 100 - kickYL
                    gameStatsPlayer(P1, I1, 16) = gameStatsPlayer(P1, I1, 16) + 1

                    If 100 - kickYL > kickReturners(P1, I1) Then
                        gameKR$(P1, I1) = "t"
                        kickReturners(P1, I1) = 100 - kickYL
                    End If

                    'Touchdown!
                    GoTo 3200

                End If

                If ydsGained + kickYL <= -.05 Then
                    'Returner didn't make it out of the endzone
                    Call CHANGEPOSS
                    Call Touchback
                    'Print #11, "TOUCHBACK, Kickoff, Nowhere"

                Else

                    ydLine = ydsGained + kickYL

                    If ydsGained > kickReturners(P1, I1) Then
                        gameKR$(P1, I1) = ""
                        kickReturners(P1, I1) = ydsGained
                    End If

                    gameStatsPlayer(P1, I1, 15) = gameStatsPlayer(P1, I1, 15) + ydsGained

                    F% = 3

                    Call FUMBLE

                    If ballFumbled = 1 Then
                        Call FumblePBP
                        GoTo 650
                    End If

                    Call AddPossessionTime(4, 7)

                    RN1 = Int(Rnd(1) * 1000) + 1
                    If RN1 >= 960 - teamRat_GAME(P1, 0) Then
                        Call CLIPPING
                    End If

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                End If

            End If

        Case 1
            'SQUIB KICK
            Call SQUIB
            Call FUMBLE

            If ballFumbled = 1 Then
                Call FumblePBP
                GoTo 650
            End If

            Call AddPossessionTime(4, 7)

            '3% chance of clipping on squib kick
            If d100% >= 97 Then
                Call CLIPPING
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

    '--- End of a "kickoff" routine


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
    'If we can get rid of references
    'to labels in the main game then
    'this can be a formal subroutine

    PointsAfterTD:

    If ruleOptType = 4 Then
        'WFL = No PAT
        convChoice = 2
    Else

        If ruleOptType = 1 And ruleOptPro < 4 Then
            'NFL prior to 1994 (2 PT conversion not supported)
            convChoice = 1

        Else

            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                pbpString$ = "1-KICK EP 2-GO FOR TWO"
                Call PBP

                ' *** Computer on Offense, or PC vs. PC ***

                'By default, go for one
                convChoice = 1

                'Determine score deficit
                I5 = score(P1, 0) - score(D, 0)

                ' ***** When to "Go for TWO" *****

                If ruleOptType = 1 And ruleOptPro >= 4 Then
                    'NFL, Post 1994 (when 2 pt conversion was implemented)
                    ' 4th quarter, down by 2 and 2 minutes left
                    ' 4th quarter, down by 9 and 5 minutes left
                    ' 4th quarter, down by 6 and 7 minutes left
                    If quarter = 4 And (I5 = -9 And gameClock! < 600 Or I5 = -2 And gameClock! < 120 Or I5 = -5 And gameClock! < 420) Then convChoice = 2

                Else
                    'College / AFL / WLAF / USFL
                    ' 3rd / 4th quarter, down by 18
                    ' 4th quarter, down by 16
                    If quarter >= 3 And I5 = -18 Or quarter = 4 And I5 = -16 Then
                        convChoice = 2
                    Else
                        '1) down by 2
                        '2) ahead by 5 | 12
                        If I5 = -2 Or I5 = 1 Or I5 = 5 Or I5 = 12 Then
                            convChoice = 2
                        Else
                            'Anytime outside the final 2-min warning
                            If Not (quarter = 4 And gameClock! > 240 Or quarter < 4) Then
                                'down by 1 | 4 | 5 | 8 | 10 | 13
                                If I5 = -1 Or I5 = -5 Or I5 = -8 Or I5 = -13 Or I5 = -4 Or I5 = -10 Then convChoice = 2
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
                    convChoice = Val(I$)
                Loop Until convChoice = 1 Or convChoice = 2

            End If

        End If

    End If

    '3702
    If convChoice = 2 Then

        'Since the "down" is not reset after the score,
        'we should treat the 2-point conversion as a
        '"1st down" for AI / play selection purposes
        'D = 1

        ydsGained = 0
        endZone = 1

        If ruleOptType = 1 And ruleOptPro >= 4 Then
            '1994 and beyond
            'NFL rules
            '2 point conversion from the 2
            ydLine = 98
        End If

        If ruleOptType = 0 Then
            'College rules
            '2 point conversion from the 3
            ydLine = 97
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
            'If RN1 <= kickerPATPct(P1, J) Then I1 = J
            If d100% <= kickerPATPct(P1, J) Then
                activeKicker(P1) = J
            End If
        Next J

        test = activeKicker(P1)
        gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) + 1

        pbpString$ = kicker$(P1, activeKicker(P1)) + " IS ON TO TRY THE EXTRA POINT"
        Call PBP

        If d100% <= kickerIdx(P1, activeKicker(P1)) Then

            gameStatsPlayer(P1, 5 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 5 + activeKicker(P1) * 4, 17) + 1

            pbpString$ = "THE KICK IS UP..."
            Call PBP
            Call MADEPAT

            'Increase score
            score(P1, 0) = score(P1, 0) + 1
            score(P1, quarter) = score(P1, quarter) + 1

            'Track the attempt for the kicker
            K1(nbrScores, 6) = activeKicker(P1)
            K1(nbrScores, 4) = K1(nbrScores, 4) + 1

            Call DisplayField

        Else

            'PAT blocked (9% chance)
            If d100% <= 9 Then

                pbpString$ = "KICK IS BLOCKED!!"
                Call PBP

                If ruleOptType <> 1 Then
                    'Not NFL
                    GoTo 6605
                End If

            Else

                Call MISSPAT

            End If

        End If

    End If

    Return 'For PointsAfterTD "sub"


    '----- NOODLE -----

    650 '
    gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

    If d100% <= 45 Then

        Call PBPINT

        pbpString$ = pbpString$ + gameTeams$(D) + " RECOVERS!!"
        Call PBP

        currDown = 1
        ydsToFirst = 10
        gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1
        ydLine = 100 - ydLine
        Call CHANGEPOSS

        '659

        If ruleOptType > 0 Or ruleOptColl >= 7 Then
            'Pro Rules or College after 1991

            RN1 = Int(Rnd(1) * 3) + 1

            If RN1 > 2 Then

                G1 = Int(Rnd(1) * 48) + 1
                X1 = 10

                If G1 >= 46 Then
                    G1 = 100
                Else
                    G1 = Int(Rnd(1) * 18)
                End If

                G1 = G1 + (Int(Rnd(1) * 11) - 5) / 10

                pbpString$ = "RETURNED FOR "

                returnYards = G1 + ydLine

                If returnYards >= 99.99 Then
                    pbpString$ = pbpString$ + Str$(Int((100 - ydLine) * 10 + .5) / 10) + " YDS "
                    Call PBP

                    'Touchdown!
                    GoTo 3200

                Else

                    'returnYards < 99.99
                    pbpString$ = pbpString$ + Str$(Int((G1) * 10 + .5) / 10) + " YDS"
                    Call PBP

                    If returnYards <= .01 Then
                        'Safety!
                        GoTo 3250
                    End If

                    ydLine = ydLine + G1

                End If

            End If

        End If

        If ydLine >= 90 Then
            ydsToFirst = 100 - ydLine
        End If
    
    Else

        pbpString$ = gameTeams$(P1) + " RECOVERS!!"
        Call PBP

        I = 50
        playSegment = 1

        GoTo 3303

    End If

    playSegment = 5

    GoSub 3900

    'College Overtime
    If NG%(14) = 3 And quarter > 4 Then
        GoTo 3364
    End If

    700 '
    Call MARKER1

    Call DISPWEATHER

    playSegment = 0
    ballFumbled = 0
    ydsGained = 0
    W5 = 0

    If U3 = 9 Then
        U3 = 0
        GoTo 340
    End If

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    Do

        Call SCOREBOARD
        Call DEFMENU

        'Offense Play Selection
        '-- Default to user-select
        autoPlaySelect = 0

        Do

            validPlay = 1

            'If computer is on Offense
            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                userSelectBC = 0
                autoPlaySelect = 1

                'This chooses the call but not the intended player
                Call ComputerOffense
                If playCall = 19 Then
                    'Call pbplog(4608)
                End If
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
                            I$ = "K"
                        End If

                        If UCase$(I$) = "Q" Then
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

                            If I$ = Chr$(27) Then GoTo 3118

                            Select Case G2

                                Case 1
                                    ' *** FLEA FLICKER ***
                                    Call FLEA(ffSuccess)

                                    If ffSuccess <= ffPctSuccess Then

                                        playType = 1
                                        W5 = 1
                                        ydsGained = (Int(Rnd(1) * 6) + 10)

                                        'Sacked
                                        GoTo 3541

                                    Else

                                        If d100% <= intChance Then

                                            ydsGained = CP
                                            FF% = 2
                                            'Intercepted!
                                            GoTo 3552

                                        Else

                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If d100% <= catchPctSuccess Then
                                                FF% = 2
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

                                    If userSelectBC = 1 Then

                                        userSelectBC = 0
                                        playType = 1
                                        I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0)

                                        Call REVGAIN
                                        GoTo 3416

                                    Else

                                        3181 '
                                        'Random ball carrier
                                        Call Rolld100(I2)

                                        For I = 9 To 0 Step -1:
                                            If I2 <= rbrushContrib(P1, I) Then
                                                I1 = I
                                            End If
                                        Next I

                                        playType = 1

                                        '8335
                                        'If the ball carrier chosen is a benched QB
                                        If I1 = (qbRushIdx(P1, 0) Or I1 = qbRushIdx(P1, 1) Or I1 = qbRushIdx(P1, 2) Or I1 = qbRushIdx(P1, 3)) Then

                                            'Check if they have any receptions
                                            If rbNumRec(P1, I1) > 0 Then
                                                'If so, use them as a runner
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

                                        Else

                                            '8336
                                            'Adjust yardage based on league rating and weather
                                            I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                                            If isReverse = 1 Then
                                                'Reverse play?
                                                Call REVGAIN
                                                GoTo 3416
                                            Else
                                                'Regular gain
                                                GoTo 3403
                                            End If

                                        End If

                                    End If

                                Case 3
                                    ' *** HB PASS ***
                                    HB% = 1

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

                                    If (coverage = 1 Or coverage = 3) And HB%(P9) = 0 Then catchPctSuccess = 30

                                    'Arbitrary 20% chance of interception
                                    intChance = 20
                                    ydsGained = Int(Rnd(1) * 71) + 30
                                    CP = Int(Rnd(1) * 11) + 30

                                    HB%(P9) = 1

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
                                            ydsGained = CP
                                            FF% = 1
                                            HB% = 0
                                            'Intercepted!
                                            GoTo 3552
                                        Else
                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If d100% <= catchPctSuccess Then
                                                FF% = 1
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

                            'we have 15 run plays, so chosenPlay needs
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

                'playCall = 25, from above
                If XX = 8 Then

                    pbpString$ = gameTeams$(P1) + " IS LETTING THE PLAY CLOCK WIND DOWN"
                    Call PBP

                    pbpString$ = "PENALTY - " + gameTeams$(P1) + " - DELAY OF GAME"
                    Call PBP

                    playSegment = 8
                    penaltyYds = 5

                    'Reset ball carrier
                    I2 = 0

                    GoTo 3326

                End If

                If XX = 7 Then
                    playCall = 9
                    playType = 1
                    GoTo 3403
                End If

                If (quarter = 3 Or quarter = 1) And playCall = 16 Or gameClock! > 120 And playCall = 16 Then
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

                    Locate 12 + KK, 14
                    Print KK; "-"; rbacks$(P1, KK);

                    If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, KK, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, KK) / 100 + .1 Then
                        Print " ###"
                    Else
                        Print
                    End If

                    If rbrushContrib(P1, KK) = 100 Then GoTo CSELECT

                Next

                For KK = 0 To 4

                    Locate 12 + KK, 41
                    Print KK + 5; "-"; rbacks$(P1, KK + 5);

                    If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, KK + 5, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, KK + 5) / 100 + .1 Then
                        Print " ###"
                    Else
                        Print
                    End If

                    If rbrushContrib(P1, KK + 5) = 100 Then GoTo CSELECT

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
                'End If

                Screen , , 0, 0

            End If

        End If

        ResolvePlay:
        'If Computer is playing offense and a long Pass was chosen inside the Opp 15 --> Change to Medium Pass
        If ((playerMode = 1 And P1 = compTeam Or playerMode = 2) And (playCall = 12 Or playCall = 15) And ydLine >= 85) Then
            playCall = 11
        End If

        'If there are timeouts and we're in the red zone, there's a 4% chance of confusion
        If timeouts(P1) > 0 And ydLine > 80 And Int(Rnd(1) * 25) + 1 = 16 Then

            pbpString$ = "THERE'S SOME CONFUSION AT THE LINE OF SCRIMMAGE"
            Call PBP

            pbpString$ = qbacks$(P1, qbNumber(P1)) + " HAS TO CALL TIME OUT TO AVOID DELAY OF GAME"
            Call PBP

            timeouts(P1) = timeouts(P1) - 1
            qbTakeKnee = 0

        End If

    Loop While (timeouts(P1) > 0 And ydLine > 80 And Int(Rnd(1) * 25) + 1 = 16)

    Call SCOREBOARD
    Call MARKER1

    If playerMode = 1 And D = compTeam Then
        Call COMPCHEAT
    End If

    If qbTakeKnee = 1 Then
        qbTakeKnee = 0
        GoTo 3403
    End If

    RN1 = Int(Rnd(1) * 1000) + 1

    If RN1 <= 36 And endZone <> 1 And playCall < 21 Then

        pbpString$ = "*** PENALTY - "
        RN2 = Int(Rnd(1) * 36) + 1

        Select Case RN2

            Case 1 To 2 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFFENSE... OFFSIDES 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 2 + teamRat_GAME(P1, 0) To 20 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "ILLEGAL PROCEDURE OFFENSE... 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 20 + teamRat_GAME(P1, 0) To 27 + teamRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFFENSE... DELAY OF GAME 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 27 + teamRat_GAME(P1, 0) To 36 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                pbpString$ = pbpString$ + "DEFENSE... ENCROACHMENT 5 YARDS ***"
                Call PBP
                penaltyYds = 5

        End Select

        playSegment = 8

        If penaltyYds > 0 Then
            GoTo 3330
        Else
            penaltyYds = penaltyYds * -1
            GoTo 3326
        End If

    End If


    'At this point the ball carrier has been chosen
    'Now we have to resolve the play outcome and yardage
    Select Case playCall

        Case 1 To 8

            ' ****  RUNNING PLAYS  ****

            Call QBLINE

            'If user picked the ball carrier, work out yardage / fumbles
            If userSelectBC = 1 Then

                userSelectBC = 0
                playType = 1

                3403 '
                'Average rush yards
                rushYds = rbRushAvg(P1, I1)

                'Adjust yardage based on DEF league ratings and weather
                I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                If gameStatsTeam(P1, 1) >= 20 And gameStatsPlayer(P1, I1, 0) / gameStatsTeam(P1, 1) > indRushPct(P1, I1) / 100 + .1 Then
                    'If team has more than 20 attempts and current runner's contribution > his average?
                    I2 = Int(I2 * .4)
                End If

                RN0 = Int(Rnd(1) * 200) + 1

                '0.5 % chance of fumble outside the end zones
                If RN0 = 97 And ydLine <= 98.899 And ydLine >= 1.101 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " FUMBLES THE SNAP!"
                    Call PBP

                    ydsGained = Int((Rnd(1) * 20) - 10) / 10

                    X = 4
                    X1 = 9
                    I1 = qbNumber(P1)

                    If ydLine + ydsGained < 99.94999 Then
                        'If we're within a foot of the first down
                        If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then Call MEASUREMENT
                    End If

                    pbpString$ = "HE GAINS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    If endZone = 1 Then Call TryForTwoResult

                    'There was a RETURN in the original code
                    'But it was always sent to with GOTO
                    'So where was it returning from?

                    ballFumbled = 1

                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1
                    If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                    GoTo 650

                End If

                'QB Sneak
                If playCall = 8 Then

                    'Print #11, qbacks$(P1, qbNumber(P1)) + " is going for the SNEAK!"
                    Call QBSNEAK

                    If endZone = 1 Then Call TryForTwoResult

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                        'Safety!
                        GoTo 3250
                    End If

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                        'Touchdown!
                        GoTo 3200
                    End If

                    Call FUMBLE

                    GoTo 3309

                End If

                If playCall = 9 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND KNEELS DOWN FOR "
                    Call PBP
                    ydsGained = -1.0

                    GoTo 3546

                End If

                'Option play = 1% cance of fumble?
                If playCall = 5 And d100% = 49 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " MAKES A BAD PITCH ON THE OPTION!!"
                    Call PBP

                    'Loss of 6 - 12 yds
                    ydsGained = Int((Rnd(1) * 7) + 6) * -1
                    X = 4
                    X1 = 9
                    I1 = qbNumber(P1)

                    pbpString$ = "HE LOSES" + Str$(Int((ydsGained * -1) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                        ballFumbled = 0

                        'Safety!
                        GoTo 3250

                    Else

                        If endZone = 1 Then Call TryForTwoResult

                        ballFumbled = 1

                        Call COMPILER

                        ydLine = ydLine + ydsGained
                        ydsToFirst = ydsToFirst - ydsGained
                        currDown = currDown + 1
                        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        GoTo 650

                    End If

                End If

                Call HANDOFF

                If dLine = 11 Then

                    Call GOALLINE

                Else

                    I2 = I2 + defYdAdj(playCall, dLine)

                    S6 = 0

                    If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then
                        If (gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3))) > (leagRat_GAME(P1, 7) / 100) And K3(P1, 0) - (leagRat_GAME(D, 0) * .3) > 4.5 And (leagRat_GAME(D, 0) * .3) > 4.5 And leagRat_GAME(P1, 7) < 50 Then
                            S6 = Int(((K3(P1, 0) - 3.6) / .3) + .5) * -1
                        End If
                    End If

                    I2 = I2 + S6

                    If P1 = gameLoc Then
                        'Home field advantage
                        I2 = I2 + 1
                    Else
                        'Away game (not home, not Neutral)
                        If gameLoc <> 2 Then I2 = I2 - 1
                    End If

                    'Select Case

                    'I2 = 0
                    If I2 < 1 Then I2 = 1

                    'I2 = 1 to 38
                    If I2 <= 38 Then

                        If (playCall <= 2 And dLine = 5 Or playCall <= 2 And dLine = 6 Or playCall >= 6 And dLine = 5 Or playCall >= 6 And dLine = 6) Or (playCall > 2 And playCall < 5 And dLine = 7 Or playCall > 2 And playCall < 5 And dLine = 8) Then

                            ' *** STUNTS ***
                            Call Rolld100(RN4)

                            Select Case I2

                                Case Is <= 15
                                    Select Case RN4
                                        Case 1 To 15: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 16 To 60: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 61 To 100: ydsGained = Int(Rnd(1) * (I2 * 2.13)) + 1
                                    End Select

                                Case 16, 17
                                    Select Case RN4
                                        Case 1 To 10: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 11 To 50: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 51 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.73)) + 1
                                    End Select

                                Case 18, 19
                                    Select Case RN4
                                        Case 1 To 10: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 11 To 40: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 41 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.5)) + 1
                                    End Select

                                Case 20, 21
                                    Select Case RN4
                                        Case 1 To 5: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 6 To 30: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 31 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.3)) + 1
                                    End Select

                                Case Is >= 22
                                    Select Case RN4
                                        Case 1 To 5: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 6 To 20: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 21 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.2)) + 1
                                    End Select

                            End Select

                            GoTo 3416

                        End If

                        Call Rolld100(RN4)

                        If (playCall <= 2 And RN4 <= 50 And coverage = 7) Or (playCall >= 6 And RN4 <= 50 And coverage = 7) Or (playCall <= 2 And coverage = 9) Or (playCall >= 6 And coverage = 9) Then
                            Call BLITZ2
                        End If

                        If playCall >= 3 And playCall <= 5 Then
                            If coverage = 8 And RN4 <= 50 Or coverage = 10 Then
                                Call BLITZ2
                            End If
                        End If

                        If coverage = 11 Then

                            Call BLITZ2

                        Else

                            If I2 > 17 Then

                                ydsGained = Int(Rnd(1) * 48) + 1

                                Select Case ydsGained

                                    Case 1 To 44
                                        ydsGained = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10)

                                    Case 45
                                        ydsGained = Int(Rnd(1) * 2 * I2) + 1

                                    Case 46 To 47
                                        ydsGained = Int(Rnd(1) * I2) + 1

                                    Case 48
                                        ydsGained = Int(Rnd(1) * 5 * I2) + 1

                                End Select

                            Else

                                If d100% > 92 Then
                                    ' *** BREAKAWAY ***
                                    Call Rolld100(RN5)
                                    If RN5 <= 50 Then
                                        ydsGained = Int(Rnd(1) * brkawayYds(I2, 0)) + brkawayYds(I2, 1)
                                    Else
                                        ydsGained = Int(Rnd(1) * brkawayYds(I2, 3)) + brkawayYds(I2, 4)
                                    End If

                                Else

                                    I5 = 0

                                    If playCall = 4 Or playCall = 5 Then
                                        I5 = 1
                                    End If

                                    For I6 = 0 To 14:
                                        If RN4 <= RM%(I5, I6) Then
                                            ydsGained = I6 - 5
                                            Exit For
                                        End If
                                    Next I6

                                    If d100% <= runYdAdj(0, I2) Then
                                        ydsGained = ydsGained + runYdAdj(1, I2)
                                    End If

                                End If

                            End If

                        End If 'Done checking div3Name$

                    Else
                        'I2 = 39 +
                        ydsGained = Int(Rnd(1) * 48) + 1

                        Select Case ydsGained

                            Case 1 To 44
                                ydsGained = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10)

                            Case 45
                                ydsGained = Int(Rnd(1) * 2 * I2) + 1

                            Case 46 To 47
                                ydsGained = Int(Rnd(1) * I2) + 1

                            Case 48
                                ydsGained = Int(Rnd(1) * 5 * I2) + 1

                        End Select

                    End If

                    'End Select

                End If 'Done checking / adjusting for position


                3416 '
                'Determine play yardage
                Call YARDAGE

                'Call PBP based on special scenarios
                '"Stuffed", Long Play or Run for TD
                If ydsGained <= 0 Then
                    Call STUFFED
                End If

                If ydsGained > 10 And ydLine + ydsGained < 99.94999 Then
                    Call LGPBP
                End If

                If ydLine + ydsGained >= 99.94999 Then
                    Call RUNTDPBP
                End If

                startYds = 100 - ydLine
                endYds = startYds - ydsGained

                Call MOVEBALL

                If ydLine + ydsGained < 99.94999 Then

                    F% = 2

                    Call FUMBLE

                    If ballFumbled <> 1 Then
                        'If we're within a foot of the first down
                        If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then
                            Call MEASUREMENT
                        End If
                    End If

                End If

                'Print / call PBP for actual yardage
                If ydsGained < 0 Then

                    pbpString$ = rbacks$(P1, I1) + " LOST"
                    'Print
                    X = 1
                    X1 = 1
                    pbpString$ = pbpString$ + Str$(Int((ydsGained * -1) * 10 + .5) / 10)

                Else

                    Call RUNPBP

                    X = 1
                    X1 = 1

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                        pbpString$ = pbpString$ + Str$(Int((100 - ydLine) * 10 + .5) / 10)
                    Else
                        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
                    End If

                End If


                pbpString$ = pbpString$ + " YARDS"
                Call PBP

                If isReverse = 1 Then isReverse = 0

                If endZone = 1 Then
                    ballFumbled = 0
                    Call TryForTwoResult
                End If

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                    ballFumbled = 0
                    'Safety!
                    GoTo 3250
                End If

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                    ballFumbled = 0
                    'Touchdown!
                    GoTo 3200
                End If

                GoTo 3309

            Else
                'userSelectBC = 0

                3401 '
                playType = 1

                'Pick random ball carrier based on contribution %
                Call Rolld100(I2)

                For I = 9 To 0 Step -1:
                    If I2 <= rbrushContrib(P1, I) Then
                        I1 = I
                    End If
                Next I

                CheckQBRunner:
                'If the QB was chosen, we need to go with a pass play.
                'The game handles all QB rushes as scrambles / etc.
                'Without this in place, rush attempts for a team run
                'way high and pass attempts run too low
                If rbacks$(P1, I1) = qbacks$(P1, qbNumber(P1)) Then
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
                    If rbNumRec(P1, I1) > 0 Then

                        'Normal rush
                        GoTo 3403

                    Else

                        'Assing active QB instead
                        For currBack = 0 To 9
                            If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                                I1 = currBack
                                'GoTo 3403
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
                    I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + climate(4)

                    If isReverse = 1 Then
                        'Reverse play?
                        Call REVGAIN
                        GoTo 3416
                    Else
                        'Regular rush yardage gain
                        GoTo 3403
                    End If

                End If

            End If 'Done checking userSelectBC

        Case 9 To 18

            Call PASSBALL(compChance)

            'SACKED
            If compChance < passCovAdj(3, playCall - 8, coverage) + leagRat_GAME(P1, 5) + leagRat_GAME(D, 6) + 1 Then

                playType = 1
                W5 = 1

                ' *** SACKED ***
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

                'Yardage for sack is negative
                ydsGained = ydsGained * -1

                If defSacks(1 - P1) <> 0 Then

                    M5 = 0
                    RN0 = Int(Rnd(1) * teamSacks(1 - P1)) + 1

                    For Z1 = 0 To 14

                        M5 = M5 + playerSacks(1 - P1, Z1)

                        If RN0 <= M5 Then
                            pbpString$ = pbpString$ + " BY " + defSacks$(1 - P1, Z1)
                            Exit For
                        End If

                    Next

                End If

                '3699
                Call PBP

                X = 4
                playType = 1

                If endZone = 1 Then Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                    'Safety!
                    GoTo 3250

                Else

                    F% = 1

                    Call FUMBLE

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

            Call Rolld100(RN0)

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

            'Fake Kick
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

            'Determine chance of QB scramble
            If RN0 <= scrambleChance Then
                willScramble = 1
            End If

            If willScramble = 1 Then

                'QB RUN
                I6 = 0

                For currBack = 0 To 9
                    If rbacks$(P1, currBack) = qbacks$(P1, qbNumber(P1)) Then
                        'Def Rush Adj doesn't apply
                        I6 = rbRushAvg(P1, currBack)
                        Exit For
                    End If
                Next currBack

                'About 10% of all football plays are outside the average gain
                specialGain = Int(Rnd(1) * 10) + 1

                If I6 = 0 Or qbMobility(P1) < 2 Then
                    'QB is not a rusher, or mobility is low
                    'Essentially, we're treating these as the expected behavior.
                    '(Low mobility QB's will often have low or negative averages)
                    '(This will translate to larger negatives below)
                    If specialGain < 10 Then
                        'average gains are 1-5 yds
                        qbRushMax = 5
                    Else
                        'longer gain
                        qbRushMax = 16
                    End If
                Else
                    'QB is a rusher
                    If specialGain < 10 Then
                        'Average gains previously maxed at 7
                        'Here we use a fraction of the rushing Average
                        'This is not the real world average, but a
                        'representation using integers.
                        '   4.8 = 16 (1.6 * 3 = 4.8)
                        '   4.2 = 14 (1.4 * 3 = 4.2)
                        '   3.9 = 13 (1.3 * 3 = 3.9)
                        '   2.7 = 9 (0.9 * 3 = 2.7)
                        'So our maximums become:
                        '   Avg 4.8 = 11
                        '   Avg 4.2 = 10
                        '   Avg 3.9 = 9
                        '   ...
                        '   Avg 2.7 = 6
                        'This limits the that even some major
                        'scrambles yield over 10 yards, but that
                        'would be the rarity.
                        qbRushMax = (I6 * 3) / 4
                    Else
                        'Here our maximums become:
                        '   Avg 4.8 = 18
                        '   Avg 3.9 = 12
                        '   Avg 2.7 = 4
                        qbRushMax = 12 + ((I6 - 13) * 2)
                    End If

                End If

                ydsGained = Int(Rnd(1) * qbRushMax)
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
                        pbpString$ = "HE PICKS UP ABOUT"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " KEEPS THE BALL AND AND RUNS FOR"
                    End If

                Else

                    If RN0 = 1 Then
                        pbpString$ = qbacks$(P1, I1) + " IS CHASED FROM THE POCKET"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " CAN'T FIND AN OPEN TARGET"
                        Call PBP
                        pbpString$ = "HE SPRINTS AHEAD FOR "
                    End If

                End If

                '-- Make sure if rush was for negative yardage, that
                ' it's recorded / tracked right

                3546 '
                X = 4
                X1 = 9
                playType = 1

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
                Else
                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
                End If

                pbpString$ = pbpString$ + " YARDS"
                Call PBP

                If endZone = 1 Then Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                    'Safety!
                    GoTo 3250
                End If

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                    gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1
                    'Touchdown!
                    GoTo 3200
                End If

                F% = 2

                Call FUMBLE

                GoTo 3309

            Else

                'Determine pass outcome
                Call Rolld100(passChance)

                'Compare value to expected interception %
                '               QB's Int % - Pass type adjustment + league DEF Int %
                intChance = qbPctInt(P1, I1) - passCovAdj(0, playCall - 8, coverage) + leagRat_GAME(D, 2)

                If passChance > (100 - intChance) Then

                    'Intercepted!
                    GoTo 3552

                Else

                    'Determine % chance of completion
                    Call AdjustCompPct

                    If passChance <= expCompPct + passTypeCmpAdj + ballPosCmpAdj + windCmpAdj Then
                        'Pass completed

                        'Determine yards based on player's average
                        If I3 > 9 Then
                            I4 = wrYdsPerC(P1, I3 - 10)
                            rushYds = I4
                        Else
                            I4 = rbYdsPerC(P1, I3)
                            rushYds = I4
                        End If

                        'Adjust yards for weather
                        If climate(4) = -3 Then
                            I4 = I4 - 1
                        End If
                        If climate(4) = -6 Then
                            I4 = I4 - 2
                        End If

                        If I4 < 11 Then
                            'Determine impact of blitz
                            Call BLITZ

                        Else

                            I4 = I4 + passCovAdj(2, playCall - 8, coverage)

                            If ydsPerComp(D) / 100 > .5 Then
                                I4 = I4 - (1 - ydsPerComp(D) / 100) * 10
                            End If
                            If ydsPerComp(D) / 100 < .5 Then
                                I4 = I4 + ydsPerComp(D) / 100 * 10
                            End If

                            'Check if the play is overused
                            Call OVERUSE

                            If playCall = 9 Or playCall = 10 Or playCall = 13 Then
                                I4 = I4 * ((((expCompPct + S6) * K3(P1, 3)) / (expCompPct + S6 + 18)) / K3(P1, 3))
                            End If

                            If playCall = 12 Or playCall = 15 Then
                                I4 = I4 * ((((expCompPct + S6) * K3(P1, 3)) / (expCompPct + S6 - 18)) / K3(P1, 3))
                            End If

                            'At this point I'm lost but playCall is the pass play type
                            RN0 = Int(Rnd(1) * 25) + 1

                            If playCall <> 12 And playCall <> 15 And RN0 = 25 Or (playCall = 12 Or playCall = 15) And RN0 = 25 And ruleOptType = 0 Or (playCall = 12 Or playCall = 15) And ruleOptType <> 0 And I >= 24 Then
                                'Longer gains
                                If RN0 = 25 And Int(Rnd(1) * 32) = 11 And score(P1, 0) > 13 And XD(D) > .6 Then
                                    ydsGained = 102
                                Else
                                    ydsGained = Int(Rnd(1) * I4 * 12) + 1 + (I4 * 6)
                                End If
                            Else
                                If RN0 <= 19 Then
                                    ydsGained = Int(Rnd(1) * 13) + 1 + ((I4 * 2.3) - 7.2)
                                Else
                                    ydsGained = (Int(Rnd(1) * (5 + (I4 * 2))) + 1) + (3 * (I4 - 1))
                                End If
                            End If

                        End If

                        3586 '
                        'ADDED AFTER REPLAYS OF 5-24-93 <-- ???
                        'Random # 1 - 3 = 1 And Pro Game = adjust by 1
                        If Int(Rnd(1) * 3) + 1 = 1 And ruleOptType <> 0 Then
                            ydsGained = ydsGained - 1
                        End If

                        Call YARDAGE

                        X = 3
                        X1 = 2

                        If ydsGained + ydLine > 99.9499 Then ydsGained = 109 - ydLine

                        'Play-by-Play for Completion %
                        Call PCPBP

                        If FF% <> 0 Then FF% = 0

                        If ydLine + ydsGained < 99.9499 Then

                            F% = 2
                            Call FUMBLE

                            If ballFumbled <> 1 Then
                                'If we're within a foot of the first down
                                If ydsGained - ydsToFirst >= -.33 And ydsGained - ydsToFirst <= .33 Then Call MEASUREMENT
                            End If

                        End If

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

                            pbpString$ = "THAT CATCH BY "
                            Call WHICHRECEIVER
                            pbpString$ = pbpString$ + " GAINED" + Str$(Int(100 - ydLine)) + " YARDS"

                            ydsGained = 100 - ydLine

                            If ydsGained > WR(P1, I3) Then
                                gameWR$(P1, I3) = "t"
                                WR(P1, I3) = ydsGained
                            End If

                            If ydsGained > quarterbacks(P1, qbNumber(P1)) Then
                                gameQB$(P1, qbNumber(P1)) = "t"
                                quarterbacks(P1, qbNumber(P1)) = ydsGained
                            End If

                            Call PBP

                        Else

                            If ydsGained < 0 Then

                                pbpString$ = "THAT CATCH BY "
                                Call WHICHRECEIVER
                                pbpString$ = pbpString$ + " LOST" + Str$(Int((ydsGained * -1) * 10 + .5) / 10) + " YARDS"

                                If ydsGained > WR(P1, I3) Then
                                    gameWR$(P1, I3) = ""
                                    WR(P1, I3) = ydsGained
                                End If

                                If ydsGained > quarterbacks(P1, qbNumber(P1)) Then
                                    gameQB$(P1, qbNumber(P1)) = ""
                                    quarterbacks(P1, qbNumber(P1)) = ydsGained
                                End If

                            Else

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 < 99.9499 Then pbpString$ = "THAT CATCH BY "
                                Call WHICHRECEIVER
                                pbpString$ = pbpString$ + " GAINED" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"

                                If ydsGained > WR(P1, I3) Then
                                    gameWR$(P1, I3) = ""
                                    WR(P1, I3) = ydsGained
                                End If

                                If ydsGained > quarterbacks(P1, qbNumber(P1)) Then
                                    gameQB$(P1, qbNumber(P1)) = ""
                                    quarterbacks(P1, qbNumber(P1)) = ydsGained
                                End If

                            End If

                        End If

                        Call PBP

                        If endZone = 1 Then ballFumbled = 0: Call TryForTwoResult

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                            ballFumbled = 0

                            'Safety!
                            GoTo 3250

                        End If

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

                            ballFumbled = 0
                            gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

                            'Touchdown!
                            GoTo 3200

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

            pbpString$ = pret$(D, puntRetNbr) + " (" + Str$(Int(prYdsPerRet(D, puntRetNbr) * 3)) + " AVG) IS DEEP TO RECEIVE"
            Call PBP

            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then
                puntChoice = 0
            Else

                Call FRAMEOPTION(9, P1)
                Print "PUNTING OPTIONS "; gameTeams$(P1)
                Locate 12, 14
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
                    X = 0
                    ydsGained = ydsPerPunt(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * ydsPerPunt(P1, 0)))) + 11

                    If climate(0) = P1 Then
                        ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - climate(1)
                    Else
                        ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + climate(1)
                    End If

                    If (coverage > 6 And kickChance >= 60 And kickChance <= 72) Or (coverage <= 6 And kickChance >= 50 And kickChance <= 56) Then
                        'Blocked punt
                        GoTo 3840
                    End If

                    pbpString$ = "THE KICK IS UP AND ON ITS WAY"
                    Call PBP

                    ydsGained = ydsGained + 2

                    Call YARDAGE
                    'Gets us a value for ydLineTeam

                    pbpString$ = "KICK BY " + gameTeams$(P1) + " TRAVELS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    gameStatsTeam(P1, 7) = gameStatsTeam(P1, 7) + 1

                    'Call pbplog(6315)

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                        'Punt travels to end zone

                        'Track punt yards
                        gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + 100 - ydLine

                        If 100 - ydLine > gameStatsTeam(P1, 26) Then
                            'Longest punt?
                            gameStatsTeam(P1, 26) = 100 - ydLine
                        End If

                        gameClock! = gameClock! - 5
                        timePoss(P1) = timePoss(P1) + 5

                        'Track touchback for punter
                        gameStatsTeam(P1, 27) = gameStatsTeam(P1, 27) + 1

                        'Call pbplog(6334)
                        Call Touchback

                    Else

                        'Track punt yards
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

                        'Call pbplog(6375)
                        Call PuntNotReturned(fairCatchZone, puntChoice)

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Call DELAY

                    End If

                    GoTo 700

                End If

            End If

            X = 0

            'Normal yards for punter
            ydsGained = ydsPerPunt(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * ydsPerPunt(P1, 0)))) + 3

            If (100 - ydLine) <= ((ydsPerPunt(P1, 0) * 2) + 28) Then
                ydsGained = ydsGained + (100 - ydLine) - (31 + ydsPerPunt(P1, 0) * 2)
            End If

            If climate(0) = P1 Then
                ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - climate(1)
            Else
                ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + climate(1)
            End If

            Call YARDAGE

            If playerMode = 2 Or (playerMode = 1 And D = compTeam) Then

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

                    If UCase$(I$) = "Y" Then

                        If puntChoice = 3 Then

                            ' *** FAKE W/PASS ***
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

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Call PBP
                            Else
                                pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                Call PBP
                            End If

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                'Touchdown!
                                GoTo 3200
                            End If

                            F% = 2
                            Call FUMBLE

                        Else

                            Call KickFakeYards(puntDefChoice)

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                                'Safety!
                                GoTo 3250
                            End If

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                'Touchdown!
                                GoTo 3200
                            End If

                            F% = 2
                            Call FUMBLE

                        End If

                        GoTo 3309

                    End If 'Done processing fake punt

                Case 4
                    Call TAKESAFETY
                    'Offensive Safety
                    GoTo 3251

            End Select

            PSNAP:
            'Call pbplog(6396)

            'Normal punt
            pbpString$ = punter$(P1, 0) + " WAITS FOR THE SNAP"
            Call PBP

            RN5 = Int(Rnd(1) * 300) + 1

            If RN5 = 12 Then
                '~0.3% chance

                pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
                Call PBP

                ydsGained = (Int(Rnd(1) * 14) + 12) * -1
                Call AddPossessionTime(4, 7)

                gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1
                gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

                If ydLine + ydsGained < -9.999 Then

                    'Offensive Safety
                    score(D, quarter) = score(D, quarter) + 2
                    score(D, 0) = score(D, 0) + 2
                    GoTo 3251

                End If

                If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

                    If d100% <= 50 Then

                        'Offensive Safety
                        score(D, quarter) = score(D, quarter) + 2
                        score(D, 0) = score(D, 0) + 2
                        GoTo 3251

                    Else

                        X1 = 10
                        Call CHANGEPOSS

                        'Touchdown!
                        GoTo 3200

                    End If

                End If

                Call CHANGEPOSS
                currDown = 1
                ydsToFirst = 10
                ydLine = 100 - ydLine

                If ydsToFirst >= 90 Then ydsToFirst = 100 - ydLine

                If d100% <= 50 Then
                    pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
                Else
                    pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
                End If

                If ydLine > 50 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
                Else
                    pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
                End If

                playSegment = 5

                GoSub 3900

            Else

                RN6 = Int(Rnd(1) * 200) + 1

                If puntDefChoice = 1 Then

                    'Punt away from returner
                    pbpString$ = "THERE'S A HEAVY RUSH!!!"
                    Call PBP

                    Select Case RN6

                        Case 1 To 16
                            'Blocked punt
                            GoTo 3840

                        Case 17 To 18
                            'Roughing the Kicker (1% chance)
                            pbpString$ = "THERE'S A FLAG ON THE PLAY!!"

                            Call PBP

                            pbpString$ = "ROUGHING THE KICKER !!!...AUTOMATIC FIRST DOWN !!!"

                            Call PBP

                            gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1
                            gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 15
                            gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

                            ydLine = ydLine + 15

                            currDown = 1
                            ydsToFirst = 10
                            playSegment = 4

                            GoSub 3900

                            Call DELAY

                            GoTo 700

                        Case Else

                            pbpString$ = punter$(P1, 0) + " TAKES THE SNAP"
                            Call PBP

                            If d100% <= 10 Then
                                ydsGained = ydsGained - (Int(Rnd(1) * 11) + 25)
                                pbpString$ = "HE SHANKS IT!"
                            Else
                                ydsGained = ydsGained - (Int(Rnd(1) * 9) + 4)
                                pbpString$ = "HE KICKS IT TOWARDS THE CORNER"
                            End If

                    End Select

                Else

                    pbpString$ = punter$(P1, 0) + " TAKES THE SNAP"
                    Call PBP

                    If ydsGained < 40 Then
                        pbpString$ = "HE GETS THE PUNT AWAY"
                    Else
                        Call PUNTPBP
                    End If

                End If

                Call PBP

                pbpString$ = "PUNT BY " + punter$(P1, 0) + " TRAVELS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                Call PBP
                'Call pbplog(6717)

                gameStatsTeam(P1, 7) = gameStatsTeam(P1, 7) + 1

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then

                    'Track punt yards
                    gameStatsTeam(P1, 8) = gameStatsTeam(P1, 8) + 100 - ydLine

                    'Update clock;
                    '-- Punts always take 5 seconds???
                    gameClock! = gameClock! - 5
                    timePoss(P1) = timePoss(P1) + 5

                    'Track touchback
                    gameStatsTeam(P1, 27) = gameStatsTeam(P1, 27) + 1

                    'Call pbplog(6736)
                    Call Touchback

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

                    If ydsGained + ydLine > 80 Then
                        'Kicks inside the 20
                        gameStatsTeam(P1, 29) = gameStatsTeam(P1, 29) + 1
                    End If

                    If ydsGained + ydLine > fairCatchLine Then
                        fairCatchZone = 1
                    Else
                        fairCatchZone = 0
                    End If

                    If ydsGained > gameStatsTeam(P1, 26) Then
                        'Longest punt?
                        gameStatsTeam(P1, 26) = ydsGained
                    End If

                    'See if punt can be returned
                    If (fairCatchZone = 1 And d100% < 85) Or (ydsGained + ydLine >= 90 And puntChance >= 5) Or puntChance > 55 Or puntDefChoice = 2 Then
                        ' - 85% of punts inside the fair catch zone
                        ' - 5% of punts inside the 10
                        ' - 45% for punts outside the 20
                        ' - Punt Safe formation
                        'Call pbplog(6782)
                        Call PuntNotReturned(fairCatchZone, puntChoice)

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Call DELAY

                    Else

                        'Call pbplog(6795)
                        Call PuntReturnYards

                        If ydsGained + ydLine >= 99.99 Then

                            'Track Punt Return
                            gameStatsPlayer(P1, puntRetNbr + 3, 14) = gameStatsPlayer(P1, puntRetNbr + 3, 14) + 1

                            'Track Return Yards
                            gameStatsPlayer(P1, puntRetNbr + 3, 15) = gameStatsPlayer(P1, puntRetNbr + 3, 15) + 100 - ydLine

                            'Track Return TD
                            gameStatsPlayer(P1, puntRetNbr + 3, 16) = gameStatsPlayer(P1, puntRetNbr + 3, 16) + 1

                            returnGain = 100 - ydLine

                            If returnGain > puntReturners(P1, puntRetNbr) Then
                                gamePR$(P1, puntRetNbr) = "t"
                                puntReturners(P1, puntRetNbr) = returnGain
                            End If

                            I1 = puntRetNbr

                            'Touchdown!
                            GoTo 3200

                        End If

                        gameStatsPlayer(P1, puntRetNbr + 3, 14) = gameStatsPlayer(P1, puntRetNbr + 3, 14) + 1
                        gameStatsPlayer(P1, puntRetNbr + 3, 15) = gameStatsPlayer(P1, puntRetNbr + 3, 15) + ydsGained

                        If ydsGained > puntReturners(P1, puntRetNbr) Then
                            gamePR$(P1, puntRetNbr) = ""
                            puntReturners(P1, puntRetNbr) = ydsGained
                        End If

                        If ydsGained + ydLine <= .01 Then
                            'Safety!
                            GoTo 3250
                        End If

                        currDown = 1
                        ydLine = ydLine + ydsGained
                        ydsToFirst = 10

                        If ydLine >= 89.99 Then ydsToFirst = 100 - ydLine

                        RN1 = Int(Rnd(1) * 1000) + 1

                        If RN1 >= 960 - teamRat_GAME(P1, 0) Then

                            pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
                            Call PBP

                            If ydLine < 20 Then
                                ydLine = ydLine / 2
                                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
                            End If

                            gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

                            If ydLine >= 20 Then
                                ydLine = ydLine - 10
                                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 10
                            End If

                        Else

                            F% = 4

                            Call FUMBLE

                            If ballFumbled = 1 Then
                                Call FumblePBP
                                GoTo 650
                            End If

                        End If

                        hangtime = Int(ydsGained / 10)
                        Call AddPossessionTime(8 + hangtime, 13 + hangtime)
                        Call DELAY

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                    End If

                End If

            End If

            GoTo 700


        Case 20

            Call Rolld100(fgChance)

            'Was previously I2 not J
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
                If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                    I1 = 0

                Else

                    Call FRAMEOPTION(6, P1)
                    Print "FIELD GOAL OPTIONS "; gameTeams$(P1)
                    Locate 12, 14
                    Locate , 14: Print "0 - ATTEMPT FIELD GOAL"
                    Locate , 14: Print "1 - FAKE FIELD GOAL (RUN)"
                    Locate , 14: Print "2 - FAKE FIELD GOAL (PASS)"

                    Do
                        I$ = GetKeyPress$
                        I1 = Val(I$)
                    Loop Until I1 >= 0 And I1 <= 2 Or I$ = Chr$(27)

                    Screen , , 0, 0

                End If

                If I$ = Chr$(27) Then GoTo 3118

                Call Rolld100(RN0)

                If playerMode = 1 And D = compTeam Or playerMode = 2 Then

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

                Screen , , 0, 0

                'Fake Field Goal Was Chosen
                If I1 = 2 Or I1 = 1 Then

                    ' *** FAKE PUNT - RUN/PASS ***
                    If fgDefChoice = 2 Then

                        pbpString$ = gameTeams$(D) + " IS IN FIELD GOAL SAFE SET...FAKE FIELD GOAL? (Y/N)"
                        Call PBP

                    Else

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "N" Then GoTo 3602

                    End If

                    If I1 = 2 Then

                        ' *** FAKE W/PASS ***
                        pbpString$ = "IT'S A FAKE!!! THE HOLDER IS LOOKING TO PASS"
                        Call PBP
                        ydsGained = 0
                        Call Rolld100(RN0)

                        passIncomplete = 0
                        passIntercepted = 0

                        Select Case I2

                            Case 1

                                Select Case RN0

                                    Case 1 To 20:
                                        ydsGained = Int(Rnd(1) * 20)

                                    Case 21 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        passIncomplete = 1

                                    Case 81 To 90:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted = 1

                                    Case 91 To 100:
                                        ydsGained = 100 - ydLine

                                End Select

                            Case 2

                                Select Case RN0

                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 7) + 6

                                    Case 11 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        playSegment = 4
                                        passIncomplete = 1
                                        GoTo 3309

                                    Case 81 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted = 1

                                End Select

                        End Select

                        If passIntercepted = 1 Then

                            X = 0
                            'Track Pass Att, QB Int
                            gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                            gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1
                            gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                            gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1

                            Call AddPossessionTime(5, 8)

                            ydsGained = Int(Rnd(1) * 10) + 1

                            Call YARDAGE

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                'Interception in the end zone
                                pbpString$ = pbpString$ + " IN THE END ZONE"
                                Call PBP
                                Call Touchback

                            Else

                                If ydsGained + ydLine > 49.99 Then
                                    pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
                                End If
                                If ydsGained + ydLine < 49.99 Then
                                    pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
                                End If

                                Call PBP
                                Call CHANGEPOSS

                                currDown = 1
                                ydLine = 100 - ydsGained - ydLine
                                ydsToScore = 100 - ydLine

                                Call YARDLINE

                                ydsGained = Int(Rnd(1) * 25) + 1

                                If ydsGained >= 24 Then ydsGained = Int(Rnd(1) * 70) + 31

                                Call YARDAGE

                                X1 = 5
                                startYds = 100 - ydLine
                                endYds = startYds - ydsGained

                                Call MOVEBALL

                                pbpString$ = "RETURNED FOR "

                                If ydsGained + ydLine > 99.99 Then
                                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                    Call PBP

                                    'Touchdown!
                                    GoTo 3200

                                Else
                                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                    Call PBP

                                    playSegment = 5
                                    ydLine = ydLine + ydsGained
                                    ydsToFirst = 10
                                    If ydLine >= 90 Then ydsToFirst = 100 - ydLine

                                    GoSub 3900

                                    Call DELAY

                                End If

                            End If

                            GoTo 700

                        Else

                            If passIncomplete <> 1 Then

                                pbpString$ = "PASS IS COMPLETE FOR"

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Else
                                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                End If

                                Call PBP

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                                    'Safety!
                                    GoTo 3250
                                End If

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                    'Touchdown!
                                    GoTo 3200
                                End If

                                F% = 2

                                Call FUMBLE

                            End If

                            GoTo 3309

                        End If

                    End If

                    Call FAKERUN(choice)

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                        'Safety!
                        GoTo 3250
                    End If

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                        'Touchdown!
                        GoTo 3200
                    End If

                    F% = 2

                    Call FUMBLE

                    GoTo 3309

                Else

                    'Real Field Goal Attempt
                    GoTo 3602

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
    X = 2
    RN1 = Int(Rnd(1) * 1000) + 1

    If RN1 <= 50 + teamRat_GAME(D, 0) And playCall <> 13 And endZone = 0 Then

        pbpString$ = "DEFENSIVE PASS INTERFERENCE"

        If playCall = 9 Or playCall = 10 Or playCall = 13 Then
            ydsGained = Int(Rnd(1) * 10) + 1
        End If

        If playCall = 12 Or playCall = 15 Then
            ydsGained = Int(Rnd(1) * 20) + 21
        End If

        If playCall = 11 Or playCall = 14 Or playCall = 16 Or playCall = 17 Or playCall = 18 Then
            ydsGained = Int(Rnd(1) * 10) + 11
        End If

        If ruleOptType = 0 And ruleOptColl >= 3 And ydsGained > 15 Then
            ydsGained = 15
        End If

        gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

        Call YARDAGE

        If ydsGained + ydLine >= 99.99 Then

            If ruleOptType <> 0 Then
                gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 100 - ydLine - 1
                pbpString$ = pbpString$ + " IN THE END ZONE"
                Call PBP

                ydLine = 99
                ydsToFirst = 1
            Else
                gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 100 - ydLine - 2

                pbpString$ = pbpString$ + " IN THE END ZONE"
                Call PBP

                ydLine = 98
                ydsToFirst = 2
            End If

        Else

            gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + ydsGained
            pbpString$ = pbpString$ + " AT THE "

            If ydsGained + ydLine < 50 Then
                pbpString$ = pbpString$ + Str$(Int((ydsGained + ydLine) * 10 + .5) / 10)
            Else
                pbpString$ = pbpString$ + Str$(Int((100 - ydLine - ydsGained) * 10 + .5) / 10)
            End If

            pbpString$ = pbpString$ + " YARD LINE"
            Call PBP

            ydLine = ydLine + ydsGained
            ydsToFirst = 10

            If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        End If

        'Automatic first down
        currDown = 1

        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1
        playSegment = 4

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

        GoTo 700

    Else

        Call INCOMPLETE
        If endZone = 1 Then Call TryForTwoResult
        playSegment = 4

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

    Call DisplayField

    Call KickoffLine
    ydsToFirst = 10
    currDown = 1

    Call SCOREBOARD

    'Pro Overtime
    If ruleOptType > 0 And quarter > 4 Then
        GoTo 3364
    End If

    GoSub PointsAfterTD

    Call CHANGEPOSS

    playSegment = 4
    U3 = 9

    ydsToFirst = 10

    GoSub 3900

    U3 = 0

    Call SCOREBOARD

    'Lineup the kickoff
    Call KickoffLine

    Call SCOREBOARD

    If playType > 0 And playType < 3 And 100 - ydLine > 10 And ruleOptType <> 0 Then
        gameStatsTeam(D, 0) = gameStatsTeam(D, 0) + 1
    End If

    'Overtime
    If quarter > 4 Then 3364

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

    ydsToFirst = 10
    currDown = 1
    playSegment = 5
    X1 = 7

    Call SCOREUPD
    Call CHANGEPOSS

    I6 = 99
    U3 = 9

    GoSub 3900

    U3 = 0
    pbpString$ = "S A F E T Y  !!!"

    Call PBP

    Call DisplayField

    'Overtime
    If quarter > 4 Then 3364

    GoTo 340

    '----- NOODLE -----

    3303 '
    If Int(ydsToFirst * 10 + .5) / 10 < .05 Then
        GoTo 3335
    End If

    If currDown = 3 Then
        thirdDownAtt(P1) = thirdDownAtt(P1) + 1
    End If

    If currDown = 4 Then
        thirdDownFail(P1) = thirdDownFail(P1) + 1
    End If

    If currDown > 4 Then
        GoTo 3339
    End If

    'Restore this when done
    RN0 = Int(Rnd(1) * 1000) + 1

    If RN0 <= 6 + teamRat_GAME(D, 0) Then

        pbpString$ = "PENALTY AFTER WHISTLE - "

        RN1 = Int(Rnd(1) * 10) + 1

        If (ruleOptType > 0 And optRulePro = 0) Or (ruleOptType = 0 And optRuleColl = 0) Then
            'No facemask penalties before 1962!
            'It was simply illegal from 1955-1961 but not penalized
            If (RN1 >= 7 And RN1 <= 10) Then RN1 = 6
        End If

        If (ruleOptType > 0 And ruleOptPro >= 5) Or (ruleOptType = 0 And ruleOptColl >= 9) Then
            'Starting 2008, all facemasks are flagrant
            If (RN1 >= 7 And RN1 <= 9) Then RN1 = 10
        End If

        Select Case RN1

            Case 1

                pbpString$ = pbpString$ + "OFF - PERSONAL FOUL - "
                gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

                If ydLine < 30 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL"
                    ydsToFirst = ydsToFirst + ydLine / 2
                    ydLine = ydLine - ydLine / 2
                    gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
                Else
                    pbpString$ = pbpString$ + "15 YARDS"
                    ydsToFirst = ydsToFirst + 15
                    ydLine = ydLine - 15
                    gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 15
                End If

                autoFD = 1

                Call PBP

            Case 2 To 6

                pbpString$ = pbpString$ + "DEF - PERSONAL FOUL - "
                gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

                If 100 - ydLine < 30 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL"
                    ydsToFirst = ydsToFirst - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
                Else
                    pbpString$ = pbpString$ + "15 YARDS"
                    ydsToFirst = ydsToFirst - 15
                    ydLine = ydLine + 15
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 15
                End If

                autoFD = 1

                Call PBP

                'Can't end on defensive penalty
                If gameClock! <= 0 Then gameClock! = 1

            Case 7 To 9
                pbpString$ = pbpString$ + "DEF - INCIDENTAL FACEMASK - "
                gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

                If 100 - ydLine < 10 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL"
                    ydsToFirst = ydsToFirst - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
                Else
                    pbpString$ = pbpString$ + "5 YARDS"
                    ydsToFirst = ydsToFirst - 5
                    ydLine = ydLine + 5
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 5
                End If

                Call PBP

                'Can't end on defensive penalty
                If gameClock! <= 0 Then gameClock! = 1

            Case 10
                pbpString$ = pbpString$ + "DEF - FACEMASK - "
                gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

                If 100 - ydLine < 30 Then
                    'Half the distance to the goal
                    pbpString$ = pbpString$ + "1/2 DISTANCE TO GOAL + 1ST DOWN!"
                    ydsToFirst = ydsToFirst - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
                Else
                    pbpString$ = pbpString$ + "15 YARDS + 1ST DOWN!"
                    ydsToFirst = ydsToFirst - 15
                    ydLine = ydLine + 15
                    gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + 15
                End If

                Call PBP

                autoFD = 1

                'Can't end on defensive penalty
                If gameClock! <= 0 Then gameClock! = 1

        End Select

    Else

        GoSub 3900

        GoTo 700

    End If



    '----- NOODLE -----

    3309 '

    pbpString$ = "*** PENALTY - "

    RN4 = Int(Rnd(1) * 1000) + 1

    Select Case playType

        Case 1
            'Run Play ???
            Select Case RN4

                Case 1 To 7 + teamRat_GAME(P1, 0)
                    pbpString$ = pbpString$ + "OFF... ILLEGAL MOTION 5 YARDS ***"
                    penaltyYds = -5

                Case 7 + teamRat_GAME(P1, 0) To 25 + teamRat_GAME(P1, 0)
                    'Pro Rules before 1974 | College Rules before 1984
                    If (ruleOptType > 0 And ruleOptPro <= 1) Or (ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF... HOLDING 15 YARDS ***"
                        penaltyYds = -15
                    Else
                        pbpString$ = pbpString$ + "OFF... HOLDING 10 YARDS ***"
                        penaltyYds = -10
                    End If

                    holding = 1

                Case 25 + teamRat_GAME(P1, 0) To 42 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    penaltyYds = 5

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

        Case 2
            'Pass Play?

            Select Case RN4

                Case 1 To 17 + teamRat_GAME(P1, 0)

                    'Pro Rules before 1978 | College Rules before 1984
                    If (ruleOptType > 0 And ruleOptPro < 3) Or (ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF... HOLDING 15 YARDS ***"
                        penaltyYds = -15
                        holding = 1
                    Else
                        pbpString$ = pbpString$ + "OFF... HOLDING 10 YARDS ***"
                        penaltyYds = -10
                        holding = 1
                    End If

                Case 17 + teamRat_GAME(P1, 0) To 22 + teamRat_GAME(P1, 0)
                    pbpString$ = pbpString$ + "OFF... ILLEGAL MOTION 5 YARDS ***"
                    penaltyYds = -5

                Case 22 + teamRat_GAME(P1, 0) To 27 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    penaltyYds = 5

                Case 27 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0) To 42 + teamRat_GAME(P1, 0) + teamRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... HOLDING 5 YARDS - 1ST DOWN! ***"
                    autoFD = 1
                    penaltyYds = 5

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    ydsToFirst = ydsToFirst - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

    End Select

    'College 1991+ Holding penalty
    'Randomly determine the "infraction point"
    'for enforcing the penalty
    If ruleOptType = 0 And ruleOptColl >= 6 And holding = 1 Then
        If playType = 1 Then
            'Run Play - past scrimmage
            penaltyYds = penaltyYds + Int(Rnd(1) * 5) + 1
        End If
        If playType = 2 Then
            'Pass Play - behind scrimmage
            penaltyYds = penaltyYds - Int(Rnd(1) * 8) + 1
        End If
    End If

    pbpString$ = pbpString$ + " 1-ACCEPT 2-DECLINE "
    Call PBP

    'Reset for next penalty
    holding = 0

    'Determine how to handle PENALTIES
    'At this point, both penalty yardage and
    'yards gained from the play have been determined
    If playerMode = 0 Then

        I$ = GetKeyPress$

    Else

        'Computer Player - default to DECLINE
        penaltyChoice = 2

        'Computer Player
        If penaltyYds < 0 Then

            'On the Offense
            If playerMode = 1 And P1 = compTeam Then
                'Computer Offense | Human Defense
                Do
                    I$ = GetKeyPress$
                    penaltyChoice = Val(I$)
                Loop Until penaltyChoice = 1 Or penaltyChoice = 2
            Else
                '4th Down and didn't get a 1st  --OR--
                ' 3rd Down and we'll have <= 3 yds for 1st --OR--
                ' we didn't net any yards on the play
                If playerMode = 1 And ((currDown > 3 And ydsGained < ydsToFirst) Or (currDown = 3 And ydsToFirst - ydsGained > 3 Or ydsGained < 0)) Then
                    I$ = GetKeyPress$
                Else
                    penaltyChoice = 1
                End If
            End If

        Else

            'Penalty Yds > 0 = On the Defense
            If playerMode = 1 And D = compTeam Then
                'Human Offense | Computer Defense
                Do
                    I$ = GetKeyPress$
                    penaltyChoice = Val(I$)
                Loop Until penaltyChoice = 1 Or penaltyChoice = 2
            Else
                'If we didn't get the first down but would be granted it,
                'or we gained <= 5 yards [penalties give us 5+ yards]
                If autoFD = 1 And ydsToFirst - ydsGained > 0 Or (ydsGained <= 5) Then
                    penaltyChoice = 1
                End If
            End If

        End If

    End If


    If penaltyChoice = 1 Then

        'Accept

        playSegment = 5

        If ruleOptType = 0 And ruleOptColl = 0 Then
            'College Pre-1968
            playSegment = 1
        End If

        pbpString$ = "PENALTY ACCEPTED"
        Call PBP

        If penaltyYds > 0 Then 3330

        penaltyYds = penaltyYds * -1

        'OFF PEN
        3326 '
        If ydLine < penaltyYds * 2 Then
            ydsToFirst = ydsToFirst + ydLine / 2
            ydLine = ydLine - ydLine / 2
            gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
        Else
            ydsToFirst = ydsToFirst + penaltyYds
            ydLine = ydLine - penaltyYds
            gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + penaltyYds
        End If

        gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

        If Int(ydsToFirst * 10 + .5) / 10 < .05 Then 3335

    Else

        pbpString$ = "PENALTY DECLINED"

        Call PBP
        Call COMPILER

        autoFD = 0
        ydLine = ydLine + ydsGained
        ydsToFirst = ydsToFirst - ydsGained
        currDown = currDown + 1

        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1
        If playSegment < 4 Or playSegment > 5 Then playSegment = 1

        If Int((ydsToFirst) * 10 + .5) / 10 < .05 Then
            currDown = 1
            ydsToFirst = 10

            If 100 - ydLine < 9.95 Then ydsToFirst = 100 - ydLine
            If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then playSegment = 1

            gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

            If gameClock! <= 0 Then gameClock! = 1

        Else

            If currDown > 4 Then 3339

        End If

    End If

    GoSub 3900

    GoTo 700


    '----- NOODLE -----

    'DEFENSIVE PENALTY

    3330 '

    If 100 - ydLine < penaltyYds * 2 Then
        ydsToFirst = ydsToFirst - (100 - ydLine) / 2
        ydLine = ydLine + (100 - ydLine) / 2
        gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + (100 - ydLine) / 2
    Else
        ydsToFirst = ydsToFirst - penaltyYds
        ydLine = ydLine + penaltyYds
        gameStatsTeam(D, 12) = gameStatsTeam(D, 12) + penaltyYds
    End If

    gameStatsTeam(D, 11) = gameStatsTeam(D, 11) + 1

    If ydsToFirst < .01 Or autoFD = 1 Then

        autoFD = 0

        3335 '
        currDown = 1
        ydsToFirst = 10

        If 100 - ydLine < 9.95 Then
            ydsToFirst = 100 - ydLine
        End If

        If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then
            playSegment = 1
        End If

        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

    End If

    GoSub 3900

    If gameClock! <= 0 Then gameClock! = 1

    GoTo 700



    '----- NOODLE -----

    'College Overtime
    3339 '
    If NG%(14) = 3 And quarter > 4 Then
        GoTo 3364
    End If

    currDown = 1
    ydsToFirst = 10
    ydLine = 100 - ydLine

    If ydLine > 90 Then ydsToFirst = 100 - ydLine

    Call CHANGEPOSS

    playSegment = 5

    GoSub 3900

    GoTo 700

    '----- NOODLE -----

    '3349
    playSegment = 6

    If Int(ydsToFirst * 10 + .5) / 10 < .05 Then

        currDown = 1
        ydsToFirst = 10

        If 100 - ydLine < 9.95 Then ydsToFirst = 100 - ydLine
        If playSegment < 1 And ruleOptType = 0 And firstDownClockOpt = 0 Then playSegment = 1

        gameStatsTeam(P1, 0) = gameStatsTeam(P1, 0) + 1

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

    Else

        GoSub 3900

    End If

    GoTo 700


    '--------------------
    ' End Of Quarter
    '--------------------

    3360 '
    gameClock! = 0
    climate(0) = 1 - climate(0)
    gameClock! = 900

    quarter = quarter + 1

    If quarter = 3 Or quarter >= 5 Then
        'If end of half
        U3 = 0
    End If

    gameOver = 0

    'quarter - 1 = previous quarter
    'We've already incremented it

    Select Case quarter - 1

        Case 1, 3
            'quarter = 2 / 4
            'End of 1st / 3rd Quarter
            Call SCOREBOARD
            pbpString$ = "END OF QUARTER"
            Call PBP

            GoTo 700

        Case 2
            'quarter = 2 / 4
            'Half Time
            Call SCOREBOARD
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

            '--- start analysis here
            3364 '
            Call SCOREBOARD

            'Check if we're configured for overtime
            If NG%(14) = 0 Or (score(0, 0) <> score(1, 0) And NG%(14) <> 3) Or (score(0, 0) <> score(1, 0) And NG%(14) = 3 And (nbrPossOT(1) = 1 And nbrPossOT(0) = 1 Or nbrPossOT(1) = 0 And nbrPossOT(0) = 0)) Then

                'Nope!
                'Leave quarter at 4
                If isOT = 0 Then
                    quarter = quarter - 1
                End If

                pbpString$ = "END OF GAME"
                Call PBP
                Call GetKeyWhenNoDelay

                gameOver = 1

            End If

            '--- should this be outside the SELECT

            'Check if game is over
            If gameOver <> 1 Then

                'Game not over
                Call SCOREBOARD
                Call MARKER1

                'Determine if we should display stats
                Select Case NG%(14)

                    Case 1
                        'Show stats before overtime
                        I$ = "Y"

                        'Sudden Death 1 Quarter
                        'play overtime as 1 quarter with no overtime
                        NG%(14) = 0

                    Case 2
                        'Show stats before overtime
                        I$ = "Y"

                        'Sudden Death to Win
                        'keep playing quarters

                    Case 3
                        'Not showing stats

                        'Sudden Death
                        'College Rules
                        '1st and 10 from the 25
                        ydLine = 75
                        ydsToScore = 25
                        ydsToFirst = 10
                        currDown = 1

                        '1st Overtime
                        isOT = 1

                        'Track overtime possesions for each team
                        If nbrPossOT(0) = 1 And nbrPossOT(1) = 1 Then quarter = quarter + 1

                        If nbrPossOT(0) = 1 And nbrPossOT(1) = 1 Or nbrPossOT(0) = 0 And nbrPossOT(1) = 0 Then

                            Erase nbrPossOT
                            P1 = Int(Rnd(1) * 2)
                            D = 1 - P1
                            Call CHANGEPOSS
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

                        GoTo 700

                End Select

                'We're going into overtime
                If UCase$(I$) = "Y" Then

                    If quarter = 5 Then

                        P1 = Int(Rnd(1) * 2)

                        Call CHANGEPOSS

                        '1 timeout for each team
                        timeouts(0) = timeouts(0) + 1
                        timeouts(1) = timeouts(1) + 1

                        '1st and 10
                        currDown = 1
                        ydsToFirst = 10

                        'Starting yard line for possession
                        Call KickoffLine

                        If P1 = 0 Then D = 1
                        If P1 = 1 Then D = 0

                        isOT = 1

                        GoTo 340

                    Else

                        timeouts(0) = timeouts(0) + 1
                        timeouts(1) = timeouts(1) + 1
                        isOT = 1

                        GoTo 700

                    End If

                End If

                'Since this increases at end of quarter,
                'we need to decrease
                quarter = quarter - 1

                pbpString$ = "END OF GAME"
                Call PBP
                Call GetKeyWhenNoDelay
            End If

            '8540
            Call TICKERUPDATE

            '-- When would this NOT happen???
            If quarter <> 3 Then
                gameOver = 1
                Call EndOfGame
                Exit Sub
            Else
                GoTo 850
            End If

    End Select

    '----- NOODLE -----

    3552 '

    If defInts(1 - P1) > 0 Then
        Call QBTHROW
        Call FINDINT
        pbpString$ = "PASS IS INTERCEPTED BY " + defTeam$
    Else
        pbpString$ = "PASS IS INTERCEPTED"
    End If

    X = 0

    'Record INT's for Defense
    gameInts(1 - P1, Z1, 0) = gameInts(1 - P1, Z1, 0) + 1

    'Track INT's for team
    gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
    gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1

    If FF% <> 1 Then
        gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
        gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1
    End If

    If endZone = 1 Then
        'Intercepted in the end zone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        GoTo 6605

    End If

    Call AddPossessionTime(5, 8)

    If FF% <> 0 Then
        FF% = 0
    Else
        If playCall = 12 Or playCall = 15 Then ydsGained = Int(Rnd(1) * 20) + 21
        If playCall <> 12 And playCall <> 15 Then ydsGained = Int(Rnd(1) * 10) + 1
        If playCall = 13 Then ydsGained = ydsGained - 5
        If playCall = 11 Or playCall = 14 Or playCall = 17 Or playCall = 16 Or playCall = 18 Then ydsGained = ydsGained + 10
    End If

    Call YARDAGE

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
        'Intercepted doesn't leave the endzone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        'College overtime
        If NG%(14) = 3 And quarter > 4 Then
            GoTo 3364
        End If

        Call Touchback
        GoTo 700

    End If

    If ydsGained + ydLine > 49.99 Then
        pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
    Else
        pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
    End If

    Call PBP

    Call CHANGEPOSS

    currDown = 1
    ydLine = 100 - ydsGained - ydLine
    ydsToScore = 100 - ydLine

    Call YARDLINE

    ydsGained = Int(Rnd(1) * 25) + 1

    If ydsGained >= 24 Then
        ydsGained = Int(Rnd(1) * 70) + 31
    End If

    Call YARDAGE

    X1 = 5
    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    pbpString$ = "RETURNED FOR "

    If ydsGained + ydLine > 99.99 Then

        pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"

        'Record INT TDs, Yds
        gameInts(P1, Z1, 2) = gameInts(P1, Z1, 2) + 1
        gameInts(P1, Z1, 1) = gameInts(P1, Z1, 1) + 100 - ydLine

        If 100 - ydLine > IR(P1, Z1) Then
            gameIR$(P1, Z1) = "t"
            IR(P1, Z1) = 100 - ydLine
        End If

        Call PBP

        'Touchdown!
        GoTo 3200

    Else

        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
        Call PBP

        playSegment = 5
        ydsToFirst = 10
        ydLine = ydLine + ydsGained

        'Record INT Yds
        gameInts(P1, Z1, 1) = gameInts(P1, Z1, 1) + ydsGained

        If ydsGained > IR(P1, Z1) Then
            gameIR$(P1, Z1) = ""
            IR(P1, Z1) = ydsGained
        End If

        If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        'College overtime
        If NG%(14) = 3 And quarter > 4 Then
            GoTo 3364
        End If

        F% = 5

        Call FUMBLE

        If ballFumbled = 1 Then Call FumblePBP: GoTo 650

        RN1 = Int(Rnd(1) * 1000) + 1

        If RN1 > 950 - teamRat_GAME(P1, 0) Then

            pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
            Call PBP

            'Record penalty
            gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

            'Record yds
            If ydLine < 20 Then
                '1/2 the distance to the goal
                ydLine = ydLine / 2
                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
            Else
                '10 yds
                ydLine = ydLine - 10
                gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 10
            End If

        End If

        GoSub 3900

        GoTo 700

    End If


    '----- NOODLE -----

    3900 '
    'Adjust possession time, check for special scenarios
    '(ie, hurry up, 2-minute warning, etc.)

    '--> Convert to a subroutine
    '--> takes a parameter regarding gameClock
    '--> then returns that so we can issue the following afterwards
    '       If gameClock! = 0 Then GoTo 3360

    If playSegment <> 8 Then

        'Pro Rules
        If ruleOptType <> 0 Then

            If gameClock! > 120.01 And quarter <> 1 And quarter <> 3 And Not (gameClock! > 159.99 And playSegment = 0) And Not (gameClock! > 147 And playSegment = 1) And Not (gameClock! > 142 And playSegment = 2) And Not (gameClock! > 137 And playSegment = 3) And Not (gameClock! > 126 And playSegment = 4) And Not (gameClock! > 128 And playSegment = 5) And Not (gameClock! > 130 And playSegment = 6) And Not (gameClock! > 132 And playSegment = 7) Then

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

            End If

        End If

        If (gameClock! <= 120 And quarter = 2) Or (gameClock! <= 120 And quarter >= 4) Then

            If playSegment < 4 Then

                Call CheckGameClock
                If gameClock! = 0 Then GoTo 3360
                Call TwoMinMenu(I1)

                Select Case I1

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
                        pbpString$ = "HE STEPS BACK AND THROWS IT OUT OF BOUNDS"
                        Call PBP

                        'Increase pass attempts
                        gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                        gameStatsPlayer(P1, qbNumber(P1), 9) = gameStatsPlayer(P1, qbNumber(P1), 9) + 1

                        currDown = currDown + 1
                        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        Call AddPossessionTime(7, 10)
                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Return 'aka Exit Sub

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
            'playSegment > 0
            If playSegment >= 1 And playSegment <= 3 Then
                timeElapsed! = 27 - (playSegment * 5) + (Int(Rnd(1) * 5) + 1)
            Else
                'playSegment >= 4
                timeElapsed! = (2 * playSegment) - 5 + (Int(Rnd(1) * 3) + 1)
            End If

            gameClock! = gameClock! - timeElapsed!
            timePoss(P1) = timePoss(P1) + timeElapsed!

        End If

        Call CheckGameClock
        If gameClock! = 0 Then GoTo 3360

    End If

    Return


    '----- NOODLE -----

    3602 'FIELD GOAL

    If ydLine >= 97 + goalPostAdj Then
        fgSuccessChance = kickerIdx(P1, activeKicker(P1))
    Else
        Call FieldGoalChance
    End If

    pbpString$ = kicker$(P1, activeKicker(P1)) + " TROTS ON TO TRY A FIELD GOAL"
    Call PBP

    pbpString$ = "THIS WILL BE A" + Str$(Int(117 + goalPostAdj - ydLine + .5)) + " YARD ATTEMPT"
    Call PBP

    pbpString$ = "HE WAITS FOR THE SNAP"
    Call PBP

    '.33% chance of bad snap
    I5 = Int(Rnd(1) * 300) + 1

    If I5 = 12 Then

        pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
        Call PBP
        ydsGained = (Int(Rnd(1) * 9) + 6) * -1

        Call AddPossessionTime(4, 7)

        gameStatsTeam(P1, 10) = gameStatsTeam(P1, 10) + 1
        gameStatsTeam(P1, 9) = gameStatsTeam(P1, 9) + 1

        If ydLine + ydsGained < -9.999 Then
            score(D, quarter) = score(D, quarter) + 2
            score(D, 0) = score(D, 0) + 2
            GoTo 3251
        End If

        If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

            If d100% <= 50 Then

                score(D, quarter) = score(D, quarter) + 2
                score(D, 0) = score(D, 0) + 2

                GoTo 3251

            Else

                X1 = 10
                Call CHANGEPOSS

                'Touchdown!
                GoTo 3200

            End If

            Call CHANGEPOSS
            currDown = 1
            ydsToFirst = 10
            ydLine = 100 - ydLine

            If ydsToFirst >= 90 Then ydsToFirst = 100 - ydLine

            If d100% <= 50 Then
                pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
            Else
                pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
            End If

            If ydLine > 50 Then
                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
            Else
                pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
            End If

            playSegment = 5

            GoSub 3900

            GoTo 700

        End If

    Else

        Call Rolld100(blockChance)

        'Increase FGA
        gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 4 + activeKicker(P1) * 4, 17) + 1

        If fgDefChoice = 1 And ((ydLine >= 68 + goalPostAdj And blockChance >= 97) Or (ydLine < 68 + goalPostAdj And blockChance >= 99)) Then
            'Blocked FG
            'Call pbplog(10916)
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

                Call CHANGEPOSS

                If d100% <= 50 Then
                    pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
                Else
                    pbpString$ = gameMascots$(P1) + " RECOVER IT IN THE END ZONE!!"
                End If

                Call PBP

                X1 = 6

            Else

                If d100% <= 94 Then

                    Call CHANGEPOSS

                    ydsToFirst = 10

                    If ydLine >= 90 Then
                        ydsToFirst = 100 - ydLine
                    End If

                    currDown = 1

                    Call DELAY

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                    GoTo 700

                Else

                    Call CHANGEPOSS

                    ydsGained = 100 - ydLine
                    startYds = 100 - ydLine
                    endYds = startYds - ydsGained

                    Call MOVEBALL

                    If d100% <= 50 Then
                        pbpString$ = gameTeams$(P1) + " TAKES IT INTO THE END ZONE!!"
                    Else
                        pbpString$ = gameMascots$(P1) + " TAKE IT INTO THE END ZONE!!"
                    End If

                    Call PBP

                    X1 = 6

                End If

            End If

            'Touchdown!
            GoTo 3200

        Else

            'Field Goal Attempt
            Call pbplog(10986)
            pbpString$ = "THE KICK IS UP AND..."
            Call PBP

            If climate(0) <> P1 Then
                fgSuccessChance = fgSuccessChance - Int(Rnd(1) * 7) + 3 - climate(1)
            Else
                fgSuccessChance = fgSuccessChance + Int(Rnd(1) * 7) - 3 + climate(1)
            End If

            Call FGATT

            If fgChance <= fgSuccessChance Then

                'Increase FGM
                gameStatsPlayer(P1, 3 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 3 + activeKicker(P1) * 4, 17) + 1
                X1 = 8
                I1 = activeKicker(P1)

                Call SCOREUPD

                currDown = 1
                ydsToFirst = 10
                score(P1, 0) = score(P1, 0) + 3
                score(P1, quarter) = score(P1, quarter) + 3

                Call FGMADE
                Call AddPossessionTime(4, 7)
                Call ITSGOOD
                Call DisplayField
                Call CHANGEPOSS

                Call SCOREBOARD

                Call KickoffLine

                Call SCOREBOARD

                Call CheckGameClock
                If gameClock! = 0 Then
                    U3 = 9
                    GoTo 3360
                End If

                If quarter > 4 Then 3364

                GoTo 340

            Else

                Call MISSEDFG
                currDown = 1
                ydsToFirst = 10

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

                'College overtime
                If quarter > 4 And NG%(14) = 3 Then 3364

            End If 'Done checking FG success

        End If ' Done handling successful FG

    End If ' Done checking for bad snap

    GoTo 700


    '----- NOODLE -----


    3840 '
    '-- We need to track how often punts are blocked
    'Call pbplog(8759)
    gameStatsTeam(P1, 28) = gameStatsTeam(P1, 28) + 1
    pbpString$ = "KICK IS BLOCKED!!!"

    Call PBP

    Call AddPossessionTime(4, 7)

    ydsGained = Int(Rnd(1) * 30) * -1

    Call YARDAGE
    ydLine = 100 - ydLine - ydsGained

    If ydLine >= 109.949 Then
        'Safety!
        GoTo 3250
    End If

    If ydLine >= 100 Then
        Call CHANGEPOSS

        pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
        Call PBP

        X1 = 6

        'Touchdown!
        GoTo 3200

    End If

    If d100% > 60 Then

        Call CHANGEPOSS
        ydsGained = 100 - ydLine
        startYds = 100 - ydLine
        endYds = startYds - ydsGained

        Call MOVEBALL

        pbpString$ = gameTeams$(P1) + " TAKES THE BALL INTO THE END ZONE!!"
        Call PBP

        X1 = 6

        'Touchdown!
        GoTo 3200

    Else

        Call CHANGEPOSS: ydsToFirst = 10

        If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        currDown = 1

        Call DELAY

        If gameClock! = 0 Then GoTo 3360

    End If

    GoTo 700


    '----- NOODLE -----


    'Pass Intercepted

    3876 '
    X = 0

    'Track attempt and interception
    gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
    gameStatsTeam(P1, 5) = gameStatsTeam(P1, 5) + 1
    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
    gameStatsPlayer(P1, I1, 11) = gameStatsPlayer(P1, I1, 11) + 1

    Call AddPossessionTime(5, 8)

    ydsGained = Int(Rnd(1) * 10) + 1
    Call YARDAGE

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

        'Interception in the end zone
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP
        Call Touchback

    Else

        If ydsGained + ydLine > 49.99 Then
            pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE"
        End If
        If ydsGained + ydLine < 49.99 Then
            pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE"
        End If

        Call PBP

        Call CHANGEPOSS

        currDown = 1
        ydLine = 100 - ydsGained - ydLine
        ydsToScore = 100 - ydLine

        Call YARDLINE

        ydsGained = Int(Rnd(1) * 25) + 1

        If ydsGained >= 24 Then
            ydsGained = Int(Rnd(1) * 70) + 31
        End If

        Call YARDAGE

        X1 = 5
        startYds = 100 - ydLine
        endYds = startYds - ydsGained

        Call MOVEBALL

        pbpString$ = "RETURNED FOR "

        If ydsGained + ydLine > 99.99 Then

            pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
            Call PBP

            'Touchdown!
            GoTo 3200

        Else

            pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
            Call PBP

            playSegment = 5
            ydLine = ydLine + ydsGained
            ydsToFirst = 10

            If ydLine >= 90 Then ydsToFirst = 100 - ydLine

        End If

        GoSub 3900

        Call DELAY

    End If

    GoTo 700

    '----- NOODLE -----

    'start of a subroutine??

    6605 '
    Call CHANGEPOSS

    ydLine = 1
    ydsToScore = 100 - ydLine

    Call YARDLINE

    If d100% <= 18 Then
        ydsGained = 110
    Else
        ydsGained = Int(Rnd(1) * 21) + 50
    End If

    Call YARDAGE

    X1 = 12
    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    pbpString$ = "RETURNED FOR "

    If ydsGained + ydLine > 99.99 Then

        pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
        Call PBP

        score(P1, quarter) = score(P1, quarter) + 2
        score(P1, 0) = score(P1, 0) + 2

        pbpString$ = "GOOD FOR 2 POINTS!!"
        Call PBP

        Call DisplayField
        Call SCOREUPD

    Else

        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
        Call PBP

    End If

    endZone = 0
    Call CHANGEPOSS

    '--- end of a subroutine

    GoTo 700

    '----- NOODLE -----

    850 '
    'This could be a small sub
    'if other places don't need
    'to go to label 330
    Call TICKER

    If halfTime = 1 Then

        halfTime = 0

        'PAH

        If autoPlay <> 1 Then
            For I3 = 0 To 1:
                Call CHOOSEQB(I3)
            Next I3
        End If

        Randomize Timer

        Call SCOREBOARD

        GoTo 330

    Else

        If gameOver = 1 Then
            Call EndOfGame
            Exit Sub
        Else
            GoTo 700
        End If

    End If

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

    Locate 24, 1
    Print Tab(80); " ";
    Locate , 1
    Print "    LANCE HAFFNER / JASON LEONARD  1986-2024"; Tab(70); Left$(Time$, 5);
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

    Shared tradeLPR$(), oppName$(), locIndicator$(), gameSite$()
    Shared teamIndicator$()
    Shared tradeLFG$(), tradeLInt$(), tradeLKR$(), tradeLPass$()

    'Shared eventSettings()
    Shared oppScore(), teamScore(), powerRating()

    Shared gameAttendance&()

    If ruleOptType <> 0 Then
        For I = 1 To 10
            If Left$(defFormation$(I), 3) = "5-2" Then defFormation$(I) = "3-4" + Right$(defFormation$(I), Len(defFormation$(I)) - 3)
            If Left$(defPlay$(I), 3) = "5-2" Then defPlay$(I) = "3-4" + Right$(defPlay$(I), Len(defPlay$(I)) - 3)
        Next
    End If

    For J1 = 1 To 15:
        defPlay$(J1) = defFormation$(J1)
    Next

    For J1 = 12 To 15:
        defPlay$(J1) = defPlay$(J1 - 7)
    Next

    defPlay$(5) = defPlay$(9)
    defPlay$(6) = defPlay$(10)

    For J1 = 12 To 15:
        defPlay$(J1 - 5) = defPlay$(J1)
    Next

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

    '-- For future releases, let's not initialize
    '-- anything that WILL get assigned by the interface
    autoPlay = 0
    ballFumbled = 0

    'Set this to 1 for a black/white experience
    BW = 0

    catchPctSuccess = 0
    chosenPlay = 0
    CP = 0
    compTeam = 0
    currDown = 0
    D = 0
    dLine = 0
    coverage = 0
    endAllGames = 0
    gameOver = 0
    endYds = 0
    endZone = 0
    expCompPct = 0
    fgAttYds = 0
    ffPctSuccess = 0
    gameLoc = 0
    goalPostAdj = 0
    halfTime = 0

    isOT = 0
    JJ = 0
    K3 = 0
    fieldSide = 0
    kickYL = 0
    playSegment = 0
    overtimeOpt = 0
    nbrScores = 0
    playerMode = 0
    playerOpt = 0
    P1 = 0
    playCall = 0
    periodNbr = 0
    puntRetNbr = 0
    quarter = 0
    qback = 0
    qbTakeKnee = 0
    revFumble = 0
    ruleOptColl = 0
    ruleOptPro = 0
    ruleOptType = 0
    playType = 0
    rushYds = 0
    S6 = 0
    startYds = 0
    tickerGames = 0
    W5 = 0
    WE = 0
    WS = 0
    winTeam = 0
    yrdLine = 0
    X = 0
    X1 = 0
    XD = 0
    XE = 0
    ydsToFirst = 0
    YC = 0
    YF = 0
    Z1 = 0
    ydsGained = 0
    ydLine = 0
    ydLineTeam = 0
    ydsToScore = 0
    gameOver = 0
    F% = 0
    FF% = 0
    goalLnYdAdj = 0
    HB% = 0
    intChance = 0
    indRushPct = 0
    PS% = 0
    firstDownClockOpt = 0

    gameClock! = 0
    pbpDelay! = 0
    timeElapsed! = 0

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
    Erase kickerFGA, kickerFGPct, kickerIdx, kickerPATPct
    Erase krNumRet, krYdsPerRet, leagRat_GAME
    Erase playerInts, playerSacks, prNumRet, prYdsPerRet, ydsPerPunt
    Erase qbArmRat, qbCompPct, qbNumAtt, qbPctInt
    Erase rbNumRec, rbrushContrib, rbRushAvg, rbYdsPerC
    Erase teamInts, teamSacks ', teamRat_GAME
    Erase wrNumRec, wrYdsPerC, ydsPerComp


    Erase defInts, defSacks
    'Erase eventSettings
    Erase FA, FM
    Erase sackStatsPlayer, gameStatsTeam, gameStatsPlayer
    Erase gameLongFG
    Erase K1, K3
    Erase gameInts, hasRunFF, playerInts, IR
    Erase kickReturners


    Erase gameFGA!, gameFGM!
    Erase gameTeamStat!, gameOppStat!, gamePlayerStat!
    Erase gameLRec!, gameLFG!, gameLInt!, gameLKR!, gameLPass!, gameLRush!
    Erase gameLPR!

    Erase nbrPossOT, puntReturners

    Erase oppScore, powerRating, teamScore

    Erase quarterbacks, qbNumber, qbMobility, runBacks
    Erase score, scoreQuarters, scoreTimes, schedGame
    Erase teamIdx_GAME, timeouts, timePoss
    Erase totalInts, totalSacks
    Erase useRandomQB, sackStatsTeam, WR, XD

    Erase HB%, NG%, indRushPct
    Erase activeKicker, RV%, passCovAdj, ST%, SX%
    Erase teamYears, thirdDownAtt, thirdDownFail
    Erase qbRushIdx, climate

    Erase diskIDs$, gameTeams$
    Erase gameIR$, gameKR$, gameLRec$, gameLRush$
    Erase tradeLPR$, gamePR$, gameQB$
    Erase gameRB$, gameSite$, gameWR$
    Erase locIndicator$, oppName$
    Erase SX$
    Erase tradeLFG$, tradeLInt$, tradeLKR$, tradeLPass$


End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP (validTeam)

    validTeam = 0

    For tgtTeam = 0 To 1

        tgtId = NG%(tgtTeam)
        tgtYear$ = RTrim$(diskIDs$(tgtTeam))

        Call ReadGameTeam(tgtYear$, gameTeams$(tgtTeam), tgtId, tgtTeam, validTeam)

        teamIdx_GAME(tgtTeam) = NG%(tgtTeam)
        X = 0

        If tgtId = 0 Then
            validTeam = 0
        End If

    Next tgtTeam

    If validTeam = 1 Then

        'Assign settings from schedule / etc.
        gameLoc = NG%(2)
        ruleOptType = NG%(3)
        pbpDelay! = NG%(4) / 10
        ruleOptColl = NG%(7)
        playerOpt = NG%(10)
        ruleOptPro = NG%(11)
        tickerStart = NG%(12)

        'Pro Rues, pre-1974
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

    J = Val(promptVal$)

    Select Case UCase$(promptVal$)

        Case "A": J = 10
        Case "B": J = 11
        Case "C": J = 12
        Case "D": J = 13
        Case "E": J = 14
        Case "F": J = 15
        Case "G": J = 16
        Case "H": J = 17

    End Select

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
'            Rolld100 Subroutine
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
'             PBP Subroutine
'----------------------------------------
'This is the classic Haffner Play-by-play
'subroutine. Given the target text, it will
'place it in the correct spot in the pbp
'text window. No text is placed if the
'delay is 0.
Sub PBP ()

    M$ = InKey$

    If UCase$(M$) = "O" Then
        Call OPTIONS
    End If

    If UCase$(M$) = "Q" Then
        Cls
        End
    End If

    If pbpDelay! > 0 Then
        'If autoPlay <> 1 Then
        Call PLACEPBP
        Call DELAY
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

            If ruleOptType = 0 Then
                If d100% <= 50 Then
                    Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD"
                Else
                    Locate , 28: Print "RED MAN HALFTIME REPORT"
                End If
            Else
                If d100% <= 50 Then
                    Locate , 29: Print "TOYOTA HALFTIME REPORT"
                Else
                    Locate , 29: Print "HONDA HALFTIME REPORT"
                End If
            End If

        Else

            If gameOver = 1 Then

                If ruleOptType = 0 Then

                    Locate , 27: Print "CHEVROLET FINAL SCOREBOARD"

                Else
                    Select Case RN0
                        Case 1 To 33:
                            Locate , 27: Print "USA TODAY FINAL SCOREBOARD"

                        Case 34 To 67:
                            Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD"

                        Case 67 To 100:
                            Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD"

                    End Select

                End If

            Else

                If halfTime = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD"

            End If

        End If

        Color 15

        If tickerGames = 0 Then
            Locate 10, 28: Print "NO OTHER GAMES SCHEDULED"
        Else
            If quarter = 1 And gameClock! < 900 And gameClock! >= 600 Then timeInterval = 1
            If quarter = 1 And gameClock! < 600 And gameClock! >= 300 Then timeInterval = 2
            If quarter = 1 And gameClock! < 300 Then timeInterval = 3

            If quarter = 2 And gameClock! < 900 And gameClock! >= 600 Then timeInterval = 4
            If quarter = 2 And gameClock! < 600 And gameClock! >= 300 Then timeInterval = 5
            If quarter = 2 And gameClock! < 300 Then timeInterval = 6

            If quarter = 3 And gameClock! < 900 And gameClock! >= 600 Then timeInterval = 9
            If quarter = 3 And gameClock! < 600 And gameClock! >= 300 Then timeInterval = 10
            If quarter = 3 And gameClock! < 300 Then timeInterval = 11

            If quarter = 4 And gameClock! < 900 And gameClock! >= 600 Then timeInterval = 12
            If quarter = 4 And gameClock! < 600 And gameClock! >= 300 Then timeInterval = 13
            If quarter = 4 And gameClock! < 300 Then timeInterval = 14

            If quarter = 5 And gameClock! < 900 And gameClock! >= 600 Then timeInterval = 15
            If quarter = 5 And gameClock! < 600 And gameClock! >= 300 Then timeInterval = 16
            If quarter = 5 And gameClock! < 300 Then timeInterval = 17

            If halfTime = 1 Then timeInterval = 8

            If Left$(SX$(1, 0), 3) <> "XXX" Then
                Locate 2, 1
                X = 1
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(1, 0), 13);
                Locate , 15
                Print Using "## "; SX%(1, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(1, 1), 13);
                Locate , 15: Print Using "## #"; SX%(1, 1, tickInterval)
            End If

            If Left$(SX$(2, 0), 3) <> "XXX" Then
                Locate 5, 1
                X = 2
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(2, 0), 13);
                Locate , 15: Print Using "## "; SX%(2, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(2, 1), 13);
                Locate , 15: Print Using "## #"; SX%(2, 1, tickInterval)
            End If

            If Left$(SX$(3, 0), 3) <> "XXX" Then
                Locate 8, 1
                X = 3
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(3, 0), 13);
                Locate , 15: Print Using "## "; SX%(3, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(3, 1), 13);
                Locate , 15: Print Using "## #"; SX%(3, 1, tickInterval)
            End If

            If Left$(SX$(4, 0), 3) <> "XXX" Then
                Locate 11, 1
                X = 4
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(4, 0), 13);
                Locate , 15: Print Using "## "; SX%(4, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(4, 1), 13);
                Locate , 15: Print Using "## #"; SX%(4, 1, tickInterval)
            End If

            If Left$(SX$(5, 0), 3) <> "XXX" Then
                Locate 14, 1
                X = 5
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(5, 0), 13);
                Locate , 15: Print Using "## "; SX%(5, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(5, 1), 13);
                Locate , 15: Print Using "## #"; SX%(5, 1, tickInterval)
            End If

            If Left$(SX$(6, 0), 3) <> "XXX" Then
                Locate 17, 1
                X = 6
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(6, 0), 13);
                Locate , 15: Print Using "## "; SX%(6, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(6, 1), 13);
                Locate , 15: Print Using "## #"; SX%(6, 1, tickInterval)
            End If

            If Left$(SX$(7, 0), 3) <> "XXX" Then
                Locate 20, 1
                X = 7
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(7, 0), 13);
                Locate , 15: Print Using "## "; SX%(7, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(7, 1), 13);
                Locate , 15: Print Using "## #"; SX%(7, 1, tickInterval)
            End If

            If Left$(SX$(8, 0), 3) <> "XXX" Then
                Locate 23, 1
                X = 8
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(8, 0), 13);
                Locate , 15: Print Using "## "; SX%(8, 0, tickInterval);
                Print tickerPeriod$(tickInterval)

                Print Left$(SX$(8, 1), 13);
                Locate , 15: Print Using "## #"; SX%(8, 1, tickInterval);
            End If

            If Left$(SX$(9, 0), 3) <> "XXX" Then
                Locate 2, 21
                X = 9
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(9, 0), 13);
                Locate , 35: Print Using "## "; SX%(9, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(9, 1), 13);
                Locate , 35: Print Using "## #"; SX%(9, 1, tickInterval)
            End If

            If Left$(SX$(10, 0), 3) <> "XXX" Then
                Locate 5, 21
                X = 10
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(10, 0), 13);
                Locate , 35: Print Using "## "; SX%(10, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(10, 1), 13);
                Locate , 35: Print Using "## #"; SX%(10, 1, tickInterval)
            End If

            If Left$(SX$(11, 0), 3) <> "XXX" Then
                Locate 8, 21
                X = 11
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(11, 0), 13);
                Locate , 35: Print Using "## "; SX%(11, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(11, 1), 13);
                Locate , 35: Print Using "## #"; SX%(11, 1, tickInterval)
            End If

            If Left$(SX$(12, 0), 3) <> "XXX" Then
                Locate 11, 21
                X = 12
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(12, 0), 13);
                Locate , 35: Print Using "## "; SX%(12, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(12, 1), 13);
                Locate , 35: Print Using "## #"; SX%(12, 1, tickInterval)
            End If

            If Left$(SX$(13, 0), 3) <> "XXX" Then
                Locate 14, 21
                X = 13
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(13, 0), 13);
                Locate , 35: Print Using "## "; SX%(13, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(13, 1), 13);
                Locate , 35: Print Using "## #"; SX%(13, 1, tickInterval)
            End If

            If Left$(SX$(14, 0), 3) <> "XXX" Then
                Locate 17, 21
                X = 14
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(14, 0), 13);
                Locate , 35: Print Using "## "; SX%(14, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(14, 1), 13);
                Locate , 35: Print Using "## #"; SX%(14, 1, tickInterval)
            End If

            If Left$(SX$(15, 0), 3) <> "XXX" Then
                Locate 20, 21
                X = 15
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(15, 0), 13);
                Locate , 35: Print Using "## "; SX%(15, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(15, 1), 13);
                Locate , 35: Print Using "## #"; SX%(15, 1, tickInterval)
            End If

            If Left$(SX$(16, 0), 3) <> "XXX" Then
                Locate 23, 21
                X = 16
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(16, 0), 13);
                Locate , 35: Print Using "## "; SX%(16, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 21

                Print Left$(SX$(16, 1), 13);
                Locate , 35: Print Using "## #"; SX%(16, 1, tickInterval);
            End If

            If Left$(SX$(17, 0), 3) <> "XXX" Then
                Locate 2, 41
                X = 17
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(17, 0), 13);
                Locate , 55: Print Using "## "; SX%(17, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(17, 1), 13);
                Locate , 55: Print Using "## #"; SX%(17, 1, tickInterval)
            End If

            If Left$(SX$(18, 0), 3) <> "XXX" Then
                Locate 5, 41
                X = 18
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(18, 0), 13);
                Locate , 55: Print Using "## "; SX%(18, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(18, 1), 13);
                Locate , 55: Print Using "## #"; SX%(18, 1, tickInterval)
            End If

            If Left$(SX$(19, 0), 3) <> "XXX" Then
                Locate 8, 41
                X = 19
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(19, 0), 13);
                Locate , 55: Print Using "## "; SX%(19, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(19, 1), 13);
                Locate , 55: Print Using "## #"; SX%(19, 1, tickInterval)
            End If

            If Left$(SX$(20, 0), 3) <> "XXX" Then
                Locate 11, 41
                X = 20
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(20, 0), 13);
                Locate , 55: Print Using "## "; SX%(20, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(20, 1), 13);
                Locate , 55: Print Using "## #"; SX%(20, 1, tickInterval)
            End If

            If Left$(SX$(21, 0), 3) <> "XXX" Then
                Locate 14, 41
                X = 21
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(21, 0), 13);
                Locate , 55: Print Using "## "; SX%(21, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(21, 1), 13);
                Locate , 55: Print Using "## #"; SX%(21, 1, tickInterval)
            End If

            If Left$(SX$(22, 0), 3) <> "XXX" Then
                Locate 17, 41
                X = 22
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(22, 0), 13);
                Locate , 55: Print Using "## "; SX%(22, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(22, 1), 13);
                Locate , 55: Print Using "## #"; SX%(22, 1, tickInterval)
            End If

            If Left$(SX$(23, 0), 3) <> "XXX" Then
                Locate 20, 41
                X = 23
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(23, 0), 13);
                Locate , 55: Print Using "## "; SX%(23, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(23, 1), 13);
                Locate , 55: Print Using "## #"; SX%(23, 1, tickInterval)
            End If

            If Left$(SX$(24, 0), 3) <> "XXX" Then
                Locate 23, 41
                X = 24
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(24, 0), 13);
                Locate , 55: Print Using "## "; SX%(24, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 41

                Print Left$(SX$(24, 1), 13);
                Locate , 55: Print Using "## #"; SX%(24, 1, tickInterval);
            End If

            If Left$(SX$(25, 0), 3) <> "XXX" Then
                Locate 2, 61
                X = 25
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(25, 0), 13);
                Locate , 75: Print Using "## "; SX%(25, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(25, 1), 13);
                Locate , 75: Print Using "## #"; SX%(25, 1, tickInterval)
            End If

            If Left$(SX$(26, 0), 3) <> "XXX" Then
                Locate 5, 61
                X = 26
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(26, 0), 13);
                Locate , 75: Print Using "## "; SX%(26, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(26, 1), 13);
                Locate , 75: Print Using "## #"; SX%(26, 1, tickInterval)
            End If

            If Left$(SX$(27, 0), 3) <> "XXX" Then
                Locate 8, 61
                X = 27
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(27, 0), 13);
                Locate , 75: Print Using "## "; SX%(27, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(27, 1), 13);
                Locate , 75: Print Using "## #"; SX%(27, 1, tickInterval)
            End If

            If Left$(SX$(28, 0), 3) <> "XXX" Then
                Locate 11, 61
                X = 28
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(28, 0), 13);
                Locate , 75: Print Using "## "; SX%(28, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(28, 1), 13);
                Locate , 75: Print Using "## #"; SX%(28, 1, tickInterval)
            End If

            If Left$(SX$(29, 0), 3) <> "XXX" Then
                Locate 14, 61
                X = 29
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(29, 0), 13);
                Locate , 75: Print Using "## "; SX%(29, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(29, 1), 13);
                Locate , 75: Print Using "## #"; SX%(29, 1, tickInterval)
            End If

            If Left$(SX$(30, 0), 3) <> "XXX" Then
                Locate 17, 61
                X = 30
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(30, 0), 13);
                Locate , 75: Print Using "## "; SX%(30, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(30, 1), 13);
                Locate , 75: Print Using "## #"; SX%(30, 1, tickInterval)
            End If

            If Left$(SX$(31, 0), 3) <> "XXX" Then
                Locate 20, 61
                X = 31
                tickInterval = TickerTime%(timeInterval, X)

                Print Left$(SX$(31, 0), 13);
                Locate , 75: Print Using "## "; SX%(31, 0, tickInterval);
                Print tickerPeriod$(tickInterval)
                Locate , 61

                Print Left$(SX$(31, 1), 13);
                Locate , 75: Print Using "## #"; SX%(31, 1, tickInterval)
            End If

            If Left$(SX$(32, 0), 3) <> "XXX" Then
                Locate 23, 61
                X = 32
                tickInterval = TickerTime%(timeInterval, X)

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

        If quarter = 1 And gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 1) = -1 Then SX%(schedGame(2) + 1, 0, 1) = score(0, 0): SX%(schedGame(2) + 1, 1, 1) = score(1, 0)
        If quarter = 1 And gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 2) = -1 Then SX%(schedGame(2) + 1, 0, 2) = score(0, 0): SX%(schedGame(2) + 1, 1, 2) = score(1, 0)

        If quarter = 2 And gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 4) = -1 Then SX%(schedGame(2) + 1, 0, 4) = score(0, 0): SX%(schedGame(2) + 1, 1, 4) = score(1, 0)
        If quarter = 2 And gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 5) = -1 Then SX%(schedGame(2) + 1, 0, 5) = score(0, 0): SX%(schedGame(2) + 1, 1, 5) = score(1, 0)

        If quarter > 2 And SX%(schedGame(2) + 1, 0, 3) = -1 Then SX%(schedGame(2) + 1, 0, 3) = score(0, 1): SX%(schedGame(2) + 1, 1, 3) = score(1, 1)
        If quarter > 2 And SX%(schedGame(2) + 1, 0, 6) = -1 Then For I2 = 6 To 8: SX%(schedGame(2) + 1, 0, I2) = score(0, 2) + score(0, 1): SX%(schedGame(2) + 1, 1, I2) = score(1, 2) + score(1, 1): Next

        If quarter = 3 And gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 9) = -1 Then SX%(schedGame(2) + 1, 0, 9) = score(0, 0): SX%(schedGame(2) + 1, 1, 9) = score(1, 0)
        If quarter = 3 And gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 10) = -1 Then SX%(schedGame(2) + 1, 0, 10) = score(0, 0): SX%(schedGame(2) + 1, 1, 10) = score(1, 0)

        If quarter > 3 And SX%(schedGame(2) + 1, 0, 11) = -1 Then SX%(schedGame(2) + 1, 0, 11) = score(0, 3) + score(0, 2) + score(0, 1): SX%(schedGame(2) + 1, 1, 11) = score(1, 3) + score(1, 2) + score(1, 1)

        If quarter = 4 And gameClock! <= 600 And SX%(schedGame(2) + 1, 0, 12) = -1 Then SX%(schedGame(2) + 1, 0, 12) = score(0, 0): SX%(schedGame(2) + 1, 1, 12) = score(1, 0)
        If quarter = 4 And gameClock! <= 300 And SX%(schedGame(2) + 1, 0, 13) = -1 Then SX%(schedGame(2) + 1, 0, 13) = score(0, 0): SX%(schedGame(2) + 1, 1, 13) = score(1, 0)

        SX%(schedGame(2) + 1, 0, 14) = score(0, 0)
        SX%(schedGame(2) + 1, 1, 14) = score(1, 0)

    End If

End Sub


'----------------------------------------
'       TickerSchedule Subroutine
'----------------------------------------
'This subroutine reads in the scores from
'the ticker and uses them to update the
'corresponding schedule file.
Sub TickerSchedule ()

    Shared scheduleFile$

    If schedGame(2) <= 32 Then

        For X = 0 To 1: SX$(schedGame(2) + 1, X) = gameTeams$(X): Next

        Open "TICKER" For Output As #1

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

    Open diskPaths$(3) + scheduleFile$ As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 5 As Q$(22), 5 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    NG%(13) = 1

    For X = 0 To 20:
        LSet Q$(X + 1) = MKI$(NG%(X))
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

    For I = 0 To 1

        I2 = 0
        I3 = 0
        I4 = 0
        I5 = 0
        I6 = 0

        For I1 = 0 To 9
            If rbNumRec(I, I1) <> 0 Then
                I2 = rbNumRec(I, I1)
                I3 = 0
            End If
        Next I1

        For I1 = 0 To 9
            If I1 = 0 Then
                K3(I, 3) = K3(I, 3) + rbNumRec(I, I1) * rbYdsPerC(I, I1)
                K3(I, 3) = K3(I, 3) + wrYdsPerC(I, I1) * (wrNumRec(I, I1) - I2)
            End If

            If I1 > 0 And I1 < 10 And rbNumRec(I, I1) <> 0 Then
                K3(I, 3) = K3(I, 3) + rbYdsPerC(I, I1) * (rbNumRec(I, I1) - I3)
            End If

            If I1 > 0 And I1 < 6 And wrNumRec(I, I1) <> 0 Then
                K3(I, 3) = K3(I, 3) + wrYdsPerC(I, I1) * (wrNumRec(I, I1) - I6)
            End If

            If rbNumRec(I, I1) <> 0 Then
                I3 = rbNumRec(I, I1)
            End If

            If wrNumRec(I, I1) <> 0 Then
                I6 = wrNumRec(I, I1)
            End If

        Next I1

        K3(I, 3) = Int((K3(I, 3) * 3) / 100)

        I2 = 0
        I3 = 0
        I6 = 0

        For I1 = 0 To 9
            If rbrushContrib(I, I1) <> 0 Then
                I4 = rbrushContrib(I, I1)
                I5 = 0
            End If
        Next I1

        For I1 = 0 To 9
            If I1 = 0 Then
                K3(I, 0) = K3(I, 0) + rbrushContrib(I, I1) * rbRushAvg(I, I1)
                I2 = I2 + qbNumAtt(I, I1) * qbCompPct(I, I1)
                I3 = I3 + qbNumAtt(I, I1) * qbPctInt(I, I1)
            End If

            If I1 > 0 And rbrushContrib(I, I1) <> 0 Then
                K3(I, 0) = K3(I, 0) + rbRushAvg(I, I1) * (rbrushContrib(I, I1) - I5)
            End If

            If I1 > 0 And I1 < 4 And qbNumAtt(I, I1) <> 0 Then
                I2 = I2 + qbCompPct(I, I1) * (qbNumAtt(I, I1) - I6)
                I3 = I3 + qbPctInt(I, I1) * (qbNumAtt(I, I1) - I6)
            End If

            If rbrushContrib(I, I1) <> 0 Then
                I5 = rbrushContrib(I, I1)
            End If

            If qbNumAtt(I, I1) <> 0 Then
                I6 = qbNumAtt(I, I1)
            End If

        Next I1

        If I = 0 Then I1 = 1
        If I = 1 Then I1 = 0

        K3(I, 0) = ((K3(I, 0) * 3) / 100) + ((leagRat_GAME(I1, 0)) * 3): ' HAD (leagRat_GAME()+1)*3
        K3(I, 0) = Int(K3(I, 0) + .5) / 10
        K3(I, 1) = Int((I2 / 100) + leagRat_GAME(I1, 1) + .5)
        K3(I, 2) = Int((I3 / 100) - leagRat_GAME(I1, 2) + .5)
        K3(I, 4) = (ydsPerPunt(I, 0) * 2) + 28
        K3(I, 5) = (kickerFGA(I, activeKicker(I)) * 5) + 25
        K3(I, 6) = kickerIdx(I, activeKicker(I))

        XE = ydsPerComp(I1) / 100

        If XE < .5 Then XD(I1) = Int(XE * 30)
        If XE > .5 Then XD(I1) = Int(((1 - XE) * 30) * -1)

        I4 = I

        If I4 = gameLoc Then

            K3(I4, 0) = K3(I4, 0) + .3
            K3(I4, 1) = K3(I4, 1) + 3

        Else

            If gameLoc <> 2 Then
                K3(I4, 0) = K3(I4, 0) - .3
                K3(I4, 1) = K3(I4, 1) - 3
            End If

        End If

    Next I

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
    D = 1

    If P1 = D Then D = 0

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

    pbpString$ = Left$(pbpString$, 76)

    Color 14 - 7 * BW, 1 - BW

    Locate 22, 3

    Print String$(74, 46);

    Locate 22, 39 - Len(pbpString$) / 2

    Print " "; pbpString$; " ";

    'Print #11, pbpString$

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

    If climate(1) = 0 Then Print "NONE ";
    If climate(1) = -7 Then Print "LIGHT ";
    If climate(1) = -15 Then Print "STRONG ";

    If climate(1) <> 0 Then
        If climate(0) = P1 Then
            Print "+";
        Else
            Print "-";
        End If
    End If

    Locate , 40
    Print "WEATHER: ";

    If climate(4) = 0 Then Print "GOOD"
    If climate(4) = -3 Then Print "RAIN/SNOW"
    If climate(4) = -6 Then Print "DOWNPOUR/BLIZZARD"

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

    Screen , , 0, 0

    Color 0, 0, 0

    Cls

    Color 15, 1 + BW * 6

    Print

    'Converted these Chr$ statements so the graphics
    'are preserved in any text editor
    Print Chr$(219); String$(38, 223)
    Print Chr$(219)
    Print Chr$(219)
    Print Chr$(219)

    Call DisplayField

End Sub


'----------------------------------------
'        DisplayField Subroutine
'----------------------------------------
'This subroutine draws the football field
'and scoreboard.
Sub DisplayField ()

    Locate 2, 40
    Color 1 + BW * 6, 0
    Print Chr$(220)

    Locate 3, 2, 0
    Color 15 * (1 - BW), 1 + BW * 6
    Print String$(22, 32); "1  2  3  4   T   ";

    For I = 0 To 1

        Locate I + 4, 2
        Print "  ";

        Color teamRat_GAME(I, 2), teamRat_GAME(I, 1)
        Print gameTeams$(I);

        Color 15 * (1 - BW), 1 + BW * 6
        Print Tab(36);

        Locate I + 4, 23

        For J = 1 To quarter
            Print Using "## "; score(I, J);
        Next J

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

    Call YARDLINE

    Color 15 * (1 - BW), 4 + BW * 3
    TI = gameClock!
    TI = TI + TI * (TI < 0)
    I = Int(TI / 60)
    J = TI - I * 60

    Call TICKERUPDATE

    Locate 3, 44: Print Using " TIME: ##.##     QUARTER: # "; I + J / 100, quarter;
    Locate 3, 53: Print ":";: Locate , 72: Print Tab(80); " ";

    Locate 4, 44: Print Using " DOWN: #"; currDown;: Print Tab(60);
    Print Using " TO GO: ##.#"; ydsToFirst;: Print Tab(80); " ";

    Locate 5, 44: Print " BALL ON ";
    If ydLine <= 50 Then
        Print "OWN";
    Else
        Print "OPP";
    End If

    Print Using " ##.# YD LINE "; ydLineTeam;
    Print Tab(80); " ";
    Locate 6, 44

    Color 0, 4 + BW * 3: Print String$(37, 220);

    Color 15 * (1 - BW), 2 + 5 * BW

    For I = 7 To 19:
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

    For II = 9 To 17
        Locate II, 4
        Print markers$
    Next

    'Print bottom row yard markers
    Locate 18, 4
    Print Chr$(192) + String$(5, 196);

    For II = 1 To 21
        Print Chr$(193) + String$(2, 196);
    Next

    Print String$(3, 196) + Chr$(217);

    'Print minor yard markers
    For II = 10 To 67 Step 3
        For J = 1 To 2
            Locate 11, II + J
            Print Chr$(34);
            Locate 16, II + J
            Print Chr$(34);
        Next
    Next

    'Print major yard numbers
    For II = 1 To 9
        For J = 9 To 17 Step 8
            Locate J, II * 6 + 9
            Print Using "#" + Chr$(179) + "0"; II + 2 * (II - 5) * (II > 5);
        Next
    Next

    'Print play-by-play box
    Locate 19, 1: Print Tab(79)

    Color 14 - 7 * BW, 1 - BW
    Locate 20, 1: Print String$(80, 32);
    Locate 21, 1: Print Chr$(219); String$(78, 223); Chr$(219);
    Locate 22, 1: Print Chr$(219) + " "; String$(76, 46); " " + Chr$(219);
    Locate 23, 1: Print Chr$(219); String$(78, 220); Chr$(219);

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
'gains / loss from a play against the
'goalline defense
Sub GOALLINE ()

    ' *** GOAL LINE DEFENSE ***
    Call Rolld100(RN6)

    If dLine > 3 And dLine < 6 Then

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

    If I2 <= 30 Then
        'Added this to avoid scenarios
        'where the rushing average from
        'OTHERS was negative, therefore
        'leading to a negative index
        If I2 <= 0 Then I2 = 1

        If RN7 <= goalLnYdAdj(I2, 1) Then ydsGained = ydsGained + goalLnYdAdj(I2, 2)

    Else
        'I2 > 30
        'Given how I2 is calculated (based on Rushing Avg),
        'it is not clear how you get a value this high
        If RN7 <= goalLnYdAdj(30, 1) Then ydsGained = ydsGained + goalLnYdAdj(30, 2)

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
        pbpString$ = gameTeams$(P1) + " IS LINING UP FOR THE KICKOFF"
    Else
        pbpString$ = gameMascots$(P1) + " ARE LINING UP FOR THE KICKOFF"
    End If

    Call PBP

    pbpString$ = "IT'S AN ONSIDE KICK!!"
    Call PBP

    Swap P1, D

    I = (Rnd(1) * 21) + 5
    ydLine = ydLine + I
    ydsGained = I

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

    If Int(ydsGained * 10 + .5) / 10 < 9.95 Then
        pbpString$ = "BALL DID NOT TRAVEL 10 YDS"
        Call PBP
        ydLine = 100 - ydLine
    Else
        I = (Rnd(1) * 100) + 1
        If I <= onsideSuccess Then
            pbpString$ = gameTeams$(D) + " RECOVERS !!!"
            Call PBP
            Call CHANGEPOSS
        Else
            pbpString$ = gameTeams$(P1) + " RECOVERS"
            Call PBP
            ydLine = 100 - ydLine
        End If
    End If

End Sub


'----------------------------------------
'         INCOMPLETE Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'text string for an incomplete pass.
Sub INCOMPLETE ()

    Call QBTHROW

    If d100% > 10 Then

        If d100% > 60 Then

            Call Rolld100(RN2)

            Select Case playCall

                Case 13
                    Select Case RN2
                        Case 1 To 40: pbpString$ = "BUT THE SCREEN FALLS INCOMPLETE"
                        Case 41 To 55: pbpString$ = "BUT THE SCREEN IS BROKEN UP"
                        Case 56 To 60: pbpString$ = "BUT " + targetWRName$ + " CAN'T HANDLE THE PASS"
                        Case 61 To 100: pbpString$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE"
                    End Select

                Case 12
                    Select Case RN2
                        Case 1 To 15: pbpString$ = "BUT THE PASS IS BROKEN UP"
                        Case 16 To 44: pbpString$ = "BUT THE PASS IS OVERTHROWN"
                        Case 45 To 74: pbpString$ = "BUT " + targetWRName$ + " CAN'T GET TO THE BALL...INCOMPLETE"
                        Case 75 To 80: pbpString$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE"
                        Case 81 To 100: pbpString$ = "BUT " + targetWRName$ + " IS COVERED...INCOMPLETE"
                    End Select

                Case Else
                    Select Case RN2
                        Case 1 To 23: pbpString$ = "BUT THE PASS FALLS INCOMPLETE"
                        Case 24 To 47: pbpString$ = "BUT THE PASS IS THROWN BEHIND " + targetWRName$ + "...INCOMPLETE"
                        Case 48 To 70: pbpString$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE"
                        Case 71 To 75: pbpString$ = targetWRName$ + " CAN'T HANDLE THE PASS...INCOMPLETE"
                        Case 76 To 81: pbpString$ = "BUT THE PASS IS BATTED DOWN AT THE LINE OF SCRIMMAGE"
                        Case 82 To 96: pbpString$ = "BUT THE PASS TO " + targetWRName$ + " IS BROKEN UP"
                        Case 97 To 199: pbpString$ = "THE PASS IS BROKEN UP AND ALMOST PICKED OFF!!"
                    End Select

            End Select

        Else

            RN2 = Int(Rnd(1) * 26) + 1

            Select Case RN2
                Case 1: pbpString$ = targetWRName$ + " DROPS A PASS RIGHT TO HIM"
                Case 2: pbpString$ = targetWRName$ + " IS HIT AND CAN'T HANG ON....INCOMPLETE PASS"
                Case 3: pbpString$ = targetWRName$ + " BROKE THE WRONG WAY...INCOMPLETE"
                Case 4: pbpString$ = targetWRName$ + " DROPS IT!"
                Case 5: pbpString$ = "DROPPED BY " + targetWRName$
                Case 6: pbpString$ = "BUT " + targetWRName$ + " CAN'T REACH IT"
                Case 7: pbpString$ = targetWRName$ + " CAN'T HAUL IT IN...INCOMPLETE"
                Case 8: pbpString$ = targetWRName$ + " MISHANDLES IT....INCOMPLETE"
                Case 9: pbpString$ = targetWRName$ + " JUGGLES IT....INCOMPLETE"
                Case 10: pbpString$ = targetWRName$ + " CAN'T FIND THE HANDLE....INCOMPLETE"
                Case 11: pbpString$ = targetWRName$ + " CAN'T PULL IT IN....INCOMPLETE"
                Case 12: pbpString$ = targetWRName$ + " BOBBLES IT....INCOMPLETE"
                Case 13: pbpString$ = targetWRName$ + " WAS OUT OF BOUNDS....INCOMPLETE"
                Case 14: pbpString$ = targetWRName$ + " CAME DOWN OUT OF BOUNDS....INCOMPLETE"
                Case 15: pbpString$ = "THE PASS IS BROKEN UP"
                Case 16: pbpString$ = "BUT " + targetWRName$ + " CAN'T HANDLE THE PASS"
                Case 17: pbpString$ = "BUT THE PASS IS OVERTHROWN"
                Case 18: pbpString$ = "BUT " + targetWRName$ + " CAN'T GET TO THE BALL...INCOMPLETE"
                Case 19: pbpString$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE"
                Case 20: pbpString$ = "BUT " + targetWRName$ + " IS COVERED...INCOMPLETE"
                Case 21: pbpString$ = "BUT THE PASS FALLS INCOMPLETE"
                Case 22: pbpString$ = "BUT THE PASS IS THROWN BEHIND " + targetWRName$ + "...INCOMPLETE"
                Case 23: pbpString$ = targetWRName$ + " CAN'T HANDLE THE PASS...INCOMPLETE"
                Case 24: pbpString$ = "BUT THE PASS IS BATTED DOWN AT THE LINE OF SCRIMMAGE"
                Case 25: pbpString$ = "BUT THE PASS TO " + targetWRName$ + " IS BROKEN UP"
                Case 26: pbpString$ = "THE PASS IS BROKEN UP AND ALMOST PICKED OFF!!"
            End Select

        End If

    Else

        ' ***PASS BROKEN UP ***
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
        If defInts(1 - P1) > 0 Then
            Call FINDINT
            pbpString$ = pbpString$ + " BY " + defTeam$
        End If

    End If

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

    If qbacks$(P1, qbNumber(P1)) = "J.HURTS" And (gameTeams$(P1) = "22 PHILADELPHIA" Or gameTeams$(P1) = "23 PHILADELPHIA") Then

        brotherlyShove = 1

        RN0 = Int(Rnd(1) * 3) + 1

        If RN0 = 1 Then
            pbpString$ = gameTeams$(P1) + " LINES UP FOR THE BROTHERLY SHOVE!"
        Else
            pbpString$ = gameTeams$(P1) + " LINES UP FOR THE TUSH PUSH!"
        End If

        Call PBP

    End If

    If brotherlyShove = 1 Then
        pbpString$ = qbacks$(P1, qbNumber(P1)) + " CHARGES FORWARD WITH SOME HELP FROM HIS LINE."
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

        Select Case dLine

            Case 1, 2
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

            Case 3, 4
                'Pinch
                If RN4 <= 30 Then
                    '-.3 to .2
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    '0.3 to 0.9
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 5, 6
                'Inside Stunt
                If RN4 <= 15 Then
                    '-.3 to .2
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    '0.3 to 0.9
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 7 To 10
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

            Case 11
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

    'Philly "converted" 94% of their attempts
    If brotherlyShove = 1 And RN4 <= 94 Then
        ydsGained = 2
    End If

    pbpString$ = "HE GAINS"

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
        pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
    Else
        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
    End If

    pbpString$ = pbpString$ + " YARDS ON THE PLAY"
    Call PBP

    I1 = qback
    X = 4
    X1 = 9
    playType = 1

End Sub


'----------------------------------------
'           OVERUSE Subroutine
'----------------------------------------
'This subroutine attempts to adjust the
'opponents response to a play that is
'being overused.
Sub OVERUSE ()

    If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then

        If (gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3))) < (leagRat_GAME(P1, 7) / 100) And K3(P1, 3) >= 18 Then I4 = I4 * .65

        If I4 <= 0 Then I4 = 1

    End If

End Sub


'----------------------------------------
'           BLITZ Subroutine
'----------------------------------------
'This subroutine determines the yardage
'gains for a blitz defense for when a
'pass will be completed.
Sub BLITZ ()

    Call Rolld100(RN5)
    XE = ydsPerComp(D) / 100

    If XE <= .5 Then
        XE = Int(XE * 100 + .5)
        If XE > 21 Then XE = 21
        If XE < 1 Then XE = 1
        If RN5 <= PS%(1, XE) Then I4 = I4 + PS%(2, XE)
    Else
        XE = Int((1 - XE) * 100 + .5)
        If XE > 21 Then XE = 21
        If XE < 1 Then XE = 1
        If RN5 <= PS%(1, XE) Then I4 = I4 - PS%(2, XE)
    End If

    Call Rolld100(RN0)
    If qbArmRat(P1, qbNumber(P1)) > 20 Then qbArmRat(P1, qbNumber(P1)) = 20
    If qbArmRat(P1, qbNumber(P1)) < -20 Then qbArmRat(P1, qbNumber(P1)) = -20

    QX = qbArmRat(P1, qbNumber(P1))

    If QX <> 0 Then

        If QX < 0 Then QX = QX * -1

        If RN0 <= ydsPerCompPctAdj(QX, 1) Then
            If qbArmRat(P1, qbNumber(P1)) > 0 Then
                I4 = I4 + ydsPerCompPctAdj(QX, 2)
            Else
                I4 = I4 - ydsPerCompPctAdj(QX, 2)
            End If
        End If

    End If

    Call OVERUSE
    I7 = 4

    altYdg = 0

    Select Case playCall
        Case 9
            I7 = 1

        Case 10
            I7 = 3
            If coverage = 9 Or coverage = 10 Then
                I8 = 21
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 11 Then
                I8 = 25
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

        Case 11
            If coverage = 7 Or coverage = 8 Then
                I8 = 21
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 9 Or coverage = 10 Then
                I8 = 25
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 11 Then I8 = 28: Call BlitzYardageLong(I8): altYdg = 1

        Case 12
            I7 = 5
            If coverage = 7 Or coverage = 8 Then
                I8 = 33
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage >= 9 Then
                I8 = 42
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

        Case 13
            I7 = 2
            If coverage = 9 Or coverage = 10 Then
                I8 = 25
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 11 Then
                I8 = 36
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

        Case 14
            If coverage = 7 Or coverage = 8 Then
                I8 = 25
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 9 Or coverage = 10 Then
                I8 = 28
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 11 Then
                I8 = 31
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

        Case 15
            I7 = 5
            If coverage >= 7 Then
                I8 = 42
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

        Case 16
            If coverage = 7 Or coverage = 8 Then
                I8 = 18
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 9 Or coverage = 10 Then
                I8 = 28
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 11 Then
                I8 = 33
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

        Case 17, 18
            If coverage = 7 Or coverage = 9 Then
                I8 = 21
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage = 8 Then
                I8 = 25
                Call BlitzYardageLong(I8)
                altYdg = 1
            End If

            If coverage >= 10 Then
                I8 = 28
                Call BlitzYardageLong(I8)
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

        For I = 1 To 14
            If d100% <= S2%(I7, I4, I) Then Exit For
        Next I

        'This calculates shorter gains after
        'an unsuccessful blitz attempt.
        ' Yardage ranges from -4 to 101
        ' 50% chance of gains under 30
        Select Case I
            Case 1: ydsGained = Int(Rnd(1) * 5) - 4
            Case 2: ydsGained = Int(Rnd(1) * 3) + 1
            Case 3: ydsGained = Int(Rnd(1) * 4) + 4
            Case 4: ydsGained = Int(Rnd(1) * 8) + 8
            Case 5: ydsGained = Int(Rnd(1) * 5) + 16
            Case 6: ydsGained = Int(Rnd(1) * 5) + 21
            Case 7: ydsGained = Int(Rnd(1) * 5) + 26
            Case 8: ydsGained = Int(Rnd(1) * 11) + 30
            Case 9: ydsGained = Int(Rnd(1) * 11) + 40
            Case 10: ydsGained = Int(Rnd(1) * 11) + 50
            Case 11: ydsGained = Int(Rnd(1) * 11) + 60
            Case 12: ydsGained = Int(Rnd(1) * 11) + 70
            Case 13: ydsGained = Int(Rnd(1) * 11) + 80
            Case 14: ydsGained = Int(Rnd(1) * 11) + 90
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
        Case 18: ydsGained = Int(Rnd(1) * 21) + 10
        Case 21: ydsGained = Int(Rnd(1) * 31) + 10
        Case 25: ydsGained = Int(Rnd(1) * 41) + 10
        Case 28: ydsGained = Int(Rnd(1) * 51) + 10
        Case 31: ydsGained = Int(Rnd(1) * 41) + 20
        Case 33: ydsGained = Int(Rnd(1) * 71) + 10
        Case 36: ydsGained = Int(Rnd(1) * 91) + 10
        Case 42: ydsGained = Int(Rnd(1) * 71) + 30
        Case Else: ydsGained = ydsGained + (I4 - 4) * 3
    End Select

End Sub


'----------------------------------------
'           BLITZ2 Subroutine
'----------------------------------------
'This is another routine for calculating
'blitz yardage gains.
Sub BLITZ2 ()

    Call Rolld100(RN4)

    Select Case I2

        Case Is <= 15

            Select Case RN4

                Case 1 To 15
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 16 To 60
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 61 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 2.22)) + 1

            End Select

        Case 16, 17

            Select Case RN4

                Case 1 To 10
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 11 To 50
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 51 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.78)) + 1

            End Select

        Case 18, 19

            Select Case RN4

                Case 1 To 10
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 11 To 40
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 41 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.54)) + 1

            End Select

        Case 20, 21

            Select Case RN4

                Case 1 To 5
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 6 To 30
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 31 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.32)) + 1

            End Select

        Case Else

            Select Case RN4

                Case 1 To 5
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 6 To 20
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 21 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.2)) + 1

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
Sub COMPCHEAT ()

    coverage = 1

    RN0 = Int(Rnd(1) * 4) + 1

    If RN0 = 2 Then

        Select Case playCall

            Case 1, 2, 6, 8
                If d100% <= 50 Then
                    dLine = 3
                Else
                    dLine = 4
                End If

            Case 3, 4, 5, 7
                dLine = 2

            Case 9, 10, 13
                coverage = 3

            Case 11, 14
                If d100% <= 50 Then
                    coverage = 3
                Else
                    coverage = 5
                End If

            Case 12, 15
                coverage = 1

            Case 16
                coverage = 2

            Case Is >= 17
                coverage = 3

        End Select

    End If

End Sub


'----------------------------------------
'           PBPRINT Subroutine
'----------------------------------------
'This subroutine handles the Play-by-play
'text for an interceptions
Sub PBPINT ()

    pbpString$ = ""

    If defSacks(D) <> 0 Then

        M5 = 0
        RN0 = Int(Rnd(1) * teamSacks(D)) + 1

        For Z1 = 0 To 14
            M5 = M5 + playerSacks(D, Z1)
            If RN0 <= M5 Then pbpString$ = defSacks$(D, Z1) + " OF ": Exit For
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
'           DEFENSE Subroutine
'----------------------------------------
'This subroutine appears to determine the
'defense to call bed on the game clock
'and yardage situation.
Sub DEFENSE ()

    skipY90 = 0

    If Not (quarter = 2 And gameClock! <= 120) Or Not (quarter >= 4 And gameClock! <= 120) Then

        Select Case currDown

            Case 1
                If ydsToFirst > 10 Then
                    Call Rolld100(RN0)
                    dLine = 1
                    coverage = 2

                    Select Case RN0

                        Case 1 To 5
                            dLine = 9
                            Call Rolld100(RN1)

                            Select Case RN1

                                Case 1 To 45: coverage = 7

                                Case 46 To 90: coverage = 8

                                Case 91 To 95: coverage = 9

                                Case 96 To 100: coverage = 10

                            End Select

                        Case 6 To 20
                            dLine = 10
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
                            dLine = 9
                            Call Rolld100(RN1)

                            Select Case RN1

                                Case 1 To 45: coverage = 7

                                Case 46 To 90: coverage = 8

                                Case 91 To 95: coverage = 9

                                Case 96 To 100: coverage = 10

                            End Select

                        Case 6 To 20
                            dLine = 10
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
                            dLine = 9
                            Call Rolld100(RN1)

                            Select Case RN1

                                Case 1 To 45: coverage = 7

                                Case 46 To 90: coverage = 8

                                Case 91 To 95: coverage = 9

                                Case 96 To 100: coverage = 10

                            End Select

                        Case 6 To 20
                            dLine = 10
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
                                dLine = 9
                                Call Rolld100(RN1)

                                Select Case RN1

                                    Case 1 To 45: coverage = 7

                                    Case 46 To 90: coverage = 8

                                    Case 91 To 95: coverage = 9

                                    Case 96 To 100: coverage = 10

                                End Select

                            Case 6 To 20
                                dLine = 10
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
                            dLine = 9
                            Call Rolld100(RN1)

                            Select Case RN1

                                Case 1 To 45: coverage = 7

                                Case 46 To 90: coverage = 8

                                Case 91 To 95: coverage = 9

                                Case 96 To 100: coverage = 10

                            End Select

                        Case 6 To 20
                            dLine = 10
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
                            dLine = 9
                            Call Rolld100(RN2)

                            Select Case RN2

                                Case 1 To 45: coverage = 7

                                Case 46 To 90: coverage = 8

                                Case 91 To 95: coverage = 9

                                Case 96 To 100: coverage = 10

                            End Select

                        Case 6 To 20
                            dLine = 10
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
            If ydLine >= 90 And coverage = 5 Or ydsToFirst < 7 And coverage = 5 Then coverage = 3
        End If

    Else

        If score(P1, 0) - score(D, 0) <= 0 Then

            If d100% <= 50 Then
                dLine = Int(Rnd(1) * 3) + 4
            Else
                dLine = 3
            End If

            Call Rolld100(RN0)

            Select Case RN0

                Case 1 To 33: coverage = 1

                Case 34 To 67: coverage = 2

                Case 68 To 100: coverage = 5

            End Select

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
                    dLine = 9
                    coverage = 10

                Case 96
                    dLine = 10
                    coverage = 9

                Case 97, 98
                    dLine = 9

                Case 99, 100
                    dLine = 10
                    coverage = 7

            End Select

        End If

    End If

End Sub


'----------------------------------------
'           MARKER Subroutine
'----------------------------------------
'This subroutine prints the yard line
'marker on the field
Sub MARKER ()

    If P1 Then
        J = FindMarkerColumn(ydsToScore) - 3
    Else
        J = FindMarkerColumn(100 - ydsToScore) - 3
    End If

    Locate 13, 4

    Color 15 * (1 - BW), 2 + 5 * BW

    If J < WS Then

        Print Left$(markers$, J - 1);

        Color 0: Print Chr$(16 + P1);

        Color 15 * (1 - BW): Print Mid$(markers$, J + 1, WS - J - 1);

        Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, WS, 7);

        Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(markers$, 67 - WS);

    Else

        If J > WE Then

            Print Left$(markers$, WS - 1);

            Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, WS, 7);

            Color 15 * (1 - BW), 2 + 5 * BW: Print Mid$(markers$, WE + 1, J - WE - 1);

            Color 0: Print Chr$(16 + P1);

            Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(markers$, 73 - J);

        Else

            Print Left$(markers$, WS - 1);

            Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, WS, J - WS);

            Color BW * 15: Print Chr$(16 + P1);

            Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(markers$, J + 1, WE - J);

            Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(markers$, 73 - WE);

        End If

    End If


End Sub


'----------------------------------------
'           MARKER1 Subroutine
'----------------------------------------
'This subroutine also has something to do
'with printing the yard marker.
Sub MARKER1 ()

    ydsToScore = 100 - ydLine
    YF = -ydsToFirst

    If P1 Then
        WS = FindMarkerColumn(ydsToScore + YF) - 3
        WE = WS + 6
    Else
        WE = FindMarkerColumn(100 - ydsToScore - YF) - 3
        WS = WE - 6
    End If

    Call MARKER

End Sub


'----------------------------------------
'          MOVEBALL Subroutine
'----------------------------------------
'This subroutine handles the play-by-play
'text for after the ball has moved. With
'that also comes Play-by-Play for the
'tackle / stop.
Sub MOVEBALL ()

    If pbpDelay! <> 0 Then

        I$ = "He's at "

        If endYds <= startYds Then

            For ydsToScore = startYds To endYds Step -1

                Call MARKER
                _Delay pbpDelay! / 20
                Call YARDLINE
                yrdLine = ydLineTeam

                If ydsToScore <= 0 Then
                    ydsToScore = endYds
                Else
                    If Int(yrdLine / 10) * 10 = yrdLine Then
                        pbpString$ = I$ + "the" + Str$(yrdLine) + " yard line"
                        Call PLACEPBP
                        I$ = ""
                    End If
                End If

            Next ydsToScore

        Else

            For ydsToScore = startYds To endYds

                Call MARKER
                _Delay pbpDelay! / 20
                Call YARDLINE
                yrdLine = ydLineTeam

                If ydsToScore <= 0 Then
                    ydsToScore = endYds
                Else
                    If Int(yrdLine / 10) * 10 = yrdLine Then
                        pbpString$ = I$ + "the" + Str$(yrdLine) + " yard line"
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

            RN0 = Int(Rnd(1) * 21) + 1

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
            End Select

            If ydsToScore >= 100 Then
                pbpString$ = "THE BALL CARRIER" + I$ + " FOR A" + Str$(Int(-ydLine)) + " YARD LOSS"
            Else
                If ydsToScore > 0 Then
                    Call MARKER
                    pbpString$ = "AND HE" + I$ + " AT THE" + Str$(Int(yrdLine)) + " YARD LINE"
                End If

            End If

        End If

        If ydsToScore <> 0 Then Call PBP

    End If

End Sub


'----------------------------------------
'         SCOREBOARD Subroutine
'----------------------------------------
'This subroutine updates the scoreboard
'with the latest values for the clock,
'and possession location.
Sub SCOREBOARD ()

    Screen , , 0, 0

    If Screen(2, 1) <> 219 Then

        Call SETSCREEN
        Call MARKER1

    Else

        YF = -ydsToFirst
        ydsToScore = 100 - ydLine

        '-- The value of BW is always 0.
        Color 15 * (1 - BW), 1 + BW * 6
        Locate 4 + P1, 3

        'Print indicator of team with possession
        Print "*";: Locate 5 - P1, 3: Print " ";

        Call YARDLINE

        Color 15 * (1 - BW), 4 + BW * 3

        TI = gameClock!
        TI = TI + TI * (TI < 0)
        I = Int(TI / 60)

        Call TICKERUPDATE

        J = TI - I * 60

        Locate 3, 44: Print Using " TIME: ##.##     QUARTER: # "; I + J / 100, quarter;
        Locate 3, 53: Print ":";
        Locate , 72: Print Tab(80); " ";

        Locate 4, 44: Print Using " DOWN: #"; currDown;
        Print Tab(60);

        If ydsToFirst >= 0.7 Then
            Print Using " TO GO: ##"; Int(ydsToFirst + 0.5);
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

        'Print Using " ##.# YD LINE "; ydLineTeam;
        Print Using " ## YD LINE "; Int(ydLineTeam + 0.5);
        Print Tab(80); " ";

    End If

End Sub


'----------------------------------------
'         ColorChange Subroutine
'----------------------------------------
'This routine checks to see if both teams
'have conflicting primary colors.
'If so, it will substitute the secondary
'color with white/grey and reverse them.
'In essence, it's the digital equivalent
'of assigning an alternate jersey color.
Sub ColorChange ()

    If teamRat_GAME(1, 2) = 15 And teamRat_GAME(1, 1) = 4 Then
        teamRat_GAME(1, 2) = 4
        teamRat_GAME(1, 1) = 7
    End If

    If teamRat_GAME(1, 2) = 15 And teamRat_GAME(1, 1) = 1 Then
        teamRat_GAME(1, 2) = 1
        teamRat_GAME(1, 1) = 7
    End If

    If teamRat_GAME(1, 2) = 0 And teamRat_GAME(1, 1) = 4 Then
        teamRat_GAME(1, 2) = 12
        teamRat_GAME(1, 1) = 0
    End If

    If teamRat_GAME(1, 2) = 1 And teamRat_GAME(1, 1) = 7 Then
        teamRat_GAME(1, 2) = 15
        teamRat_GAME(1, 1) = 1
    End If

    If teamRat_GAME(1, 2) = 4 And teamRat_GAME(1, 1) = 7 Then
        teamRat_GAME(1, 2) = 15
        teamRat_GAME(1, 1) = 4
    End If

    If teamRat_GAME(1, 2) = 14 And teamRat_GAME(1, 1) = 4 Then
        teamRat_GAME(1, 2) = 4
        teamRat_GAME(1, 1) = 7
    End If

End Sub


'----------------------------------------
'           DEFMENU Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'selecting their defensive choices.
Sub DEFMENU ()


    If playerMode = 1 And D = compTeam Or playerMode = 2 Then
        'Computer on Defense or PC vs PC

        Call DEFENSE
        Call DEFGOALLINE

    Else

        Do

            goodSelection = 1

            'Determine Line Formation
            Call FRAMEOPTION(10, D)
            Print "SELECT DEFENSIVE LINE PLAY "; gameTeams$(D)

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

                If UCase$(I$) = "Q" Then
                    Cls
                    End
                End If

                If UCase$(I$) = "G" And ydLine >= 94.999 Then
                    dLine = 11
                    Screen , , 0, 0
                    Exit Sub
                End If

                If UCase$(I$) = "T" Then
                    Call TimeoutCalled(P1, periodOver)
                    Call CheckGameClock
                    If gameClock! = 0 Then Exit Sub
                    'If periodOver = 1 Then GoTo 3360
                End If

                dLine = Val(I$)

            Loop Until dLine >= 0 And dLine <= 9 And UCase$(I$) <> "G" And UCase$(I$) <> "T"

            'Adjustments ???
            dLine = dLine + 1

            Screen , , 0, 0

            Select Case dLine
                Case 5, 6
                    dLine = dLine + 4
                Case 7 To 10
                    dLine = dLine - 2
            End Select

            'Determine Pass Coverage
            Call FRAMEOPTION(10, D)
            Print "SELECT PASS COVERAGE "; gameTeams$(D)

            For coverage = 1 To 5
                Locate 10 + coverage, 14
                Print coverage - 1; "- "; passCov$(coverage); Tab(41); coverage + 4; "- "; passCov$(coverage + 5)
            Next coverage

            Locate , 41: Print " B - BLITZ ENGAGE"

            Do
                J$ = GetKeyPress$

                'Main loop condition
                If J$ <> Chr$(27) Then
                    If UCase$(I$) = "B" Then coverage = 10: Exit Do
                    coverage = Val(J$)
                End If

            Loop Until coverage >= 0 And coverage <= 9 Or I$ = Chr$(27)

            If J$ <> Chr$(27) Then

                ' 3065
                coverage = coverage + 1
                Screen , , 0, 0

                'More loop conditions
                '(5-2 Pass Rush Or Inside Stunt) AND (Blitz ILB or Blitz Engage)
                If (dLine = 5 Or dLine = 6) And (coverage = 7 Or coverage = 9 Or coverage = 11) Then
                    goodSelection = 0
                End If

                '(5-2 Inside Stunt Or 4-3 Outside Stunt) AND (Blitz OLB or Blitz Engage)
                If (dLine = 7 Or dLine = 8) And (coverage = 8 Or coverage = 10 Or coverage = 11) Then
                    goodSelection = 0
                End If

            End If

        Loop Until goodSelection = 1

    End If

End Sub


'----------------------------------------
'         DEGFOALLINE Subroutine
'----------------------------------------
'This subroutine automatically picks a
'goal line defense when 5 yards are left.
Sub DEFGOALLINE ()

    If ydLine > 94.999 Then
        dLine = 11
        coverage = 1
    End If

End Sub


'----------------------------------------
'            LGPBP Subroutine
'----------------------------------------
'This subroutine chooses the play-by-play
'text for the start of a play where any
'long gain has been calculated.
Sub LGPBP ()

    RN0 = Int(Rnd(1) * 20) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE BREAKS FREE"
        Case 2: pbpString$ = "HE DODGES A TACKLER"
        Case 3: pbpString$ = "HE STIFF ARMS A TACKLER"
        Case 4: pbpString$ = "HE HEADS DOWN THE SIDELINE"
        Case 5: pbpString$ = "HE SEES DAYLIGHT"
        Case 6: pbpString$ = "HE CUTS BACK"
        Case 7: pbpString$ = "HE CUTS UPFIELD"
        Case 8: pbpString$ = "HE CHARGES THROUGH A GAPING HOLE"
        Case 9: pbpString$ = "HE MAY GO ALL THE WAY"
        Case 10: pbpString$ = "HE BREAKS IT"
        Case 11: pbpString$ = "HE TURNS IT ON"
        Case 12: pbpString$ = "HE SHIFTS INTO HIGH GEAR"
        Case 13: pbpString$ = "HE BREAKS IT BIG!!"
        Case 14: pbpString$ = "HE BURSTS THROUGH"
        Case 15: pbpString$ = "HE RUNS WILD"
        Case 16: pbpString$ = "HE POPS A BIG ONE"
        Case 17: pbpString$ = "HE BUSTS ONE"
        Case 18: pbpString$ = "HE POPS THROUGH THERE"
        Case 19: pbpString$ = "HE POPS OUT THERE"
        Case 20: pbpString$ = "HE SEES A BREAK"
    End Select

    Call PBP

End Sub


'----------------------------------------
'          RUNTDPBP Subroutine
'----------------------------------------
'This subroutine generates the Play-by-Play
'for a rushing TD.
Sub RUNTDPBP ()

    RN0 = Int(Rnd(1) * 29) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE BREAKS FREE"
        Case 2: pbpString$ = "HE DODGES A TACKLER"
        Case 3: pbpString$ = "HE STIFF ARMS A TACKLER"
        Case 4: pbpString$ = "HE HEADS DOWN THE SIDELINE"
        Case 5: pbpString$ = "HE SEES DAYLIGHT"
        Case 6: pbpString$ = "HE CUTS BACK"
        Case 7: pbpString$ = "HE CUTS UPFIELD"
        Case 8: pbpString$ = "HE CHARGES THROUGH A GAPING HOLE"
        Case 9: pbpString$ = "HE MAY GO ALL THE WAY"
        Case 10: pbpString$ = "HE BREAKS IT"
        Case 11: pbpString$ = "HE TURNS IT ON"
        Case 12: pbpString$ = "HE SHIFTS INTO HIGH GEAR"
        Case 13: pbpString$ = "HE BREAKS IT BIG!!"
        Case 14: pbpString$ = "HE BURSTS THROUGH"
        Case 15: pbpString$ = "HE RUNS WILD"
        Case 16: pbpString$ = "HE POPS A BIG ONE"
        Case 17: pbpString$ = "HE BUSTS ONE"
        Case 18: pbpString$ = "HE POPS THROUGH THERE"
        Case 19: pbpString$ = "HE POPS OUT THERE"
        Case 20: pbpString$ = "HE SEES A BREAK"
        Case 21: pbpString$ = "HE'S GONE!"
        Case 22: pbpString$ = "NO ONE WILL CATCH HIM"
        Case 23: pbpString$ = "SEE YOU LATER!!"
        Case 24: pbpString$ = "HE GETS THE CORNER"
        Case 25: pbpString$ = "HE COULD GO"
        Case 26: pbpString$ = "HE'LL GO THE DISTANCE"
        Case 27: pbpString$ = "THERE HE GOES!!"
        Case 28: pbpString$ = "HE'LL GO ALL THE WAY"
        Case 29: pbpString$ = "HE'S IN THE SECONDARY"
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
        I2 = NG%(20)
    Else

        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 1 And choice <= 3 Or I$ = "0"

    End If

    Screen , , 0, 0

    Select Case choice

        Case 1
            climate(4) = -3
            climate(5) = 1
            climate(6) = -10

        Case 2
            climate(4) = -6
            climate(5) = 2
            climate(6) = -20

        Case 3

            Call Rolld100(RN1)

            Select Case RN1

                Case 20 To 38
                    'Snow affects 7% of games
                    'Rain affects 12% of games
                    climate(4) = -3
                    climate(5) = 1
                    climate(6) = -10

                Case 7 To 12
                    'Blizzards affect 1% of games
                    'Heavy rain affects 5% of games
                    climate(4) = -6
                    climate(5) = 2
                    climate(6) = -20
 
            End Select

    End Select

    Call OPTIONBOX(9)

    Print "WIND OPTIONS "
    Locate 12, 14
    Locate , 14: Print "0 - NONE                        "
    Locate , 14: Print "1 - LIGHT TO MODERATE (< 15 MPH)"
    Locate , 14: Print "2 - STRONG AND GUSTY (>= 15 MPH)"
    Locate , 14: Print "3 - COMPUTER SELECT             "

    If autoPlay = 1 Then
        choice = NG%(19)
    Else
        Do
            I$ = GetKeyPress$
            choice = Val(I$)
        Loop Until choice >= 0 And choice <= 3
    End If

    Screen , , 0, 0

    Select Case choice

        Case 1
            climate(1) = -7
            climate(2) = -5
            climate(3) = -10

        Case 2
            climate(1) = -15
            climate(2) = -15
            climate(3) = -30

        Case 3

            Call Rolld100(RN1)
            
            Select Case RN1

                Case 1 To 26
                    'Moderate wind impacts 27% of games
                    climate(1) = -7
                    climate(2) = -5
                    climate(3) = -10

                Case 45 To 60
                    'Heavy wind impacts 15% of games
                    climate(1) = -15
                    climate(2) = -15
                    climate(3) = -30

            End Select

    End Select

End Sub


'----------------------------------------
'            FGATT Subroutine
'----------------------------------------
'This subroutine tracks and increments FG
'attempts based on yardage increments for
'the current kicker.
Sub FGATT ()

    fgAttYds = Int(117 + goalPostAdj - ydLine + .5)

    Select Case fgAttYds

        Case 1 To 19
            FA(P1, activeKicker(P1), 0) = FA(P1, activeKicker(P1), 0) + 1

        Case 20 To 29
            FA(P1, activeKicker(P1), 1) = FA(P1, activeKicker(P1), 1) + 1

        Case 30 To 39
            FA(P1, activeKicker(P1), 2) = FA(P1, activeKicker(P1), 2) + 1

        Case 40 To 49
            FA(P1, activeKicker(P1), 3) = FA(P1, activeKicker(P1), 3) + 1

        Case Is >= 50
            FA(P1, activeKicker(P1), 4) = FA(P1, activeKicker(P1), 4) + 1

    End Select

End Sub


'----------------------------------------
'            FGMADE Subroutine
'----------------------------------------
'This subroutine tracks and increments FG
'made based on yardage increments for
'the current kicker.
Sub FGMADE ()

    If fgAttYds > gameLongFG(P1, activeKicker(P1)) Then
        gameLongFG(P1, activeKicker(P1)) = fgAttYds
    End If

    Select Case fgAttYds

        Case 1 To 19
            FM(P1, activeKicker(P1), 0) = FM(P1, activeKicker(P1), 0) + 1

        Case 20 To 29
            FM(P1, activeKicker(P1), 1) = FM(P1, activeKicker(P1), 1) + 1

        Case 30 To 39
            FM(P1, activeKicker(P1), 2) = FM(P1, activeKicker(P1), 2) + 1

        Case 40 To 49
            FM(P1, activeKicker(P1), 3) = FM(P1, activeKicker(P1), 3) + 1

        Case Is >= 50
            FM(P1, activeKicker(P1), 4) = FM(P1, activeKicker(P1), 4) + 1

    End Select

End Sub


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
                kickLimit = 40

            Case 2 To 3
                '1980's - 1990's
                kickLimit = 50

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
'       ComputerOffense Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'selecting their offensive choices.
Sub ComputerOffense ()

    'So 50 yarder is attempted from Opp 33 (67)
    fgAttLine = (117 - fgLimit% + goalPostAdj)

    If endZone = 1 Then

        'We should only be here if going for 2
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

    Else

        '4th Qtr, < 35 secs / down to go, No TOs (Def), Offense leading
        If quarter = 4 And gameClock! <= 35 * (4 - currDown) And timeouts(D) = 0 And score(P1, 0) > score(D, 0) Then

            playCall = 9
            playType = 1
            qbTakeKnee = 1

        Else

            If ((quarter = 2 And ydLine > 40) Or quarter >= 4) And gameClock! <= 120 Then

                '2 Min Warning
                '--------------------
                'Down by >= 4 (Off)
                If score(D, 0) - score(P1, 0) > 3 Then
                    If quarter >= 4 Then 8880
                    If quarter = 2 Then 8885
                End If

                'Score Tied or Down
                If score(D, 0) - score(P1, 0) >= 0 Then 8885

                '2nd Qtr, Past Own 40 Yd Line
                If ydLine > 40 And quarter = 2 Then 8885

                If d100% <= 60 Then
                    playCall = 3
                Else
                    playCall = 4
                End If

                'In FG range, < 22 secs to go, 2nd Qtr | 4th Qtr Tie | Overtime
                If ydLine >= fgAttLine - 3 And gameClock! <= 22 And (quarter = 2 Or quarter > 4 Or (quarter = 4 And score(P1, 0) - score(D, 0) = 0)) Then
                    'Field Goal
                    'Call pbplog(15308)
                    playCall = 20
                End If

                '4th Down, >= 3 Yds to go, 40+ Yd FG Att, Modern Era (FGs uncommon before 1960)
                If currDown = 4 And ydsToFirst >= 3 And ydLine >= fgAttLine - 3 And ydLine < 87 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                    'Field Goal
                    'Call pbplog(15315)
                    playCall = 20
                End If

                'Outside LONG FG range, 4th Down
                If ydLine < fgAttLine - 3 And currDown = 4 Then
                    'Punt
                    'Call pbplog(13328)
                    playCall = 19
                End If

            Else

                'Regular GamePlay
                If ruleOptType <> 0 Then
                    leagueRushPct = leagRat_GAME(P1, 7)
                Else
                    'Do we need to adjust rush play call frequency?
                    'This is predicated on the idea that QB sneaks
                    'are recorded differently in NCAA. If QB rushes,
                    'in my game, get redirected to passes, then teams
                    'should be passing more often???
                    leagueRushPct = leagRat_GAME(P1, 7) ' - 4
                End If

                '4th Down, 3rd Qtr+, Inside opponent's 45, Down by 15 or more (Off)
                If quarter >= 3 And currDown = 4 And ydLine >= 55 And score(D, 0) - score(P1, 0) > 14 Then
                    GoTo 8833
                End If

                '4th Down, 4th Qtr+, Inside Opponent's field, Down by 11 or more (Off)
                If quarter >= 4 And currDown = 4 And ydLine >= 50 And score(D, 0) - score(P1, 0) > 10 Then
                    GoTo 8833
                End If

                '4th Down, 1 Yd To Go, 40+ Yd FG Att, 3 Pt. Lead (Off)
                If currDown = 4 And ydsToFirst <= 1 And ydLine >= fgAttLine - 3 And ydLine < 87 And score(P1, 0) - score(D, 0) <= 3 And score(P1, 0) - score(D, 0) > 0 Then
                    'Go for it!
                    GoTo 8827
                End If

                '4th Qtr | OT
                '---------------
                '4th Down, In FG Range, <= Score Within 3 Pts, Modern Era (FGs uncommon before 1960)
                If quarter >= 4 And currDown = 4 And ydLine >= fgAttLine - 3 And score(P1, 0) - score(D, 0) <= 3 And score(P1, 0) - score(D, 0) >= -3 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                    'Field Goal
                    'Call pbplog(15363)
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, In FG Range, <= 3 Pt. Lead (Def), Modern Era (FGs uncommon before 1960)
                If quarter >= 4 And currDown = 4 And ydLine >= fgAttLine - 3 And score(D, 0) - score(P1, 0) <= 3 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                    'Field Goal
                    'Call pbplog(15371)
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, <= 2 Yds to Go, Long FG range, Down by 11+ (Off)
                If quarter >= 4 And currDown = 4 And ydLine >= fgAttLine + 10 And ydLine < fgAttLine - 3 + goalPostAdj And ydsToFirst <= 2 And score(D, 0) - score(P1, 0) > 10 Then
                    GoTo 8827
                End If

                '4th Down, <5 Minutes, Long FG range, Down by 1-10 (Off), Modern Era (FGs uncommon before 1960)
                If quarter >= 4 And currDown = 4 And gameClock! < 240 And ydLine >= fgAttLine + 10 And ydLine < fgAttLine - 3 And score(D, 0) - score(P1, 0) <= 10 And score(D, 0) - score(P1, 0) > 0 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                    'Field Goal
                    'Call pbplog(15384)
                    playCall = 20
                    GoTo 8800
                End If

                '4th Qtr Only
                '---------------
                '4th Down, < 4 Minutes, Down by 4 to 11 (Off), Inside Red Zone
                If quarter = 4 And currDown = 4 And gameClock! > 240 And score(D, 0) - score(P1, 0) >= 4 And score(D, 0) - score(P1, 0) <= 11 And ydLine > 80 + goalPostAdj Then
                    'Field Goal
                    'Call pbplog(15394)
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, Down by 4 or More, Inside Red Zone
                If quarter = 4 And currDown = 4 And score(D, 0) - score(P1, 0) >= 4 And ydLine >= 80 + goalPostAdj Then
                    GoTo 8833
                End If

                'Any Qtr
                '---------------
                '4th Down, < 3 Yds to Go, Inside Opponent's 40
                If currDown = 4 And ydsToFirst < 3 And ydLine >= 60 Then
                    GoTo 8827
                End If

                '4th Down, >= 3 Yds To Go, <= 49 Yd Att, Modern Era (FGs uncommon before 1960)
                If currDown = 4 And ydsToFirst >= 3 And ydLine >= 68 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                    'Field Goal
                    '-- Most field goals are done here
                    'Call pbplog(15414)
                    playCall = 20
                    GoTo 8800
                End If

                '4th Down, Outside FG range
                If currDown = 4 And ydLine < fgAttLine + 3 Then
                    'Punt
                    '-- Most punts are done here
                    playCall = 19
                    GoTo 8800
                End If

                'Inside Team's Own 8 Yd Line
                If ydLine <= 8 Then
                    GoTo 8827
                End If

                '2nd Down, <=12 Yds To Go, Mostly Rushing league era
                If leagueRushPct > 75 And currDown = 2 And ydsToFirst <= 12 Then
                    GoTo 8827
                End If

                '3rd Down, <= 5 Yds to Go, Mostly Rushing league era
                If currDown = 3 And ydsToFirst <= 5 And leagueRushPct > 75 Then
                    GoTo 8827
                End If

                '1st or 2nd Down, Mostly Rushing league era
                If (currDown = 1 And leagueRushPct >= 70) Or (currDown <= 2 And leagueRushPct >= 80) Then
                    GoTo 8827
                End If

                '1st Down, >= 11 Yds To Go
                If currDown = 1 And ydsToFirst > 10 Then
                    GoTo 8833
                End If

                '2nd Down
                If currDown = 2 And ydsToFirst > 10 Then
                    If ydsToFirst > 10 Then GoTo 8833
                    If ydsToFirst < 5 Then GoTo 8827
                End If

                '3nd Down
                If currDown = 3 And ydsToFirst >= 4 Then
                    If ydsToFirst >= 4 Then GoTo 8833
                    If ydsToFirst < 2 Then GoTo 8827
                End If

                '4th Down
                If currDown = 4 Then
                    'Inside Red Zone??
                    'How much time left?
                    If ydsToFirst >= 4 Then GoTo 8833
                    If ydsToFirst < 2 Then GoTo 8827
                End If

                '4th Qtr
                '---------------
                '< 8 Minutes in Game
                'Either team is ahead by 9 or more pts
                Call Rolld100(RN0)

                If quarter = 4 And gameClock! <= 480 And score(D, 0) - score(P1, 0) >= 9 And RN0 >= leagueRushPct - leagRat_GAME(P1, 7) / 2 Then
                    GoTo 8833
                End If
                If quarter = 4 And gameClock! <= 480 And score(P1, 0) - score(D, 0) >= 9 And RN0 >= leagueRushPct + leagRat_GAME(P1, 7) / 2 Then
                    GoTo 8833
                End If

                If RN0 > leagRat_GAME(P1, 7) Then
                    GoTo 8833
                End If

                '----- DECISION BRANCH 1 -----
                '----- RUN | PASS DECIDE -----
                8827 '
                If gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3) <> 0 Then
                    'leagueRushPct = Run / Pass %
                    If gameStatsTeam(P1, 1) / (gameStatsTeam(P1, 1) + gameStatsTeam(P1, 3)) > leagueRushPct / 100 And leagueRushPct < 50 Then
                        'PASS!
                        GoTo 8833
                    End If
                End If

                Call Rolld100(RN0)

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

                If ydsToFirst <= 1.32 Then

                    'QB Sneak
                    'Print #11, "Evaluating sneak"
                    If RN0 <= sneakChance Then
                        playCall = 8
                    End If

                Else

                    Call Rolld100(RN1)

                    Select Case RN1

                        'Long Pass
                        Case 1 To 2:
                            If leagueRushPct < 75 Then
                                playCall = 15
                            End If

                            'Med Pass
                        Case 3 To 5:
                            If leagueRushPct < 75 Then
                                playCall = 14
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

                If qbacks$(P1, qbNumber(P1)) = "J.HURTS" And (gameTeams$(P1) = "22 PHILADELPHIA" Or gameTeams$(P1) = "23 PHILADELPHIA") And ydsToFirst <= 2 Then
                    playCall = 8
                End If

                GoTo 8800

                '----- DECISION BRANCH 2 -----
                '----- PASSING SCENARIOS -----
                8833 '
                Call Rolld100(RN0)

                Select Case RN0

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
                '----------------------------------------
                If currDown >= 3 And ydsToFirst >= 5 Then
                    '-- If Draw was selected, change to Long Pass
                    If playCall = 6 Then playCall = 12

                    '-- If Short Pass was selected, change to Medium Pass
                    If playCall = 10 Then playCall = 11

                    '-- If 3-Step Drop or screen was selected, change to Rollout
                    If (playCall = 13 Or playCall = 9) Then playCall = 17

                End If

                'Inside Opponent's 10 or < 7 Yds To Go
                '----------------------------------------
                '-- If Long Pass was selected, change to Medium Pass
                If (playCall = 12 Or playCall = 15) And ydLine > 90 Or ydsToFirst < 7 Then
                    playCall = 11
                End If

                GoTo 8800

            End If

            '----- DECISION MADE -----
            8880 '
            '2nd Qtr, 4th Down, 2 Min Warning
            If quarter = 2 And currDown = 4 Then

                'Outside FG range
                If ydLine < fgAttLine - 3 Then
                    'Punt
                    playCall = 19
                Else
                    'In FG range, Modern Era (FGs uncommon before 1960)
                    If (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                        'Field Goal
                        'Call pbplog(15661)
                        playCall = 20
                    End If
                End If

                GoTo 8800

            End If

            'Any Qtr
            '---------------
            '< 22 secs to go, No TOs (Off)
            If gameClock! <= 22 And timeouts(P1) = 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            '< 12 secs to go, TOs Remaining (Off)
            If gameClock! <= 12 And timeouts(P1) > 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            GoTo 8892

            '-----

            8885 '
            '4th Down, 2nd Qtr --OR-- 4th Qtr + Score Tied, Outside FG range
            If ydLine < fgAttLine - 3 And currDown = 4 And (quarter = 2 Or (quarter >= 4 And score(P1, 0) = score(D, 0))) Then
                'Punt
                playCall = 19
                GoTo 8800
            End If

            '2nd Qtr Only
            '4th Down, In FG range, Modern Era (FGs uncommon before 1960)
            If ydLine >= fgAttLine - 3 And quarter = 2 And currDown = 4 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                'Field Goal
                'Call pbplog(15702)
                playCall = 20
                GoTo 8800
            End If

            'Any Qtr
            '---------------
            'Outside FG range, < 37 secs to go, No TOs (Off)
            If ydLine < fgAttLine - 3 And gameClock! <= 37 And timeouts(P1) = 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            'Outside FG range, < 22 secs to go, TOs Remaining (Off)
            If ydLine < fgAttLine - 3 And gameClock! <= 12 And timeouts(P1) > 0 Then
                'Pass
                playCall = 12
                GoTo 8800
            End If

            'In FG range, < 22 secs to go, No TOs (Off)
            If ydLine < fgAttLine - 3 And gameClock! <= 22 And timeouts(P1) = 0 Then
                'Field Goal
                'Call pbplog(15726)
                playCall = 20
                GoTo 8800
            End If

            'In FG range, < 12 secs to go, TOs Remaining (Off)
            If ydLine < fgAttLine - 3 And gameClock! <= 12 And timeouts(P1) > 0 Then
                'Field Goal
                'Call pbplog(15734)
                playCall = 20
                GoTo 8800
            End If

            'In FG range, 4th Down, Modern Era (FGs uncommon before 1960)
            If ydLine >= fgAttLine - 3 And currDown = 4 And (ruleOptType <> 0 Or (ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960)) Then
                'Field Goal
                'Call pbplog(15742)
                playCall = 20
                GoTo 8800
            End If

            8892 '
            'Sideline pass
            playCall = 16

            '50% random chance of Short/Med/Long Pass
            If d100% < 50 Then
                playCall = Int(Rnd(1) * 3) + 10
            End If

            'Inside Opponent's 10 = Medium Pass
            If ydLine >= 90 Then
                playCall = 11
            End If

            'In the Red Zone, Down by < 3  (Off) --OR-- Inside Opponent's 10, Down by 3
            'Random choice of Blast / Dive / Off Tackle / Sweep
            If ydLine > 80 And score(D, 0) - score(P1, 0) < 3 Or ydLine > 90 And score(D, 0) - score(P1, 0) = 3 Then
                playCall = Int(Rnd(1) * 4) + 1
            End If

        End If

        'Final Checks
        '-----
        8800 '
        'Overtime and 4th Down - Change to Long Pass
        If quarter > 4 And NG%(14) = 3 And currDown = 4 Then

            'Punt chosen but score tied
            If playCall = 19 And score(D, 0) = score(P1, 0) Then
                playCall = 12
            End If

            'Punt chosen, Ahead by 3 or less (Off)
            '(ie, if we punt and give the team the ball,
            'they could win by a FG; let's go for it
            If playCall = 19 And score(D, 0) - score(P1, 0) <= 3 Then
                playCall = 12
            End If

            'Punt or FG chosen, Down by 4 or more (Off)
            '(ie, we have to try and get a 1st down)
            If (playCall = 19 Or playCall = 20) And score(D, 0) - score(P1, 0) > 3 Then
                playCall = 12
            End If

        End If

    End If

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

    Call PBP

    test! = ydsGained - ydsToFirst
    Select Case ydsGained - ydsToFirst

        Case Is < -.2
            pbpString$ = "HE'S ABOUT A FOOT SHORT"

        Case -0.20 To -0.11
            pbpString$ = "HE'S ABOUT 6 INCHES SHORT"

        Case -0.10 To -0.01
            pbpString$ = "HE'S SHORT BY INCHES"

        Case Else
            pbpString$ = "THE OFFICIAL SIGNALS FIRST DOWN!"

    End Select

    Call PBP
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
'          CLIPPING Subroutine
'----------------------------------------
'This subroutine handles the yardage and
'Play-by-Play for a clipping penalty.
Sub CLIPPING ()

    pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
    Call PBP

    If ydLine < 20 Then
        ydLine = ydLine / 2
        gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + ydLine / 2
    End If

    gameStatsTeam(P1, 11) = gameStatsTeam(P1, 11) + 1

    If ydLine >= 20 Then
        ydLine = ydLine - 10
        gameStatsTeam(P1, 12) = gameStatsTeam(P1, 12) + 10
    End If

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
                If qbNumAtt(currTeam, currQB - 1) < 100 Then

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

    If choice <= 3 And ruleOptType <> 0 And useRandomQB(currTeam) = 0 Then

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

    value = qbMobility(currTeam)

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
'      KOChoices_NoDefer Subroutine
'----------------------------------------
'This subroutine gives the user choices
'to handle a pro kick off.
Sub KOChoices_NoDefer ()

    Call SCOREBOARD

    If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

        koChoice = 2

    Else
        Call OPTIONBOX(9)
        Print gameTeams$(winTeam); " DO YOU ELECT TO:"
        Locate 12, 14
        Locate , 14: Print "1 - KICK OFF"
        Locate , 14: Print "2 - RECEIVE"
        Locate , 14: Print "3 - DEFEND GOAL WITH WIND AT BACK"
        Locate , 14: Print "4 - DEFEND GOAL WITH WIND IN FACE"

        Do
            I$ = GetKeyPress$
            koChoice = Val(I$)
        Loop Until koChoice >= 1 And koChoice <= 4

    End If

    Screen , , 0, 0

    Select Case koChoice

        Case 1, 2

            Call SCOREBOARD

            'Computer Player
            If playerMode = 1 And D = compTeam Or playerMode = 2 Then

                defendChoice = 1

            Else

                Call OPTIONBOX(7)

                Print gameTeams$(1 - winTeam); " DO YOU ELECT TO:"
                Locate 12, 14
                Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
                Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

                Do
                    I$ = GetKeyPress$
                    defendChoice = Val(I$)
                Loop Until defendChoice >= 1 And defendChoice <= 2

            End If

            Screen , , 0, 0

            If defendChoice = 1 Then
                climate(0) = D
            Else
                'defendChoice = 2
                climate(0) = 1 - D
            End If

            If koChoice = 1 Then
                Call CHANGEPOSS
                winTeam = 1 - winTeam
            End If


        Case 3, 4

            If koChoice = 3 Then
                climate(0) = P1
            Else
                'koChoice = 4
                climate(0) = D
            End If

            Swap P1, D

            Call SCOREBOARD

            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                defendChoice = 2

            Else

                Call OPTIONBOX(7)
                Print gameTeams$(1 - winTeam); " DO YOU ELECT TO:"
                Locate 12, 14
                Locate , 14: Print "1 - KICK OFF"
                Locate , 14: Print "2 - RECEIVE"

                Do
                    I$ = GetKeyPress$
                    defendChoice = Val(I$)
                Loop Until defendChoice >= 1 And defendChoice <= 2

            End If

            Screen , , 0, 0

            If defendChoice = 1 Then
                Call CHANGEPOSS
            End If

    End Select

End Sub


'----------------------------------------
'    KOChoices_Defer Subroutine
'----------------------------------------
'This subroutine gives the user choices
'to handle a college kick off.
Sub KOChoices_Defer ()

    If quarter > 2 Then

        Call KOChoices_NoDefer

    Else

        Call SCOREBOARD

        If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

            koChoice = 2

        Else

            Call OPTIONBOX(9)
            Print gameTeams$(P1); " DO YOU ELECT TO:"
            Locate 12, 14
            Locate , 14: Print "1 - KICK OFF"
            Locate , 14: Print "2 - RECEIVE"
            Locate , 14: Print "3 - DEFEND GOAL WITH WIND AT BACK"
            Locate , 14: Print "4 - DEFEND GOAL WITH WIND IN FACE"
            Locate , 14: Print "5 - DEFER CHOICE TO 2ND HALF"

            Do
                I$ = GetKeyPress$
                koChoice = Val(I$)
            Loop Until koChoice >= 1 And koChoice <= 5

        End If

        Screen , , 0, 0

        Select Case koChoice

            Case 1, 2

                Call SCOREBOARD

                If playerMode = 1 And D = compTeam Or playerMode = 2 Then

                    defendChoice = 1

                Else

                    Call OPTIONBOX(7)
                    Print gameTeams$(D); " DO YOU ELECT TO:"
                    Locate 12, 14
                    Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
                    Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

                    Do
                        I$ = GetKeyPress$
                        defendChoice = Val(I$)
                    Loop Until defendChoice >= 1 And defendChoice <= 2
                End If

                Screen , , 0, 0

                If defendChoice = 1 Then
                    climate(0) = D
                Else
                    'defendChoice = 2
                    climate(0) = 1 - D
                End If

                If koChoice = 1 Then
                    Call CHANGEPOSS
                    winTeam = 1 - winTeam
                End If

            Case 3, 4

                If koChoice = 3 Then
                    climate(0) = P1
                Else
                    'koChoice = 4
                    climate(0) = D
                End If

                Swap P1, D

                Call SCOREBOARD

                If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                    defendChoice = 2

                Else

                    Call OPTIONBOX(7)
                    Print gameTeams$(P1); " DO YOU ELECT TO:"

                    Locate 12, 14
                    Locate , 14: Print "1 - KICK OFF"
                    Locate , 14: Print "2 - RECEIVE"

                    Do
                        I$ = GetKeyPress$
                        defendChoice = Val(I$)
                    Loop Until defendChoice >= 1 And defendChoice <= 2

                End If

                Screen , , 0, 0

                If defendChoice = 1 Then
                    Call CHANGEPOSS
                End If

            Case 5

                Swap P1, D
                winTeam = P1
                Call KOChoices_NoDefer

        End Select

    End If

End Sub


'----------------------------------------
'       PuntNotReturned Subroutine
'----------------------------------------
'This subroutine factors in for all the
'possible reasons a kick would not be
'returned, and handles PBP for each.
Sub PuntNotReturned (fairCatchPossible, puntType)

    isTouchback = 0

    'Let's see where the ball landed
    ydLine = ydLine + ydsGained
    ydsToScore = ydLine

    Call YARDLINE

    'Call pbplog(13905)

    'Calculating yard line for return team
    ydLine = 100 - ydLine

    Call AddPossessionTime(4, 7)

    'RN0 = Int(Rnd(1) * 47) + 1
    Call Rolld100(RN0)

    If RN0 <= 8 Or puntType = 1 Then

        RN1 = Int(Rnd(1) * 4) + 1

        'Out-of-bounds
        If RN1 <= 2 Then
            pbpString$ = "THE BALL GOES OUT OF BOUNDS AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
        Else
            pbpString$ = "THE KICK SOARS OUT OF BOUNDS AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
        End If

        'Call pbplog(13921)

    Else

        'Downed or inside the team's 15
        'If ydLineTeam <= 15 And (RN0 >= 10 And RN0 <= 42) Then
        If ydLineTeam <= 15 Then

            'Call pbplog(13929)

            If RN0 <= 45 Then

                'Call pbplog(13933)

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

                Select Case RN1

                    Case 1
                        pbpString$ = "IT IS DOWNED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"

                    Case 2
                        pbpString$ = recoverTeam$ + "DOWNS IT AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"

                    Case 3
                        pbpString$ = "THE BALL IS DOWNED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"

                    Case 4
                        pbpString$ = "DOWNED BY" + recoverTeam$ + "ON THE" + Str$(Int(ydLineTeam)) + " YARD LINE"

                End Select

                Call PBP

                fairCatchPossible = 0

            Else

                'Fair catch outside 15
                fairCatchPossible = 1
                'Call pbplog(13979)

            End If

        Else

            'Outside the 15, outside fair catch
            'Call pbplog(13985)

        End If

    End If

    If fairCatchPossible = 1 And currDown = 4 Then

        'Call pbplog(13992)
        pbpString$ = "A FAIR CATCH IS CALLED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"

        If (ruleOptType > 0 And ruleOptPro >= 9) Or (ruleOptType = 0 And ruleOptColl >= 11) Then
            'Newer rules allow touchback on fair catch
            isTouchback = 1
        End If

    Else

        'No fair catch
        'Call pbplog(14000)

    End If

    Call PBP

    If isTouchback = 1 Then
        'Punt ended up in end zone
        Call Touchback
    Else
        'Punt returned
        Call CHANGEPOSS
        currDown = 1
        ydsToFirst = 10
    End If

End Sub


'----------------------------------------
'         TAKESAFETY Subroutine
'----------------------------------------
'This subroutine handles ball position,
'variables and Play-by-play for when a
'player takes a safety voluntarily.
Sub TAKESAFETY ()

    Call AddPossessionTime(4, 7)

    ydsGained = (ydLine * -1) - 1
    gameStatsTeam(P1, 1) = gameStatsTeam(P1, 1) + 1
    gameStatsTeam(P1, 2) = gameStatsTeam(P1, 2) + ydsGained

    pbpString$ = punter$(P1, 0) + " IS HEADED OUT OF THE END ZONE"
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
Sub FINDINT ()

    M5 = 0
    RN0 = Int(Rnd(1) * teamInts(1 - P1)) + 1

    For Z1 = 0 To 9

        M5 = M5 + playerInts(1 - P1, Z1)

        If RN0 <= M5 Then
            defTeam$ = defInts$(1 - P1, Z1)
            Exit For
        End If

    Next

End Sub


'----------------------------------------
'           YARDAGE Subroutine
'----------------------------------------
'This subroutine I believe is used to
'round yardage gained
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

    'Call Rolld100(I3)
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
                    If I2 <> 1 Then
                        '6 to 53
                        ydsGained = Int((Rnd(1) * 39) + 12) + (prYdsPerRet(P1, puntRetNbr) - 3) * 3
                    End If

                Case 993 To 1000
                    '0.8% chance of TD; was 1.1%
                    If I2 <> 1 Then
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
                    '4% chance of TD; was 6%
                    If I2 <> 1 Then
                        ydsGained = 110
                    End If

            End Select

        Case Is >= 6

            If RN0 > 925 And I2 <> 1 Then
                '7.5% chance of TD; was 10%
                ydsGained = 110
            Else
                '95% chance
                '0 to 34
                ydsGained = Int((Rnd(1) * 26)) + (prYdsPerRet(P1, puntRetNbr) - 6) * 3
            End If

    End Select

    X1 = 4
    Call YARDAGE
    Call MARKER

    pbpString$ = "THE BALL IS TAKEN BY " + pret$(P1, puntRetNbr) + " AT THE" + Str$(Int(ydLineTeam))
    Call PBP

    'Print #11, "Punt RETURNED for: "; ydsGained; "yards"
    'Print #11, "Punter Rating = "; prYdsPerRet(P1, puntRetNbr); "yds / return"
    'Print #11, "Random # = "; RN0

    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

End Sub


'----------------------------------------
'          KickFakeYards Subroutine
'----------------------------------------
'This subroutine handles yardage gains and
'Play-by-Play text for a fake play.
Sub KickFakeYards (choice)

    pbpString$ = "IT'S A FAKE!!! THE UPBACK IS RUNNING THE BALL"
    Call PBP

    Call Rolld100(RN0)

    Select Case choice

        Case 0

            If RN0 <= 36 Then
                ydsGained = Int(Rnd(1) * 27)
            Else
                ydsGained = Int(Rnd(1) * 6) - 2
            End If

        Case 1

            Select Case RN0

                Case 1 To 36
                    ydsGained = Int(Rnd(1) * 21)

                Case 37 To 90
                    ydsGained = Int(Rnd(1) * 11) - 4

                Case 91 To 100
                    ydsGained = 100 - ydLine

            End Select

        Case 2

            If RN0 <= 8 Then
                ydsGained = Int(Rnd(1) * 21)
            Else
                ydsGained = Int(Rnd(1) * 7) - 7
            End If

    End Select

    Call YARDAGE

    playSegment = 1

    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    pbpString$ = "THE UPBACK GAINED"
    Print
    X = 10
    X1 = 11

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
        pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
    Else
        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
    End If

    pbpString$ = pbpString$ + " YARDS ON THE FAKE RUN"
    Call PBP

End Sub


'----------------------------------------
'           FUMBLE Subroutine
'----------------------------------------
'This subroutine I believe tries to calc-
'ulate if a player has fumbled the ball.
Sub FUMBLE ()

    Call Rolld100(fumbleChance)

    If revFumble = 1 Then

        fumbleChance = fumbleChance - 10
        revFumble = 0

    Else

        If F% = 1 Then fumbleChance = fumbleChance - 6
        If F% = 2 Then fumbleChance = fumbleChance + 1
        If F% = 4 Then fumbleChance = fumbleChance - 2
        If F% = 5 Then fumbleChance = fumbleChance + 2

        If playCall = 1 Then fumbleChance = fumbleChance + 2
        If playCall = 2 Then fumbleChance = fumbleChance - 1
        If playCall = 5 Then fumbleChance = fumbleChance - 1

    End If

    fumbleChance = fumbleChance - climate(5)
    
    F% = 0

    If fumbleChance <= leagRat_GAME(P1, 3) + leagRat_GAME(D, 4) Then
        ballFumbled = 1
    End If

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

    CP = Int(Rnd(1) * 16) + 35

    If hasRunFF(P1) > 0 Then

        catchPctSuccess = 3

        'increase chance of INT
        intChance = intChance + 10

        If ffPctSuccess = 5 Then ffPctSuccess = 30

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

    If RV%(P9) = 0 Then

        Select Case RN0

            Case 1 To 20
                ydsGained = (Int(Rnd(1) * 8) + 5) * -1
                revFumble = 1

            Case 21 To 80
                ydsGained = Int(Rnd(1) * 11)

            Case 81 To 100
                ydsGained = Int(Rnd(1) * 81)

        End Select

    Else

        Select Case RN0

            Case 1 To 5:
                ydsGained = Int(Rnd(1) * 81)

            Case 6 To 55:
                ydsGained = Int(Rnd(1) * 11)

            Case 56 To 100:
                ydsGained = (Int(Rnd(1) * 8) + 5) * -1
                revFumble = 1

        End Select

    End If

    RV%(P9) = 1

End Sub


'----------------------------------------
'           FAKERUN Subroutine
'----------------------------------------
'This subroutine handles yardage and the
'Play-by-Play text for a fake run on a
'kicking lineup.
Sub FAKERUN (choice)

    pbpString$ = "IT'S A FAKE!!! THE HOLDER IS RUNNING THE BALL"
    Call PBP

    Call Rolld100(RN0)

    Select Case choice
        Case 1
            Select Case RN0
                Case 1 To 20: ydsGained = Int(Rnd(1) * 21)
                Case 21 To 95: ydsGained = Int(Rnd(1) * 11) - 4
                Case 96 To 100: ydsGained = 100 - ydLine
            End Select

        Case 2
            If RN0 <= 8 Then
                ydsGained = Int(Rnd(1) * 21)
            Else
                ydsGained = Int(Rnd(1) * 7) - 7
            End If

    End Select

    Call YARDAGE

    playSegment = 1

    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    pbpString$ = "THE HOLDER GAINED"
    Print

    X = 10
    X1 = 11

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
        pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
    Else
        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
    End If

    pbpString$ = pbpString$ + " YARDS ON THE FAKE RUN"
    Call PBP

End Sub


'----------------------------------------
'           HANDOFF Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'text for a run play.
Sub HANDOFF ()

    If dLine = 11 Then
        pbpString$ = defFormation$(dLine)
    Else
        pbpString$ = defFormation$(dLine) + " " + passCov$(coverage)
    End If

    Call PBP

    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP"

    If playCall = 4 Or playCall = 5 Then

        fieldSide = Int(Rnd(1) * 2)

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
'          PASSBALL Subroutine
'----------------------------------------
'This subroutine helps setup the stage for
'a pass play, including Play-by-Play text.
'This includes determining who the ball is
'going to and where they might be.
Sub PASSBALL (passChance)

    S6 = 0

    Call QBLINE

    playType = 2

    'Pick QB based on contribution
    Call Rolld100(qbChance)

    For J = 3 To 0 Step -1
        If qbChance <= qbNumAtt(P1, J) Then I1 = J
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

    Do

        Call Rolld100(recChance)
        newRandomNum% = 0

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

        If I3 > 9 Then
            If wdRec$(P1, I3 - 10) = qbacks$(P1, qbNumber(P1)) Then
                newRandomNum% = 1
            End If
        End If

        If I3 < 10 Then
            If rbacks$(P1, I3) = qbacks$(P1, qbNumber(P1)) Then
                newRandomNum% = 1
            End If
        End If

    Loop Until newRandomNum% = 0

    If dLine = 11 Then
        pbpString$ = defFormation$(dLine)
    Else
        pbpString$ = defFormation$(dLine) + " " + passCov$(coverage)
    End If

    Call PBP
    Call PASSPBP

    If I3 > 9 Then
        targetWRName$ = wdRec$(P1, I3 - 10)
    Else
        targetWRName$ = rbacks$(P1, I3)
    End If

    RN0 = Int(Rnd(1) * 6) + 1

    Select Case RN0
        Case 1: pbpString$ = "HE'S GOT "
        Case 2: pbpString$ = "HE SPOTS "
        Case 3: pbpString$ = "HE'S LOOKING FOR "
        Case 4: pbpString$ = "HE SEES "
        Case 5: pbpString$ = "HE'S SEARCHING FOR "
        Case 6: pbpString$ = "HE'S GOT "
    End Select

    Call WHICHRECEIVER

    passDir = Int(Rnd(1) * 5) + 1
    fieldSide = Int(Rnd(1) * 2)

    Select Case passDir

        Case 1, 6
            pbpString$ = pbpString$ + " IN THE OPEN"

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

    Call Rolld100(passChance)

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

    If P1 = gameLoc Then
        'Add 3% for home game
        expCompPct = expCompPct + 3
    Else
        'P1 <> gameLoc
        If gameLoc <> 2 Then expCompPct = expCompPct - 3
    End If

    'Adjust ?? based on position
    Select Case ydLine

        'Goal Line to Own 0
        Case 0 To 40.001:
            ballPosCmpAdj = 3

            'Own 40 to Opp 20
        Case 40.002 To 80.001:
            ballPosCmpAdj = -2

            'Opp 20 to Opp 15
        Case 80.002 To 85.001:
            ballPosCmpAdj = -4

            'Opp 15 to Opp 10
        Case 85.002 To 90.001:
            ballPosCmpAdj = -10

            'Opp 10 to Opp 5
        Case 90.002 To 95.001:
            ballPosCmpAdj = -26

            'Opp 5 to  Goal
        Case Is > 95.001:
            ballPosCmpAdj = -13

    End Select

    'Adjust for pass type
    passTypeCmpAdj = passCovAdj(1, playCall - 8, coverage)

    'Adjust for wind direction NOT in team's favor
    windCmpAdj = 0

    If climate(0) <> P1 Then

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

    If X1 <> 7 Then
        K1(nbrScores, 1) = P1
    Else
        K1(nbrScores, 1) = D
    End If

    K1(nbrScores, 0) = X1
    K1(nbrScores, 5) = 100 - ydLine

    If X1 = 3 Then K1(nbrScores, 5) = 100 - kickYL

    If X1 = 8 Then K1(nbrScores, 5) = 117 + goalPostAdj - ydLine

    K1(nbrScores, 2) = I1
    If X1 = 5 Or X1 = 12 Then
        K1(nbrScores, 2) = Z1
    End If

    K1(nbrScores, 3) = I3
    If K1(nbrScores, 5) < 1 Then
        K1(nbrScores, 5) = 1
    End If

End Sub


'----------------------------------------
'          COMPILER Subroutine
'----------------------------------------
' ...explanation...
Sub COMPILER ()

    If X >= 1 And X <= 4 Then

        Select Case X

            Case 1, 4
                'Increase team rush attempts
                gameStatsTeam(P1, 1) = gameStatsTeam(P1, 1) + 1

                'Increase team rush yards
                If ydsGained + ydLine < 99.95 Then
                    gameStatsTeam(P1, 2) = gameStatsTeam(P1, 2) + ydsGained
                Else
                    gameStatsTeam(P1, 2) = gameStatsTeam(P1, 2) + 100 - ydLine
                End If

            Case 2
                'Increase team pass attempts
                gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1

            Case 3
                'Increase team pass attempts + completions
                gameStatsTeam(P1, 3) = gameStatsTeam(P1, 3) + 1
                gameStatsTeam(P1, 4) = gameStatsTeam(P1, 4) + 1

                'Increase team passing yards
                '-- Do we never substract passing yards for a sack?
                '-- Or does that happen elsewuere?
                If ydsGained + ydLine < 99.95 Then
                    gameStatsTeam(P1, 6) = gameStatsTeam(P1, 6) + ydsGained
                Else
                    gameStatsTeam(P1, 6) = gameStatsTeam(P1, 6) + 100 - ydLine
                End If

        End Select


        Select Case X

            Case 1

                'Increase player rush attempt
                gameStatsPlayer(P1, I1, 0) = gameStatsPlayer(P1, I1, 0) + 1

                'Inrease player rush yards and scores
                If ydsGained + ydLine >= 99.95 Then
                    gameStatsPlayer(P1, I1, 1) = gameStatsPlayer(P1, I1, 1) + 100 - ydLine
                    gameStatsPlayer(P1, I1, 2) = gameStatsPlayer(P1, I1, 2) + 1
                    BG = 100 - ydLine
                    If BG > runBacks(P1, I1) Then
                        gameRB$(P1, I1) = "t"
                        runBacks(P1, I1) = BG
                    End If
                Else
                    gameStatsPlayer(P1, I1, 1) = gameStatsPlayer(P1, I1, 1) + ydsGained
                    If ydsGained > runBacks(P1, I1) Then
                        gameRB$(P1, I1) = ""
                        runBacks(P1, I1) = ydsGained
                    End If
                End If

            Case 2

                If HB% = 0 Then
                    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                End If

            Case 3

                If HB% <> 1 Then

                    gameStatsPlayer(P1, I1, 9) = gameStatsPlayer(P1, I1, 9) + 1
                    gameStatsPlayer(P1, I1, 10) = gameStatsPlayer(P1, I1, 10) + 1

                    If ydsGained + ydLine >= 99.95 Then
                        gameStatsPlayer(P1, I1, 12) = gameStatsPlayer(P1, I1, 12) + 100 - ydLine
                        gameStatsPlayer(P1, I1, 13) = gameStatsPlayer(P1, I1, 13) + 1
                    Else
                        gameStatsPlayer(P1, I1, 12) = gameStatsPlayer(P1, I1, 12) + ydsGained
                    End If

                End If

                If I3 > 9 Then

                    If ydsGained + ydLine >= 99.95 Then
                        gameStatsPlayer(P1, I3 - 10, 7) = gameStatsPlayer(P1, I3 - 10, 7) + 100 - ydLine
                        gameStatsPlayer(P1, I3 - 10, 8) = gameStatsPlayer(P1, I3 - 10, 8) + 1
                    Else
                        gameStatsPlayer(P1, I3 - 10, 7) = gameStatsPlayer(P1, I3 - 10, 7) + ydsGained
                    End If

                    gameStatsPlayer(P1, I3 - 10, 6) = gameStatsPlayer(P1, I3 - 10, 6) + 1

                Else

                    If ydsGained + ydLine >= 99.95 Then
                        gameStatsPlayer(P1, I3, 4) = gameStatsPlayer(P1, I3, 4) + 100 - ydLine
                        gameStatsPlayer(P1, I3, 5) = gameStatsPlayer(P1, I3, 5) + 1
                    Else
                        gameStatsPlayer(P1, I3, 4) = gameStatsPlayer(P1, I3, 4) + ydsGained
                    End If

                    gameStatsPlayer(P1, I3, 3) = gameStatsPlayer(P1, I3, 3) + 1

                End If

            Case 4

                If W5 = 0 Or W5 = 1 And ruleOptType = 0 Then
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 0) = gameStatsPlayer(P1, qbNumber(P1) + 10, 0) + 1
                End If

                If W5 = 0 And ydsGained + ydLine < 99.95 Or W5 = 1 And ruleOptType = 0 And ydsGained + ydLine < 99.95 Then
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 1) = gameStatsPlayer(P1, qbNumber(P1) + 10, 1) + ydsGained
                End If

                If W5 = 0 And ydsGained + ydLine >= 99.95 Or W5 = 1 And ruleOptType = 0 And ydsGained + ydLine >= 99.95 Then
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 1) = gameStatsPlayer(P1, qbNumber(P1) + 10, 1) + 100 - ydLine
                    gameStatsPlayer(P1, qbNumber(P1) + 10, 2) = gameStatsPlayer(P1, qbNumber(P1) + 10, 2) + 1
                End If

                If W5 = 1 Then
                    sackStatsTeam(P1, 0) = sackStatsTeam(P1, 0) + 1
                    sackStatsTeam(P1, 1) = sackStatsTeam(P1, 1) + ydsGained * -1
                    sackStatsPlayer(1 - P1, Z1, 0) = sackStatsPlayer(1 - P1, Z1, 0) + 1
                    sackStatsPlayer(1 - P1, Z1, 1) = sackStatsPlayer(1 - P1, Z1, 1) + ydsGained * -1
                End If

        End Select

    End If

    If HB% = 1 Then HB% = 0
    X = 0

End Sub


'----------------------------------------
'           TWOMIN Subroutine
'----------------------------------------
'This subroutine gives the user a menu of
'options for the last 2 minutes of the
'game and handles the appropriate changes
'to team variables
Sub TwoMinMenu (menuChoice)

    If pbpDelay > 0 Then

        Call OPTIONBOX(7)

        Locate 9, 14: Print "1 - HUDDLE"
        Locate , 14: Print "2 - TIME OUT "; gameTeams$(P1); " "; timeouts(P1)
        Locate , 14: Print "3 - TIME OUT "; gameTeams$(D); " "; timeouts(D):
        Locate , 14: Print "4 - HURRY UP OFFENSE"
        Locate , 14: Print "5 - THROW AWAY PASS"

    End If

    Do

        If playerMode > 0 Then

            scoreDiff = score(P1, 0) - score(D, 0)

            'Is it possible for more than one of these to be TRUE at the same time?
            'If not, consider setting a flag value to then be checked outside the if
            'That would only give us one Exit Do
            If (playerMode = 1 And D = compTeam Or playerMode = 2) Then

                If scoreDiff > 0 And scoreDiff <= 14 And timeouts(D) > 0 And quarter = 4 Then
                    menuChoice = 3
                Else

                    If timeouts(P1) > 0 Then
                        menuChoice = 2
                    Else
                        menuChoice = 4
                    End If

                End If
            End If

            If playerMode = 2 Then
                menuChoice = 1
                Exit Do
            End If

            If quarter = 2 And ydLine > 15 And (playerMode = 1 And P1 = compTeam Or playerMode = 2) And scoreDiff <= 0 Then
                If timeouts(P1) > 0 Then
                    menuChoice = 2
                Else
                    menuChoice = 4
                End If
                Exit Do
            End If

            'leagRat_GAME(P1, 7) = league Rush % (how often run plays are called)
            If quarter = 2 And ydLine > 40 And scoreDiff > 0 And leagRat_GAME(P1, 7) < 65 And (playerMode = 1 And P1 = compTeam Or playerMode = 2) And timeouts(P1) > 0 Then
                menuChoice = 2
                Exit Do
            End If

            If quarter = 2 And ydLine > 40 And scoreDiff >= 0 And leagRat_GAME(P1, 7) < 65 And (playerMode = 2 Or playerMode = 1 And P1 = compTeam) Then
                menuChoice = 4
                Exit Do
            End If

        End If

        Do
            I$ = GetKeyPress$
            menuChoice = Val(I$)
        Loop Until menuChoice >= 1 And menuChoice <= 5

    Loop Until Not ((menuChoice = 2 Or menuChoice = 4 Or menuChoice = 5) And playerMode = 1 And P1 = compTeam) Or (menuChoice = 3 And playerMode = 1 And D = compTeam)
    '(menuChoice = 1 Or menuChoice = 3 And Not (playerMode = 1 And P1 = compTeam)) Or (menuChoice <> 3 And Not (playerMode = 1 And D=compTeam))

    Color 0, 5 * (1 - BW)

End Sub


'----------------------------------------
'       FieldGoalChance Subroutine
'----------------------------------------
'This subroutine calculcates the distance
'for a FG
Sub FieldGoalChance ()

    windDist = 0

    If climate(0) = P1 Then

        If climate(1) = -7 Then
            windDist = -5
        End If
        If climate(1) = -15 Then
            windDist = -15
        End If

    Else
        If climate(1) = -7 Then
            windDist = 7
        End If
        If climate(1) = -15 Then
            windDist = 15
        End If

    End If

    'Increase average FG% chance for newer eras
    Select Case teamYears(P1)

        Case 1970 To 1982
            adjFGPct(0) = 86
            adjFGPct(1) = 65
            adjFGPct(2) = 46
            adjFGPct(3) = 26
            adjFGPct(4) = 20

        Case 1983 To 2007
            adjFGPct(0) = 95
            adjFGPct(1) = 81
            adjFGPct(2) = 62
            adjFGPct(3) = 49
            adjFGPct(4) = 43

        Case Is >= 2008
            adjFGPct(0) = 98
            adjFGPct(1) = 91
            adjFGPct(2) = 76
            adjFGPct(3) = 67
            adjFGPct(4) = 61

    End Select


    For I = 4 To 0 Step -1:

        If ydLine >= adjFGLine(I) + goalPostAdj + windDist Then

            fgSuccessChance = adjFGPct(I) - ((9 - kickerFGA(P1, activeKicker(P1))) * 5)

        End If

        'Inside opponent's 32? (Less than 50 yard attempt)
        If ydLine >= adjFGLine(I) + goalPostAdj + windDist And ydLine >= 68 + goalPostAdj Then
            'Adjust success chance based on how far it is from the midpoint of distance range
            'For example, for 30-39 yd attempts, 35 yd is the midpoint
            'So success for 35 is the average; 34 yd attempt is +1%; 36 yd is -1%
            fgSuccessChance = fgSuccessChance + (ydLine - adjFGLine(I)) + goalPostAdj - 4 + windDist
        End If

    Next I

    If fgSuccessChance <= 0 Then
        'Always a 5% chance success
        'Data backs this up since even the worst league years,
        'kickers made 5% of kicks over 50+
        fgSuccessChance = 5
    End If

    If fgSuccessChance >= 100 Then
        'Always a 2% chance failure
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

    If dLine > 3 And dLine < 6 Then

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

    If I2 <= 30 Then
        If RN7 <= goalLnYdAdj(I2, 1) Then ydsGained = ydsGained + goalLnYdAdj(I2, 2)
    Else
        'I2 < 30
        If RN7 <= goalLnYdAdj(30, 1) Then ydsGained = ydsGained + goalLnYdAdj(30, 2)
    End If

End Sub


'----------------------------------------
'           PASSPBP Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'string for pass play.
Sub PASSPBP ()

    Call Rolld100(RN0)

    If coverage >= 7 Then

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

                If defSacks(1 - P1) = 0 Then
                    defTeam$ = gameTeams$(1 - P1)
                Else

                    M5 = 0
                    RN1 = Int(Rnd(1) * teamSacks(1 - P1)) + 1

                    For Z1 = 0 To 14
                        M5 = M5 + playerSacks(1 - P1, Z1)
                        If RN1 <= M5 Then
                            defTeam$ = defSacks$(1 - P1, Z1)
                            Exit For
                        End If
                    Next

                End If

        End Select

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

    End If

    Call PBP

End Sub


'----------------------------------------
'            SACK Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'text for a sack.
Sub SACK ()

    pbpString$ = "BUT HE WON'T BE ABLE TO GET THIS PASS AWAY!"
    Call PBP

    pbpString$ = qbacks$(P1, I1)

    If HB% = 1 Then pbpString$ = rbacks$(P1, I1)

    If endZone = 1 Then
        pbpString$ = pbpString$ + " IS DROPPED"
    Else

        RN0 = Int(Rnd(1) * 12) + 1
        Call Rolld100(RN1)

        Select Case RN0
            Case 1: pbpString$ = pbpString$ + " IS SACKED"
            Case 2: If RN1 <= 20 Then pbpString$ = pbpString$ + " IS NAILED"
            Case 3: If RN1 <= 30 Then pbpString$ = pbpString$ + " IS BURIED"
            Case 4: If RN1 <= 40 Then pbpString$ = pbpString$ + " IS DRILLED"
            Case 5: If RN1 <= 50 Then pbpString$ = pbpString$ + " IS HIT"
            Case 6: If RN1 <= 60 Then pbpString$ = pbpString$ + " IS THROWN"
            Case 7: If RN1 <= 70 Then pbpString$ = pbpString$ + " IS HAMMERED"
            Case 8: If RN1 <= 80 Then pbpString$ = pbpString$ + " IS TACKLED"
            Case 9: If RN1 <= 90 Then pbpString$ = pbpString$ + " IS SWARMED UNDER"
            Case 10: pbpString$ = pbpString$ + " IS LEVELED"
            Case 11: pbpString$ = pbpString$ + " IS SNOWED UNDER"
            Case 12: pbpString$ = pbpString$ + " IS WRAPPED UP"
        End Select

    End If

    pbpString$ = pbpString$ + " FOR A" + Str$(Int(ydsGained + .5)) + " YARD LOSS"

End Sub


'----------------------------------------
'           QBLINE Subroutine
'----------------------------------------
'This subroutine handles Play-by-Play for
'when the QB approaches the o-line.
Sub QBLINE ()

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
    If Not (currDown <= 2 Or currDown = 3 And RN1 <= 80) Then

        RN0 = Int(Rnd(1) * 8) + 1

        Select Case RN0
            Case 1: pbpString$ = gameTeams$(P1) + " COMES UP TO THE LINE"
            Case 2: pbpString$ = gameTeams$(P1) + " BREAKS THE HUDDLE AND LINES UP"
            Case 3: pbpString$ = "THE " + gameMascots$(P1) + " COME UP TO THE LINE"
            Case 4: pbpString$ = "THE " + gameMascots$(P1) + " BREAK THE HUDDLE AND LINE UP"
            Case 5: pbpString$ = qbacks$(P1, qback) + " BARKS OUT THE SIGNALS"
            Case 6: pbpString$ = qbacks$(P1, qback) + " LOOKS OVER THE DEFENSE"
            Case 7: pbpString$ = "HERE IS THE " + downDesc$(currDown) + " DOWN AND" + Str$(Int(ydsToFirst)) + " YARD PLAY FOR " + gameTeams$(P1)
            Case 8: pbpString$ = "HERE IS THE " + downDesc$(currDown) + " DOWN AND" + Str$(Int(ydsToFirst)) + " YARD PLAY FOR THE " + gameMascots$(P1)
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

    pbpString$ = gameTeams$(P1) + " IS LINING UP FOR THE KICKOFF"
    Call PBP

    pbpString$ = "IT'S A SQUIBBER DOWN THE FIELD"
    Call PBP

    Swap P1, D

    X = 0

    kickYL = 100 - (ydLine + Int(Rnd(1) * 16) + 29)
    kickYL = kickYL + (Int(Rnd(1) * 11) - 5) / 10
    ydLine = kickYL
    ydsToScore = 100 - ydLine

    Call MARKER
    Call YARDLINE

    pbpString$ = "KICK TAKEN BY AN UPMAN AT THE" + Str$(Int(kickYL + .5)) + " YD LINE "
    Call PBP

    ydsGained = Int(Rnd(1) * 7) + 9

    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

    ydLine = kickYL + ydsGained
    F% = 3

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
        Case 2: pbpString$ = "HE MEETS A STONE WALL"
        Case 3: pbpString$ = "HE BOUNCES OFF ONE TACKLER"
        Case 4: pbpString$ = "HE SLIPS AND FALLS"
        Case 5: pbpString$ = "THE HOLE IS CLOSED"
        Case 6: pbpString$ = "HE FIGHTS FOR EXTRA YARDAGE"
        Case 7: pbpString$ = "HE'S STOOD UP!"
        Case 8: pbpString$ = "HIS FEET GO OUT FROM UNDER HIM"
        Case 9: pbpString$ = "HE SLIPS TO THE TURF"
        Case 10: pbpString$ = "DOWN HE GOES!!"
        Case 11: pbpString$ = "HE SLIPS TRYING TO MAKE HIS CUT"
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

        Case 1 To 35: pbpString$ = "NO GOOD"

        Case 36 To 60: pbpString$ = "IT SAILS WIDE...NO GOOD"

        Case 61 To 80: pbpString$ = "HE MISSED IT!"

        Case 81 To 85: pbpString$ = "IT HITS THE UPRIGHTS....NO GOOD!"

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

        Case 1 To 10: pbpString$ = "IT'S GOOD!!!"
        Case 11 To 20: pbpString$ = "THE KICK IS GOOD!!!"
        Case 21 To 30: pbpString$ = "HE PUTS IT THROUGH THE UPRIGHTS!!!"
        Case 31 To 40: pbpString$ = "HE SPLITS THE UPRIGHTS!!!"
        Case 41 To 50: pbpString$ = "IT'S GOOD!!!"
        Case 51 To 60: pbpString$ = "HE HAMMERS IT THROUGH!!!"
        Case 61 To 70: pbpString$ = "RIGHT DOWN THE MIDDLE!!!"
        Case 71 To 80: pbpString$ = "IT HAS THE DISTANCE...IT'S GOOD!!!"
        Case 81 To 90: pbpString$ = "HE NAILS IT...IT'S GOOD!!!"
        Case 91 To 100: pbpString$ = "IT'S GOOD!"

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

        Case 1 To 50: pbpString$ = "NO GOOD"
        Case 51 To 70: pbpString$ = "HE MISSED IT!"
        Case 71 To 75: pbpString$ = "BAD SNAP FROM CENTER!! NO KICK": gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) = gameStatsPlayer(P1, 6 + activeKicker(P1) * 4, 17) - 1
        Case 76 To 95: pbpString$ = "THE KICK SAILS WIDE!!...NO GOOD!"
        Case 96 To 100: pbpString$ = "IT HITS THE UPRIGHTS!!...NO GOOD!"

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
        Case 1 To 25: pbpString$ = "IT'S GOOD!!!"
        Case 26 To 50: pbpString$ = "THE KICK IS GOOD!!!"
        Case 51 To 60: pbpString$ = "HE PUTS IT THROUGH THE UPRIGHTS!!!"
        Case 61 To 70: pbpString$ = "HE SPLITS THE UPRIGHTS!!!"
        Case 71 To 80: pbpString$ = "IT'S GOOD!!!"
        Case 81 To 90: pbpString$ = "HE HAMMERS IT THROUGH!!!"
        Case 91 To 100: pbpString$ = "RIGHT DOWN THE MIDDLE!!!"
    End Select

    Call PBP

End Sub


'----------------------------------------
'         KickoffLine Subroutine
'----------------------------------------
'This subroutine handles the setup for a
'kick off by determining where to place
'the tee
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

            Case 2 To 3
                'In 1974, the NFL moved the kick line to the 35.
                'They had a return rate of 92%
                returnRate = 92
            
            Case 4 To 5
                'In 1994, the NFL moved the kick line to the 30.
                'They had a return rate up to 88%
                returnRate = 88
                
            Case 6 To 9
                'In 2011, the NFL moved the kick line to the 35.
                'They also eliminated running starts.
                'Return rates fell to nearly 50%
                returnRate = 50

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
        'Kick will travel 55 to 67 yds
        'that is, outside the end zone (returnable)
        kickYL = 100 - (ydLine + Int(Rnd(1) * 13) + 55 + goalPostAdj)
    Else
        'Kick will travel 68 to 75 yds
        'that is, inside the end zone (not returnable)
        kickYL = 100 - (ydLine + Int(Rnd(1) * 8) + 68 + goalPostAdj)
    End If

    If ruleOptType = 0 And ruleOptColl = 0 Then
        'College before 1968
        kickYL = kickYL + 10
    End If

    'Adjust kick distance if factoring for wind

    If climate(0) = D Then
        'Wind direction favors return team for 3-6 yards
        kickYL = kickYL - Int(Rnd(1) * 7) + 3 - climate(1)
    Else
        'Wind direction favors kicking team for -3 to 3 yards
        kickYL = kickYL + Int(Rnd(1) * 7) - 3 + climate(1)
    End If

    kickDist = 100 - ydLine - kickYL

End Sub


'----------------------------------------
'           QBTHROW Subroutine
'----------------------------------------
'This subroutine creates a play-by-play
'string for a QB pass.
Sub QBTHROW ()

    Call Rolld100(RN0)

    Select Case RN0
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

    Call PBP

End Sub


'----------------------------------------
'           RUNPBP Subroutine
'----------------------------------------
'This subroutine creates a play-by-play
'string for a run.
Sub RUNPBP ()

    If playCall <> 4 And playCall <> 5 Then

        RN0 = Int(Rnd(1) * 46) + 1

        Select Case RN0
            Case 1: pbpString$ = rbacks$(P1, I1) + " GAINS"
            Case 2: pbpString$ = rbacks$(P1, I1) + " BLASTS AHEAD FOR"
            Case 3: pbpString$ = rbacks$(P1, I1) + " SCOOTS OUT FOR"
            Case 4: pbpString$ = rbacks$(P1, I1) + " SCOOTS AHEAD FOR"
            Case 5: pbpString$ = rbacks$(P1, I1) + " IS AHEAD FOR"
            Case 6: pbpString$ = rbacks$(P1, I1) + " POPS THROUGH FOR"
            Case 7: pbpString$ = rbacks$(P1, I1) + " SLASHES AHEAD FOR"
            Case 8: pbpString$ = rbacks$(P1, I1) + " HAMMERS AHEAD FOR"
            Case 9: pbpString$ = rbacks$(P1, I1) + " HAMMERS THROUGH FOR"
            Case 10: pbpString$ = rbacks$(P1, I1) + " HAMMERS OUT"
            Case 11: pbpString$ = rbacks$(P1, I1) + " MUSCLES AHEAD FOR"
            Case 12: pbpString$ = rbacks$(P1, I1) + " MUSCLES THROUGH FOR"
            Case 13: pbpString$ = rbacks$(P1, I1) + " MUSCLES OUT"
            Case 14: pbpString$ = rbacks$(P1, I1) + " DASHES AHEAD FOR"
            Case 15: pbpString$ = rbacks$(P1, I1) + " DASHES THROUGH FOR"
            Case 16: pbpString$ = rbacks$(P1, I1) + " DASHES OUT FOR"
            Case 17: pbpString$ = rbacks$(P1, I1) + " RUMBLES AHEAD FOR"
            Case 18: pbpString$ = rbacks$(P1, I1) + " RUMBLES THROUGH FOR"
            Case 19: pbpString$ = rbacks$(P1, I1) + " RUMBLES FOR"
            Case 20: pbpString$ = rbacks$(P1, I1) + " CRACKS AHEAD FOR"
            Case 21: pbpString$ = rbacks$(P1, I1) + " CRACKS THROUGH FOR"
            Case 22: pbpString$ = rbacks$(P1, I1) + " DARTS AHEAD FOR"
            Case 23: pbpString$ = rbacks$(P1, I1) + " DARTS THROUGH FOR"
            Case 24: pbpString$ = rbacks$(P1, I1) + " DARTS FOR"
            Case 25: pbpString$ = rbacks$(P1, I1) + " KNIFES AHEAD FOR"
            Case 26: pbpString$ = rbacks$(P1, I1) + " KNIFES THROUGH FOR"
            Case 27: pbpString$ = rbacks$(P1, I1) + " BULLS HIS WAY FOR"
            Case 28: pbpString$ = rbacks$(P1, I1) + " BULLS AHEAD FOR"
            Case 29: pbpString$ = rbacks$(P1, I1) + " BULLS THROUGH FOR"
            Case 30: pbpString$ = rbacks$(P1, I1) + " MUSCLES HIS WAY FOR"
            Case 31: pbpString$ = rbacks$(P1, I1) + " BLASTS HIS WAY FOR"
            Case 32: pbpString$ = rbacks$(P1, I1) + " POWERS HIS WAY FOR"
            Case 33: pbpString$ = rbacks$(P1, I1) + " POWERS AHEAD FOR"
            Case 34: pbpString$ = rbacks$(P1, I1) + " POWERS THROUGH FOR"
            Case 35: pbpString$ = rbacks$(P1, I1) + " DIVES AHEAD FOR"
            Case 36: pbpString$ = rbacks$(P1, I1) + " DIVES OUT FOR"
            Case 37: pbpString$ = rbacks$(P1, I1) + " DIVES THROUGH FOR"
            Case 38: pbpString$ = rbacks$(P1, I1) + " CHARGES AHEAD FOR"
            Case 39: pbpString$ = rbacks$(P1, I1) + " CHARGES HIS WAY FOR"
            Case 40: pbpString$ = rbacks$(P1, I1) + " CHARGES THROUGH FOR"
            Case 41: pbpString$ = rbacks$(P1, I1) + " SMASHES HIS WAY FOR"
            Case 42: pbpString$ = rbacks$(P1, I1) + " SMASHES AHEAD FOR"
            Case 43: pbpString$ = rbacks$(P1, I1) + " SMASHES THROUGH FOR"
            Case 44: pbpString$ = rbacks$(P1, I1) + " BOLTS AHEAD FOR"
            Case 45: pbpString$ = rbacks$(P1, I1) + " BOLTS THROUGH FOR"
            Case 46: pbpString$ = rbacks$(P1, I1) + " BOLTS FOR"
        End Select

    Else

        RN0 = Int(Rnd(1) * 23) + 1

        Select Case RN0
            Case 1: pbpString$ = rbacks$(P1, I1) + " GALLOPS AHEAD FOR"
            Case 2: pbpString$ = rbacks$(P1, I1) + " GALLOPS THROUGH FOR"
            Case 3: pbpString$ = rbacks$(P1, I1) + " GALLOPS FOR"
            Case 4: pbpString$ = rbacks$(P1, I1) + " DASHES AHEAD FOR"
            Case 5: pbpString$ = rbacks$(P1, I1) + " DASHES THROUGH FOR"
            Case 6: pbpString$ = rbacks$(P1, I1) + " DASHES FOR"
            Case 7: pbpString$ = rbacks$(P1, I1) + " SCOOTS AHEAD FOR"
            Case 8: pbpString$ = rbacks$(P1, I1) + " SCOOTS THROUGH FOR"
            Case 9: pbpString$ = rbacks$(P1, I1) + " SCOOTS OUT FOR"
            Case 10: pbpString$ = rbacks$(P1, I1) + " DASHES OUT FOR"
            Case 11: pbpString$ = rbacks$(P1, I1) + " SCAMPERS THROUGH FOR"
            Case 12: pbpString$ = rbacks$(P1, I1) + " SCAMPERS OUT FOR"
            Case 13: pbpString$ = rbacks$(P1, I1) + " SCAMPERS FOR"
            Case 14: pbpString$ = rbacks$(P1, I1) + " SCAMPERS AHEAD FOR"
            Case 15: pbpString$ = rbacks$(P1, I1) + " SLIPS THROUGH FOR"
            Case 16: pbpString$ = rbacks$(P1, I1) + " SLIPS OUT FOR"
            Case 17: pbpString$ = rbacks$(P1, I1) + " SLIPS AHEAD FOR"
            Case 18: pbpString$ = rbacks$(P1, I1) + " SKIRTS OUTSIDE FOR"
            Case 19: pbpString$ = rbacks$(P1, I1) + " SKIRTS OUT FOR"
            Case 20: pbpString$ = rbacks$(P1, I1) + " TURNS THE CORNER FOR"
            Case 21: pbpString$ = rbacks$(P1, I1) + " TURNS UPFIELD FOR"
            Case 22: pbpString$ = rbacks$(P1, I1) + " GETS THE CORNER FOR"
            Case 23: pbpString$ = rbacks$(P1, I1) + " GETS UPFIELD FOR"

        End Select

    End If

End Sub


'----------------------------------------
'            PCPBP Subroutine
'----------------------------------------
'This subroutine builds the play-by-play
'text for a pass completion, along with
'udpating variables for position.
Sub PCPBP ()

    Call QBTHROW

    If FF% = 0 Then
        'Adjust Comp % based on ???

        Select Case playCall

            Case 9, 10
                If rushYds < 3 Then
                    CP = Int(Rnd(1) * 7) - 2
                Else
                    CP = Int(Rnd(1) * 6) + 2
                End If

            Case 13:
                If rushYds < 3 Then
                    CP = Int(Rnd(1) * 4) - 7
                Else
                    CP = (Int(Rnd(1) * 4) + 1) * -1
                End If

            Case 11, 14, 16, 17, 18:
                If rushYds < 4 Then
                    CP = Int(Rnd(1) * 5) + 3
                Else
                    If rushYds = 4 Or rushYds = 5 Then
                        CP = Int(Rnd(1) * 9) + 6
                    Else
                        CP = Int(Rnd(1) * 9) + 8
                    End If
                End If

            Case 12, 15
                If rushYds < 3 Then
                    CP = Int(Rnd(1) * 5) + 3
                Else
                    If rushYds = 3 Or rushYds = 4 Then
                        CP = Int(Rnd(1) * 10) + 10
                    Else
                        CP = Int(Rnd(1) * 36) + 20
                    End If
                End If
        End Select

    End If

    PC = CP

    If CP > ydsGained Then
        CP = ydsGained
        IT = 1
    End If

    ydsToScore = 100 - ydLine - CP

    Call YARDLINE
    Call MARKER

    If ydsToScore <= 0 Then

        pbpString$ = targetWRName$ + " MAKES THE CATCH IN THE END ZONE!!!"
        Call PBP

    Else

        Call Rolld100(RN0)

        Select Case playCall
            Case 9, 10
                Select Case RN0
                    Case 1 To 33
                        pbpString$ = targetWRName$ + " HAS IT OVER THE MIDDLE"
                    Case 34 To 67
                        pbpString$ = targetWRName$ + " CATCHES IT IN THE FLAT"
                    Case 68 To 199
                        pbpString$ = "HE DUMPS IT TO " + targetWRName$
                End Select

            Case 13
                Select Case RN0
                    Case 1 To 33
                        pbpString$ = "HE SCREENS IT TO " + targetWRName$
                    Case 34 To 67
                        pbpString$ = "HE DUMPS THE SCREEN TO " + targetWRName$
                    Case 68 To 199
                        pbpString$ = "HE HAS " + targetWRName$ + " ON THE SCREEN PASS"
                End Select

            Case 11, 12, 14, 15
                Select Case RN0
                    Case 1 To 33
                        pbpString$ = targetWRName$ + " TAKES IT OVER THE MIDDLE"
                    Case 34 To 67
                        pbpString$ = "HE HAS " + targetWRName$
                    Case 68 To 199
                        pbpString$ = "HE COMPLETES IT TO " + targetWRName$
                End Select

            Case 16
                If RN0 <= 50 Then
                    pbpString$ = "HE HAS " + targetWRName$ + " NEAR THE SIDELINE"
                Else
                    pbpString$ = "HE FINDS " + targetWRName$
                End If

            Case 17, 18
                Select Case RN0
                    Case 1 To 50
                        pbpString$ = "HE HITS " + targetWRName$

                    Case 51 To 90
                        pbpString$ = "HE FIRES IT TO " + targetWRName$

                    Case 91 To 100
                        pbpString$ = targetWRName$ + " MAKES A GREAT CATCH"
                End Select

        End Select

        If FF% <> 0 Then pbpString$ = targetWRName$ + " MAKES A GREAT CATCH"

        pbpString$ = pbpString$ + " AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"

        If IT = 1 Then
            IT = 0
            Call PBP
            Call PBP
        Else
            'Adjust team position
            Call PBP
            Call PBP

            startYds = 100 - ydLine - CP
            endYds = 100 - ydLine - ydsGained

            Call MOVEBALL

        End If

    End If


End Sub


'----------------------------------------
'          Touchback Subroutine
'----------------------------------------
'This subroutine handles a touchback BY
'update the variables for down, YARDLINE
'Also calls Play-by-Play and sets the
'ball position marker.
Sub Touchback

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

                Case Is >= 8
                    '2018-PRESENT
                    ydLine = 25
                    ydsToScore = 75

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

    '1st and 10
    currDown = 1
    ydsToFirst = 10

    pbpString$ = "TOUCHBACK"

    Call PBP
    Call MARKER
    Call YARDLINE

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
    timeElapsed! = Int(Rnd(1) * secsDelta) + minSeconds
    gameClock! = gameClock! - timeElapsed!
    timePoss(P1) = timePoss(P1) + timeElapsed!

End Sub


'----------------------------------------
'          FumblePBP Subroutine
'----------------------------------------
'This subroutine creates the play-by-play
'text for a fumnle
Sub FumblePBP ()

    RN0 = Int(Rnd(1) * 6) + 1

    Select Case RN0

        Case 1: pbpString$ = "*** FUMBLE ON THE PLAY!! ***"
        Case 2: pbpString$ = "*** LOOSE BALL!! ***"
        Case 3: pbpString$ = "*** BALL IS ON THE GROUND!! ***"
        Case 4: pbpString$ = "*** BALL IS KNOCKED LOOSE!! ***"
        Case 5: pbpString$ = "*** BALL IS STRIPPED!! ***"
        Case 6: pbpString$ = "*** FREE BALL !! ***"

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

    If playType = 1 Then K1(nbrScores, 4) = 20
    If playType = 2 Then K1(nbrScores, 4) = 30

    pbpString$ = "TRY FOR TWO IS "
    gameStatsTeam(P1, 35) = gameStatsTeam(P1, 35) + 1

    If ydsGained < 2 And (ruleOptType = 1 And ruleOptPro >= 4) Then

        pbpString$ = pbpString$ + "NO GOOD"
        Call PBP

    Else

        If ydsGained < 3 And ruleOptType <> 1 And ruleOptPro < 4 Then
            'Any league but NFL, prior to 1994
            '3 yards needed for conversion
            pbpString$ = pbpString$ + "NO GOOD"
            Call PBP
        Else
            'ydsGained = 2-3, ruleOptType = 1 (NFL) Or ruleOptPro >= 4 (Pro Years after 1993)
            pbpString$ = pbpString$ + "GOOD"
            Call PBP

            score(P1, 0) = score(P1, 0) + 2
            score(P1, quarter) = score(P1, quarter) + 2
            K1(nbrScores, 4) = K1(nbrScores, 4) + 1
            gameStatsTeam(P1, 36) = gameStatsTeam(P1, 36) + 1

            Call DisplayField

        End If

    End If

    'No longer dealing with endzone situation
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
            'Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
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
    If _FileExists("EVENTS.BOX") Then

        Open "EVENTS.BOX" For Input As #1

        For X = 1 To 13
            Input #1, eventSettings(X)
        Next

        Close #1

        checkSpecial% = 0

        If quarter > eventSettings(1) Then
            checkSpecial% = 1
        Else

            If score(1, 0) > eventSettings(2) Or score(0, 0) > eventSettings(2) Or score(1, 0) < eventSettings(3) Or score(0, 0) < eventSettings(3) Then
                checkSpecial% = 1
            Else

                For I = 0 To 1

                    For I1 = 0 To 9

                        '--Combine into multiple Or
                        If gameStatsPlayer(I, I1, 1) > eventSettings(4) Then
                            checkSpecial% = 1
                        Else
                            If gameStatsPlayer(I, I1, 2) > eventSettings(5) Then
                                checkSpecial% = 1
                            Else
                                If gameStatsPlayer(I, I1, 4) > eventSettings(6) Then
                                    checkSpecial% = 1
                                Else
                                    If gameStatsPlayer(I, I1, 5) > eventSettings(7) Then
                                        checkSpecial% = 1
                                    End If
                                End If
                            End If
                        End If

                        If checkSpecial% = 1 Then Exit For

                    Next

                Next

                If checkSpecial% <> 1 Then

                    For I = 0 To 1

                        For I1 = 0 To 5

                            If gameStatsPlayer(I, I1, 7) > eventSettings(6) Then
                                checkSpecial% = 1
                            Else
                                If gameStatsPlayer(I, I1, 8) > eventSettings(7) Then
                                    checkSpecial% = 1
                                End If
                            End If

                            If checkSpecial% = 1 Then Exit For

                        Next

                    Next

                End If

                If checkSpecial% <> 1 Then

                    For I = 0 To 1
                        For I1 = 0 To 3
                            If gameStatsPlayer(I, I1, 12) > eventSettings(8) Then
                                checkSpecial% = 1
                            Else
                                If gameStatsPlayer(I, I1, 13) > eventSettings(9) Then
                                    checkSpecial% = 1
                                End If
                            End If
                        Next
                    Next

                End If

                If checkSpecial% <> 1 Then

                    For I = 0 To 1
                        If gameStatsPlayer(I, 3, 17) > eventSettings(10) Then
                            checkSpecial% = 1
                        Else
                            If gameStatsPlayer(I, 7, 17) > eventSettings(10) Then
                                checkSpecial% = 1
                            End If
                        End If
                    Next

                End If

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

        Color 14: Print "(0) ";
        Color 15: Print "STATS TO SCREEN"
        Print

        Color 14: Print "(1) ";
        Color 15: Print "STATS TO PRINTER"
        Print

        Color 14: Print "(2) ";
        Color 15: Print "STATS TO TEXT FILE"

        If autoPlay = 1 And playerMode = 2 Then
            X = 2
        Else

            Do
                J$ = GetKeyPress$
                X = Val(J$)
            Loop Until X >= 0 And X <= 2

        End If

        Call BOXOPTIONS(X)

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
                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or (autoPlay = 1 And playerMode = 2)

    Call TICKER

    'If we had special events
    If specialStat = 1 Then

        boxName$ = "BOXSCORE" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".BSS"
        'boxName$ = "BOX" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".SPC"

        'Append special events
        'Call BOX2SPC(boxName$)

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
                    Cls
                    Print "ENTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";
                    Input boxName$
                    boxName$ = boxName$ + ".BSS"
                    'boxName$ = boxName$ + ".SPC"
                Else
                    'ScheduleName_GameXXX.BS
                    boxName$ = Mid$(scheduleFile$, 1, Len(scheduleFile$) - 4) + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
                    'boxName$ = "BOX" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2))))
                End If

            End If

            'Includes a special option to send to printer when done
            Call BOX2FILE(outputChoice, boxName$, 0)

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
        Open (diskPaths$(1) + boxName$) For Append As #1
    Else
        Open (diskPaths$(1) + boxName$) For Output As #1
    End If

    If gameLoc = 2 Then
        Print #1, "SITE: NEUTRAL"
    Else
        Print #1, "SITE: "; gameStadium$
    End If

    Print #1, Using "ATTENDANCE: ###,###"; actualAttendance&
    Print #1,

    Print #1, Tab(20); "Q1 Q2 Q3 Q4 ";
    If quarter > 4 Then
        For I1 = 5 To quarter
            Print #1, "OT ";
        Next
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
            If scoreQuarters(I) > 4 Then Print #1, "OF OT "; scoreQuarters(I) - 4; " -";

            '# of yards for score
            If K1(I, 0) <> 7 Then Print #1, Int(K1(I, 5) + .5);

            tryPAT = 1

            Select Case K1(I, 0)

                Case 1
                    Print #1, "RUN BY "; rbacks$(K1(I, 1), K1(I, 2))

                Case 2
                    Print #1, "PASS BY "; qbacks$(K1(I, 1), K1(I, 2));: Print #1, " TO ";
                    If K1(I, 3) > 9 Then Print #1, wdRec$(K1(I, 1), K1(I, 3) - 10)
                    If K1(I, 3) <= 9 Then Print #1, rbacks$(K1(I, 1), K1(I, 3))

                Case 3
                    Print #1, "K.O. RETURN BY "; kickRet$(K1(I, 1), K1(I, 2))

                Case 4
                    Print #1, "PUNT RETURN BY "; pret$(K1(I, 1), K1(I, 2))

                Case 5
                    Print #1, "INTERCEPTION RETURN BY "; defInts$(K1(I, 1), K1(I, 2))

                Case 6
                    Print #1, "BLOCKED KICK RECOVERED IN END ZONE"

                Case 7
                    Print #1, "SAFETY"
                    tryPAT = 0

                Case 8
                    Print #1, "FG BY "; kicker$(K1(I, 1), K1(I, 2))
                    tryPAT = 0

                Case 9
                    Print #1, "RUN BY "; qbacks$(K1(I, 1), K1(I, 2))

                Case 10
                    Print #1, "FUMBLE RETURN "

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
        Print #1, gameStatsTeam(0, 1); "-"; Int(gameStatsTeam(0, 2) + .5); Tab(29); "RUSHING YDS"; Tab(48); gameStatsTeam(1, 1); "-"; Int(gameStatsTeam(1, 2) + .5)
    Else
        Print #1, gameStatsTeam(0, 1) - sackStatsTeam(0, 0); "-"; Int(gameStatsTeam(0, 2) + sackStatsTeam(0, 1) + .5); Tab(29); "RUSHING YDS"; Tab(48); gameStatsTeam(1, 1) - sackStatsTeam(1, 0); "-"; Int(gameStatsTeam(1, 2) + sackStatsTeam(1, 1) + .5)
    End If

    Print #1, Tab(16); Int(gameStatsTeam(0, 6) + .5); Tab(29); "PASSING YDS"; Tab(51); Int(gameStatsTeam(1, 6) + .5)

    Print #1, Tab(16); Int(gameStatsTeam(0, 6) + .5) + Int(gameStatsTeam(0, 2) + .5); Tab(30); "TOTAL YDS"; Tab(51); Int(gameStatsTeam(1, 6) + .5) + Int(gameStatsTeam(1, 2) + .5)

    Print #1, Tab(13); gameStatsTeam(0, 4); "-"; gameStatsTeam(0, 3); "-"; gameStatsTeam(0, 5); Tab(31); "PASSES"; Tab(47); gameStatsTeam(1, 4); "-"; gameStatsTeam(1, 3); "-"; gameStatsTeam(1, 5)

    If gameStatsTeam(0, 7) <> 0 And gameStatsTeam(1, 7) <> 0 Then
        Print #1, Tab(15); gameStatsTeam(0, 7); "-"; Int((gameStatsTeam(0, 8) / gameStatsTeam(0, 7)) * 10 + .5) / 10; Tab(32); "PUNTS"; Tab(49); gameStatsTeam(1, 7); "-"; Int((gameStatsTeam(1, 8) / gameStatsTeam(1, 7)) * 10 + .5) / 10
    End If

    Print #1, Tab(15); gameStatsTeam(0, 9); "-"; gameStatsTeam(0, 10); Tab(31); "FUM/LOST"; Tab(50); gameStatsTeam(1, 9); "-"; gameStatsTeam(1, 10)

    Print #1, Tab(15); gameStatsTeam(0, 11); "-"; Int(gameStatsTeam(0, 12) + .5); Tab(31); "PENALTIES"; Tab(50); gameStatsTeam(1, 11); "-"; Int(gameStatsTeam(1, 12) + .5)

    Print #1, Tab(15); sackStatsTeam(0, 0); "-"; Int(sackStatsTeam(0, 1) + .5); Tab(32); "SACKED"; Tab(50); sackStatsTeam(1, 0); "-"; Int(sackStatsTeam(1, 1) + .5)

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

                Print #1, Tab(18); Using "###  #### ###.#  "; gameStatsPlayer(I, I1, 0), Int(gameStatsPlayer(I, I1, 1) + .5), Int(gameStatsPlayer(I, I1, 1) + .5) / gameStatsPlayer(I, I1, 0);

                If I1 <= 9 Then
                    'Long   TD
                    Print #1, Using "###   ##"; runBacks(I, I1), gameStatsPlayer(I, I1, 2)
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
                Print #1, Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 6), Int(gameStatsPlayer(I, I1, 7) + .5), Int(gameStatsPlayer(I, I1, 7) + .5) / gameStatsPlayer(I, I1, 6), WR(I, I1 + 10), gameStatsPlayer(I, I1, 8)
            End If

        Next I1

        For I1 = 0 To 9

            If gameStatsPlayer(I, I1, 3) <> 0 Then
                Print #1, rbacks$(I, I1); Tab(19);
                Print #1, Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 3), Int(gameStatsPlayer(I, I1, 4) + .5), Int(gameStatsPlayer(I, I1, 4) + .5) / gameStatsPlayer(I, I1, 3), WR(I, I1), gameStatsPlayer(I, I1, 5)
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
                Print #1, Using "###  ### ###.#  ### ##.# "; gameStatsPlayer(I, I1, 9), gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 10) / gameStatsPlayer(I, I1, 9) * 100, Int(gameStatsPlayer(I, I1, 12) + .5), Int(gameStatsPlayer(I, I1, 12) + .5) / gameStatsPlayer(I, I1, 9);

                If gameStatsPlayer(I, I1, 10) <> 0 Then
                    'Y/C, TD
                    Print #1, Using "##.#  ##"; Int(gameStatsPlayer(I, I1, 12) + .5) / gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 13);
                Else
                    'TDs
                    Print #1, Using "      ##"; gameStatsPlayer(I, I1, 13);
                End If

                'TD%, Ints, Int%, Longest
                Print #1, Using "###.# ## ###.# ###"; gameStatsPlayer(I, I1, 13) / gameStatsPlayer(I, I1, 9) * 100, gameStatsPlayer(I, I1, 11), gameStatsPlayer(I, I1, 11) / gameStatsPlayer(I, I1, 9) * 100, quarterbacks(I, I1)
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
                Print #1, Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), Int(gameStatsPlayer(I, I1, 15) + .5), Int(gameStatsPlayer(I, I1, 15) + .5) / gameStatsPlayer(I, I1, 14), kickReturners(I, I1), gameStatsPlayer(I, I1, 16)
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
                Print #1, Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), Int(gameStatsPlayer(I, I1, 15) + .5), Int(gameStatsPlayer(I, I1, 15) + .5) / gameStatsPlayer(I, I1, 14), puntReturners(I, I1 - 3), gameStatsPlayer(I, I1, 16)
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
                Print #1, Tab(33); Using "### ### ###.# ##  #"; gameStatsPlayer(I, 3, 17), gameStatsPlayer(I, 4, 17), gameStatsPlayer(I, 3, 17) / gameStatsPlayer(I, 4, 17) * 100, gameLongFG(I, 0), FM(I, 0, 0);

                'FG's 0 - 19
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 0); FM(I, 0, 1);
                'FG's 20 - 29
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 1); FM(I, 0, 2);
                'FG's 30 - 39
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 2); FM(I, 0, 3);
                'FG's 40 - 49
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 3); FM(I, 0, 4);
                'FG's 50+
                Print #1, "- ";: Print #1, Using "#"; FA(I, 0, 4)
            End If

        End If

        If gameStatsPlayer(I, 8, 17) <> 0 Or gameStatsPlayer(I, 10, 17) <> 0 Then

            Print #1, kicker$(I, 1); Tab(19);
            If gameStatsPlayer(I, 10, 17) <> 0 Then Print #1, Using "### ### ###.# "; gameStatsPlayer(I, 9, 17), gameStatsPlayer(I, 10, 17), gameStatsPlayer(I, 9, 17) / gameStatsPlayer(I, 10, 17) * 100;

            If gameStatsPlayer(I, 8, 17) = 0 Then
                Print #1,
            Else
                Print #1, Tab(33); Using "### ### ###.# ##  #"; gameStatsPlayer(I, 7, 17), gameStatsPlayer(I, 8, 17), gameStatsPlayer(I, 7, 17) / gameStatsPlayer(I, 8, 17) * 100, gameLongFG(I, 1), FM(I, 1, 0);

                'FG's 0 - 19
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 0); FM(I, 1, 1);
                'FG's 20 - 29
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 1); FM(I, 1, 2);
                'FG's 30 - 39
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 2); FM(I, 1, 3);
                'FG's 40 - 49
                Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 3); FM(I, 1, 4);
                'FG's 50+
                Print #1, "- ";: Print #1, Using "#"; FA(I, 1, 4)
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
                Print #1, Using "##  ####  ##.#  ###  ##"; gameInts(I, I1, 0), Int(gameInts(I, I1, 1) + .5), Int(gameInts(I, I1, 1) + .5) / gameInts(I, I1, 0), IR(I, I1), gameInts(I, I1, 2)
            End If
        Next I1

        Print #1,

    Next I

    Print #1, "SACKS"

    For I = 0 To 1:
        Print #1, gameTeams$(I); Tab(19);
        Print #1, "NO.   YDS LOST"
        For I1 = 0 To 14
            If sackStatsPlayer(I, I1, 0) <> 0 Then
                Print #1, defSacks$(I, I1); Tab(19);
                Print #1, Using " ##       ##"; sackStatsPlayer(I, I1, 0), Int(sackStatsPlayer(I, I1, 1) + .5)
            End If
        Next I1
        Locate , 1: Print #1,
    Next I

    If printChoice = 1 Then Print #1, Chr$(12)

    Close #1

    If printChoice = 1 Then

        Open diskPaths$(1) + boxName$ For Input As #2

        Do
            Line Input #2, X$
            LPrint X$
        Loop While Left$(X$, 1) <> Chr$(12)

        Close #2

        Kill "TEMPBOX"

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

    If gameLoc = 2 Then
        Print "SITE: ";
        Color 15
        Print "NEUTRAL"
    Else
        Print "SITE: ";
        Color 15
        Print gameStadium$
    End If

    Color 14
    Print "ATTENDANCE: ";
    Color 15
    Print Using "###,###"; actualAttendance&
    Print

    Call ScoreSummary

    If Not (autoPlay = 1 And playerMode = 2) Then
        Locate 24, 27
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

        If quarter = 6 Then
            Print "OT ";
        Else
            If quarter > 6 Then
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
            Print gameTeams$(K1(I, 1)); "-";

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

            If K1(I, 0) <> 7 Then Print Int(K1(I, 5) + .5);

            tryPAT = 1

            Select Case K1(I, 0)

                Case 1
                    Print "RUN BY "; rbacks$(K1(I, 1), K1(I, 2))

                Case 2
                    Print "PASS BY "; qbacks$(K1(I, 1), K1(I, 2)); " TO ";: If K1(I, 3) > 9 Then Print wdRec$(K1(I, 1), K1(I, 3) - 10)
                    If K1(I, 3) <= 9 Then Print rbacks$(K1(I, 1), K1(I, 3))

                Case 3
                    Print "K.O. RETURN BY "; kickRet$(K1(I, 1), K1(I, 2))

                Case 4
                    Print "PUNT RETURN BY "; pret$(K1(I, 1), K1(I, 2))

                Case 5
                    Print "INTERCEPTION RETURN BY "; defInts$(K1(I, 1), K1(I, 2))

                Case 6
                    Print "BLOCKED KICK RECOVERED IN END ZONE"

                Case 7
                    Print "SAFETY"
                    tryPAT = 0

                Case 8
                    Print "FG BY "; kicker$(K1(I, 1), K1(I, 2))
                    tryPAT = 0

                Case 9
                    Print "RUN BY "; qbacks$(K1(I, 1), K1(I, 2))

                Case 10
                    Print "FUMBLE RETURN "

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
                Locate 24, 20
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

    Color , 1
    Cls

    Call OPTIONBOX(15)
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

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
        Locate , 18: Print gameStatsTeam(0, 1); "-"; Int(gameStatsTeam(0, 2) + .5);
        Locate , 34: Print "RUSHING YDS";
        Locate , 53: Print gameStatsTeam(1, 1); "-"; Int(gameStatsTeam(1, 2) + .5)
    Else
        Locate , 18: Print gameStatsTeam(0, 1) - sackStatsTeam(0, 0); "-"; Int(gameStatsTeam(0, 2) + sackStatsTeam(0, 1) + .5);
        Locate , 34: Print "RUSHING YDS";
        Locate , 53: Print gameStatsTeam(1, 1) - sackStatsTeam(1, 0); "-"; Int(gameStatsTeam(1, 2) + sackStatsTeam(1, 1) + .5)
    End If

    Locate , 20: Print Int(gameStatsTeam(0, 6) + .5);
    Locate , 34: Print "PASSING YDS";
    Locate , 55: Print Int(gameStatsTeam(1, 6) + .5)

    Locate , 35: Print "TOTAL YDS";
    Locate , 20: Print Int(gameStatsTeam(0, 6) + .5) + Int(gameStatsTeam(0, 2) + .5);
    Locate , 55: Print Int(gameStatsTeam(1, 6) + .5) + Int(gameStatsTeam(1, 2) + .5)

    Locate , 37: Print "PASSES";
    Locate , 16: Print gameStatsTeam(0, 4); "-"; gameStatsTeam(0, 3); "-"; gameStatsTeam(0, 5);
    Locate , 51: Print gameStatsTeam(1, 4); "-"; gameStatsTeam(1, 3); "-"; gameStatsTeam(1, 5)

    If gameStatsTeam(0, 7) <> 0 And gameStatsTeam(1, 7) <> 0 Then
        Locate , 38: Print "PUNTS";
        Locate , 15: Print gameStatsTeam(0, 7); "-"; Int((gameStatsTeam(0, 8) / gameStatsTeam(0, 7)) * 10 + .5) / 10;
        Locate , 50: Print gameStatsTeam(1, 7); "-"; Int((gameStatsTeam(1, 8) / gameStatsTeam(1, 7)) * 10 + .5) / 10
    End If

    Locate , 36: Print "FUM/LOST";
    Locate , 18: Print gameStatsTeam(0, 9); "-"; gameStatsTeam(0, 10);
    Locate , 53: Print gameStatsTeam(1, 9); "-"; gameStatsTeam(1, 10)

    Locate , 35: Print "PENALTIES";
    Locate , 18: Print gameStatsTeam(0, 11); "-"; Int(gameStatsTeam(0, 12) + .5);
    Locate , 53: Print gameStatsTeam(1, 11); "-"; Int(gameStatsTeam(1, 12) + .5)

    Locate , 37: Print "SACKED";
    Locate , 18: Print sackStatsTeam(0, 0); "-"; Int(sackStatsTeam(0, 1) + .5);
    Locate , 53: Print sackStatsTeam(1, 0); "-"; Int(sackStatsTeam(1, 1) + .5)

    Locate , 18: Print Using "##"; Int(timePoss(0) / 60);
    Print ":";
    Print Using "##"; timePoss(0) - Int(timePoss(0) / 60) * 60;
    Locate , 31: Print "TIME OF POSSESSION";
    Locate , 53: Print Using "##"; Int(timePoss(1) / 60);
    Print ":";
    Print Using "##"; timePoss(1) - Int(timePoss(1) / 60) * 60

    Locate , 32: Print "THIRD DOWN EFF";
    Locate , 16: Print thirdDownAtt(0) - thirdDownFail(0); "-"; thirdDownAtt(0);
    Locate , 51: Print thirdDownAtt(1) - thirdDownFail(1); "-"; thirdDownAtt(1)

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27
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

                Locate , 35: Print Using "###  #### ###.#  "; gameStatsPlayer(I, I1, 0), Int(gameStatsPlayer(I, I1, 1) + .5), Int(gameStatsPlayer(I, I1, 1) + .5) / gameStatsPlayer(I, I1, 0);

                If I1 <= 9 Then
                    Print Using "###   ##"; runBacks(I, I1), gameStatsPlayer(I, I1, 2)
                Else
                    Print Using "      ##"; gameStatsPlayer(I, I1, 2)
                End If

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27
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
                Locate , 36: Print Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 6), Int(gameStatsPlayer(I, I1, 7) + .5), Int(gameStatsPlayer(I, I1, 7) + .5) / gameStatsPlayer(I, I1, 6), WR(I, I1 + 10), gameStatsPlayer(I, I1, 8)
                nbrPlayers = nbrPlayers + 1
            End If

            If (nbrPlayers > 0 And nbrPlayers Mod 16 = 0) Then
                'Create a 2nd page if there are enough players
                'to spam out past the page
                Call GetKeyWhenNoDelay

                Call FRAMESTATS(22)
                Locate 24, 27
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
                Locate , 36: Print Using "##    ###   ##.#  ###    ##"; gameStatsPlayer(I, I1, 3), Int(gameStatsPlayer(I, I1, 4) + .5), Int(gameStatsPlayer(I, I1, 4) + .5) / gameStatsPlayer(I, I1, 3), WR(I, I1), gameStatsPlayer(I, I1, 5)
                nbrPlayers = nbrPlayers + 1
            End If

            If (nbrPlayers > 0 And nbrPlayers Mod 16 = 0) Then
                'Create a 2nd page if there are enough players
                'to spam out past the page
                Call GetKeyWhenNoDelay

                Call FRAMESTATS(22)
                Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

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
    Locate 24, 27
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
                Locate , 22: Print Using "###  ### ###.#  ### ##.# "; gameStatsPlayer(I, I1, 9), gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 10) / gameStatsPlayer(I, I1, 9) * 100, Int(gameStatsPlayer(I, I1, 12) + .5), Int(gameStatsPlayer(I, I1, 12) + .5) / gameStatsPlayer(I, I1, 9);

                If gameStatsPlayer(I, I1, 10) <> 0 Then
                    'Y/C, TD
                    Print Using "##.#  ##"; Int(gameStatsPlayer(I, I1, 12) + .5) / gameStatsPlayer(I, I1, 10), gameStatsPlayer(I, I1, 13);
                Else
                    Print Using "      ##"; gameStatsPlayer(I, I1, 13);
                End If

                '             TD%, I, I%,   LG
                Print Using "###.# ## ###.# ###"; gameStatsPlayer(I, I1, 13) / gameStatsPlayer(I, I1, 9) * 100, gameStatsPlayer(I, I1, 11), gameStatsPlayer(I, I1, 11) / gameStatsPlayer(I, I1, 9) * 100, quarterbacks(I, I1)

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27
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
                Print Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), Int(gameStatsPlayer(I, I1, 15) + .5), Int(gameStatsPlayer(I, I1, 15) + .5) / gameStatsPlayer(I, I1, 14), kickReturners(I, I1), gameStatsPlayer(I, I1, 16)

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
                Print Using "##  ####  ##.#  ###  ##"; gameStatsPlayer(I, I1, 14), Int(gameStatsPlayer(I, I1, 15) + .5), Int(gameStatsPlayer(I, I1, 15) + .5) / gameStatsPlayer(I, I1, 14), puntReturners(I, I1 - 3), gameStatsPlayer(I, I1, 16)
            End If
        Next I1

        Print
    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27
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
                Print Using "### ### ###.# ##  #"; gameStatsPlayer(I, 3, 17), gameStatsPlayer(I, 4, 17), gameStatsPlayer(I, 3, 17) / gameStatsPlayer(I, 4, 17) * 100, gameLongFG(I, 0), FM(I, 0, 0);
                Print "- ";: Print Using "#  #"; FA(I, 0, 0); FM(I, 0, 1);
                Print "- ";: Print Using "#  #"; FA(I, 0, 1); FM(I, 0, 2);
                Print "- ";: Print Using "#  #"; FA(I, 0, 2); FM(I, 0, 3);
                Print "- ";: Print Using "#  #"; FA(I, 0, 3); FM(I, 0, 4);
                Print "- ";: Print Using "#"; FA(I, 0, 4)
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
                Print Using "### ### ###.# ##  #"; gameStatsPlayer(I, 7, 17), gameStatsPlayer(I, 8, 17), gameStatsPlayer(I, 7, 17) / gameStatsPlayer(I, 8, 17) * 100, gameLongFG(I, 1), FM(I, 1, 0);
                Print "- ";: Print Using "#  #"; FA(I, 1, 0); FM(I, 1, 1);
                Print "- ";: Print Using "#  #"; FA(I, 1, 1); FM(I, 1, 2);
                Print "- ";: Print Using "#  #"; FA(I, 1, 2); FM(I, 1, 3);
                Print "- ";: Print Using "#  #"; FA(I, 1, 3); FM(I, 1, 4);
                Print "- ";: Print Using "#"; FA(I, 1, 4)

            End If

        End If

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27
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
                Locate , 36: Print Using "##  ####  ##.#  ###  ##"; gameInts(I, I1, 0), Int(gameInts(I, I1, 1) + .5), Int(gameInts(I, I1, 1) + .5) / gameInts(I, I1, 0), IR(I, I1), gameInts(I, I1, 2)
            End If
        Next I1

        Locate , 1: Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27
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
            If sackStatsPlayer(I, I1, 0) <> 0 Then
                Locate , 25: Print defSacks$(I, I1);
                Locate , 41: Print Using " ##       ##"; sackStatsPlayer(I, I1, 0), Int(sackStatsPlayer(I, I1, 1) + .5)
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
        If autoPlay = 1 Then

            Call TickerSchedule

            If schedGame(1) < schedGame(0) + schedGame(2) Then
                Kill "GAME82"
                endAllGames = 1
            Else

                Open "GAME82" For Output As #1

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

    Shared tradeLPR$(), oppName$()
    Shared gameSite$()
    Shared locIndicator$()

    Shared tradeLFG$(), tradeLInt$(), tradeLKR$(), tradeLPass$()

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
            Input #1, gameLRush!(teamIdx, I)
            Input #1, gameLRush$(teamIdx, I, 0)
            Input #1, gameLRush$(teamIdx, I, 1)
        Next

        For I = 0 To 15:
            Input #1, gameLRec!(teamIdx, I)
            Input #1, gameLRec$(teamIdx, I, 0)
            Input #1, gameLRec$(teamIdx, I, 1)
        Next

        For I = 0 To 3:
            Input #1, gameLPass!(teamIdx, I)
            Input #1, tradeLPass$(teamIdx, I, 0)
            Input #1, tradeLPass$(teamIdx, I, 1)
        Next

        For I = 0 To 2:
            Input #1, gameLKR!(teamIdx, I)
            Input #1, tradeLKR$(teamIdx, I, 0)
            Input #1, tradeLKR$(teamIdx, I, 1)
        Next

        For I = 0 To 2:
            Input #1, gameLPR!(teamIdx, I)
            Input #1, tradeLPR$(teamIdx, I, 0)
            Input #1, tradeLPR$(teamIdx, I, 1)
        Next

        For I = 0 To 1:
            Input #1, gameLFG!(teamIdx, I)
            Input #1, tradeLFG$(teamIdx, I)
            For I1 = 0 To 4:
                Input #1, gameFGM!(teamIdx, I, I1)
                Input #1, gameFGA!(teamIdx, I, I1)
            Next
        Next

        For I = 0 To 9:
            Input #1, gameLInt!(teamIdx, I)
            Input #1, tradeLInt$(teamIdx, I, 0)
            Input #1, tradeLInt$(teamIdx, I, 1)
        Next

        Close #1

    End If

    For I = 0 To 9
        If runBacks(teamIdx, I) > gameLRush!(teamIdx, I) Then
            gameLRush!(teamIdx, I) = runBacks(teamIdx, I)
            gameLRush$(teamIdx, I, 0) = gameRB$(teamIdx, I)
            gameLRush$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If

        If IR(teamIdx, I) > gameLInt!(teamIdx, I) Then
            gameLInt!(teamIdx, I) = IR(teamIdx, I)
            tradeLInt$(teamIdx, I, 0) = gameIR$(teamIdx, I)
            tradeLInt$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 15
        If WR(teamIdx, I) > gameLRec!(teamIdx, I) Then
            gameLRec!(teamIdx, I) = WR(teamIdx, I)
            gameLRec$(teamIdx, I, 0) = gameWR$(teamIdx, I)
            gameLRec$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 3
        If quarterbacks(teamIdx, I) > gameLPass!(teamIdx, I) Then
            gameLPass!(teamIdx, I) = quarterbacks(teamIdx, I)
            tradeLPass$(teamIdx, I, 0) = gameQB$(teamIdx, I)
            tradeLPass$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 2
        If kickReturners(teamIdx, I) > gameLKR!(teamIdx, I) Then
            gameLKR!(teamIdx, I) = kickReturners(teamIdx, I)
            tradeLKR$(teamIdx, I, 0) = gameKR$(teamIdx, I)
            tradeLKR$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If

        If puntReturners(teamIdx, I) > gameLPR!(teamIdx, I) Then
            gameLPR!(teamIdx, I) = puntReturners(teamIdx, I)
            tradeLPR$(teamIdx, I, 0) = gamePR$(teamIdx, I)
            tradeLPR$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    If gameStatsTeam(teamIdx, 26) > gameTeamStat!(28) Then
        gameTeamStat!(28) = gameStatsTeam(teamIdx, 26)
    End If

    For I = 0 To 1

        If gameLongFG(teamIdx, I) > gameLFG!(teamIdx, I) Then
            gameLFG!(teamIdx, I) = gameLongFG(teamIdx, I)
            tradeLFG$(teamIdx, 1) = gameTeams$(1 - teamIdx)
        End If

        For I1 = 0 To 4
            gameFGM!(teamIdx, I, I1) = gameFGM!(teamIdx, I, I1) + FM(teamIdx, I, I1)
            gameFGA!(teamIdx, I, I1) = gameFGA!(teamIdx, I, I1) + FA(teamIdx, I, I1)
        Next

    Next

    For J = 0 To 13
        test3 = gameStatsPlayer(teamIdx, J, 17)
        For K = 0 To 17:
            gamePlayerStat!(J, K) = gamePlayerStat!(J, K) + Int(gameStatsPlayer(teamIdx, J, K) + .5)
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
    Next

    For L = 0 To 12:
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
            totalSacks(teamIdx, J, J1) = totalSacks(teamIdx, J, J1) + sackStatsPlayer(teamIdx, J, J1)
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
        Print #2, gameLRush!(teamIdx, I)
        Print #2, gameLRush$(teamIdx, I, 0)
        Print #2, gameLRush$(teamIdx, I, 1)
    Next

    For I = 0 To 15:
        Print #2, gameLRec!(teamIdx, I)
        Print #2, gameLRec$(teamIdx, I, 0)
        Print #2, gameLRec$(teamIdx, I, 1)
    Next

    For I = 0 To 3:
        Print #2, gameLPass!(teamIdx, I)
        Print #2, tradeLPass$(teamIdx, I, 0)
        Print #2, tradeLPass$(teamIdx, I, 1)
    Next

    For I = 0 To 2:
        Print #2, gameLKR!(teamIdx, I)
        Print #2, tradeLKR$(teamIdx, I, 0)
        Print #2, tradeLKR$(teamIdx, I, 1)
    Next

    For I = 0 To 2:
        Print #2, gameLPR!(teamIdx, I)
        Print #2, tradeLPR$(teamIdx, I, 0)
        Print #2, tradeLPR$(teamIdx, I, 1)
    Next

    For I = 0 To 1:
        Print #2, gameLFG!(teamIdx, I)
        Print #2, tradeLFG$(teamIdx, I)
        For I1 = 0 To 4:
            Print #2, gameFGM!(teamIdx, I, I1)
            Print #2, gameFGA!(teamIdx, I, I1)
        Next
    Next

    For I = 0 To 9:
        Print #2, gameLInt!(teamIdx, I)
        Print #2, tradeLInt$(teamIdx, I, 0)
        Print #2, tradeLInt$(teamIdx, I, 1)
    Next

    Close #2

    Print "<<<<<"
    Print
    Print

    numberGames = 0

    'Read stats in from file
    If _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Open diskPaths$(1) + statFile$ + "S" For Input As #3

        Input #3, numberGames

        For I = 1 To MAX_SCHED_STATS:
            Input #3, teamScore(I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, oppName$(I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, oppScore(I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, locIndicator$(I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, powerRating(0, I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, powerRating(1, I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, gameAttendance&(I)
        Next

        For I = 1 To MAX_SCHED_STATS:
            Input #3, gameSite$(I)
        Next

        Close #3

    End If

    If numberGames < MAX_SCHED_STATS Then numberGames = numberGames + 1
    If gameLoc = teamIdx Then locIndicator$(numberGames) = "H"
    If gameLoc <> teamIdx Then locIndicator$(numberGames) = "A"
    If gameLoc = 2 Then locIndicator$(numberGames) = "X"
    If gameLoc = teamIdx And periodNbr > 4 Then locIndicator$(numberGames) = "H-" + Str$(periodNbr - 4) + " OT"
    If gameLoc <> teamIdx And periodNbr > 4 Then locIndicator$(numberGames) = "A-" + Str$(periodNbr - 4) + " OT"
    If gameLoc = 2 And periodNbr > 4 Then locIndicator$(numberGames) = "X-" + Str$(periodNbr - 4) + " OT"

    teamScore(numberGames) = score(teamIdx, 0)
    oppName$(numberGames) = gameTeams$(1 - teamIdx)
    oppScore(numberGames) = score(1 - teamIdx, 0)
    gameAttendance&(numberGames) = actualAttendance&
    gameSite$(numberGames) = gameStadium$
    powerRating(0, numberGames) = teamRat_GAME(teamIdx, 4)
    powerRating(1, numberGames) = teamRat_GAME(1 - teamIdx, 4)

    'Write stats to new file
    Open diskPaths$(1) + statFile$ + "S" For Output As #4

    Print #4, numberGames

    For I = 1 To MAX_SCHED_STATS:
        Print #4, teamScore(I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, oppName$(I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, oppScore(I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, locIndicator$(I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, powerRating(0, I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, powerRating(1, I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, gameAttendance&(I)
    Next

    For I = 1 To MAX_SCHED_STATS:
        Print #4, gameSite$(I)
    Next

    Close #4

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
    Print #11, lineNumber, ydLine, ydsToFirst, fgAttLine, gameClock!, score(P1, 0), score(D, 0)
    'Print #11, gameTeams$(0); " vs "; gameTeams$(1)
    'Print #11, "Kickoff!"; ydLine
    'ydsToScore = 100 - ydLine
    'ydLineTeam = ydsToScore - (ydsToScore > 50) * (100 - ydsToScore - ydsToScore)
    'Print #11, downDesc$(currDown); " down and"; ydsToFirst; "at the"; ydLineTeam
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
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
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
        Rows = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";
        Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";
        Color 15: Print "-SELECTS TEAM";

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
        rbrushContrib(gameIdx, I1) = CVI(Q$(I1 * 5 + 17))
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

        indRushPct(gameIdx, 0) = rbrushContrib(gameIdx, 0)

        For currBack = 9 To 1 Step -1
            If rbrushContrib(gameIdx, currBack) <> 0 Then
                indRushPct(gameIdx, currBack) = rbrushContrib(gameIdx, currBack) - rbrushContrib(gameIdx, currBack - 1)
            End If
        Next

        teamSacks(gameIdx) = 0
        teamInts(gameIdx) = 0

        For I1 = 0 To 14
            teamSacks(gameIdx) = teamSacks(gameIdx) + playerSacks(gameIdx, I1)
            If playerSacks(gameIdx, 0) > 0 Then defSacks(gameIdx) = 1
        Next I1

        For I1 = 0 To 9
            teamInts(gameIdx) = teamInts(gameIdx) + playerInts(gameIdx, I1)
            If playerInts(gameIdx, 0) > 0 Then defInts(gameIdx) = 1
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

    Call POSTGAME

    If autoPlay <> 1 Then

        Cls

        'Using Home Team's color for the box
        Call TEAMBOX(1, 1)

        Do
            pbpString$ = "PLAY ANOTHER GAME? (Y/N)"
            Locate 7, (80 - Len(pbpString$)) / 2
            Print pbpString$

            Color 15, 0
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    Close #11

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
        Cls
        Print "PLEASE WAIT ..."
        Call FOOTBALL
    End If

End Sub


'----------------------------------------
'        FindMarkerColumn Function
'----------------------------------------
'It's not clear at this time what this
'function does
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

    teamYear% = Val(yearFromName$)

    If teamYear% >= 0 And teamYear% < 25 Then
        GetTeamYear% = 2000 + teamYear%
    Else
        GetTeamYear% = 1900 + teamYear%
    End If

End Function
