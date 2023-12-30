$Debug

Const GAME_VERSION$ = "6.56"
Const MAX_TEAMS = 1000
Const MAX_GAMES = 1500
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

Const MAX_SCHED_STATS = 120
Const SCHEDULE_SIZE_BYTES = 52

DefInt A-Z

' General purpose
'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'QBWRITE.bi'

' For Utils
'$INCLUDE: 'BoxRoutines.bi'
'$INCLUDE: 'FileTeamSelection.bi'
'INCLUDE: 'Variables.bi'


'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim oppScore(MAX_SCHED_STATS), powerRating(0 To 1, 1 To MAX_SCHED_STATS)
Dim teamScore(MAX_SCHED_STATS)

Dim gameAttendance&(MAX_SCHED_STATS)

Dim gamePK$(1, 2, 1)
Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim statsZ!(38), statsZ1!(38), statsZ2!(50, 18)

Dim statsZ1$(40), statsZ2$(40)

'-- For Road Data
Dim APR%(1, MAX_SCHED_STATS), ORD%(MAX_SCHED_STATS)
Dim TR%(MAX_SCHED_STATS)

Dim CRDR!(MAX_SCHED_STATS)

Dim gameSiteRoad$(MAX_SCHED_STATS)
Dim HR$(MAX_SCHED_STATS), ORD$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)
'Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
'Dim yearNumber$(MAX_GAMES, 1)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$, TN$

Dim location$(2), modeAbbrev$(3), overtime$(3)
Dim playMode$(3)
Dim rulesType$(5), ruleYrPro$(3), ruleYrColl$(5)
Dim teamIndicator$(1), weather$(3), windSetting$(3)
Dim yesNo$(1), yesNoText$(1)


' *** Miscellaneous Use ***
' -------------------------
Dim Shared NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

Dim Shared confLosses, confTies, confWins
Dim Shared fullLosses, fullTies, fullWins
Dim Shared PTSFC, PTSAC

Dim AN1$(9), AN2$(5), AN3$(3), AN4$(2), AN5$(2), AN6$(3), AN7$(1)
Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(3), AR7$(1)
Dim AR$(1), DIN$(9), DRI$(9), DSN$(14), DSR$(15)
Dim LCN$(0 To 15, 0 To 1), LCR$(0 To 15, 0 To 1), LFN$(0 To 1), LFR$(0 To 1)
Dim LIN$(0 To 9, 0 To 1), LIR$(0 To 9, 0 To 1), LKN$(0 To 2, 0 To 1), LKR$(0 To 2, 0 To 1)
Dim LPN$(0 To 3, 0 To 1), LPR$(0 To 3, 0 To 1), LRN$(0 To 9, 0 To 1), LRR$(0 To 9, 0 To 1)
Dim PKN$(0 To 2, 0 To 1), PKR$(0 To 2, 0 To 1)

Dim statsLC$(0 To 15, 0 To 1), statsLF$(0 To 1), statsLI$(0 To 9, 0 To 1)
Dim statsLK$(0 To 2, 0 To 1), statsLP$(0 To 3, 0 To 1), statsLR$(0 To 9, 0 To 1)
Dim statsPK$(0 To 2, 1)

Dim GIN%(0 To 9, 2), GIR%(0 To 9, 2), GSN%(0 To 15, 1), GSR%(0 To 15, 1)
Dim statsGI%(0 To 9, 2), statsGS%(0 To 14, 0 To 1), statsZ1%(40)

Dim AF!(0 To 1, 0 To 4), AM!(1, 4)
Dim AFN!(0 To 1, 0 To 4), AFR!(0 To 1, 0 To 4)
Dim AMN!(1, 4), AMR!(1, 4)

Dim K2!(13, 17)

'--LF! is only 0 to 9 b/c of career behavior
Dim statsLC!(0 To 15), statsLF!(0 To 9), statsLI!(0 To 9), statsLK!(0 To 3)
Dim statsLP!(0 To 3), statsLR!(0 To 9), statsPK!(0 To 2)

Dim LCN!(15), LCR!(15), LIN!(0 To 9), LIR!(0 To 9)
Dim LFR!(1), LFN!(1), LKN!(2), LKR!(2)
Dim LPN!(3), LPR!(3), LRN!(0 To 9), LRR!(0 To 9)
Dim PKN!(2), PKR!(2)

Dim ZN2!(14, 18), ZR!(38), ZR1!(38), ZR2!(14, 18)

Dim ratingsQB!(0 To 3, 10)

'----------------------------------------
' Used in CAREER routines
'----------------------------------------

Dim carRBacks$(50), carWdRec$(40), carQBacks$(20), carKickRet$(15), carPRet$(15), carPunters$(10), carKicker$(10)
Dim carDefInts$(55), carDefSacks$(55), carKRS$(15, 15, 1), carPRS$(15, 15, 1)
Dim careerQBS$(15, 20, 1), careerRBC$(9), careerRBS$(15, 50, 1), careerRC$(6), careerWRS$(15, 40, 1)

Dim viewRBacks$(1, 50), viewWdRec$(1, 40), viewQBacks$(1, 20), viewKickRet$(1, 15), viewPRet$(1, 15), viewPunter$(1, 10), viewKicker$(1, 10)
Dim viewDefInts$(1, 55), viewDefSacks$(1, 55)

Dim A1C$(50), A2C$(40), A3C$(20), A4C$(15), A5C$(15), A6C$(10), A7C$(10)
Dim DFC$(1), DIC$(55), DSC$(55)
Dim FGC$(6), KRC$(4), KRRS$(15, 15, 1)
Dim PRC$(4), PRRS$(15, 15, 1), PUC$(2)
Dim QBC$(8), RBR$(1), WRC$(4)

Dim KRS!(15, 15, 6), PRS!(15, 15, 6)
Dim QBBS!(15, 20, 12), RBBS!(15, 50, 10), WRRS!(15, 40, 6)

Dim DFYR%(1), FGYR%(6), KRYR%(4), PRYR%(4)
Dim PUYR%(2), QBYR%(8), RBYR%(9), WRYR%(4)

Dim DFFC!(1), FGGC!(6), KRRC!(4), PRRC!(4)
Dim PUUC!(2), QBBC!(8), RBBC!(9), WRRC!(4)

Dim careerFF!(15, 30), careerII!(15, 30), careerPP!(15, 30), careerYY!(15, 30)
Dim careerFGS!(15, 10, 17), careerGIS!(15, 55, 3), careerGSS!(15, 55, 3)
Dim careerKRS!(15, 15, 11), careerPRS!(15, 15, 11), careerPUS!(15, 10, 4)
Dim careerQBS!(15, 20, 13), careerRBS!(15, 50, 11), careerWRS!(15, 40, 11)
Dim careerRC!(6), careerTT!(50)


'----------------------------------------
' Used in COMPILER routines
'----------------------------------------
Dim fumbGain, puntNum
Dim puntName$

Dim indRecDesc$(50, 2), compKR$(3), compPK$(2), compPR$(3)
Dim compQB$(4), compRB$(10), compWR$(16)
Dim compZ1$(60), compZ2$(1 To 240), compZ3$(1 To 240)
Dim H1$(30), N2$(240), N3$(240), NT$(10, 20)
Dim PS$(10), PT$(1200)
Dim TB$(18), TR$(10, 20), teamRecDesc$(50), TT$(10, 20)

Dim compPR!(11, 11), compQB!(1 To 11, 1 To 11), compRB!(11, 11)
Dim compTT!(10, 20, 9), compWR!(16, 13)
Dim compZ1!(1 To 240, 1 To 2), compZ2!(14, 18)

Dim indRecords!(50), DT!(38), EA!(2), EM!(2), EP!(2)
Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(60), O2!(60), O3!(60), OT!(38)
Dim PT!(1200, 5), PTSA!(60), PTSF!(60)
Dim TD!(16), TG!(16), TP!(18), teamRecords!(50)
Dim YD!(16)

'----------------------------------------
' Used in DRAFT routines
'----------------------------------------
Dim FGA_DRAFT(1), FGPct_DRAFT(1)
Dim PAT_DRAFT(1), PATPct_DRAFT(1)

Dim krNumRet_DRAFT(2), krRetAvg_DRAFT(2)
Dim leagRat_DRAFT(7), numInt_DRAFT(9), numSack_DRAFT(14)
Dim prRet_DRAFT(2), prRetAvg_DRAFT(2), puntAvg_DRAFT(2)
Dim qbArmRat_DRAFT(3), qbPA_DRAFT(3), qbPCPct_DRAFT(3), qbIntPct_DRAFT(3)
Dim rbAtt_DRAFT(9), rbAvg_DRAFT(9), rbRec_DRAFT(15), rbRecAvg_DRAFT(9)
Dim tmRat_DRAFT(4), wrRec_DRAFT(5), wrAvg_DRAFT(5)

Dim intNam_DRAFT$(9), krNam_DRAFT$(2)
Dim prNam_DRAFT$(2), pntNam_DRAFT$(2), pkNam_DRAFT$(1)
Dim qbNam_DRAFT$(3), rbNam_DRAFT$(9)
Dim sackNam_DRAFT$(14), wrNam_DRAFT$(5)


'----------------------------------------
' Used in TRADE routines
'----------------------------------------
Dim haveStats(1)

' ** Team File **
Dim FGA_TRADE(1, 1), FGPct_TRADE(1, 1)
Dim PAT_TRADE(1, 1), PATPct_TRADE(1, 1)

Dim krNumRet_TRADE(1, 2), krRetAvg_TRADE(1, 2)
Dim leagRat_TRADE(1, 7), numInt_TRADE(1, 9), numSack_TRADE(1, 14)
Dim prRet_TRADE(1, 2), prRetAvg_TRADE(1, 2), puntAvg_TRADE(1, 3)
Dim qbArmRat_TRADE(1, 3), qbPA_TRADE(1, 3), qbPCPct_TRADE(1, 3), qbIntPct_TRADE(1, 3)
Dim rbAtt_TRADE(1, 9), rbAvg_TRADE(1, 9), rbRec_TRADE(2, 15), rbRecAvg_TRADE(1, 9)
Dim tmRat_TRADE(1, 4), wrAvg_TRADE(1, 5), wrRec_TRADE(1, 5)
Dim ydCmpAdj_TRADE!(1), ydsPerComp_TRADE(1)

Dim krNam_TRADE$(2, 3), nicks_TRADE$(1)
Dim prNam_TRADE$(2, 3), pntNam_TRADE$(2, 3), pkNam_TRADE$(1, 1), puntNam_TRADE$(1)
Dim qbNam_TRADE$(2, 4), rbNam_TRADE$(2, 9)
Dim teams_TRADE$(1), wrNam_TRADE$(2, 6)


' ** Stats File **
Dim tradeAF!(1, 1, 4), tradeAM!(1, 1, 4)
Dim tradeLC!(2, 15), tradeLF!(1, 1), tradeLI!(1, 9)
Dim tradeLK!(1, 2), tradeLP!(1, 3), tradeLR!(1, 9)
Dim tradePK!(1, 2), tradeT!(1)
Dim tradeZ!(1, 38), tradeZ1!(1, 38), tradeZ2!(1, 13, 17)

Dim tradeGI%(1, 9, 2), tradeGS%(1, 14, 1)

Dim statNam_TRADE$(1)
Dim tradeLF$(1, 1), tradeLI$(1, 9, 1)
Dim tradeLK$(1, 2, 1), tradeLP$(1, 3, 1)

Dim tradeLC$(1, 15, 1), tradeLR$(1, 9, 1)
Dim tradePK$(1, 2, 1), tradePR$(1, 2)
Dim tradeQB$(1, 3), tradeRB$(1, 9), tradeWR$(1, 5)
Dim tradeF$(1), tradeFG$(1, 1), tradeKR$(1, 2)
Dim tradeYN$(1)

'----------------------------------------
'   Used in Head-To-Head routines
'----------------------------------------
Dim div1$, div2$, div3$, div1_2$, div2_2$, div3_2$

'These are mostly keeping track of quantities (# wins, # losses, etc)
'so they could probably be INTEGERS.
'However from the original code they are intended to be Singles

Dim awayLosses!(30), AL2!(30), awayWins!(30), AW2!(30)

Dim G1!(100), G12!(100), G2!(100), G22!(100)
Dim G3!(100), G32!(100), G4!(100), G42!(100)
Dim HL!(30), HL2!(30), HT!(30), HT2!(30), HW!(30), HW2!(30)
Dim RT2!(30), RT!(30)
Dim TAL!(4), TAL2!(4), TAW!(4), TAW2!(4)
Dim TG1!(4), TG12!(4), TG2!(4), TG22!(4)
Dim TG3!(4), TG32!(4), TG4!(4), TG42!(4)
Dim THL!(4), THL2!(4), THW!(4), THW2!(4)
Dim THT!(4), THT2!(4), TRT!(4), TRT2!(4)

Dim DV$(3), DV2$(3), Z12$(30), Z22$(30)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
'   ind recs, team recs
Dim indRecCategory$(46), teamRecCategory$(43)


'----------------------------------------
' Used in STAT / INPUT
'----------------------------------------
Dim JA, JB, JC, JD, JE, JF, JG, JH, JI

Dim gameCount(30), lookyP%(1)

Dim GMA!(300), GMB!(180), GMC!(120)

Dim A1L!(300, 8), A2L!(180, 4), A3L!(120, 13), A3R!(120, 1)
Dim A4L!(90, 4), A5L!(90, 4), A6L!(30, 2)
Dim A7L!(60, 5), A8L!(300), A9L!(450)
Dim teamStats!(1, 12), TYP!(480), lookyTT!(31, 30), sackStats!(1, 1)

Dim A1L$(300), A1T$(300), A2L$(180), A2T$(180), A3T$(120), A4L$(90), A4T$(90)
Dim A5L$(90), A5T$(90), A6L$(30), A6T$(30), A7L$(60), A7T$(60)
Dim A8L$(300), A8T$(300), A9L$(450), A9T$(450)
Dim expCategories$(74)

Dim intNam_TRADE$(1, 9)
Dim LCL$(480), LKL$(90), LPL$(120), LRL$(300), PKL$(90)
Dim lookyRBacks$(10), lookyWdRec$(6), lookyQBacks$(4), lookyKickRet$(3), lookyPRet$(3), lookyPunter$(2), lookyKicker$(2)
Dim lookyTT$(31, 30)
Dim QBL$(120), sackNam_TRADE$(1, 14)
Dim TMM$(480), TPP$(480), TYY$(480)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim LD!(240), LZ!(240)
Dim seeL!(60), seeT!(60), seeW!(60)

Dim seeP$(60), TM$(60) ', TR$(10, 20)
Dim seePR$(1200), seeT$(60)


'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%

Dim scheduleAP%(1), scheduleNG%(MAX_GAMES, 20)

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleQB_V$(20), scheduleQB_H$(20)
Dim scheduleYN$(MAX_GAMES, 1)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim tickerStart
Dim actualAttendance&, avgAttendance&

Dim Shared ballPosCmpAdj, passTypeCmpAdj, windCmpAdj

'BW = black & white; as in, set to 1 and the game will lose all color
Dim Shared autoPlay, ballFumbled, BW
Dim Shared catchPctSuccess, chosenPlay, coverage, compTeam, CP, currDown ', compOffense
Dim Shared D, dLine
Dim Shared endAllGames, endGame, endYds, endZone, expCompPct
Dim Shared fgSuccessChance, fgAttYds, ffPctSuccess
Dim Shared gameLoc, goalPostAdj, halfTime

'These all seem to only be used for loops, but it is difficult to confirm
Dim Shared I1, I2, I3, I4, I5, I6, I7, I8, I9, I, J

Dim Shared isOT, JJ, K3, fieldSide, kickYL
Dim Shared O, overtimeOpt, nbrScores
Dim Shared penaltyYds, playerMode, playerOpt, playType
Dim Shared P1, playCall, periodNbr, puntRetNbr
Dim Shared quarter, qback, qbTakeKnee
Dim Shared revFumble, ruleOptColl, ruleOptPro, ruleOptType, rushYds
Dim Shared S6, startYds, tickerGames
Dim Shared W5, WE, WS, winTeam, yrdLine

'-- There is no indication of X being used in any way!!!
'-- it seems similar to I/J loop variables
Dim Shared X, X1

'The below 2 variables are related to scouting
Dim Shared XD, XE

Dim Shared ydsToFirst As Single
Dim Shared ydsGained As Single, ydLine As Single, ydLineTeam As Single, ydsToScore As Single
Dim Shared YC, YF, Z1

Dim Shared BO%, EG%, F%, FF%, GL%, HB%, intChance
Dim Shared PC%, PS%, R5%

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared adjF0(4), adjF1(4), AF(1, 1, 0 To 4), AM(1, 1, 0 To 4)
Dim Shared defInts(1), defSacks(1), defYdAdj(0 To 8, 0 To 10)
Dim Shared FA(1, 1, 4), FM(1, 1, 4)
Dim Shared gameInts(1, 9, 2), gameLongFG(1, 1), gameSacks(1, 14, 1), gameStats(1, 36)
Dim Shared gameZ(0 To 38), gameZ1(0 To 38), gameZ2(0 To 13, 0 To 17)
Dim Shared K1(50, 6), K2(1, 13, 17), K3(1, 6)
Dim Shared hasRunFF(1), playerInts(1, 9), IR(1, 9)
Dim Shared kickerFGA(1, 1), kickerFGPct(1, 1), kickerIdx(1, 1), kickerPATPct(1, 1)
Dim Shared kickReturners(0 To 1, 0 To 2), krNumRet(1, 2), krYdsPerRet(1, 2)
Dim Shared LC(1, 20), leagRat_GAME(1, 7), LF(1, 1), LI(1, 0 To 9), LK(1, 0 To 2), LP(1, 0 To 3), LR(1, 0 To 9)
Dim Shared nbrPossOT(1), penaltyYds(2), PK(0 To 1, 0 To 2), playerSacks(1, 14)
Dim Shared puntReturners(0 To 1, 0 To 2), prNumRet(1, 2), prYdsPerRet(1, 2)
Dim Shared quarterbacks(1, 3), qbNumber(1)
Dim Shared qbArmRat(1, 3), qbMobility(1), qbNumAtt(1, 9), qbCompPct(1, 9), qbPctInt(1, 9)
Dim Shared runBacks(1, 17), rbRushAtt(1, 9), rbRushAvg(1, 9), rbNumRec(1, 9), rbYdsPerC(1, 9)
Dim Shared score(0 To 1, 0 To 10), scoreQuarters(50), scoreTimes(50), schedGame(2)
Dim Shared teamIdx_GAME(2), teamInts(1), teamSacks(1), timeouts(1), timePoss(1), tmRat_GAME(2, 9)
Dim Shared totalInts(0 To 1, 0 To 9, 0 To 2), totalSacks(0 To 1, 0 To 14, 0 To 1)
Dim Shared useRandomQB(1)
Dim Shared W6(1, 1), WR(1, 20), wrNumRec(1, 9), wrYdsPerC(1, 9)
Dim Shared XD(1), ydsPerComp(1), ydsPerPunt(1, 2)

Dim Shared BY%(38, 4), GL%(1 To 30, 1 To 2), HB%(1)
Dim Shared NG%(20), PC%(1, 9), PS%(2, 21)
Dim Shared Q7%(1), QR%(50, 2)
Dim Shared RM%(1, 14), RN%(1, 38), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14)
Dim Shared teamYears(1), thirdDownAtt(1), thirdDownFail(1)
Dim Shared V4%(1, 3), WX%(6)

Dim scheduleFile$

Dim Shared defTeam$, gameStadium$, markers$
Dim Shared pbpString$, targetWRName$, YN$

Dim Shared defFormation$(0 To 15), defPlay$(1 To 15), defInts$(1, 9)
Dim Shared defSacks$(1, 14), downDesc$(0 To 4), direction$(0 To 1)
Dim Shared gameMascots$(1), gameTeams$(0 To 1), gadget$(3)
Dim Shared kicker$(1, 1), kickRet$(1, 2)
Dim Shared offensePlay$(50), passCov$(0 To 11)
Dim Shared penaltyDesc$(2), playDesc$(30), playSelect$(9)
Dim Shared pret$(1, 2), punter$(1, 2), qbacks$(1, 3)
Dim Shared rbacks$(1, 10), SX$(1 To 33, 0 To 1)
Dim Shared diskIDs$(0 To 1), tickerPeriod$(14), wdRec$(1, 5)

'These appear only used to be assigned a "t" value when a TD is implied.
'They are included in the compiling of stats file
'I believe they are for tracking records / "longest" plays
Dim Shared gameIR$(1, 9), gameKR$(1, 2), gameLC$(1, 20, 1), gameLR$(1, 9, 1)
Dim Shared gamePR$(1, 2), gameRB$(1, 17), gameQB$(1, 3), gameWR$(1, 20)



'$INCLUDE: 'MainRoutines.bi'
'$INCLUDE: 'TITLE.bi'

' For Game
'$INCLUDE: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
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
For I = 0 To 45: Read indRecCategory$(I): Next

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
For I = 0 To 43: Read teamRecCategory$(I): Next


Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,RUSING TOUCHDOWNS,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING AVERAGE,RECEIVING TOUCHDOWNS,LONGEST RECEPTION
Data PASS ATTEMPTS,PASS COMPLETIONS,INTERCEPTIONS,PASSING YARDS,TOUCHDOWN PASSES,COMPLETION PERCENTAGE,AVERAGE YDS/COMP.,QUARTERBACK RATING,LONGEST PASS
Data KICKOFF RETURNS,KICKOFF RETURN YARDS,AVERAGE GAIN/KO RETURN,KICKOFF RETURN TD'S,LONGEST KICKOFF Return
Data PUNT RETURNS,PUNT RETURN YARDS,AVERAGE GAIN/PUNT RETURN,PUNT RETURN TD'S,LONGEST PUNT Return
Data PUNTS,PUNTING YARDS,AVERAGE PER PUNT
Data F.G.'s MADE,F.G. ATTEMPTS,F.G. PERCENTAGE,E.P.'s MADE,E.P. ATTEMPTS,E.P. PERCENTAGE,LONGEST s
Data INTERCEPTIONS,SACKS
Data NON-KICKERS,KICKERS,ALL PLAYERS
Data FIRST DOWNS,RUSH ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,PASSING YARDS,TOTAL YARDS,PASS COMPLETION PCT.,FUMBLES,FUMBLES LOST,INTERCEPTIONS THROWN,TURNOVER RATIO,PENALTIES,PENALTY YARDAGE,SACKS ALLOWED,SACK YARDAGE
Data OPP. FIRST DOWNS,OPP. RUSH ATTEMPTS,OPP. RUSHING YARDS,OPP. RUSHING AVERAGE,OPP. PASSING YARDS,OPP. TOTAL YARDS,OPP. PASS COMPLETION PCT.,OPP. FUMBLES,OPP. FUMBLES RECOVERED,INTERCEPTIONS,OPP. PENALTIES,OPP. PENALTY YARDAGE,SACKS
Data SACK YARDAGE LOST,ALL-PURPOSE YARDS
For I = 0 To 73: Read expCategories$(I): Next


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

Data 90,88,80,78,60,68,20,62,-5,52: 'FIELD GOAL INFO

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

Data 69,80,77,61,62,77,64,45,36,69,49,11,2,15,48,45,31,21,29,78,55,48,65,70,58,61,78,70,68,65,70,63,65,70,76,81,87,79: ' RN%(0,X)
Data -4,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,6,7: ' RN%(1,X)

'BY%
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
    Read adjF0(I)
    Read adjF1(I)
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
            Read S1%(I, I1, J)
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
        Read RN%(I, I1)
    Next
Next

For I = 1 To 38:
    For I1 = 1 To 4:
        'Breakaway yardage???
        Read BY%(I, I1)
    Next
Next

For I = 1 To 30:
    For I1 = 1 To 2:
        'Goalline yard adjustments
        Read GL%(I, I1)
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
        '???
        Read PS%(I, I1)
    Next
Next

For I = 1 To 2:
    For I1 = 0 To 20:
        'Y/C% Adjustment
        Read QR%(I1, I)
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

rulesType$(0) = "PLAY USING COLLEGE RULES"
rulesType$(1) = "PLAY USING NFL RULES    "
rulesType$(2) = "PLAY USING USFL RULES   "
rulesType$(3) = "PLAY USING AFL RULES    "
rulesType$(4) = "PLAY USING WFL RULES    "
rulesType$(5) = "PLAY USING WLAF RULES   "

ruleYrPro$(0) = "1978-1993"
ruleYrPro$(1) = "PRE-1974 "
ruleYrPro$(2) = "1974-1977"
ruleYrPro$(3) = "POST-1993"
'ruleYrPro$(3) = "1994-2007"
'ruleYrPro$(4) = "POST-2007"

ruleYrColl$(0) = "PRE-1968 "
ruleYrColl$(1) = "1968-1980"
ruleYrColl$(2) = "1981-1983"
ruleYrColl$(3) = "1984-1991"
ruleYrColl$(4) = "1991     "
ruleYrColl$(5) = "POST-1991"
'ruleYrColl$(5) = "1992-2007"
'ruleYrColl$(6) = "POST-2007"


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

If Not _FileExists("DEV.BAS") Then

    Open "DEV.BAS" For Output As #1

    For I = 0 To 2
        Select Case opSysType$
            Case "[WINDOWS]": Print #1, _CWD$ + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": Print #1, _CWD$ + "/"
        End Select
    Next

    Close #1

End If

Open "DEV.BAS" For Input As #2

For I = 0 To 2
    Input #2, diskPaths$(I)
Next

Close #2

Selection% = 1

Do

    Call Main_Menu(Selection%)

Loop Until Selection% = 7

End

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
Sub Main_Menu (menuSelection%)

    Do

        _Title "3-in-1 Football - Menu"

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        'menuChoices$(6) = "HARD DRIVE INSTALLATION         "
        menuChoices$(6) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(7) = "EXIT TO DOS                     "

        menuSelection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection%

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

    Loop Until menuSelection% = 7 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 7

End Sub


'----------------------------------------
'       ConfigureDrives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Shared opSysType$

    Color 15, 1

    FG = 0
    BG = 7

    For I = 0 To 2
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print
    Locate , 31: Print "PATHNAME EXAMPLES:"

    Select Case opSysType$
        Case "[WINDOWS]":
            Locate , 31: Print "A:"
            Locate , 31: Print "C:\FOOT\TEAMS"
        Case "[LINUX]":
            Locate , 31: Print "/mnt/floppy"
            Locate , 31: Print "/usr/bin/foot/teams"
        Case "[MAC]", "[MACOSX]":
            Locate , 31: Print "/Volumes/floppy"
            Locate , 31: Print "/Applications/Foot/teams"
    End Select

    Print
    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 61), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 61), FG, BG, -1
    Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
    Locate , 18: QPrint PadRight$(diskPaths$(2), 59), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    GetDiv3:
    Locate 11, 18 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(2) = _Trim$(diskPaths$(2))

    Done:
    Color 14, 1
    Locate 23, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 Then GoTo GetDiv3
    If X$ = "N" Then GoTo GetDiv1
    If X$ <> "Y" Then GoTo Done 'insist on either Y or N or up-arrow

    Open "DEV.BAS" For Output As #1

    For R = 0 To 2:

        Select Case opSysType$
            Case "[WINDOWS]": diskPaths$(R) = diskPaths$(R) + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": diskPaths$(R) = diskPaths$(R) + "/"
        End Select

        Print #1, diskPaths$(R)

    Next R

    Close #1

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'...explanation...
Sub TeamMgt_Menu ()

    Do

        Cls

        Call TITLE

        ReDim menuChoices$(1 To 7)

        menuChoices$(1) = "INPUT A NEW TEAM            "
        menuChoices$(2) = "EDIT AN EXISTING TEAM       "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT  "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT   "
        menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM"
        menuChoices$(6) = "TRADE PLAYERS               "
        menuChoices$(7) = "RETURN TO MAIN MENU         "
        menuSelection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case menuSelection%

                Case 1:
                    Call InputTeam

                Case 2:

                    Call ChooseTeam(YN$, NTMS, teamNbr, A$)

                    If teamNbr <> 0 Then
                        Call EditTeam(A$, YN$, teamNbr)
                    End If

                Case 3:
                    Call ChooseTeam(YN$, NTMS, teamNbr, A$)
                    If teamNbr <> 0 Then
                        Call PrintRoster(A$, teamNbr)
                    End If

                Case 4:
                    Call ChooseTeam(YN$, NTMS, teamNbr, A$)
                    If teamNbr <> 0 Then
                        Call ViewRoster(A$, teamNbr)
                    End If

                Case 5:
                    Call DRAFT

                Case 6:
                    Call TRADE

            End Select

        End If

    Loop Until menuSelection% = 7 Or ExitCode% = 27

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

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS       "
        menuChoices$(2) = "ENCYCLOPEDIA / TEAM REPLAY OPTIONS"
        'menuChoices$(3) = "MODIFY REPLAY STATS               "
        menuChoices$(3) = "MERGE SEPARATE STATISTICAL FILES  "
        menuChoices$(4) = "RETURN TO MAIN MENU               "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1:
                    3 Call LOOKY

                Case 2:
                    Call CAREER

                Case 3:
                    Call MERGE

                Case 4:
                    '

            End Select

        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

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

        menuChoices$(1) = "CREATE / EDIT          "
        menuChoices$(2) = "UPDATE STATS/ STANDINGS"
        menuChoices$(3) = "VIEW / PRINT REPORTS   "
        menuChoices$(4) = "HEAD-TO-HEAD STATS     "
        menuChoices$(5) = "LEAGUE RECORDS         "
        menuChoices$(6) = "EXPANDED LEADERS       "
        menuChoices$(7) = "DELETE LEAGUE STATS    "

        menuChoices$(8) = "CREATE / EDIT          "
        menuChoices$(9) = "UPDATE STATS/ STANDINGS"
        menuChoices$(10) = "VIEW / PRINT REPORTS   "
        menuChoices$(11) = "HEAD-TO-HEAD STATS     "
        menuChoices$(12) = "CONFERENCE RECORDS     "
        menuChoices$(13) = "EXPANDED LEADERS       "
        menuChoices$(14) = "DELETE CONFERENCE STATS"

        menuChoices$(15) = "CREATE / EDIT          "
        menuChoices$(16) = "UPDATE POLL / STATS    "
        menuChoices$(17) = "VIEW / PRINT REPORTS   "
        menuChoices$(18) = "NATIONAL RECORDS       "
        menuChoices$(19) = "DELETE NATIONAL STATS  "
        menuChoices$(20) = "RETURN TO 3 IN 1 MENU  "
        menuChoices$(21) = "EXIT TO DOS            "
        Selection% = 1
        Count% = 21
        Start% = 1
        FileFGColor% = 15: FileBGColor% = 1
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
        Print "TRACK AND UPDATE TO GENERATE A NATIONAL POLL, NATIONAL LEADERS."

        _MouseShow "DEFAULT"
        MAMenu menuChoices$(), Selection%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% = 27 Then Exit Sub

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: Call CREATPRO
                Case 2: Call CompileLeague
                Case 3: Call SEEPRO
                Case 4: Call HD2HDPRO
                Case 5: Call Records_Menu(1)
                Case 6: Call NEWLDR(1) 'Call ViewExpandedStats(1)
                Case 7: Call BLANKPRO
                Case 8: Call CREATCOL
                Case 9: Call CompileCollege
                Case 10: Call SEECOLL
                Case 11: Call HD2HDCOL
                Case 12: Call Records_Menu(2)
                Case 13: Call NEWLDR(0) 'Call ViewExpandedStats(0)
                Case 14: Call BLANKCOL
                Case 15: Call CREATNAT
                Case 16: Call CompileNatlOrg
                Case 17: Call SEENATL
                Case 18: Call Records_Menu(3)
                Case 19: Call BLANKORG

            End Select

        End If

        If Selection% = 21 Then
            End
            System
        End If

    Loop Until Selection% >= 20 Or ExitCode% = 27

End Sub


' General purpose
'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'
'$INCLUDE: 'QBWRITE.bm'

' For Utils
'$INCLUDE: 'BoxRoutines.bm'
'$INCLUDE: 'FileTeamSelection.bm'
'$INCLUDE: 'TITLE.bm'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANKCOL.BAS'
'$INCLUDE: 'BLANKORG.BAS'
'$INCLUDE: 'BLANKPRO.BAS'
'$INCLUDE: 'CAREDIT.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARMERGE.BAS'
'$INCLUDE: 'CARPRNT.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CNGTM.BAS'
'$INCLUDE: 'COMPCOLL.BAS'
'$INCLUDE: 'COMPLEAG.BAS'
'$INCLUDE: 'COMPNATL.BAS'
'$INCLUDE: 'CREATCOL.BAS'
'$INCLUDE: 'CREATNAT.BAS'
'$INCLUDE: 'CREATPRO.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'HD2HDCOL.BAS'
'$INCLUDE: 'HD2HDPRO.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECOLL.BAS'
'$INCLUDE: 'SEENATL.BAS'
'$INCLUDE: 'SEEPRO.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
'$INCLUDE: 'QBFunctions.bm'

' For Game
'$INCLUDE: 'FOOTBALL.BAS'
'$INCLUDE: 'GameRoutines.bm'

