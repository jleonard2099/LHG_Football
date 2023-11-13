$Debug

Const GAME_VERSION$ = "6.45"
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

Dim ratingsQB!(3, 10)

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

Dim BRC$(50, 2), compKR$(3), compPK$(2), compPR$(3)
Dim compQB$(4), compRB$(10), compWR$(16)
Dim compZ1$(50), compZ2$(240), compZ3$(240)
Dim H1$(30), N2$(240), N3$(240), NT$(10, 20)
Dim PS$(10), PT$(1200)
Dim TB$(18), TR$(10, 20), TRC$(50), TT$(10, 20)

Dim compPR!(11, 11), compQB!(11, 11), compRB!(11, 11)
Dim compTT!(10, 20, 9), compWR!(16, 13)
Dim compZ1!(240, 2), compZ2!(14, 18)

Dim BRC!(50), DT!(38), EA!(2), EM!(2), EP!(2)
Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(50), O2!(50), O3!(50), OT!(38)
Dim PT!(1200, 5), PTSA!(50), PTSF!(50)
Dim TD!(16), TG!(16), TP!(18), TRC!(50)
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
Dim TP$(46), TP1$(43)


'----------------------------------------
' Used in STAT / INPUT
'----------------------------------------
Dim JA, JB, JC, JD, JE, JF, JG, JH, JI

Dim gameCount(30), lookyP%(1)

Dim GMA!(300), GMB!(180), GMC!(100)

Dim A1L!(300, 8), A2L!(180, 4), A3L!(100, 13), A3R!(100, 1)
Dim A4L!(90, 4), A5L!(90, 4), A6L!(30, 2)
Dim A7L!(60, 5), A8L!(300), A9L!(450)
Dim teamStats!(1, 12), TYP!(480), lookyTT!(31, 30), sackStats!(1, 1)

Dim A1L$(300), A1T$(300), A2L$(180), A2T$(180), A3T$(100), A4L$(90), A4T$(90)
Dim A5L$(90), A5T$(90), A6L$(30), A6T$(30), A7L$(60), A7T$(60)
Dim A8L$(300), A8T$(300), A9L$(450), A9T$(450)
Dim expCategories$(74)

Dim intNam_TRADE$(1, 9)
Dim LCL$(480), LKL$(90), LPL$(100), LRL$(300), PKL$(90)
Dim lookyRBacks$(10), lookyWdRec$(6), lookyQBacks$(4), lookyKickRet$(3), lookyPRet$(3), lookyPunter$(2), lookyKicker$(2)
Dim lookyTT$(31, 30)
Dim QBL$(100), sackNam_TRADE$(1, 14)
Dim TMM$(480), TPP$(480), TYY$(480)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim LD!(240), LZ!(240)
Dim seeL!(50), seeT!(50), seeW!(50)

Dim seeP$(50), TM$(50) ', TR$(10, 20)
Dim seePR$(1200), seeT$(50)


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

Dim Shared isOT, JJ, K3, kickDir, kickYL, O, overtimeOpt
Dim Shared nbrScores, playerMode, playerOpt, P1, P2, periodNbr, puntRetNbr
Dim Shared quarter, qback, qbTakeKnee
Dim Shared revFumble, ruleOptColl, ruleOptPro, ruleOptType
Dim Shared R1, rushYds
Dim Shared S6, startYds, tickerGames
Dim Shared W5, WE, WS, winTeam, yrdLine

'-- There is no indication of X being used in any way!!!
'-- it seems similar to I/J loop variables
Dim Shared X, X1

'The below 2 variables are related to scouting
Dim Shared XD, XE

Dim Shared Y1, YC, YF, Z1
Dim Shared ydsGained, ydLine, ydLineTeam, ydsToScore

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
Dim Shared puntReturners(0 To 1, 0 To 2), prNumRet(1, 2), prYdsPerRet(1, 2), puntYdsPerP(1, 2)
Dim Shared quarterbacks(1, 3), qbNumber(1)
Dim Shared qbArmRat(1, 3), qbMobility(1), qbNumAtt(1, 9), qbCompPct(1, 9), qbPctInt(1, 9)
Dim Shared runBacks(1, 17), rbRushAtt(1, 9), rbRushAvg(1, 9), rbNumRec(1, 9), rbYdsPerC(1, 9)
Dim Shared score(0 To 1, 0 To 10), scoreQuarters(50), scoreTimes(50), schedGame(2)
Dim Shared teamIdx_GAME(2), teamInts(1), teamSacks(1), timeouts(1), timePoss(1), tmRat_GAME(2, 9)
Dim Shared totalInts(0 To 1, 0 To 9, 0 To 2), totalSacks(0 To 1, 0 To 14, 0 To 1)
Dim Shared useRandomQB(1)
Dim Shared W6(1, 1), WR(1, 20), wrNumRec(1, 9), wrYdsPerC(1, 9), XD(1), ydsPerComp(1)

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
For I = 0 To 45: Read TP$(I): Next

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
For I = 0 To 43: Read TP1$(I): Next


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

ruleYrColl$(0) = "PRE-1968 "
ruleYrColl$(1) = "1968-1980"
ruleYrColl$(2) = "1981-1983"
ruleYrColl$(3) = "1984-1991"
ruleYrColl$(4) = "1991     "
ruleYrColl$(5) = "POST-1991"

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

        ReDim menuChoices$(1 To 5)

        menuChoices$(1) = "VIEW / PRINT COMPILED STATS       "
        menuChoices$(2) = "ENCYCLOPEDIA / TEAM REPLAY OPTIONS"
        menuChoices$(3) = "MODIFY REPLAY STATS               "
        menuChoices$(4) = "MERGE SEPARATE STATISTICAL FILES  "
        menuChoices$(5) = "RETURN TO MAIN MENU               "
        Selection% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
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
                    Call EditStats

                Case 4:
                    Call MERGE

            End Select

        End If

    Loop Until Selection% = 5 Or ExitCode% = 27

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
'INCLUDE: 'BLANKPRO.BAS'


'----------------------------------------
'          BLANKPRO Subroutine
'----------------------------------------
'This routine assists the user in choosing
'a league, and then proceeds to delete all
'stats files for all the associated teams
Sub BLANKPRO ()

    _Title "3-in-1 Football - Erase League Stats"

    Dim H1$(40)

    leagueFile$ = ChooseFile$(diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        MsgText$ = "WARNING!!! THIS WILL ERASE ALL COMPILED STATS FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL STATS, HIT 'Y' FOR YES"
        BoxWidth = 27
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I2$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

        If UCase$(I2$) = "Y" Then

            Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #1

            Input #1, league$, div1$, div2$, div3$

            For XW = 1 To 30
                Input #1, H1$(XW)
            Next

            Close #1

            For XW = 1 To 30

                If H1$(XW) <> Chr$(42) Then
                    statFile$ = diskPaths$(1) + H1$(XW)
                    Print "DELETING "; H1$(XW); " STATS"
                    If _FileExists(statFile$) Then Kill statFile$
                    If _FileExists(statFile$ + "S") Then Kill statFile$ + "S"

                End If

            Next

        End If 'Done confirming to move forward with deletion

    End If

End Sub



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
    If Not _FileExists("DEV.BAS") Then

        Open "DEV.BAS" For Output As #1

        For I = 0 To 2
            Print #1, _CWD$ + "\"
        Next

        Close #1

    End If

    Open "DEV.BAS" For Input As #2

    For I = 0 To 2
        Input #2, diskPaths$(I)
    Next

    Close #2

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
    Input #4, R5%
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

        'Read in schedule options
        Open diskPaths$(0) + scheduleFile$ As #7 Len = SCHEDULE_SIZE_BYTES

        For X = 0 To 20
            Field #7, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
        Next

        Field #7, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

        Get #7, schedGame(0) + schedGame(2)

        For X = 0 To 20:
            NG%(X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1:
            diskIDs$(X) = Q$(22 + X)
        Next

        Close #7

        autoPlay = 1

    Else

        'Open "pbplog" For Append As #11

    End If


    If autoPlay = 1 Then

        Call AUTOSETUP

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
            Color tmRat_GAME(0, 2), tmRat_GAME(0, 1): Print gameTeams$(0)
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color tmRat_GAME(1, 2), tmRat_GAME(1, 1): Print gameTeams$(1)
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

            Locate , 3: Print "* NCAA: 1968+, CLOCK STOPPED AFTER A 1ST DOWN"
            Locate , 3: Print "        PRE-1984, PASS INTERFERENCE SPOTTED AT INFRACTION"
            Locate , 3: Print "        1992+, ADVANCING FUMBLE ALLOWED"
            Locate , 3: Print "* PRO:  PRE-1974, GOAL POSTS ON GOAL, BALL ON 20 AFTER MISS FG"
            Locate , 3: Print "        1994+, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"
            Locate , 3: Print "        2017+, 10 MINUTE OT, 2 TOs, EACH TEAM POSSESS + FIRST SCORE WINS"
            Locate , 3: Print "* PRO KO: PRE-1974, @ 40 YL; 1974-1993, @ 35 YL; 1994-PRESENT, @ 30 YL"
            Locate , 3: Print "* NCAA HOLDING : PRE-1984, 15 YDS; 1984+ 10 YDS @ PT OF INFRACTION"
            Locate , 3: Print "* PRO HOLDING  : PRE-1974, 15 YDS; 1974-77, 10 YDS RUN / 15 YDS PASS;"
            Locate , 3: Print "                 1978+, 10 YDS ON ALL";

            Do

                Color 7

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then
                    iVal = Val(I$)
                    If UCase$(I$) = "A" Then iVal = 10
                End If

            Loop Until iVal >= 0 And iVal <= 10 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case iVal

                    Case 0:
                        Open "DEFAULT" For Output As #8
                        Print #8, playerOpt '
                        Print #8, ruleOptType '
                        Print #8, gameLoc ' Home Field advantage
                        Print #8, pbpDelay! '
                        Print #8, R5% '
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
                        If ruleOptPro > 3 Then ruleOptPro = 0

                    Case 7
                        ruleOptColl = ruleOptColl + 1
                        If ruleOptColl > 5 Then ruleOptColl = 0

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

                If iVal >= 4 Then
                    goalPostAdj = 0
                    R5% = 0
                    If ruleOptPro = 1 And ruleOptType <> 0 Then goalPostAdj = -10
                    If ruleOptType = 4 Then goalPostAdj = 0
                    If ruleOptColl = 0 And ruleOptType = 0 Then R5% = 1
                End If

            End If

        Loop Until (iVal = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or UCase$(I$) = "Q"

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

            'End
            'System
            Exit Sub

        End If

    End If

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
                'Use random # +/- 5,000 of team's average attendance
                '-- real #'s reflect a wider range throughout the year,
                '-- however many teams have their avg. attendance close
                '-- to max capacity. Since we don't have max capacity
                '-- available for now, we're less likely to go over if
                '-- we don't go higher on our range.
                actualAttendance& = avgAttendance& + Int(Rnd(1) * 10000) - 5000

                'If original team's average attendance was less than 10,000,
                'we could end up with a negative number. So let's use an
                'arbitrary one instead.
                'Random # from 500 - 700
                ' ** I commented this out as it seems absurdly low.
                ' ** Was it meant to entertain the idea of high school football???
                'If actualAttendance& < 0 Then actualAttendance& = Int(Rnd(1) * 500) + 200
            End If

        End If

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

    goalPostAdj = 0
    R5% = 0

    If ruleOptPro = 1 And ruleOptType <> 0 Then goalPostAdj = -10
    If ruleOptType = 4 Then goalPostAdj = 0
    If ruleOptColl = 0 And ruleOptType = 0 Then R5% = 1

    Color 15, 0

    If ruleOptType <> 0 Then
        leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 2
        leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 2
    End If

    leagRat_GAME(0, 7) = leagRat_GAME(0, 7) + 4
    leagRat_GAME(1, 7) = leagRat_GAME(1, 7) + 4

    Call NAMEEDIT

    For I3 = 0 To 1

        If autoPlay = 1 Then
            'Autoamtic determine QB
            Erase Q7%

            qbNumber(0) = NG%(5)

            If qbNumber(0) = 4 Then
                useRandomQB(0) = 1
            Else
                qbMobility(0) = NG%(6)
            End If

            qbNumber(1) = NG%(8)

            If qbNumber(1) = 4 Then
                useRandomQB(1) = 1
            Else
                qbMobility(1) = NG%(9)
            End If

            For I = 0 To 3
                V4%(I3, I) = -1
            Next I

            If useRandomQB(I3) <> 1 Then
                For I4 = 0 To 9:
                    For I5 = 0 To 3
                        If rbacks$(I3, I4) = qbacks$(I3, I5) And I5 <> qbNumber(I3) Then V4%(I3, I5) = I4
                    Next I5
                Next I4
            End If

        Else

            'Manual prompt for QB
            '-- we could probably update the routine
            '-- to handle autoPlay = 1 by itself
            Call CHANGEQB

        End If

    Next I3

    Call SETWEATHER

    '----------
    'Coin Toss
    '----------
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

    Call d100(RN)

    If RN <= 50 Then
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
    If ruleOptType <> 0 Then
        Call KICKOFF
    Else
        Call COLKICKOFF
    End If

    340 '
    Swap P1, D

    Call MARKER1

    X = 0
    U3 = 0

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    For I = 0 To 2
        If krNumRet(D, I) = 100 Then KN = I: Exit For
    Next

    ' We found a returner for the team
    ' Now let's set them up for the kickoff
    I2 = Int(100 * Rnd(1)) + 1

    For I = 2 To 0 Step -1
        If I2 <= krNumRet(D, I) Then I1 = I
    Next I

    Call d100(RN)

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

    If KN > 0 And RN <= 50 Then
        pbpString$ = kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) AND " + kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) ARE DEEP"
    Else
        pbpString$ = kickRet$(D, KN2) + " (" + Str$(Int(krYdsPerRet(D, KN2) * 3)) + " AVG) AND " + kickRet$(D, I1) + " (" + Str$(Int(krYdsPerRet(D, I1) * 3)) + " AVG) ARE DEEP"
    End If

    Call PBP

    If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

        I$ = "0"
        If gameClock! <= 180 And quarter = 4 And score(P1, 0) - score(D, 0) < 0 Then I$ = "2"

    Else

        pbpString$ = "0-REGULAR  1-SQUIBB  2-ONSIDE KICK " + gameTeams$(P1)
        Call PBP

        Do
            I$ = GetKeyPress$
        Loop Until Val(I$) >= 0 And Val(I$) <= 2

    End If

    skipReturnCatch% = 0
    skipReturnYds% = 0

    Select Case Val(I$)

        Case 0
            Call KICKSET
            'kickYL represents yard line where kick lands

            If kickYL <= -9.99 Then

                'Kicked out of endzone
                'Automatic touchback
                Call CHANGEPOSS
                Call Touchback
                skipReturnCatch% = 1
                skipReturnYds% = 1

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

                    'Computer is receiving? Return it unless kickYL < -2
                    If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                        If kickYL < -2 Then
                            I$ = "1"
                        Else
                            I$ = "2"
                        End If

                    Else

                        pbpString$ = "1-DOWN IT 2-RETURN IT"
                        Call PBP
                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = "1" Or I$ = "2"

                    End If

                    If Val(I$) = 1 Then

                        'Received in end zone,
                        'kneel for touchback
                        Call CHANGEPOSS
                        Call Touchback
                        skipReturnYds% = 1
                        skipReturnCatch% = 1

                    Else

                        skipReturnYds% = 1

                    End If

                End If

            End If

            If skipReturnCatch% <> 1 Then

                'Normal kick return - adjust position
                'within 0.5 yards
                kickYL = kickYL + (Int(Rnd(1) * 11) - 5) / 10

                ydLine = kickYL
                ydsToScore = 100 - ydLine

                Call MARKER
                Call YARDLINE

                pbpString$ = "KICK TAKEN BY " + kickRet$(P1, I1) + " AT THE" + Str$(Int(kickYL + .5)) + " YD LINE "
                Call PBP

            End If

            'Determine return yards based on returner's average
            If skipReturnYds% <> 1 Then

                RN = Int(Rnd(1) * 1000) + 1

                Select Case krYdsPerRet(P1, I1)

                    Case Is <= 8

                        Select Case RN
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
                        If RN <= 50 Then
                            'Kick return for TD - 5% chance
                            ydsGained = 110
                        Else
                            '15 to 33 yds
                            ydsGained = Int((Rnd(1) * 19) + 15)
                        End If

                    Case Is >= 10
                        If RN <= 80 Then
                            'Kick return for TD - 8% chance
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
                K2(P1, I1, 14) = K2(P1, I1, 14) + 1

                If ydsGained + kickYL > 89.99 Then

                    K2(P1, I1, 15) = K2(P1, I1, 15) + 100 - kickYL
                    K2(P1, I1, 16) = K2(P1, I1, 16) + 1
                    If 100 - kickYL > kickReturners(P1, I1) Then gameKR$(P1, I1) = "t": kickReturners(P1, I1) = 100 - kickYL

                    GoTo 3200

                End If

                If ydsGained + kickYL <= -.05 Then

                    Call CHANGEPOSS
                    Call Touchback

                Else

                    ydLine = ydsGained + kickYL

                    If ydsGained > kickReturners(P1, I1) Then
                        gameKR$(P1, I1) = ""
                        kickReturners(P1, I1) = ydsGained
                    End If

                    K2(P1, I1, 15) = K2(P1, I1, 15) + ydsGained

                    F% = 3

                    Call FUMBLE

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    Call AddPossessionTime(4, 7)

                    I = Int(Rnd(1) * 1000) + 1
                    If I >= 960 - tmRat_GAME(P1, 0) Then Call CLIPPING

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                End If

            End If

        Case 1
            'SQUIB KICK
            Call SQUIB
            Call FUMBLE

            If ballFumbled = 1 Then Call FumblePBP: GoTo 650

            Call AddPossessionTime(4, 7)
            Call d100(I)

            '3% chance of clipping on squib kick
            If I >= 97 Then
                Call CLIPPING
            End If

            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

        Case 2
            'Onside Kick
            Call ONSIDE

            currDown = 1
            Y1 = 10

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

        If ruleOptType = 1 And ruleOptPro <> 3 Then
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

                If ruleOptType = 1 And ruleOptPro = 3 Then
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
                Loop Until I$ = "1" Or I$ = "2"

                convChoice = Val(I$)

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

        'NFL, Post 1994
        If ruleOptType = 1 And ruleOptPro = 3 Then
            ydLine = 98
        Else
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
        Call d100(RN1)

        For J = 1 To 0 Step -1:
            'Auto-determine target kicker, I1
            'If RN1 <= kickerPATPct(P1, J) Then I1 = J
            If RN1 <= kickerPATPct(P1, J) Then Q7%(P1) = J

        Next J

        'Determine success
        Call d100(RN1)

        K2(P1, 6 + Q7%(P1) * 4, 17) = K2(P1, 6 + Q7%(P1) * 4, 17) + 1

        pbpString$ = kicker$(P1, Q7%(P1)) + " IS ON TO TRY THE EXTRA POINT"
        Call PBP

        If RN1 <= kickerIdx(P1, Q7%(P1)) Then

            K2(P1, 5 + Q7%(P1) * 4, 17) = K2(P1, 5 + Q7%(P1) * 4, 17) + 1

            pbpString$ = "THE KICK IS UP..."
            Call PBP
            Call MADEPAT

            'Increase score
            score(P1, 0) = score(P1, 0) + 1
            score(P1, quarter) = score(P1, quarter) + 1

            'Track the attempt for the kicker
            K1(nbrScores, 6) = Q7%(P1)
            K1(nbrScores, 4) = K1(nbrScores, 4) + 1

            Call DisplayField

        Else

            Call d100(RN2)

            '-- 9% chance of blocked PAT
            If RN2 <= 9 Then

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

    Call d100(I6)

    gameStats(P1, 9) = gameStats(P1, 9) + 1

    If I6 <= 50 Then

        Call PBPINT

        pbpString$ = pbpString$ + gameTeams$(D) + " RECOVERS!!"
        Call PBP

        currDown = 1
        Y1 = 10
        gameStats(P1, 10) = gameStats(P1, 10) + 1
        ydLine = 100 - ydLine
        Call CHANGEPOSS

        '659
        If ruleOptType <> 0 Or ruleOptColl >= 5 Then

            I = Int(Rnd(1) * 3) + 1

            If I > 2 Then

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
                    GoTo 3200
                Else
                    'returnYards < 99.99
                    pbpString$ = pbpString$ + Str$(Int((G1) * 10 + .5) / 10) + " YDS"
                    Call PBP
                    If returnYards <= .01 Then
                        GoTo 3250
                    End If

                    ydLine = ydLine + G1

                End If

            End If

        End If

        If ydLine >= 90 Then Y1 = 100 - ydLine
    Else
        'I6 > 50 Then
        pbpString$ = gameTeams$(P1) + " RECOVERS!!"
        Call PBP

        I = 50
        O = 1

        GoTo 3303

    End If

    O = 5

    GoSub 3900

    'College Overtime
    If NG%(14) = 3 And quarter > 4 Then 3364

    700 '
    Call MARKER1

    Call DISPWEATHER

    O = 0
    ballFumbled = 0
    ydsGained = 0
    W5 = 0

    If U3 = 9 Then U3 = 0: GoTo 340

    Call CheckGameClock
    If gameClock! = 0 Then GoTo 3360

    Do

        Call SCOREBOARD

        Call DEFMENU

        'Offense Play Selection
        validPlay% = 1
        autoPlaySelect% = 0

        Do

            'If computer is on Offense
            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

                Call ComputerOffense

                chosenPlay = P2

                'If a run play was not chosen, we must
                'shift chosenPlay because it should be a unique
                'value over 14
                If P2 > 8 Then chosenPlay = P2 + 6

                autoPlaySelect% = 1

            Else

                Do
                    3118 '
                    Call FRAMEOPTION(10, P1)
                    Print "SELECT OFFENSE "; gameTeams$(P1)

                    For P2 = 0 To 4:
                        Locate 12 + P2, 14
                        Print P2; "-"; playSelect$(P2); Tab(41); P2 + 5; "-"; playSelect$(P2 + 5)
                    Next P2

                    Locate , 15
                    Print "G -GADGET PLAY"

                    Do

                        I$ = GetKeyPress$

                        If I$ = "8" Then I$ = "T"

                        If UCase$(I$) = "Q" Then
                            'End
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
                                    Call FLEA

                                    If RN <= ffPctSuccess Then

                                        R1 = 1
                                        W5 = 1
                                        ydsGained = (Int(Rnd(1) * 6) + 10)

                                        'Sacked
                                        GoTo 3541

                                    Else

                                        Call d100(RN)

                                        If RN <= intChance Then

                                            ydsGained = CP
                                            FF% = 2
                                            'Intercepted!
                                            GoTo 3552

                                        Else

                                            Call d100(RN)
                                            targetWRName$ = wdRec$(P1, I3)
                                            I3 = I3 + 10

                                            If RN <= catchPctSuccess Then
                                                FF% = 2
                                                GoTo 3586
                                            Else
                                                ydsGained = 0
                                                GoTo 3535
                                            End If

                                        End If

                                    End If

                                Case 2
                                    ' *** REVERSE PLAY ***
                                    RV% = 1

                                    GoSub 3128

                                    Call QBLINE

                                    If KJ = 1 Then

                                        KJ = 0
                                        R1 = 1
                                        I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0)
                                        Call REVGAIN
                                        GoTo 3416

                                    Else

                                        3181 '
                                        'Random ball carrier
                                        Call d100(I2)

                                        For I = 9 To 0 Step -1:
                                            If I2 <= rbRushAtt(P1, I) Then I1 = I
                                        Next I

                                        R1 = 1

                                        If I1 = (V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3)) Then

                                            If rbNumRec(P1, I1) > 0 Then
                                                'Regular gain
                                                GoTo 3403

                                            Else

                                                For I5 = 0 To 9
                                                    'Check for QB as a runner
                                                    If rbacks$(P1, I5) = qbacks$(P1, qbNumber(P1)) Then
                                                        I1 = I5
                                                        GoTo 3403
                                                        'Regular gain
                                                    End If
                                                Next

                                                If RV% = 1 Then
                                                    'Reverse play?
                                                    GoTo 3181
                                                Else
                                                    GoTo 3401
                                                End If

                                            End If

                                        Else

                                            'Adjust yardage based on league rating and weather
                                            I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + WX%(4)

                                            If RV% = 1 Then
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
                                    Call d100(RN)

                                    ydsGained = Int(Rnd(1) * 71) + 30
                                    CP = Int(Rnd(1) * 11) + 30

                                    HB%(P9) = 1

                                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND HANDS OFF TO " + rbacks$(P1, I1)
                                    Call PBP

                                    pbpString$ = rbacks$(P1, I1) + " HAS THE BALL AND WANTS TO THROW!!!"
                                    Call PBP

                                    Call d100(RN)
                                    I3 = Int(Rnd(1) * 3)

                                    pbpString$ = "HE HAS " + wdRec$(P1, I3) + " OPEN DOWNFIELD"
                                    Call PBP

                                    If coverage = 10 Or coverage = 11 Or coverage = 8 And RN <= 35 Then
                                        ydsGained = (Int(Rnd(1) * 4) + 4)
                                        'Sacked
                                        GoTo 3541
                                    End If

                                    Call d100(RN)

                                    If RN <= intChance Then
                                        'Interception
                                        ydsGained = CP
                                        FF% = 1
                                        HB% = 0
                                        'Intercepted!
                                        GoTo 3552
                                    Else
                                        Call d100(RN1)
                                        targetWRName$ = wdRec$(P1, I3)
                                        I3 = I3 + 10

                                        If RN1 <= catchPctSuccess Then FF% = 1: GoTo 3586
                                        ydsGained = 0

                                        GoTo 3535
                                    End If

                            End Select

                        End If

                        If UCase$(I$) = "T" Then 850

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
                                Input P2
                            Loop Until P2 >= 0 And P2 <= 15

                            If P2 = 0 Then Screen , , 0, 0: GoTo 3118

                            chosenPlay = P2

                            'Here we are categorizing plays that are
                            'simply alternate names for the same thing

                            'Power Sweep / Quick Pitch = Toss Sweep
                            If P2 = 9 Or P2 = 10 Then P2 = 4

                            'Quick Trap = Trap / Misdirection
                            If P2 = 11 Then P2 = 7

                            'Isolation = Off Tackle
                            If P2 = 12 Then P2 = 3

                            'Sprint Draw = Draw
                            If P2 = 13 Then P2 = 6

                            'Over the Top = Trap / Misdirection
                            If P2 = 14 Then P2 = 7

                            'Counter = Dive
                            If P2 = 15 Then P2 = 2

                        Case 1

                            Call PASSMENU

                            Do
                                Locate 19, 14
                                Print "             SELECT PLAY ";
                                Input P2
                            Loop Until P2 >= 0 And P2 <= 14

                            Screen , , 0, 0

                            If P2 = 0 Then GoTo 3118

                            'we have 15 run plays, so chosenPlay needs
                            'a unique value up to 29
                            chosenPlay = P2 + 15

                            'Here we are categorizing plays that are
                            'simply alternate names for the same thing

                            'Cross / Curl = Medium Pass
                            If P2 = 11 Or P2 = 12 Then P2 = 3

                            'Deep Post / Deep Corner = Long Pass
                            If P2 = 13 Or P2 = 14 Then P2 = 4

                            'P2 needs a unique value as well
                            'that represents a chosen play,
                            'not just a number on the menu
                            'So here we account for the
                            'presence of 8 run play types
                            P2 = P2 + 8

                        Case Is >= 2
                            'P2 needs a unique value; values of XX from 3+
                            'will translate to 20+
                            P2 = XX + 17

                    End Select

                Loop Until P2 <> 19 Or ydLine < 70

                If P2 = 26 Then P2 = 24

                'P2 = 25, from above
                If XX = 8 Then

                    pbpString$ = gameTeams$(P1) + " IS LETTING THE PLAY CLOCK WIND DOWN"
                    Call PBP

                    pbpString$ = "PENALTY - " + gameTeams$(P1) + " - DELAY OF GAME"
                    Call PBP

                    O = 8
                    penaltyYds = 5
                    'Reset ball carrier
                    I2 = 0

                    GoTo 3326

                End If

                If XX = 7 Then
                    P2 = 9
                    R1 = 1
                    GoTo 3403
                End If

                If (quarter = 3 Or quarter = 1) And P2 = 16 Or gameClock! > 120 And P2 = 16 Then
                    pbpString$ = "CAN ONLY USE LAST 2 MIN. OF EACH HALF"
                    Call PBP
                    validPlay% = 0
                End If

            End If

        Loop Until validPlay% = 1

        If autoPlaySelect% = 0 Then

            If P2 = 8 Then
                Screen , , 0, 0
            End If

            If P2 >= 1 And P2 <= 8 Then

                3128 '
                ' !!! Beginning of a sub !!!
                ' but where does it end???
                KJ = 1
                Call FRAMEOPTION(11, P1)

                Print "SELECT BALL CARRIER":

                For KK = 0 To 4
                    Locate 12 + KK, 14
                    Print KK; "-"; rbacks$(P1, KK);
                    If gameStats(P1, 1) >= 20 And K2(P1, KK, 0) / gameStats(P1, 1) > PC%(P1, KK) / 100 + .1 Then Print " ###" Else Print
                    If rbRushAtt(P1, KK) = 100 Then GoTo CSELECT
                Next

                For KK = 0 To 4
                    Locate 12 + KK, 41
                    Print KK + 5; "-"; rbacks$(P1, KK + 5);
                    If gameStats(P1, 1) >= 20 And K2(P1, KK + 5, 0) / gameStats(P1, 1) > PC%(P1, KK + 5) / 100 + .1 Then Print " ###" Else Print
                    If rbRushAtt(P1, KK + 5) = 100 Then GoTo CSELECT
                Next KK

                CSELECT:
                Locate 17, 28: Print "C-COMPUTER SELECT"

                Do
                    I$ = GetKeyPress$

                    If UCase$(I$) = "C" Then
                        KJ = 0
                    Else
                        I1 = Val(I$)
                    End If

                Loop Until I1 >= 0 And I1 <= 9 Or KJ = 0
                'End If

                Screen , , 0, 0

            End If

        End If

        'If Computer is playing offense and a long Pass was chosen inside the Opp 15 --> Change to Medium Pass
        If ((playerMode = 1 And P1 = compTeam Or playerMode = 2) And (P2 = 12 Or P2 = 15) And ydLine >= 85) Then P2 = 11

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

    If playerMode = 1 And D = compTeam Then Call COMPCHEAT

    If qbTakeKnee = 1 Then qbTakeKnee = 0: GoTo 3403

    I = Int(Rnd(1) * 1000) + 1
    'I = Int(Rnd(1) * 100) + 1

    If I <= 36 And endZone <> 1 And P2 < 21 Then

        pbpString$ = "*** PENALTY - "
        I = Int(Rnd(1) * 36) + 1

        Select Case I

            Case 1 To 2 + tmRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFFENSE... OFFSIDES 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 2 + tmRat_GAME(P1, 0) To 20 + tmRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "ILLEGAL PROCEDURE OFFENSE... 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 20 + tmRat_GAME(P1, 0) To 27 + tmRat_GAME(P1, 0)
                pbpString$ = pbpString$ + "OFFENSE... DELAY OF GAME 5 YARDS ***"
                Call PBP
                penaltyYds = -5

            Case 27 + tmRat_GAME(P1, 0) To 36 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                pbpString$ = pbpString$ + "DEFENSE... ENCROACHMENT 5 YARDS ***"
                Call PBP
                penaltyYds = 5

        End Select

        O = 8

        If penaltyYds > 0 Then
            GoTo 3330
        Else
            penaltyYds = penaltyYds * -1
            GoTo 3326
        End If

    End If


    Select Case P2

        Case 1 To 8

            ' ****  RUNNING PLAYS  ****

            Call QBLINE

            If KJ = 1 Then

                KJ = 0
                R1 = 1

                3403 '
                'Average rush yards
                rushYds = rbRushAvg(P1, I1)

                'Adjust yardage based on DEF league ratings and weather
                I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + WX%(4)

                If gameStats(P1, 1) >= 20 And K2(P1, I1, 0) / gameStats(P1, 1) > PC%(P1, I1) / 100 + .1 Then I2 = Int(I2 * .4)

                RN = Int(Rnd(1) * 200) + 1

                '1/2 % chance of fumble outside the end zones
                If RN = 97 And ydLine <= 98.899 And ydLine >= 1.101 Then

                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " FUMBLES THE SNAP!"
                    Call PBP

                    ydsGained = Int((Rnd(1) * 20) - 10) / 10

                    X = 4
                    X1 = 9
                    I1 = qbNumber(P1)

                    If ydLine + ydsGained < 99.94999 Then
                        If ydsGained - Y1 >= -.3 And ydsGained - Y1 <= .3 Then Call MEASUREMENT
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
                    Y1 = Y1 - ydsGained
                    currDown = currDown + 1
                    If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                    GoTo 650

                End If

                'QB Sneak
                If P2 = 8 Then

                    Call QBSNEAK

                    If endZone = 1 Then Call TryForTwoResult

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then 3200
                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then 3250

                    Call FUMBLE

                    GoTo 3309

                End If

                If P2 = 9 Then
                    pbpString$ = qbacks$(P1, qbNumber(P1)) + " TAKES THE SNAP AND KNEELS DOWN FOR "
                    Call PBP
                    ydsGained = -1.0
                    GoTo 3546
                End If

                Call d100(RN0)

                'Option play = 1% cance of fumble?
                If P2 = 5 And RN0 = 49 Then

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
                        GoTo 3250

                    Else

                        If endZone = 1 Then Call TryForTwoResult

                        ballFumbled = 1

                        Call COMPILER

                        ydLine = ydLine + ydsGained
                        Y1 = Y1 - ydsGained
                        currDown = currDown + 1
                        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        GoTo 650

                    End If

                End If

                Call HANDOFF

                If dLine = 11 Then

                    Call GOALLINE

                Else

                    I2 = I2 + defYdAdj(P2, dLine)

                    S6 = 0

                    If gameStats(P1, 1) + gameStats(P1, 3) <> 0 Then
                        If (gameStats(P1, 1) / (gameStats(P1, 1) + gameStats(P1, 3))) > (leagRat_GAME(P1, 7) / 100) And K3(P1, 0) - (leagRat_GAME(D, 0) * .3) > 4.5 And (leagRat_GAME(D, 0) * .3) > 4.5 And leagRat_GAME(P1, 7) < 50 Then
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

                        If (P2 <= 2 And dLine = 5 Or P2 <= 2 And dLine = 6 Or P2 >= 6 And dLine = 5 Or P2 >= 6 And dLine = 6) Or (P2 > 2 And P2 < 5 And dLine = 7 Or P2 > 2 And P2 < 5 And dLine = 8) Then

                            ' *** STUNTS ***
                            Call d100(I4)

                            Select Case I2

                                Case Is <= 15
                                    Select Case I4
                                        Case 1 To 15: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 16 To 60: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 61 To 100: ydsGained = Int(Rnd(1) * (I2 * 2.13)) + 1
                                    End Select

                                Case 16, 17
                                    Select Case I4
                                        Case 1 To 10: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 11 To 50: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 51 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.73)) + 1
                                    End Select

                                Case 18, 19
                                    Select Case I4
                                        Case 1 To 10: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 11 To 40: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 41 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.5)) + 1
                                    End Select

                                Case 20, 21
                                    Select Case I4
                                        Case 1 To 5: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 6 To 30: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 31 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.3)) + 1
                                    End Select

                                Case Is >= 22
                                    Select Case I4
                                        Case 1 To 5: ydsGained = Int(Rnd(1) * 3) - 2
                                        Case 6 To 20: ydsGained = Int(Rnd(1) * 2) + 1
                                        Case 21 To 100: ydsGained = Int(Rnd(1) * (I2 * 1.2)) + 1
                                    End Select

                            End Select

                            GoTo 3416

                        End If

                        Call d100(I4)

                        If (P2 <= 2 And I4 <= 50 And coverage = 7) Or (P2 >= 6 And I4 <= 50 And coverage = 7) Or (P2 <= 2 And coverage = 9) Or (P2 >= 6 And coverage = 9) Then
                            Call BLITZ2
                        End If

                        If P2 >= 3 And P2 <= 5 Then
                            If coverage = 8 And I4 <= 50 Or coverage = 10 Then
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

                                Call d100(I4)

                                If I4 > 92 Then
                                    ' *** BREAKAWAY ***
                                    Call d100(I5)
                                    If I5 <= 50 Then
                                        ydsGained = Int(Rnd(1) * BY%(I2, 0)) + BY%(I2, 1)
                                    Else
                                        ydsGained = Int(Rnd(1) * BY%(I2, 3)) + BY%(I2, 4)
                                    End If

                                Else

                                    I5 = 0

                                    If P2 = 4 Or P2 = 5 Then I5 = 1

                                    For I6 = 0 To 14:
                                        If I4 <= RM%(I5, I6) Then ydsGained = I6 - 5: Exit For
                                    Next I6

                                    Call d100(I4)
                                    If I4 <= RN%(0, I2) Then ydsGained = ydsGained + RN%(1, I2)

                                End If

                            End If

                        End If 'Done checking div3$

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

                End If

                'Done checking / adjusting for position

                3416 '
                Call YARDAGE

                'Call PBP based on special scenarios
                '"Stuffed", Long Play or Run for TD
                If ydsGained <= 0 Then Call STUFFED
                If ydsGained > 10 And ydLine + ydsGained < 99.94999 Then Call LGPBP
                If ydLine + ydsGained >= 99.94999 Then Call RUNTDPBP

                startYds = 100 - ydLine
                endYds = startYds - ydsGained

                Call MOVEBALL

                If ydLine + ydsGained < 99.94999 Then

                    F% = 2

                    Call FUMBLE

                    If ballFumbled <> 1 Then
                        If ydsGained - Y1 >= -.3 And ydsGained - Y1 <= .3 Then Call MEASUREMENT
                    End If

                End If

                'Print / call PBP for actual yardage
                If ydsGained < 0 Then

                    pbpString$ = rbacks$(P1, I1) + " LOST"
                    Print
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

                If RV% > 0 Then RV% = 0

                If endZone = 1 Then ballFumbled = 0: Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then ballFumbled = 0: GoTo 3200
                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then ballFumbled = 0: GoTo 3250

                GoTo 3309

            Else

                'KJ = 0

                3401 '
                'Random ball carrier
                Call d100(I2)

                For I = 9 To 0 Step -1:
                    If I2 <= rbRushAtt(P1, I) Then I1 = I
                Next I

                R1 = 1

                If I1 = (V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3)) Then


                    If rbNumRec(P1, I1) > 0 Then
                        GoTo 3403
                    Else

                        For I5 = 0 To 9
                            'Adjust for QB as a runner
                            If rbacks$(P1, I5) = qbacks$(P1, qbNumber(P1)) Then I1 = I5: GoTo 3403
                        Next

                        If RV% = 1 Then
                            'Reverse play?
                            GoTo 3181
                        Else
                            'Look for new ball carrier
                            GoTo 3401
                        End If

                    End If

                Else

                    'Adjust rush yds based on league rating and weather
                    I2 = rbRushAvg(P1, I1) + leagRat_GAME(D, 0) + WX%(4)

                    If RV% = 1 Then
                        'Reverse play?
                        Call REVGAIN
                        GoTo 3416
                    Else
                        'Regular rush yardage gain
                        GoTo 3403
                    End If

                End If

            End If 'Done checking KJ

        Case 9 To 18

            Call PASSBALL

            'SACKED
            If I < S1%(3, P2 - 8, coverage) + leagRat_GAME(P1, 5) + leagRat_GAME(D, 6) + 1 Then

                R1 = 1
                W5 = 1

                ' *** SACKED ***
                If P2 = 12 Or P2 = 15 Then ydsGained = (Int(Rnd(1) * 9) + 5)
                If P2 = 10 Or P2 = 9 Or P2 = 13 Then ydsGained = (Int(Rnd(1) * 5) + 3)
                If P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18 Then ydsGained = (Int(Rnd(1) * 7) + 4)

                3541 '
                Call YARDAGE
                Call SACK

                'Yardage for sack is negative
                ydsGained = ydsGained * -1

                If defSacks(1 - P1) <> 0 Then

                    M5 = 0
                    RN = Int(Rnd(1) * teamSacks(1 - P1)) + 1

                    For Z1 = 0 To 14

                        M5 = M5 + playerSacks(1 - P1, Z1)

                        If RN <= M5 Then
                            pbpString$ = pbpString$ + " BY " + defSacks$(1 - P1, Z1)
                            Exit For
                        End If

                    Next

                End If

                '3699
                Call PBP

                X = 4
                R1 = 1

                If endZone = 1 Then Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then

                    GoTo 3250

                Else

                    F% = 1

                    Call FUMBLE

                End If

                GoTo 3309

            End If

            'FORCED TO RUN ON BOOTLEG/ROLLOUT/KEEPER
            rollout% = 0

            'When would mobility ever be 0???
            If qbMobility(P1) > 0 Then
                Call d100(I)
                If I <= qbMobility(P1) Then GoTo 3545
            End If

            If qbMobility(P1) > 0 Then GoTo 3532

            If (P2 = 9 Or P2 = 10 Or P2 = 13) And I <= 5 Then rollout% = 1
            If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17) And I <= 10 Then rollout% = 1
            If (P2 = 12 Or P2 = 15) And I <= 15 Then rollout% = 1
            If P2 = 18 And I <= 35 Then rollout% = 1

            If rollout% = 1 Then
                3545 '
                If qbMobility(P1) > 0 Then
                    'Yardage gain = 0-12, based on mobility
                    'R1 = 4/3 of rating, rounded up
                    'Gain is from 0 to R1
                    R1 = (Int((qbMobility(P1) * .67) + .5)) * 2
                    ydsGained = Int(Rnd(1) * R1)
                Else
                    'Mobility = 0
                    'Yardage gain = 1-12
                    ydsGained = Int(Rnd(1) * 12) + 1
                End If

                Call YARDAGE

                'QB Keeper
                If P2 = 17 Or P2 = 18 Then

                    'QB RUN
                    I6 = 0

                    For I4 = 0 To 9
                        If rbacks$(P1, I4) = qbacks$(P1, qbNumber(P1)) Then
                            I6 = rbRushAvg(P1, I4)
                            Exit For
                        End If
                    Next I4

                    I7 = Int(Rnd(1) * 10) + 1

                    If I6 = 0 Then
                        If I7 <= 9 Then
                            ydsGained = Int(Rnd(1) * 5)
                        Else
                            ydsGained = Int(Rnd(1) * 16)
                        End If
                    Else
                        If I7 <= 9 Then
                            ydsGained = Int(Rnd(1) * 7)
                        Else
                            ydsGained = Int(Rnd(1) * (12 + ((I6 - 13) * 3) * 2))
                        End If
                    End If

                    Call YARDAGE

                    startYds = 100 - ydLine
                    endYds = startYds - ydsGained

                    Call MOVEBALL

                    If P2 = 17 Then
                        pbpString$ = qbacks$(P1, I1) + " KEEPS IT ON THE ROLLOUT AND RUNS FOR"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " KEEPS IT ON THE BOOTLEG AND RUNS FOR"
                    End If

                Else
                    pbpString$ = qbacks$(P1, I1) + " IS CHASED FROM THE POCKET"
                    Call PBP
                    pbpString$ = "HE IS FORCED TO RUN FOR"
                End If

                3546 '
                X = 4
                X1 = 9
                R1 = 1

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
                Else
                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
                End If

                pbpString$ = pbpString$ + " YARDS"

                Call PBP

                If endZone = 1 Then Call TryForTwoResult

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then gameStats(P1, 0) = gameStats(P1, 0) + 1: GoTo 3200

                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then 3250

                F% = 2

                Call FUMBLE

                GoTo 3309
            Else

                3532 '
                'Determine pass outcome
                Call d100(I)

                'Compare value to expected interception %
                '               QB's Int % - Pass type adjustment + league DEF Int %
                intChance = qbPctInt(P1, I1) - S1%(0, P2 - 8, coverage) + leagRat_GAME(D, 2)

                '                If I > (100 - qbPctInt(P1, I1) - S1%(0, P2 - 8, coverage) + leagRat_GAME(D, 2)) Then
                If I > (100 - intChance) Then

                    'Intercepted!
                    GoTo 3552

                Else

                    'Determine % chance of completion
                    Call AdjustCompPct

                    If I <= expCompPct + passTypeCmpAdj + ballPosCmpAdj + windCmpAdj Then
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
                        If WX%(4) = -3 Then I4 = I4 - 1
                        If WX%(4) = -6 Then I4 = I4 - 2

                        If I4 < 11 Then
                            'Determine impact of blitz
                            Call BLITZ

                        Else

                            I4 = I4 + S1%(2, P2 - 8, coverage)

                            If ydsPerComp(D) / 100 > .5 Then I4 = I4 - (1 - ydsPerComp(D) / 100) * 10
                            If ydsPerComp(D) / 100 < .5 Then I4 = I4 + ydsPerComp(D) / 100 * 10

                            'Check if the play is overused
                            Call OVERUSE

                            If P2 = 9 Or P2 = 10 Or P2 = 13 Then
                                I4 = I4 * ((((expCompPct + S6) * K3(P1, 3)) / (expCompPct + S6 + 18)) / K3(P1, 3))
                            End If

                            If P2 = 12 Or P2 = 15 Then
                                I4 = I4 * ((((expCompPct + S6) * K3(P1, 3)) / (expCompPct + S6 - 18)) / K3(P1, 3))
                            End If

                            'At this point I'm lost but P2 is the pass play type
                            I = Int(Rnd(1) * 25) + 1

                            If P2 <> 12 And P2 <> 15 And I = 25 Or (P2 = 12 Or P2 = 15) And I = 25 And ruleOptType = 0 Or (P2 = 12 Or P2 = 15) And ruleOptType <> 0 And I >= 24 Then
                                'Longer gains
                                If I = 25 And Int(Rnd(1) * 32) = 11 And score(P1, 0) > 13 And XD(D) > .6 Then
                                    ydsGained = 102
                                Else
                                    ydsGained = Int(Rnd(1) * I4 * 12) + 1 + (I4 * 6)
                                End If
                            Else
                                If I <= 19 Then
                                    ydsGained = Int(Rnd(1) * 13) + 1 + ((I4 * 2.3) - 7.2)
                                Else
                                    ydsGained = (Int(Rnd(1) * (5 + (I4 * 2))) + 1) + (3 * (I4 - 1))
                                End If
                            End If

                        End If

                        3586 '
                        'ADDED AFTER REPLAYS OF 5-24-93 <-- ???
                        'Random # 1 - 3 = 1 And Pro Game = adjust by 1
                        If Int(Rnd(1) * 3) + 1 = 1 And ruleOptType <> 0 Then ydsGained = ydsGained - 1

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
                                If ydsGained - Y1 >= -.3 And ydsGained - Y1 <= .3 Then Call MEASUREMENT
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

                                If ydsGained > WR(P1, I3) Then gameWR$(P1, I3) = "": WR(P1, I3) = ydsGained
                                If ydsGained > quarterbacks(P1, qbNumber(P1)) Then gameQB$(P1, qbNumber(P1)) = "": quarterbacks(P1, qbNumber(P1)) = ydsGained
                            End If

                        End If

                        Call PBP

                        If endZone = 1 Then ballFumbled = 0: Call TryForTwoResult

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then
                            ballFumbled = 0
                            GoTo 3250
                        End If

                        If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                            ballFumbled = 0
                            gameStats(P1, 0) = gameStats(P1, 0) + 1
                            GoTo 3200
                        End If

                        If P2 = 16 Then

                            I = Int(Rnd(1) * 10) + 1

                            If I <= 7 Then

                                O = 5

                                RN = Int(Rnd(1) * 5) + 1
                                Select Case RN
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

            Call d100(I3)

            For J = 2 To 0 Step -1
                If I3 <= prNumRet(D, J) Then I1 = J
            Next J

            puntRetNbr = I1

            pbpString$ = pret$(D, puntRetNbr) + " (" + Str$(Int(prYdsPerRet(D, puntRetNbr) * 3)) + " AVG) IS DEEP TO RECEIVE"
            Call PBP

            If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then
                I1 = 0
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
                        I1 = Val(I$)
                    End If

                Loop Until I1 >= 0 And I1 <= 5 Or Not (ydLine > 20 And I1 = 4) Or Not (currDown = 4 And I1 = 5) Or Not (currDown < 4 And I1 < 5)

                Screen , , 0, 0

                If I1 = 5 Then

                    Call d100(I)
                    X = 0
                    ydsGained = puntYdsPerP(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * puntYdsPerP(P1, 0)))) + 11

                    If WX%(0) = P1 Then
                        ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - WX%(1)
                    Else
                        ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + WX%(1)
                    End If

                    Call d100(RN)

                    If RN <= 50 Then
                        pbpString$ = gameTeams$(P1) + " IS QUICK KICKING!!"
                    Else
                        pbpString$ = gameMascots$(P1) + " ARE QUICK KICKING!!"
                    End If

                    Call PBP

                    If (coverage > 6 And I >= 60 And I <= 72) Or (coverage <= 6 And I >= 50 And I <= 56) Then 3840

                    pbpString$ = "THE KICK IS UP AND ON ITS WAY"
                    Call PBP

                    ydsGained = ydsGained + 2

                    Call YARDAGE

                    pbpString$ = "KICK BY " + gameTeams$(P1) + " TRAVELS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                    Call PBP

                    gameStats(P1, 7) = gameStats(P1, 7) + 1

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then

                        gameStats(P1, 8) = gameStats(P1, 8) + 100 - ydLine

                        If 100 - ydLine > gameStats(P1, 26) Then gameStats(P1, 26) = 100 - ydLine

                        gameClock! = gameClock! - 5
                        timePoss(P1) = timePoss(P1) + 5
                        gameStats(P1, 27) = gameStats(P1, 27) + 1

                        Call Touchback

                        GoTo 700
                    End If

                    gameStats(P1, 8) = gameStats(P1, 8) + ydsGained

                    If ydsGained + ydLine > 80 Then gameStats(P1, 29) = gameStats(P1, 29) + 1
                    If ydsGained > gameStats(P1, 26) Then gameStats(P1, 26) = ydsGained

                    ydLine = ydLine + ydsGained
                    ydsToScore = ydLine
                    Call YARDLINE
                    ydLine = 100 - ydLine
                    Call AddPossessionTime(4, 7)

                    RN = Int(Rnd(1) * 48) + 1

                    If RN <= 11 Or I1 = 1 Then
                        pbpString$ = "THE BALL GOES OUT OF BOUNDS AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                        Call PBP
                    Else
                        If RN <= 20 Or ydLineTeam <= 15 Then
                            pbpString$ = "THE BALL IS DOWNED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                            Call PBP
                        Else
                            If currDown = 4 Then
                                pbpString$ = "A FAIR CATCH IS CALLED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                                Call PBP
                            End If
                        End If
                    End If

                    Call CHANGEPOSS

                    currDown = 1
                    Y1 = 10

                    Call CheckGameClock
                    If gameClock! = 0 Then GoTo 3360

                    Call DELAY

                    GoTo 700

                End If

            End If

            X = 0
            ydsGained = puntYdsPerP(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * puntYdsPerP(P1, 0)))) + 3

            If 100 - ydLine <= ((puntYdsPerP(P1, 0) * 2) + 28) Then ydsGained = ydsGained + 100 - ydLine - (31 + puntYdsPerP(P1, 0) * 2)

            If WX%(0) = P1 Then
                ydsGained = ydsGained - Int(Rnd(1) * 7) + 3 - WX%(1)
            Else
                ydsGained = ydsGained + Int(Rnd(1) * 7) - 3 + WX%(1)
            End If

            Call YARDAGE

            Call d100(RN)

            If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                If RN <= 10 Then
                    choice = 1
                Else
                    choice = 0
                End If

            Else

                Call FRAMEOPTION(9, D)
                Print "THE PUNTING TEAM FOR "; gameTeams$(P1); " IS ON THE FIELD"
                Locate 12, 14
                Locate , 14: Print "0 - SET UP PUNT RETURN"
                Locate , 14: Print "1 - GO FOR THE BLOCK"
                Locate , 14: Print "2 - CALL PUNT-SAFE"
                Print
                Locate , 14: Print "SELECT OPTION "; gameTeams$(D); " COACH"

                Do
                    I$ = GetKeyPress$
                    choice = Val(I$)
                Loop Until choice >= 0 And choice <= 2

                Screen , , 0, 0

            End If

            'PUNTSNAP
            Select Case I1

                Case 2, 3

                    ' *** FAKE PUNT - RUN/PASS ***
                    If choice = 2 Then
                        pbpString$ = gameTeams$(D) + " IS IN PUNT SAFE SET...FAKE PUNT? (Y/N)"
                        Call PBP

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    End If

                    If UCase$(I$) = "Y" Then

                        If I1 = 3 Then
                            ' *** FAKE W/PASS ***
                            ydsGained = 0
                            pbpString$ = "IT'S A FAKE!!! " + punter$(P1, 0) + " IS LOOKING TO PASS"
                            Call PBP

                            Call d100(RN)

                            If choice = 0 Then

                                Select Case RN
                                    Case 1 To 36:
                                        ydsGained = Int(Rnd(1) * 27)

                                    Case 37 To 90:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP:
                                        O = 4
                                        GoTo 3309

                                    Case 91 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876
                                End Select

                            End If

                            If choice = 1 Then

                                Select Case RN
                                    Case 1 To 26:
                                        ydsGained = Int(Rnd(1) * 20)

                                    Case 27 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
                                        GoTo 3309

                                    Case 81 To 90:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        GoTo 3876

                                    Case 91 To 100: ydsGained = 100 - ydLine
                                End Select

                            End If

                            If choice = 2 Then

                                Select Case RN

                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 7) + 6

                                    Case 11 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
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

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then 3250
                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then 3200

                            F% = 2
                            Call FUMBLE

                        Else

                            Call FAKEYARDS(choice)

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then 3200
                            If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then 3250

                            F% = 2
                            Call FUMBLE

                        End If

                        GoTo 3309

                    End If 'Done processing fake punt

                Case 4
                    Call TAKESAFETY
                    GoTo 3251

            End Select

            PSNAP:
            pbpString$ = punter$(P1, 0) + " WAITS FOR THE SNAP"
            Call PBP

            I5 = Int(Rnd(1) * 300) + 1

            If I5 = 12 Then

                pbpString$ = "THERE'S A BAD SNAP FROM CENTER!!"
                Call PBP

                ydsGained = (Int(Rnd(1) * 14) + 12) * -1
                Call AddPossessionTime(4, 7)

                gameStats(P1, 10) = gameStats(P1, 10) + 1
                gameStats(P1, 9) = gameStats(P1, 9) + 1

                If ydLine + ydsGained < -9.999 Then
                    score(D, quarter) = score(D, quarter) + 2
                    score(D, 0) = score(D, 0) + 2
                    GoTo 3251
                End If

                If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

                    Call d100(RN0)

                    If RN0 <= 50 Then
                        score(D, quarter) = score(D, quarter) + 2
                        score(D, 0) = score(D, 0) + 2
                        GoTo 3251
                    Else
                        X1 = 10
                        Call CHANGEPOSS
                        GoTo 3200
                    End If

                End If

                Call CHANGEPOSS
                currDown = 1
                Y1 = 10
                ydLine = 100 - ydLine

                If Y1 >= 90 Then Y1 = 100 - ydLine

                Call d100(RN)

                If RN <= 50 Then
                    pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
                Else
                    pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
                End If

                If ydLine > 50 Then
                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
                Else
                    pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
                End If

                O = 5

                GoSub 3900

            Else

                Call d100(I5)

                If choice = 1 Then pbpString$ = "THERE'S A HEAVY RUSH!!!": Call PBP

                If choice = 1 And I5 >= 72 And I5 <= 76 Then 3840

                If choice = 1 And I5 >= 13 And I5 <= 17 Then

                    pbpString$ = "THERE'S A FLAG ON THE PLAY!!"

                    Call PBP

                    pbpString$ = "ROUGHING THE KICKER !!!...AUTOMATIC FIRST DOWN !!!"

                    Call PBP

                    gameStats(D, 11) = gameStats(D, 11) + 1
                    gameStats(D, 12) = gameStats(D, 12) + 15
                    ydLine = ydLine + 15
                    currDown = 1
                    Y1 = 10
                    gameStats(P1, 0) = gameStats(P1, 0) + 1
                    O = 4

                    GoSub 3900

                    Call DELAY

                    GoTo 700
                End If

                pbpString$ = punter$(P1, 0) + " TAKES THE SNAP"
                Call PBP

                Call d100(RN)

                If I1 = 1 Then

                    If RN <= 10 Then
                        ydsGained = ydsGained - (Int(Rnd(1) * 11) + 25)
                        pbpString$ = "HE SHANKS IT!"
                    Else
                        ydsGained = ydsGained - (Int(Rnd(1) * 9) + 4)
                        pbpString$ = "HE KICKS IT TOWARDS THE CORNER"
                    End If

                Else

                    If ydsGained < 40 Then
                        pbpString$ = "HE GETS THE PUNT AWAY"
                    Else
                        Call PUNTPBP
                    End If

                End If

                Call PBP

                pbpString$ = "PUNT BY " + punter$(P1, 0) + " TRAVELS" + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                Call PBP

                gameStats(P1, 7) = gameStats(P1, 7) + 1

                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then

                    'Track punt yards
                    gameStats(P1, 8) = gameStats(P1, 8) + 100 - ydLine

                    'Update clock;
                    '-- Punts always take 5 seconds???
                    gameClock! = gameClock! - 5
                    timePoss(P1) = timePoss(P1) + 5

                    'Track touchback
                    gameStats(P1, 27) = gameStats(P1, 27) + 1

                    Call Touchback

                Else

                    'Track punt yards
                    gameStats(P1, 8) = gameStats(P1, 8) + ydsGained

                    Call d100(I)

                    If ydsGained + ydLine > 80 Then gameStats(P1, 29) = gameStats(P1, 29) + 1

                    'Determine if longest punt and track, if so
                    If ydsGained > gameStats(P1, 26) Then gameStats(P1, 26) = ydsGained

                    Call d100(RN0)

                    '  Punt inside the 10 and
                    '  Fake Punt
                    '  40% Chance overall
                    '  Normal Punt, 80% Chance
                    If ydsGained + ydLine >= 90 And I >= 5 Or choice = 2 Or I > 60 Or I1 = 1 And RN0 <= 80 Then

                        ydLine = ydLine + ydsGained
                        ydsToScore = ydLine

                        Call YARDLINE

                        ydLine = 100 - ydLine

                        Call AddPossessionTime(4, 7)

                        RN = Int(Rnd(1) * 48) + 1

                        If RN <= 11 Or I1 = 1 Then
                            pbpString$ = "THE BALL GOES OUT OF BOUNDS AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                        Else
                            If RN <= 20 Or ydLineTeam <= 15 Then
                                pbpString$ = "THE BALL IS DOWNED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                            Else
                                If currDown = 4 Then
                                    pbpString$ = "A FAIR CATCH IS CALLED AT THE" + Str$(Int(ydLineTeam)) + " YARD LINE"
                                End If
                            End If
                        End If

                        Call PBP

                        Call CHANGEPOSS

                        currDown = 1
                        Y1 = 10

                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Call DELAY

                    Else

                        Call PUNTYARDS

                        If ydsGained + ydLine >= 99.99 Then
                            'Track Punt Return
                            K2(P1, puntRetNbr + 3, 14) = K2(P1, puntRetNbr + 3, 14) + 1
                            'Track Return Yards
                            K2(P1, puntRetNbr + 3, 15) = K2(P1, puntRetNbr + 3, 15) + 100 - ydLine
                            'Track Return TD
                            K2(P1, puntRetNbr + 3, 16) = K2(P1, puntRetNbr + 3, 16) + 1

                            BG = 100 - ydLine
                            If BG > puntReturners(P1, puntRetNbr) Then gamePR$(P1, puntRetNbr) = "t": puntReturners(P1, puntRetNbr) = BG

                            I1 = puntRetNbr
                            GoTo 3200

                        End If

                        K2(P1, puntRetNbr + 3, 14) = K2(P1, puntRetNbr + 3, 14) + 1
                        K2(P1, puntRetNbr + 3, 15) = K2(P1, puntRetNbr + 3, 15) + ydsGained

                        If ydsGained > puntReturners(P1, puntRetNbr) Then gamePR$(P1, puntRetNbr) = "": puntReturners(P1, puntRetNbr) = ydsGained

                        If ydsGained + ydLine <= .01 Then 3250

                        currDown = 1
                        ydLine = ydLine + ydsGained
                        Y1 = 10

                        If ydLine >= 89.99 Then Y1 = 100 - ydLine

                        I = Int(Rnd(1) * 1000) + 1

                        If I >= 960 - tmRat_GAME(P1, 0) Then
                            pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
                            Call PBP

                            If ydLine < 20 Then ydLine = ydLine / 2: gameStats(P1, 12) = gameStats(P1, 12) + ydLine / 2

                            gameStats(P1, 11) = gameStats(P1, 11) + 1

                            If ydLine >= 20 Then ydLine = ydLine - 10: gameStats(P1, 12) = gameStats(P1, 12) + 10

                        Else

                            F% = 4

                            Call FUMBLE

                            If ballFumbled = 1 Then Call FumblePBP: GoTo 650
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

            Call d100(I)

            'Was previously I2 not J
            For J = 1 To 0 Step -1
                If I <= kickerFGPct(P1, J) Then I1 = J
            Next J

            Q7%(P1) = I1

            'FG range is considered up to 62 yards
            '--- this should be changed, but not
            '--- without considering how the
            '--- distance impacts accuracy
            If ydLine < 52 + goalPostAdj Then

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

                Call d100(RN)

                If playerMode = 1 And D = compTeam Or playerMode = 2 Then

                    choice = 1

                Else

                    Call FRAMEOPTION(9, D)

                    Print "THE FIELD GOAL UNIT FOR "; gameTeams$(P1); " IS ON THE FIELD"
                    Locate 12, 14
                    Locate , 14: Print "1 - GO FOR THE BLOCK"
                    Locate , 14: Print "2 - CALL FIELD GOAL-SAFE"
                    Print
                    Locate , 14: Print "SELECT OPTION "; gameTeams$(D); " COACH"

                    Do
                        I$ = GetKeyPress$
                        choice = Val(I$)
                    Loop Until choice >= 1 And choice <= 2

                End If

                Screen , , 0, 0

                'Fake Field Goal Was Chosen
                If I1 = 2 Or I1 = 1 Then

                    ' *** FAKE PUNT - RUN/PASS ***
                    If choice = 2 Then

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
                        Call d100(RN)

                        passIncomplete% = 0
                        passIntercepted% = 0

                        Select Case I2

                            Case 1
                                Select Case RN
                                    Case 1 To 20:
                                        ydsGained = Int(Rnd(1) * 20)
                                    Case 21 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
                                        passIncomplete% = 1
                                    Case 81 To 90:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted% = 1
                                    Case 91 To 100:
                                        ydsGained = 100 - ydLine
                                End Select

                            Case 2
                                Select Case RN
                                    Case 1 To 10:
                                        ydsGained = Int(Rnd(1) * 7) + 6
                                    Case 11 To 80:
                                        pbpString$ = "THE PASS FALLS INCOMPLETE"
                                        Call PBP
                                        O = 4
                                        passIncomplete% = 1
                                        GoTo 3309
                                    Case 81 To 100:
                                        pbpString$ = "THE PASS IS INTERCEPTED!"
                                        Call PBP
                                        passIntercepted% = 1
                                End Select

                        End Select

                        If passIntercepted% = 1 Then

                            X = 0
                            'Track Pass Att, QB Int
                            gameStats(P1, 3) = gameStats(P1, 3) + 1
                            gameStats(P1, 5) = gameStats(P1, 5) + 1
                            K2(P1, I1, 9) = K2(P1, I1, 9) + 1
                            K2(P1, I1, 11) = K2(P1, I1, 11) + 1

                            Call AddPossessionTime(5, 8)

                            ydsGained = Int(Rnd(1) * 10) + 1

                            Call YARDAGE

                            If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                pbpString$ = pbpString$ + " IN THE END ZONE"
                                Call PBP
                                Call Touchback
                            Else
                                If ydsGained + ydLine > 49.99 Then pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE": Call PBP
                                If ydsGained + ydLine < 49.99 Then pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE": Call PBP

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
                                    GoTo 3200
                                Else
                                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                    Call PBP
                                    O = 5
                                    ydLine = ydLine + ydsGained
                                    Y1 = 10
                                    If ydLine >= 90 Then Y1 = 100 - ydLine

                                    GoSub 3900

                                    Call DELAY
                                End If

                            End If

                            GoTo 700

                        Else

                            If passIncomplete% <> 1 Then

                                pbpString$ = "PASS IS COMPLETE FOR"

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
                                    pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARDS"
                                Else
                                    pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
                                End If

                                Call PBP

                                If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then 3250
                                If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then 3200

                                F% = 2

                                Call FUMBLE

                            End If

                            GoTo 3309

                        End If

                    End If

                    Call FAKERUN(choice)

                    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then 3200
                    If Int((ydsGained + ydLine) * 10 + .5) / 10 < .05 Then 3250

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
            I3 = P1
            Call CHANGEQB

            GoTo 700

        Case 23
            Call BOXOPTIONS(0)
            Call TICKER

        Case 24
            I3 = P1
            Call CHOOSEKICKER

            GoTo 700

    End Select

    '----------

    3535 ' PASS NOT COMPLETE
    'Incomplete / Interference Penalty
    X = 2
    I = Int(Rnd(1) * 1000) + 1

    If I <= 50 + tmRat_GAME(D, 0) And P2 <> 13 And endZone = 0 Then

        pbpString$ = "DEFENSIVE PASS INTERFERENCE"
        If P2 = 9 Or P2 = 10 Or P2 = 13 Then ydsGained = Int(Rnd(1) * 10) + 1
        If P2 = 12 Or P2 = 15 Then ydsGained = Int(Rnd(1) * 20) + 21
        If P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18 Then ydsGained = Int(Rnd(1) * 10) + 11

        If ruleOptType = 0 And ruleOptColl >= 3 And ydsGained > 15 Then ydsGained = 15

        gameStats(D, 11) = gameStats(D, 11) + 1

        Call YARDAGE

        If ydsGained + ydLine >= 99.99 Then

            If ruleOptType <> 0 Then
                gameStats(D, 12) = gameStats(D, 12) + 100 - ydLine - 1
                pbpString$ = pbpString$ + " IN THE END ZONE"
                Call PBP

                ydLine = 99
                Y1 = 1
            Else
                gameStats(D, 12) = gameStats(D, 12) + 100 - ydLine - 2

                pbpString$ = pbpString$ + " IN THE END ZONE"
                Call PBP

                ydLine = 98
                Y1 = 2
            End If

        Else

            gameStats(D, 12) = gameStats(D, 12) + ydsGained
            pbpString$ = pbpString$ + " AT THE "

            If ydsGained + ydLine < 50 Then
                pbpString$ = pbpString$ + Str$(Int((ydsGained + ydLine) * 10 + .5) / 10)
            Else
                pbpString$ = pbpString$ + Str$(Int((100 - ydLine - ydsGained) * 10 + .5) / 10)
            End If

            pbpString$ = pbpString$ + " YARD LINE"
            Call PBP

            ydLine = ydLine + ydsGained
            Y1 = 10

            If ydLine >= 90 Then Y1 = 100 - ydLine

        End If

        currDown = 1
        gameStats(P1, 0) = gameStats(P1, 0) + 1
        O = 4

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

        GoTo 700

    Else

        Call INCOMPLETE
        If endZone = 1 Then Call TryForTwoResult
        O = 4

    End If

    GoTo 3309


    '----- NOODLE -----

    3200 '
    Randomize Timer

    Call d100(RN)

    'So essentially we have a 50/50 chance
    'of seeing the team name or the mascot name
    'IE, TOUCHDOWN! 89 SAN FRANCISCO
    '    TOUCHDOWN! 49ERS
    If RN <= 50 Then
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

    ydLine = 40
    Y1 = 10
    currDown = 1
    Call SCOREBOARD

    'Pro Overtime
    If ruleOptType <> 0 And quarter > 4 Then 3364

    GoSub PointsAfterTD

    ydLine = 35
    ydsToScore = 65
    Y1 = 10

    Call CHANGEPOSS

    O = 4
    U3 = 9

    GoSub 3900

    U3 = 0

    Call SCOREBOARD

    If ruleOptType <> 0 And goalPostAdj < 0 Then
        ydLine = 40
        ydsToScore = 60
    End If

    If ruleOptType = 1 And ruleOptPro = 3 Or ruleOptType = 4 Then
        ydLine = 30
        ydsToScore = 70
    End If

    If ruleOptType = 0 And ruleOptColl = 0 Then
        ydLine = 40
        ydsToScore = 60
    End If

    Call SCOREBOARD

    If R1 > 0 And R1 < 3 And 100 - ydLine > 10 And ruleOptType <> 0 Then gameStats(D, 0) = gameStats(D, 0) + 1

    'Overtime
    If quarter > 4 Then 3364

    GoTo 340

    '----- NOODLE -----

    3250 '
    ' *** SAFETY ***
    score(D, quarter) = score(D, quarter) + 2
    score(D, 0) = score(D, 0) + 2
    gameStats(D, 19) = gameStats(D, 19) + 1

    Call COMPILER

    3251 '
    ydLine = 20
    Y1 = 10
    currDown = 1
    O = 5
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
    If Int(Y1 * 10 + .5) / 10 < .05 Then

        3335 '
        currDown = 1
        Y1 = 10

        If 100 - ydLine < 9.95 Then Y1 = 100 - ydLine
        If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

        gameStats(P1, 0) = gameStats(P1, 0) + 1

        GoSub 3900

        If gameClock! <= 0 Then gameClock! = 1

        GoTo 700

    End If

    If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1
    If currDown = 4 Then thirdDownFail(P1) = thirdDownFail(P1) + 1
    If currDown > 4 Then 3339

    I = Int(Rnd(1) * 1000) + 1

    If I <= 6 + tmRat_GAME(D, 0) Then

        pbpString$ = "PENALTY AFTER WHISTLE - "
        I = Int(Rnd(1) * 10) + 1

        Select Case I
            Case 1
                pbpString$ = pbpString$ + "OFF PERSONAL FOUL - 15 YARDS"
                Call PBP

                gameStats(P1, 11) = gameStats(P1, 11) + 1

                If ydLine < 30 Then
                    Y1 = Y1 + ydLine / 2
                    ydLine = ydLine - ydLine / 2
                    gameStats(P1, 12) = gameStats(P1, 12) + ydLine / 2
                Else
                    Y1 = Y1 + 15
                    ydLine = ydLine - 15
                    gameStats(P1, 12) = gameStats(P1, 12) + 15
                End If

            Case 2 To 6
                pbpString$ = pbpString$ + "DEF PERSONAL FOUL - 15 YARDS"
                Call PBP

                gameStats(D, 11) = gameStats(D, 11) + 1

                If 100 - ydLine < 30 Then
                    Y1 = Y1 - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStats(D, 12) = gameStats(D, 12) + (100 - ydLine) / 2
                Else
                    Y1 = Y1 - 15
                    ydLine = ydLine + 15
                    gameStats(D, 12) = gameStats(D, 12) + 15
                End If

                If gameClock! <= 0 Then gameClock! = 1

            Case 7 To 9
                pbpString$ = pbpString$ + "DEF INCIDENTAL FACEMASK - 5 YARDS"
                Call PBP

                gameStats(D, 11) = gameStats(D, 11) + 1

                If 100 - ydLine < 10 Then
                    Y1 = Y1 - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStats(D, 12) = gameStats(D, 12) + (100 - ydLine) / 2
                Else
                    Y1 = Y1 - 5
                    ydLine = ydLine + 5
                    gameStats(D, 12) = gameStats(D, 12) + 5
                End If

                If gameClock! <= 0 Then gameClock! = 1

            Case 10
                pbpString$ = pbpString$ + "DEF FLAGRANT FACEMASK - 15 YARDS 1ST DOWN!"
                Call PBP

                gameStats(D, 11) = gameStats(D, 11) + 1

                If 100 - ydLine < 30 Then
                    Y1 = Y1 - (100 - ydLine) / 2
                    ydLine = ydLine + (100 - ydLine) / 2
                    gameStats(D, 12) = gameStats(D, 12) + (100 - ydLine) / 2
                Else
                    Y1 = Y1 - 15
                    ydLine = ydLine + 15
                    gameStats(D, 12) = gameStats(D, 12) + 15
                End If

                If gameClock! <= 0 Then gameClock! = 1

        End Select

    Else

        GoSub 3900

        GoTo 700

    End If

    '----- NOODLE -----

    3309 '
    pbpString$ = "*** PENALTY - "

    I4 = Int(Rnd(1) * 1000) + 1

    Select Case R1

        Case 1
            'Run Play ???
            Select Case I4

                Case 1 To 7 + tmRat_GAME(P1, 0)
                    pbpString$ = pbpString$ + "OFFENSE... ILLEGAL MOTION 5 YARDS ***"
                    penaltyYds = -5

                Case 7 + tmRat_GAME(P1, 0) To 25 + tmRat_GAME(P1, 0)
                    'Pro Rules before 1974 | College Rules before 1984
                    If (ruleOptType <> 0 And ruleOptPro = 1 Or ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF... HOLDING 15 YARDS ***"
                        penaltyYds = -15
                    Else
                        pbpString$ = pbpString$ + "OFF... HOLDING 10 YARDS ***"
                        penaltyYds = -10
                    End If

                    HL% = 1

                Case 25 + tmRat_GAME(P1, 0) To 42 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    penaltyYds = 5

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    Y1 = Y1 - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

        Case 2
            'Pass Play?

            Select Case I4

                Case 1 To 17 + tmRat_GAME(P1, 0)

                    'Pro Rules before 1978 | College Rules before 1984
                    If (ruleOptType <> 0 And ruleOptPro <> 0 And ruleOptPro <> 3 Or ruleOptType = 0 And ruleOptColl < 3) Then
                        pbpString$ = pbpString$ + "OFF... HOLDING 15 YARDS ***"
                        penaltyYds = -15
                        HL% = 1
                    Else
                        pbpString$ = pbpString$ + "OFF... HOLDING 10 YARDS ***"
                        penaltyYds = -10
                        HL% = 1
                    End If

                Case 17 + tmRat_GAME(P1, 0) To 22 + tmRat_GAME(P1, 0)
                    pbpString$ = pbpString$ + "OFF... ILLEGAL MOTION 5 YARDS ***"
                    penaltyYds = -5

                Case 22 + tmRat_GAME(P1, 0) To 27 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***"
                    penaltyYds = 5

                Case 27 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0) To 42 + tmRat_GAME(P1, 0) + tmRat_GAME(D, 0)
                    pbpString$ = pbpString$ + "DEF... HOLDING 5 YARDS - 1ST DOWN! ***"
                    IH = 1
                    penaltyYds = 5

                Case Else
                    Call COMPILER

                    ydLine = ydLine + ydsGained
                    Y1 = Y1 - ydsGained
                    currDown = currDown + 1

                    If ballFumbled = 1 Then Call FumblePBP: GoTo 650

                    GoTo 3303

            End Select

    End Select

    'Pro Rules | College 1991+ Or HL%% = 1 ??
    If (ruleOptType <> 0 Or ruleOptColl >= 4) Or (ruleOptType = 0 And HL% = 1) Then
        If R1 = 1 Then penaltyYds = penaltyYds + Int(Rnd(1) * 5) + 1
        If R1 = 2 Then penaltyYds = penaltyYds - Int(Rnd(1) * 8) + 1
    End If

    HL% = 0

    pbpString$ = pbpString$ + " 1-ACCEPT 2-DECLINE "
    Call PBP

    If playerMode <> 0 Then

        I$ = "2"

        If playerMode = 1 Then

            If (penaltyYds > 0 And D = compTeam) Or (penaltyYds < 0 And P1 = compTeam) Then I$ = GetKeyPress$

            If Not (penaltyYds < 0 And P1 <> compTeam And currDown > 3 And ydsGained < Y1) Then

                If penaltyYds > 0 And D <> compTeam Then

                    If IH = 1 And Y1 - ydsGained > 0 Then
                        I$ = "1"
                    Else
                        If ydsGained > 5 Or penaltyYds < 0 And P1 <> compTeam And currDown = 3 And Y1 - ydsGained > 3 Or penaltyYds < 0 And P1 <> compTeam And ydsGained < 0 Then
                            '3319
                            'Not sure why this is designed as is I can only imagine
                            'it was helpful to have many statements going to 3319
                            'because it wasn't written from if/then logic to start with
                        End If

                    End If

                End If

            End If

        End If

    End If

    3319 '
    If Val(I$) = 1 Then

        O = 5

    Else

        pbpString$ = "PENALTY DECLINED"

        Call PBP
        Call COMPILER

        IH = 0
        ydLine = ydLine + ydsGained
        Y1 = Y1 - ydsGained
        currDown = currDown + 1

        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1
        If O < 4 Or O > 5 Then O = 1

        If Int((Y1) * 10 + .5) / 10 < .05 Then
            currDown = 1
            Y1 = 10

            If 100 - ydLine < 9.95 Then Y1 = 100 - ydLine
            If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

            gameStats(P1, 0) = gameStats(P1, 0) + 1

            GoSub 3900

            If gameClock! <= 0 Then gameClock! = 1

        Else

            If currDown > 4 Then 3339

            GoSub 3900

        End If

        GoTo 700

    End If

    3325 '
    If ruleOptType = 0 And ruleOptColl = 0 Then O = 1
    pbpString$ = "PENALTY ACCEPTED"
    Call PBP

    If penaltyYds > 0 Then 3330

    penaltyYds = penaltyYds * -1

    'OFF PEN

    3326 If ydLine < penaltyYds * 2 Then
        Y1 = Y1 + ydLine / 2
        ydLine = ydLine - ydLine / 2
        gameStats(P1, 12) = gameStats(P1, 12) + ydLine / 2
    Else
        Y1 = Y1 + penaltyYds
        ydLine = ydLine - penaltyYds
        gameStats(P1, 12) = gameStats(P1, 12) + penaltyYds
    End If

    gameStats(P1, 11) = gameStats(P1, 11) + 1

    If Int(Y1 * 10 + .5) / 10 < .05 Then 3335

    GoSub 3900

    GoTo 700

    '----- NOODLE -----

    'DEFENSIVE PENALTY

    3330 '
    If 100 - ydLine < penaltyYds * 2 Then
        Y1 = Y1 - (100 - ydLine) / 2
        ydLine = ydLine + (100 - ydLine) / 2
        gameStats(D, 12) = gameStats(D, 12) + (100 - ydLine) / 2
    Else
        Y1 = Y1 - penaltyYds
        ydLine = ydLine + penaltyYds
        gameStats(D, 12) = gameStats(D, 12) + penaltyYds
    End If

    gameStats(D, 11) = gameStats(D, 11) + 1

    If Y1 < .01 Or IH = 1 Then
        IH = 0
    Else

        If gameClock! <= 0 Then gameClock! = 1

        GoSub 3900

        'skipSomething
        GoTo 700

    End If

    'If skipSomething

    '3335
    currDown = 1
    Y1 = 10

    If 100 - ydLine < 9.95 Then Y1 = 100 - ydLine
    If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

    gameStats(P1, 0) = gameStats(P1, 0) + 1

    GoSub 3900

    If gameClock! <= 0 Then gameClock! = 1

    'End If

    GoTo 700

    '----- NOODLE -----

    'College Overtime
    3339 '
    If NG%(14) = 3 And quarter > 4 Then 3364

    currDown = 1
    Y1 = 10
    ydLine = 100 - ydLine

    If ydLine > 90 Then Y1 = 100 - ydLine

    Call CHANGEPOSS

    O = 5

    GoSub 3900

    GoTo 700

    '----- NOODLE -----

    '3349
    O = 6

    If Int(Y1 * 10 + .5) / 10 < .05 Then

        currDown = 1
        Y1 = 10

        If 100 - ydLine < 9.95 Then Y1 = 100 - ydLine
        If O < 1 And ruleOptType = 0 And R5% = 0 Then O = 1

        gameStats(P1, 0) = gameStats(P1, 0) + 1

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
    WX%(0) = 1 - WX%(0)
    gameClock! = 900

    quarter = quarter + 1

    If quarter = 3 Or quarter >= 5 Then
        'If end of half
        U3 = 0
    End If

    endGame = 0

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

                Call TEAMBOX(1, 1)

                If Not (autoPlay = 1 And playerMode = 2) Then
                    Do
                        pbpString$ = "DISPLAY STATS AGAIN? (Y/N)"
                        Locate 7, (80 - Len(pbpString$)) / 2
                        Print pbpString$
                        Color 15, 0
                        I$ = GetKeyPress$
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

                endGame = 1

            End If

            '--- should this be outside the SELECT

            'Check if game is over
            If endGame <> 1 Then

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
                        Y1 = 10
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
                        Y1 = 10

                        'Starting yard line for possession
                        '-- College Rules ??
                        ydLine = 40

                        '-- Pro Rules
                        If ruleOptType <> 0 Then ydLine = 35

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
                endGame = 1
                Call EndOfGame
                Exit Sub
            Else
                GoTo 850
            End If

    End Select

    '----- NOODLE -----

    3552 '
    'Call pbplog(7443)

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
    gameStats(P1, 3) = gameStats(P1, 3) + 1
    gameStats(P1, 5) = gameStats(P1, 5) + 1

    If FF% <> 1 Then
        K2(P1, I1, 9) = K2(P1, I1, 9) + 1
        K2(P1, I1, 11) = K2(P1, I1, 11) + 1
    End If

    If endZone = 1 Then
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        GoTo 6605

    End If

    Call AddPossessionTime(5, 8)

    If FF% <> 0 Then
        FF% = 0
    Else
        If P2 = 12 Or P2 = 15 Then ydsGained = Int(Rnd(1) * 20) + 21
        If P2 <> 12 And P2 <> 15 Then ydsGained = Int(Rnd(1) * 10) + 1
        If P2 = 13 Then ydsGained = ydsGained - 5
        If P2 = 11 Or P2 = 14 Or P2 = 17 Or P2 = 16 Or P2 = 18 Then ydsGained = ydsGained + 10
    End If

    Call YARDAGE

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then

        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP

        'College overtime
        If NG%(14) = 3 And quarter > 4 Then 3364

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

        GoTo 3200

    Else

        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
        Call PBP

        O = 5
        Y1 = 10
        ydLine = ydLine + ydsGained

        'Record INT Yds
        gameInts(P1, Z1, 1) = gameInts(P1, Z1, 1) + ydsGained

        If ydsGained > IR(P1, Z1) Then
            gameIR$(P1, Z1) = ""
            IR(P1, Z1) = ydsGained
        End If

        If ydLine >= 90 Then Y1 = 100 - ydLine

        'College overtime
        If NG%(14) = 3 And quarter > 4 Then 3364

        F% = 5

        Call FUMBLE

        If ballFumbled = 1 Then Call FumblePBP: GoTo 650

        I = Int(Rnd(1) * 1000) + 1

        If I > 950 - tmRat_GAME(P1, 0) Then

            pbpString$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
            Call PBP

            'Record penalty
            gameStats(P1, 11) = gameStats(P1, 11) + 1

            'Record yds
            If ydLine < 20 Then
                '1/2 the distance to the goal
                ydLine = ydLine / 2
                gameStats(P1, 12) = gameStats(P1, 12) + ydLine / 2
            Else
                '10 yds
                ydLine = ydLine - 10
                gameStats(P1, 12) = gameStats(P1, 12) + 10
            End If

        End If

        GoSub 3900

        GoTo 700

    End If


    '----- NOODLE -----

    3900 '
    'Adjust possession time, check for special scenarios
    '(ie, hurry up, 2-minute warning, etc.)

    If O <> 8 Then

        'Pro Rules
        If ruleOptType <> 0 Then

            If gameClock! > 120.01 And quarter <> 1 And quarter <> 3 And Not (gameClock! > 159.99 And O = 0) And Not (gameClock! > 147 And O = 1) And Not (gameClock! > 142 And O = 2) And Not (gameClock! > 137 And O = 3) And Not (gameClock! > 126 And O = 4) And Not (gameClock! > 128 And O = 5) And Not (gameClock! > 130 And O = 6) And Not (gameClock! > 132 And O = 7) Then

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

            If O < 4 Then

                Call CheckGameClock
                If gameClock! = 0 Then GoTo 3360
                Call TWOMIN

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
                            O = 9

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
                            O = 9

                            Call AddPossessionTime(7, 10)
                            Call CheckGameClock
                            If gameClock! = 0 Then GoTo 3360

                            Return 'aka Exit Sub

                        End If

                    Case 4
                        If O = 0 Or O = 1 Then
                            pbpString$ = "HURRY UP"
                            Call PBP
                            O = O + 2
                        End If

                    Case 5
                        pbpString$ = qbacks$(P1, qbNumber(P1)) + " IS TRYING TO GET THE TEAM LINED UP"
                        Call PBP

                        O = 3
                        pbpString$ = "HE STEPS BACK AND THROWS IT OUT OF BOUNDS"
                        Call PBP

                        gameStats(P1, 3) = gameStats(P1, 3) + 1
                        K2(P1, qbNumber(P1), 9) = K2(P1, qbNumber(P1), 9) + 1

                        currDown = currDown + 1
                        If currDown = 3 Then thirdDownAtt(P1) = thirdDownAtt(P1) + 1

                        Call AddPossessionTime(7, 10)
                        Call CheckGameClock
                        If gameClock! = 0 Then GoTo 3360

                        Return 'aka Exit Sub

                End Select

            End If

        End If

        If O = 0 Then

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
            'O > 0
            If O >= 1 And O <= 3 Then
                timeElapsed! = 27 - (O * 5) + (Int(Rnd(1) * 5) + 1)
            Else
                'O >= 4
                timeElapsed! = (2 * O) - 5 + (Int(Rnd(1) * 3) + 1)
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
        fgSuccessChance = kickerIdx(P1, Q7%(P1))
    Else
        Call FGDIST
    End If

    pbpString$ = kicker$(P1, Q7%(P1)) + " TROTS ON TO TRY A FIELD GOAL"
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

        ' *** BAD SNAP FROM CENTER ***

        Call AddPossessionTime(4, 7)

        gameStats(P1, 10) = gameStats(P1, 10) + 1
        gameStats(P1, 9) = gameStats(P1, 9) + 1

        If ydLine + ydsGained < -9.999 Then
            score(D, quarter) = score(D, quarter) + 2
            score(D, 0) = score(D, 0) + 2
            GoTo 3251
        End If

        If ydLine + ydsGained <= .001 And ydLine + ydsGained >= -9.999 Then

            Call d100(RN0)

            If RN0 <= 50 Then
                score(D, quarter) = score(D, quarter) + 2
                score(D, 0) = score(D, 0) + 2
                GoTo 3251
            Else
                X1 = 10
                Call CHANGEPOSS
                GoTo 3200
            End If

            Call CHANGEPOSS
            currDown = 1
            Y1 = 10
            ydLine = 100 - ydLine

            If Y1 >= 90 Then Y1 = 100 - ydLine

            Call d100(RN)

            If RN <= 50 Then
                pbpString$ = gameTeams$(P1) + " TAKES OVER ON THE"
            Else
                pbpString$ = gameMascots$(P1) + " TAKE OVER ON THE"
            End If

            If ydLine > 50 Then
                pbpString$ = pbpString$ + Str$(Int(100 - ydLine)) + " YARD LINE"
            Else
                pbpString$ = pbpString$ + Str$(Int(ydLine)) + " YARD LINE"
            End If

            O = 5

            GoSub 3900

            GoTo 700

        End If

    Else

        Call d100(I)

        If (I >= 97 And ydLine >= 68 + goalPostAdj And I2 <> 2) Or (I >= 92 And ydLine < 68 + goalPostAdj And I2 <> 2) Then

            K2(P1, 4 + Q7%(P1) * 4, 17) = K2(P1, 4 + Q7%(P1) * 4, 17) + 1
            pbpString$ = "KICK IS BLOCKED!!!"
            Call PBP

            Call AddPossessionTime(4, 7)

            ydsGained = Int(Rnd(1) * 22) * -1
            Call YARDAGE
            ydLine = 100 - ydLine - ydsGained

            If ydLine >= 109.949 Then 3250

            If ydLine >= 100 Then

                Call CHANGEPOSS

                Call d100(RN)
                If RN <= 50 Then
                    pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
                Else
                    pbpString$ = gameMascots$(P1) + " RECOVER IT IN THE END ZONE!!"
                End If

                Call PBP

                X1 = 6

            Else

                Call d100(RN)

                If RN <= 94 Then
                    Call CHANGEPOSS

                    Y1 = 10

                    If ydLine >= 90 Then Y1 = 100 - ydLine

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

                    Call d100(RN)
                    If RN <= 50 Then
                        pbpString$ = gameTeams$(P1) + " TAKES IT INTO THE END ZONE!!"
                    Else
                        pbpString$ = gameMascots$(P1) + " TAKE IT INTO THE END ZONE!!"
                    End If

                    Call PBP

                    X1 = 6

                End If

            End If

            GoTo 3200

        End If

        'Increase FG attempt
        K2(P1, 4 + Q7%(P1) * 4, 17) = K2(P1, 4 + Q7%(P1) * 4, 17) + 1

        pbpString$ = "THE KICK IS UP AND ON ITS WAY"
        Call PBP

        If WX%(0) <> P1 Then
            fgSuccessChance = fgSuccessChance - Int(Rnd(1) * 7) + 3 - WX%(1)
        Else
            fgSuccessChance = fgSuccessChance + Int(Rnd(1) * 7) - 3 + WX%(1)
        End If

        Call FGATT

        If I <= fgSuccessChance Then

            'Increase FGM
            K2(P1, 3 + Q7%(P1) * 4, 17) = K2(P1, 3 + Q7%(P1) * 4, 17) + 1
            X1 = 8
            I1 = Q7%(P1)

            Call SCOREUPD

            currDown = 1
            Y1 = 10
            score(P1, 0) = score(P1, 0) + 3
            score(P1, quarter) = score(P1, quarter) + 3

            Call FGMADE
            Call AddPossessionTime(4, 7)
            Call ITSGOOD
            Call DisplayField

            ydLine = 35

            Call CHANGEPOSS
            Call SCOREBOARD

            If ruleOptType <> 0 And goalPostAdj < 0 Then ydLine = 40
            If ruleOptType = 1 And ruleOptPro = 3 Or ruleOptType = 4 Then ydLine = 30
            If ruleOptColl = 0 And ruleOptType = 0 Then ydLine = 40

            Call SCOREBOARD
            Call CheckGameClock
            If gameClock! = 0 Then U3 = 9: GoTo 3360

            If quarter > 4 Then 3364

            GoTo 340

        Else

            Call MISSEDFG
            currDown = 1
            Y1 = 10

            Call CHANGEPOSS

            If ydLine > 40 And ydLine <= 79.99 And ruleOptPro = 3 And ruleOptType = 1 Then
                ydLine = ydLine - 7
                ydLine = 100 - ydLine
            Else
                If ydLine > 79.99 Or goalPostAdj < 0 Then ydLine = 20
                If ydLine > 40 And ydLine <= 79.99 And goalPostAdj = 0 Then ydLine = 100 - ydLine
            End If

            Call AddPossessionTime(4, 7)
            Call CheckGameClock
            If gameClock! = 0 Then GoTo 3360

            'College overtime
            If quarter > 4 And NG%(14) = 3 Then 3364

        End If

    End If

    GoTo 700


    '----- NOODLE -----


    3840 '
    gameStats(P1, 28) = gameStats(P1, 28) + 1
    pbpString$ = "KICK IS BLOCKED!!!"

    Call PBP

    Call AddPossessionTime(4, 7)

    ydsGained = Int(Rnd(1) * 30) * -1

    Call YARDAGE
    ydLine = 100 - ydLine - ydsGained

    If ydLine >= 109.949 Then 3250

    If ydLine >= 100 Then
        Call CHANGEPOSS

        pbpString$ = gameTeams$(P1) + " RECOVERS IT IN THE END ZONE!!"
        Call PBP

        X1 = 6
        GoTo 3200
    End If

    Call d100(RN)

    If RN > 60 Then

        Call CHANGEPOSS
        ydsGained = 100 - ydLine
        startYds = 100 - ydLine
        endYds = startYds - ydsGained

        Call MOVEBALL

        pbpString$ = gameTeams$(P1) + " TAKES THE BALL INTO THE END ZONE!!"
        Call PBP

        X1 = 6

        GoTo 3200

    Else

        Call CHANGEPOSS: Y1 = 10

        If ydLine >= 90 Then Y1 = 100 - ydLine

        currDown = 1

        Call DELAY

        If gameClock! = 0 Then GoTo 3360

    End If

    GoTo 700


    '----- NOODLE -----


    'Pass Intercepted

    3876 '
    X = 0
    gameStats(P1, 3) = gameStats(P1, 3) + 1
    gameStats(P1, 5) = gameStats(P1, 5) + 1
    K2(P1, I1, 9) = K2(P1, I1, 9) + 1
    K2(P1, I1, 11) = K2(P1, I1, 11) + 1

    Call AddPossessionTime(5, 8)

    ydsGained = Int(Rnd(1) * 10) + 1
    Call YARDAGE

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.9499 Then
        pbpString$ = pbpString$ + " IN THE END ZONE"
        Call PBP
        Call Touchback
    Else

        If ydsGained + ydLine > 49.99 Then pbpString$ = pbpString$ + " AT THE " + Str$(Int(100 - ydsGained - ydLine)) + " YARD LINE": Call PBP
        If ydsGained + ydLine < 49.99 Then pbpString$ = pbpString$ + " AT THE " + Str$(Int(ydsGained + ydLine)) + " YARD LINE": Call PBP

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
            GoTo 3200

        Else

            pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10) + " YARDS"
            Call PBP

            O = 5
            ydLine = ydLine + ydsGained
            Y1 = 10

            If ydLine >= 90 Then Y1 = 100 - ydLine

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

    Call d100(RN)
    If RN <= 18 Then
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
            For I3 = 0 To 1: Call CHANGEQB: Next I3
        End If

        Randomize Timer

        Call SCOREBOARD

        GoTo 330

    Else

        If endGame = 1 Then
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
    Print Tab(80);

    Locate , 33: Color 14
    Print "GAME OPTIONS MENU"

    Locate 24, 1
    Print Tab(80);
    Locate , 1
    Print "    LANCE HAFFNER 1986-2000"; Tab(70); Left$(Time$, 5);
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

    Shared gamePK$(), oppName$(), locIndicator$(), gameSite$()
    Shared teamIndicator$()
    Shared tradeLF$(), tradeLI$(), tradeLK$(), tradeLP$()

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
    endGame = 0
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
    kickDir = 0
    kickYL = 0
    O = 0
    overtimeOpt = 0
    nbrScores = 0
    playerMode = 0
    playerOpt = 0
    P1 = 0
    P2 = 0
    periodNbr = 0
    puntRetNbr = 0
    quarter = 0
    qback = 0
    qbTakeKnee = 0
    revFumble = 0
    ruleOptColl = 0
    ruleOptPro = 0
    ruleOptType = 0
    R1 = 0
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
    Y1 = 0
    YC = 0
    YF = 0
    Z1 = 0
    ydsGained = 0
    ydLine = 0
    ydLineTeam = 0
    ydsToScore = 0
    BO% = 0
    EG% = 0
    F% = 0
    FF% = 0
    GL% = 0
    HB% = 0
    intChance = 0
    PC% = 0
    PS% = 0
    R5% = 0

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

    Erase gamePK$, oppName$, locIndicator$, gameSite$
    Erase tradeLF$, tradeLI$, tradeLK$, tradeLP$

    Erase oppScore, powerRating, teamScore

    Erase AF, AM, defInts, defSacks
    Erase FA, FM
    Erase gameLongFG, gameStats, gameZ, gameZ1, gameZ2
    Erase K1, K2, K3
    Erase gameInts, gameSacks, hasRunFF, playerInts, IR
    Erase kickerFGA, kickerFGPct, kickerIdx, kickerPATPct
    Erase kickReturners, krNumRet, krYdsPerRet
    Erase LC, leagRat_GAME, LF, LI, LK, LP, LR
    Erase nbrPossOT
    Erase PK, playerSacks, prNumRet, prYdsPerRet
    Erase puntReturners, puntYdsPerP
    Erase quarterbacks, qbNumber
    Erase qbArmRat, qbMobility, qbNumAtt, qbCompPct, qbPctInt
    Erase runBacks, rbRushAtt, rbRushAvg, rbNumRec, rbYdsPerC
    Erase score, scoreQuarters, scoreTimes, schedGame
    Erase teamIdx_GAME, teamInts, teamSacks, timeouts, timePoss
    Erase totalInts, totalSacks, tmRat_GAME
    Erase useRandomQB
    Erase W6, WR, wrNumRec, wrYdsPerC, XD, ydsPerComp

    Erase HB%, NG%, PC%
    Erase Q7%, RV%, S1%, ST%, SX%
    Erase teamYears, thirdDownAtt, thirdDownFail
    Erase V4%, WX%

    Erase defInts$, defSacks$
    Erase gameMascots$, gameTeams$, kicker$, kickRet$
    Erase passCov$, playDesc$
    Erase pret$, punter$, qbacks$
    Erase rbacks$, SX$
    Erase wdRec$, diskIDs$

    Erase gameIR$, gameKR$, gameLC$, gameLR$
    Erase gamePR$, gameRB$, gameQB$, gameWR$

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP

    For P9 = 0 To 1

        teamNbr = NG%(P9)
        Call ReadGameTeam(diskIDs$(P9), gameTeams$(P9), teamNbr, P9)
        'NG%(P9) = teamNbr
        teamIdx_GAME(P9) = NG%(P9)
        X = 0

    Next P9

    'Assign settings from schedule / etc.
    gameLoc = NG%(2)
    ruleOptType = NG%(3)
    pbpDelay! = NG%(4) / 10
    ruleOptColl = NG%(7)
    playerMode = NG%(10)
    ruleOptPro = NG%(11)
    tickerStart = NG%(12)

    'If college rules, pre-1974
    If ruleOptPro = 1 And ruleOptType <> 0 Then
        'is this an adjustment for
        'kicking distance based on
        'placement of the goal posts???
        goalPostAdj = -10
    End If

    'If WFL rules
    If ruleOptType = 4 Then
        goalPostAdj = 0
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
    Color tmRat_GAME(teamIdx, 2), tmRat_GAME(teamIdx, 1)

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
    Color tmRat_GAME(teamIdx, 2), tmRat_GAME(teamIdx, 1)

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
'          OPTIONBOX Subroutine
'----------------------------------------
'This subroutine draws an ASCII box in grey.
'The box starts at row 8 and is approx. 60
'char's wide. When done, the cursor is set
'at the first printable row and column
'inside the box.
Sub OPTIONBOX (J)

    PCopy 0, 1
    Screen , , 1, 1

    Locate 8, 8
    Color 0, 7

    'Print box top
    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

    'Print box sides
    For II = 1 To J
        Locate 8 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next

    'Print box bottom
    Locate 9 + J, 8
    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 10, 15

End Sub


'----------------------------------------
'         FRAMESTATS Subroutine
'----------------------------------------
'This subroutine draws an ASCII box in grey.
'The box starts at row 8 and is approx. 77
'char's wide. When done, the cursor is set
'at the first printable row and column
'inside the box.
Sub FRAMESTATS (J)

    Locate 1, 1
    Color 0, 7

    Print Chr$(214); String$(77, 196); Chr$(183);

    For II = 1 To J
        Locate 1 + II, 1
        Print Chr$(186); String$(77, 32); Chr$(186);
    Next

    Locate 2 + J, 1
    Print Chr$(211); String$(77, 196); Chr$(189);

    Locate 2, 32

End Sub


'----------------------------------------
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Sub d100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub


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

    If UCase$(M$) = "Q" Then Cls: End
    If UCase$(M$) = "O" Then Call OPTIONS

    If pbpDelay! > 0 Then
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

        Call d100(RN)

        If halfTime = 1 Then

            If ruleOptType = 0 Then
                If RN <= 50 Then
                    Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD"
                Else
                    Locate , 28: Print "RED MAN HALFTIME REPORT"
                End If
            Else
                If RN <= 50 Then
                    Locate , 29: Print "TOYOTA HALFTIME REPORT"
                Else
                    Locate , 29: Print "HONDA HALFTIME REPORT"
                End If
            End If

        Else

            If endGame = 1 Then

                If ruleOptType = 0 Then

                    Locate , 27: Print "CHEVROLET FINAL SCOREBOARD"

                Else
                    Select Case RN
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
    If endGame = 1 Then tickerIdx = 14

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

    Open diskPaths$(0) + scheduleFile$ As #1 Len = 52

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$
    Next

    Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

    NG%(13) = 1

    For X = 0 To 20: LSet Q$(X + 1) = MKI$(NG%(X)): Next

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
            If rbNumRec(I, I1) <> 0 Then I2 = rbNumRec(I, I1): I3 = 0
        Next I1

        For I1 = 0 To 9
            If I1 = 0 Then K3(I, 3) = K3(I, 3) + rbNumRec(I, I1) * rbYdsPerC(I, I1): K3(I, 3) = K3(I, 3) + wrYdsPerC(I, I1) * (wrNumRec(I, I1) - I2)
            If I1 > 0 And I1 < 10 And rbNumRec(I, I1) <> 0 Then K3(I, 3) = K3(I, 3) + rbYdsPerC(I, I1) * (rbNumRec(I, I1) - I3)
            If I1 > 0 And I1 < 6 And wrNumRec(I, I1) <> 0 Then K3(I, 3) = K3(I, 3) + wrYdsPerC(I, I1) * (wrNumRec(I, I1) - I6)
            If rbNumRec(I, I1) <> 0 Then I3 = rbNumRec(I, I1)
            If wrNumRec(I, I1) <> 0 Then I6 = wrNumRec(I, I1)
        Next I1

        K3(I, 3) = Int((K3(I, 3) * 3) / 100)

        I2 = 0
        I3 = 0
        I6 = 0

        For I1 = 0 To 9
            If rbRushAtt(I, I1) <> 0 Then I4 = rbRushAtt(I, I1): I5 = 0
        Next I1

        For I1 = 0 To 9
            If I1 = 0 Then K3(I, 0) = K3(I, 0) + rbRushAtt(I, I1) * rbRushAvg(I, I1): I2 = I2 + qbNumAtt(I, I1) * qbCompPct(I, I1): I3 = I3 + qbNumAtt(I, I1) * qbPctInt(I, I1)
            If I1 > 0 And rbRushAtt(I, I1) <> 0 Then K3(I, 0) = K3(I, 0) + rbRushAvg(I, I1) * (rbRushAtt(I, I1) - I5)
            If I1 > 0 And I1 < 4 And qbNumAtt(I, I1) <> 0 Then I2 = I2 + qbCompPct(I, I1) * (qbNumAtt(I, I1) - I6): I3 = I3 + qbPctInt(I, I1) * (qbNumAtt(I, I1) - I6)
            If rbRushAtt(I, I1) <> 0 Then I5 = rbRushAtt(I, I1)
            If qbNumAtt(I, I1) <> 0 Then I6 = qbNumAtt(I, I1)
        Next I1

        If I = 0 Then I1 = 1
        If I = 1 Then I1 = 0

        K3(I, 0) = ((K3(I, 0) * 3) / 100) + ((leagRat_GAME(I1, 0)) * 3): ' HAD (leagRat_GAME()+1)*3
        K3(I, 0) = Int(K3(I, 0) + .5) / 10
        K3(I, 1) = Int((I2 / 100) + leagRat_GAME(I1, 1) + .5)
        K3(I, 2) = Int((I3 / 100) - leagRat_GAME(I1, 2) + .5)
        K3(I, 4) = (puntYdsPerP(I, 0) * 2) + 28
        K3(I, 5) = (kickerFGA(I, Q7%(I)) * 5) + 25
        K3(I, 6) = kickerIdx(I, Q7%(I))

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

    ydLine = 35
    ydsToScore = 65

    '1st and 10
    currDown = 1
    Y1 = 10
    YF = -Y1

    If ruleOptType <> 0 And goalPostAdj < 0 Then
        ydLine = 40
        ydsToScore = 60
    End If

    If ruleOptType = 1 And ruleOptPro = 3 Or ruleOptType = 4 Then
        ydLine = 30
        ydsToScore = 70
    End If

    If ruleOptType = 0 And ruleOptColl = 0 Then
        ydLine = 40
        ydsToScore = 60
    End If

End Sub


'----------------------------------------
'           KICKER Subroutine
'----------------------------------------
'This subroutine gives the user a choice
'of kicker for a kicking attempt.
Sub CHOOSEKICKER ()

    Color , 1
    Cls

    Call OPTIONBOX(10)

    Locate , 31: Print "SELECT KICKER": Print
    Locate , 18
    Print " # NAME            FG%  %ATT  EP%  %ATT"

    For I = 0 To 1

        If kicker$(I3, I) <> "XXX" And kicker$(I3, I) <> "END" And kicker$(I3, I) <> "" Then
            Locate , 18
            Print I; "-"; kicker$(I3, I);
            Locate , 36
            Print Using " ###   ###  ###   ###"; kickerFGA(I3, I) * 5 + 25, kickerFGPct(I3, I), kickerIdx(I3, I), kickerPATPct(I3, I)
        End If

        Locate , 1

    Next I

    Print

    Locate , 19: Print "SELECT (0-1) ";

    Do
        I$ = GetKeyPress$
        choice = Val(I$)
    Loop Until choice = 0 Or choice = 1

    Q7%(I3) = choice

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

    If WX%(1) = 0 Then Print "NONE ";
    If WX%(1) = -7 Then Print "LIGHT ";
    If WX%(1) = -15 Then Print "STRONG ";

    If WX%(1) <> 0 Then
        If WX%(0) = P1 Then
            Print "+";
        Else
            Print "-";
        End If
    End If

    Locate , 40
    Print "WEATHER: ";

    If WX%(4) = 0 Then Print "GOOD"
    If WX%(4) = -3 Then Print "RAIN/SNOW"
    If WX%(4) = -6 Then Print "DOWNPOUR/BLIZZARD"

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

        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1)
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
    Print Using " TO GO: ##.#"; Y1;: Print Tab(80); " ";

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
    Call d100(I6)

    If dLine > 3 And dLine < 6 Then

        Select Case ydLine

            Case 94.999 To 95.998

                Select Case I6
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

                Select Case I6
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

                Select Case I6
                    Case 1 To 24: ydsGained = 4
                    Case 25 To 28: ydsGained = 2
                    Case 29 To 69: ydsGained = 1
                    Case 70 To 85: ydsGained = 0
                    Case 86 To 88: ydsGained = -1
                    Case 89 To 93: ydsGained = -2
                    Case 94 To 100: ydsGained = -4
                End Select

            Case 97.999 To 98.998

                Select Case I6
                    Case 1 To 43: ydsGained = 3
                    Case 44 To 65: ydsGained = 1
                    Case 66 To 83: ydsGained = 0
                    Case 84 To 92: ydsGained = -1
                    Case 93 To 94: ydsGained = -2
                    Case 95 To 100: ydsGained = -4
                End Select

            Case Is >= 98.999
                Select Case I6
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

                Select Case I6
                    Case 1 To 17: ydsGained = 6
                    Case 18 To 22: ydsGained = 4
                    Case 23 To 35: ydsGained = 3
                    Case 36 To 52: ydsGained = 2
                    Case 53 To 76: ydsGained = 1
                    Case 77 To 93: ydsGained = 0
                    Case 94 To 100: ydsGained = -1
                End Select

            Case 95.999 To 96.998

                Select Case I6
                    Case 1 To 26: ydsGained = 5
                    Case 27 To 29: ydsGained = 3
                    Case 30 To 49: ydsGained = 2
                    Case 50 To 78: ydsGained = 1
                    Case 79 To 94: ydsGained = 0
                    Case 95 To 100: ydsGained = -1
                End Select

            Case 96.999 To 97.998

                Select Case I6
                    Case 1 To 27: ydsGained = 4
                    Case 28 To 32: ydsGained = 2
                    Case 33 To 80: ydsGained = 1
                    Case 81 To 98: ydsGained = 0
                    Case 99 To 100: ydsGained = -1
                End Select

            Case 97.999 To 98.998

                Select Case I6
                    Case 1 To 46: ydsGained = 3
                    Case 47 To 72: ydsGained = 1
                    Case 73 To 91: ydsGained = 0
                    Case 92 To 100: ydsGained = -1
                End Select

            Case Is >= 98.999

                Select Case I6
                    Case 1 To 61: ydsGained = 2
                    Case 62 To 96: ydsGained = 0
                    Case 97 To 100: ydsGained = -1
                End Select

        End Select

    End If

    Call d100(I6)

    If I2 <= 30 Then
        'Added this to avoid scenarios
        'where the rushing average from
        'OTHERS was negative, therefore
        'leading to a negative index
        If I2 <= 0 Then I2 = 1

        If I6 <= GL%(I2, 1) Then ydsGained = ydsGained + GL%(I2, 2)

    Else
        'I2 > 30
        'Given how I2 is calculated (based on Rushing Avg),
        'it is not clear how you get a value this high
        If I6 <= GL%(30, 1) Then ydsGained = ydsGained + GL%(30, 2)

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
    Color tmRat_GAME(0, 2), tmRat_GAME(0, 1): Print gameTeams$(0);

    Color 0, 7
    Locate , 52
    Color tmRat_GAME(1, 2), tmRat_GAME(1, 1): Print gameTeams$(1)

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

    Locate 16, 38: If K3(0, 3) >= 18 Then Print "*";
    Locate 16, 60: If K3(1, 3) >= 18 Then Print "*";

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
                    K2(I, I1, 0) = K2(I, I1, 0) + K2(I, I2 + 10, 0)
                    K2(I, I1, 1) = K2(I, I1, 1) + K2(I, I2 + 10, 1)
                    K2(I, I1, 2) = K2(I, I1, 2) + K2(I, I2 + 10, 2)
                    K2(I, I2 + 10, 0) = 0
                    K2(I, I2 + 10, 1) = 0
                    K2(I, I2 + 10, 2) = 0
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

    Call d100(X)

    pbpString$ = kicker$(P1, Q7%(P1)) + " IS PLACING THE BALL ON THE TEE"
    Call PBP

    If X <= 50 Then
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

    If Int(ydsGained * 10 + .5) / 10 < 9.95 Then
        pbpString$ = "BALL DID NOT TRAVEL 10 YDS"
        Call PBP
        ydLine = 100 - ydLine
    Else
        I = (Rnd(1) * 100) + 1
        If I <= 78 Then
            pbpString$ = gameTeams$(P1) + " RECOVERS"
            Call PBP
            ydLine = 100 - ydLine
        Else
            pbpString$ = gameTeams$(D) + " RECOVERS !!!"
            Call PBP
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

    Call QBTHROW

    Call d100(RN)

    If RN > 10 Then

        Call d100(RN)

        If RN > 60 Then

            Call d100(RN)

            Select Case P2

                Case 13
                    Select Case RN
                        Case 1 To 40: pbpString$ = "BUT THE SCREEN FALLS INCOMPLETE"
                        Case 41 To 55: pbpString$ = "BUT THE SCREEN IS BROKEN UP"
                        Case 56 To 60: pbpString$ = "BUT " + targetWRName$ + " CAN'T HANDLE THE PASS"
                        Case 61 To 100: pbpString$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE"
                    End Select

                Case 12
                    Select Case RN
                        Case 1 To 15: pbpString$ = "BUT THE PASS IS BROKEN UP"
                        Case 16 To 44: pbpString$ = "BUT THE PASS IS OVERTHROWN"
                        Case 45 To 74: pbpString$ = "BUT " + targetWRName$ + " CAN'T GET TO THE BALL...INCOMPLETE"
                        Case 75 To 80: pbpString$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE"
                        Case 81 To 100: pbpString$ = "BUT " + targetWRName$ + " IS COVERED...INCOMPLETE"
                    End Select

                Case Else
                    Select Case RN
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

            RN = Int(Rnd(1) * 26) + 1

            Select Case RN
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
        RN = Int(Rnd(1) * 6) + 1
        Select Case RN
            Case 1: pbpString$ = "PASS IS BROKEN UP"
            Case 2: pbpString$ = "PASS IS TIPPED AWAY"
            Case 3: pbpString$ = "PASS IS KNOCKED AWAY"
            Case 4: pbpString$ = "PASS IS KNOCKED DOWN"
            Case 5: pbpString$ = "PASS IS DEFLECTED"
            Case 6: pbpString$ = "PASS IS BATTED AWAY"
        End Select

        'Handle an interception
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
    pbpString$ = qbacks$(P1, qbNumber(P1)) + " KEEPS IT ON THE SNEAK"

    Call PBP

    Call d100(I4)

    If (coverage = 11 Or coverage = 7 Or coverage = 9) Then

        If I4 <= 20 Then
            ydsGained = (Int(Rnd(1) * 6) - 3) / 10
        Else
            ydsGained = (Int(Rnd(1) * 7) + 3) / 10
        End If

    Else

        Select Case dLine

            Case 11
                If I4 <= 40 Then
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 1, 2

                Select Case I4
                    Case 1 To 10
                        ydsGained = (Int(Rnd(1) * 6) - 3) / 10

                    Case 11 To 80
                        ydsGained = (Int(Rnd(1) * 7) + 3) / 10

                    Case 81 To 100
                        ydsGained = Int(Rnd(1) * 2) + 1
                End Select

            Case 3, 4
                If I4 <= 30 Then
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 5, 6
                If I4 <= 15 Then
                    ydsGained = (Int(Rnd(1) * 6) - 3) / 10
                Else
                    ydsGained = (Int(Rnd(1) * 7) + 3) / 10
                End If

            Case 7 To 10
                Select Case I4
                    Case 1 To 10
                        ydsGained = (Int(Rnd(1) * 6) - 3) / 10

                    Case 11 To 80
                        ydsGained = (Int(Rnd(1) * 7) + 3) / 10

                    Case 81 To 100
                        ydsGained = Int(Rnd(1) * 2) + 1

                End Select

        End Select

    End If

    pbpString$ = "HE GAINS"
    I1 = qback
    X = 4
    X1 = 9
    R1 = 1

    If Int((ydsGained + ydLine) * 10 + .5) / 10 >= 99.94999 Then
        pbpString$ = pbpString$ + Str$(Int(100 - ydLine))
    Else
        pbpString$ = pbpString$ + Str$(Int((ydsGained) * 10 + .5) / 10)
    End If

    pbpString$ = pbpString$ + " YARDS ON THE PLAY"
    Call PBP

End Sub


'----------------------------------------
'           OVERUSE Subroutine
'----------------------------------------
'This subroutine attempts to adjust the
'opponents response to a play that is
'being overused.
Sub OVERUSE ()

    If gameStats(P1, 1) + gameStats(P1, 3) <> 0 Then

        If (gameStats(P1, 1) / (gameStats(P1, 1) + gameStats(P1, 3))) < (leagRat_GAME(P1, 7) / 100) And K3(P1, 3) >= 18 Then I4 = I4 * .65

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

    Call d100(I5)
    XE = ydsPerComp(D) / 100

    If XE <= .5 Then
        XE = Int(XE * 100 + .5)
        If XE > 21 Then XE = 21
        If XE < 1 Then XE = 1
        If I5 <= PS%(1, XE) Then I4 = I4 + PS%(2, XE)
    Else
        XE = Int((1 - XE) * 100 + .5)
        If XE > 21 Then XE = 21
        If XE < 1 Then XE = 1
        If I5 <= PS%(1, XE) Then I4 = I4 - PS%(2, XE)
    End If

    Call d100(RN)
    If qbArmRat(P1, qbNumber(P1)) > 20 Then qbArmRat(P1, qbNumber(P1)) = 20
    If qbArmRat(P1, qbNumber(P1)) < -20 Then qbArmRat(P1, qbNumber(P1)) = -20

    QX = qbArmRat(P1, qbNumber(P1))

    If QX <> 0 Then

        If QX < 0 Then QX = QX * -1

        If RN <= QR%(QX, 1) Then
            If qbArmRat(P1, qbNumber(P1)) > 0 Then
                I4 = I4 + QR%(QX, 2)
            Else
                I4 = I4 - QR%(QX, 2)
            End If
        End If

    End If

    Call OVERUSE
    I7 = 4

    altYdg% = 0

    Select Case P2
        Case 9
            I7 = 1

        Case 10
            I7 = 3
            If coverage = 9 Or coverage = 10 Then I8 = 21: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 11 Then I8 = 25: Call BlitzYardageLong(I8): altYdg% = 1

        Case 11
            If coverage = 7 Or coverage = 8 Then I8 = 21: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 9 Or coverage = 10 Then I8 = 25: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 11 Then I8 = 28: Call BlitzYardageLong(I8): altYdg% = 1

        Case 12
            I7 = 5
            If coverage = 7 Or coverage = 8 Then I8 = 33: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage >= 9 Then I8 = 42: Call BlitzYardageLong(I8): altYdg% = 1

        Case 13
            I7 = 2
            If coverage = 9 Or coverage = 10 Then I8 = 25: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 11 Then I8 = 36: Call BlitzYardageLong(I8): altYdg% = 1

        Case 14
            If coverage = 7 Or coverage = 8 Then I8 = 25: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 9 Or coverage = 10 Then I8 = 28: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 11 Then I8 = 31: Call BlitzYardageLong(I8): altYdg% = 1

        Case 15
            I7 = 5
            If coverage >= 7 Then I8 = 42: Call BlitzYardageLong(I8): altYdg% = 1

        Case 16
            If coverage = 7 Or coverage = 8 Then I8 = 18: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 9 Or coverage = 10 Then I8 = 28: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 11 Then I8 = 33: Call BlitzYardageLong(I8): altYdg% = 1

        Case 17, 18
            If coverage = 7 Or coverage = 9 Then I8 = 21: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage = 8 Then I8 = 25: Call BlitzYardageLong(I8): altYdg% = 1
            If coverage >= 10 Then I8 = 28: Call BlitzYardageLong(I8): altYdg% = 1

    End Select

    If altYdg% <> 1 Then

        I4 = I4 + S1%(2, P2 - 8, coverage)

        If I4 > 10 Then I4 = 10
        If I4 < 1 Then I4 = 1

        Call d100(I9)

        For I = 1 To 14
            If I9 <= S2%(I7, I4, I) Then Exit For
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

    Call d100(I4)

    Select Case I2

        Case Is <= 15

            Select Case I4

                Case 1 To 15
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 16 To 60
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 61 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 2.22)) + 1

            End Select

        Case 16, 17

            Select Case I4

                Case 1 To 10
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 11 To 50
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 51 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.78)) + 1

            End Select

        Case 18, 19

            Select Case I4

                Case 1 To 10
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 11 To 40
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 41 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.54)) + 1

            End Select

        Case 20, 21

            Select Case I4

                Case 1 To 5
                    ydsGained = Int(Rnd(1) * 5) - 4

                Case 6 To 30
                    ydsGained = Int(Rnd(1) * 2) + 1

                Case 31 To 100
                    ydsGained = Int(Rnd(1) * (I2 * 1.32)) + 1

            End Select

        Case Else

            Select Case I4

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
'is based on the chosen offense. But that
'is somewhat a guess.
Sub COMPCHEAT ()

    coverage = 1

    R0 = Int(Rnd(1) * 4) + 1
    Call d100(I3)

    If R0 = 2 Then

        Select Case P2

            Case 1, 2, 6, 8
                If I3 <= 50 Then
                    dLine = 3
                Else
                    dLine = 4
                End If

            Case 3, 4, 5, 7
                dLine = 2

            Case 9, 10, 13
                coverage = 3

            Case 11, 14
                If I3 <= 50 Then
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
        RN = Int(Rnd(1) * teamSacks(D)) + 1

        For Z1 = 0 To 14
            M5 = M5 + playerSacks(D, Z1)
            If RN <= M5 Then pbpString$ = defSacks$(D, Z1) + " OF ": Exit For
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
                If Y1 > 10 Then
                    Call d100(R0)
                    dLine = 1
                    coverage = 2

                    Select Case R0

                        Case 1 To 5
                            dLine = 9
                            Call d100(RN)

                            Select Case RN

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

                If Y1 < 10 Then
                    '8867
                    Call d100(R0)
                    dLine = 1
                    coverage = 1
                    If R0 <= 50 Then
                        dLine = 2
                        coverage = 3
                    Else
                        dLine = 4
                    End If

                    skipY90 = 1

                End If

            Case 2
                If Y1 > 8 Then
                    Call d100(R0)
                    dLine = 1
                    coverage = 2

                    Select Case R0

                        Case 1 To 5
                            dLine = 9
                            Call d100(RN)

                            Select Case RN

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

                If Y1 < 5 Then
                    '8867
                    Call d100(R0)
                    dLine = 1
                    coverage = 1
                    If R0 <= 50 Then
                        dLine = 2
                        coverage = 3
                    Else
                        dLine = 4
                    End If

                    skipY90 = 1

                End If

            Case 3
                If Y1 >= 3 Then
                    Call d100(R0)
                    dLine = 1
                    coverage = 2

                    Select Case R0
                        Case 1 To 5
                            dLine = 9
                            Call d100(RN)

                            Select Case RN

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
                    Call d100(R0)
                    dLine = 1
                    coverage = 1
                    If R0 <= 50 Then
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
                        Call d100(R0)
                        dLine = 1
                        coverage = 2

                        Select Case R0
                            Case 1 To 5
                                dLine = 9
                                Call d100(RN)

                                Select Case RN

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

                If Y1 >= 3 Then
                    Call d100(R0)
                    dLine = 1
                    coverage = 2

                    Select Case R0
                        Case 1 To 5
                            dLine = 9
                            Call d100(RN)

                            Select Case RN

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
                    Call d100(R0)
                    dLine = 1
                    coverage = 1
                    If R0 <= 50 Then
                        dLine = 2
                        coverage = 3
                    Else
                        dLine = 4
                    End If

                    skipY90 = 1

                End If

            Case Else
                Call d100(R0)

                If R0 <= leagRat_GAME(P1, 7) Then
                    Call d100(R0)
                    dLine = 1
                    coverage = 1
                    If R0 <= 50 Then
                        dLine = 2
                        coverage = 3
                    Else
                        dLine = 4
                    End If

                Else
                    Call d100(R0)
                    dLine = 1
                    coverage = 2

                    Select Case R0
                        Case 1 To 5
                            dLine = 9
                            Call d100(RN)

                            Select Case RN

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
            If ydLine >= 90 And coverage = 5 Or Y1 < 7 And coverage = 5 Then coverage = 3
        End If

    Else

        If score(P1, 0) - score(D, 0) <= 0 Then

            Call d100(R0)

            If R0 <= 50 Then
                dLine = Int(Rnd(1) * 3) + 4
            Else
                dLine = 3
            End If

            Call d100(R0)

            Select Case R0

                Case 1 To 33: coverage = 1

                Case 34 To 67: coverage = 2

                Case 68 To 100: coverage = 5

            End Select

        Else

            Call d100(R0)
            Call d100(RN)

            Select Case RN
                Case 1 To 50:
                    dLine = 1
                    If R0 <= 90 Then
                        coverage = 1
                    Else
                        coverage = 2
                    End If

                Case 51 To 70
                    dLine = 2
                    If R0 <= 90 Then
                        coverage = 1
                    Else
                        coverage = 2
                    End If

                Case 71 To 94
                    dLine = 3
                    If R0 <= 90 Then
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

    If P1 Then J = FNP(ydsToScore) - 3 Else J = FNP(100 - ydsToScore) - 3

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
    YF = -Y1

    If P1 Then
        WS = FNP(ydsToScore + YF) - 3
        WE = WS + 6
    Else
        WE = FNP(100 - ydsToScore - YF) - 3
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

            RN = Int(Rnd(1) * 21) + 1

            Select Case RN
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
                pbpString$ = "The ball carrier" + I$ + " for a" + Str$(Int(-ydLine)) + " yard loss"
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

        YF = -Y1
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

        Print Using " TO GO: ##.#"; Y1;
        Print Tab(80); " ";

        Locate 5, 44: Print " BALL ON ";
        If ydLine <= 50 Then
            Print "OWN";
        Else
            Print "OPP";
        End If

        Print Using " ##.# YD LINE "; ydLineTeam;
        Print Tab(80); " ";

    End If

End Sub


'----------------------------------------
'           DEFMENU Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'selecting their defensive choices.
Sub DEFMENU ()


    If playerMode = 1 And D = compTeam Or playerMode = 2 Then

        Call DEFENSE
        Call DEFGOALLINE

    Else

        goodSelection% = 1

        Do

            Call FRAMEOPTION(10, D)
            Print "SELECT DEFENSIVE LINE PLAY "; gameTeams$(D)

            For dLine = 1 To 5:
                Locate 11 + dLine, 14
                Print dLine - 1; "- "; defPlay$(dLine); Tab(41); dLine + 4; "- "; defPlay$(dLine + 5)
            Next dLine

            Print
            Locate , 28: Print " G - GOAL LINE (5 YL AND IN)"

            Do
                I$ = GetKeyPress$

                If UCase$(I$) = "Q" Then Cls: End
                If UCase$(I$) = "G" And ydLine >= 94.999 Then
                    dLine = 11
                    Screen , , 0, 0
                    Exit Sub
                End If

                dLine = Val(I$)

            Loop Until dLine >= 0 And dLine <= 9 And UCase$(I$) <> "G"

            dLine = dLine + 1

            Screen , , 0, 0

            Select Case dLine
                Case 5, 6
                    dLine = dLine + 4
                Case 7 To 10
                    dLine = dLine - 2
            End Select

            Call FRAMEOPTION(10, D)
            Print "SELECT PASS COVERAGE "; gameTeams$(D)

            For coverage = 1 To 5
                Locate 11 + coverage, 14
                Print coverage - 1; "- "; passCov$(coverage); Tab(41); coverage + 4; "- "; passCov$(coverage + 5)
            Next coverage

            Locate , 41: Print " B - BLITZ ENGAGE"

            Do
                I$ = GetKeyPress$

                'Main loop condition
                If I$ <> Chr$(27) Then
                    If UCase$(I$) = "B" Then coverage = 10: Exit Do
                    coverage = Val(I$)
                End If

            Loop Until coverage >= 0 And coverage <= 9 Or I$ = Chr$(27)

            If I$ <> Chr$(27) Then
                ' 3065
                coverage = coverage + 1
                Screen , , 0, 0

                'More loop conditions
                If (dLine = 5 Or dLine = 6) And (coverage = 7 Or coverage = 9 Or coverage = 11) Then goodSelection% = 0
                If (dLine = 7 Or dLine = 8) And (coverage = 8 Or coverage = 10 Or coverage = 11) Then goodSelection% = 0

            End If

        Loop Until goodSelection% = 1

    End If

End Sub


'----------------------------------------
'         DEGFOALLINE Subroutine
'----------------------------------------
'This subroutine automatically picks a
'goal line defense when 5 yards are left.
Sub DEFGOALLINE ()

    If ydLine > 94.999 Then dLine = 11: coverage = 1

End Sub


'----------------------------------------
'            LGPBP Subroutine
'----------------------------------------
'This subroutine chooses the play-by-play
'text for the start of a play where any
'long gain has been calculated.
Sub LGPBP ()

    RN = Int(Rnd(1) * 20) + 1

    Select Case RN
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

    RN = Int(Rnd(1) * 29) + 1

    Select Case RN
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

    If tmRat_GAME(gameLoc, 3) <> 1 Then

        Call SETSCREEN

        Call OPTIONBOX(9)

        Print "WEATHER OPTIONS "
        Locate 12, 14
        Locate , 14: Print "0 - GOOD"
        Locate , 14: Print "1 - BAD (RAIN/SNOW)"
        Locate , 14: Print "2 - TERRIBLE (BLIZZARD/HEAVY RAIN)"
        Locate , 14: Print "3 - COMPUTER SELECT"

        If autoPlay = 1 Then
            'read weather from schedule options
            I2 = NG%(20)
        Else

            Do
                I$ = GetKeyPress$
                choice = Val(I$)
            Loop Until choice >= 1 And choice <= 3 Or I$ = "0"

        End If

        Screen , , 0, 0

        'If choice = 0 Then 200

        If choice = 1 Then
            WX%(4) = -3
            WX%(5) = 1
            WX%(6) = -10
        End If

        If choice = 2 Then
            WX%(4) = -6
            WX%(5) = 2
            WX%(6) = -20
        End If

        If choice = 3 Then

            I = Int(Rnd(1) * 22) + 1

            If I = 18 Then
                WX%(4) = -3
                WX%(5) = 1
                WX%(6) = -10
            End If

            If I = 7 Then
                WX%(4) = -6
                WX%(5) = 2
                WX%(6) = -20
            End If

        End If

        Call OPTIONBOX(9)

        Print "WIND OPTIONS "
        Locate 12, 14
        Locate , 14: Print "0 - NONE"
        Locate , 14: Print "1 - LIGHT TO MODERATE"
        Locate , 14: Print "2 - STRONG AND GUSTY"
        Locate , 14: Print "3 - COMPUTER SELECT"

        If autoPlay = 1 Then
            choice = NG%(19)
        Else
            Do
                I$ = GetKeyPress$
                choice = Val(I$)
            Loop Until choice >= 0 And choice <= 3
        End If

        Screen , , 0, 0

        'If choice = 0 Then 299
        If choice = 1 Then
            WX%(1) = -7
            WX%(2) = -5
            WX%(3) = -10
        End If

        If choice = 2 Then
            WX%(1) = -15
            WX%(2) = -15
            WX%(3) = -30
        End If

        If choice = 3 Then

            I = Int(Rnd(1) * 22) + 1

            If I = 14 Then
                WX%(1) = -7
                WX%(2) = -5
                WX%(3) = -10
            End If

            If I = 3 Then
                WX%(1) = -15
                WX%(2) = -15
                WX%(3) = -30
            End If

        End If

    End If

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
            FA(P1, Q7%(P1), 0) = FA(P1, Q7%(P1), 0) + 1

        Case 20 To 29
            FA(P1, Q7%(P1), 1) = FA(P1, Q7%(P1), 1) + 1

        Case 30 To 39
            FA(P1, Q7%(P1), 2) = FA(P1, Q7%(P1), 2) + 1

        Case 40 To 49
            FA(P1, Q7%(P1), 3) = FA(P1, Q7%(P1), 3) + 1

        Case Is >= 50
            FA(P1, Q7%(P1), 4) = FA(P1, Q7%(P1), 4) + 1

    End Select

End Sub


'----------------------------------------
'            FGMADE Subroutine
'----------------------------------------
'This subroutine tracks and increments FG
'made based on yardage increments for
'the current kicker.
Sub FGMADE ()

    Select Case fgAttYds

        Case 1 To 19
            FM(P1, Q7%(P1), 0) = FM(P1, Q7%(P1), 0) + 1

        Case 20 To 29
            FM(P1, Q7%(P1), 1) = FM(P1, Q7%(P1), 1) + 1

        Case 30 To 39
            FM(P1, Q7%(P1), 2) = FM(P1, Q7%(P1), 2) + 1

        Case 40 To 49
            FM(P1, Q7%(P1), 3) = FM(P1, Q7%(P1), 3) + 1

        Case 50 To gameLongFG(P1, Q7%(P1))
            FM(P1, Q7%(P1), 4) = FM(P1, Q7%(P1), 4) + 1

        Case Is > gameLongFG(P1, Q7%(P1))
            gameLongFG(P1, Q7%(P1)) = fgAttYds

    End Select

End Sub


'----------------------------------------
'           ComputerOffense Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'selecting their offensive choices.
Sub ComputerOffense ()

    If endZone = 0 Then

        '4th Quarter
        'Any down, Less than 35 seconds to go, 0 Timeouts Opponent, Offense leading
        If quarter = 4 And gameClock! <= 35 * (4 - currDown) And timeouts(D) = 0 And score(P1, 0) > score(D, 0) Then

            P2 = 9
            R1 = 1
            qbTakeKnee = 1

        Else

            'If not inside 2 minute warning and past Own 40 Yd Line
            If Not (quarter = 2 And gameClock! <= 120 And ydLine > 40 Or quarter >= 4 And gameClock! <= 120) Then

                '4th Quarter+,
                '4th Down, Opponent's side of field, Score = Down by 11 or more
                If currDown = 4 And quarter >= 4 And ydLine >= 50 And score(D, 0) - score(P1, 0) > 10 Then 8833

                '3rd Quarter+,
                '4th Down, Inside opponent's 45, Score = Down by 15 or more
                If currDown = 4 And quarter >= 3 And ydLine >= 55 And score(D, 0) - score(P1, 0) > 14 Then 8833

                '4th Down, 1 Yd To Go, 3 Pt. Lead, Opponent's 32 to 38, Score Not Tied
                If currDown = 4 And Y1 <= 1 And score(P1, 0) - score(D, 0) <= 3 And score(P1, 0) - score(D, 0) > 0 And ydLine >= 62 + goalPostAdj And ydLine <= 68 + goalPostAdj Then 8827

                '4th Quarter+
                '---------------
                '4th Down, Opponent's 32 to 38, Score = 3 Pt. Margin, Pro Rules or College Rules + Team after 1960 = Field Goal
                If quarter >= 4 And currDown = 4 And score(P1, 0) - score(D, 0) <= 3 And score(P1, 0) - score(D, 0) >= -3 And ydLine >= 62 + goalPostAdj And ydLine < 68 + goalPostAdj And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20: GoTo 8800

                '4th Down, Opponent's 32 to 38, Score <= 3 Pt. Lead, Pro Rules or College Rules + Team after 1960 = Field Goal
                If quarter >= 4 And currDown = 4 And ydLine >= 62 + goalPostAdj And ydLine < 68 + goalPostAdj And score(D, 0) - score(P1, 0) <= 3 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20: GoTo 8800

                '4th Down, <= 2 Yds to Go, Opponent's 25 - 35, Score - Down by 11 or more
                If quarter >= 4 And ydLine >= 65 + goalPostAdj And ydLine < 75 + goalPostAdj And Y1 <= 2 And score(D, 0) - score(P1, 0) > 10 Then 8827

                '4th Down, <= 2 Yds to Go, Opponent's 25 - 35, Score - Down by 10 or less, 4 Minutes Remaining, Pro Rules or College Rules + Team after 1960 = Field Goal
                If quarter >= 4 And ydLine >= 65 + goalPostAdj And ydLine < 75 + goalPostAdj And score(D, 0) - score(P1, 0) <= 10 And score(D, 0) - score(P1, 0) > 0 And currDown = 4 And gameClock! < 240 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20: GoTo 8800



                '4th Quarter Only
                '---------------
                '4th Down, More Than 2 Minutes, Score - Down by 4 to 10, Inside Red Zone = Field Goal
                If currDown = 4 And quarter = 4 And gameClock! > 240 And score(D, 0) - score(P1, 0) >= 4 And score(D, 0) - score(P1, 0) <= 10 And ydLine > 80 + goalPostAdj Then P2 = 20: GoTo 8800

                '4th Down, Down by 4 or More, Inside Red Zone
                If currDown = 4 And quarter = 4 And score(D, 0) - score(P1, 0) >= 4 And ydLine >= 80 + goalPostAdj Then 8833


                'Any Quarter
                '---------------
                '4th Down, Any Time, <= 2 Yds to Go, Inside Opponent's 40
                If currDown = 4 And ydLine >= 60 And Y1 <= 2 Then 8827

                '4th Down, Any Time, >= 3 Yds To Go, Inside Opponent's 32, Pro Rules or College Rules + Team after 1960 = Field Goal
                If currDown = 4 And ydLine >= 68 + goalPostAdj And Y1 > 2 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20: GoTo 8800

                '4th Down, Any Time, Outside Opponent's 32 = Punt
                If currDown = 4 And ydLine < 68 + goalPostAdj Then P2 = 19: GoTo 8800

                Call d100(R0)

                '-- Several of these could be triggered
                '-- during a 2-point conversion
                'Any Time, Inside Team's Own 8 Yd Line
                If ydLine <= 8 Then 8827

                '2nd Down, Any Time, <=12 Yds To Go
                If leagRat_GAME(P1, 7) > 75 And currDown = 2 And Y1 <= 12 Then 8827

                '3rd Down, Any Time, <= 5 Yds to Go
                If leagRat_GAME(P1, 7) > 75 And currDown = 3 And Y1 <= 5 Then 8827

                '1st or 2nd Down, Any Time
                If currDown = 1 And leagRat_GAME(P1, 7) >= 70 Or currDown <= 2 And leagRat_GAME(P1, 7) >= 80 Then 8827

                '1st Down, Any Time, >= 11 Yds To Go
                If currDown = 1 And Y1 > 10 Then 8833

                '2nd Down, Any Time, >= 11 Yds To Go
                If currDown = 2 And Y1 > 10 Then 8833

                '2nd Down, Any Time, <= 4 Yds to Go
                If currDown = 2 And Y1 < 5 Then 8827

                '3nd Down, Any Time, >= 4 Yds to Go
                If currDown = 3 And Y1 >= 4 Then 8833

                '3rd Down, Any Time, 1 Yds to Go
                '---> this could be triggered during a 2 Pt Conversion
                If currDown = 3 And Y1 < 2 Then 8827

                '4th Down, Any Time, >= 4 Yds to Go
                If currDown = 4 And Y1 >= 4 Then 8833

                '4th Down, Any Time, 1 Yds To Go
                If currDown = 4 And Y1 < 2 Then 8827

                '4th Quarter
                '---------------
                'Less than 8 Minutes in Game
                'Either team is ahead by 9 or more pts
                If quarter = 4 And gameClock! <= 480 And score(D, 0) - score(P1, 0) >= 9 And R0 >= leagRat_GAME(P1, 7) - leagRat_GAME(P1, 7) / 2 Then 8833
                If quarter = 4 And gameClock! <= 480 And score(P1, 0) - score(D, 0) >= 9 And R0 >= leagRat_GAME(P1, 7) + leagRat_GAME(P1, 7) / 2 Then 8833


                If R0 > leagRat_GAME(P1, 7) Then 8833


                '----- DECISION BRANCH 1 -----
                8827 '
                If gameStats(P1, 1) + gameStats(P1, 3) <> 0 Then
                    If gameStats(P1, 1) / (gameStats(P1, 1) + gameStats(P1, 3)) > leagRat_GAME(P1, 7) / 100 And leagRat_GAME(P1, 7) < 50 Then 8833
                End If

                Call d100(R0)

                Select Case R0

                    'Long Pass
                    Case 1 To 2: If leagRat_GAME(P1, 7) < 75 Then P2 = 15

                        'Med Pass
                    Case 3 To 5: If leagRat_GAME(P1, 7) < 75 Then P2 = 14

                        'Option
                    Case 6 To 10: P2 = 5

                        'Sweep
                    Case 11 To 20: P2 = 4

                        'Trap
                    Case 21 To 30: P2 = 7

                        'Off Tackle
                    Case 31 To 50: P2 = 3

                        'Dive
                    Case 51 To 70: P2 = 2

                        'Blast
                    Case 71 To 100: P2 = 1

                End Select

                GoTo 8800



                '----- DECISION BRANCH 2 -----
                8833 '
                Call d100(R0)

                Select Case R0

                    ' Draw
                    Case 1 To 5: P2 = 6

                        ' Rollout
                    Case 6 To 30: P2 = 17

                        ' 3-Step Drop
                    Case 31 To 36: P2 = 9

                        ' Screen
                    Case 37 To 44: P2 = 13

                        ' Long Pass
                    Case 45 To 63: P2 = 12

                        ' Medium Pass
                    Case 64 To 89: P2 = 11

                        ' Short Pass
                    Case 90 To 100: P2 = 10

                End Select

                '3rd / 4th Down, Any Time, >= 5 Yds To Go
                '----------------------------------------
                '-- If Draw was selected, change to Long Pass
                If currDown >= 3 And Y1 >= 5 And P2 = 6 Then P2 = 12

                '-- If Short Pass was selected, change to Medium Pass
                If currDown >= 3 And Y1 >= 5 And P2 = 10 Then P2 = 11

                '-- If 3-Step Drop or screen was selected, change to Rollout
                If currDown >= 3 And Y1 >= 5 And (P2 = 13 Or P2 = 9) Then P2 = 17

                'Any Down, Any Time, Inside Opponent's 10 or < 7 Yds To Go
                '----------------------------------------
                '-- If Long Pass was selected, change to Medium Pass
                If ydLine > 90 And (P2 = 12 Or P2 = 15) Or Y1 < 7 And (P2 = 12 Or P2 = 15) Then P2 = 11

                GoTo 8800

            Else
                '2nd / 4th Quarter
                '2 Min Warning
                '--------------------
                'Score - Down by >= 4
                If score(D, 0) - score(P1, 0) > 3 Then
                    If quarter >= 4 Then 8880
                    If quarter = 2 Then 8885
                End If

                'Score Tied or Down
                If score(D, 0) - score(P1, 0) >= 0 Then 8885

                Call d100(R0)

                '2nd Quarter, Past Own 40 Yd Line
                If ydLine > 40 And quarter = 2 Then 8885

                P2 = 4

                If R0 <= 60 Then P2 = 3

                '2nd Quarter / Overtime / 4th Quarter and Tied Score
                'Any down, Inside Opponent's 35, Less than 22 seconds to go = Field Goal
                If ydLine >= 65 + goalPostAdj And gameClock! <= 22 And quarter = 2 Or ydLine >= 65 + goalPostAdj And gameClock! <= 22 And quarter > 4 Or ydLine >= 65 + goalPostAdj And gameClock! <= 22 And quarter = 4 And score(P1, 0) - score(D, 0) = 0 Then P2 = 20

                '4th Down, Inside Opponent's 35, >= 1 Yard to go, Pro Rules or College Rules + Team after 1960 = Field Goal
                If ydLine >= 65 + goalPostAdj And currDown = 4 And Y1 >= 2 And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20

                '4th Down, Outside Opponents' 35 = Punt
                If currDown = 4 And ydLine <= 65 + goalPostAdj Then P2 = 19

            End If

            '----- DECISION MADE -----
            8880 '
            '2nd Quarter
            '4th Down, Any Time
            If quarter = 2 And currDown = 4 Then

                If ydLine < 65 + goalPostAdj Then
                    'Outside Opponent's 35 = Punt
                    P2 = 19
                Else
                    'Inside Opponent's 35, Pro Rules, or College Rules + Team after 1960 = Field Goal
                    '--- COMPARE A
                    If (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then
                        P2 = 20
                    End If
                End If

                GoTo 8800

            End If

            'Any Quarter
            '---------------
            'Any Down, Less than 22 seconds to go, 0 Timeouts Offense = Pass
            If gameClock! <= 22 And timeouts(P1) = 0 Then P2 = 12: GoTo 8800

            'Any Down, Less than 12 sceonds to go, 1 or more timeouts Offense = Pass
            If gameClock! <= 12 And timeouts(P1) <> 0 Then P2 = 12: GoTo 8800

            GoTo 8892

            '-----

            8885 '
            '2nd / 4th Quarter
            '4th Down, Any Time, Outside Opponent's 35, Score Tied = Punt
            If quarter = 2 And currDown = 4 And ydLine < 65 + goalPostAdj Or quarter >= 4 And ydLine < 65 + goalPostAdj And score(P1, 0) = score(D, 0) And currDown = 4 Then P2 = 19: GoTo 8800


            '2nd Quarter Only
            '4th Down, Any Time, Inside Opponent's 35, Pro Rules or College Rules + Team after 1960 = FIeld Goal
            '--- COMPARE B
            If quarter = 2 And currDown = 4 And ydLine >= 65 + goalPostAdj And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20: GoTo 8800


            'Any Quarter
            '---------------
            'Any Down, Less than 37 seconds to go, 0 Timeouts Offense, Outside Opponent's 35 = Pass (???)
            If gameClock! <= 37 And timeouts(P1) = 0 And ydLine < 65 + goalPostAdj Then P2 = 12: GoTo 8800

            'Any Down, Less than 22 seconds to go, 0 Timeouts Offense, Outside opponent's 35 = Pass
            If gameClock! <= 12 And timeouts(P1) <> 0 And ydLine < 65 + goalPostAdj Then P2 = 12: GoTo 8800

            'Any Down, Less than 22 seconds to go, 0 Timeouts Offense, Inside Opponent's 35 = Field Goal
            If gameClock! <= 22 And timeouts(P1) = 0 And ydLine >= 65 + goalPostAdj Then P2 = 20: GoTo 8800

            'Any Down, Less than 12 seconds to go, 0 Timeouts Offense, Inside Opponent's 35 = Field Goal
            '--- This is essentially the same as above ???
            If gameClock! <= 12 And timeouts(P1) <> 0 And ydLine >= 65 + goalPostAdj Then P2 = 20: GoTo 8800

            '4th Down, Any Time, Inside Opponent's 35, Pro Rules or College Rules + Team after 1960 = Field Goal
            If currDown = 4 And ydLine >= 65 + goalPostAdj And (ruleOptType <> 0 Or ruleOptType = 0 And teamYears(1) > 1960 And teamYears(0) > 1960) Then P2 = 20: GoTo 8800

            8892 '
            Call d100(R0)

            'Sideline pass
            P2 = 16

            '50% random chance of Short/Med/Long Pass
            If R0 < 50 Then P2 = Int(Rnd(1) * 3) + 10

            'Inside Opponent's 10 = Medium Pass
            If ydLine >= 90 Then P2 = 11

            'In the Red Zone, Score - Down by <3 ---or--- Inside Opponent's 10 and Down by 3
            'Random choice of Blast / Dive / Off Tackle / Sweep
            If ydLine > 80 And score(D, 0) - score(P1, 0) < 3 Or ydLine > 90 And score(D, 0) - score(P1, 0) = 3 Then P2 = Int(Rnd(1) * 4) + 1

        End If


        'Final Checks
        '-----

        8800 '
        'Overtime and 4th Down
        'Change to Long Pass
        If quarter > 4 And NG%(14) = 3 And currDown = 4 Then

            'Punt chosen but score tied
            If P2 = 19 And score(D, 0) = score(P1, 0) Then P2 = 12

            'Punt chosen, Score - Ahead by 3 or less
            '(ie, if we punt and give the team the ball,
            'they could win by a FG; let's go for it
            If P2 = 19 And score(D, 0) - score(P1, 0) <= 3 Then P2 = 12

            'Punt or FG chosen, Down by 4 or more
            '(ie, we have to try and get a 1st down)
            If (P2 = 19 Or P2 = 20) And score(D, 0) - score(P1, 0) > 3 Then P2 = 12

        End If


    Else

        'We should only be here if going for 2
        Call d100(R0)

        Select Case R0

            'Rollout Pass
            Case 1 To 25: P2 = 17

                ' Medium Pass
            Case 26 To 50: P2 = 11

                ' Short Pass
            Case 51 To 70: P2 = 10

                ' Option
            Case 71 To 80: P2 = 5

                ' Sweep
            Case 81 To 90: P2 = 4

                ' Off Tackle
            Case 91 To 100: P2 = 3

        End Select

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

    'Are these numbers accurate? I'm not confident they are
    'I believe we're converting these numbers as yards to
    'feet / inches???
    '1 yard = 36 inches
    Select Case ydsGained - Y1

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

    If I3 > 9 Then pbpString$ = pbpString$ + wdRec$(P1, I3 - 10) Else pbpString$ = pbpString$ + rbacks$(P1, I3)

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
        gameStats(P1, 12) = gameStats(P1, 12) + ydLine / 2
    End If

    gameStats(P1, 11) = gameStats(P1, 11) + 1

    If ydLine >= 20 Then
        ydLine = ydLine - 10
        gameStats(P1, 12) = gameStats(P1, 12) + 10
    End If

End Sub


'----------------------------------------
'          CHANGEQB Subroutine
'----------------------------------------
'This subroutine gives the user a menu for
'choosing a quarterback and sets up the
'corresponding arrays for the game.
Sub CHANGEQB ()

    Color 15, 0
    Cls

    Call TEAMBOX(12, I3)

    useRandomQB(I3) = 0

    Locate , 31
    Print "SELECT QUARTERBACK"
    Print
    Locate , 18: Print " # QB NAME    % OF  COMP  INT"
    Locate , 18: Print "              ATTS  PCT   PCT  ARM"

    For I = 0 To 3

        If qbacks$(I3, I) <> "XXX" And qbacks$(I3, I) <> "END" And qbacks$(I3, I) <> "" Then

            If I = 0 Then

                Locate , 18: Print I; "-"; qbacks$(I3, I);
                Locate , 32
                If I = 0 Then
                    Print qbNumAtt(I3, I);
                Else
                    Print Int(qbNumAtt(I3, I)) - qbNumAtt(I3, I - 1);
                End If

                Locate , 38: Print qbCompPct(I3, I);
                Locate , 44: Print qbPctInt(I3, I);
                Locate , 49: Print qbArmRat(I3, I)

            Else

                If qbNumAtt(I3, I - 1) < 100 Then

                    Locate , 18: Print I; "-"; qbacks$(I3, I);
                    Locate , 32

                    If I = 0 Then
                        Print qbNumAtt(I3, I);
                    Else
                        Print Int(qbNumAtt(I3, I)) - qbNumAtt(I3, I - 1);
                    End If

                    Locate , 38: Print qbCompPct(I3, I);
                    Locate , 44: Print qbPctInt(I3, I);
                    Locate , 49: Print qbArmRat(I3, I)

                End If

                Locate , 1

            End If

        End If

    Next I

    Locate , 19: Print "4 -COMPUTER SELECT PASSER EACH PLAY"
    Print
    Locate , 19: Print "SELECT (0-4) ";
    Color 15, 0

    Do
        I$ = GetKeyPress$
        choice = Val(I$)
    Loop Until choice >= 0 And choice <= 4

    If choice <= 3 Then qbNumber(I3) = choice

    If choice = 4 Then useRandomQB(I3) = 1

    If ruleOptType <> 0 And choice <= 3 And useRandomQB(I3) <> 1 Then

        Call TEAMBOX(12, I3)

        Locate 8, 15: Print "ENTER "; qbacks$(I3, choice); " MOBILITY RATING (1-9)"
        Print
        Locate , 16: Print "THIS IS HOW OFTEN HE RUNS OUT OF THE POCKET"
        Locate , 16: Print "SOME QB'S LIKE MARINO & FOUTS ALMOST NEVER DO"
        Locate , 16: Print "OTHERS LIKE ELWAY & TARKENTON WILL TAKE OFF"
        Locate , 16: Print "IF UNSURE SIMPLY TYPE 5"
        Locate , 16: Print "        1-LOW, 9-HIGH"

        Color 15, 0

        Do
            I$ = GetKeyPress$
            qbMobility(I3) = Val(I$)

        Loop Until qbMobility(I3) >= 1 And qbMobility(I3) <= 9

    End If

    For I = 0 To 3:
        V4%(I3, I) = -1
    Next I

    If useRandomQB(I3) <> 1 Then

        For I4 = 0 To 9:

            For I5 = 0 To 3
                If rbacks$(I3, I4) = qbacks$(I3, I5) And I5 <> qbNumber(I3) Then V4%(I3, I5) = I4
            Next I5

        Next I4

    End If

End Sub


'----------------------------------------
'           KICKOFF Subroutine
'----------------------------------------
'This subroutine gives the user choices
'to handle a pro kick off.
Sub KICKOFF ()

    Call SCOREBOARD

    If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then

        I9 = 2

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
            I9 = Val(I$)
        Loop Until I9 >= 1 And I9 <= 4

    End If

    Screen , , 0, 0

    If I9 = 1 Or I9 = 2 Then

        Call SCOREBOARD

        If playerMode = 1 And D = compTeam Or playerMode = 2 Then
            I1 = 1
        Else
            Call OPTIONBOX(7)

            Print gameTeams$(1 - winTeam); " DO YOU ELECT TO:"
            Locate 12, 14
            Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
            Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

            Do
                I$ = GetKeyPress$
                I1 = Val(I$)
            Loop Until I1 >= 1 And I1 <= 2

        End If

        Screen , , 0, 0
        WX%(0) = D

        If I1 = 2 Then WX%(0) = 1 - D

        If I9 = 1 Then
            Call CHANGEPOSS
            winTeam = 1 - winTeam
        End If

    End If

    If I9 = 3 Or I9 = 4 Then

        If I9 = 3 Then WX%(0) = P1
        If I9 = 4 Then WX%(0) = D

        Swap P1, D

        Call SCOREBOARD

        If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then
            I1 = 2
        Else
            Call OPTIONBOX(7)
            Print gameTeams$(1 - winTeam); " DO YOU ELECT TO:"
            Locate 12, 14
            Locate , 14: Print "1 - KICK OFF"
            Locate , 14: Print "2 - RECEIVE"

            Do
                I$ = GetKeyPress$
                I1 = Val(I$)
            Loop Until I1 >= 1 And I1 <= 2

        End If

        Screen , , 0, 0
        If I1 = 1 Then Call CHANGEPOSS

    End If

End Sub


'----------------------------------------
'         COLKICKOFF Subroutine
'----------------------------------------
'This subroutine gives the user choices
'to handle a college kick off.
Sub COLKICKOFF ()

    If quarter > 2 Then
        Call KICKOFF
    Else

        Call SCOREBOARD

        If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then
            I9 = 2
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
                I9 = Val(I$)
            Loop Until I9 >= 1 And I9 <= 5

        End If

        Screen , , 0, 0

        Select Case I9

            Case 1, 2

                Call SCOREBOARD

                If playerMode = 1 And D = compTeam Or playerMode = 2 Then
                    I1 = 1
                Else
                    Call OPTIONBOX(7)
                    Print gameTeams$(D); " DO YOU ELECT TO:"
                    Locate 12, 14
                    Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
                    Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

                    Do
                        I$ = GetKeyPress$
                        I1 = Val(I$)
                    Loop Until I1 >= 1 And I1 <= 2
                End If

                Screen , , 0, 0
                WX%(0) = D

                If I1 = 2 Then WX%(0) = 1 - D
                If I9 = 1 Then Call CHANGEPOSS: winTeam = 1 - winTeam

            Case 3, 4

                If I9 = 3 Then WX%(0) = P1
                If I9 = 4 Then WX%(0) = D

                Swap P1, D

                Call SCOREBOARD

                If playerMode = 1 And P1 = compTeam Or playerMode = 2 Then
                    I1 = 2
                Else
                    Call OPTIONBOX(7)
                    Print gameTeams$(P1); " DO YOU ELECT TO:"

                    Locate 12, 14
                    Locate , 14: Print "1 - KICK OFF"
                    Locate , 14: Print "2 - RECEIVE"

                    Do
                        I$ = GetKeyPress$
                        I1 = Val(I$)
                    Loop Until I1 >= 1 And I1 <= 2
                End If

                Screen , , 0, 0

                If I1 = 1 Then Call CHANGEPOSS

            Case 5
                Swap P1, D
                winTeam = P1
                Call KICKOFF

        End Select

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
    gameStats(P1, 1) = gameStats(P1, 1) + 1
    gameStats(P1, 2) = gameStats(P1, 2) + ydsGained

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
    RN = Int(Rnd(1) * teamInts(1 - P1)) + 1

    For Z1 = 0 To 9
        M5 = M5 + playerInts(1 - P1, Z1)
        If RN <= M5 Then defTeam$ = defInts$(1 - P1, Z1): Exit For
    Next

End Sub


'----------------------------------------
'           YARDAGE Subroutine
'----------------------------------------
'This subroutine I believe is used to
'round yardage gained
'-0.4 to 0.6
Sub YARDAGE ()

    ydsGained = ydsGained + (Int(Rnd(1) * 11) - 5) / 10

End Sub


'----------------------------------------
'          PUNTYARDS Subroutine
'----------------------------------------
'This subroutine handles the calculations
'for punt yardage and who receives the
'punt.
Sub PUNTYARDS ()

    Call d100(I3)
    ydLine = ydLine + ydsGained

    Call CHANGEPOSS

    ydsToScore = ydLine
    ydLine = 100 - ydLine

    Call YARDLINE

    RN = Int(Rnd(1) * 1000) + 1

    Select Case prYdsPerRet(P1, puntRetNbr)

        Case 1 To 4

            Select Case RN

                Case 1 To 50
                    ydsGained = Int((Rnd(1) * 6) - 5)

                Case 51 To 899
                    ydsGained = Int((Rnd(1) * 14) + 0) + (prYdsPerRet(P1, puntRetNbr) - 3) * 3

                Case 900 To 988
                    If I2 <> 1 Then ydsGained = Int((Rnd(1) * 39) + 12) + (prYdsPerRet(P1, puntRetNbr) - 3) * 3

                Case 989 To 1000
                    If I2 <> 1 Then ydsGained = 110

            End Select

        Case 5

            Select Case RN

                Case 1 To 900
                    ydsGained = Int((Rnd(1) * 25) + 0)

                Case 901 To 940
                    ydsGained = Int((Rnd(1) * 39) + 12)

                Case 941 To 1000
                    If I2 <> 1 Then ydsGained = 110

            End Select

        Case Is >= 6

            If RN > 900 And I2 <> 1 Then
                ydsGained = 110
            Else
                ydsGained = Int((Rnd(1) * 26) + 0) + (prYdsPerRet(P1, puntRetNbr) - 6) * 3
            End If

    End Select

    X1 = 4
    Call YARDAGE
    Call MARKER

    pbpString$ = "THE BALL IS TAKEN BY " + pret$(P1, puntRetNbr) + " AT THE" + Str$(Int(ydLineTeam))
    Call PBP

    startYds = 100 - ydLine
    endYds = startYds - ydsGained

    Call MOVEBALL

End Sub


'----------------------------------------
'          FAKEYARDS Subroutine
'----------------------------------------
'This subroutine handles yardage gains and
'Play-by-Play text for a fake play.
Sub FAKEYARDS (choice)

    pbpString$ = "IT'S A FAKE!!! THE UPBACK IS RUNNING THE BALL"
    Call PBP

    Call d100(RN)

    Select Case choice

        Case 0

            If RN <= 36 Then
                ydsGained = Int(Rnd(1) * 27)
            Else
                ydsGained = Int(Rnd(1) * 6) - 2
            End If

        Case 1

            Select Case RN
                Case 1 To 36
                    ydsGained = Int(Rnd(1) * 21)

                Case 37 To 90
                    ydsGained = Int(Rnd(1) * 11) - 4

                Case 91 To 100
                    ydsGained = 100 - ydLine
            End Select

        Case 2
            If RN <= 8 Then
                ydsGained = Int(Rnd(1) * 21)
            Else
                ydsGained = Int(Rnd(1) * 7) - 7
            End If

    End Select

    Call YARDAGE

    O = 1

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

    Call d100(F8)

    If revFumble = 1 Then

        F8 = F8 - 10
        revFumble = 0

    Else

        If F% = 1 Then F8 = F8 - 6
        If F% = 2 Then F8 = F8 + 1
        If F% = 4 Then F8 = F8 - 2
        If F% = 5 Then F8 = F8 + 2

        If P2 = 1 Then F8 = F8 + 2
        If P2 = 2 Then F8 = F8 - 1
        If P2 = 5 Then F8 = F8 - 1

    End If

    F8 = F8 - WX%(5)
    F% = 0

    If F8 <= leagRat_GAME(P1, 3) + leagRat_GAME(D, 4) Then ballFumbled = 1

End Sub


'----------------------------------------
'            FLEA Subroutine
'----------------------------------------
'This subroutine handles yardage and the
'Play-by-Play text for a flea flicker.
Sub FLEA ()

    I1 = qbNumber(P1)

    catchPctSuccess = qbCompPct(P1, qbNumber(P1)) + Int(leagRat_GAME(D, 1)) - 4

    intChance = qbPctInt(P1, qbNumber(P1)) + leagRat_GAME(D, 2) + 5

    ffPctSuccess = 5
    If coverage = 7 Or coverage = 8 Then ffPctSuccess = 30
    If coverage = 9 Or coverage = 10 Then ffPctSuccess = 50
    If coverage = 11 Then ffPctSuccess = 70

    Call d100(RN)

    If RN <= 25 Then
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

    Call d100(RN)

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

    Call d100(RN)

    If RV%(P9) = 0 Then

        Select Case RN

            Case 1 To 20
                ydsGained = (Int(Rnd(1) * 8) + 5) * -1
                revFumble = 1

            Case 21 To 80
                ydsGained = Int(Rnd(1) * 11)

            Case 81 To 100
                ydsGained = Int(Rnd(1) * 81)

        End Select

    Else

        Select Case RN

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

    Call d100(RN)

    Select Case choice
        Case 1
            Select Case RN
                Case 1 To 20: ydsGained = Int(Rnd(1) * 21)
                Case 21 To 95: ydsGained = Int(Rnd(1) * 11) - 4
                Case 96 To 100: ydsGained = 100 - ydLine
            End Select

        Case 2
            If RN <= 8 Then
                ydsGained = Int(Rnd(1) * 21)
            Else
                ydsGained = Int(Rnd(1) * 7) - 7
            End If

    End Select

    Call YARDAGE

    O = 1

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

    If P2 = 4 Or P2 = 5 Then

        kickDir = Int(Rnd(1) * 2)

        If rbacks$(P1, I1) = qbacks$(P1, qbNumber(P1)) Then

            pbpString$ = pbpString$ + " AND KEEPS IT ON THE " + playDesc$(chosenPlay)
            Call PBP

        Else
            RN = Int(Rnd(1) * 2) + 1

            If RN = 1 Then
                pbpString$ = pbpString$ + " AND TOSSES OUT TO " + rbacks$(P1, I1)
            Else
                'RN = 2
                pbpString$ = pbpString$ + " AND PITCHES OUT TO " + rbacks$(P1, I1)
            End If

            Call PBP

            pbpString$ = rbacks$(P1, I1) + " GOES AROUND THE " + direction$(kickDir) + " END"
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
Sub PASSBALL ()

    S6 = 0

    Call QBLINE

    Call d100(I)

    R1 = 2

    For J = 3 To 0 Step -1
        If I <= qbNumAtt(P1, J) Then I1 = J
    Next J

    If useRandomQB(P1) = 1 Then
        qbNumber(P1) = I1
    Else
        I1 = qbNumber(P1)
    End If

    Do

        Call d100(RN2)
        newRandomNum% = 0

        For J = 15 To 10 Step -1
            If RN2 <= wrNumRec(P1, J - 10) Then I3 = J
        Next J

        For J = 9 To 0 Step -1
            If RN2 <= rbNumRec(P1, J) Then I3 = J
        Next J

        If I3 > 9 Then
            If wdRec$(P1, I3 - 10) = qbacks$(P1, qbNumber(P1)) Then newRandomNum% = 1
        End If

        If I3 < 10 Then
            If rbacks$(P1, I3) = qbacks$(P1, qbNumber(P1)) Then newRandomNum% = 1
        End If

    Loop Until newRandomNum% = 0

    If dLine = 11 Then
        pbpString$ = defFormation$(dLine)
    Else
        pbpString$ = defFormation$(dLine) + " " + passCov$(coverage)
    End If

    Call PBP
    Call PASSPBP

    If I3 > 9 Then targetWRName$ = wdRec$(P1, I3 - 10) Else targetWRName$ = rbacks$(P1, I3)

    RN = Int(Rnd(1) * 6) + 1

    Select Case RN
        Case 1: pbpString$ = "HE'S GOT "
        Case 2: pbpString$ = "HE SPOTS "
        Case 3: pbpString$ = "HE'S LOOKING FOR "
        Case 4: pbpString$ = "HE SEES "
        Case 5: pbpString$ = "HE'S SEARCHING FOR "
        Case 6: pbpString$ = "HE'S GOT "
    End Select

    Call WHICHRECEIVER

    I = Int(Rnd(1) * 5) + 1
    kickDir = Int(Rnd(1) * 2)

    Select Case I
        Case 1, 6
            pbpString$ = pbpString$ + " IN THE OPEN"
            Call PBP

        Case 2
            pbpString$ = pbpString$ + " ON THE SIDELINE"
            Call PBP

            'I'm not confident the below is accurate
            'I think Case 3 should also run code for 4/5
            'And Case 4 should also run code for 5
        Case 3
            If P2 <> 13 Then pbpString$ = pbpString$ + " OVER THE MIDDLE": Call PBP

        Case 4
            If P2 <> 13 Then pbpString$ = pbpString$ + " DOWNFIELD": Call PBP

        Case 5
            If P2 = 12 Or P2 = 15 Then pbpString$ = pbpString$ + " DEEP": Call PBP

    End Select

    Call d100(I)

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
    passTypeCmpAdj = S1%(1, P2 - 8, coverage)

    'Adjust for wind direction NOT in team's favor
    windCmpAdj = 0

    If WX%(0) <> P1 Then

        If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18) Then
            windCmpAdj = WX%(2)
        End If

        If (P2 = 12 Or P2 = 15) Then
            windCmpAdj = WX%(3) + WX%(6)
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
    If X1 = 5 Or X1 = 12 Then K1(nbrScores, 2) = Z1

    K1(nbrScores, 3) = I3
    If K1(nbrScores, 5) < 1 Then K1(nbrScores, 5) = 1

End Sub


'----------------------------------------
'          COMPILER Subroutine
'----------------------------------------
' ...explanation...
Sub COMPILER ()

    If X >= 1 And X <= 4 Then

        Select Case X

            Case 1, 4
                gameStats(P1, 1) = gameStats(P1, 1) + 1
                If ydsGained + ydLine < 99.95 Then
                    gameStats(P1, 2) = gameStats(P1, 2) + ydsGained
                Else
                    gameStats(P1, 2) = gameStats(P1, 2) + 100 - ydLine
                End If

            Case 2
                gameStats(P1, 3) = gameStats(P1, 3) + 1

            Case 3
                gameStats(P1, 3) = gameStats(P1, 3) + 1
                gameStats(P1, 4) = gameStats(P1, 4) + 1

                If ydsGained + ydLine < 99.95 Then
                    gameStats(P1, 6) = gameStats(P1, 6) + ydsGained
                Else
                    gameStats(P1, 6) = gameStats(P1, 6) + 100 - ydLine
                End If

        End Select


        Select Case X

            Case 1

                K2(P1, I1, 0) = K2(P1, I1, 0) + 1

                If ydsGained + ydLine >= 99.95 Then
                    K2(P1, I1, 1) = K2(P1, I1, 1) + 100 - ydLine
                    K2(P1, I1, 2) = K2(P1, I1, 2) + 1
                    BG = 100 - ydLine
                    If BG > runBacks(P1, I1) Then
                        gameRB$(P1, I1) = "t"
                        runBacks(P1, I1) = BG
                    End If
                Else
                    K2(P1, I1, 1) = K2(P1, I1, 1) + ydsGained
                    If ydsGained > runBacks(P1, I1) Then
                        gameRB$(P1, I1) = ""
                        runBacks(P1, I1) = ydsGained
                    End If
                End If

                If HB% = 1 Then HB% = 0
                X = 0

            Case 2

                If HB% = 0 Then K2(P1, I1, 9) = K2(P1, I1, 9) + 1
                If HB% = 1 Then HB% = 0
                X = 0

            Case 3

                If HB% <> 1 Then

                    K2(P1, I1, 9) = K2(P1, I1, 9) + 1
                    K2(P1, I1, 10) = K2(P1, I1, 10) + 1

                    If ydsGained + ydLine >= 99.95 Then
                        K2(P1, I1, 12) = K2(P1, I1, 12) + 100 - ydLine
                        K2(P1, I1, 13) = K2(P1, I1, 13) + 1
                    Else
                        K2(P1, I1, 12) = K2(P1, I1, 12) + ydsGained
                    End If

                End If

                If I3 > 9 Then

                    If ydsGained + ydLine >= 99.95 Then
                        K2(P1, I3 - 10, 7) = K2(P1, I3 - 10, 7) + 100 - ydLine
                        K2(P1, I3 - 10, 8) = K2(P1, I3 - 10, 8) + 1
                    Else
                        K2(P1, I3 - 10, 7) = K2(P1, I3 - 10, 7) + ydsGained
                    End If

                    K2(P1, I3 - 10, 6) = K2(P1, I3 - 10, 6) + 1

                Else

                    If ydsGained + ydLine >= 99.95 Then
                        K2(P1, I3, 4) = K2(P1, I3, 4) + 100 - ydLine
                        K2(P1, I3, 5) = K2(P1, I3, 5) + 1
                    Else
                        K2(P1, I3, 4) = K2(P1, I3, 4) + ydsGained
                    End If

                    K2(P1, I3, 3) = K2(P1, I3, 3) + 1

                End If

                If HB% = 1 Then HB% = 0
                X = 0

            Case 4
                If W5 = 0 Or W5 = 1 And ruleOptType = 0 Then
                    K2(P1, qbNumber(P1) + 10, 0) = K2(P1, qbNumber(P1) + 10, 0) + 1
                End If

                If W5 = 0 And ydsGained + ydLine < 99.95 Or W5 = 1 And ruleOptType = 0 And ydsGained + ydLine < 99.95 Then
                    K2(P1, qbNumber(P1) + 10, 1) = K2(P1, qbNumber(P1) + 10, 1) + ydsGained
                End If

                If W5 = 0 And ydsGained + ydLine >= 99.95 Or W5 = 1 And ruleOptType = 0 And ydsGained + ydLine >= 99.95 Then
                    K2(P1, qbNumber(P1) + 10, 1) = K2(P1, qbNumber(P1) + 10, 1) + 100 - ydLine
                    K2(P1, qbNumber(P1) + 10, 2) = K2(P1, qbNumber(P1) + 10, 2) + 1
                End If

                If W5 = 1 Then
                    W6(P1, 0) = W6(P1, 0) + 1
                    W6(P1, 1) = W6(P1, 1) + ydsGained * -1
                    gameSacks(1 - P1, Z1, 0) = gameSacks(1 - P1, Z1, 0) + 1
                    gameSacks(1 - P1, Z1, 1) = gameSacks(1 - P1, Z1, 1) + ydsGained * -1
                End If

                If HB% = 1 Then HB% = 0

                X = 0

        End Select

    End If

End Sub


'----------------------------------------
'           TWOMIN Subroutine
'----------------------------------------
'This subroutine gives the user a menu of
'options for the last 2 minutes of the
'game and handles the appropriate changes
'to team variables
'-- we should have I1 as a param rather than
'-- it being a global
Sub TWOMIN ()

    Call OPTIONBOX(7)

    Locate 9, 14: Print "1 - HUDDLE"
    Locate , 14: Print "2 - TIME OUT "; gameTeams$(P1); " "; timeouts(P1)
    Locate , 14: Print "3 - TIME OUT "; gameTeams$(D); " "; timeouts(D):
    Locate , 14: Print "4 - HURRY UP OFFENSE"
    Locate , 14: Print "5 - THROW AWAY PASS"

    Do

        If playerMode <> 0 Then

            I5 = score(P1, 0) - score(D, 0)

            'Is it possible for more than one of these to be TRUE at the same time?
            'If not, consider setting a flag value to then be checked outside the if
            'That would only give us one Exit Do
            If (playerMode = 1 And D = compTeam Or playerMode = 2) Then

                If I5 > 0 And I5 <= 14 And timeouts(D) > 0 And quarter = 4 Then I1 = 3: Exit Do

                If I5 <= 0 And I5 >= -14 Then

                    If timeouts(P1) > 0 Then
                        I1 = 2
                    Else
                        I1 = 4
                    End If

                    Exit Do

                End If

            End If

            If playerMode = 2 Then I1 = 1: Exit Do

            If quarter = 2 And ydLine > 15 And (playerMode = 1 And P1 = compTeam Or playerMode = 2) And I5 <= 0 Then
                If timeouts(P1) > 0 Then
                    I1 = 2
                Else
                    I1 = 4
                End If
                Exit Do
            End If

            If quarter = 2 And ydLine > 40 And I5 > 0 And leagRat_GAME(P1, 7) < 65 And (playerMode = 1 And P1 = compTeam Or playerMode = 2) And timeouts(P1) > 0 Then
                I1 = 2
                Exit Do
            End If

            If quarter = 2 And ydLine > 40 And I5 >= 0 And leagRat_GAME(P1, 7) < 65 And (playerMode = 2 Or playerMode = 1 And P1 = compTeam) Then
                I1 = 4
                Exit Do
            End If

        End If

        I$ = GetKeyPress$
        I1 = Val(I$)

    Loop Until Not (I1 < 1 Or I1 > 5 Or (I1 = 2 Or I1 = 4 Or I1 = 5) And playerMode = 1 And P1 = compTeam Or I1 = 3 And playerMode = 1 And D = compTeam)

    Color 0, 5 * (1 - BW)

End Sub


'----------------------------------------
'           FGDIST Subroutine
'----------------------------------------
'This subroutine calculcates the distance
'for a FG
Sub FGDIST ()

    I9 = 0

    If WX%(0) = P1 Then
        If WX%(1) = -7 Then I9 = -7
        If WX%(1) = -15 Then I9 = -15
    Else
        If WX%(1) = -7 Then I9 = 7
        If WX%(1) = -15 Then I9 = 15
    End If

    For I = 4 To 0 Step -1:

        If ydLine >= adjF1(I) + goalPostAdj + I9 Then
            fgSuccessChance = adjF0(I) - ((9 - kickerFGA(P1, Q7%(P1))) * 5)
        End If

        If ydLine >= 68 + goalPostAdj And ydLine >= adjF1(I) + goalPostAdj + I9 Then
            fgSuccessChance = fgSuccessChance + (ydLine - adjF1(I)) + goalPostAdj - 4 + I9
        End If

    Next I

    'Force there to always be a 5% chance of failure?
    If fgSuccessChance <= 0 Then
        fgSuccessChance = 5
    End If

    If fgSuccessChance >= 100 Then
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
    Locate 12
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
    Locate 12
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

    Call d100(I6)

    If dLine > 3 And dLine < 6 Then

        Select Case ydLine

            Case 94.999 To 95.998

                Select Case I6
                    Case 1 To 17: ydsGained = 6
                    Case 18 To 22: ydsGained = 4
                    Case 23 To 35: ydsGained = 3
                    Case 36 To 52: ydsGained = 2
                    Case 53 To 76: ydsGained = 1
                    Case 77 To 93: ydsGained = 0
                    Case 94 To 100: ydsGained = -1
                End Select

            Case 95.999 To 96.998

                Select Case I6
                    Case 1 To 26: ydsGained = 5
                    Case 27 To 29: ydsGained = 3
                    Case 30 To 49: ydsGained = 2
                    Case 50 To 78: ydsGained = 1
                    Case 79 To 94: ydsGained = 0
                    Case 95 To 100: ydsGained = -1
                End Select

            Case 96.999 To 97.998

                Select Case I6
                    Case 1 To 27: ydsGained = 4
                    Case 28 To 32: ydsGained = 2
                    Case 33 To 80: ydsGained = 1
                    Case 81 To 98: ydsGained = 0
                    Case 99 To 100: ydsGained = -1
                End Select

            Case 97.999 To 98.998

                Select Case I6
                    Case 1 To 46: ydsGained = 3
                    Case 47 To 72: ydsGained = 1
                    Case 73 To 91: ydsGained = 0
                    Case 92 To 100: ydsGained = -1
                End Select

            Case Is >= 98.999

                Select Case I6
                    Case 1 To 61: ydsGained = 2
                    Case 62 To 96: ydsGained = 0
                    Case 97 To 100: ydsGained = -1
                End Select

        End Select

    Else

        Select Case ydLine

            Case 94.999 To 95.998

                Select Case I6
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

                Select Case I6
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

                Select Case I6
                    Case 1 To 24: ydsGained = 4
                    Case 25 To 28: ydsGained = 2
                    Case 29 To 69: ydsGained = 1
                    Case 70 To 85: ydsGained = 0
                    Case 86 To 88: ydsGained = -1
                    Case 89 To 93: ydsGained = -2
                    Case 94 To 100: ydsGained = -4
                End Select

            Case 97.999 To 98.998

                Select Case I6
                    Case 1 To 43: ydsGained = 3
                    Case 44 To 65: ydsGained = 1
                    Case 66 To 83: ydsGained = 0
                    Case 84 To 92: ydsGained = -1
                    Case 93 To 94: ydsGained = -2
                    Case 95 To 100: ydsGained = -4
                End Select

            Case Is >= 98.999

                Select Case I6
                    Case 1 To 57: ydsGained = 2
                    Case 58 To 74: ydsGained = 0
                    Case 75 To 83: ydsGained = -1
                    Case 84 To 90: ydsGained = -2
                    Case 91 To 97: ydsGained = -3
                    Case 98 To 100: ydsGained = -4
                End Select

        End Select

    End If

    Call d100(I6)

    If I2 <= 30 Then
        If I6 <= GL%(I2, 1) Then ydsGained = ydsGained + GL%(I2, 2)
    Else
        'I2 < 30
        If I6 <= GL%(30, 1) Then ydsGained = ydsGained + GL%(30, 2)
    End If

End Sub


'----------------------------------------
'           PASSPBP Subroutine
'----------------------------------------
'This subroutine develops the Play-by-play
'string for pass play.
Sub PASSPBP ()

    Call d100(RN)

    If coverage >= 7 Then

        Select Case RN

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
                    RN = Int(Rnd(1) * teamSacks(1 - P1)) + 1

                    For Z1 = 0 To 14
                        M5 = M5 + playerSacks(1 - P1, Z1)
                        If RN <= M5 Then defTeam$ = defSacks$(1 - P1, Z1): Exit For
                    Next

                End If

        End Select

        RN = Int(Rnd(1) * 15) + 1
        If (coverage >= 7 Or dLine >= 9) Then

            Select Case RN

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

            Select Case P2

                Case 9, 10
                    If RN <= 50 Then
                        pbpString$ = qbacks$(P1, I1) + " TAKES THE SNAP AND DROPS BACK...LOOKING SHORT..."
                    Else
                        pbpString$ = qbacks$(P1, I1) + " DROPS QUICKLY AND SETS..."
                    End If

                Case 11, 16
                    Select Case RN

                        Case 1 To 30: pbpString$ = qbacks$(P1, I1) + " DROPS BACK AND LOOKS..."
                        Case 31 To 40: pbpString$ = qbacks$(P1, I1) + " DROPS BACK...HE'S UNDER PRESSURE..."
                        Case 41 To 60: pbpString$ = qbacks$(P1, I1) + " TO THROW...HAS TIME..."
                        Case 61 To 70: pbpString$ = qbacks$(P1, I1) + " BACK TO PASS...HAS PROTECTION..."
                        Case 71 To 80: pbpString$ = qbacks$(P1, I1) + " DROPS BACK...IS BEING CHASED!!"
                        Case 81 To 100: pbpString$ = qbacks$(P1, I1) + " HAS PLENTY OF TIME TO THROW..."

                    End Select

                Case 12
                    If RN <= 50 Then
                        pbpString$ = qbacks$(P1, I1) + " DROPS BACK...HE WANTS TO GO DEEP"
                    Else
                        pbpString$ = qbacks$(P1, I1) + " TAKES A DEEP DROP...HE'S LOOKING LONG"
                    End If

                Case 13
                    If RN <= 50 Then
                        pbpString$ = qbacks$(P1, I1) + " DROPPING...BEING CHASED..."
                    Else
                        pbpString$ = qbacks$(P1, I1) + " DROPS BACK...HE'S UNDER PRESSURE..."
                    End If

                Case 14, 15
                    If RN <= 50 Then
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

        R0 = Int(Rnd(1) * 12) + 1
        Call d100(RN)

        Select Case R0
            Case 1: pbpString$ = pbpString$ + " IS SACKED"
            Case 2: If RN <= 20 Then pbpString$ = pbpString$ + " IS NAILED"
            Case 3: If RN <= 30 Then pbpString$ = pbpString$ + " IS BURIED"
            Case 4: If RN <= 40 Then pbpString$ = pbpString$ + " IS DRILLED"
            Case 5: If RN <= 50 Then pbpString$ = pbpString$ + " IS HIT"
            Case 6: If RN <= 60 Then pbpString$ = pbpString$ + " IS THROWN"
            Case 7: If RN <= 70 Then pbpString$ = pbpString$ + " IS HAMMERED"
            Case 8: If RN <= 80 Then pbpString$ = pbpString$ + " IS TACKLED"
            Case 9: If RN <= 90 Then pbpString$ = pbpString$ + " IS SWARMED UNDER"
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
    Call d100(RN1)

    'If it's a running QB...???
    If useRandomQB(P1) = 1 Then
        qbNumber(P1) = 0
    Else
        qback = qbNumber(P1)
    End If

    If Not (currDown <= 2 Or currDown = 3 And RN1 <= 80) Then

        RN = Int(Rnd(1) * 8) + 1

        Select Case RN
            Case 1: pbpString$ = gameTeams$(P1) + " COMES UP TO THE LINE"
            Case 2: pbpString$ = gameTeams$(P1) + " BREAKS THE HUDDLE AND LINES UP"
            Case 3: pbpString$ = "THE " + gameMascots$(P1) + " COME UP TO THE LINE"
            Case 4: pbpString$ = "THE " + gameMascots$(P1) + " BREAK THE HUDDLE AND LINE UP"
            Case 5: pbpString$ = qbacks$(P1, qback) + " BARKS OUT THE SIGNALS"
            Case 6: pbpString$ = qbacks$(P1, qback) + " LOOKS OVER THE DEFENSE"
            Case 7: pbpString$ = "HERE IS THE " + downDesc$(currDown) + " DOWN AND" + Str$(Int(Y1)) + " YARD PLAY FOR " + gameTeams$(P1)
            Case 8: pbpString$ = "HERE IS THE " + downDesc$(currDown) + " DOWN AND" + Str$(Int(Y1)) + " YARD PLAY FOR THE " + gameMascots$(P1)
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

    pbpString$ = kicker$(P1, Q7%(P1)) + " IS PLACING THE BALL ON THE TEE"
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
    kickDir = Int(Rnd(1) * 2)

    Call d100(RN)

    Select Case RN

        Case 1 To 35: pbpString$ = "NO GOOD"

        Case 36 To 60: pbpString$ = "IT SAILS WIDE...NO GOOD"

        Case 61 To 80: pbpString$ = "HE MISSED IT!"

        Case 81 To 85: pbpString$ = "IT HITS THE UPRIGHTS....NO GOOD!"

        Case 86 To 90: pbpString$ = "HE HOOKS IT...NO GOOD!"

        Case 91 To 95:
            If 117 + goalPostAdj - ydLine > 45 Then
                pbpString$ = "IT FALLS SHORT....NO GOOD!"
            Else
                pbpString$ = "IT'S NO GOOD ... OFF TO THE " + direction$(kickDir)
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

    Call d100(RN)

    Select Case RN

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

    Call d100(RN)

    Select Case RN

        Case 1 To 50: pbpString$ = "NO GOOD"
        Case 51 To 70: pbpString$ = "HE MISSED IT!"
        Case 71 To 75: pbpString$ = "BAD SNAP FROM CENTER!! NO KICK": K2(P1, 6 + Q7%(P1) * 4, 17) = K2(P1, 6 + Q7%(P1) * 4, 17) - 1
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

    Call d100(RN)

    Select Case RN
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
'           KICKSET Subroutine
'----------------------------------------
'This subroutine handles the setup for a
'kick off by handling Play-by-Play
Sub KICKSET ()

    pbpString$ = kicker$(P1, Q7%(P1)) + " IS PLACING THE BALL ON THE TEE"
    Call PBP

    Call d100(RN)

    If RN <= 50 Then
        pbpString$ = gameTeams$(P1) + " IS LINING UP FOR THE KICKOFF"
    Else
        pbpString$ = "THE " + gameMascots$(P1) + " ARE LINING UP FOR THE KICKOFF"
    End If

    Call PBP

    pbpString$ = "AND THE KICK IS ON ITS WAY"
    Call PBP

    'Swap defense and offense
    Swap P1, D

    Call d100(RN)

    If RN <= 88 Then
        '88% chance of kick travelling 55 to 67 yds
        kickYL = 100 - (ydLine + Int(Rnd(1) * 13) + 55 + goalPostAdj)
    Else
        '12% chance of kick travelling 68 to 75 yds
        kickYL = 100 - (ydLine + Int(Rnd(1) * 8) + 68 + goalPostAdj)
    End If

    If ruleOptType = 0 And ruleOptColl = 0 Then kickYL = kickYL + 10

    If WX%(0) = P1 Then
        kickYL = kickYL - Int(Rnd(1) * 7) + 3 - WX%(1)
    Else
        kickYL = kickYL + Int(Rnd(1) * 7) - 3 + WX%(1)
    End If

End Sub


'----------------------------------------
'           QBTHROW Subroutine
'----------------------------------------
'This subroutine creates a play-by-play
'string for a QB pass.
Sub QBTHROW ()

    Call d100(RN)
    Select Case RN
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

    If P2 <> 4 And P2 <> 5 Then

        RN = Int(Rnd(1) * 46) + 1

        Select Case RN
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

        RN = Int(Rnd(1) * 23) + 1

        Select Case RN
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

        Select Case P2

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
        Call d100(RN)

        Select Case P2
            Case 9, 10
                Select Case RN
                    Case 1 To 33
                        pbpString$ = targetWRName$ + " HAS IT OVER THE MIDDLE"
                    Case 34 To 67
                        pbpString$ = targetWRName$ + " CATCHES IT IN THE FLAT"
                    Case 68 To 199
                        pbpString$ = "HE DUMPS IT TO " + targetWRName$
                End Select

            Case 13
                Select Case RN
                    Case 1 To 33
                        pbpString$ = "HE SCREENS IT TO " + targetWRName$
                    Case 34 To 67
                        pbpString$ = "HE DUMPS THE SCREEN TO " + targetWRName$
                    Case 68 To 199
                        pbpString$ = "HE HAS " + targetWRName$ + " ON THE SCREEN PASS"
                End Select

            Case 11, 12, 14, 15
                Select Case RN
                    Case 1 To 33
                        pbpString$ = targetWRName$ + " TAKES IT OVER THE MIDDLE"
                    Case 34 To 67
                        pbpString$ = "HE HAS " + targetWRName$
                    Case 68 To 199
                        pbpString$ = "HE COMPLETES IT TO " + targetWRName$
                End Select

            Case 16
                If RN <= 50 Then
                    pbpString$ = "HE HAS " + targetWRName$ + " NEAR THE SIDELINE"
                Else
                    pbpString$ = "HE FINDS " + targetWRName$
                End If

            Case 17, 18
                Select Case RN
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

    '20 yard line
    ydLine = 20
    ydsToScore = 80

    '1st and 10
    currDown = 1
    Y1 = 10

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

    RN = Int(Rnd(1) * 6) + 1

    Select Case RN

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
'          TryForTwoResult Subroutine
'----------------------------------------
'This subroutine makes a generic attempt
'to determine if the attempt at 2PC is
'successfull. It also gives Play-by-Play
'text for the attempt.
Sub TryForTwoResult ()

    If R1 = 1 Then K1(nbrScores, 4) = 20
    If R1 = 2 Then K1(nbrScores, 4) = 30

    pbpString$ = "TRY FOR TWO IS "
    gameStats(P1, 35) = gameStats(P1, 35) + 1

    If ydsGained < 2 And ruleOptType = 1 And ruleOptPro = 3 Then

        pbpString$ = pbpString$ + "NO GOOD"
        Call PBP

    Else

        If ydsGained < 3 And ruleOptType <> 1 And ruleOptPro <> 3 Then
            pbpString$ = pbpString$ + "NO GOOD"
            Call PBP
        Else
            'If ydsGained >= 3 Or ruleOptType = 1 And ruleOptPro = 3 And ydsGained >= 2 Then
            pbpString$ = pbpString$ + "GOOD"
            Call PBP

            score(P1, 0) = score(P1, 0) + 2
            score(P1, quarter) = score(P1, quarter) + 2
            K1(nbrScores, 4) = K1(nbrScores, 4) + 1
            gameStats(P1, 36) = gameStats(P1, 36) + 1

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

    Dim eventSettings(13)

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
                        If K2(I, I1, 1) > eventSettings(4) Then
                            checkSpecial% = 1
                        Else
                            If K2(I, I1, 2) > eventSettings(5) Then
                                checkSpecial% = 1
                            Else
                                If K2(I, I1, 4) > eventSettings(6) Then
                                    checkSpecial% = 1
                                Else
                                    If K2(I, I1, 5) > eventSettings(7) Then
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

                            If K2(I, I1, 7) > eventSettings(6) Then
                                checkSpecial% = 1
                            Else
                                If K2(I, I1, 8) > eventSettings(7) Then
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
                            If K2(I, I1, 12) > eventSettings(8) Then
                                checkSpecial% = 1
                            Else
                                If K2(I, I1, 13) > eventSettings(9) Then
                                    checkSpecial% = 1
                                End If
                            End If
                        Next
                    Next

                End If

                If checkSpecial% <> 1 Then

                    For I = 0 To 1
                        If K2(I, 3, 17) > eventSettings(10) Then
                            checkSpecial% = 1
                        Else
                            If K2(I, 7, 17) > eventSettings(10) Then
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

    'Beep
    Spec$ = diskPaths$(1) + "*.BSS"
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
    endGame = 1

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

        NM$ = "BOXSCORE" + Mid$(Str$(specialCount), 2, Len(Str$(specialCount))) + ".BSS"

        'Append special events
        'Call BOX2SPC(NM$)

        Call BOX2FILE(2, NM$)

    End If

    Call SAVESTATS

    endGame = 0
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
                NM$ = "TEMPBOX"
            Else
                'File = prompt | auto-create
                If autoPlay% <> 1 Then
                    Cls
                    Print "ENTER 8-CHARACTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";: Input NM$
                    NM$ = NM$ + ".BSS"
                Else
                    'ScheduleName_GameXXX.BS
                    NM$ = Mid$(scheduleFile$, 1, Len(scheduleFile$) - 4) + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
                End If

            End If

            'Includes a special option to send to printer when done
            Call BOX2FILE(outputChoice, NM$)

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
Sub BOX2FILE (printChoice, NM$)

    Shared actualAttendance&

    Open diskPaths$(1) + NM$ For Append As #1

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

    'gameStats() = Offense Stats
    Print #1, Tab(10 + hSpace1); gameTeams$(0); Tab(45 + hSpace2); gameTeams$(1)

    'gameStats(x, 0) = First downs
    Print #1, Tab(17); gameStats(0, 0); Tab(29); "FIRST DOWNS"; Tab(51); gameStats(1, 0)

    Print #1, Tab(14);

    'gameStats(x, 1) = Rush Att
    'gameStats(x, 2) = Rush Yds
    If ruleOptType = 0 Then
        Print #1, gameStats(0, 1); "-"; Int(gameStats(0, 2) + .5); Tab(29); "RUSHING YDS"; Tab(48); gameStats(1, 1); "-"; Int(gameStats(1, 2) + .5)
    Else
        Print #1, gameStats(0, 1) - W6(0, 0); "-"; Int(gameStats(0, 2) + W6(0, 1) + .5); Tab(29); "RUSHING YDS"; Tab(48); gameStats(1, 1) - W6(1, 0); "-"; Int(gameStats(1, 2) + W6(1, 1) + .5)
    End If

    'gameStats(x, 6) = Pass Yds
    Print #1, Tab(16); Int(gameStats(0, 6) + .5); Tab(29); "PASSING YDS"; Tab(51); Int(gameStats(1, 6) + .5)

    Print #1, Tab(16); Int(gameStats(0, 6) + .5) + Int(gameStats(0, 2) + .5); Tab(30); "TOTAL YDS"; Tab(51); Int(gameStats(1, 6) + .5) + Int(gameStats(1, 2) + .5)

    'gameStats(x, 3) = Pass Att
    'gameStats(x, 4) = Pass Comp
    'gameStats(x, 5) = Pass Int
    Print #1, Tab(13); gameStats(0, 4); "-"; gameStats(0, 3); "-"; gameStats(0, 5); Tab(31); "PASSES"; Tab(47); gameStats(1, 4); "-"; gameStats(1, 3); "-"; gameStats(1, 5)

    'gameStats(x, 7) = Punts
    'gameStats(x, 8) = Punt Yds
    If gameStats(0, 7) <> 0 And gameStats(1, 7) <> 0 Then
        Print #1, Tab(15); gameStats(0, 7); "-"; Int((gameStats(0, 8) / gameStats(0, 7)) * 10 + .5) / 10; Tab(32); "PUNTS"; Tab(49); gameStats(1, 7); "-"; Int((gameStats(1, 8) / gameStats(1, 7)) * 10 + .5) / 10
    End If

    'gameStats(x, 9) = Fumbles Total
    'gameStats(x, 10) = Fumbles Lost
    Print #1, Tab(15); gameStats(0, 9); "-"; gameStats(0, 10); Tab(31); "FUM/LOST"; Tab(50); gameStats(1, 9); "-"; gameStats(1, 10)

    'gameStats(x, 11) = Penalties
    'gameStats(x, 12) = Penalty Yards
    Print #1, Tab(15); gameStats(0, 11); "-"; Int(gameStats(0, 12) + .5); Tab(31); "PENALTIES"; Tab(50); gameStats(1, 11); "-"; Int(gameStats(1, 12) + .5)

    Print #1, Tab(15); W6(0, 0); "-"; Int(W6(0, 1) + .5); Tab(32); "SACKED"; Tab(50); W6(1, 0); "-"; Int(W6(1, 1) + .5)

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

            If K2(I, I1, 0) <> 0 Then

                If I1 <= 9 Then
                    Print #1, rbacks$(I, I1);
                Else
                    Print #1, qbacks$(I, I1 - 10);
                End If

                'K2(x, 0) = Rush Att
                'K2(x, 1) = Yds
                'K2(x, 2) = TD
                Print #1, Tab(18); Using "###  #### ###.#  "; K2(I, I1, 0), Int(K2(I, I1, 1) + .5), Int(K2(I, I1, 1) + .5) / K2(I, I1, 0);

                If I1 <= 9 Then
                    'Long   TD
                    Print #1, Using "###   ##"; runBacks(I, I1), K2(I, I1, 2)
                Else
                    'TD
                    Print #1, Using "      ##"; K2(I, I1, 2)
                End If
            End If

        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "RECEIVING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "TC    YDS    AVG   LG    TD"

        For I1 = 0 To 5

            If K2(I, I1, 6) <> 0 Then
                Print #1, wdRec$(I, I1); Tab(19);
                'K2(x, 6) = Catches
                'K2(x, 7) = Yds
                'K2(x, 8) = TD
                Print #1, Using "##    ###   ##.#  ###    ##"; K2(I, I1, 6), Int(K2(I, I1, 7) + .5), Int(K2(I, I1, 7) + .5) / K2(I, I1, 6), WR(I, I1 + 10), K2(I, I1, 8)
            End If

        Next I1

        For I1 = 0 To 9

            If K2(I, I1, 3) <> 0 Then
                Print #1, rbacks$(I, I1); Tab(19);
                'K2(x, 3) = Catches
                'K2(x, 4) = Yds
                'K2(x, 5) = TD
                Print #1, Using "##    ###   ##.#  ###    ##"; K2(I, I1, 3), Int(K2(I, I1, 4) + .5), Int(K2(I, I1, 4) + .5) / K2(I, I1, 3), WR(I, I1), K2(I, I1, 5)
            End If

        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "PASSING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "ATT  COM COM %  YDS  Y/A  Y/C  TD  TD%  I    I%  LG"

        For I1 = 0 To 3

            If K2(I, I1, 9) <> 0 Then

                Print #1, qbacks$(I, I1); Tab(19);
                'K2(x, 9) = Pass Att
                'K2(x, 10) = Completions
                'K2(x, 11) = Interceptions
                'K2(x, 12) = Yards
                'K2(x, 13) = TDs
                Print #1, Using "###  ### ###.#  ### ##.# "; K2(I, I1, 9), K2(I, I1, 10), K2(I, I1, 10) / K2(I, I1, 9) * 100, Int(K2(I, I1, 12) + .5), Int(K2(I, I1, 12) + .5) / K2(I, I1, 9);

                If K2(I, I1, 10) <> 0 Then
                    'Y/C, TD
                    Print #1, Using "##.#  ##"; Int(K2(I, I1, 12) + .5) / K2(I, I1, 10), K2(I, I1, 13);
                Else
                    'TDs
                    Print #1, Using "      ##"; K2(I, I1, 13);
                End If

                'TD%, Ints, Int%, Longest
                Print #1, Using "###.# ## ###.# ###"; K2(I, I1, 13) / K2(I, I1, 9) * 100, K2(I, I1, 11), K2(I, I1, 11) / K2(I, I1, 9) * 100, quarterbacks(I, I1)
            End If

        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "KICKOFF RETURNS"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "KR   YDS   AVG   LG  TD"

        For I1 = 0 To 2
            If K2(I, I1, 14) <> 0 Then
                Print #1, kickRet$(I, I1); Tab(19);
                'K2(x, 14) = # Returns
                'K2(x, 15) = Yards
                'K2(x, 16) = TDs
                Print #1, Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), kickReturners(I, I1), K2(I, I1, 16)
            End If
        Next I1

        Print #1,

    Next I

    Print #1,
    Print #1, Tab(17); "PUNT RETURNS"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "PR   YDS   AVG   LG  TD"

        For I1 = 3 To 5
            If K2(I, I1, 14) <> 0 Then
                Print #1, pret$(I, I1 - 3); Tab(19);
                'K2(x, 14) = # Returns
                'K2(x, 15) = Yards
                'K2(x, 16) = TDs
                Print #1, Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), puntReturners(I, I1 - 3), K2(I, I1, 16)
            End If
        Next I1

        Print #1,

    Next I

    Print #1, Tab(17); "PUNTING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "PNT   AVG  LG  TB  BLK  20"

        If gameStats(I, 7) <> 0 Then
            Print #1, punter$(I, 0); Tab(19);
            'gameStats(x, 7) = # Punts
            'gameStats(x, 8) = Yards
            'gameStats(x, 26) = Longest
            'gameStats(x, 27) = Touchbacks
            'gameStats(x, 28) = Blocks
            'gameStats(x, 29) = 20
            Print #1, Using "###  ##.#  ##  ##   ##  ##  ##  ##.#  ##.#"; gameStats(I, 7), gameStats(I, 8) / gameStats(I, 7), gameStats(I, 26), gameStats(I, 27), gameStats(I, 28), gameStats(I, 29)
        End If

        Print #1,

    Next I

    Print #1, Tab(17); "KICKING"

    For I = 0 To 1

        Print #1, gameTeams$(I); Tab(19); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

        If K2(I, 4, 17) <> 0 Or K2(I, 6, 17) <> 0 Then

            Print #1, kicker$(I, 0); Tab(19);
            'K2(x, 5, 17) = EPM, 1st PK
            'K2(x, 6, 17) = EPA, 1st PK
            If K2(I, 6, 17) <> 0 Then Print #1, Using "### ### ###.# "; K2(I, 5, 17), K2(I, 6, 17), K2(I, 5, 17) / K2(I, 6, 17) * 100;

            If K2(I, 4, 17) = 0 Then
                Print #1,
            Else
                Print #1, Tab(33); Using "### ### ###.# ##  #"; K2(I, 3, 17), K2(I, 4, 17), K2(I, 3, 17) / K2(I, 4, 17) * 100, gameLongFG(I, 0), FM(I, 0, 0);
                'K2(x, 3, 17) = FGM, 1st PK
                'K2(x, 4, 17) = FGA, 1st PK

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

        If K2(I, 8, 17) <> 0 Or K2(I, 10, 17) <> 0 Then

            Print #1, kicker$(I, 1); Tab(19);
            'K2(x, 7, 17) = EPM, 2nd PK
            'K2(x, 8, 17) = EPA, 2nd PK
            If K2(I, 10, 17) <> 0 Then Print #1, Using "### ### ###.# "; K2(I, 9, 17), K2(I, 10, 17), K2(I, 9, 17) / K2(I, 10, 17) * 100;

            If K2(I, 8, 17) = 0 Then
                Print #1,
            Else
                Print #1, Tab(33); Using "### ### ###.# ##  #"; K2(I, 7, 17), K2(I, 8, 17), K2(I, 7, 17) / K2(I, 8, 17) * 100, gameLongFG(I, 1), FM(I, 1, 0);
                'K2(x, 9, 17) = FGM, 2nd PK
                'K2(x, 10, 17) = FGA, 2nd PK

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
            If gameSacks(I, I1, 0) <> 0 Then
                Print #1, defSacks$(I, I1); Tab(19);
                Print #1, Using " ##       ##"; gameSacks(I, I1, 0), Int(gameSacks(I, I1, 1) + .5)
            End If
        Next I1
        Locate , 1: Print #1,
    Next I

    If printChoice = 1 Then Print #1, Chr$(12)

    Close #1

    If printChoice = 1 Then

        Open diskPaths$(1) + NM$ For Input As #2

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
Sub BOX2SPC (NM$)

    Open diskPaths$(1) + NM$ For Append As #1

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

        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1)
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
    Color tmRat_GAME(0, 2), tmRat_GAME(0, 1)
    Print gameTeams$(0);

    Color 0, 7
    Locate , 50
    Color tmRat_GAME(1, 2), tmRat_GAME(1, 1)
    Print gameTeams$(1)

    Color 0, 7
    Locate , 20: Print gameStats(0, 0);
    Locate , 34: Print "FIRST DOWNS";
    Locate , 55: Print gameStats(1, 0)

    If ruleOptType = 0 Then
        Locate , 18: Print gameStats(0, 1); "-"; Int(gameStats(0, 2) + .5);
        Locate , 34: Print "RUSHING YDS";
        Locate , 53: Print gameStats(1, 1); "-"; Int(gameStats(1, 2) + .5)
    Else
        Locate , 18: Print gameStats(0, 1) - W6(0, 0); "-"; Int(gameStats(0, 2) + W6(0, 1) + .5);
        Locate , 34: Print "RUSHING YDS";
        Locate , 53: Print gameStats(1, 1) - W6(1, 0); "-"; Int(gameStats(1, 2) + W6(1, 1) + .5)
    End If

    Locate , 20: Print Int(gameStats(0, 6) + .5);
    Locate , 34: Print "PASSING YDS";
    Locate , 55: Print Int(gameStats(1, 6) + .5)

    Locate , 35: Print "TOTAL YDS";
    Locate , 20: Print Int(gameStats(0, 6) + .5) + Int(gameStats(0, 2) + .5);
    Locate , 55: Print Int(gameStats(1, 6) + .5) + Int(gameStats(1, 2) + .5)

    Locate , 37: Print "PASSES";
    Locate , 16: Print gameStats(0, 4); "-"; gameStats(0, 3); "-"; gameStats(0, 5);
    Locate , 51: Print gameStats(1, 4); "-"; gameStats(1, 3); "-"; gameStats(1, 5)

    If gameStats(0, 7) <> 0 And gameStats(1, 7) <> 0 Then
        Locate , 38: Print "PUNTS";
        Locate , 15: Print gameStats(0, 7); "-"; Int((gameStats(0, 8) / gameStats(0, 7)) * 10 + .5) / 10;
        Locate , 50: Print gameStats(1, 7); "-"; Int((gameStats(1, 8) / gameStats(1, 7)) * 10 + .5) / 10
    End If

    Locate , 36: Print "FUM/LOST";
    Locate , 18: Print gameStats(0, 9); "-"; gameStats(0, 10);
    Locate , 53: Print gameStats(1, 9); "-"; gameStats(1, 10)

    Locate , 35: Print "PENALTIES";
    Locate , 18: Print gameStats(0, 11); "-"; Int(gameStats(0, 12) + .5);
    Locate , 53: Print gameStats(1, 11); "-"; Int(gameStats(1, 12) + .5)

    Locate , 37: Print "SACKED";
    Locate , 18: Print W6(0, 0); "-"; Int(W6(0, 1) + .5);
    Locate , 53: Print W6(1, 0); "-"; Int(W6(1, 1) + .5)

    Locate , 18: Print Using "##"; Int(timePoss(0) / 60);: Print ":";: Print Using "##"; timePoss(0) - Int(timePoss(0) / 60) * 60;
    Locate , 31: Print "TIME OF POSSESSION";
    Locate , 53: Print Using "##"; Int(timePoss(1) / 60);: Print ":";: Print Using "##"; timePoss(1) - Int(timePoss(1) / 60) * 60

    Locate , 32: Print "THIRD DOWN EFF";
    Locate , 16: Print thirdDownAtt(0) - thirdDownFail(0); "-"; thirdDownAtt(0);
    Locate , 51: Print thirdDownAtt(1) - thirdDownFail(1); "-"; thirdDownAtt(1)

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 32: Print "RUSHING STATISTICS"
    Print
    Call ADDBACKS

    For I = 0 To 1

        Locate , 20
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36
        Print "TC   YDS   AVG   LG   TD"

        For I1 = 0 To 13

            If K2(I, I1, 0) <> 0 Then

                Locate , 20
                If I1 <= 9 Then
                    Print rbacks$(I, I1);
                Else
                    Print qbacks$(I, I1 - 10);
                End If

                Locate , 35: Print Using "###  #### ###.#  "; K2(I, I1, 0), Int(K2(I, I1, 1) + .5), Int(K2(I, I1, 1) + .5) / K2(I, I1, 0);

                If I1 <= 9 Then
                    Print Using "###   ##"; runBacks(I, I1), K2(I, I1, 2)
                Else
                    Print Using "      ##"; K2(I, I1, 2)
                End If

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 32: Print "RECEIVING STATISTICS"
    Print
    nbrPlayers = 0

    For I = 0 To 1

        Locate , 18
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);: Color 0, 7
        Locate , 36: Print "TC    YDS    AVG   LG    TD"

        For I1 = 0 To 5

            If K2(I, I1, 6) <> 0 Then
                Locate , 18
                Print wdRec$(I, I1);
                Locate , 36: Print Using "##    ###   ##.#  ###    ##"; K2(I, I1, 6), Int(K2(I, I1, 7) + .5), Int(K2(I, I1, 7) + .5) / K2(I, I1, 6), WR(I, I1 + 10), K2(I, I1, 8)
                nbrPlayers = nbrPlayers + 1
            End If

            If (nbrPlayers Mod 16 = 0) Then
                'Create a 2nd page if there are enough players
                'to spam out past the page
                Call GetKeyWhenNoDelay

                Call FRAMESTATS(22)
                Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

                Locate 2, 32: Print "RECEIVING STATISTICS"
                Print

                Locate , 18
                Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
                Color 0, 7
                Locate , 36: Print "TC    YDS    AVG   LG    TD"

            End If

        Next I1

        For I1 = 0 To 9

            If K2(I, I1, 3) <> 0 Then
                Locate , 18
                Print rbacks$(I, I1);
                Locate , 36: Print Using "##    ###   ##.#  ###    ##"; K2(I, I1, 3), Int(K2(I, I1, 4) + .5), Int(K2(I, I1, 4) + .5) / K2(I, I1, 3), WR(I, I1), K2(I, I1, 5)
                nbrPlayers = nbrPlayers + 1
            End If

            If (nbrPlayers Mod 16 = 0) Then
                'Create a 2nd page if there are enough players
                'to spam out past the page
                Call GetKeyWhenNoDelay

                Call FRAMESTATS(22)
                Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

                Locate 2, 32: Print "RECEIVING STATISTICS"
                Print

                Locate , 18
                Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
                Color 0, 7
                Locate , 36: Print "TC    YDS    AVG   LG    TD"

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 32: Print "PASSING STATISTICS"
    Print

    For I = 0 To 1
        Locate , 6
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 22: Print "ATT  COM COM %  YDS  Y/A  Y/C  TD  TD%  I    I%  LG"

        For I1 = 0 To 3

            If K2(I, I1, 9) <> 0 Then

                Locate , 6
                Print qbacks$(I, I1);
                Locate , 22: Print Using "###  ### ###.#  ### ##.# "; K2(I, I1, 9), K2(I, I1, 10), K2(I, I1, 10) / K2(I, I1, 9) * 100, Int(K2(I, I1, 12) + .5), Int(K2(I, I1, 12) + .5) / K2(I, I1, 9);

                If K2(I, I1, 10) <> 0 Then
                    'Y/C, TD
                    Print Using "##.#  ##"; Int(K2(I, I1, 12) + .5) / K2(I, I1, 10), K2(I, I1, 13);
                Else
                    Print Using "      ##"; K2(I, I1, 13);
                End If

                '             TD%, I, I%,   LG
                Print Using "###.# ## ###.# ###"; K2(I, I1, 13) / K2(I, I1, 9) * 100, K2(I, I1, 11), K2(I, I1, 11) / K2(I, I1, 9) * 100, quarterbacks(I, I1)

            End If

        Next I1

        Print

    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 25: Print "KICKOFF RETURN STATISTICS"
    Print

    For I = 0 To 1
        Locate , 20
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36: Print "KR   YDS   AVG   LG  TD"

        For I1 = 0 To 2

            If K2(I, I1, 14) <> 0 Then
                Locate , 20
                Print kickRet$(I, I1);
                Locate , 36
                Print Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), kickReturners(I, I1), K2(I, I1, 16)

            End If

        Next I1

        Print
    Next I

    Print
    Locate , 25: Print "PUNT RETURN STATISTICS"
    Print

    For I = 0 To 1
        Locate , 20
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
        Color 0, 7
        Locate , 36: Print "PR   YDS   AVG   LG  TD"

        For I1 = 3 To 5
            If K2(I, I1, 14) <> 0 Then
                Locate , 20
                Print pret$(I, I1 - 3);
                Locate , 36
                Print Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), puntReturners(I, I1 - 3), K2(I, I1, 16)
            End If
        Next I1

        Print
    Next I

    Call GetKeyWhenNoDelay

    Call FRAMESTATS(22)
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 29: Print "PUNTING STATISTICS"
    Print

    For I = 0 To 1
        Locate , 10
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1)
        Print gameTeams$(I);: Color 0, 7
        Locate , 26: Print "PNT   AVG  LG  TB  BLK  20"

        If gameStats(I, 7) <> 0 Then
            Locate , 10
            Print punter$(I, 0);
            Locate , 26
            Print Using "###  ##.#  ##  ##   ##  ##  ##  ##.#  ##.#"; gameStats(I, 7), gameStats(I, 8) / gameStats(I, 7), gameStats(I, 26), gameStats(I, 27), gameStats(I, 28), gameStats(I, 29)
        End If

        Print
    Next I

    Print
    Locate , 29: Print "KICKING STATISTICS"
    Print

    For I = 0 To 1

        Locate , 3
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);: Color 0, 7
        Locate , 18: Print "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

        If K2(I, 4, 17) <> 0 Or K2(I, 6, 17) <> 0 Then
            Locate , 3
            Print kicker$(I, 0);
            Locate , 18

            If K2(I, 6, 17) <> 0 Then
                Print Using "### ### ###.# "; K2(I, 5, 17), K2(I, 6, 17), K2(I, 5, 17) / K2(I, 6, 17) * 100;
            End If

            If K2(I, 4, 17) = 0 Then
                Print
            Else
                Locate , 32
                Print Using "### ### ###.# ##  #"; K2(I, 3, 17), K2(I, 4, 17), K2(I, 3, 17) / K2(I, 4, 17) * 100, gameLongFG(I, 0), FM(I, 0, 0);
                Print "- ";: Print Using "#  #"; FA(I, 0, 0); FM(I, 0, 1);
                Print "- ";: Print Using "#  #"; FA(I, 0, 1); FM(I, 0, 2);
                Print "- ";: Print Using "#  #"; FA(I, 0, 2); FM(I, 0, 3);
                Print "- ";: Print Using "#  #"; FA(I, 0, 3); FM(I, 0, 4);
                Print "- ";: Print Using "#"; FA(I, 0, 4)
            End If

        End If

        If K2(I, 8, 17) <> 0 Or K2(I, 10, 17) <> 0 Then

            Locate , 3
            Print kicker$(I, 1);
            Locate , 18

            If K2(I, 10, 17) <> 0 Then
                Print Using "### ### ###.# "; K2(I, 9, 17), K2(I, 10, 17), K2(I, 9, 17) / K2(I, 10, 17) * 100;
            End If

            If K2(I, 8, 17) = 0 Then
                Print
            Else

                Locate , 32
                Print Using "### ### ###.# ##  #"; K2(I, 7, 17), K2(I, 8, 17), K2(I, 7, 17) / K2(I, 8, 17) * 100, gameLongFG(I, 1), FM(I, 1, 0);
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
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 35: Print "INTERCEPTIONS"
    Print

    For I = 0 To 1:
        Locate , 20
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);
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
    Locate 24, 27: Print "PRESS ANY KEY TO CONTINUE";

    Locate 2, 39: Print "SACKS"
    Print

    For I = 0 To 1
        Locate , 25
        Color tmRat_GAME(I, 2), tmRat_GAME(I, 1): Print gameTeams$(I);: Color 0, 7

        Locate , 41
        Print "NO.   YDS LOST"

        For I1 = 0 To 14
            If gameSacks(I, I1, 0) <> 0 Then
                Locate , 25: Print defSacks$(I, I1);
                Locate , 41: Print Using " ##       ##"; gameSacks(I, I1, 0), Int(gameSacks(I, I1, 1) + .5)
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

        PX = 0

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

    Shared gamePK$(), oppName$()
    Shared gameSite$()
    Shared locIndicator$()
    Shared tradeLF$(), tradeLI$(), tradeLK$(), tradeLP$()

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
                Input #1, gameZ2(J, K)
            Next
        Next

        For L = 0 To 38:
            Input #1, gameZ(L)
        Next

        For L = 0 To 38:
            Input #1, gameZ1(L)
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
            Input #1, LR(teamIdx, I)
            Input #1, gameLR$(teamIdx, I, 0)
            Input #1, gameLR$(teamIdx, I, 1)
        Next

        For I = 0 To 15:
            Input #1, LC(teamIdx, I)
            Input #1, gameLC$(teamIdx, I, 0)
            Input #1, gameLC$(teamIdx, I, 1)
        Next

        For I = 0 To 3:
            Input #1, LP(teamIdx, I)
            Input #1, tradeLP$(teamIdx, I, 0)
            Input #1, tradeLP$(teamIdx, I, 1)
        Next

        For I = 0 To 2:
            Input #1, LK(teamIdx, I)
            Input #1, tradeLK$(teamIdx, I, 0)
            Input #1, tradeLK$(teamIdx, I, 1)
        Next

        For I = 0 To 2:
            Input #1, PK(teamIdx, I)
            Input #1, gamePK$(teamIdx, I, 0)
            Input #1, gamePK$(teamIdx, I, 1)
        Next

        For I = 0 To 1:
            Input #1, LF(teamIdx, I)
            Input #1, tradeLF$(teamIdx, I)
            For I1 = 0 To 4:
                Input #1, AM(teamIdx, I, I1)
                Input #1, AF(teamIdx, I, I1)
            Next
        Next

        For I = 0 To 9:
            Input #1, LI(teamIdx, I)
            Input #1, tradeLI$(teamIdx, I, 0)
            Input #1, tradeLI$(teamIdx, I, 1)
        Next

        Close #1

    End If

    For I = 0 To 9
        If runBacks(teamIdx, I) > LR(teamIdx, I) Then
            LR(teamIdx, I) = runBacks(teamIdx, I)
            gameLR$(teamIdx, I, 0) = gameRB$(teamIdx, I)
            gameLR$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If

        If IR(teamIdx, I) > LI(teamIdx, I) Then
            LI(teamIdx, I) = IR(teamIdx, I)
            tradeLI$(teamIdx, I, 0) = gameIR$(teamIdx, I)
            tradeLI$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 15
        If WR(teamIdx, I) > LC(teamIdx, I) Then
            LC(teamIdx, I) = WR(teamIdx, I)
            gameLC$(teamIdx, I, 0) = gameWR$(teamIdx, I)
            gameLC$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 3
        If quarterbacks(teamIdx, I) > LP(teamIdx, I) Then
            LP(teamIdx, I) = quarterbacks(teamIdx, I)
            tradeLP$(teamIdx, I, 0) = gameQB$(teamIdx, I)
            tradeLP$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    For I = 0 To 2
        If kickReturners(teamIdx, I) > LK(teamIdx, I) Then
            LK(teamIdx, I) = kickReturners(teamIdx, I)
            tradeLK$(teamIdx, I, 0) = gameKR$(teamIdx, I)
            tradeLK$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If

        If puntReturners(teamIdx, I) > PK(teamIdx, I) Then
            PK(teamIdx, I) = puntReturners(teamIdx, I)
            gamePK$(teamIdx, I, 0) = gamePR$(teamIdx, I)
            gamePK$(teamIdx, I, 1) = gameTeams$(1 - teamIdx)
        End If
    Next

    If gameStats(teamIdx, 26) > gameZ(28) Then gameZ(28) = gameStats(teamIdx, 26)

    For I = 0 To 1

        If gameLongFG(teamIdx, I) > LF(teamIdx, I) Then
            LF(teamIdx, I) = gameLongFG(teamIdx, I)
            tradeLF$(teamIdx, 1) = gameTeams$(1 - teamIdx)
        End If

        For I1 = 0 To 4
            AM(teamIdx, I, I1) = AM(teamIdx, I, I1) + FM(teamIdx, I, I1)
            AF(teamIdx, I, I1) = AF(teamIdx, I, I1) + FA(teamIdx, I, I1)
        Next

    Next

    For J = 0 To 13
        For K = 0 To 17:
            gameZ2(J, K) = gameZ2(J, K) + Int(K2(teamIdx, J, K) + .5)
        Next
    Next

    gameZ(32) = gameZ(32) + score(teamIdx, 1)
    gameZ(33) = gameZ(33) + score(teamIdx, 2)
    gameZ(34) = gameZ(34) + score(teamIdx, 3)
    gameZ(35) = gameZ(35) + score(teamIdx, 4)

    For K = 5 To periodNbr
        gameZ(36) = gameZ(36) + score(teamIdx, K)
    Next

    gameZ1(32) = gameZ1(32) + score(1 - teamIdx, 1)
    gameZ1(33) = gameZ1(33) + score(1 - teamIdx, 2)
    gameZ1(34) = gameZ1(34) + score(1 - teamIdx, 3)
    gameZ1(35) = gameZ1(35) + score(1 - teamIdx, 4)

    For K = 5 To periodNbr
        gameZ1(36) = gameZ1(36) + score(1 - teamIdx, K)
    Next

    For J = 0 To 3
        gameZ(17) = gameZ(17) + K2(teamIdx, J, 13)
        gameZ1(17) = gameZ1(17) + K2(1 - teamIdx, J, 13)
    Next

    For J = 0 To 13
        gameZ(15) = gameZ(15) + K2(teamIdx, J, 2)
        gameZ1(15) = gameZ1(15) + K2(1 - teamIdx, J, 2)
    Next

    For J = 0 To 2
        gameZ(18) = gameZ(18) + K2(teamIdx, J, 16)
        gameZ1(18) = gameZ1(18) + K2(1 - teamIdx, J, 16)
        gameZ(22) = gameZ(22) + K2(teamIdx, J, 14)
        gameZ1(22) = gameZ1(22) + K2(1 - teamIdx, J, 14)
        gameZ(23) = gameZ(23) + K2(teamIdx, J, 15)
        gameZ1(23) = gameZ1(23) + K2(1 - teamIdx, J, 15)
        gameZ(19) = gameZ(19) + K2(teamIdx, J + 3, 16)
        gameZ1(18) = gameZ1(19) + K2(1 - teamIdx, J + 3, 16)
        gameZ(24) = gameZ(24) + K2(teamIdx, J + 3, 14)
        gameZ1(24) = gameZ1(24) + K2(1 - teamIdx, J + 3, 14)
        gameZ(25) = gameZ(25) + K2(teamIdx, J + 3, 15)
        gameZ1(25) = gameZ1(25) + K2(1 - teamIdx, J + 3, 15)
    Next

    For J = 0 To 9
        gameZ(20) = gameZ(20) + gameInts(teamIdx, J, 2)
        gameZ1(20) = gameZ1(20) + gameInts(1 - teamIdx, J, 2)
        gameZ(26) = gameZ(26) + gameInts(teamIdx, J, 0)
        gameZ1(26) = gameZ1(26) + gameInts(1 - teamIdx, J, 0)
        gameZ(27) = gameZ(27) + gameInts(teamIdx, J, 1)
        gameZ1(27) = gameZ1(27) + gameInts(1 - teamIdx, J, 1)
    Next

    gameZ(21) = gameZ(21) + gameStats(teamIdx, 19)
    gameZ1(21) = gameZ1(21) + gameStats(1 - teamIdx, 19)
    gameZ(37) = gameZ(37) + gameStats(teamIdx, 35)
    gameZ1(37) = gameZ1(37) + gameStats(1 - teamIdx, 35)
    gameZ(38) = gameZ(38) + gameStats(teamIdx, 36)
    gameZ1(38) = gameZ1(38) + gameStats(1 - teamIdx, 36)
    gameZ(30) = gameZ(30) + gameStats(teamIdx, 28)
    gameZ1(30) = gameZ1(30) + gameStats(1 - teamIdx, 28)
    gameZ(29) = gameZ(29) + gameStats(teamIdx, 27)
    gameZ1(29) = gameZ1(29) + gameStats(1 - teamIdx, 27)
    gameZ(31) = gameZ(31) + gameStats(teamIdx, 29)
    gameZ1(31) = gameZ1(31) + gameStats(1 - teamIdx, 29)

    For L = 0 To 12:
        gameZ(L) = gameZ(L) + gameStats(teamIdx, L)
    Next

    For L = 0 To 12:
        gameZ1(L) = gameZ1(L) + gameStats(1 - teamIdx, L)
    Next

    gameZ(13) = gameZ(13) + W6(teamIdx, 0)
    gameZ(14) = gameZ(14) + W6(teamIdx, 1)
    gameZ1(13) = gameZ1(13) + W6(1 - teamIdx, 0)
    gameZ1(14) = gameZ1(14) + W6(1 - teamIdx, 1)

    For J = 0 To 9
        For J1 = 0 To 2
            totalInts(teamIdx, J, J1) = totalInts(teamIdx, J, J1) + gameInts(teamIdx, J, J1)
        Next
    Next

    For J = 0 To 14
        For J1 = 0 To 1
            totalSacks(teamIdx, J, J1) = totalSacks(teamIdx, J, J1) + gameSacks(teamIdx, J, J1)
        Next
    Next

    Cls

    Print ">>>>>"
    Print " THE STATS ARE BEING SAVED UNDER"
    Print "THE FOLLOWING NAME::: ";
    Color tmRat_GAME(teamIdx, 2), tmRat_GAME(teamIdx, 1): Print statFile$
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
            Print #2, gameZ2(J, K)
        Next
    Next

    For L = 0 To 38:
        Print #2, gameZ(L)
    Next

    For L = 0 To 38:
        Print #2, gameZ1(L)
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
        Print #2, LR(teamIdx, I)
        Print #2, gameLR$(teamIdx, I, 0)
        Print #2, gameLR$(teamIdx, I, 1)
    Next

    For I = 0 To 15:
        Print #2, LC(teamIdx, I)
        Print #2, gameLC$(teamIdx, I, 0)
        Print #2, gameLC$(teamIdx, I, 1)
    Next

    For I = 0 To 3:
        Print #2, LP(teamIdx, I)
        Print #2, tradeLP$(teamIdx, I, 0)
        Print #2, tradeLP$(teamIdx, I, 1)
    Next

    For I = 0 To 2:
        Print #2, LK(teamIdx, I)
        Print #2, tradeLK$(teamIdx, I, 0)
        Print #2, tradeLK$(teamIdx, I, 1)
    Next

    For I = 0 To 2:
        Print #2, PK(teamIdx, I)
        Print #2, gamePK$(teamIdx, I, 0)
        Print #2, gamePK$(teamIdx, I, 1)
    Next

    For I = 0 To 1:
        Print #2, LF(teamIdx, I)
        Print #2, tradeLF$(teamIdx, I)
        For I1 = 0 To 4:
            Print #2, AM(teamIdx, I, I1)
            Print #2, AF(teamIdx, I, I1)
        Next
    Next

    For I = 0 To 9:
        Print #2, LI(teamIdx, I)
        Print #2, tradeLI$(teamIdx, I, 0)
        Print #2, tradeLI$(teamIdx, I, 1)
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
    powerRating(0, numberGames) = tmRat_GAME(teamIdx, 4)
    powerRating(1, numberGames) = tmRat_GAME(1 - teamIdx, 4)

    'Write stats to new file
    Open diskPaths$(1) + statFile$ + "S" For Output As #4

    Print #4, numberGames%

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
    Erase gameZ, gameZ1, gameZ2

End Sub


'----------------------------------------
'      Custom Function
'----------------------------------------
'It's not clear at this time what this
'function does
Function FNP (X)
    FNP = 11 + Int((X + Int(X / 5) - 1) / 2)
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


'----------------------------------------
'            pbplog Function
'----------------------------------------
'This subroutine is designed to help me
'print information about where the game
'is at during the code. The general idea
'was originally to be called everywhere
'there is PBP. This helps me debug where
'issues are happening in a game.
Sub pbplog (lineNumber)

    Print #11, "Called from line "; lineNumber
    Print #11, gameTeams$(0); " vs "; gameTeams$(1)
    Print #11, "Random # =  "; I
    Print #11, "INT % Chance = "; intChance
    Print #11,

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

        Color 15, 0
        Cls

        JJ = 7

        Locate 6, 8
        Color 15, 4

        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

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

                Locate 8, 27: Print I$
                Locate 8, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES WERE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO MAIN MENU"
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

                        previousYear$ = yearNumber$

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, gameIdx)

                If ExitCode% <> 27 And numberTeams <> 0 Then

                    Do
                        Locate 2, 33
                        Color tmRat_GAME(gameIdx, 2), tmRat_GAME(gameIdx, 1): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor% = 15: FileBGColor% = 4
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, gameIdx)

        End If

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx)

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
        tmRat_GAME(gameIdx, I1) = CVI(Q$(I1 + 11))
    Next I1

    For I1 = 0 To 9
        rbacks$(gameIdx, I1) = RTrim$(Q$(I1 * 5 + 16))
        rbRushAtt(gameIdx, I1) = CVI(Q$(I1 * 5 + 17))
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
    puntYdsPerP(gameIdx, 0) = CVI(Q$(123))

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

    If Left$(gameMascots$(gameIdx), 1) = " " Or gameMascots$(gameIdx) = "" Then gameMascots$(gameIdx) = gameTeams$(gameIdx)

    PC%(gameIdx, 0) = rbRushAtt(gameIdx, 0)

    For I1 = 9 To 1 Step -1
        If rbRushAtt(gameIdx, I1) <> 0 Then
            PC%(gameIdx, I1) = rbRushAtt(gameIdx, I1) - rbRushAtt(gameIdx, I1 - 1)
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

End Sub


'----------------------------------------
'      GetKeyWhenNoDelay Subroutine
'----------------------------------------
' ...explanation...
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

    'Close #11

    If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
        Cls
        Print "PLEASE WAIT ..."
        Call FOOTBALL
    End If

End Sub

