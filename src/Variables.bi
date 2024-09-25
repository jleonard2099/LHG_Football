
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)
Dim powerRating(0 To 1, MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim statsZ!(38), statsZ1!(38), statsZ2!(50, 18)

'Record / Standings



' Conferences / Orgs
Dim memberConf$(MAX_CONFERENCES)
'Static values here are so we can cover
'TEAMS_PER_CONFERENCE and TEAMS_PER_LEAGUE
'Ideally we would ReDim each based on these
'values when needed
Dim memberIdx(40)
ReDim memberName$(40), memberNames_2nd$(40)
ReDim memberYear$(40), memberYear_2nd$(40)
ReDim statFiles$(30), statFilesComp$(60)

'-- For Road Data
Dim gameAtt_Road!(MAX_SCHED_STATS)

Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)
Dim powerRating_Road(1, MAX_SCHED_STATS)

Dim gameSite_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS), oppName_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(1), scheduleNG%(MAX_SCHEDULE_GAMES, 20)
ReDim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

ReDim scheduleH$(MAX_SCHEDULE_GAMES), scheduleV$(MAX_SCHEDULE_GAMES)
'ReDim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)
ReDim scheduleQB_V$(MAX_SCHEDULE_GAMES), scheduleQB_H$(MAX_SCHEDULE_GAMES)
ReDim scheduleYN$(MAX_SCHEDULE_GAMES, 1)
'ReDim yearNumber$(MAX_SCHEDULE_GAMES, 1)


' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim eventSettings(13)

Dim location$(2), modeAbbrev$(3), overtime$(3)
Dim playMode$(3)
Dim rulesType$(6), ruleYrPro$(0 To 9), ruleYrColl$(0 To 11)
Dim teamIndicator$(1), weather$(3), windSetting$(3)
Dim yesNo$(1), yesNoText$(1)


'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------

'Different aligned names by position
Dim rbName_Align$(0 to 9), wrName_Align$(0 to 5), qbName_Align$(0 to 3), krName_Align$(0 to 2), prName_Align$(0 to 2), punterName_Align$(0 to 3), pkName_Align$(0 to 1)
Dim intName_Align$(9), sackName_Align$(14)

'Different original names by position
Dim rbName_Road$(0 To 9), wrName_Road$(0 To 5), qbName_Road$(0 To 3), krName_Road$(0 To 2), prName_Road$(0 To 2), punterName_Road$(0 To 1), pkName_Road$(0 To 1)
Dim intName_Road$(9), sackName_Road$(15)

Dim teamNameAlign$

Dim longCatchName_Align$(0 To 15, 0 To 1), longCatchName_Road$(0 To 15, 0 To 1)
Dim longFGName_Align$(0 To 1), longFGName_Road$(0 To 1)
Dim longIntName_Align$(0 To 9, 0 To 1), longIntName_Road$(0 To 9, 0 To 1)
Dim longKRName_Align$(0 To 2, 0 To 1), longKRName_Road$(0 To 2, 0 To 1)
Dim longPassName_Align$(0 To 3, 0 To 1), longPassName_Road$(0 To 3, 0 To 1)
Dim longRunName_Align$(0 To 9, 0 To 1), longRunName_Road$(0 To 9, 0 To 1)
Dim longPRName_Align$(0 To 2, 0 To 1), longPRName_Road$(0 To 2, 0 To 1)

Dim FGA_Align!(0 To 1, 0 To 4), FGA_Road!(0 To 1, 0 To 4)
Dim FGM_Align!(1, 4), FGM_Road!(1, 4)
Dim longCatchVal_Align!(15), longCatchVal_Road!(15)
Dim longFGVal_Road!(1), longFGVal_Align!(1)
Dim longKRVal_Align!(2), longKRVal_Road!(2)
Dim longIntVal_Align!(0 To 9), longIntVal_Road!(0 To 9)
Dim longPassVal_Align!(3), longPassVal_Road!(3)
Dim longRunVal_Align!(0 To 9), longRunVal_Align!(0 To 9)
Dim longPRVal_Align!(2), longPRVal_Road!(2)
Dim nbrInt_Align(0 To 9, 2), nbrInt_Road(0 To 9, 2), nbrSack_Align(0 To 15, 1), nbrSack_Road(0 To 15, 1)

Dim alignZ2!(14, 18), roadZ0!(38), roadZ1!(38), roadZ2!(14, 18)


'----------------------------------------
' Used in CAREER / LEADER routines
'----------------------------------------
Dim TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

Dim careerWRR$, careerQBR$, careerKRR$, careerPRR$

Dim carLdrPlyrName$(62, 20), carLdrTDIndic$(62, 20)
Dim carLdrVal!(62, 20)

Dim carDefInts$(55), carDefSacks$(55)

'S = stats ???
Dim careerKRS$(MAX_CAREER_YEARS, 15, 1), careerPRS$(MAX_CAREER_YEARS, 15, 1)
Dim careerQBS$(MAX_CAREER_YEARS, 20, 1), careerRBS$(MAX_CAREER_YEARS, 50, 1), careerWRS$(MAX_CAREER_YEARS, 40, 1)

Dim carRBacks$(50), carWdRec$(40), carQBacks$(20), carKickRet$(15), carPRet$(15), carPunter$(10), carKicker$(10)

Dim careerLC$(60, 1), careerLF$(0 To 1), careerLI$(0 To 9, 0 To 1)
Dim careerLK$(15, 1), careerLP$(20, 1), careerLR$(50, 1)
Dim careerPK$(15, 1)

Dim careerDefRecPlyr$(1), careerFGRecPlyr$(6), careerKRRecPlyr$(4), careerPRRecPlyr$(4), careerPuntRecPlyr$(2)
Dim careerQBRecPlyr$(8), careerRBRecPlyr$(9), careerRBR$(1), careerWRRecPlyr$(4)

Dim carYear_DF(1), carYear_FG(6), carYear_KRet(4), carYear_PRet(4)
Dim carYear_Punt(2), carYear_QB(8), carYear_RB(9), carYear_WR(4)
Dim carGameSacks(55, 1), carGameInts(55, 2)

Dim careerDefRecVal!(1), careerFGRecVal!(6), careerKRRecVal!(4), careerPRRecVal!(4)
Dim careerPuntRecVal!(2), careerQBRecVal!(8), careerRBRecVal!(9), careerWRRecVal!(4)

Dim careerAF!(9, 4), careerAM!(9, 4)
Dim careerLC!(60), careerLK!(15), careerLR!(50), careerLP!(20)
Dim careerPK!(15)

Dim careerFGS!(MAX_CAREER_YEARS, 10, 17), careerGIS!(MAX_CAREER_YEARS, 55, 3), careerGSS!(MAX_CAREER_YEARS, 55, 3)
Dim careerKRS!(MAX_CAREER_YEARS, 15, 11), careerPRS!(MAX_CAREER_YEARS, 15, 11), careerPUS!(MAX_CAREER_YEARS, 10, 4)
Dim careerQBS!(MAX_CAREER_YEARS, 20, 13), careerRBS!(MAX_CAREER_YEARS, 50, 11), careerWRS!(MAX_CAREER_YEARS, 40, 11)

Dim careerRecPlyr$(6)
Dim careerRecVal!(6), careerTeamTotal!(50)

'Dim carLdrPlyrName$(62, 20)
'Dim carLdrVal!(62, 20)


'----------------------------------------
'   Used in COMPARE routines
'----------------------------------------


'----------------------------------------
' Used in COMPILER routines
'----------------------------------------
Dim Shared confLosses, confTies, confWins
Dim Shared fullLosses, fullTies, fullWins
Dim Shared PTSFC, PTSAC

Dim fumbGain, puntNum
Dim puntName$

Dim indRecDesc$(50, 2), teamRecDesc$(50)
Dim compRB$(10), compWR$(16), compQB$(4)
Dim compKR$(3), compPR$(3), compPK$(2)
Dim compZ2$(1 To 240), compZ3$(1 To 240)

Dim N2$(240), N3$(240), NT$(10, 20)
Dim PS$(10), PT$(1200)
Dim TB$(18), TT$(10, 20)

Dim compRB!(11, 11), compWR!(16, 13), compQB!(1 To 11, 1 To 11)
Dim compPR!(11, 11)

Dim compTT!(10, 20, 9)
Dim compZ1!(1 To 240, 1 To 2), compZ2!(14, 18)

Dim indRecords!(50), teamRecords!(50)

Dim DT!(38), EA!(2), EM!(2), EP!(2)
Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(60), O2!(60), O3!(60), OT!(38)
Dim PT!(1200, 5), PTSA!(60), PTSF!(60)
Dim TD!(16), TG!(16), TP!(18)
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
Dim teamRat_DRAFT(4), wrRec_DRAFT(5), wrAvg_DRAFT(5)

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
Dim teamRat_TRADE(1, 4), wrAvg_TRADE(1, 5), wrRec_TRADE(1, 5)
Dim ydCmpAdj_TRADE!(1), ydsPerComp_TRADE(1)

Dim krNam_TRADE$(2, 3), nicks_TRADE$(1)
Dim prNam_TRADE$(2, 3), pntNam_TRADE$(2, 3), pkNam_TRADE$(1, 1), puntNam_TRADE$(1)
Dim qbNam_TRADE$(2, 4), rbNam_TRADE$(2, 9)
Dim tmInfo_TRADE$(1), wrNam_TRADE$(2, 6)


' ** Stats File **
Dim teamIndexes(1)

Dim tradeAF!(1, 1, 4), tradeAM!(1, 1, 4)
Dim tradeLC!(2, 15), tradeLF!(1, 1), tradeLI!(1, 9)
Dim tradeLK!(1, 2), tradeLP!(1, 3), tradeLR!(1, 9)
Dim tradePK!(1, 2)

Dim tradeZ!(1, 38), tradeZ1!(1, 38), tradeZ2!(1, 13, 17)

Dim tradeGI%(1, 9, 2), tradeGS%(1, 14, 1)


Dim gamePK$(1, 2, 1)
Dim statNam_TRADE$(1)
Dim tradeLF$(1, 1), tradeLI$(1, 9, 1)
Dim tradeLK$(1, 2, 1), tradeLP$(1, 3, 1)

Dim tradeLC$(1, 15, 1), tradeLR$(1, 9, 1)
Dim tradePK$(1, 2, 1), tradePR$(1, 2)
Dim tradeQB$(1, 3), tradeRB$(1, 9), tradeWR$(1, 5)
Dim tradeFG$(1, 1), tradeKR$(1, 2)
Dim tradeYN$(1)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim div1Name$, div2Name$, div3Name$
Dim div1Name_2$, div2Name_2$, div3Name_2$

'These are mostly keeping track of quantities (# wins, # losses, etc)
'so they could probably be INTEGERS.
'However from the original code they are intended to be Singles

Dim awayLosses!(30), awayLosses_2nd!(30)
Dim awayWins!(30), awayWins_2nd!(30)
Dim homeScoreTeam!(100), homeScoreTeam_2nd!(100)
Dim homeScoreOpp!(100), homeScoreOpp_2nd!(100)
Dim awayScoreTeam!(100), awayScoreTeam_2nd!(100)
Dim awayScoreOpp!(100), awayScoreOpp_2nd!(100)
Dim homeLosses!(30), homeLosses_2nd!(30)
Dim homeTies!(30), homeTies_2nd!(30)
Dim homeWins!(30), homeWins_2nd!(30)
Dim awayTies_2nd!(30), awayTies!(30)
Dim totAwayLosses!(4), totAwayLosses_2nd!(4)
Dim totAwayWins!(4), totAwayWins_2nd!(4)
Dim totHomeScoreTeam!(4), totHomeScoreTeam_2nd(4)
Dim totHomeScoreOpp!(4), totHomeScoreOpp_2nd!(4)
Dim totAwayScoreTeam!(4), totAwayScoreTeam_2nd!(4)
Dim totAwayScoreOpp!(4), totAwayScoreOpp_2nd!(4)
Dim totHomeLosses!(4), totHomeLosses_2nd!(4)
Dim totHomeWins!(4), totHomeWins_2nd!(4)
Dim totHomeTies!(4), totHomeTies_2nd!(4)
Dim totAwayTies!(4), totAwayTies_2nd!(4)

Dim DV$(3), DV2$(3)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
Dim indRecCategory$(46), teamRecCategory$(43)


'----------------------------------------
' Used in STAT / INPUT
'----------------------------------------
Dim JA, JB, JC, JD, JE, JF, JG, JH, JI

'-----

Dim A1L!(300, 8), A2L!(180, 4), A3L!(120, 13), A3R!(120, 1)
Dim A4L!(90, 4), A5L!(90, 4), A6L!(30, 2)
Dim A7L!(60, 5), A8L!(300), A9L!(450)

Dim A1L$(300), A1T$(300), A2L$(180), A2T$(180), A3T$(120)
Dim A4L$(90), A4T$(90), A5L$(90), A5T$(90)
Dim A6L$(30), A6T$(30), A7L$(60), A7T$(60)
Dim A8L$(300), A8T$(300), A9L$(450), A9T$(450)

'-----

Dim gameCount(30), lookyP%(1)

Dim GMA!(300), GMB!(180), GMC!(120)

Dim teamStats!(1, 12), TYP!(480), lookyTT!(31, 0 To 29), sackStats!(1, 1)


Dim expCategories$(74)

Dim intNam_TRADE$(1, 9)
Dim LCL$(480), LKL$(90), LPL$(120), LRL$(300), PKL$(90)

Dim lookyRBacks$(10), lookyWdRec$(6), lookyQBacks$(4), lookyKickRet$(3), lookyPRet$(3), lookyPunter$(2), lookyKicker$(2)
Dim lookyTT$(31, 0 To 29)

Dim QBL$(120), sackNam_TRADE$(1, 14)
Dim TMM$(480), TPP$(480), TYY$(480)

Dim playerStats!(13, 17)

'--LF! is only 0 to 9 b/c of career behavior
Dim statsAF!(0 To 1, 0 To 4), statsAM!(1, 4)
Dim statsLC!(0 To 15), statsLF!(0 To 9), statsLI!(0 To 9), statsLK!(0 To 3)
Dim statsLP!(0 To 3), statsLR!(0 To 9), statsPK!(0 To 2)
Dim statsGI%(0 To 9, 2), statsGS%(0 To 14, 0 To 1)

Dim statsLC$(0 To 15, 0 To 1), statsLF$(0 To 1), statsLI$(0 To 9, 0 To 1)
Dim statsLK$(0 To 2, 0 To 1), statsLP$(0 To 3, 0 To 1), statsLR$(0 To 9, 0 To 1)
Dim statsPK$(0 To 2, 1)

Dim ratingsQB!(0 To 3, 10)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim LD!(240), LZ!(240)
Dim seeL!(60), seeT!(60), seeW!(60)

Dim seeP$(60), TM$(60)
Dim seePR$(1200), seeT$(60)


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
Dim Shared endAllGames, endYds, endZone, expCompPct
Dim Shared fieldSide, firstDownClockOpt
Dim Shared fgSuccessChance, fgAttYds, ffPctSuccess
Dim Shared gameLoc, gameOver, goalPostAdj, halfTime

'These all seem to only be used for loops, but it is difficult to confirm
Dim Shared I1, I2, I3, I4, I5, I6, I7, I8, I9

Dim Shared isOT, JJ, K3
Dim Shared kickDist, kickYL, playSegment, overtimeOpt, nbrScores
Dim Shared penaltyYds, playerMode, playerOpt, playType
Dim Shared P1, playCall, periodNbr, puntRetNbr
Dim Shared quarter, qback, qbTakeKnee
Dim Shared revFumble, ruleOptColl, ruleOptPro, ruleOptType, rushYds
Dim Shared S6, startYds, tickerGames
Dim Shared W5, WE, WS, winTeam, yrdLine

'playSegment appears to represent a position in the order of a play
'0   Start clock
'1   Huddle?
'2   Snap
'3   Play in action
'4   Stop the clock (incomplete pass, first down)
'5   Possession change (Safety, Defense recover fumble, kick return, etc.)
'6   End of Play (Whistle)
'7   ???
'8   Assess penalty
'9   Timeout


'-- There is no indication of X being used in any way!!!
'-- it seems similar to I/J loop variables
Dim Shared X, X1

'The below 2 variables are related to scouting
Dim Shared XD, XE

Dim Shared ydsToFirst As Single
Dim Shared ydsGained As Single, ydLine As Single, ydLineTeam As Single, ydsToScore As Single
Dim Shared YC, YF, Z1

Dim Shared F%, FF%, goalLnYdAdj, HB%, intChance
Dim Shared PC%, PS%

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared adjF0(4), adjF1(4)
Dim Shared brkawayYds(38, 4), climate(6)
'Climate array
'index 0 = wind direction
'index 1 = wind strength
'index 2 =
'index 3 =
'index 4 = weather
'index 5 =
'index 6 =

Dim Shared gameAF!(1, 1, 0 To 4), gameAM!(1, 1, 0 To 4)
Dim Shared gameLC!(1, 20), gameLF!(1, 1), gameLI!(1, 0 To 9), gameLK!(1, 0 To 2), gameLP!(1, 0 To 3), gameLR!(1, 0 To 9)
Dim Shared gamePK!(0 To 1, 0 To 2)
Dim Shared gameZ0!(0 To 38), gameZ1!(0 To 38), gameZ2!(0 To 13, 0 To 17)

Dim Shared activeKicker(1)
Dim Shared defInts(1), defSacks(1), defYdAdj(0 To 8, 0 To 10)
Dim Shared FA(1, 1, 4), FM(1, 1, 4)
Dim Shared gameInts(1, 9, 2), gameLongFG(1, 1), gameStatsPlayer(1, 13, 17), gameStatsTeam(1, 36)
Dim Shared goalLnYdAdj(1 To 30, 1 To 2)
Dim Shared K1(50, 6), K3(1, 6)
Dim Shared hasRunFF(1), playerInts(1, 9), IR(1, 9)
Dim Shared kickerFGA(1, 1), kickerFGPct(1, 1), kickerIdx(1, 1), kickerPATPct(1, 1)
Dim Shared kickReturners(0 To 1, 0 To 2), krNumRet(1, 2), krYdsPerRet(1, 2)
Dim Shared leagRat_GAME(1, 7)
Dim Shared nbrPossOT(1)
Dim Shared passCovAdj(3, 10, 11), penaltyYds(2), playerSacks(1, 14)
Dim Shared puntReturners(0 To 1, 0 To 2), prNumRet(1, 2), prYdsPerRet(1, 2)
Dim Shared quarterbacks(1, 3), qbNumber(1)
Dim Shared qbArmRat(1, 3), qbMobility(1), qbNumAtt(1, 9), qbCompPct(1, 9), qbPctInt(1, 9)
Dim Shared runYdAdj(1, 38)
Dim Shared runBacks(1, 17), rbrushContrib(1, 9), rbRushAvg(1, 9), rbNumRec(1, 9), rbYdsPerC(1, 9)
Dim Shared sackStatsPlayer(1, 14, 1)
Dim Shared score(0 To 1, 0 To 10), scoreQuarters(50), scoreTimes(50), schedGame(2)
Dim Shared teamIdx_GAME(2), teamInts(1), teamSacks(1), timeouts(1), timePoss(1), teamRat_GAME(2, 9)
Dim Shared teamYears(1), thirdDownAtt(1), thirdDownFail(1)
Dim Shared totalInts(0 To 1, 0 To 9, 0 To 2), totalSacks(0 To 1, 0 To 14, 0 To 1)
Dim Shared useRandomQB(1)
Dim Shared sackStatsTeam(1, 1), WR(1, 20), wrNumRec(1, 9), wrYdsPerC(1, 9)
Dim Shared XD(1), ydsPerComp(1), ydsPerCompPctAdj(50, 2), ydsPerPunt(1, 2)

Dim Shared HB%(1), NG%(20), indRushPct(1, 9), PS%(2, 21)
Dim Shared RM%(1, 14), RV%(1)
Dim Shared S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14)
Dim Shared qbRushIdx(1, 3)

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
