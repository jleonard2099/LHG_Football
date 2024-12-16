
Dim opSysType$

Dim Shared currGame

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)
Dim powerRating(0 To 1, MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim statsLRec$(0 To 15, 0 To 1), statsLFG$(0 To 1), statsLInt$(0 To 9, 0 To 1)
Dim statsLKR$(0 To 2, 0 To 1), statsLPass$(0 To 3, 0 To 1), statsLRush$(0 To 9, 0 To 1)
Dim statsLPR$(0 To 2, 0 To 1)

Dim statsFGA!(0 To 1, 0 To 4), statsFGM!(1, 4)
Dim statsLRec!(0 To 15), statsLFG!(0 To 9), statsLInt!(0 To 9), statsLKR!(0 To 3)
Dim statsLPass!(0 To 3), statsLRush!(0 To 9), statsLPR!(0 To 2)
Dim statsGI(0 To 9, 2), statsGS(0 To 14, 0 To 1)

Dim statsTeam!(38), statsOpp!(38), statsPlayers!(50, 18)

'Record / Standings



' Conferences / Orgs
Dim memberConf$(MAX_CONFERENCES)
'Static values here are so we can cover
'TEAMS_PER_CONFERENCE and TEAMS_PER_LEAGUE
'Ideally we would ReDim each based on these
'values when needed
Dim memberIdx(40)
Dim memberName$(40), memberNames_2nd$(40)
Dim memberYear$(40), memberYear_2nd$(40)
Dim statFilesComp$(60)

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
Dim Shared DT$, leagLdrTeams$

Dim eventSettings(13)

Dim location$(2), modeAbbrev$(3), overtime$(0 To 8)
Dim playMode$(3)
Dim rulesType$(6), ruleYrPro$(0 To 9), ruleYrColl$(0 To 11)
Dim teamIndicator$(1), weather$(3), windSetting$(3)
Dim yesNo$(1), yesNoText$(1)


'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
'Different aligned names by position
Dim rbName_Align$(0 To 9), wrName_Align$(0 To 5), qbName_Align$(0 To 3), krName_Align$(0 To 2), prName_Align$(0 To 2), punterName_Align$(0 To 3), pkName_Align$(0 To 1)
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
Dim nbrInt_Align(0 To 9, 2), nbrInt_Road(0 To 9, 2), nbrSack_Align(0 To 15, 1), nbrSack_Road(0 To 15, 1)

Dim longCatchVal_Align!(15), longCatchVal_Road!(15)
Dim longFGVal_Road!(1), longFGVal_Align!(1)
Dim longKRVal_Align!(2), longKRVal_Road!(2)
Dim longIntVal_Align!(0 To 9), longIntVal_Road!(0 To 9)
Dim longPassVal_Align!(3), longPassVal_Road!(3)
Dim longRunVal_Align!(0 To 9), longRunVal_Road!(0 To 9)
Dim longPRVal_Align!(2), longPRVal_Road!(2)

Dim statsTeam_Road!(38), statsOpp_Road!(38)
Dim statsPlayers_Align!(14, 18), statsPlayers_Road!(14, 18)


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

Dim careerLRec$(60, 1), careerLFG$(0 To 1), careerLInt$(0 To 9, 0 To 1)
Dim careerLKR$(15, 1), careerLPass$(20, 1), careerLRush$(50, 1)
Dim careerLPR$(15, 1)

Dim careerDefRecPlyr$(1), careerFGRecPlyr$(6), careerKRRecPlyr$(4), careerPRRecPlyr$(4), careerPuntRecPlyr$(2)
Dim careerQBRecPlyr$(8), careerRBRecPlyr$(9), careerRBR$(1), careerWRRecPlyr$(4)

Dim carYear_DF(1), carYear_FG(6), carYear_KRet(4), carYear_PRet(4)
Dim carYear_Punt(2), carYear_QB(8), carYear_RB(9), carYear_WR(4)
Dim carGameSacks(55, 1), carGameInts(55, 2)

Dim careerDefRecVal!(1), careerFGRecVal!(6), careerKRRecVal!(4), careerPRRecVal!(4)
Dim careerPuntRecVal!(2), careerQBRecVal!(8), careerRBRecVal!(9), careerWRRecVal!(4)

Dim careerFGA!(9, 4), careerFGM(9, 4)
Dim careerLRec!(60), careerLKR!(15), careerLRush!(50), careerLPass!(20)
Dim careerLPR!(15)

Dim careerFGS!(MAX_CAREER_YEARS, 10, 17), careerGIS!(MAX_CAREER_YEARS, 55, 3), careerGSS!(MAX_CAREER_YEARS, 55, 3)
Dim careerKRS!(MAX_CAREER_YEARS, 15, 11), careerPRS!(MAX_CAREER_YEARS, 15, 11), careerPUS!(MAX_CAREER_YEARS, 10, 4)
Dim careerQBS!(MAX_CAREER_YEARS, 20, 13), careerRBS!(MAX_CAREER_YEARS, 50, 11), careerWRS!(MAX_CAREER_YEARS, 40, 11)

Dim careerRecPlyr$(6)
Dim careerRecVal!(6), careerTeamTotal!(50)

' For Expanded Leaders
Dim expLdrRBName$(300), expLdrRBTeam$(300), expLdrWRName$(180), expLdrWRTeam$(180), expLdrQBTeam$(120)
Dim expLdrKRName$(90), expLdrKRTeam$(90), expLdrPRName$(90), expLdrPRTeam$(90)
Dim expLdrPuntName$(30), expLdrPuntTeam$(30), expLdrKickName$(60), expLdrKickTeam$(60)
Dim expLdrIntName$(300), expLdrIntTeam$(300), expLdrSackName$(450), expLdrSackTeam$(450)
Dim expLdrQBName$(120)

Dim expLdrLRec$(480), expLdrLKR$(90), expLdrLPass$(120), expLdrLRush$(300), expLdrLPR$(90)
Dim expLdrTeam$(480), expLdrPlyrName$(480), expLdrTD$(480)

Dim expCategories$(74)

Dim expLdrRBGames!(300), expLdrWRGames!(180), expLdrQBGames!(120)

Dim expLdrRBVal!(300, 8), expLdrWRVal!(180, 4), expLdrQBVal!(120, 13), expLdrQBRat!(120, 1)
Dim expLdrKRVal!(90, 4), expLdrPRVal!(90, 4), expLdrPuntVal!(30, 2)
Dim expLdrKickVal!(60, 5), expLdrIntVal!(300), expLdrSackVal!(450)

Dim expLdrVal!(480)



'For Expanded Standings
Dim teamLoseStreak(40), teamWinStreak(40)
Dim teamTotWins(40)

Dim totHomeLosses(40), totHomeWins(40)
Dim totRoadLosses(40), totRoadWins(40)
Dim totNeutralLosses(40), totNeutralWins(40)

Dim totTeamLosses(40), lastLoseStreak(40), lastWinStreak(40)

Dim expStdLeagName$(40)

'----------------------------------------
' Used in COMPILE / SEE routines
'----------------------------------------
Dim Shared confLosses, confTies, confWins
Dim Shared fullLosses, fullTies, fullWins
Dim Shared confPtsFor, confPtsAgnst

Dim fumbGain, puntNum
Dim puntName$

Dim leaderName$(1 To 240), leaderTeam$(1 To 240)
Dim leaderVal!(1 To 240, 1 To 2)
Dim leaderAllPurpName$(18)
Dim leaderAllPurpTDs!(16)
Dim leaderAllPurpYds!(16)

Dim compRB$(10), compWR$(16), compQB$(4)
Dim compKR$(3), compPR$(3), compKicker$(2)

Dim compRB!(11, 11), compWR!(16, 13), compQB!(1 To 11, 1 To 11)
Dim compKR!(11, 11), compPR!(11, 11)
Dim compPunts!(16)
Dim compPAT!(2), compPAM!(2)
Dim compFGA!(2), compFGM!(2)
Dim compPAPct!(2), compFGPct!(2)

Dim indRecDesc$(0 To 50, 0 To 2), teamRecDesc$(0 To 50)
Dim indRecords!(0 To 50), teamRecords!(0 To 50)

Dim leagPtsAgnst!(60), leagPtsFor!(60)

Dim totalScoring!(18), totalTeamVal!(10, 20, 9)
Dim totalTeamName$(10, 20)

Dim natLdrName$(240), natLdrTeam$(240)
Dim natLdrTotTeam$(10, 20)

Dim natLdrTotVal!(10, 20, 9)
Dim natLdrVal!(240, 2)

Dim pollTeam$(1200)
Dim pollRatings!(1200, 5)

Dim statsGeneral!(14, 18)

Dim defTotals!(38), offTotals!(38)


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
Dim intNam_TRADE$(1, 9), sackNam_TRADE$(1, 14)
Dim tmInfo_TRADE$(1), wrNam_TRADE$(2, 6)


' ** Stats File **
Dim teamIndexes(1)

Dim tradeFGA!(1, 1, 4), tradeFGM(1, 1, 4)
Dim tradeGI(1, 9, 2), tradeGS(1, 14, 1)
Dim tradeLcompFGPct!(1, 1), tradeLKR!(1, 2)
Dim tradeLInt!(1, 9), tradeLPR!(1, 2)
Dim tradeLPass!(1, 3), tradeLRec!(2, 15), tradeLRush!(1, 9)
Dim tradeTeamStat!(1, 38), tradeOppStat!(1, 38), tradePlayerStat!(1, 13, 17)

Dim statNam_TRADE$(1)
Dim tradeLFG$(1, 1), tradeLInt$(1, 9, 1)
Dim tradeLKR$(1, 2, 1), tradeLPass$(1, 3, 1)
Dim tradeLRec$(1, 15, 1), tradeLRush$(1, 9, 1)
Dim tradeLPR$(1, 2, 1)

Dim tradeQB$(1, 3), tradeRB$(1, 9), tradeWR$(1, 5)
Dim tradeFG$(1, 1), tradeKR$(1, 2), tradePR$(1, 2)
Dim tradeYN$(1)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim div1Name$, div2Name$, div3Name$, div4Name$
Dim div1Name_2$, div2Name_2$, div3Name_2$, div4Name_2$

Dim divNames$(4), divNames_2nd$(4)

'These are mostly keeping track of quantities (# wins, # losses, etc)
'so they could probably be INTEGERS.
'However from the original code they are intended to be Singles

Dim awayLosses!(30), awayLosses_2nd!(30)
Dim awayTies_2nd!(30), awayTies!(30)
Dim awayWins!(30), awayWins_2nd!(30)
Dim awayScoreTeam!(100), awayScoreTeam_2nd!(100)
Dim awayScoreOpp!(100), awayScoreOpp_2nd!(100)

Dim homeLosses!(30), homeLosses_2nd!(30)
Dim homeTies!(30), homeTies_2nd!(30)
Dim homeWins!(30), homeWins_2nd!(30)
Dim homeScoreTeam!(100), homeScoreTeam_2nd!(100)
Dim homeScoreOpp!(100), homeScoreOpp_2nd!(100)

Dim totHomeLosses!(4), totHomeLosses_2nd!(4)
Dim totHomeTies!(4), totHomeTies_2nd!(4)
Dim totHomeWins!(4), totHomeWins_2nd!(4)
Dim totHomeScoreTeam!(4), totHomeScoreTeam_2nd(4)
Dim totHomeScoreOpp!(4), totHomeScoreOpp_2nd!(4)

Dim totAwayLosses!(4), totAwayLosses_2nd!(4)
Dim totAwayTies!(4), totAwayTies_2nd!(4)
Dim totAwayWins!(4), totAwayWins_2nd!(4)
Dim totAwayScoreTeam!(4), totAwayScoreTeam_2nd!(4)
Dim totAwayScoreOpp!(4), totAwayScoreOpp_2nd!(4)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
Dim indRecCategory$(46), teamRecCategory$(43)


'----------------------------------------
' Used in STAT / INPUT
'----------------------------------------
Dim JA, JB, JC, JD, JE, JF, JG, JH, JI

Dim gameCount(30), scoreTotals(1)

Dim playerStats!(13, 17), sackStats!(1, 1), teamStats!(1, 12)

Dim schedStatTeam$(31, 0 To 29)
Dim schedStatVal!(31, 0 To 29)

Dim ratingsQB!(0 To 3, 10)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim leaderVal1!(240), leaderVal2!(240)
Dim divLosses!(60), divTies!(60), divWins!(60)

Dim leagRecText$(60), leagLdrTeams$(60)
Dim pollLdrTeams$(1200), divTeamNames$(60)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim tickerStart
Dim actualAttendance&, avgAttendance&

Dim Shared ballPosCmpAdj, passTypeCmpAdj, windCmpAdj

'BW = black & white; as in, set to 1 and the game will lose all color
Dim Shared autoPlay, avgRecYds, ballFumbled, BW
Dim Shared catchPctSuccess, checkFumbRev, chosenPlay
Dim Shared coverage, compTeam, catchYds, currDown ', compOffense
Dim Shared D, dLine
Dim Shared endAllGames, endYds, endZone, expCompPct
Dim Shared fgSuccessChance, fgAttYds, ffPctSuccess
Dim Shared fieldSide, firstDownClockOpt
Dim Shared fumbleRoll, fumbleChance, fumbPlayType, fumbleRating
Dim Shared gameLoc, gameOver, goalPostAdj, halfTime

'These all seem to only be used for loops, but it is difficult to confirm
Dim Shared I1, I3, I5, I6, I7, I8, I9

Dim Shared I4 As Single
Dim Shared kickYL!

Dim Shared isOT, JJ, K3
Dim Shared kickDist, playSegment, overtimeOpt, nbrScores
Dim Shared penaltyYds, playerMode, playerOpt, playType
Dim Shared P1, playCall, periodNbr, puntRetNbr
Dim Shared quarter, qback, qbTakeKnee
Dim Shared ruleOptColl, ruleOptPro, ruleOptType, rushBase
Dim Shared S6, startYds, tickerGames
Dim Shared W5, WE, WS, winTeam

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

'The below 2 variables are related to scout report
Dim Shared XD, XE

Dim Shared ydsToFirst As Single
Dim Shared ydsGained As Single, ydLine As Single, ydLineTeam As Single, ydsToScore As Single

Dim Shared convChoice, fgAttLine, impactTeam
DIm Shared loseDown
Dim Shared rushEra, scoreX0, scoreX1
Dim Shared YC, YF, Z1

Dim Shared FF%, goalLnYdAdj, HB%, intChance
Dim Shared PC%, PS%

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared adjFGPct(4), adjFGLine(4)
Dim Shared brkawayYds(38, 4), climate(6)

Dim Shared gameFGA!(1, 1, 0 To 4), gameFGM!(1, 1, 0 To 4)
Dim Shared gameLRec!(1, 20), gameLFG!(1, 1), gameLInt!(1, 0 To 9), gameLKR!(1, 0 To 2), gameLPass!(1, 0 To 3), gameLRush!(1, 0 To 9)
Dim Shared gameLPR!(0 To 1, 0 To 2)
Dim Shared gameTeamStat!(0 To 38), gameOppStat!(0 To 38), gamePlayerStat!(0 To 13, 0 To 17)

Dim Shared activeKicker(1)
Dim Shared defInts(1), defSacks(1)
Dim Shared defYdAdj(1 To 7, 1 To 12)
Dim Shared defYdAdjEraA(1 To 7, 1 To 12), defYdAdjEraB(1 To 7, 1 To 12)
Dim Shared FA(1, 1, 4), FM(1, 1, 4)
Dim Shared gameInts(1, 9, 2), gameLongFG(1, 1), gameStatsPlayer(1, 13, 17), gameStatsTeam(1, 36)

Dim Shared goalLnYdAdj(1 To 30, 1 To 2)
Dim Shared hasRunFF(1), hasRunRev(1)
Dim Shared indRushPct(1, 9), IR(1, 9), K1(50, 6), K3(1, 6)
Dim Shared kickerFGA(1, 1), kickerFGPct(1, 1), kickerIdx(1, 1), kickerPATPct(1, 1)
Dim Shared kickReturners(0 To 1, 0 To 2), krNumRet(1, 2), krYdsPerRet(1, 2)
Dim Shared leagRat_GAME(1, 7)
Dim Shared nbrPossOT(1), otTDs(0 To 1)
Dim Shared passCovAdj(3, 10, 11), passFakeSuccess(0 To 2)
Dim Shared playerInts(1, 9), playerSacks(1, 14)
Dim Shared prNumRet(1, 2), prYdsPerRet(1, 2), puntReturners(0 To 1, 0 To 2)
Dim Shared quarterbacks(1, 3), qbNumber(1)
Dim Shared qbArmRat(1, 3), qbMobility(1), qbNumAtt(1, 9), qbCompPct(1, 9), qbPctInt(1, 9)
Dim Shared qbRushIdx(1, 3)
Dim Shared runYdAdj(1, 38)
Dim Shared runBacks(1, 17), rbRushContrib(1, 9), rbRushAvg(1, 9), rbNumRec(1, 9), rbYdsPerC(1, 9)
Dim Shared sackStatsPlayer(1, 14, 1)
Dim Shared score(0 To 1, 0 To 20), scoreQuarters(50), scoreTimes(50), schedGame(2)
Dim Shared teamIdx_GAME(2), teamInts(1), teamSacks(1), timeouts(1), timePoss(1), teamRat_GAME(2, 9)
Dim Shared teamYears(1), thirdDownAtt(1), thirdDownFail(1)
Dim Shared totalInts(0 To 1, 0 To 9, 0 To 2), totalSacks(0 To 1, 0 To 14, 0 To 1)
Dim Shared useRandomQB(1)
Dim Shared sackStatsTeam(1, 1), WR(1, 20), wrNumRec(1, 9), wrYdsPerC(1, 9)
Dim Shared XD(1), ydsPerComp(1), ydsPerCompPctAdj(50, 2), ydsPerPunt(1, 2)

Dim Shared HB%(1), NG%(20), PS%(2, 21)
Dim Shared RM%(1, 14)
Dim Shared S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14)

Dim scheduleFile$

Dim Shared defTeam$, gameStadium$, markers$
Dim Shared pbpString$, targetWRName$, YN$

Dim Shared defFormation$(1 To 10)
Dim Shared defFormEraA$(1 To 10), defFormEraB$(1 To 10)
Dim Shared defPlay$(1 To 15)
Dim Shared defInts$(1, 9), defSacks$(1, 14)
Dim Shared downDesc$(0 To 4), direction$(0 To 1)
Dim Shared gameMascots$(1), gameTeams$(0 To 1), gadget$(3)
Dim Shared kicker$(1, 1), kickRet$(1, 2)
Dim Shared passCov$(0 To 11), playDesc$(30), playSelect$(9)
Dim Shared pret$(1, 2), punter$(1, 2), qbacks$(1, 3)
Dim Shared rbacks$(1, 10), SX$(1 To 33, 0 To 1)
Dim Shared diskIDs$(0 To 1), tickerPeriod$(14), wdRec$(1, 5)

'These appear only used to be assigned a "t" value when a TD is implied.
'They are included in the compiling of stats file
'I believe they are for tracking records / "longest" plays
Dim Shared gameIR$(1, 9), gameKR$(1, 2), gameLRec$(1, 20, 1), gameLRush$(1, 9, 1)
Dim Shared gamePR$(1, 2), gameRB$(1, 17), gameQB$(1, 3), gameWR$(1, 20)
