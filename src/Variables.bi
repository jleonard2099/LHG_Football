'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------
Dim statsO%(NUM_STATRECORDS), statsPR%(1, 120)
Dim teamScore(NUM_STATRECORDS)

Dim statsZ!(38), statsZ1!(38), statsZ2!(50, 18)

Dim gamePK$(1, 2, 1), HO$(NUM_STATRECORDS)
Dim statsH$(NUM_STATRECORDS), statsO$(NUM_STATRECORDS)
Dim statStadium$(NUM_STATRECORDS)
Dim statsZ1$(40), statsZ2$(40)

Dim statsAttendance&(NUM_STATRECORDS)


'-- For Utilities
Dim ORD%(NUM_STATRECORDS)
Dim TR%(NUM_STATRECORDS)

Dim CRD!(NUM_STATRECORDS), CRDR!(NUM_STATRECORDS)

Dim gameSite$(NUM_STATRECORDS), gameSiteRoad$(NUM_STATRECORDS)
Dim ORD$(NUM_STATRECORDS)


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
Dim LC!(0 To 15), LF!(0 To 9), LI!(0 To 9), LK!(0 To 3)
Dim LP!(0 To 3), LR!(0 To 9), PK!(0 To 2)

Dim LCN!(15), LCR!(15), LIN!(0 To 9), LIR!(0 To 9)
Dim LFR!(1), LFN!(1), LKN!(2), LKR!(2)
Dim LPN!(3), LPR!(3), LRN!(0 To 9), LRR!(0 To 9)
Dim PKN!(2), PKR!(2)

Dim ZN2!(14, 18), ZR!(38), ZR1!(38), ZR2!(14, 18)


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
Dim FG
Dim puntNum
Dim PU$

Dim compAP%(1, 120), APR%(1, 120)
Dim compT%(NUM_STATRECORDS)

Dim BRC$(50, 2), compPK$(2)
Dim compZ1$(50), compZ2$(240), compZ3$(240)
Dim H1$(30), KR$(3), N2$(240), N3$(240), NT$(10, 20)
Dim PR$(3), PS$(10), PT$(1200), QB$(4), RB$(10)
Dim TB$(18), TR$(10, 20), TRC$(50), TT$(10, 20)
Dim WR$(16)

Dim compTT!(10, 20, 9), compZ1!(240, 2), compZ2!(14, 18)
Dim BRC!(50), DT!(38), EA!(2), EM!(2), EP!(2)
Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(50), O2!(50), O3!(50), OT!(38)
Dim PR!(11, 11), PT!(1200, 5), PTSA!(50), PTSF!(50)
Dim QB!(11, 11), RB!(11, 11)
Dim TD!(16), TG!(16), TP!(18), TRC!(50)
Dim WR!(16, 13), YD!(16)



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
Dim tradePK!(1, 2)
Dim tradeT!(1)
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

'These are mostly keeping track of
'quantities (# wins, # losses, etc)
'so they could probably be INT's
'However from the original code
'they are intended to be Singles
Dim AL!(30), AL2!(30), AW!(30), AW2!(30)
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

Dim lookyP%(1), Y%(30)

Dim GMA!(300), GMB!(180), GMC!(100)

Dim A1L!(300, 8), A2L!(180, 4), A3L!(100, 13), A3R!(100, 1)
Dim A4L!(90, 4), A5L!(90, 4), A6L!(30, 2)
Dim A7L!(60, 5), A8L!(300), A9L!(450)
Dim K!(1, 12)
Dim TYP!(480), lookyTT!(31, 30), W6!(1, 1)

Dim A1L$(300), A1T$(300), A2L$(180), A2T$(180), A3T$(100), A4L$(90), A4T$(90)
Dim A5L$(90), A5T$(90), A6L$(30), A6T$(30), A7L$(60), A7T$(60)
Dim A8L$(300), A8T$(300), A9L$(450), A9T$(450)
Dim expCategories$(74), HR$(100)

Dim intNam_TRADE$(1, 9), sackNam_TRADE$(1, 14)
Dim LCL$(480), LKL$(90), LPL$(100), LRL$(300), PKL$(90)
Dim lookyRBacks$(10), lookyWdRec$(6), lookyQBacks$(4), lookyKickRet$(3), lookyPRet$(3), lookyPunter$(2), lookyKicker$(2)
Dim lookyTT$(31, 30)
Dim QBL$(100)
Dim TMM$(480), TPP$(480), TYY$(480)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim L!(50), LD!(240), LZ!(240), T!(50), W!(50)
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
Dim scheduleFile$
Dim tickerStart
Dim actualAttendance&, avgAttendance&

Dim Shared A, A2, A3, A4, A5, A6, A7, A8, A9, autoPlay
Dim Shared B, B1, B2, B3, B4, B5, B7, B8, BW
Dim Shared CP, compTeam ', compOffense
Dim Shared D, D1, D2, D3, DDI, DDS, DI, DR, DS, DT
Dim Shared E, EY, endGame, endAllGames, F2, F8, F9
Dim Shared G, gameLoc, goalPostAdj, halfTime
Dim Shared I1, I2, I3, I4, I5, I6, I7, I8, I9, I
Dim Shared J, JJ, K3, N, NT, O, OT, overtimeOpt
Dim Shared playerMode, playerOpt, P1, P2, PN, PQ, PR, PW
Dim Shared Q, QB, ruleOptColl, ruleOptPro, ruleOptType, R1, RP, RY
Dim Shared S, S2, S3, S6, SY
Dim Shared T1, TMT, W5, WE, WS, winTeam
Dim Shared X, X1, X2, XD, XE
Dim Shared Y, Y1, YC, YF, YL, YT, Z1

Dim Shared BO%, C%, EG%, F%, GL%, HB%, intChance
Dim Shared PA%, PC%, PS%, Q6%, Q7%, QBN%, QX%, R5%, RF%, S1%, SX%
Dim Shared WX%, yrdLine

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared AF(1, 1, 0 to 4), AM(1, 1, 0 to 4)
Dim Shared C(50), DDI(1), DDS(1), defInts(1), defSacks(1)
Dim Shared F(4), F1(4), FA(1, 1, 4), FL(1, 1), FM(1, 1, 4)
Dim Shared K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6)
Dim Shared gameZ(0 to 38), gameZ1(0 to 38), gameZ2(0 to 13, 0 to 17)
Dim Shared GI(0 to 1, 0 to 9, 0 to 2), GS(0 to 1, 0 to 14, 0 to 1), hasRunFF(1), ints(1, 9), IR(1, 9)
Dim Shared kickerFGA(1, 1), kickerFGPct(1, 1), kickerNumPAT(1, 1), kickerPATPct(1, 1)
Dim Shared KR(1, 2), krNumRet(1, 2), krYdsPerRet(1, 2)
Dim Shared LC(1, 20), leagRat_GAME(1, 7), LF(1, 1), LI(1, 0 to 9), LK(1, 0 to 2), LP(1, 0 to 3), LR(1, 0 to 9), OT(1)
Dim Shared P(2), PK(0 to 1, 0 to 2), PR(1, 2), prNumRet(1, 2), prYdsPerRet(1, 2), puntYdsPerP(1, 2)
Dim Shared QB(1, 3), qbArmRat(1, 3), qbMobility(1), qbNumAtt(1, 9), qbCompPct(1, 9), qbPctInt(1, 9)
Dim Shared RB(1, 17), rbRushAtt(1, 9), rbRushAvg(1, 9), rbNumRec(1, 9), rbYdsPerC(1, 9)
Dim Shared sacks(1, 14), score(0 To 1, 0 To 10), scoreTimes(50), schedGame(2)
Dim Shared SI(1, 9, 2), SK(1, 14, 1), teamIdx_GAME(2), timeouts(1), timePoss(1), tmRat_GAME(2, 9)
Dim Shared W6(1, 1), WR(1, 20), wrNumRec(1, 9), wrYdsPerC(1, 9), XD(1), ydsPerComp(1)

Dim Shared BY%(38, 4), D3%(0 To 8, 0 To 10)
Dim Shared GL%(1 To 30, 1 To 2), HB%(1)
Dim Shared NG%(20), PC%(1, 9), PS%(2, 21)
Dim Shared Q6%(1), Q7%(1), QR%(50, 2), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14)
Dim Shared thirdDownAtt(1), thirdDownFail(1)
Dim Shared V4%(1, 3), WX%(6), YR%(1)

Dim Shared A1$, A2$, A3$, A4$, A5$, A6$, A7$
Dim Shared D2$, DI$, DN$, DR$, DS$, DV$, F$
Dim Shared G$, gameStadium$, I$, LO$, NM$, NN$, NY$
Dim Shared PS$, Q$, ruleOptPro$, ruleOptColl$, RP$, RV$
Dim Shared SD$, SX$, U$, U5$, X$, YN$

Dim Shared B$(1), D2$(15), defInts$(1, 9), downDesc$(4), DR$(1), defSacks$(1, 14)
Dim Shared D$(15), D1$(11), offensePlay$(50), R$(14)
Dim Shared gameMascots$(1), gameTeams$(0 To 1), G$(3)
Dim Shared kicker$(1, 1), kickRet$(1, 2)
Dim Shared NN$(0 To 1), P$(2), playSelect$(9)
Dim Shared qbacks$(1, 3), pret$(1, 2), punter$(1, 2), rbacks$(1, 10)
Dim Shared RP$(30), SX$(1 To 33, 0 To 1)

'Trying to figure out how these below are used.
'It seems obvious but they are used sparingly. 
'They appear to simply be assigned a "t" value when a TD is implied.
'They are included in the compiling of stats file
Dim Shared gameIR$(1, 9), gameKR$(1, 2), gameLC$(1, 20, 1), gameLR$(1, 9, 1)
Dim Shared gamePR$(1, 2), gameRB$(1, 17), gameQB$(1, 3), gameWR$(1, 20) 

Dim Shared wdRec$(1, 5), Y$(1), YN$(1)
