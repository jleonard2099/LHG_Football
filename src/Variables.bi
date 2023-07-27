'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)

' *** Reading Team Data ***
' -------------------------
Dim Shared A$

'-- transition away from this
Dim Shared fileLength&

Dim teamAttendance&

Dim ydsCompAdj 'YC! is used elsewhere

Dim mascot$, stadium$, teamNick$

Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

Dim leagueRatings%(7), teamRatings%(5)

Dim rushAtt(9), rushAvg(9), rbRecepts(15), rbRecAvg(9)
Dim wrRecept(5), wrRecAvg(5)
Dim passAtt(0 To 3), compPct(3), qbIntPct(3)
Dim krRet(2), krRetAvg(2), prRet(2), prRetAvg(2), puntAvg(1)
Dim pkFGA(1), pkFGPct(1), pkPAT(1), pkPATPct(1)
Dim numInts(9), numSacks(14), armRating(3)

Dim rbName$(9), wrName$(5), qbName$(4), krName$(2), prName$(2)
Dim punterName$(3), pkName$(1), intName$(9), sackName$(15)


' *** Schedule Data ***
' -------------------------
Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)
Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim yearNumber$(MAX_GAMES, 1)

' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$, TN$

Dim modeAbbrev$(3), yesNo$(1)
Dim Shared location$(2), MO$(3)
Dim Shared strG9$(5), SD$(3)
Dim Shared Y8$(3), Y9$(5)
Dim WN$(3), WT$(3)

' *** Miscellaneous Use ***
' -------------------------
Dim Shared NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

Dim Shared confLosses, confTies, confWins
Dim Shared fullLosses, fullTies, fullWins
Dim Shared PTSFC, PTSAC

Dim AN1$(9), AN2$(5), AN3$(3), AN4$(2), AN5$(2), AN6$(3), AN7$(1)
Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(3), AR7$(1)
Dim AR$(1), DIN$(9), DRI$(9), DSN$(14), DSR$(15)
Dim LC$(15, 1), LCN$(15, 1), LCR$(15, 1), LFN$(1), LFR$(1)
Dim statsLF$(1), statsLI$(9, 1), statsLK$(2, 1), statsLP$(3, 1)
Dim LIN$(9, 1), LIR$(9, 1), LKN$(2, 1), LKR$(2, 1)
Dim LPN$(3, 1), LPR$(3, 1), LR$(9, 1), LRN$(9, 1), LRR$(9, 1)
Dim PKN$(2, 1), PKR$(2, 1)

Dim statsPK$(2, 1)

Dim statsGI%(9, 2), GIN%(9, 2), GIR%(9, 2), statsGS%(15, 1), GSN%(15, 1), GSR%(15, 1)
Dim statsZ1%(40)

Dim AF!(1, 4), AFN!(1, 4), AFR!(1, 4)
Dim AMR!(1, 4), AM!(1, 4), AMN!(1, 4)
Dim LC!(16), LCN!(15), LCR!(15), LI!(0 To 9), LIN!(0 To 9), LIR!(0 To 9)
Dim LF!(0 To 9), LFR!(1), LFN!(1), LK!(3), LKN!(2), LKR!(2)
Dim LP!(4), LPN!(3), LPR!(3), LR!(9), LRN!(0 To 9), LRR!(0 To 9)
Dim PK!(3), PKN!(2), PKR!(2)
Dim statsZ!(38), statsZ1!(38)
Dim statsZ2!(50, 18), ZN2!(14, 18), ZR!(38), ZR1!(38), ZR2!(14, 18)


'----------------------------------------
' Used in ALIGN / MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%

Dim alignA$(15), alignAR$(15)
Dim ORD$(120)

Dim AR(15, 14)
Dim ORD%(120)
Dim W0R(15), WR1(15)
Dim ZR(15), ZR1(15)

Dim AN$(15)
Dim AN(15, 14)
Dim W0N(15), WN1(15)


'----------------------------------------
' Used in CAREER routines
'----------------------------------------
Dim careerA1$(50), careerA2$(40), careerA3$(20), careerA4$(15), careerA5$(15), careerA6$(10), careerA7$(10)
Dim careerDI$(55), careerDS$(55), careerKRS$(15, 15, 1), careerPRS$(15, 15, 1)
Dim careerQBS$(15, 20, 1), careerRBC$(9), careerRBS$(15, 50, 1), careerRC$(6), careerWRS$(15, 40, 1)

Dim carviewA1$(1, 50), carviewA2$(1, 40), carviewA3$(1, 20), carviewA4$(1, 15), carviewA5$(1, 15), carviewA6$(1, 10), carviewA7$(1, 10)
Dim carviewDI$(1, 55), carviewDS$(1, 55)

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

Dim BRC$(50, 2), compZ1$(60), compZ2$(240), compZ3$(240)
Dim compPK$(2), H1$(30)
Dim KR$(3), N2$(240), N3$(240), NT$(10, 20)
Dim PR$(3), compPS$(10), PT$(1200), QB$(4), RB$(10)
Dim TB$(18), TR$(10, 20), TRC$(50), TT$(10, 20)
Dim WR$(16)


Dim BRC!(50), compZ1!(240, 2), compZ2!(1 To 14, 1 To 18)
Dim DT!(38), EA!(2), EM!(2), EP!(2)
Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(60), O2!(60), O3!(60), OT!(38)
Dim PR!(11, 11), PT!(1200, 5), PTSA!(60), PTSF!(60)
Dim QB!(11, 11), RB!(11, 11)
Dim TD!(16), TG!(16), TP!(18), TRC!(50), TT!(10, 20, 9)
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
Dim tradeLC$(1, 15, 1), tradeLF$(1, 1), tradeLI$(1, 9, 1)
Dim tradeLK$(1, 2, 1), tradeLP$(1, 3, 1), tradeLR$(1, 9, 1)
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
'	ind recs, team recs
Dim TP$(46), TP1$(43)


'----------------------------------------
' Used in STAT / INPUT
'----------------------------------------
Dim JA, JB, JC, JD, JE, JF, JG, JH, JI
Dim TMR, totStats

Dim statsO%(120), lookyP%(1)
Dim statsT%(120), TR%(120), Y%(30)

Dim GMA!(300), GMB!(180), GMC!(120)

Dim A1L!(300, 8), A2L!(180, 4), A3L!(120, 13), A3R!(120, 1)
Dim A4L!(90, 4), A5L!(90, 4), A6L!(30, 2)
Dim A7L!(60, 5), A8L!(300), A9L!(450)
Dim CRD!(120), CRDR!(120)
Dim GI!(1, 9, 2), GS!(1, 14, 1)
Dim K!(1, 12), K2!(1, 13, 17)
Dim TYP!(480), lookyTT!(31, 30), W6!(1, 1)

Dim A1L$(300), A1T$(300), A2L$(180), A2T$(180), A3T$(120), A4L$(90), A4T$(90)
Dim A5L$(90), A5T$(90), A6L$(30), A6T$(30), A7L$(60), A7T$(60)
Dim A8L$(300), A8T$(300), A9L$(450), A9T$(450)
Dim compO$(120), expCategories$(74), statsH$(120), HR$(120)
Dim LCL$(480), LKL$(90), LPL$(120), LRL$(300), PKL$(90)
Dim lookyA1$(1, 10), lookyA2$(1, 6), lookyA3$(1, 4), lookyA4$(1, 3), lookyA5$(1, 3), lookyA6$(1, 2), lookyA7$(1, 2)
Dim intNam_TRADE$(1, 9), sackNam_TRADE$(1, 14), lookyTT$(31, 30)
Dim QBL$(120), gameSITE$(120), SITER$(120)
Dim TMM$(480), TPP$(480), TYY$(480)

Dim Z1$(40), Z2$(40)



'----------------------------------------
' Used in SEExxx routines
'----------------------------------------
Dim L!(60), LD!(240), LZ!(240), T!(60), W!(60)
Dim seeP$(60), TM$(60) ', TR$(10, 20)
Dim seePR$(1200), seeT$(60)


'----------------------------------------
' Used in SCHEDULE routines
'----------------------------------------
Dim BS%, NS%

Dim scheduleAP%(1), scheduleL%(13), scheduleNG%(MAX_GAMES, 20)

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleQB_V$(20), scheduleQB_H$(20)
Dim scheduleYN$(MAX_GAMES, 1)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$

Dim tickerStart

Dim Shared A, A2, A3, A4, A5, A6, A7, A8, A9, AP
Dim Shared B, B1, B2, B3, B4, B5, B7, B8, BW, CP, CT
Dim Shared D, D1, D2, D3, DDI, DDS, DI, DR, DS, DT
Dim Shared E, EY, endGame, endAllGames, F2, F8, F9
Dim Shared G, G9, halfTime, I1, I2, I3, I4, I5, I6, I7, I8, I9, I
Dim Shared J, J6, JJ, K3, N, NT, O, OT, P1, P2, PN, PQ, PR, PW
Dim Shared Q, QB, R1, RP, RY, S, S2, S3, S6, SN, SY
Dim Shared T1, TMT, W5, WE, WS, X, X1, X2, XD, XE
Dim Shared Y, Y1, Y9, YC, YF, YL, YT, Z1

Dim Shared intB8%, BO%, C%, EG%, F%, GL%, H%, HB%, intI%, intDI%, intDS%
Dim Shared PA%, PC%, PS%, Q6%, Q7%, QBN%, QX%, R5%, RF%, S1%, SX%
Dim Shared W%, WX%, intY8%, intYL%

Dim Shared gameClock!, pbpDelay!, timeElapsed!

Dim Shared actualAttendance&, averageAttendance&

Dim Shared A(1, 9), A1(1, 9), A2(1, 9), A3(1, 9), A4(1, 9), A5(1, 9), A6(1, 9), A7(1, 9), A8(1, 9), A9(1, 2)
Dim Shared AF(1, 1, 4), AM(1, 1, 4), B(1, 2), B1(1, 2), B2(1, 2), B3(1, 2), B4(1, 1), B5(1, 1), B6(1, 7), B7(1, 1), B8(1, 1)
Dim Shared C(50), DDI(1), DDS(1), DI(1, 9), DS(1, 14), F(4), F1(4), FA(1, 1, 4), FL(1, 1), FM(1, 1, 4)
Dim Shared GI(1, 9, 2), GS(1, 14, 1), IR(1, 9)
Dim Shared K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6), KR(1, 2)
Dim Shared LC(1, 20), LF(1, 1), LI(1, 9), LK(1, 2), LP(1, 3), LR(1, 9), OT(1), P(2), PK(1, 2), PR(1, 2)
Dim Shared QB(1, 3), RB(1, 17), S(0 To 1, 0 To 10), SI(1, 9, 2), SK(1, 14, 1), scoreTimes(50), T1(1), timePoss(1)
Dim Shared W6(1, 1), WR(1, 20), XD(1), YC(1)

Dim Shared AP%(2), intB8%(2), BY%(38, 4), D3%(0 To 8, 0 To 10), intDI%(1), intDS%(1)
Dim Shared intG9%(1), GL%(1 To 30, 1 To 2), HB%(1)
Dim Shared NG%(20), O%(120), PA%(2, 9), PC%(1, 9), intPR%(1, 120), PS%(2, 21)
Dim Shared Q6%(1), Q7%(1), QR%(50, 2), QX%(1, 3), R9%(1), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14)
Dim Shared TE%(1), TF%(1), TeamScore%(120), V4%(1, 3), WX%(6)

Dim Shared Z(38), Z1(38), Z2(13, 17)

Dim Shared CRD&(120)

Dim Shared A1$, A2$, A3$, A4$, A5$, A6$, A7$, D2$, DI$, DN$, DR$, DS$, DV$
Dim Shared F$, G$, strG9$, I$, LO$, NM$, NN$, PS$, Q$, RP$, RV$, NY$
Dim Shared SD$, SITE$, SX$, U$, U5$, X$, Y8$, Y9$, YN$

Dim Shared A$(0 To 1), A1$(1, 10), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2), A7$(1, 1)
Dim Shared AA$(1), B$(1), D$(15), D1$(11), D2$(15), DI$(1, 9), DN$(4), DR$(1), DS$(1, 14)
Dim Shared G$(3), H$(120), HO$(120), strIR$(1, 9), strKR$(1, 2)
Dim Shared strLC$(1, 20, 1), LF$(1, 1), LI$(1, 9, 1), LK$(1, 2, 1), LP$(1, 3, 1), strLR$(1, 9, 1)
Dim Shared O$(50), NN$(0 To 1), P$(2), PK$(1, 2, 1), strPR$(1, 2), PS$(9), strQB$(1, 3)
Dim Shared R$(14), strRB$(1, 17), RP$(30), SITE$(120), SX$(1 To 33, 0 To 1)
Dim Shared strWR$(1, 20), Y$(1), YN$(1), YR%(1)
