'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim opSysType$

Dim homeScores(MAX_GAMES), visitorScores(MAX_GAMES)

Dim yearNumber$(MAX_GAMES, 1)
Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)

Dim Shared NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

Dim Shared confLosses, confTies, confWins
Dim Shared fullLosses, fullTies, fullWins
Dim Shared PTSFC, PTSAC

Dim Shared fileLength&, teamAttendance&

'A$ / AA$ may not be needed anymore
'S$ could be converted as well to Dim / Shared
Dim Shared A$, AA$, S$

'Used only with career files
Dim Shared DT$
Dim Shared TM$, TN$

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' Game Options
Dim modeAbbrev$(3), yesNo$(1)
Dim Shared location$(2), MO$(3)
Dim Shared strG9$(5), SD$(3)
Dim Shared Y8$(3), Y9$(5)
Dim WN$(3), WT$(3)

'----------------------------------------
' Used across multiple routines
'----------------------------------------
Dim AN1$(9), AN2$(5), AN3$(3), AN4$(2), AN5$(2), AN6$(3), AN7$(1)
Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(3), AR7$(1)
Dim AR$(1), DIN$(9), DRI$(9), DSN$(14), DSR$(15)
Dim LC$(15, 1), LCN$(15, 1), LCR$(15, 1), LFN$(1), LFR$(1)
Dim statsLF$(1), statsLI$(9, 1), statsLK$(2, 1), statsLP$(3, 1)
Dim LIN$(9, 1), LIR$(9, 1), LKN$(2, 1), LKR$(2, 1)
Dim LPN$(3, 1), LPR$(3, 1), LR$(9, 1), LRN$(9, 1), LRR$(9, 1)
Dim PKN$(2, 1), PKR$(2, 1)
Dim statsAA$(1)
Dim statsA1$(9), statsA2$(5), statsA3$(4), statsA4$(2), statsA5$(2), statsA6$(3), statsA7$(1)
Dim statsDI$(9), statsDS$(15)
Dim statsPK$(2, 1)

'TP$() = individual record categories
'TP1$() = team record categories
Dim TP$(46), TP1$(43)


Dim statsGI%(9, 2), GIN%(9, 2), GIR%(9, 2), statsGS%(15, 1), GSN%(15, 1), GSR%(15, 1)
Dim statsPA%(5), statsZ1%(40)

Dim AF!(1, 4), AFN!(1, 4), AFR!(1, 4)
Dim AMR!(1, 4), AM!(1, 4), AMN!(1, 4)
Dim LC!(16), LCN!(15), LCR!(15), LI!(0 To 9), LIN!(0 To 9), LIR!(0 To 9)
Dim LF!(0 To 9), LFR!(1), LFN!(1), LK!(3), LKN!(2), LKR!(2)
Dim LP!(4), LPN!(3), LPR!(3), LR!(9), LRN!(0 To 9), LRR!(0 To 9)
Dim PK!(3), PKN!(2), PKR!(2)
Dim statsZ!(38), statsZ1!(38)
Dim statsZ2!(50, 18), ZN2!(14, 18), ZR!(38), ZR1!(38), ZR2!(14, 18)


'----------------------------------------
' Used across ALIGN routines
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
' Used across CAREER routines
'----------------------------------------
Dim careerA0%(9), careerA1%(9), careerA2%(15), careerA3%(9), careerA4%(5)
Dim careerA5%(5), careerA6%(0 To 3), careerA7%(3), careerA8%(3), careerA9%(2)
Dim careerB%(2), careerB1%(2), careerB2%(2), careerB3%(1), careerB4%(1)
Dim careerB5%(1), careerB6%(7), careerB7%(1), careerB8%(1)
Dim careerDI%(9), careerDS%(14), careerQX%(3)

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
' Used across COMPILER routines
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
' Used across DRAFT.BAS routines
'----------------------------------------
Dim ydsCompAdj
Dim ATT&

Dim C0%(9), C1%(9), C2%(15), C3%(9), C4%(5), C5%(5), C6%(3), C7%(3), C8%(3), C9%(2)
Dim D%(2), D1%(2), D2%(2), draftD3%(2), D4%(1), D5%(1), D6%(7), D7%(1), D8%(1)
Dim FI%(9), FS%(14)
Dim draftSX%(3), QA%(4)

Dim C1$(9), C2$(5), C3$(3), C4$(2), C5$(2), C6$(2), C7$(1)
Dim FI$(9), FS$(14)


'----------------------------------------
' Used across NEWLDR.BAS,
'   LOOKY.BAS routines
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
Dim lookyDI$(1, 9), lookyDS$(1, 14), lookyTT$(31, 30)
Dim QBL$(120), gameSITE$(120), SITER$(120)
Dim TMM$(480), TPP$(480), TYY$(480)

Dim Z1$(40), Z2$(40)


'----------------------------------------
'   Used across Head-To-Head routines
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
' Used across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$

Dim scheduleAP%(1), scheduleL%(13), scheduleNG%(MAX_GAMES, 20)

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleQB_V$(20), scheduleQB_H$(20)
Dim scheduleYN$(MAX_GAMES, 1)

'----------------------------------------
' Used across TRADE.BAS routines
'----------------------------------------
Dim statsA$(1)
Dim tradeA1$(2, 9), tradeA2$(2, 6), tradeA3$(2, 4), tradeA4$(2, 3)
Dim tradeA5$(2, 3), tradeA6$(2, 3), tradeA7$(1, 1)
Dim tradeF$(1), tradeFG$(1, 1), tradeKR$(1, 2)
Dim NY$(1), PU$(1), SA$(1), TN$(1), tradeYN$(1)

Dim tradeA0%(1, 9), tradeA1%(1, 9), tradeA2%(2, 15), tradeA3%(1, 9), tradeA4%(1, 5)
Dim tradeA5%(1, 5), tradeA6%(1, 3), tradeA7%(1, 3), tradeA8%(1, 3), tradeA9%(1, 2)
Dim tradeB%(1, 2), tradeB1%(1, 2), tradeB2%(1, 2), tradeB3%(1, 3), tradeB4%(1, 1)
Dim tradeB5%(1, 1), tradeB6%(1, 7), tradeB7%(1, 1), tradeB8%(1, 1)
Dim tradeDI%(1, 9), tradeDS%(1, 14), tradeGI%(1, 9, 2), tradeGS%(1, 14, 1)
Dim tradePA%(1, 4), tradeQX%(1, 3), tradeYC%(1)
Dim tradeS(1)

Dim tradeT!(1)
Dim tradeLC$(1, 15, 1), tradeLF$(1, 1), tradeLI$(1, 9, 1)
Dim tradeLK$(1, 2, 1), tradeLP$(1, 3, 1), tradeLR$(1, 9, 1)
Dim tradePK$(1, 2, 1), tradePR$(1, 2)
Dim tradeQB$(1, 3), tradeRB$(1, 9), tradeWR$(1, 5)
Dim tradeAF!(1, 1, 4), tradeAM!(1, 1, 4)
Dim tradeLC!(2, 15), tradeLF!(1, 1), tradeLI!(1, 9)
Dim tradeLK!(1, 2), tradeLP!(1, 3), tradeLR!(1, 9)
Dim tradePK!(1, 2)
Dim tradeZ!(1, 38), tradeZ1!(1, 38), tradeZ2!(1, 13, 17)
Dim YC!(1)


'----------------------------------------
' Used across SEExxx.BAS routines
'----------------------------------------
Dim L!(60), LD!(240), LZ!(240), T!(60), W!(60)
Dim seeP$(60), TM$(60) ', TR$(10, 20)
Dim seePR$(1200), seeT$(60)


'----------------------------------------
' Used across Game Routines
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
Dim Shared J6(1), K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6), KR(1, 2)
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
