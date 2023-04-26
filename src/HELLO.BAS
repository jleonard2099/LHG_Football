$Debug

Const GAME_VERSION$ = "6.40"
Const MAX_TEAMS = 500
Const MAX_GAMES = 1500
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

'Const NUM_STATRECORDS = 162

DefInt A-Z

'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'

' For Utils
'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'TITLE.BI'
'INCLUDE: 'Variables.BI'



'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared A$, AA$, S$
Dim Shared ATT&

'Used only with career files
Dim Shared TN$
Dim Shared NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

Dim Shared confLosses, confTies, confWins
Dim Shared fullLosses, fullTies, fullWins

Dim Shared DT$, TM$


'File size - does this really need shared?
Dim Shared Y As Long

Dim Shared diskPaths$(0 To 3), Q$(0 To 230), QQ$(0 To 4)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

Dim Shared DirCount%

Dim yearNumber$(MAX_GAMES, 1)
Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)


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
Dim statsA$(1), statsAA$(1)
Dim statsA1$(9), statsA2$(5), statsA3$(4), statsA4$(2), statsA5$(2), statsA6$(3), statsA7$(1)
Dim statsDI$(9), statsDS$(15)
Dim statsPK$(2, 1)

Dim statsGI%(9, 2), GIN%(9, 2), GIR%(9, 2), statsGS%(15, 1), GSN%(15, 1), GSR%(15, 1)
Dim statsPA%(5), statsZ1%(40)

Dim AF!(1, 4), AFN!(1, 4), AFR!(1, 4)
Dim AMR!(1, 4), AM!(1, 4), AMN!(1, 4)
Dim LC!(16), LCN!(15), LCR!(15), LI!(0 To 9), LIN!(0 To 9), LIR!(0 To 9)
Dim LF!(0 To 9), LFR!(1), LFN!(1), LK!(2), LKN!(2), LKR!(2)
Dim LP!(3), LPN!(3), LPR!(3), LR!(9), LRN!(0 To 9), LRR!(0 To 9)
Dim PK!(2), PKN!(2), PKR!(2)
Dim Z!(38), Z1!(38)
Dim Z2!(50, 18), ZN2!(14, 18), ZR!(38), ZR1!(38), ZR2!(14, 18)


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

Dim H1$(30)
Dim TR$(10, 20)

Dim BRC$(50, 2), compZ1$(60), compZ2$(240), compZ3$(240)
Dim KR$(3), compPK$(2)
Dim N2$(240), N3$(240), NT$(10, 20)
Dim PR$(3), compPS$(10), PT$(1200), QB$(4), RB$(10)
Dim TB$(18), TRC$(50), TT$(10, 20), WR$(16)

Dim DT!(38), TT!(10, 20, 9)

Dim BRC!(50), compZ1!(240, 2) ', DT!(38)
Dim EA!(2), EM!(2), EP!(2)
Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(60), O2!(60), O3!(60), OT!(38)
Dim PR!(11, 11), PT!(1200, 5), PTSA!(60), PTSF!(60)
Dim QB!(11, 11), RB!(11, 11)
Dim TD!(16), TG!(16), TP!(18), TRC!(50)
Dim WR!(16, 13), YD!(16)

Dim compAP%(1, 120), APR%(1, 120)


'---------------------------------------ts
' Used across DRAFT.BAS routines
'----------------------------------------
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

Dim MO$(3) ', Z1$(40), Z2$(40)

Dim E%(13)


'----------------------------------------
' Used across TRADE.BAS routines
'----------------------------------------
Dim tradeA1$(2, 9), tradeA2$(2, 6), tradeA3$(2, 4), tradeA4$(2, 3)
Dim tradeA5$(2, 3), tradeA6$(2, 3), tradeA7$(1, 1)
Dim tradeF$(1), tradeFG$(1, 1), tradeKR$(1, 2)
Dim NY$(1), PU$(1), SA$(1), TN$(1), tradeYN$(1)

Dim S!(1), YC!(1)

Dim tradeA0%(1, 9), tradeA1%(1, 9), tradeA2%(2, 15), tradeA3%(1, 9), tradeA4%(1, 5)
Dim tradeA5%(1, 5), tradeA6%(1, 3), tradeA7%(1, 3), tradeA8%(1, 3), tradeA9%(1, 2)
Dim tradeB%(1, 2), tradeB1%(1, 2), tradeB2%(1, 2), tradeB3%(1, 3), tradeB4%(1, 1)
Dim tradeB5%(1, 1), tradeB6%(1, 7), tradeB7%(1, 1), tradeB8%(1, 1)
Dim tradeDI%(1, 9), tradeDS%(1, 14), tradeGI%(1, 9, 2), tradeGS%(1, 14, 1)
Dim tradePA%(1, 4), tradeQX%(1, 3), tradeYC%(1)

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

Dim Shared A1$, A2$, A3$, A4$, A5$, A6$, A7$, D2$, DI$, DN$, DR$, DS$, DV$
Dim Shared F$, G$, strG9$, I$, LO$, NM$, NN$, PS$, Q$, RP$, RV$, NY$
Dim Shared SD$, SITE$, ST$, SX$, U$, U5$, X$, Y8$, Y9$, YN$

Dim Shared A$(0 To 1), A1$(1, 10), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2), A7$(1, 1)
Dim Shared AA$(1), B$(1), D$(15), D1$(11), D2$(15), DI$(1, 9), DN$(4), DR$(1), DS$(1, 14)
Dim Shared G$(3), strG9$(5), H$(120), HO$(120), strIR$(1, 9), strKR$(1, 2)
Dim Shared strLC$(1, 20, 1), LF$(1, 1), LI$(1, 9, 1), LK$(1, 2, 1), LO$(2), LP$(1, 3, 1), strLR$(1, 9, 1)
Dim Shared O$(50), NN$(1), P$(2), PK$(1, 2, 1), strPR$(1, 2), PS$(9), strQB$(1, 3)
Dim Shared R$(14), strRB$(1, 17), RP$(30), SD$(3), SITE$(120), SX$(1 To 33, 0 To 1)
Dim Shared U5$(3), strWR$(1, 20), Y$(1), Y8$(3), Y9$(5), YN$(1), YR%(1)

Dim Shared A, A2, A3, A4, A5, A6, A7, A8, A9, AP
Dim Shared B, B1, B2, B3, B4, B5, B7, B8, BW, CP, CT
Dim Shared D, D1, D2, D3, DDI, DDS, DI, DR, DS, DT, E, EY
Dim Shared F2, F8, F9, G, G9, I1, I2, I3, I4, I5, I6, I7, I8, I9, I
Dim Shared J, J6, JJ, K3, N, NT, O, OT, P1, P2, PN, PQ, PR, PW, Q, QB, R1, RP, RY
Dim Shared S, S2, S3, S6, SN, SY, T, T1, TE, TMT, TP, W5, WE, WS
Dim Shared X, X1, X2, XD, XE, Y, Y1, Y9, YC, YF, YL, YT, Z1

Dim Shared pbpDelay!

Dim Shared A(1, 9), A1(1, 9), A2(1, 9), A3(1, 9), A4(1, 9), A5(1, 9), A6(1, 9), A7(1, 9), A8(1, 9), A9(1, 2)
Dim Shared AF(1, 1, 4), AM(1, 1, 4), B(1, 2), B1(1, 2), B2(1, 2), B3(1, 2), B4(1, 1), B5(1, 1), B6(1, 7), B7(1, 1), B8(1, 1)
Dim Shared C(50), CRD(120), DDI(1), DDS(1), DI(1, 9), DS(1, 14), F(4), F1(4), FA(1, 1, 4), FL(1, 1), FM(1, 1, 4)
Dim Shared GI(1, 9, 2), GS(1, 14, 1), IR(1, 9)
Dim Shared J6(1), K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6), KR(1, 2)
Dim Shared LC(1, 20), LF(1, 1), LI(1, 9), LK(1, 2), LP(1, 3), LR(1, 9), OT(1), P(2), PK(1, 2), PR(1, 2)
Dim Shared QB(1, 3), RB(1, 17), S(0 To 1, 0 To 10), SI(1, 9, 2), SK(1, 14, 1), T(50), T1(1), TP(1)
Dim Shared W6(1, 1), WR(1, 20), XD(1), YC(1), Z(38), Z1(38), Z2(13, 17)

Dim Shared AP%(2), intB8%(2), BY%(38, 4), D3%(0 To 8, 0 To 10), intDI%(1), intDS%(1)
Dim Shared intG9%(1), GL%(1 To 30, 1 To 2), HB%(1)
Dim Shared NG%(20), O%(120), PA%(2, 9), PC%(1, 9), intPR%(1, 120), PS%(2, 21)
Dim Shared Q6%(1), Q7%(1), QR%(50, 2), QX%(1, 3), R9%(1), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14)
Dim Shared TE%(1), TF%(1), TeamScore%(120), V4%(1, 3), WX%(6)

Dim Shared intB8%, BO%, C%, EG%, F%, GL%, H%, HB%, HT%, intI%, intDI%, intDS%
Dim Shared PA%, PC%, PS%, Q6%, Q7%, QBN%, QX%, R5%, RF%, S1%, ST%, SX%, TS%
Dim Shared W%, WX%, intY8%, intYL%

Dim Shared actualAttendance&



' For Game
'$INCLUDE: 'GameRoutines.BI'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------

'TP$() = individual record categories
'TP1$() = team record categories
Dim TP$(46), TP1$(43)

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
Data F.G.'s MADE,F.G. ATTEMPES,F.G. PERCENTAGE,E.P.'s MADE,E.P. ATTEMPTS,E.P. PERCENTAGE,LONGEST FIELD GOAL
Data INTERCEPTIONS,SACKS
Data NON-KICKERS,KICKERS,ALL PLAYERS
Data FIRST DOWNS,RUSH ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,PASSING YARDS,TOTAL YARDS,PASS COMPLETION PCT.,FUMBLES,FUMBLES LOST,INTERCEPTIONS THROWN,TURNOVER RATIO,PENALTIES,PENALTY YARDAGE,SACKS ALLOWED,SACK YARDAGE
Data OPP. FIRST DOWNS,OPP. RUSH ATTEMPTS,OPP. RUSHING YARDS,OPP. RUSHING AVERAGE,OPP. PASSING YARDS,OPP. TOTAL YARDS,OPP. PASS COMPLETION PCT.,OPP. FUMBLES,OPP. FUMBLES RECOVERED,INTERCEPTIONS,OPP. PENALTIES,OPP. PENALTY YARDAGE,SACKS
Data SACK YARDAGE LOST,ALL-PURPOSE YARDS
For I = 0 To 73: Read expCategories$(I): Next


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F
Data 4-3 READ,5-2 READ,4-3 PINCH,5-2 PINCH,4-3 INSIDE STUNT
Data 5-2 INSIDE STUNT,4-3 OUTSIDE STUNT,5-2 OUTSIDE STUNT
Data 4-3 PASS RUSH,5-2 PASS RUSH
Data STANDARD ZONE,STANDARD M-M,SHORT ZONE,MAN UNDER,NICKEL,PREVENT
Data BLITZ (1-ILB),BLITZ (1-OLB),BLITZ (2-ILB),BLITZ (2-OLB),BLITZ ENGAGE
Data BLAST,DIVE,OFF TACKLE,END SWEEP,OPTION RUN
Data DRAW PLAY,TRAP,3 STEP DROP,SHORT PASS,MED PASS
Data LONG PASS,SHORT SCREEN,MED-PLAY ACT PASS,LONG-PLAY ACT PASS,SIDELINE PASS
Data ROLLOUT PASS,BOOTLEG PASS,PUNT,FG ATT,SCOUT REPORT
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
Data 0,0,1,4,11,21,31,49,62,74,81,85,88,90,92: ' RM%(0, )
Data 3,5,8,10,15,23,32,47,58,68,75,80,85,88,92: ' RM%(1, )
Data 69,80,77,61,62,77,64,45,36,69,49,11,2,15,48,45,31,21,29,78,55,48,65,70,58,61,78,70,68,65,70,63,65,70,76,81,87,79: ' RN%(0, )
Data -4,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,6,7: ' RN%(1, )
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

For I = 0 To 14: Read R$(I): Next
For I = 1 To 10: Read D$(I): Next I
For I = 1 To 11: Read D1$(I): Next I
For I = 1 To 20: Read O$(I): Next I
For I = 1 To 7: For I1 = 1 To 10: Read D3%(I, I1): Next I1: Next I
For I = 0 To 2: Read P$(I): Read P(I): Next I
For I = 0 To 4: Read F(I): Read F1(I): Next I
For I = 0 To 3: For I1 = 1 To 10: For J = 1 To 11: Read S1%(I, I1, J): Next J: Next I1: Next I
For I = 0 To 1: For I1 = 0 To 14: Read RM%(I, I1): Next: Next
For I = 0 To 1: For I1 = 1 To 38: Read RN%(I, I1): Next: Next
For I = 1 To 38: For I1 = 1 To 4: Read BY%(I, I1): Next: Next
For I = 1 To 30: For I1 = 1 To 2: Read GL%(I, I1): Next: Next
For I = 1 To 5: For I1 = 1 To 10: For J = 1 To 14: Read S2%(I, I1, J): Next: Next: Next
For I = 1 To 2: For I1 = 1 To 21: Read PS%(I, I1): Next: Next
For I = 1 To 2: For I1 = 0 To 20: Read QR%(I1, I): Next: Next
For I = 1 To 29: Read RP$(I): Next

'I think these are alternate offense options
'that did not get implemented. There is some
'overlap so perhaps a rewrite of sorts was
'in progress.
Data BLAST,DIVE,OFF TACKLE,TOSS SWEEP,OPTION RUN,DRAW PLAY,MISDIRECTION
Data QB SNEAK,POWER SWEEP,QUICK PITCH,QUICK TRAP,ISOLATION,SPRINT DRAW
Data OVER THE TOP,COUNTER
Data 3 STEP DROP,SHORT PASS,MEDIUM PASS,LONG PASS,SCREEN PASS
Data MED PLAY ACT PASS,LONG PLAY ACT PASS,SIDELINE PASS,ROLLOUT PASS
Data BOOTLEG PASS,CROSS,CURL PASS,DEEP POST,DEEP CORNER


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 2
        Print #1, _CWD$ + "\"
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 2
    Input #1, diskPaths$(I)
Next
Close 1

Selection% = 1

Do
    Call Main_Menu(Selection%)
Loop Until Selection% = 8

End

System


'----------------------------------------

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
Print #9,
Close #9
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    'Dim A$(1 To 15)

    Do
        Cls

        Call TITLE

        'Erase menuChoices$
        ReDim menuChoices$(1 To 8)
        menuChoices$(1) = "PLAY SINGLE GAME                "
        menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
        menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
        menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
        menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
        menuChoices$(6) = "HARD DRIVE INSTALLATION         "
        menuChoices$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(8) = "EXIT TO DOS                     "

        menuSelection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 16, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case menuSelection%
                Case 1: Call FOOTBALL
                Case 2: Call TeamMgt_Menu
                Case 3: Call Replay_Menu
                Case 4: Call Leaders_Menu
                Case 5: Call SCHEDULE
                Case 6: '
                Case 7: Call ConfigureDrives
                Case 8: Cls
            End Select
        End If

    Loop Until menuSelection% = 8 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 8

End Sub


'----------------------------------------
'       ConfigureDrives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Color 15, 1
    FG = 0
    BG = 7

    For I = 0 To 2
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print
    Locate , 31: Print "PATHNAME EXAMPLES:"
    Print
    Locate , 31: Print "A:"
    Locate , 31: Print "C:\FULL\TEAMS"

    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 30), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 30), FG, BG, -1
    Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
    Locate , 18: QPrint PadRight$(diskPaths$(2), 30), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    GetDiv3:
    Locate 11, 18 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(2) = _Trim$(diskPaths$(2))

    Done:
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
        diskPaths$(R) = diskPaths$(R) + "\"
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
        'Erase menuChoices$
        menuChoices$(1) = "INPUT A NEW TEAM            "
        menuChoices$(2) = "EDIT AN EXISTING TEAM       "
        menuChoices$(3) = "PRINT TEAM ROSTER / REPORT  "
        menuChoices$(4) = "VIEW TEAM ROSTER / REPORT   "
        menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM"
        menuChoices$(6) = "TRADE PLAYERS               "
        menuChoices$(7) = "RETURN TO 3 IN 1 MAIN MENU  "
        menuSelection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 27
        Call box0(7, 26, 15, 55, 2, 15, 1)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case menuSelection%
                Case 1:
                    Call TINPUT
                Case 2:
                    Call GetTeam_CNGTM(YN$, tmNbr)
                    Call ChangeTeam(YN$, tmNbr)
                Case 3:
                    Call GetTeam_CNGTM(YN$, tmNbr)
                    Call PrintRoster
                Case 4:
                    Call GetTeam_CNGTM(YN$, tmNbr)
                    Call ViewRoster
                Case 5:
                    Call DRAFT
                Case 6:
                    Call TRADE
                Case 7:
                    Cls
            End Select
        End If

    Loop Until menuSelection% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

    Do

        Cls

        Call TITLE

        'Erase menuChoices$
        ReDim menuChoices$(1 To 4)
        menuChoices$(1) = "VIEW / PRINT COMPILED STATISTICS "
        menuChoices$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
        menuChoices$(3) = "MERGE SEPARATE STATISTICAL FILES "
        menuChoices$(4) = "RETURN TO 3 IN 1 MAIN MENU       "
        Selection% = 1
        Count% = 4
        Start% = 1
        ExitCode% = 4
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 12, 58, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call LOOKY
                Case 2: Call CAREER
                Case 3: Call MERGE
                Case 4: Cls
            End Select
        End If

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

    Do

        Call TITLE
        ReDim menuChoices$(1 To 21)
        'Erase menuChoices$
        menuChoices$(1) = "CREATE/ EDIT           "
        menuChoices$(2) = "UPDATE STATS/ STANDINGS"
        menuChoices$(3) = "VIEW/ PRINT REPORTS    "
        menuChoices$(4) = "HEAD-TO-HEAD STATS     "
        menuChoices$(5) = "LEAGUE RECORDS         "
        menuChoices$(6) = "EXPANDED LEADERS       "
        menuChoices$(7) = "DELETE LEAGUE STATS    "
        menuChoices$(8) = "CREATE/ EDIT           "
        menuChoices$(9) = "UPDATE STATS/ STANDINGS"
        menuChoices$(10) = "VIEW/ PRINT REPORTS    "
        menuChoices$(11) = "HEAD-TO-HEAD STATS     "
        menuChoices$(12) = "CONFERENCE RECORDS     "
        menuChoices$(13) = "EXPANDED LEADERS       "
        menuChoices$(14) = "DELETE CONFERENCE STATS"
        menuChoices$(15) = "CREATE/ EDIT           "
        menuChoices$(16) = "UPDATE POLL/ STATS     "
        menuChoices$(17) = "VIEW/ PRINT REPORTS    "
        menuChoices$(18) = "NATIONAL RECORDS       "
        menuChoices$(19) = "DELETE NATIONAL STATS  "
        menuChoices$(20) = "RETURN TO 3 IN 1 MENU  "
        menuChoices$(21) = "EXIT TO DOS            "
        Selection% = 1
        Count% = 21
        Start% = 1
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
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
        MAMenu menuChoices$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% = 27 Then Exit Sub

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call CREATPRO
                Case 2: Call COMPPRO
                Case 3: Call SEEPRO
                Case 4: Call HD2HDPRO
                Case 5: Call REC(1)
                Case 6: Call NEWLDR
                Case 7: Call BLANKPRO
                Case 8: Call CREATCOL
                Case 9: Call COMPCOLL
                Case 10: Call SEECOLL
                Case 11: Call HD2HDCOL
                Case 12: Call REC(2)
                Case 13: Call NEWLDR
                Case 14: Call BLANKCOL
                Case 15: Call CREATNAT
                Case 16: Call COMPNATL
                Case 17: Call SEENATL
                Case 18: Call REC(3)
                Case 19: Call BLANKORG
                Case 20 Or 21: Cls
            End Select
        End If

        If Selection% = 21 Then
            End
            System
        End If
    Loop Until Selection% >= 20 Or ExitCode% = 27

End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'

' For Utils
'$INCLUDE: 'MainRoutines.BM'
'$INCLUDE: 'TITLE.BM'
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
'$INCLUDE: 'COMPNATL.BAS'
'$INCLUDE: 'COMPPRO.BAS'
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
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'

' For Game
'$INCLUDE: 'FOOTBALL.BAS'
'$INCLUDE: 'GameRoutines.BM'

