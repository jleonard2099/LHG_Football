'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared A$, AA$, S$
Dim Shared B8%
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

Dim A1$(9), A2$(5), A3$(4), A4$(2), A5$(2), A6$(3), A7$(1)
Dim AN1$(9), AN2$(5), AN3$(3), AN4$(2), AN5$(2), AN6$(3), AN7$(1)
Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(3), AR7$(1)
Dim A$(1), AA$(1), AR$(1), DI$(9), DIN$(9), DRI$(9), DS$(15), DSN$(14), DSR$(15)
Dim LC$(15, 1), LCN$(15, 1), LCR$(15, 1), LF$(1), LFN$(1), LFR$(1)
Dim LI$(9, 1), LIN$(9, 1), LIR$(9, 1), LK$(2, 1), LKN$(2, 1), LKR$(2, 1)
Dim LP$(3, 1), LPN$(3, 1), LPR$(3, 1), LR$(9, 1), LRN$(9, 1), LRR$(9, 1)
Dim PK$(2, 1), PKN$(2, 1), PKR$(2, 1)

Dim GI%(9, 2), GIN%(9, 2), GIR%(9, 2), GS%(15, 1), GSN%(15, 1), GSR%(15, 1)
Dim PA%(5), Z1%(40)

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

Dim A0%(9), A1%(9), A2%(15), A3%(9), A4%(5)
Dim A5%(5), A6%(0 To 3), A7%(3), A8%(3), A9%(2)
Dim B%(2), B1%(2), B2%(2), B3%(1), B4%(1)
Dim B5%(1), B6%(7), B7%(1), B8%(1)
Dim DI%(9), DS%(14), QX%(3)

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
Dim FG, PN
Dim PU$

Dim H1$(30)
Dim TR$(10, 20)

Dim BRC$(50, 2), compZ1$(60), compZ2$(240), compZ3$(240)
'Dim KR$(3), compPK$(2)
Dim N2$(240), N3$(240), NT$(10, 20)
Dim PR$(3), PS$(10), PT$(1200), QB$(4), RB$(10)
'Dim TB$(18), TRC$(50), TT$(10, 20), WR$(16)

Dim DT!(38), TT!(10, 20, 9)

Dim BRC!(50), compZ1!(240, 2) ', DT!(38)
Dim EA!(2), EM!(2), EP!(2)
'Dim FA!(2), FG!(2), FM!(2), KR!(11, 11)
Dim N1!(240, 2), NT!(10, 20, 9)
Dim O1!(60), O2!(60), O3!(60), OT!(38)
Dim PR!(11, 11), PT!(1200, 5), PTSA!(60), PTSF!(60)
Dim QB!(11, 11), RB!(11, 11)
'Dim TD!(16), TG!(16), TP!(18), TRC!(50)
'Dim WR!(16, 13), YD!(16)

Dim AP%(1, 120), APR%(1, 120)


'----------------------------------------
' Used across DRAFT.BAS routines
'----------------------------------------
Dim C0%(9), C1%(9), C2%(15), C3%(9), C4%(5), C5%(5), C6%(3), C7%(3), C8%(3), C9%(2)
Dim D%(2), D1%(2), D2%(2), D3%(2), D4%(1), D5%(1), D6%(7), D7%(1), D8%(1)
Dim FI%(9), FS%(14)
Dim SX%(3), QA%(4)

Dim C1$(9), C2$(5), C3$(3), C4$(2), C5$(2), C6$(2), C7$(1)
Dim FI$(9), FS$(14)


'----------------------------------------
' Used across NEWLDR.BAS,
'   LOOKY.BAS routines
'----------------------------------------

Dim GL, JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ
Dim PW, SS, TMR, TS

Dim O%(120), P%(1)
Dim T%(120), TR%(120), Y%(30)

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
Dim expCategories$(74), H$(120), HR$(120)
Dim LCL$(480), LKL$(90), LPL$(120), LRL$(300), PKL$(90)
Dim lookyA1$(1, 10), lookyA2$(1, 6), lookyA3$(1, 4), lookyA4$(1, 3), lookyA5$(1, 3), lookyA6$(1, 2), lookyA7$(1, 2)
Dim lookyDI$(1, 9), lookyDS$(1, 14), lookyTT$(31, 30)
Dim O$(120), QBL$(120), SITE$(120), SITER$(120)
Dim TMM$(480), TPP$(480), TYY$(480)

Dim Z1$(40), Z2$(40)


'----------------------------------------
'   Used across Head-To-Head routines
'----------------------------------------
Dim D1$, D2$, D3$, D12$, D22$, D32$

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
'      Used across REC.BAS routines
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
' Used across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$

Dim NG%(MAX_GAMES, 20)

Dim scheduleAP%(1), scheduleL%(13)

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleQB_V$(20), scheduleQB_H$(20)
Dim scheduleYN$(MAX_GAMES, 1)
scheduleYN$(0, 0) = ""
scheduleYN$(0, 1) = ""

Dim MO$(3) ', Z1$(40), Z2$(40)

Dim E%(13)

MO$(0) = "HH"
MO$(1) = "C1"
MO$(2) = "C2"
MO$(3) = "CC"


'----------------------------------------
' Used across TRADE.BAS routines
'----------------------------------------

Dim tradeA1$(2, 9), tradeA2$(2, 6), tradeA3$(2, 4), tradeA4$(2, 3)
Dim tradeA5$(2, 3), tradeA6$(2, 3), tradeA7$(1, 1)
Dim tradeF$(1), tradeFG$(1, 1), tradeKR$(1, 2)
Dim NY$(1), PU$(1), SA$(1), TN$(1), YN$(1)

Dim S!(1), YC!(1)

Dim tradeA0%(1, 9), tradeA1%(1, 9), tradeA2%(2, 15), tradeA3%(1, 9), tradeA4%(1, 5)
Dim tradeA5%(1, 5), tradeA6%(1, 3), tradeA7%(1, 3), tradeA8%(1, 3), tradeA9%(1, 2)
Dim tradeB%(1, 2), tradeB1%(1, 2), tradeB2%(1, 2), tradeB3%(1, 3), tradeB4%(1, 1)
Dim tradeB5%(1, 1), tradeB6%(1, 7), tradeB7%(1, 1), tradeB8%(1, 1)
Dim tradeDI%(1, 9), tradeDS%(1, 14), tradeGI%(1, 9, 2), tradeGS%(1, 14, 1)
Dim tradePA%(1, 4), tradeQX%(1, 3), YC%(1)

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
Dim P$(60), TM$(60) ', TR$(10, 20)
Dim seePR$(1200), seeT$(60)

