$Debug
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football"
''$EXEICON:'lhg_3in1.ico'
'_ICON

DECLARE FUNCTION FNP(X)

DECLARE SUB ADDBACKS ()
DECLARE SUB ADDTIME ()
DECLARE SUB BLITZ ()
DECLARE SUB BLITZ2 ()
DECLARE SUB BOX2SCREEN ()
DECLARE SUB BOXDRAW (J)
DECLARE SUB CHANGEPOSS ()
DECLARE SUB CHANGEQB()
DECLARE SUB COMPCHEAT ()
DECLARE SUB DBOX ()
DECLARE SUB DECIDE ()
DECLARE SUB DEFENSE ()
DECLARE SUB DEFMENU ()
DECLARE SUB DISPFIELD ()
DECLARE SUB DISPWEATHER ()
DECLARE SUB FINDSPACE ()
DECLARE SUB FINDTEAMS ()
DECLARE SUB GETSPEED ()
DECLARE SUB GETTEAM ()
DECLARE SUB GOALLINE ()
DECLARE SUB HALFSTART ()
DECLARE SUB INCOMPLETE ()
DECLARE SUB KICKER ()
DECLARE SUB LGPBP ()
DECLARE SUB MARKER ()
DECLARE SUB MARKER1 ()
DECLARE SUB MEASUREMENT ()
DECLARE SUB MOVEBALL ()
DECLARE SUB NAMEEDIT ()
DECLARE SUB OFFMENU ()
DECLARE SUB OVERUSE ()
DECLARE SUB PASSMENU ()
DECLARE SUB PASSPBP ()
DECLARE SUB PBOX ()
DECLARE SUB PBP ()
DECLARE SUB PBPINT ()
DECLARE SUB PCPBP ()
DECLARE SUB PLACEPBP ()
DECLARE SUB PRINTBOX ()
DECLARE SUB PUNTYARDS ()
DECLARE SUB QBBOX ()
DECLARE SUB QBSNEAK ()
DECLARE SUB SAVESTATS ()
DECLARE SUB SCOREBOARD ()
DECLARE SUB SCOUT ()
DECLARE SUB SCOUTREPT ()
DECLARE SUB SELECTTEAM ()
DECLARE SUB SETSCREEN ()
DECLARE SUB SETUP ()
DECLARE SUB SETWEATHER ()
DECLARE SUB STATS2SCREEN ()
DECLARE SUB STUFFED ()
DECLARE SUB TICKER ()
DECLARE SUB TICKUPDATE ()
DECLARE SUB YARDLINE ()

Dim A(1, 9), A1(1, 9), A2(1, 9), A3(1, 9), A4(1, 9), A5(1, 9), A6(1, 9), A7(1, 9), A8(1, 9)
Dim A9(1, 2), B(1, 2), B1(1, 2), B3(1, 2), B6(1, 7), Q6%(1), YC(1), AA$(1)
Dim B2(1, 2), T1(1), K3(1, 6), R9%(1), C(50), T(50), PS$(9), NG%(20), XD(1), RN%(1, 38)
Dim DI(1, 9), DS(1, 14), RP$(30), SD$(3), OT(1)
Dim DV$(1), AP%(2), U5$(3), strG9$(5), Y8$(3), Y9$(5), LO$(2), Q7%(1), intB8%(2), NN$(1)
Dim E%(13)
Dim FA(1, 1, 4), FM(1, 1, 4), strPR$(1, 2), PR(1, 2), FL(1, 1), B$(1), Y$(1), LR(1, 9)
Dim GL%(30, 2), PS%(2, 21), FP%(1), QR%(50, 2), QX%(1, 3), YN$(1), WX%(6), PA%(2, 9)
Dim J6(1), S(1, 10), D2$(15), D$(15), D1$(11), O$(50), D3%(7, 10), P$(2), P(2), F(4), F1(4)
Dim K(1, 36), K1(50, 6), K2(1, 13, 17), V4%(1, 3), DN$(4), DR$(1), DI$(1, 9), DS$(1, 14)
Dim LF(1, 1), LF$(1, 1), LI$(1, 9, 1), LI(1, 9), AM(1, 1, 4), AF(1, 1, 4)
Dim LP(1, 3), LP$(1, 3, 1), A7$(1, 1), DDI(1), LK(1, 2), LK$(1, 2, 1), PK(1, 2), PK$(1, 2, 1)
Dim O%(120), Time%(120), H$(120), HO$(120)
Dim GI(1, 9, 2), GS(1, 14, 1), PC%(1, 9), RB(1, 17), WR(1, 20)
Dim strRB$(1, 17), strWR$(1, 20)
Dim LC(1, 20), strLC$(1, 20, 1), IR(1, 9), strIR$(1, 9), strLR$(1, 9, 1)
Dim intPR%(1, 120)
Dim RM%(1, 14), BY%(38, 4), SGL(15)
Dim RQ%(1), TP(1), intDS%(1), DDS(1), B4(1, 1), B5(1, 1), B7(1, 1), B8(1, 1), intG9%(1)

Dim RV%(1), HB%(1), TE%(1), TF%(1), intDI%(1)
Dim QB(1, 3), strQB$(1, 3), KR(1, 2), strKR$(1, 2)

Dim S1%(3, 10, 11), W6(1, 1): Dim A$(1), A1$(1, 10), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2)
Dim S2%(5, 10, 14), SK(1, 14, 1), SI(1, 9, 2), YR%(1), CRD(120), SITE$(120), Q$(203)
Dim SX%(32, 2, 14), SX$(32, 1), G$(3), G%(1), R$(14), ST%(32), Z(38), Z1(38), Z2(13, 17)
Dim V$(500), V%(500)

Common Shared A5(), Q7%(), XE, YC(), XD(), P1, W%, D, T1(), Y, YT, D1, Y1, YF, intY8%
Common Shared B7(), B4(), B3(), B6(), A8(), A7(), A6(), A1(), A(), A2(), K3(), A3(), A4()
Common Shared BY%(), GL%(), S2%(), PS%(), QR%(), G9, D2$(), DR$(), WX%(), ST$, DV$()
Common Shared D3, I1, P2, YN$, intB8%, intB8%(), NN$(), AA$(), I, QX%(), A9(), B(), B1(), B2()
Common Shared DR, S6, S2, PQ, S3, S1%, PW, HB%, W5, F2, I9, RP, RP$(), SD$(), OT(), QBN%
Common Shared DS(), intDS%(), DI(), intDI%(), PC%(), DDS(), DDI(), Q$(), HT%, EG%, TE, SX$()
Common Shared I2, I6, D2, X$, A1$(), A2$(), A3$(), A4$(), A5$(), A6$(), A7$(), DS$(), DI$()
Common Shared I3, B5(), B8(), Q, T, SX%(), AP, AP%(), S(), U$, BW, SN, PA%(), A$(), YL, G
Common Shared J6(), H%, N7, R5%, Y8, Y9, G$(), PS$(), DN$(), U5$(), strG9$(), Y8$(), Y9$(), LO$()
Common Shared LF(), LF$(), AM(), AF(), FM(), FA(), LI(), LI$(), NM$, X2, T(), TP()
Common Shared LR(), strLR$(), LC(), strLC$(), LP(), LP$(), LK(), LK$(), PK(), PK$()
Common Shared N, F%, TP, PR, F8, RF%, F9, intI%, C%, S%, CP, intG9%(), RV%(), O, E, RY
Common Shared intPR%(), CRD(), SITE$(), ATT, H$(), SITE$, K1(), TE%(), TF%(), J, RV$, BO%
Common Shared PR(), strPR$(), FL(), K2(), SI(), K(), W6(), SK(), O%(), HO$(), Time%(), HS$()
Common Shared Q6%(), X1, QB, R1, I7, I8, I4, I5, Z1, WS, WE, F$, EY, SY, intYL%, V$(), V%()
Common Shared R$(), D$(), D1$(), O$(), D3%(), P$(), P(), F(), F1(), S1%(), RM%(), RN%()
Common Shared RB(), strRB$(), IR(), strIR$(), WR(), strWR$(), QB(), strQB$(), KR(), strKR$(), C()
Common Shared TMT, FF$, NG%(), YN$(), YR%(), DT, JJ, RQ%(), R9%(), V4%(), NT, CT, NY$
Common Shared TS%, X, ST%, ST%(), I$, PN, B$(), Y$(), Z(), Z1(), Z2(), GI(), GS()

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
' BEGIN GOAL LINE
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
Cls
For I = 0 To 14: Read R$(I): Next
For I = 1 To 10: Read D$(I): Next I
For I = 1 To 11: Read D1$(I): Next I
For I = 1 To 20: Read O$(I): Next I
For I = 1 To 7: For I1 = 1 To 10: Read D3%(I, I1): Next I1: Next I
For I = 0 To 2: Read P$(I): Read P(I): Next I
For I = 0 To 4: Read F(I): Read F1(I): Next I
For I = 0 To 3: For I1 = 1 To 10: For I2 = 1 To 11: Read S1%(I, I1, I2): Next I2: Next I1: Next I
For I = 0 To 1: For I1 = 0 To 14: Read RM%(I, I1): Next: Next
For I = 0 To 1: For I1 = 1 To 38: Read RN%(I, I1): Next: Next
For I = 1 To 38: For I1 = 1 To 4: Read BY%(I, I1): Next: Next
For I = 1 To 30: For I1 = 1 To 2: Read GL%(I, I1): Next: Next
For I = 1 To 5: For I1 = 1 To 10: For I2 = 1 To 14: Read S2%(I, I1, I2): Next: Next: Next
For I = 1 To 2: For I1 = 1 To 21: Read PS%(I, I1): Next: Next
For I = 1 To 2: For I1 = 0 To 20: Read QR%(I1, I): Next: Next
For I = 1 To 29: Read RP$(I): Next
Data BLAST,DIVE,OFF TACKLE,TOSS SWEEP,OPTION RUN,DRAW PLAY,MISDIRECTION
Data QB SNEAK,POWER SWEEP,QUICK PITCH,QUICK TRAP,ISOLATION,SPRINT DRAW
Data OVER THE TOP,COUNTER
Data 3 STEP DROP,SHORT PASS,MEDIUM PASS,LONG PASS,SCREEN PASS
Data MED PLAY ACT PASS,LONG PLAY ACT PASS,SIDELINE PASS,ROLLOUT PASS
Data BOOTLEG PASS,CROSS,CURL PASS,DEEP POST,DEEP CORNER
If G9 <> 0 Then
    For I = 1 To 10
        If Left$(D$(I), 3) = "5-2" Then D$(I) = "3-4" + Right$(D$(I), Len(D$(I)) - 3)
        If Left$(D2$(I), 3) = "5-2" Then D2$(I) = "3-4" + Right$(D2$(I), Len(D2$(I)) - 3)
    Next
End If
For J1 = 1 To 15: D2$(J1) = D$(J1): Next
For J1 = 12 To 15: D2$(J1) = D2$(J1 - 7): Next
D2$(5) = D2$(9): D2$(6) = D2$(10)
For J1 = 12 To 15: D2$(J1 - 5) = D2$(J1): Next
DR$(0) = "RIGHT": DR$(1) = "LEFT": D$(11) = "GOAL LINE"
For I = 0 To 6: WX%(I) = 0: Next

Open "DEFAULT" For Input As #1
Input #1, J6(0)
Input #1, G9
Input #1, H%:
Input #1, N7
Input #1, R5%
Input #1, Y8
Input #1, Y9
Input #1, NG%(14)
Close 1

'Call SETUP

_Blink Off

Randomize Timer

ST$ = "12:30P"
Open "DEV.BAS" For Input As #1
Input #1, DV$(0)
Input #1, DV$(1)
Close #1

G$(1) = "FLEA FLICKER": G$(2) = "REVERSE": G$(3) = "HB PASS"
PS$(0) = "RUN": PS$(1) = "PASS": PS$(3) = "FIELD GOAL ATTEMPT": PS$(2) = "PUNT": PS$(4) = "SCOUTING REPORT": PS$(5) = "CHANGE/SELECT QB": PS$(6) = "SHOW GAME STATS": PS$(7) = "QB KNEEL DOWN": PS$(8) = "TEN MINUTE TICKER ": PS$(9) = "CHANGE/SELECT KICKER"
DN$(0) = "FIRST": DN$(1) = DN$(0): DN$(2) = "SECOND"
DN$(3) = "THIRD": DN$(4) = "FOURTH"
U5$(0) = "HUMAN OPPONENT           "
U5$(2) = "COMPUTER IS HOME TEAM    "
U5$(1) = "COMPUTER IS VISITING TEAM"
U5$(3) = "COMPUTER VS COMPUTER     "
strG9$(0) = "PLAY USING COLLEGE RULES"
strG9$(1) = "PLAY USING NFL RULES    "
strG9$(2) = "PLAY USING USFL RULES   "
strG9$(3) = "PLAY USING AFL RULES    "
strG9$(4) = "PLAY USING WFL RULES    "
strG9$(5) = "PLAY USING WLAF RULES   "
SD$(0) = "NO OVERTIME          "
SD$(1) = "ONE 15-MINUTE QUARTER"
SD$(2) = "SUDDEN DEATH TO WIN  "
SD$(3) = "NCAA DIVISION I-AA   "
Y8$(0) = "1978-1993": Y8$(1) = "PRE-1974 ": Y8$(2) = "1974-1977": Y8$(3) = "POST-1993"
Y9$(0) = "PRE-1968 ": Y9$(1) = "1968-1980": Y9$(2) = "1981-1983"
Y9$(3) = "1984-1991"
Y9$(4) = "1991     "
Y9$(5) = "POST-1991"
LO$(1) = "HOME FIELD  "
LO$(2) = "NEUTRAL SITE"
If Not _FileExists("TICKER") Then TMT = 0: GoTo 23
Open "TICKER" For Input As #2
For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
    Input #2, SX%(I, I1, I2)
Next: Next: Next
For I = 1 To 32: For I1 = 0 To 1
    Input #2, SX$(I, I1)
Next: Next
For I = 1 To 32
    Input #2, ST%(I)
Next
Close 2
TMT = 1

23 If Not _FileExists("GAME82") Then 28
Open "GAME82" For Input As #2
Input #2, FF$
For I = 0 To 2: Input #2, AP%(I): Next
Close 2
Open DV$(0) + FF$ As #1 Len = 52
For X = 0 To 20
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$
Next
Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)
Get #1, AP%(0) + AP%(2)
For X = 0 To 20: NG%(X) = CVI(Q$(X + 1)): Next
For X = 0 To 1: YN$(X) = Q$(22 + X): Next
Close 1
AP = 1

28 If AP = 1 Then
    N7 = NG%(4) / 10
    J6(0) = NG%(10)
    G9 = NG%(3)
    Y9 = NG%(7)
    Y8 = NG%(11)
End If

intY8% = 0
If AP = 1 And Y8 = 1 And G9 <> 0 Then intY8% = -10
If AP = 1 And G9 = 4 Then intY8% = 0
If AP = 1 Then H% = NG%(2): GoTo 89

60 Cls: Color 2: Print Tab(29); "SELECT OPENING OPTIONS": Print
Locate 3, 2: Color 14: Print "(1) ";: Color 15: Print U5$(J6(0))
Locate 4, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM: ";: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color , 0
Locate 5, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM: ";: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color , 0
Locate 6, 2: Color 14: Print "(4) ";: Color 15: Print "TYPE OF GAME: "; strG9$(G9)
Locate 7, 2: Color 14: Print "(5) ";: Color 15: Print "LOCATION: "; LO$(H%)
Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; N7
Locate 9, 2: Color 14: Print "(7) ";: Color 15: Print "START GAME"
Locate 10, 2: Color 14: Print "(8) ";: Color 15: Print "COLLEGE RULES: "; Y9$(Y9)
Locate 11, 2: Color 14: Print "(9) ";: Color 15: Print "PRO RULES: "; Y8$(Y8)
Locate 12, 2: Color 14: Print "(A) ";: Color 15: Print "SUDDEN DEATH: "; SD$(NG%(14))
Locate 13, 2: Color 14: Print "(0) ";: Color 15: Print "SAVE SETTINGS"
Locate 14, 2: Print "NOTES: CLOCK IS STOPPED AFTER A 1ST DOWN IN COLLEGE BEGINNING IN 1968"
Locate , 2: Print "       HOLDING IS A 15 YARD PENALTY FOR COLLEGE PRE-1981 AND PRO PRE-1974"
Locate , 2: Print "       1991-PRESENT, COLLEGE HOLDING MARKED FROM POINT OF INFRACTION"
Locate , 2: Print "       1974-77, PRO HOLDING ON A RUN IS 10 YDS AND 15 ON A PASS"
Locate , 2: Print "       1978-1994, ALL PRO HOLDING PENALTIES ARE 10 YARDS"
Locate , 2: Print "       PRE-1974, IN THE PROS, GOAL POSTS WERE ON THE GOAL LINE, THE BALL WAS"
Locate , 2: Print "       PLACED ON THE 20 AFTER MISSED FIELD GOALS, KICKOFFS WERE FROM THE 40"
Locate , 2: Print "       1992-PRESENT, COLLEGE RULES ALLOW ADVANCEMENT OF A FUMBLE"
Locate , 2: Print "       PRE-1984, IN COLLEGE PASS INTERFERENCE AT SPOT OF INFRACTION"
Locate , 2: Print "       PRO 1994-, KO FROM 35 TO 30, 2-PT CONVERSION, MISSED FG @ POINT OF KICK"
35 Color 7
I$ = GetKeyPress$

I = Rnd(1)
If I$ = Chr$(27) Or UCase$(I$) = "Q" Then Run "HELLO.EXE"
I = Val(I$)
If UCase$(I$) = "A" Then I = 10
If I < 0 Or I > 10 Then 35
If I = 0 Then Open "DEFAULT" For Output As #1: Print #1, J6(0): Print #1, G9: Print #1, H%: Print #1, N7: Print #1, R5%: Print #1, Y8: Print #1, Y9: Print #1, NG%(14): Close 1: GoTo 60
If I = 1 Then J6(0) = J6(0) + 1: If J6(0) > 3 Then J6(0) = 0
If I = 1 Then 60
If I = 2 Then I = 0: GoSub 100: GoTo 60
If I = 3 Then I = 1: GoSub 100: GoTo 60
If I = 4 Then G9 = G9 + 1: If G9 = 6 Then G9 = 0
If I = 5 Then H% = H% + 1: If H% = 3 Then H% = 1

36 If I = 6 Then Locate 8, 32: Input N7: If N7 < 0 Then 36
If I = 7 Then 65
If I = 8 Then Y9 = Y9 + 1: If Y9 > 5 Then Y9 = 0
If I = 9 Then Y8 = Y8 + 1: If Y8 > 3 Then Y8 = 0
If I = 10 Then NG%(14) = NG%(14) + 1: If NG%(14) > 3 Then NG%(14) = 0
intY8% = 0: R5% = 0
If Y8 = 1 And G9 <> 0 Then intY8% = -10
If G9 = 4 Then intY8% = 0
If Y9 = 0 And G9 = 0 Then R5% = 1
GoTo 60

65 If A$(0) = "" Or A$(1) = "" Then 60
Cls: Color 3: JJ = 6
SN = 1: Locate 8, 8: Color 14, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 8 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 9 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15
Locate 10, 11
Print "IF YOU ARE USING THE 10 MINUTE TICKER OPTION FOR THIS GAME,"
Locate , 11: Print "ENTER STARTING TIME FOR GAME IN THIS FORMAT"
Locate , 11: Print "(EXAMPLES: 1:00P, 11:30A). OTHERWISE, JUST HIT ENTER": Print
Locate , 11: Input "START TIME: "; ST$

89 intY8% = 0: R5% = 0
If Y8 = 1 And G9 <> 0 Then intY8% = -10
If G9 = 4 Then intY8% = 0
If Y9 = 0 And G9 = 0 Then R5% = 1
L = Len(ST$)
If L = 5 Then HR = Val(Left$(ST$, 1)): MIN = Val(Mid$(ST$, 3, 2))
If L = 6 Then HR = Val(Left$(ST$, 2)): MIN = Val(Mid$(ST$, 4, 2))
T$ = Right$(ST$, 1)
If UCase$(T$) = "P" And Left$(ST$, 2) <> "12" Then HR = HR + 12
TM = HR * 60 + MIN
ST% = Int(TM / 30 - 18)
Color 15, 0
If J6(0) = 1 Then CT = 0: GoTo 88
If J6(0) = 2 Then J6(0) = 1: CT = 1: GoTo 88
If J6(0) = 3 Then J6(0) = 2

88 If AP <> 1 Then 105
For I = 0 To 1
    intB8% = NG%(I)
    YN$ = YN$(I)
    Call GETTEAM: GoTo 122

    100 Color 15, 0: Cls
    JJ = 9
    SN = 1: Locate 6, 8: Color 15, 4
    Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
    Print " º"; String$(61, 32); "º ";: Next
    Locate 7 + JJ, 8
    Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
    Locate , 12: Print "ENTER DISK ID: ": Print
    Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    Print
    Locate , 12: Print "MAKE SURE NUM LOCK KEY IS OFF TO USE ARROW KEYS"
    Locate , 12: Print "AND PG UP/PG DN KEYS"
    Print
    Locate , 12: Print "(JUST HIT ENTER TO USE PREVIOUS ID, IF SAME)"
    Locate 7, 27: Input NY$
    Color 15, 0
    If NY$ = "" Then Call SELECTTEAM: GoTo 56
    YN$ = NY$
    For I1 = 1 To 500: V$(I1) = "": Next
    Call FINDTEAMS

    56 If BO% = 1 Then BO% = 0: GoTo 100

    58 Locate 2, 33: Color PA%(I, 2), PA%(I, 1): Print A$(I): Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";: I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 100
    Color 15, 0

    122 If Right$(A$(I), 1) = " " Then A$(I) = Left$(A$(I), Len(A$(I)) - 1): GoTo 122

    124 If Len(AA$(I)) > 1 And Right$(AA$(I), 1) = " " Then AA$(I) = Left$(AA$(I), Len(AA$(I)) - 1): GoTo 124
    X = 0
    If AP <> 1 Then Return
Next I

105 If G9 <> 0 Then B6(0, 7) = B6(0, 7) + 2: B6(1, 7) = B6(1, 7) + 2
B6(0, 7) = B6(0, 7) + 4: B6(1, 7) = B6(1, 7) + 4

Call NAMEEDIT

For I3 = 0 To 1
    If AP <> 1 Then 127
    Q7%(0) = 0: Q7%(1) = 0
    Q6%(0) = NG%(5)
    If Q6%(0) = 4 Then RQ%(0) = 1: GoTo 123
    R9%(0) = NG%(6)

    123 Q6%(1) = NG%(8)
    If Q6%(1) = 4 Then RQ%(1) = 1: GoTo 130
    R9%(1) = NG%(9)

    130 For I = 0 To 3
        V4%(I3, I) = -1
    Next I
    If RQ%(I3) = 1 Then 125
    For I4 = 0 To 9: For I5 = 0 To 3
        If A1$(I3, I4) = A3$(I3, I5) And I5 <> Q6%(I3) Then V4%(I3, I5) = I4
    Next I5: Next I4

    125 GoTo 128

    127 Call CHANGEQB

128 Next I3

Call SETWEATHER


'Q = 5 : T = 3600: goto 3364
If G9 <> 0 Then Call KICKOFF: GoTo 340
Call COLKICKOFF

340 Swap P1, D
Call MARKER1
X = 0
U3 = 0
If T <= 0 Then Call ADDTIME: GoTo 3360
For I = 0 To 2
    If A9(D, I) = 100 Then KN = I: GoTo KRFOUND
Next
KRFOUND:
I2 = Int(100 * Rnd(1)) + 1
For I = 2 To 0 Step -1
    If I2 <= A9(D, I) Then I1 = I
Next I
RN = Int(Rnd(1) * 100) + 1
If KN = 1 And I1 = 1 Then KN2 = 0
If KN = 1 And I1 = 0 Then KN2 = 1
If KN = 2 And I1 = 1 Then KN2 = 0
If KN = 2 And I1 = 0 Then KN2 = 1
If KN = 0 Then U$ = A4$(D, I1) + " (" + Str$(Int(B(D, I1) * 3)) + " AVG) IS DEEP TO RECEIVE THE KICK": Call PBP
If KN > 0 And RN <= 50 Then U$ = A4$(D, I1) + " (" + Str$(Int(B(D, I1) * 3)) + " AVG) AND " + A4$(D, KN2) + " (" + Str$(Int(B(D, KN2) * 3)) + " AVG) ARE DEEP": Call PBP
If KN > 0 And RN > 50 Then U$ = A4$(D, KN2) + " (" + Str$(Int(B(D, KN2) * 3)) + " AVG) AND " + A4$(D, I1) + " (" + Str$(Int(B(D, I1) * 3)) + " AVG) ARE DEEP": Call PBP
If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then
    I$ = "0"
    If T <= 180 And Q = 4 And S(P1, 0) - S(D, 0) < 0 Then 3950
    GoTo 344
End If

342 U$ = "0-REGULAR  1-SQUIBB  2-ONSIDE KICK " + A$(P1): Call PBP
343 I$ = GetKeyPress$
If Val(I$) < 0 Or Val(I$) > 2 Then 343

344 If I$ = "2" Then 3950
If I$ = "1" Then 3970

Call KICKSET
If N <= -9.99 Then Call CHANGEPOSS: GoTo 615
X = 0
If N < .01 Then 605
N = N + (Int(Rnd(1) * 11) - 5) / 10
Y = N
YT = 100 - Y: Call MARKER
Call YARDLINE: U$ = "KICK TAKEN BY " + A4$(P1, I1) + " AT THE" + Str$(Int(N + .5)) + " YD LINE "
Call PBP

450 RN = Int(Rnd(1) * 1000) + 1
If B(P1, I1) >= 10 And RN <= 80 Then G = 110: GoTo 452
If B(P1, I1) >= 10 Then G = Int((Rnd(1) * 19) + 15) + (B(P1, I1) - 10) * 3: GoTo 452
If B(P1, I1) = 9 And RN <= 50 Then G = 110: GoTo 452
If B(P1, I1) = 9 Then G = Int((Rnd(1) * 19) + 15): GoTo 452
If B(P1, I1) <= 8 And RN <= 5 Then G = 110: GoTo 452
If B(P1, I1) <= 8 And RN <= 20 Then G = Int((Rnd(1) * 21) + 45) + (B(P1, I1) - 7) * 3: GoTo 452
If B(P1, I1) <= 8 And RN <= 70 Then G = Int((Rnd(1) * 21) + 25) + (B(P1, I1) - 7) * 3: GoTo 452
If B(P1, I1) <= 8 Then G = Int((Rnd(1) * 10) + 15) + (B(P1, I1) - 7) * 3: GoTo 452

452 SY = 100 - Y: EY = SY - G: Call MOVEBALL
X1 = 3: K2(P1, I1, 14) = K2(P1, I1, 14) + 1
If G + N > 89.99 Then K2(P1, I1, 15) = K2(P1, I1, 15) + 100 - N: K2(P1, I1, 16) = K2(P1, I1, 16) + 1: GoSub 3637: GoTo 3200
If N + G <= -.05 Then Call CHANGEPOSS: GoTo 615
Y = N + G
If G > KR(P1, I1) Then strKR$(P1, I1) = "": KR(P1, I1) = G
K2(P1, I1, 15) = K2(P1, I1, 15) + G: F% = 3: Call FUMBLE: If F9 = 1 Then 640
TP = Int(Rnd(1) * 3 + 4)
T = T - TP: TP(P1) = TP(P1) + TP: I = Int(Rnd(1) * 1000) + 1
If I >= 960 - PA%(P1, 0) Then Call CLIPPING

463 Call ADDTIME: If T <= 0 Then Call ADDTIME: GoTo 3360
GoTo 700

605 Y = -(N - 2 * N): YT = 100 - Y: Call MARKER
Call YARDLINE
U$ = "KICK TAKEN BY " + A4$(P1, I1) + Str$(Int((N - 2 * N) + .5)) + " YDS DEEP IN THE END ZONE"
Call PBP
If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then
    I$ = "2"
    If N < -2 Then I$ = "1"
    GoTo 610
End If
U$ = "1-DOWN IT 2-RETURN IT"
Call PBP
608 I$ = GetKeyPress$

610 If Val(I$) = 1 Then Call CHANGEPOSS
If Val(I$) <> 1 Then 450

615 Call CHANGEPOSS: Y = 20: YT = 80: Y1 = 10: D1 = 1: U$ = "TOUCHBACK": Call PBP: Call MARKER: Call YARDLINE
GoTo 700

640 RN = Int(Rnd(1) * 6) + 1
If RN = 1 Then U$ = "*** FUMBLE ON THE PLAY!! ***"
If RN = 2 Then U$ = "*** LOOSE BALL!! ***"
If RN = 3 Then U$ = "*** BALL IS ON THE GROUND!! ***"
If RN = 4 Then U$ = "*** BALL IS KNOCKED LOOSE!! ***"
If RN = 5 Then U$ = "*** BALL IS STRIPPED!! ***"
If RN = 6 Then U$ = "*** FREE BALL !! ***"
Call PBP

650 I6 = Int(Rnd(1) * 100) + 1: K(P1, 9) = K(P1, 9) + 1
If I6 <= 50 Then
    Call PBPINT
    U$ = U$ + A$(D) + " RECOVERS!!"
    Call PBP
    D1 = 1
    Y1 = 10
    K(P1, 10) = K(P1, 10) + 1
    Y = 100 - Y
    Call CHANGEPOSS
    GoTo 659
End If

652 If I6 <= 50 And Y >= 90 Then Y1 = 100 - Y
If I6 > 50 Then
    U$ = A$(P1) + " RECOVERS!!"
    Call PBP
    I = 50
    O = 1
    GoTo 3303
End If
O = 5
GoSub 3900
If NG%(14) = 3 And Q > 4 Then 3364
GoTo 700

659 If G9 = 0 And Y9 < 5 Then 652
I = Int(Rnd(1) * 3) + 1
If I <= 2 Then 652
G1 = Int(Rnd(1) * 48) + 1: X1 = 10
If G1 >= 46 Then G1 = 100: GoTo 667
G1 = Int(Rnd(1) * 18)

667 G1 = G1 + (Int(Rnd(1) * 11) - 5) / 10
U$ = "RETURNED FOR "
If G1 + Y >= 99.99 Then U$ = U$ + Str$(Int((100 - Y) * 10 + .5) / 10) + " YDS ": Call PBP: GoTo 3200
If G1 + Y < 99.99 Then U$ = U$ + Str$(Int((G1) * 10 + .5) / 10) + " YDS": Call PBP
If G1 + Y <= .01 Then 3250
Y = Y + G1
GoTo 652

700 Call MARKER1: Call DISPWEATHER
O = 0: F9 = 0: G = 0: W5 = 0
If U3 = 9 Then U3 = 0: GoTo 340
If T <= 0 Then Call ADDTIME: GoTo 3360

702 Call SCOREBOARD: Call DEFMENU: GoTo 3117

703 If J6(0) = 1 And P1 = CT And (P2 = 12 Or P2 = 15) And Y >= 85 Or J6(0) = 2 And (P2 = 12 Or P2 = 15) And Y >= 85 Then P2 = 11
If T1(P1) > 0 And Y > 80 And Int(Rnd(1) * 25) + 1 = 16 Then U$ = "THERE'S SOME CONFUSION AT THE LINE OF SCRIMMAGE": Call PBP: U$ = A3$(P1, Q6%(P1)) + " HAS TO CALL TIME OUT TO AVOID DELAY OF GAME": Call PBP: T1(P1) = T1(P1) - 1: QBN% = 0: GoTo 702
Call SCOREBOARD: Call MARKER1
If J6(0) = 1 And D = CT Then Call COMPCHEAT
If QBN% = 1 Then QBN% = 0: GoTo 3403
I = Int(Rnd(1) * 1000) + 1
If I <= 36 And E <> 1 And P2 < 21 Then 800
'        1    2    3    4     5    6    7    8    9   10   11   12   13    14   15  16   17   18   19   20    21   22  23  24

709 On P2 GOTO 3400, 3400, 3400, 3400, 3400, 3400, 3400, 3400, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3500, 3800, 3600, 6000, 8299, 900, 950

800 U$ = "*** PENALTY - ": I = Int(Rnd(1) * 36) + 1
If I <= 2 + PA%(P1, 0) Then U$ = U$ + "OFFENSE... OFFSIDES 5 YARDS ***": Call PBP: P = -5: GoTo 801
If I > 2 + PA%(P1, 0) And I <= 20 + PA%(P1, 0) Then U$ = U$ + "ILLEGAL PROCEDURE OFFENSE... 5 YARDS ***": Call PBP: P = -5: GoTo 801
If I > 20 + PA%(P1, 0) And I <= 27 + PA%(P1, 0) Then U$ = U$ + "OFFENSE... DELAY OF GAME 5 YARDS ***": Call PBP: P = -5: GoTo 801
If I > 27 + PA%(P1, 0) And I <= 36 + PA%(P1, 0) + PA%(D, 0) Then U$ = U$ + "DEFENSE... ENCROACHMENT 5 YARDS ***": Call PBP: P = 5: GoTo 801
GoTo 709

801 O = 8
If P > 0 Then 3330
P = P * -1: GoTo 3326

850 Call TICKER
If HT% = 1 Then HT% = 0: GoTo PAH
If EG% = 1 Then EG% = 0: GoTo 9300
GoTo 700

900 GK = 1
GoTo 8531

950 I3 = P1
Call KICKER
GoTo 700

3117 If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then
    Call OFFMENU
    RP = P2
    If P2 > 8 Then RP = P2 + 6
    GoTo 703
End If

3118 J = 10: Call PBOX
Print "SELECT OFFENSE "; A$(P1)
For P2 = 0 To 4: Locate 12 + P2, 14
    Print P2; "-"; PS$(P2); Tab(41); P2 + 5; "-"; PS$(P2 + 5)
Next P2
Locate , 15: Print "G -GADGET PLAY"
3119 I$ = GetKeyPress$
If UCase$(I$) = "Q" Then Cls: End
If I$ = "8" Then I$ = "T"
If UCase$(I$) = "G" Then 3160
If UCase$(I$) = "T" Then 850
XX = Val(I$): If XX < 0 Or XX > 9 Then 3119
Screen , , 0, 0: If XX >= 2 Then P2 = XX + 17: GoTo 3124
If XX = 0 Then 3140
If XX = 1 Then 3150

3124 If P2 = 19 And Y >= 70 Then 3118
If P2 = 26 Then P2 = 24
If XX = 8 Then U$ = A$(P1) + " IS LETTING THE PLAY CLOCK WIND DOWN": Call PBP: U$ = "PENALTY - " + A$(P1) + " - DELAY OF GAME": Call PBP: O = 8: I2 = 0: P = 5: GoTo 3326
If XX = 7 Then P2 = 9: R1 = 1: GoTo 3403
If (Q = 3 Or Q = 1) And P2 = 16 Or T > 120 And P2 = 16 Then U$ = "CAN ONLY USE LAST 2 MIN. OF EACH HALF": Call PBP: GoTo 3117
If P2 < 1 Or P2 > 8 Then 703
If P2 = 8 Then 3133

3128 KJ = 1: J = 11: Call PBOX: Print "SELECT BALL CARRIER":
For KK = 0 To 4
    Locate 12 + KK, 14
    Print KK; "-"; A1$(P1, KK);
    If K(P1, 1) >= 20 And K2(P1, KK, 0) / K(P1, 1) > PC%(P1, KK) / 100 + .1 Then Print " ###" Else Print
    If A(P1, KK) = 100 Then GoTo CSELECT
Next
For KK = 0 To 4
    Locate 12 + KK, 41
    Print KK + 5; "-"; A1$(P1, KK + 5);
    If K(P1, 1) >= 20 And K2(P1, KK + 5, 0) / K(P1, 1) > PC%(P1, KK + 5) / 100 + .1 Then Print " ###" Else Print
    If A(P1, KK + 5) = 100 Then GoTo CSELECT
Next KK
CSELECT:
Locate 17, 28: Print "C-COMPUTER SELECT"
3129 I$ = GetKeyPress$
If UCase$(I$) = "C" Then KJ = 0: GoTo 3133
I1 = Val(I$): If I1 < 0 Or I1 > 9 Then 3129

3133 Screen , , 0, 0: GoTo 703

3140 Call RUNMENU

3141 Locate 20, 14: Print "             SELECT PLAY ";: Input P2
If P2 = 0 Then Screen , , 0, 0: GoTo 3118
If P2 < 1 Or P2 > 15 Then 3141
RP = P2
If P2 = 9 Or P2 = 10 Then P2 = 4
If P2 = 11 Then P2 = 7
If P2 = 12 Then P2 = 3
If P2 = 13 Then P2 = 6
If P2 = 14 Then P2 = 7
If P2 = 15 Then P2 = 2

3143 GoTo 3124

3150 Call PASSMENU

3151 Locate 19, 14: Print "             SELECT PLAY ";: Input P2
If P2 = 0 Then Screen , , 0, 0: GoTo 3118
If P2 < 0 Or P2 > 14 Then 3151
Screen , , 0, 0
RP = P2 + 15
If P2 = 11 Or P2 = 12 Then P2 = 3
If P2 = 13 Or P2 = 14 Then P2 = 4
P2 = P2 - 1
P2 = P2 + 9
GoTo 3124

3160 J = 8: Call PBOX: Print "SELECT GADGET PLAY "; A$(P1)
For G2 = 1 To 3: Locate 11 + G2, 14
Print G2; "- "; G$(G2): Next G2
3161 I$ = GetKeyPress$
If I$ = Chr$(27) Then Screen , , 0, 0: GoTo 3118
G2 = Val(I$): If G2 < 1 Or G2 > 3 Then 3161
Screen , , 0, 0
On G2 GOTO 3170, 3180, 3190

3170 '*** FLEA FLICKER ***
Call FLEA
If RN <= S% Then R1 = 1: W5 = 1: G = (Int(Rnd(1) * 6) + 10): GoTo 3541
RN = Int(Rnd(1) * 100) + 1
If RN <= intI% Then G = CP: FF% = 2: GoTo 3552
RN = Int(Rnd(1) * 100) + 1
RV$ = A2$(P1, I3): I3 = I3 + 10
If RN <= C% Then FF% = 2: GoTo 3586
G = 0: GoTo 3535

3180 '*** REVERSE PLAY ***
RV% = 1
GoSub 3128
Call QBLINE
If KJ = 1 Then KJ = 0: R1 = 1: I2 = A1(P1, I1) + B6(D, 0): GoTo 3183

3181 I2 = Int(Rnd(1) * 100) + 1: For I = 9 To 0 Step -1: If I2 <= A(P1, I) Then I1 = I
Next I
R1 = 1
GoTo 8335

3183 Call REVGAIN
GoTo 3416

3190 '*** HB PASS ***
HB% = 1
J = 11: Call PBOX: Print "SELECT PASSER":
For KK = 0 To 4
Locate 12 + KK, 14: Print KK; "-"; A1$(P1, KK); Tab(41); KK + 5; "-"; A1$(P1, KK + 5): Next KK
3191 I$ = GetKeyPress$
I1 = Val(I$): If I1 < 0 Or I1 > 9 Then 3191
Screen , , 0, 0
C% = 5: If (D3 = 1 Or D3 = 3) And HB%(P9) = 0 Then C% = 30
intI% = 20
RN = Int(Rnd(1) * 100) + 1
G = Int(Rnd(1) * 71) + 30
CP = Int(Rnd(1) * 11) + 30
HB%(P9) = 1
U$ = A3$(P1, Q6%(P1)) + " TAKES THE SNAP AND HANDS OFF TO " + A1$(P1, I1): Call PBP
U$ = A1$(P1, I1) + " HAS THE BALL AND WANTS TO THROW!!!": Call PBP
RN = Int(Rnd(1) * 100) + 1
I3 = Int(Rnd(1) * 3)
U$ = "HE HAS " + A2$(P1, I3) + " OPEN DOWNFIELD": Call PBP
If D3 = 10 Or D3 = 11 Or D3 = 8 And RN <= 35 Then G = (Int(Rnd(1) * 4) + 4): GoTo 3541
RN = Int(Rnd(1) * 100) + 1
If RN <= intI% Then G = CP: FF% = 1: HB% = 0: GoTo 3552
RN = Int(Rnd(1) * 100) + 1
RV$ = A2$(P1, I3): I3 = I3 + 10
If RN <= C% Then FF% = 1: GoTo 3586
G = 0: GoTo 3535

3200 Randomize Timer
RN = Int(Rnd(1) * 100) + 1
If RN <= 50 Then
    U$ = "T O U C H D O W N !!! " + A$(P1)
Else
    U$ = "T O U C H D O W N !!! " + AA$(P1)
End If
Call PBP
Call SCORE
Call COMPILER
If G9 <> 4 Then
    S(P1, Q) = S(P1, Q) + 6: S(P1, 0) = S(P1, 0) + 6
Else
    S(P1, Q) = S(P1, Q) + 7: S(P1, 0) = S(P1, 0) + 7
End If
Call DISPFIELD
Y = 40: Y1 = 10: D1 = 1
Call SCOREBOARD
If G9 <> 0 And Q > 4 Then 3364
GoSub 3700: Y = 35: YT = 65: Y1 = 10: Call CHANGEPOSS: O = 4: U3 = 9
GoSub 3900: U3 = 0
Call SCOREBOARD
If G9 <> 0 And intY8% < 0 Then Y = 40: YT = 60
If G9 = 1 And Y8 = 3 Or G9 = 4 Then Y = 30: YT = 70
If G9 = 0 And Y9 = 0 Then Y = 40: YT = 60
Call SCOREBOARD
If R1 > 0 And R1 < 3 And 100 - Y > 10 And G9 <> 0 Then K(D, 0) = K(D, 0) + 1
If Q > 4 Then 3364
GoTo 340

3250 '*** SAFETY ***
S(D, Q) = S(D, Q) + 2: S(D, 0) = S(D, 0) + 2
K(D, 19) = K(D, 19) + 1
Call COMPILER

3251 Y = 20: Y1 = 10: D1 = 1: O = 5: X1 = 7: Call SCORE: Call CHANGEPOSS
I6 = 99: U3 = 9: GoSub 3900: U3 = 0
U$ = "S A F E T Y  !!!": Call PBP: Call DISPFIELD
If Q > 4 Then 3364
GoTo 340

3300 GoTo 3309

3302 Call COMPILER: Y = Y + G: Y1 = Y1 - G: D1 = D1 + 1
If F9 = 1 Then 640

3303 If Int(Y1 * 10 + .5) / 10 < .05 Then 3335
If D1 = 3 Then TE%(P1) = TE%(P1) + 1
If D1 = 4 Then TF%(P1) = TF%(P1) + 1
If D1 > 4 Then 3339
I = Int(Rnd(1) * 1000) + 1: If I <= 6 + PA%(D, 0) Then 3342
GoSub 3900
GoTo 700

3309 U$ = "*** PENALTY - "
I4 = Int(Rnd(1) * 1000) + 1
If R1 = 1 And I4 <= 7 + PA%(P1, 0) Then U$ = U$ + "OFFENSE... ILLEGAL MOTION 5 YARDS ***": P = -5: GoTo 3314
If R1 = 1 And I4 <= 25 + PA%(P1, 0) And (G9 <> 0 And Y8 = 1 Or G9 = 0 And Y9 < 3) Then U$ = U$ + "OFF... HOLDING 15 YARDS ***": P = -15: HL% = 1: GoTo 3314
If R1 = 1 And I4 <= 25 + PA%(P1, 0) Then U$ = U$ + "OFF... HOLDING 10 YARDS ***": P = -10: HL% = 1: GoTo 3314
If R1 = 1 And I4 <= 42 + PA%(P1, 0) + PA%(D, 0) Then U$ = U$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***": P = 5: GoTo 3314
If R1 = 2 And I4 <= 17 + PA%(P1, 0) And (G9 <> 0 And Y8 <> 0 And Y8 <> 3 Or G9 = 0 And Y9 < 3) Then U$ = U$ + "OFF... HOLDING 15 YARDS ***": P = -15: HL% = 1: GoTo 3314
If R1 = 2 And I4 <= 17 + PA%(P1, 0) Then U$ = U$ + "OFF... HOLDING 10 YARDS ***": P = -10: HL% = 1: GoTo 3314
If R1 = 2 And I4 <= 22 + PA%(P1, 0) Then U$ = U$ + "OFF... ILLEGAL MOTION 5 YARDS ***": P = -5: GoTo 3314
If R1 = 2 And I4 <= 27 + PA%(P1, 0) + PA%(D, 0) Then U$ = U$ + "DEF... LINED IN NEUTRAL ZONE 5 YARDS ***": P = 5: GoTo 3314
If R1 = 2 And I4 <= 42 + PA%(P1, 0) + PA%(D, 0) Then U$ = U$ + "DEF... HOLDING 5 YARDS - 1ST DOWN! ***": IH = 1: P = 5: GoTo 3314
GoTo 3302

3314 If G9 = 0 And Y9 < 4 Then 3315
If G9 <> 0 Or HL% <> 1 Then 3315
If R1 = 2 Then P = P - Int(Rnd(1) * 8) + 1
If R1 = 1 Then P = P + Int(Rnd(1) * 5) + 1

3315 HL% = 0
U$ = U$ + " 1-ACCEPT 2-DECLINE ": Call PBP
If J6(0) <> 0 Then
    I$ = "2"
    If P > 0 And J6(0) = 1 And D = CT Then 3318
    If P < 0 And J6(0) = 1 And P1 = CT Then 3318
    If P > 0 And J6(0) = 1 And D <> CT And IH = 1 And Y1 - G > 0 Then 8924
    If P < 0 And J6(0) = 1 And P1 <> CT And D1 > 3 And G < Y1 Then 3319
    If P > 0 And J6(0) = 1 And D <> CT And G > 5 Or P < 0 And J6(0) = 1 And P1 <> CT And D1 = 3 And Y1 - G > 3 Or P < 0 And J6(0) = 1 And P1 <> CT And G < 0 Then 3319

    8924 I$ = "1"
    GoTo 3319
End If
3318 I$ = GetKeyPress$

3319 If Val(I$) = 1 Then O = 5: GoTo 3325
U$ = "PENALTY DECLINED": Call PBP
Call COMPILER
IH = 0: Y = Y + G: Y1 = Y1 - G: D1 = D1 + 1: If D1 = 3 Then TE%(P1) = TE%(P1) + 1
If O < 4 Or O > 5 Then O = 1
If Int((Y1) * 10 + .5) / 10 < .05 Then 3335
If D1 > 4 Then 3339

3323 GoSub 3900
GoTo 700

3325 If G9 = 0 And Y9 = 0 Then O = 1
U$ = "PENALTY ACCEPTED": Call PBP: If P > 0 Then 3330
P = P * -1
'OFF PEN

3326 If Y < P * 2 Then Y1 = Y1 + Y / 2: Y = Y - Y / 2: K(P1, 12) = K(P1, 12) + Y / 2
If Y >= P * 2 Then Y1 = Y1 + P: Y = Y - P: K(P1, 12) = K(P1, 12) + P
K(P1, 11) = K(P1, 11) + 1: If Int(Y1 * 10 + .5) / 10 < .05 Then 3335
GoTo 3323
'DEF PEN

3330 If 100 - Y < P * 2 Then Y1 = Y1 - (100 - Y) / 2: Y = Y + (100 - Y) / 2: K(D, 12) = K(D, 12) + (100 - Y) / 2
If 100 - Y >= P * 2 Then Y1 = Y1 - P: Y = Y + P: K(D, 12) = K(D, 12) + P
K(D, 11) = K(D, 11) + 1: If Y1 < .01 Or IH = 1 Then IH = 0: GoTo 3335
If T <= 0 Then T = 1
GoTo 3323

3335 D1 = 1: Y1 = 10: If 100 - Y < 9.95 Then Y1 = 100 - Y
If O < 1 And G9 = 0 And R5% = 0 Then O = 1
K(P1, 0) = K(P1, 0) + 1: GoSub 3900
If T <= 0 Then T = 1
GoTo 700

3339 If NG%(14) = 3 And Q > 4 Then 3364
D1 = 1: Y1 = 10: Y = 100 - Y: If Y > 90 Then Y1 = 100 - Y
Call CHANGEPOSS: O = 5: GoSub 3900
GoTo 700

3342 U$ = "PENALTY AFTER WHISTLE - ": I = Int(Rnd(1) * 10) + 1
If I = 1 Then U$ = U$ + "OFF PERSONAL FOUL - 15 YARDS": Call PBP: K(P1, 11) = K(P1, 11) + 1: GoTo 3344
If I > 1 And I <= 6 Then U$ = U$ + "DEF PERSONAL FOUL - 15 YARDS": Call PBP: K(D, 11) = K(D, 11) + 1: GoTo 3347
If I > 6 And I <= 9 Then U$ = U$ + "DEF INCIDENTAL FACEMASK - 5 YARDS": Call PBP: K(D, 11) = K(D, 11) + 1: GoTo 3355
If I > 9 Then U$ = U$ + "DEF FLAGRANT FACEMASK - 15 YARDS 1ST DOWN!": Call PBP: K(D, 11) = K(D, 11) + 1: GoTo 3347

3344 If Y < 30 Then Y1 = Y1 + Y / 2: Y = Y - Y / 2: K(P1, 12) = K(P1, 12) + Y / 2
If Y >= 30 Then Y1 = Y1 + 15: Y = Y - 15: K(P1, 12) = K(P1, 12) + 15
GoTo 3349

3347 If 100 - Y < 30 Then Y1 = Y1 - (100 - Y) / 2: Y = Y + (100 - Y) / 2: K(D, 12) = K(D, 12) + (100 - Y) / 2
If 100 - Y >= 30 Then Y1 = Y1 - 15: Y = Y + 15: K(D, 12) = K(D, 12) + 15
If T <= 0 Then T = 1

3349 O = 6: If Int(Y1 * 10 + .5) / 10 < .05 Then 3335
GoSub 3900
GoTo 700

3355 If 100 - Y < 10 Then Y1 = Y1 - (100 - Y) / 2: Y = Y + (100 - Y) / 2: K(D, 12) = K(D, 12) + (100 - Y) / 2
If 100 - Y >= 10 Then Y1 = Y1 - 5: Y = Y + 5: K(D, 12) = K(D, 12) + 5
If T <= 0 Then T = 1
GoTo 3349

3360 T = 0: Q = Q + 1: WX%(0) = 1 - WX%(0): T = 900: If Q = 3 Or Q >= 5 Then U3 = 0
On Q - 1 GOTO 3362, 3363, 3362, 3364, 3364, 3364, 3364, 3364, 3364, 3364

3362 Call SCOREBOARD: U$ = "END OF QUARTER": Call PBP: GoTo 700

3363 Call SCOREBOARD: U$ = "HALF TIME": Call PBP: W% = 1 - W%: Call HALFSTART: X = 0: HT% = 1: GoSub 8530
GoTo 850
PAH:
If AP <> 1 Then For I3 = 0 To 1: Call CHANGEQB: Next I3
Randomize Timer: Call SCOREBOARD
If G9 <> 0 Then Call KICKOFF: GoTo 340
Call COLKICKOFF: GoTo 340

3364 Call SCOREBOARD
If NG%(14) = 0 Then 3380
If S(0, 0) <> S(1, 0) And NG%(14) <> 3 Then 3380
If S(0, 0) <> S(1, 0) And NG%(14) = 3 And (OT(1) = 1 And OT(0) = 1 Or OT(1) = 0 And OT(0) = 0) Then 3380
'     locate 23,1:print OT(0), OT(1)
'     I$ = GetKeyPress$
Call SCOREBOARD: Call MARKER1
If NG%(14) = 1 Then I$ = "Y": NG%(14) = 0: GoTo 3366
If NG%(14) = 2 Then I$ = "Y": GoTo 3366
If NG%(14) = 3 Then 3367

3366 If UCase$(I$) = "Y" And Q = 5 Then P1 = Int(Rnd(1) * 2): Call CHANGEPOSS: T1(0) = T1(0) + 1: T1(1) = T1(1) + 1: D1 = 1: Y1 = 10: GoSub 3369: GoSub 3371: OT = 1: GoTo 340
If UCase$(I$) = "Y" And Q > 5 Then T1(0) = T1(0) + 1: T1(1) = T1(1) + 1: OT = 1: GoTo 700
Q = Q - 1: Print " END OF GAME"
GoTo 8500

3367 Y = 75: YT = 25: Y1 = 10: D1 = 1: OT = 1
If OT(0) = 1 And OT(1) = 1 Then Q = Q + 1
If OT(0) = 1 And OT(1) = 1 Or OT(0) = 0 And OT(1) = 0 Then
    OT(0) = 0: OT(1) = 0
    P1 = Int(Rnd(1) * 2)
    D = 1 - P1
    Call CHANGEPOSS
    OT(P1) = 1
ElseIf OT(0) = 1 Then
    OT(1) = 1
    P1 = 1: D = 0
    If S(P1, 0) > S(D, 0) Then 3364
ElseIf OT(1) = 1 Then
    OT(0) = 1
    P1 = 0: D = 1
    If S(P1, 0) > S(D, 0) Then 3364
End If
GoTo 700

3369 Y = 40: If G9 <> 0 Then Y = 35
Return

3371 If P1 = 0 Then D = 1
If P1 = 1 Then D = 0
Return

3380 If OT = 1 Then 3382
Q = Q - 1

3382 Print " END OF GAME": GoTo 8500

3400 '****  RUNNING PLAYS  ****
Call QBLINE
If KJ = 1 Then KJ = 0: R1 = 1: I2 = A1(P1, I1) + B6(D, 0) + WX%(4): GoTo 3403

3401 I2 = Int(Rnd(1) * 100) + 1: For I = 9 To 0 Step -1: If I2 <= A(P1, I) Then I1 = I
Next I: R1 = 1: GoTo 8335

3403 I2 = A1(P1, I1): RY = I2
'IF I2>=15 OR I2<=19 THEN I2=I2+1
I2 = I2 + B6(D, 0) + WX%(4)
If K(P1, 1) >= 20 And K2(P1, I1, 0) / K(P1, 1) > PC%(P1, I1) / 100 + .1 Then I2 = Int(I2 * .4)
RN = Int(Rnd(1) * 200) + 1: If RN = 97 And Y <= 98.899 And Y >= 1.101 Then 3450
If P2 = 8 Then 4600
If P2 = 9 Then U$ = A3$(P1, Q6%(P1)) + " TAKES THE SNAP AND KNEELS DOWN FOR ": Call PBP: G = -1.0: GoTo 3546
If P2 = 5 And Int(Rnd(1) * 100) + 1 = 49 Then 3466
Call HANDOFF
If D2 = 11 Then Call GOALLINE: GoTo 3416
I2 = I2 + D3%(P2, D2): S6 = 0: GoSub 4058
I2 = I2 + S6
If P1 = H% Then I2 = I2 + 1
If P1 <> H% And H% <> 2 Then I2 = I2 - 1
If I2 < 1 Then I2 = 1
If I2 <= 38 Then 3456

3407 G = Int(Rnd(1) * 48) + 1: If G >= 45 Then 3411
G = (Int(Rnd(1) * 11) - 1) - (((18 - I2) * 2) / 10): GoTo 3416

3411 If G = 48 Then G = Int(Rnd(1) * 5 * I2) + 1: GoTo 3416
If G = 47 Or G = 46 Then G = Int(Rnd(1) * I2) + 1: GoTo 3416
If G = 45 Then G = Int(Rnd(1) * 2 * I2) + 1

3416 Call YARDAGE
If G <= 0 Then Call STUFFED
If G > 10 And Y + G < 99.94999 Then Call LGPBP
If Y + G >= 99.94999 Then Call RUNTDPBP
SY = 100 - Y: EY = SY - G: Call MOVEBALL
If Y + G >= 99.94999 Then 3417
F% = 2
Call FUMBLE
If F9 = 1 Then 3417
If G - Y1 >= -.3 And G - Y1 <= .3 Then Call MEASUREMENT

3417 If G < 0 Then U$ = A1$(P1, I1) + " LOST": Print: X = 1: X1 = 1: U$ = U$ + Str$(Int((G * -1) * 10 + .5) / 10): GoTo 3420
If G >= 0 Then
    Call RUNPBP
    X = 1
    X1 = 1
End If
If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then U$ = U$ + Str$(Int((100 - Y) * 10 + .5) / 10)
If Int((G + Y) * 10 + .5) / 10 < 99.94999 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10)

3420 U$ = U$ + " YARDS"
'IF RV%=1 THEN U$=U$+"REVERSE"
'IF RV%=0 THEN U$=U$+RP$(RP)
If RV% > 0 Then RV% = 0
Call PBP
If E = 1 Then F9 = 0: GoTo 3708
If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then F9 = 0: GoTo 3200
If Int((G + Y) * 10 + .5) / 10 < .05 Then F9 = 0: GoTo 3250
GoTo 3300
    

3450 U$ = A3$(P1, Q6%(P1)) + " FUMBLES THE SNAP!": Call PBP: G = Int((Rnd(1) * 20) - 10) / 10
X = 4: X1 = 9: I1 = Q6%(P1)
If Y + G >= 99.94999 Then 3453
If G - Y1 >= -.3 And G - Y1 <= .3 Then Call MEASUREMENT

3453 U$ = "HE GAINS" + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP

3452 If E = 1 Then 3708
F9 = 1: Call COMPILER: Y = Y + G: Y1 = Y1 - G: D1 = D1 + 1: If D1 = 3 Then TE%(P1) = TE%(P1) + 1
GoTo 650

3456 If P2 <= 2 And D2 = 5 Or P2 <= 2 And D2 = 6 Or P2 >= 6 And D2 = 5 Or P2 >= 6 And D2 = 6 Then 3470
If P2 > 2 And P2 < 5 And D2 = 7 Or P2 > 2 And P2 < 5 And D2 = 8 Then 3470
I4 = Int(Rnd(1) * 100) + 1: If P2 <= 2 And I4 <= 50 And D3 = 7 Or P2 >= 6 And I4 <= 50 And D3 = 7 Then 4300
If P2 >= 3 And P2 <= 5 And D3 = 8 And I4 <= 50 Then 4300
If P2 <= 2 And D3 = 9 Or P2 >= 6 And D3 = 9 Then 4300
If P2 >= 3 And P2 <= 5 And D3 = 10 Then 4300
If D3 = 11 Then 4300
If I2 > 17 Then 3407
I4 = Int(Rnd(1) * 100) + 1: If I4 > 92 Then 4200
I5 = 0: If P2 = 4 Or P2 = 5 Then I5 = 1
For I6 = 0 To 14: If I4 <= RM%(I5, I6) Then G = I6 - 5: GoTo 3464
Next I6

3464 I4 = Int(Rnd(1) * 100) + 1: If I4 <= RN%(0, I2) Then G = G + RN%(1, I2)
GoTo 3416

3466 U$ = A3$(P1, Q6%(P1)) + " MAKES A BAD PITCH ON THE OPTION!!": Call PBP
G = Int((Rnd(1) * 7) + 6) * -1: X = 4: X1 = 9: I1 = Q6%(P1): U$ = "HE LOSES" + Str$(Int((G * -1) * 10 + .5) / 10) + " YARDS": Call PBP
If Int((G + Y) * 10 + .5) / 10 < .05 Then F9 = 0: GoTo 3250
GoTo 3452

3470 '*** STUNTS ***
I4 = Int(Rnd(1) * 100) + 1
If I2 > 15 Then 3476
If I4 <= 15 Then G = Int(Rnd(1) * 3) - 2
If I4 > 15 And I4 <= 60 Then G = Int(Rnd(1) * 2) + 1
If I4 > 60 Then G = Int(Rnd(1) * (I2 * 2.13)) + 1
GoTo 3494

3476 If I2 > 17 Then 3481
If I4 <= 10 Then G = Int(Rnd(1) * 3) - 2
If I4 > 10 And I4 <= 50 Then G = Int(Rnd(1) * 2) + 1
If I4 > 50 Then G = Int(Rnd(1) * (I2 * 1.73)) + 1
GoTo 3494

3481 If I2 > 19 Then 3486
If I4 <= 10 Then G = Int(Rnd(1) * 3) - 2
If I4 > 10 And I4 <= 40 Then G = Int(Rnd(1) * 2) + 1
If I4 > 40 Then G = Int(Rnd(1) * (I2 * 1.5)) + 1
GoTo 3494

3486 If I2 > 21 Then 3491
If I4 <= 5 Then G = Int(Rnd(1) * 3) - 2
If I4 > 5 And I4 <= 30 Then G = Int(Rnd(1) * 2) + 1
If I4 > 30 Then G = Int(Rnd(1) * (I2 * 1.3)) + 1
GoTo 3494

3491 If I4 <= 5 Then G = Int(Rnd(1) * 3) - 2
If I4 > 5 And I4 <= 20 Then G = Int(Rnd(1) * 2) + 1
If I4 > 20 Then G = Int(Rnd(1) * (I2 * 1.2)) + 1

3494 GoTo 3416

3500 Call PASSBALL
'SACKED
If I < S1%(3, P2 - 8, D3) + B6(P1, 5) + B6(D, 6) + 1 Then R1 = 1: W5 = 1: GoTo 3538
'FORCED TO RUN ON BOOTLEG/ROLLOUT/KEEPER
If R9%(P1) > 0 Then I = Int(Rnd(1) * 100) + 1: If I <= R9%(P1) Then 3545
If R9%(P1) > 0 Then 3532
If (P2 = 12 Or P2 = 15) And I <= 15 Then 3545
If (P2 = 9 Or P2 = 10 Or P2 = 13) And I <= 5 Then 3545
If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17) And I <= 10 Then 3545
If P2 = 18 And I <= 35 Then 3545

3532 I = Int(Rnd(1) * 100) + 1
'INTERCEPTED
If I > 100 - A8(P1, I1) - S1%(0, P2 - 8, D3) + B6(D, 2) Then 3552
Call ARMADJUST
If I <= S2 + S3 + PQ + PW Then 3575

3535 X = 2: I = Int(Rnd(1) * 1000) + 1: If I <= 50 + PA%(D, 0) And P2 <> 13 And E = 0 Then 3750
Call INCOMPLETE
If E = 1 Then 3708
O = 4
GoTo 3309

3538 ' *** SACKED ***
If P2 = 12 Or P2 = 15 Then G = (Int(Rnd(1) * 9) + 5)
If P2 = 10 Or P2 = 9 Or P2 = 13 Then G = (Int(Rnd(1) * 5) + 3)
If P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18 Then G = (Int(Rnd(1) * 7) + 4)

3541 Call YARDAGE
Call SACK
G = G * -1
'SACK
If intDS%(1 - P1) = 0 Then 3699
M5 = 0
RN = Int(Rnd(1) * DDS(1 - P1)) + 1
For Z1 = 0 To 14
    M5 = M5 + DS(1 - P1, Z1)
    If RN <= M5 Then U$ = U$ + " BY " + DS$(1 - P1, Z1): GoTo 3699
Next

3699 Call PBP
X = 4: R1 = 1
If E = 1 Then 3708
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
F% = 1
Call FUMBLE
GoTo 3309

3545 If R9%(P1) > 0 Then R1 = Int((R9%(P1) * .67) + .5): R1 = (R1 * 2): G = Int(Rnd(1) * R1)
If R9%(P1) = 0 Then G = Int(Rnd(1) * 12) + 1
Call YARDAGE
If P2 = 17 Then GoSub 8340: SY = 100 - Y: EY = SY - G: Call MOVEBALL: U$ = A3$(P1, I1) + " KEEPS IT ON THE ROLLOUT AND RUNS FOR": GoTo 3546
If P2 = 18 Then GoSub 8340: SY = 100 - Y: EY = SY - G: Call MOVEBALL: U$ = A3$(P1, I1) + " KEEPS IT ON THE BOOTLEG AND RUNS FOR": GoTo 3546
U$ = A3$(P1, I1) + " IS CHASED FROM THE POCKET": Call PBP: U$ = "HE IS FORCED TO RUN FOR"

3546 X = 4: X1 = 9: R1 = 1: If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then U$ = U$ + Str$(Int(100 - Y))
If Int((G + Y) * 10 + .5) / 10 < 99.94999 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10)
U$ = U$ + " YARDS": Call PBP: If E = 1 Then 3708
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then K(P1, 0) = K(P1, 0) + 1: GoTo 3200
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
F% = 2: Call FUMBLE: GoTo 3309

3552 If intDI%(1 - P1) > 0 Then
    Call QBTHROW
    Call FINDINT
    U$ = "PASS IS INTERCEPTED BY " + X$
Else
    U$ = "PASS IS INTERCEPTED"
End If

3554 X = 0
SI(1 - P1, Z1, 0) = SI(1 - P1, Z1, 0) + 1
K(P1, 3) = K(P1, 3) + 1: K(P1, 5) = K(P1, 5) + 1
If FF% <> 1 Then K2(P1, I1, 9) = K2(P1, I1, 9) + 1: K2(P1, I1, 11) = K2(P1, I1, 11) + 1
If E = 1 Then 6600
TP = (Int(Rnd(1) * 3) + 5): T = T - TP: TP(P1) = TP(P1) + TP
If FF% <> 0 Then FF% = 0: GoTo 3565
If P2 = 12 Or P2 = 15 Then G = Int(Rnd(1) * 20) + 21
If P2 <> 12 And P2 <> 15 Then G = Int(Rnd(1) * 10) + 1
If P2 = 13 Then G = G - 5
If P2 = 11 Or P2 = 14 Or P2 = 17 Or P2 = 16 Or P2 = 18 Then G = G + 10

3565 Call YARDAGE
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then
    U$ = U$ + " IN THE END ZONE"
    Call PBP
    If NG%(14) = 3 And Q > 4 Then 3364
    GoTo 615
End If
If G + Y > 49.99 Then U$ = U$ + " AT THE " + Str$(Int(100 - G - Y)) + " YARD LINE": Call PBP
If G + Y < 49.99 Then U$ = U$ + " AT THE " + Str$(Int(G + Y)) + " YARD LINE": Call PBP
Call CHANGEPOSS
D1 = 1: Y = 100 - G - Y: YT = 100 - Y: Call YARDLINE: G = Int(Rnd(1) * 25) + 1: If G < 24 Then 3570
G = Int(Rnd(1) * 70) + 31

3570 Call YARDAGE
X1 = 5
SY = 100 - Y
EY = SY - G
Call MOVEBALL
U$ = "RETURNED FOR "
If G + Y > 99.99 Then U$ = U$ + Str$(Int(100 - Y)) + " YARDS": SI(P1, Z1, 1) = SI(P1, Z1, 1) + 100 - Y: SI(P1, Z1, 2) = SI(P1, Z1, 2) + 1: GoSub 3639: Call PBP: GoTo 3200
U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
Call PBP
O = 5: Y = Y + G: Y1 = 10: SI(P1, Z1, 1) = SI(P1, Z1, 1) + G
If G > IR(P1, Z1) Then strIR$(P1, Z1) = "": IR(P1, Z1) = G
If Y >= 90 Then Y1 = 100 - Y
If NG%(14) = 3 And Q > 4 Then 3364
F% = 5: Call FUMBLE: If F9 = 1 Then 640
I = Int(Rnd(1) * 1000) + 1
If I <= 950 - PA%(P1, 0) Then 3573
U$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***": Call PBP
If Y < 20 Then Y = Y / 2: K(P1, 12) = K(P1, 12) + Y / 2
K(P1, 11) = K(P1, 11) + 1
If Y >= 20 Then Y = Y - 10: K(P1, 12) = K(P1, 12) + 10

3573 GoSub 3900: GoTo 700

3575 If I3 > 9 Then I4 = A5(P1, I3 - 10): RY = I4
If I3 <= 9 Then I4 = A3(P1, I3): RY = I4
If WX%(4) = -3 Then I4 = I4 - 1
If WX%(4) = -6 Then I4 = I4 - 2
If I4 < 11 Then Call BLITZ: GoTo 3586
I4 = I4 + S1%(2, P2 - 8, D3)
If YC(D) / 100 > .5 Then I4 = I4 - (1 - YC(D) / 100) * 10
If YC(D) / 100 < .5 Then I4 = I4 + YC(D) / 100 * 10
Call OVERUSE
If P2 = 9 Or P2 = 10 Or P2 = 13 Then I4 = I4 * ((((S2 + S6) * K3(P1, 3)) / (S2 + S6 + 18)) / K3(P1, 3))
If P2 = 12 Or P2 = 15 Then I4 = I4 * ((((S2 + S6) * K3(P1, 3)) / (S2 + S6 - 18)) / K3(P1, 3))
I = Int(Rnd(1) * 25) + 1: If P2 <> 12 And P2 <> 15 And I = 25 Or (P2 = 12 Or P2 = 15) And I = 25 And G9 = 0 Or (P2 = 12 Or P2 = 15) And G9 <> 0 And I >= 24 Then 3583
If I <= 19 Then 3585
G = (Int(Rnd(1) * (5 + (I4 * 2))) + 1) + (3 * (I4 - 1)): GoTo 3586

3583 G = Int(Rnd(1) * I4 * 12) + 1 + (I4 * 6)
If I = 25 And Int(Rnd(1) * 32) = 11 And S(P1, 0) > 13 And XD(D) > .6 Then G = 102
GoTo 3586

3585 G = Int(Rnd(1) * 13) + 1 + ((I4 * 2.3) - 7.2)

3586 ' ADDED AFTER REPLAYS OF 5-24-93
If Int(Rnd(1) * 3) + 1 = 1 And G9 <> 0 Then G = G - 1
Call YARDAGE
X = 3: X1 = 2: If G + Y > 99.9499 Then G = 109 - Y
Call PCPBP
If FF% <> 0 Then FF% = 0
If Y + G >= 99.9499 Then 3617
F% = 2: Call FUMBLE: If F9 = 1 Then 3617
If G - Y1 >= -.3 And G - Y1 <= .3 Then Call MEASUREMENT

3617 If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then U$ = "THAT CATCH BY ": Call WHICHRECEIVER: U$ = U$ + " GAINED" + Str$(Int(100 - Y)) + " YARDS": GoSub 3633: Call PBP: GoTo 3589
If G < 0 Then U$ = "THAT CATCH BY ": Call WHICHRECEIVER: U$ = U$ + " LOST" + Str$(Int((G * -1) * 10 + .5) / 10) + " YARDS": GoSub 3634: Call PBP: GoTo 3589
If Int((G + Y) * 10 + .5) / 10 < 99.9499 Then U$ = "THAT CATCH BY ": Call WHICHRECEIVER: U$ = U$ + " GAINED" + Str$(Int((G) * 10 + .5) / 10) + " YARDS": GoSub 3634: Call PBP

3589 If E = 1 Then F9 = 0: GoTo 3708
If Int((G + Y) * 10 + .5) / 10 < .05 Then F9 = 0: GoTo 3250
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then F9 = 0: K(P1, 0) = K(P1, 0) + 1: GoTo 3200
If P2 <> 16 Then 3594
I = Int(Rnd(1) * 10) + 1
If I <= 7 Then
    O = 5
    RN = Int(Rnd(1) * 5) + 1
    If RN = 1 Then U$ = "AND HE GETS OUT OF BOUNDS"
    If RN = 2 Then U$ = "AND HE'S KNOCKED OUT OF BOUNDS"
    If RN = 3 Then U$ = "AND HE STEPS OUT OF BOUNDS"
    If RN = 4 Then U$ = "AND HE'S PUSHED OUT OF BOUNDS"
    If RN = 5 Then U$ = "AND HE'S SHOVED OUT OF BOUNDS"
    Call PBP
    F9 = 0
    GoTo 3309
End If

3594 'F%=2:CALL FUMBLE
GoTo 3309
    

3600 I = Int(Rnd(1) * 100) + 1
For I2 = 1 To 0 Step -1
    If I <= B5(P1, I2) Then I1 = I2
Next I2
Q7%(P1) = I1
If Y < 52 + intY8% Then 3621
GoTo 4800

3602 'FIELD GOAL
If Y >= 97 + intY8% Then 3622
Call FGDIST

3606 U$ = A7$(P1, Q7%(P1)) + " TROTS ON TO TRY A FIELD GOAL": Call PBP
U$ = "THIS WILL BE A" + Str$(Int(117 + intY8% - Y + .5)) + " YARD ATTEMPT": Call PBP
U$ = "HE WAITS FOR THE SNAP": Call PBP
I5 = Int(Rnd(1) * 300) + 1
If I5 = 12 Then U$ = "THERE'S A BAD SNAP FROM CENTER!!": Call PBP: G = (Int(Rnd(1) * 9) + 6) * -1: GoTo 3890
I = Int(Rnd(1) * 100) + 1: If I >= 97 And Y >= 68 + intY8% And I2 <> 2 Then 3640
If I >= 92 And Y < 68 + intY8% And I2 <> 2 Then 3640
K2(P1, 4 + Q7%(P1) * 4, 17) = K2(P1, 4 + Q7%(P1) * 4, 17) + 1
U$ = "THE KICK IS UP AND ON ITS WAY": Call PBP
If WX%(0) <> P1 Then F2 = F2 - Int(Rnd(1) * 7) + 3 - WX%(1)
If WX%(0) = P1 Then F2 = F2 + Int(Rnd(1) * 7) - 3 + WX%(1)
Call FGATT
If I <= F2 Then K2(P1, 3 + Q7%(P1) * 4, 17) = K2(P1, 3 + Q7%(P1) * 4, 17) + 1: X1 = 8: I1 = Q7%(P1): Call SCORE: D1 = 1: Y1 = 10: S(P1, 0) = S(P1, 0) + 3: S(P1, Q) = S(P1, Q) + 3: Call FGMADE: GoTo 3616
Call MISSFG
D1 = 1: Y1 = 10
Call CHANGEPOSS
If Y > 40 And Y <= 79.99 And Y8 = 3 And G9 = 1 Then Y = Y - 7: Y = 100 - Y: GoTo 3613
If Y > 79.99 Or intY8% < 0 Then Y = 20
If Y > 40 And Y <= 79.99 And intY8% = 0 Then Y = 100 - Y

3613 TP = (Int(Rnd(1) * 3) + 4): T = T - TP: TP(P1) = TP(P1) + TP
If T <= 0 Then Call ADDTIME: GoTo 3360
If Q > 4 And NG%(14) = 3 Then 3364
GoTo 700

3616 TP = (Int(Rnd(1) * 3) + 4)
T = T - TP: TP(P1) = TP(P1) + TP
Call MADEFG
Call DISPFIELD
Y = 35
Call CHANGEPOSS
Call SCOREBOARD
If G9 <> 0 And intY8% < 0 Then Y = 40
If G9 = 1 And Y8 = 3 Or G9 = 4 Then Y = 30
If Y9 = 0 And G9 = 0 Then Y = 40
Call SCOREBOARD
If T <= 0 Then Call ADDTIME: U3 = 9: GoTo 3360
If Q > 4 Then 3364
GoTo 340

3621 U$ = "NOT IN FIELD GOAL RANGE": Call PBP: GoTo 700

3622 F2 = B7(P1, Q7%(P1))
GoTo 3606

3633 G = 100 - Y
If G > WR(P1, I3) Then strWR$(P1, I3) = "t": WR(P1, I3) = G
If G > QB(P1, Q6%(P1)) Then strQB$(P1, Q6%(P1)) = "t": QB(P1, Q6%(P1)) = G
Return

3634 If G > WR(P1, I3) Then strWR$(P1, I3) = "": WR(P1, I3) = G
If G > QB(P1, Q6%(P1)) Then strQB$(P1, Q6%(P1)) = "": QB(P1, Q6%(P1)) = G
Return

3637 If 100 - N > KR(P1, I1) Then strKR$(P1, I1) = "t": KR(P1, I1) = 100 - N
Return

3639 If 100 - Y > IR(P1, Z1) Then strIR$(P1, Z1) = "t": IR(P1, Z1) = 100 - Y
Return

3640 K2(P1, 4 + Q7%(P1) * 4, 17) = K2(P1, 4 + Q7%(P1) * 4, 17) + 1
U$ = "KICK IS BLOCKED!!!": Call PBP
TP = Int(Rnd(1) * 3) + 4: T = T - TP: TP(P1) = TP(P1) + TP
G = Int(Rnd(1) * 22) * -1: Call YARDAGE: Y = 100 - Y - G
If Y >= 109.949 Then 3250
If Y >= 100 Then
    Call CHANGEPOSS
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 Then
        U$ = A$(P1) + " RECOVERS IT IN THE END ZONE!!"
    Else
        U$ = AA$(P1) + " RECOVER IT IN THE END ZONE!!"
    End If
    Call PBP
    X1 = 6
    GoTo 3200
End If
RN = Int(Rnd(1) * 100) + 1: If RN <= 94 Then 3642
Call CHANGEPOSS
G = 100 - Y
SY = 100 - Y
EY = SY - G
Call MOVEBALL
RN = Int(Rnd(1) * 100) + 1
If RN <= 50 Then
    U$ = A$(P1) + " TAKES IT INTO THE END ZONE!!"
Else
    U$ = AA$(P1) + " TAKE IT INTO THE END ZONE!!"
End If
Call PBP
X1 = 6
GoTo 3200

3642 Call CHANGEPOSS: Y1 = 10: If Y >= 90 Then Y1 = 100 - Y
D1 = 1
Call DELAY
If T <= 0 Then Call ADDTIME: GoTo 3360
GoTo 700

3661 BG = 100 - Y
If BG > PR(P1, PR) Then strPR$(P1, PR) = "t": PR(P1, PR) = BG
Return

3700 If G9 = 4 Then I$ = "2": GoTo 3702
If G9 = 1 And Y8 <> 3 Then I$ = "1": GoTo 3703
U$ = "1-KICK EP 2-GO FOR TWO": Call PBP
If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then
    I$ = "1"
    I5 = S(P1, 0) - S(D, 0)
    If G9 = 1 And Y8 = 3 Then 8932
    If Q >= 3 And I5 = -18 Or Q = 4 And I5 = -16 Then I$ = "2": GoTo 3702
    If I5 = -2 Or I5 = 1 Or I5 = 5 Or I5 = 12 Then I$ = "2"
    If Q = 4 And T > 240 Or Q < 4 Then 3702
    If I5 = -1 Or I5 = -5 Or I5 = -8 Or I5 = -13 Or I5 = -4 Or I5 = -10 Then I$ = "2": GoTo 3702
    GoTo 3702

    8932 If Q = 4 And (I5 = -9 And T < 600 Or I5 = -2 And T < 120 Or I5 = -5 And T < 420) Then I$ = "2"
    GoTo 3702
End If

3701 I$ = GetKeyPress$
3702 GoSub 3715
If Val(I$) = 2 Then 3707

3703 K1(X2, 4) = K1(X2, 4) + 10
I = Int(Rnd(1) * 100) + 1: For I2 = 1 To 0 Step -1: If I <= B8(P1, I2) Then I1 = I2
Next I2
Q7%(P1) = I1
I = Int(Rnd(1) * 100) + 1
K2(P1, 6 + Q7%(P1) * 4, 17) = K2(P1, 6 + Q7%(P1) * 4, 17) + 1
U$ = A7$(P1, Q7%(P1)) + " IS ON TO TRY THE EXTRA POINT": Call PBP
If I <= B7(P1, Q7%(P1)) Then
    K2(P1, 5 + Q7%(P1) * 4, 17) = K2(P1, 5 + Q7%(P1) * 4, 17) + 1
    U$ = "THE KICK IS UP...": Call PBP
    Call MADEPAT
    S(P1, 0) = S(P1, 0) + 1: S(P1, Q) = S(P1, Q) + 1
    K1(X2, 6) = Q7%(P1): K1(X2, 4) = K1(X2, 4) + 1
    Call DISPFIELD
End If
If I > B7(P1, Q7%(P1)) Then 3730

3706 Return

3707 G = 0: Y = 97: E = 1
If G9 = 1 And Y8 = 3 Then Y = 98
GoTo 700

3708 If R1 = 1 Then K1(X2, 4) = 20
If R1 = 2 Then K1(X2, 4) = 30
U$ = "TRY FOR TWO IS ": E = 0
K(P1, 35) = K(P1, 35) + 1
If G < 2 And G9 = 1 And Y8 = 3 Then U$ = U$ + "NO GOOD": Call PBP: GoTo 3714
If G < 3 And G9 <> 1 And Y8 <> 3 Then U$ = U$ + "NO GOOD": Call PBP
If G >= 3 Or G9 = 1 And Y8 = 3 And G >= 2 Then U$ = U$ + "GOOD": Call PBP: S(P1, 0) = S(P1, 0) + 2: S(P1, Q) = S(P1, Q) + 2: K1(X2, 4) = K1(X2, 4) + 1: K(P1, 36) = K(P1, 36) + 1: Call DISPFIELD

3714 Return

3715 If G9 = 1 And Y8 <> 3 Then I$ = "1"
Return

3730 I = Int(Rnd(1) * 100) + 1
If I <= 9 Then U$ = "KICK IS BLOCKED!!": Call PBP: If G9 <> 1 Then 6605 Else GoTo 3706
Call MISSPAT
GoTo 3706

3750 U$ = "DEFENSIVE PASS INTERFERENCE"
If P2 = 12 Or P2 = 15 Then G = Int(Rnd(1) * 20) + 21
If P2 = 9 Or P2 = 10 Or P2 = 13 Then G = Int(Rnd(1) * 10) + 1
If P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18 Then G = Int(Rnd(1) * 10) + 11
If G9 = 0 And Y9 >= 3 And G > 15 Then G = 15
K(D, 11) = K(D, 11) + 1: Call YARDAGE
If G9 <> 0 And G + Y >= 99.99 Then K(D, 12) = K(D, 12) + 100 - Y - 1: U$ = U$ + " IN THE END ZONE": Call PBP: Y = 99: Y1 = 1
If G9 = 0 And G + Y >= 99.99 Then K(D, 12) = K(D, 12) + 100 - Y - 2: U$ = U$ + " IN THE END ZONE": Call PBP: Y = 98: Y1 = 2
If G + Y < 99.99 Then K(D, 12) = K(D, 12) + G: U$ = U$ + " AT THE ": GoSub 3759: U$ = U$ + " YARD LINE": Call PBP: Y = Y + G: Y1 = 10: If Y >= 90 Then Y1 = 100 - Y
D1 = 1: K(P1, 0) = K(P1, 0) + 1: O = 4: GoSub 3900
If T <= 0 Then T = 1
GoTo 700

3759 If G + Y < 50 Then U$ = U$ + Str$(Int((G + Y) * 10 + .5) / 10)
If G + Y >= 50 Then U$ = U$ + Str$(Int((100 - Y - G) * 10 + .5) / 10)
Return

3800 I3 = Int(Rnd(1) * 100) + 1: For I2 = 2 To 0 Step -1: If I3 <= B1(D, I2) Then I1 = I2
Next I2
PR = I1
U$ = A5$(D, PR) + " (" + Str$(Int(B2(D, PR) * 3)) + " AVG) IS DEEP TO RECEIVE": Call PBP
If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then I1 = 0: GoTo 3812
J = 9: Call PBOX: Print "PUNTING OPTIONS "; A$(P1): Locate 12, 14
Locate , 14: Print "0 - PUNT NORMAL"
Locate , 14: Print "1 - PUNT AWAY FROM THE RETURN MAN"
Locate , 14: Print "2 - FAKE PUNT (RUN)"
Locate , 14: Print "3 - FAKE PUNT (PASS)"
Locate , 14: Print "4 - TAKE INTENTIONAL SAFETY (INSIDE OWN 20)"
Locate , 14: Print "5 - QUICK KICK (1ST-3RD DOWN)"
3801 I$ = GetKeyPress$
If I$ = Chr$(27) Then Screen , , 0, 0: GoTo 3118
I1 = Val(I$): If I1 < 0 Or I1 > 5 Then 3801
If Y > 20 And I1 = 4 Then 3801
If D1 = 4 And I1 = 5 Then 3801
If D1 < 4 And I1 < 5 Then 3801
Screen , , 0, 0
If I1 = 5 Then 3802
GoTo 3812

3802 I = Int(Rnd(1) * 100) + 1: X = 0: G = B3(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * B3(P1, 0)))) + 11
If WX%(0) = P1 Then G = G - Int(Rnd(1) * 7) + 3 - WX%(1)
If WX%(0) <> P1 Then G = G + Int(Rnd(1) * 7) - 3 + WX%(1)
RN = Int(Rnd(1) * 100) + 1
If RN <= 50 Then
    U$ = A$(P1) + " IS QUICK KICKING!!": Call PBP
Else
    U$ = AA$(P1) + " ARE QUICK KICKING!!": Call PBP
End If
If D3 > 6 And I >= 60 And I <= 72 Then 3840
If D3 <= 6 And I >= 50 And I <= 56 Then 3840
U$ = "THE KICK IS UP AND ON ITS WAY": Call PBP
G = G + 2
Call YARDAGE
U$ = "KICK BY " + A$(P1) + " TRAVELS" + Str$(Int((G) * 10 + .5) / 10) + " YARDS"
Call PBP
K(P1, 7) = K(P1, 7) + 1
If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then K(P1, 8) = K(P1, 8) + 100 - Y: GoSub 3811: T = T - 5: TP(P1) = TP(P1) + 5: K(P1, 27) = K(P1, 27) + 1: GoTo 615
K(P1, 8) = K(P1, 8) + G
If G + Y > 80 Then K(P1, 29) = K(P1, 29) + 1
If G > K(P1, 26) Then K(P1, 26) = G
GoTo 3829

3811 If 100 - Y > K(P1, 26) Then K(P1, 26) = 100 - Y
Return

3812 X = 0: G = B3(P1, 0) + 19 + Int(Rnd(1) * (17 + (2 * B3(P1, 0)))) + 3
If 100 - Y <= ((B3(P1, 0) * 2) + 28) Then G = G + 100 - Y - (31 + B3(P1, 0) * 2)
If WX%(0) = P1 Then G = G - Int(Rnd(1) * 7) + 3 - WX%(1)
If WX%(0) <> P1 Then G = G + Int(Rnd(1) * 7) - 3 + WX%(1)
Call YARDAGE
RN = Int(Rnd(1) * 100) + 1
If (J6(0) = 1 And D = CT Or J6(0) = 2) And RN <= 10 Then I2 = 1: GoTo PUNTSNAP
If J6(0) = 1 And D = CT Or J6(0) = 2 Then I2 = 0: GoTo PUNTSNAP
J = 9: Call DBOX: Print "THE PUNTING TEAM FOR "; A$(P1); " IS ON THE FIELD": Locate 12, 14
Locate , 14: Print "0 - SET UP PUNT RETURN"
Locate , 14: Print "1 - GO FOR THE BLOCK"
Locate , 14: Print "2 - CALL PUNT-SAFE"
Print: Locate , 14: Print "SELECT OPTION "; A$(D); " COACH"

3814 I$ = GetKeyPress$
I2 = Val(I$): If I2 < 0 Or I2 > 2 Then 3814
Screen , , 0, 0
PUNTSNAP:
If I1 = 2 Or I1 = 3 Then 3860
If I1 = 4 Then 3880

PSNAP:
U$ = A6$(P1, 0) + " WAITS FOR THE SNAP": Call PBP
I5 = Int(Rnd(1) * 300) + 1
If I5 = 12 Then U$ = "THERE'S A BAD SNAP FROM CENTER!!": Call PBP: G = (Int(Rnd(1) * 14) + 12) * -1: GoTo 3890
I5 = Int(Rnd(1) * 100) + 1
If I2 = 1 Then U$ = "THERE'S A HEAVY RUSH!!!": Call PBP
If I2 = 1 And I5 >= 72 And I5 <= 76 Then 3840
If I2 = 1 And I5 >= 13 And I5 <= 17 Then 3846
U$ = A6$(P1, 0) + " TAKES THE SNAP": Call PBP
RN = Int(Rnd(1) * 100) + 1
If I1 = 1 And RN <= 10 Then G = G - (Int(Rnd(1) * 11) + 25): U$ = "HE SHANKS IT!": GoTo 3881
If I1 = 1 And RN > 10 Then G = G - (Int(Rnd(1) * 9) + 4): U$ = "HE KICKS IT TOWARDS THE CORNER": GoTo 3881
If G < 40 Then U$ = "HE GETS THE PUNT AWAY": GoTo 3881
Call PUNTPBP

3881 Call PBP
U$ = "PUNT BY " + A6$(P1, 0) + " TRAVELS" + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP
K(P1, 7) = K(P1, 7) + 1
If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then K(P1, 8) = K(P1, 8) + 100 - Y: T = T - 5: TP(P1) = TP(P1) + 5: K(P1, 27) = K(P1, 27) + 1: GoTo 615
K(P1, 8) = K(P1, 8) + G: I = Int(Rnd(1) * 100) + 1
If G + Y > 80 Then K(P1, 29) = K(P1, 29) + 1
If G > K(P1, 26) Then K(P1, 26) = G
If G + Y >= 90 And I >= 5 Or I2 = 2 Or I > 60 Or I1 = 1 And Int(Rnd(1) * 100) + 1 <= 80 Then 3829
Call PUNTYARDS
If G + Y >= 99.99 Then K2(P1, PR + 3, 14) = K2(P1, PR + 3, 14) + 1: K2(P1, PR + 3, 15) = K2(P1, PR + 3, 15) + 100 - Y: K2(P1, PR + 3, 16) = K2(P1, PR + 3, 16) + 1: GoSub 3661: I1 = PR: GoTo 3200
K2(P1, PR + 3, 14) = K2(P1, PR + 3, 14) + 1
K2(P1, PR + 3, 15) = K2(P1, PR + 3, 15) + G
If G > PR(P1, PR) Then strPR$(P1, PR) = "": PR(P1, PR) = G
If G + Y <= .01 Then 3250
D1 = 1: Y = Y + G: Y1 = 10: If Y >= 89.99 Then Y1 = 100 - Y
I = Int(Rnd(1) * 1000) + 1
If I >= 960 - PA%(P1, 0) Then
    U$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
    Call PBP
    If Y < 20 Then Y = Y / 2: K(P1, 12) = K(P1, 12) + Y / 2
    K(P1, 11) = K(P1, 11) + 1: If Y >= 20 Then Y = Y - 10: K(P1, 12) = K(P1, 12) + 10
    GoTo 3825
End If
F% = 4: Call FUMBLE
If F9 = 1 Then 640

3825 TP = (Int(Rnd(1) * 5) + 8) + Int(G / 10): T = T - TP: TP(P1) = TP(P1) + TP
Call DELAY: If T <= 0 Then Call ADDTIME: GoTo 3360
GoTo 700

3829 Y = Y + G: YT = Y: Call YARDLINE
Y = 100 - Y: TP = Int(Rnd(1) * 3) + 4: T = T - TP: TP(P1) = TP(P1) + TP
RN = Int(Rnd(1) * 48) + 1
If RN <= 11 Or I1 = 1 Then 3851
If RN <= 20 Then 3853
If YL <= 15 Then 3853
GoTo 3852

3851 U$ = "THE BALL GOES OUT OF BOUNDS AT THE" + Str$(Int(YL)) + " YARD LINE": GoTo 3855

3852 If D1 = 4 Then U$ = "A FAIR CATCH IS CALLED AT THE" + Str$(Int(YL)) + " YARD LINE": GoTo 3855

3853 U$ = "THE BALL IS DOWNED AT THE" + Str$(Int(YL)) + " YARD LINE"

3855 Call PBP
Call CHANGEPOSS
D1 = 1: Y1 = 10
If T <= 0 Then Call ADDTIME: GoTo 3360
Call DELAY
GoTo 700

3840 K(P1, 28) = K(P1, 28) + 1
U$ = "KICK IS BLOCKED!!!": Call PBP
TP = Int(Rnd(1) * 3) + 4: T = T - TP: TP(P1) = TP(P1) + TP
G = Int(Rnd(1) * 30) * -1
Call YARDAGE
Y = 100 - Y - G
If Y >= 109.949 Then 3250
If Y >= 100 Then Call CHANGEPOSS: U$ = A$(P1) + " RECOVERS IT IN THE END ZONE!!": Call PBP: X1 = 6: GoTo 3200
RN = Int(Rnd(1) * 100) + 1
If RN <= 60 Then 3842
Call CHANGEPOSS: G = 100 - Y: SY = 100 - Y: EY = SY - G
Call MOVEBALL: U$ = A$(P1) + " TAKES THE BALL INTO THE END ZONE!!": Call PBP
X1 = 6
GoTo 3200

3842 Call CHANGEPOSS: Y1 = 10
If Y >= 90 Then Y1 = 100 - Y
D1 = 1
Call DELAY
If T <= 0 Then Call ADDTIME: GoTo 3360
GoTo 700

3846 U$ = "THERE'S A FLAG ON THE PLAY!!"
Call PBP
U$ = "ROUGHING THE KICKER !!!...AUTOMATIC FIRST DOWN !!!"
Call PBP
K(D, 11) = K(D, 11) + 1: K(D, 12) = K(D, 12) + 15: Y = Y + 15: D1 = 1: Y1 = 10
K(P1, 0) = K(P1, 0) + 1: O = 4
GoSub 3900
Call DELAY
GoTo 700
    

3860 '*** FAKE PUNT - RUN/PASS ***
If I2 <> 2 Then 3862
U$ = A$(D) + " IS IN PUNT SAFE SET...FAKE PUNT (YN)": Call PBP

3861 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then GoTo PSNAP

3862 If I1 = 3 Then 3875
Call FAKEYARDS
If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then 3200
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
F% = 2
Call FUMBLE
GoTo 3300

3875 '*** FAKE W/PASS ***
G = 0: U$ = "IT'S A FAKE!!! " + A6$(P1, 0) + " IS LOOKING TO PASS": Call PBP
RN = Int(Rnd(1) * 100) + 1
If I2 = 0 And RN <= 36 Then G = Int(Rnd(1) * 27)
If I2 = 0 And RN > 36 And RN <= 90 Then U$ = "THE PASS FALLS INCOMPLETE": Call PBP: O = 4: GoTo 3300
If I2 = 0 And RN > 90 Then U$ = "THE PASS IS INTERCEPTED!": Call PBP: GoTo 3876
If I2 = 1 And RN <= 26 Then G = Int(Rnd(1) * 20)
If I2 = 1 And RN > 26 And RN <= 80 Then U$ = "THE PASS FALLS INCOMPLETE": Call PBP: O = 4: GoTo 3300
If I2 = 1 And RN > 80 And RN <= 90 Then U$ = "THE PASS IS INTERCEPTED!": Call PBP: GoTo 3876
If I2 = 1 And RN > 90 Then G = 100 - Y
If I2 = 2 And RN <= 10 Then G = Int(Rnd(1) * 7) + 6
If I2 = 2 And RN > 10 And I2 <= 80 Then U$ = "THE PASS FALLS INCOMPLETE": Call PBP: O = 4: GoTo 3300
If I2 = 2 And RN > 80 Then U$ = "THE PASS IS INTERCEPTED!": Call PBP: GoTo 3876
GoTo 3878

3876 X = 0
K(P1, 3) = K(P1, 3) + 1: K(P1, 5) = K(P1, 5) + 1
K2(P1, I1, 9) = K2(P1, I1, 9) + 1: K2(P1, I1, 11) = K2(P1, I1, 11) + 1
TP = (Int(Rnd(1) * 3) + 5): T = T - TP: TP(P1) = TP(P1) + TP
G = Int(Rnd(1) * 10) + 1
Call YARDAGE
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then U$ = U$ + " IN THE END ZONE": Call PBP: GoTo 615
If G + Y > 49.99 Then U$ = U$ + " AT THE " + Str$(Int(100 - G - Y)) + " YARD LINE": Call PBP
If G + Y < 49.99 Then U$ = U$ + " AT THE " + Str$(Int(G + Y)) + " YARD LINE": Call PBP
Call CHANGEPOSS: D1 = 1: Y = 100 - G - Y: YT = 100 - Y: Call YARDLINE: G = Int(Rnd(1) * 25) + 1: If G < 24 Then 3877
G = Int(Rnd(1) * 70) + 31

3877 Call YARDAGE: X1 = 5: SY = 100 - Y: EY = SY - G: Call MOVEBALL: U$ = "RETURNED FOR ": If G + Y > 99.99 Then U$ = U$ + Str$(Int(100 - Y)) + " YARDS": Call PBP: GoTo 3200
U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP: O = 5: Y = Y + G: Y1 = 10: If Y >= 90 Then Y1 = 100 - Y
GoSub 3900
Call DELAY: GoTo 700

3878 U$ = "PASS IS COMPLETE FOR": If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then U$ = U$ + Str$(Int(100 - Y)) + " YARDS": Call PBP
If Int((G + Y) * 10 + .5) / 10 < 99.9499 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then 3200
F% = 2: Call FUMBLE: GoTo 3300

3880 '*** TAKE SAFETY ***
Call TAKESAFETY
GoTo 3251

3890 '*** BAD SNAP FROM CENTER ***
TP = Int(Rnd(1) * 3) + 4: T = T - TP: TP(P1) = TP(P1) + TP
K(P1, 10) = K(P1, 10) + 1: K(P1, 9) = K(P1, 9) + 1
If Y + G < -9.999 Then S(D, Q) = S(D, Q) + 2: S(D, 0) = S(D, 0) + 2: GoTo 3251
If Y + G <= .001 And Y + G >= -9.999 And Int(Rnd(1) * 100) + 1 <= 50 Then S(D, Q) = S(D, Q) + 2: S(D, 0) = S(D, 0) + 2: GoTo 3251
If Y + G <= .001 And Y + G >= -9.999 Then X1 = 10: Call CHANGEPOSS: GoTo 3200
Call CHANGEPOSS: D1 = 1: Y1 = 10: Y = 100 - Y: If Y1 >= 90 Then Y1 = 100 - Y
RN = Int(Rnd(1) * 100) + 1
If RN <= 50 Then
    U$ = A$(P1) + " TAKES OVER ON THE"
Else
    U$ = AA$(P1) + " TAKE OVER ON THE"
End If
If Y > 50 Then U$ = U$ + Str$(Int(100 - Y)) + " YARD LINE"
If Y <= 50 Then U$ = U$ + Str$(Int(Y)) + " YARD LINE"
O = 5: GoSub 3900
GoTo 700

3900 If O = 8 Then Return
If G9 <> 0 Then 3930

3901 If T <= 120 And Q = 2 Or T <= 120 And Q >= 4 Then 3907

3902 If O = 0 Then TP = (Int(Rnd(1) * 5) + 33): T = T - TP: TP(P1) = TP(P1) + TP
If O = 0 And G9 <> 0 And T > 40 Then T = T - 4: TP(P1) = TP(P1) + 4
If O = 0 And G9 <> 0 And T > 43 And Val(Left$(A$(0), 2)) >= 90 And Val(Left$(A$(1), 2)) >= 90 Then T = T - 3: TP(P1) = TP(P1) + 3
If O = 0 And G9 = 0 And T > 43 And Val(Left$(A$(0), 2)) = 56 And Val(Left$(A$(1), 2)) = 56 Then T = T + 3: TP(P1) = TP(P1) - 3
If O = 0 And G9 = 0 And T > 43 And Val(Left$(A$(0), 2)) = 57 And Val(Left$(A$(1), 2)) = 57 Then T = T + 3: TP(P1) = TP(P1) - 3
If O = 0 And G9 <> 0 And T > 43 And Val(Left$(A$(0), 2)) >= 93 And Val(Left$(A$(1), 2)) >= 93 Then T = T + 3: TP(P1) = TP(P1) - 3
If O >= 1 And O <= 3 Then TP = 27 - (O * 5) + (Int(Rnd(1) * 5) + 1): T = T - TP: TP(P1) = TP(P1) + TP
If O >= 4 Then TP = (2 * O) - 5 + (Int(Rnd(1) * 3) + 1): T = T - TP: TP(P1) = TP(P1) + TP
If T <= 0 Then Call ADDTIME: GoTo 3360
Return

3907 If O >= 4 Then 3902
If T <= 0 Then Call ADDTIME: GoTo 3360
Call TWOMIN
If I1 = 5 Then U$ = A3$(P1, Q6%(P1)) + " IS TRYING TO GET THE TEAM LINED UP": Call PBP: O = 3: U$ = "HE STEPS BACK AND THROWS IT OUT OF BOUNDS": K(P1, 3) = K(P1, 3) + 1: K2(P1, Q6%(P1), 9) = K2(P1, Q6%(P1), 9) + 1: Call PBP: D1 = D1 + 1: If D1 = 3 Then TE%(P1) = TE%(P1) + 1
If I1 = 5 Then 3922
If I1 = 1 Then U$ = "HUDDLE": Call PBP: GoTo 3902
If I1 = 2 And T1(P1) = 0 Then U$ = "HUDDLE": Call PBP: GoTo 3902
If I1 = 3 And T1(D) = 0 Then U$ = "HUDDLE": Call PBP: GoTo 3902
If I1 = 4 And O = 0 Then U$ = "HURRY UP": Call PBP: O = 2
If I1 = 4 And O = 1 Then U$ = "HURRY UP": Call PBP: O = 3
If I1 = 2 Then U$ = "TIME OUT " + A$(P1) + " - THEY HAVE" + Str$(T1(P1) - 1) + " REMAINING": Call PBP: T1(P1) = T1(P1) - 1: O = 9
If I1 = 3 Then U$ = "TIME OUT " + A$(D) + " - THEY HAVE" + Str$(T1(D) - 1) + " REMAINING": Call PBP: T1(D) = T1(D) - 1: O = 9
If I1 = 1 Or I1 = 4 Then 3902

3922 TP = Int(Rnd(1) * 3) + 7: T = T - TP: TP(P1) = TP(P1) + TP
If T <= 0 Then Call ADDTIME: GoTo 3360
Return

3930 If T <= 120.01 Then 3901
If Q = 1 Then 3901
If Q = 3 Then 3901
If T > 159.99 And O = 0 Then 3901
If T > 147 And O = 1 Then 3901
If T > 142 And O = 2 Then 3901
If T > 137 And O = 3 Then 3901
If T > 126 And O = 4 Then 3901
If T > 128 And O = 5 Then 3901
If T > 130 And O = 6 Then 3901
If T > 132 And O = 7 Then 3901
U$ = "* * * T W O   M I N U T E   W A R N I N G * * *": Call PBP
If T <= 126.01 And T >= 120.99 Then T = T - 6: TP(P1) = TP(P1) + 6
If T > 125.99 Then TP(P1) = TP(P1) + (T - 120): T = 120
Return

3950 Call ONSIDE
D1 = 1: Y1 = 10
If T <= 0 Then Call ADDTIME: GoTo 3360
GoTo 700

3970 'SQUIB KICK
Call SQUIB
Call FUMBLE
If F9 = 1 Then 640
TP = Int(Rnd(1) * 3 + 4): T = T - TP: TP(P1) = TP(P1) + TP: I = Int(Rnd(1) * 100) + 1
If I >= 97 Then Call CLIPPING: GoTo 463
Call ADDTIME
If T <= 0 Then Call ADDTIME: GoTo 3360
GoTo 700

4058 If K(P1, 1) + K(P1, 3) = 0 Then Return
If (K(P1, 1) / (K(P1, 1) + K(P1, 3))) > (B6(P1, 7) / 100) And K3(P1, 0) - (B6(D, 0) * .3) > 4.5 And (B6(D, 0) * .3) > 4.5 And B6(P1, 7) < 50 Then S6 = Int(((K3(P1, 0) - 3.6) / .3) + .5) * -1
Return

4200 '*** BREAKAWAY ***
I5 = Int(Rnd(1) * 100) + 1: If I5 <= 50 Then G = Int(Rnd(1) * BY%(I2, 0)) + BY%(I2, 1)
If I5 > 50 Then G = Int(Rnd(1) * BY%(I2, 3)) + BY%(I2, 4)
GoTo 3416

4300 Call BLITZ2
GoTo 3416

4600 Call QBSNEAK
If E = 1 Then 3708
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then 3200
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
Call FUMBLE
GoTo 3300

4800 '*** FIELD GOAL DECISION ***
If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then I1 = 0: GoTo 4812
J = 6: Call PBOX: Print "FIELD GOAL OPTIONS "; A$(P1): Locate 12, 14
Locate , 14: Print "0 - ATTEMPT FIELD GOAL"
Locate , 14: Print "1 - FAKE FIELD GOAL (RUN)"
Locate , 14: Print "2 - FAKE FIELD GOAL (PASS)"

4801 I$ = GetKeyPress$
If I$ = Chr$(27) Then Screen , , 0, 0: GoTo 3118
I1 = Val(I$): If I1 < 0 Or I1 > 2 Then 4801
Screen , , 0, 0

4812 RN = Int(Rnd(1) * 100) + 1
If J6(0) = 1 And D = CT Or J6(0) = 2 Then I2 = 1: GoTo 4815
J = 9: Call DBOX: Print "THE FIELD GOAL UNIT FOR "; A$(P1); " IS ON THE FIELD": Locate 12, 14
Locate , 14: Print "1 - GO FOR THE BLOCK"
Locate , 14: Print "2 - CALL FIELD GOAL-SAFE"
Print: Locate , 14: Print "SELECT OPTION "; A$(D); " COACH"

4814 I$ = GetKeyPress$
I2 = Val(I$): If I2 < 1 Or I2 > 2 Then 4814

4815 Screen , , 0, 0
If I1 = 2 Or I1 = 1 Then 4860
GoTo 3602

4860 '*** FAKE PUNT - RUN/PASS ***
If I2 <> 2 Then 4862
U$ = A$(D) + " IS IN FIELD GOAL SAFE SET...FAKE FIELD GOAL (YN)": Call PBP

4861 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then GoTo 3602

4862 If I1 = 2 Then 4875
Call FAKERUN
If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then 3200
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
F% = 2
Call FUMBLE
GoTo 3300

4875 '*** FAKE W/PASS ***
U$ = "IT'S A FAKE!!! THE HOLDER IS LOOKING TO PASS": Call PBP
G = 0: RN = Int(Rnd(1) * 100) + 1
If I2 = 1 And RN <= 20 Then G = Int(Rnd(1) * 20)
If I2 = 1 And RN > 20 And RN <= 80 Then U$ = "THE PASS FALLS INCOMPLETE": Call PBP: O = 4: GoTo 3300
If I2 = 1 And RN > 80 And RN <= 90 Then U$ = "THE PASS IS INTERCEPTED!": Call PBP: GoTo 4876
If I2 = 1 And RN > 90 Then G = 100 - Y
If I2 = 2 And RN <= 10 Then G = Int(Rnd(1) * 7) + 6
If I2 = 2 And RN > 10 And I2 <= 80 Then U$ = "THE PASS FALLS INCOMPLETE": Call PBP: O = 4: GoTo 3300
If I2 = 2 And RN > 80 Then U$ = "THE PASS IS INTERCEPTED!": Call PBP: GoTo 4876
GoTo 4878

4876 X = 0
K(P1, 3) = K(P1, 3) + 1: K(P1, 5) = K(P1, 5) + 1: K2(P1, I1, 9) = K2(P1, I1, 9) + 1: K2(P1, I1, 11) = K2(P1, I1, 11) + 1
TP = (Int(Rnd(1) * 3) + 5): T = T - TP: TP(P1) = TP(P1) + TP
G = Int(Rnd(1) * 10) + 1
Call YARDAGE: If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then U$ = U$ + " IN THE END ZONE": Call PBP: GoTo 615
If G + Y > 49.99 Then U$ = U$ + " AT THE " + Str$(Int(100 - G - Y)) + " YARD LINE": Call PBP
If G + Y < 49.99 Then U$ = U$ + " AT THE " + Str$(Int(G + Y)) + " YARD LINE": Call PBP
Call CHANGEPOSS: D1 = 1: Y = 100 - G - Y: YT = 100 - Y: Call YARDLINE: G = Int(Rnd(1) * 25) + 1
If G >= 24 Then G = Int(Rnd(1) * 70) + 31
Call YARDAGE: X1 = 5: SY = 100 - Y: EY = SY - G: Call MOVEBALL: U$ = "RETURNED FOR "
If G + Y > 99.99 Then U$ = U$ + Str$(Int(100 - Y)) + " YARDS": Call PBP: GoTo 3200
U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP: O = 5: Y = Y + G: Y1 = 10
If Y >= 90 Then Y1 = 100 - Y
GoSub 3900
Call DELAY
GoTo 700

4878 U$ = "PASS IS COMPLETE FOR"
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then U$ = U$ + Str$(Int(100 - Y)) + " YARDS": Call PBP
If Int((G + Y) * 10 + .5) / 10 < 99.9499 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP
If Int((G + Y) * 10 + .5) / 10 < .05 Then 3250
If Int((G + Y) * 10 + .5) / 10 >= 99.9499 Then 3200
F% = 2: Call FUMBLE
GoTo 3300

6000 Call SCOUTREPT: GoTo 700
    

6600 '
U$ = U$ + " IN THE END ZONE": Call PBP

6605 Call CHANGEPOSS: Y = 1: YT = 100 - Y: Call YARDLINE
RN = Int(Rnd(1) * 100) + 1
If RN <= 18 Then G = 110
If RN > 18 Then G = Int(Rnd(1) * 21) + 50
Call YARDAGE
X1 = 12: SY = 100 - Y: EY = SY - G: Call MOVEBALL
U$ = "RETURNED FOR "
If G + Y > 99.99 Then U$ = U$ + Str$(Int(100 - Y)) + " YARDS": Call PBP: GoTo 6620
U$ = U$ + Str$(Int((G) * 10 + .5) / 10) + " YARDS": Call PBP: GoTo 6630

6620 '
S(P1, Q) = S(P1, Q) + 2: S(P1, 0) = S(P1, 0) + 2: U$ = "GOOD FOR 2 POINTS!!": Call PBP: Call DISPFIELD
Call SCORE

6630 E = 0: Call CHANGEPOSS: Return

8299 I3 = P1: Call CHANGEQB: GoTo 700

8335 If I1 = (V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3)) And RV% = 1 Then 8337
If I1 = V4%(P1, 0) Or I1 = V4%(P1, 1) Or I1 = V4%(P1, 2) Or I1 = V4%(P1, 3) Then 8337
I2 = A1(P1, I1) + B6(D, 0) + WX%(4)
If RV% = 1 Then 3183
GoTo 3403

8337 If A2(P1, I1) > 0 Then 3403
For I5 = 0 To 9
    If A1$(P1, I5) = A3$(P1, Q6%(P1)) Then I1 = I5: GoTo 3403
Next
If RV% = 1 Then 3181
GoTo 3401

8340 'QB RUN
I6 = 0: For I4 = 0 To 9
    If A1$(P1, I4) = A3$(P1, Q6%(P1)) Then I6 = A1(P1, I4): GoTo 8345
Next I4

8345 I7 = Int(Rnd(1) * 10) + 1
If I6 = 0 And I7 <= 9 Then G = Int(Rnd(1) * 5): GoTo 8350
If I6 = 0 Then G = Int(Rnd(1) * 16): GoTo 8350
If I7 <= 9 Then G = Int(Rnd(1) * 7): GoTo 8350
G = Int(Rnd(1) * (12 + ((I6 - 13) * 3) * 2))

8350 Call YARDAGE: Return

8500 If H% = 2 Then SITE$ = "NEUTRAL FIELD": ATT = Int(Rnd(1) * 10000) + 1 + 5000: GoTo 8540
If Not _FileExists(DV$(0) + "STADIUMS." + NN$(H%)) Then ATT = Int(Rnd(1) * 5000) + 10000: SITE$ = "MEMORIAL STADIUM": GoTo 8540
Open DV$(0) + "STADIUMS." + NN$(H%) For Random As #2 Len = 50
Field #2, 15 As X$, 30 As Q$(1), 4 As Q$(3), 1 As Q$(4)
Get #2, intB8%(H%)
SITE$ = RTrim$(Q$(1))
AVGATT = CVL(Q$(3))
Z$ = Q$(4)
Close #2
If Z$ <> "x" Then ATT = Int(Rnd(1) * 5000) + 10000: SITE$ = "MEMORIAL STADIUM": GoTo 8540
ATT = Int(Rnd(1) * 10000) + 1 - 5000 + AVGATT
If ATT < 0 Then ATT = Int(Rnd(1) * 500) + 200

8540 Call TICKUPDATE
GoSub 8650
If X <> 0 Then GoTo 8628
If X = 0 Then Cls
Call BOX2SCREEN

8530 Call DECIDE: Color , 1: Cls

8531 'VIEW STATS
Call STATS2SCREEN
If GK = 1 Then GK = 0: Cls: GoTo 700

8628 GoSub 8655
Cls
I = 1
JJ = 1
Call QBBOX
U$ = "DISPLAY STATISTICS AGAIN (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
If AP = 1 And J6(0) = 2 Then 8631
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 8500

8631 If Q = 3 Then Return

Call EVENTS(SPECIAL%, Count%)

'If we had special events
If SPECIAL% = 1 Then
    'Copy the BOX file as a "Special" file (.SPC)
    srcFile$ = DV$(1) + NM$
    dstFile$ = DV$(1) + "BOX" + Mid$(Str$(Count), 2, Len(Str$(Count%))) + ".SPC"
    Call FCopy(srcFile$, dstFile$, null$, ErrCode%)
End If

SPECIAL% = 0

8632 If AP = 1 Then 9300
EG% = 1
GoTo 850

8650 Color 15, 0: Cls
Color 14: Print "(0) ";: Color 15: Print "BOXSCORE TO SCREEN ": Print
Color 14: Print "(1) ";: Color 15: Print "BOXSCORE TO PRINTER": Print
Color 14: Print "(2) ";: Color 15: Print "BOXSCORE TO TEXTFILE"
If AP = 1 And J6(0) = 2 Then X = 2: GoTo 8680

8651 I$ = GetKeyPress$
X = Val(I$)
On Err GOTO 8650
GoTo 8680

8655 If X <> 0 Then X = 0
Return

8680 If X = 1 Then GoSub 9800
If X = 2 Then GoSub 9700
Return

9100 PX = 0
Cls
If AP <> 1 Then 9138
Call TICK
If AP%(1) < AP%(0) + AP%(2) Then Kill "GAME82": EG = 1: GoTo 9135
Open "GAME82" For Output As #2
Print #2, FF$
For I = 0 To 2: Print #2, AP%(I): Next: Close 2

9135 Cls

9138 Call SAVESTATS
If AP = 1 Then 8083
If D% = 3 Then Run "HELLO.EXE"
Cls: Print "PUT IN 3 IN 1 GAME DISK & HIT ANY KEY"
I$ = GetKeyPress$

8083 If EG = 1 Then Run "HELLO.EXE"
Run "FOOTBALL.EXE"

9300 Cls
I = 1
JJ = 1
Call QBBOX
U$ = "SAVE STATISTICS (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
If AP = 1 Then 9306
9301 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then GoTo 9309

9306 GoTo 9100

9309 Run "HELLO.EXE"

9700 If AP = 1 Then NM$ = "BOX" + Mid$(Str$(AP%(0) + AP%(2)), 2, Len(Str$(AP%(0) + AP%(2)))): GoTo 9701
Cls: Input "ENTER FILENAME TO SAVE GAME SUMMARY UNDER "; NM$
NM$ = NM$ + ".SPC"

9701 Call PRINTBOX
GoTo 8628

9800 NM$ = "TEMPBOX"
Call PRINTBOX
Open DV$(1) + NM$ For Input As #2

9801 Line Input #2, X$
If Left$(X$, 1) = Chr$(12) Then LPrint X$: Close #2: GoTo 8628
LPrint X$: GoTo 9801
Close 2
GoTo 8628


Sub SETUP ()
End Sub


Sub SCOUT ()
    For I = 0 To 1
        I2 = 0: I3 = 0: I4 = 0: I5 = 0: I6 = 0
        For I1 = 0 To 9
            If A2(I, I1) <> 0 Then I2 = A2(I, I1): I3 = 0
        Next I1
        For I1 = 0 To 9
            If I1 = 0 Then K3(I, 3) = K3(I, 3) + A2(I, I1) * A3(I, I1): K3(I, 3) = K3(I, 3) + A5(I, I1) * (A4(I, I1) - I2)
            If I1 > 0 And I1 < 10 And A2(I, I1) <> 0 Then K3(I, 3) = K3(I, 3) + A3(I, I1) * (A2(I, I1) - I3)
            If I1 > 0 And I1 < 6 And A4(I, I1) <> 0 Then K3(I, 3) = K3(I, 3) + A5(I, I1) * (A4(I, I1) - I6)
            If A2(I, I1) <> 0 Then I3 = A2(I, I1)
            If A4(I, I1) <> 0 Then I6 = A4(I, I1)
        Next I1
        K3(I, 3) = Int((K3(I, 3) * 3) / 100)
        I2 = 0: I3 = 0: I6 = 0
        For I1 = 0 To 9
            If A(I, I1) <> 0 Then I4 = A(I, I1): I5 = 0
        Next I1
        For I1 = 0 To 9
            If I1 = 0 Then K3(I, 0) = K3(I, 0) + A(I, I1) * A1(I, I1): I2 = I2 + A6(I, I1) * A7(I, I1): I3 = I3 + A6(I, I1) * A8(I, I1)
            If I1 > 0 And A(I, I1) <> 0 Then K3(I, 0) = K3(I, 0) + A1(I, I1) * (A(I, I1) - I5)
            If I1 > 0 And I1 < 4 And A6(I, I1) <> 0 Then I2 = I2 + A7(I, I1) * (A6(I, I1) - I6): I3 = I3 + A8(I, I1) * (A6(I, I1) - I6)
            If A(I, I1) <> 0 Then I5 = A(I, I1)
            If A6(I, I1) <> 0 Then I6 = A6(I, I1)
        Next I1
        If I = 0 Then I1 = 1
        If I = 1 Then I1 = 0
        K3(I, 0) = ((K3(I, 0) * 3) / 100) + ((B6(I1, 0)) * 3): ' HAD (B6()+1)*3
        K3(I, 0) = Int(K3(I, 0) + .5) / 10
        K3(I, 1) = Int((I2 / 100) + B6(I1, 1) + .5)
        K3(I, 2) = Int((I3 / 100) - B6(I1, 2) + .5)
        K3(I, 4) = (B3(I, 0) * 2) + 28
        K3(I, 5) = (B4(I, Q7%(I)) * 5) + 25
        K3(I, 6) = B7(I, Q7%(I))
        XE = YC(I1) / 100: If XE < .5 Then XD(I1) = Int(XE * 30)
        If XE > .5 Then XD(I1) = Int(((1 - XE) * 30) * -1)
        I4 = I
        If I4 = H% Then K3(I4, 0) = K3(I4, 0) + .3: K3(I4, 1) = K3(I4, 1) + 3
        If I4 <> H% And H% <> 2 Then K3(I4, 0) = K3(I4, 0) - .3: K3(I4, 1) = K3(I4, 1) - 3
    Next I
End Sub


Sub HALFSTART ()
    P1 = W%: D = 1: If P1 = D Then D = 0
    T1(0) = 3: T1(1) = T1(0): Y = 35: YT = 65: D1 = 1: Y1 = 10: YF = -Y1
    If G9 <> 0 And intY8% < 0 Then Y = 40: YT = 60
    If G9 = 1 And Y8 = 3 Or G9 = 4 Then Y = 30: YT = 70
    If G9 = 0 And Y9 = 0 Then Y = 40: YT = 60
End Sub


Sub BOXDRAW (J)
    PCopy 0, 1: Screen , , 1, 1: SN = 1: Locate 8, 8
    Color 0, 7: Print " Ö"; String$(61, 196); "· ";
    For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next
    Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15
End Sub


Sub KICKER ()
    Color , 1: Cls: J = 10: Call BOXDRAW(J)
    Locate , 31: Print "SELECT KICKER": Print
    Locate , 18
    Print " # NAME            FG%  %ATT  EP%  %ATT"
    For I = 0 To 1
        If A7$(I3, I) = "XXX" Or A7$(I3, I) = "END" Or A7$(I3, I) = "" Then 957
        Locate , 18: Print I; "-"; A7$(I3, I);: Locate , 36
        Print Using " ###   ###  ###   ###"; B4(I3, I) * 5 + 25, B5(I3, I), B7(I3, I), B8(I3, I)

    957 Locate , 1: Next I
    Print: Locate , 19: Print "SELECT (0-1) ";

    958 I$ = GetKeyPress$
    I2 = Val(I$): If I2 < 0 Or I2 > 1 Then 958
    Q7%(I3) = I2
End Sub


Sub GETAKEY ()

    950 I$ = GetKeyPress$
End Sub


Sub TICKUPDATE ()
    If AP <> 1 Or AP%(2) > 32 Then 952
    If Q = 1 And T <= 600 And SX%(AP%(2) + 1, 0, 1) = -1 Then SX%(AP%(2) + 1, 0, 1) = S(0, 0): SX%(AP%(2) + 1, 1, 1) = S(1, 0)
    If Q = 1 And T <= 300 And SX%(AP%(2) + 1, 0, 2) = -1 Then SX%(AP%(2) + 1, 0, 2) = S(0, 0): SX%(AP%(2) + 1, 1, 2) = S(1, 0)
    If Q > 2 And SX%(AP%(2) + 1, 0, 3) = -1 Then SX%(AP%(2) + 1, 0, 3) = S(0, 1): SX%(AP%(2) + 1, 1, 3) = S(1, 1)
    If Q = 2 And T <= 600 And SX%(AP%(2) + 1, 0, 4) = -1 Then SX%(AP%(2) + 1, 0, 4) = S(0, 0): SX%(AP%(2) + 1, 1, 4) = S(1, 0)
    If Q = 2 And T <= 300 And SX%(AP%(2) + 1, 0, 5) = -1 Then SX%(AP%(2) + 1, 0, 5) = S(0, 0): SX%(AP%(2) + 1, 1, 5) = S(1, 0)
    If Q > 2 And SX%(AP%(2) + 1, 0, 6) = -1 Then For I2 = 6 To 8: SX%(AP%(2) + 1, 0, I2) = S(0, 2) + S(0, 1): SX%(AP%(2) + 1, 1, I2) = S(1, 2) + S(1, 1): Next
    If Q = 3 And T <= 600 And SX%(AP%(2) + 1, 0, 9) = -1 Then SX%(AP%(2) + 1, 0, 9) = S(0, 0): SX%(AP%(2) + 1, 1, 9) = S(1, 0)
    If Q = 3 And T <= 300 And SX%(AP%(2) + 1, 0, 10) = -1 Then SX%(AP%(2) + 1, 0, 10) = S(0, 0): SX%(AP%(2) + 1, 1, 10) = S(1, 0)
    If Q > 3 And SX%(AP%(2) + 1, 0, 11) = -1 Then SX%(AP%(2) + 1, 0, 11) = S(0, 3) + S(0, 2) + S(0, 1): SX%(AP%(2) + 1, 1, 11) = S(1, 3) + S(1, 2) + S(1, 1)
    If Q = 4 And T <= 600 And SX%(AP%(2) + 1, 0, 12) = -1 Then SX%(AP%(2) + 1, 0, 12) = S(0, 0): SX%(AP%(2) + 1, 1, 12) = S(1, 0)
    If Q = 4 And T <= 300 And SX%(AP%(2) + 1, 0, 13) = -1 Then SX%(AP%(2) + 1, 0, 13) = S(0, 0): SX%(AP%(2) + 1, 1, 13) = S(1, 0)
    SX%(AP%(2) + 1, 0, 14) = S(0, 0): SX%(AP%(2) + 1, 1, 14) = S(1, 0)

    952 'HI
End Sub


Sub PLACEPBP ()
    U$ = Left$(U$, 76)
    Color 14 - 7 * BW, 1 - BW
    Locate 22, 3
    Print String$(74, 46);
    Locate 22, 39 - Len(U$) / 2
    Print " "; U$; " ";
End Sub


Sub PBP ()
    M$ = GetKeyPress$
    If UCase$(M$) = "Q" Then Cls: End
    Call PLACEPBP
    If N7 <> 0 Then
        U! = Timer
        While Timer < U! + N7
        Wend
    End If
End Sub


Sub DISPWEATHER ()
    Color 15 * (1 - BW), 2 + 5 * BW
    Locate 7, 20: Print "WIND: ";
    If WX%(1) = 0 Then Print "NONE ";
    If WX%(1) = -7 Then Print "LIGHT ";
    If WX%(1) = -15 Then Print "STRONG ";
    If WX%(1) <> 0 And WX%(0) = P1 Then Print "+";
    If WX%(1) <> 0 And WX%(0) <> P1 Then Print "-";
    Locate , 40: Print "WEATHER: ";
    If WX%(4) = 0 Then Print "GOOD"
    If WX%(4) = -3 Then Print "RAIN/SNOW"
    If WX%(4) = -6 Then Print "DOWNPOUR/BLIZZARD"
End Sub


Sub CHANGEPOSS ()
    Swap P1, D
End Sub


Sub SETSCREEN ()
    Screen , , 0, 0
    SN = 0
    Color 0, 0, 0
    Cls: Color 15, 1 + BW * 6
    Print
    'Cnoverted these Chr$ statements so the graphics
    'are preserved in any text editor
    Print Chr$(219); String$(38, 223)
    Print Chr$(219)
    Print Chr$(219)
    Print Chr$(219)
    Call DISPFIELD
End Sub


Sub DISPFIELD ()
    Locate 2, 40
    Color 1 + BW * 6, 0
    ' Need to get the Asc values of these char's
    ' This way we can use Chr$ references instead
    ' So other text editors can't screw them up
    Print "Ü"
    Color 15 * (1 - BW), 1 + BW * 6
    Locate 3, 2, 0
    Print String$(22, 32); "1  2  3  4   T   ";

    For I = 0 To 1
        Locate I + 4, 2
        Print "  ";
        Color PA%(I, 2), PA%(I, 1)
        Print A$(I);
        Color 15 * (1 - BW), 1 + BW * 6
        Print Tab(36);
        Locate I + 4, 23

        ' Previously read as Q + (Q = 5)
        ' What is Q?
        For J = 1 To Q + (Q - 5)
            Print Using "## "; S(I, J);
        Next J

        Locate , 35
        Print Using "###   "; S(I, 0);
    Next I
    Locate 4 + P1, 3
    Print "*";
    Locate 5 - P1, 3
    Print " ";
    Color 0, 1 + BW * 6
    Locate 6, 1
    Print "Û"; String$(39, 220);
    Color 15, 4 + BW * 3
    Locate 2, 43
    Print "Û"; String$(36, 223);
    Locate 3, 43
    Print "Û";
    Locate 4, 43
    Print "Û";
    Locate 5, 43
    Print "Û";
    Locate 2, 80: Color 0: Print "ß";
    Call YARDLINE
    Color 15 * (1 - BW)
    Color 15 * (1 - BW), 4 + BW * 3: TI = T: TI = TI + TI * (TI < 0)
    I = Int(TI / 60)
    J = TI - I * 60
    Call TICKUPDATE
    Locate 3, 44: Print Using " TIME: ##.##     QUARTER: # "; I + J / 100, Q;
    Locate 3, 53: Print ":";: Locate , 72: Print Tab(80); " ";
    Locate 4, 44: Print Using " DOWN: #"; D1;: Print Tab(60);
    Print Using " TO GO: ##.#"; Y1;: Print Tab(80); " ";
    Locate 5, 44: Print " BALL ON ";: If Y <= 50 Then Print "OWN";
    If Y > 50 Then Print "OPP";
    Print Using " ##.# YD LINE "; YL;: Print Tab(80); " ";
    Locate 6, 44: Color 0, 4 + BW * 3: Print String$(37, 220);
    Color 15 * (1 - BW), 2 + 5 * BW: For I = 7 To 19: Locate I, 1
    Print String$(80, 32);: Next I
    Call DISPWEATHER
    Color 15 * (1 - BW), 2 + 5 * BW: Locate 8, 4: Print "ÚÄÄÄ";
    For II = 1 To 21: Print "ÄÄÂ";: Next: Print "ÄÄÄÄÄ¿";
    F$ = "³°°°°°": For II = 1 To 20: F$ = F$ + "³  "
    Next: F$ = F$ + "³°°°°°³"
    For II = 9 To 17: Locate II, 4: Print F$: Next: Locate 18, 4
    Print "ÀÄÄÄÄÄ";: For II = 1 To 21: Print "ÁÄÄ";: Next: Print "ÄÄÄÙ";
    For II = 10 To 67 Step 3: For J = 1 To 2: Locate 11, II + J: Print Chr$(34);
        Locate 16, II + J
    Print Chr$(34);: Next: Next
    For II = 1 To 9: For J = 9 To 17 Step 8: Locate J, II * 6 + 9
    Print Using "#³0"; II + 2 * (II - 5) * (II > 5);: Next: Next
    Locate 19, 1: Print Tab(79): Color 14 - 7 * BW, 1 - BW
    Locate 20, 1: Print String$(80, 32);
    Locate 21, 1: Print "Û"; String$(78, 223); "Û";
    Locate 22, 1: Print "Û "; String$(76, 46); " Û";
    Locate 23, 1: Print "Û"; String$(78, 220); "Û";
End Sub


Sub YARDLINE ()
    YL = YT - (YT > 50) * (100 - YT - YT)
End Sub


Sub GOALLINE ()
    '*** GOAL LINE DEFENSE ***
    I6 = Int(Rnd(1) * 100) + 1
    If D2 <= 3 Or D2 >= 6 Then 4400
    GoTo 4500

    4400 If Y >= 94.999 And I6 <= 100 Then G = -1
    If Y >= 94.999 And I6 <= 93 Then G = 0
    If Y >= 94.999 And I6 <= 76 Then G = 1
    If Y >= 94.999 And I6 <= 52 Then G = 2
    If Y >= 94.999 And I6 <= 35 Then G = 3
    If Y >= 94.999 And I6 <= 22 Then G = 4
    If Y >= 94.999 And I6 <= 17 Then G = 6
    If Y >= 95.999 And I6 <= 100 Then G = -1
    If Y >= 95.999 And I6 <= 94 Then G = 0
    If Y >= 95.999 And I6 <= 78 Then G = 1
    If Y >= 95.999 And I6 <= 49 Then G = 2
    If Y >= 95.999 And I6 <= 29 Then G = 3
    If Y >= 95.999 And I6 <= 26 Then G = 5
    If Y >= 96.999 And I6 <= 100 Then G = -1
    If Y >= 96.999 And I6 <= 98 Then G = 0
    If Y >= 96.999 And I6 <= 80 Then G = 1
    If Y >= 96.999 And I6 <= 32 Then G = 2
    If Y >= 96.999 And I6 <= 27 Then G = 4
    If Y >= 97.999 And I6 <= 100 Then G = -1
    If Y >= 97.999 And I6 <= 91 Then G = 0
    If Y >= 97.999 And I6 <= 72 Then G = 1
    If Y >= 97.999 And I6 <= 46 Then G = 3
    If Y >= 98.999 And I6 <= 100 Then G = -1
    If Y >= 98.999 And I6 <= 96 Then G = 0
    If Y >= 98.999 And I6 <= 61 Then G = 2
    GoTo 4550

    4500 If Y >= 94.999 And I6 <= 100 Then G = -4
    If Y >= 94.999 And I6 <= 98 Then G = -3
    If Y >= 94.999 And I6 <= 91 Then G = -2
    If Y >= 94.999 And I6 <= 84 Then G = -1
    If Y >= 94.999 And I6 <= 77 Then G = 0
    If Y >= 94.999 And I6 <= 63 Then G = 1
    If Y >= 94.999 And I6 <= 43 Then G = 2
    If Y >= 94.999 And I6 <= 29 Then G = 3
    If Y >= 94.999 And I6 <= 17 Then G = 4
    If Y >= 94.999 And I6 <= 14 Then G = 6
    If Y >= 95.999 And I6 <= 100 Then G = -5
    If Y >= 95.999 And I6 <= 97 Then G = -4
    If Y >= 95.999 And I6 <= 94 Then G = -3
    If Y >= 95.999 And I6 <= 91 Then G = -2
    If Y >= 95.999 And I6 <= 90 Then G = -1
    If Y >= 95.999 And I6 <= 86 Then G = 0
    If Y >= 95.999 And I6 <= 72 Then G = 1
    If Y >= 95.999 And I6 <= 45 Then G = 2
    If Y >= 95.999 And I6 <= 31 Then G = 3
    If Y >= 95.999 And I6 <= 29 Then G = 5
    If Y >= 96.999 And I6 <= 100 Then G = -4
    If Y >= 96.999 And I6 <= 93 Then G = -2
    If Y >= 96.999 And I6 <= 88 Then G = -1
    If Y >= 96.999 And I6 <= 85 Then G = 0
    If Y >= 96.999 And I6 <= 69 Then G = 1
    If Y >= 96.999 And I6 <= 28 Then G = 2
    If Y >= 96.999 And I6 <= 24 Then G = 4
    If Y >= 97.999 And I6 <= 100 Then G = -4
    If Y >= 97.999 And I6 <= 94 Then G = -2
    If Y >= 97.999 And I6 <= 92 Then G = -1
    If Y >= 97.999 And I6 <= 83 Then G = 0
    If Y >= 97.999 And I6 <= 65 Then G = 1
    If Y >= 97.999 And I6 <= 43 Then G = 3
    If Y >= 98.999 And I6 <= 100 Then G = -4
    If Y >= 98.999 And I6 <= 97 Then G = -3
    If Y >= 98.999 And I6 <= 90 Then G = -2
    If Y >= 98.999 And I6 <= 83 Then G = -1
    If Y >= 98.999 And I6 <= 74 Then G = 0
    If Y >= 98.999 And I6 <= 57 Then G = 2

    4550 I6 = Int(Rnd(1) * 100) + 1
    If I2 <= 30 And I6 <= GL%(I2, 1) Then G = G + GL%(I2, 2)
    If I2 > 30 And I6 <= GL%(30, 1) Then G = G + GL%(30, 2)
End Sub
    

Sub FINDSPACE ()
    I2 = 0

    9611 I2 = I2 + 1: I3 = I2 + 1: If I3 = 13 Then I3 = 12
    If I2 = 13 Then 9615
    If Asc(Mid$(X$, I2, I2)) = 32 And Asc(Mid$(X$, I3, I3)) = 32 Then 9615
    GoTo 9611

    9615 '
End Sub


Sub NAMEEDIT ()
    For I = 0 To 1

        9601 If Right$(A$(I), 1) = " " Then A$(I) = Left$(A$(I), Len(A$(I)) - 1): GoTo 9601

        9602 If Len(AA$(I)) > 1 And Right$(AA$(I), 1) = " " Then AA$(I) = Left$(AA$(I), Len(AA$(I)) - 1): GoTo 9602
        For I1 = 0 To 9: X$ = A1$(I, I1): Call FINDSPACE: A1$(I, I1) = Left$(X$, I2 - 1): Next I1
        For I1 = 0 To 5: X$ = A2$(I, I1): Call FINDSPACE: A2$(I, I1) = Left$(X$, I2 - 1): Next I1
        For I1 = 0 To 3: X$ = A3$(I, I1): Call FINDSPACE: A3$(I, I1) = Left$(X$, I2 - 1): Next I1
        For I1 = 0 To 2: X$ = A4$(I, I1): Call FINDSPACE: A4$(I, I1) = Left$(X$, I2 - 1): Next I1
        For I1 = 0 To 2: X$ = A5$(I, I1): Call FINDSPACE: A5$(I, I1) = Left$(X$, I2 - 1): Next I1
        X$ = A6$(I, 0): Call FINDSPACE: A6$(I, 0) = Left$(X$, I2 - 1)
        For I1 = 0 To 1: X$ = A7$(I, I1): Call FINDSPACE: A7$(I, I1) = Left$(X$, I2 - 1): Next I1
        For I1 = 0 To 14: X$ = DS$(I, I1): Call FINDSPACE: DS$(I, I1) = Left$(X$, I2 - 1): Next I1
        For I1 = 0 To 9: X$ = DI$(I, I1): Call FINDSPACE: DI$(I, I1) = Left$(X$, I2 - 1): Next I1
    Next I
End Sub


Sub SCOUTREPT ()
    J = 13: Call BOXDRAW(J): Locate 8, 30: Print "SCOUTING REPORT": Locate 10, 30
    Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 0, 7
    Locate , 52: Color PA%(1, 2), PA%(1, 1)
    Print A$(1): Color 0, 7: Print
    Locate , 12
    Print "RUSH AVG.";: Locate , 34: Print Using "#.#"; K3(0, 0);
    Locate , 56: Print Using "#.#"; K3(1, 0)
    Locate , 12: Print "FUM %";: Locate , 34
    Print B6(0, 3) + B6(1, 4);: Locate , 56: Print B6(1, 3) + B6(0, 4)
    Locate , 12: Print "COMP %";: Locate , 34: Print K3(0, 1);: Locate , 56: Print K3(1, 1)
    Locate , 12: Print "INT %";: Locate , 34: Print K3(0, 2);: Locate , 56: Print K3(1, 2)
    Locate , 12: Print "AVG/COMP";: Locate , 34: Print Using "##.#"; K3(0, 3) + XD(1);: Locate , 56: Print Using "##.#"; K3(1, 3) + XD(0)
    Locate , 12: Print "PUNT AVG";: Locate , 34: Print K3(0, 4);: Locate , 56: Print K3(1, 4)
    Locate , 12: Print "FG % ";: Locate , 34: Print K3(0, 5);: Locate , 56: Print K3(1, 5)
    Locate , 12: Print "EP % ";: Locate , 34: Print K3(0, 6);: Locate , 56: Print K3(1, 6)
    Locate , 12: Print "SACKED %";: Locate , 34: Print B6(0, 5) + B6(1, 6);: Locate , 56: Print B6(1, 5) + B6(0, 6)
    Locate , 12: Print "RUN/PASS %";: Locate , 34: Print B6(0, 7);: Locate , 56: Print B6(1, 7)
    Locate 12, 38: If K3(0, 0) - (B6(1, 0) * .3) > 4.5 And B6(0, 7) < 50 Then Print "*";
    Locate 12, 60: If K3(1, 0) - (B6(0, 0) * .3) > 4.5 And B6(1, 7) < 50 Then Print "*";
    Locate 16, 38: If K3(0, 3) >= 18 Then Print "*";
    Locate 16, 60: If K3(1, 3) >= 18 Then Print "*";
    I$ = GetKeyPress$
End Sub

Sub GETTEAM ()

    9500 If Not _FileExists(DV$(0) + "FOOT." + YN$) Then 9650
    Open DV$(0) + "FOOT." + YN$ As #1 Len = 934
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
    Get #1, intB8%: intB8%(I) = intB8%: NN$(I) = YN$
    A$(I) = Q$(1): AA$(I) = Q$(2)
    For I1 = 0 To 7: B6(I, I1) = CVI(Q$(I1 + 3)): Next I1
    For I1 = 0 To 4: PA%(I, I1) = CVI(Q$(I1 + 11)): Next I1
    For I1 = 0 To 9
        A1$(I, I1) = Q$(I1 * 5 + 16)
        A(I, I1) = CVI(Q$(I1 * 5 + 17))
        A1(I, I1) = CVI(Q$(I1 * 5 + 18))
        A2(I, I1) = CVI(Q$(I1 * 5 + 19))
        A3(I, I1) = CVI(Q$(I1 * 5 + 20))
    Next I1
    For I1 = 0 To 5
        A2$(I, I1) = Q$(I1 * 3 + 66)
        A4(I, I1) = CVI(Q$(I1 * 3 + 67))
        A5(I, I1) = CVI(Q$(I1 * 3 + 68))
    Next I1
    For I1 = 0 To 3
        A3$(I, I1) = Q$(I1 * 5 + 84)
        A6(I, I1) = CVI(Q$(I1 * 5 + 85))
        A7(I, I1) = CVI(Q$(I1 * 5 + 86))
        A8(I, I1) = CVI(Q$(I1 * 5 + 87))
        QX%(I, I1) = CVI(Q$(I1 * 5 + 88))
    Next I1
    For I1 = 0 To 2
        A4$(I, I1) = Q$(I1 * 3 + 104)
        A9(I, I1) = CVI(Q$(I1 * 3 + 105))
        B(I, I1) = CVI(Q$(I1 * 3 + 106))
    Next I1
    For I1 = 0 To 2
        A5$(I, I1) = Q$(I1 * 3 + 113)
        B1(I, I1) = CVI(Q$(I1 * 3 + 114))
        B2(I, I1) = CVI(Q$(I1 * 3 + 115))
    Next I1
    A6$(I, 0) = Q$(122)
    B3(I, 0) = CVI(Q$(123))
    A7$(I, 0) = Q$(124)
    B4(I, 0) = CVI(Q$(125))
    B5(I, 0) = CVI(Q$(126))
    B7(I, 0) = CVI(Q$(127))
    B8(I, 0) = CVI(Q$(128))
    A7$(I, 1) = Q$(129)
    B4(I, 1) = CVI(Q$(130))
    B5(I, 1) = CVI(Q$(131))
    B7(I, 1) = CVI(Q$(132))
    B8(I, 1) = CVI(Q$(133))
    YC(I) = CVI(Q$(134))
    For I1 = 0 To 14
        DS$(I, I1) = Q$(I1 * 2 + 135)
        DS(I, I1) = CVI(Q$(I1 * 2 + 136))
    Next I1
    For I1 = 0 To 9
        DI$(I, I1) = Q$(I1 * 2 + 165)
        DI(I, I1) = CVI(Q$(I1 * 2 + 166))
    Next I1
    Close #1
    If Left$(AA$(I), 1) = " " Or AA$(I) = "" Then AA$(I) = A$(I)
    PC%(I, 0) = A(I, 0)
    For I1 = 9 To 1 Step -1
        If A(I, I1) = 0 Then 9594
        PC%(I, I1) = A(I, I1) - A(I, I1 - 1)

    9594 Next
    DDS(I) = 0: DDI(I) = 0
    For I1 = 0 To 14: DDS(I) = DDS(I) + DS(I, I1): If DS(I, 0) > 0 Then intDS%(I) = 1
    Next I1
    For I1 = 0 To 9: DDI(I) = DDI(I) + DI(I, I1): If DI(I, 0) > 0 Then intDI%(I) = 1
    Next I1
    GoTo 9595

    9650 Cls:
    Print "ERROR::: TEAM FILE NOT FOUND...BE SURE CORRECT ID AND TEAM NUMBERS ARE USED"
    Locate 4, 1: Input "SEASON DISK ID (USUALLY LAST TWO DIGITS OF YEAR) "; YN$
    Locate 6, 1: Input "ENTER ID NUMBER FOR TEAM  "; intB8%
    GoTo 9500

    9595 '
End Sub


Sub TICKER ()
    '*** TICKER ***
    If AP = 1 And J6(0) = 2 Then 884
    Screen , , 1, 1: Color 14, 0: Cls
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 And HT% = 1 And G9 = 0 Then Locate , 23: Print "PRUDENTIAL AT THE HALF SCOREBOARD": GoTo 851
    If RN > 50 And HT% = 1 And G9 = 0 Then Locate , 28: Print "RED MAN HALF TIME REPORT": GoTo 851
    If HT% = 1 And RN <= 50 Then Locate , 29: Print "TOYOTA HALFTIME REPORT": GoTo 851
    If HT% = 1 And RN > 50 Then Locate , 29: Print "HONDA HALFTIME REPORT": GoTo 851
    If EG% = 1 And G9 = 0 Then Locate , 27: Print "CHEVROLET FINAL SCOREBOARD": GoTo 851
    If EG% = 1 And RN <= 33 Then Locate , 27: Print "USA TODAY FINAL SCOREBOARD": GoTo 851
    If EG% = 1 And RN > 33 And RN <= 67 Then Locate , 24: Print "FEDERAL EXPRESS FINAL SCOREBOARD": GoTo 851
    If EG% = 1 And RN > 67 Then Locate , 23: Print "ESPN SPORTSCENTER FINAL SCOREBOARD": GoTo 851
    If HT% = 0 Then Locate , 30: Print "LANCE HAFFNER GAMES SCOREBOARD": GoTo 851

    851 Color 15: If TMT = 0 Then Locate 10, 28: Print "NO OTHER GAMES SCHEDULED": GoTo 883
    If Q = 1 And T < 900 And T >= 600 Then TE = 1
    If Q = 1 And T < 600 And T >= 300 Then TE = 2
    If Q = 1 And T < 300 Then TE = 3
    If Q = 2 And T < 900 And T >= 600 Then TE = 4
    If Q = 2 And T < 600 And T >= 300 Then TE = 5
    If Q = 2 And T < 300 Then TE = 6
    If Q = 3 And T < 900 And T >= 600 Then TE = 9
    If Q = 3 And T < 600 And T >= 300 Then TE = 10
    If Q = 3 And T < 300 Then TE = 11
    If Q = 4 And T < 900 And T >= 600 Then TE = 12
    If Q = 4 And T < 600 And T >= 300 Then TE = 13
    If Q = 4 And T < 300 Then TE = 14
    If Q = 5 And T < 900 And T >= 600 Then TE = 15
    If Q = 5 And T < 600 And T >= 300 Then TE = 16
    If Q = 5 And T < 300 Then TE = 17
    If HT% = 1 Then TE = 8
    If Left$(SX$(1, 0), 3) = "XXX" Then 852
    Locate 2, 1: X = 1: GoSub 885: Print Left$(SX$(1, 0), 13);: Locate , 15
    Print Using "## "; SX%(1, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(1, 1), 13);: Locate , 15: Print Using "## #"; SX%(1, 1, TS%)

    852 If Left$(SX$(2, 0), 3) = "XXX" Then 853
    Locate 5, 1: X = 2: GoSub 885: Print Left$(SX$(2, 0), 13);: Locate , 15: Print Using "## "; SX%(2, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(2, 1), 13);: Locate , 15: Print Using "## #"; SX%(2, 1, TS%)

    853 If Left$(SX$(3, 0), 3) = "XXX" Then 854
    Locate 8, 1: X = 3: GoSub 885: Print Left$(SX$(3, 0), 13);: Locate , 15: Print Using "## "; SX%(3, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(3, 1), 13);: Locate , 15: Print Using "## #"; SX%(3, 1, TS%)

    854 If Left$(SX$(4, 0), 3) = "XXX" Then 855
    Locate 11, 1: X = 4: GoSub 885: Print Left$(SX$(4, 0), 13);: Locate , 15: Print Using "## "; SX%(4, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(4, 1), 13);: Locate , 15: Print Using "## #"; SX%(4, 1, TS%)

    855 If Left$(SX$(5, 0), 3) = "XXX" Then 856
    Locate 14, 1: X = 5: GoSub 885: Print Left$(SX$(5, 0), 13);: Locate , 15: Print Using "## "; SX%(5, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(5, 1), 13);: Locate , 15: Print Using "## #"; SX%(5, 1, TS%)

    856 If Left$(SX$(6, 0), 3) = "XXX" Then 857
    Locate 17, 1: X = 6: GoSub 885: Print Left$(SX$(6, 0), 13);: Locate , 15: Print Using "## "; SX%(6, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(6, 1), 13);: Locate , 15: Print Using "## #"; SX%(6, 1, TS%)

    857 If Left$(SX$(7, 0), 3) = "XXX" Then 858
    Locate 20, 1: X = 7: GoSub 885: Print Left$(SX$(7, 0), 13);: Locate , 15: Print Using "## "; SX%(7, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(7, 1), 13);: Locate , 15: Print Using "## #"; SX%(7, 1, TS%)

    858 If Left$(SX$(8, 0), 3) = "XXX" Then 859
    Locate 23, 1: X = 8: GoSub 885: Print Left$(SX$(8, 0), 13);: Locate , 15: Print Using "## "; SX%(8, 0, TS%);: Print R$(TS%)
    Print Left$(SX$(8, 1), 13);: Locate , 15: Print Using "## #"; SX%(8, 1, TS%);

    859 If Left$(SX$(9, 0), 3) = "XXX" Then 860
    Locate 2, 21: X = 9: GoSub 885: Print Left$(SX$(9, 0), 13);: Locate , 35: Print Using "## "; SX%(9, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(9, 1), 13);: Locate , 35: Print Using "## #"; SX%(9, 1, TS%)

    860 If Left$(SX$(10, 0), 3) = "XXX" Then 861
    Locate 5, 21: X = 10: GoSub 885: Print Left$(SX$(10, 0), 13);: Locate , 35: Print Using "## "; SX%(10, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(10, 1), 13);: Locate , 35: Print Using "## #"; SX%(10, 1, TS%)

    861 If Left$(SX$(11, 0), 3) = "XXX" Then 862
    Locate 8, 21: X = 11: GoSub 885: Print Left$(SX$(11, 0), 13);: Locate , 35: Print Using "## "; SX%(11, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(11, 1), 13);: Locate , 35: Print Using "## #"; SX%(11, 1, TS%)

    862 If Left$(SX$(12, 0), 3) = "XXX" Then 863
    Locate 11, 21: X = 12: GoSub 885: Print Left$(SX$(12, 0), 13);: Locate , 35: Print Using "## "; SX%(12, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(12, 1), 13);: Locate , 35: Print Using "## #"; SX%(12, 1, TS%)

    863 If Left$(SX$(13, 0), 3) = "XXX" Then 864
    Locate 14, 21: X = 13: GoSub 885: Print Left$(SX$(13, 0), 13);: Locate , 35: Print Using "## "; SX%(13, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(13, 1), 13);: Locate , 35: Print Using "## #"; SX%(13, 1, TS%)

    864 If Left$(SX$(14, 0), 3) = "XXX" Then 865
    Locate 17, 21: X = 14: GoSub 885: Print Left$(SX$(14, 0), 13);: Locate , 35: Print Using "## "; SX%(14, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(14, 1), 13);: Locate , 35: Print Using "## #"; SX%(14, 1, TS%)

    865 If Left$(SX$(15, 0), 3) = "XXX" Then 866
    Locate 20, 21: X = 15: GoSub 885: Print Left$(SX$(15, 0), 13);: Locate , 35: Print Using "## "; SX%(15, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(15, 1), 13);: Locate , 35: Print Using "## #"; SX%(15, 1, TS%)

    866 If Left$(SX$(16, 0), 3) = "XXX" Then 867
    Locate 23, 21: X = 16: GoSub 885: Print Left$(SX$(16, 0), 13);: Locate , 35: Print Using "## "; SX%(16, 0, TS%);: Print R$(TS%)
    Locate , 21: Print Left$(SX$(16, 1), 13);: Locate , 35: Print Using "## #"; SX%(16, 1, TS%);

    867 If Left$(SX$(17, 0), 3) = "XXX" Then 868
    Locate 2, 41: X = 17: GoSub 885: Print Left$(SX$(17, 0), 13);: Locate , 55: Print Using "## "; SX%(17, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(17, 1), 13);: Locate , 55: Print Using "## #"; SX%(17, 1, TS%)

    868 If Left$(SX$(18, 0), 3) = "XXX" Then 869
    Locate 5, 41: X = 18: GoSub 885: Print Left$(SX$(18, 0), 13);: Locate , 55: Print Using "## "; SX%(18, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(18, 1), 13);: Locate , 55: Print Using "## #"; SX%(18, 1, TS%)

    869 If Left$(SX$(19, 0), 3) = "XXX" Then 870
    Locate 8, 41: X = 19: GoSub 885: Print Left$(SX$(19, 0), 13);: Locate , 55: Print Using "## "; SX%(19, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(19, 1), 13);: Locate , 55: Print Using "## #"; SX%(19, 1, TS%)

    870 If Left$(SX$(20, 0), 3) = "XXX" Then 871
    Locate 11, 41: X = 20: GoSub 885: Print Left$(SX$(20, 0), 13);: Locate , 55: Print Using "## "; SX%(20, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(20, 1), 13);: Locate , 55: Print Using "## #"; SX%(20, 1, TS%)

    871 If Left$(SX$(21, 0), 3) = "XXX" Then 872
    Locate 14, 41: X = 21: GoSub 885: Print Left$(SX$(21, 0), 13);: Locate , 55: Print Using "## "; SX%(21, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(21, 1), 13);: Locate , 55: Print Using "## #"; SX%(21, 1, TS%)

    872 If Left$(SX$(22, 0), 3) = "XXX" Then 873
    Locate 17, 41: X = 22: GoSub 885: Print Left$(SX$(22, 0), 13);: Locate , 55: Print Using "## "; SX%(22, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(22, 1), 13);: Locate , 55: Print Using "## #"; SX%(22, 1, TS%)

    873 If Left$(SX$(23, 0), 3) = "XXX" Then 874
    Locate 20, 41: X = 23: GoSub 885: Print Left$(SX$(23, 0), 13);: Locate , 55: Print Using "## "; SX%(23, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(23, 1), 13);: Locate , 55: Print Using "## #"; SX%(23, 1, TS%)

    874 If Left$(SX$(24, 0), 3) = "XXX" Then 875
    Locate 23, 41: X = 24: GoSub 885: Print Left$(SX$(24, 0), 13);: Locate , 55: Print Using "## "; SX%(24, 0, TS%);: Print R$(TS%)
    Locate , 41: Print Left$(SX$(24, 1), 13);: Locate , 55: Print Using "## #"; SX%(24, 1, TS%);

    875 If Left$(SX$(25, 0), 3) = "XXX" Then 876
    Locate 2, 61: X = 25: GoSub 885: Print Left$(SX$(25, 0), 13);: Locate , 75: Print Using "## "; SX%(25, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(25, 1), 13);: Locate , 75: Print Using "## #"; SX%(25, 1, TS%)

    876 If Left$(SX$(26, 0), 3) = "XXX" Then 877
    Locate 5, 61: X = 26: GoSub 885: Print Left$(SX$(26, 0), 13);: Locate , 75: Print Using "## "; SX%(26, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(26, 1), 13);: Locate , 75: Print Using "## #"; SX%(26, 1, TS%)

    877 If Left$(SX$(27, 0), 3) = "XXX" Then 878
    Locate 8, 61: X = 27: GoSub 885: Print Left$(SX$(27, 0), 13);: Locate , 75: Print Using "## "; SX%(27, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(27, 1), 13);: Locate , 75: Print Using "## #"; SX%(27, 1, TS%)

    878 If Left$(SX$(28, 0), 3) = "XXX" Then 879
    Locate 11, 61: X = 28: GoSub 885: Print Left$(SX$(28, 0), 13);: Locate , 75: Print Using "## "; SX%(28, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(28, 1), 13);: Locate , 75: Print Using "## #"; SX%(28, 1, TS%)

    879 If Left$(SX$(29, 0), 3) = "XXX" Then 880
    Locate 14, 61: X = 29: GoSub 885: Print Left$(SX$(29, 0), 13);: Locate , 75: Print Using "## "; SX%(29, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(29, 1), 13);: Locate , 75: Print Using "## #"; SX%(29, 1, TS%)

    880 If Left$(SX$(30, 0), 3) = "XXX" Then 881
    Locate 17, 61: X = 30: GoSub 885: Print Left$(SX$(30, 0), 13);: Locate , 75: Print Using "## "; SX%(30, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(30, 1), 13);: Locate , 75: Print Using "## #"; SX%(30, 1, TS%)

    881 If Left$(SX$(31, 0), 3) = "XXX" Then 882
    Locate 20, 61: X = 31: GoSub 885: Print Left$(SX$(31, 0), 13);: Locate , 75: Print Using "## "; SX%(31, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(31, 1), 13);: Locate , 75: Print Using "## #"; SX%(31, 1, TS%)

    882 If Left$(SX$(32, 0), 3) = "XXX" Then 883
    Locate 23, 61: X = 32: GoSub 885: Print Left$(SX$(32, 0), 13);: Locate , 75: Print Using "## "; SX%(32, 0, TS%);: Print R$(TS%)
    Locate , 61: Print Left$(SX$(32, 1), 13);: Locate , 75: Print Using "## #"; SX%(32, 1, TS%);
    883 I$ = GetKeyPress$

    884 Screen , , 0, 0
    GoTo 886

    885 TS% = TE + (ST% - ST%(X)) * 3
    If TS% < 0 Then TS% = 0
    If TS% > 14 Then TS% = 14
    If EG% = 1 Then TS% = 14
    Return

    886 '
End Sub


Sub SAVESTATS ()
    Cls
    PN = Q
    For I = 0 To 1
        B$(I) = A$(I): Y$(I) = B$(I)
    Next
    For Q = 0 To 1
        C% = 0
        Cls
        Print "COMPILE STATS FOR ";
        Color PA%(Q, 2), PA%(Q, 1): Print B$(Q);: Color 15, 0
        Print " (Y/N) ";
        If AP = 1 Then 8093
        I$ = GetKeyPress$
        If I$ = "N" Then C% = 1: GoTo 8093
        If UCase$(Left$(DV$(1), 1)) = "C" Then 8093
        Cls

        8093 If C% = 1 Then _Continue
        For XX = 4 To 11: If Mid$(B$(Q), XX, 1) = " " Or Mid$(B$(Q), XX, 1) = "." Then Mid$(B$(Q), XX, 1) = "_"
        Next XX
        For XX = 12 To 14: If Mid$(B$(Q), XX, 2) = "ST" Then Mid$(B$(Q), 9, 1) = "_": Mid$(B$(Q), 10, 2) = "ST"
        Next XX
        QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$(Q), XX, 1) = "_" Then QQ = XX - 4: GoTo 8137
            GoTo 8138

        8137 Next XX

        8138 F$ = Mid$(B$(Q), 4, QQ)
        R$ = F$ + "." + Left$(B$(Q), 2)
        If Not _FileExists(DV$(1) + R$) Then 8056
        Open DV$(1) + R$ For Input As #2
        Input #2, B$(Q)
        For I = 0 To 9: Input #2, A1$(Q, I): Next: For I = 0 To 5: Input #2, A2$(Q, I): Next: For I = 0 To 3: Input #2, A3$(Q, I): Next
        For I = 0 To 2: Input #2, A4$(Q, I): Next: For I = 0 To 2: Input #2, A5$(Q, I): Next
        Input #2, A6$(Q, 0): For I = 0 To 1: Input #2, A7$(Q, I): Next
        For J = 0 To 13: For K = 0 To 17: Input #2, Z2(J, K): Next: Next
        For L = 0 To 38: Input #2, Z(L): Next
        For L = 0 To 38: Input #2, Z1(L): Next
        For J = 0 To 9
            Input #2, DI$(Q, J)
            For J1 = 0 To 2
                Input #2, GI(Q, J, J1)
            Next
        Next
        For J = 0 To 14
            Input #2, DS$(Q, J)
            For J1 = 0 To 1
                Input #2, GS(Q, J, J1)
            Next
        Next
        For I = 0 To 9: Input #2, LR(Q, I): Input #2, strLR$(Q, I, 0): Input #2, strLR$(Q, I, 1): Next
        For I = 0 To 15: Input #2, LC(Q, I): Input #2, strLC$(Q, I, 0): Input #2, strLC$(Q, I, 1): Next
        For I = 0 To 3: Input #2, LP(Q, I): Input #2, LP$(Q, I, 0): Input #2, LP$(Q, I, 1): Next
        For I = 0 To 2: Input #2, LK(Q, I): Input #2, LK$(Q, I, 0): Input #2, LK$(Q, I, 1): Next
        For I = 0 To 2: Input #2, PK(Q, I): Input #2, PK$(Q, I, 0): Input #2, PK$(Q, I, 1): Next
        For I = 0 To 1: Input #2, LF(Q, I): Input #2, LF$(Q, I): For I1 = 0 To 4: Input #2, AM(Q, I, I1): Input #2, AF(Q, I, I1): Next: Next
        For I = 0 To 9: Input #2, LI(Q, I): Input #2, LI$(Q, I, 0): Input #2, LI$(Q, I, 1): Next
        Close #2

        8056 For I = 0 To 9
            If RB(Q, I) > LR(Q, I) Then LR(Q, I) = RB(Q, I): strLR$(Q, I, 0) = strRB$(Q, I): strLR$(Q, I, 1) = A$(1 - Q)
            If IR(Q, I) > LI(Q, I) Then LI(Q, I) = IR(Q, I): LI$(Q, I, 0) = strIR$(Q, I): LI$(Q, I, 1) = A$(1 - Q)
        Next
        For I = 0 To 15
            If WR(Q, I) > LC(Q, I) Then LC(Q, I) = WR(Q, I): strLC$(Q, I, 0) = strWR$(Q, I): strLC$(Q, I, 1) = A$(1 - Q)
        Next
        For I = 0 To 3
            If QB(Q, I) > LP(Q, I) Then LP(Q, I) = QB(Q, I): LP$(Q, I, 0) = strQB$(Q, I): LP$(Q, I, 1) = A$(1 - Q)
            If KR(Q, I) > LK(Q, I) Then LK(Q, I) = KR(Q, I): LK$(Q, I, 0) = strKR$(Q, I): LK$(Q, I, 1) = A$(1 - Q)
            If PR(Q, I) > PK(Q, I) Then PK(Q, I) = PR(Q, I): PK$(Q, I, 0) = strPR$(Q, I): PK$(Q, I, 1) = A$(1 - Q)
        Next
        If K(Q, 26) > Z(28) Then Z(28) = K(Q, 26)
        For I = 0 To 1
            If FL(Q, I) > LF(Q, I) Then LF(Q, I) = FL(Q, I): LF$(Q, 1) = A$(1 - Q)
            For I1 = 0 To 4
                AM(Q, I, I1) = AM(Q, I, I1) + FM(Q, I, I1)
                AF(Q, I, I1) = AF(Q, I, I1) + FA(Q, I, I1)
        Next: Next
        For J = 0 To 13
            For K = 0 To 17: Z2(J, K) = Z2(J, K) + Int(K2(Q, J, K) + .5): Next
        Next
        Z(32) = Z(32) + S(Q, 1)
        Z(33) = Z(33) + S(Q, 2)
        Z(34) = Z(34) + S(Q, 3)
        Z(35) = Z(35) + S(Q, 4)
        For K = 5 To PN
            Z(36) = Z(36) + S(Q, K)
        Next
        Z1(32) = Z1(32) + S(1 - Q, 1)
        Z1(33) = Z1(33) + S(1 - Q, 2)
        Z1(34) = Z1(34) + S(1 - Q, 3)
        Z1(35) = Z1(35) + S(1 - Q, 4)
        For K = 5 To PN
            Z1(36) = Z1(36) + S(1 - Q, K)
        Next
        For J = 0 To 3
            Z(17) = Z(17) + K2(Q, J, 13): Z1(17) = Z1(17) + K2(1 - Q, J, 13)
        Next
        For J = 0 To 13
            Z(15) = Z(15) + K2(Q, J, 2): Z1(15) = Z1(15) + K2(1 - Q, J, 2)
        Next
        For J = 0 To 2
            Z(18) = Z(18) + K2(Q, J, 16): Z1(18) = Z1(18) + K2(1 - Q, J, 16)
            Z(22) = Z(22) + K2(Q, J, 14): Z1(22) = Z1(22) + K2(1 - Q, J, 14)
            Z(23) = Z(23) + K2(Q, J, 15): Z1(23) = Z1(23) + K2(1 - Q, J, 15)
            Z(19) = Z(19) + K2(Q, J + 3, 16): Z1(18) = Z1(19) + K2(1 - Q, J + 3, 16)
            Z(24) = Z(24) + K2(Q, J + 3, 14): Z1(24) = Z1(24) + K2(1 - Q, J + 3, 14)
            Z(25) = Z(25) + K2(Q, J + 3, 15): Z1(25) = Z1(25) + K2(1 - Q, J + 3, 15)
        Next
        For J = 0 To 9
            Z(20) = Z(20) + SI(Q, J, 2): Z1(20) = Z1(20) + SI(1 - Q, J, 2)
            Z(26) = Z(26) + SI(Q, J, 0): Z1(26) = Z1(26) + SI(1 - Q, J, 0)
            Z(27) = Z(27) + SI(Q, J, 1): Z1(27) = Z1(27) + SI(1 - Q, J, 1)
        Next
        Z(21) = Z(21) + K(Q, 19): Z1(21) = Z1(21) + K(1 - Q, 19)
        Z(37) = Z(37) + K(Q, 35): Z1(37) = Z1(37) + K(1 - Q, 35)
        Z(38) = Z(38) + K(Q, 36): Z1(38) = Z1(38) + K(1 - Q, 36)
        Z(30) = Z(30) + K(Q, 28): Z1(30) = Z1(30) + K(1 - Q, 28)
        Z(29) = Z(29) + K(Q, 27): Z1(29) = Z1(29) + K(1 - Q, 27)
        Z(31) = Z(31) + K(Q, 29): Z1(31) = Z1(31) + K(1 - Q, 29)
        For L = 0 To 12: Z(L) = Z(L) + K(Q, L): Next
        For L = 0 To 12: Z1(L) = Z1(L) + K(1 - Q, L): Next
        Z(13) = Z(13) + W6(Q, 0)
        Z(14) = Z(14) + W6(Q, 1)
        Z1(13) = Z1(13) + W6(1 - Q, 0)
        Z1(14) = Z1(14) + W6(1 - Q, 1)
        For J = 0 To 9
            For J1 = 0 To 2
                GI(Q, J, J1) = GI(Q, J, J1) + SI(Q, J, J1)
            Next
        Next
        For J = 0 To 14
            For J1 = 0 To 1
                GS(Q, J, J1) = GS(Q, J, J1) + SK(Q, J, J1)
            Next
        Next
        Print ">>>>>": Print: Print: Print " THE STATS ARE BEING SAVED UNDER": Print "THE FOLLOWING NAME::: ";
        Color PA%(Q, 2), PA%(Q, 1): Print R$: Color 15, 0
        Open DV$(1) + R$ For Output As #1
        Print #1, Y$(Q)
        For I = 0 To 9: Print #1, A1$(Q, I): Next: For I = 0 To 5: Print #1, A2$(Q, I): Next: For I = 0 To 3: Print #1, A3$(Q, I): Next
        For I = 0 To 2: Print #1, A4$(Q, I): Next: For I = 0 To 2: Print #1, A5$(Q, I): Next
        Print #1, A6$(Q, 0): For I = 0 To 1: Print #1, A7$(Q, I): Next
        For J = 0 To 13
            For K = 0 To 17: Print #1, Z2(J, K): Next
        Next
        For L = 0 To 38: Print #1, Z(L): Next
        For L = 0 To 38: Print #1, Z1(L): Next
        For J = 0 To 9
            Print #1, DI$(Q, J)
            For J1 = 0 To 2
                Print #1, GI(Q, J, J1)
            Next
        Next
        For J = 0 To 14
            Print #1, DS$(Q, J)
            For J1 = 0 To 1
                Print #1, GS(Q, J, J1)
            Next
        Next
        For I = 0 To 9: Print #1, LR(Q, I): Print #1, strLR$(Q, I, 0): Print #1, strLR$(Q, I, 1): Next
        For I = 0 To 15: Print #1, LC(Q, I): Print #1, strLC$(Q, I, 0): Print #1, strLC$(Q, I, 1): Next
        For I = 0 To 3: Print #1, LP(Q, I): Print #1, LP$(Q, I, 0): Print #1, LP$(Q, I, 1): Next
        For I = 0 To 2: Print #1, LK(Q, I): Print #1, LK$(Q, I, 0): Print #1, LK$(Q, I, 1): Next
        For I = 0 To 2: Print #1, PK(Q, I): Print #1, PK$(Q, I, 0): Print #1, PK$(Q, I, 1): Next
        For I = 0 To 1: Print #1, LF(Q, I): Print #1, LF$(Q, I): For I1 = 0 To 4: Print #1, AM(Q, I, I1): Print #1, AF(Q, I, I1): Next: Next
        For I = 0 To 9: Print #1, LI(Q, I): Print #1, LI$(Q, I, 0): Print #1, LI$(Q, I, 1): Next
        Close #1
        Print
        Y% = 0: R$ = R$ + "S"
        If Not _FileExists(DV$(1) + R$) Then 8103
        Open DV$(1) + R$ For Input As #1
        Input #1, Y%
        For I = 1 To 120: Input #1, O%(I): Next
        For I = 1 To 120: Input #1, HO$(I): Next
        For I = 1 To 120: Input #1, Time%(I): Next
        For I = 1 To 120: Input #1, H$(I): Next
        For I = 1 To 120: Input #1, intPR%(0, I): Next
        For I = 1 To 120: Input #1, intPR%(1, I): Next
        For I = 1 To 120: Input #1, CRD(I): Next
        For I = 1 To 120: Input #1, SITE$(I): Next
        Close #1

        8103 If Y% < 120 Then Y% = Y% + 1
        If H% = Q Then H$(Y%) = "H"
        If H% <> Q Then H$(Y%) = "A"
        If H% = 2 Then H$(Y%) = "X"
        If H% = Q And PN > 4 Then H$(Y%) = "H-" + Str$(PN - 4) + " OT"
        If H% <> Q And PN > 4 Then H$(Y%) = "A-" + Str$(PN - 4) + " OT"
        If H% = 2 And PN > 4 Then H$(Y%) = "X-" + Str$(PN - 4) + " OT"
        O%(Y%) = S(Q, 0): HO$(Y%) = A$(1 - Q): Time%(Y%) = S(1 - Q, 0)
        CRD(Y%) = ATT: SITE$(Y%) = SITE$
        intPR%(0, Y%) = PA%(Q, 4): intPR%(1, Y%) = PA%(1 - Q, 4)
        Open DV$(1) + R$ For Output As #1
        Print #1, Y%
        For I = 1 To 120: Print #1, O%(I): Next
        For I = 1 To 120: Print #1, HO$(I): Next
        For I = 1 To 120: Print #1, Time%(I): Next
        For I = 1 To 120: Print #1, H$(I): Next
        For I = 1 To 120: Print #1, intPR%(0, I): Next
        For I = 1 To 120: Print #1, intPR%(1, I): Next
        For I = 1 To 120: Print #1, CRD(I): Next
        For I = 1 To 120: Print #1, SITE$(I): Next
        Close #1
        For I = 1 To 120
            intPR%(0, I) = 0: intPR%(1, I) = 0: O%(I) = 0: Time%(I) = 0: HO$(I) = "": H$(I) = ""
        Next
        For J = 0 To 10: For K = 0 To 17
        Z2(J, K) = 0: Next: Next
        For L = 0 To 38: Z(L) = 0: Next
        For L = 0 To 38: Z1(L) = 0: Next

    8079 Next Q
End Sub


Sub ADDBACKS ()
    For I = 0 To 1
        For I1 = 0 To 9
            For I2 = 0 To 3
                If A1$(I, I1) = A3$(I, I2) Then K2(I, I1, 0) = K2(I, I1, 0) + K2(I, I2 + 10, 0): K2(I, I1, 1) = K2(I, I1, 1) + K2(I, I2 + 10, 1): K2(I, I1, 2) = K2(I, I1, 2) + K2(I, I2 + 10, 2): K2(I, I2 + 10, 0) = 0: K2(I, I2 + 10, 1) = 0: K2(I, I2 + 10, 2) = 0
    Next I2: Next I1: Next I
End Sub


Sub GETSPEED ()
    If AP = 1 And J6(0) = 2 Then 4098
    I$ = GetKeyPress$

4098 End Sub


Sub DECIDE ()
    If X = 0 And AP <> 1 Or AP = 1 And J6(0) <> 2 Then Call GETSPEED
End Sub


Sub DRAWABOX ()
    SN = 1: Locate 1, 1: Color 0, 7: Print "Ö"; String$(77, 196); "·";
    For II = 1 To J: Locate 1 + II, 1: Print "º"; String$(77, 32); "º";: Next
    Locate 2 + J, 1: Print "Ó"; String$(77, 196); "½";: Locate 2, 32
End Sub


Sub ONSIDE ()
    X = Int(Rnd(1) * 100) + 1
    U$ = A7$(P1, Q7%(P1)) + " IS PLACING THE BALL ON THE TEE": Call PBP
    If X <= 50 Then
        U$ = A$(P1) + " IS LINING UP FOR THE KICKOFF"
    Else
        U$ = AA$(P1) + " ARE LINING UP FOR THE KICKOFF"
    End If
    Call PBP
    U$ = "IT'S AN ONSIDE KICK!!": Call PBP: Swap P1, D
    I = (Rnd(1) * 21) + 5: Y = Y + I: G = I: TP = ((Rnd(1) * 3) + 4): T = T - TP: TP(P1) = TP(P1) + TP
    If Int(G * 10 + .5) / 10 < 9.95 Then U$ = "BALL DID NOT TRAVEL 10 YDS": Call PBP: Y = 100 - Y: GoTo 3956
    I = (Rnd(1) * 100) + 1
    If I <= 78 Then U$ = A$(P1) + " RECOVERS": Call PBP: Y = 100 - Y: GoTo 3956
    If I > 78 Then U$ = A$(D) + " RECOVERS !!!": Call PBP: Call CHANGEPOSS

    3956 Rem
End Sub


Sub PRINTBOX ()
    Open DV$(1) + NM$ For Output As #1
    If H% = 2 Then Print #1, "SITE: NEUTRAL  ATTENDANCE: "; ATT
    If H% <> 2 Then Print #1, "SITE: "; SITE$;: Print #1, Using "  ATTENDANCE: ####,###"; ATT
    Print #1,
    For I = 0 To 1
        Print #1, A$(I); Tab(20);
        Print #1, Using "## ## ## ## "; S(I, 1), S(I, 2), S(I, 3), S(I, 4);
        If Q > 4 Then
            For I1 = 5 To Q
                Print #1, Using "## "; S(I, I1);
            Next
        End If
        Print #1, "- ";
        Print #1, Using "###"; S(I, 0)
    Next I
    Print #1,
    If X2 = 0 Then 9730
    For I = 1 To X2
        Print #1, A$(K1(I, 1)); "-";
        TI = T(I): TI = TI + TI * (TI < 0): IR% = Int(TI / 60)
        J = TI - IR% * 60
        Print #1, Tab(17);: Print #1, Using "##"; IR%;: Print #1, ":";
        If J >= 10 Then Print #1, Using "## "; J; Else Print #1, Using "0#"; J;
        Print #1, Tab(23);
        If C(I) = 1 Then Print #1, "OF 1ST -";
        If C(I) = 2 Then Print #1, "OF 2ND -";
        If C(I) = 3 Then Print #1, "OF 3RD -";
        If C(I) = 4 Then Print #1, "OF 4TH -";
        If C(I) > 4 Then Print #1, "OF OT "; C(I) - 4; " -";
        If K1(I, 0) <> 7 Then Print #1, Int(K1(I, 5) + .5);
        On K1(I, 0) GOTO 9708, 9705, 9709, 9710, 9711, 9712, 9713, 9714, 9715, 9716, 9704, 9729

        9704 Print #1, "FAKE PUNT", K1(I, 2): GoTo 9717

        9705 Print #1, "PASS BY "; A3$(K1(I, 1), K1(I, 2));: Print #1, " TO ";
        If K1(I, 3) > 9 Then Print #1, A2$(K1(I, 1), K1(I, 3) - 10)
        If K1(I, 3) <= 9 Then Print #1, A1$(K1(I, 1), K1(I, 3))
        GoTo 9717

        9708 Print #1, "RUN BY "; A1$(K1(I, 1), K1(I, 2)): GoTo 9717

        9709 Print #1, "K.O. RETURN BY "; A4$(K1(I, 1), K1(I, 2)): GoTo 9717

        9710 Print #1, "PUNT RETURN BY "; A5$(K1(I, 1), K1(I, 2)): GoTo 9717

        9711 Print #1, "INTERCEPTION RETURN BY "; DI$(K1(I, 1), K1(I, 2)): GoTo 9717

        9729 Print #1, "2-PT DEFENSE RETURN BY "; DI$(K1(I, 1), K1(I, 2)): GoTo 9726

        9712 Print #1, "BLOCKED KICK RECOVERED IN END ZONE": GoTo 9717

        9713 Print #1, "SAFETY": GoTo 9726

        9714 Print #1, "FG BY "; A7$(K1(I, 1), K1(I, 2)): GoTo 9726

        9715 Print #1, "RUN BY "; A3$(K1(I, 1), K1(I, 2)): GoTo 9717

        9716 Print #1, "FUMBLE RETURN ": GoTo 9717

        9717 Print #1, Tab(17);
        If K1(I, 4) = 10 Then Print #1, " (KICK FAILED)"
        If K1(I, 4) = 20 Then Print #1, " (RUN FAILED)"
        If K1(I, 4) = 30 Then Print #1, " (PASS FAILED)"
        If K1(I, 4) = 11 Then Print #1, " (KICK BY "; A7$(K1(I, 1), K1(I, 6));: Print #1, ")"
        If K1(I, 4) = 21 Then Print #1, " (RUN GOOD)"
        If K1(I, 4) = 31 Then Print #1, " (PASS GOOD)"

    9726 Next I

    9730 Print #1,
    Print #1, Tab(32); "TEAM STATISTICS"
    Print #1, Tab(15); A$(0); Tab(50); A$(1)
    Print #1, Tab(20); K(0, 0); Tab(34); "FIRST DOWNS"; Tab(55); K(1, 0)
    Print #1, Tab(18);
    If G9 = 0 Then Print #1, K(0, 1); "-"; Int(K(0, 2) + .5); Tab(34); "RUSHING YDS"; Tab(53); K(1, 1); "-"; Int(K(1, 2) + .5)
    If G9 <> 0 Then Print #1, K(0, 1) - W6(0, 0); "-"; Int(K(0, 2) + W6(0, 1) + .5); Tab(34); "RUSHING YDS"; Tab(53); K(1, 1) - W6(1, 0); "-"; Int(K(1, 2) + W6(1, 1) + .5)
    Print #1, Tab(20); Int(K(0, 6) + .5); Tab(34); "PASSING YDS"; Tab(55); Int(K(1, 6) + .5)
    Print #1, Tab(20); Int(K(0, 6) + .5) + Int(K(0, 2) + .5); Tab(35); "TOTAL YDS"; Tab(55); Int(K(1, 6) + .5) + Int(K(1, 2) + .5)
    Print #1, Tab(16); K(0, 4); "-"; K(0, 3); "-"; K(0, 5); Tab(37); "PASSES"; Tab(51); K(1, 4); "-"; K(1, 3); "-"; K(1, 5)
    If K(0, 7) = 0 Or K(1, 7) = 0 Then 9731
    Print #1, Tab(15); K(0, 7); "-"; Int((K(0, 8) / K(0, 7)) * 10 + .5) / 10; Tab(38); "PUNTS"; Tab(50); K(1, 7); "-"; Int((K(1, 8) / K(1, 7)) * 10 + .5) / 10

    9731 Print #1, Tab(18); K(0, 9); "-"; K(0, 10); Tab(36); "FUM/LOST"; Tab(53); K(1, 9); "-"; K(1, 10)
    Print #1, Tab(18); K(0, 11); "-"; Int(K(0, 12) + .5); Tab(35); "PENALTIES"; Tab(53); K(1, 11); "-"; Int(K(1, 12) + .5)
    Print #1, Tab(18); W6(0, 0); "-"; Int(W6(0, 1) + .5); Tab(37); "SACKED"; Tab(53); W6(1, 0); "-"; Int(W6(1, 1) + .5)
    Print #1, Tab(18): Print #1, Using "##"; Int(TP(0) / 60);: Print #1, ":";
    Print #1, Using "##"; TP(0) - Int(TP(0) / 60) * 60;
    Print #1, Tab(31); "TIME OF POSSESSION";
    Print #1, Tab(53);
    Print #1, Using "##"; Int(TP(1) / 60);
    Print #1, ":";
    Print #1, Using "##"; TP(1) - Int(TP(1) / 60) * 60
    Print #1, Tab(16); TE%(0) - TF%(0); "-"; TE%(0); Tab(32); "THIRD DOWN EFF"; Tab(51); TE%(1) - TF%(1); "-"; TE%(1)
    Print #1,
    Print #1, Tab(17); "RUSHING STATISTICS"
    Call ADDBACKS
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "TC   YDS   AVG   LG   TD"
        For I1 = 0 To 13
            If K2(I, I1, 0) = 0 Then 9732
            If I1 <= 9 Then Print #1, A1$(I, I1);
            If I1 > 9 Then Print #1, A3$(I, I1 - 10);
            Print #1, Tab(18); Using "###  #### ###.#  "; K2(I, I1, 0), Int(K2(I, I1, 1) + .5), Int(K2(I, I1, 1) + .5) / K2(I, I1, 0);
            If I1 <= 9 Then Print #1, Using "###   ##"; RB(I, I1), K2(I, I1, 2)
            If I1 > 9 Then Print #1, Using "      ##"; K2(I, I1, 2)

        9732 Next I1
        Print #1,
    Next I
    Print #1, Tab(17); "RECEIVING"
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "TC    YDS    AVG   LG    TD"
        For I1 = 0 To 5
            If K2(I, I1, 6) = 0 Then 9733
            Print #1, A2$(I, I1); Tab(19);
            Print #1, Using "##    ###   ##.#  ###    ##"; K2(I, I1, 6), Int(K2(I, I1, 7) + .5), Int(K2(I, I1, 7) + .5) / K2(I, I1, 6), WR(I, I1 + 10), K2(I, I1, 8)

        9733 Next I1
        For I1 = 0 To 9
            If K2(I, I1, 3) = 0 Then 9736
            Print #1, A1$(I, I1); Tab(19);
            Print #1, Using "##    ###   ##.#  ###    ##"; K2(I, I1, 3), Int(K2(I, I1, 4) + .5), Int(K2(I, I1, 4) + .5) / K2(I, I1, 3), WR(I, I1), K2(I, I1, 5)

        9736 Next I1
        Print #1,
    Next I
    Print #1, Tab(17); "PASSING"
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "ATT  COM COM %  YDS  Y/A  Y/C  TD  TD%  I    I%  LG"
        For I1 = 0 To 3
            If K2(I, I1, 9) = 0 Then 9734
            Print #1, A3$(I, I1); Tab(19);
            Print #1, Using "###  ### ###.#  ### ##.# "; K2(I, I1, 9), K2(I, I1, 10), K2(I, I1, 10) / K2(I, I1, 9) * 100, Int(K2(I, I1, 12) + .5), Int(K2(I, I1, 12) + .5) / K2(I, I1, 9);
            If K2(I, I1, 10) <> 0 Then Print #1, Using "##.#  ##"; Int(K2(I, I1, 12) + .5) / K2(I, I1, 10), K2(I, I1, 13); Else Print #1, Using "      ##"; K2(I, I1, 13);
            Print #1, Using "###.# ## ###.# ###"; K2(I, I1, 13) / K2(I, I1, 9) * 100, K2(I, I1, 11), K2(I, I1, 11) / K2(I, I1, 9) * 100, QB(I, I1)

        9734 Next I1
        Print #1,
    Next I
    Print #1, Tab(17); "KICKOFF RETURNS"
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "KR   YDS   AVG   LG  TD"
        For I1 = 0 To 2
            If K2(I, I1, 14) = 0 Then 9735
            Print #1, A4$(I, I1); Tab(19);
            Print #1, Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), KR(I, I1), K2(I, I1, 16)

        9735 Next I1
        Print #1,
    Next I
    Print #1,
    Print #1, Tab(17); "PUNT RETURNS"
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "PR   YDS   AVG   LG  TD"
        For I1 = 3 To 5
            If K2(I, I1, 14) = 0 Then 9737
            Print #1, A5$(I, I1 - 3); Tab(19);
            Print #1, Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), PR(I, I1 - 3), K2(I, I1, 16)

        9737 Next I1
        Print #1,
    Next I
    Print #1, Tab(17); "PUNTING"
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "PNT   AVG  LG  TB  BLK  20"
        If K(I, 7) = 0 Then 9738
        Print #1, A6$(I, 0); Tab(19);
        Print #1, Using "###  ##.#  ##  ##   ##  ##  ##  ##.#  ##.#"; K(I, 7), K(I, 8) / K(I, 7), K(I, 26), K(I, 27), K(I, 28), K(I, 29)

        9738 Print #1,
    Next I
    Print #1, Tab(17); "KICKING"
    For I = 0 To 1
        Print #1, A$(I); Tab(19); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
        If K2(I, 4, 17) = 0 And K2(I, 6, 17) = 0 Then 9739
        Print #1, A7$(I, 0); Tab(19);
        If K2(I, 6, 17) <> 0 Then Print #1, Using "### ### ###.# "; K2(I, 5, 17), K2(I, 6, 17), K2(I, 5, 17) / K2(I, 6, 17) * 100;
        If K2(I, 4, 17) = 0 Then Print #1,: GoTo 9739
        Print #1, Tab(33); Using "### ### ###.# ##  #"; K2(I, 3, 17), K2(I, 4, 17), K2(I, 3, 17) / K2(I, 4, 17) * 100, FL(I, 0), FM(I, 0, 0);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 0); FM(I, 0, 1);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 1); FM(I, 0, 2);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 2); FM(I, 0, 3);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 0, 3); FM(I, 0, 4);
        Print #1, "- ";: Print #1, Using "#"; FA(I, 0, 4)

        9739 If K2(I, 8, 17) = 0 And K2(I, 10, 17) = 0 Then 9740
        Print #1, A7$(I, 1); Tab(19);
        If K2(I, 10, 17) <> 0 Then Print #1, Using "### ### ###.# "; K2(I, 9, 17), K2(I, 10, 17), K2(I, 9, 17) / K2(I, 10, 17) * 100;
        If K2(I, 8, 17) = 0 Then Print #1,: GoTo 9740
        Print #1, Tab(33); Using "### ### ###.# ##  #"; K2(I, 7, 17), K2(I, 8, 17), K2(I, 7, 17) / K2(I, 8, 17) * 100, FL(I, 1), FM(I, 1, 0);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 0); FM(I, 1, 1);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 1); FM(I, 1, 2);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 2); FM(I, 1, 3);
        Print #1, "- ";: Print #1, Using "#  #"; FA(I, 1, 3); FM(I, 1, 4);
        Print #1, "- ";: Print #1, Using "#"; FA(I, 1, 4)

        9740 Print #1,
    Next I
    Print #1,: Print #1, "INTERCEPTIONS"
    For I = 0 To 1: Print #1, A$(I); Tab(16);
        Print #1, "IR   YDS   AVG   LG  TD"
        For I1 = 0 To 9
            If SI(I, I1, 0) = 0 Then GoTo PIX
            Print #1, DI$(I, I1); Tab(16);
            Print #1, Using "##  ####  ##.#  ###  ##"; SI(I, I1, 0), Int(SI(I, I1, 1) + .5), Int(SI(I, I1, 1) + .5) / SI(I, I1, 0), IR(I, I1), SI(I, I1, 2)
            PIX:
    Next I1: Print #1,: Next I
    Print #1, "SACKS"
    For I = 0 To 1: Print #1, A$(I); Tab(19);
        Print #1, "NO.   YDS LOST"
        For I1 = 0 To 14
            If SK(I, I1, 0) = 0 Then GoTo PSX
            Print #1, DS$(I, I1); Tab(19);
            Print #1, Using " ##       ##"; SK(I, I1, 0), Int(SK(I, I1, 1) + .5)
            PSX:
    Next I1: Locate , 1: Print #1,: Next I
    Print #1, Chr$(12)
    Close #1

End Sub


Sub BOX2SCREEN ()
    If H% = 2 Then Print "SITE: NEUTRAL  ATTENDANCE: "; ATT
    If H% <> 2 Then Print "SITE: "; SITE$;: Print Using "  ATTENDANCE: ####,###"; ATT
    Print
    For I = 0 To 1: Print A$(I);
        Locate , 20
        Print Using "## ## ## ## "; S(I, 1), S(I, 2), S(I, 3), S(I, 4);
        If Q > 4 Then
            For I1 = 5 To Q
                Print Using "## "; S(I, I1);
            Next
        End If
        Print "- ";
        Print Using "###"; S(I, 0)
    Next I
    Print
    If X2 = 0 Then 8526
    For I = 1 To X2
        Print A$(K1(I, 1)); "-";
        TI = T(I): TI = TI + TI * (TI < 0): IR% = Int(TI / 60)
        J = TI - IR% * 60
        Locate , 17
        Print Using "##.##"; IR% + J / 100;
        Locate , 19: Print ":";
        Locate , 23
        If C(I) = 1 Then Print "OF 1ST -";
        If C(I) = 2 Then Print "OF 2ND -";
        If C(I) = 3 Then Print "OF 3RD -";
        If C(I) = 4 Then Print "OF 4TH -";
        If C(I) = 5 Then Print "OF OVERTIME -";
        If K1(I, 0) <> 7 Then Print Int(K1(I, 5) + .5);
        On K1(I, 0) GOTO 8508, 8505, 8509, 8510, 8511, 8512, 8513, 8514, 8515, 8516, 8504, 8529

        8504 Print "FAKE PUNT ", K1(I, 2): GoTo 8517

        8505 Print "PASS BY "; A3$(K1(I, 1), K1(I, 2)); " TO ";: If K1(I, 3) > 9 Then Print A2$(K1(I, 1), K1(I, 3) - 10)
        If K1(I, 3) <= 9 Then Print A1$(K1(I, 1), K1(I, 3))
        GoTo 8517

        8508 Print "RUN BY "; A1$(K1(I, 1), K1(I, 2)): GoTo 8517

        8509 Print "K.O. RETURN BY "; A4$(K1(I, 1), K1(I, 2)): GoTo 8517

        8510 Print "PUNT RETURN BY "; A5$(K1(I, 1), K1(I, 2)): GoTo 8517

        8529 Print "2-PT DEFENSE RETURN BY "; DI$(K1(I, 1), K1(I, 2)): GoTo 8525

        8511 Print "INTERCEPTION RETURN BY "; DI$(K1(I, 1), K1(I, 2)): GoTo 8517

        8512 Print "BLOCKED KICK RECOVERED IN END ZONE": GoTo 8517

        8513 Print "SAFETY": GoTo 8525

        8514 Print "FG BY "; A7$(K1(I, 1), K1(I, 2)): GoTo 8525

        8515 Print "RUN BY "; A3$(K1(I, 1), K1(I, 2)): GoTo 8517

        8516 Print "FUMBLE RETURN ": GoTo 8517

        8517 Locate , 17
        If K1(I, 4) = 10 Then Print " (KICK FAILED)"
        If K1(I, 4) = 20 Then Print " (RUN FAILED)"
        If K1(I, 4) = 30 Then Print " (PASS FAILED)"
        If K1(I, 4) = 11 Then Print " (KICK BY "; A7$(K1(I, 1), K1(I, 6));: Print ")"
        If K1(I, 4) = 21 Then Print " (RUN GOOD)"
        If K1(I, 4) = 31 Then Print " (PASS GOOD)"

        8525 If I = 10 And K1(11, 0) <> 0 And X = 0 Then Call GETSPEED: Cls
        If I = 20 And K1(21, 0) <> 0 And X = 0 Then Call GETSPEED: Cls
        If I = 30 And K1(31, 0) <> 0 And X = 0 Then Call GETSPEED: Cls
        If I = 40 And K1(41, 0) <> 0 And X = 0 Then Call GETSPEED: Cls
    Next I

    8526
End Sub


Sub STATS2SCREEN ()
    J = 14: Call BOXDRAW(J): Locate , 32: Print "TEAM STATISTICS"
    Locate , 15: Color PA%(0, 2), PA%(0, 1): Print A$(0);
    Color 0, 7
    Locate , 50
    Color PA%(1, 2), PA%(1, 1): Print A$(1)
    Color 0, 7
    Locate , 20: Print K(0, 0);: Locate , 34: Print "FIRST DOWNS";: Locate , 55: Print K(1, 0)
    If G9 = 0 Then Locate , 18: Print K(0, 1); "-"; Int(K(0, 2) + .5);: Locate , 34: Print "RUSHING YDS";: Locate , 53: Print K(1, 1); "-"; Int(K(1, 2) + .5)
    If G9 <> 0 Then Locate , 18: Print K(0, 1) - W6(0, 0); "-"; Int(K(0, 2) + W6(0, 1) + .5);: Locate , 34: Print "RUSHING YDS";: Locate , 53: Print K(1, 1) - W6(1, 0); "-"; Int(K(1, 2) + W6(1, 1) + .5)
    Locate , 20: Print Int(K(0, 6) + .5);: Locate , 34: Print "PASSING YDS";: Locate , 55: Print Int(K(1, 6) + .5)
    Locate , 35: Print "TOTAL YDS";: Locate , 20: Print Int(K(0, 6) + .5) + Int(K(0, 2) + .5);: Locate , 55: Print Int(K(1, 6) + .5) + Int(K(1, 2) + .5)
    Locate , 37: Print "PASSES";: Locate , 16: Print K(0, 4); "-"; K(0, 3); "-"; K(0, 5);: Locate , 51: Print K(1, 4); "-"; K(1, 3); "-"; K(1, 5)
    If K(0, 7) = 0 Or K(1, 7) = 0 Then 8538
    Locate , 38: Print "PUNTS";: Locate , 15: Print K(0, 7); "-"; Int((K(0, 8) / K(0, 7)) * 10 + .5) / 10;: Locate , 50: Print K(1, 7); "-"; Int((K(1, 8) / K(1, 7)) * 10 + .5) / 10

    8538 Locate , 36: Print "FUM/LOST";: Locate , 18: Print K(0, 9); "-"; K(0, 10);: Locate , 53: Print K(1, 9); "-"; K(1, 10)
    Locate , 35: Print "PENALTIES";: Locate , 18: Print K(0, 11); "-"; Int(K(0, 12) + .5);: Locate , 53: Print K(1, 11); "-"; Int(K(1, 12) + .5)
    Locate , 37: Print "SACKED";: Locate , 18: Print W6(0, 0); "-"; Int(W6(0, 1) + .5);: Locate , 53: Print W6(1, 0); "-"; Int(W6(1, 1) + .5)
    Locate , 18: Print Using "##"; Int(TP(0) / 60);: Print ":";: Print Using "##"; TP(0) - Int(TP(0) / 60) * 60;
    Locate , 31: Print "TIME OF POSSESSION";: Locate , 53: Print Using "##"; Int(TP(1) / 60);: Print ":";: Print Using "##"; TP(1) - Int(TP(1) / 60) * 60
    Locate , 32: Print "THIRD DOWN EFF";: Locate , 16: Print TE%(0) - TF%(0); "-"; TE%(0);: Locate , 51: Print TE%(1) - TF%(1); "-"; TE%(1)
    Call DECIDE
    PCopy 0, 1
    'SCREEN,,0,0
    Rem
    Color 0, 7: Cls: Locate , 32: Print "RUSHING STATISTICS"
    Call ADDBACKS
    For I = 0 To 1
        Locate , 10
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 26
        Print "TC   YDS   AVG   LG   TD"
        For I1 = 0 To 13
            If K2(I, I1, 0) = 0 Then 8560
            Locate , 10
            If I1 <= 9 Then Print A1$(I, I1);
            If I1 > 9 Then Print A3$(I, I1 - 10);
            Locate , 25: Print Using "###  #### ###.#  "; K2(I, I1, 0), Int(K2(I, I1, 1) + .5), Int(K2(I, I1, 1) + .5) / K2(I, I1, 0);
            If I1 <= 9 Then Print Using "###   ##"; RB(I, I1), K2(I, I1, 2)
            If I1 > 9 Then Print Using "      ##"; K2(I, I1, 2)

        8560 Next I1
        Print
    Next I
    Call DECIDE
    PCopy 0, 1
    J = 22: Call DRAWABOX: Locate , 32: Print "RECEIVING STATISTICS"
    For I = 0 To 1
        Locate , 10
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 28: Print "TC    YDS    AVG   LG    TD"
        For I1 = 0 To 5
            If K2(I, I1, 6) = 0 Then 8570
            Locate , 10
            Print A2$(I, I1);
            Locate , 28: Print Using "##    ###   ##.#  ###    ##"; K2(I, I1, 6), Int(K2(I, I1, 7) + .5), Int(K2(I, I1, 7) + .5) / K2(I, I1, 6), WR(I, I1 + 10), K2(I, I1, 8)

        8570 Next I1
        For I1 = 0 To 9
            If K2(I, I1, 3) = 0 Then 8571
            Locate , 10
            Print A1$(I, I1);
            Locate , 28: Print Using "##    ###   ##.#  ###    ##"; K2(I, I1, 3), Int(K2(I, I1, 4) + .5), Int(K2(I, I1, 4) + .5) / K2(I, I1, 3), WR(I, I1), K2(I, I1, 5)

        8571 Next I1
        Print
    Next I
    Call DECIDE
    PCopy 0, 1
    J = 22: Call DRAWABOX: Locate , 32: Print "PASSING STATISTICS"
    For I = 0 To 1
        Locate , 10
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 26: Print "ATT  COM COM %  YDS  Y/A  Y/C  TD  TD%  I    I%  LG"
        For I1 = 0 To 3
            If K2(I, I1, 9) = 0 Then 8580
            Locate , 10
            Print A3$(I, I1);
            Locate , 26: Print Using "###  ### ###.#  ### ##.# "; K2(I, I1, 9), K2(I, I1, 10), K2(I, I1, 10) / K2(I, I1, 9) * 100, Int(K2(I, I1, 12) + .5), Int(K2(I, I1, 12) + .5) / K2(I, I1, 9);
            If K2(I, I1, 10) <> 0 Then Print Using "##.#  ##"; Int(K2(I, I1, 12) + .5) / K2(I, I1, 10), K2(I, I1, 13); Else Print Using "      ##"; K2(I, I1, 13);
            Print Using "###.# ## ###.# ###"; K2(I, I1, 13) / K2(I, I1, 9) * 100, K2(I, I1, 11), K2(I, I1, 11) / K2(I, I1, 9) * 100, QB(I, I1)

        8580 Next I1
        Print
    Next I
    Call DECIDE
    J = 22: Call DRAWABOX: Locate , 25: Print "KICKOFF RETURN STATISTICS"
    For I = 0 To 1
        Locate , 10
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 26: Print "KR   YDS   AVG   LG  TD"
        For I1 = 0 To 2
            If K2(I, I1, 14) = 0 Then 8590
            Locate , 10
            Print A4$(I, I1);
            Locate , 26
            Print Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), KR(I, I1), K2(I, I1, 16)

        8590 Next I1
        Print
    Next I
    Print
    Locate , 25: Print "PUNT RETURN STATISTICS"
    For I = 0 To 1
        Locate , 10
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 26: Print "PR   YDS   AVG   LG  TD"
        For I1 = 3 To 5
            If K2(I, I1, 14) = 0 Then 8591
            Locate , 10
            Print A5$(I, I1 - 3);
            Locate , 26
            Print Using "##  ####  ##.#  ###  ##"; K2(I, I1, 14), Int(K2(I, I1, 15) + .5), Int(K2(I, I1, 15) + .5) / K2(I, I1, 14), PR(I, I1 - 3), K2(I, I1, 16)

        8591 Next I1
        Print
    Next I
    Call DECIDE
    Screen , , 0, 0
    J = 22: Call DRAWABOX: Locate , 29: Print "PUNTING STATISTICS": Print
    For I = 0 To 1
        Locate , 10
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 26: Print "PNT   AVG  LG  TB  BLK  20"
        If K(I, 7) = 0 Then 8592
        Locate , 10
        Print A6$(I, 0);
        Locate , 26
        Print Using "###  ##.#  ##  ##   ##  ##  ##  ##.#  ##.#"; K(I, 7), K(I, 8) / K(I, 7), K(I, 26), K(I, 27), K(I, 28), K(I, 29)

        8592 Print
    Next I
    Print
    Locate , 29: Print "KICKING STATISTICS": Print
    For I = 0 To 1
        Locate , 2
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 18: Print "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
        If K2(I, 4, 17) = 0 And K2(I, 6, 17) = 0 Then 8593
        Locate , 2
        Print A7$(I, 0);
        Locate , 18
        If K2(I, 6, 17) <> 0 Then Print Using "### ### ###.# "; K2(I, 5, 17), K2(I, 6, 17), K2(I, 5, 17) / K2(I, 6, 17) * 100;
        If K2(I, 4, 17) = 0 Then Print: GoTo 8593
        Locate , 32
        Print Using "### ### ###.# ##  #"; K2(I, 3, 17), K2(I, 4, 17), K2(I, 3, 17) / K2(I, 4, 17) * 100, FL(I, 0), FM(I, 0, 0);
        Print "- ";: Print Using "#  #"; FA(I, 0, 0); FM(I, 0, 1);
        Print "- ";: Print Using "#  #"; FA(I, 0, 1); FM(I, 0, 2);
        Print "- ";: Print Using "#  #"; FA(I, 0, 2); FM(I, 0, 3);
        Print "- ";: Print Using "#  #"; FA(I, 0, 3); FM(I, 0, 4);
        Print "- ";: Print Using "#"; FA(I, 0, 4)

        8593 If K2(I, 8, 17) = 0 And K2(I, 10, 17) = 0 Then 8594
        Locate , 2
        Print A7$(I, 1);
        Locate , 18
        If K2(I, 10, 17) <> 0 Then Print Using "### ### ###.# "; K2(I, 9, 17), K2(I, 10, 17), K2(I, 9, 17) / K2(I, 10, 17) * 100;
        If K2(I, 8, 17) = 0 Then Print: GoTo 8594
        Locate , 32
        Print Using "### ### ###.# ##  #"; K2(I, 7, 17), K2(I, 8, 17), K2(I, 7, 17) / K2(I, 8, 17) * 100, FL(I, 1), FM(I, 1, 0);
        Print "- ";: Print Using "#  #"; FA(I, 1, 0); FM(I, 1, 1);
        Print "- ";: Print Using "#  #"; FA(I, 1, 1); FM(I, 1, 2);
        Print "- ";: Print Using "#  #"; FA(I, 1, 2); FM(I, 1, 3);
        Print "- ";: Print Using "#  #"; FA(I, 1, 3); FM(I, 1, 4);
        Print "- ";: Print Using "#"; FA(I, 1, 4)

        8594 Print
    Next I
    Call DECIDE
    Screen , , 0, 0
    J = 22: Call DRAWABOX: Locate , 35: Print "INTERCEPTIONS": Print
    For I = 0 To 1: Locate , 19
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7: Locate , 35
        Print "IR   YDS   AVG   LG  TD"
        For I1 = 0 To 9
            If SI(I, I1, 0) = 0 Then GoTo IX
            Locate , 19: Print DI$(I, I1);
            Locate , 35: Print Using "##  ####  ##.#  ###  ##"; SI(I, I1, 0), Int(SI(I, I1, 1) + .5), Int(SI(I, I1, 1) + .5) / SI(I, I1, 0), IR(I, I1), SI(I, I1, 2)
            IX:
    Next I1: Locate , 1: Print: Next I
    Call DECIDE
    Screen , , 0, 0
    J = 22: Call DRAWABOX: Locate , 39: Print "SACKS": Print
    For I = 0 To 1
        Locate , 19
        Color PA%(I, 2), PA%(I, 1): Print A$(I);: Color 0, 7
        Locate , 35
        Print "NO.   YDS LOST"
        For I1 = 0 To 14
            If SK(I, I1, 0) = 0 Then GoTo SX
            Locate , 19: Print DS$(I, I1);
            Locate , 35: Print Using " ##       ##"; SK(I, I1, 0), Int(SK(I, I1, 1) + .5)
            SX:
    Next I1: Locate , 1: Print: Next I
    Call DECIDE
End Sub


Sub INCOMPLETE ()
    Call QBTHROW

    4135 RN = Int(Rnd(1) * 100) + 1
    If RN <= 10 Then 4137
    RN = Int(Rnd(1) * 100) + 1
    If RN > 60 Then 4136
    RN = Int(Rnd(1) * 26) + 1
    If RN = 1 Then U$ = RV$ + " DROPS A PASS RIGHT TO HIM"
    If RN = 2 Then U$ = RV$ + " IS HIT AND CAN'T HANG ON....INCOMPLETE PASS"
    If RN = 3 Then U$ = RV$ + " BROKE THE WRONG WAY...INCOMPLETE"
    If RN = 4 Then U$ = RV$ + " DROPS IT!"
    If RN = 5 Then U$ = "DROPPED BY " + RV$
    If RN = 6 Then U$ = "BUT " + RV$ + " CAN'T REACH IT"
    If RN = 7 Then U$ = RV$ + " CAN'T HAUL IT IN...INCOMPLETE"
    If RN = 8 Then U$ = RV$ + " MISHANDLES IT....INCOMPLETE"
    If RN = 9 Then U$ = RV$ + " JUGGLES IT....INCOMPLETE"
    If RN = 10 Then U$ = RV$ + " CAN'T FIND THE HANDLE....INCOMPLETE"
    If RN = 11 Then U$ = RV$ + " CAN'T PULL IT IN....INCOMPLETE"
    If RN = 12 Then U$ = RV$ + " BOBBLES IT....INCOMPLETE"
    If RN = 13 Then U$ = RV$ + " WAS OUT OF BOUNDS....INCOMPLETE"
    If RN = 14 Then U$ = RV$ + " CAME DOWN OUT OF BOUNDS....INCOMPLETE"
    If RN = 15 Then U$ = "THE PASS IS BROKEN UP"
    If RN = 16 Then U$ = "BUT " + RV$ + " CAN'T HANDLE THE PASS"
    If RN = 17 Then U$ = "BUT THE PASS IS OVERTHROWN"
    If RN = 18 Then U$ = "BUT " + RV$ + " CAN'T GET TO THE BALL...INCOMPLETE"
    If RN = 19 Then U$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE"
    If RN = 20 Then U$ = "BUT " + RV$ + " IS COVERED...INCOMPLETE"
    If RN = 21 Then U$ = "BUT THE PASS FALLS INCOMPLETE"
    If RN = 22 Then U$ = "BUT THE PASS IS THROWN BEHIND " + RV$ + "...INCOMPLETE"
    If RN = 23 Then U$ = RV$ + " CAN'T HANDLE THE PASS...INCOMPLETE"
    If RN = 24 Then U$ = "BUT THE PASS IS BATTED DOWN AT THE LINE OF SCRIMMAGE"
    If RN = 25 Then U$ = "BUT THE PASS TO " + RV$ + " IS BROKEN UP"
    If RN = 26 Then U$ = "THE PASS IS BROKEN UP AND ALMOST PICKED OFF!!"
    GoTo 4145

    4137 '***PASS BROKEN UP ***
    RN = Int(Rnd(1) * 6) + 1
    If RN = 1 Then U$ = "PASS IS BROKEN UP"
    If RN = 2 Then U$ = "PASS IS TIPPED AWAY"
    If RN = 3 Then U$ = "PASS IS KNOCKED AWAY"
    If RN = 4 Then U$ = "PASS IS KNOCKED DOWN"
    If RN = 5 Then U$ = "PASS IS DEFLECTED"
    If RN = 6 Then U$ = "PASS IS BATTED AWAY"
    If intDI%(1 - P1) > 0 Then Call FINDINT: U$ = U$ + " BY " + X$
    GoTo 4145

    4136 RN = Int(Rnd(1) * 100) + 1
    If P2 = 13 And RN <= 40 Then U$ = "BUT THE SCREEN FALLS INCOMPLETE": GoTo 4145
    If P2 = 13 And RN <= 55 Then U$ = "BUT THE SCREEN IS BROKEN UP": GoTo 4145
    If P2 = 13 And RN <= 60 Then U$ = "BUT " + RV$ + " CAN'T HANDLE THE PASS": GoTo 4145
    If P2 = 13 Then U$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE": GoTo 4145
    If P2 = 12 And RN <= 15 Then U$ = "BUT THE PASS IS BROKEN UP": GoTo 4145
    If P2 = 12 And RN <= 44 Then U$ = "BUT THE PASS IS OVERTHROWN": GoTo 4145
    If P2 = 12 And RN <= 74 Then U$ = "BUT " + RV$ + " CAN'T GET TO THE BALL...INCOMPLETE": GoTo 4145
    If P2 = 12 And RN <= 80 Then U$ = "PASS IS ALMOST PICKED OFF!!...INCOMPLETE": GoTo 4145
    If P2 = 12 Then U$ = "BUT " + RV$ + " IS COVERED...INCOMPLETE": GoTo 4145
    If RN <= 23 Then U$ = "BUT THE PASS FALLS INCOMPLETE": GoTo 4145
    If RN <= 47 Then U$ = "BUT THE PASS IS THROWN BEHIND " + RV$ + "...INCOMPLETE": GoTo 4145
    If RN <= 70 Then U$ = "BUT THE PASS IS OVERTHROWN AND INCOMPLETE": GoTo 4145
    If RN <= 75 Then U$ = RV$ + " CAN'T HANDLE THE PASS...INCOMPLETE": GoTo 4145
    If RN <= 81 Then U$ = "BUT THE PASS IS BATTED DOWN AT THE LINE OF SCRIMMAGE": GoTo 4145
    If RN <= 96 Then U$ = "BUT THE PASS TO " + RV$ + " IS BROKEN UP": GoTo 4145
    U$ = "THE PASS IS BROKEN UP AND ALMOST PICKED OFF!!"

    4145 Call PBP
End Sub


Sub QBSNEAK ()
    '*** QB SNEAK ***
    U$ = A3$(P1, Q6%(P1)) + " KEEPS IT ON THE SNEAK": Call PBP
    I4 = Int(Rnd(1) * 100) + 1
    If (D3 = 11 Or D3 = 7 Or D3 = 9) And I4 <= 20 Then 4676
    If (D3 = 11 Or D3 = 7 Or D3 = 9) Then 4677
    If D2 = 11 And I4 <= 40 Then 4676
    If D2 = 11 Then 4677
    If (D2 = 1 Or D2 = 2) And I4 <= 10 Then 4676
    If (D2 = 1 Or D2 = 2) And I4 <= 80 Then 4677
    If D2 = 1 Or D2 = 2 Then 4678
    If (D2 = 3 Or D2 = 4) And I4 <= 30 Then 4676
    If D2 = 3 Or D2 = 4 Then 4677
    If (D2 = 5 Or D2 = 6) And I4 <= 15 Then 4676
    If D2 = 5 Or D2 = 6 Then 4677
    If (D2 >= 7 And D2 <= 10) And I4 <= 10 Then 4676
    If (D2 >= 7 And D2 <= 10) And I4 <= 80 Then 4677
    If D2 >= 7 And D2 <= 10 Then 4678

    4676 G = (Int(Rnd(1) * 6) - 3) / 10: GoTo 4680

    4677 G = (Int(Rnd(1) * 7) + 3) / 10: GoTo 4680

    4678 G = Int(Rnd(1) * 2) + 1

    4680 U$ = "HE GAINS": I1 = QB: X = 4: X1 = 9: R1 = 1
    If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then U$ = U$ + Str$(Int(100 - Y))
    If Int((G + Y) * 10 + .5) / 10 < 99.94999 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10)
    U$ = U$ + " YARDS ON THE PLAY": Call PBP
End Sub


Sub OVERUSE ()
    If K(P1, 1) + K(P1, 3) = 0 Then 4050
    If (K(P1, 1) / (K(P1, 1) + K(P1, 3))) < (B6(P1, 7) / 100) And K3(P1, 3) >= 18 Then I4 = I4 * .65
    If I4 <= 0 Then I4 = 1

    4050 '
End Sub


Sub BLITZ ()
    I5 = Int(Rnd(1) * 100) + 1: XE = YC(D) / 100
    If XE > .5 Then 4706
    XE = Int(XE * 100 + .5)
    If XE > 21 Then XE = 21
    If XE < 1 Then XE = 1
    If I5 <= PS%(1, XE) Then I4 = I4 + PS%(2, XE)
    GoTo 4709

    4706 XE = Int((1 - XE) * 100 + .5)
    If XE > 21 Then XE = 21
    If XE < 1 Then XE = 1
    If I5 <= PS%(1, XE) Then I4 = I4 - PS%(2, XE)

    4709 RN = Int(Rnd(1) * 100) + 1
    If QX%(P1, Q6%(P1)) > 20 Then QX%(P1, Q6%(P1)) = 20
    If QX%(P1, Q6%(P1)) < -20 Then QX%(P1, Q6%(P1)) = -20
    QX = QX%(P1, Q6%(P1))
    If QX = 0 Then 4710
    If QX < 0 Then QX = QX * -1
    If RN <= QR%(QX, 1) And QX%(P1, Q6%(P1)) > 0 Then I4 = I4 + QR%(QX, 2): GoTo 4710
    If RN <= QR%(QX, 1) Then I4 = I4 - QR%(QX, 2): GoTo 4710

    4710 Call OVERUSE: I7 = 4
    If P2 = 9 Then I7 = 1
    If P2 = 13 Then I7 = 2
    If P2 = 10 Then I7 = 3
    If P2 = 12 Or P2 = 15 Then I7 = 5
    If P2 = 10 And (D3 = 9 Or D3 = 10) Then I8 = 21: GoTo 4750
    If P2 = 10 And D3 = 11 Then I8 = 25: GoTo 4750
    If P2 = 11 And (D3 = 7 Or D3 = 8) Then I8 = 21: GoTo 4750
    If P2 = 11 And (D3 = 9 Or D3 = 10) Then I8 = 25: GoTo 4750
    If P2 = 11 And D3 = 11 Then I8 = 28: GoTo 4750
    If P2 = 12 And (D3 = 7 Or D3 = 8) Then I8 = 33: GoTo 4750
    If P2 = 12 And D3 >= 9 Then I8 = 42: GoTo 4750
    If P2 = 13 And (D3 = 9 Or D3 = 10) Then I8 = 25: GoTo 4750
    If P2 = 13 And D3 = 11 Then I8 = 36: GoTo 4750
    If P2 = 14 And (D3 = 7 Or D3 = 8) Then I8 = 25: GoTo 4750
    If P2 = 14 And (D3 = 9 Or D3 = 10) Then I8 = 28: GoTo 4750
    If P2 = 14 And D3 = 11 Then I8 = 31: GoTo 4750
    If P2 = 15 And D3 >= 7 Then I8 = 42: GoTo 4750
    If P2 = 16 And (D3 = 7 Or D3 = 8) Then I8 = 18: GoTo 4750
    If P2 = 16 And (D3 = 9 Or D3 = 10) Then I8 = 28: GoTo 4750
    If P2 = 16 And D3 = 11 Then I8 = 33: GoTo 4750
    If (P2 = 17 Or P2 = 18) And (D3 = 7 Or D3 = 9) Then I8 = 21: GoTo 4750
    If (P2 = 17 Or P2 = 18) And D3 = 8 Then I8 = 25: GoTo 4750
    If (P2 = 17 Or P2 = 18) And D3 >= 10 Then I8 = 28: GoTo 4750
    I4 = I4 + S1%(2, P2 - 8, D3)
    If I4 > 10 Then I4 = 10
    If I4 < 1 Then I4 = 1
    I9 = Int(Rnd(1) * 100) + 1: For I = 1 To 14
        If I9 <= S2%(I7, I4, I) Then 4725
    Next I

    4725 If I = 1 Then G = Int(Rnd(1) * 5) - 4
    If I = 2 Then G = Int(Rnd(1) * 3) + 1
    If I = 3 Then G = Int(Rnd(1) * 4) + 4
    If I = 4 Then G = Int(Rnd(1) * 8) + 8
    If I = 5 Then G = Int(Rnd(1) * 5) + 16
    If I = 6 Then G = Int(Rnd(1) * 5) + 21
    If I = 7 Then G = Int(Rnd(1) * 5) + 26
    If I = 8 Then G = Int(Rnd(1) * 11) + 30
    If I = 9 Then G = Int(Rnd(1) * 11) + 40
    If I = 10 Then G = Int(Rnd(1) * 11) + 50
    If I = 11 Then G = Int(Rnd(1) * 11) + 60
    If I = 12 Then G = Int(Rnd(1) * 11) + 70
    If I = 13 Then G = Int(Rnd(1) * 11) + 80
    If I = 14 Then G = Int(Rnd(1) * 11) + 90
    GoTo 4700

    4750 '*** BLITZ ***
    If I8 = 18 Then G = Int(Rnd(1) * 21) + 10
    If I8 = 21 Then G = Int(Rnd(1) * 31) + 10
    If I8 = 25 Then G = Int(Rnd(1) * 41) + 10
    If I8 = 28 Then G = Int(Rnd(1) * 51) + 10
    If I8 = 31 Then G = Int(Rnd(1) * 41) + 20
    If I8 = 33 Then G = Int(Rnd(1) * 71) + 10
    If I8 = 36 Then G = Int(Rnd(1) * 91) + 10
    If I8 = 42 Then G = Int(Rnd(1) * 71) + 30
    G = G + (I4 - 4) * 3

    4700 '
End Sub


Sub BLITZ2 ()
    '*** BLITZ ***
    I4 = Int(Rnd(1) * 100) + 1
    If I2 > 15 Then 4307
    If I4 <= 15 Then G = Int(Rnd(1) * 5) - 4
    If I4 > 15 And I4 <= 60 Then G = Int(Rnd(1) * 2) + 1
    If I4 > 60 Then G = Int(Rnd(1) * (I2 * 2.22)) + 1
    GoTo 4323

    4307 If I2 > 17 Then 4312
    If I4 <= 10 Then G = Int(Rnd(1) * 5) - 4
    If I4 > 10 And I4 <= 50 Then G = Int(Rnd(1) * 2) + 1
    If I4 > 50 Then G = Int(Rnd(1) * (I2 * 1.78)) + 1
    GoTo 4323

    4312 If I2 > 19 Then 4317
    If I4 <= 10 Then G = Int(Rnd(1) * 5) - 4
    If I4 > 10 And I4 <= 40 Then G = Int(Rnd(1) * 2) + 1
    If I4 > 40 Then G = Int(Rnd(1) * (I2 * 1.54)) + 1
    GoTo 4323

    4317 If I2 > 21 Then 4322
    If I4 <= 5 Then G = Int(Rnd(1) * 5) - 4
    If I4 > 5 And I4 <= 30 Then G = Int(Rnd(1) * 2) + 1
    If I4 > 30 Then G = Int(Rnd(1) * (I2 * 1.32)) + 1
    GoTo 4323

    4322 If I4 <= 5 Then G = Int(Rnd(1) * 5) - 4
    If I4 > 5 And I4 <= 20 Then G = Int(Rnd(1) * 2) + 1
    If I4 > 20 Then G = Int(Rnd(1) * (I2 * 1.2)) + 1

    4323 '
End Sub


Sub COMPCHEAT ()

    1100 R0 = Int(Rnd(1) * 4) + 1: D3 = 1: I3 = Int(Rnd(1) * 100) + 1: If R0 <> 2 Then 1111
    If (P2 = 1 Or P2 = 2 Or P2 = 6 Or P2 = 8) And I3 <= 50 Then D2 = 3
    If (P2 = 1 Or P2 = 2 Or P2 = 6 Or P2 = 8) And I3 > 50 Then D2 = 4
    If P2 >= 3 And P2 <= 5 Or P2 = 7 Then D2 = 2
    If P2 = 9 Or P2 = 10 Or P2 = 13 Then D3 = 3
    If P2 = 11 Or P2 = 14 And I3 <= 50 Then D3 = 3
    If P2 = 11 Or P2 = 14 And I3 > 50 Then D3 = 5
    If P2 = 12 Or P2 = 15 Then D3 = 1
    If P2 = 16 Then D3 = 2
    If P2 >= 17 Then D3 = 3

    1111 '
End Sub


Sub PBPINT ()
    U$ = ""
    If intDS%(D) = 0 Then 685
    M5 = 0
    RN = Int(Rnd(1) * DDS(D)) + 1
    For Z1 = 0 To 14
        M5 = M5 + DS(D, Z1)
        If RN <= M5 Then U$ = DS$(D, Z1) + " OF ": GoTo 685
    Next

    685 Rem
End Sub


Sub ADDTIME ()
    If T <= 0 Then TP(P1) = TP(P1) + T: T = T - T
End Sub


Sub DEFENSE ()
    '*** COMPUTER DEFENSE ***
    If Q = 2 And T <= 120 Then 8895
    If Q >= 4 And T <= 120 Then 8895
    If D1 = 4 And Q = 4 And Y >= 68 And S(D, 0) - S(P1, 0) > 10 Then 8860
    If D1 = 4 And Q >= 3 And Y >= 68 And S(D, 0) - S(P1, 0) > 21 Then 8860
    If D1 = 1 And Y1 > 10 Then 8860
    If D1 = 1 And Y1 < 10 Then 8867
    If D1 = 2 And Y1 > 8 Then 8860
    If D1 = 2 And Y1 < 5 Then 8867
    If D1 = 3 And Y1 >= 3 Then 8860
    If D1 = 3 And Y1 < 3 Then 8867
    If D1 = 4 And Y1 >= 3 Then 8860
    If D1 = 4 And Y1 < 3 Then 8867
    R0 = Int(Rnd(1) * 100) + 1
    If R0 <= B6(P1, 7) Then 8867

    8860 R0 = Int(Rnd(1) * 100) + 1
    D2 = 1: D3 = 2
    If R0 <= 95 Then D3 = 1
    If R0 <= 90 Then D2 = 2
    If R0 <= 55 Then D2 = 1: D3 = 5
    If R0 <= 20 Then D2 = 10: D3 = 3
    If R0 <= 5 Then D2 = 9: GoTo 8960

    8865 If Y >= 90 And D3 = 5 Or Y1 < 7 And D3 = 5 Then D3 = 3
    GoTo 8866

    8895 If S(P1, 0) - S(D, 0) > 0 Then 8899
    R0 = Int(Rnd(1) * 100) + 1: D2 = 3: If R0 <= 50 Then D2 = Int(Rnd(1) * 3) + 4
    R0 = Int(Rnd(1) * 100) + 1
    If R0 <= 33 Then D3 = 1
    If R0 > 33 And R0 <= 67 Then D3 = 2
    If R0 > 67 Then D3 = 5
    GoTo 8866

    8899 R0 = Int(Rnd(1) * 100) + 1: RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 Then D2 = 1
    If RN > 50 And RN <= 70 Then D2 = 2
    If RN > 70 And RN <= 94 Then D2 = 3
    If RN = 95 Then D2 = 9: D3 = 10: GoTo 8866
    If RN = 96 Then D2 = 10: D3 = 9: GoTo 8866
    If RN > 96 And RN <= 98 Then D2 = 9: GoTo 8866
    If RN > 98 Then D2 = 10: D3 = 7: GoTo 8866
    If R0 <= 90 Then D3 = 1
    If R0 > 90 Then D3 = 2
    GoTo 8866

    8867 R0 = Int(Rnd(1) * 100) + 1
    D2 = 1: D3 = 1
    If R0 <= 50 Then D2 = 2: D3 = 3
    If R0 <= 5 Then D2 = 4
    GoTo 8866

    8960 RN = Int(Rnd(1) * 100) + 1
    If RN <= 45 Then D3 = 7: GoTo 8865
    If RN <= 90 Then D3 = 8: GoTo 8865
    If RN <= 95 Then D3 = 9: GoTo 8865
    D3 = 10
    GoTo 8865

    8866 Rem
End Sub


Sub MARKER ()
    If P1 Then J = FNP(YT) - 3 Else J = FNP(100 - YT) - 3
    Locate 13, 4: Color 15 * (1 - BW), 2 + 5 * BW
    If J < WS Then 2110 Else If J > WE Then 2115 Else GoTo 2125

    2110 Print Left$(F$, J - 1);: Color 0: Print Chr$(16 + P1);: Color 15 * (1 - BW)
    Print Mid$(F$, J + 1, WS - J - 1);: Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(F$, WS, 7);
    Color 15 * (1 - BW), 2 + 5 * BW: Print Right$(F$, 67 - WS);
    GoTo 2126

    2115 Print Left$(F$, WS - 1);: Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(F$, WS, 7);
    Color 15 * (1 - BW), 2 + 5 * BW: Print Mid$(F$, WE + 1, J - WE - 1);: Color 0
    Print Chr$(16 + P1);
    Color 15 * (1 - BW), 2 + 5 * BW
    Print Right$(F$, 73 - J);
    GoTo 2126

    2125 Print Left$(F$, WS - 1);: Color 2 + 5 * BW, 7 * (1 - BW): Print Mid$(F$, WS, J - WS);
    Color BW * 15: Print Chr$(16 + P1);: Color 2 + 5 * BW, 7 * (1 - BW)
    Print Mid$(F$, J + 1, WE - J);: Color 15 * (1 - BW), 2 + 5 * BW
    Print Right$(F$, 73 - WE);

    2126 Rem
End Sub


Sub MARKER1 ()
    YT = 100 - Y: YF = -Y1
    If P1 Then WS = FNP(YT + YF) - 3: WE = WS + 6 Else WE = FNP(100 - YT - YF) - 3: WS = WE - 6
    Call MARKER
End Sub


Sub DBOX ()
    PCopy 0, 1: Screen , , 1, 1: SN = 1: Locate 8, 8: Color PA%(D, 2), PA%(D, 1)
    Print " Ö"; String$(61, 196); "· ";: For II = 1 To J: Locate 8 + II, 8
    Print " º"; String$(61, 32); "º ";: Next
    Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15
End Sub


Sub PBOX ()
    PCopy 0, 1: Screen , , 1, 1: SN = 1: Locate 8, 8: Color PA%(P1, 2), PA%(P1, 1)
    Print " Ö"; String$(61, 196); "· ";: For II = 1 To J: Locate 8 + II, 8
    Print " º"; String$(61, 32); "º ";: Next
    Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15
End Sub


Sub MOVEBALL ()
    If N7 = 0 Then 2155
    I$ = "He's at "
    If EY > SY Then 2151
    For YT = SY To EY Step -1
        Call MARKER: U! = Timer: While Timer < U! + N7 / 20: Wend
        Call YARDLINE
        intYL% = YL
        If YT <= 0 Then YT = EY Else If Int(intYL% / 10) * 10 = intYL% Then U$ = I$ + "the" + Str$(intYL%) + " yard line": Call PLACEPBP: I$ = ""
    Next YT
    GoTo 2154

    2151 For YT = SY To EY
        Call MARKER: U! = Timer: While Timer < U! + N7 / 20: Wend
        Call YARDLINE
        intYL% = YL
        If YT <= 0 Then YT = EY Else If Int(intYL% / 10) * 10 = intYL% Then U$ = I$ + "the" + Str$(intYL%) + " yard line": Call PLACEPBP: I$ = ""
    Next YT

    2154 If YT <= 0 Then YT = 0: GoTo 2156 Else YT = YT + 1
    Call YARDLINE
    RN = Int(Rnd(1) * 21) + 1
    If RN = 1 Then I$ = " IS TACKLED"
    If RN = 2 Then I$ = " GOES OUT OF BOUNDS"
    If RN = 3 Then I$ = " IS HIT"
    If RN = 4 Then I$ = " GOES DOWN"
    If RN = 4 Then I$ = " IS GANG TACKLED"
    If RN = 5 Then I$ = " IS STOPPED"
    If RN = 6 Then I$ = " IS TRIPPED UP"
    If RN = 7 Then I$ = " IS WRAPPED UP"
    If RN = 8 Then I$ = " IS RIDDEN DOWN"
    If RN = 9 Then I$ = " IS LEVELED"
    If RN = 10 Then I$ = " IS PULLED DOWN"
    If RN = 11 Then I$ = " IS HAULED DOWN"
    If RN = 12 Then I$ = " IS FORCED OUT"
    If RN = 13 Then I$ = " IS SNOWED UNDER"
    If RN = 14 Then I$ = " IS THROWN DOWN"
    If RN = 15 Then I$ = " IS POPPED"
    If RN = 16 Then I$ = " IS SMACKED"
    If RN = 17 Then I$ = " IS SLAMMED"
    If RN = 18 Then I$ = " IS STOPPED"
    If RN = 19 Then I$ = " IS POPPED"
    If RN = 19 Then I$ = " STEPS OUT"
    If RN = 20 Then I$ = " IS PUSHED OUT"
    If RN = 21 Then I$ = " IS KNOCKED OUT OF BOUNDS"
    If YT >= 100 Then 2165
    If YT <= 0 Then 2156
    Call MARKER
    U$ = "AND HE" + I$ + " AT THE" + Str$(Int(intYL%)) + " YARD LINE"
    GoTo 2155

    2165 U$ = "The ball carrier" + I$ + " for a" + Str$(Int(-Y)) + " yard loss"

    2155 Call PBP

    2156 Rem
End Sub


Sub SCOREBOARD ()
    Screen , , 0, 0
    If Screen(2, 1) <> 219 Then Call SETSCREEN: Call MARKER1: GoTo 1980
    YF = -Y1: YT = 100 - Y
    Color 15 * (1 - BW), 1 + BW * 6: Locate 4 + P1, 3
    Print "*";: Locate 5 - P1, 3: Print " ";
    Call YARDLINE
    Color 15 * (1 - BW), 4 + BW * 3
    Color 15 * (1 - BW), 4 + BW * 3: TI = T: TI = TI + TI * (TI < 0): I = Int(TI / 60)
    Call TICKUPDATE
    J = TI - I * 60
    Locate 3, 44: Print Using " TIME: ##.##     QUARTER: # "; I + J / 100, Q;
    Locate 3, 53: Print ":";: Locate , 72: Print Tab(80); " ";
    Locate 4, 44: Print Using " DOWN: #"; D1;: Print Tab(60);
    Print Using " TO GO: ##.#"; Y1;: Print Tab(80); " ";
    Locate 5, 44: Print " BALL ON ";: If Y <= 50 Then Print "OWN";
    If Y > 50 Then Print "OPP";
    Print Using " ##.# YD LINE "; YL;: Print Tab(80); " ";

    1980 Rem
End Sub


Sub DEFMENU ()
    If J6(0) = 1 And D = CT Or J6(0) = 2 Then Call DEFENSE: Call DEFGOALLINE: GoTo 3055

    3056 J = 10: Call DBOX: Print "SELECT DEFENSIVE LINE PLAY "; A$(D)
    For D2 = 1 To 5: Locate 11 + D2, 14
        Print D2 - 1; "- "; D2$(D2); Tab(41); D2 + 4; "- "; D2$(D2 + 5)
    Next D2
    Print
    Locate , 28: Print " G - GOAL LINE (5 YL AND IN)"

    3057 I$ = GetKeyPress$
    If UCase$(I$) = "Q" Then Cls: End
    If UCase$(I$) = "G" And Y >= 94.999 Then D2 = 11: Screen , , 0, 0: GoTo 3055
    If UCase$(I$) = "G" Then 3057
    D2 = Val(I$): If D2 < 0 Or D2 > 9 Then 3057
    D2 = D2 + 1: Screen , , 0, 0
    If D2 = 5 Or D2 = 6 Then D2 = D2 + 4: GoTo 3061
    If D2 > 6 And D2 < 11 Then D2 = D2 - 2

    3061 J = 10: Call DBOX: Print "SELECT PASS COVERAGE "; A$(D)
    For D3 = 1 To 5: Locate 11 + D3, 14
        Print D3 - 1; "- "; D1$(D3); Tab(41); D3 + 4; "- "; D1$(D3 + 5)
    Next D3
    Locate , 41: Print " B - BLITZ ENGAGE"

    3062 I$ = GetKeyPress$
    If I$ = Chr$(27) Then Screen , , 0, 0: GoTo 3056
    If UCase$(I$) = "B" Then D3 = 10: GoTo 3065
    D3 = Val(I$): If D3 < 0 Or D3 > 9 Then 3062

    3065 D3 = D3 + 1: Screen , , 0, 0
    If (D2 = 5 Or D2 = 6) And (D3 = 7 Or D3 = 9 Or D3 = 11) Then 3056
    If (D2 = 7 Or D2 = 8) And (D3 = 8 Or D3 = 10 Or D3 = 11) Then 3056

    3055 '
End Sub


Sub DEFGOALLINE ()
    If Y > 94.999 Then D2 = 11: D3 = 1
End Sub


Sub LGPBP ()
    '*** PBP TO START LONG GAIN ***
    RN = Int(Rnd(1) * 20) + 1
    If RN = 1 Then U$ = "HE BREAKS FREE"
    If RN = 2 Then U$ = "HE DODGES A TACKLER"
    If RN = 3 Then U$ = "HE STIFF ARMS A TACKLER"
    If RN = 4 Then U$ = "HE HEADS DOWN THE SIDELINE"
    If RN = 5 Then U$ = "HE SEES DAYLIGHT"
    If RN = 6 Then U$ = "HE CUTS BACK"
    If RN = 7 Then U$ = "HE CUTS UPFIELD"
    If RN = 8 Then U$ = "HE CHARGES THROUGH A GAPING HOLE"
    If RN = 9 Then U$ = "HE MAY GO ALL THE WAY"
    If RN = 10 Then U$ = "HE BREAKS IT"
    If RN = 11 Then U$ = "HE TURNS IT ON"
    If RN = 12 Then U$ = "HE SHIFTS INTO HIGH GEAR"
    If RN = 13 Then U$ = "HE BREAKS IT BIG!!"
    If RN = 14 Then U$ = "HE BURSTS THROUGH"
    If RN = 15 Then U$ = "HE RUNS WILD"
    If RN = 16 Then U$ = "HE POPS A BIG ONE"
    If RN = 17 Then U$ = "HE BUSTS ONE"
    If RN = 18 Then U$ = "HE POPS THROUGH THERE"
    If RN = 19 Then U$ = "HE POPS OUT THERE"
    If RN = 20 Then U$ = "HE SEES A BREAK"
    Call PBP
End Sub

Sub RUNTDPBP ()
    '*** PBP TO START LONG GAIN ***
    RN = Int(Rnd(1) * 29) + 1
    If RN = 1 Then U$ = "HE BREAKS FREE"
    If RN = 2 Then U$ = "HE DODGES A TACKLER"
    If RN = 3 Then U$ = "HE STIFF ARMS A TACKLER"
    If RN = 4 Then U$ = "HE HEADS DOWN THE SIDELINE"
    If RN = 5 Then U$ = "HE SEES DAYLIGHT"
    If RN = 6 Then U$ = "HE CUTS BACK"
    If RN = 7 Then U$ = "HE CUTS UPFIELD"
    If RN = 8 Then U$ = "HE CHARGES THROUGH A GAPING HOLE"
    If RN = 9 Then U$ = "HE MAY GO ALL THE WAY"
    If RN = 10 Then U$ = "HE BREAKS IT"
    If RN = 11 Then U$ = "HE TURNS IT ON"
    If RN = 12 Then U$ = "HE SHIFTS INTO HIGH GEAR"
    If RN = 13 Then U$ = "HE BREAKS IT BIG!!"
    If RN = 14 Then U$ = "HE BURSTS THROUGH"
    If RN = 15 Then U$ = "HE RUNS WILD"
    If RN = 16 Then U$ = "HE POPS A BIG ONE"
    If RN = 17 Then U$ = "HE BUSTS ONE"
    If RN = 18 Then U$ = "HE POPS THROUGH THERE"
    If RN = 19 Then U$ = "HE POPS OUT THERE"
    If RN = 20 Then U$ = "HE SEES A BREAK"
    If RN = 21 Then U$ = "HE'S GONE!"
    If RN = 22 Then U$ = "NO ONE WILL CATCH HIM"
    If RN = 23 Then U$ = "SEE YOU LATER!!"
    If RN = 24 Then U$ = "HE GETS THE CORNER"
    If RN = 25 Then U$ = "HE COULD GO"
    If RN = 26 Then U$ = "HE'LL GO THE DISTANCE"
    If RN = 27 Then U$ = "THERE HE GOES!!"
    If RN = 28 Then U$ = "HE'LL GO ALL THE WAY"
    If RN = 29 Then U$ = "HE'S IN THE SECONDARY"
    Call PBP
End Sub

Sub SETWEATHER ()
    For I = 0 To 1: YR%(I) = Val(Left$(A$(I), 2)): Next
    If YR%(0) >= 0 And YR%(0) <= 15 Then YR%(0) = 99
    If YR%(1) >= 0 And YR%(1) <= 15 Then YR%(1) = 99
    If PA%(H%, 3) = 1 Then 299
    Call SETSCREEN: J = 9: Call BOXDRAW(J): Print "WEATHER OPTIONS ": Locate 12, 14
    Locate , 14: Print "0 - GOOD"
    Locate , 14: Print "1 - BAD (RAIN/SNOW)"
    Locate , 14: Print "2 - TERRIBLE (BLIZZARD/HEAVY RAIN)"
    Locate , 14: Print "3 - COMPUTER SELECT"
    If AP = 1 Then I2 = NG%(20): GoTo 155

    153 I$ = GetKeyPress$
    I2 = Val(I$): If I2 < 0 Or I2 > 3 Then 153

    155 Screen , , 0, 0
    If I2 = 0 Then 200
    If I2 = 3 Then I = Int(Rnd(1) * 22) + 1: If I = 18 Then WX%(4) = -3: WX%(5) = 1: WX%(6) = -10
    If I2 = 3 And I = 7 Then WX%(4) = -6: WX%(5) = 2: WX%(6) = -20
    If I2 = 1 Then WX%(4) = -3: WX%(5) = 1: WX%(6) = -10
    If I2 = 2 Then WX%(4) = -6: WX%(5) = 2: WX%(6) = -20

    200 J = 9: Call BOXDRAW(J): Print "WIND OPTIONS ": Locate 12, 14
    Locate , 14: Print "0 - NONE"
    Locate , 14: Print "1 - LIGHT TO MODERATE"
    Locate , 14: Print "2 - STRONG AND GUSTY"
    Locate , 14: Print "3 - COMPUTER SELECT"
    If AP = 1 Then I2 = NG%(19): GoTo 156

    154 I$ = GetKeyPress$
    I2 = Val(I$): If I2 < 0 Or I2 > 3 Then 154

    156 Screen , , 0, 0
    If I2 = 0 Then 299
    If I2 = 3 Then I = Int(Rnd(1) * 22) + 1: If I = 14 Then WX%(1) = -7: WX%(2) = -5: WX%(3) = -10
    If I2 = 3 And I = 3 Then WX%(1) = -15: WX%(2) = -15: WX%(3) = -30
    If I2 = 1 Then WX%(1) = -7: WX%(2) = -5: WX%(3) = -10
    If I2 = 2 Then WX%(1) = -15: WX%(2) = -15: WX%(3) = -30

    299 W% = Int(Rnd(1) * 2)
    Call SCOUT
    Call HALFSTART
    Q = 1
    T = 900
    For I = 0 To 1: For I1 = 0 To 4: S(I, I1) = 0: Next I1: Next I: Call SETSCREEN
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 Then
        U$ = A$(W%) + " WON THE COIN TOSS": Call PBP
    Else
        U$ = AA$(W%) + " WON THE COIN TOSS": Call PBP
    End If
End Sub


Sub FGATT ()
    DT = Int(117 + intY8% - Y + .5)
    If DT <= 19 Then FA(P1, Q7%(P1), 0) = FA(P1, Q7%(P1), 0) + 1
    If DT > 19 And DT <= 29 Then FA(P1, Q7%(P1), 1) = FA(P1, Q7%(P1), 1) + 1
    If DT > 29 And DT <= 39 Then FA(P1, Q7%(P1), 2) = FA(P1, Q7%(P1), 2) + 1
    If DT > 39 And DT <= 49 Then FA(P1, Q7%(P1), 3) = FA(P1, Q7%(P1), 3) + 1
    If DT > 49 Then FA(P1, Q7%(P1), 4) = FA(P1, Q7%(P1), 4) + 1
End Sub


Sub FGMADE ()
    If DT <= 19 Then FM(P1, Q7%(P1), 0) = FM(P1, Q7%(P1), 0) + 1
    If DT > 19 And DT <= 29 Then FM(P1, Q7%(P1), 1) = FM(P1, Q7%(P1), 1) + 1
    If DT > 29 And DT <= 39 Then FM(P1, Q7%(P1), 2) = FM(P1, Q7%(P1), 2) + 1
    If DT > 39 And DT <= 49 Then FM(P1, Q7%(P1), 3) = FM(P1, Q7%(P1), 3) + 1
    If DT > 49 Then FM(P1, Q7%(P1), 4) = FM(P1, Q7%(P1), 4) + 1
    If DT > FL(P1, Q7%(P1)) Then FL(P1, Q7%(P1)) = DT
End Sub


Sub OFFMENU ()
    If E = 1 Then
        R0 = Int(Rnd(1) * 100) + 1
        If R0 <= 100 Then P2 = 3
        If R0 <= 90 Then P2 = 4
        If R0 <= 80 Then P2 = 5
        If R0 <= 70 Then P2 = 10
        If R0 <= 50 Then P2 = 11
        If R0 <= 25 Then P2 = 17
    End If
    If Q = 4 And T <= 35 * (4 - D1) And T1(D) = 0 And S(P1, 0) > S(D, 0) Then P2 = 9: R1 = 1: QBN% = 1: GoTo 8800
    If Q = 2 And T <= 120 And Y > 40 Or Q >= 4 And T <= 120 Then 8871
    If D1 = 4 And Q >= 4 And Y >= 50 And S(D, 0) - S(P1, 0) > 10 Then 8833
    If D1 = 4 And Q >= 3 And Y >= 55 And S(D, 0) - S(P1, 0) > 14 Then 8833
    If D1 = 4 And Y1 <= 1 And S(P1, 0) - S(D, 0) <= 3 And S(P1, 0) - S(D, 0) > 0 And Y >= 62 + intY8% And Y <= 68 + intY8% Then 8827
    If Q >= 4 And D1 = 4 And S(P1, 0) - S(D, 0) <= 3 And S(P1, 0) - S(D, 0) >= -3 And Y >= 62 + intY8% And Y < 68 + intY8% And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800
    If Q >= 4 And D1 = 4 And Y >= 62 + intY8% And Y < 68 + intY8% And S(D, 0) - S(P1, 0) <= 3 And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800
    If Q >= 4 And Y >= 65 + intY8% And Y < 75 + intY8% And Y1 <= 2 And S(D, 0) - S(P1, 0) > 10 Then 8827
    If Q >= 4 And Y >= 65 + intY8% And Y < 75 + intY8% And S(D, 0) - S(P1, 0) <= 10 And S(D, 0) - S(P1, 0) > 0 And D1 = 4 And T < 240 And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800
    If D1 = 4 And Q = 4 And T > 240 And S(D, 0) - S(P1, 0) >= 4 And S(D, 0) - S(P1, 0) <= 10 And Y > 80 + intY8% Then P2 = 20: GoTo 8800
    If D1 = 4 And Q = 4 And S(D, 0) - S(P1, 0) >= 4 And Y >= 80 + intY8% Then 8833
    If D1 = 4 And Y >= 60 And Y1 <= 2 Then 8827
    If D1 = 4 And Y >= 68 + intY8% And Y1 > 2 And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800
    If D1 = 4 And Y < 68 + intY8% Then P2 = 19: GoTo 8800
    R0 = Int(Rnd(1) * 100) + 1: If Y <= 8 Then 8827
    If B6(P1, 7) > 75 And D1 = 2 And Y1 <= 12 Then 8827
    If B6(P1, 7) > 75 And D1 = 3 And Y1 <= 5 Then 8827
    If D1 = 1 And B6(P1, 7) >= 70 Or D1 <= 2 And B6(P1, 7) >= 80 Then 8827
    If D1 = 1 And Y1 > 10 Then 8833
    If D1 = 2 And Y1 > 10 Then 8833
    If D1 = 2 And Y1 < 5 Then 8827
    If D1 = 3 And Y1 >= 4 Then 8833
    If D1 = 3 And Y1 < 2 Then 8827
    If D1 = 4 And Y1 >= 4 Then 8833
    If D1 = 4 And Y1 < 2 Then 8827
    If Q = 4 And T <= 480 And S(D, 0) - S(P1, 0) >= 9 And R0 >= B6(P1, 7) - B6(P1, 7) / 2 Then 8833
    If Q = 4 And T <= 480 And S(P1, 0) - S(D, 0) >= 9 And R0 >= B6(P1, 7) + B6(P1, 7) / 2 Then 8833
    If R0 > B6(P1, 7) Then 8833

    8827 If K(P1, 1) + K(P1, 3) = 0 Then 8828
    If K(P1, 1) / (K(P1, 1) + K(P1, 3)) > B6(P1, 7) / 100 And B6(P1, 7) < 50 Then 8833

    8828 R0 = Int(Rnd(1) * 100) + 1: P2 = 1
    If R0 <= 70 Then P2 = 2
    If R0 <= 50 Then P2 = 3
    If R0 <= 30 Then P2 = 7
    If R0 <= 20 Then P2 = 4
    If R0 <= 10 Then P2 = 5
    If R0 <= 5 And B6(P1, 7) < 75 Then P2 = 14
    If R0 <= 2 And B6(P1, 7) < 75 Then P2 = 15
    GoTo 8800

    8833 R0 = Int(Rnd(1) * 100) + 1: P2 = 10
    If R0 <= 89 Then P2 = 11
    If R0 <= 63 Then P2 = 12
    If R0 <= 44 Then P2 = 13
    If R0 <= 36 Then P2 = 9
    If R0 <= 30 Then P2 = 17
    If R0 <= 5 Then P2 = 6
    If D1 >= 3 And Y1 >= 5 And P2 = 6 Then P2 = 12
    If D1 >= 3 And Y1 >= 5 And P2 = 10 Then P2 = 11
    If D1 >= 3 And Y1 >= 5 And (P2 = 13 Or P2 = 9) Then P2 = 17
    If Y > 90 And (P2 = 12 Or P2 = 15) Or Y1 < 7 And (P2 = 12 Or P2 = 15) Then P2 = 11
    GoTo 8800

    8871 If S(D, 0) - S(P1, 0) > 3 And Q >= 4 Then 8880
    If S(D, 0) - S(P1, 0) > 3 And Q = 2 Then 8885
    If S(D, 0) - S(P1, 0) >= 0 Then 8885
    R0 = Int(Rnd(1) * 100) + 1: If Y > 40 And Q = 2 Then 8885
    P2 = 4: If R0 <= 60 Then P2 = 3
    If Y >= 65 + intY8% And T <= 22 And Q = 2 Or Y >= 65 + intY8% And T <= 22 And Q > 4 Or Y >= 65 + intY8% And T <= 22 And Q = 4 And S(P1, 0) - S(D, 0) = 0 Then P2 = 20
    If Y >= 65 + intY8% And D1 = 4 And Y1 >= 2 And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20
    If D1 = 4 And Y <= 65 + intY8% Then P2 = 19
    GoTo 8800

    8880 If Q = 2 And D1 = 4 And Y < 65 + intY8% Then P2 = 19: GoTo 8800
    If Q = 2 And D1 = 4 And Y >= 65 + intY8% And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800
    If T <= 22 And T1(P1) = 0 Then P2 = 12: GoTo 8800
    If T <= 12 And T1(P1) <> 0 Then P2 = 12: GoTo 8800
    GoTo 8892

    8885 If Q = 2 And D1 = 4 And Y < 65 + intY8% Or Q >= 4 And Y < 65 + intY8% And S(P1, 0) = S(D, 0) And D1 = 4 Then P2 = 19: GoTo 8800
    If Q = 2 And D1 = 4 And Y >= 65 + intY8% And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800
    If T <= 37 And T1(P1) = 0 And Y < 65 + intY8% Then P2 = 12: GoTo 8800
    If T <= 12 And T1(P1) <> 0 And Y < 65 + intY8% Then P2 = 12: GoTo 8800
    If T <= 22 And T1(P1) = 0 And Y >= 65 + intY8% Then P2 = 20: GoTo 8800
    If T <= 12 And T1(P1) <> 0 And Y >= 65 + intY8% Then P2 = 20: GoTo 8800
    If D1 = 4 And Y >= 65 + intY8% And (G9 <> 0 Or G9 = 0 And YR%(1) > 60 And YR%(0) > 60) Then P2 = 20: GoTo 8800

    8892 R0 = Int(Rnd(1) * 100) + 1: P2 = 16: If R0 < 50 Then P2 = Int(Rnd(1) * 3) + 10
    If Y >= 90 Then P2 = 11
    If Y > 80 And S(D, 0) - S(P1, 0) < 3 Or Y > 90 And S(D, 0) - S(P1, 0) = 3 Then P2 = Int(Rnd(1) * 4) + 1

    8800 If Q > 4 And NG%(14) = 3 And D1 = 4 And P2 = 19 And S(D, 0) = S(P1, 0) Then P2 = 12
    If Q > 4 And NG%(14) = 3 And D1 = 4 And (P2 = 19 Or P2 = 20) And S(D, 0) - S(P1, 0) > 3 Then P2 = 12
    If Q > 4 And NG%(14) = 3 And D1 = 4 And P2 = 19 And S(D, 0) - S(P1, 0) <= 3 Then P2 = 12
End Sub


Sub MEASUREMENT ()
    'MEASUREMENT
    U$ = "HE'S CLOSE TO A FIRST DOWN": Call PBP
    U$ = "THE OFFICIAL SIGNALS FOR A MEASUREMENT": Call PBP: Call PBP
    If G - Y1 < -.2 Then U$ = "HE'S ABOUT A FOOT SHORT": GoTo 3427
    If G - Y1 < -.1 Then U$ = "HE'S ABOUT 6 INCHES SHORT": GoTo 3427
    If G - Y1 <= -.01 Then U$ = "HE'S SHORT BY INCHES": GoTo 3427
    U$ = "THE OFFICIAL SIGNALS FIRST DOWN!"

    3427 Call PBP: Call PBP
End Sub


Sub WHICHRECEIVER
    If I3 > 9 Then U$ = U$ + A2$(P1, I3 - 10) Else U$ = U$ + A1$(P1, I3)
End Sub


Sub CLIPPING ()
    U$ = "*** PENALTY - 10 YD ILLEGAL BLOCK ON THE RETURN ***"
    Call PBP
    If Y < 20 Then Y = Y / 2: K(P1, 12) = K(P1, 12) + Y / 2
    K(P1, 11) = K(P1, 11) + 1: If Y >= 20 Then Y = Y - 10: K(P1, 12) = K(P1, 12) + 10
End Sub


Sub QBBOX ()
    Locate 6, 8: Color PA%(I, 2), PA%(I, 1)
    Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
    For II = 1 To JJ: Locate 6 + II, 8
    Print " º"; String$(61, 32); "º ";: Next
    Locate 7 + JJ, 8
    Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";:
    Locate 7
End Sub


Sub CHANGEQB ()

    8300 Color 15, 0: Cls: JJ = 12: I = I3
    Call QBBOX
    RQ%(I3) = 0
    Locate , 31
    Print "SELECT QUARTERBACK"
    Print: Locate , 18: Print " # QB NAME    % OF  COMP  INT"
    Locate , 18: Print "              ATTS  PCT   PCT  ARM"
    For I = 0 To 3
        If A3$(I3, I) = "XXX" Or A3$(I3, I) = "END" Or A3$(I3, I) = "" Then 8307
        If I = 0 Then 8304
        If A6(I3, I - 1) >= 100 Then 8307

        8304 Locate , 18: Print I; "-"; A3$(I3, I);: Locate , 32: If I = 0 Then Print A6(I3, I);: GoTo 8306
        If I > 0 Then Print Int(A6(I3, I)) - A6(I3, I - 1);

        8306 Locate , 38: Print A7(I3, I);: Locate , 44: Print A8(I3, I);: Locate , 49: Print QX%(I3, I)

    8307 Locate , 1: Next I
    Locate , 19: Print "4 -COMPUTER SELECT PASSER EACH PLAY"
    Print: Locate , 19: Print "SELECT (0-4) ";
    Color 15, 0

    8310 I$ = GetKeyPress$
    I2 = Val(I$): If I2 < 0 Or I2 > 4 Then 8300
    If I2 = 4 Then RQ%(I3) = 1
    If I2 <= 3 Then Q6%(I3) = I2
    If G9 = 0 Or I2 > 3 Or RQ%(I3) = 1 Then 8320

    8314 JJ = 12
    I = I3
    Call QBBOX
    Locate 8, 15: Print "ENTER "; A3$(I3, I2); " MOBILITY RATING (1-9)"
    Print
    Locate , 16: Print "THIS IS HOW OFTEN HE RUNS OUT OF THE POCKET"
    Locate , 16: Print "SOME QB'S LIKE MARINO & FOUTS ALMOST NEVER DO"
    Locate , 16: Print "OTHERS LIKE ELWAY & TARKENTON WILL TAKE OFF"
    Locate , 16: Print "IF UNSURE SIMPLY TYPE 5"
    Locate , 16: Print "        1-LOW, 9-HIGH"
    Color 15, 0

    8318 I$ = GetKeyPress$
    R9%(I3) = Val(I$): If R9%(I3) < 1 Or R9%(I3) > 9 Then 8314

    8320 For I = 0 To 3: V4%(I3, I) = -1: Next I
    If RQ%(I3) = 1 Then 8325
    For I4 = 0 To 9: For I5 = 0 To 3
        If A1$(I3, I4) = A3$(I3, I5) And I5 <> Q6%(I3) Then V4%(I3, I5) = I4
    Next I5: Next I4

    8325 Rem
End Sub

Sub FINDTEAMS ()
    NT = 0
    Open DV$(0) + "FOOT." + YN$ As #1 Len = 934
    Y = LOF(1)
    Field #1, 15 As Q$(0), 640 As X$
    For I1 = 1 To Y / 934
        Get #1, I1
        For I2 = 1 To 15
            If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 6105
        Next I2
        NT = NT + 1
        V$(NT) = Q$(0): V%(NT) = I1

    6105 Next I1
    Close 1
    Call SELECTTEAM
End Sub

Sub KICKOFF ()
    Call SCOREBOARD
    If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then I9 = 2: GoTo 1005
    J = 9: Call BOXDRAW(J): Print A$(W%); " DO YOU ELECT TO:": Locate 12, 14
    Locate , 14: Print "1 - KICK OFF"
    Locate , 14: Print "2 - RECEIVE"
    Locate , 14: Print "3 - DEFEND GOAL WITH WIND AT BACK"
    Locate , 14: Print "4 - DEFEND GOAL WITH WIND IN FACE"

    1001 I$ = GetKeyPress$
    I9 = Val(I$): If I9 < 1 Or I9 > 4 Then 1001

    1005 Screen , , 0, 0
    If I9 < 3 Then 1006
    If I9 > 2 Then 1011

    1006 Call SCOREBOARD: If J6(0) = 1 And D = CT Or J6(0) = 2 Then I1 = 1: GoTo 1010
    J = 7: Call BOXDRAW(J): Print A$(1 - W%); " DO YOU ELECT TO:": Locate 12, 14
    Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
    Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

    1007 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 1 Or I1 > 2 Then 1007

    1010 Screen , , 0, 0
    WX%(0) = D: If I1 = 2 Then WX%(0) = 1 - D
    If I9 = 1 Then Call CHANGEPOSS: W% = 1 - W%
    GoTo 1017

    1011 If I9 = 3 Then WX%(0) = P1
    If I9 = 4 Then WX%(0) = D
    Swap P1, D
    Call SCOREBOARD: If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then I1 = 2: GoTo 1015
    J = 7: Call BOXDRAW(J): Print A$(1 - W%); " DO YOU ELECT TO:": Locate 12, 14
    Locate , 14: Print "1 - KICK OFF"
    Locate , 14: Print "2 - RECEIVE"

    1012 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 1 Or I1 > 2 Then 1012

    1015 Screen , , 0, 0
    If I1 = 1 Then Call CHANGEPOSS

    1017 Rem
End Sub

Sub COLKICKOFF ()

    1200 'COLL KO
    If Q > 2 Then Call KICKOFF: GoTo 1216
    Call SCOREBOARD: If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then I9 = 2: GoTo 1205
    J = 9: Call BOXDRAW(J): Print A$(P1); " DO YOU ELECT TO:": Locate 12, 14
    Locate , 14: Print "1 - KICK OFF"
    Locate , 14: Print "2 - RECEIVE"
    Locate , 14: Print "3 - DEFEND GOAL WITH WIND AT BACK"
    Locate , 14: Print "4 - DEFEND GOAL WITH WIND IN FACE"
    Locate , 14: Print "5 - DEFER CHOICE TO 2ND HALF"

    1201 I$ = GetKeyPress$
    I9 = Val(I$): If I9 < 1 Or I9 > 5 Then 1201

    1205 Screen , , 0, 0
    If I9 < 3 Then 1206
    If I9 > 2 Then 1211

    1206 Call SCOREBOARD: If J6(0) = 1 And D = CT Or J6(0) = 2 Then I1 = 1: GoTo 1210
    J = 7: Call BOXDRAW(J): Print A$(D); " DO YOU ELECT TO:": Locate 12, 14
    Locate , 14: Print "1 - DEFEND GOAL WITH WIND AT BACK"
    Locate , 14: Print "2 - DEFEND GOAL WITH WIND IN FACE"

    1207 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 1 Or I1 > 2 Then 1207

    1210 Screen , , 0, 0
    WX%(0) = D: If I1 = 2 Then WX%(0) = 1 - D
    If I9 = 1 Then Call CHANGEPOSS: W% = 1 - W%
    GoTo 1216

    1211 If I9 = 3 Then WX%(0) = P1
    If I9 = 4 Then WX%(0) = D
    If I9 = 5 Then Swap P1, D: W% = P1: Call KICKOFF: GoTo 1216
    Swap P1, D
    Call SCOREBOARD: If J6(0) = 1 And P1 = CT Or J6(0) = 2 Then I1 = 2: GoTo 1215
    J = 7: Call BOXDRAW(J): Print A$(P1); " DO YOU ELECT TO:": Locate 12, 14
    Locate , 14: Print "1 - KICK OFF"
    Locate , 14: Print "2 - RECEIVE"

    1212 I$ = GetKeyPress$
    I1 = Val(I$): If I1 < 1 Or I1 > 2 Then 1212

    1215 Screen , , 0, 0
    If I1 = 1 Then Call CHANGEPOSS

    1216 Rem
End Sub

Sub TAKESAFETY ()
    TP = Int(Rnd(1) * 3) + 4: T = T - TP: TP(P1) = TP(P1) + TP
    G = (Y * -1) - 1: K(P1, 1) = K(P1, 1) + 1: K(P1, 2) = K(P1, 2) + G
    U$ = A6$(P1, 0) + " IS HEADED OUT OF THE END ZONE": Call PBP
    S(D, Q) = S(D, Q) + 2: S(D, 0) = S(D, 0) + 2
End Sub

Sub DELAY ()
    If J6(0) <> 2 Then 8412
    For DL = 1 To N7
    Next

    8412 Rem
End Sub

Sub FINDINT ()
    'FIND INT
    M5 = 0: RN = Int(Rnd(1) * DDI(1 - P1)) + 1
    For Z1 = 0 To 9
        M5 = M5 + DI(1 - P1, Z1)
        If RN <= M5 Then X$ = DI$(1 - P1, Z1): GoTo 3660
    Next

    3660 Rem
End Sub

Sub YARDAGE ()
    G = G + (Int(Rnd(1) * 11) - 5) / 10
End Sub

Sub PUNTYARDS ()
    I3 = Int(Rnd(1) * 100) + 1: Y = Y + G: Call CHANGEPOSS: YT = Y: Y = 100 - Y: Call YARDLINE
    RN = Int(Rnd(1) * 1000) + 1
    If B2(P1, PR) >= 6 And RN > 900 And I2 <> 1 Then G = 110: GoTo 3819
    If B2(P1, PR) >= 6 Then G = Int((Rnd(1) * 26) + 0) + (B2(P1, PR) - 6) * 3: GoTo 3819
    If B2(P1, PR) = 5 And RN > 940 And I2 <> 1 Then G = 110: GoTo 3819
    If B2(P1, PR) = 5 And RN > 900 Then G = Int((Rnd(1) * 39) + 12): GoTo 3819
    If B2(P1, PR) = 5 Then G = Int((Rnd(1) * 25) + 0): GoTo 3819
    If B2(P1, PR) <= 4 And RN > 988 And I2 <> 1 Then G = 110: GoTo 3819
    If B2(P1, PR) <= 4 And RN > 900 And I2 <> 1 Then G = Int((Rnd(1) * 39) + 12) + (B2(P1, PR) - 3) * 3: GoTo 3819
    If B2(P1, PR) <= 4 And RN > 50 Then G = Int((Rnd(1) * 14) + 0) + (B2(P1, PR) - 3) * 3: GoTo 3819
    If B2(P1, PR) <= 4 Then G = Int((Rnd(1) * 6) - 5)

    3819 X1 = 4
    Call YARDAGE
    Call MARKER
    U$ = "THE BALL IS TAKEN BY " + A5$(P1, PR) + " AT THE" + Str$(Int(YL)): Call PBP
    SY = 100 - Y: EY = SY - G: Call MOVEBALL
End Sub

Sub FAKEYARDS ()
    U$ = "IT'S A FAKE!!! THE UPBACK IS RUNNING THE BALL": Call PBP
    RN = Int(Rnd(1) * 100) + 1
    If I2 = 0 And RN <= 36 Then G = Int(Rnd(1) * 27)
    If I2 = 0 And RN > 36 Then G = Int(Rnd(1) * 6) - 2
    If I2 = 1 And RN <= 36 Then G = Int(Rnd(1) * 21)
    If I2 = 1 And RN > 36 And RN <= 90 Then G = Int(Rnd(1) * 11) - 4
    If I2 = 1 And RN > 90 Then G = 100 - Y
    If I2 = 2 And RN <= 8 Then G = Int(Rnd(1) * 21)
    If I2 = 2 And RN > 8 Then G = Int(Rnd(1) * 7) - 7
    Call YARDAGE: O = 1
    SY = 100 - Y: EY = SY - G: Call MOVEBALL: U$ = "THE UPBACK GAINED": Print: X = 10: X1 = 11
    If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then U$ = U$ + Str$(Int(100 - Y))
    If Int((G + Y) * 10 + .5) / 10 < 99.94999 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10)
    U$ = U$ + " YARDS ON THE FAKE RUN": Call PBP
End Sub

Sub FUMBLE ()
    F8 = Int(Rnd(1) * 100) + 1
    If RF% = 1 Then F8 = F8 - 10: RF% = 0: GoTo 3627
    If F% = 2 Then F8 = F8 + 1
    If P2 = 1 Then F8 = F8 + 2
    If P2 = 5 Then F8 = F8 - 1
    If P2 = 2 Then F8 = F8 - 1
    If F% = 1 Then F8 = F8 - 6
    If F% = 5 Then F8 = F8 + 2
    If F% = 4 Then F8 = F8 - 2

    3627 F8 = F8 - WX%(5)
    F% = 0
    If F8 <= B6(P1, 3) + B6(D, 4) Then F9 = 1
End Sub

Sub FLEA ()
    I1 = Q6%(P1)
    C% = A7(P1, Q6%(P1)) + Int(B6(D, 1)) - 4
    intI% = A8(P1, Q6%(P1)) + B6(D, 2) + 5
    S% = 5
    RN = Int(Rnd(1) * 100) + 1
    If D3 = 11 Then S% = 70
    If D3 = 9 Or D3 = 10 Then S% = 50
    If D3 = 7 Or D3 = 8 Then S% = 30
    If RN <= 25 Then G = Int(Rnd(1) * 50) + 51
    If RN > 25 Then G = Int(Rnd(1) * 15) + 35
    CP = Int(Rnd(1) * 16) + 35
    If intG9%(P1) > 0 Then C% = 3: intI% = intI% + 10: If S% = 5 Then S% = 30
    intG9%(P1) = 1
    U$ = A1$(P1, 0) + " TAKES THE HANDOFF": Call PBP
    U$ = "HE TOSSES IT BACK TO " + A3$(P1, Q6%(P1)) + " FOR A FLEA FLICKER!!!": Call PBP
    I3 = Int(Rnd(1) * 3)
    U$ = A2$(P1, I3) + " IS OPEN DOWNFIELD": Call PBP
    RN = Int(Rnd(1) * 100) + 1
End Sub

Sub REVGAIN ()
    U$ = A3$(P1, Q6%(P1)) + " TAKES THE SNAP"
    U$ = U$ + " AND HANDS OFF TO " + A1$(P1, I1) + " ON THE REVERSE!!": Call PBP
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 20 And RV%(P9) = 0 Then G = (Int(Rnd(1) * 8) + 5) * -1: RF% = 1
    If RN > 20 And RN <= 80 And RV%(P9) = 0 Then G = Int(Rnd(1) * 11)
    If RN > 80 And RV%(P9) = 0 Then G = Int(Rnd(1) * 81)
    If RV%(P9) > 0 And RN <= 5 Then G = Int(Rnd(1) * 81)
    If RV%(P9) > 0 And RN > 5 And RN <= 55 Then G = Int(Rnd(1) * 11)
    If RV%(P9) > 0 And RN > 55 Then G = (Int(Rnd(1) * 8) + 5) * -1: RF% = 1
    RV%(P9) = 1
End Sub

Sub FAKERUN ()
    U$ = "IT'S A FAKE!!! THE HOLDER IS RUNNING THE BALL": Call PBP
    RN = Int(Rnd(1) * 100) + 1
    If I2 = 1 And RN <= 20 Then G = Int(Rnd(1) * 21)
    If I2 = 1 And RN > 20 And RN <= 95 Then G = Int(Rnd(1) * 11) - 4
    If I2 = 1 And RN > 95 Then G = 100 - Y
    If I2 = 2 And RN <= 8 Then G = Int(Rnd(1) * 21)
    If I2 = 2 And RN > 8 Then G = Int(Rnd(1) * 7) - 7
    Call YARDAGE: O = 1
    SY = 100 - Y: EY = SY - G: Call MOVEBALL: U$ = "THE HOLDER GAINED": Print: X = 10: X1 = 11
    If Int((G + Y) * 10 + .5) / 10 >= 99.94999 Then U$ = U$ + Str$(Int(100 - Y))
    If Int((G + Y) * 10 + .5) / 10 < 99.94999 Then U$ = U$ + Str$(Int((G) * 10 + .5) / 10)
    U$ = U$ + " YARDS ON THE FAKE RUN": Call PBP
End Sub

Sub HANDOFF ()

    4110 If D2 = 11 Then U$ = D$(D2): Call PBP: GoTo 4111
    U$ = D$(D2) + " " + D1$(D3): Call PBP

    4111 U$ = A3$(P1, Q6%(P1)) + " TAKES THE SNAP"
    If P2 = 4 Or P2 = 5 Then 4115
    If A1$(P1, I1) = A3$(P1, Q6%(P1)) Then U$ = U$ + " AND KEEPS IT ON THE " + RP$(RP): Call PBP: GoTo 4125
    U$ = U$ + " AND HANDS OFF TO " + A1$(P1, I1) + " ON THE " + RP$(RP): Call PBP: GoTo 4125

    4115 DR = Int(Rnd(1) * 2)
    If A1$(P1, I1) = A3$(P1, Q6%(P1)) Then U$ = U$ + " AND KEEPS IT ON THE " + RP$(RP): Call PBP: GoTo 4125
    RN = Int(Rnd(1) * 2) + 1: On RN GOTO 4118, 4119

    4118 U$ = U$ + " AND TOSSES OUT TO " + A1$(P1, I1): Call PBP: GoTo 4120

    4119 U$ = U$ + " AND PITCHES OUT TO " + A1$(P1, I1): Call PBP: GoTo 4120

    4120 U$ = A1$(P1, I1) + " GOES AROUND THE " + DR$(DR) + " END": Call PBP

    4125 Rem
End Sub


Sub PASSBALL ()
    S6 = 0
    Call QBLINE
    I = Int(Rnd(1) * 100) + 1: R1 = 2
    For I2 = 3 To 0 Step -1
        If I <= A6(P1, I2) Then I1 = I2
    Next I2
    If RQ%(P1) = 1 Then Q6%(P1) = I1: GoTo 3506
    I1 = Q6%(P1)

    3506 I = Int(Rnd(1) * 100) + 1
    For I2 = 15 To 10 Step -1
        If I <= A4(P1, I2 - 10) Then I3 = I2
    Next I2
    For I2 = 9 To 0 Step -1
        If I <= A2(P1, I2) Then I3 = I2
    Next I2
    If I3 > 9 And A2$(P1, I3 - 10) = A3$(P1, Q6%(P1)) Or I3 < 10 And A1$(P1, I3) = A3$(P1, Q6%(P1)) Then 3506
    If D2 = 11 Then U$ = D$(D2) Else U$ = D$(D2) + " " + D1$(D3)
    Call PBP
    Call PASSPBP
    RN = Int(Rnd(1) * 6) + 1
    If I3 > 9 Then RV$ = A2$(P1, I3 - 10) Else RV$ = A1$(P1, I3)
    If RN = 1 Then U$ = "HE'S GOT "
    If RN = 2 Then U$ = "HE SPOTS "
    If RN = 3 Then U$ = "HE'S LOOKING FOR "
    If RN = 4 Then U$ = "HE SEES "
    If RN = 5 Then U$ = "HE'S SEARCHING FOR "
    If RN = 6 Then U$ = "HE'S GOT "

    3515 Call WHICHRECEIVER
    I = Int(Rnd(1) * 5) + 1
    DR = Int(Rnd(1) * 2)
    On I GOTO 3516, 3517, 3518, 3519, 3520

    3516 U$ = U$ + " IN THE OPEN": Call PBP: GoTo 3525

    3517 U$ = U$ + " ON THE SIDELINE": Call PBP: GoTo 3525

    3518 If P2 <> 13 Then U$ = U$ + " OVER THE MIDDLE": Call PBP: GoTo 3525

    3519 If P2 <> 13 Then U$ = U$ + " DOWNFIELD": Call PBP: GoTo 3525

    3520 If P2 = 12 Or P2 = 15 Then U$ = U$ + " DEEP": Call PBP: GoTo 3525
    GoTo 3516

    3525 I = Int(Rnd(1) * 100) + 1
End Sub

Sub ARMADJUST ()
    S2 = A7(P1, I1) + Int(B6(D, 1))
    If P1 = H% Then S2 = S2 + 3
    If P1 <> H% And H% <> 2 Then S2 = S2 - 3
    If Y <= 40.001 Then PQ = 3
    If Y > 40.001 And Y <= 80.001 Then PQ = -2
    If Y > 80.001 And Y <= 85.001 Then PQ = -4
    If Y > 85.001 And Y <= 90.001 Then PQ = -10
    If Y > 90.001 And Y <= 95.001 Then PQ = -26
    If Y > 95.001 Then PQ = -13
    S3 = S1%(1, P2 - 8, D3)
    PW = 0
    If WX%(0) <> P1 And (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18) Then PW = WX%(2)
    If WX%(0) <> P1 And (P2 = 12 Or P2 = 15) Then PW = WX%(3) + WX%(6)
End Sub

Sub SCORE ()
    X2 = X2 + 1: T(X2) = T: C(X2) = Q
    If X1 <> 7 Then K1(X2, 1) = P1
    If X1 = 7 Then K1(X2, 1) = D
    K1(X2, 0) = X1
    K1(X2, 5) = 100 - Y
    If X1 = 3 Then K1(X2, 5) = 100 - N
    If X1 = 8 Then K1(X2, 5) = 117 + intY8% - Y
    K1(X2, 2) = I1
    If X1 = 5 Or X1 = 12 Then K1(X2, 2) = Z1
    K1(X2, 3) = I3
    If K1(X2, 5) < 1 Then K1(X2, 5) = 1
End Sub

Sub COMPILER ()
    If X < 1 Or X > 4 Then 7000
    On X GOTO 7002, 7005, 7006, 7002

    7002 K(P1, 1) = K(P1, 1) + 1: If G + Y < 99.95 Then K(P1, 2) = K(P1, 2) + G
    If G + Y >= 99.95 Then K(P1, 2) = K(P1, 2) + 100 - Y
    GoTo 7008

    7005 K(P1, 3) = K(P1, 3) + 1: GoTo 7008

    7006 K(P1, 3) = K(P1, 3) + 1: K(P1, 4) = K(P1, 4) + 1: If G + Y < 99.95 Then K(P1, 6) = K(P1, 6) + G
    If G + Y >= 99.95 Then K(P1, 6) = K(P1, 6) + 100 - Y

    7008 On X GOTO 7009, 7012, 7013, 7022

    7009 K2(P1, I1, 0) = K2(P1, I1, 0) + 1: If G + Y >= 99.95 Then K2(P1, I1, 1) = K2(P1, I1, 1) + 100 - Y: K2(P1, I1, 2) = K2(P1, I1, 2) + 1: GoSub 3635
    If G + Y < 99.95 Then K2(P1, I1, 1) = K2(P1, I1, 1) + G: GoSub 3636

    7011 If HB% = 1 Then HB% = 0
    X = 0: GoTo 7000

    7012 If HB% = 0 Then K2(P1, I1, 9) = K2(P1, I1, 9) + 1
    GoTo 7011

    7013 If HB% = 1 Then 7015
    K2(P1, I1, 9) = K2(P1, I1, 9) + 1: K2(P1, I1, 10) = K2(P1, I1, 10) + 1: If G + Y >= 99.95 Then K2(P1, I1, 12) = K2(P1, I1, 12) + 100 - Y: K2(P1, I1, 13) = K2(P1, I1, 13) + 1
    If G + Y < 99.95 Then K2(P1, I1, 12) = K2(P1, I1, 12) + G

    7015 If I3 > 9 Then K2(P1, I3 - 10, 6) = K2(P1, I3 - 10, 6) + 1
    If I3 <= 9 Then K2(P1, I3, 3) = K2(P1, I3, 3) + 1
    If I3 > 9 And G + Y >= 99.95 Then K2(P1, I3 - 10, 7) = K2(P1, I3 - 10, 7) + 100 - Y: K2(P1, I3 - 10, 8) = K2(P1, I3 - 10, 8) + 1
    If I3 > 9 And G + Y < 99.95 Then K2(P1, I3 - 10, 7) = K2(P1, I3 - 10, 7) + G
    If I3 <= 9 And G + Y >= 99.95 Then K2(P1, I3, 4) = K2(P1, I3, 4) + 100 - Y: K2(P1, I3, 5) = K2(P1, I3, 5) + 1
    If I3 <= 9 And G + Y < 99.95 Then K2(P1, I3, 4) = K2(P1, I3, 4) + G
    GoTo 7011

    7022 If W5 = 0 Or W5 = 1 And G9 = 0 Then K2(P1, Q6%(P1) + 10, 0) = K2(P1, Q6%(P1) + 10, 0) + 1
    If W5 = 0 And G + Y < 99.95 Or W5 = 1 And G9 = 0 And G + Y < 99.95 Then K2(P1, Q6%(P1) + 10, 1) = K2(P1, Q6%(P1) + 10, 1) + G
    If W5 = 0 And G + Y >= 99.95 Or W5 = 1 And G9 = 0 And G + Y >= 99.95 Then K2(P1, Q6%(P1) + 10, 1) = K2(P1, Q6%(P1) + 10, 1) + 100 - Y: K2(P1, Q6%(P1) + 10, 2) = K2(P1, Q6%(P1) + 10, 2) + 1
    If W5 = 1 Then W6(P1, 0) = W6(P1, 0) + 1: W6(P1, 1) = W6(P1, 1) + G * -1: SK(1 - P1, Z1, 0) = SK(1 - P1, Z1, 0) + 1: SK(1 - P1, Z1, 1) = SK(1 - P1, Z1, 1) + G * -1
    GoTo 7011

    3635 BG = 100 - Y
    If BG > RB(P1, I1) Then strRB$(P1, I1) = "t": RB(P1, I1) = BG
    Return

    3636 If G > RB(P1, I1) Then strRB$(P1, I1) = "": RB(P1, I1) = G
    Return

    7000 Rem
End Sub

Sub TWOMIN ()
    J = 7: Call BOXDRAW(J)
    Locate 9, 14: Print "1 - HUDDLE"
    Locate , 14: Print "2 - TIME OUT "; A$(P1); " "; T1(P1)
    Locate , 14: Print "3 - TIME OUT "; A$(D); " "; T1(D):
    Locate , 14: Print "4 - HURRY UP OFFENSE"
    Locate , 14: Print "5 - THROW AWAY PASS"

    3910 If J6(0) = 0 Then 3911
    I5 = S(P1, 0) - S(D, 0)
    If (J6(0) = 1 And D = CT Or J6(0) = 2) And I5 > 0 And I5 <= 14 And T1(D) > 0 And Q = 4 Then I1 = 3: GoTo 3913
    If Q = 2 And Y > 15 And (J6(0) = 1 And P1 = CT Or J6(0) = 2) And I5 <= 0 And T1(P1) > 0 Then I1 = 2: GoTo 3913
    If Q = 2 And Y > 40 And I5 > 0 And B6(P1, 7) < 65 And T1(P1) > 0 And (J6(0) = 1 And P1 = CT Or J6(0) = 2) Then I1 = 2: GoTo 3913
    If (J6(0) = 1 And P1 = CT Or J6(0) = 2) And I5 <= 0 And I5 >= -14 And T1(P1) > 0 Then I1 = 2: GoTo 3913
    If Q = 2 And Y > 15 And (J6(0) = 1 And P1 = CT Or J6(0) = 2) And I5 <= 0 And T1(P1) <= 0 Then I1 = 4: GoTo 3913
    If Q = 2 And I5 >= 0 And Y > 40 And B6(P1, 7) < 65 And (J6(0) = 2 Or J6(0) = 1 And P1 = CT) Then I1 = 4: GoTo 3913
    If (J6(0) = 1 And P1 = CT Or J6(0) = 2) And I5 <= 0 And I5 >= -14 And T1(P1) <= 0 Then I1 = 4: GoTo 3913
    If J6(0) = 2 Then I1 = 1: GoTo 3913

    3911 I$ = GetKeyPress$
    I1 = Val(I$)
    If I1 < 1 Or I1 > 5 Or (I1 = 2 Or I1 = 4 Or I1 = 5) And J6(0) = 1 And P1 = CT Or I1 = 3 And J6(0) = 1 And D = CT Then 3910

    3913 Color 0, 5 * (1 - BW): Screen , , 0, 0
End Sub

Sub FGDIST ()
    I9 = 0: If WX%(0) = P1 And WX%(1) = -7 Then I9 = -7
    If WX%(0) = P1 And WX%(1) = -15 Then I9 = -15
    If WX%(0) <> P1 And WX%(1) = -7 Then I9 = 7
    If WX%(0) <> P1 And WX%(1) = -15 Then I9 = 15
    For I = 4 To 0 Step -1: If Y >= F1(I) + intY8% + I9 Then F2 = F(I) - ((9 - B4(P1, Q7%(P1))) * 5)
        If Y >= 68 + intY8% And Y >= F1(I) + intY8% + I9 Then F2 = F2 + (Y - F1(I)) + intY8% - 4 + I9
    Next I
    If F2 <= 0 Then F2 = 5
    If F2 >= 100 Then F2 = 95
End Sub

Sub RUNMENU ()
    J = 12: Call PBOX: Print "SELECT RUNNING PLAY "; A$(P1)
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

Sub PASSMENU ()
    J = 11: Call PBOX: Print "SELECT PASS PLAY "; A$(P1)
    Locate 12
    Locate , 14: Print "1 - 3 STEP DROP           8 - SIDELINE PASS"
    Locate , 14: Print "2 - SHORT PASS            9 - ROLLOUT PASS "
    Locate , 14: Print "3 - MEDIUM PASS          10 - BOOTLEG PASS "
    Locate , 14: Print "4 - LONG PASS            11 - CROSS        "
    Locate , 14: Print "5 - SCREEN PASS          12 - CURL PASS    "
    Locate , 14: Print "6 - MED PLAY ACT PASS    13 - DEEP POST    "
    Locate , 14: Print "7 - LONG PLAY ACT PASS   14 - DEEP CORNER  "
End Sub

Sub SHORTYARDAGE ()
    '*** GOAL LINE DEFENSE ***
    I6 = Int(Rnd(1) * 100) + 1
    If D2 <= 3 Or D2 >= 6 Then 4405
    GoTo 4505

    4405 If Y >= 94.999 And I6 <= 100 Then G = -1
    If Y >= 94.999 And I6 <= 93 Then G = 0
    If Y >= 94.999 And I6 <= 76 Then G = 1
    If Y >= 94.999 And I6 <= 52 Then G = 2
    If Y >= 94.999 And I6 <= 35 Then G = 3
    If Y >= 94.999 And I6 <= 22 Then G = 4
    If Y >= 94.999 And I6 <= 17 Then G = 6
    If Y >= 95.999 And I6 <= 100 Then G = -1
    If Y >= 95.999 And I6 <= 94 Then G = 0
    If Y >= 95.999 And I6 <= 78 Then G = 1
    If Y >= 95.999 And I6 <= 49 Then G = 2
    If Y >= 95.999 And I6 <= 29 Then G = 3
    If Y >= 95.999 And I6 <= 26 Then G = 5
    If Y >= 96.999 And I6 <= 100 Then G = -1
    If Y >= 96.999 And I6 <= 98 Then G = 0
    If Y >= 96.999 And I6 <= 80 Then G = 1
    If Y >= 96.999 And I6 <= 32 Then G = 2
    If Y >= 96.999 And I6 <= 27 Then G = 4
    If Y >= 97.999 And I6 <= 100 Then G = -1
    If Y >= 97.999 And I6 <= 91 Then G = 0
    If Y >= 97.999 And I6 <= 72 Then G = 1
    If Y >= 97.999 And I6 <= 46 Then G = 3
    If Y >= 98.999 And I6 <= 100 Then G = -1
    If Y >= 98.999 And I6 <= 96 Then G = 0
    If Y >= 98.999 And I6 <= 61 Then G = 2
    GoTo 4555

    4505 If Y >= 94.999 And I6 <= 100 Then G = -4
    If Y >= 94.999 And I6 <= 98 Then G = -3
    If Y >= 94.999 And I6 <= 91 Then G = -2
    If Y >= 94.999 And I6 <= 84 Then G = -1
    If Y >= 94.999 And I6 <= 77 Then G = 0
    If Y >= 94.999 And I6 <= 63 Then G = 1
    If Y >= 94.999 And I6 <= 43 Then G = 2
    If Y >= 94.999 And I6 <= 29 Then G = 3
    If Y >= 94.999 And I6 <= 17 Then G = 4
    If Y >= 94.999 And I6 <= 14 Then G = 6
    If Y >= 95.999 And I6 <= 100 Then G = -5
    If Y >= 95.999 And I6 <= 97 Then G = -4
    If Y >= 95.999 And I6 <= 94 Then G = -3
    If Y >= 95.999 And I6 <= 91 Then G = -2
    If Y >= 95.999 And I6 <= 90 Then G = -1
    If Y >= 95.999 And I6 <= 86 Then G = 0
    If Y >= 95.999 And I6 <= 72 Then G = 1
    If Y >= 95.999 And I6 <= 45 Then G = 2
    If Y >= 95.999 And I6 <= 31 Then G = 3
    If Y >= 95.999 And I6 <= 29 Then G = 5
    If Y >= 96.999 And I6 <= 100 Then G = -4
    If Y >= 96.999 And I6 <= 93 Then G = -2
    If Y >= 96.999 And I6 <= 88 Then G = -1
    If Y >= 96.999 And I6 <= 85 Then G = 0
    If Y >= 96.999 And I6 <= 69 Then G = 1
    If Y >= 96.999 And I6 <= 28 Then G = 2
    If Y >= 96.999 And I6 <= 24 Then G = 4
    If Y >= 97.999 And I6 <= 100 Then G = -4
    If Y >= 97.999 And I6 <= 94 Then G = -2
    If Y >= 97.999 And I6 <= 92 Then G = -1
    If Y >= 97.999 And I6 <= 83 Then G = 0
    If Y >= 97.999 And I6 <= 65 Then G = 1
    If Y >= 97.999 And I6 <= 43 Then G = 3
    If Y >= 98.999 And I6 <= 100 Then G = -4
    If Y >= 98.999 And I6 <= 97 Then G = -3
    If Y >= 98.999 And I6 <= 90 Then G = -2
    If Y >= 98.999 And I6 <= 83 Then G = -1
    If Y >= 98.999 And I6 <= 74 Then G = 0
    If Y >= 98.999 And I6 <= 57 Then G = 2

    4555 I6 = Int(Rnd(1) * 100) + 1
    If I2 <= 30 And I6 <= GL%(I2, 1) Then G = G + GL%(I2, 2)
    If I2 > 30 And I6 <= GL%(30, 1) Then G = G + GL%(30, 2)
End Sub

Sub PASSPBP ()
    '*** PASS PLAY BY PLAY ***
    RN = Int(Rnd(1) * 100) + 1
    If D3 >= 7 And RN <= 33 Then U$ = "BLITZ IS COMING...!!": Call PBP: GoTo 4906
    If D3 >= 7 And RN <= 66 Then U$ = "HERE COMES THE BLITZ...!!": Call PBP: GoTo 4906
    If D3 >= 7 Then U$ = "BLITZ IS ON...!!": Call PBP
    If intDS%(1 - P1) = 0 Then X$ = A$(1 - P1): GoTo 4906
    M5 = 0
    RN = Int(Rnd(1) * DDS(1 - P1)) + 1
    For Z1 = 0 To 14
        M5 = M5 + DS(1 - P1, Z1)
        If RN <= M5 Then X$ = DS$(1 - P1, Z1): GoTo 4906
    Next

    4906 RN = Int(Rnd(1) * 15) + 1
    If (D3 >= 7 Or D2 >= 9) And RN = 1 Then U$ = A3$(P1, I1) + " HAS TO HURRY"
    If (D3 >= 7 Or D2 >= 9) And RN = 2 Then U$ = A3$(P1, I1) + " IS UNDER A HEAVY PASS RUSH"
    If (D3 >= 7 Or D2 >= 9) And RN = 3 Then U$ = A3$(P1, I1) + " IS BEING RUSHED"
    If (D3 >= 7 Or D2 >= 9) And RN = 4 Then U$ = A3$(P1, I1) + " IS BEING PRESSURED"
    If (D3 >= 7 Or D2 >= 9) And RN = 5 Then U$ = A3$(P1, I1) + " FEELS THE HEAT"
    If (D3 >= 7 Or D2 >= 9) And RN = 6 Then U$ = A3$(P1, I1) + " IS BEING CHASED"
    If (D3 >= 7 Or D2 >= 9) And RN = 7 Then U$ = A3$(P1, I1) + " IS BEING HURRIED"
    If (D3 >= 7 Or D2 >= 9) And RN = 8 Then U$ = A3$(P1, I1) + " IS FORCED OUT OF THE POCKET"
    If (D3 >= 7 Or D2 >= 9) And RN = 9 Then U$ = A3$(P1, I1) + " IS SCRAMBLING"
    If (D3 >= 7 Or D2 >= 9) And RN = 10 Then U$ = A3$(P1, I1) + " IS UNDER THE GUN"
    If (D3 >= 7 Or D2 >= 9) And RN = 11 Then U$ = A3$(P1, I1) + "'S POCKET IS COLLAPSING"
    If (D3 >= 7 Or D2 >= 9) And RN = 12 Then U$ = A3$(P1, I1) + "'S PROTECTION IS BREAKING DOWN"
    If (D3 >= 7 Or D2 >= 9) And RN = 13 Then U$ = "HERE COMES " + X$
    If (D3 >= 7 Or D2 >= 9) And RN = 14 Then U$ = X$ + " BREAKS THROUGH"
    If (D3 >= 7 Or D2 >= 9) And RN = 15 Then U$ = X$ + " IS COMING THROUGH"
    If D3 >= 7 Or D2 >= 9 Then 4905
    If (P2 = 9 Or P2 = 10) And RN <= 50 Then U$ = A3$(P1, I1) + " TAKES THE SNAP AND DROPS BACK...LOOKING SHORT..."
    If (P2 = 9 Or P2 = 10) And RN > 50 Then U$ = A3$(P1, I1) + " DROPS QUICKLY AND SETS..."
    If (P2 = 14 Or P2 = 15) And RN <= 50 Then U$ = A3$(P1, I1) + " MAKES A GOOD PLAY FAKE..."
    If (P2 = 14 Or P2 = 15) And RN > 50 Then U$ = A3$(P1, I1) + " FAKES THE GIVE AND WANTS TO THROW IT..."
    If (P2 = 11 Or P2 = 16) And RN <= 30 Then U$ = A3$(P1, I1) + " DROPS BACK AND LOOKS..."
    If (P2 = 11 Or P2 = 16) And RN > 30 And RN <= 40 Then U$ = A3$(P1, I1) + " DROPS BACK...HE'S UNDER PRESSURE..."
    If (P2 = 11 Or P2 = 16) And RN > 40 And RN <= 60 Then U$ = A3$(P1, I1) + " TO THROW...HAS TIME..."
    If (P2 = 11 Or P2 = 16) And RN > 60 And RN <= 70 Then U$ = A3$(P1, I1) + " BACK TO PASS...HAS PROTECTION..."
    If (P2 = 11 Or P2 = 16) And RN > 70 And RN <= 80 Then U$ = A3$(P1, I1) + " DROPS BACK...IS BEING CHASED!!"
    If (P2 = 11 Or P2 = 16) And RN > 80 Then U$ = A3$(P1, I1) + " HAS PLENTY OF TIME TO THROW..."
    If P2 = 13 And RN <= 50 Then U$ = A3$(P1, I1) + " DROPPING...BEING CHASED..."
    If P2 = 13 And RN > 50 Then U$ = A3$(P1, I1) + " DROPS BACK...HE'S UNDER PRESSURE..."
    If P2 = 17 Then U$ = A3$(P1, I1) + " ROLLS OUT..."
    If P2 = 18 Then U$ = A3$(P1, I1) + " HAS IT ON THE BOOTLEG"
    If P2 = 12 And RN <= 50 Then U$ = A3$(P1, I1) + " DROPS BACK...HE WANTS TO GO DEEP"
    If P2 = 12 And RN > 50 Then U$ = A3$(P1, I1) + " TAKES A DEEP DROP...HE'S LOOKING LONG"

    4905 Call PBP
End Sub

Sub SACK ()
    U$ = "BUT HE WON'T BE ABLE TO GET THIS PASS AWAY!": Call PBP
    U$ = A3$(P1, I1)
    If HB% = 1 Then U$ = A1$(P1, I1)
    If E = 1 Then U$ = U$ + " IS DROPPED": GoTo 4907
    RN = Int(Rnd(1) * 12) + 1
    If RN = 1 Then U$ = U$ + " IS SACKED"
    If RN = 2 And RN <= 20 Then U$ = U$ + " IS NAILED"
    If RN = 3 And RN <= 30 Then U$ = U$ + " IS BURIED"
    If RN = 4 And RN <= 40 Then U$ = U$ + " IS DRILLED"
    If RN = 5 And RN <= 50 Then U$ = U$ + " IS HIT"
    If RN = 6 And RN <= 60 Then U$ = U$ + " IS THROWN"
    If RN = 7 And RN <= 70 Then U$ = U$ + " IS HAMMERED"
    If RN = 8 And RN <= 80 Then U$ = U$ + " IS TACKLED"
    If RN = 9 And RN <= 90 Then U$ = U$ + " IS SWARMED UNDER"
    If RN = 10 Then U$ = U$ + " IS LEVELED"
    If RN = 11 Then U$ = U$ + " IS SNOWED UNDER"
    If RN = 12 Then U$ = U$ + " IS WRAPPED UP"

    4907 U$ = U$ + " FOR A" + Str$(Int(G + .5)) + " YARD LOSS"
End Sub

Sub QBLINE ()
    QB = 0
    If RQ%(P1) = 1 Then Q6%(P1) = 0 Else QB = Q6%(P1)
    If D1 <= 2 Or D1 = 3 And Int(Rnd(1) * 100) + 1 <= 80 Then 4100
    RN = Int(Rnd(1) * 8) + 1
    If RN = 1 Then U$ = A$(P1) + " COMES UP TO THE LINE"
    If RN = 2 Then U$ = A$(P1) + " BREAKS THE HUDDLE AND LINES UP"
    If RN = 3 Then U$ = "THE " + AA$(P1) + " COME UP TO THE LINE"
    If RN = 4 Then U$ = "THE " + AA$(P1) + " BREAK THE HUDDLE AND LINE UP"
    If RN = 5 Then U$ = A3$(P1, QB) + " BARKS OUT THE SIGNALS"
    If RN = 6 Then U$ = A3$(P1, QB) + " LOOKS OVER THE DEFENSE"
    If RN = 7 Then U$ = "HERE IS THE " + DN$(D1) + " DOWN AND" + Str$(Int(Y1)) + " YARD PLAY FOR " + A$(P1)
    If RN = 8 Then U$ = "HERE IS THE " + DN$(D1) + " DOWN AND" + Str$(Int(Y1)) + " YARD PLAY FOR THE " + AA$(P1)
    Call PBP

    4100 Rem
End Sub

Sub SQUIB ()
    U$ = A7$(P1, Q7%(P1)) + " IS PLACING THE BALL ON THE TEE": Call PBP
    U$ = A$(P1) + " IS LINING UP FOR THE KICKOFF": Call PBP
    U$ = "IT'S A SQUIBBER DOWN THE FIELD": Call PBP
    Swap P1, D
    N = 100 - (Y + Int(Rnd(1) * 16) + 29)
    X = 0
    N = N + (Int(Rnd(1) * 11) - 5) / 10: Y = N: YT = 100 - Y
    Call MARKER: Call YARDLINE
    U$ = "KICK TAKEN BY AN UPMAN AT THE" + Str$(Int(N + .5)) + " YD LINE ": Call PBP
    G = Int(Rnd(1) * 7) + 9
    SY = 100 - Y: EY = SY - G: Call MOVEBALL
    Y = N + G: F% = 3
End Sub

Sub PUNTPBP ()
    RN = Int(Rnd(1) * 12) + 1
    If RN = 1 Then U$ = "HE HITS IT"
    If RN = 2 Then U$ = "HE NAILS IT"
    If RN = 3 Then U$ = "HE KICKS IT"
    If RN = 4 Then U$ = "HE LAUNCHES IT"
    If RN = 5 Then U$ = "HE BOOTS IT"
    If RN = 6 Then U$ = "HE BOOMS IT"
    If RN = 7 Then U$ = "HE HITS ONE"
    If RN = 8 Then U$ = "HE NAILS ONE"
    If RN = 9 Then U$ = "HE KICKS ONE"
    If RN = 10 Then U$ = "HE LAUNCHES ONE"
    If RN = 11 Then U$ = "HE BOOTS ONE"
    If RN = 12 Then U$ = "HE BOOMS ONE"
    Call PBP
    RN = Int(Rnd(1) * 6) + 1
    If RN = 1 Then U$ = "IT'S A HIGH, SPIRALING KICK"
    If RN = 2 Then U$ = "IT'S A HIGH FLOATER"
    If RN = 3 Then U$ = "IT'S A HIGH END OVER END KICK"
    If RN = 4 Then U$ = "IT'S A LOW, LINE DRIVE"
    If RN = 5 Then U$ = "HE GETS IT TO TURN OVER"
    If RN = 6 Then U$ = "HE DOESN'T GET IT TO TURN OVER"
End Sub

Sub TICK ()
    Open "GAME82" For Input As #2
    Input #2, FF$
    For I = 0 To 2: Input #2, AP%(I): Next: Close 2
    If AP%(2) > 32 Then 9134
    For X = 0 To 1: SX$(AP%(2) + 1, X) = A$(X): Next
    Open "TICKER" For Output As #2
    For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
    Print #2, SX%(I, I1, I2): Next: Next: Next
    For I = 1 To 32: For I1 = 0 To 1: Print #2, SX$(I, I1): Next: Next
    For I = 1 To 32: Print #2, ST%(I): Next
    Close 2

    9134 Open DV$(0) + FF$ As #1 Len = 52
    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$
    Next
    Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)
    NG%(13) = 1
    For X = 0 To 20: LSet Q$(X + 1) = MKI$(NG%(X)): Next
    For X = 0 To 1: LSet Q$(22 + X) = YN$(X): Next
    LSet Q$(24) = MKI$(S(0, 0)): LSet Q$(25) = MKI$(S(1, 0))
    Put #1, AP%(0) + AP%(2): Close 1
    AP%(2) = AP%(2) + 1
    AP = 1
End Sub

Sub STUFFED ()
    '*** PBP FOR LOSS/STUFF ***
    RN = Int(Rnd(1) * 12) + 1
    If RN = 1 Then U$ = "HE'S SNOWED UNDER BY A GANG OF TACKLERS"
    If RN = 2 Then U$ = "HE MEETS A STONE WALL"
    If RN = 3 Then U$ = "HE BOUNCES OFF ONE TACKLER"
    If RN = 4 Then U$ = "HE SLIPS AND FALLS"
    If RN = 5 Then U$ = "THE HOLE IS CLOSED"
    If RN = 6 Then U$ = "HE FIGHTS FOR EXTRA YARDAGE"
    If RN = 7 Then U$ = "HE'S STOOD UP!"
    If RN = 8 Then U$ = "HIS FEET GO OUT FROM UNDER HIM"
    If RN = 9 Then U$ = "HE SLIPS TO THE TURF"
    If RN = 10 Then U$ = "DOWN HE GOES!!"
    If RN = 11 Then U$ = "HE SLIPS TRYING TO MAKE HIS CUT"
    If RN = 12 Then U$ = "HE CAN'T KEEP HIS FOOTING"
    Call PBP
End Sub

Sub MISSFG
    DR = Int(Rnd(1) * 2)
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 35 Then U$ = "NO GOOD"
    If RN > 35 And RN <= 60 Then U$ = "IT SAILS WIDE...NO GOOD"
    If RN > 60 And RN <= 80 Then U$ = "HE MISSED IT!"
    If RN > 80 And RN <= 85 Then U$ = "IT HITS THE UPRIGHTS....NO GOOD!"
    If RN > 85 And RN <= 90 Then U$ = "HE HOOKS IT...NO GOOD!"
    If RN > 90 And RN <= 95 And 117 + intY8% - Y > 45 Then U$ = "IT FALLS SHORT....NO GOOD!"
    If RN > 90 And RN <= 95 And 117 + intY8% - Y <= 45 Then U$ = "IT'S NO GOOD ... OFF TO THE " + DR$(DR)
    If RN > 95 Then U$ = "IT'S NO GOOD!"
    Call PBP
End Sub

Sub MADEFG
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 10 Then U$ = "IT'S GOOD!!!"
    If RN > 10 And RN <= 20 Then U$ = "THE KICK IS GOOD!!!"
    If RN > 20 And RN <= 30 Then U$ = "HE PUTS IT THROUGH THE UPRIGHTS!!!"
    If RN > 30 And RN <= 40 Then U$ = "HE SPLITS THE UPRIGHTS!!!"
    If RN > 40 And RN <= 50 Then U$ = "IT'S GOOD!!!"
    If RN > 50 And RN <= 60 Then U$ = "HE HAMMERS IT THROUGH!!!"
    If RN > 60 And RN <= 70 Then U$ = "RIGHT DOWN THE MIDDLE!!!"
    If RN > 70 And RN <= 80 Then U$ = "IT HAS THE DISTANCE...IT'S GOOD!!!"
    If RN > 80 And RN <= 90 Then U$ = "HE NAILS IT...IT'S GOOD!!!"
    If RN > 90 Then U$ = "IT'S GOOD!"
    Call PBP
End Sub

Sub MADEPAT
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 25 Then U$ = "IT'S GOOD!!!"
    If RN > 25 And RN <= 50 Then U$ = "THE KICK IS GOOD!!!"
    If RN > 50 And RN <= 60 Then U$ = "HE PUTS IT THROUGH THE UPRIGHTS!!!"
    If RN > 60 And RN <= 70 Then U$ = "HE SPLITS THE UPRIGHTS!!!"
    If RN > 70 And RN <= 80 Then U$ = "IT'S GOOD!!!"
    If RN > 80 And RN <= 90 Then U$ = "HE HAMMERS IT THROUGH!!!"
    If RN > 90 Then U$ = "RIGHT DOWN THE MIDDLE!!!"
    Call PBP
End Sub


Sub MISSPAT
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 Then U$ = "NO GOOD"
    If RN > 50 And RN <= 70 Then U$ = "HE MISSED IT!"
    If RN > 70 And RN <= 75 Then U$ = "BAD SNAP FROM CENTER!! NO KICK": K2(P1, 6 + Q7%(P1) * 4, 17) = K2(P1, 6 + Q7%(P1) * 4, 17) - 1
    If RN > 75 And RN <= 95 Then U$ = "THE KICK SAILS WIDE!!...NO GOOD!"
    If RN > 95 Then U$ = "IT HITS THE UPRIGHTS!!...NO GOOD!"
    Call PBP
End Sub


Sub EVENTS (SPECIAL%, Count%)
    Dim E%(13)
    Open "EVENTS.BOX" For Input As #1
    For X = 1 To 13
        Input #1, E%(X)
    Next
    Close 1
    If Q > E%(1) Then GoSub 7100: GoTo 8633
    If S(1, 0) > E%(2) Or S(0, 0) > E%(2) Then GoSub 7100: GoTo 8633
    If S(1, 0) < E%(3) Or S(0, 0) < E%(3) Then GoSub 7100: GoTo 8633
    For I = 0 To 1
        For I1 = 0 To 9
            If K2(I, I1, 1) > E%(4) Then GoSub 7100: GoTo 8633
            If K2(I, I1, 2) > E%(5) Then GoSub 7100: GoTo 8633
            If K2(I, I1, 4) > E%(6) Then GoSub 7100: GoTo 8633
            If K2(I, I1, 5) > E%(7) Then GoSub 7100: GoTo 8633
    Next: Next
    For I = 0 To 1
        For I1 = 0 To 5
            If K2(I, I1, 7) > E%(6) Then GoSub 7100: GoTo 8633
            If K2(I, I1, 8) > E%(7) Then GoSub 7100: GoTo 8633
    Next: Next
    For I = 0 To 1
        For I1 = 0 To 3
            If K2(I, I1, 12) > E%(8) Then GoSub 7100: GoTo 8633
            If K2(I, I1, 13) > E%(9) Then GoSub 7100: GoTo 8633
    Next: Next
    For I = 0 To 1
        If K2(I, 3, 17) > E%(10) Then GoSub 7100: GoTo 8633
        If K2(I, 7, 17) > E%(10) Then GoSub 7100: GoTo 8633
    Next

    8633 GoTo 8635

    7100 Beep: Spec$ = DV$(1) + "*.SPC"
    null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
    Count% = DirCount%
    Count% = Count% + 1
    SPECIAL% = 1
    Return

    8635 Rem
End Sub

Sub SELECTTEAM ()
    SELECTION% = 1
    Count% = NT
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    20 MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If EXITCODE% = 27 Then BO% = 1: GoTo 6102
    If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
    If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
    If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
    If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
    If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
    intB8% = V%(SELECTION%)
    Locate 2, 33: Color 31: Print "LOADING..."
    Call GETTEAM

    6102 Rem
End Sub

Sub KICKSET ()
    U$ = A7$(P1, Q7%(P1)) + " IS PLACING THE BALL ON THE TEE": Call PBP
    RN = Int(Rnd(1) * 100) + 1
    If RN <= 50 Then
        U$ = A$(P1) + " IS LINING UP FOR THE KICKOFF"
    Else
        U$ = "THE " + AA$(P1) + " ARE LINING UP FOR THE KICKOFF"
    End If
    Call PBP
    U$ = "AND THE KICK IS ON ITS WAY": Call PBP: Swap P1, D
    RN = Int(Rnd(1) * 100) + 1
    '*** added +intY8% to the next 2 lines ***
    If RN <= 88 Then N = 100 - (Y + Int(Rnd(1) * 13) + 55 + intY8%)
    If RN > 88 Then N = 100 - (Y + Int(Rnd(1) * 8) + 68 + intY8%)
    If G9 = 0 And Y9 = 0 Then N = N + 10
    If WX%(0) = P1 Then N = N - Int(Rnd(1) * 7) + 3 - WX%(1)
    If WX%(0) <> P1 Then N = N + Int(Rnd(1) * 7) - 3 + WX%(1)
End Sub

Sub QBTHROW ()
    RN = Int(Rnd(1) * 100) + 1
    If RN = 1 Then U$ = "HE PASSES IT"
    If RN = 2 Then U$ = "HE FIRES IT"
    If RN = 3 Then U$ = "HE FIRES THE BALL"
    If RN = 4 Then U$ = "HE FIRES ONE"
    If RN = 5 Then U$ = "HE FIRES A PASS"
    If RN = 6 Then U$ = "HE RIFLES IT"
    If RN = 7 Then U$ = "HE RIFLES THE BALL"
    If RN = 8 Then U$ = "HE RIFLES ONE"
    If RN = 9 Then U$ = "HE RIFLES A PASS"
    If RN = 10 Then U$ = "HE THROWS"
    If RN = 11 Then U$ = "HE TOSSES"
    If RN = 12 Then U$ = "HE DRILLS A PASS"
    If RN = 13 Then U$ = "HE FLOATS A PASS"
    If RN = 14 Then U$ = "HE THREADS IT"
    If RN = 15 Then U$ = "HE DRILLS THE BALL"
    If RN = 16 Then U$ = "HE UNLOADS THE BALL"
    If RN = 17 Then U$ = "HE UNLOADS IT"
    If RN = 18 Then U$ = "HE RIPS THE BALL"
    If RN = 19 Then U$ = "HE RIPS IT"
    If RN = 20 Then U$ = "THROWN ON A LINE"
    If RN = 21 Then U$ = "HE DRILLS IT"
    If RN = 22 Then U$ = "HE FLINGS IT"
    If RN = 23 Then U$ = "HE FIRES A BULLET"
    If RN <= 23 Then Call PBP
End Sub

Sub RUNPBP ()
    If P2 = 4 Or P2 = 5 Then 6616
    RN = Int(Rnd(1) * 46) + 1
    If RN = 1 Then U$ = A1$(P1, I1) + " GAINS"
    If RN = 2 Then U$ = A1$(P1, I1) + " BLASTS AHEAD FOR"
    If RN = 3 Then U$ = A1$(P1, I1) + " SCOOTS OUT FOR"
    If RN = 4 Then U$ = A1$(P1, I1) + " SCOOTS AHEAD FOR"
    If RN = 5 Then U$ = A1$(P1, I1) + " IS AHEAD FOR"
    If RN = 6 Then U$ = A1$(P1, I1) + " POPS THROUGH FOR"
    If RN = 7 Then U$ = A1$(P1, I1) + " SLASHES AHEAD FOR"
    If RN = 8 Then U$ = A1$(P1, I1) + " HAMMERS AHEAD FOR"
    If RN = 9 Then U$ = A1$(P1, I1) + " HAMMERS THROUGH FOR"
    If RN = 10 Then U$ = A1$(P1, I1) + " HAMMERS OUT"
    If RN = 11 Then U$ = A1$(P1, I1) + " MUSCLES AHEAD FOR"
    If RN = 12 Then U$ = A1$(P1, I1) + " MUSCLES THROUGH FOR"
    If RN = 13 Then U$ = A1$(P1, I1) + " MUSCLES OUT"
    If RN = 14 Then U$ = A1$(P1, I1) + " DASHES AHEAD FOR"
    If RN = 15 Then U$ = A1$(P1, I1) + " DASHES THROUGH FOR"
    If RN = 16 Then U$ = A1$(P1, I1) + " DASHES OUT FOR"
    If RN = 17 Then U$ = A1$(P1, I1) + " RUMBLES AHEAD FOR"
    If RN = 18 Then U$ = A1$(P1, I1) + " RUMBLES THROUGH FOR"
    If RN = 19 Then U$ = A1$(P1, I1) + " RUMBLES FOR"
    If RN = 20 Then U$ = A1$(P1, I1) + " CRACKS AHEAD FOR"
    If RN = 21 Then U$ = A1$(P1, I1) + " CRACKS THROUGH FOR"
    If RN = 22 Then U$ = A1$(P1, I1) + " DARTS AHEAD FOR"
    If RN = 23 Then U$ = A1$(P1, I1) + " DARTS THROUGH FOR"
    If RN = 24 Then U$ = A1$(P1, I1) + " DARTS FOR"
    If RN = 25 Then U$ = A1$(P1, I1) + " KNIFES AHEAD FOR"
    If RN = 26 Then U$ = A1$(P1, I1) + " KNIFES THROUGH FOR"
    If RN = 27 Then U$ = A1$(P1, I1) + " BULLS HIS WAY FOR"
    If RN = 28 Then U$ = A1$(P1, I1) + " BULLS AHEAD FOR"
    If RN = 29 Then U$ = A1$(P1, I1) + " BULLS THROUGH FOR"
    If RN = 30 Then U$ = A1$(P1, I1) + " MUSCLES HIS WAY FOR"
    If RN = 31 Then U$ = A1$(P1, I1) + " BLASTS HIS WAY FOR"
    If RN = 32 Then U$ = A1$(P1, I1) + " POWERS HIS WAY FOR"
    If RN = 33 Then U$ = A1$(P1, I1) + " POWERS AHEAD FOR"
    If RN = 34 Then U$ = A1$(P1, I1) + " POWERS THROUGH FOR"
    If RN = 35 Then U$ = A1$(P1, I1) + " DIVES AHEAD FOR"
    If RN = 36 Then U$ = A1$(P1, I1) + " DIVES OUT FOR"
    If RN = 37 Then U$ = A1$(P1, I1) + " DIVES THROUGH FOR"
    If RN = 38 Then U$ = A1$(P1, I1) + " CHARGES AHEAD FOR"
    If RN = 39 Then U$ = A1$(P1, I1) + " CHARGES HIS WAY FOR"
    If RN = 40 Then U$ = A1$(P1, I1) + " CHARGES THROUGH FOR"
    If RN = 41 Then U$ = A1$(P1, I1) + " SMASHES HIS WAY FOR"
    If RN = 42 Then U$ = A1$(P1, I1) + " SMASHES AHEAD FOR"
    If RN = 43 Then U$ = A1$(P1, I1) + " SMASHES THROUGH FOR"
    If RN = 44 Then U$ = A1$(P1, I1) + " BOLTS AHEAD FOR"
    If RN = 45 Then U$ = A1$(P1, I1) + " BOLTS THROUGH FOR"
    If RN = 46 Then U$ = A1$(P1, I1) + " BOLTS FOR"
    GoTo 6617

    6616 RN = Int(Rnd(1) * 23) + 1
    If RN = 1 Then U$ = A1$(P1, I1) + " GALLOPS AHEAD FOR"
    If RN = 2 Then U$ = A1$(P1, I1) + " GALLOPS THROUGH FOR"
    If RN = 3 Then U$ = A1$(P1, I1) + " GALLOPS FOR"
    If RN = 4 Then U$ = A1$(P1, I1) + " DASHES AHEAD FOR"
    If RN = 5 Then U$ = A1$(P1, I1) + " DASHES THROUGH FOR"
    If RN = 6 Then U$ = A1$(P1, I1) + " DASHES FOR"
    If RN = 7 Then U$ = A1$(P1, I1) + " SCOOTS AHEAD FOR"
    If RN = 8 Then U$ = A1$(P1, I1) + " SCOOTS THROUGH FOR"
    If RN = 9 Then U$ = A1$(P1, I1) + " SCOOTS OUT FOR"
    If RN = 10 Then U$ = A1$(P1, I1) + " DASHES OUT FOR"
    If RN = 11 Then U$ = A1$(P1, I1) + " SCAMPERS THROUGH FOR"
    If RN = 12 Then U$ = A1$(P1, I1) + " SCAMPERS OUT FOR"
    If RN = 13 Then U$ = A1$(P1, I1) + " SCAMPERS FOR"
    If RN = 14 Then U$ = A1$(P1, I1) + " SCAMPERS AHEAD FOR"
    If RN = 15 Then U$ = A1$(P1, I1) + " SLIPS THROUGH FOR"
    If RN = 16 Then U$ = A1$(P1, I1) + " SLIPS OUT FOR"
    If RN = 17 Then U$ = A1$(P1, I1) + " SLIPS AHEAD FOR"
    If RN = 18 Then U$ = A1$(P1, I1) + " SKIRTS OUTSIDE FOR"
    If RN = 19 Then U$ = A1$(P1, I1) + " SKIRTS OUT FOR"
    If RN = 20 Then U$ = A1$(P1, I1) + " TURNS THE CORNER FOR"
    If RN = 21 Then U$ = A1$(P1, I1) + " TURNS UPFIELD FOR"
    If RN = 22 Then U$ = A1$(P1, I1) + " GETS THE CORNER FOR"
    If RN = 23 Then U$ = A1$(P1, I1) + " GETS UPFIELD FOR"

6617 End Sub

Sub PCPBP ()
    Call QBTHROW

    4150 If FF% <> 0 Then 4165
    If P2 = 13 And RY < 3 Then CP = Int(Rnd(1) * 4) - 7: GoTo 4165
    If P2 = 13 Then CP = (Int(Rnd(1) * 4) + 1) * -1: GoTo 4165
    If (P2 = 9 Or P2 = 10) And RY < 3 Then CP = Int(Rnd(1) * 7) - 2: GoTo 4165
    If (P2 = 9 Or P2 = 10) Then CP = Int(Rnd(1) * 6) + 2: GoTo 4165
    If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18) And RY < 4 Then CP = Int(Rnd(1) * 5) + 3: GoTo 4165
    If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18) And RY < 6 Then CP = Int(Rnd(1) * 9) + 6: GoTo 4165
    If (P2 = 11 Or P2 = 14 Or P2 = 16 Or P2 = 17 Or P2 = 18) Then CP = Int(Rnd(1) * 9) + 8: GoTo 4165
    If (P2 = 12 Or P2 = 15) And RY < 3 Then CP = Int(Rnd(1) * 5) + 3: GoTo 4165
    If (P2 = 12 Or P2 = 15) And RY < 5 Then CP = Int(Rnd(1) * 10) + 10: GoTo 4165
    CP = Int(Rnd(1) * 36) + 20

    4165 PC = CP: If CP > G Then CP = G: IT = 1
    YT = 100 - Y - CP: Call YARDLINE: Call MARKER
    If YT <= 0 Then U$ = RV$ + " MAKES THE CATCH IN THE END ZONE!!!": Call PBP: GoTo 4175
    RN = Int(Rnd(1) * 100) + 1
    If (P2 = 9 Or P2 = 10) And RN <= 33 Then U$ = RV$ + " HAS IT OVER THE MIDDLE"
    If (P2 = 9 Or P2 = 10) And RN > 33 And RN <= 67 Then U$ = RV$ + " CATCHES IT IN THE FLAT"
    If (P2 = 9 Or P2 = 10) And RN > 67 Then U$ = "HE DUMPS IT TO " + RV$
    If (P2 = 11 Or P2 = 12 Or P2 = 14 Or P2 = 15) And RN <= 33 Then U$ = RV$ + " TAKES IT OVER THE MIDDLE"
    If (P2 = 11 Or P2 = 12 Or P2 = 14 Or P2 = 15) And RN > 33 And RN <= 67 Then U$ = "HE HAS " + RV$
    If (P2 = 11 Or P2 = 12 Or P2 = 14 Or P2 = 15) And RN > 67 Then U$ = "HE COMPLETES IT TO " + RV$
    If P2 = 16 And RN <= 50 Then U$ = "HE HAS " + RV$ + " NEAR THE SIDELINE"
    If P2 = 16 And RN > 50 Then U$ = "HE FINDS " + RV$
    If P2 = 13 And RN <= 33 Then U$ = "HE SCREENS IT TO " + RV$
    If P2 = 13 And RN > 33 And RN <= 67 Then U$ = "HE DUMPS THE SCREEN TO " + RV$
    If P2 = 13 And RN > 67 Then U$ = "HE HAS " + RV$ + " ON THE SCREEN PASS"
    If (P2 = 17 Or P2 = 18) And RN <= 50 Then U$ = "HE HITS " + RV$
    If (P2 = 17 Or P2 = 18) And RN > 50 And RN <= 90 Then U$ = "HE FIRES IT TO " + RV$
    If FF% <> 0 Or (P2 = 17 Or P2 = 18) And RN > 90 Then U$ = RV$ + " MAKES A GREAT CATCH"
    U$ = U$ + " AT THE" + Str$(Int(YL)) + " YARD LINE"
    If IT = 1 Then IT = 0: Call PBP: Call PBP: GoTo 4175
    Call PBP: Call PBP: SY = 100 - Y - CP: EY = 100 - Y - G: Call MOVEBALL

    4175 Rem
End Sub


Function FNP (X)
    FNP = 11 + Int((X + Int(X / 5) - 1) / 2)
End Function

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
