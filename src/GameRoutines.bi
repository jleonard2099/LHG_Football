Declare Function FNP(X)
Declare Sub ADDBACKS ()
Declare Sub ADDTIME ()
Declare Sub BLITZ ()
Declare Sub BLITZ2 ()
Declare Sub BOX2SCREEN ()
Declare Sub BOXDRAW (J)
Declare Sub CHANGEPOSS ()
Declare Sub CHANGEQB()
Declare Sub COMPCHEAT ()
Declare Sub DBOX ()
Declare Sub DECIDE ()
Declare Sub DEFENSE ()
Declare Sub DEFMENU ()
Declare Sub DISPFIELD ()
Declare Sub DISPWEATHER ()
Declare Sub FINDSPACE ()
Declare Sub FINDTEAMS ()
Declare Sub GETSPEED ()
Declare Sub GETTEAM ()
Declare Sub GOALLINE ()
Declare Sub HALFSTART ()
Declare Sub INCOMPLETE ()
Declare Sub KICKER ()
Declare Sub LGPBP ()
Declare Sub MARKER ()
Declare Sub MARKER1 ()
Declare Sub MEASUREMENT ()
Declare Sub MOVEBALL ()
Declare Sub NAMEEDIT ()
Declare Sub OFFMENU ()
Declare Sub OVERUSE ()
Declare Sub PASSMENU ()
Declare Sub PASSPBP ()
Declare Sub PBOX ()
Declare Sub PBP ()
Declare Sub PBPINT ()
Declare Sub PCPBP ()
Declare Sub PLACEPBP ()
Declare Sub PRINTBOX ()
Declare Sub PUNTYARDS ()
Declare Sub QBBOX ()
Declare Sub QBSNEAK ()
Declare Sub SAVESTATS ()
Declare Sub SCOREBOARD ()
Declare Sub SCOUT ()
Declare Sub SCOUTREPT ()
Declare Sub SELECTTEAM ()
Declare Sub SETSCREEN ()
'Declare Sub SETUP ()
Declare Sub SETWEATHER ()
Declare Sub STATS2SCREEN ()
Declare Sub STUFFED ()
Declare Sub TICKER ()
Declare Sub TICKUPDATE ()
Declare Sub YARDLINE ()

Dim scheduleFile$

Dim Shared A$, A1$, A2$, A3$, A4$, A5$, A6$, A7$, AA$, D2$, DI$, DN$, DR$, DS$, DV$
Dim Shared F$, G$, strG9$, I$, LO$, NM$, NN$, PS$, Q$, RP$, RV$, NY$
Dim Shared SD$, SITE$, ST$, SX$, U$, U5$, X$, Y8$, Y9$, YN$

Dim Shared A$(0 To 1), A1$(1, 10), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2), A7$(1, 1)
Dim Shared AA$(1), B$(1), D$(15), D1$(11), D2$(15), DI$(1, 9), DN$(4), DR$(1), DS$(1, 14), diskPaths$(1)
Dim Shared G$(3), strG9$(5), H$(120), HO$(120), strIR$(1, 9), strKR$(1, 2)
Dim Shared strLC$(1, 20, 1), LF$(1, 1), LI$(1, 9, 1), LK$(1, 2, 1), LO$(2), LP$(1, 3, 1), strLR$(1, 9, 1)
Dim Shared O$(50), NN$(1), P$(2), PK$(1, 2, 1), strPR$(1, 2), PS$(9), Q$(203), strQB$(1, 3)
Dim Shared R$(14), strRB$(1, 17), RP$(30), SD$(3), SITE$(120), SX$(1 To 33, 0 To 1)
Dim Shared U5$(3), teamNames$(500), strWR$(1, 20), Y$(1), Y8$(3), Y9$(5), YN$(1), YR%(1)

Dim Shared A, A2, A3, A4, A5, A6, A7, A8, A9, AP
Dim Shared B, B1, B2, B3, B4, B5, B7, B8, BW, CP, CT
Dim Shared D, D1, D2, D3, DDI, DDS, DI, DR, DS, DT, E, EY
'Dim Shared F2, F8, F9, G, G9, I1, I2, I3, I4, I5, I6, I7, I8, I9, I
'Dim Shared J, J6, JJ, K3, N, NT, O, OT, P1, P2, PN, PQ, PR, PW, Q, QB, R1, RP, RY
'Dim Shared S, S2, S3, S6, SN, SY, T, T1, TE, TMT, TP, W5, WE, WS
Dim Shared X, X1, X2, XD, XE, Y, Y1, Y9, YC, YF, YL, YT, Z1

Dim Shared pbpDelay!
Dim Shared ATT&

Dim Shared A(1, 9), A1(1, 9), A2(1, 9), A3(1, 9), A4(1, 9), A5(1, 9), A6(1, 9), A7(1, 9), A8(1, 9), A9(1, 2)
Dim Shared AF(1, 1, 4), AM(1, 1, 4), B(1, 2), B1(1, 2), B2(1, 2), B3(1, 2), B4(1, 1), B5(1, 1), B6(1, 7), B7(1, 1), B8(1, 1)
Dim Shared C(50), CRD(120), DDI(1), DDS(1), DI(1, 9), DS(1, 14), F(4), F1(4), FA(1, 1, 4), FL(1, 1), FM(1, 1, 4)
Dim Shared GI(1, 9, 2), GS(1, 14, 1), IR(1, 9), J6(1), K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6), KR(1, 2)
Dim Shared LC(1, 20), LF(1, 1), LI(1, 9), LK(1, 2), LP(1, 3), LR(1, 9), OT(1), P(2), PK(1, 2), PR(1, 2)
Dim Shared QB(1, 3), RB(1, 17), S(0 To 1, 0 To 10), SI(1, 9, 2), SK(1, 14, 1), T(50), T1(1), TP(1)
Dim Shared W6(1, 1), WR(1, 20), XD(1), YC(1), Z(38), Z1(38), Z2(13, 17)

Dim Shared AP%(2), intB8%(2), BY%(38, 4), D3%(0 To 8, 0 To 10), intDI%(1), intDS%(1)
Dim Shared intG9%(1), GL%(1 To 30, 1 To 2), HB%(1)
Dim Shared NG%(20), O%(120), PA%(2, 9), PC%(1, 9), intPR%(1, 120), PS%(2, 21)
'Dim Shared Q6%(1), Q7%(1), QR%(50, 2), QX%(1, 3), R9%(1), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(1 To 32), SX%(1 To 33, 0 To 1, 0 To 14), TE%(1), TF%(1), Time%(120)
Dim Shared teamIndex%(500), V4%(1, 3), WX%(6)

Dim Shared AP%, intB8%, BO%, C%, EG%, F%, GL%, H%, HB%, HT%, intI%, intDI%, intDS%
Dim Shared PA%, PC%, PS%, Q6%, Q7%, QBN%, QX%, R5%, RF%, S%, S1%, S2%, ST%, SX%, TS%
Dim Shared W%, WX%, intY8%, intYL%




Dim Shared F2, F8, F9, G, G9, I1, I2, I3, I4, I5, I6, I7, I8, I9, I
Dim Shared J, J6, JJ, K3, N, NT, O, OT, P1, P2, PN, PQ, PR, PW, Q, QB, R1, RP, RY
Dim Shared S, S2, S3, S6, SN, SY, T, T1, TE, TMT, TP, W5, WE, WS
Dim Shared Q6%(1), Q7%(1), QR%(50, 2), QX%(1, 3), R9%(1), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)

On Error GoTo Errhandler

'----------------------------------------
'           GAME CODE
'----------------------------------------
_Title "3-in-1 Football - GRIDIRON!"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

Color 7, 0
Cls

Randomize Timer

_Blink Off


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
