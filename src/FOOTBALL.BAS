
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'GameRoutines.BI'

_Title "3-in-1 Football - GRIDIRON!"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

Dim Shared A$, A1$, A2$, A3$, A4$, A5$, A6$, A7$, AA$, D2$, DI$, DN$, DR$, DS$, DV$
Dim Shared F$, FF$, G$, strG9$, I$, LO$, NM$, NN$, PS$, Q$, RP$, RV$, NY$
Dim Shared SD$, SITE$, ST$, SX$, U$, U5$, X$, Y8$, Y9$, YN$
 
Dim Shared A$(1), A1$(1, 10), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2), A7$(1, 1)
Dim Shared AA$(1), B$(1), D$(15), D1$(11), D2$(15), DI$(1, 9), DN$(4), DR$(1), DS$(1, 14), DV$(1)
Dim Shared G$(3), strG9$(5), H$(120), HO$(120), strIR$(1, 9), strKR$(1, 2)
Dim Shared strLC$(1, 20, 1), LF$(1, 1), LI$(1, 9, 1), LK$(1, 2, 1), LO$(2), LP$(1, 3, 1), strLR$(1, 9, 1)
Dim Shared O$(50), NN$(1), P$(2), PK$(1, 2, 1), strPR$(1, 2), PS$(9), Q$(203), strQB$(1, 3)
Dim Shared R$(14), strRB$(1, 17), RP$(30), SD$(3), SITE$(120), SX$(32, 1)
Dim Shared U5$(3), V$(500), strWR$(1, 20), Y$(1), Y8$(3), Y9$(5), YN$(1), YR%(1)

Dim Shared A, A2, A3, A4, A5, A6, A7, A8, A9, AP, ATT
Dim Shared B, B1, B2, B3, B4, B5, B7, B8, BW, CP, CT
Dim Shared D, D1, D2, D3, DDI, DDS, DI, DR, DS, DT, E, EY
Dim Shared F2, F8, F9, G, G9, I1, I2, I3, I4, I5, I6, I7, I8, I9, I
Dim Shared J, J6, JJ, K3, N, NT, O, OT, P1, P2, PN, PQ, PR, PW, Q, QB, R1, RP, RY
Dim Shared S, S2, S3, S6, SN, SY, T, T1, TE, TMT, TP, W5, WE, WS
Dim Shared X, X1, X2, XD, XE, Y, Y1, Y9, YC, YF, YL, YT, Z1
Dim Shared displayPause As Single

Dim Shared A(1, 9), A1(1, 9), A2(1, 9), A3(1, 9), A4(1, 9), A5(1, 9), A6(1, 9), A7(1, 9), A8(1, 9), A9(1, 2)
Dim Shared AF(1, 1, 4), AM(1, 1, 4), B(1, 2), B1(1, 2), B2(1, 2), B3(1, 2), B4(1, 1), B5(1, 1), B6(1, 7), B7(1, 1), B8(1, 1)
Dim Shared C(50), CRD(120), DDI(1), DDS(1), DI(1, 9), DS(1, 14), F(4), F1(4), FA(1, 1, 4), FL(1, 1), FM(1, 1, 4)
Dim Shared GI(1, 9, 2), GS(1, 14, 1), IR(1, 9), J6(1), K(1, 36), K1(50, 6), K2(1, 13, 17), K3(1, 6), KR(1, 2)
Dim Shared LC(1, 20), LF(1, 1), LI(1, 9), LK(1, 2), LP(1, 3), LR(1, 9), OT(1), P(2), PK(1, 2), PR(1, 2)
Dim Shared QB(1, 3), RB(1, 17), S(1, 10), SI(1, 9, 2), SK(1, 14, 1), T(50), T1(1), TP(1)
Dim Shared W6(1, 1), WR(1, 20), XD(1), YC(1), Z(38), Z1(38), Z2(13, 17)

Dim Shared AP%(2), intB8%(2), BY%(38, 4), D3%(7, 10), intDI%(1), intDS%(1)
Dim Shared intG9%(1), GL%(31, 2), HB%(1)
Dim Shared NG%(20), O%(120), PA%(2, 9), PC%(1, 9), intPR%(1, 120), PS%(2, 21)
Dim Shared Q6%(1), Q7%(1), QR%(50, 2), QX%(1, 3), R9%(1), RM%(1, 14), RN%(1, 38), RQ%(1), RV%(1)
Dim Shared S1%(3, 10, 11), S2%(5, 10, 14), ST%(32), SX%(32, 2, 14), TE%(1), TF%(1), Time%(120)
Dim Shared teamIndex%(500), V4%(1, 3), WX%(6)

Dim Shared AP%, intB8%, BO%, C%, EG%, F%, GL%, H%, HB%, HT%, intI%, intDI%, intDS%
Dim Shared PA%, PC%, PS%, Q6%, Q7%, QBN%, QX%, R5%, RF%, S%, S1%, S2%, ST%, SX%, TS%
Dim Shared W%, WX%, intY8%, intYL%

'----------------------------------------
'           GAME CODE
'----------------------------------------

'Initialize variables
'--------------------
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

' Setup Game
'--------------------
Cls

Randomize Timer

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
Input #1, displayPause
Input #1, R5%
Input #1, Y8
Input #1, Y9
Input #1, NG%(14)
Close 1

_Blink Off

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

23 If _FileExists("GAME82") Then
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
End If

28 If AP = 1 Then
    displayPause = NG%(4) / 10
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
Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; displayPause
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

If I = 0 Then Open "DEFAULT" For Output As #1: Print #1, J6(0): Print #1, G9: Print #1, H%: Print #1, displayPause: Print #1, R5%: Print #1, Y8: Print #1, Y9: Print #1, NG%(14): Close 1: GoTo 60
If I = 1 Then J6(0) = J6(0) + 1: If J6(0) > 3 Then J6(0) = 0
If I = 1 Then 60

If I = 2 Then I = 0: GoSub 100: GoTo 60
If I = 3 Then I = 1: GoSub 100: GoTo 60

If I = 4 Then G9 = G9 + 1: If G9 = 6 Then G9 = 0
If I = 5 Then H% = H% + 1: If H% = 3 Then H% = 1

36 If I = 6 Then Locate 8, 32: Input displayPause: If displayPause < 0 Then 36
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
Cls
Color 3
JJ = 6
SN = 1

'TICKER PROMPT
Locate 8, 8: Color 14, 1

'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
    Locate 8 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 9 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

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
    Call GETTEAM
    GoTo 122

    100 Color 15, 0
    Cls
    JJ = 9
    SN = 1
    Locate 6, 8: Color 15, 4

    Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";
    For II = 1 To JJ
        Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
    Next
    Locate 7 + JJ, 8

    Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

    Locate 7, 32
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

    For I1 = 1 To 500
        V$(I1) = ""
    Next

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

3363 Call SCOREBOARD: U$ = "HALF TIME"
Call PBP
W% = 1 - W%
Call HALFSTART
X = 0
HT% = 1
GoSub 8530

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
Q = Q - 1: U$ = "END OF GAME": Call PBP 'Print " END OF GAME"
' -- this code below should be enough to prompt us for a key press before end of game
'CALL DECIDE
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

'Determine attendance randomly
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
For I = 0 To 2: Print #2, AP%(I): Next
Close 2

9135 Cls

9138 Call SAVESTATS
If AP = 1 Then 8083
If D% = 3 Then Run "HELLO.EXE"
Cls ': Print "PUT IN 3 IN 1 GAME DISK & HIT ANY KEY"
'I$ = GetKeyPress$

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


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'GameRoutines.BM'
