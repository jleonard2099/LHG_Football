'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - View Stat Leaders"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB CATEGORIES ()
DECLARE SUB BUILDSTATS ()

'Dim Z(38), Z1(38)
Dim TT(30, 30), TT$(30, 30), Z1$(30)
Dim menuItems$(7)
'Dim PP(120) As Single, FF(120) As Single, YY(120) As Single
Dim II(120) As Single
'Dim A1$(1, 10), A2$(1, 6), A3$(1, 4), A4$(1, 3), A5$(1, 3), A6$(1, 1), A7$(1, 2)
'Dim DI$(1, 10), GI(1, 9, 2), DS$(1, 15), GS(1, 14, 1), Z2(14, 18), LC(16), LC$(16, 1)
Dim A1L$(300), A1T$(300), LRL$(300), A1L(300, 8), A2L$(180), A2T$(180), LCL$(480), A2L(180, 4)
Dim A3R(120, 1), QBL$(120), A3T$(120), LPL$(120), A3L(120, 13) As Single, A4L$(90), A4T$(90), LKL$(90), A4L(90, 4)
Dim A5L$(90), A5T$(90), PKL$(90), A5L(90, 4), A6L$(30), A6T$(30), A6L(30, 2)
Dim A7L$(60), A7T$(60), A7L(60, 5)
Dim A8L$(300), A8T$(300), A8L(300)
'Dim D As Single
Dim A9L$(450), A9T$(450), A9L(450)
'Dim PK(2), LK(2), LK$(2, 1), PK$(2, 1), DV$(2), LR(9), LR$(9, 1)
Dim TYY$(480)
'Dim LP(3), LP$(3, 1), A$(1), GMA(300), GMB(180), GMC(120)

Dim Shared DV$, LG$, JA, JB, JC, JD, JE, JF, JG, JH, JI, TS, JJ
Dim Shared TT, TT$, TMR, A1L$, A1T$, LRL$, LCL$, GMA
Dim Shared A1L, A2L$, A2T$, GMB, A2L
Dim Shared QBL$, A3T$, LPL$, A3R, GMC, A3L As Single
Dim Shared A4L$, A4T$, LKL$, A4L, A5L$, A5T$, PKL$, A5L
Dim Shared A6L$, A6T$, A6L
Dim Shared A7L$, A7T$, A7L, A8L$, A8T$, A8L
Dim Shared A9L$, A9T$, A9L, Z1$
Dim Shared PP As Single, FF As Single, II As Single, YY As Single
Dim Shared A1$, A2$, A3$, A4$, A5$, A6$, A7$
Dim Shared DI$, GI, DS$, GS, Z2, LC, LC$
Dim Shared A$, Z, Z1, LR, LR$, LP, LP$, LK, LK$, PK, PK$
Dim Shared LF(1), AM(1, 4), AF(1, 4), TP$(74), Y%(30), Y%, LF$(1)

'STACK 0
Open "DEV.BAS" For Input As #2: For X = 0 To 2: Input #2, DV$(X): Next: Close 2

ReDim TPP$(480), TMM$(480), TYP(480) As Single
TMR = 1660
TS = 0
PV = 0

Call CATEGORIES

5 Call TITLE
menuItems$(1) = "VIEW EXPANDED PRO LEAGUE FILES  "
menuItems$(2) = "VIEW EXPANDED COLLEGE CONFERENCE"
menuItems$(3) = "RETURN TO LEAGUE LEADER MENU    "
JA = 0: JB = 0: JC = 0: JD = 0: JE = 0: JF = 0: JG = 0: JH = 0: JI = 0: JJ = 0: TS = 0
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 11, 57, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SCANCODE% = 3 Then Run "HELLO.EXE  3"
I = SELECTION%
CT% = I

2500 For X = 1 To 30
    Z1$(X) = "XXX"
Next
Spec$ = DV$(2) + "*.TMS": GoSub 1440
LG$ = F$
Open DV$(2) + F$ + ".TMS" For Input As #2
If I = 1 Then Input #2, LGF$: Input #2, D1$: Input #2, D2$: Input #2, D3$: For X = 1 To 30: Input #2, Z1$(X): Next
If I = 2 Then Input #2, LGF$: Input #2, D1$: Input #2, D2$: For X = 1 To 30: Input #2, Z1$(X): Next
Close 2
Cls
Call BUILDSTATS
Call SAVELEADER

170 For I = 1 To 30: TMM$(I) = "": Next
GoSub 10100: RR = 0: Locate 2, 24: Color 4, 15: Print "LEADER SELECTION MENU": Print
Locate , 20: Color 14: Print "(A) ";: Color 1: Print "VIEW RUSHING LEADERS"
Locate , 20: Color 14: Print "(B) ";: Color 1: Print "VIEW RECEIVING LEADERS"
Locate , 20: Color 14: Print "(C) ";: Color 1: Print "VIEW QUARTERBACK LEADERS"
Locate , 20: Color 14: Print "(D) ";: Color 1: Print "VIEW KICKOFF RETURN LEADERS"
Locate , 20: Color 14: Print "(E) ";: Color 1: Print "VIEW PUNT RETURN LEADERS"
Locate , 20: Color 14: Print "(F) ";: Color 1: Print "VIEW PUNTING LEADERS"
Locate , 20: Color 14: Print "(G) ";: Color 1: Print "VIEW PLACE KICKING LEADERS"
Locate , 20: Color 14: Print "(H) ";: Color 1: Print "VIEW INTERCEPTION, SACK LEADERS"
Locate , 20: Color 14: Print "(I) ";: Color 1: Print "VIEW ALL-PURPOSE LEADERS"
Locate , 20: Color 14: Print "(J) ";: Color 1: Print "VIEW SCORING LEADERS"
Locate , 20: Color 14: Print "(K) ";: Color 1: Print "VIEW TEAM TOTALS"
Locate , 20: Color 14: Print "(L) ";: Color 1: Print "RETURN TO MAIN MENU"
171 I$ = GetKeyPress$
If UCase$(I$) = "A" Then 180
If UCase$(I$) = "B" Then 190
If UCase$(I$) = "C" Then 200
If UCase$(I$) = "D" Then 210
If UCase$(I$) = "E" Then 220
If UCase$(I$) = "F" Then 230
If UCase$(I$) = "G" Then 240
If UCase$(I$) = "H" Then 250
If UCase$(I$) = "I" Then 300
If UCase$(I$) = "J" Then 270
If UCase$(I$) = "K" Then 280
If UCase$(I$) = "L" Then 5
GoTo 170

180 Color , 0: Cls: Print: Print: Print: Print: J = 8: ZZ = 1
Locate 2, 22: Color 0, 4: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15

182 For I = 0 To 4: Color 14: Locate I + 3, 24: Print Using "##"; I;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 5 Then 182
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 0: GoTo 400
If B9 = 1 Then X = 1: XX = 1: GoTo 400
If B9 = 2 Then X = 0: X1 = 1: XX = 2: GoTo 420
If B9 = 3 Then X = 2: XX = 3: GoTo 400
If B9 = 4 Then X = 6: XX = 4: GoTo 430

190 Color , 0: Cls: Print: Print: Print: Print: J = 8: ZZ = 1
'
Locate 2, 22: Color 0, 2: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 5 To 9: Color 14: Locate I - 2, 24: Print Using "##"; I - 5;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 5 Then 190
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 3: X1 = 0: XX = 5: GoTo 440
If B9 = 1 Then X = 4: X1 = 1: XX = 6: GoTo 440
If B9 = 2 Then X = 3: X1 = 4: X2 = 0: X3 = 1: XX = 7: GoTo 460
If B9 = 3 Then X = 5: X1 = 2: XX = 8: GoTo 440
If B9 = 4 Then X = 3: X1 = 7: XX = 9: GoTo 480

200 Color , 0: Cls: Print: Print: Print: Print: J = 12: ZZ = 1
'
Locate 2, 22: Color 0, 3: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 10 To 18: Color 14: Locate I - 7, 24: Print Using "##"; I - 10;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 8 Then 200
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 10: GoTo 500
If B9 = 1 Then X = 1: XX = 11: GoTo 500
If B9 = 2 Then X = 2: XX = 12: GoTo 500
If B9 = 3 Then X = 3: XX = 13: GoTo 500
If B9 = 4 Then X = 4: XX = 14: GoTo 500
If B9 = 5 Then X = 0: X1 = 1: XX = 15: GoTo 520
If B9 = 6 Then X = 1: X1 = 3: X2 = 0: XX = 16: GoTo 530
If B9 = 7 Then X = 0: XX = 17: GoTo 510
If B9 = 8 Then X = 5: XX = 18: GoTo 540

210 Color , 0: Cls: Print: Print: Print: Print: J = 8: ZZ = 1
'
Locate 2, 22: Color 0, 5: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 19 To 23: Color 14: Locate I - 16, 24: Print Using "##"; I - 19;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 4 Then 210
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 19: GoTo 550
If B9 = 1 Then X = 1: XX = 20: GoTo 550
If B9 = 2 Then X = 0: X1 = 1: XX = 21: GoTo 570
If B9 = 3 Then X = 2: XX = 22: GoTo 550
If B9 = 4 Then X = 3: XX = 23: GoTo 580

220 Color , 0: Cls: Print: Print: Print: Print: J = 8: ZZ = 1
'
Locate 2, 22: Color 0, 6: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 24 To 28: Color 14: Locate I - 21, 24: Print Using "##"; I - 24;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 4 Then 220
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 24: GoTo 600
If B9 = 1 Then X = 1: XX = 25: GoTo 600
If B9 = 2 Then X = 0: X1 = 1: XX = 26: GoTo 620
If B9 = 3 Then X = 2: XX = 27: GoTo 600
If B9 = 4 Then X = 3: XX = 28: GoTo 630

230 Color , 0: Cls: Print: Print: Print: Print: J = 6: ZZ = 1
'
Locate 2, 22: Color 0, 9: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 29 To 31: Color 14: Locate I - 26, 24: Print Using "##"; I - 29;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 2 Then 230
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 29: GoTo 650
If B9 = 1 Then X = 1: XX = 30: GoTo 650
If B9 = 2 Then X = 0: X1 = 1: XX = 31: GoTo 670

240 Color , 0: Cls: Print: Print: Print: Print: J = 10: ZZ = 1
'
Locate 2, 22: Color 0, 10: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 32 To 38: Color 14: Locate I - 29, 24: Print Using "##"; I - 32;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 6 Then 240
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 32: GoTo 680
If B9 = 1 Then X = 1: XX = 33: GoTo 680
If B9 = 2 Then X = 0: X1 = 1: XX = 34: GoTo 690
If B9 = 3 Then X = 2: XX = 35: GoTo 680
If B9 = 4 Then X = 3: XX = 36: GoTo 680
If B9 = 5 Then X = 2: X1 = 3: XX = 37: GoTo 690
If B9 = 6 Then X = 4: XX = 38: GoTo 680

250 Color , 0: Cls: Print: Print: Print: Print: J = 6: ZZ = 1
'
Locate 2, 22: Color 0, 12: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 39 To 40: Color 14: Locate I - 36, 24: Print Using "##"; I - 39;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 1 Then 250
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 0: XX = 39: GoTo 1700
If B9 = 1 Then X = 0: XX = 40: GoTo 1710

270 Color , 0: Cls: Print: Print: Print: Print: J = 6: ZZ = 1
'
Locate 2, 22: Color 0, 13: Print " Ö"; String$(36, 196); "· ";
For II = 1 To J: Locate 2 + II, 22: Print " º"; String$(36, 32); "º ";: Next
Locate 3 + J, 22: Print " Ó"; String$(36, 196); "½ ";: Locate 10, 15
For I = 41 To 43: Color 14: Locate I - 38, 24: Print Using "##"; I - 41;: Print " ";: Color 15: Print TP$(I): Next
Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 2 Then 270
stpl = 0: PPP = 0: GoSub 5000
If B9 = 0 Then X = 2: X1 = 5: XX = 41: KC = 1: GoTo 1720
If B9 = 1 Then XF = 0: XF1 = 2: XX = 42: KC = 2: GoTo 1720
If B9 = 2 Then X = 2: X1 = 5: XF = 0: XF1 = 2: XX = 43: KC = 3: GoTo 1720

280 Color , 0: Cls: Print: Print: Print: Print: J = 17: ZZ = 1
'
Locate 2, 2: Color 0, 14: Print " Ö"; String$(66, 196); "· ";
For II = 1 To J: Locate 2 + II, 2: Print " º"; String$(66, 32); "º ";: Next
Locate 3 + J, 2: Print " Ó"; String$(66, 196); "½ ";: Locate 10, 15
For I = 44 To 58: Color 14: Locate I - 41, 4: Print Using "##"; I - 44;: Print " ";: Color 15: Print TP$(I): Next
For I = 59 To 72: Color 14: Locate I - 56, 40: Print Using "##"; I - 44;: Print " ";: Color 15: Print TP$(I): Next
Print: Print: Locate , 25: Input "SELECT NUMBER TO VIEW"; B9
If B9 > 28 Then 280
GoSub 5000
If B9 = 0 Then X = 0: XX = 44: GoTo 1760
If B9 = 1 Then X = 1: X1 = 13: XX = 45: GoTo 1765
If B9 = 2 Then X = 2: X1 = 14: XX = 46: GoTo 1770
If B9 = 3 Then X = 1: X1 = 13: X2 = 2: X3 = 14: XX = 47: GoTo 1780
If B9 = 4 Then X = 6: XX = 48: GoTo 1760
If B9 = 5 Then X = 2: X1 = 6: XX = 49: GoTo 1770
If B9 = 6 Then X = 4: X1 = 3: XX = 50: GoTo 1800
If B9 = 7 Then X = 9: XX = 51: GoTo 1760
If B9 = 8 Then X = 10: XX = 52: GoTo 1760
If B9 = 9 Then X = 5: XX = 53: GoTo 1760
If B9 = 10 Then X = 5: X1 = 10: X2 = 20: X3 = 25: XX = 54: GoTo 1820
If B9 = 11 Then X = 11: XX = 55: GoTo 1760
If B9 = 12 Then X = 12: XX = 56: GoTo 1760
If B9 = 13 Then X = 13: XX = 57: GoTo 1760
If B9 = 14 Then X = 14: XX = 58: GoTo 1760
If B9 = 15 Then X = 15: XX = 59: GoTo 1760
If B9 = 16 Then X = 16: X1 = 28: XX = 60: GoTo 1765
If B9 = 17 Then X = 17: X1 = 29: XX = 61: GoTo 1770
If B9 = 18 Then X = 16: X1 = 28: X2 = 17: X3 = 29: XX = 62: GoTo 1780
If B9 = 19 Then X = 21: XX = 63: GoTo 1760
If B9 = 20 Then X = 17: X1 = 21: XX = 64: GoTo 1770
If B9 = 21 Then X = 19: X1 = 18: XX = 65: GoTo 1800
If B9 = 22 Then X = 24: XX = 66: GoTo 1760
If B9 = 23 Then X = 25: XX = 67: GoTo 1760
If B9 = 24 Then X = 20: XX = 68: GoTo 1760
If B9 = 25 Then X = 26: XX = 69: GoTo 1760
If B9 = 26 Then X = 27: XX = 70: GoTo 1760
If B9 = 27 Then X = 28: XX = 71: GoTo 1760
If B9 = 28 Then X = 29: XX = 72: GoTo 1760

300 Color 0, 0: Cls: GoSub 5000: Cls: PPP = 0: X = 1: X1 = 4: XX = 73
For I = 1 To JA
    If A1L(I, X) + A1L(I, X1) = 0 Then 309
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X) + A1L(I, X1)

309 Next I
For I = 1 To JB
    If A2L(I, X) = 0 Then 319
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A2L$(I): TMM$(PPP) = A2T$(I): TYP(PPP) = A2L(I, X)

319 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

400 Cls: PPP = 0
For I = 1 To JA
    If A1L(I, X) = 0 Then 409
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X)

409 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

420 Cls: PPP = 0
For I = 1 To JA
    If A1L(I, X) < Y%(1) * 10 Then 424
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X1) / A1L(I, X)

424 Next I
GoSub 5300
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

430 Cls: PPP = 0
For I = 1 To JA
    If A1L(I, X) = 0 Then 434
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X): TYY$(PPP) = LRL$(I)

434 Next I
GoSub 5300
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

440 Cls: PPP = 0
For I = 1 To JA
    If A1L(I, X) = 0 Then 449
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X)

449 Next I
For I = 1 To JB
    If A2L(I, X1) = 0 Then 454
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A2L$(I): TMM$(PPP) = A2T$(I): TYP(PPP) = A2L(I, X1)

454 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

460 Cls: PPP = 0
For I = 1 To JA
    If A1L(I, X) < Y%(1) * 4 Then 469
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X1) / A1L(I, X)

469 Next I
For I = 1 To JB
    If A2L(I, X2) < Y%(1) * 4 Then 474
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A2L$(I): TMM$(PPP) = A2T$(I): TYP(PPP) = A2L(I, X3) / A2L(I, X2)

474 Next I
GoSub 5300
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

480 Cls: PPP = 0
For I = 1 To JB
    If A2L(I, 0) = 0 Then 484
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A2L$(I): TMM$(PPP) = A2T$(I): TYP(PPP) = A2L(I, X): TYY$(PPP) = LCL$(I)

484 Next I
For I = 1 To JA
    If A1L(I, 4) = 0 Then 494
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = A1L(I, X1): TYY$(PPP) = LCL$(I)

494 Next I
GoSub 5300
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

500 Cls: PPP = 0
For I = 1 To JC
    If A3L(I, X) = 0 Then 509
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = A3L(I, X)

509 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

510 Cls: PPP = 0
For I = 1 To JC
    If A3L(I, X) < Y%(1) * 15 Then 519
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = A3R(I, X)

519 Next I
GoSub 5300
TVN = 4: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

520 Cls: PPP = 0
For I = 1 To JC
    If A3L(I, X) < Y%(1) * 15 Then 529
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    If A3L(I, X1) = 0 Then 527
    TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = (A3L(I, X1) / A3L(I, X)) * 100: GoTo 529

    527 TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = 0

529 Next I
GoSub 5300
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

530 Cls: PPP = 0
For I = 1 To JC
    If A3L(I, X2) < Y%(1) * 15 Then 539
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    If A3L(I, X1) = 0 Then 537
    TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = A3L(I, X1) / A3L(I, X): GoTo 539

    537 TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = 0

539 Next I
GoSub 5300
TVN = 4: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

540 Cls: PPP = 0
For I = 1 To JC
    If A3L(I, X) = 0 Then 544
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = QBL$(I): TMM$(PPP) = A3T$(I): TYP(PPP) = A3L(I, X): TYY$(PPP) = LPL$(I)

544 Next I
GoSub 5300
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

550 Cls: PPP = 0
For I = 1 To JD
    If A4L(I, X) = 0 Then 559
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A4L$(I): TMM$(PPP) = A4T$(I): TYP(PPP) = A4L(I, X)

559 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

570 Cls: PPP = 0
For I = 1 To JD
    If A4L(I, X) = 0 Then 579
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    If A4L(I, X1) = 0 Then 578
    TPP$(PPP) = A4L$(I): TMM$(PPP) = A4T$(I): TYP(PPP) = A4L(I, X1) / A4L(I, X): GoTo 579

    578 TPP$(PPP) = A4L$(I): TMM$(PPP) = A4T$(I): TYP = 0

579 Next I
GoSub 5300
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

580 Cls: PPP = 0
For I = 1 To JD
    If A4L(I, X) = 0 Then 584
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A4L$(I): TMM$(PPP) = A4T$(I): TYP(PPP) = A4L(I, X): TYY$(PPP) = LKL$(I)

584 Next I
GoSub 5300
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

600 Cls: PPP = 0
For I = 1 To JE
    If A5L(I, X) = 0 Then 609
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A5L$(I): TMM$(PPP) = A5T$(I): TYP(PPP) = A5L(I, X)

609 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

620 Cls: PPP = 0
For I = 1 To JE
    If A5L(I, X) = 0 Then 629
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    If A5L(I, X1) = 0 Then 628
    TPP$(PPP) = A5L$(I): TMM$(PPP) = A5T$(I): TYP(PPP) = A5L(I, X1) / A5L(I, X): GoTo 629

    628 TPP$(PPP) = A5L$(I): TMM$(PPP) = A5T$(I): TYP = 0

629 Next I
GoSub 5300
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

630 Cls: PPP = 0
For I = 1 To JE
    If A5L(I, X) = 0 Then 634
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A5L$(I): TMM$(PPP) = A5T$(I): TYP(PPP) = A5L(I, X): TYY$(PPP) = PKL$(I)

634 Next I
GoSub 5300
TVN = 3: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

650 Cls: PPP = 0
For I = 1 To JF
    If A6L(I, X) = 0 Then 659
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A6L$(I): TMM$(PPP) = A6T$(I): TYP(PPP) = A6L(I, X)

659 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

670 Cls: PPP = 0
For I = 1 To JF
    If A6L(I, X) = 0 Then 679
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    If A6L(I, X1) = 0 Then 678
    TPP$(PPP) = A6L$(I): TMM$(PPP) = A6T$(I): TYP(PPP) = A6L(I, X1) / A6L(I, X): GoTo 679

    678 TPP$(PPP) = A6L$(I): TMM$(PPP) = A6T$(I): TYP = 0

679 Next I
GoSub 5300
TVN = 2: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

680 Cls: PPP = 0
For I = 1 To JG
    If A7L(I, X) = 0 Then 689
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A7L$(I): TMM$(PPP) = A7T$(I): TYP(PPP) = A7L(I, X)

689 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

690 Cls: PPP = 0
For I = 1 To JG
    If A7L(I, X) = 0 Then 699
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    If A7L(I, X1) = 0 Then 698
    TPP$(PPP) = A7L$(I): TMM$(PPP) = A7T$(I): TYP(PPP) = (A7L(I, X) / A7L(I, X1)) * 100: GoTo 699

    698 TPP$(PPP) = A7L$(I): TMM$(PPP) = A7T$(I): TYP = 0

699 Next I
GoSub 5300
TVN = 4: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

700 GoSub 10000: R = Len(TP$(XX)): Locate 2, 11: Color 14, 1: Print "PLAYER             TEAM";: Locate , 60 - (Int(R) / 2): Print TP$(XX)
For W = 1 To QW
    RR = RR + 1
    If W = 17 Or W = 33 Or W = 49 Or W = 65 Or W = 81 Or W = 97 Or W = 113 Or W = 129 Or W = 145 Or W = 161 Then GoSub 731: GoSub 743
    If W = 177 Or W = 193 Or W = 209 Or W = 225 Or W = 241 Or W = 257 Or W = 273 Or W = 289 Or W = 305 Or W = 321 Then GoSub 731: GoSub 743
    If W = 337 Or W = 353 Or W = 369 Or W = 385 Or W = 401 Or W = 417 Or W = 433 Or W = 449 Or W = 465 Or W = 481 Then GoSub 731: GoSub 743
    If W = 497 Or W = 513 Or W = 529 Or W = 545 Or W = 561 Or W = 577 Or W = 593 Or W = 609 Or W = 625 Or W = 641 Then GoSub 731: GoSub 743
    If W = 657 Or W = 673 Or W = 689 Or W = 705 Or W = 721 Or W = 737 Or W = 753 Or W = 769 Then GoSub 731: GoSub 743
    If TVN = 1 Then 709
    If TVN = 2 Then 708
    If TVN = 3 Then 707
    If TVN = 4 Then 706

    706 Locate RR + 2, 7: Color 15: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.#"; TYP(W): GoTo 710

    707 Locate RR + 2, 7: Color 15: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.#"; TYP(W);: Print Tab(63); TYY$(W): GoTo 710

    708 Locate RR + 2, 7: Color 15: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.##"; TYP(W): GoTo 710

    709 Locate RR + 2, 7: Color 15: Print Using "###"; W;: Print " ";: Color 3: Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "#####"; TYP(W)

710 Next W
Locate 22, 17: Color 0, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";: Color 1, 0: Print "Ü"
Locate , 18: Print String$(50, 223)
I$ = GetKeyPress$
If I$ = Chr$(27) Then 170
Color 0, 0: Locate 22: Print "                                                                     "
Locate 23: Print "                                                                     "
Locate 22, 32: Color 0, 15: Print "VIEW AGAIN (Y/N)";: Color 1, 0: Print "Ü"
Locate , 33: Print String$(16, 223)
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then RR = 0: GoTo 700
GoTo 170

731 RR = 1: Locate 22, 17: Color 0, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";: Color 1, 0: Print "Ü"
Locate , 18: Print String$(50, 223)
I$ = GetKeyPress$
If I$ = Chr$(27) Then 170
Return

743 For I = 3 To 18: Color 1, 1
Locate I, 7: Print "                                                                  ": Next
Return

850 LPrint "     PLAYER                  TEAM                     "; TP$(XX)
For W = 1 To LG
    If W = 50 Or W = 100 Or W = 150 Or W = 200 Then GoSub 943
    If W = 250 Or W = 300 Or W = 350 Or W = 400 Then GoSub 943
    If W = 450 Or W = 500 Or W = 550 Or W = 600 Then GoSub 943
    If W = 650 Or W = 700 Or W = 750 Or W = 800 Then GoSub 943
    If W = 850 Or W = 900 Then GoSub 943
    If TVN = 1 Then 909
    If TVN = 2 Then 908
    If TVN = 3 Then 907
    If TVN = 4 Then 906

    906 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.#"; TYP(W): GoTo 910

    907 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.#"; TYP(W);: LPrint Tab(63); TYY$(W): GoTo 910

    908 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "###.##"; TYP(W): GoTo 910

    909 LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);: LPrint Tab(58); Using "#####"; TYP(W)

910 Next W
GoTo 170

943 LPrint Chr$(12)
LPrint "     PLAYER                  TEAM                     "; TP$(XX)
Return

1700 Cls: PPP = 0
For I = 1 To JH
    If A8L(I) = 0 Then 1702
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A8L$(I): TMM$(PPP) = A8T$(I): TYP(PPP) = A8L(I)

1702 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

1710 Cls: PPP = 0
For I = 1 To JI
    If A9L(I) = 0 Then 1712
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A9L$(I): TMM$(PPP) = A9T$(I): TYP(PPP) = A9L(I)

1712 Next I
GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

1720 If KC = 1 Or KC = 3 Then GoTo 1725
If KC = 2 Then GoTo 1740

1725 Cls: PPP = 0
For I = 1 To JA
    If A1L(I, X) + A1L(I, X1) = 0 Then 1732
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A1L$(I): TMM$(PPP) = A1T$(I): TYP(PPP) = (A1L(I, X) + A1L(I, X1)) * 6

1732 Next I
For I = 1 To JB
    If A2L(I, X) = 0 Then 1734
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A2L$(I): TMM$(PPP) = A2T$(I): TYP(PPP) = A2L(I, X) * 6

1734 Next I
If KC = 1 Then 1750
If KC = 3 Then 1742

1740 Cls: PPP = 0

1742 For I = 1 To JG
    If A7L(I, XF) + A7L(I, XF1) = 0 Then 1744
    PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
    TPP$(PPP) = A7L$(I): TMM$(PPP) = A7T$(I): TYP(PPP) = (A7L(I, XF) * 3) + A7L(I, XF1)

1744 Next I

1750 GoSub 5300
TVN = 1: QW = PPP: GoSub 8000
If PV = 1 Then 700
If PV = 2 Then 850

1760 For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X)
Next I
GoTo 1775

1765 For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) - TT(I, X1)
Next I
GoTo 1775

1770 For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X) + TT(I, X1)
Next I

1775 GoSub 5500
TVN = 1: QW = TS: GoSub 8000
If PV = 1 Then 3650
If PV = 2 Then 4650

1780 For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = (TT(I, X2) + TT(I, X3)) / (TT(I, X) - TT(I, X1))
Next I
GoTo 1805

1800 For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = (TT(I, X) / TT(I, X1)) * 100
Next I

1805 GoSub 5500
TVN = 2: QW = TS: GoSub 8000
If PV = 1 Then 3650
If PV = 2 Then 4650

1820 For I = 1 To TS
    TMM$(I) = TT$(I, X): TYP(I) = TT(I, X2) + TT(I, X3) - TT(I, X) - TT(I, X1)
Next I
GoTo 1775
GoTo 5

3650 GoSub 11000: Color 14, 4: R = Len(TP$(XX)): Locate 2, 40 - (Int(R) / 2): Print TP$(XX)
Locate 3, 11: Color 14, 4: Print "TEAM                               TEAM "
For W = 1 To 15
    If TVN = 1 Then 3709
    If TVN = 2 Then 3708

    3708 If TMM$(W) = "" Then 3710
    Locate W + 3, 7: Color 12: Print Using "###"; W;: Print " ";: Color 15: Print TMM$(W);: Print Tab(29); Using "##.#"; TYP(W): GoTo 3710

    3709 If TMM$(W) = "" Then 3710
    Locate W + 3, 7: Color 12: Print Using "###"; W;: Print " ";: Color 15: Print TMM$(W);: Print Tab(28); Using "#####"; TYP(W)

3710 Next W
For W = 16 To 30
    If TVN = 1 Then 3712
    If TVN = 2 Then 3711

    3711 If TMM$(W) = "" Then 3713
    Locate W - 12, 42: Color 12: Print Using "###"; W;: Print " ";: Color 15: Print TMM$(W);: Print Tab(64); Using "##.#"; TYP(W): GoTo 3713

    3712 If TMM$(W) = "" Then 3713
    Locate W - 12, 42: Color 12: Print Using "###"; W;: Print " ";: Color 15: Print TMM$(W);: Print Tab(63); Using "#####"; TYP(W)

3713 Next W
Locate 22, 17: Color 0, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";: Color 4, 0: Print "Ü"
Locate , 18: Print String$(50, 223)
I$ = GetKeyPress$
If I$ = Chr$(27) Then 170
Color 0, 0: Locate 22: Print "                                                                    "
Locate 23: Print "                                                                    "
Locate 22, 32: Color 0, 15: Print "VIEW AGAIN (Y/N)";: Color 4, 0: Print "Ü"
Locate , 33: Print String$(16, 223)
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then RR = 0: GoTo 3650
GoTo 170
Locate 22: Color 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
I$ = GetKeyPress$
If I$ = Chr$(27) Then 170
Return
Cls: Color 14: Print "     TEAM                     "; TP$(XX)
Return
LPrint "     TEAM                     "; TP$(XX)
For W = 1 To QW
    If TVN = 1 Then 3909
    If TVN = 2 Then 3908

    3908 Color 12: LPrint Using "###"; W;: LPrint " ";: Color 3: LPrint TMM$(W);: Color 15: LPrint Tab(33); Using ".###"; TYP(W): GoTo 3910

    3909 Color 12: LPrint Using "###"; W;: LPrint " ";: Color 3: LPrint TMM$(W);: Color 15: LPrint Tab(32); Using "#####"; TYP(W)

3910 Next W
GoTo 170

4650 LPrint "     TEAM                     "; TP$(XX)
For W = 1 To LG
    If TVN = 1 Then 4709
    If TVN = 2 Then 4708
    If TVN = 3 Then 4707
    If TVN = 4 Then 4706

    4706 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(35); Using "###.#"; TYP(W): GoTo 4710

    4707 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(35); Using "###.#"; TYP(W);: LPrint Tab(63); TYY$(W): GoTo 4710

    4708 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(35); Using "###.##"; TYP(W): GoTo 4710

    4709 LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);: LPrint Tab(35); Using "#####"; TYP(W)

4710 Next W
GoTo 170

5000 Print: Print: Print: Print: J = 6: GoSub 5007
Color 3: Locate , 30: Print "SELECT OPTION:"
Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"
5002 I$ = GetKeyPress$
If UCase$(I$) = "H" Then GoSub 7000: GoTo 5015
If UCase$(I$) = "L" Then GoSub 7000: GoTo 5016
If UCase$(I$) = "R" Then GoTo 5017
If UCase$(I$) <> "H" And UCase$(I$) <> "L" And UCase$(I$) <> "R" Then 5002

5007 '
Locate 8, 8: Color 15, 1: Print " Ö"; String$(57, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(57, 32); "º ";: Next
Locate 9 + J, 8: Print " Ó"; String$(57, 196); "½ ";: Locate 10, 15: Return

5015 JX = 1: GoTo 5030

5016 JX = 2: GoTo 5030

5017 If ZZ = 1 GoTo 170

5030 Return

5300 For I = 2 To PPP: stpl = stpl + 1: Locate 5, 5: Print "sorting player "; stpl
    For J = PPP To I Step -1
        If JX = 1 Then 5364
        If JX = 2 Then 5365

        5364 If TYP(J - 1) < TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J): Swap TYY$(J - 1), TYY$(J)
        GoTo 5366

        5365 If TYP(J - 1) > TYP(J) Then Swap TPP$(J - 1), TPP$(J): Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J): Swap TYY$(J - 1), TYY$(J)

    5366 Next J
Next I
Return

5500 Cls: For I = 2 To TS: stpl = stpl + 1: Locate 5, 5: Print "sorting team "; stpl
    For J = TS To I Step -1
        If JX = 1 Then 5505
        If JX = 2 Then 5506

        5505 If TYP(J - 1) < TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)
        GoTo 5507

        5506 If TYP(J - 1) > TYP(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP(J - 1), TYP(J)

    5507 Next J
Next I
Return

7000 Print: Print: Print: Print: Print: J = 4: GoSub 7007: Color 12: Locate , 30: Print "SELECT OPTION:"
Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"
7002 I$ = GetKeyPress$
If UCase$(I$) = "P" Then PV = 2: GoTo 7015
If UCase$(I$) = "V" Then PV = 1: GoTo 7015
If UCase$(I$) = "R" Then GoTo 5017
If UCase$(I$) <> "P" And UCase$(I$) <> "V" And UCase$(I$) <> "R" Then 7002

7007 '
Locate 9, 15: Color 15, 0: Print " Ö"; String$(40, 196); "· ";
For II = 1 To J: Locate 9 + II, 15: Print " º"; String$(40, 32); "º ";: Next
Locate 10 + J, 15: Print " Ó"; String$(40, 196); "½ ": Locate 10, 10

7015 Return

8000 Color , 0: Cls: If PV = 1 Then 8015
Print: Print: Print: Print: Print: J = 2: GoSub 8007
Locate , 25: Color 1: Print "MAXIMUM = "; QW
Locate , 25: Input "ENTER LENGTH OF LIST"; LG
If LG > QW Then 8000
GoTo 8015

8007 '
Locate 10, 20: Color 4, 15: Print " Ö"; String$(30, 196); "· ";
For II = 1 To J: Locate 10 + II, 20: Print " º"; String$(30, 32); "º ";: Next
Locate 11 + J, 20: Print " Ó"; String$(30, 196); "½ ": Locate 11

8015 Return

10000 Cls: J = 18
Locate 1, 5: Color 15, 1: Print " Ö"; String$(67, 196); "· ";
For II = 1 To J: Color 15, 1: Locate 1 + II, 5: Print " º"; String$(67, 32); "º ";: Color 7, 1: Print String$(2, 177): Next
Color 15, 1: Locate 1 + J, 5: Print " Ó"; String$(67, 196); "½ ";: Color 7, 1: Print String$(2, 177)
Locate , 7: Print String$(71, 177)

10001 Return

10100 Color 0, 0: Cls: J = 15
Locate 1, 15: Color 4, 15: Print " Ö"; String$(40, 196); "· ";
For II = 1 To J: Color 4, 15: Locate 1 + II, 15: Print " º"; String$(40, 32); "º ";: Color 7, 4: Print String$(2, 177): Next
Color 4, 15: Locate 1 + J, 15: Print " Ó"; String$(40, 196); "½ ";: Color 7, 4: Print String$(2, 177)
Locate , 17: Print String$(44, 177)

10101 Return

11000 Cls: J = 18
Locate 1, 5: Color 15, 4: Print " Ö"; String$(67, 196); "· ";
For II = 1 To J: Color 15, 4: Locate 1 + II, 5: Print " º"; String$(67, 32); "º ";: Color 7, 4: Print String$(2, 177): Next
Color 15, 4: Locate 1 + J, 5: Print " Ó"; String$(67, 196); "½ ";: Color 7, 4: Print String$(2, 177)
Locate , 7: Print String$(71, 177)

11001 Return

1440 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4600
ReDim F$(0 To Count%)
For X = 1 To Count%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1445 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT CONFERENCE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS CONFERENCE";

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 5
F$ = F$(SELECTION%)
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1445

3913 F$ = RTrim$(F$)
F$ = Left$(F$, Len(F$) - 4)
Return

4600 Locate 10, 8
Print "NO CONFERENCE FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
I$ = GetKeyPress$
GoTo 5

Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,RUSING TOUCHDOWNS,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING AVERAGE,RECEIVING TOUCHDOWNS,LONGEST RECEPTION
Data PASS ATTEMPTS,PASS COMPLETIONS,INTERCEPTIONS,PASSING YARDS,TOUCHDOWN PASSES,COMPLETION PERCENTAGE,AVERAGE YDS/COMP.,QUARTERBACK RATING,LONGEST PASS
Data KICKOFF RETURNS,KICKOFF RETURN YARDS,AVERAGE GAIN/KO RETURN,KICKOFF RETURN TD'S,LONGEST KICKOFF RETURN
Data PUNT RETURNS,PUNT RETURN YARDS,AVERAGE GAIN/PUNT RETURN,PUNT RETURN TD'S,LONGEST PUNT RETURN
Data PUNTS,PUNTING YARDS,AVERAGE PER PUNT
Data F.G.'s MADE,F.G. ATTEMPES,F.G. PERCENTAGE,E.P.'s MADE,E.P. ATTEMPTS,E.P. PERCENTAGE,LONGEST FIELD GOAL
Data INTERCEPTIONS,SACKS
Data NON-KICKERS,KICKERS,ALL PLAYERS
Data FIRST DOWNS,RUSH ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,PASSING YARDS,TOTAL YARDS,PASS COMPLETION PCT.,FUMBLES,FUMBLES LOST,INTERCEPTIONS THROWN,TURNOVER RATIO,PENALTIES,PENALTY YARDAGE,SACKS ALLOWED,SACK YARDAGE
Data OPP. FIRST DOWNS,OPP. RUSH ATTEMPTS,OPP. RUSHING YARDS,OPP. RUSHING AVERAGE,OPP. PASSING YARDS,OPP. TOTAL YARDS,OPP. PASS COMPLETION PCT.,OPP. FUMBLES,OPP. FUMBLES RECOVERED,INTERCEPTIONS,OPP. PENALTIES,OPP. PENALTY YARDAGE,SACKS
Data SACK YARDAGE LOST,ALL-PURPOSE YARDS


'------------------------------
'    BUILDSTATS Subroutine
'------------------------------
' ...explanation...
Sub BUILDSTATS ()
    For ZX = 1 To 30
        If Z1$(ZX) = "XXX" Or Z1$(ZX) = "*" Or Z1$(ZX) = " " Then 150
        If Not _FileExists(DV$(1) + Z1$(ZX)) Then 150
        Print "CONVERTING STAT FILE OF "; Z1$(ZX)
        Open DV$(1) + Z1$(ZX) For Input As #1
        Input #1, A$(PX)
        For I = 0 To 9: Input #1, A1$(PX, I): Next I
        For I = 0 To 5: Input #1, A2$(PX, I): Next I
        For I = 0 To 3: Input #1, A3$(PX, I): Next I
        For I = 0 To 2: Input #1, A4$(PX, I): Next I
        For I = 0 To 2: Input #1, A5$(PX, I): Next I
        Input #1, A6$(PX, 0)
        For I = 0 To 1: Input #1, A7$(PX, I): Next I
        For J = 0 To 13
            For K = 0 To 17: Input #1, Z2(J, K): Next K
        Next J
        For L = 0 To 38: Input #1, Z(L): Next L
        For L = 0 To 38: Input #1, Z1(L): Next L
        For J = 0 To 9: Input #1, DI$(PX, J): For J1 = 0 To 2: Input #1, GI(PX, J, J1): Next: Next
        For J = 0 To 14: Input #1, DS$(PX, J): For J1 = 0 To 1: Input #1, GS(PX, J, J1): Next: Next
        For I = 0 To 9: Input #1, LR(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
        For I = 0 To 15: Input #1, LC(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
        For I = 0 To 3: Input #1, LP(I): Input #1, LP$(I, 0): Input #1, LP$(I, 1): Next
        For I = 0 To 2: Input #1, LK(I): Input #1, LK$(I, 0): Input #1, LK$(I, 1): Next
        For I = 0 To 2: Input #1, PK(I): Input #1, PK$(I, 0): Input #1, PK$(I, 1): Next
        For I = 0 To 1: Input #1, LF(I): Input #1, LF$(I): For I1 = 0 To 4: Input #1, AM(I, I1): Input #1, AF(I, I1): Next: Next
        Close #1
        Open DV$(1) + Z1$(ZX) + "S" For Input As #1
        Input #1, Y%
        Close #1
        TS = TS + 1
        For I = 0 To 14
            TT(TS, I) = Z(I)
            TT$(TS, I) = A$(PX)
        Next
        For I = 15 To 29
            TT(TS, I) = Z1(I - 15)
            TT$(TS, I) = A$(PX)
        Next
        For I = 0 To 9
            JA = JA + 1: A1L$(JA) = A1$(PX, I): A1T$(JA) = A$(PX): LRL$(JA) = LR$(I, 0): LCL$(JA) = LC$(I, 0)
            For I1 = 0 To 5: A1L(JA, I1) = Z2(I, I1): Next I1
            A1L(JA, 6) = LR(I): A1L(JA, 7) = LC(I)
        Next I
        For I = 0 To 5
            JB = JB + 1: A2L$(JB) = A2$(PX, I): A2T$(JB) = A$(PX): LCL$(JB) = LC$(I + 10, 0): GMB(JB) = Y%
            For I1 = 6 To 8: A2L(JB, I1 - 6) = Z2(I, I1): Next I1
            A2L(JB, 3) = LC(I + 10)
        Next I
        For I = 0 To 3
            JC = JC + 1: QBL$(JC) = A3$(PX, I): A3T$(JC) = A$(PX): LPL$(JC) = LP$(I, 0): GMC(JC) = Y%
            For I1 = 9 To 13: A3L(JC, I1 - 9) = Z2(I, I1): Next I1
            A3L(JC, 5) = LP(I)
            GoSub 8900: A3R(JC, 0) = A3L(JC, 12)
        Next I
        For I = 0 To 2
            JD = JD + 1: A4L$(JD) = A4$(PX, I): A4T$(JD) = A$(PX): LKL$(JD) = LK$(I, 0)
            For I1 = 14 To 16: A4L(JD, I1 - 14) = Z2(I, I1): Next I1
            A4L(JD, 3) = LK(I)
        Next I
        For I = 0 To 2
            JE = JE + 1: A5L$(JE) = A5$(PX, I): A5T$(JE) = A$(PX): PKL$(JE) = PK$(I, 0)
            For I1 = 14 To 16: A5L(JE, I1 - 14) = Z2(I + 3, I1): Next I1
            A5L(JE, 3) = PK(I)
        Next I
        JF = JF + 1: A6L$(JF) = A6$(PX, 0): A6T$(JF) = A$(PX): A6L(JF, 0) = Z(7): A6L(JF, 1) = Z(8)
        JG = JG + 1: A7L$(JG) = A7$(PX, 0): A7T$(JG) = A$(PX)
        For I = 0 To 3: A7L(JG, I) = Z2(I + 3, 17): Next
        A7L(JG, 4) = LF(0)
        JG = JG + 1: A7L$(JG) = A7$(PX, 1): A7T$(JG) = A$(PX)
        For I = 0 To 3: A7L(JG, I) = Z2(I + 7, 17): Next
        A7L(JG, 4) = LF(1)
        For I = 0 To 9
            JH = JH + 1: A8L$(JH) = DI$(PX, I): A8T$(JH) = A$(PX): A8L(JH) = GI(PX, I, 0)
        Next
        For I = 0 To 14
            JI = JI + 1: A9L$(JI) = DS$(PX, I): A9T$(JI) = A$(PX): A9L(JI) = GS(PX, I, 0)
        Next
        JJ = JJ + 1
        Y%(JJ) = Y%

    150 Next ZX
    GoTo 155

    8900 'QUARTERBACK PASSER RATINGS
    If A3L(JC, 0) > 0 Then A3L(JC, 11) = Int(A3L(JC, 1) / A3L(JC, 0) * 1000) / 10
    If CT% = 3 Then 9100: ' GO TO COLLEGE QB
    If A3L(JC, 0) = 0 Then A3L(JC, 11) = 0: A3L(JC, 8) = 0: A3L(JC, 10) = 0: A3L(JC, 9) = 0: GoTo 9005
    A3L(JC, 8) = Int(A3L(JC, 4) / A3L(JC, 0) * 1000) / 10
    A3L(JC, 10) = Int(A3L(JC, 3) / A3L(JC, 0) * 100) / 100
    A3L(JC, 9) = Int(A3L(JC, 2) / A3L(JC, 0) * 1000) / 10

    9005 PP(JC) = A3L(JC, 11) - 30
    If PP(JC) < 0 Then PP(JC) = 0: GoTo 9009
    If PP(JC) > 77.5 Then PP(JC) = 2.375: GoTo 9009
    PP(JC) = PP(JC) * .05

    9009 If A3L(JC, 8) > 11.9 Then FF(JC) = 2.375: GoTo 9011
    FF(JC) = A3L(JC, 8) * .2

    9011 If A3L(JC, 9) = 0 Then II(JC) = 0: GoTo 9014
    If A3L(JC, 9) > 9.5 Then II(JC) = 0: GoTo 9014
    II(JC) = 2.375 - (.25 * A3L(JC, 9))

    9014 A3L(JC, 10) = A3L(JC, 10) - 3
    If A3L(JC, 10) < 0 Then YY(JC) = 0: GoTo 9020
    If A3L(JC, 10) > 12.5 Then YY(JC) = 2.375: GoTo 9020
    D = Int(A3L(JC, 10) * 100) / 2
    If D = A3L(JC, 10) / 2 Then YY(JC) = A3L(JC, 10) / 2 * .3 + A3L(JC, 10) / 2 * .2: GoTo 9020
    YY(JC) = A3L(JC, 10) / 2 * .3 + A3L(JC, 10) / 2 * .2 + .003

    9020 A3L(JC, 12) = PP(JC) + FF(JC) + II(JC) + YY(JC)
    If A3L(JC, 12) > 9.5 Then A3L(JC, 12) = 158.3: GoTo 9027
    A3L(JC, 12) = A3L(JC, 12) * 50 / 3

    9027 A3L(JC, 12) = Int(A3L(JC, 12) * 10) / 10
    Return

    9100 'COL QB RATING
    '********** CALCULATE QUARTERBACK PASSING EFFICIENCY ****************
    '* QB(1)=PASS ATT, QB(2)=COMP, QB(3)=INT, QB(4)=YDS, QB(5)=TD'S     *
    '* QB(6)=COMP%, QB(7)=RATING, QB(8)=TD%, QB(9)=INT%, QB(10)=YDS/ATT *
    '********************************************************************
    If A3L(JC, 0) = 0 Then A3L(JC, 11) = 0: A3L(JC, 8) = 0: A3L(JC, 10) = 0: A3L(JC, 9) = 0: GoTo 9105
    A3L(JC, 12) = Int(A3L(JC, 3) / A3L(JC, 0)) * 8.399999 + A3L(JC, 11) + Int(A3L(JC, 4) / A3L(JC, 0) * 1000) / 10 * 3.3 - Int(A3L(JC, 2) / A3L(JC, 0) * 1000) / 10 * 2
    A3L(JC, 12) = Int(A3L(JC, 12) * 10) / 10

    9105 Return

    155 '
End Sub

'------------------------------
'    SAVELEADER Subroutine
'------------------------------
' ...explanation...
Sub SAVELEADER ()
    Print "SAVING LEADER FILES . . . . PLEASE WAIT"
    Open DV$(2) + LG$ + ".LDS" For Output As #1
    Write #1, JA, JB, JC, JD, JE, JF, JG, JH, JI, TS, JJ
    For I = 1 To 30: For I1 = 0 To 29: Write #1, TT(I, I1), TT$(I, I1): Next
    Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next
    For I = 1 To 300: Write #1, A1L$(I), A1T$(I), LRL$(I), LCL$(I), GMA(I)
    For I1 = 0 To 7: Write #1, A1L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 180: Write #1, A2L$(I), A2T$(I), LCL$(I), GMB(I)
    For I1 = 0 To 3: Write #1, A2L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 120: Write #1, QBL$(I), A3T$(I), LPL$(I), A3R(I, 0), GMC(I)
    For I1 = 0 To 6: Write #1, A3L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 90: Write #1, A4L$(I), A4T$(I), LKL$(I)
    For I1 = 0 To 3: Write #1, A4L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 90: Write #1, A5L$(I), A5T$(I), PKL$(I)
    For I1 = 0 To 3: Write #1, A5L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 30: Write #1, A6L$(I), A6T$(I)
    For I1 = 0 To 1: Write #1, A6L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 60: Write #1, A7L$(I), A7T$(I)
    For I1 = 0 To 4: Write #1, A7L(I, I1): Next I1: Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 300: Write #1, A8L$(I), A8T$(I), A8L(I): Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 450: Write #1, A9L$(I), A9T$(I), A9L(I): Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR: Next I
    For I = 1 To 30: Write #1, Y%(I): Next
    Close #1
End Sub


'------------------------------
'    CATEGORIES Subroutine
'------------------------------
' ...explanation...
Sub CATEGORIES ()
    For I = 0 To 73: Read TP$(I): Next
End Sub


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 32: Color 12
    Print "3 IN 1 FOOTBALL V6.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
