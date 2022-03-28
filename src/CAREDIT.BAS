'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Edit Career Files"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim A1$(50), A2$(40), A3$(20), A4$(15), A5$(15), A6$(10), A7$(10), TT(50)
Dim RC(6), RC$(6), D$(1)
Dim FF(15, 30), YY(15, 30), II(15, 30), PP(15, 30), RBS$(15, 50, 1), WRS$(15, 40, 1), QBS$(15, 20, 1), PRS$(15, 15, 1), KRS$(15, 15, 1)
ReDim DI$(55), GIS(15, 55, 3), DS$(55), GSS(15, 55, 3)', GS%(1, 55), GI%(1, 55)
ReDim RBS(15, 50, 11), WRS(15, 40, 11), QBS(15, 20, 13), PRS(15, 15, 11), KRS(15, 15, 11), PUS(15, 10, 4), FGS(15, 10, 17)

Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next
Color 0, 0: Cls: TMR = 6420
Close #1
    
Spec$ = D$(1) + "*.CAR"
COUNT% = FCount%(Spec$)
If COUNT% = 0 Then 4500
ReDim F$(0 To COUNT%)
For X = 1 To COUNT%
    F$(X) = Space$(12)
Next
F$(0) = Spec$
Call ReadFile(F$())
'CALL SortStr(VARPTR(F$(1)), COUNT%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT FILE TO VIEW": Color 15
SELECTION% = 1
START% = 1
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
Print "-SELECTS FILE   ESC-RETURN TO MAIN MENU";

_MouseShow "DEFAULT"
20 MAMenu F$(), SELECTION%, START%, COUNT%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then Run "CAREER.EXE"
If EXITCODE% = -71 Then START% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then START% = COUNT%: EXITCODE% = 3: SELECTION% = COUNT%: GoTo 20
If EXITCODE% = -73 Then START% = START% - 80: If START% < 1 Then START% = 1
If EXITCODE% = -81 Then START% = START% + 80: If START% > COUNT% Then START% = COUNT%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = START%: GoTo 20
F$ = F$(SELECTION%)
    
Locate 2, 33: Print F$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

F$ = RTrim$(F$)
Z$ = Left$(F$, Len(F$) - 4)
Color 15: Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

Open D$(1) + Z$ + ".CAR" For Input As #2
Input #2, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
For I = 0 To 49: Input #2, A1$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 39: Input #2, A2$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 19: Input #2, A3$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 14: Input #2, A4$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 14: Input #2, A5$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 9: Input #2, A6$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 9: Input #2, A7$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 54: Input #2, DI$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 54: Input #2, DS$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For N = 1 To 15: For J = 0 To 49: For K = 0 To 9: Input #2, RBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 49: Input #2, RBS$(N, J, 0), RBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Input #2, WRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: Input #2, WRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Input #2, QBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: Input #2, QBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Input #2, KRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Input #2, KRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Input #2, PRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Input #2, PRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Input #2, PUS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Input #2, FGS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Input #2, GIS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Input #2, GSS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
Close 2

GoTo 2000

101 TMR = 3840: Color 15: Locate 12, 10: Print "WRITING CAREER FILE . . . . PLEASE WAIT"

Open D$(1) + Z$ + ".CAR" For Output As #2
Write #2, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
For I = 0 To 49: Write #2, A1$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 39: Write #2, A2$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 19: Write #2, A3$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 14: Write #2, A4$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 14: Write #2, A5$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 9: Write #2, A6$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 9: Write #2, A7$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 54: Write #2, DI$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 54: Write #2, DS$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For N = 1 To 15: For J = 0 To 49: For K = 0 To 9: Write #2, RBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 49: Write #2, RBS$(N, J, 0), RBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Write #2, WRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: Write #2, WRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Write #2, QBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: Write #2, QBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #2, KRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Write #2, KRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #2, PRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Write #2, PRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Write #2, PUS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Write #2, FGS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #2, GIS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #2, GSS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
Close #2

375 Run "CAREER.EXE"

2000 Cls: Color 7: Print "EDIT ";
Color 12: Print "R";: Color 7: Print "UNNING BACKS, ";: Color 12: Print "W";: Color 7: Print "IDE RECEIVERS, ";
    COLOR 12:PRINT "Q";:COLOR 7:PRINT "UARTERBACKS, KI";:COLOR 12:PRINT "C";:COLOR 7:PRINT "K RETURNERS, P";:_
    COLOR 12:PRINT "U";:COLOR 7:PRINT "NT RETURNERS, ";:COLOR 12:PRINT "P";:COLOR 7:PRINT "UNTERS, ";
    COLOR 12:PRINT "K";:COLOR 7:PRINT "ICKERS, ";:COLOR 12:PRINT "I";:COLOR 7:PRINT "NTERCEPTIONS, DE";:COLOR 12:PRINT "F";:COLOR 7:PRINT "ENSIVE SACKS, UP";:_
    COLOR 12:PRINT"D";:COLOR 7:PRINT "ATE CAREER FILE"
Color 7: Print "OR ";
Color 12: Print "E";: Color 7: Print "XIT WITHOUT UPDATING"
2002 I$ = GetKeyPress$
If UCase$(I$) = "R" Then 2020
If UCase$(I$) = "W" Then 2120
If UCase$(I$) = "Q" Then 2220
If UCase$(I$) = "C" Then 2320
If UCase$(I$) = "U" Then 2420
If UCase$(I$) = "P" Then 2520
If UCase$(I$) = "K" Then 2620
If UCase$(I$) = "I" Then 2720
If UCase$(I$) = "F" Then 2820
If UCase$(I$) = "D" Then 101
If UCase$(I$) = "E" Then 375

2020 Cls: Color 12: Print Z$
For I = 0 To 16: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A1$(I), 12): Next
For I = 17 To 33: Locate I - 15, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A1$(I), 12): Next
For I = 34 To 50: Locate I - 32, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A1$(I), 12): Next

2041 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (51 TO EXIT)"; B9: If B9 < 0 Or B9 > 51 Then 2041
If B9 = 51 Then 2000
If B9 = 50 Then 2060
GoSub 6000: Cls: Locate 5, 15: Print "CHANGING RUNNING BACK FILE . . . . . PLEASE WAIT"
For I = B9 To 49: A1$(I) = A1$(I + 1): Next
UPP = (50 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 48: For I1 = 0 To 9: RBS(J, I, I1) = RBS(J, I + 1, I1): RBS$(J, I, 0) = RBS$(J, I + 1, 0): RBS$(J, I, 1) = RBS$(J, I + 1, 1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2060 A1$(49) = "": For J = 1 To 15: For I1 = 0 To 9: RBS(J, 50, I1) = 0: RBS$(J, 50, 0) = "": RBS$(J, 50, 1) = "": Next: Next: GoTo 2070

2070 GoTo 2000

2120 Cls: Color 12: Print Z$
For I = 0 To 19: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A2$(I), 12): Next
For I = 20 To 39: Locate I - 18, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A2$(I), 12): Next

2141 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (40 TO EXIT)"; B9: If B9 < 0 Or B9 > 40 Then 2141
If B9 = 40 Then 2000
If B9 = 39 Then 2160
GoSub 6100: Cls: Locate 5, 15: Print "CHANGING RECEIVER FILE . . . . . PLEASE WAIT"
For I = B9 To 38: A2$(I) = A2$(I + 1): Next
UPP = (39 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 38: For I1 = 0 To 5: WRS(J, I, I1) = WRS(J, I + 1, I1): WRS$(J, I, 0) = WRS$(J, I + 1, 0): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2160 A2$(39) = "": For J = 1 To 15: For I1 = 0 To 5: WRS(J, 39, I1) = 0: WRS$(J, 39, 0) = "": Next: Next: GoTo 2170

2170 GoTo 2000

2220 Cls: Color 12: Print Z$
For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A3$(I), 12): Next
For I = 10 To 19: Locate I - 8, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A3$(I), 12): Next

2241 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (20 TO EXIT)"; B9: If B9 < 0 Or B9 > 20 Then 2241
If B9 = 20 Then 2000
If B9 = 19 Then 2260
GoSub 6200: Cls: Locate 5, 15: Print "CHANGING QUARTERBACK FILE . . . . . PLEASE WAIT"
For I = B9 To 18: A3$(I) = A3$(I + 1): Next
UPP = (19 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 18: For I1 = 0 To 7: QBS(J, I, I1) = QBS(J, I + 1, I1): QBS$(J, I, 0) = QBS$(J, I + 1, 0): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2260 A3$(19) = "": For J = 1 To 15: For I1 = 0 To 7: QBS(J, 19, I1) = 0: QBS$(J, 19, 0) = "": Next: Next: GoTo 2270

2270 GoTo 2000

2320 Cls: Color 12: Print Z$
For I = 0 To 14: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A4$(I), 12): Next

2341 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (15 TO EXIT)"; B9: If B9 < 0 Or B9 > 15 Then 2341
If B9 = 15 Then 2000
If B9 = 14 Then 2360
GoSub 6300: Cls: Locate 5, 15: Print "CHANGING KICK RETURNER FILE . . . . . PLEASE WAIT"
For I = B9 To 13: A4$(I) = A4$(I + 1): Next
UPP = (14 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 13: For I1 = 0 To 5: KRS(J, I, I1) = KRS(J, I + 1, I1): KRS$(J, I, 0) = KRS$(J, I + 1, 0): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2360 A4$(14) = "": For J = 1 To 15: For I1 = 0 To 5: KRS(J, 14, I1) = 0: KRS$(J, 14, 0) = "": Next: Next: GoTo 2370

2370 GoTo 2000

2420 Cls: Color 12: Print Z$
For I = 0 To 14: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A5$(I), 12): Next

2441 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (15 TO EXIT)"; B9: If B9 < 0 Or B9 > 15 Then 2441
If B9 = 15 Then 2000
If B9 = 14 Then 2460
GoSub 6400: Cls: Locate 5, 15: Print "CHANGING PUNT RETURNER FILE . . . . . PLEASE WAIT"
For I = B9 To 13: A5$(I) = A5$(I + 1): Next
UPP = (14 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 13: For I1 = 0 To 5: PRS(J, I, I1) = PRS(J, I + 1, I1): PRS$(J, I, 0) = PRS$(J, I + 1, 0): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2460 A5$(14) = "": For J = 1 To 15: For I1 = 0 To 5: PRS(J, 14, I1) = 0: PRS$(J, 14, 0) = "": Next: Next: GoTo 2470

2470 GoTo 2000

2520 Cls: Color 12: Print Z$
For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A6$(I), 12): Next

2541 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (10 TO EXIT)"; B9: If B9 < 0 Or B9 > 10 Then 2541
If B9 = 10 Then 2000
If B9 = 9 Then 2560
GoSub 6500: Cls: Locate 5, 15: Print "CHANGING PUNTER FILE . . . . . PLEASE WAIT"
For I = B9 To 8: A6$(I) = A6$(I + 1): Next
UPP = (9 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 8: For I1 = 0 To 3: PUS(J, I, I1) = PUS(J, I + 1, I1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2560 A6$(9) = "": For J = 1 To 15: For I1 = 0 To 3: PUS(J, 9, I1) = 0: Next: Next: GoTo 2570

2570 GoTo 2000

2620 Cls: Color 12: Print Z$
For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A7$(I), 12): Next

2641 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (10 TO EXIT)"; B9: If B9 < 0 Or B9 > 10 Then 2641
If B9 = 10 Then 2000
If B9 = 9 Then 2660
GoSub 6600: Cls: Locate 5, 15: Print "CHANGING FIELD GOAL KICKER FILE . . . . . PLEASE WAIT"
For I = B9 To 8: A7$(I) = A7$(I + 1): Next
UPP = (9 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 8: For I1 = 0 To 16: FGS(J, I, I1) = FGS(J, I + 1, I1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2660 For J = 1 To 15: For I1 = 0 To 16: FGS(J, 9, I1) = 0: Next: Next: GoTo 2670

2670 GoTo 2000

2720 Cls: Color 12: Print Z$
For I = 0 To 17: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DI$(I), 12): Next
For I = 18 To 35: Locate I - 16, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DI$(I), 12): Next
For I = 36 To 54: Locate I - 34, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DI$(I), 12): Next

2741 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (55 TO EXIT)"; B9: If B9 < 0 Or B9 > 55 Then 2741
If B9 = 55 Then 2000
If B9 = 54 Then 2760
GoSub 6700: Cls: Locate 5, 15: Print "CHANGING INTERCEPTION FILE . . . . . PLEASE WAIT"
For I = B9 To 8: DI$(I) = DI$(I + 1): Next
UPP = (54 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 53: For I1 = 0 To 2: GIS(J, I, I1) = GIS(J, I + 1, I1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2760 DI$(54) = "": For J = 1 To 15: For I1 = 0 To 2: GIS(J, 54, I1) = 0: Next: Next: GoTo 2770

2770 GoTo 2000

2820 Cls: Color 12: Print Z$
For I = 0 To 17: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DS$(I), 12): Next
For I = 18 To 35: Locate I - 16, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DS$(I), 12): Next
For I = 36 To 54: Locate I - 34, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DS$(I), 12): Next

2841 Locate 22: Color 15: Input "ENTER NUMBER OF PLAYER TO DELETE (55 TO EXIT)"; B9: If B9 < 0 Or B9 > 55 Then 2841
If B9 = 55 Then 2000
If B9 = 54 Then 2860
GoSub 6800: Cls: Locate 5, 15: Print "CHANGING INTERCEPTION FILE . . . . . PLEASE WAIT"
For I = B9 To 8: DS$(I) = DS$(I + 1): Next
UPP = (54 - B9) * 15
I = B9: For J = 1 To 15: For I = B9 To 53: For I1 = 0 To 2: GSS(J, I, I1) = GSS(J, I + 1, I1): Next: Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP: Next: Next

2860 DS$(54) = "": For J = 1 To 15: For I1 = 0 To 2: GSS(J, 54, I1) = 0: Next: Next: GoTo 2870

2870 GoTo 2000

6000 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A1$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A1$(B9);: Color 14: Print " HIT 'E'"
6004 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6030
If UCase$(I$) = "N" Then 6080
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6004

6030 LPrint Z$
LPrint "RUSHER         YEAR   ATT   YARDS AVERAGE  TDS   REC  YDS  AVG  TDS RUN-LONG-REC"
VV = 0
For I = 1 To 15: VV = VV + RBS(I, B9, 7): Next
LPrint Left$(A1$(B9), 13);
For V = 1 To VV
    LPrint Tab(15); RBS(V, B9, 8);
    LPrint Tab(23); Using "###"; RBS(V, B9, 0);
    LPrint Tab(30); Using "####"; RBS(V, B9, 1);
    If RBS(V, B9, 1) = 0 Or RBS(V, B9, 0) = 0 Then LPrint Tab(37); " 0.0";
    If RBS(V, B9, 0) > 0 Then LPrint Tab(37); Using "##.#"; RBS(V, B9, 1) / RBS(V, B9, 0);
    LPrint Tab(44); Using "###"; RBS(V, B9, 2);
    LPrint Tab(50); Using "###"; RBS(V, B9, 3);
    LPrint Tab(54); Using "####"; RBS(V, B9, 4);
    If RBS(V, B9, 4) = 0 Or RBS(V, B9, 3) = 0 Then LPrint Tab(59); " 0.0";
    If RBS(V, B9, 3) > 0 Then LPrint Tab(59); Using "##.#"; RBS(V, B9, 4) / RBS(V, B9, 3);
    LPrint Tab(65); Using "##"; RBS(V, B9, 5);
    LPrint Tab(71); Using "##"; RBS(V, B9, 6);: LPrint RBS$(V, B9, 0);
    LPrint Tab(77); Using "##"; RBS(V, B9, 9);: LPrint RBS$(V, B9, 1)
Next V
RC(0) = 0: RC(1) = 0: RC$(0) = "": RC$(1) = ""
For V = 1 To VV
    TT1 = TT1 + RBS(V, B9, 0)
    TT2 = TT2 + RBS(V, B9, 1)
    TT3 = TT3 + RBS(V, B9, 2)
    TT4 = TT4 + RBS(V, B9, 3)
    TT5 = TT5 + RBS(V, B9, 4)
    TT6 = TT6 + RBS(V, B9, 5)
    If RBS(V, B9, 6) > RC(0) Then 6050 Else GoTo 6051

    6050 RC(0) = RBS(V, B9, 6): RC$(0) = RBS$(V, B9, 0)

    6051 If RBS(V, B9, 9) > RC(1) Then 6052 Else GoTo 6055

    6052 RC(1) = RBS(V, B9, 9): RC$(1) = RBS$(V, B9, 1)

6055 Next V
LPrint
LPrint "TOTALS";
LPrint Tab(18); Using "##"; VV;
LPrint Tab(22); Using "####"; TT1;
LPrint Tab(29); Using "#####"; TT2;
If TT1 = 0 Or TT2 = 0 Then LPrint Tab(37); " 0.0";
If TT1 > 0 Then LPrint Tab(37); Using "##.#"; TT2 / TT1;
LPrint Tab(44); Using "###"; TT3;
LPrint Tab(49); Using "####"; TT4;
LPrint Tab(53); Using "#####"; TT5;
If TT4 = 0 Or TT5 = 0 Then LPrint Tab(59); " 0.0";
If TT4 > 0 Then LPrint Tab(59); Using "##.#"; TT5 / TT4;
LPrint Tab(64); Using "###"; TT6;
LPrint Tab(71); Using "##"; RC(0);: LPrint RC$(0);
LPrint Tab(77); Using "##"; RC(1);: LPrint RC$(1)
TT1 = 0: TT2 = 0: TT3 = 0: TT4 = 0: TT5 = 0: TT6 = 0

6080 Return

6100 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A2$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A2$(B9);: Color 14: Print " HIT 'E'"
6104 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6130
If UCase$(I$) = "N" Then 6180
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6104

6130 LPrint Z$
LPrint "RECEIVER        YEAR        REC.      YARDS      AVG.     TD  LONGEST REC."
VV = 0
For I = 1 To 15: VV = VV + WRS(I, B9, 4): Next
LPrint Left$(A2$(B9), 13);
For V = 1 To VV
    LPrint Tab(16); WRS(V, B9, 5);
    LPrint Tab(29); "###"; WRS(V, B9, 0);
    LPrint Tab(40); "####"; WRS(V, B9, 1);
    If WRS(V, B9, 0) = 0 Or WRS(V, B9, 1) = 0 Then LPrint Tab(49); " 0.0";
    If WRS(V, B9, 0) > 0 Then LPrint Tab(49); "##.#"; WRS(V, B9, 1) / WRS(V, B9, 0);
    LPrint Tab(59); "##"; WRS(V, B9, 2);
    LPrint Tab(67); "##"; WRS(V, B9, 3);: LPrint WRS$(V, B9, 0)
Next V
RC(0) = 0: RC$(0) = ""
For V = 1 To VV
    TT7 = TT7 + WRS(V, B9, 0)
    TT8 = TT8 + WRS(V, B9, 1)
    TT9 = TT9 + WRS(V, B9, 2)
    If WRS(V, B9, 3) > RC(0) Then 6145 Else GoTo 6148

    6145 RC(0) = WRS(V, B9, 3): RC$(0) = WRS$(V, B9, 0)

6148 Next V
LPrint "TOTALS";
LPrint Tab(19); "##"; VV;
LPrint Tab(28); "####"; TT7;
LPrint Tab(39); "#####"; TT8;
If TT7 = 0 Or TT8 = 0 Then LPrint Tab(49); " 0.0";
If TT7 > 0 Then LPrint Tab(49); "##.#"; TT8 / TT7;
LPrint Tab(58); "###"; TT9;
LPrint Tab(67); "##"; RC(0);: LPrint RC$(0)
TT7 = 0: TT8 = 0: TT9 = 0

6180 Return

6200 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A3$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A3$(B9);: Color 14: Print " HIT 'E'"
6204 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6230
If UCase$(I$) = "N" Then 6280
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6204

6230 LPrint Z$
LPrint "QUARTERBACK  YEAR  COMP.    ATT.    INT  TDS   YARDS  PCT. YD/COMP  RATING  LONG.PASS"
VV = 0
For I = 1 To 15: VV = VV + QBS(I, B9, 6): Next
LPrint Left$(A3$(B9), 13);
For V = 1 To VV
    LPrint Tab(13); QBS(V, B9, 7);
    LPrint Tab(21); "###"; QBS(V, B9, 1);
    LPrint Tab(29); "###"; QBS(V, B9, 0);
    LPrint Tab(38); "##"; QBS(V, B9, 2);
    LPrint Tab(43); "##"; QBS(V, B9, 4);
    LPrint Tab(49); "####"; QBS(V, B9, 3);
    If QBS(V, B9, 1) = 0 Or QBS(V, B9, 0) = 0 Then LPrint Tab(56); " 00";
    If QBS(V, B9, 1) > 0 Then LPrint Tab(56); "##"; (QBS(V, B9, 1) / QBS(V, B9, 0)) * 100;
    If QBS(V, B9, 0) = 0 Then LPrint Tab(63); " 0.0";
    If QBS(V, B9, 1) > 0 Then LPrint Tab(63); "##.#"; QBS(V, B9, 3) / QBS(V, B9, 1);
    GoSub 8999
    LPrint Tab(70); "###.#"; QBS(V, B9, 12);
    LPrint Tab(80); "##"; QBS(V, B9, 5);: LPrint QBS$(V, B9, 0)
Next V
RC(0) = 0: RC$(0) = ""
For V = 1 To VV
    TT10 = TT10 + QBS(V, B9, 1)
    TT11 = TT11 + QBS(V, B9, 0)
    TT12 = TT12 + QBS(V, B9, 2)
    TT13 = TT13 + QBS(V, B9, 4)
    TT14 = TT14 + QBS(V, B9, 3)
    If QBS(V, B9, 5) > RC(O) Then 6251 Else GoTo 6255

    6251 RC(0) = QBS(V, B9, 5): RC$(0) = QBS$(V, B9, 0)

6255 Next V
LPrint "TOTALS";
LPrint Tab(16); "##"; VV;
LPrint Tab(20); "####"; TT10;
LPrint Tab(28); "####"; TT11;
LPrint Tab(37); "###"; TT12;
LPrint Tab(41); "####"; TT13;
LPrint Tab(48); "#####"; TT14;
If TT10 = 0 Or TT11 = 0 Then LPrint Tab(56); " 00";
If TT10 > 0 Then LPrint Tab(56); "##"; (TT10 / TT11) * 100;
If TT11 = 0 Then LPrint Tab(63); " 0.0";
If TT10 > 0 Then LPrint Tab(63); "##.#"; TT14 / TT10;
GoSub 9099
LPrint Tab(70); "###.#"; TT31;
LPrint Tab(80); "##"; RC(0);: LPrint RC$(0)
TT10 = 0: TT11 = 0: TT12 = 0: TT13 = 0: TT14 = 0

6280 Return

6300 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A4$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A4$(B9);: Color 14: Print " HIT 'E'"
6304 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6330
If UCase$(I$) = "N" Then 6380
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6304

6330 LPrint Z$
LPrint "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"
VV = 0
For I = 1 To 15: VV = VV + KRS(I, B9, 4): Next
LPrint Left$(A4$(B9), 13);
For V = 1 To VV
    LPrint Tab(22); KRS(V, B9, 5);
    LPrint Tab(32); "###"; KRS(V, B9, 0);
    LPrint Tab(42); "####"; KRS(V, B9, 1);
    If KRS(V, B9, 0) = 0 Or KRS(V, B9, 1) = 0 Then LPrint Tab(52); " 00";
    If KRS(V, B9, 0) > 0 Then LPrint Tab(52); "##.#"; KRS(V, B9, 1) / KRS(V, B9, 0);
    LPrint Tab(62); "##"; KRS(V, B9, 2);
    LPrint Tab(68); "##"; KRS(V, B9, 3);: LPrint KRS$(V, B9, 0)
Next V
RC(0) = 0: RC$(0) = ""
For V = 1 To VV
    TT15 = TT15 + KRS(V, B9, 0)
    TT16 = TT16 + KRS(V, B9, 1)
    TT17 = TT17 + KRS(V, B9, 2)
    If KRS(V, B9, 3) > RC(0) Then 6345 Else GoTo 6347

    6345 RC(0) = KRS(V, B9, 3): RC$(0) = KRS$(V, B9, 0)

6347 Next V
LPrint "TOTALS";
LPrint Tab(25); "##"; VV;
LPrint Tab(32); "###"; TT15;
LPrint Tab(41); "#####"; TT16;
If TT15 = 0 Or TT16 = 0 Then LPrint Tab(52); " 00";
If TT15 > 0 Then LPrint Tab(52); "##.#"; TT16 / TT15;
LPrint Tab(61); "###"; TT17;
LPrint Tab(68); "##"; RC(0);: LPrint RC$(0)
TT15 = 0: TT16 = 0: TT17 = 0

6380 Return

6400 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A5$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A5$(B9);: Color 14: Print " HIT 'E'"
6404 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6430
If UCase$(I$) = "N" Then 6480
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6404

6430 LPrint Z$
LPrint "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"
VV = 0
For I = 1 To 15: VV = VV + PRS(I, B9, 4): Next
LPrint Left$(A5$(B9), 13);
For V = 1 To VV
    LPrint Tab(22); PRS(V, B9, 5);
    LPrint Tab(32); "###"; PRS(V, B9, 0);
    LPrint Tab(42); "####"; PRS(V, B9, 1);
    If PRS(V, B9, 0) = 0 Or PRS(V, B9, 1) = 0 Then LPrint Tab(52); " 00";
    If PRS(V, B9, 0) > 0 Then LPrint Tab(52); "##.#"; PRS(V, B9, 1) / PRS(V, B9, 0);
    LPrint Tab(62); "##"; PRS(V, B9, 2);
    LPrint Tab(68); "##"; PRS(V, B9, 3);: LPrint PRS$(V, B9, 0)
Next V
RC(0) = 0: RC$(0) = ""
For V = 1 To VV
    TT18 = TT18 + PRS(V, B9, 0)
    TT19 = TT19 + PRS(V, B9, 1)
    TT20 = TT20 + PRS(V, B9, 2)
    If PRS(V, B9, 3) > RC(0) Then 6445 Else GoTo 6447

    6445 RC(0) = PRS(V, B9, 3): RC$(0) = PRS$(V, B9, 0)

6447 Next V
LPrint "TOTALS";
LPrint Tab(25); "##"; VV;
LPrint Tab(32); "###"; TT18;
LPrint Tab(41); "#####"; TT19;
If TT18 = 0 Or TT19 = 0 Then LPrint Tab(52); " 00";
If TT18 > 0 Then LPrint Tab(52); "##.#"; TT19 / TT18;
LPrint Tab(61); "###"; TT20;
LPrint Tab(68); "##"; RC(0);: LPrint RC$(0)
TT18 = 0: TT19 = 0: TT20 = 0

6480 Return

6500 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A6$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A6$(B9);: Color 14: Print " HIT 'E'"
6504 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6530
If UCase$(I$) = "N" Then 6580
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6504

6530 LPrint Z$
LPrint "PUNTERS           YEAR     ATT.     YARDS      AVG."
VV = 0
For I = 1 To 15: VV = VV + PUS(I, B9, 2): Next
LPrint Left$(A6$(B9), 13);
For V = 1 To VV
    LPrint Tab(18); PUS(V, B9, 3);
    LPrint Tab(28); "###"; PUS(V, B9, 0);
    LPrint Tab(38); "####"; PUS(V, B9, 1);
    If PUS(V, B9, 0) = 0 Or PUS(V, B9, 1) = 0 Then LPrint Tab(47); "00.0"
    If PUS(V, B9, 0) > 0 Then LPrint Tab(47); "##.#"; PUS(V, B9, 1) / PUS(V, B9, 0)
Next V
For V = 1 To VV
    TT21 = TT21 + PUS(V, B9, 0)
    TT22 = TT22 + PUS(V, B9, 1)
Next V
LPrint "TOTALS";
LPrint Tab(21); "##"; VV;
LPrint Tab(27); "####"; TT21;
LPrint Tab(37); "#####"; TT22;
If TT21 = 0 Or TT22 = 0 Then LPrint Tab(47); " 00"
If TT21 > 0 Then LPrint Tab(47); "##.#"; TT22 / TT21
TT21 = 0: TT22 = 0

6580 Return

6600 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print A7$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print A7$(B9);: Color 14: Print " HIT 'E'"
6604 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6630
If UCase$(I$) = "N" Then 6680
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6604

6630 LPrint Z$
Locate 2: LPrint "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50- +"
VV = 0
For I = 1 To 15: VV = VV + FGS(I, B9, 15): Next
LPrint Left$(A7$(B9), 13);
For V = 1 To VV
    LPrint Tab(13); FGS(V, B9, 16);
    LPrint Tab(19); "###"; FGS(V, B9, 3);
    LPrint Tab(23); "###"; FGS(V, B9, 2);
    If FGS(V, B9, 2) = 0 Or FGS(V, B9, 3) = 0 Then LPrint Tab(27); "  0";
    If FGS(V, B9, 2) > 0 Then LPrint Tab(27); "###"; (FGS(V, B9, 2) / FGS(V, B9, 3)) * 100;
    LPrint Tab(32); "###"; FGS(V, B9, 1);
    LPrint Tab(36); "###"; FGS(V, B9, 0);
    If FGS(V, B9, 0) = 0 Or FGS(V, B9, 1) = 0 Then LPrint Tab(40); "  0";
    If FGS(V, B9, 0) > 0 Then LPrint Tab(40); "###"; (FGS(V, B9, 0) / FGS(V, B9, 1)) * 100;
    LPrint Tab(46); "##"; FGS(V, B9, 4);
    LPrint Tab(49); "##"; FGS(V, B9, 5);: LPrint "-";
    LPrint Tab(52); "##"; FGS(V, B9, 10);
    LPrint Tab(55); "##"; FGS(V, B9, 6);: LPrint "-";
    LPrint Tab(58); "##"; FGS(V, B9, 11);
    LPrint Tab(61); "##"; FGS(V, B9, 7);: LPrint "-";
    LPrint Tab(64); "##"; FGS(V, B9, 12);
    LPrint Tab(67); "##"; FGS(V, B9, 8);: LPrint "-";
    LPrint Tab(70); "##"; FGS(V, B9, 13);
    LPrint Tab(73); "##"; FGS(V, B9, 9);: LPrint "-";
    LPrint Tab(76); "##"; FGS(V, B9, 14)
Next V
For V = 1 To VV
    TT(23) = TT(23) + FGS(V, B9, 0)
    TT(24) = TT(24) + FGS(V, B9, 1)
    TT(34) = TT(34) + FGS(V, B9, 2)
    TT(35) = TT(35) + FGS(V, B9, 3)
    TT(36) = TT(36) + FGS(V, B9, 5)
    TT(37) = TT(37) + FGS(V, B9, 6)
    TT(38) = TT(38) + FGS(V, B9, 7)
    TT(39) = TT(39) + FGS(V, B9, 8)
    TT(40) = TT(40) + FGS(V, B9, 9)
    TT(41) = TT(41) + FGS(V, B9, 10)
    TT(42) = TT(42) + FGS(V, B9, 11)
    TT(43) = TT(43) + FGS(V, B9, 12)
    TT(44) = TT(44) + FGS(V, B9, 13)
    TT(45) = TT(45) + FGS(V, B9, 14)
Next V
RC(0) = 0
For V = 1 To VV
    If FGS(V, B9, 4) >= RC(0) Then 6648 Else GoTo 6649

    6648 RC(0) = FGS(V, B9, 4)

6649 Next
LPrint "TOTALS";
LPrint Tab(16); VV;
LPrint Tab(19); "###"; TT(35);
LPrint Tab(23); "###"; TT(34);
If TT(34) = 0 Or TT(35) = 0 Then LPrint Tab(27); "  0";
If TT(34) > 0 Then LPrint Tab(27); "###"; (TT(34) / TT(35)) * 100;
LPrint Tab(32); "###"; TT(24);
LPrint Tab(36); "###"; TT(23);
If TT(23) = 0 Or TT(24) = 0 Then LPrint Tab(40); "  0";
If TT(23) > 0 Then LPrint Tab(40); "###"; (TT(23) / TT(24)) * 100;
LPrint Tab(46); "##"; RC(0);
LPrint Tab(49); "##"; TT(36);: LPrint "-";
LPrint Tab(52); "##"; TT(41);
LPrint Tab(55); "##"; TT(37);: LPrint "-";
LPrint Tab(58); "##"; TT(42);
LPrint Tab(61); "##"; TT(38);: LPrint "-";
LPrint Tab(64); "##"; TT(43);
LPrint Tab(67); "##"; TT(39);: LPrint "-";
LPrint Tab(70); "##"; TT(44);
LPrint Tab(73); "##"; TT(40);: LPrint "-";
LPrint Tab(76); "##"; TT(45)
For J = 23 To 45: TT(J) = 0: Next

6680 Return

6700 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print DI$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print DI$(B9);: Color 14: Print " HIT 'E'"
6704 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6730
If UCase$(I$) = "N" Then 6780
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6704

6730 LPrint Z$
LPrint "INTERCEPTORS        YEAR     INTERCEPTIONS"
VV = 0
For I = 1 To 15: VV = VV + GIS(I, B9, 1): Next
LPrint Left$(DI$(B9), 13);
For V = 1 To VV
    LPrint Tab(20); GIS(V, B9, 2);
    LPrint Tab(34); "###"; GIS(V, B9, 0)
Next V
TT(32) = 0
For V = 1 To VV
    TT(32) = TT(32) + GIS(V, B9, 0)
Next V
LPrint "TOTALS";
LPrint Tab(23); "##"; VV;
LPrint Tab(33); "####"; TT(32)

6780 Return

6800 Cls: Locate 5, 20: Color 15: Print "PRINT CAREER STATS OF ";: Color 12: Print DS$(B9);: Color 15: Print " BEFORE "
Locate , 20: Print "DELETING THEM (Y/N)"
Color 14: Print: Locate , 20: Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
Locate , 20: Print "OF ";: Color 12: Print DS$(B9);: Color 14: Print " HIT 'E'"
6804 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 6830
If UCase$(I$) = "N" Then 6880
If UCase$(I$) = "E" Then 2000
If UCase$(I$) <> "Y" And UCase$(I$) <> "N" And UCase$(I$) <> "E" Then 6804

6830 LPrint Z$
LPrint "SACKERS             YEAR     INTERCEPTIONS"
VV = 0
For I = 1 To 15: VV = VV + GSS(I, B9, 1): Next
LPrint Left$(DS$(B9), 13);
For V = 1 To VV
    LPrint Tab(20); GSS(V, B9, 2);
    LPrint Tab(34); "###"; GSS(V, B9, 0)
Next V
TT(33) = 0
For V = 1 To VV
    TT(33) = TT(33) + GSS(V, B9, 0)
Next V
LPrint "TOTALS";
LPrint Tab(23); "##"; VV;
LPrint Tab(33); "####"; TT(33)

6880 Return

8999 If QBS(V, B9, 0) > 0 Then QBS(V, B9, 11) = Int(QBS(V, B9, 1) / QBS(V, B9, 0) * 1000) / 10
If QBS(V, B9, 0) = 0 Then QBS(V, B9, 11) = 0: QBS(V, B9, 8) = 0: QBS(V, B9, 10) = 0: QBS(V, B9, 9) = 0: GoTo 9005
QBS(V, B9, 8) = Int(QBS(V, B9, 4) / QBS(V, B9, 0) * 1000) / 10
QBS(V, B9, 10) = Int(QBS(V, B9, 3) / QBS(V, B9, 0) * 100) / 100
QBS(V, B9, 9) = Int(QBS(V, B9, 2) / QBS(V, B9, 0) * 1000) / 10

9005 PP(V, B9) = QBS(V, B9, 11) - 30
If PP(V, B9) < 0 Then PP(V, B9) = 0: GoTo 9009
If PP(V, B9) > 77.5 Then PP(V, B9) = 2.375: GoTo 9009
PP(V, B9) = PP(V, B9) * .05

9009 If QBS(V, B9, 8) > 11.9 Then FF(V, B9) = 2.375: GoTo 9011
FF(V, B9) = QBS(V, B9, 8) * .2

9011 If QBS(V, B9, 9) = 0 Then II(V, B9) = 0: GoTo 9014
If QBS(V, B9, 9) > 9.5 Then II(V, B9) = 0: GoTo 9014
II(V, B9) = 2.375 - (.25 * QBS(V, B9, 9))

9014 QBS(V, B9, 10) = QBS(V, B9, 10) - 3
If QBS(V, B9, 10) < 0 Then YY(V, B9) = 0: GoTo 9020
If QBS(V, B9, 10) > 12.5 Then YY(V, B9) = 2.375: GoTo 9020
D = Int(QBS(V, B9, 10) * 100) / 2
If D = QBS(V, B9, 10) / 2 Then YY(V, B9) = QBS(V, B9, 10) / 2 * .3 + QBS(V, B9, 10) / 2 * .2: GoTo 9020
YY(V, B9) = QBS(V, B9, 10) / 2 * .3 + QBS(V, B9, 10) / 2 * .2 + .003

9020 QBS(V, B9, 12) = PP(V, B9) + FF(V, B9) + II(V, B9) + YY(V, B9)
If QBS(V, B9, 12) > 9.5 Then QBS(V, B9, 12) = 158.3: GoTo 9027
QBS(V, B9, 12) = QBS(V, B9, 12) * 50 / 3

9027 QBS(V, B9, 12) = Int(QBS(V, B9, 12) * 10) / 10
Return

9099 If TT11 > 0 Then TT30 = Int(TT10 / TT11 * 1000) / 10
If TT11 = 0 Then TT30 = 0: TT27 = 0: TT29 = 0: TT28 = 0: GoTo 9105
TT27 = Int(TT13 / TT11 * 1000) / 10
TT29 = Int(TT14 / TT11 * 100) / 100
TT28 = Int(TT12 / TT11 * 1000) / 10

9105 PPT = TT30 - 30
If PPT < 0 Then PPT = 0: GoTo 9109
If PPT > 77.5 Then PPT = 2.375: GoTo 9109
PPT = PPT * .05

9109 If TT27 > 11.9 Then FFT = 2.375: GoTo 9111
FFT = TT27 * .2

9111 If TT28 = 0 Then IIT = 0: GoTo 9114
If TT28 > 9.5 Then IIT = 0: GoTo 9114
IIT = 2.375 - (.25 * TT28)

9114 TT29 = TT29 - 3
If TT29 < 0 Then YYT = 0: GoTo 9120
If TT29 > 12.5 Then YYT = 2.375: GoTo 9120
D = Int(TT29 * 100) / 2
If D = TT29 / 2 Then YYT = TT29 / 2 * .3 + TT29 / 2 * .2: GoTo 9120
YYT = TT29 / 2 * .3 + TT29 / 2 * .2 + .003

9120 TT31 = PPT + FFT + IIT + YYT
If TT31 > 9.5 Then TT31 = 158.3: GoTo 9127
TT31 = TT31 * 50 / 3

9127 TT31 = Int(TT31 * 10) / 10
Return

4500 Cls
Print "NO CAREER FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
Run "CAREER.EXE"

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
