'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Update Career Files"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

Dim Z2(50, 18), A1$(50), A2$(40), A3$(20), A4$(15), A5$(15), A6$(10), A7$(10)
Dim A1C$(50), A2C$(40), A3C$(20), A4C$(15), A5C$(15), A6C$(10), A7C$(10)
Dim DIC$(55), DSC$(55)
Dim Z(38), Z1(38), teamItems$(500), V%(500), PA%(2), Q$(500)
Dim LR(50), LR$(50, 1), LC(60), LC$(60, 1), LP(20), LP$(20, 1), LK(15), LK$(15, 1), PK(15)
Dim PK$(15, 1), DI$(55), GIS(15, 55, 3), DS$(55), GSS(15, 55, 3), GS%(55, 1), GI%(55, 2)
Dim LF$(1), LI(9), LI$(9, 1), menuItems$(3)
Dim AF(1, 4), AM(1, 4), LF(1), D$(1)
Dim Y As Long
Dim RBBC(9) As Single, RBC$(9), RBYR(9)
Dim RBR$(1), WRRC(4) As Single, WRC$(4), WRYR(4), QBBC(8) As Single, QBC$(8)
Dim QBYR(8), KRRC(4) As Single, KRC$(4)
Dim KRYR(4), PRRC(4) As Single, PRC$(4), PRYR(4), PUUC(2) As Single, PUC$(2)
Dim PUYR(2), FGGC(6) As Single, FGC$(6)
Dim FGYR(6), DFFC(1) As Single, DFC$(1), DFYR(1), PP(15, 20) As Single
Dim FF(15, 20) As Single, II(15, 20) As Single, YY(15, 20) As Single
Dim RBS$(15, 50, 1)
Dim WRS$(15, 40, 1)
Dim QBS$(15, 20, 1)
Dim KRRS$(15, 15, 1), PRRS$(15, 15, 1)
ReDim PUS(15, 10, 4), FGS(15, 10, 17)
ReDim RBBS(15, 50, 10) As Single, WRRS(15, 40, 6) As Single
ReDim QBBS(15, 20, 12) As Single, PRS(15, 15, 6) As Single, KRS(15, 15, 6) As Single

1 Cls
TMR = 12840: NB = -1: NC = -1: ND = -1: NE = -1: NF = -1: NG = -1: NH = -1: NI = -1: NJ = 0: NK = 0: D = 0: JX = 0: VV = 0

Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next: Close #1

Color , 2: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "3 IN 1 FOOTBALL UTILITIES"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
menuItems$(1) = "MERGE / UPDATE CAREER FILES       "
menuItems$(2) = "EXIT TO MAIN MENU                 "
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 10, 59, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
RN = SELECTION%
If SCANCODE% = 27 Then RN = 2
If RN = 1 Then 10
Run "CAREER.EXE"

10 Color 14, 0: Cls: Locate 5, 27: Print "- - - C A U T I O N - - -"
Color 15: Locate 8, 8: Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
Print "FILE.  IF YOU ARE CREATING A NEW CAREER FILE, USE OPTION #1.  IF YOU ARE ADDING"
Print "AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER SLOTS (XXX, END,"
Print "ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
Locate , 8: Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF DARREN NELSON IS"
Print "LISTED AS 'NELSON' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
Print "'D.NELSON', HE SHOULD BE CHANGED TO 'NELSON' IN THE SEASON FILE BEFORE"
Print "MERGING, OR YOU COULD CHANGE HIM TO 'D.NELSON' IN THE CAREER FILE"
Print: Print
Color 12: Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

2018 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1150
Run "CAREER.EXE"

900 Cls: Color 7: Print "UNABLE TO LOCATE STAT FILE"
Print: Print "HIT ANY KEY TO CONTINUE"

902 I$ = GetKeyPress$
GoTo 1

1000
For J = 20 To 22
    Locate J
    Print "                                                                                "
Next
Return

1150 Cls: For J = 15 To 18: Locate J: Print String$(68, " ")
Next
GoSub 4000
GoSub 3056
B$ = A$: GoSub 6585
Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
Cls: Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; Y
Locate 9, 25: Print "ANY CHANGE (YN)"

60 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1150
VV = 0: Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"

76 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then JX = 1
If UCase$(I$) = "N" Then JX = 0
Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
If Not _FileExists(D$(1) + Z$ + ".CAR") Then 900
Open D$(1) + Z$ + ".CAR" For Input As #1
Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
For I = 0 To 49: Input #1, A1C$(I): Next
For I = 0 To 39: Input #1, A2C$(I): Next
For I = 0 To 19: Input #1, A3C$(I): Next
For I = 0 To 14: Input #1, A4C$(I): Next
For I = 0 To 14: Input #1, A5C$(I): Next
For I = 0 To 9: Input #1, A6C$(I): Next
For I = 0 To 9: Input #1, A7C$(I): Next
For I = 0 To 54: Input #1, DIC$(I): Next
For I = 0 To 54: Input #1, DSC$(I): Next
For N = 1 To 15: For J = 0 To 49: For K = 0 To 9
Input #1, RBBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 49: Input #1, RBS$(N, J, 0), RBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Input #1, WRRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: Input #1, WRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Input #1, QBBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: Input #1, QBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Input #1, KRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Input #1, KRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Input #1, PRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Input #1, PRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Input #1, PUS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Input #1, FGS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Input #1, GIS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Input #1, GSS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
Close #1

GoSub 1000

If Not _FileExists(D$(1) + Z1$) Then 900

Open D$(1) + Z1$ + "S" For Input As #1
Input #1, GAMES
Close 1

Open D$(1) + Z1$ For Input As #1
Input #1, A$
For I = 0 To 9: Input #1, A1$(I): Next I
For I = 0 To 5: Input #1, A2$(I): Next I
For I = 0 To 3: Input #1, A3$(I): Next I
For I = 0 To 2: Input #1, A4$(I): Next I
For I = 0 To 2: Input #1, A5$(I): Next I
Input #1, A6$(0)
For I = 0 To 1: Input #1, A7$(I): Next I
For J = 0 To 13
    For K = 0 To 17: Input #1, Z2(J, K): Next K
Next J
For L = 0 To 38: Input #1, Z(L): Next L
For L = 0 To 38: Input #1, Z1(L): Next L
For J = 0 To 9: Input #1, DI$(J): For J1 = 0 To 2: Input #1, GI%(J, J1): Next: Next
For J = 0 To 14: Input #1, DS$(J): For J1 = 0 To 1: Input #1, GS%(J, J1): Next: Next
For I = 0 To 9: Input #1, LR(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
For I = 0 To 15: Input #1, LC(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
For I = 0 To 3: Input #1, LP(I): Input #1, LP$(I, 0): Input #1, LP$(I, 1): Next
For I = 0 To 2: Input #1, LK(I): Input #1, LK$(I, 0): Input #1, LK$(I, 1): Next
For I = 0 To 2: Input #1, PK(I): Input #1, PK$(I, 0): Input #1, PK$(I, 1): Next
For I = 0 To 1: Input #1, LF(I): Input #1, LF$(I): For I1 = 0 To 4: Input #1, AM(I, I1): Input #1, AF(I, I1): Next: Next
For I = 0 To 9: Input #1, LI(I): Input #1, LI$(I, 0): Input #1, LI$(I, 1): Next
Close #1
For I = 0 To 9: T% = Len(A1$(I))
    If T% = 0 Then A1$(I) = "XXX"
Next
For I = 0 To 5: T% = Len(A2$(I))
    If T% = 0 Then A2$(I) = "XXX"
Next
For I = 0 To 3: T% = Len(A3$(I))
    If T% = 0 Then A3$(I) = "XXX"
Next
For I = 0 To 2: T% = Len(A4$(I))
    If T% = 0 Then A4$(I) = "XXX"
Next
For I = 0 To 2: T% = Len(A5$(I))
    If T% = 0 Then A5$(I) = "XXX"
Next
For I = 0 To 1: T% = Len(A7$(I))
    If T% = 0 Then A7$(I) = "XXX"
Next
For I = 0 To 9: T% = Len(DI$(I))
    If T% = 0 Then DI$(I) = "XXX"
Next
For I = 0 To 14: T% = Len(DS$(I))
    If T% = 0 Then DS$(I) = "XXX"
Next
For I = 0 To 9
    RR = 0: CHK = 0
    For J = 0 To 49
        If A1$(I) <> A1C$(J) Then 1423
        For W = 1 To 15: If RBBS(W, J, 8) <> Y Then 1413
            CHK = CHK + 1

        1413 Next W
        N = 0
        For W = 1 To 15
            N = N + RBBS(W, J, 7)
        Next
        If CHK = 0 Then N = N + 1
        For K = 0 To 5
            RBBS(N, J, K) = Z2(I, K)
        Next K
        RBBS(N, J, 6) = LR(I): RBS$(N, J, 0) = LR$(I, 0)
        RBBS(N, J, 9) = LC(I): RBS$(N, J, 1) = LC$(I, 0)
        RBBS(N, J, 7) = 1: RBBS(N, J, 8) = Y
        GoTo 1430

        1423 RR = RR + 1
        If RR = 50 Then GoSub 1450
    Next J

1430 Next I
GoTo 1460

1450 If A1$(I) = "END" Or A1$(I) = "XXX" Or Left$(A1$(I), 1) = " " Then 1456
NB = NB + 1
A1C$(NB) = A1$(I)
N = 1
For K = 0 To 5: RBBS(N, NB, K) = Z2(I, K): Next
RBBS(N, NB, 6) = LR(I): RBS$(N, NB, 0) = LR$(I, 0)
RBBS(N, NB, 9) = LC(I): RBS$(N, NB, 1) = LC$(I, 0)
RBBS(N, NB, 7) = 1: RBBS(N, NB, 8) = Y

1456 Return

1460 For I = 0 To 5
    RR = 0: CHK = 0
    For J = 0 To 39
        If A2$(I) <> A2C$(J) Then 1473

        1464 For W = 1 To 15
            If WRRS(W, J, 5) <> Y Then 1466

            1465 CHK = CHK + 1

        1466 Next W
        N = 0
        For W = 1 To 15
            N = N + WRRS(W, J, 4)
        Next
        If CHK = 0 Then N = N + 1
        For K = 6 To 8
            WRRS(N, J, K - 6) = Z2(I, K)
        Next K
        WRRS(N, J, 3) = LC(I + 10): WRS$(N, J, 0) = LC$(I + 10, 0)
        WRRS(N, J, 4) = 1: WRRS(N, J, 5) = Y
        GoTo 1476

        1473 RR = RR + 1
        If RR = 40 Then GoSub 1478
    Next J

1476 Next I
GoTo 1490

1478 If A2$(I) = "END" Or A2$(I) = "XXX" Or Left$(A2$(I), 1) = " " Then 1484 Else GoTo 1479

1479 NC = NC + 1
A2C$(NC) = A2$(I)
N = 1
For K = 6 To 8: WRRS(N, NC, K - 6) = Z2(I, K): Next
WRRS(N, NC, 3) = LC(I + 10): WRS$(N, NC, 0) = LC$(I + 10, 0)
WRRS(N, NC, 4) = 1: WRRS(N, NC, 5) = Y

1484 Return

1490 For I = 0 To 3
    RR = 0: CHK = 0
    For J = 0 To 19
        If A3$(I) <> A3C$(J) Then 1503

        1494 For W = 1 To 15: If QBBS(W, J, 7) <> Y Then 1496

            1495 CHK = CHK + 1

        1496 Next W
        N = 0
        For W = 1 To 15
            N = N + QBBS(W, J, 6)
        Next
        If CHK = 0 Then N = N + 1
        For K = 9 To 13
            QBBS(N, J, K - 9) = Z2(I, K)
        Next K
        QBBS(N, J, 5) = LP(I): QBS$(N, J, 0) = LP$(I, 0)
        QBBS(N, J, 6) = 1: QBBS(N, J, 7) = Y
        GoTo 1506

        1503 RR = RR + 1
        If RR = 20 Then GoSub 1508
    Next J

1506 Next I
GoTo 1520

1508 If A3$(I) = "END" Or A3$(I) = "XXX" Or Left$(A3$(I), 1) = " " Then 1514

1509 ND = ND + 1
A3C$(ND) = A3$(I)
N = 1
For K = 9 To 13: QBBS(N, ND, K - 9) = Z2(I, K): Next
QBBS(N, ND, 5) = LP(I): QBS$(N, ND, 0) = LP$(I, 0)
QBBS(N, ND, 6) = 1: QBBS(N, ND, 7) = Y

1514 Return

1520 For I = 0 To 2
    RR = 0: CHK = 0
    For J = 0 To 14
        If A4$(I) <> A4C$(J) Then 1533

        1524 For W = 1 To 15: If KRS(W, J, 5) <> Y Then 1526

            1525 CHK = CHK + 1

        1526 Next W
        N = 0
        For W = 1 To 15
            N = N + KRS(W, J, 4)
        Next
        If CHK = 0 Then N = N + 1
        For K = 14 To 16
            KRS(N, J, K - 14) = Z2(I, K)
        Next K
        KRS(N, J, 3) = LK(I): KRRS$(N, J, 0) = LK$(I, 0)
        KRS(N, J, 4) = 1: KRS(N, J, 5) = Y
        GoTo 1536

        1533 RR = RR + 1
        If RR = 15 Then GoSub 1538
    Next J

1536 Next I
GoTo 1550

1538 If A4$(I) = "END" Or A4$(I) = "XXX" Or Left$(A4$(I), 1) = " " Then 1544

1539 NE = NE + 1
A4C$(NE) = A4$(I)
N = 1
For K = 14 To 16: KRS(N, NE, K - 14) = Z2(I, K): Next
KRS(N, NE, 3) = LK(I): KRRS$(N, NE, 0) = LK$(I, 0)
KRS(N, NE, 4) = 1: KRS(N, NE, 5) = Y

1544 Return

1550 For I = 0 To 2
    RR = 0: CHK = 0
    For J = 0 To 14
        If A5$(I) = A5C$(J) Then 1554 Else GoTo 1563

        1554 For W = 1 To 15: If PRS(W, J, 5) <> Y Then 1556

            1555 CHK = CHK + 1

        1556 Next W
        N = 0
        For W = 1 To 15
            N = N + PRS(W, J, 4)
        Next
        If CHK = 0 Then N = N + 1
        For K = 14 To 16
            PRS(N, J, K - 14) = Z2(I + 3, K)
        Next K
        PRS(N, J, 3) = PK(I): PRRS$(N, J, 0) = PK$(I, 0)
        PRS(N, J, 4) = 1: PRS(N, J, 5) = Y
        GoTo 1566

        1563 RR = RR + 1
        If RR = 15 Then GoSub 1568
    Next J

1566 Next I
GoTo 1580

1568 If A5$(I) = "END" Or A5$(I) = "XXX" Or Left$(A5$(I), 1) = " " Then 1574

1569 NF = NF + 1
A5C$(NF) = A5$(I)
N = 1
For K = 14 To 16: PRS(N, NF, K - 14) = Z2(I + 3, K): Next
PRS(N, NF, 3) = PK(I): PRRS$(N, NF, 0) = PK$(I, 0)
PRS(N, NF, 4) = 1: PRS(N, NF, 5) = Y

1574 Return

1580 For I = 0 To 0
    RR = 0: CHK = 0
    For J = 0 To 9
        If A6$(I) <> A6C$(J) Then 1593

        1584 For W = 1 To 15: If PUS(W, J, 3) <> Y Then 1586

            1585 CHK = CHK + 1

        1586 Next W
        If CHK > 0 Then 1588 Else GoTo 1589

        1588 N = PUS(1, J, 2) + PUS(2, J, 2) + PUS(3, J, 2) + PUS(4, J, 2) + PUS(5, J, 2) + PUS(6, J, 2) + PUS(7, J, 2) + PUS(8, J, 2) + PUS(9, J, 2) + PUS(10, J, 2) + PUS(11, J, 2) + PUS(12, J, 2) + PUS(13, J, 2) + PUS(14, J, 2) + PUS(15, J, 2): GoTo 1590

        1589 N = PUS(1, J, 2) + PUS(2, J, 2) + PUS(3, J, 2) + PUS(4, J, 2) + PUS(5, J, 2) + PUS(6, J, 2) + PUS(7, J, 2) + PUS(8, J, 2) + PUS(9, J, 2) + PUS(10, J, 2) + PUS(11, J, 2) + PUS(12, J, 2) + PUS(13, J, 2) + PUS(14, J, 2) + PUS(15, J, 2) + 1

        1590 PUS(N, J, 0) = Z(7): PUS(N, J, 1) = Z(8)
        PUS(N, J, 2) = 1: PUS(N, J, 3) = Y
        GoTo 1596

        1593 RR = RR + 1
        If RR = 10 Then GoSub 1598
    Next J

1596 Next I
GoTo 1610

1598 If A6$(I) = "END" Or A6$(I) = "XXX" Or Left$(A6$(I), 1) = " " Then 1604

1599 NG = NG + 1
A6C$(NG) = A6$(I)
N = 1
PUS(N, NG, 0) = Z(7): PUS(N, NG, 1) = Z(8)
PUS(N, NG, 2) = 1: PUS(N, NG, 3) = Y

1604 Return

1610 For I = 0 To 1
    RR = 0: CHK = 0
    For J = 0 To 9
        If A7$(I) = A7C$(J) Then 1614 Else GoTo 1633

        1614 For W = 1 To 15: If FGS(W, J, 16) = Y Then 1615 Else GoTo 1616

            1615 CHK = CHK + 1

        1616 Next W
        If CHK > 0 Then 1618 Else GoTo 1619

        1618 N = FGS(1, J, 15) + FGS(2, J, 15) + FGS(3, J, 15) + FGS(4, J, 15) + FGS(5, J, 15) + FGS(6, J, 15) + FGS(7, J, 15) + FGS(8, J, 15) + FGS(9, J, 15) + FGS(10, J, 15) + FGS(11, J, 15) + FGS(12, J, 15) + FGS(13, J, 15) + FGS(14, J, 15) + FGS(15, J, 15): GoTo 1620

        1619 N = FGS(1, J, 15) + FGS(2, J, 15) + FGS(3, J, 15) + FGS(4, J, 15) + FGS(5, J, 15) + FGS(6, J, 15) + FGS(7, J, 15) + FGS(8, J, 15) + FGS(9, J, 15) + FGS(10, J, 15) + FGS(11, J, 15) + FGS(12, J, 15) + FGS(13, J, 15) + FGS(14, J, 15) + FGS(15, J, 15) + 1
        If I = 0 Then 1620
        If I = 1 Then 1630

        1620 For K = 3 To 6
        FGS(N, J, K - 3) = Z2(K, 17): Next K
        FGS(N, J, 4) = LF(I)
        FGS(N, J, 15) = 1: FGS(N, J, 16) = Y
        For J1 = 0 To 4: FGS(N, J, J1 + 5) = AM(I, J1): Next
        For J1 = 0 To 4: FGS(N, J, J1 + 10) = AF(I, J1): Next
        GoTo 1632

        1630 For K = 7 To 10
        FGS(N, J, K - 7) = Z2(K, 17): Next K
        FGS(N, J, 4) = LF(I)
        FGS(N, J, 15) = 1: FGS(N, J, 16) = Y
        For J1 = 0 To 4: FGS(N, J, J1 + 5) = AM(I, J1): Next
        For J1 = 0 To 4: FGS(N, J, J1 + 10) = AF(I, J1): Next

        1632 GoTo 1636

        1633 RR = RR + 1
        If RR = 10 Then GoSub 1638
    Next J

1636 Next I
GoTo 1660

1638 If A7$(I) = "END" Or A7$(I) = "XXX" Or Left$(A7$(I), 1) = " " Then 1650 Else GoTo 1639

1639 NH = NH + 1
A7C$(NH) = A7$(I)
N = 1
If I = 0 Then 1642
If I = 1 Then 1645

1642 For K = 3 To 6:
FGS(N, NH, K - 3) = Z2(K, 17): Next K
FGS(N, NH, 4) = LF(I)
FGS(N, NH, 15) = 1: FGS(N, NH, 16) = Y
For J1 = 0 To 4: FGS(N, NH, J1 + 5) = AM(I, J1): Next
For J1 = 0 To 4: FGS(N, NH, J1 + 10) = AF(I, J1): Next
GoTo 1660

1645 For K = 7 To 10:
FGS(N, NH, K - 7) = Z2(K, 17): Next K
FGS(N, NH, 4) = LF(I)
FGS(N, NH, 15) = 1: FGS(N, J, 16) = Y
For J1 = 0 To 4: FGS(N, NH, J1 + 5) = AM(I, J1): Next
For J1 = 0 To 4: FGS(N, NH, J1 + 10) = AF(I, J1): Next

1650 Return

1660 For I = 0 To 9
    RR = 0: CHK = 0
    For J = 0 To 54
        If DI$(I) = DIC$(J) Then 1664 Else GoTo 1673

        1664 For W = 1 To 15: If GIS(W, J, 2) = Y Then 1665 Else GoTo 1666

            1665 CHK = CHK + 1

        1666 Next W
        If CHK > 0 Then 1668 Else GoTo 1669

        1668 N = GIS(1, J, 1) + GIS(2, J, 1) + GIS(3, J, 1) + GIS(4, J, 1) + GIS(5, J, 1) + GIS(6, J, 1) + GIS(7, J, 1) + GIS(8, J, 1) + GIS(9, J, 1) + GIS(10, J, 1) + GIS(11, J, 1) + GIS(12, J, 1) + GIS(13, J, 1) + GIS(14, J, 1) + GIS(15, J, 1): GoTo 1670

        1669 N = GIS(1, J, 1) + GIS(2, J, 1) + GIS(3, J, 1) + GIS(4, J, 1) + GIS(5, J, 1) + GIS(6, J, 1) + GIS(7, J, 1) + GIS(8, J, 1) + GIS(9, J, 1) + GIS(10, J, 1) + GIS(11, J, 1) + GIS(12, J, 1) + GIS(13, J, 1) + GIS(14, J, 1) + GIS(15, J, 1) + 1

        1670 GIS(N, J, 0) = GI%(I, 0)
        GIS(N, J, 1) = 1: GIS(N, J, 2) = Y
        GoTo 1676

        1673 RR = RR + 1
        If RR = 10 Then GoSub 1678
    Next J

1676 Next I
GoTo 1700

1678 If DI$(I) = "END" Or DI$(I) = "XXX" Or Left$(DI$(I), 1) = " " Then 1684 Else GoTo 1679

1679 NJ = NJ + 1
DIC$(NJ) = DI$(I)
N = 1
GIS(N, NJ, 0) = GI%(I, 0)
GIS(N, NJ, 1) = 1: GIS(N, NJ, 2) = Y

1684 Return

1700 For I = 0 To 14
    RR = 0: CHK = 0
    For J = 0 To 54
        If DS$(I) = DSC$(J) Then 1704 Else GoTo 1713

        1704 For W = 1 To 15: If GSS(W, J, 2) = Y Then 1705 Else GoTo 1706

            1705 CHK = CHK + 1

        1706 Next W
        If CHK > 0 Then 1708 Else GoTo 1709

        1708 N = GSS(1, J, 1) + GSS(2, J, 1) + GSS(3, J, 1) + GSS(4, J, 1) + GSS(5, J, 1) + GSS(6, J, 1) + GSS(7, J, 1) + GSS(8, J, 1) + GSS(9, J, 1) + GSS(10, J, 1) + GSS(11, J, 1) + GSS(12, J, 1) + GSS(13, J, 1) + GSS(14, J, 1) + GSS(15, J, 1): GoTo 1710

        1709 N = GSS(1, J, 1) + GSS(2, J, 1) + GSS(3, J, 1) + GSS(4, J, 1) + GSS(5, J, 1) + GSS(6, J, 1) + GSS(7, J, 1) + GSS(8, J, 1) + GSS(9, J, 1) + GSS(10, J, 1) + GSS(11, J, 1) + GSS(12, J, 1) + GSS(13, J, 1) + GSS(14, J, 1) + GSS(15, J, 1) + 1

        1710 GSS(N, J, 0) = GS%(I, 0)
        GSS(N, J, 1) = 1: GSS(N, J, 2) = Y
        GoTo 1716

        1713 RR = RR + 1
        If RR = 15 Then GoSub 1718
    Next J

1716 Next I
GoTo 1800

1718 If DS$(I) = "END" Or DS$(I) = "XXX" Or Left$(DS$(I), 1) = " " Then 1724 Else GoTo 1719

1719 NK = NK + 1
DSC$(NK) = DS$(I)
N = 1
GSS(N, NK, 0) = GS%(I, 0)
GSS(N, NK, 1) = 1: GSS(N, NK, 2) = Y

1724 Return

1800 If JX = 1 Then 1804
If JX = 0 Then 1805

1804 GoSub 2080

1805 GoSub 1000
Open D$(1) + Z$ + ".CAR" For Output As #1
Write #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
For I = 0 To 49: Write #1, A1C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 39: Write #1, A2C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 19: Write #1, A3C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 14: Write #1, A4C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 14: Write #1, A5C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 9: Write #1, A6C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 9: Write #1, A7C$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 54: Write #1, DIC$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For I = 0 To 54: Write #1, DSC$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next
For N = 1 To 15: For J = 0 To 49: For K = 0 To 9: Write #1, RBBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 49: Write #1, RBS$(N, J, 0), RBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Write #1, WRRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: Write #1, WRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Write #1, QBBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: Write #1, QBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #1, KRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Write #1, KRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #1, PRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Write #1, PRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Write #1, PUS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Write #1, FGS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #1, GIS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #1, GSS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "#####"; TMR: Next J: Next N
Close #1
Clear
Run "CAREER1.EXE"

2080 If Not _FileExists(D$(1) + Z$ + ".CLD") Then 2090
Open D$(1) + Z$ + ".CLD" For Input As #1
For I = 0 To 9: Input #1, RBBC(I), RBC$(I), RBYR(I): Next
Input #1, RBR$(0), RBR$(1)
For I = 0 To 4: Input #1, WRRC(I), WRC$(I), WRYR(I): Next
Input #1, WRR$
For I = 0 To 8: Input #1, QBBC(I), QBC$(I), QBYR(I): Next
Input #1, QBR$
For I = 0 To 4: Input #1, KRRC(I), KRC$(I), KRYR(I): Next
Input #1, KRR$
For I = 0 To 4: Input #1, PRRC(I), PRC$(I), PRYR(I): Next
Input #1, PRR$
For I = 0 To 2: Input #1, PUUC(I), PUC$(I), PUYR(I): Next
For I = 0 To 6: Input #1, FGGC(I), FGC$(I), FGYR(I): Next
For I = 0 To 1: Input #1, DFFC(I), DFC$(I), DFYR(I): Next
Close #1

2090 For I = 0 To 49
    If A1C$(I) = "" Or A1C$(I) = "XXX" Or A1C$(I) = "END" Then 2180
    For J = 1 To 15
        If RBBS(J, I, 8) = 0 Then 2180 Else GoTo 2103

        2103 If RBBS(J, I, 0) > RBBC(0) Then 2104 Else GoTo 2113

        2104 RBBC(0) = RBBS(J, I, 0): RBC$(0) = A1C$(I): RBYR(0) = RBBS(J, I, 8)

        2113 If RBBS(J, I, 1) > RBBC(1) Then 2114 Else GoTo 2123

        2114 RBBC(1) = RBBS(J, I, 1): RBC$(1) = A1C$(I): RBYR(1) = RBBS(J, I, 8)

        2123 If RBBS(J, I, 0) = 0 Then 2133
        If RBBS(J, I, 1) / RBBS(J, I, 0) <= RBBC(2) Or RBBS(J, I, 0) < GAMES * 10 Then 2133

        2125 RBBC(2) = RBBS(J, I, 1) / RBBS(J, I, 0): RBC$(2) = A1C$(I): RBYR(2) = RBBS(J, I, 8)

        2133 If RBBS(J, I, 2) <= RBBC(3) Then 2143

        2134 RBBC(3) = RBBS(J, I, 2): RBC$(3) = A1C$(I): RBYR(3) = RBBS(J, I, 8)

        2143 If RBBS(J, I, 3) <= RBBC(4) Then 2153

        2144 RBBC(4) = RBBS(J, I, 3): RBC$(4) = A1C$(I): RBYR(4) = RBBS(J, I, 8)

        2153 If RBBS(J, I, 4) <= RBBC(5) Then 2163

        2154 RBBC(5) = RBBS(J, I, 4): RBC$(5) = A1C$(I): RBYR(5) = RBBS(J, I, 8)

        2163 If RBBS(J, I, 3) = 0 Then 2173
        If RBBS(J, I, 4) / RBBS(J, I, 3) <= RBBC(6) Or RBBS(J, I, 3) < GAMES * 3 Then 2173

        2165 RBBC(6) = RBBS(J, I, 4) / RBBS(J, I, 3): RBC$(6) = A1C$(I): RBYR(6) = RBBS(J, I, 8)

        2173 If RBBS(J, I, 5) <= RBBC(7) Then 2175

        2174 RBBC(7) = RBBS(J, I, 5): RBC$(7) = A1C$(I): RBYR(7) = RBBS(J, I, 8)

        2175 If RBBS(J, I, 6) <= RBBC(8) Then 2177

        2176 RBBC(8) = RBBS(J, I, 6): RBC$(8) = A1C$(I): RBYR(8) = RBBS(J, I, 8): RBR$(0) = RBS$(J, I, 0)

        2177 If RBBS(J, I, 9) <= RBBC(9) Then 2179

        2178 RBBC(9) = RBBS(J, I, 9): RBC$(9) = A1C$(I): RBYR(9) = RBBS(J, I, 8): RBR$(1) = RBS$(J, I, 1)

    2179 Next J

2180 Next I
For I = 0 To 39
    If A2C$(I) = "" Or A2C$(I) = "XXX" Or A2C$(I) = "END" Then 2280
    For J = 1 To 15
        If WRRS(J, I, 5) = 0 Then 2280

        2203 If WRRS(J, I, 0) <= WRRC(0) Then 2213

        2204 WRRC(0) = WRRS(J, I, 0): WRC$(0) = A2C$(I): WRYR(0) = WRRS(J, I, 5)

        2213 If WRRS(J, I, 1) <= WRRC(1) Then 2223

        2214 WRRC(1) = WRRS(J, I, 1): WRC$(1) = A2C$(I): WRYR(1) = WRRS(J, I, 5)

        2223 If WRRS(J, I, 0) = 0 Then 2233
        If WRRS(J, I, 1) / WRRS(J, I, 0) <= WRRC(2) Or WRRS(J, I, 0) < GAMES * 3 Then 2233

        2225 WRRC(2) = WRRS(J, I, 1) / WRRS(J, I, 0): WRC$(2) = A2C$(I): WRYR(2) = WRRS(J, I, 5)

        2233 If WRRS(J, I, 2) <= WRRC(3) Then 2235

        2234 WRRC(3) = WRRS(J, I, 2): WRC$(3) = A2C$(I): WRYR(3) = WRRS(J, I, 5)

        2235 If WRRS(J, I, 3) <= WRRC(4) Then 2279

        2236 WRRC(4) = WRRS(J, I, 3): WRC$(4) = A2C$(I): WRYR(4) = WRRS(J, I, 5): WRR$ = WRS$(J, I, 0)

    2279 Next J

2280 Next I
For I = 0 To 19
    If A3C$(I) = "" Or A3C$(I) = "XXX" Or A3C$(I) = "END" Then 2380
    For J = 1 To 15: GoSub 8999
        If QBBS(J, I, 7) = 0 Then 2380

        2303 If QBBS(J, I, 1) <= QBBC(0) Then 2313

        2304 QBBC(0) = QBBS(J, I, 1): QBC$(0) = A3C$(I): QBYR(0) = QBBS(J, I, 7)

        2313 If QBBS(J, I, 0) <= QBBC(1) Then 2323

        2314 QBBC(1) = QBBS(J, I, 0): QBC$(1) = A3C$(I): QBYR(1) = QBBS(J, I, 7)

        2323 If QBBS(J, I, 2) <= QBBC(2) Then 2333

        2324 QBBC(2) = QBBS(J, I, 2): QBC$(2) = A3C$(I): QBYR(2) = QBBS(J, I, 7)

        2333 If QBBS(J, I, 4) <= QBBC(3) Then 2343

        2334 QBBC(3) = QBBS(J, I, 4): QBC$(3) = A3C$(I): QBYR(3) = QBBS(J, I, 7)

        2343 If QBBS(J, I, 3) <= QBBC(4) Then 2353

        2344 QBBC(4) = QBBS(J, I, 3): QBC$(4) = A3C$(I): QBYR(4) = QBBS(J, I, 7)

        2353 If QBBS(J, I, 0) = 0 Then 2363
        If QBBS(J, I, 1) / QBBS(J, I, 0) <= QBBC(5) Or QBBS(J, I, 0) < GAMES * 15 Then 2363

        2355 QBBC(5) = QBBS(J, I, 1) / QBBS(J, I, 0): QBC$(5) = A3C$(I): QBYR(5) = QBBS(J, I, 7)

        2363 If QBBS(J, I, 1) = 0 Then 2373
        If QBBS(J, I, 3) / QBBS(J, I, 1) <= QBBC(6) Or QBBS(J, I, 0) < GAMES * 15 Then 2373

        2365 QBBC(6) = QBBS(J, I, 3) / QBBS(J, I, 1): QBC$(6) = A3C$(I): QBYR(6) = QBBS(J, I, 7)

        2373 If QBBS(J, I, 12) <= QBBC(7) Then 2375

        2374 QBBC(7) = QBBS(J, I, 12): QBC$(7) = A3C$(I): QBYR(7) = QBBS(J, I, 7)

        2375 If QBBS(J, I, 5) <= QBBC(8) Then 2379

        2376 QBBC(8) = QBBS(J, I, 5): QBC$(8) = A3C$(I): QBYR(8) = QBBS(J, I, 7): QBR$ = QBS$(J, I, 0)

    2379 Next J

2380 Next I
For I = 0 To 14
    If A4C$(I) = "" Or A4C$(I) = "XXX" Or A4C$(I) = "END" Then 2480
    For J = 1 To 15
        If KRS(J, I, 5) = 0 Then 2480

        2403 If KRS(J, I, 0) <= KRRC(0) Then 2413

        2404 KRRC(0) = KRS(J, I, 0): KRC$(0) = A4C$(I): KRYR(0) = KRS(J, I, 5)

        2413 If KRS(J, I, 1) <= KRRC(1) Then 2423

        2414 KRRC(1) = KRS(J, I, 1): KRC$(1) = A4C$(I): KRYR(1) = KRS(J, I, 5)

        2423 If KRS(J, I, 0) = 0 Then 2433
        If KRS(J, I, 1) / KRS(J, I, 0) <= KRRC(2) Or KRS(J, I, 0) < GAMES * 1.2 Then 2433

        2425 KRRC(2) = KRS(J, I, 1) / KRS(J, I, 0): KRC$(2) = A4C$(I): KRYR(2) = KRS(J, I, 5)

        2433 If KRS(J, I, 2) <= KRRC(3) Then 2435

        2434 KRRC(3) = KRS(J, I, 2): KRC$(3) = A4C$(I): KRYR(3) = KRS(J, I, 5)

        2435 If KRS(J, I, 3) <= KRRC(4) Then 2479

        2436 KRRC(4) = KRS(J, I, 3): KRC$(4) = A4C$(I): KRYR(4) = KRS(J, I, 5): KRR$ = KRRS$(J, I, 0)

    2479 Next J

2480 Next I
For I = 0 To 14
    If A5C$(I) = "" Or A5C$(I) = "XXX" Or A5C$(I) = "END" Then 2580
    For J = 1 To 15
        If PRS(J, I, 5) = 0 Then 2580

        2503 If PRS(J, I, 0) > PRRC(0) Then 2504 Else GoTo 2513

        2504 PRRC(0) = PRS(J, I, 0): PRC$(0) = A5C$(I): PRYR(0) = PRS(J, I, 5)

        2513 If PRS(J, I, 1) <= PRRC(1) Then 2523

        2514 PRRC(1) = PRS(J, I, 1): PRC$(1) = A5C$(I): PRYR(1) = PRS(J, I, 5)

        2523 If PRS(J, I, 0) = 0 Then 2533
        If PRS(J, I, 1) / PRS(J, I, 0) <= PRRC(2) Or PRS(J, I, 0) < GAMES * 1.2 Then 2533

        2525 PRRC(2) = PRS(J, I, 1) / PRS(J, I, 0): PRC$(2) = A5C$(I): PRYR(2) = PRS(J, I, 5)

        2533 If PRS(J, I, 2) <= PRRC(3) Then 2535

        2534 PRRC(3) = PRS(J, I, 2): PRC$(3) = A5C$(I): PRYR(3) = PRS(J, I, 5)

        2535 If PRS(J, I, 3) <= PRRC(4) Then 2579

        2536 PRRC(4) = PRS(J, I, 3): PRC$(4) = A5C$(I): PRYR(4) = PRS(J, I, 5): PRR$ = PRRS$(J, I, 0)

    2579 Next J

2580 Next I
For I = 0 To 9
    If A6C$(I) = "" Or A6C$(I) = "XXX" Or A6C$(I) = "END" Then 2680
    For J = 1 To 15
        If PUS(J, I, 3) = 0 Then 2680 Else GoTo 2603

        2603 If PUS(J, I, 0) <= PUUC(0) Then 2613

        2604 PUUC(0) = PUS(J, I, 0): PUC$(0) = A6C$(I): PUYR(0) = PUS(J, I, 3)

        2613 If PUS(J, I, 1) <= PUUC(1) Then 2623

        2614 PUUC(1) = PUS(J, I, 1): PUC$(1) = A6C$(I): PUYR(1) = PUS(J, I, 3)

        2623 If PUS(J, I, 0) = 0 Then 2679
        If PUS(J, I, 1) / PUS(J, I, 0) <= PUUC(2) Then 2679

        2625 PUUC(2) = PUS(J, I, 1) / PUS(J, I, 0): PUC$(2) = A6C$(I): PUYR(2) = PUS(J, I, 3)

    2679 Next J

2680 Next I
For I = 0 To 9
    If A7C$(I) = "" Or A7C$(I) = "XXX" Or A7C$(I) = "END" Then 2780
    For J = 1 To 15
        If FGS(J, I, 16) = 0 Then 2780
        If FGS(J, I, 1) <= FGGC(0) Then 2713
        FGGC(0) = FGS(J, I, 1): FGC$(0) = A7C$(I): FGYR(0) = FGS(J, I, 16)

        2713 If FGS(J, I, 0) <= FGGC(1) Then 2723
        FGGC(1) = FGS(J, I, 0): FGC$(1) = A7C$(I): FGYR(1) = FGS(J, I, 16)

        2723 If FGS(J, I, 1) = 0 Then 2733
        If FGS(J, I, 0) / FGS(J, I, 1) <= FGGC(2) Or FGS(J, I, 1) < GAMES Then 2733

        2725 FGGC(2) = FGS(J, I, 0) / FGS(J, I, 1): FGC$(2) = A7C$(I): FGYR(2) = FGS(J, I, 16)

        2733 If FGS(J, I, 3) <= FGGC(3) Then 2743

        2734 FGGC(3) = FGS(J, I, 3): FGC$(3) = A7C$(I): FGYR(3) = FGS(J, I, 16)

        2743 If FGS(J, I, 2) <= FGGC(4) Then 2753

        2744 FGGC(4) = FGS(J, I, 2): FGC$(4) = A7C$(I): FGYR(4) = FGS(J, I, 16)

        2753 If FGS(J, I, 2) = 0 Then 2756
        If FGS(J, I, 2) / FGS(J, I, 3) <= FGGC(5) Or FGS(J, I, 3) < GAMES Then 2756

        2755 FGGC(5) = FGS(J, I, 2) / FGS(J, I, 3): FGC$(5) = A7C$(I): FGYR(5) = FGS(J, I, 16)

        2756 If FGS(J, I, 4) <= FGGC(6) Then 2779

        2757 FGGC(6) = FGS(J, I, 4): FGC$(6) = A7C$(I): FGYR(6) = FGS(J, I, 16)

    2779 Next J

2780 Next I
For I = 0 To 54
    If DIC$(I) = "" Or DIC$(I) = "XXX" Or DIC$(I) = "END" Then 2810
    For J = 1 To 15
        If GIS(J, I, 2) = 0 Then 2810

        2803 If GIS(J, I, 0) <= DFFC(0) Then 2809

        2804 DFFC(0) = GIS(J, I, 0): DFC$(0) = DIC$(I): FGYR(0) = GIS(J, I, 2)

    2809 Next J

2810 Next I
For I = 0 To 54
    If DSC$(I) = "" Or DSC$(I) = "XXX" Or DSC$(I) = "END" Then 2830
    For J = 1 To 15
        If GSS(J, I, 2) = 0 Then 2830

        2823 If GSS(J, I, 0) <= DFFC(1) Then 2829

        2824 DFFC(1) = GSS(J, I, 0): DFC$(1) = DSC$(I): FGYR(1) = GSS(J, I, 2)

    2829 Next J

2830 Next I
Open D$(1) + Z$ + ".CLD" For Output As #1
For I = 0 To 9: Write #1, RBBC(I), RBC$(I), RBYR(I): Next
Write #1, RBR$(0), RBR$(1)
For I = 0 To 4: Write #1, WRRC(I), WRC$(I), WRYR(I): Next
Write #1, WRR$
For I = 0 To 8: Write #1, QBBC(I), QBC$(I), QBYR(I): Next
Write #1, QBR$
For I = 0 To 4: Write #1, KRRC(I), KRC$(I), KRYR(I): Next
Write #1, KRR$
For I = 0 To 4: Write #1, PRRC(I), PRC$(I), PRYR(I): Next
Write #1, PRR$
For I = 0 To 2: Write #1, PUUC(I), PUC$(I), PUYR(I): Next
For I = 0 To 6: Write #1, FGGC(I), FGC$(I), FGYR(I): Next
For I = 0 To 1: Write #1, DFFC(I), DFC$(I), DFYR(I): Next
Close #1
Return

8999 If QBBS(J, I, 0) > 0 Then QBBS(J, I, 11) = Int(QBBS(J, I, 1) / QBBS(J, I, 0) * 1000) / 10
If QBBS(J, I, 0) = 0 Then QBBS(J, I, 11) = 0: QBBS(J, I, 8) = 0: QBBS(J, I, 10) = 0: QBBS(J, I, 9) = 0: GoTo 9005
QBBS(J, I, 8) = Int(QBBS(J, I, 5) / QBBS(J, I, 0) * 1000) / 10
QBBS(J, I, 10) = Int(QBBS(J, I, 4) / QBBS(J, I, 0) * 100) / 100
QBBS(J, I, 9) = Int(QBBS(J, I, 3) / QBBS(J, I, 0) * 1000) / 10

9005 PP(J, I) = QBBS(J, I, 11) - 30
If PP(J, I) < 0 Then PP(J, I) = 0: GoTo 9009
If PP(J, I) > 77.5 Then PP(J, I) = 2.375: GoTo 9009
PP(J, I) = PP(J, I) * .05

9009 If QBBS(J, I, 8) > 11.9 Then FF(J, I) = 2.375: GoTo 9011
FF(J, I) = QBBS(J, I, 8) * .2

9011 If QBBS(J, I, 9) = 0 Then II(J, I) = 0: GoTo 9014
If QBBS(J, I, 9) > 9.5 Then II(J, I) = 0: GoTo 9014
II(J, I) = 2.375 - (.25 * QBBS(J, I, 9))

9014 QBBS(J, I, 10) = QBBS(J, I, 10) - 3
If QBBS(J, I, 10) < 0 Then YY(J, I) = 0: GoTo 9020
If QBBS(J, I, 10) > 12.5 Then YY(J, I) = 2.375: GoTo 9020
D = Int(QBBS(J, I, 10) * 100) / 2
If D = QBBS(J, I, 10) / 2 Then YY(J, I) = QBBS(J, I, 10) / 2 * .3 + QBBS(J, I, 10) / 2 * .2: GoTo 9020
YY(J, I) = QBBS(J, I, 10) / 2 * .3 + QBBS(J, I, 10) / 2 * .2 + .003

9020 QBBS(J, I, 12) = PP(J, I) + FF(J, I) + II(J, I) + YY(J, I)
If QBBS(J, I, 12) > 9.5 Then QBBS(J, I, 12) = 158.3: GoTo 9027
QBBS(J, I, 12) = QBBS(J, I, 12) * 50 / 3

9027 QBBS(J, I, 12) = Int(QBBS(J, I, 12) * 10) / 10
Return

3056 Color 15, 0: Cls
JJ = 6
SN = 1
Locate 6, 8
Color 15, 4
'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To JJ
	Locate 6 + II, 8
	Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "THIS IS THE ID FOR TEAM YOU ARE UPDATING"
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then Z$ = "END": Clear: GoTo 1
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060

Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0
If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
ERASE teamItems$
GoSub 1100

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color PA%(2), PA%(1)
Print A$
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3056
Color 15, 0
Return

1100 NTMS = 0
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 934
    Get #1, I1
    For I2 = 1 To 15
        If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1105
    Next I2
    NTMS = NTMS + 1
    teamItems$(NTMS) = Q$(0): V%(NTMS) = I1

1105 Next I1
Close 1

1102 SELECTION% = 1
Count% = NTMS
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
21 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 21
If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 21
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 21
T1 = V%(SELECTION%)
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Get #1, T1
A$ = Q$(1)
For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1
Close 1

9006 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9006
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

4000 Spec$ = D$(1) + "*.CAR"
Count% = FCount%(Spec$)
If Count% = 0 Then 4500
ReDim careerFiles$(0 To Count%)
For X = 1 To Count%
    careerFiles$(X) = Space$(12)
Next
careerFiles$(0) = Spec$
Call ReadFile(careerFiles$())
'CALL SortStr(VARPTR(careerFiles$(1)), COUNT%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT FILE TO UPDATE": Color 15
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
Print "-SELECTS TEAM LIBRARY";

_MouseShow "DEFAULT"
20 MAMenu careerFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 1
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
targetFile$ = careerFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
Z$ = Left$(targetFile$, Len(targetFile$) - 4)
Return

4500 GoSub 4600
Locate , 10: Print "NO CAREER FILES WERE FOUND"
Locate , 10: Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
Clear
GoTo 1

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
