$Debug
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Career Files Menu"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

Dim Z2(50, 18), A1$(50), A2$(40), A3$(20), A4$(15), A5$(15), A6$(10), A7$(10)
Dim Z(38), Z1(38), teamItems$(500), V%(500), PA%(2), Q$(500)
Dim LR(50), LR$(50, 1), LC(60), LC$(60, 1), LP(20), LP$(20, 1), LK(15), LK$(15, 1), PK(15)
Dim PK$(15, 1), DI$(55), GIS(15, 55, 3), DS$(55), GSS(15, 55, 3), GS%(55, 1), GI%(55, 2)

'Some of the below were changed only to work with the code
' they may end up actually hiding issues???
Dim LF$(1), LI(9), LI$(9, 1), menuItems$(8)
Dim AF(9, 4), AM(9, 4), LF(9), D$(1)

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
Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next: Close #1
TMR = 6420: NB = -1: NC = -1: ND = -1: NE = -1: NF = -1: NG = -1: NH = -1: NI = -1: NJ = 0: NK = 0: D = 0: JX = 0: VV = 0
Color , 2: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "3 IN 1 FOOTBALL UTILITIES"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
menuItems$(1) = "CREATE AND MERGE INITIAL STAT FILE"
menuItems$(2) = "MERGE / UPDATE CAREER FILES       "
menuItems$(3) = "VIEW CAREER FILE                  "
menuItems$(4) = "PRINT CAREER FILE                 "
menuItems$(5) = "EDIT CAREER FILE                  "
menuItems$(6) = "VIEW / PRINT CAREER RECORDS       "
menuItems$(7) = "EXIT TO MAIN MENU                 "
SELECTION% = 1
Count% = 7
Start% = 1
SCANCODE% = 7
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 15, 59, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
RN = SELECTION%
If SCANCODE% = 27 Then RN = 7
If RN = 1 Then 1900
If RN = 2 Then Run "CAREER1.EXE"
If RN = 3 Then Run "CARVIEW.EXE"
If RN = 4 Then Run "CARPRNT.EXE"
If RN = 5 Then Run "CAREDIT.EXE"
If RN = 6 Then 4000
Run "HELLO.EXE"

7 '
Locate 8, 8: Color 15, 1

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
Locate 10, 15
Return

10 Cls: For J = 15 To 18: Locate J: Print "                                                                    "
Next
Color 11: Locate 15, 5: Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; Z$
GoSub 3056
B$ = A$: GoSub 6585
Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
Cls: Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; Y

Locate 9, 25: Print "ANY CHANGE (YN)"
60 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 10

VV = 1
Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"
76 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then JX = 1
If UCase$(I$) = "N" Then JX = 0
    
If Not _FileExists(D$(1) + Z1$) Then 900
Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
Locate 20, 20: Print String$(50, " ")

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

For I = 0 To 9
    T% = Len(A1$(I))
    If T% = 0 Then A1$(I) = "XXX"
Next
For I = 0 To 5
    T% = Len(A2$(I))
    If T% = 0 Then A2$(I) = "XXX"
Next
For I = 0 To 3
    T% = Len(A3$(I))
    If T% = 0 Then A3$(I) = "XXX"
Next
For I = 0 To 2
    T% = Len(A4$(I))
    If T% = 0 Then A4$(I) = "XXX"
Next
For I = 0 To 2
    T% = Len(A5$(I))
    If T% = 0 Then A5$(I) = "XXX"
Next
For I = 0 To 1
    T% = Len(A7$(I))
    If T% = 0 Then A7$(I) = "XXX"
Next
For I = 0 To 9
    T% = Len(DI$(I))
    If T% = 0 Then DI$(I) = "XXX"
Next
For I = 0 To 14
    T% = Len(DS$(I))
    If T% = 0 Then DS$(I) = "XXX"
Next
For I = 0 To 49
    N = 1
    If A1$(I) = "END" Or A1$(I) = "XXX" Or A1$(I) = "" Then 404
    NB = NB + 1: GoTo 408

    404 A1$(I) = ""

    408 For J = 0 To 5
        RBBS(N, I, J) = Z2(I, J)
    Next J

    RBBS(N, I, 6) = LR(I): RBS$(N, I, 0) = LR$(I, 0)
    RBBS(N, I, 9) = LC(I): RBS$(N, I, 1) = LC$(I, 0)
    RBBS(N, I, 7) = 1: RBBS(N, I, 8) = Y
Next I

For I = 0 To 39
    N = 1
    If A2$(I) = "END" Or A2$(I) = "XXX" Or A2$(I) = "" Then 424

    423 NC = NC + 1: GoTo 428

    424 A2$(I) = ""

    428 For J = 6 To 8
        WRRS(N, I, J - 6) = Z2(I, J)
    Next J

    WRRS(N, I, 3) = LC(I + 10): WRS$(N, I, 0) = LC$(I + 10, 0)
    WRRS(N, I, 4) = 1: WRRS(N, I, 5) = Y
Next I

For I = 0 To 19
    N = 1
    If A3$(I) = "END" Or A3$(I) = "XXX" Or A3$(I) = "" Then 444

    443 ND = ND + 1: GoTo 448

    444 A3$(I) = ""

    448 For J = 9 To 13: QBBS(N, I, J - 9) = Z2(I, J)
    Next J
    QBBS(N, I, 5) = LP(I): QBS$(N, I, 0) = LP$(I, 0)
    QBBS(N, I, 6) = 1: QBBS(N, I, 7) = Y
Next I
For I = 0 To 14
    N = 1
    If A4$(I) = "END" Or A4$(I) = "XXX" Or A4$(I) = "" Then 464

    463 NE = NE + 1: GoTo 468

    464 A4$(I) = ""

    468 For J = 14 To 16: KRS(N, I, J - 14) = Z2(I, J)
    Next J
    KRS(N, I, 3) = LK(I): KRRS$(N, I, 0) = LK$(I, 0)
    KRS(N, I, 4) = 1: KRS(N, I, 5) = Y
Next I
For I = 0 To 14
    N = 1
    If A5$(I) = "END" Or A5$(I) = "XXX" Or A5$(I) = "" Then 484

    483 NF = NF + 1: GoTo 488

    484 A5$(I) = ""

    488 For J = 14 To 16: PRS(N, I, J - 14) = Z2(I + 3, J)
    Next J
    PRS(N, I, 3) = PK(I): PRRS$(N, I, 0) = PK$(I, 0)
    PRS(N, I, 4) = 1: PRS(N, I, 5) = Y
Next I
For I = 0 To 9
    N = 1
    If A6$(I) = "END" Or A6$(I) = "XXX" Or A6$(I) = "" Then 504

    503 NG = NG + 1: GoTo 508

    504 A6$(I) = ""

    508 PUS(N, I, 0) = Z(7): PUS(N, I, 1) = Z(8)
    PUS(N, I, 2) = 1: PUS(N, I, 3) = Y
Next I
For I = 0 To 9
    N = 1
    If A7$(I) = "END" Or A7$(I) = "XXX" Or A7$(I) = "" Then 524

    523 NH = NH + 1: GoTo 528

    524 A7$(I) = ""

    528 If I = 0 Or I = 2 Or I = 4 Or I = 6 Or I = 8 Then 530
    If I = 1 Or I = 3 Or I = 5 Or I = 7 Or I = 9 Then 535

    530 For J = 3 To 6
        FGS(N, I, J - 3) = Z2(J, 17)
    Next J

    FGS(N, I, 4) = LF(I)
    FGS(N, I, 15) = 1: FGS(N, I, 16) = Y

    For I1 = 0 To 4: FGS(N, I, I1 + 5) = AM(I, I1): Next
    For I1 = 0 To 4: FGS(N, I, I1 + 10) = AF(I, I1): Next
    GoTo 536

    535 For J = 7 To 10:
    FGS(N, I, J - 7) = Z2(J, 17): Next J
    FGS(N, I, 4) = LF(I)
    FGS(N, I, 15) = 1: FGS(N, I, 16) = Y
    For I1 = 0 To 4: FGS(N, I, I1 + 5) = AM(I, I1): Next
    For I1 = 0 To 4: FGS(N, I, I1 + 10) = AF(I, I1): Next

536 Next I
For I = 0 To 54
    N = 1
    If DI$(I) = "END" Or DI$(I) = "XXX" Or DI$(I) = "" Then 554

    553 NJ = NJ + 1: GoTo 558

    554 DI$(I) = ""

    558 GIS(N, I, 0) = GI%(I, 0)
    GIS(N, I, 1) = 1: GIS(N, I, 2) = Y
Next I
For I = 0 To 54
    N = 1
    If DS$(I) = "END" Or DS$(I) = "XXX" Or DS$(I) = "" Then 584

    583 NK = NK + 1: GoTo 588

    584 DS$(I) = ""

    588 GSS(N, I, 0) = GS%(I, 0)
    GSS(N, I, 1) = 1: GSS(N, I, 2) = Y
Next I
If JX = 1 Then 704
If JX = 0 Then 705

704 GoSub 2080

705 Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
Locate 20, 20: Print "                                                  "
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
For N = 1 To 15: For J = 0 To 49: For K = 0 To 9: Write #2, RBBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 49: Write #2, RBS$(N, J, 0), RBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Write #2, WRRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: Write #2, WRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Write #2, QBBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: Write #2, QBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #2, KRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Write #2, KRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #2, PRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Write #2, PRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Write #2, PUS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Write #2, FGS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #2, GIS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #2, GSS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
Close #2
Clear: GoTo 1

900 Cls: Color 7: Print "UNABLE TO LOCATE STAT FILE"
Print: Line Input "HIT ENTER TO CONTINUE"; DUMMY$
Clear: GoTo 1

2080 If Not _FileExists(D$(1) + Left$(Z$, JX) + ".CLD") Then 2100
GoSub 9500
Open D$(1) + Left$(Z$, JX) + ".CLD" For Input As #1
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

2100 For I = 0 To 49
    If A1$(I) = "" Or A1$(I) = "XXX" Or A1$(I) = "END" Then 2180
    For J = 1 To 15
        If RBBS(J, I, 8) = 0 Then 2180

        2103 If RBBS(J, I, 0) <= RBBC(0) Then 2113

        2104 RBBC(0) = RBBS(J, I, 0): RBC$(0) = A1$(I): RBYR(0) = RBBS(J, I, 8)

        2113 If RBBS(J, I, 1) <= RBBC(1) Then 2123

        2114 RBBC(1) = RBBS(J, I, 1): RBC$(1) = A1$(I): RBYR(1) = RBBS(J, I, 8)

        2123 If RBBS(J, I, 0) = 0 Then 2133
        If RBBS(J, I, 1) / RBBS(J, I, 0) <= RBBC(2) Or RBBS(J, I, 0) < GAMES * 10 Then 2133

        2125 RBBC(2) = RBBS(J, I, 1) / RBBS(J, I, 0): RBC$(2) = A1$(I): RBYR(2) = RBBS(J, I, 8)

        2133 If RBBS(J, I, 2) <= RBBC(3) Then 2143

        2134 RBBC(3) = RBBS(J, I, 2): RBC$(3) = A1$(I): RBYR(3) = RBBS(J, I, 8)

        2143 If RBBS(J, I, 3) <= RBBC(4) Then 2153

        2144 RBBC(4) = RBBS(J, I, 3): RBC$(4) = A1$(I): RBYR(4) = RBBS(J, I, 8)

        2153 If RBBS(J, I, 4) <= RBBC(5) Then 2163

        2154 RBBC(5) = RBBS(J, I, 4): RBC$(5) = A1$(I): RBYR(5) = RBBS(J, I, 8)

        2163 If RBBS(J, I, 3) = 0 Then 2173
        If RBBS(J, I, 4) / RBBS(J, I, 3) <= RBBC(6) Or RBBS(J, I, 3) < GAMES * 3 Then 2173

        2165 RBBC(6) = RBBS(J, I, 4) / RBBS(J, I, 3): RBC$(6) = A1$(I): RBYR(6) = RBBS(J, I, 8)

        2173 If RBBS(J, I, 5) <= RBBC(7) Then 2175

        2174 RBBC(7) = RBBS(J, I, 5): RBC$(7) = A1$(I): RBYR(7) = RBBS(J, I, 8)

        2175 If RBBS(J, I, 6) <= RBBC(8) Then 2177

        2176 RBBC(8) = RBBS(J, I, 6): RBC$(8) = A1$(I): RBYR(8) = RBBS(J, I, 8): RBR$(0) = RBS$(J, I, 0)

        2177 If RBBS(J, I, 9) <= RBBC(9) Then 2179

        2178 RBBC(9) = RBBS(J, I, 9): RBC$(9) = A1$(I): RBYR(9) = RBBS(J, I, 8): RBR$(1) = RBS$(J, I, 1)

    2179 Next J

2180 Next I
For I = 0 To 39
    If A2$(I) = "" Or A2$(I) = "XXX" Or A2$(I) = "END" Then 2280
    For J = 1 To 15
        If WRRS(J, I, 5) = 0 Then 2280 Else GoTo 2203

        2203 If WRRS(J, I, 0) <= WRRC(0) Then 2213

        2204 WRRC(0) = WRRS(J, I, 0): WRC$(0) = A2$(I): WRYR(0) = WRRS(J, I, 5)

        2213 If WRRS(J, I, 1) <= WRRC(1) Then 2223

        2214 WRRC(1) = WRRS(J, I, 1): WRC$(1) = A2$(I): WRYR(1) = WRRS(J, I, 5)

        2223 If WRRS(J, I, 0) = 0 Then 2233
        If WRRS(J, I, 1) / WRRS(J, I, 0) <= WRRC(2) Or WRRS(J, I, 0) < GAMES * 3 Then 2233

        2225 WRRC(2) = WRRS(J, I, 1) / WRRS(J, I, 0): WRC$(2) = A2$(I): WRYR(2) = WRRS(J, I, 5)

        2233 If WRRS(J, I, 2) <= WRRC(3) Then 2235

        2234 WRRC(3) = WRRS(J, I, 2): WRC$(3) = A2$(I): WRYR(3) = WRRS(J, I, 5)

        2235 If WRRS(J, I, 3) <= WRRC(4) Then 2279

        2236 WRRC(4) = WRRS(J, I, 3): WRC$(4) = A2$(I): WRYR(4) = WRRS(J, I, 5): WRR$ = WRS$(J, I, 0)

    2279 Next J

2280 Next I
For I = 0 To 19
    If A3$(I) = "" Or A3$(I) = "XXX" Or A3$(I) = "END" Then 2380
    For J = 1 To 15: GoSub 8999
        If QBBS(J, I, 7) = 0 Then 2380

        2303 If QBBS(J, I, 1) <= QBBC(0) Then 2313

        2304 QBBC(0) = QBBS(J, I, 1): QBC$(0) = A3$(I): QBYR(0) = QBBS(J, I, 7)

        2313 If QBBS(J, I, 0) <= QBBC(1) Then 2323

        2314 QBBC(1) = QBBS(J, I, 0): QBC$(1) = A3$(I): QBYR(1) = QBBS(J, I, 7)

        2323 If QBBS(J, I, 2) <= QBBC(2) Then 2333

        2324 QBBC(2) = QBBS(J, I, 2): QBC$(2) = A3$(I): QBYR(2) = QBBS(J, I, 7)

        2333 If QBBS(J, I, 4) <= QBBC(3) Then 2343

        2334 QBBC(3) = QBBS(J, I, 4): QBC$(3) = A3$(I): QBYR(3) = QBBS(J, I, 7)

        2343 If QBBS(J, I, 3) <= QBBC(4) Then 2353

        2344 QBBC(4) = QBBS(J, I, 3): QBC$(4) = A3$(I): QBYR(4) = QBBS(J, I, 7)

        2353 If QBBS(J, I, 0) = 0 Then 2363
        If QBBS(J, I, 1) / QBBS(J, I, 0) <= QBBC(5) Or QBBS(J, I, 0) < GAMES * 15 Then 2363

        2355 QBBC(5) = QBBS(J, I, 1) / QBBS(J, I, 0): QBC$(5) = A3$(I): QBYR(5) = QBBS(J, I, 7)

        2363 If QBBS(J, I, 1) = 0 Then 2373
        If QBBS(J, I, 3) / QBBS(J, I, 1) <= QBBC(6) Or QBBS(J, I, 0) < GAMES * 15 Then 2373

        2365 QBBC(6) = QBBS(J, I, 3) / QBBS(J, I, 1): QBC$(6) = A3$(I): QBYR(6) = QBBS(J, I, 7)

        2373 If QBBS(J, I, 12) <= QBBC(7) Then 2375

        2374 QBBC(7) = QBBS(J, I, 12): QBC$(7) = A3$(I): QBYR(7) = QBBS(J, I, 7)

        2375 If QBBS(J, I, 5) <= QBBC(8) Then 2379

        2376 QBBC(8) = QBBS(J, I, 5)
        QBC$(8) = A3$(I)
        QBYR(8) = QBBS(J, I, 7)
        QBR$ = QBS$(J, I, 0)

    2379 Next J

2380 Next I
For I = 0 To 14
    If A4$(I) = "" Or A4$(I) = "XXX" Or A4$(I) = "END" Then 2480
    For J = 1 To 15
        If KRS(J, I, 5) = 0 Then 2480

        2403 If KRS(J, I, 0) <= KRRC(0) Then 2413

        2404 KRRC(0) = KRS(J, I, 0): KRC$(0) = A4$(I): KRYR(0) = KRS(J, I, 5)

        2413 If KRS(J, I, 1) <= KRRC(1) Then 2423

        2414 KRRC(1) = KRS(J, I, 1): KRC$(1) = A4$(I): KRYR(1) = KRS(J, I, 5)

        2423 If KRS(J, I, 0) = 0 Then 2433
        If KRS(J, I, 1) / KRS(J, I, 0) <= KRRC(2) Or KRS(J, I, 0) < GAMES * 1.2 Then 2433

        2425 KRRC(2) = KRS(J, I, 1) / KRS(J, I, 0): KRC$(2) = A4$(I): KRYR(2) = KRS(J, I, 5)

        2433 If KRS(J, I, 2) <= KRRC(3) Then 2435

        2434 KRRC(3) = KRS(J, I, 2): KRC$(3) = A4$(I): KRYR(3) = KRS(J, I, 5)

        2435 If KRS(J, I, 3) <= KRRC(4) Then 2479

        2436 KRRC(4) = KRS(J, I, 3): KRC$(4) = A4$(I): KRYR(4) = KRS(J, I, 5): KRR$ = KRRS$(J, I, 0)

    2479 Next J

2480 Next I
For I = 0 To 14
    If A5$(I) = "" Or A5$(I) = "XXX" Or A5$(I) = "END" Then 2580
    For J = 1 To 15
        If PRS(J, I, 5) = 0 Then 2580

        2503 If PRS(J, I, 0) <= PRRC(0) Then 2513

        2504 PRRC(0) = PRS(J, I, 0): PRC$(0) = A5$(I): PRYR(0) = PRS(J, I, 5)

        2513 If PRS(J, I, 1) <= PRRC(1) Then 2523

        2514 PRRC(1) = PRS(J, I, 1): PRC$(1) = A5$(I): PRYR(1) = PRS(J, I, 5)

        2523 If PRS(J, I, 0) = 0 Then 2533
        If PRS(J, I, 1) / PRS(J, I, 0) <= PRRC(2) Or PRS(J, I, 0) < GAMES * 1.2 Then 2533

        2525 PRRC(2) = PRS(J, I, 1) / PRS(J, I, 0): PRC$(2) = A5$(I): PRYR(2) = PRS(J, I, 5)

        2533 If PRS(J, I, 2) <= PRRC(3) Then 2535

        2534 PRRC(3) = PRS(J, I, 2): PRC$(3) = A5$(I): PRYR(3) = PRS(J, I, 5)

        2535 If PRS(J, I, 3) <= PRRC(4) Then 2579

        2536 PRRC(4) = PRS(J, I, 3): PRC$(4) = A5$(I): PRYR(4) = PRS(J, I, 5): PRR$ = PRRS$(J, I, 0)

    2579 Next J

2580 Next I
For I = 0 To 9
    If A6$(I) = "" Or A6$(I) = "XXX" Or A6$(I) = "END" Then 2680
    For J = 1 To 15
        If PUS(J, I, 3) = 0 Then 2680

        2603 If PUS(J, I, 0) <= PUUC(0) Then 2613

        2604 PUUC(0) = PUS(J, I, 0): PUC$(0) = A6$(I): PUYR(0) = PUS(J, I, 3)

        2613 If PUS(J, I, 1) <= PUUC(1) Then 2623

        2614 PUUC(1) = PUS(J, I, 1): PUC$(1) = A6$(I): PUYR(1) = PUS(J, I, 3)

        2623 If PUS(J, I, 0) = 0 Then 2679
        If PUS(J, I, 1) / PUS(J, I, 0) <= PUUC(2) Then 2679

        2625 PUUC(2) = PUS(J, I, 1) / PUS(J, I, 0): PUC$(2) = A6$(I): PUYR(2) = PUS(J, I, 3)

    2679 Next J

2680 Next I
For I = 0 To 9
    If A7$(I) = "" Or A7$(I) = "XXX" Or A7$(I) = "END" Then 2780
    For J = 1 To 15
        If FGS(J, I, 16) = 0 Then 2780

        2703 If FGS(J, I, 1) <= FGGC(0) Then 2713

        2704 FGGC(0) = FGS(J, I, 1): FGC$(0) = A7$(I): FGYR(0) = FGS(J, I, 16)

        2713 If FGS(J, I, 0) <= FGGC(1) Then 2723

        2714 FGGC(1) = FGS(J, I, 0): FGC$(1) = A7$(I): FGYR(1) = FGS(J, I, 16)

        2723 If FGS(J, I, 1) = 0 Then 2733
        If FGS(J, I, 0) / FGS(J, I, 1) <= FGGC(2) Or FGS(J, I, 1) < GAMES Then 2733

        2725 FGGC(2) = FGS(J, I, 0) / FGS(J, I, 1): FGC$(2) = A7$(I): FGYR(2) = FGS(J, I, 16)

        2733 If FGS(J, I, 3) <= FGGC(3) Then 2743

        2734 FGGC(3) = FGS(J, I, 3): FGC$(3) = A7$(I): FGYR(3) = FGS(J, I, 16)

        2743 If FGS(J, I, 2) <= FGGC(4) Then 2753

        2744 FGGC(4) = FGS(J, I, 2): FGC$(4) = A7$(I): FGYR(4) = FGS(J, I, 16)

        2753 If FGS(J, I, 2) = 0 Then 2756
        If FGS(J, I, 2) / FGS(J, I, 3) <= FGGC(5) Or FGS(J, I, 3) < GAMES Then 2756

        2755 FGGC(5) = FGS(J, I, 2) / FGS(J, I, 3): FGC$(5) = A7$(I): FGYR(5) = FGS(J, I, 16)

        2756 If FGS(J, I, 4) <= FGGC(6) Then 2779

        2757 FGGC(6) = FGS(J, I, 4): FGC$(6) = A7$(I): FGYR(6) = FGS(J, I, 16)

    2779 Next J

2780 Next I
For I = 0 To 54
    If DI$(I) = "" Or DI$(I) = "XXX" Or DI$(I) = "END" Then 2810
    For J = 1 To 15
        If GIS(J, I, 2) = 0 Then 2810

        2803 If GIS(J, I, 0) <= DFFC(0) Then 2809

        2804 DFFC(0) = GIS(J, I, 0): DFC$(0) = DI$(I): FGYR(0) = GIS(J, I, 2)

    2809 Next J

2810 Next I
For I = 0 To 54
    If DS$(I) = "" Or DS$(I) = "XXX" Or DS$(I) = "END" Then 2830
    For J = 1 To 15
        If GSS(J, I, 2) = 0 Then 2830

        2823 If GSS(J, I, 0) <= DFFC(1) Then 2829

        2824 DFFC(1) = GSS(J, I, 0): DFC$(1) = DS$(I): FGYR(1) = GSS(J, I, 2)

    2829 Next J

2830 Next I
If VV = 1 Then 2950
If VV = 0 Then 2981

2950 Open D$(1) + Z$ + ".CLD" For Output As #1: GoTo 2982

2981 GoSub 9500: Open D$(1) + Left$(Z$, JX) + ".CLD" For Output As #1

2982 For I = 0 To 9: Write #1, RBBC(I), RBC$(I), RBYR(I): Next
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

4000 Spec$ = D$(1) + "*.CLD"
Count% = FCount%(Spec$)
If Count% = 0 Then 4500
ReDim conferenceLeaderFiles$(0 To Count%)
For X = 1 To Count%
    conferenceLeaderFiles$(X) = Space$(12)
Next
conferenceLeaderFiles$(0) = Spec$
Call ReadFile(conferenceLeaderFiles$())
'CALL SortStr(VARPTR(conferenceLeaderFiles$(1)), COUNT%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT FILE TO VIEW": Color 15
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
Print "-SELECTS FILE";

_MouseShow "DEFAULT"
20 MAMenu conferenceLeaderFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 1
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
targetFile$ = conferenceLeaderFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
ZZ$ = Left$(targetFile$, Len(targetFile$) - 4)
Open D$(1) + ZZ$ + ".CLD" For Input As #1
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

Cls: Locate 5, 10: Color 12: Print "P";: Color 7: Print "rint or ";: Color 12: Print "V";: Color 7: Print "iew"
5101 I$ = GetKeyPress$
If UCase$(I$) = "V" Then 5200
If UCase$(I$) = "P" Then 6200
If UCase$(I$) <> "V" And UCase$(I$) <> "P" Then 5101

5200 Cls: Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
Color 3: Print "RUSHING ATTEMPTS";: Color 7: Print Tab(32); Using "####"; RBBC(0);: Print Tab(50); RBC$(0);: Print Tab(64); RBYR(0)
Color 3: Print "RUSHING YARDS";: Color 7: Print Tab(32); Using "####"; RBBC(1);: Print Tab(50); RBC$(1);: Print Tab(64); RBYR(1)
Color 3: Print "RUSHING AVERAGE";: Color 7: Print Tab(32); Using "##.#"; RBBC(2);: Print Tab(50); RBC$(2);: Print Tab(64); RBYR(2)
Color 3: Print "RUSHING TOUCHDOWNS";: Color 7: Print Tab(33); Using "###"; RBBC(3);: Print Tab(50); RBC$(3);: Print Tab(64); RBYR(3)
Color 3: Print "LONGEST RUN";: Color 7: Print Tab(33); Using "###"; RBBC(8);: Print Tab(36); RBR$(0);: Print Tab(50); RBC$(8);: Print Tab(64); RBYR(8)
Color 3: Print "RECEPTIONS (BACKS)";: Color 7: Print Tab(32); Using "####"; RBBC(4);: Print Tab(50); RBC$(4);: Print Tab(64); RBYR(4)
Color 3: Print "RECEIVING YARDS (BACKS)";: Color 7: Print Tab(32); Using "####"; RBBC(5);: Print Tab(50); RBC$(5);: Print Tab(64); RBYR(5)
Color 3: Print "RECEIVING AVERAGE (BACKS)";: Color 7: Print Tab(32); Using "##.#"; RBBC(6);: Print Tab(50); RBC$(6);: Print Tab(64); RBYR(6)
Color 3: Print "RECEIVING TOUCHDOWNS (BACKS)";: Color 7: Print Tab(32); Using " ###"; RBBC(7);: Print Tab(50); RBC$(7);: Print Tab(64); RBYR(7)
Color 3: Print "LONGEST RECEPTION (BACKS)";: Color 7: Print Tab(33); Using "###"; RBBC(9);: Print Tab(37); RBR$(1);: Print Tab(50); RBC$(9);: Print Tab(64); RBYR(9)
Color 3: Print "RECEPTIONS";: Color 7: Print Tab(32); Using "####"; WRRC(0);: Print Tab(50); WRC$(0);: Print Tab(64); WRYR(0)
Color 3: Print "RECEIVING YARDS";: Color 7: Print Tab(32); Using "####"; WRRC(1);: Print Tab(50); WRC$(1);: Print Tab(64); WRYR(1)
Color 3: Print "RECEIVING AVERAGE";: Color 7: Print Tab(32); Using "##.#"; WRRC(2);: Print Tab(50); WRC$(2);: Print Tab(64); WRYR(2)
Color 3: Print "RECEIVING TOUCHDOWNS";: Color 7: Print Tab(32); Using " ###"; WRRC(3);: Print Tab(50); WRC$(3);: Print Tab(64); WRYR(3)
Color 3: Print "LONGEST RECEPTION";: Color 7: Print Tab(32); Using " ###"; WRRC(4);: Print Tab(36); WRR$;: Print Tab(50); WRC$(4);: Print Tab(64); WRYR(4)
Color 3: Print "COMPLETIONS";: Color 7: Print Tab(32); Using "####"; QBBC(0);: Print Tab(50); QBC$(0);: Print Tab(64); QBYR(0)
Color 3: Print "ATTEMPTS";: Color 7: Print Tab(32); Using "####"; QBBC(1);: Print Tab(50); QBC$(1);: Print Tab(64); QBYR(1)
Color 3: Print "INTERCEPTIONS";: Color 7: Print Tab(32); Using "####"; QBBC(2);: Print Tab(50); QBC$(2);: Print Tab(64); QBYR(2)
Color 15: Print: Print "HIT ANY KEY TO CONTINUE"
5251 I$ = GetKeyPress$

Cls: Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
Color 3: Print "TOUCHDOWNS";: Color 7: Print Tab(31); Using " ####"; QBBC(3);: Print Tab(50); QBC$(3);: Print Tab(64); QBYR(3)
Color 3: Print "PASSING YARDS";: Color 7: Print Tab(32); Using "####"; QBBC(4);: Print Tab(50); QBC$(4);: Print Tab(64); QBYR(4)
Color 3: Print "COMPLETION PERCENTAGE";: Color 7: Print Tab(32); Using "##.#"; QBBC(5) * 100;: Print Tab(50); QBC$(5);: Print Tab(64); QBYR(5)
Color 3: Print "YARDS PER COMPLETION";: Color 7: Print Tab(32); Using "##.#"; QBBC(6);: Print Tab(50); QBC$(6);: Print Tab(64); QBYR(6)
Color 3: Print "QUARTERBACK RATING";: Color 7: Print Tab(31); Using " ##.#"; QBBC(7);: Print Tab(50); QBC$(7);: Print Tab(64); QBYR(7)
Color 3: Print "LONGEST PASS";: Color 7: Print Tab(32); Using " ###"; QBBC(8);: Print Tab(36); QBR$;: Print Tab(50); QBC$(8);: Print Tab(64); QBYR(8)
Color 3: Print "KICK RETURN ATTEMPTS";: Color 7: Print Tab(33); Using "###"; KRRC(0);: Print Tab(50); KRC$(0);: Print Tab(64); KRYR(0)
Color 3: Print "KICK RETURN YARDS";: Color 7: Print Tab(32); Using "####"; KRRC(1);: Print Tab(50); KRC$(1);: Print Tab(64); KRYR(1)
Color 3: Print "KICK RETURN AVERAGE";: Color 7: Print Tab(32); Using "##.#"; KRRC(2);: Print Tab(50); KRC$(2);: Print Tab(64); KRYR(2)
Color 3: Print "KICK RETURN TOUCHDOWNS";: Color 7: Print Tab(32); Using " ###"; KRRC(3);: Print Tab(50); KRC$(3);: Print Tab(64); KRYR(3)
Color 3: Print "LONGEST KICK RETURN";: Color 7: Print Tab(32); Using " ###"; KRRC(4);: Print Tab(36); KRR$;: Print Tab(50); KRC$(4);: Print Tab(64); KRYR(4)
Color 3: Print "PUNT RETURN ATTEMPTS";: Color 7: Print Tab(33); Using "###"; PRRC(0);: Print Tab(50); PRC$(0);: Print Tab(64); PRYR(0)
Color 3: Print "PUNT RETURN YARDS";: Color 7: Print Tab(32); Using "####"; PRRC(1);: Print Tab(50); PRC$(1);: Print Tab(64); PRYR(1)
Color 3: Print "PUNT RETURN AVERAGE";: Color 7: Print Tab(32); Using "##.#"; PRRC(2);: Print Tab(50); PRC$(2);: Print Tab(64); PRYR(2)
Color 3: Print "PUNT RETURN TOUCHDOWNS";: Color 7: Print Tab(32); Using " ###"; PRRC(3);: Print Tab(50); PRC$(3);: Print Tab(64); PRYR(3)
Color 3: Print "LONGEST PUNT RETURN";: Color 7: Print Tab(32); Using " ###"; PRRC(4);: Print Tab(36); PRR$;: Print Tab(50); PRC$(4);: Print Tab(64); PRYR(4)
Color 3: Print "PUNT ATTEMPTS";: Color 7: Print Tab(32); Using "####"; PUUC(0);: Print Tab(50); PUC$(0);: Print Tab(64); PUYR(0)
Color 3: Print "PUNTING YARDS";: Color 7: Print Tab(32); Using "####"; PUUC(1);: Print Tab(50); PUC$(1);: Print Tab(64); PUYR(1)
Color 15: Print: Print "HIT ANY KEY TO CONTINUE"
5261 I$ = GetKeyPress$

Cls: Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
Color 3: Print "PUNTING AVERAGE";: Color 7: Print Tab(32); Using "##.#"; PUUC(2);: Print Tab(50); PUC$(2);: Print Tab(64); PUYR(2)
Color 3: Print "FIELD GOAL ATTEMPTS";: Color 7: Print Tab(32); Using "####"; FGGC(0);: Print Tab(50); FGC$(0);: Print Tab(64); FGYR(0)
Color 3: Print "FIELD GOALS MADE";: Color 7: Print Tab(32); Using "####"; FGGC(1);: Print Tab(50); FGC$(1);: Print Tab(64); FGYR(1)
Color 3: Print "FIELD GOAL PERCENTAGE";: Color 7: Print Tab(31); Using "###.#"; FGGC(2) * 100;: Print Tab(50); FGC$(2);: Print Tab(64); FGYR(2)
Color 3: Print "LONGEST FIELD GOAL MADE";: Color 7: Print Tab(34); Using "##"; FGGC(6);: Print Tab(50); FGC$(6);: Print Tab(64); FGYR(6)
Color 3: Print "EXTRA POINT ATTEMPTS";: Color 7: Print Tab(32); Using "####"; FGGC(3);: Print Tab(50); FGC$(3);: Print Tab(64); FGYR(3)
Color 3: Print "EXTRA POINTS MADE";: Color 7: Print Tab(32); Using "####"; FGGC(4);: Print Tab(50); FGC$(4);: Print Tab(64); FGYR(4)
Color 3: Print "EXTRA POINT PERCENTAGE";: Color 7: Print Tab(31); Using "###.#"; FGGC(5) * 100;: Print Tab(50); FGC$(5);: Print Tab(64); FGYR(5)
Color 15: Print: Print "VIEW FILE AGAIN (Y/N)"
5391 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 5200
Clear: GoTo 1

6200 LPrint Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
LPrint "RUSHING ATTEMPTS";: LPrint Tab(32); Using "####"; RBBC(0);: LPrint Tab(50); RBC$(0);: LPrint Tab(64); RBYR(0)
LPrint "RUSHING YARDS";: LPrint Tab(32); Using "####"; RBBC(1);: LPrint Tab(50); RBC$(1);: LPrint Tab(64); RBYR(1)
LPrint "RUSHING AVERAGE";: LPrint Tab(32); Using "##.#"; RBBC(2);: LPrint Tab(50); RBC$(2);: LPrint Tab(64); RBYR(2)
LPrint "RUSHING TOUCHDOWNS";: LPrint Tab(33); Using "###"; RBBC(3);: LPrint Tab(50); RBC$(3);: LPrint Tab(64); RBYR(3)
LPrint "LONGEST RUN";: LPrint Tab(33); Using "###"; RBBC(8);: LPrint Tab(36); RBR$(0);: LPrint Tab(50); RBC$(8);: LPrint Tab(64); RBYR(8)
LPrint "RECEPTIONS (BACKS)";: LPrint Tab(32); Using "####"; RBBC(4);: LPrint Tab(50); RBC$(4);: LPrint Tab(64); RBYR(4)
LPrint "RECEIVING YARDS (BACKS)";: LPrint Tab(32); Using "####"; RBBC(5);: LPrint Tab(50); RBC$(5);: LPrint Tab(64); RBYR(5)
LPrint "RECEIVING AVERAGE (BACKS)";: LPrint Tab(32); Using "##.#"; RBBC(6);: LPrint Tab(50); RBC$(6);: LPrint Tab(64); RBYR(6)
LPrint "RECEIVING TOUCHDOWNS (BACKS)";: LPrint Tab(32); Using " ###"; RBBC(7);: LPrint Tab(50); RBC$(7);: LPrint Tab(64); RBYR(7)
LPrint "LONGEST RECEPTION (BACKS)";: LPrint Tab(33); Using "###"; RBBC(9);: LPrint Tab(37); RBR$(1);: LPrint Tab(50); RBC$(9);: LPrint Tab(64); RBYR(9)
LPrint "RECEPTIONS";: LPrint Tab(32); Using "####"; WRRC(0);: LPrint Tab(50); WRC$(0);: LPrint Tab(64); WRYR(0)
LPrint "RECEIVING YARDS";: LPrint Tab(32); Using "####"; WRRC(1);: LPrint Tab(50); WRC$(1);: LPrint Tab(64); WRYR(1)
LPrint "RECEIVING AVERAGE";: LPrint Tab(32); Using "##.#"; WRRC(2);: LPrint Tab(50); WRC$(2);: LPrint Tab(64); WRYR(2)
LPrint "RECEIVING TOUCHDOWNS";: LPrint Tab(32); Using " ###"; WRRC(3);: LPrint Tab(50); WRC$(3);: LPrint Tab(64); WRYR(3)
LPrint "LONGEST RECEPTION";: LPrint Tab(32); Using " ###"; WRRC(4);: LPrint Tab(36); WRR$;: LPrint Tab(50); WRC$(4);: LPrint Tab(64); WRYR(4)
LPrint "COMPLETIONS";: LPrint Tab(32); Using "####"; QBBC(0);: LPrint Tab(50); QBC$(0);: LPrint Tab(64); QBYR(0)
LPrint "ATTEMPTS";: LPrint Tab(32); Using "####"; QBBC(1);: LPrint Tab(50); QBC$(1);: LPrint Tab(64); QBYR(1)
LPrint "INTERCEPTIONS";: LPrint Tab(32); Using "####"; QBBC(2);: LPrint Tab(50); QBC$(2);: LPrint Tab(64); QBYR(2)
LPrint "TOUCHDOWNS";: LPrint Tab(31); Using " ####"; QBBC(3);: LPrint Tab(50); QBC$(3);: LPrint Tab(64); QBYR(3)
LPrint "PASSING YARDS";: LPrint Tab(32); Using "####"; QBBC(4);: LPrint Tab(50); QBC$(4);: LPrint Tab(64); QBYR(4)
LPrint "COMPLETION PERCENTAGE";: LPrint Tab(32); Using "##.#"; QBBC(5) * 100;: LPrint Tab(50); QBC$(5);: LPrint Tab(64); QBYR(5)
LPrint "YARDS PER COMPLETION";: LPrint Tab(32); Using "##.#"; QBBC(6);: LPrint Tab(50); QBC$(6);: LPrint Tab(64); QBYR(6)
LPrint "QUARTERBACK RATING";: LPrint Tab(31); Using " ##.#"; QBBC(7);: LPrint Tab(50); QBC$(7);: LPrint Tab(64); QBYR(7)
LPrint "LONGEST PASS";: LPrint Tab(32); Using " ###"; QBBC(8);: LPrint Tab(36); QBR$;: LPrint Tab(50); QBC$(8);: LPrint Tab(64); QBYR(8)
LPrint "KICK RETURN ATTEMPTS";: LPrint Tab(33); Using "###"; KRRC(0);: LPrint Tab(50); KRC$(0);: LPrint Tab(64); KRYR(0)
LPrint "KICK RETURN YARDS";: LPrint Tab(32); Using "####"; KRRC(1);: LPrint Tab(50); KRC$(1);: LPrint Tab(64); KRYR(1)
LPrint "KICK RETURN AVERAGE";: LPrint Tab(32); Using "##.#"; KRRC(2);: LPrint Tab(50); KRC$(2);: LPrint Tab(64); KRYR(2)
LPrint "KICK RETURN TOUCHDOWNS";: LPrint Tab(32); Using " ###"; KRRC(3);: LPrint Tab(50); KRC$(3);: LPrint Tab(64); KRYR(3)
LPrint "LONGEST KICK RETURN";: LPrint Tab(32); Using " ###"; KRRC(4);: LPrint Tab(36); KRR$;: LPrint Tab(50); KRC$(4);: LPrint Tab(64); KRYR(4)
LPrint "PUNT RETURN ATTEMPTS";: LPrint Tab(33); Using "###"; PRRC(0);: LPrint Tab(50); PRC$(0);: LPrint Tab(64); PRYR(0)
LPrint "PUNT RETURN YARDS";: LPrint Tab(32); Using "####"; PRRC(1);: LPrint Tab(50); PRC$(1);: LPrint Tab(64); PRYR(1)
LPrint "PUNT RETURN AVERAGE";: LPrint Tab(32); Using "##.#"; PRRC(2);: LPrint Tab(50); PRC$(2);: LPrint Tab(64); PRYR(2)
LPrint "PUNT RETURN TOUCHDOWNS";: LPrint Tab(32); Using " ###"; PRRC(3);: LPrint Tab(50); PRC$(3);: LPrint Tab(64); PRYR(3)
LPrint "LONGEST PUNT RETURN";: LPrint Tab(32); Using " ###"; PRRC(4);: LPrint Tab(36); PRR$;: LPrint Tab(50); PRC$(4);: LPrint Tab(64); PRYR(4)
LPrint "PUNT ATTEMPTS";: LPrint Tab(32); Using "####"; PUUC(0);: LPrint Tab(50); PUC$(0);: LPrint Tab(64); PUYR(0)
LPrint "PUNTING YARDS";: LPrint Tab(32); Using "####"; PUUC(1);: LPrint Tab(50); PUC$(1);: LPrint Tab(64); PUYR(1)
LPrint "PUNTING AVERAGE";: LPrint Tab(32); Using "##.#"; PUUC(2);: LPrint Tab(50); PUC$(2);: LPrint Tab(64); PUYR(2)
LPrint "FIELD GOAL ATTEMPTS";: LPrint Tab(32); Using "####"; FGGC(0);: LPrint Tab(50); FGC$(0);: LPrint Tab(64); FGYR(0)
LPrint "FIELD GOALS MADE";: LPrint Tab(32); Using "####"; FGGC(1);: LPrint Tab(50); FGC$(1);: LPrint Tab(64); FGYR(1)
LPrint "FIELD GOAL PERCENTAGE";: LPrint Tab(31); Using "###.#"; FGGC(2) * 100;: LPrint Tab(50); FGC$(2);: LPrint Tab(64); FGYR(2)
LPrint "LONGEST FIELD GOAL MADE";: LPrint Tab(34); Using "##"; FGGC(6);: LPrint Tab(50); FGC$(6);: LPrint Tab(64); FGYR(6)
LPrint "EXTRA POINT ATTEMPTS";: LPrint Tab(32); Using "####"; FGGC(3);: LPrint Tab(50); FGC$(3);: LPrint Tab(64); FGYR(3)
LPrint "EXTRA POINTS MADE";: LPrint Tab(32); Using "####"; FGGC(4);: LPrint Tab(50); FGC$(4);: LPrint Tab(64); FGYR(4)
LPrint "EXTRA POINT PERCENTAGE";: LPrint Tab(31); Using "###.#"; FGGC(5) * 100;: LPrint Tab(50); FGC$(5);: LPrint Tab(64); FGYR(5)
Clear: GoTo 1

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

9500 D = 0
For J = 0 To 8
    D = D + 1
    X$ = Mid$(Z$, D, 1)
    If X$ = "." Then JX = D - 1
Next J
Return

3056 Color 15, 0: Cls
JJ = 6
SN = 1
Locate 6, 8: Color 15, 4
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
Erase teamItems$
GoSub 1100

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color PA%(2), PA%(1): Print A$
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
    
21 _MouseShow "DEFAULT"
MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
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

'Positions 1 and 2 will hold team colors
For I1 = 0 To 2
    PA%(I1) = CVI(Q$(I1 + 11))
Next I1
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

4500 GoSub 4600
Print "NO CAREER FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
Clear: GoTo 1

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
    Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

1900 Color 14, 0: Cls: Locate 5, 27: Print " - - - C A U T I O N - - -"

1901 Color 15: Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"

1902 Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"

1903 Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"

1904 Locate , 18: Print "            USE OPTION #2."

1905 Print: Print: Print

1906 Color 12: Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"
1907 I$ = GetKeyPress$
1908 If UCase$(I$) = "Y" Then GoTo 10
GoTo 1

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
