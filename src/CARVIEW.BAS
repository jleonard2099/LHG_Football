'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - View Career Files"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim A1$(1, 50), A2$(1, 40), A3$(1, 20), A4$(1, 15), A5$(1, 15), A6$(1, 10), A7$(1, 10), TT(50)
Dim RC(6), RC$(6), D$(1)
Dim FF(15, 30), YY(15, 30), II(15, 30), PP(15, 30), RBS$(15, 50, 1), WRS$(15, 40, 1), QBS$(15, 20, 1), PRS$(15, 15, 1), KRS$(15, 15, 1)
ReDim DI$(1, 55), GIS(15, 55, 3), DS$(1, 55), GSS(15, 55, 3)', GS%(1, 55), GI%(1, 55)
ReDim RBS(15, 50, 11), WRS(15, 40, 11), QBS(15, 20, 13), PRS(15, 15, 11), KRS(15, 15, 11), PUS(15, 10, 4), FGS(15, 10, 17)

Open "DEV.BAS" For Input As #1
For I = 0 To 1: Input #1, D$(I): Next
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
Print "-SELECTS FILE";

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
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310
    
F$ = RTrim$(F$)
Z$ = Left$(F$, Len(F$) - 4)
Color 15: Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"
    
Open D$(1) + Z$ + ".CAR" For Input As #1
Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
For I = 0 To 49: Input #1, A1$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 39: Input #1, A2$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 19: Input #1, A3$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 14: Input #1, A4$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 14: Input #1, A5$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 9: Input #1, A6$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 9: Input #1, A7$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 54: Input #1, DI$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For I = 0 To 54: Input #1, DS$(PX, I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
For N = 1 To 15: For J = 0 To 49: For K = 0 To 9: Input #1, RBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 49: Input #1, RBS$(N, J, 0), RBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Input #1, WRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 39: Input #1, WRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Input #1, QBS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 19: Input #1, QBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Input #1, KRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Input #1, KRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Input #1, PRS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 14: Input #1, PRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Input #1, PUS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Input #1, FGS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Input #1, GIS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Input #1, GSS(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
Close #1
    
GoTo 2000

101 Cls: For I = 1 To 50: TT(I) = 0: Next

104 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "RUSHER         YEAR  ATT.  YARDS AVG. TDS   REC. YARDS  AVG. TDS RUN-LONGEST-REC"
If RT = 1 Then 113
If RT = 2 Then 108

108 I = B9: GoTo 114

113 For I = 0 To 49
    If A1$(PX, I) = "" Or A1$(PX, I) = "XXX" Then 260

    114 Locate 3: Color 15: Print Left$(A1$(PX, I), 13);
    VV = RBS(1, I, 7) + RBS(2, I, 7) + RBS(3, I, 7) + RBS(4, I, 7) + RBS(5, I, 7) + RBS(6, I, 7) + RBS(7, I, 7) + RBS(7, I, 7) + RBS(9, I, 7) + RBS(10, I, 7) + RBS(11, I, 7) + RBS(12, I, 7) + RBS(13, I, 7) + RBS(14, I, 7) + RBS(15, I, 7)
    For V = 1 To VV
        Color 7: Print Tab(15); RBS(V, I, 8);
        Print Tab(23); Using "###"; RBS(V, I, 0);
        Print Tab(29); Using "####"; RBS(V, I, 1);
        If RBS(V, I, 1) = 0 Or RBS(V, I, 0) = 0 Then Print Tab(34); " 0.0";
        If RBS(V, I, 0) > 0 Then Print Tab(34); Using "##.#"; RBS(V, I, 1) / RBS(V, I, 0);
        Print Tab(39); Using "###"; RBS(V, I, 2);
        Print Tab(45); Using "###"; RBS(V, I, 3);
        Print Tab(51); Using "####"; RBS(V, I, 4);
        If RBS(V, I, 4) = 0 Or RBS(V, I, 3) = 0 Then Print Tab(57); " 0.0";
        If RBS(V, I, 3) > 0 Then Print Tab(57); Using "##.#"; RBS(V, I, 4) / RBS(V, I, 3);
        Print Tab(63); Using "##"; RBS(V, I, 5);
        Print Tab(67); Using "##"; RBS(V, I, 6);
        Print Tab(69); RBS$(V, I, 0);
        Print Tab(77); Using "##"; RBS(V, I, 9);
        Print Tab(79); RBS$(V, I, 1)
    Next V
    GoSub 600
    For J = 1 To 6: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    233 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 236
    If I$ = Chr$(27) Then 2000

    236 For T = 3 To 20: Locate T
        Print String$(90, " ")
    Next
    If RT = 1 Then 260
    If RT = 2 Then GoTo 2000

260 Next I
Cls

301 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "RECEIVER        YEAR    REC.   YARDS    AVG.  TD  LONGEST REC."
If RT = 1 Then 306
If RT = 2 Then 305

305 I = B9: GoTo 309

306 For I = 0 To 39
    If A2$(PX, I) = "" Or A2$(PX, I) = "XXX" Then 329

    309 Locate 3: Color 15: Print Left$(A2$(PX, I), 13);
    VV = WRS(1, I, 4) + WRS(2, I, 4) + WRS(3, I, 4) + WRS(4, I, 4) + WRS(5, I, 4) + WRS(6, I, 4) + WRS(7, I, 4) + WRS(8, I, 4) + WRS(9, I, 4) + WRS(10, I, 4) + WRS(11, I, 4) + WRS(12, I, 4) + WRS(13, I, 4) + WRS(14, I, 4) + WRS(15, I, 4)
    For V = 1 To VV
        Color 7: Print Tab(16); WRS(V, I, 5);
        Print Tab(25); Using "###"; WRS(V, I, 0);
        Print Tab(33); Using "####"; WRS(V, I, 1);
        If WRS(V, I, 0) = 0 Or WRS(V, I, 1) = 0 Then Print Tab(41); " 0.0";
        If WRS(V, I, 0) > 0 Then Print Tab(41); Using "##.#"; WRS(V, I, 1) / WRS(V, I, 0);
        Print Tab(47); Using "##"; WRS(V, I, 2);
        Print Tab(54); Using "##"; WRS(V, I, 3);
        Print Tab(56); WRS$(V, I, 0)
    Next V
    GoSub 630
    For J = 7 To 9: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    321 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 324
    If I$ = Chr$(27) Then 2000

    324 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 329
    If RT = 2 Then GoTo 2000

329 Next I
Cls

334 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "QUARTERBACK  YEAR  COMP. ATT. INT TDS YARDS  PCT. YD/COMP RATING LONG.PASS"; ""
If RT = 1 Then 339
If RT = 2 Then 338

338 I = B9: GoTo 340

339 For I = 0 To 19
    If A3$(PX, I) = "" Or A3$(PX, I) = "XXX" Then 375

    340 Locate 3: Color 15: Print Left$(A3$(PX, I), 12);
    VV = QBS(1, I, 6) + QBS(2, I, 6) + QBS(3, I, 6) + QBS(4, I, 6) + QBS(5, I, 6) + QBS(6, I, 6) + QBS(7, I, 6) + QBS(8, I, 6) + QBS(9, I, 6) + QBS(10, I, 6) + QBS(11, I, 6) + QBS(12, I, 6) + QBS(13, I, 6) + QBS(14, I, 6) + QBS(15, I, 6)
    For V = 1 To VV
        Color 7: Print Tab(13); QBS(V, I, 7);
        Print Tab(21); Using "###"; QBS(V, I, 1);
        Print Tab(26); Using "###"; QBS(V, I, 0);
        Print Tab(32); Using "##"; QBS(V, I, 2);
        Print Tab(36); Using "##"; QBS(V, I, 4);
        Print Tab(40); Using "####"; QBS(V, I, 3);
        If QBS(V, I, 1) = 0 Or QBS(V, I, 0) = 0 Then Print Tab(47); " 00";
        If QBS(V, I, 1) > 0 Then Print Tab(47); Using "##"; (QBS(V, I, 1) / QBS(V, I, 0)) * 100;
        If QBS(V, I, 1) = 0 Then Print Tab(53); " 0.0";
        If QBS(V, I, 1) > 0 Then Print Tab(53); Using "##.#"; QBS(V, I, 3) / QBS(V, I, 1);
        GoSub 8999
        Print Tab(60); Using "###.#"; QBS(V, I, 12);
        Print Tab(70); Using "##"; QBS(V, I, 5);
        Print Tab(72); QBS$(V, I, 0)
    Next V
    GoSub 660
    For J = 10 To 14: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    364 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 367
    If I$ = Chr$(27) Then 2000

    367 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 375
    If RT = 2 Then GoTo 2000

375 Next I
Cls

384 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST RET."
If RT = 1 Then 389
If RT = 2 Then 388

388 I = B9: GoTo 390

389 For I = 0 To 14
    If A4$(PX, I) = "" Or A4$(PX, I) = "XXX" Then 415

    390 Locate 3: Color 15: Print Left$(A4$(PX, I), 13);
    VV = KRS(1, I, 4) + KRS(2, I, 4) + KRS(3, I, 4) + KRS(4, I, 4) + KRS(5, I, 4) + KRS(6, I, 4) + KRS(7, I, 4) + KRS(8, I, 4) + KRS(9, I, 4) + KRS(10, I, 4) + KRS(11, I, 4) + KRS(12, I, 4) + KRS(13, I, 4) + KRS(14, I, 4) + KRS(15, I, 4)
    For V = 1 To VV
        Color 7: Print Tab(22); KRS(V, I, 5);
        Print Tab(32); Using "###"; KRS(V, I, 0);
        Print Tab(42); Using "####"; KRS(V, I, 1);
        If KRS(V, I, 0) = 0 Or KRS(V, I, 1) = 0 Then Print Tab(52); " 00";
        If KRS(V, I, 0) > 0 Then Print Tab(52); Using "##.#"; KRS(V, I, 1) / KRS(V, I, 0);
        Print Tab(62); Using "##"; KRS(V, I, 2);
        Print Tab(70); Using "##"; KRS(V, I, 3);
        Print Tab(72); KRS$(V, I, 0)
    Next V
    GoSub 690
    For J = 15 To 17: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    401 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 404
    If I$ = Chr$(27) Then 2000

    404 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 415
    If RT = 2 Then 2000

415 Next I
Cls

424 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST RET."
If RT = 1 Then 429
If RT = 2 Then 428

428 I = B9: GoTo 430

429 For I = 0 To 14
    If A5$(PX, I) = "" Or A5$(PX, I) = "XXX" Then 450

    430 Locate 3: Color 15: Print Left$(A5$(PX, I), 13);
    VV = PRS(1, I, 4) + PRS(2, I, 4) + PRS(3, I, 4) + PRS(4, I, 4) + PRS(5, I, 4) + PRS(6, I, 4) + PRS(7, I, 4) + PRS(8, I, 4) + PRS(9, I, 4) + PRS(10, I, 4) + PRS(11, I, 4) + PRS(12, I, 4) + PRS(13, I, 4) + PRS(14, I, 4) + PRS(15, I, 4)
    For V = 1 To VV
        Color 7: Print Tab(22); PRS(V, I, 5);
        Print Tab(32); Using "###"; PRS(V, I, 0);
        Print Tab(42); Using "####"; PRS(V, I, 1);
        If PRS(V, I, 0) = 0 Or PRS(V, I, 1) = 0 Then Print Tab(52); " 00";
        If PRS(V, I, 0) > 0 Then Print Tab(52); Using "##.#"; PRS(V, I, 1) / PRS(V, I, 0);
        Print Tab(62); Using "##"; PRS(V, I, 2);
        Print Tab(70); Using "##"; PRS(V, I, 3);
        Print Tab(72); PRS$(V, I, 0)
    Next V
    GoSub 720
    For J = 18 To 20: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    441 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 444
    If I$ = Chr$(27) Then 2000

    444 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 450
    If RT = 2 Then 2000

450 Next I
Cls

464 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "PUNTERS           YEAR     ATT.     YARDS      AVG."
If RT = 1 Then 469
If RT = 2 Then 468

468 I = B9: GoTo 470

469 For I = 0 To 9
    If A6$(PX, I) = "" Or A6$(PX, I) = "XXX" Then 495

    470 Locate 3: Color 15: Print Left$(A6$(PX, I), 13);
    VV = PUS(1, I, 2) + PUS(2, I, 2) + PUS(3, I, 2) + PUS(4, I, 2) + PUS(5, I, 2) + PUS(6, I, 2) + PUS(7, I, 2) + PUS(8, I, 2) + PUS(9, I, 2) + PUS(10, I, 2) + PUS(11, I, 2) + PUS(12, I, 2) + PUS(13, I, 2) + PUS(14, I, 2) + PUS(15, I, 2)
    For V = 1 To VV
        Color 7: Print Tab(18); PUS(V, I, 3);
        Print Tab(28); Using "###"; PUS(V, I, 0);
        Print Tab(38); Using "####"; PUS(V, I, 1);
        If PUS(V, I, 0) = 0 Or PUS(V, I, 1) = 0 Then Print Tab(47); "00.0";
        If PUS(V, I, 0) > 0 Then Print Tab(47); Using "##.#"; PUS(V, I, 1) / PUS(V, I, 0);
    Next V
    GoSub 750
    For J = 21 To 22: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

    481 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 484
    If I$ = Chr$(27) Then 2000

    484 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 495
    If RT = 2 Then 2000

495 Next I
Cls

504 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50- +"
If RT = 1 Then 509
If RT = 2 Then 508

508 I = B9: GoTo 510

509 For I = 0 To 9
    If A7$(PX, I) = "" Or A7$(PX, I) = "XXX" Then 535

    510 Locate 3: Color 15: Print Left$(A7$(PX, I), 13);
    VV = FGS(1, I, 15) + FGS(2, I, 15) + FGS(3, I, 15) + FGS(4, I, 15) + FGS(5, I, 15) + FGS(6, I, 15) + FGS(7, I, 15) + FGS(8, I, 15) + FGS(9, I, 15) + FGS(10, I, 15) + FGS(11, I, 15) + FGS(12, I, 15) + FGS(13, I, 15) + FGS(14, I, 15) + FGS(15, I, 15)
    For V = 1 To VV
        Color 7: Print Tab(13); FGS(V, I, 16);
        Print Tab(19); Using "###"; FGS(V, I, 3);
        Print Tab(23); Using "###"; FGS(V, I, 2);
        If FGS(V, I, 2) = 0 Or FGS(V, I, 3) = 0 Then Print Tab(27); "  0";
        If FGS(V, I, 2) > 0 Then Print Tab(27); Using "###"; (FGS(V, I, 2) / FGS(V, I, 3)) * 100;
        Print Tab(32); Using "###"; FGS(V, I, 1);
        Print Tab(36); Using "###"; FGS(V, I, 0);
        If FGS(V, I, 0) = 0 Or FGS(V, I, 1) = 0 Then Print Tab(40); "  0";
        If FGS(V, I, 0) > 0 Then Print Tab(40); Using "###"; (FGS(V, I, 0) / FGS(V, I, 1)) * 100;
        Print Tab(46); Using "##"; FGS(V, I, 4);
        Print Tab(49); Using "##"; FGS(V, I, 5);: Print "-";
        Print Tab(52); Using "##"; FGS(V, I, 10);
        Print Tab(55); Using "##"; FGS(V, I, 6);: Print "-";
        Print Tab(58); Using "##"; FGS(V, I, 11);
        Print Tab(61); Using "##"; FGS(V, I, 7);: Print "-";
        Print Tab(64); Using "##"; FGS(V, I, 12);
        Print Tab(67); Using "##"; FGS(V, I, 8);: Print "-";
        Print Tab(70); Using "##"; FGS(V, I, 13);
        Print Tab(73); Using "##"; FGS(V, I, 9);: Print "-";
        Print Tab(76); Using "##"; FGS(V, I, 14)
    Next V
    GoSub 780
    For J = 23 To 45: TT(J) = 0: Next
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    521 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 524
    If I$ = Chr$(27) Then 2000

    524 For T = 3 To 20: Locate T
        Print String$(90, " ")
    Next
    If RT = 1 Then 535
    If RT = 2 Then 2000
535 Next I
GoSub 1000

Cls: Locate 5, 20: Color 14: Print "VIEW AGAIN (Y/N)"
    
589 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 101 Else GoTo 591

591 GoTo 2000

595 Run "CAREER.EXE"

600 VV = RBS(1, B9, 7) + RBS(2, B9, 7) + RBS(3, B9, 7) + RBS(4, B9, 7) + RBS(5, B9, 7) + RBS(6, B9, 7) + RBS(7, B9, 7) + RBS(7, B9, 7) + RBS(9, B9, 7) + RBS(10, B9, 7) + RBS(11, B9, 7) + RBS(12, B9, 7) + RBS(13, B9, 7) + RBS(14, B9, 7) + RBS(15, B9 _
, 7)
For V = 1 To VV
    TT(1) = TT(1) + RBS(V, I, 0)
    TT(2) = TT(2) + RBS(V, I, 1)
    TT(3) = TT(3) + RBS(V, I, 2)
    TT(4) = TT(4) + RBS(V, I, 3)
    TT(5) = TT(5) + RBS(V, I, 4)
    TT(6) = TT(6) + RBS(V, I, 5)
Next V
RC(1) = RBS(1, I, 6): RC$(1) = RBS$(1, I, 0)
RC(2) = RBS(1, I, 9): RC$(2) = RBS$(1, I, 1)
For V = 2 To VV
    If RBS(V, I, 6) >= RC(1) Then 606 Else GoTo 607

    606 RC(1) = RBS(V, I, 6): RC$(1) = RBS$(V, I, 0)

    607 If RBS(V, I, 9) >= RC(2) Then 608 Else GoTo 610

    608 RC(2) = RBS(V, I, 9): RC$(2) = RBS$(V, I, 1)

610 Next
Print: Color 3
Print: Print "TOTALS";
Print Tab(18); Using "##"; VV;
Print Tab(22); Using "####"; TT(1);
Print Tab(28); Using "#####"; TT(2);
If TT(1) = 0 Or TT(2) = 0 Then Print Tab(34); " 0.0";
If TT(1) > 0 Then Print Tab(34); Using "##.#"; TT(2) / TT(1);
Print Tab(39); Using "###"; TT(3);
Print Tab(44); Using "####"; TT(4);
Print Tab(50); Using "#####"; TT(5);
If TT(4) = 0 Or TT(5) = 0 Then Print Tab(57); " 0.0";
If TT(4) > 0 Then Print Tab(57); Using "##.#"; TT(5) / TT(4);
Print Tab(62); Using "###"; TT(6);
Print Tab(67); Using "##"; RC(1);
Print Tab(69); RC$(1);
Print Tab(77); Using "##"; RC(2);
Print Tab(79); RC$(2);
Return

630 VV = WRS(1, I, 4) + WRS(2, I, 4) + WRS(3, I, 4) + WRS(4, I, 4) + WRS(5, I, 4) + WRS(6, I, 4) + WRS(7, I, 4) + WRS(8, I, 4) + WRS(9, I, 4) + WRS(10, I, 4) + WRS(11, I, 4) + WRS(12, I, 4) + WRS(13, I, 4) + WRS(14, I, 4) + WRS(15, I, 4)
For V = 1 To VV
    TT(7) = TT(7) + WRS(V, I, 0)
    TT(8) = TT(8) + WRS(V, I, 1)
    TT(9) = TT(9) + WRS(V, I, 2)
Next V
RC(2) = WRS(1, I, 3): RC$(2) = WRS$(1, I, 0)
For V = 2 To VV
    If WRS(V, I, 3) >= RC(2) Then 636 Else GoTo 637

    636 RC(2) = WRS(V, I, 3): RC$(2) = WRS$(V, I, 0)

637 Next
Print: Color 3: Print "TOTALS";
Print Tab(19); Using "##"; VV;
Print Tab(24); Using "####"; TT(7);
Print Tab(32); Using "#####"; TT(8);
If TT(7) = 0 Or TT(8) = 0 Then Print Tab(41); " 0.0";
If TT(7) > 0 Then Print Tab(41); Using "##.#"; TT(8) / TT(7);
Print Tab(46); Using "###"; TT(9);
Print Tab(54); Using "##"; RC(2);
Print Tab(56); RC$(2)
Return

660 VV = QBS(1, I, 6) + QBS(2, I, 6) + QBS(3, I, 6) + QBS(4, I, 6) + QBS(5, I, 6) + QBS(6, I, 6) + QBS(7, I, 6) + QBS(8, I, 6) + QBS(9, I, 6) + QBS(10, I, 6) + QBS(11, I, 6) + QBS(12, I, 6) + QBS(13, I, 6) + QBS(14, I, 6) + QBS(15, I, 6)
For V = 1 To VV
    TT(10) = TT(10) + QBS(V, I, 1)
    TT(11) = TT(11) + QBS(V, I, 0)
    TT(12) = TT(12) + QBS(V, I, 2)
    TT(13) = TT(13) + QBS(V, I, 4)
    TT(14) = TT(14) + QBS(V, I, 3)
Next V
RC(3) = QBS(1, I, 5): RC$(3) = QBS$(1, I, 0)
For V = 2 To VV
    If QBS(V, I, 5) >= RC(3) Then 668 Else GoTo 669

    668 RC(3) = QBS(V, I, 5): RC$(3) = QBS$(V, I, 0)

669 Next
Print: Color 3: Print: Print "TOTALS";
Print Tab(16); Using "##"; VV;
Print Tab(20); Using "####"; TT(10);
Print Tab(25); Using "####"; TT(11);
Print Tab(31); Using "###"; TT(12);
Print Tab(34); Using "####"; TT(13);
Print Tab(39); Using "#####"; TT(14);
If TT(10) = 0 Or TT(11) = 0 Then Print Tab(47); " 00";
If TT(10) > 0 Then Print Tab(47); Using "##"; (TT(10) / TT(11)) * 100;
If TT(11) = 0 Then Print Tab(53); " 0.0";
If TT(10) > 0 Then Print Tab(53); Using "##.#"; TT(14) / TT(10);
GoSub 9099
Print Tab(60); Using "###.#"; TT(31);
Print Tab(70); Using "##"; RC(3);
Print Tab(72); RC$(3)
Return

690 VV = KRS(1, I, 4) + KRS(2, I, 4) + KRS(3, I, 4) + KRS(4, I, 4) + KRS(5, I, 4) + KRS(6, I, 4) + KRS(7, I, 4) + KRS(8, I, 4) + KRS(9, I, 4) + KRS(10, I, 4) + KRS(11, I, 4) + KRS(12, I, 4) + KRS(13, I, 4) + KRS(14, I, 4) + KRS(15, I, 4)
For V = 1 To VV
    TT(15) = TT(15) + KRS(V, I, 0)
    TT(16) = TT(16) + KRS(V, I, 1)
    TT(17) = TT(17) + KRS(V, I, 2)
Next V
RC(4) = KRS(1, I, 3): RC$(4) = KRS$(1, I, 0)
For V = 2 To VV
    If KRS(V, I, 3) >= RC(4) Then 696 Else GoTo 697

    696 RC(4) = KRS(V, I, 3): RC$(4) = KRS$(V, I, 0)

697 Next
Print: Color 3: Print: Print "TOTALS";
Print Tab(25); Using "##"; VV;
Print Tab(32); Using "###"; TT(15);
Print Tab(41); Using "#####"; TT(16);
If TT(15) = 0 Or TT(16) = 0 Then Print Tab(52); " 00";
If TT(15) > 0 Then Print Tab(52); Using "##.#"; TT(16) / TT(15);
Print Tab(61); Using "###"; TT(17);
Print Tab(70); Using "##"; RC(4);
Print Tab(72); RC$(4)
Return

720 VV = PRS(1, I, 4) + PRS(2, I, 4) + PRS(3, I, 4) + PRS(4, I, 4) + PRS(5, I, 4) + PRS(6, I, 4) + PRS(7, I, 4) + PRS(8, I, 4) + PRS(9, I, 4) + PRS(10, I, 4) + PRS(11, I, 4) + PRS(12, I, 4) + PRS(13, I, 4) + PRS(14, I, 4) + PRS(15, I, 4)
For V = 1 To VV
    TT(18) = TT(18) + PRS(V, I, 0)
    TT(19) = TT(19) + PRS(V, I, 1)
    TT(20) = TT(20) + PRS(V, I, 2)
Next V
RC(5) = PRS(1, I, 3): RC$(5) = PRS$(1, I, 0)
For V = 2 To VV
    If PRS(V, I, 3) >= RC(5) Then 726 Else GoTo 727

    726 RC(5) = PRS(V, I, 3): RC$(5) = PRS$(V, I, 0)

727 Next
Print: Color 3: Print: Print "TOTALS";
Print Tab(25); Using "##"; VV;
Print Tab(32); Using "###"; TT(18);
Print Tab(41); Using "#####"; TT(19);
If TT(18) = 0 Or TT(19) = 0 Then Print Tab(52); " 00";
If TT(18) > 0 Then Print Tab(52); Using "##.#"; TT(19) / TT(18);
Print Tab(61); Using "###"; TT(20);
Print Tab(70); Using "##"; RC(5);
Print Tab(72); RC$(5)
Return

750 VV = PUS(1, I, 2) + PUS(2, I, 2) + PUS(3, I, 2) + PUS(4, I, 2) + PUS(5, I, 2) + PUS(6, I, 2) + PUS(7, I, 2) + PUS(8, I, 2) + PUS(9, I, 2) + PUS(10, I, 2) + PUS(11, I, 2) + PUS(12, I, 2) + PUS(13, I, 2) + PUS(14, I, 2) + PUS(15, I, 2)
For V = 1 To VV
    TT(21) = TT(21) + PUS(V, I, 0)
    TT(22) = TT(22) + PUS(V, I, 1)
Next V
Print: Color 3: Print: Print "TOTALS";
Print Tab(21); Using "##"; VV;
Print Tab(27); Using "####"; TT(21);
Print Tab(37); Using "#####"; TT(22);
If TT(21) = 0 Or TT(22) = 0 Then Print Tab(47); " 00"
If TT(21) > 0 Then Print Tab(47); Using "##.#"; TT(22) / TT(21)
Return

780 VV = FGS(1, I, 15) + FGS(2, I, 15) + FGS(3, I, 15) + FGS(4, I, 15) + FGS(5, I, 15) + FGS(6, I, 15) + FGS(7, I, 15) + FGS(8, I, 15) + FGS(9, I, 15) + FGS(10, I, 15) + FGS(11, I, 15) + FGS(12, I, 15) + FGS(13, I, 15) + FGS(14, I, 15) + FGS(15, I, 15)
For V = 1 To VV
    TT(23) = TT(23) + FGS(V, I, 0)
    TT(24) = TT(24) + FGS(V, I, 1)
    TT(34) = TT(34) + FGS(V, I, 2)
    TT(35) = TT(35) + FGS(V, I, 3)
    TT(36) = TT(36) + FGS(V, I, 5)
    TT(37) = TT(37) + FGS(V, I, 6)
    TT(38) = TT(38) + FGS(V, I, 7)
    TT(39) = TT(39) + FGS(V, I, 8)
    TT(40) = TT(40) + FGS(V, I, 9)
    TT(41) = TT(41) + FGS(V, I, 10)
    TT(42) = TT(42) + FGS(V, I, 11)
    TT(43) = TT(43) + FGS(V, I, 12)
    TT(44) = TT(44) + FGS(V, I, 13)
    TT(45) = TT(45) + FGS(V, I, 14)
Next V
RC(6) = FGS(1, I, 4)
For V = 2 To VV
    If FGS(V, I, 4) >= RC(6) Then 785 Else GoTo 786

    785 RC(6) = FGS(V, I, 4)

786 Next
Print: Color 3: Print: Print "TOTALS";
Print Tab(16); VV;
Print Tab(19); Using "###"; TT(35);
Print Tab(23); Using "###"; TT(34);
If TT(34) = 0 Or TT(35) = 0 Then Print Tab(27); "  0";
If TT(34) > 0 Then Print Tab(27); Using "###"; (TT(34) / TT(35)) * 100;
Print Tab(32); Using "###"; TT(24);
Print Tab(36); Using "###"; TT(23);
If TT(23) = 0 Or TT(24) = 0 Then Print Tab(40); "  0";
If TT(23) > 0 Then Print Tab(40); Using "###"; (TT(23) / TT(24)) * 100;
Print Tab(46); Using "##"; RC(6);
Print Tab(49); Using "##"; TT(36);: Print "-";
Print Tab(52); Using "##"; TT(41);
Print Tab(55); Using "##"; TT(37);: Print "-";
Print Tab(58); Using "##"; TT(42);
Print Tab(61); Using "##"; TT(38);: Print "-";
Print Tab(64); Using "##"; TT(43);
Print Tab(67); Using "##"; TT(39);: Print "-";
Print Tab(70); Using "##"; TT(44);
Print Tab(73); Using "##"; TT(40);: Print "-";
Print Tab(76); Using "##"; TT(45)
Return

1000 Cls

1001 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "INTERCEPTORS        YEAR     INTERCEPTIONS"
If RT = 1 Then 1006
If RT = 2 Then 1005

1005 I = B9: GoTo 1008

1006 For I = 0 To 54
    If DI$(PX, I) = "" Or DI$(PX, I) = "XXX" Then 1028

    1008 Locate 3: Color 15: Print Left$(DI$(PX, I), 13);
    VV = GIS(1, I, 1) + GIS(2, I, 1) + GIS(3, I, 1) + GIS(4, I, 1) + GIS(5, I, 1) + GIS(6, I, 1) + GIS(7, I, 1) + GIS(8, I, 1) + GIS(9, I, 1) + GIS(10, I, 1) + GIS(11, I, 1) + GIS(12, I, 1) + GIS(13, I, 1) + GIS(14, I, 1) + GIS(15, I, 1)
    For V = 1 To VV
        Color 7: Print Tab(20); GIS(V, I, 2);
        Print Tab(34); Using "###"; GIS(V, I, 0);
    Next V
    GoSub 1820
    TT(32) = 0
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"
    
    1017 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 1020
    If I$ = Chr$(27) Then 2000

    1020 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 1028
    If RT = 2 Then 2000

1028 Next I
Cls

1031 Color 11: Locate 1: Print Z$
Color 12: Locate 2: Print "DEFENSIVE SACKS     YEAR        SACKS"
If RT = 1 Then 1036
If RT = 2 Then 1035

1035 I = B9: GoTo 1038

1036 For I = 0 To 54
    If DS$(PX, I) = "" Or DS$(PX, I) = "XXX" Then 1058

    1038 Locate 3: Color 15: Print Left$(DS$(PX, I), 13);
    VV = GSS(1, I, 1) + GSS(2, I, 1) + GSS(3, I, 1) + GSS(4, I, 1) + GSS(5, I, 1) + GSS(6, I, 1) + GSS(7, I, 1) + GSS(8, I, 1) + GSS(9, I, 1) + GSS(10, I, 1) + GSS(11, I, 1) + GSS(12, I, 1) + GSS(13, I, 1) + GSS(14, I, 1) + GSS(15, I, 1)
    For V = 1 To VV
        Color 7: Print Tab(20); GSS(V, I, 2);
        Print Tab(34); Using "###"; GSS(V, I, 0);
    Next V
    GoSub 1920
    TT(33) = 0
    Print: Print: Print
    Color 14
    Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

    1047 I$ = GetKeyPress$
    If I$ = Chr$(0) Then 1050
    If I$ = Chr$(27) Then 2000

    1050 For T = 3 To 20: Locate T
        Print String$(76, " ")
    Next
    If RT = 1 Then 1058
    If RT = 2 Then 2000

1058 Next I
Return

1820 VV = GIS(1, I, 1) + GIS(2, I, 1) + GIS(3, I, 1) + GIS(4, I, 1) + GIS(5, I, 1) + GIS(6, I, 1) + GIS(7, I, 1) + GIS(8, I, 1) + GIS(9, I, 1) + GIS(10, I, 1) + GIS(11, I, 1) + GIS(12, I, 1) + GIS(13, I, 1) + GIS(14, I, 1) + GIS(15, I, 1)
For V = 1 To VV
    TT(32) = TT(32) + GIS(V, I, 0)
Next V
Print: Color 3
Print: Print "TOTALS";
Print Tab(23); Using "##"; VV;
Print Tab(33); Using "####"; TT(32)
Return

1920 VV = GSS(1, I, 1) + GSS(2, I, 1) + GSS(3, I, 1) + GSS(4, I, 1) + GSS(5, I, 1) + GSS(6, I, 1) + GSS(7, I, 1) + GSS(8, I, 1) + GSS(9, I, 1) + GSS(10, I, 1) + GSS(11, I, 1) + GSS(12, I, 1) + GSS(13, I, 1) + GSS(14, I, 1) + GSS(15, I, 1)
For V = 1 To VV
    TT(33) = TT(33) + GSS(V, I, 0)
Next V
Print: Color 3
Print: Print "TOTALS";
Print Tab(23); Using "##"; VV;
Print Tab(33); Using "####"; TT(33)
Return

2000 Cls: Color 7: Print "VIEW ";: Color 12: Print "R";: Color 7
Print "UNNING BACKS, ";: Color 12: Print "W";: Color 7
Print "IDE RECEIVERS, ";: Color 12: Print "Q";: Color 7
Print "UARTERBACKS, ";: Color 12: Print "K";: Color 7
Print "ICK RETURNERS, P";
Color 12: Print "U";: Color 7: Print "NT"
Print "RETURNERS, ";: Color 12: Print "P";: Color 7
Print "UNTERS, KI";: Color 12: Print "C";: Color 7: Print "KERS, ";
Color 12: Print "I";: Color 7: Print "NTERCEPTIONS, ";: Color 12
Print "D";: Color 7: Print "EFENSIVE SACKS,"
Color 7: Print "R";: Color 12: Print "O";: Color 7
Print "STER IN SEQUENCE, OR ";: Color 12: Print "E";: Color 7
Print "XIT TO MAIN MENU."
GoSub 9200

2005 I$ = GetKeyPress$
If UCase$(I$) = "R" Then 2020
If UCase$(I$) = "W" Then 2120
If UCase$(I$) = "Q" Then 2220
If UCase$(I$) = "K" Then 2320
If UCase$(I$) = "U" Then 2420
If UCase$(I$) = "P" Then 2520
If UCase$(I$) = "C" Then 2620
If UCase$(I$) = "I" Then 2820
If UCase$(I$) = "D" Then 2920
If UCase$(I$) = "O" Then 2018
If UCase$(I$) = "E" Then 595

2018 RT = 1: GoTo 101

2020 Cls: Color 12: Print Z$
For I = 0 To 16: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A1$(PX, I), 12): Next
For I = 17 To 33: Locate I - 15, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A1$(PX, I), 12): Next
For I = 34 To 49: Locate I - 32, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A1$(PX, I), 12): Next
Print
Print: Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 104

2120 Cls: Color 12: Print Z$
For I = 0 To 19: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A2$(PX, I), 12): Next
For I = 20 To 39: Locate I - 18, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A2$(PX, I), 12): Next
Print
Print: Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 301

2220 Cls: Color 12: Print Z$
For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A3$(PX, I), 12): Next
For I = 10 To 19: Locate I - 8, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A3$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 334

2320 Cls: Color 12: Print Z$
For I = 0 To 7: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A4$(PX, I), 12): Next
For I = 8 To 14: Locate I - 6, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A4$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 384

2420 Cls: Color 12: Print Z$
For I = 0 To 7: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A5$(PX, I), 12): Next
For I = 8 To 14: Locate I - 6, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A5$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 424

2520 Cls: Color 12: Print Z$
For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A6$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 464

2620 Cls: Color 12: Print Z$
For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(A7$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 504

2820 Cls: Color 12: Print Z$
For I = 0 To 18: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DI$(PX, I), 12): Next
For I = 19 To 36: Locate I - 17, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DI$(PX, I), 12): Next
For I = 37 To 54: Locate I - 35, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DI$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 1001

2920 Cls: Color 12: Print Z$
For I = 0 To 18: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DS$(PX, I), 12): Next
For I = 19 To 36: Locate I - 17, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DS$(PX, I), 12): Next
For I = 37 To 54: Locate I - 35, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(DS$(PX, I), 12): Next
Print
Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
RT = 2
Cls: GoTo 1031

8999 If QBS(V, I, 0) > 0 Then QBS(V, I, 11) = Int(QBS(V, I, 1) / QBS(V, I, 0) * 1000) / 10
If QBS(V, I, 0) = 0 Then QBS(V, I, 11) = 0: QBS(V, I, 8) = 0: QBS(V, I, 10) = 0: QBS(V, I, 9) = 0: GoTo 9005
QBS(V, I, 8) = Int(QBS(V, I, 4) / QBS(V, I, 0) * 1000) / 10
QBS(V, I, 10) = Int(QBS(V, I, 3) / QBS(V, I, 0) * 100) / 100
QBS(V, I, 9) = Int(QBS(V, I, 2) / QBS(V, I, 0) * 1000) / 10

9005 PP(V, I) = QBS(V, I, 11) - 30
If PP(V, I) < 0 Then PP(V, I) = 0: GoTo 9009
If PP(V, I) > 77.5 Then PP(V, I) = 2.375: GoTo 9009
PP(V, I) = PP(V, I) * .05

9009 If QBS(V, I, 8) > 11.9 Then FF(V, I) = 2.375: GoTo 9011
FF(V, I) = QBS(V, I, 8) * .2

9011 If QBS(V, I, 9) = 0 Then II(V, I) = 0: GoTo 9014
If QBS(V, I, 9) > 9.5 Then II(V, I) = 0: GoTo 9014
II(V, I) = 2.375 - (.25 * QBS(V, I, 9))

9014 QBS(V, I, 10) = QBS(V, I, 10) - 3
If QBS(V, I, 10) < 0 Then YY(V, I) = 0: GoTo 9020
If QBS(V, I, 10) > 12.5 Then YY(V, I) = 2.375: GoTo 9020
D = Int(QBS(V, I, 10) * 100) / 2
If D = QBS(V, I, 10) / 2 Then YY(V, I) = QBS(V, I, 10) / 2 * .3 + QBS(V, I, 10) / 2 * .2: GoTo 9020
YY(V, I) = QBS(V, I, 10) / 2 * .3 + QBS(V, I, 10) / 2 * .2 + .003

9020 QBS(V, I, 12) = PP(V, I) + FF(V, I) + II(V, I) + YY(V, I)
If QBS(V, I, 12) > 9.5 Then QBS(V, I, 12) = 158.3: GoTo 9027
QBS(V, I, 12) = QBS(V, I, 12) * 50 / 3

9027 QBS(V, I, 12) = Int(QBS(V, I, 12) * 10) / 10
Return

9099 If TT(11) > 0 Then TT(30) = Int(TT(10) / TT(11) * 1000) / 10
If TT(11) = 0 Then TT(30) = 0: TT(27) = 0: TT(29) = 0: TT(28) = 0: GoTo 9105
TT(27) = Int(TT(13) / TT(11) * 1000) / 10
TT(29) = Int(TT(14) / TT(11) * 100) / 100
TT(28) = Int(TT(12) / TT(11) * 1000) / 10

9105 PPT = TT(30) - 30
If PPT < 0 Then PPT = 0: GoTo 9109
If PPT > 77.5 Then PPT = 2.375: GoTo 9109
PPT = PPT * .05

9109 If TT(27) > 11.9 Then FFT = 2.375: GoTo 9111
FFT = TT(27) * .2

9111 If TT(28) = 0 Then IIT = 0: GoTo 9114
If TT(28) > 9.5 Then IIT = 0: GoTo 9114
IIT = 2.375 - (.25 * TT(28))

9114 TT(29) = TT(29) - 3
If TT(29) < 0 Then YYT = 0: GoTo 9120
If TT(29) > 12.5 Then YYT = 2.375: GoTo 9120
D = Int(TT(29) * 100) / 2
If D = TT(29) / 2 Then YYT = TT(29) / 2 * .3 + TT(29) / 2 * .2: GoTo 9120
YYT = TT(29) / 2 * .3 + TT(29) / 2 * .2 + .003

9120 TT(31) = PPT + FFT + IIT + YYT
If TT(31) > 9.5 Then TT(31) = 158.3: GoTo 9127
TT(31) = TT(31) * 50 / 3

9127 TT(31) = Int(TT(31) * 10) / 10
Return

9200 For I = 1 To 50: TT(I) = 0: Next
Return

If QBS(V, B9, 1) > 0 Then QBS(V, B9, 11) = Int(QBS(V, B9, 2) / QBS(V, B9, 1) * 1000) / 10
If QBS(V, B9, 1) = 0 Then QBS(V, B9, 11) = 0: QBS(V, B9, 8) = 0: QBS(V, B9, 10) = 0: QBS(V, B9, 9) = 0: GoTo 9405
QBS(V, B9, 8) = Int(QBS(V, B9, 5) / QBS(V, B9, 1) * 1000) / 10
QBS(V, B9, 10) = Int(QBS(V, B9, 4) / QBS(V, B9, 1) * 100) / 100
QBS(V, B9, 9) = Int(QBS(V, B9, 3) / QBS(V, B9, 1) * 1000) / 10

9405 PP(V, B9) = QBS(V, B9, 11) - 30
If PP(V, B9) < 0 Then PP(V, B9) = 0: GoTo 9409
If PP(V, B9) > 77.5 Then PP(V, B9) = 2.375: GoTo 9409
PP(V, B9) = PP(V, B9) * .05

9409 If QBS(V, B9, 8) > 11.9 Then FF(V, B9) = 2.375: GoTo 9411
FF(V, B9) = QBS(V, B9, 8) * .2

9411 If QBS(V, B9, 9) = 0 Then II(V, B9) = 0: GoTo 9414
If QBS(V, B9, 9) > 9.5 Then II(V, B9) = 0: GoTo 9414
II(V, B9) = 2.375 - (.25 * QBS(V, B9, 9))

9414 QBS(V, B9, 10) = QBS(V, B9, 10) - 3
If QBS(V, B9, 10) < 0 Then YY(V, B9) = 0: GoTo 9420
If QBS(V, B9, 10) > 12.5 Then YY(V, B9) = 2.375: GoTo 9420
D = Int(QBS(V, B9, 10) * 100) / 2
If D = QBS(V, B9, 10) / 2 Then YY(V, B9) = QBS(V, B9, 10) / 2 * .3 + QBS(V, B9, 10) / 2 * .2: GoTo 9420
YY(V, B9) = QBS(V, B9, 10) / 2 * .3 + QBS(V, B9, 10) / 2 * .2 + .003

9420 QBS(V, B9, 12) = PP(V, B9) + FF(V, B9) + II(V, B9) + YY(V, B9)
If QBS(V, B9, 12) > 9.5 Then QBS(V, B9, 12) = 158.3: GoTo 9427
QBS(V, B9, 12) = QBS(V, B9, 12) * 50 / 3

9427 QBS(V, B9, 12) = Int(QBS(V, B9, 12) * 10) / 10
Return

4500 Cls
Print "NO CAREER FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
Run "CAREER.EXE"

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
