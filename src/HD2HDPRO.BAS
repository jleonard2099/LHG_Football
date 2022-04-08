'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Head-to-Head League Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

1 Dim O%(120), O$(120), T%(120), H$(120), Z1$(30), HT(30), RT(30), HW(30), HL(30), AW(30), AL(30)
'Dim O1$(20)
Dim G1(100), G2(100), G3(100), G4(100), Q$(203), Z12$(30)
'Dim TM1$(30), TM12$(30), LGD$(2), LD$(2)
Dim HT2(30), RT2(30), HW2(30), HL2(30), AW2(30), AL2(30)
Dim G12(100), G22(100), G32(100), G42(100), Q2$(101)
Dim D$(2), QQ$(1), Z1%(30), Z2$(30), Z12%(30), Z22$(30)
Dim THW(4), THL(4), TAW(4), TAL(4), THT(4), TRT(4), TG1(4), TG2(4), TG3(4), TG4(4)
Dim THW2(4), THL2(4), TAW2(4), TAL2(4), THT2(4), TRT2(4), TG12(4), TG22(4), TG32(4), TG42(4)
Dim DV$(3), DV2$(3), teamItems$(500), V%(500), PA%(4)
Dim Y As Long

Open "DEV.BAS" For Input As #2
For I = 0 To 2
    Input #2, D$(I)
Next
Close #2

5 Color , 0: Cls: GoSub 8
Print: Print: Print: Print: J = 6: GoSub 7: Color 3: Locate , 25: Print "SELECT OPTION:"
Locate , 25: Color 15: Print "O";: Color 7: Print "NE LEAGUE"
Locate , 25: Color 15: Print "T";: Color 7: Print "WO LEAGUES WITH INTERLEAGUE PLAY"
Locate , 25: Color 15: Print "R";: Color 7: Print "ETURN TO MAIN MENU"
2 I$ = GetKeyPress$
If UCase$(I$) = "O" Then X2 = 0: GoTo 39
If UCase$(I$) = "T" Then X2 = 1: GoTo 39

90 Run "HELLO.EXE 3"

39 ER = 0

40 GoSub 3056
B$ = QQ$(1): GoSub 6585
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
If Not _FileExists(D$(1) + Z$ + "S") Then GoSub 1600: GoTo 39
Open D$(1) + Z$ + "S" For Input As #2
Input #2, YY%
For I = 1 To 120: Input #2, O%(I): Next I
For I = 1 To 120: Input #2, O$(I): Next I
For I = 1 To 120: Input #2, T%(I): Next I
For I = 1 To 120: Input #2, H$(I): Next I
Close #2
If SL = 1 GoTo 49
GoSub 1440: LG$ = F$
If X2 = 0 Then GoTo 49
GoSub 1440: LG2$ = F$

49 J = 5: GoSub 2075: Color 7
Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
Locate , 30: Print "STATS TO ";: Color 15: Print "F";: Color 7: Print "ILE"
8651 I$ = GetKeyPress$
Color 14, 0
If UCase$(I$) = "P" Then X1 = 1
If UCase$(I$) = "S" Then VPP = 1

78 W$ = LG$ + ".LIB"
Open D$(2) + W$ For Input As #1
Input #1, X%
If X% = 0 Then Input #1, LG$: Input #1, D1$: Input #1, D2$: For X = 1 To 15: Input #1, Z1$(X): Input #1, Z1%(X): Input #1, Z2$(X): Next: NT = 15: Close 1
If X% = 1 Then Input #1, LG$: Input #1, D1$: Input #1, D2$: Input #1, D3$: For X = 1 To 30: Input #1, Z1$(X): Input #1, Z1%(X): Input #1, Z2$(X): Next: NT = 30: Close 1
If X2 <> 1 Then 145
Q2$ = LG2$ + ".LIB"
Open D$(2) + Q2$ For Input As #1
Input #1, X%
If X% = 0 Then Input #1, LG2$: Input #1, D12$: Input #1, D22$: For X = 1 To 15: Input #1, Z12$(X): Input #1, Z12%(X): Input #1, Z22$(X): Next: NT = 15: Close 1
If X% = 1 Then Input #1, LG2$: Input #1, D12$: Input #1, D22$: Input #1, D32$: For X = 1 To 30: Input #1, Z12$(X): Input #1, Z12%(X): Input #1, Z22$(X): Next: NT = 30: Close 1

145 Color 15: Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"
For K = 1 To 30: AW(K) = 0: HW(K) = 0: AL(K) = 0: HL(K) = 0: RT(K) = 0: HT(K) = 0: Next
For K = 1 To 3: THW(K) = 0: THL(K) = 0: TAW(K) = 0: TAL(K) = 0: THT(K) = 0: TRT(K) = 0: TG1(K) = 0: TG2(K) = 0: TG3(K) = 0: TG4(K) = 0: Next
For K = 1 To 30: G1(K) = 0: G2(K) = 0: G3(K) = 0: G4(K) = 0: Next
For K = 1 To 30
    If Z1$(K) = "XXX" Then 181
    For I = 1 To YY%
        If O%(I) > T%(I) And Left$(H$(I), 1) = "A" And O$(I) = Z1$(K) Then AW(K) = AW(K) + 1
        If O%(I) > T%(I) And Left$(H$(I), 1) = "H" And O$(I) = Z1$(K) Then HW(K) = HW(K) + 1
        If O%(I) < T%(I) And Left$(H$(I), 1) = "A" And O$(I) = Z1$(K) Then AL(K) = AL(K) + 1
        If O%(I) < T%(I) And Left$(H$(I), 1) = "H" And O$(I) = Z1$(K) Then HL(K) = HL(K) + 1
        If O%(I) = T%(I) And Left$(H$(I), 1) = "A" And O$(I) = Z1$(K) Then RT(K) = RT(K) + 1
        If O%(I) = T%(I) And Left$(H$(I), 1) = "H" And O$(I) = Z1$(K) Then HT(K) = HT(K) + 1
        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "H" Then G1(K) = G1(K) + O%(I)
        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "H" Then G2(K) = G2(K) + T%(I)
        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "A" Then G3(K) = G3(K) + O%(I)
        If O$(I) = Z1$(K) And Left$(H$(I), 1) = "A" Then G4(K) = G4(K) + T%(I)
    Next I

181 Next K
If X2 = 1 Then 1900

183 If VPP <> 1 Then 4100

190 DV$(1) = D1$
DV$(2) = D2$
DV$(3) = D3$

200 Color 12, 0: Cls
Locate 2, 35: Print Z$
Color 3: Locate 4: Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
    LOCATE 5: PRINT TAB(18); "W"; TAB(21); "L"; TAB(24); "T"; TAB(27); "PTF"; TAB(32); "PTA"; TAB(35); CHR$(179); TAB(37); "W"; TAB(40); "L"; TAB(43); "T"; TAB(46); "PTF"; TAB(50); "PTA"; TAB(53); CHR$(179); TAB(55); "W"; TAB(58); "L"; TAB(61); "T"; _
TAB(64); "PTF"; TAB(68); "PTA"
RDD = RDD + 1
If RDD = 1 Then 210
If RDD = 2 Then 211
If RDD = 3 Then 212

210 DD = 1: EE = 10: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 396 Else GoTo 215

211 DD = 11: EE = 20: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 396 Else GoTo 215

212 DD = 21: EE = 30: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 396 Else GoTo 215

215 Color 11: Print LG$; " "; DV$(RDD);: Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)
For K = DD To EE
    If Z1$(K) = "XXX" Or Z1$(K) = QQ$(1) Then 250
    Color 15: Locate , 1: Print Left$(Z1$(K), 15);
    Color 7: Locate , 17: Print Using "##"; HW(K);
    Locate , 20: Print Using "##"; HL(K);
    Locate , 23: Print Using "##"; HT(K);
    Locate , 27: Print Using "###"; G1(K);
    Locate , 32: Print Using "###"; G2(K);
    Color 3: Print Tab(35); Chr$(179);
    Color 7: Locate , 36: Print Using "##"; AW(K);
    Locate , 39: Print Using "##"; AL(K);
    Locate , 42: Print Using "##"; RT(K);
    Locate , 47: Print Using "##"; G3(K);
    Locate , 51: Print Using "##"; G4(K);
    Color 3: Print Tab(53); Chr$(179);
    Color 7: Locate , 54: Print Using "##"; HW(K) + AW(K);
    Locate , 57: Print Using "##"; HL(K) + AL(K);
    Locate , 60: Print Using "##"; HT(K) + RT(K);
    Locate , 64: Print Using "###"; G1(K) + G3(K);
    Locate , 68: Print Using "###"; G2(K) + G4(K)
    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);
    THW(RDD) = THW(RDD) + HW(K)
    THL(RDD) = THL(RDD) + HL(K)
    TAW(RDD) = TAW(RDD) + AW(K)
    TAL(RDD) = TAL(RDD) + AL(K)
    TRT(RDD) = TRT(RDD) + RT(K)
    THT(RDD) = THT(RDD) + HT(K)
    TG1(RDD) = TG1(RDD) + G1(K)
    TG2(RDD) = TG2(RDD) + G2(K)
    TG3(RDD) = TG3(RDD) + G3(K)
    TG4(RDD) = TG4(RDD) + G4(K)

250 Next K
Print
Color 11: Locate , 2: Print "TOTALS";
Locate , 17: Print Using "##"; THW(RDD);
Locate , 20: Print Using "##"; THL(RDD);
Locate , 23: Print Using "##"; THT(RDD);
Locate , 27: Print Using "###"; TG1(RDD);
Locate , 32: Print Using "###"; TG2(RDD);
Color 3: Print Tab(35); Chr$(179);
Color 11: Locate , 36: Print Using "##"; TAW(RDD);
Locate , 39: Print Using "##"; TAL(RDD);
Locate , 42: Print Using "##"; TRT(RDD);
Locate , 46: Print Using "###"; TG3(RDD);
Locate , 50: Print Using "###"; TG4(RDD);
Color 3: Print Tab(53); Chr$(179);
Color 11: Locate , 54: Print Using "##"; THW(RDD) + TAW(RDD);
Locate , 57: Print Using "##"; THL(RDD) + TAL(RDD);
Locate , 60: Print Using "##"; THT(RDD) + TRT(RDD);
Locate , 64: Print Using "###"; TG1(RDD) + TG3(RDD);
Locate , 68: Print Using "###"; TG2(RDD) + TG4(RDD)
Print: Print: Print
Color 14
Print "HIT ANY KEY TO CONTINUE"
391 I$ = GetKeyPress$

396 If RDD = 3 Then 397 Else GoTo 200

397 If X2 = 1 Then 2190

400 Cls: Color 12: Locate 1, 35: Print Z$
Color 3: Locate 3: Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
    LOCATE 4: PRINT TAB(18); "W"; TAB(21); "L"; TAB(24); "T"; TAB(27); "PTF"; TAB(32); "PTA"; TAB(35); CHR$(179); TAB(37); "W"; TAB(40); "L"; TAB(43); "T"; TAB(46); "PTF"; TAB(50); "PTA"; TAB(53); CHR$(179); TAB(55); "W"; TAB(58); "L"; TAB(61); "T"; _
TAB(64); "PTF"; TAB(68); "PTA"
Color 11: Print LG$;: Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)
For W = 1 To 3
    If DV$(W) = "XXX" Or DV$(W) = "" Then 490 Else GoTo 461

    461 Color 15: Locate , 2: Print DV$(W);
    Color 7: Locate , 17: Print Using "##"; THW(W);
    Locate , 20: Print Using "##"; THL(W);
    Locate , 23: Print Using "##"; THT(W);
    Locate , 27: Print Using "###"; TG1(W);
    Locate , 32: Print Using "###"; TG2(W);
    Color 3: Print Tab(35); Chr$(179);
    Color 7: Locate , 36: Print Using "##"; TAW(W);
    Locate , 39: Print Using "##"; TAL(W);
    Locate , 42: Print Using "##"; TRT(W);
    Locate , 46: Print Using "###"; TG3(W);
    Locate , 50: Print Using "###"; TG4(W);
    Color 3: Print Tab(53); Chr$(179);
    Color 7: Locate , 54: Print Using "##"; THW(W) + TAW(W);
    Locate , 57: Print Using "##"; THL(W) + TAL(W);
    Locate , 60: Print Using "##"; THT(W) + TRT(W);
    Locate , 64: Print Using "###"; TG1(W) + TG3(W);
    Locate , 68: Print Using "###"; TG2(W) + TG4(W)
    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

490 Next W
THWW = 0: THLL = 0: THTT = 0: TAWW = 0: TALL = 0: TRTT = 0
For I = 1 To 4
    THWW = THWW + THW(I): THLL = THLL + THL(I): THTT = THTT + THT(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I): TRTT = TRTT + TRT(I)
Next
TG11 = 0: TG22 = 0: TG33 = 0: TG44 = 0
For I = 1 To 4
    TG11 = TG11 + TG1(I): TG22 = TG22 + TG2(I): TG33 = TG33 + TG3(I): TG44 = TG44 + TG4(I)
Next
Print
Color 11: Locate , 2: Print "TOTALS";
Locate , 17: Print Using "##"; THWW;
Locate , 20: Print Using "##"; THLL;
Locate , 23: Print Using "##"; THTT;
Locate , 27: Print Using "###"; TG11;
Locate , 32: Print Using "###"; TG22;
Color 3: Print Tab(35); Chr$(179);
Color 11: Locate , 36: Print Using "##"; TAWW;
Locate , 39: Print Using "##"; TALL;
Locate , 42: Print Using "##"; TRTT;
Locate , 46: Print Using "###"; TG33;
Locate , 50: Print Using "###"; TG44;
Color 3: Print Tab(53); Chr$(179);
Color 11: Locate , 54: Print Using "##"; THWW + TAWW;
Locate , 57: Print Using "##"; THLL + TALL;
Locate , 60: Print Using "##"; THTT + TRTT;
Locate , 64: Print Using "###"; TG11 + TG33;
Locate , 68: Print Using "###"; TG22 + TG44
If X2 = 1 Then GoTo 2400

526 Print: Print: Print
Color 14
Print "HIT ANY KEY TO CONTINUE"
527 I$ = GetKeyPress$

600 Cls: GoSub 608
Print: Print: Print: Print: J = 6: GoSub 607: Color 3: Locate , 25: Print "SELECT OPTION:"
Color 7: Locate , 25: Print "VIEW ";: Color 15: Print "S";: Color 7: Print "AME TEAM AGAIN"
Color 7: Locate , 25: Print "VIEW ";: Color 15: Print "A";: Color 7: Print "NOTHER TEAM IN THE SAME LEAGUE"
Locate , 25: Color 15: Print "R";: Color 7: Print "ETURN TO MAIN MENU"
602 I$ = GetKeyPress$
If UCase$(I$) = "S" Then RDD = 0: GoTo 630
If UCase$(I$) = "A" Then GoTo 620
If UCase$(I$) = "R" Then GoTo 621
If UCase$(I$) <> "S" And UCase$(I$) <> "A" And UCase$(I$) <> "R" Then 602

607 '
Locate 8, 8: Color 15, 1: Print " Ö"; String$(61, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next
Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15: Return

608 Color , 2: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "3 IN 1 FOOTBALL UTILITIES"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
Return

620 RDD = 0: SL = 1: VPP = 0: GoTo 40

621 RDD = 0: SL = 0: VPP = 0: GoTo 1

630 For K = 1 To 3: THW(K) = 0: THL(K) = 0: TAW(K) = 0: TAL(K) = 0: THT(K) = 0: TRT(K) = 0: TG1(K) = 0: TG2(K) = 0: TG3(K) = 0: TG4(K) = 0: Next
For K = 1 To 3: THW2(K) = 0: THL2(K) = 0: TAW2(K) = 0: TAL2(K) = 0: THT2(K) = 0: TRT2(K) = 0: TG12(K) = 0: TG22(K) = 0: TG32(K) = 0: TG42(K) = 0: Next
GoTo 49

1600 Cls: Color 14: Print "UNABLE TO LOCATE STAT FILE "; D$(1) + Z$
Print: Print: Print: Print "HIT ANY KEY TO CONTINUE"
1602 I$ = GetKeyPress$
Return

1900 For K = 1 To 30: AW2(K) = 0: HW2(K) = 0: AL2(K) = 0: HL2(K) = 0: RT2(K) = 0: HT2(K) = 0: Next
For K = 1 To 3: THW2(K) = 0: THL2(K) = 0: TAW2(K) = 0: TAL2(K) = 0: THT2(K) = 0: TRT(K) = 0: TG12(K) = 0: TG22(K) = 0: TG32(K) = 0: TG42(K) = 0: Next
For K = 1 To 30: G12(K) = 0: G22(K) = 0: G32(K) = 0: G42(K) = 0: Next
For K = 1 To 30
    If Z12$(K) = "XXX" Then 1981
    For I = 1 To YY%
        If O%(I) > T%(I) And Left$(H$(I), 1) = "A" And O$(I) = Z12$(K) Then AW2(K) = AW2(K) + 1
        If O%(I) > T%(I) And Left$(H$(I), 1) = "H" And O$(I) = Z12$(K) Then HW2(K) = HW2(K) + 1
        If O%(I) < T%(I) And Left$(H$(I), 1) = "A" And O$(I) = Z12$(K) Then AL2(K) = AL2(K) + 1
        If O%(I) < T%(I) And Left$(H$(I), 1) = "H" And O$(I) = Z12$(K) Then HL2(K) = HL2(K) + 1
        If O%(I) = T%(I) And Left$(H$(I), 1) = "A" And O$(I) = Z12$(K) Then RT2(K) = RT2(K) + 1
        If O%(I) = T%(I) And Left$(H$(I), 1) = "H" And O$(I) = Z12$(K) Then HT2(K) = HT2(K) + 1
        If O$(I) = Z12$(K) And Left$(H$(I), 1) = "H" Then G12(K) = G12(K) + O%(I)
        If O$(I) = Z12$(K) And Left$(H$(I), 1) = "H" Then G22(K) = G22(K) + T%(I)
        If O$(I) = Z12$(K) And Left$(H$(I), 1) = "A" Then G32(K) = G32(K) + O%(I)
        If O$(I) = Z12$(K) And Left$(H$(I), 1) = "A" Then G42(K) = G42(K) + T%(I)
    Next I

1981 Next K
GoTo 183

2190 DV2$(1) = D12$
DV2$(2) = D22$
DV2$(3) = D32$
RDD = 0

2200 Cls: Color 12
Locate 2, 35: Print Z$
Color 3: Locate 4: Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
    LOCATE 5: PRINT TAB(18); "W"; TAB(21); "L"; TAB(24); "T"; TAB(27); "PTF"; TAB(32); "PTA"; TAB(35); CHR$(179); TAB(37); "W"; TAB(40); "L"; TAB(43); "T"; TAB(46); "PTF"; TAB(50); "PTA"; TAB(53); CHR$(179); TAB(55); "W"; TAB(58); "L"; TAB(61); "T" _
; TAB(64); "PTF"; TAB(68); "PTA"
RDD = RDD + 1
If RDD = 1 Then 2210
If RDD = 2 Then 2211
If RDD = 3 Then 2212

2210 DD = 1: EE = 10: If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then 2396 Else GoTo 2215

2211 DD = 11: EE = 20: If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then 2396 Else GoTo 2215

2212 DD = 21: EE = 30: If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then 2396 Else GoTo 2215

2215 Color 11: Print LG2$; " "; DV2$(RDD);: Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)
For K = DD To EE
    If Z12$(K) = "XXX" Or Z12$(K) = QQ$(1) Then 2250
    Color 15: Locate , 1: Print Left$(Z12$(K), 15);
    Color 7: Locate , 17: Print Using "##"; HW2(K);
    Locate , 20: Print Using "##"; HL2(K);
    Locate , 23: Print Using "##"; HT2(K);
    Locate , 27: Print Using "###"; G12(K);
    Locate , 32: Print Using "###"; G22(K);
    Color 3: Print Tab(35); Chr$(179);
    Color 7: Locate , 36: Print Using "##"; AW2(K);
    Locate , 39: Print Using "##"; AL2(K);
    Locate , 42: Print Using "##"; RT2(K);
    Locate , 47: Print Using "##"; G32(K);
    Locate , 51: Print Using "##"; G42(K);
    Color 3: Print Tab(53); Chr$(179);
    Color 7: Locate , 54: Print Using "##"; HW2(K) + AW2(K);
    Locate , 57: Print Using "##"; HL2(K) + AL2(K);
    Locate , 60: Print Using "##"; HT2(K) + RT2(K);
    Locate , 64: Print Using "###"; G12(K) + G32(K);
    Locate , 68: Print Using "###"; G22(K) + G42(K)
    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);
    THW2(RDD) = THW2(RDD) + HW2(K)
    THL2(RDD) = THL2(RDD) + HL2(K)
    TAW2(RDD) = TAW2(RDD) + AW2(K)
    TAL2(RDD) = TAL2(RDD) + AL2(K)
    TRT2(RDD) = TRT2(RDD) + RT2(K)
    THT2(RDD) = THT2(RDD) + HT2(K)
    TG12(RDD) = TG12(RDD) + G12(K)
    TG22(RDD) = TG22(RDD) + G22(K)
    TG32(RDD) = TG32(RDD) + G32(K)
    TG42(RDD) = TG42(RDD) + G42(K)

2250 Next K
Print
Color 11: Locate , 2: Print "TOTALS";
Locate , 17: Print Using "##"; THW2(RDD);
Locate , 20: Print Using "##"; THL2(RDD);
Locate , 23: Print Using "##"; THT2(RDD);
Locate , 27: Print Using "###"; TG12(RDD);
Locate , 32: Print Using "###"; TG22(RDD);
Color 3: Print Tab(35); Chr$(179);
Color 11: Locate , 36: Print Using "##"; TAW2(RDD);
Locate , 39: Print Using "##"; TAL2(RDD);
Locate , 42: Print Using "##"; TRT2(RDD);
Locate , 46: Print Using "###"; TG32(RDD);
Locate , 50: Print Using "###"; TG42(RDD);
Color 3: Print Tab(53); Chr$(179);
Color 11: Locate , 54: Print Using "##"; THW2(RDD) + TAW2(RDD);
Locate , 57: Print Using "##"; THL2(RDD) + TAL2(RDD);
Locate , 60: Print Using "##"; THT2(RDD) + TRT2(RDD);
Locate , 64: Print Using "###"; TG12(RDD) + TG32(RDD);
Locate , 68: Print Using "###"; TG22(RDD) + TG42(RDD)
Print: Print: Print
Color 14
Print "HIT ANY KEY TO CONTINUE"
2391 I$ = GetKeyPress$
If RDD = 3 Then 2396 Else GoTo 2200

2396 GoTo 400

2400 Print
Print LG2$
For W = 1 To 3
    If DV2$(W) = "XXX" Or DV2$(W) = "" Then 2490 Else GoTo 2461

    2461 Color 15: Locate , 2: Print DV2$(W);
    Color 7: Locate , 17: Print Using "##"; THW2(W);
    Locate , 20: Print Using "##"; THL2(W);
    Locate , 23: Print Using "##"; THT2(W);
    Locate , 27: Print Using "###"; TG12(W);
    Locate , 32: Print Using "###"; TG22(W);
    Color 3: Print Tab(35); Chr$(179);
    Color 7: Locate , 36: Print Using "##"; TAW2(W);
    Locate , 39: Print Using "##"; TAL2(W);
    Locate , 42: Print Using "##"; TRT2(W);
    Locate , 46: Print Using "###"; TG32(W);
    Locate , 50: Print Using "###"; TG42(W);
    Color 3: Print Tab(53); Chr$(179);
    Color 7: Locate , 54: Print Using "##"; THW2(W) + TAW2(W);
    Locate , 57: Print Using "##"; THL2(W) + TAL2(W);
    Locate , 60: Print Using "##"; THT2(W) + TRT2(W);
    Locate , 64: Print Using "###"; TG12(W) + TG32(W);
    Locate , 68: Print Using "###"; TG22(W) + TG42(W)
    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

2490 Next W
THWW2 = 0: THLL2 = 0: THTT2 = 0: TAWW2 = 0: TALL2 = 0: TRTT2 = 0
For I = 1 To 4
    THWW2 = THWW2 + THW2(I): THLL2 = THLL2 + THL2(I): THTT2 = THTT2 + THT2(I): TAWW2 = TAWW2 + TAW2(I): TALL2 = TALL2 + TAL2(I): TRTT2 = TRTT2 + TRT2(I)
Next
TG112 = 0: TG222 = 0: TG332 = 0: TG442 = 0
For I = 1 To 4
    TG112 = TG112 + TG12(I): TG222 = TG222 + TG22(I): TG332 = TG332 + TG32(I): TG442 = TG442 + TG42(I)
Next
Print
Color 11: Locate , 2: Print "TOTALS";
Locate , 17: Print Using "##"; THWW2;
Locate , 20: Print Using "##"; THLL2;
Locate , 23: Print Using "##"; THTT2;
Locate , 27: Print Using "###"; TG112;
Locate , 32: Print Using "###"; TG222;
Color 3: Print Tab(35); Chr$(179);
Color 11: Locate , 36: Print Using "##"; TAWW2;
Locate , 39: Print Using "##"; TALL2;
Locate , 42: Print Using "##"; TRTT2;
Locate , 46: Print Using "###"; TG332;
Locate , 50: Print Using "###"; TG442;
Color 3: Print Tab(53); Chr$(179);
Color 11: Locate , 54: Print Using "##"; THWW2 + TAWW2;
Locate , 57: Print Using "##"; THLL2 + TALL2;
Locate , 60: Print Using "##"; THTT2 + TRTT2;
Locate , 64: Print Using "###"; TG112 + TG332;
Locate , 68: Print Using "###"; TG222 + TG442
Print: Print: Print
Color 14
Print "HIT ANY KEY TO CONTINUE"
2521 I$ = GetKeyPress$
GoTo 6405

4100 If X1 = 1 Then
    SF$ = "REPORT"
Else
    Color 15, 0: Cls
    JJ = 7
    Locate 6, 8: Color 15, 1
    Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
    Print " º"; String$(61, 32); "º ";: Next
    Locate 7 + JJ, 8
    Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
    Locate , 12: Print "ENTER NAME FOR FILE: ": Print
    Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
    Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
    Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
    Locate 7, 37: Input SF$
    If SF$ = "" Then 4100
    Locate 13, 12: Print "ANY CHANGE (YN) "
      
    705 I$ = GetKeyPress$
    If UCase$(I$) = "Y" Then 4100
    Color 15, 0
    SF$ = SF$ + ".RPT"
End If
Open SF$ For Output As #2
DV$(1) = D1$
DV$(2) = D2$
DV$(3) = D3$
Print #2, Tab(48); Z$: Print #2,

4202 Print #2, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
Print #2, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
Print #2, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
Print #2, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
Print #2, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"
RDD = RDD + 1
If RDD = 1 Then 4210
If RDD = 2 Then 4211
If RDD = 3 Then 4212

4210 DD = 1: EE = 10: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 4396 Else GoTo 4215

4211 DD = 11: EE = 20: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 4396 Else GoTo 4215

4212 DD = 21: EE = 30: If DV$(RDD) = "XXX" Or DV$(RDD) = "" Then 4396 Else GoTo 4215

4215 Print #2, LG$; " "; DV$(RDD)
For K = DD To EE
    If Z1$(K) = "XXX" Or Z1$(K) = QQ$(1) Then 4250
    Print #2, Left$(Z1$(K), 15);
    Print #2, Tab(17); Using "##"; HW(K);
    Print #2, Tab(20); Using "##"; HL(K);
    Print #2, Tab(23); Using "##"; HT(K);
    Print #2, Tab(27); Using "###"; G1(K);
    Print #2, Tab(32); Using "###"; G2(K);
    Print #2, Tab(38); Using "##"; AW(K);
    Print #2, Tab(41); Using "##"; AL(K);
    Print #2, Tab(44); Using "##"; RT(K);
    Print #2, Tab(48); Using "###"; G3(K);
    Print #2, Tab(52); Using "###"; G4(K);
    Print #2, Tab(58); Using "##"; HW(K) + AW(K);
    Print #2, Tab(61); Using "##"; HL(K) + AL(K);
    Print #2, Tab(64); Using "##"; HT(K) + RT(K);
    Print #2, Tab(68); Using "###"; G1(K) + G3(K);
    Print #2, Tab(72); Using "###"; G2(K) + G4(K)
    THW(RDD) = THW(RDD) + HW(K)
    THL(RDD) = THL(RDD) + HL(K)
    TAW(RDD) = TAW(RDD) + AW(K)
    TAL(RDD) = TAL(RDD) + AL(K)
    TRT(RDD) = TRT(RDD) + RT(K)
    THT(RDD) = THT(RDD) + HT(K)
    TG1(RDD) = TG1(RDD) + G1(K)
    TG2(RDD) = TG2(RDD) + G2(K)
    TG3(RDD) = TG3(RDD) + G3(K)
    TG4(RDD) = TG4(RDD) + G4(K)

4250 Next K
Print #2,
Print #2, "TOTALS";
Print #2, Tab(17); Using "##"; THW(RDD);
Print #2, Tab(20); Using "##"; THL(RDD);
Print #2, Tab(23); Using "##"; THT(RDD);
Print #2, Tab(27); Using "###"; TG1(RDD);
Print #2, Tab(32); Using "###"; TG2(RDD);
Print #2, Tab(38); Using "##"; TAW(RDD);
Print #2, Tab(41); Using "##"; TAL(RDD);
Print #2, Tab(44); Using "##"; TRT(RDD);
Print #2, Tab(48); Using "###"; TG3(RDD);
Print #2, Tab(52); Using "###"; TG4(RDD);
Print #2, Tab(58); Using "##"; THW(RDD) + TAW(RDD);
Print #2, Tab(61); Using "##"; THL(RDD) + TAL(RDD);
Print #2, Tab(64); Using "##"; THT(RDD) + TRT(RDD);
Print #2, Tab(68); Using "###"; TG1(RDD) + TG3(RDD);
Print #2, Tab(72); Using "###"; TG2(RDD) + TG4(RDD)
Print #2,: Print #2,

4396 If RDD = 3 Then 4397 Else GoTo 4202

4397 If X2 = 1 Then 6190

4400 Print #2, Tab(48); Z$
Print #2, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
Print #2, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
Print #2, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
Print #2, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
Print #2, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"
Print #2, LG$
For W = 1 To 3
    Print #2, DV$(W);
    Print #2, Tab(17); Using "##"; THW(W);
    Print #2, Tab(20); Using "##"; THL(W);
    Print #2, Tab(23); Using "##"; THT(W);
    Print #2, Tab(27); Using "###"; TG1(W);
    Print #2, Tab(32); Using "###"; TG2(W);
    Print #2, Tab(38); Using "##"; TAW(W);
    Print #2, Tab(41); Using "##"; TAL(W);
    Print #2, Tab(44); Using "##"; TRT(W);
    Print #2, Tab(48); Using "###"; TG3(W);
    Print #2, Tab(52); Using "###"; TG4(W);
    Print #2, Tab(58); Using "##"; THW(W) + TAW(W);
    Print #2, Tab(61); Using "##"; THL(W) + TAL(W);
    Print #2, Tab(64); Using "##"; THT(W) + TRT(W);
    Print #2, Tab(68); Using "###"; TG1(W) + TG3(W);
    Print #2, Tab(72); Using "###"; TG2(W) + TG4(W)
Next W
THWW = 0: THLL = 0: THTT = 0: TAWW = 0: TALL = 0: TRTT = 0
For I = 1 To 4
    THWW = THWW + THW(I): THLL = THLL + THL(I): THTT = THTT + THT(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I): TRTT = TRTT + TRT(I)
Next
TG11 = 0: TG22 = 0: TG33 = 0: TG44 = 0
For I = 1 To 4
    TG11 = TG11 + TG1(I): TG22 = TG22 + TG2(I): TG33 = TG33 + TG3(I): TG44 = TG44 + TG4(I)
Next
Print #2,
Print #2, "TOTALS";
Print #2, Tab(17); Using "##"; THWW;
Print #2, Tab(20); Using "##"; THLL;
Print #2, Tab(23); Using "##"; THTT;
Print #2, Tab(27); Using "###"; TG11;
Print #2, Tab(32); Using "###"; TG22;
Print #2, Tab(38); Using "##"; TAWW;
Print #2, Tab(41); Using "##"; TALL;
Print #2, Tab(44); Using "##"; TRTT;
Print #2, Tab(48); Using "###"; TG33;
Print #2, Tab(52); Using "###"; TG44;
Print #2, Tab(58); Using "##"; THWW + TAWW;
Print #2, Tab(61); Using "##"; THLL + TALL;
Print #2, Tab(64); Using "##"; THTT + TRTT;
Print #2, Tab(68); Using "###"; TG11 + TG33;
Print #2, Tab(72); Using "###"; TG22 + TG44
If X2 = 1 Then 6400
GoTo 6405

6190 DV2$(1) = D12$
DV2$(2) = D22$
DV2$(3) = D32$
RDD = 0
Print #2, Tab(48); Z$: Print #2,

6202 Print #2, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
Print #2, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
Print #2, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
Print #2, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
Print #2, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"
RDD = RDD + 1
If RDD = 1 Then 6210
If RDD = 2 Then 6211
If RDD = 3 Then 6212

6210 DD = 1: EE = 10: If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then 6396 Else GoTo 6215

6211 DD = 11: EE = 20: If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then 6396 Else GoTo 6215

6212 DD = 21: EE = 30: If DV2$(RDD) = "XXX" Or DV2$(RDD) = "" Then 6396 Else GoTo 6215

6215 Print #2, LG2$; " "; DV2$(RDD)
For K = DD To EE
    If Z12$(K) = "XXX" Or Z12$(K) = QQ$(1) Then 6250
    Print #2, Left$(Z12$(K), 15);
    Print #2, Tab(17); Using "##"; HW2(K);
    Print #2, Tab(20); Using "##"; HL2(K);
    Print #2, Tab(23); Using "##"; HT2(K);
    Print #2, Tab(27); Using "###"; G12(K);
    Print #2, Tab(32); Using "###"; G22(K);
    Print #2, Tab(38); Using "##"; AW2(K);
    Print #2, Tab(41); Using "##"; AL2(K);
    Print #2, Tab(44); Using "##"; RT2(K);
    Print #2, Tab(48); Using "###"; G32(K);
    Print #2, Tab(52); Using "###"; G42(K);
    Print #2, Tab(58); Using "##"; HW2(K) + AW2(K);
    Print #2, Tab(61); Using "##"; HL2(K) + AL2(K);
    Print #2, Tab(64); Using "##"; HT2(K) + RT2(K);
    Print #2, Tab(68); Using "###"; G12(K) + G32(K);
    Print #2, Tab(72); Using "###"; G22(K) + G42(K)
    THW2(RDD) = THW2(RDD) + HW2(K)
    THL2(RDD) = THL2(RDD) + HL2(K)
    TAW2(RDD) = TAW2(RDD) + AW2(K)
    TAL2(RDD) = TAL2(RDD) + AL2(K)
    TRT2(RDD) = TRT2(RDD) + RT2(K)
    THT2(RDD) = THT2(RDD) + HT2(K)
    TG12(RDD) = TG12(RDD) + G12(K)
    TG22(RDD) = TG22(RDD) + G22(K)
    TG32(RDD) = TG32(RDD) + G32(K)
    TG42(RDD) = TG42(RDD) + G42(K)

6250 Next K
Print #2,
Print #2, "TOTALS";
Print #2, Tab(17); Using "##"; THW2(RDD);
Print #2, Tab(20); Using "##"; THL2(RDD);
Print #2, Tab(23); Using "##"; THT2(RDD);
Print #2, Tab(27); Using "###"; TG12(RDD);
Print #2, Tab(32); Using "###"; TG22(RDD);
Print #2, Tab(38); Using "##"; TAW2(RDD);
Print #2, Tab(41); Using "##"; TAL2(RDD);
Print #2, Tab(44); Using "##"; TRT2(RDD);
Print #2, Tab(48); Using "###"; TG32(RDD);
Print #2, Tab(52); Using "###"; TG42(RDD);
Print #2, Tab(58); Using "##"; THW2(RDD) + TAW2(RDD);
Print #2, Tab(61); Using "##"; THL2(RDD) + TAL2(RDD);
Print #2, Tab(64); Using "##"; THT2(RDD) + TRT2(RDD);
Print #2, Tab(68); Using "###"; TG12(RDD) + TG32(RDD);
Print #2, Tab(72); Using "###"; TG22(RDD) + TG42(RDD)
Print #2,: Print #2,
If RDD = 3 Then 6396 Else GoTo 6202

6396 GoTo 4400

6400 Print #2,
Print #2, LG2$
For W = 1 To 3
    Print #2, DV2$(W);
    Print #2, Tab(17); Using "##"; THW2(W);
    Print #2, Tab(20); Using "##"; THL2(W);
    Print #2, Tab(23); Using "##"; THT2(W);
    Print #2, Tab(27); Using "###"; TG12(W);
    Print #2, Tab(32); Using "###"; TG22(W);
    Print #2, Tab(38); Using "##"; TAW2(W);
    Print #2, Tab(41); Using "##"; TAL2(W);
    Print #2, Tab(44); Using "##"; TRT2(W);
    Print #2, Tab(48); Using "###"; TG32(W);
    Print #2, Tab(52); Using "###"; TG42(W);
    Print #2, Tab(58); Using "##"; THW2(W) + TAW2(W);
    Print #2, Tab(61); Using "##"; THL2(W) + TAL2(W);
    Print #2, Tab(64); Using "##"; THT2(W) + TRT2(W);
    Print #2, Tab(68); Using "###"; TG12(W) + TG32(W);
    Print #2, Tab(72); Using "###"; TG22(W) + TG42(W)
Next W
THWW2 = 0: THLL2 = 0: THTT2 = 0: TAWW2 = 0: TALL2 = 0: TRTT2 = 0
For I = 1 To 4
    THWW2 = THWW2 + THW2(I): THLL2 = THLL2 + THL2(I): THTT2 = THTT2 + THT2(I): TAWW2 = TAWW2 + TAW2(I): TALL2 = TALL2 + TAL2(I): TRTT2 = TRTT2 + TRT2(I)
Next
TG112 = 0: TG222 = 0: TG332 = 0: TG442 = 0
For I = 1 To 4
    TG112 = TG112 + TG12(I): TG222 = TG222 + TG22(I): TG332 = TG332 + TG32(I): TG442 = TG442 + TG42(I)
Next
Print #2,
Print #2, "TOTALS";
Print #2, Tab(17); Using "##"; THWW2;
Print #2, Tab(20); Using "##"; THLL2;
Print #2, Tab(23); Using "##"; THTT2;
Print #2, Tab(27); Using "###"; TG112;
Print #2, Tab(32); Using "###"; TG222;
Print #2, Tab(38); Using "##"; TAWW2;
Print #2, Tab(41); Using "##"; TALL2;
Print #2, Tab(44); Using "##"; TRTT2;
Print #2, Tab(48); Using "###"; TG332;
Print #2, Tab(52); Using "###"; TG442;
Print #2, Tab(58); Using "##"; THWW2 + TAWW2;
Print #2, Tab(61); Using "##"; THLL2 + TALL2;
Print #2, Tab(64); Using "##"; THTT2 + TRTT2;
Print #2, Tab(68); Using "###"; TG112 + TG332;
Print #2, Tab(72); Using "###"; TG222 + TG442

6405 Close 2
If X1 <> 1 Then 600
Open "REPORT" For Input As #2
Do While Not EOF(2)
    Line Input #2, X$
    LPrint X$
Loop
Close 2
LPrint Chr$(12)
X1 = 0
GoTo 600

3056 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Print
Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
If I$ = Chr$(27) Then Z$ = "END": GoTo 5
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

3058 Locate 2, 33: Color PA%(2), PA%(1): Print QQ$(1)
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
20 MAMenu teamItems$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then BO% = 1: Return
If ExitCode% = -71 Then Start% = 1: ExitCode% = 3: SELECTION% = 1: GoTo 20
If ExitCode% = -79 Then Start% = NTMS: ExitCode% = 3: SELECTION% = NTMS: GoTo 20
If ExitCode% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If ExitCode% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If ExitCode% = -81 Or ExitCode% = -73 Then ExitCode% = 3: SELECTION% = Start%: GoTo 20
T1 = V%(SELECTION%)
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Get #1, T1
QQ$(1) = Q$(1)
For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1
Close 1

9005 If Right$(QQ$(1), 1) = " " Then QQ$(1) = Left$(QQ$(1), Len(QQ$(1)) - 1): GoTo 9005
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

7 '
Locate 8, 8: Color 15, 1: Print " Ö"; String$(61, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next
Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15: Return

8 Color , 2: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "3 IN 1 FOOTBALL UTILITIES"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
Return

1440 Spec$ = D$(2) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
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
3806 I$ = GetKeyPress$
GoTo 5

2075 '
Locate 8, 8: Color 15, 1: Print " Ö"; String$(61, 196); "· ";
For II = 1 To J: Locate 8 + II, 8: Print " º"; String$(61, 32); "º ";: Next
Locate 9 + J, 8: Print " Ó"; String$(61, 196); "½ ";: Locate 10, 15: Return

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
