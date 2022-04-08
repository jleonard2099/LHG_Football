'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Merge Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim menuItems$(4), D$(1), AR$(1), A$(1)
Dim Z2(14, 18), Z(38), Z1(38), DS$(15)
Dim GIR%(9, 2), LRR$(9, 1), LR$(9, 1), LCR$(15, 1), LPR$(3, 1), LP$(3, 1), LKR$(2, 1)
Dim GI%(9, 2)
'Dim GI$(1, 10), DI%(10), GIR$(1, 10), DIR%(10)
Dim LK$(2, 1), PK$(2, 1), PKR$(2, 1), AMR(1, 4), AM(1, 4), AFR(1, 4), AF(1, 4), DI$(9)
Dim teamItems$(500), V%(500), Q$(500), PA%(4), APR%(1, 120), CRDR(120), SITER$(120)
Dim AP%(1, 120), CRD(120), SITE$(120)
Dim A1$(9), A2$(5), A3$(3), A4$(2), A5$(2), A6$(2), A7$(1)
Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(2), AR7$(1)
Dim GS%(15, 1), LC(15), LC$(15, 1), LFR(1), LFR$(1), LF(1), LF$(1)
Dim ZR2(14, 18), ZR(38), ZR1(38), DSR$(15), DRI$(9)
Dim GSR%(15, 1), LCR(15)
Dim LP(3), LPR(3), LK(2), LKR(2), PK(2), PKR(2)
Dim LR(9), LI(9), LI$(9, 1), LRR(9), LIR(9), LIR$(9, 1)
Dim O%(120), ORD%(120), O$(120), ORD$(120), T%(120), TR%(120), H$(120), HR$(120)

1 Cls
Open "DEV.BAS" For Input As #1: For I = 0 To 1: Input #1, D$(I): Next: Close #1
TMR = 6420: NB = -1: NC = -1: ND = -1: NE = -1: NF = -1: NG = -1: NH = -1: NI = -1: NJ = 0: NK = 0: D = 0: JX = 0: VV = 0
Color , 2: Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
Print "3 IN 1 FOOTBALL UTILITIES"
Locate 24, 1: Print Tab(80);: Locate , 1
Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
Locate 4, 1: Color 2, 0
menuItems$(1) = "MERGE STATISTICAL FILES           "
menuItems$(2) = "ALIGN STATISTICAL FILES           "
menuItems$(3) = "EXIT TO MAIN MENU                 "
SELECTION% = 1
Count% = 3
Start% = 1
SCANCODE% = 3
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

Call box0(7, 24, 11, 59, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
RN = SELECTION%
If SCANCODE% = 27 Then RN = 3
If RN = 1 Then 11
If RN = 2 Then Run "ALIGN.EXE"

5 Run "HELLO.EXE"

11 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"
Color 15: Locate 4, 5: Print "EXAMPLE - A:"
Locate , 5: Print "          C:\HAFFNER"
Locate , 5: Print "          C:\HAFFNER\1980"
Locate , 5: Print "          D:\FOOTBALL"

27 For J = 10 To 13: Locate J
Print "                                                            ": Next
Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$
Locate , 5: Locate , 5: Line Input "CURRENT STATS:  "; PC$
Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"
38 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 27
GoSub 3056
B$ = A$: GoSub 6585
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
Cls
Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; PR$
Locate , 15: Print "MASTER STATS ARE ON DRIVE "; PC$
Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$
Locate 14, 15: Print "ANY CHANGE (YN)"
60 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 11
If Not _FileExists(PR$ + "\" + Z$) Then 900
Open PR$ + "\" + Z$ For Input As #1
Input #1, AR$(PX)
For I = 0 To 9: Input #1, AR1$(I): Next I
For I = 0 To 5: Input #1, AR2$(I): Next I
For I = 0 To 3: Input #1, AR3$(I): Next I
For I = 0 To 2: Input #1, AR4$(I): Next I
For I = 0 To 2: Input #1, AR5$(I): Next I
Input #1, AR6$(0)
For I = 0 To 1: Input #1, AR7$(I): Next I
For J = 0 To 13
    For K = 0 To 17: Input #1, ZR2(J, K): Next K
Next J
For L = 0 To 38: Input #1, ZR(L): Next L
For L = 0 To 38: Input #1, ZR1(L): Next L
For J = 0 To 9: Input #1, DRI$(J): For J1 = 0 To 2: Input #1, GIR%(J, J1): Next: Next
For J = 0 To 14: Input #1, DSR$(J): For J1 = 0 To 1: Input #1, GSR%(J, J1): Next: Next
For I = 0 To 9: Input #1, LRR(I): Input #1, LRR$(I, 0): Input #1, LRR$(I, 1): Next
For I = 0 To 15: Input #1, LCR(I): Input #1, LCR$(I, 0): Input #1, LCR$(I, 1): Next
For I = 0 To 3: Input #1, LPR(I): Input #1, LPR$(I, 0): Input #1, LPR$(I, 1): Next
For I = 0 To 2: Input #1, LKR(I): Input #1, LKR$(I, 0): Input #1, LKR$(I, 1): Next
For I = 0 To 2: Input #1, PKR(I): Input #1, PKR$(I, 0): Input #1, PKR$(I, 1): Next
For I = 0 To 1: Input #1, LFR(I): Input #1, LFR$(I): For I1 = 0 To 4
Input #1, AMR(I, I1): Input #1, AFR(I, I1): Next: Next
For I = 0 To 9: Input #1, LIR(I): Input #1, LIR$(I, 0)
Input #1, LIR$(I, 1): Next
Close #1
GoTo 500

200 If Not _FileExists(PC$ + "\" + Z$) Then 900
Open PC$ + "\" + Z$ For Input As #1
Input #1, A$(PX)
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
Close 1
GoTo 600

319 Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."
For I = 0 To 13
    For K = 0 To 17
        Z2(I, K) = Z2(I, K) + ZR2(I, K)
Next K: Next I
For I = 0 To 27: Z(I) = Z(I) + ZR(I): Z1(I) = Z1(I) + ZR1(I): Next
For I = 29 To 38: Z(I) = Z(I) + ZR(I): Z1(I) = Z1(I) + ZR1(I): Next
If ZR(28) > Z(28) Then Z(28) = ZR(28)
For I = 0 To 9: For I1 = 0 To 2
    GI%(I, I1) = GI%(I, I1) + GIR%(I, I1)
Next: Next
For I = 0 To 14: For I1 = 0 To 1
    GS%(I, I1) = GS%(I, I1) + GSR%(I, I1)
Next: Next
For I = 0 To 9
    If LRR(I) <= LR(I) Then 334

    333 LR(I) = LRR(I): LR$(I, 0) = LRR$(I, 0): LR$(I, 1) = LRR$(I, 1)

334 Next
For I = 0 To 15
    If LCR(I) > LC(I) Then 336 Else GoTo 337

    336 LC(I) = LCR(I): LC$(I, 0) = LCR$(I, 0): LC$(I, 1) = LCR$(I, 1)

337 Next
For I = 0 To 3: If LPR(I) > LP(I) Then 339 Else GoTo 340

    339 LP(I) = LPR(I): LP$(I, 0) = LPR$(I, 0): LP$(I, 1) = LPR$(I, 1)

340 Next
For I = 0 To 2: If LKR(I) > LK(I) Then 342 Else GoTo 343

    342 LK(I) = LKR(I): LK$(I, 0) = LKR$(I, 0): LK$(I, 1) = LKR$(I, 1)

343 Next
For I = 0 To 2: If PKR(I) > PK(I) Then 345 Else GoTo 346

    345 PK(I) = PKR(I): PK$(I, 0) = PKR$(I, 0): PK$(I, 1) = PKR$(I, 1)

346 Next
For I = 0 To 1: If LFR(I) > LF(I) Then 348 Else GoTo 349

    348 LF(I) = LFR(I): LF$(I) = LFR$(I)

349 Next
For I = 0 To 1: For I1 = 0 To 4
    AM(I, I1) = AM(I, I1) + AMR(I, I1): AF(I, I1) = AF(I, I1) + AFR(I, I1)
Next: Next
For I = 0 To 9
    If LIR(I) <= LI(I) Then 347
    LI(I) = LIR(I): LI$(I, 0) = LIR$(I, 0): LI$(I, 1) = LIR$(I, 1)

347 Next
Open PC$ + "\" + Z$ For Output As #1
Print #1, A$(PX)
For I = 0 To 9: Print #1, A1$(I): Next I
For I = 0 To 5: Print #1, A2$(I): Next I
For I = 0 To 3: Print #1, A3$(I): Next I
For I = 0 To 2: Print #1, A4$(I): Next I
For I = 0 To 2: Print #1, A5$(I): Next I
Print #1, A6$(0)
For I = 0 To 1: Print #1, A7$(I): Next I
For J = 0 To 13
    For K = 0 To 17: Print #1, Z2(J, K): Next K
Next J
For L = 0 To 38: Print #1, Z(L): Next L
For L = 0 To 38: Print #1, Z1(L): Next L
For J = 0 To 9: Print #1, DI$(J): For J1 = 0 To 2: Print #1, GI%(J, J1): Next: Next
For J = 0 To 14: Print #1, DS$(J): For J1 = 0 To 1: Print #1, GS%(J, J1): Next: Next
For I = 0 To 9: Print #1, LR(I): Print #1, LR$(I, 0): Print #1, LR$(I, 1): Next
For I = 0 To 15: Print #1, LC(I): Print #1, LC$(I, 0): Print #1, LC$(I, 1): Next
For I = 0 To 3: Print #1, LP(I): Print #1, LP$(I, 0): Print #1, LP$(I, 1): Next
For I = 0 To 2: Print #1, LK(I): Print #1, LK$(I, 0): Print #1, LK$(I, 1): Next
For I = 0 To 2: Print #1, PK(I): Print #1, PK$(I, 0): Print #1, PK$(I, 1): Next
For I = 0 To 1: Print #1, LF(I): Print #1, LF$(I): For I1 = 0 To 4: Print #1, AM(I, I1): Print #1, AF(I, I1): Next: Next
For I = 0 To 9: Print #1, LI(I): Print #1, LI$(I, 0): Print #1, LI$(I, 1): Next
Close 1
GoTo 800

500 Open PR$ + "\" + Z$ + "S" For Input As #4
Input #4, YR%
For I = 1 To 120: Input #4, ORD%(I): Next I
For I = 1 To 120: Input #4, ORD$(I): Next I
For I = 1 To 120: Input #4, TR%(I): Next I
For I = 1 To 120: Input #4, HR$(I): Next I
For I = 0 To 1
For I1 = 1 To 120: Input #4, APR%(I, I1): Next: Next
For I = 1 To 120: Input #4, CRDR(I): Next I
For I = 1 To 120: Input #4, SITER$(I): Next I
Close #4
GoTo 200

600 Open PC$ + "\" + Z$ + "S" For Input As #5
Input #5, Y%
For I = 1 To 120: Input #5, O%(I): Next I
For I = 1 To 120: Input #5, O$(I): Next I
For I = 1 To 120: Input #5, T%(I): Next I
For I = 1 To 120: Input #5, H$(I): Next I
For I = 0 To 1
For I1 = 1 To 120: Input #5, AP%(I, I1): Next: Next
For I = 1 To 120: Input #5, CRD(I): Next I
For I = 1 To 120: Input #5, SITE$(I): Next I
Close #5
GoTo 1200

700 For K = 1 To YR%
    Y% = Y% + 1
    O%(Y%) = ORD%(K)
    O$(Y%) = ORD$(K)
    T%(Y%) = TR%(K)
    H$(Y%) = HR$(K)
    CRD(Y%) = CRDR(K)
    SITE$(Y%) = SITER$(K)
    For I = 0 To 1
        AP%(I, Y%) = APR%(I, K)
    Next
Next
GoTo 319

800 Open PC$ + "\" + Z$ + "S" For Output As #6
Write #6, Y%
For I = 1 To 120: Write #6, O%(I): Next I
For I = 1 To 120: Write #6, O$(I): Next I
For I = 1 To 120: Write #6, T%(I): Next I
For I = 1 To 120: Write #6, H$(I): Next I
For I = 0 To 1
For I1 = 1 To 120: Write #6, AP%(I, I1): Next: Next
For I = 1 To 120: Write #6, CRD(I): Next I
For I = 1 To 120: Write #6, SITE$(I): Next I
Close #6
Y = 0
Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"
For J = 1 To 5000: Next J
GoTo 1

900 Cls: Locate 2, 10: Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
Print: Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
GoTo 1

1200 SEQ = 0
For K = 0 To 9
    If AR1$(K) = A1$(K) Then 1220

    1210 SEQ = SEQ + 1

1220 Next
For K = 0 To 5
    If AR2$(K) = A2$(K) Then 1224

    1223 SEQ = SEQ + 1

1224 Next
For K = 0 To 3
    If AR3$(K) = A3$(K) Then 1228

    1227 SEQ = SEQ + 1

1228 Next
For K = 0 To 2
    If AR4$(K) = A4$(K) Then 1232

    1231 SEQ = SEQ + 1

1232 Next
For K = 0 To 2
    If AR5$(K) = A5$(K) Then 1236

    1235 SEQ = SEQ + 1

1236 Next
If AR6$(0) = A6$(0) Then 1240

1239 SEQ = SEQ + 1

1240 For I = 0 To 1
    If AR7$(0) = A7$(0) Then 1243

    1242 SEQ = SEQ + 1

1243 Next
If SEQ = 0 Then 700

1270 Cls: Color 14: Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$
Color 15: Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
Locate 11, 24: Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"
Locate , 31: Print "ENTER STATS MANUALLY."
Locate 15, 29: Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$
GoTo 5

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
If I$ = Chr$(27) Then Z$ = "END": Clear: GoTo 1
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0
If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
ERASE teamItems$
GoSub 1101

3060 If BO% = 1 Then BO% = 0: GoTo 3056

3058 Locate 2, 33: Color PA%(2), PA%(1): Print A$
Color 15, 4: Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 3056
Color 15, 0
Return

1101 NTMS = 0
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

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
