'$INCLUDE: 'Comp.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Update League Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

Dim RB(11, 11), QB(11, 11), KR(11, 11), PR(11, 11), WR$(16), WR(16, 13)
Dim O1(60), Z1$(60), DT(38), OT(38), TB$(18), YD(16), TD(16), TP(18), TG(16)
Dim PS$(10), RB$(10), QB$(4), KR$(3), PR$(3), PK$(2)
Dim FM(2), FA(2), EM(2), EA(2)
Dim D$(2), FG(2), EP(2), BRC(50), BRC$(50, 2), TRC(50), TRC$(50)
Dim PP(4), QT(4), II(4), YY(4)
Dim PTSF(60), PTSA(60), O2(60), O%(120), O$(120), T%(120), H1$(30)
Dim Z2$(240), Z3$(240), TT(10, 20, 9), TT$(10, 20)
Dim Z1(240, 2), H$(120), O3(60), AP%(1, 120)
'Dim CN$(40)
Dim Z2(14, 18), DI$(9), GI%(9, 2), DS$(14), GS%(14, 1)
'Dim N2$(240), N3$(240), N1(240, 2), NT(10, 20, 9), NT$(10, 20)
Dim LR(9), LR$(9, 1), LC(15), LC$(15, 1), LP(3), LP$(3, 1), LK(2), LK$(2, 1)
Dim PK(2), PKR$(2, 1), LF(1), LF$(1), AM(1, 4), AF(1, 4), LI$(9, 1), LI(9)

Open "DEV.BAS" For Input As #1: For I = 0 To 2: Input #1, D$(I): Next: Close 1

Cls

4001 TRC(11) = 999: TRC(22) = 999
For I = 25 To 27: TRC(I) = 9999: Next
For I = 29 To 36: TRC(I) = 9999: Next
TRC(38) = 999: TRC(39) = 999
TRC(42) = 999: TRC(43) = 999
If Not _FileExists(D$(1) + "RECORDS.PFB") Then 4003

Open D$(1) + "RECORDS.PFB" For Input As #1
For I = 0 To 50: Input #1, BRC(I): Input #1, BRC$(I, 0): Input #1, BRC$(I, 1)
    Input #1, BRC$(I, 2)
Next
For I = 0 To 50: Input #1, TRC(I): Input #1, TRC$(I): Next
Close 1

4003 Spec$ = D$(2) + "*.LIB"
Count% = FCount%(Spec$)
If Count% = 0 Then 4500
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO UPDATE": Color 15
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
Print "-SELECTS LEAGUE";

_MouseShow "DEFAULT"
20 MAMenu leagueFiles$(), SELECTION%, START%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 2900
If EXITCODE% = -71 Then START% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then START% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 20
If EXITCODE% = -73 Then START% = START% - 80: If START% < 1 Then START% = 1
If EXITCODE% = -81 Then START% = START% + 80: If START% > Count% Then START% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = START%: GoTo 20
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
WE$ = Left$(targetFile$, Len(targetFile$) - 4)
Cls: Print "IS THIS THE FINAL UPDATE FOR THE SEASON (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then UP$ = "Y"
C$ = ","
For X = 1 To 30
    PTSF(X) = 0
    PTSA(X) = 0
Next X
For X = 1 To 240
    Z2$(X) = "*"
Next
CA = 2
For I = 1 To 20
    TT(CA, I, 5) = 9999
Next
CA = 4
For I = 1 To 20
    TT(CA, I, 5) = 9999
Next
CA = 6
For I = 1 To 20
    TT(CA, I, 3) = 9999
Next
CA = 8
For I = 1 To 20
    TT(CA, I, 8) = 9999
Next
CA = 10
For I = 1 To 20
    TT(CA, I, 7) = -99
Next
Open D$(2) + WE$ + ".TMS" For Input As #2
Input #2, FF$, D1$, D2$, D3$
For XW = 1 To 30
    Input #2, H1$(XW)
Next
Close 2
GoSub 3018

70 For XW = 1 To 30
    GoSub 700: GoSub 900
Next
AA = 1: ZZ = 10: GoSub 950
AA = 11: ZZ = 20: GoSub 950
AA = 21: ZZ = 30: GoSub 950
GoSub 4010

99 '

2900 Run "HELLO.EXE 3"
End

700 WN = 0: LS = 0: TY = 0
If H1$(XW) = Chr$(42) Then Return
GoSub 9131
If Not _FileExists(D$(1) + H1$(XW) + "S") Then 895
Open D$(1) + H1$(XW) + "S" For Input As #2
Input #2, Y%
For I = 1 To 120: Input #2, O%(I): Next I
For I = 1 To 120: Input #2, O$(I): PTSF(XW) = PTSF(XW) + O%(I): Next I
For I = 1 To 120: Input #2, T%(I): PTSA(XW) = PTSA(XW) + T%(I): Next I
For I = 1 To 120: Input #2, H$(I): Next I
For I = 0 To 1
    For I1 = 1 To 120
        Input #2, AP%(I, I1)
Next: Next
Close 2
WN = 0: LS = 0: TY = 0: CW = 0: CT = 0: CL = 0
GoSub 5500
Print "FINDING "; H1$(XW); " STATS"
Return

811 Open D$(1) + H1$(XW) For Input As #1: Input #1, TM$
For I = 1 To 10: Input #1, RB$(I): Next
For I = 1 To 6: Input #1, WR$(I): Next
For I = 1 To 4: Input #1, QB$(I): Next
For I = 1 To 3: Input #1, KR$(I): Next
For I = 1 To 3: Input #1, PR$(I): Next I
Input #1, PU$
For I = 1 To 2: Input #1, PK$(I): Next
For J = 1 To 14
    For K = 1 To 18
        Input #1, Z2(J, K)
Next K: Next J
For L = 0 To 38: Input #1, OT(L): Next L
For L = 0 To 38: Input #1, DT(L): Next L
For J = 0 To 9: Input #1, DI$(J)
    For J1 = 0 To 2
        Input #1, GI%(J, J1)
Next: Next
For J = 0 To 14: Input #1, DS$(J): For J1 = 0 To 1: Input #1, GS%(J, J1): Next: Next
For I = 0 To 9: Input #1, LR(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
For I = 0 To 15: Input #1, LC(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
For I = 0 To 3: Input #1, LP(I): Input #1, LP$(I, 0): Input #1, LP$(I, 1): Next
For I = 0 To 2: Input #1, LK(I): Input #1, LK$(I, 0): Input #1, LK$(I, 1): Next
For I = 0 To 2: Input #1, PK(I): Input #1, PKR$(I, 0): Input #1, PKR$(I, 1): Next
For I = 0 To 1: Input #1, LF(I): Input #1, LF$(I): For I1 = 0 To 4: Input #1, AM(I, I1): Input #1, AF(I, I1): Next: Next
For I = 0 To 9: Input #1, LI(I): Input #1, LI$(I, 0): Input #1, LI$(I, 1): Next
Close 1
For I = 1 To 10: For J = 1 To 6: RB(I, J) = Z2(I, J): Next: Next
For I = 1 To 6: For J = 1 To 3: WR(I, J) = Z2(I, J + 6): Next: Next
For I = 1 To 4: For J = 1 To 5: QB(I, J) = Z2(I, J + 9): Next: Next
For I = 1 To 3: For J = 1 To 3: KR(I, J) = Z2(I, J + 14): Next: Next
For I = 1 To 3: For J = 1 To 3: PR(I, J) = Z2(I + 3, J + 14): Next: Next
TG(8) = OT(7): TG(9) = OT(8)
FM(1) = Z2(4, 18): FA(1) = Z2(5, 18): EM(1) = Z2(6, 18): EA(1) = Z2(7, 18)
FM(2) = Z2(8, 18): FA(2) = Z2(9, 18): EM(2) = Z2(10, 18): EA(2) = Z2(11, 18)
For I = 1 To 10: RB(I, 7) = RB(I, 2) + RB(I, 5): RB(I, 8) = RB(I, 3) + RB(I, 6): WR$(I + 6) = RB$(I): WR(I + 6, 2) = RB(I, 5): WR(I + 6, 1) = RB(I, 4): WR(I + 6, 3) = RB(I, 6): Next
For I = 1 To 10
    TB$(I) = RB$(I)
    YD(I) = RB(I, 7)
    TD(I) = RB(I, 8)
Next
For I = 1 To 6
    TB$(I + 10) = WR$(I)
    YD(I + 10) = WR(I, 2)
    TD(I + 10) = WR(I, 3)
Next
For I = 1 To 16
    For I1 = 1 To 3
        If KR$(I1) = TB$(I) Then YD(I) = YD(I) + KR(I1, 2): TD(I) = TD(I) + KR(I1, 3)
    Next
Next
For I = 1 To 16
    For I1 = 1 To 3
        If PR$(I1) = TB$(I) Then YD(I) = YD(I) + PR(I1, 2): TD(I) = TD(I) + PR(I1, 3)
    Next
Next
For I = 1 To 16: TP(I) = TD(I) * 6: Next
For I = 1 To 2: TB$(16 + I) = PK$(I): TP(16 + I) = EM(I) + FM(I) * 3: Next
For I = 1 To 10: RB(I, 9) = 0: If RB(I, 1) > 0 Then RB(I, 9) = Int(RB(I, 2) / RB(I, 1) * 10) / 10
Next
For I = 1 To 10: RB(I, 10) = 0: If RB(I, 4) > 0 Then RB(I, 10) = Int(RB(I, 5) / RB(I, 4) * 10) / 10
Next
For I = 1 To 16: WR(I, 4) = 0: If WR(I, 1) > 0 Then WR(I, 4) = Int(WR(I, 2) / WR(I, 1) * 10) / 10
Next
For I = 1 To 4: QB(I, 6) = 0: If QB(I, 1) > 0 Then QB(I, 6) = Int(QB(I, 2) / QB(I, 1) * 1000) / 10
    GoSub 2000
Next
For I = 1 To 3: KR(I, 4) = 0: If KR(I, 1) > 0 And KR(I, 1) >= Y% Then KR(I, 4) = Int(KR(I, 2) / KR(I, 1) * 10) / 10: GoTo 859
    KR(I, 4) = 0

859 Next
For I = 1 To 3: PR(I, 4) = 0: If PR(I, 1) > 0 And PR(I, 1) >= Y% Then PR(I, 4) = Int(PR(I, 2) / PR(I, 1) * 10) / 10: GoTo 862
    PR(I, 4) = 0

862 Next
PN = 0: If TG(8) > 0 Then PN = Int(TG(9) / TG(8) * 10) / 10
For I = 1 To 2
    FG(I) = 0: If FA(I) >= Y% Then FG(I) = Int(FM(I) / FA(I) * 1000) / 10
    EP(I) = 0: If EA(I) >= Y% Then EP(I) = Int(EM(I) / EA(I) * 1000) / 10
Next

895 Return

900 L = Len(H1$(XW)): WT$ = "": For X = 1 To L: A = Asc(Mid$(H1$(XW), X, 1))
WT$ = WT$ + Chr$(A): Next X
For X = 1 To 30: If Z1$(X) <> WT$ Then 945
    If O1(X) = WN And O2(X) = LS And O3(X) = TY Then Return
    O1(X) = WN: O2(X) = LS: O3(X) = TY

945 Next X: GoSub 811: GoSub 3100
Return

950 For X = AA To ZZ - 1: For Y = X + 1 To ZZ: If O1(X) - O2(X) > O1(Y) - O2(Y) Then 985
    If O1(X) - O2(X) = O1(Y) - O2(Y) And O1(X) <= O1(Y) And O1(X) > O2(X) Then 985
        If O1(X) - O2(X) = O1(Y) - O2(Y) And O1(X) >= O1(Y) And O1(X) < O2(X) Then 985
        Swap Z1$(X), Z1$(Y)
        Swap O1(X), O1(Y)
        Swap O2(X), O2(Y)
        Swap O3(X), O3(Y)
        Swap PTSF(X), PTSF(Y)
        Swap PTSA(X), PTSA(Y)

    985 Next Y
Next X
Return

2000 If QB(I, 1) = 0 Then QB(I, 6) = 0: QB(I, 8) = 0: QB(I, 10) = 0: QB(I, 9) = 0: GoTo 2005
QB(I, 8) = Int(QB(I, 5) / QB(I, 1) * 1000) / 10
QB(I, 10) = Int(QB(I, 4) / QB(I, 1) * 100) / 100
QB(I, 9) = Int(QB(I, 3) / QB(I, 1) * 1000) / 10

2005 PP(I) = QB(I, 6) - 30
If PP(I) < 0 Then PP(I) = 0: GoTo 2009
If PP(I) > 77.5 Then PP(I) = 2.375: GoTo 2009
PP(I) = PP(I) * .05

2009 If QB(I, 8) > 11.9 Then QT(I) = 2.375: GoTo 2011
QT(I) = QB(I, 8) * .2

2011 If QB(I, 9) = 0 Then II(I) = 2.375: GoTo 2014
If QB(I, 9) > 9.5 Then II(I) = 0: GoTo 2014
II(I) = 2.375 - (.25 * QB(I, 9))

2014 QB(I, 10) = QB(I, 10) - 3
If QB(I, 10) < 0 Then YY(I) = 0: GoTo 2020
If QB(I, 10) > 12.5 Then YY(I) = 2.375: GoTo 2020
D = Int(QB(I, 10) * 100) / 2
If D = QB(I, 10) / 2 Then YY(I) = QB(I, 10) / 2 * .3 + QB(I, 10) / 2 * .2: GoTo 2020
YY(I) = QB(I, 10) / 2 * .3 + QB(I, 10) / 2 * .2 + .003

2020 QB(I, 7) = PP(I) + QT(I) + II(I) + YY(I)
If QB(I, 7) >= 9.5 Then QB(I, 7) = 158.3: GoTo 2027
QB(I, 7) = QB(I, 7) * 50 / 3

2027 QB(I, 7) = Int(QB(I, 7) * 10) / 10
Return

2100 Swap TT$(CA, Z1), TT$(CA, Z)
For MN = 1 To 9
    Swap TT(CA, Z1, MN), TT(CA, Z, MN)
Next MN
Return

2500 'TEAM CATEGORIES
'CATEGORY 1 - TOTAL OFFENSE
CA = 1
If UCase$(UP$) = "Y" And (OT(6) + OT(2) + OT(14)) / Y% > TRC(0) Then TRC(0) = (OT(6) + OT(2) + OT(14)) / Y%: TRC$(0) = TM$
If OT(6) + OT(2) + OT(14) > TRC(1) Then TRC(1) = OT(6) + OT(2) + OT(14): TRC$(1) = TM$
If UCase$(UP$) = "Y" And (OT(6) + OT(2) + OT(14)) / (OT(1) + OT(3) - OT(13)) > TRC(2) Then TRC(2) = (OT(6) + OT(2) + OT(14)) / (OT(1) + OT(3) - OT(13)): TRC$(2) = TM$
If (OT(6) + OT(2) + OT(14)) / Y% <= TT(CA, 20, 5) Then 2506
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = OT(1) + OT(3) - OT(13)
TT(CA, 20, 3) = OT(6) + OT(2) + OT(14)
TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 5) >= TT(CA, Z1, 5) Then 2505
        GoSub 2100

2505 Next Z1: Next Z

2506 'CATEGORY 2 - TOTAL DEFENSE
CA = 2
If UCase$(UP$) = "Y" And (DT(6) + DT(2) + DT(14)) / Y% < TRC(25) Then TRC(25) = (DT(6) + DT(2) + DT(14)) / Y%: TRC$(25) = TM$
If UCase$(UP$) = "Y" And DT(6) + DT(2) + DT(14) < TRC(26) Then TRC(26) = DT(6) + DT(2) + DT(14): TRC$(26) = TM$
If UCase$(UP$) = "Y" And (DT(6) + DT(2) + DT(14)) / (DT(1) + DT(3) - DT(13)) < TRC(27) Then TRC(27) = (DT(6) + DT(2) + DT(14)) / (DT(1) + DT(3) - DT(13)): TRC$(27) = TM$
If UCase$(UP$) = "Y" And (DT(6) + DT(2) + DT(14)) / Y% > TRC(28) Then TRC(28) = (DT(6) + DT(2) + DT(14)) / Y%: TRC$(28) = TM$
If (DT(6) + DT(2) + DT(14)) / Y% >= TT(CA, 20, 5) Then 2507
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = DT(1) + DT(3) - DT(13)
TT(CA, 20, 3) = DT(6) + DT(2) + DT(14)
TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 5) <= TT(CA, Z1, 5) Then 2510
        GoSub 2100

2510 Next Z1: Next Z

2507 'CATEGORY 3 - RUSHING OFFENSE
CA = 3
If UCase$(UP$) = "Y" And (OT(2) + OT(14)) / Y% > TRC(3) Then TRC(3) = (OT(2) + OT(14)) / Y%: TRC$(3) = TM$
If OT(2) + OT(14) > TRC(4) Then TRC(4) = OT(2) + OT(14): TRC$(4) = TM$
If UCase$(UP$) = "Y" And (OT(2) + OT(14)) / (OT(1) - OT(13)) > TRC(5) Then TRC(5) = (OT(2) + OT(14)) / (OT(1) - OT(13)): TRC$(5) = TM$
If (OT(2) + OT(14)) / Y% <= TT(CA, 20, 5) Then 2516
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = OT(1) - OT(13)
TT(CA, 20, 3) = OT(2) + OT(14)
TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 5) >= TT(CA, Z1, 5) Then 2515
        GoSub 2100

2515 Next Z1: Next Z

2516 'CATEGORY 4 - RUSHING DEFENSE
CA = 4
If UCase$(UP$) = "Y" And (DT(2) + DT(14)) / Y% < TRC(29) Then TRC(29) = (DT(2) + DT(14)) / Y%: TRC$(29) = TM$
If UCase$(UP$) = "Y" And DT(2) + DT(14) < TRC(30) Then TRC(30) = DT(2) + DT(14): TRC$(30) = TM$
If UCase$(UP$) = "Y" And (DT(2) + DT(14)) / (DT(1) - DT(13)) < TRC(31) Then TRC(31) = (DT(2) + DT(14)) / (DT(1) - DT(13)): TRC$(31) = TM$
If (DT(2) + DT(14)) / Y% >= TT(CA, 20, 5) Then 2521
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = DT(1) - DT(13)
TT(CA, 20, 3) = DT(2) + DT(14)
TT(CA, 20, 4) = TT(CA, 20, 3) / TT(CA, 20, 2)
TT(CA, 20, 5) = TT(CA, 20, 3) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 5) <= TT(CA, Z1, 5) Then 2520
        GoSub 2100

2520 Next Z1: Next Z

2521 'CATEGORY 5 - SCORING OFFENSE
CA = 5
PTS = 0: OPTS = 0
For I = 1 To Y%
    PTS = PTS + O%(I)
    OPTS = OPTS + T%(I)
Next
If UCase$(UP$) = "Y" And PTS / Y% > TRC(13) Then TRC(13) = PTS / Y%: TRC$(13) = TM$
If PTS > TRC(12) Then TRC(12) = PTS: TRC$(12) = TM$
If UCase$(UP$) = "Y" And (PTS - OPTS) / Y% > TRC(14) Then TRC(14) = (PTS - OPTS) / Y%: TRC$(14) = TM$
If PTS / Y% <= TT(CA, 20, 3) Then 2526
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = PTS
TT(CA, 20, 3) = TT(CA, 20, 2) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 3) >= TT(CA, Z1, 3) Then 2525
        GoSub 2100

2525 Next Z1: Next Z

2526 'CATEGORY 6 - SCORING DEFENSE
CA = 6
PTS = 0
For I = 1 To Y%
    PTS = PTS + T%(I)
Next
If UCase$(UP$) = "Y" And PTS / Y% < TRC(39) Then TRC(39) = PTS / Y%: TRC$(39) = TM$
If UCase$(UP$) = "Y" And PTS < TRC(38) Then TRC(38) = PTS: TRC$(38) = TM$
If UCase$(UP$) = "Y" And PTS / Y% > TRC(41) Then TRC(41) = PTS / Y%: TRC$(41) = TM$
If PTS > TRC(40) Then TRC(40) = PTS: TRC$(40) = TM$
If PTS / Y% >= TT(CA, 20, 3) Then 2531
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = PTS
TT(CA, 20, 3) = TT(CA, 20, 2) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 3) <= TT(CA, Z1, 3) Then 2530
        GoSub 2100

2530 Next Z1: Next Z

2531 'CATEGORY 7 - PASSING OFFENSE
CA = 7
If UCase$(UP$) = "Y" And OT(6) / Y% > TRC(6) Then TRC(6) = OT(6) / Y%: TRC$(6) = TM$
If OT(6) > TRC(7) Then TRC(7) = OT(6): TRC$(7) = TM$
If UCase$(UP$) = "Y" And OT(6) / OT(3) > TRC(8) Then TRC(8) = OT(6) / OT(3): TRC$(8) = TM$
If UCase$(UP$) = "Y" And OT(6) / OT(4) > TRC(9) Then TRC(9) = OT(6) / OT(4): TRC$(9) = TM$
If UCase$(UP$) = "Y" And OT(4) / OT(3) > TRC(10) Then TRC(10) = OT(4) / OT(3): TRC$(10) = TM$
If UCase$(UP$) = "Y" And OT(5) / OT(3) < TRC(11) Then TRC(11) = OT(5) / OT(3): TRC$(11) = TM$
If OT(6) / Y% <= TT(CA, 20, 8) Then 2536
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = OT(3)
TT(CA, 20, 3) = OT(4)
TT(CA, 20, 4) = OT(5)
TT(CA, 20, 5) = TT(CA, 20, 3) / TT(CA, 20, 2)
TT(CA, 20, 6) = OT(6)
TT(CA, 20, 7) = OT(6) / OT(3)
TT(CA, 20, 8) = TT(CA, 20, 6) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 8) >= TT(CA, Z1, 8) Then 2535
        GoSub 2100

2535 Next Z1: Next Z

2536 'CATEGORY 8 - PASSING DEFENSE
CA = 8
If UCase$(UP$) = "Y" And DT(6) / Y% < TRC(32) Then TRC(32) = DT(6) / Y%: TRC$(32) = TM$
If UCase$(UP$) = "Y" And DT(6) < TRC(33) Then TRC(33) = DT(6): TRC$(33) = TM$
If UCase$(UP$) = "Y" And DT(6) / DT(3) < TRC(34) Then TRC(34) = DT(6) / DT(3): TRC$(34) = TM$
If UCase$(UP$) = "Y" And DT(6) / DT(4) < TRC(35) Then TRC(35) = DT(6) / DT(4): TRC$(35) = TM$
If UCase$(UP$) = "Y" And DT(4) / DT(3) < TRC(36) Then TRC(36) = DT(4) / DT(3): TRC$(36) = TM$
If UCase$(UP$) = "Y" And DT(5) / DT(3) > TRC(37) Then TRC(37) = DT(5) / DT(3): TRC$(37) = TM$
If DT(6) / Y% >= TT(CA, 20, 8) Then 2541
TT(CA, 20, 1) = Y%
TT(CA, 20, 2) = DT(3)
TT(CA, 20, 3) = DT(4)
TT(CA, 20, 4) = DT(5)
TT(CA, 20, 5) = TT(CA, 20, 3) / TT(CA, 20, 2)
TT(CA, 20, 6) = DT(6)
TT(CA, 20, 7) = DT(6) / DT(3)
TT(CA, 20, 8) = TT(CA, 20, 6) / Y%
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 8) <= TT(CA, Z1, 8) Then 2540
        GoSub 2100

2540 Next Z1: Next Z

2541 'CATEGORY 9 - PUNTING
CA = 9
If UCase$(UP$) = "Y" And OT(8) / OT(7) > TRC(15) Then TRC(15) = OT(8) / OT(7): TRC$(15) = TM$
If OT(8) / OT(7) <= TT(CA, 20, 2) Then 2546
TT(CA, 20, 1) = OT(7)
TT(CA, 20, 2) = OT(8) / OT(7)
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 2) >= TT(CA, Z1, 2) Then 2545
        GoSub 2100

2545 Next Z1: Next Z

2546 'CATEGORY 10 - TURNOVER MARGIN
CA = 10
FG = OT(9) - OT(10) + DT(10)
IG = DT(5)
FL = OT(10) + DT(9) - DT(10)
IL = OT(5)
TM = (FG + IG - FL - IL) / Y%
If TM <= TT(CA, 20, 7) Then 2599
TT(CA, 20, 1) = FG
TT(CA, 20, 2) = IG
TT(CA, 20, 3) = FG + IG
TT(CA, 20, 4) = FL
TT(CA, 20, 5) = IL
TT(CA, 20, 6) = FL + IL
TT(CA, 20, 7) = TM
TT$(CA, 20) = WT$
For Z = 1 To 19
    For Z1 = Z + 1 To 20
        If TT(CA, Z, 7) >= TT(CA, Z1, 7) Then 2550
        GoSub 2100

2550 Next Z1: Next Z

2599 'PUNT RETURNS
If UCase$(UP$) = "Y" And OT(25) / OT(24) > TRC(16) Then TRC(16) = OT(25) / OT(24): TRC$(16) = TM$
If UCase$(UP$) = "Y" And DT(25) / DT(24) < TRC(42) Then TRC(42) = DT(25) / DT(24): TRC$(42) = TM$
'KO RETURN
If UCase$(UP$) = "Y" And OT(23) / OT(22) > TRC(17) Then TRC(17) = OT(23) / OT(22): TRC$(17) = TM$
If UCase$(UP$) = "Y" And DT(23) / DT(22) < TRC(43) Then TRC(43) = DT(23) / DT(22): TRC$(43) = TM$
'FIRST DOWNS
If OT(0) > TRC(18) Then TRC(18) = OT(0): TRC$(18) = TM$
If UCase$(UP$) = "Y" And OT(0) / Y% > TRC(19) Then TRC(19) = OT(0) / Y%: TRC$(19) = TM$
'FUMBLES
If OT(9) > TRC(20) Then TRC(20) = OT(9): TRC$(20) = TM$
If OT(10) > TRC(21) Then TRC(21) = OT(10): TRC$(21) = TM$
If UCase$(UP$) = "Y" And OT(10) < TRC(22) Then TRC(22) = OT(10): TRC$(22) = TM$
'PENALTIES
If UCase$(UP$) = "Y" And OT(11) / Y% > TRC(23) Then TRC(23) = OT(11) / Y%: TRC$(23) = TM$
If UCase$(UP$) = "Y" And OT(12) / Y% > TRC(24) Then TRC(24) = OT(12) / Y%: TRC$(24) = TM$
Return

3018 Open D$(2) + WE$ + ".LIB" For Input As #2
Input #2, LG%
Close 2
If LG% <> 1 Then 3095
Open D$(2) + WE$ + ".TMS" For Input As #2
Input #2, FF$, D1$, D2$, D3
For X = 1 To 30
Input #2, Z1$(X): Next X: Close 2
Return

3095 Cls
Print "ERROR:: THIS IS A COLLEGE CONFERENCE"
Print "        RETURNING TO MAIN MENU"
Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
3096 I$ = GetKeyPress$
GoTo 99

3100 Cls
GoSub 2500
For X = 1 To 10
    Print "   CHECKING STATS OF "; RB$(X)
    If RB$(X) = "OTHERS" Then 3165
    If RB(X, 1) > BRC(0) Then BRC(0) = RB(X, 1): BRC$(0, 0) = RB$(X): BRC$(0, 1) = TM$
    If RB(X, 2) > BRC(1) Then BRC(1) = RB(X, 2): BRC$(1, 0) = RB$(X): BRC$(1, 1) = TM$
    If RB(X, 3) > BRC(2) Then BRC(2) = RB(X, 3): BRC$(2, 0) = RB$(X): BRC$(2, 1) = TM$
    If RB(X, 9) > BRC(3) And UCase$(UP$) = "Y" And RB(X, 1) >= Y% * 10 Then BRC(3) = RB(X, 9): BRC$(3, 0) = RB$(X): BRC$(3, 1) = TM$
    If LR(X - 1) > BRC(4) Then BRC(4) = LR(X - 1): BRC$(4, 0) = RB$(X): BRC$(4, 1) = TM$: BRC$(4, 2) = LR$(X - 1, 0)
    PS$(X) = WT$
    For Y = 1 To 20: If Z2$(Y) = RB$(X) And Z3$(Y) = WT$ Then GoSub 4818: GoTo 3118
    Next Y
    If RB(X, 1) < Y% * 10 Or RB(X, 9) < Z1(20, 1) Then 3118
    Z2$(20) = RB$(X)
    Z3$(20) = WT$: Z1(20, 1) = RB(X, 9): Z1(20, 2) = RB(X, 1): GoSub 4820

    3118 For Z = 41 To 50
        If Z2$(Z) = RB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = Int(RB(X, 2)): GoTo 3123
    Next Z: If Int(RB(X, 2)) <= Z1(50, 1) Then 3150
    Z1(50, 1) = Int(RB(X, 2)): Z2$(50) = RB$(X)
    Z3$(50) = WT$

    3123 For Z = 41 To 49: For Z1 = Z + 1 To 50: If Z1(Z, 1) >= Z1(Z1, 1) Then 3130
        GoSub 3450

    3130 Next Z1: Next Z

    3150 For Z = 61 To 70
        If Z2$(Z) = RB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = RB(X, 3): GoTo 3155
    Next Z: If RB(X, 3) <= Z1(70, 1) Then 3162
    Z1(70, 1) = RB(X, 3): Z2$(70) = RB$(X)
    Z3$(70) = WT$

    3155 For Z = 61 To 69: For Z1 = Z + 1 To 70: If Z1(Z, 1) >= Z1(Z1, 1) Then 3159
        GoSub 3450

    3159 Next Z1: Next Z

    3162 GoSub 3520

3165 Next X
For X = 1 To 16
    Print "   CHECKING STATS OF "; WR$(X)
    If WR$(X) = "OTHERS" Then 3237
    If WR(X, 1) > BRC(5) Then BRC(5) = WR(X, 1): BRC$(5, 0) = WR$(X): BRC$(5, 1) = TM$
    If WR(X, 2) > BRC(6) Then BRC(6) = WR(X, 2): BRC$(6, 0) = WR$(X): BRC$(6, 1) = TM$
    If WR(X, 3) > BRC(7) Then BRC(7) = WR(X, 3): BRC$(7, 0) = WR$(X): BRC$(7, 1) = TM$
    If WR(X, 4) > BRC(8) And UCase$(UP$) = "Y" And WR(X, 1) >= Y% * 3 Then BRC(8) = WR(X, 4): BRC$(8, 0) = WR$(X): BRC$(8, 1) = TM$
    If LC(X - 1) > BRC(9) Then BRC(9) = LC(X - 1): BRC$(9, 0) = WR$(X): BRC$(9, 1) = TM$: BRC$(9, 2) = LC$(X - 1, 0)
    For Z = 21 To 40: If Z2$(Z) = WR$(X) And Z3$(Z) = WT$ Then GoSub 4825: GoTo 3178
    Next Z
    If WR(X, 1) < Y% * 3 Or WR(X, 4) < Z1(40, 1) Then 3178
    Z2$(40) = WR$(X)
    Z3$(40) = WT$: Z1(40, 1) = WR(X, 4): Z1(40, 2) = WR(X, 1): GoSub 4827

    3178 For Z = 51 To 60
        If Z2$(Z) = WR$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = Int(WR(X, 2)): GoTo 3183
    Next Z: If Int(WR(X, 2)) <= Z1(60, 1) Then 3210
    Z1(60, 1) = Int(WR(X, 2)): Z2$(60) = WR$(X)
    Z3$(60) = WT$

    3183 For Z = 51 To 59: For Z1 = Z + 1 To 60: If Z1(Z, 1) >= Z1(Z1, 1) Then 3190
        GoSub 3450

    3190 Next Z1: Next Z

    3210 For Z = 91 To 100
        If Z2$(Z) = WR$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = WR(X, 1): GoTo 3215
    Next Z: If WR(X, 1) <= Z1(100, 1) Then 3225
    Z1(100, 1) = WR(X, 1): Z2$(100) = WR$(X)
    Z3$(100) = WT$

    3215 For Z = 91 To 99: For Z1 = Z + 1 To 100: If Z1(Z, 1) >= Z1(Z1, 1) Then 3219
        GoSub 3450

    3219 Next Z1: Next Z

    3225 For Z = 71 To 80
        If Z2$(Z) = WR$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = WR(X, 3): GoTo 3230
    Next Z: If WR(X, 3) <= Z1(80, 1) Then 3237
    Z1(80, 1) = WR(X, 3): Z2$(80) = WR$(X)
    Z3$(80) = WT$

    3230 For Z = 71 To 79: For Z1 = Z + 1 To 80: If Z1(Z, 1) >= Z1(Z1, 1) Then 3234
        GoSub 3450

    3234 Next Z1: Next Z

3237 Next X
For X = 1 To 16
    Print "   CHECKING STATS OF "; TB$(X)
    If TB$(X) = "OTHERS" Then 3267
    If YD(X) > BRC(18) Then BRC(18) = YD(X): BRC$(18, 0) = TB$(X): BRC$(18, 1) = TM$
    If TP(X) > BRC(19) Then BRC(19) = TP(X): BRC$(19, 0) = TB$(X): BRC$(19, 1) = TM$
    If TP(X) > BRC(21) Then BRC(21) = TP(X): BRC$(21, 0) = TB$(X): BRC$(21, 1) = TM$
    For Z = 101 To 110
        If Z2$(Z) = TB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = YD(X): GoTo 3245
    Next Z: If YD(X) <= Z1(110, 1) Then 3255
    Z1(110, 1) = YD(X): Z2$(110) = TB$(X)
    Z3$(110) = WT$

    3245 For Z = 101 To 109: For Z1 = Z + 1 To 110: If Z1(Z, 1) >= Z1(Z1, 1) Then 3249
        GoSub 3450

    3249 Next Z1: Next Z

    3255 For Z = 111 To 120
        If Z2$(Z) = TB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = TP(X): GoTo 3260
    Next Z: If TP(X) <= Z1(120, 1) Then 3267
    Z1(120, 1) = TP(X): Z2$(120) = TB$(X)
    Z3$(120) = WT$

    3260 For Z = 111 To 119: For Z1 = Z + 1 To 120: If Z1(Z, 1) >= Z1(Z1, 1) Then 3264
        GoSub 3450

    3264 Next Z1: Next Z

3267 Next X
GoSub 3500
For X = 1 To 4
    Print "   CHECKING STATS OF "; QB$(X)
    If QB$(X) = "OTHERS" Then 3315
    If QB(X, 1) > BRC(10) Then BRC(10) = QB(X, 1): BRC$(10, 0) = QB$(X): BRC$(10, 1) = TM$
    If QB(X, 2) > BRC(11) Then BRC(11) = QB(X, 2): BRC$(11, 0) = QB$(X): BRC$(11, 1) = TM$
    If QB(X, 5) > BRC(12) Then BRC(12) = QB(X, 5): BRC$(12, 0) = QB$(X): BRC$(12, 1) = TM$
    If QB(X, 3) > BRC(13) Then BRC(13) = QB(X, 3): BRC$(13, 0) = QB$(X): BRC$(13, 1) = TM$
    If QB(X, 4) > BRC(14) Then BRC(14) = QB(X, 4): BRC$(14, 0) = QB$(X): BRC$(14, 1) = TM$
    If QB(X, 6) > BRC(15) And UCase$(UP$) = "Y" And QB(X, 1) >= Y% * 15 Then BRC(15) = QB(X, 6): BRC$(15, 0) = QB$(X): BRC$(15, 1) = TM$
    If QB(X, 7) > BRC(16) And UCase$(UP$) = "Y" And QB(X, 1) >= Y% * 15 Then BRC(16) = QB(X, 7): BRC$(16, 0) = QB$(X): BRC$(16, 1) = TM$
    If LP(X - 1) > BRC(17) Then BRC(17) = LP(X - 1): BRC$(17, 0) = QB$(X): BRC$(17, 1) = TM$: BRC$(17, 2) = LP$(X - 1, 0)
    For Y = 121 To 130: If Z2$(Y) = QB$(X) And Z3$(Y) = WT$ Then GoSub 4832: GoTo 3279
    Next Y
    If QB(X, 1) < Y% * 15 Or QB(X, 6) < Z1(130, 1) Then 3279
    Z2$(130) = QB$(X)
    Z3$(130) = WT$: Z1(130, 1) = QB(X, 6): Z1(130, 2) = QB(X, 1): GoSub 4834

    3279 For Y = 131 To 140: If Z2$(Y) = QB$(X) And Z3$(Y) = WT$ Then GoSub 4839: GoTo 3287
    Next Y
    If QB(X, 1) < Y% * 15 Or QB(X, 7) < Z1(140, 1) Then 3287
    Z2$(140) = QB$(X)
    Z3$(140) = WT$: Z1(140, 1) = QB(X, 7): Z1(140, 2) = QB(X, 1): GoSub 4841

    3287 For Z = 141 To 150
        If Z2$(Z) = QB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = Int(QB(X, 4)): GoTo 3294
    Next Z: If Int(QB(X, 4)) <= Z1(150, 1) Then 3302
    Z1(150, 1) = Int(QB(X, 4)): Z2$(150) = QB$(X)

    3294 Z3$(150) = WT$
    For Z = 141 To 149: For Z1 = Z + 1 To 150: If Z1(Z, 1) >= Z1(Z1, 1) Then 3299
        GoSub 3450

    3299 Next Z1: Next Z

    3302 For Z = 151 To 160
        If Z2$(Z) = QB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = QB(X, 5): GoTo 3307
    Next Z: If QB(X, 5) <= Z1(160, 1) Then 3315
    Z1(160, 1) = QB(X, 5): Z2$(160) = QB$(X)
    Z3$(160) = WT$

    3307 For Z = 151 To 159: For Z1 = Z + 1 To 160: If Z1(Z, 1) >= Z1(Z1, 1) Then 3311
        GoSub 3450

    3311 Next Z1: Next Z

3315 Next X
For X = 1 To 3
    Print "   CHECKING STATS OF "; KR$(X)
    If KR$(X) = "OTHERS" Then 3355
    If KR(X, 1) > BRC(22) Then BRC(22) = KR(X, 1): BRC$(22, 0) = KR$(X): BRC$(22, 1) = TM$
    If KR(X, 2) > BRC(23) Then BRC(23) = KR(X, 2): BRC$(23, 0) = KR$(X): BRC$(23, 1) = TM$
    If KR(X, 3) > BRC(24) Then BRC(24) = KR(X, 3): BRC$(24, 0) = KR$(X): BRC$(24, 1) = TM$
    If KR(X, 4) > BRC(25) And UCase$(UP$) = "Y" And KR(X, 1) >= Y% * 1.2 Then BRC(25) = KR(X, 4): BRC$(25, 0) = KR$(X): BRC$(25, 1) = TM$
    If LK(X - 1) > BRC(26) Then BRC(26) = LK(X - 1): BRC$(26, 0) = KR$(X): BRC$(26, 1) = TM$: BRC$(26, 2) = LK$(X - 1, 0)
    For Z = 161 To 170
        If Z2$(Z) = KR$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = KR(X, 4): Z1(Z, 2) = KR(X, 1): GoTo 3345
    Next Z
    If KR(X, 1) < Y% * 1.2 Or KR(X, 4) <= Z1(170, 1) Then 3355
    Z1(170, 1) = KR(X, 4): Z2$(170) = KR$(X)
    Z3$(170) = WT$: Z1(170, 2) = KR(X, 1)

    3345 For Z = 161 To 169: For Z1 = Z + 1 To 170: If Z1(Z, 1) >= Z1(Z1, 1) Then 3349
        GoSub 3455

    3349 Next Z1: Next Z

3355 Next X
For X = 1 To 3
    Print "   CHECKING STATS OF "; PR$(X)
    If PR$(X) = "OTHERS" Then 3375
    If PR(X, 1) > BRC(27) Then BRC(27) = PR(X, 1): BRC$(27, 0) = PR$(X): BRC$(27, 1) = TM$
    If PR(X, 2) > BRC(28) Then BRC(28) = PR(X, 2): BRC$(28, 0) = PR$(X): BRC$(28, 1) = TM$
    If PR(X, 3) > BRC(29) Then BRC(29) = PR(X, 3): BRC$(29, 0) = PR$(X): BRC$(29, 1) = TM$
    If PR(X, 4) > BRC(30) And UCase$(UP$) = "Y" And PR(X, 1) >= Y% * 1.2 Then BRC(30) = PR(X, 4): BRC$(30, 0) = PR$(X): BRC$(30, 1) = TM$
    If PK(X - 1) > BRC(31) Then BRC(31) = PK(X - 1): BRC$(31, 0) = PR$(X): BRC$(31, 1) = TM$: BRC$(31, 2) = PKR$(X - 1, 0)
    For Z = 171 To 180
        If Z2$(Z) = PR$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = PR(X, 4): Z1(Z, 2) = PR(X, 1): GoTo 3365
    Next Z
    If PR(X, 1) < Y% * 1.2 Or PR(X, 4) <= Z1(180, 1) Then 3375
    Z1(180, 1) = PR(X, 4): Z2$(180) = PR$(X)
    Z3$(180) = WT$: Z1(180, 2) = PR(X, 1)

    3365 For Z = 171 To 179: For Z1 = Z + 1 To 180: If Z1(Z, 1) >= Z1(Z1, 1) Then 3369
        GoSub 3455

    3369 Next Z1: Next Z

3375 Next X
Print "   CHECKING STATS OF "; PU$
If TG(8) > BRC(32) Then BRC(32) = TG(8): BRC$(32, 0) = PU$: BRC$(32, 1) = TM$
If TG(9) > BRC(33) Then BRC(33) = TG(9): BRC$(33, 0) = PU$: BRC$(33, 1) = TM$
If PN > BRC(34) And UCase$(UP$) = "Y" Then BRC(34) = PN: BRC$(34, 0) = PU$: BRC$(34, 1) = TM$
For Z = 181 To 190
    If Z2$(Z) = PU$ And Z3$(Z) = WT$ Then Z1(Z, 1) = PN: Z1(Z, 2) = TG(8): GoTo 3382
Next Z: If PN <= Z1(190, 1) Then 3390
Z1(190, 1) = PN: Z2$(190) = PU$
Z3$(190) = WT$: Z1(190, 2) = TG(8)

3382 For Z = 181 To 189: For Z1 = Z + 1 To 190: If Z1(Z, 1) >= Z1(Z1, 1) Then 3386
    GoSub 3455

3386 Next Z1: Next Z

3390 For X = 1 To 2
    Print "   CHECKING STATS OF "; PK$(X)
    If FM(X) > BRC(35) Then BRC(35) = FM(X): BRC$(35, 0) = PK$(X): BRC$(35, 1) = TM$
    If FA(X) > BRC(36) Then BRC(36) = FA(X): BRC$(36, 0) = PK$(X): BRC$(36, 1) = TM$
    If FG(X) > BRC(37) And UCase$(UP$) = "Y" Then BRC(37) = FG(X): BRC$(37, 0) = PK$(X): BRC$(37, 1) = TM$
    If LF(X - 1) > BRC(38) Then BRC(38) = LF(X - 1): BRC$(38, 0) = PK$(X): BRC$(38, 1) = TM$: BRC$(38, 2) = LF$(X - 1)
    If EM(X) > BRC(39) Then BRC(39) = EM(X): BRC$(39, 0) = PK$(X): BRC$(39, 1) = TM$
    If EA(X) > BRC(40) Then BRC(40) = EA(X): BRC$(40, 0) = PK$(X): BRC$(40, 1) = TM$
    If EP(X) > BRC(41) And UCase$(UP$) = "Y" Then BRC(41) = EP(X): BRC$(41, 0) = PK$(X): BRC$(41, 1) = TM$
    For Z = 201 To 210
        If Z2$(Z) = PK$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = FG: Z1(Z, 2) = FA(X): GoTo 3396
    Next Z: If FG(X) <= Z1(210, 1) Then 3403
    Z1(210, 1) = FG(X): Z2$(210) = PK$(X)
    Z3$(210) = WT$: Z1(210, 2) = FA(X)

    3396 For Z = 201 To 209: For Z1 = Z + 1 To 210: If Z1(Z, 1) >= Z1(Z1, 1) Then 3400
        GoSub 3455

    3400 Next Z1: Next Z

    3403 'PRINT"   CHECKING STATS OF ";EP$
    For Z = 211 To 220
        If Z2$(Z) = PK$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = EP(X): Z1(Z, 2) = EA(X): GoTo 3409
    Next Z: If EP(X) <= Z1(220, 1) Then 3416
    Z1(220, 1) = EP(X): Z2$(220) = PK$(X)
    Z3$(220) = WT$: Z1(220, 2) = EA(X)

    3409 For Z = 211 To 219: For Z1 = Z + 1 To 220: If Z1(Z, 1) >= Z1(Z1, 1) Then 3413
        GoSub 3455

    3413 Next Z1: Next Z

3416 Next X
Print "   CHECKING TEAM SACKS AND INTERCEPTIONS"
For X = 0 To 9
    If Left$(DI$(X), 1) = " " Or DI$(X) = "" Or DI$(X) = "END" Then 3427
    If GI%(X, 0) > BRC(42) Then BRC(42) = GI%(X, 0): BRC$(42, 0) = DI$(X): BRC$(42, 1) = TM$
    If GI%(X, 1) > BRC(43) Then BRC(43) = GI%(X, 1): BRC$(43, 0) = DI$(X): BRC$(43, 1) = TM$
    If GI%(X, 2) > BRC(44) Then BRC(44) = GI%(X, 2): BRC$(44, 0) = DI$(X): BRC$(44, 1) = TM$
    If LI(X - 1) > BRC(45) Then BRC(45) = LI(X - 1): BRC$(45, 0) = DI$(X): BRC$(45, 1) = TM$: BRC$(45, 2) = LI$(X - 1, 0)
    For Z = 221 To 230
        If Z2$(Z) = DI$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = GI%(X, 0): GoTo 3422
    Next Z: If GI%(X, 0) <= Z1(230, 1) Then 3427
    Z1(230, 1) = GI%(X, 0): Z2$(230) = DI$(X)
    Z3$(230) = WT$

    3422 For Z = 221 To 229: For Z1 = Z + 1 To 230: If Z1(Z, 1) >= Z1(Z1, 1) Then 3426
        GoSub 3450

    3426 Next Z1: Next Z

3427 Next X
For X = 0 To 14
    If Left$(DS$(X), 1) = " " Or DS$(X) = "" Or DS$(X) = "END" Then 3439
    If GS%(X, 0) > BRC(46) Then BRC(46) = GS%(X, 0): BRC$(46, 0) = DS$(X): BRC$(46, 1) = TM$
    For Z = 231 To 240
        If Z2$(Z) = DS$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = GS%(X, 0): GoTo 3434
    Next Z: If GS%(X, 0) <= Z1(240, 1) Then 3439
    Z1(240, 1) = GS%(X, 0): Z2$(240) = DS$(X)
    Z3$(240) = WT$

    3434 For Z = 231 To 239: For Z1 = Z + 1 To 240: If Z1(Z, 1) >= Z1(Z1, 1) Then 3438
        GoSub 3450

    3438 Next Z1: Next Z

3439 Next X
Return

3450 Swap Z2$(Z1), Z2$(Z)
Swap Z3$(Z1), Z3$(Z)
Swap Z1(Z1, 1), Z1(Z, 1)
Return

3455 Swap Z2$(Z1), Z2$(Z)
Swap Z3$(Z1), Z3$(Z)
For MN = 1 To 2
    Swap Z1(Z1, MN), Z1(Z, MN)
Next MN
Return

3500 For X = 17 To 18
    If TP(X) > BRC(20) Then BRC(20) = TP(X): BRC$(20, 0) = TB$(X): BRC$(20, 1) = TM$
    If TP(X) > BRC(21) Then BRC(21) = TP(X): BRC$(21, 0) = TB$(X): BRC$(21, 1) = TM$
    For Z = 191 To 200
        If Z2$(Z) = TB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = TP(X): GoTo 3505
    Next Z: If TP(X) <= Z1(200, 1) Then 3512
    Z1(200, 1) = TP(X): Z2$(200) = TB$(X)
    Z3$(200) = WT$

    3505 For Z = 191 To 199: For Z1 = Z + 1 To 200: If Z1(Z, 1) >= Z1(Z1, 1) Then 3509
        GoSub 3450

    3509 Next Z1: Next Z

3512 Next X
Return

3520 For Z = 81 To 90
    If Z2$(Z) = RB$(X) And Z3$(Z) = WT$ Then Z1(Z, 1) = RB(X, 1): GoTo 3525
Next Z: If RB(X, 1) <= Z1(90, 1) Then Return
Z1(90, 1) = RB(X, 1): Z2$(90) = RB$(X)
Z3$(90) = WT$

3525 For Z = 81 To 89: For Z1 = Z + 1 To 90: If Z1(Z, 1) >= Z1(Z1, 1) Then 3529
    GoSub 3450

3529 Next Z1: Next Z
Return

4010 Cls
Open D$(2) + WE$ + ".LDR" For Output As #2
Print "POSTING "; WE$; " LEAGUE LEADERS"
For X = 1 To 30: Write #2, Z1$(X), O1(X), O2(X), O3(X), PTSF(X), PTSA(X): Next
For X = 1 To 240
Write #2, Z2$(X), Z3$(X), Z1(X, 1), Z1(X, 2): Next X
For I = 1 To 10
    For I1 = 1 To 20
        For I2 = 1 To 9
            Write #2, TT(I, I1, I2)
Next: Next: Next
For I = 1 To 10
    For I1 = 1 To 20
        Write #2, TT$(I, I1)
Next: Next
Close 2
Open D$(1) + "RECORDS.PFB" For Output As #1
For I = 0 To 50: Print #1, BRC(I): Print #1, BRC$(I, 0): Print #1, BRC$(I, 1)
    Print #1, BRC$(I, 2)
Next
For I = 0 To 50: Print #1, TRC(I): Print #1, TRC$(I): Next
Close 1
Return

4818 Z1(Y, 1) = RB(X, 9): Z1(Y, 2) = RB(X, 1): If Z1(Y, 2) >= RA Then 4820
Z2$(Y) = "*": Z3$(Y) = "*": Z1(Y, 1) = 0: Z1(Y, 2) = 0

4820 For Y = 1 To 19: For Z = Y + 1 To 20: If Z1(Y, 1) >= Z1(Z, 1) Then 4824
    TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
    TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
    For Z1 = 1 To 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: Next Z1

4824 Next Z: Next Y: Return

4825 Z1(Z, 1) = WR(X, 4): Z1(Z, 2) = WR(X, 1): If Z1(Z, 2) >= RC Then 4827
Z2$(Z) = "*": Z3$(Z) = "*": Z1(Z, 1) = 0: Z1(Z, 2) = 0

4827 For Y = 21 To 39: For Z = Y + 1 To 40: If Z1(Y, 1) >= Z1(Z, 1) Then 4831
    TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
    TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
    For Z1 = 1 To 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: Next Z1

4831 Next Z: Next Y: Return

4832 Z1(Y, 1) = QB(X, 6): Z1(Y, 2) = QB(X, 1): If Z1(Y, 2) >= PA Then 4834
Z2$(Y) = "*": Z3$(Y) = "*": Z1(Y, 1) = 0: Z1(Y, 2) = 0

4834 For Y = 121 To 129: For Z = Y + 1 To 130: If Z1(Y, 1) >= Z1(Z, 1) Then 4838
    TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
    TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
    For Z1 = 1 To 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: Next Z1

4838 Next Z: Next Y: Return

4839 Z1(Y, 1) = QB(X, 7): Z1(Y, 2) = QB(X, 1): If Z1(Y, 2) >= PA Then 4841
Z2$(Y) = "*": Z3$(Y) = "*": Z1(Y, 1) = 0: Z1(Y, 2) = 0

4841 For Y = 131 To 139: For Z = Y + 1 To 140: If Z1(Y, 1) >= Z1(Z, 1) Then 4845
    TN$ = Z3$(Y): Z3$(Y) = Z3$(Z): Z3$(Z) = TN$
    TN$ = Z2$(Y): Z2$(Y) = Z2$(Z): Z2$(Z) = TN$
    For Z1 = 1 To 2: J = Z1(Y, Z1): Z1(Y, Z1) = Z1(Z, Z1): Z1(Z, Z1) = J: Next Z1

4845 Next Z: Next Y: Return
L = Len(TN$): WX$ = "": For X = 1 To (L - 3): A = Asc(Mid$(TN$, X, 1))
WT$ = WT$ + Chr$(A): Next X
TN$ = WT$: Return

5500 For I = 1 To Y%
    If O%(I) > T%(I) Then WN = WN + 1
    If O%(I) < T%(I) Then LS = LS + 1
    If O%(I) = T%(I) Then TY = TY + 1
Next
Return

9131 H1$(XW) = Right$(H1$(XW), 2) + " " + Left$(H1$(XW), Len(H1$(XW)) - 3): GoSub 9900: Return

9900 For XX = 4 To 11: If Mid$(H1$(XW), XX, 1) = " " Or Mid$(H1$(XW), XX, 1) = "." Then Mid$(H1$(XW), XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(H1$(XW), XX, 2) = "ST" Then Mid$(H1$(XW), 9, 1) = "_": Mid$(H1$(XW), 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(H1$(XW), XX, 1) = "_" Then QQ = XX - 4: GoTo 9906
    GoTo 9907

9906 Next XX

9907 G$ = H1$(XW): H1$(XW) = Right$(H1$(XW), Len(H1$(XW)) - 3): YR$ = Left$(G$, 2)
If Len(H1$(XW)) > 8 Then H1$(XW) = Left$(H1$(XW), 8)
H1$(XW) = H1$(XW) + "." + YR$: Return

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

4500 Locate 10, 8
GoSub 4600
Print "NO LEAGUE FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
GoTo 2900

'$INCLUDE: 'Comp.BM'
'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'

