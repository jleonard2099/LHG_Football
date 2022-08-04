'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - View Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

Dim O%(120), O$(120), T%(120), P%(1), LC(15), LC$(15, 1), Q$(500), teamNames$(500)
Dim teamIndex%(500), LR(9), LI(9), LI$(9, 1)
Dim K(1, 12), K2(1, 13, 17), Z2(13, 17), Z(38), PA%(5), LP(3), LK(2), PK(2), LF(1)
Dim W6(1, 1), A$(1), A1$(1, 9), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2)
Dim Z1(38), CRD(120), SITE$(120), D$(2), TT(3), II(3), PP(3), YY(3)
Dim A7$(0, 1), QB(3, 10), AM(1, 4), AF(1, 4), LR$(9, 1), LP$(3, 1), LK$(2, 1)
Dim PK$(2, 1), LF$(1)
Dim H$(120), DS$(1, 14), DI$(1, 9), GI(1, 9, 2), GS(1, 14, 1), AP%(1, 120)
Open "DEV.BAS" For Input As #1: Input #1, D$(0): Input #1, D$(1): Close #1

14 Cls: Call TITLE
Print: Print: Print: Print: J = 6: GoSub 2075: Locate , 30: Color 3: Print "VIEW/PRINT STATS IN:"
Locate , 30: Color 15: Print "C";: Color 7: Print "OLLEGE FORMAT"
Locate , 30: Color 15: Print "P";: Color 7: Print "RO FORMAT"
Locate , 30: Color 15: Print "E";: Color 7: Print "XIT TO MAIN MENU"
24 I$ = GetKeyPress$
If UCase$(I$) = "C" Then G9 = 0: GoTo 86
If UCase$(I$) = "P" Then G9 = 1: GoTo 86
If UCase$(I$) = "E" Or I$ = Chr$(27) Then Z$ = "END": GoTo 90
GoTo 24

86 GoSub 3056
B$ = A$: GoSub 6585
Color 15, 0: Cls: Call TITLE
Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
D% = 0

90 If UCase$(Z$) = "END" Then Run "HELLO.EXE"
PX = 0: GoSub 9800
If Not _FileExists(D$(1) + Z$) Then 5025

95 Open D$(1) + Z$ For Input As #1
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
For I = 0 To 9: Input #1, LI(I): Input #1, LI$(I, 0): Input #1, LI$(I, 1): Next
Close #1
For J = 0 To 13
    For K = 0 To 17: K2(PX, J, K) = Z2(J, K): Next K
Next J
For L = 0 To 12: K(0, L) = Z(L): Next L
For L = 0 To 12: K(1, L) = Z1(L): Next L
W6(0, 0) = Z(13)
W6(0, 1) = Z(14)
W6(1, 0) = Z1(13): W6(1, 1) = Z1(14)
GoTo 8530

5025 Cls: Print "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
I$ = GetKeyPress$
GoTo 14

8530 '*** VIEW STATS ON SCREEN X=0, PRINTER X=1 ***
DM% = 0
Cls: Call TITLE
J = 4: GoSub 2075: Color 7
Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
8651 I$ = GetKeyPress$
Color 14, 0
If UCase$(I$) = "S" Then X = 0: GoTo 8535
If UCase$(I$) = "P" Then X = 1: GoTo 9910
GoTo 8651

8535 GoSub 8660
GoSub 9806
For I = 1 To Y%
    Color 7
    If I / 2 = Int(I / 2) Then Color 15
    Print O$(I);: Locate , 19: Print Using "###"; O%(I);: Print "-";: Print Using "###   "; T%(I);
    If O%(I) > T%(I) Then W% = W% + 1: Print "W";
    If O%(I) < T%(I) Then L% = L% + 1: Print "L";
    If O%(I) = T%(I) Then X% = X% + 1: Print "T";
    Locate , 31: Print Using "####,###   "; CRD(I);: Print H$(I); "  "; Left$(SITE$(I), 24);
    Locate , 69
    Print "(";: Print Using "##"; W%;: Print "-";: Print Using "##"; L%;: Print "-";: Print Using "##"; X%;: Print ")"
    P%(0) = P%(0) + O%(I): P%(1) = P%(1) + T%(I)
    If I = 20 Or I = 40 Or I = 60 Or I = 80 Or I = 100 Or I = 120 Then I$ = GetKeyPress$: GoSub 9806
Next I
If G9 = 0 Then Locate 24, 1: Print Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; PW; SS;
P%(0) = 0: P%(1) = 0: W% = 0: L% = 0: X% = 0
GoSub 8670: GoSub 8660
Call TITLE
Locate 2: Color 12: Print A$(0); " STATISTICS REPORT"
Print
Color 14: Print "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
Color 15: Print A$(0);: Locate , 30: Print Using " ###  ###  ###  ###   ###"; Z(32); Z(33); Z(34); Z(35); Z(32) + Z(33) + Z(34) + Z(35) + Z(36)
Print "OPPONENTS";: Locate , 30: Print Using " ###  ###  ###  ###   ###"; Z1(32); Z1(33); Z1(34); Z1(35); Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)
Print
Color 14: Print "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
Color 15: Print A$(0);: Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z(32) / Y%; Z(33) / Y%; Z(34) / Y%; Z(35) / Y%; (Z(32) + Z(33) + Z(34) + Z(35) + Z(36)) / Y%
Print "OPPONENTS";: Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z1(32) / Y%; Z1(33) / Y%; Z1(34) / Y%; Z1(35) / Y%; (Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)) / Y%
Print
HD = 0: AD = 0: ND = 0: TD = 0
HT = 0: CT = 0: NT = 0: TT = 0
HA = 0: AA = 0: NA = 0: TA = 0
For I = 1 To Y%
    If Left$(H$(I), 1) = "H" Then HD = HD + 1: HT = HT + CRD(I)
    If Left$(H$(I), 1) = "A" Then AD = AD + 1: CT = CT + CRD(I)
    If Left$(H$(I), 1) = "X" Then ND = ND + 1: NT = NT + CRD(I)
    TD = TD + 1: TT = TT + CRD(I)
Next
If HD <> 0 Then HA = HT / HD
If AD <> 0 Then AA = CT / AD
If ND <> 0 Then NA = NT / ND
If TD <> 0 Then TA = TT / TD
Color 14: Print "ATTENDANCE    DATES      TOTAL    AVERAGE"
Color 15: Print Using "HOME            ##    #####,###   ###,###"; HD; HT; HA
Color 15: Print Using "AWAY            ##    #####,###   ###,###"; AD; CT; AA
Color 15: Print Using "NEUTRAL         ##    #####,###   ###,###"; ND; NT; NA
Color 15: Print Using "TOTAL           ##    #####,###   ###,###"; TD; TT; TA
GoSub 8670: GoSub 8660
Call TITLE
Locate 2: Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); A$(0); Tab(61); "OPPONENT TOTALS"
Color 7
Print "FIRST DOWNS"; Tab(45); Using "####"; K(0, 0);: Print Tab(65); Using "####"; K(1, 0)
Print "RUSHING ATTEMPTS"; Tab(45);
If G9 = 0 Then Print Using "####"; K(0, 1); Else Print Using "####"; K(0, 1) - W6(0, 0);
Print Tab(65);
If G9 = 0 Then Print Using "####"; K(1, 1) Else Print Using "####"; K(1, 1) - W6(1, 0)
Color 15
Print "NET RUSHING YARDAGE"; Tab(45);
If G9 = 0 Then Print Using "####"; K(0, 2); Else Print Using "####"; K(0, 2) + W6(0, 1);
Print Tab(65);
If G9 = 0 Then Print Using "####"; K(1, 2) Else Print Using "####"; K(1, 2) + W6(1, 1)
Color 7
Print "YARDS PER RUSH"; Tab(45);
If G9 = 0 And K(0, 1) <> 0 Then Print Using "##.#"; K(0, 2) / K(0, 1);
If G9 <> 0 And K(0, 1) - W6(0, 0) <> 0 Then Print Using "##.#"; (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0));
Print Tab(65);
If G9 = 0 And K(1, 1) <> 0 Then Print Using "##.#"; K(1, 2) / K(1, 1)
If G9 <> 0 And K(1, 1) - W6(1, 0) <> 0 Then Print Using "##.#"; (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0))
If G9 = 0 And K(1, 1) = 0 Or G9 <> 0 And K(1, 1) - W6(1, 0) = 0 Then Print
Print "RUSHING YARDS PER GAME"; Tab(44);
If G9 = 0 Then Print Using "###.#"; K(0, 2) / Y%; Else Print Using "###.#"; (K(0, 2) + W6(0, 1)) / Y%;
Print Tab(64);
If G9 = 0 Then Print Using "###.#"; K(1, 2) / Y% Else Print Using "###.#"; (K(1, 2) + W6(1, 1)) / Y%
Print
Print "PASSES ATTEMPTED"; Tab(45); Using "####"; K(0, 3);: Print Tab(65); Using "####"; K(1, 3)
Print "PASSES COMPLETED"; Tab(45); Using "####"; K(0, 4);: Print Tab(65); Using "####"; K(1, 4)
Print "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; K(0, 5);: Print Tab(65); Using "####"; K(1, 5)
Print "PASS COMPLETION PERCENTAGE"; Tab(44);
If K(0, 3) <> 0 Then Print Using "###.#"; K(0, 4) / K(0, 3) * 100;
If K(1, 3) <> 0 Then Print Tab(64); Using "###.#"; K(1, 4) / K(1, 3) * 100
If K(1, 3) = 0 Then Print
Color 15
Print "NET YARDS PASSING"; Tab(45); Using "####"; K(0, 6);: Print Tab(65); Using "####"; K(1, 6)
Color 7
Print "YARDS PER PASS ATTEMPT"; Tab(45);
If K(0, 3) <> 0 Then Print Using "##.#"; K(0, 6) / K(0, 3);
If K(1, 3) <> 0 Then Print Tab(65); Using "##.#"; K(1, 6) / K(1, 3)
If K(1, 3) = 0 Then Print
Print "YARDS PER PASS COMPLETION"; Tab(45);
If K(0, 4) <> 0 Then Print Using "##.#"; K(0, 6) / K(0, 4);
If K(1, 4) <> 0 Then Print Tab(65); Using "##.#"; K(1, 6) / K(1, 4)
If K(1, 4) = 0 Then Print
Print "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; K(0, 6) / Y%;: Print Tab(64); Using "###.#"; K(1, 6) / Y%
Print
Print "TOTAL PLAYS"; Tab(45);
If G9 = 0 Then Print Using "####"; K(0, 1) + K(0, 3); Else Print Using "####"; K(0, 1) - W6(0, 0) + K(0, 3);
Print Tab(65);
If G9 = 0 Then Print Using "####"; K(1, 1) + K(1, 3) Else Print Using "####"; K(1, 1) - W6(1, 0) + K(1, 3)
Print "TOTAL PLAYS PER GAME"; Tab(44);
If G9 = 0 Then Print Using "###.#"; (K(0, 1) + K(0, 3)) / Y%; Else Print Using "###.#"; (K(0, 1) - W6(0, 0) + K(0, 3)) / Y%;
Print Tab(64);
If G9 = 0 Then Print Using "###.#"; (K(1, 1) + K(1, 3)) / Y% Else Print Using "###.#"; (K(1, 1) - W6(1, 0) + K(1, 3)) / Y%
Color 15
Print "TOTAL NET YARDS"; Tab(45);
If G9 = 0 Then Print Using "####"; K(0, 2) + K(0, 6); Else Print Using "####"; K(0, 2) + W6(0, 1) + K(0, 6);
Print Tab(65);
If G9 = 0 Then Print Using "####"; K(1, 2) + K(1, 6) Else Print Using "####"; K(1, 2) + W6(1, 1) + K(1, 6)
Color 7
Print "YARDS GAINED PER PLAY"; Tab(45);
If G9 = 0 Then Print Using "##.#"; (K(0, 2) + K(0, 6)) / (K(0, 1) + K(0, 3)); Else Print Using "##.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / (K(0, 1) - W6(0, 0) + K(0, 3));
Print Tab(64);
If G9 = 0 Then Print Using "##.#"; (K(1, 2) + K(1, 6)) / (K(1, 1) + K(1, 3)) Else Print Using "##.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / (K(1, 1) - W6(1, 0) + K(1, 3))
Print "YARDS GAINED PER GAME"; Tab(44);
If G9 = 0 Then Print Using "###.#"; (K(0, 2) + K(0, 6)) / Y%; Else Print Using "###.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / Y%;
Print Tab(64);
If G9 = 0 Then Print Using "###.#"; (K(1, 2) + K(1, 6)) / Y% Else Print Using "###.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / Y%
GoSub 8670: GoSub 8660
Call TITLE
Locate 2: Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); A$(0); Tab(61); "OPPONENT TOTALS"
Color 7
Print "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
Print Using "###"; Z(22);: Print "/";: Print Using "####"; Z(23);
Print Tab(61); Using "###"; Z1(22);: Print "/";: Print Using "####"; Z1(23)
Print "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
If Z(22) <> 0 Then Print Using "##.#"; Z(23) / Z(22);
If Z1(22) <> 0 Then Print Tab(65); Using "##.#"; Z1(23) / Z1(22)
If Z1(22) = 0 Then Print
Print "KICKOFF RETURNS PER GAME"; Tab(45);: Print Using "##.#"; Z(22) / Y%;: Print Tab(65); Using "##.#"; Z1(22) / Y%
Print "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
Print Using "###"; Z(24);: Print "/";: Print Using "####"; Z(25);
Print Tab(61); Using "###"; Z1(24);: Print "/";: Print Using "####"; Z1(25)
Print "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
If Z(24) <> 0 Then Print Using "##.#"; Z(25) / Z(24);
If Z1(24) <> 0 Then Print Tab(65); Using "##.#"; Z1(25) / Z1(24)
If Z1(24) = 0 Then Print
Print "PUNT RETURNS PER GAME"; Tab(45);: Print Using "##.#"; Z(24) / Y%;: Print Tab(65); Using "##.#"; Z1(24) / Y%
Print "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
Print Using "###"; Z(26);: Print "/";: Print Using "####"; Z(27);
Print Tab(61); Using "###"; Z1(26);: Print "/";: Print Using "####"; Z1(27)
Print "AVERAGE YARDAGE PER RETURN"; Tab(45);
If Z(26) <> 0 Then Print Using "##.#"; Z(27) / Z(26);
If Z1(26) <> 0 Then Print Tab(65); Using "##.#"; Z1(27) / Z1(26)
If Z1(26) = 0 Then Print
Print "INTERCEPTION RETURNS PER GAME"; Tab(45);: Print Using "##.#"; Z(26) / Y%;: Print Tab(65); Using "##.#"; Z1(26) / Y%
Print
Print "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
Print Using "###"; K(0, 7);: Print "/";: Print Using "####"; K(0, 8);
Print Tab(61); Using "###"; K(1, 7);: Print "/";: Print Using "####"; K(1, 8)
Print "AVERAGE YARDS PER PUNT"; Tab(45);
If K(0, 7) <> 0 Then Print Using "##.#"; K(0, 8) / K(0, 7);
If K(1, 7) <> 0 Then Print Tab(65); Using "##.#"; K(1, 8) / K(1, 7)
If K(1, 7) = 0 Then Print
Print "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print Using "##.#"; K(0, 7) / Y%;: Print Tab(65); Using "##.#"; K(1, 7) / Y%
Print
Print "FUMBLES / FUMBLES LOST"; Tab(41);
Print Using "###"; K(0, 9);: Print "/";: Print Using "####"; K(0, 10);
Print Tab(61); Using "###"; K(1, 9);: Print "/";: Print Using "####"; K(1, 10)
Print "PENALTIES / YARDS PENALIZED"; Tab(41);
Print Using "###"; K(0, 11);: Print "/";: Print Using "####"; K(0, 12);
Print Tab(61); Using "###"; K(1, 11);: Print "/";: Print Using "####"; K(1, 12)
Print "AVERAGE YARDS PER PENALTY"; Tab(45);
If K(0, 11) <> 0 Then Print Using "##.#"; K(0, 12) / K(0, 11);
If K(1, 11) <> 0 Then Print Tab(65); Using "##.#"; K(1, 12) / K(1, 11)
If K(1, 11) = 0 Then Print
Print "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
Print Using "##.#"; K(0, 11) / Y%;: Print "/";: Print Using "##.#"; K(0, 12) / Y%;
Print Tab(60); Using "##.#"; K(1, 11) / Y%;: Print "/";: Print Using "##.#"; K(1, 12) / Y%
Print "SAFETIES"; Tab(45);: Print Using "####"; Z(21);: Print Tab(65); Using "####"; Z1(21)
Print "TIMES SACKED / YARDS LOST"; Tab(41);
Print Using "###"; W6(0, 0);: Print "/";: Print Using "####"; W6(0, 1);
Print Tab(61); Using "###"; W6(1, 0);: Print "/";: Print Using "####"; W6(1, 1)
GoSub 8670: GoSub 8660
Call TITLE
Locate 4, 1
Color 12
Print "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"
I = 0
GoSub 8699
For I1 = 0 To 13
    If K2(I, I1, 0) = 0 Then 8560
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    If I1 <= 9 Then Print A1$(I, I1);
    If I1 > 9 Then Print A3$(I, I1 - 10);
    Print Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 0); K2(I, I1, 1); K2(I, I1, 1) / K2(I, I1, 0); K2(I, I1, 1) / Y%; K2(I, I1, 2);
    If I1 > 9 Then Print: GoTo 8560
    Print Using "###"; LR(I1);: Print LR$(I1, 0); Tab(64); LR$(I1, 1)

8560 Next I1
Color 14
Print
Print A$(0); Tab(18);
If G9 = 0 Then Print Using "####  ####   ##.#      ###.#    ##"; K(0, 1); K(0, 2); K(0, 2) / K(0, 1); K(0, 2) / Y%; Z(15)
If G9 <> 0 Then Print Using "####  ####   ##.#      ###.#    ##"; K(0, 1) - W6(0, 0); K(0, 2) + W6(0, 1); (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0)); (K(0, 2) + W6(0, 1)) / Y%; Z(15)
Print "OPPONENT TOTALS"; Tab(18);
If G9 = 0 Then Print Using "####  ####   ##.#      ###.#    ##"; K(1, 1); K(1, 2); K(1, 2) / K(1, 1); K(1, 2) / Y%; Z1(15)
If G9 <> 0 Then Print Using "####  ####   ##.#      ###.#    ##"; K(1, 1) - W6(1, 0); K(1, 2) + W6(1, 1); (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0)); (K(1, 2) + W6(1, 1)) / Y%; Z1(15)
GoSub 8670: GoSub 8660
Call TITLE
Locate 4, 1
Color 12
Print "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"
I = 0
For I1 = 0 To 5
    If K2(I, I1, 6) = 0 Then 8562
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print A2$(I, I1); Tab(18);
    Print Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 6); K2(I, I1, 7); K2(I, I1, 7) / K2(I, I1, 6); K2(I, I1, 7) / Y%; K2(I, I1, 8);
    Print Using "###"; LC(I1 + 10);: Print LC$(I1 + 10, 0); Tab(64); LC$(I1 + 10, 1)

8562 Next I1
For I1 = 0 To 9
    If K2(I, I1, 3) = 0 Then 8578
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print A1$(I, I1); Tab(18);
    Print Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 3); K2(I, I1, 4); K2(I, I1, 4) / K2(I, I1, 3); K2(I, I1, 4) / Y%; K2(I, I1, 5);
    Print Using "###"; LC(I1);: Print LC$(I1, 0); Tab(64); LC$(I1, 1)

8578 Next I1
Print
Color 14
Print A$(0); Tab(18);
Print Using "####  ####   ##.#      ###.#    ##"; K(0, 4); K(0, 6); K(0, 6) / K(0, 4); K(0, 6) / Y%; Z(17)
Print "OPPONENT TOTALS"; Tab(18);
Print Using "####  ####   ##.#      ###.#    ##"; K(1, 4); K(1, 6); K(1, 6) / K(1, 4); K(1, 6) / Y%; Z1(17)
GoSub 8670: GoSub 8660
Call TITLE
Locate 4, 1
Color 12
Print "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"
For I1 = 0 To 3
    If K2(I, I1, 9) = 0 Then 8588
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print A3$(I, I1); Tab(18); Using "####  ### ###.# #### ##.#"; K2(I, I1, 9); K2(I, I1, 10); K2(I, I1, 10) / K2(I, I1, 9) * 100; K2(I, I1, 12); K2(I, I1, 12) / K2(I, I1, 9);
    If K2(I, I1, 10) = 0 Then Print "   --"; Else Print Using " ##.#"; K2(I, I1, 12) / K2(I, I1, 10);
    Print Using " ###.# ### ### ###.#     "; K2(I, I1, 12) / Y%; K2(I, I1, 13); K2(I, I1, 11); K2(I, I1, 11) / K2(I, I1, 9) * 100;
    If G9 = 0 Then GoSub 2500: Print Using "###.#"; RT
    If G9 = 1 Then GoSub 2000: Print Using "###.#"; RT
    'PRINT TAB(57); USING "###"; LP(I1); : PRINT LP$(I1,0)

8588 Next I1
Print
Color 14
Print A$(0); Tab(18);
Print Using "####  ### ###.# #### ##.#"; K(0, 3); K(0, 4); K(0, 4) / K(0, 3) * 100; K(0, 6); K(0, 6) / K(0, 3);
Print Using " ##.#"; K(0, 6) / K(0, 4);
Print Using " ###.# ### ### ###.#     "; K(0, 6) / Y%; Z(17); K(0, 5); K(0, 5) / K(0, 3) * 100
Print "OPPONENT TOTALS"; Tab(18);
Print Using "####  ### ###.# #### ##.#"; K(1, 3); K(1, 4); K(1, 4) / K(1, 3) * 100; K(1, 6); K(1, 6) / K(1, 3);
Print Using " ##.#"; K(1, 6) / K(1, 4);
Print Using " ###.# ### ### ###.#     "; K(1, 6) / Y%; Z1(17); K(1, 5); K(1, 5) / K(1, 3) * 100
Print
Print
Color 12
Print "PASSING           LONG PASS    AGAINST"
For I1 = 0 To 3
    If K2(I, I1, 9) = 0 Then 8589
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print A3$(I, I1); Tab(22); Using "###"; LP(I1);: Print LP$(I1, 0); Tab(32); LP$(I1, 1)

8589 Next
GoSub 8670: GoSub 8660: Call TITLE
Locate 3, 1
Color 12
Print "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
I = 0
For I1 = 0 To 2
    If K2(I, I1, 14) = 0 Then 8593
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print A4$(I, I1); Tab(18);
    Print Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
    Print Using "###"; LK(I1);: Print LK$(I1, 0); Tab(64); LK$(I1, 1)

8593 Next I1
Print
Color 14
Print A$(0); Tab(18);
If Z(22) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z(22); Z(23); Z(23) / Z(22); Z(23) / Y%; Z(18)
Print "OPPONENT TOTALS"; Tab(18);
If Z1(22) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z1(22); Z1(23); Z1(23) / Z1(22); Z1(23) / Y%; Z1(18)
Print
Print
Print
Color 12
Print "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
I = 0
For I1 = 3 To 5
    If K2(I, I1, 14) = 0 Then 8610
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print A5$(I, I1 - 3); Tab(18);
    Print Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
    Print Using "###"; PK(I1 - 3);: Print PK$(I1 - 3, 0); Tab(64); PK$(I1 - 3, 1)

8610 Next I1
Print
Color 14
Print A$(0); Tab(18);
If Z(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z(24); Z(25); Z(25) / Z(24); Z(25) / Y%; Z(19)
Print "OPPONENT TOTALS"; Tab(18);
If Z1(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z1(24); Z1(25); Z1(25) / Z1(24); Z1(25) / Y%; Z1(19)
GoSub 8670: GoSub 8660
Call TITLE
I = 0
Locate 3, 1
Color 12
Print "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
Color 15
Print A6$(I, 0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); K(0, 7) / Y%; Z(31); Z(30); Z(28)
Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); K(1, 7) / Y%; Z1(31); Z1(30)
Print
Print
Print
Color 12
Print "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
Color 15
Print A$(0); Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); Z1(24); Z1(25); Z1(25) / Z1(24); (K(0, 8) - Z1(25)) / K(0, 7)
Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); Z(24); Z(25); Z(25) / Z(24); (K(1, 8) - Z(25)) / K(1, 7)
Print
Print
Print
Color 12
Print "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
Color 15
If K2(I, 4, 17) = 0 And K2(I, 6, 17) = 0 Then 8685
Print A7$(I, 0);
Print Tab(16);
If K2(I, 6, 17) <> 0 Then Print Using "### ### ###.# "; K2(I, 5, 17); K2(I, 6, 17); K2(I, 5, 17) / K2(I, 6, 17) * 100;
If K2(I, 4, 17) = 0 Then Print: GoTo 8685
Print Tab(30);
Print Using "### ### ###.# ## ##"; K2(I, 3, 17); K2(I, 4, 17); K2(I, 3, 17) / K2(I, 4, 17) * 100; LF(0); AM(0, 0);
Print "-";: Print Using "## ##"; AF(0, 0); AM(0, 1);
Print "-";: Print Using "## ##"; AF(0, 1); AM(0, 2);
Print "-";: Print Using "## ##"; AF(0, 2); AM(0, 3);
Print "-";: Print Using "## ##"; AF(0, 3); AM(0, 4);
Print "-";: Print Using "##"; AF(0, 4)

8685 If K2(I, 8, 17) = 0 And K2(I, 10, 17) = 0 Then 8594
Print A7$(I, 1); Tab(16);
If K2(I, 10, 17) <> 0 Then Print Using "### ### ###.# "; K2(I, 9, 17); K2(I, 10, 17); K2(I, 9, 17) / K2(I, 10, 17) * 100;
If K2(I, 8, 17) = 0 Then Print: GoTo 8594
Print Tab(30);
Print Using "### ### ###.# ## ##"; K2(I, 7, 17); K2(I, 8, 17); K2(I, 7, 17) / K2(I, 8, 17) * 100; LF(1); AM(1, 0);
Print "-";: Print Using "## ##"; AF(1, 0); AM(1, 1);
Print "-";: Print Using "## ##"; AF(1, 1); AM(1, 2);
Print "-";: Print Using "## ##"; AF(1, 2); AM(1, 3);
Print "-";: Print Using "## ##"; AF(1, 3); AM(1, 4);
Print "-";: Print Using "##"; AF(1, 4)

8594 Print
NOONE:
GoSub 8670: GoSub 8660: Call TITLE
Locate 3, 1: Color 12
Print "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
I = 0
For I1 = 0 To 9
    If GI(I, I1, 0) = 0 Then 8628
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print DI$(I, I1); Tab(18);
    Print Using "####  ####   ##.#      ###.#    ##    "; GI(I, I1, 0); GI(I, I1, 1); GI(I, I1, 1) / GI(I, I1, 0); GI(I, I1, 1) / Y%; GI(I, I1, 2);
    Print Using "###"; LI(I1);: Print LI$(I1, 0); Tab(64); LI$(I1, 1)

8628 Next I1
GoSub 8670: GoSub 8660: Call TITLE
Locate 4, 1: Color 12
Print "DEFENSIVE SACKS    SACKS      YARDS LOST"
For I1 = 0 To 14
    If GS(I, I1, 0) = 0 Then 8627
    DM% = DM% + 1
    Color 7
    If DM% / 2 = Int(DM% / 2) Then Color 15
    Print DS$(I, I1); Tab(20); Using "###            ###"; GS(I, I1, 0); GS(I, I1, 1)

8627 Next
I$ = GetKeyPress

8624 Cls: Print "DISPLAY STATISTICS AGAIN (YN)";
8629 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 8530
GoTo 8800

8655 If X = 1 Then X = 0
Return

8660 If X = 0 Then Cls
Return

8670 If X = 0 Then I$ = GetKeyPress$
Return

8699 If D% = 1 Then 8704
For I1 = 0 To 9: For I2 = 0 To 3
    IF A1$(I, I1) = A3$(I, I2) THEN K2(I, I1, 0) = K2(I, I1, 0) + K2(I, I2 + 10, 0): K2(I, I1, 1) = K2(I, I1, 1) + K2(I, I2 + 10, 1): K2(I, I1, 2) = K2(I, I1, 2) + K2(I, I2 + 10, 2): K2(I, I2 + 10, 0) = 0: K2(I, I2 + 10, 1) = 0: K2(I, I2 + 10, 2)  _
= 0
Next I2: Next I1

8704 D% = 1: Return

8800 Cls: Print "RESET STATS TO 0 ? (YN)";:
8801 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then 14
Print: Print "ARE YOU SURE ? (YN) ";
8804 I$ = GetKeyPress$
If UCase$(I$) = "N" Then GoTo 14 'X = FRE(0)
If UCase$(I$) = "Y" Then 8812

8812 Kill D$(1) + Z$
Kill D$(1) + Z$ + "S"
Cls
GoTo 14

9800 P%(0) = 0: P%(1) = 0: L% = 0: W% = 0: X% = 0
If Not _FileExists(D$(1) + Z$) Then 5025
Open D$(1) + Z$ + "S" For Input As #2
Input #2, X$
Y% = Val(X$)
For I = 1 To 120: Input #2, O%(I): Next I
For I = 1 To 120: Input #2, O$(I): Next I
For I = 1 To 120: Input #2, T%(I): Next I
For I = 1 To 120: Input #2, H$(I): Next I
For I = 0 To 1
For I1 = 1 To 120: Input #2, AP%(I, I1): Next: Next
For I = 1 To 120: Input #2, CRD(I): Next I
For I = 1 To 120: Input #2, SITE$(I): Next I
Close #2
'STRENGTH OF SCHEDULE
PW = 0: SS = 0
For I = 1 To Y%
    SS = AP%(1, I) + SS
Next
SS = SS / Y%
'INDIVIDUAL GAME POWER RATINGS
GL = 0: PW = 0
For I = 1 To Y%
    RAT = O%(I) - T%(I) + AP%(1, I)
    If Left$(H$(I), 1) = "H" Then RAT = RAT - 3
    If Left$(H$(I), 1) = "A" Then RAT = RAT + 3
    PW = PW + RAT
Next
If Y% > 4 Then 9805
GL = 5 - Y%
PW = PW + GL * AP%(0, 1)

9805 PW = PW / (Y% + GL)
Return

9806 '*** ADD IN CLEAR SCREEN AND TITLE BAR ***
Cls
Color 12: Print "GAME-BY-GAME REPORT FOR "; A$(0)
Color 14
Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"
Return

9910 ' *** PRINT STATS ***
Open "REPORT" For Output As #2
Print #2, "GAME-BY-GAME REPORT FOR "; A$(0)
Print #2,
Print #2, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"
For I = 1 To Y%
    Print #2, O$(I); Tab(19); Using "###"; O%(I);
    Print #2, "-";: Print #2, Using "###   "; T%(I);
    If O%(I) > T%(I) Then W% = W% + 1: Print #2, "W";
    If O%(I) < T%(I) Then L% = L% + 1: Print #2, "L";
    If O%(I) = T%(I) Then X% = X% + 1: Print #2, "T";
    Print #2, Tab(31);: Print #2, Using "####,###   "; CRD(I);: Print #2, H$(I); "  "; Left$(SITE$(I), 24);
    Print #2, Tab(69);
    Print #2, "(";: Print #2, Using "##"; W%;: Print #2, "-";: Print #2, Using "##"; L%;: Print #2, "-";: Print #2, Using "##"; X%;: Print #2, ")"
    P%(0) = P%(0) + O%(I): P%(1) = P%(1) + T%(I)
Next I
Print #2,
If G9 = 0 Then Print #2, Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; PW; SS
P%(0) = 0: P%(1) = 0: W% = 0: L% = 0: X% = 0
Print #2,
Print #2,
Print #2, "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
Print #2, A$(0); Tab(30); Using " ###  ###  ###  ###   ###"; Z(32); Z(33); Z(34); Z(35); Z(32) + Z(33) + Z(34) + Z(35) + Z(36)
Print #2, "OPPONENTS"; Tab(30); Using " ###  ###  ###  ###   ###"; Z1(32); Z1(33); Z1(34); Z1(35); Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)
Print #2,
Print #2, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
Print #2, A$(0); Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z(32) / Y%; Z(33) / Y%; Z(34) / Y%; Z(35) / Y%; (Z(32) + Z(33) + Z(34) + Z(35) + Z(36)) / Y%
Print #2, "OPPONENTS"; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z1(32) / Y%; Z1(33) / Y%; Z1(34) / Y%; Z1(35) / Y%; (Z1(32) + Z1(33) + Z1(34) + Z1(35) + Z(36)) / Y%
Print #2,
HD = 0: AD = 0: ND = 0: TD = 0
HT = 0: CT = 0: NT = 0: TT = 0
HA = 0: AA = 0: NA = 0: TA = 0
For I = 1 To Y%
    If Left$(H$(I), 1) = "H" Then HD = HD + 1: HT = HT + CRD(I)
    If Left$(H$(I), 1) = "A" Then AD = AD + 1: CT = CT + CRD(I)
    If Left$(H$(I), 1) = "X" Then ND = ND + 1: NT = NT + CRD(I)
    TD = TD + 1: TT = TT + CRD(I)
Next
If HD <> 0 Then HA = HT / HD
If AD <> 0 Then AA = CT / AD
If ND <> 0 Then NA = NT / ND
If TD <> 0 Then TA = TT / TD
Print #2, "ATTENDANCE    DATES      TOTAL    AVERAGE"
Print #2, Using "HOME            ##    #####,###   ###,###"; HD; HT; HA
Print #2, Using "AWAY            ##    #####,###   ###,###"; AD; CT; AA
Print #2, Using "NEUTRAL         ##    #####,###   ###,###"; ND; NT; NA
Print #2, Using "TOTAL           ##    #####,###   ###,###"; TD; TT; TA
Print #2, Chr$(12)
Print #2, "TEAM STATISTICS CATEGORY"; Tab(41); A$(0); Tab(61); "OPPONENT TOTALS"
Print #2, "FIRST DOWNS"; Tab(45); Using "####"; K(0, 0);: Print #2, Tab(65); Using "####"; K(1, 0)
Print #2, "RUSHING ATTEMPTS"; Tab(45);
If G9 = 0 Then Print #2, Using "####"; K(0, 1); Else Print #2, Using "####"; K(0, 1) - W6(0, 0);
Print #2, Tab(65);
If G9 = 0 Then Print #2, Using "####"; K(1, 1) Else Print #2, Using "####"; K(1, 1) - W6(1, 0)
Print #2, "NET RUSHING YARDAGE"; Tab(45);
If G9 = 0 Then Print #2, Using "####"; K(0, 2); Else Print #2, Using "####"; K(0, 2) + W6(0, 1);
Print #2, Tab(65);
If G9 = 0 Then Print #2, Using "####"; K(1, 2) Else Print #2, Using "####"; K(1, 2) + W6(1, 1)
Print #2, "YARDS PER RUSH"; Tab(45);
If G9 = 0 And K(0, 1) <> 0 Then Print #2, Using "##.#"; K(0, 2) / K(0, 1);
If G9 <> 0 And K(0, 1) - W6(0, 0) <> 0 Then Print #2, Using "##.#"; (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0));
Print #2, Tab(65);
If G9 = 0 And K(1, 1) <> 0 Then Print #2, Using "##.#"; K(1, 2) / K(1, 1)
If G9 <> 0 And K(1, 1) - W6(1, 0) <> 0 Then Print #2, Using "##.#"; (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0))
If G9 = 0 And K(1, 1) = 0 Or G9 <> 0 And K(1, 1) - W6(1, 0) = 0 Then Print #2,
Print #2, "RUSHING YARDS PER GAME"; Tab(44);
If G9 = 0 Then Print #2, Using "###.#"; K(0, 2) / Y%; Else Print #2, Using "###.#"; (K(0, 2) + W6(0, 1)) / Y%;
Print #2, Tab(64);
If G9 = 0 Then Print #2, Using "###.#"; K(1, 2) / Y% Else Print #2, Using "###.#"; (K(1, 2) + W6(1, 1)) / Y%
Print #2,
Print #2, "PASSES ATTEMPTED"; Tab(45); Using "####"; K(0, 3);: Print #2, Tab(65); Using "####"; K(1, 3)
Print #2, "PASSES COMPLETED"; Tab(45); Using "####"; K(0, 4);: Print #2, Tab(65); Using "####"; K(1, 4)
Print #2, "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; K(0, 5);: Print #2, Tab(65); Using "####"; K(1, 5)
Print #2, "PASS COMPLETION PERCENTAGE"; Tab(44);
If K(0, 3) <> 0 Then Print #2, Using "###.#"; K(0, 4) / K(0, 3) * 100;
If K(1, 3) <> 0 Then Print #2, Tab(64); Using "###.#"; K(1, 4) / K(1, 3) * 100
If K(1, 3) = 0 Then Print #2,
Print #2, "NET YARDS PASSING"; Tab(45); Using "####"; K(0, 6);: Print #2, Tab(65); Using "####"; K(1, 6)
Print #2, "YARDS PER PASS ATTEMPT"; Tab(45);
If K(0, 3) <> 0 Then Print #2, Using "##.#"; K(0, 6) / K(0, 3);
If K(1, 3) <> 0 Then Print #2, Tab(65); Using "##.#"; K(1, 6) / K(1, 3)
If K(1, 3) = 0 Then Print #2,
Print #2, "YARDS PER PASS COMPLETION"; Tab(45);
If K(0, 4) <> 0 Then Print #2, Using "##.#"; K(0, 6) / K(0, 4);
If K(1, 4) <> 0 Then Print #2, Tab(65); Using "##.#"; K(1, 6) / K(1, 4)
If K(1, 4) = 0 Then Print #2,
Print #2, "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; K(0, 6) / Y%;: Print #2, Tab(64); Using "###.#"; K(1, 6) / Y%
Print #2,
Print #2, "TOTAL PLAYS"; Tab(45);
If G9 = 0 Then Print #2, Using "####"; K(0, 1) + K(0, 3); Else Print #2, Using "####"; K(0, 1) - W6(0, 0) + K(0, 3);
Print #2, Tab(65);
If G9 = 0 Then Print #2, Using "####"; K(1, 1) + K(1, 3) Else Print #2, Using "####"; K(1, 1) - W6(1, 0) + K(1, 3)
Print #2, "TOTAL PLAYS PER GAME"; Tab(44);
If G9 = 0 Then Print #2, Using "###.#"; (K(0, 1) + K(0, 3)) / Y%; Else Print #2, Using "###.#"; (K(0, 1) - W6(0, 0) + K(0, 3)) / Y%;
Print #2, Tab(64);
If G9 = 0 Then Print #2, Using "###.#"; (K(1, 1) + K(1, 3)) / Y% Else Print #2, Using "###.#"; (K(1, 1) - W6(1, 0) + K(1, 3)) / Y%
Print #2, "TOTAL NET YARDS"; Tab(45);
If G9 = 0 Then Print #2, Using "####"; K(0, 2) + K(0, 6); Else Print #2, Using "####"; K(0, 2) + W6(0, 1) + K(0, 6);
Print #2, Tab(65);
If G9 = 0 Then Print #2, Using "####"; K(1, 2) + K(1, 6) Else Print #2, Using "####"; K(1, 2) + W6(1, 1) + K(1, 6)
Print #2, "YARDS GAINED PER PLAY"; Tab(45);
If G9 = 0 Then Print #2, Using "##.#"; (K(0, 2) + K(0, 6)) / (K(0, 1) + K(0, 3)); Else Print #2, Using "##.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / (K(0, 1) - W6(0, 0) + K(0, 3));
Print #2, Tab(64);
If G9 = 0 Then Print #2, Using "##.#"; (K(1, 2) + K(1, 6)) / (K(1, 1) + K(1, 3)) Else Print #2, Using "##.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / (K(1, 1) - W6(1, 0) + K(1, 3))
Print #2, "YARDS GAINED PER GAME"; Tab(44);
If G9 = 0 Then Print #2, Using "###.#"; (K(0, 2) + K(0, 6)) / Y%; Else Print #2, Using "###.#"; (K(0, 2) + W6(0, 1) + K(0, 6)) / Y%;
Print #2, Tab(64);
If G9 = 0 Then Print #2, Using "###.#"; (K(1, 2) + K(1, 6)) / Y% Else Print #2, Using "###.#"; (K(1, 2) + W6(1, 1) + K(1, 6)) / Y%
Print #2,
Print #2, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
Print #2, Using "###"; Z(22);: Print #2, "/";: Print #2, Using "####"; Z(23);
Print #2, Tab(61); Using "###"; Z1(22);: Print #2, "/";: Print #2, Using "####"; Z1(23)
Print #2, "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
If Z(22) <> 0 Then Print #2, Using "##.#"; Z(23) / Z(22);
If Z1(22) <> 0 Then Print #2, Tab(65); Using "##.#"; Z1(23) / Z1(22)
If Z1(22) = 0 Then Print #2,
Print #2, "KICKOFF RETURNS PER GAME"; Tab(45);: Print #2, Using "##.#"; Z(22) / Y%;: Print #2, Tab(65); Using "##.#"; Z1(22) / Y%
Print #2, "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
Print #2, Using "###"; Z(24);: Print #2, "/";: Print #2, Using "####"; Z(25);
Print #2, Tab(61); Using "###"; Z1(24);: Print #2, "/";: Print #2, Using "####"; Z1(25)
Print #2, "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
If Z(24) <> 0 Then Print #2, Using "##.#"; Z(25) / Z(24);
If Z1(24) <> 0 Then Print #2, Tab(65); Using "##.#"; Z1(25) / Z1(24)
If Z1(24) = 0 Then Print #2,
Print #2, "PUNT RETURNS PER GAME"; Tab(45);: Print #2, Using "##.#"; Z(24) / Y%;: Print #2, Tab(65); Using "##.#"; Z1(24) / Y%
Print #2, "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
Print #2, Using "###"; Z(26);: Print #2, "/";: Print #2, Using "####"; Z(27);
Print #2, Tab(61); Using "###"; Z1(26);: Print #2, "/";: Print #2, Using "####"; Z1(27)
Print #2, "AVERAGE YARDAGE PER RETURN"; Tab(45);
If Z(26) <> 0 Then Print #2, Using "##.#"; Z(27) / Z(26);
If Z1(26) <> 0 Then Print #2, Tab(65); Using "##.#"; Z1(27) / Z1(26)
If Z1(26) = 0 Then Print #2,
Print #2, "INTERCEPTION RETURNS PER GAME"; Tab(45);: Print #2, Using "##.#"; Z(26) / Y%;: Print #2, Tab(65); Using "##.#"; Z1(26) / Y%
Print #2,
Print #2, "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
Print #2, Using "###"; K(0, 7);: Print #2, "/";: Print #2, Using "####"; K(0, 8);
Print #2, Tab(61); Using "###"; K(1, 7);: Print #2, "/";: Print #2, Using "####"; K(1, 8)
Print #2, "AVERAGE YARDS PER PUNT"; Tab(45);
If K(0, 7) <> 0 Then Print #2, Using "##.#"; K(0, 8) / K(0, 7);
If K(1, 7) <> 0 Then Print #2, Tab(65); Using "##.#"; K(1, 8) / K(1, 7)
If K(1, 7) = 0 Then Print #2,
Print #2, "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print #2, Using "##.#"; K(0, 7) / Y%;: Print #2, Tab(65); Using "##.#"; K(1, 7) / Y%
Print #2,
Print #2, "FUMBLES / FUMBLES LOST"; Tab(41);
Print #2, Using "###"; K(0, 9);: Print #2, "/";: Print #2, Using "####"; K(0, 10);
Print #2, Tab(61); Using "###"; K(1, 9);: Print #2, "/";: Print #2, Using "####"; K(1, 10)
Print #2, "PENALTIES / YARDS PENALIZED"; Tab(41);
Print #2, Using "###"; K(0, 11);: Print #2, "/";: Print #2, Using "####"; K(0, 12);
Print #2, Tab(61); Using "###"; K(1, 11);: Print #2, "/";: Print #2, Using "####"; K(1, 12)
Print #2, "AVERAGE YARDS PER PENALTY"; Tab(45);
If K(0, 11) <> 0 Then Print #2, Using "##.#"; K(0, 12) / K(0, 11);
If K(1, 11) <> 0 Then Print #2, Tab(65); Using "##.#"; K(1, 12) / K(1, 11)
If K(1, 11) = 0 Then Print #2,
Print #2, "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
Print #2, Using "##.#"; K(0, 11) / Y%;: Print #2, "/";: Print #2, Using "##.#"; K(0, 12) / Y%;
Print #2, Tab(60); Using "##.#"; K(1, 11) / Y%;: Print #2, "/";: Print #2, Using "##.#"; K(1, 12) / Y%
Print #2, "SAFETIES"; Tab(45);: Print #2, Using "####"; Z(21);: Print #2, Tab(65); Using "####"; Z1(21)
Print #2, "TIMES SACKED / YARDS LOST"; Tab(41);
Print #2, Using "###"; W6(0, 0);: Print #2, "/";: Print #2, Using "####"; W6(0, 1);
Print #2, Tab(61); Using "###"; W6(1, 0);: Print #2, "/";: Print #2, Using "####"; W6(1, 1)
Print #2, Chr$(12)
Print #2, "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"
I = 0
GoSub 8699
For I1 = 0 To 13
    If K2(I, I1, 0) = 0 Then 5560
    If I1 <= 9 Then Print #2, A1$(I, I1);
    If I1 > 9 Then Print #2, A3$(I, I1 - 10);
    Print #2, Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 0); K2(I, I1, 1); K2(I, I1, 1) / K2(I, I1, 0); K2(I, I1, 1) / Y%; K2(I, I1, 2);
    If I1 > 9 Then Print #2,: GoTo 5560
    Print #2, Using "###"; LR(I1);: Print #2, LR$(I1, 0); Tab(64); LR$(I1, 1)

5560 Next I1
Print #2, A$(0); Tab(18);
If G9 = 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K(0, 1); K(0, 2); K(0, 2) / K(0, 1); K(0, 2) / Y%; Z(15)
If G9 <> 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K(0, 1) - W6(0, 0); K(0, 2) + W6(0, 1); (K(0, 2) + W6(0, 1)) / (K(0, 1) - W6(0, 0)); (K(0, 2) + W6(0, 1)) / Y%; Z(15)
Print #2, "OPPONENT TOTALS"; Tab(18);
If G9 = 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K(1, 1); K(1, 2); K(1, 2) / K(1, 1); K(1, 2) / Y%; Z1(15)
If G9 <> 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K(1, 1) - W6(1, 0); K(1, 2) + W6(1, 1); (K(1, 2) + W6(1, 1)) / (K(1, 1) - W6(1, 0)); (K(1, 2) + W6(1, 1)) / Y%; Z1(15)
Print #2,
Print #2, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"
I = 0
For I1 = 0 To 5
    If K2(I, I1, 6) = 0 Then 5562
    Print #2, A2$(I, I1); Tab(18);
    Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 6); K2(I, I1, 7); K2(I, I1, 7) / K2(I, I1, 6); K2(I, I1, 7) / Y%; K2(I, I1, 8);
    Print #2, Using "###"; LC(I1 + 10);: Print #2, LC$(I1 + 10, 0); Tab(64); LC$(I1 + 10, 1)

5562 Next I1
For I1 = 0 To 9
    If K2(I, I1, 3) = 0 Then 5578
    Print #2, A1$(I, I1); Tab(18);
    Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 3); K2(I, I1, 4); K2(I, I1, 4) / K2(I, I1, 3); K2(I, I1, 4) / Y%; K2(I, I1, 5);
    Print #2, Using "###"; LC(I1);: Print #2, LC$(I1, 0); Tab(64); LC$(I1, 1)

5578 Next I1
Print #2, A$(0); Tab(18);
Print #2, Using "####  ####   ##.#      ###.#    ##"; K(0, 4); K(0, 6); K(0, 6) / K(0, 4); K(0, 6) / Y%; Z(17)
Print #2, "OPPONENT TOTALS"; Tab(18);
Print #2, Using "####  ####   ##.#      ###.#    ##"; K(1, 4); K(1, 6); K(1, 6) / K(1, 4); K(1, 6) / Y%; Z1(17)
Print #2,
Print #2, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"
For I1 = 0 To 3
    If K2(I, I1, 9) = 0 Then 5588
    Print #2, A3$(I, I1); Tab(18); Using "####  ### ###.# #### ##.#"; K2(I, I1, 9); K2(I, I1, 10); K2(I, I1, 10) / K2(I, I1, 9) * 100; K2(I, I1, 12); K2(I, I1, 12) / K2(I, I1, 9);
    If K2(I, I1, 10) = 0 Then Print #2, "   --"; Else Print #2, Using " ##.#"; K2(I, I1, 12) / K2(I, I1, 10);
    Print #2, Using " ###.# ### ### ###.#     "; K2(I, I1, 12) / Y%; K2(I, I1, 13); K2(I, I1, 11); K2(I, I1, 11) / K2(I, I1, 9) * 100;
    If G9 = 0 Then GoSub 2500: Print #2, Using "###.#"; RT
    If G9 = 1 Then GoSub 2000: Print #2, Using "###.#"; RT

5588 Next I1
Print #2, A$(0); Tab(18);
Print #2, Using "####  ### ###.# #### ##.#"; K(0, 3); K(0, 4); K(0, 4) / K(0, 3) * 100; K(0, 6); K(0, 6) / K(0, 3);
Print #2, Using " ##.#"; K(0, 6) / K(0, 4);
Print #2, Using " ###.# ### ### ###.#     "; K(0, 6) / Y%; Z(17); K(0, 5); K(0, 5) / K(0, 3) * 100
Print #2, "OPPONENT TOTALS"; Tab(18);
Print #2, Using "####  ### ###.# #### ##.#"; K(1, 3); K(1, 4); K(1, 4) / K(1, 3) * 100; K(1, 6); K(1, 6) / K(1, 3);
Print #2, Using " ##.#"; K(1, 6) / K(1, 4);
Print #2, Using " ###.# ### ### ###.#     "; K(1, 6) / Y%; Z1(17); K(1, 5); K(1, 5) / K(1, 3) * 100
Print #2,
Print #2,
Print #2, "PASSING           LONG PASS    AGAINST"
For I1 = 0 To 3
    If K2(I, I1, 9) = 0 Then 5589
    Print #2, A3$(I, I1); Tab(22); Using "###"; LP(I1);: Print #2, LP$(I1, 0); Tab(32); LP$(I1, 1)

5589 Next
Print #2, Chr$(12)
Print #2, "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
I = 0
For I1 = 0 To 2
    If K2(I, I1, 14) = 0 Then 5593
    Print #2, A4$(I, I1); Tab(18);
    Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
    Print #2, Using "###"; LK(I1);: Print #2, LK$(I1, 0); Tab(64); LK$(I1, 1)

5593 Next I1
Print #2, A$(0); Tab(18);
If Z(22) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z(22); Z(23); Z(23) / Z(22); Z(23) / Y%; Z(18)
Print #2, "OPPONENT TOTALS"; Tab(18);
If Z1(22) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z1(22); Z1(23); Z1(23) / Z1(22); Z1(23) / Y%; Z1(18)
Print #2,
Print #2,
Print #2,
Print #2, "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
I = 0
For I1 = 3 To 5
    If K2(I, I1, 14) = 0 Then 5610
    Print #2, A5$(I, I1 - 3); Tab(18);
    Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2(I, I1, 14); K2(I, I1, 15); K2(I, I1, 15) / K2(I, I1, 14); K2(I, I1, 15) / Y%; K2(I, I1, 16);
    Print #2, Using "###"; PK(I1 - 3);: Print #2, PK$(I1 - 3, 0); Tab(64); PK$(I1 - 3, 1)

5610 Next I1
Print #2, A$(0); Tab(18);
If Z(24) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z(24); Z(25); Z(25) / Z(24); Z(25) / Y%; Z(19)
Print #2, "OPPONENT TOTALS"; Tab(18);
If Z1(24) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z1(24); Z1(25); Z1(25) / Z1(24); Z1(25) / Y%; Z1(19)
Print #2,
I = 0
Print #2, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
Print #2, A6$(I, 0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); K(0, 7) / Y%; Z(31); Z(30); Z(28)
Print #2, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); K(1, 7) / Y%; Z1(31); Z1(30)
Print #2,
Print #2,
Print #2,
Print #2, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
Print #2, A$(0); Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(0, 7); K(0, 8); K(0, 8) / K(0, 7); Z1(24); Z1(25); Z1(25) / Z1(24); (K(0, 8) - Z1(25)) / K(0, 7)
Print #2, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K(1, 7); K(1, 8); K(1, 8) / K(1, 7); Z(24); Z(25); Z(25) / Z(24); (K(1, 8) - Z(25)) / K(1, 7)
Print #2,
Print #2,
Print #2,
Print #2, "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
If K2(I, 4, 17) = 0 And K2(I, 6, 17) = 0 Then 5685
Print #2, A7$(I, 0);
Print #2, Tab(16);
If K2(I, 6, 17) <> 0 Then Print #2, Using "### ### ###.# "; K2(I, 5, 17); K2(I, 6, 17); K2(I, 5, 17) / K2(I, 6, 17) * 100;
If K2(I, 4, 17) = 0 Then Print #2,: GoTo 5685
Print #2, Tab(30);
Print #2, Using "### ### ###.# ## ##"; K2(I, 3, 17); K2(I, 4, 17); K2(I, 3, 17) / K2(I, 4, 17) * 100; LF(0); AM(0, 0);
Print #2, "-";: Print #2, Using "## ##"; AF(0, 0); AM(0, 1);
Print #2, "-";: Print #2, Using "## ##"; AF(0, 1); AM(0, 2);
Print #2, "-";: Print #2, Using "## ##"; AF(0, 2); AM(0, 3);
Print #2, "-";: Print #2, Using "## ##"; AF(0, 3); AM(0, 4);
Print #2, "-";: Print #2, Using "##"; AF(0, 4)

5685 If K2(I, 8, 17) = 0 And K2(I, 10, 17) = 0 Then 5594
Print #2, A7$(I, 1); Tab(16);
If K2(I, 10, 17) <> 0 Then Print #2, Using "### ### ###.# "; K2(I, 9, 17); K2(I, 10, 17); K2(I, 9, 17) / K2(I, 10, 17) * 100;
If K2(I, 8, 17) = 0 Then Print #2,: GoTo 5594
Print #2, Tab(30);
Print #2, Using "### ### ###.# ## ##"; K2(I, 7, 17); K2(I, 8, 17); K2(I, 7, 17) / K2(I, 8, 17) * 100; LF(1); AM(1, 0);
Print #2, "-";: Print #2, Using "## ##"; AF(1, 0); AM(1, 1);
Print #2, "-";: Print #2, Using "## ##"; AF(1, 1); AM(1, 2);
Print #2, "-";: Print #2, Using "## ##"; AF(1, 2); AM(1, 3);
Print #2, "-";: Print #2, Using "## ##"; AF(1, 3); AM(1, 4);
Print #2, "-";: Print #2, Using "##"; AF(1, 4)

5594 Print #2,
Print #2,
Print #2, "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
I = 0
For I1 = 0 To 9
    If GI(I, I1, 0) = 0 Then 5628
    Print #2, DI$(I, I1); Tab(18);
    Print #2, Using "####  ####   ##.#      ###.#    ##    "; GI(I, I1, 0); GI(I, I1, 1); GI(I, I1, 1) / GI(I, I1, 0); GI(I, I1, 1) / Y%; GI(I, I1, 2);
    Print #2, Using "###"; LI(I1);: Print #2, LI$(I1, 0); Tab(64); LI$(I1, 1)

5628 Next I1
Print #2,
Print #2, "DEFENSIVE SACKS    SACKS      YARDS LOST"
For I1 = 0 To 14
    If GS(I, I1, 0) = 0 Then 5627
    Print #2, DS$(I, I1); Tab(20); Using "###            ###"; GS(I, I1, 0); GS(I, I1, 1)

5627 Next
Close 2
Open "REPORT" For Input As #2
Do While Not EOF(2)
    Line Input #2, X$
    LPrint X$
Loop
Close 2
LPrint Chr$(12)
GoTo 8624

3056 Color 15, 0: Cls
JJ = 6
SN = 1: Locate 6, 8: Color 15, 4
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
If I$ = Chr$(27) Then Z$ = "END": GoTo 90
If I$ = Chr$(13) Then GoSub 1102: GoTo 3060
Locate 7, 27: Print I$
Locate 7, 28: Line Input NY$
NY$ = I$ + NY$
Color 15, 0
If NY$ = "" Then GoSub 1102: GoTo 3060
YN$ = NY$
ERASE teamNames$
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
    teamNames$(NTMS) = Q$(0): teamIndex%(NTMS) = I1

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
20 MAMenu teamNames$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NTMS Then Start% = NTMS
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T1 = teamIndex%(SELECTION%)
Open D$(0) + "FOOT." + YN$ For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Get #1, T1
A$ = Q$(1)
For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1
Close 1

9005 If Right$(A$, 1) = " " Then A$ = Left$(A$, Len(A$) - 1): GoTo 9005
Return

6585 For XX = 4 To 11: If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(B$, XX, 1) = "_" Then QQ = XX - 4: GoTo 6615
    GoTo 6620

6615 Next XX

6620 Return

2000 '*** NFL QB RATING ***
QB(I1, 8) = Int(K2(I, I1, 13) / K2(I, I1, 9) * 1000) / 10: QB(I1, 6) = K2(I, I1, 10) / K2(I, I1, 9) * 100
QB(I1, 10) = Int(K2(I, I1, 12) / K2(I, I1, 9) * 100) / 100
QB(I1, 9) = Int(K2(I, I1, 11) / K2(I, I1, 9) * 1000) / 10
PP(I) = QB(I1, 6) - 30
If PP(I) < 0 Then PP(I) = 0: GoTo 2009
If PP(I) > 77.5 Then PP(I) = 2.375: GoTo 2009
PP(I) = PP(I) * .05

2009 If QB(I1, 8) > 11.9 Then TT(I) = 2.375: GoTo 2011
TT(I) = QB(I1, 8) * .2

2011 If QB(I1, 9) = 0 Then II(I) = 2.375: GoTo 2014
If QB(I1, 9) > 9.5 Then II(I) = 0: GoTo 2014
II(I) = 2.375 - (.25 * QB(I1, 9))

2014 QB(I1, 10) = QB(I1, 10) - 3
If QB(I1, 10) < 0 Then YY(I) = 0: GoTo 2020
If QB(I1, 10) > 12.5 Then YY(I) = 2.375: GoTo 2020
D = Int(QB(I1, 10) * 100) / 2
If D = QB(I1, 10) / 2 Then YY(I) = QB(I1, 10) / 2 * .3 + QB(I1, 10) / 2 * .2: GoTo 2020
YY(I) = QB(I1, 10) / 2 * .3 + QB(I1, 10) / 2 * .2 + .003

2020 QB(I1, 7) = PP(I) + TT(I) + II(I) + YY(I)
If QB(I1, 7) >= 9.5 Then QB(I1, 7) = 158.3: GoTo 2027
QB(I1, 7) = QB(I1, 7) * 50 / 3

2027 QB(I1, 7) = Int(QB(I1, 7) * 10) / 10: RT = QB(I1, 7)
Return

2075 '
Locate 8, 8: Color 15, 1
'Print box top
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

'Print box sides
For II = 1 To J
Locate 8 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next

'Print box bottom
Locate 9 + J, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

Locate 7, 32
Locate 10, 15
Return

2500 '*** COLLEGE QB RATING ***
QB(I1, 7) = Int(K2(I, I1, 12) / K2(I, I1, 9)) * 8.399999 + K2(I, I1, 10) / K2(I, I1, 9) * 100 + Int(K2(I, I1, 13) / K2(I, I1, 9) * 1000) / 10 * 3.3 - Int(K2(I, I1, 11) / K2(I, I1, 9) * 1000) / 10 * 2
QB(I1, 7) = Int(QB(I1, 7) * 10) / 10: RT = QB(I1, 7)
Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 2
    Locate 1, 1: Print Tab(80);: Locate , 26: Color 0
    Print "3 IN 1 FOOTBALL STATISTICS"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1991"; Tab(70); Left$(Time$, 5);
    Locate 4, 1: Color 2, 0
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
