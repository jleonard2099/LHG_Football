'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Trade Players"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DECLARE SUB TITLE ()

'Dim A(2, 7), A1(2, 7), A2%(2, 15), A3(2, 7), A4(2, 6), A5(2, 6), A6(2, 3), A7(2, 3), A8(2, 3), A9(2, 3)
'Dim B(2, 3), B1(2, 3), B2(2, 3), B3(2, 3), B6(2, 7), Q6%(2, 1)
'Dim K(2, 12), K1(20, 5), K2(2, 10, 17), V4%(2, 2)
Dim C2(15), GS%(1, 14, 1), LC(2, 15)
'Dim T1(1), K3(2, 6), R9%(1), RB(2, 11, 11), WR(2, 11, 11), QB(2, 11, 11), KR(2, 11, 11), PR(2, 11, 11)
Dim Q$(300)
Dim A$(1), A1$(2, 9), A2$(2, 6), A3$(2, 4), A4$(2, 3), A5$(2, 3), A6$(2, 3), teamItems$(500), V%(500)
'Dim TT(2, 15), OT(2, 15)
Dim DS$(1, 14), DS%(1, 14), Z2(1, 13, 17), Z(1, 38), Z1(1, 38)
Dim D$(1), YN$(1), NY$(1), menuItems$(4), SA$(1), TN$(1), RB$(1, 9), WR$(1, 5)
Dim QB$(1, 3), KR$(1, 2), PR$(1, 2), PU$(1), FG$(1, 1), DI$(1, 9), LR(1, 9)
Dim LR$(1, 9, 1), LC$(1, 15, 1), LP$(1, 3, 1), F$(1), GI%(1, 9, 2)
Dim LK(1, 2), LK$(1, 2, 1), PK(1, 2), PK$(1, 2, 1), AF(1, 1, 4), AM(1, 1, 4)
Dim LI(1, 9), LI$(1, 9, 1), PA%(1, 4), YC%(1), LP(1, 3)
Dim LF(1, 1), LF$(1, 1), A0%(1, 9), A1%(1, 9), A3%(1, 9), B6%(1, 7), AA$(1), T(1), YC(1)
Dim A4%(1, 5), A5%(1, 5), A6%(1, 3), A7%(1, 3), A8%(1, 3), QX%(1, 3)
Dim A9%(1, 2), B%(1, 2), B1%(1, 2), B2%(1, 2), B3%(1, 3), DI%(1, 9)
Dim B4%(1, 1), B5%(1, 1), B7%(1, 1), B8%(1, 1), A7$(1, 1), S(1)

Open "DEV.BAS" For Input As #1
Input #1, D$(0): Input #1, D$(1)
Close #1

99 Call TITLE
menuItems$(1) = "TRADE PLAYERS            "
menuItems$(2) = "RETURN TO 3 & 1 MAIN MENU"
SELECTION% = 1
Count% = 2
Start% = 1
SCANCODE% = 2
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25
    
Call box0(7, 24, 10, 50, 2, 31)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Or SELECTION% = 2 Then 1
If SELECTION% = 1 Then 2110

1 Run "HELLO.EXE 2"

100 For XX = 4 To 11: If Mid$(SA$(J), XX, 1) = " " Or Mid$(SA$(J), XX, 1) = "/" Or Mid$(SA$(J), XX, 1) = "." Then Mid$(SA$(J), XX, 1) = "_"
Next XX
For XX = 12 To 14: If Mid$(SA$(J), XX, 2) = "ST" Then Mid$(SA$(J), 9, 1) = "_": Mid$(SA$(J), 10, 2) = "ST"
Next XX
QQ = 8: For XX = 11 To 4 Step -1: If Mid$(SA$(J), XX, 1) = "_" Then QQ = XX - 4: GoTo 106
    GoTo 107

106 Next XX

107 YR$ = Left$(SA$(J), 2): F$(J) = Mid$(SA$(J), 4, QQ) + "." + YR$
If Not _FileExists(D$(1) + F$(J)) Then S(J) = 1: GoTo 108
Open D$(1) + F$(J) For Input As #1
Input #1, TN$(J)
For I = 0 To 9: Input #1, RB$(J, I): Next
For I = 0 To 5: Input #1, WR$(J, I): Next
For I = 0 To 3: Input #1, QB$(J, I): Next
For I = 0 To 2: Input #1, KR$(J, I): Next
For I = 0 To 2: Input #1, PR$(J, I): Next
Input #1, PU$(J)
For I = 0 To 1: Input #1, FG$(J, I): Next
For X = 0 To 13
    For Y = 0 To 17: Input #1, Z2(J, X, Y)
Next: Next
For X = 0 To 38: Input #1, Z(J, X): Next
For X = 0 To 38: Input #1, Z1(J, X): Next
For X = 0 To 9: Input #1, DI$(J, X): For J1 = 0 To 2: Input #1, GI%(J, X, J1): Next: Next
For X = 0 To 14: Input #1, DS$(J, X): For J1 = 0 To 1: Input #1, GS%(J, X, J1): Next: Next
For I = 0 To 9: Input #1, LR(J, I): For J1 = 0 To 1: Input #1, LR$(J, I, J1): Next: Next
For I = 0 To 15: Input #1, LC(J, I): For J1 = 0 To 1: Input #1, LC$(J, I, J1): Next: Next
For I = 0 To 3: Input #1, LP(J, I): For J1 = 0 To 1: Input #1, LP$(J, I, J1): Next: Next
For I = 0 To 2: Input #1, LK(J, I): For J1 = 0 To 1: Input #1, LK$(J, I, J1): Next: Next
For I = 0 To 2: Input #1, PK(J, I): For J1 = 0 To 1: Input #1, PK$(J, I, J1): Next: Next
For I = 0 To 1: Input #1, LF(J, I): Input #1, LF$(J, I): For I1 = 0 To 4: Input #1, AM(J, I, I1): Input #1, AF(J, I, I1): Next: Next
For I = 0 To 9: Input #1, LI(J, I): Input #1, LI$(J, I, 0): Input #1, LI$(J, I, 1): Next
Close #1

108 Return

330 Locate 12, 1
Print "SAVING ";: Color PA%(J, 2), PA%(J, 1)
Print A$(J);
Color 7, 0: Print " STATS": Z = 0
Open D$(1) + F$(J) For Output As #1
Print #1, TN$(J)
For I = 0 To 9: Print #1, RB$(J, I): Next
For I = 0 To 5: Print #1, WR$(J, I): Next
For I = 0 To 3: Print #1, QB$(J, I): Next
For I = 0 To 2: Print #1, KR$(J, I): Next
For I = 0 To 2: Print #1, PR$(J, I): Next
Print #1, PU$(J)
For I = 0 To 1: Print #1, FG$(J, I): Next
For X = 0 To 13
    For Y = 0 To 17: Print #1, Z2(J, X, Y)
Next: Next
For X = 0 To 38: Print #1, Z(J, X): Next
For X = 0 To 38: Print #1, Z1(J, X): Next
For X = 0 To 9: Print #1, DI$(J, X): For J1 = 0 To 2: Print #1, GI%(J, X, J1): Next: Next
For X = 0 To 14: Print #1, DS$(J, X): For J1 = 0 To 1: Print #1, GS%(J, X, J1): Next: Next
For I = 0 To 9: Print #1, LR(J, I): For J1 = 0 To 1: Print #1, LR$(J, I, J1): Next: Next
For I = 0 To 15: Print #1, LC(J, I): For J1 = 0 To 1: Print #1, LC$(J, I, J1): Next: Next
For I = 0 To 3: Print #1, LP(J, I): For J1 = 0 To 1: Print #1, LP$(J, I, J1): Next: Next
For I = 0 To 2: Print #1, LK(J, I): For J1 = 0 To 1: Print #1, LK$(J, I, J1): Next: Next
For I = 0 To 2: Print #1, PK(J, I): For J1 = 0 To 1: Print #1, PK$(J, I, J1): Next: Next
For I = 0 To 1: Print #1, LF(J, I): Print #1, LF$(J, I): For I1 = 0 To 4: Print #1, AM(J, I, I1): Print #1, AF(J, I, I1): Next: Next
For I = 0 To 9: Print #1, LI(J, I): Print #1, LI$(J, I, 0): Print #1, LI$(J, I, 1): Next
Close #1
Return

1000 For I = 0 To 9
    A0%(J, I) = 0: A1%(J, I) = 0: A2%(J, I) = 0: A3%(J, I) = 0: A1$(J, I) = ""
Next
For I = 0 To 5
    A4%(J, I) = 0: A5%(J, I) = 0: A2$(J, I) = ""
Next
For I = 0 To 3
    A6%(J, I) = 0: A7%(J, I) = 0: A8%(J, I) = 0: QX%(J, I) = 0: A3$(J, I) = ""
Next
For I = 0 To 2
    A9%(J, I) = 0: B%(J, I) = 0: B1%(J, I) = 0: B2%(J, I) = 0: A4$(J, I) = "": A5$(J, I) = ""
Next
A6$(J, 0) = "": B3%(J, I) = 0
For I = 0 To 1
    B4%(J, I) = 0: B5%(J, I) = 0: B7%(J, I) = 0: B8%(J, I) = 0: A7$(J, I) = ""
Next
Return

1100 NT = 0
Open D$(0) + "FOOT." + YN$(J) For Random As #1 Len = 934
Y = LOF(1)
Field #1, 15 As Q$(0), 640 As X$
For I1 = 1 To Y / 934
    Get #1, I1
    For I2 = 1 To 15
        If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1105
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1

1105 Next I1
Close 1
TS% = Int((NT - 1) / 80)

1102 SELECTION% = 1
Count% = NT
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
20 MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then BO% = 1: Return
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 20
If EXITCODE% = -79 Then Start% = NT: EXITCODE% = 3: SELECTION% = NT: GoTo 20
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > NT Then Start% = NT
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 20
T(J) = V%(SELECTION%)
GoTo 9500

2110 GoSub 9900

2111 J = 0
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID FOR TEAM A: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 38: Input NY$(J)
If NY$(J) = "" Then GoSub 1102: GoTo 2112
YN$(J) = NY$(J)
For I = 1 To 500: teamItems$(I) = "": Next
GoSub 1100

2112 If BO% = 1 Then BO% = 0: GoTo 2111

2113 Locate 2, 33: Color PA%(J, 2), PA%(J, 1): Print A$(J): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2111
Color 15, 0
GoSub 9900
Locate 3: Print "DOES THIS TEAM HAVE COMPILED STATS (YN)": S(0) = 0
2136 I$ = GetKeyPress$
If UCase$(I$) = "N" Then S(0) = 1: GoTo 2150

2145 GoSub 100

2150 J = 1
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID FOR TEAM B: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 38: Input NY$(J)
If NY$(J) = "" Then GoSub 1102: GoTo 2152
YN$(J) = NY$(J)
For I = 1 To 500: teamItems$(I) = "": Next
GoSub 1100

2152 If BO% = 1 Then BO% = 0: GoTo 2150

2153 Locate 2, 33: Color PA%(J, 2), PA%(J, 1): Print A$(J): Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2150
Color 15, 0
GoSub 9900: Locate 3: Print "DOES THIS TEAM HAVE COMPILED STATS (YN)": S(1) = 0
2186 I$ = GetKeyPress$

2195 GoSub 100: J = 1

2200 GoSub 9900: Locate 5
Locate , 5: Color 3: Print "TRADE ";: Color 15: Print "R";: Color 3: Print "UNNING BACKS"
Locate , 5: Color 3: Print "TRADE ";: Color 15: Print "W";: Color 3: Print "IDE RECEIVERS"
Locate , 5: Color 3: Print "TRADE ";: Color 15: Print "Q";: Color 3: Print "UARTERBACKS"
Locate , 5: Color 3: Print "TRADE ";: Color 15: Print "K";: Color 3: Print "ICKOFF RETURN"
Locate , 5: Color 3: Print "TRADE ";: Color 15: Print "P";: Color 3: Print "UNT RETURN"
Locate , 5: Color 3: Print "TRADE P";: Color 15: Print "U";: Color 3: Print "NTERS"
Locate , 5: Color 3: Print "TRADE PLACE K";: Color 15: Print "I";: Color 3: Print "CKERS"
Locate , 5: Color 15: Print "E";: Color 3: Print "ND AND SAVE TRADES"
2202 I$ = GetKeyPress$
If UCase$(I$) = "E" Then 2500
If UCase$(I$) = "R" Then GoSub 2216: GoTo 2222
If UCase$(I$) = "W" Then GoSub 2216: GoTo 2256
If UCase$(I$) = "Q" Then GoSub 2216: GoTo 2290
If UCase$(I$) = "K" Then GoSub 2216: GoTo 2324
If UCase$(I$) = "P" Then GoSub 2216: GoTo 2358
If UCase$(I$) = "U" Then GoSub 2216: GoTo 2392
If UCase$(I$) = "I" Then GoSub 2216: GoTo 2426
GoTo 2202
If S(0) = 1 Or S(1) = 1 Then 2221

2216 GoSub 9900
Locate 5, 5
Color 15: Print "T";: Color 3: Print "RADE PLAYERS' REPLAY STATS"
Locate , 5: Color 15: Print "Z";: Color 3: Print "ERO PLAYERS' STATS"
2217 I$ = GetKeyPress$
If UCase$(I$) <> "T" And UCase$(I$) <> "Z" Then 2216
If UCase$(I$) = "T" Then W1 = 1
If UCase$(I$) = "Z" Then W1 = 0

2221 Return

2222 GoSub 9900:: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
For I = 0 To 9: Locate I + 5, 1: Print I; " "; A1$(0, I): Locate I + 5, 20: Print I; " "; A1$(1, I)
Next I

2225 Locate 17, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 7, 0: Print " PLAYER: ";: Input I$
I = Val(I$): If I < 0 Or I > 9 Then 2225
Print A1$(0, I)

2228 Locate 19, 1: Color PA%(1, 2), PA%(1, 1): Print A$(1);: Color 7, 0: Print " PLAYER: ";: Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 9 Then 2228
Print A1$(1, I1)
Locate 23, 1: Print "ANY CHANGE (YN) ";
2232 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2225
GoSub 2234: GoTo 2237

2234 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A1$(0, I);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A1$(1, I1): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "       TC    ###                                 TC    ###"; Z2(0, I, 0); Z2(1, I1, 0)
Locate , 5: Print Using "      YDS   ####                                YDS   ####"; Z2(0, I, 1); Z2(1, I1, 1)
Locate , 5: Print Using " ##.# AVG   "; A1%(0, I) * .3;
If Z2(0, I, 0) > 0 Then Print Using "##.#"; Z2(0, I, 1) / Z2(0, I, 0); Else Print " -.-";
Print Using "                           ##.# AVG   "; A1%(1, I1) * .3;
If Z2(1, I1, 0) > 0 Then Print Using "##.#"; Z2(1, I1, 1) / Z2(1, I1, 0) Else Print " -.-"
Locate , 5: Print Using "      TDS     ##                                TDS     ##"; Z2(0, I, 2); Z2(1, I1, 2)
Print
Locate , 5: Print Using "       RC     ##                                 RC     ##"; Z2(0, I, 3); Z2(1, I1, 3)
Locate , 5: Print Using "      YDS   ####                                YDS   ####"; Z2(0, I, 4); Z2(1, I1, 4)
Locate , 5: Print Using " ##.# AVG   "; A3%(0, I) * 3;
If Z2(0, I, 3) > 0 Then Print Using "##.#"; Z2(0, I, 4) / Z2(0, I, 3); Else Print " -.-";
Print Using "                           ##.# AVG   "; A3%(1, I1) * 3;
If Z2(1, I1, 3) > 0 Then Print Using "##.#"; Z2(1, I1, 4) / Z2(1, I1, 3) Else Print " -.-"
Locate , 5: Print Using "      TDS     ##                                TDS     ##"; Z2(0, I, 5); Z2(1, I1, 5)
2212 I$ = GetKeyPress$
Return

2237 Swap A1$(0, I), A1$(1, I1)
Swap A0%(0, I), A0%(1, I1)
Swap LR(0, I), LR(1, I1)
Swap LR$(0, I, 0), LR$(1, I1, 0)
Swap LR$(0, I, 1), LR$(1, I1, 1)
Swap LC(0, I), LC(1, I1)
Swap LC$(0, I, 0), LC$(1, I1, 0)
Swap LC$(0, I, 1), LC$(1, I1, 1)
Swap A1%(0, I), A1%(1, I1)
Swap A2%(0, I), A2%(1, I1)
Swap A3%(0, I), A3%(1, I1)
Swap RB$(0, I), RB$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2244
For I2 = 0 To 5: Swap Z2(0, I, I2), Z2(1, I1, I2): Next
GoSub 2234
GoTo 2200

2244 For I2 = 0 To 5: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: Next I2: GoSub 2234
GoTo 2200

2256 GoSub 9900: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
For I = 0 To 5: Locate I + 5, 1: Print I; " "; A2$(0, I): Locate I + 5, 20: Print I; " "; A2$(1, I)
Next I

2259 Locate 17, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 7, 0: Print " PLAYER: ";: Input I$
I = Val(I$): If I < 0 Or I > 5 Then 2259
Print A2$(0, I)

2262 Locate 19, 1: Color PA%(1, 2), PA%(1, 1): Print A$(1);: Color 7, 0: Print " PLAYER: ";: Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 5 Then 2262
Print A2$(1, I1)
Locate 23, 1: Print "ANY CHANGE (YN) ";
2266 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2259
GoSub 2268: GoTo 2270

2268 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A2$(0, I);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A2$(1, I1): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "       RC     ##                                 RC     ##"; Z2(0, I, 6); Z2(1, I1, 6)
Locate , 5: Print Using "      YDS   ####                                YDS   ####"; Z2(0, I, 7); Z2(1, I1, 7)
Locate , 5: Print Using " ##.# AVG   "; A5%(0, I) * 3;
If Z2(0, I, 6) > 0 Then Print Using "##.#"; Z2(0, I, 7) / Z2(0, I, 6); Else Print " -.-";
Print Using "                           ##.# AVG   "; A5%(1, I1) * 3;
If Z2(1, I1, 6) > 0 Then Print Using "##.#"; Z2(1, I1, 7) / Z2(1, I1, 6) Else Print " -.-"
Locate , 5: Print Using "      TDS     ##                                TDS     ##"; Z2(0, I, 8); Z2(1, I1, 8)
2269 I$ = GetKeyPress$
Return

2270 Swap A2$(0, I), A2$(1, I1)
Swap LC(0, I + 10), LC(1, I1 + 10)
Swap LC$(0, I + 10, 0), LC$(1, I1 + 10, 0)
Swap LC$(0, I + 10, 1), LC$(1, I1 + 10, 1)
Swap A4%(0, I), A4%(1, I1)
Swap A5%(0, I), A5%(1, I1)
Swap WR$(0, I), WR$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2278
For I2 = 6 To 8: Swap Z2(0, I, I2), Z2(1, I1, I2): Next
GoSub 2268: GoTo 2200

2278 For I2 = 6 To 8: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: Next I2: GoSub 2268
GoTo 2200

2290 GoSub 9900: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
For I = 0 To 3: Locate I + 5, 1: Print I; " "; A3$(0, I): Locate I + 5, 20: Print I; " "; A3$(1, I)
Next I

2293 Locate 17, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 7, 0: Print " PLAYER: ";: Input I$
I = Val(I$): If I < 0 Or I > 3 Then 2293
Print A3$(0, I)

2296 Locate 19, 1: Color PA%(1, 2), PA%(1, 1): Print A$(1);: Color 7, 0: Print " PLAYER: ";: Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 3 Then 2296
Print A3$(1, I1)
Locate 23, 1: Print "ANY CHANGE (YN) ";
2300 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2293
GoSub 2302: GoTo 2304

2302 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A3$(0, I);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A3$(1, I1): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "       PC    ###                                 PC    ###"; Z2(0, I, 10); Z2(1, I1, 10)
Locate , 5: Print Using "       PA   ####                                 PA   ####"; Z2(0, I, 9); Z2(1, I1, 9)
Locate , 5: Print Using "       PI     ##                                 PI     ##"; Z2(0, I, 11); Z2(1, I1, 11)
Locate , 5: Print Using "      TDS     ##                                TDS     ##"; Z2(0, I, 13); Z2(1, I1, 13)
Locate , 5: Print Using "      YDS   ####                                YDS   ####"; Z2(0, I, 12); Z2(1, I1, 12)
Locate , 5: Print Using " ##.# CMP%  "; A7%(0, I);
If Z2(0, I, 9) > 0 Then Print Using "##.#"; Z2(0, I, 10) / Z2(0, I, 9) * 100; Else Print " -.-";
Print Using "                           ##.# CMP%  "; A7%(1, I1);
If Z2(1, I1, 9) > 0 Then Print Using "##.#"; Z2(1, I1, 10) / Z2(1, I1, 9) * 100 Else Print " -.-"
Locate , 5: Print Using " ##.# INT%  "; A8%(0, I);
If Z2(0, I, 9) > 0 Then Print Using "##.#"; Z2(0, I, 11) / Z2(0, I, 9) * 100; Else Print " -.-";
Print Using "                           ##.# INT%  "; A8%(1, I1);
If Z2(1, I1, 9) > 0 Then Print Using "##.#"; Z2(1, I1, 11) / Z2(1, I1, 9) * 100 Else Print " -.-"
2303 I$ = GetKeyPress$
Return

2304 Swap A3$(0, I), A3$(1, I1)
Swap LP(0, I), LP(1, I1)
Swap LP$(0, I, 0), LP$(1, I1, 0)
Swap LP$(0, I, 1), LP$(1, I1, 1)
Swap A6%(0, I), A6%(1, I1)
Swap A7%(0, I), A7%(1, I1)
Swap A8%(0, I), A8%(1, I1): Swap QX%(0, I), QX%(1, I1)
Swap QB$(0, I), QB$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2312
For I2 = 9 To 13: Swap Z2(0, I, I2), Z2(1, I1, I2): Next I2
For I2 = 0 To 2: Swap Z2(0, I + 10, I2), Z2(1, I1 + 10, I2): Next I2
GoSub 2302: GoTo 2200

2312 For I2 = 9 To 13: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: Next I2
For I2 = 0 To 2: Z2(0, I + 10, I2) = 0: Z2(1, I1 + 10, I2) = 0: Next
GoSub 2302: GoTo 2200

2324 GoSub 9900: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
For I = 0 To 2: Locate I + 5, 1: Print I; " "; A4$(0, I): Locate I + 5, 20: Print I; " "; A4$(1, I)
Next I

2327 Locate 17, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 7, 0: Print " PLAYER: ";: Input I$
I = Val(I$): If I < 0 Or I > 2 Then 2327
Print A4$(0, I)

2328 Locate 19, 1: Color PA%(1, 2), PA%(1, 1): Print A$(1);: Color 7, 0: Print " PLAYER: ";: Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 2 Then 2328
Print A4$(1, I1)
Locate 23, 1: Print "ANY CHANGE (YN) ";
2334 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2327
GoSub 2336: GoTo 2338

2336 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A4$(0, I);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A4$(1, I1): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "      RET     ##                                RET     ##"; Z2(0, I, 14); Z2(1, I1, 14)
Locate , 5: Print Using "      YDS   ####                                YDS   ####"; Z2(0, I, 15); Z2(1, I1, 15)
Locate , 5: Print Using " ##.# AVG   "; B%(0, I) * 3;
If Z2(0, I, 14) > 0 Then Print Using "##.#"; Z2(0, I, 15) / Z2(0, I, 14); Else Print " -.-";
Print Using "                           ##.# AVG   "; B%(1, I1) * 3;
If Z2(1, I1, 14) > 0 Then Print Using "##.#"; Z2(1, I1, 15) / Z2(1, I1, 14) Else Print " -.-"
Locate , 5: Print Using "      TDS     ##                                TDS     ##"; Z2(0, I, 16); Z2(1, I1, 16)
2337 I$ = GetKeyPress$
Return

2338 Swap A4$(0, I), A4$(1, I1)
Swap A9%(0, I), A9%(1, I1)
Swap LK(0, I), LK(1, I1)
Swap LK$(0, I, 0), LK$(1, I1, 0)
Swap LK$(0, I, 1), LK$(1, I1, 1)
Swap B%(0, I), B%(1, I1)
Swap KR$(0, I), KR$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2346
For I2 = 14 To 16: Swap Z2(0, I, I2), Z2(1, I1, I2): Next I2
GoSub 2336: GoTo 2200

2346 For I2 = 14 To 16: Z2(0, I, I2) = 0: Z2(1, I1, I2) = 0: Next I2
GoSub 2336: GoTo 2200

2358 GoSub 9900: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
For I = 0 To 2: Locate I + 5, 1: Print I; " "; A5$(0, I): Locate I + 5, 20: Print I; " "; A5$(1, I)
Next I

2361 Locate 17, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 7, 0: Print " PLAYER: ";: Input I$
I = Val(I$): If I < 0 Or I > 2 Then 2361
Print A5$(0, I)

2362 Locate 19, 1: Color PA%(1, 2), PA%(1, 1): Print A$(1);: Color 7, 0: Print " PLAYER: ";: Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 2 Then 2362
Print A5$(1, I1)
Locate 23, 1: Print "ANY CHANGE (YN) ";
2368 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2361
GoSub 2370: GoTo 2372

2370 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A5$(0, I);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A5$(1, I1): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "      RET     ##                                RET     ##"; Z2(0, I + 3, 14); Z2(1, I1 + 3, 14)
Locate , 5: Print Using "      YDS   ####                                YDS   ####"; Z2(0, I + 3, 15); Z2(1, I1 + 3, 15)
Locate , 5: Print Using " ##.# AVG   "; B2%(0, I) * 3;
If Z2(0, I, 14) > 0 Then Print Using "##.#"; Z2(0, I + 3, 15) / Z2(0, I + 3, 14); Else Print " -.-";
Print Using "                           ##.# AVG   "; B2%(1, I1) * 3;
If Z2(1, I1, 14) > 0 Then Print Using "##.#"; Z2(1, I1 + 3, 15) / Z2(1, I1 + 3, 14) Else Print " -.-"
Locate , 5: Print Using "      TDS     ##                                TDS     ##"; Z2(0, I + 3, 16); Z2(1, I1 + 3, 16)
2371 I$ = GetKeyPress$
Return

2372 Swap A5$(0, I), A5$(1, I1)
Swap PK(0, I), PK(1, I1)
Swap PK$(0, I, 0), PK$(1, I1, 0)
Swap PK$(0, I, 1), PK$(1, I1, 1)
Swap B1%(0, I), B1%(1, I1)
Swap B2%(0, I), B2%(1, I1)
Swap PR$(0, I), PR$(1, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2380
For I2 = 14 To 16: Swap Z2(0, I + 3, I2), Z2(1, I1 + 3, I2): Next I2
GoSub 2370: GoTo 2200

2380 For I2 = 14 To 16: Z2(0, I + 3, I2) = 0: Z2(1, I1 + 3, I2) = 0: Next I2
GoSub 2370: GoTo 2200

2392 GoSub 9900: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
Print: Print A6$(0, 0);: Locate , 20: Print " "; A6$(1, 0)
Locate 22, 1: Print "ANY CHANGE (YN) ";
2395 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2200
GoSub 2397: GoTo 2399

2397 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A6$(0, 0);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A6$(1, 0): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "      PNT     ##                                RET     ##"; Z(0, 7); Z(1, 7)
Locate , 5: Print Using " ##.# AVG   "; B3%(0, 0) * 2 + 28;
If Z(0, 7) > 0 Then Print Using "##.#"; Z(0, 8) / Z(0, 7); Else Print " -.-";
Print Using "                           ##.# AVG   "; B3%(1, 0) * 2 + 28;
If Z(1, 7) > 0 Then Print Using "##.#"; Z(1, 8) / Z(1, 7) Else Print " -.-"
2398 I$ = GetKeyPress$
Return

2399 Swap A6$(0, 0), A6$(1, 0)
Swap B3%(0, 0), B3%(1, 0)
Swap PU$(0), PU$(1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2403
Swap Z(0, 7), Z(1, 7): Swap Z(0, 8), Z(1, 8)

2403 GoSub 2397: GoTo 2200

2426 GoSub 9900: Locate 4, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0): Color 7, 0: Locate 4, 20: Color PA%(1, 2), PA%(1, 1): Print A$(1): Color 7, 0
For I = 0 To 1: Locate I + 5, 1: Print I; " "; A7$(0, I): Locate I + 5, 20: Print I; " "; A7$(1, I)
Next I

2427 Locate 17, 1: Color PA%(0, 2), PA%(0, 1): Print A$(0);: Color 7, 0: Print " PLAYER: ";: Input I$
I = Val(I$): If I < 0 Or I > 1 Then 2427
Print A7$(0, I)

2428 Locate 19, 1: Color PA%(1, 2), PA%(1, 1): Print A$(1);: Color 7, 0: Print " PLAYER: ";: Input I$
I1 = Val(I$): If I1 < 0 Or I1 > 1 Then 2428
Print A7$(1, I1)
Locate 22, 1: Print "ANY CHANGE (YN) ";
2429 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2427
GoSub 2431: GoTo 2433

2431 GoSub 9900: Locate 3
Locate , 7: Color PA%(0, 2), PA%(0, 1): Print A7$(0, I);: Color 7, 0: Locate , 49: Color PA%(1, 2), PA%(1, 1): Print A7$(1, I1): Color 7, 0
Print
Locate , 5: Print "ACTUAL     REPLAY                         ACTUAL     REPLAY"
Print
Locate , 5: Print Using "      FGA  ##                                   FGA  ##"; Z2(0, 4 * I + 4, 17); Z2(1, 4 * I1 + 4, 17)
Locate , 5: Print Using "      FGM  ##                                   FGM  ##"; Z2(0, 4 * I + 3, 17); Z2(1, 4 * I1 + 3, 17)
Locate , 5: Print Using " ###  FG% "; B4%(0, I) * 5 + 25;
If Z2(0, 4 * I + 4, 17) > 0 Then Print Using "### "; Z2(0, I * 4 + 3, 17) / Z2(0, I * 4 + 4, 17) * 100; Else Print " -.-";
Print Using "                             ###  FG% "; B4%(1, I1) * 5 + 25;
If Z2(1, 4 * I1 + 4, 17) > 0 Then Print Using "### "; Z2(1, I1 * 4 + 3, 17) / Z2(1, I1 * 4 + 4, 17) * 100 Else Print " -.-"
Locate , 5: Print Using "      EPA  ##                                   EPA  ##"; Z2(0, 4 * I + 6, 17); Z2(1, 4 * I1 + 6, 17)
Locate , 5: Print Using "      EPM  ##                                   EPM  ##"; Z2(0, 4 * I + 5, 17); Z2(1, 4 * I1 + 5, 17)
Locate , 5: Print Using " ###  EP% "; B7%(0, I);
If Z2(0, 4 * I + 6, 17) > 0 Then Print Using "### "; Z2(0, 4 * I + 5, 17) / Z2(0, 4 * I + 6, 17) * 100; Else Print " -.-";
Print Using "                             ###  EP% "; B7%(1, I1);
If Z2(1, 4 * I1 + 6, 17) > 0 Then Print Using "### "; Z2(1, 4 * I1 + 5, 17) / Z2(1, 4 * I1 + 6, 17) * 100 Else Print " -.-"
2432 I$ = GetKeyPress$
Return

2433 Swap A7$(0, I), A7$(1, I1)
Swap B4%(0, I), B4%(1, I1)
Swap LF(0, I), LF(1, I1)
For I2 = 0 To 4
    Swap AM(0, I, I2), AM(1, I1, I2)
    Swap AF(0, I, I2), AF(1, I1, I2)
Next I2
Swap B5%(0, I), B5%(1, I1)
Swap B7%(0, I), B7%(1, I1)
Swap B8%(0, I), B8%(1, I1)
Swap FG$(J, I), FG$(J, I1)
If S(0) = 1 Or S(1) = 1 Or W1 = 0 Then 2437
Swap Z2(0, 4 * I + 3, 17), Z2(1, 4 * I1 + 3, 17)
Swap Z2(0, 4 * I + 4, 17), Z2(1, 4 * I1 + 4, 17)
Swap Z2(0, 4 * I + 5, 17), Z2(1, 4 * I1 + 5, 17)
Swap Z2(0, 4 * I + 6, 17), Z2(1, 4 * I1 + 6, 17)
GoSub 2431: GoTo 2200

2437 Z2(0, 4 * I + 3, 17) = 0: Z2(1, 4 * I1 + 3, 17) = 0
Z2(0, 4 * I + 4, 17) = 0: Z2(1, 4 * I1 + 4, 17) = 0
Z2(0, 4 * I + 5, 17) = 0: Z2(1, 4 * I1 + 5, 17) = 0
Z2(0, 4 * I + 6, 17) = 0: Z2(1, 4 * I1 + 6, 17) = 0
GoSub 2431: GoTo 2200

2500 For J = 0 To 1
    GoSub 9900
    Locate 3

    2505 GoSub 9600
Next J
For J = 0 To 1
    If S(J) = 1 Then 2525
    GoSub 9900

    2520 GoSub 330

2525 Next J
GoTo 99

9400 For I = 0 To 15: C2(I) = 0: Next
For I = 9 To 1 Step -1
    If A0%(J, I) = 0 Then 9403
    A0%(J, I) = A0%(J, I) - A0%(J, I - 1)

9403 Next
For I = 0 To 5: A2%(J, I + 10) = A4%(J, I): Next
U = 0: For I = 1 To 15
    If A2%(J, I) = 0 Then U = U + 1: GoTo 9408
    C2(I) = A2%(J, I) - A2%(J, I - 1 - U): U = 0

9408 Next
For I = 1 To 15: A2%(J, I) = C2(I): Next
For I = 0 To 5: A4%(J, I) = A2%(J, I + 10): Next
For I = 3 To 1 Step -1
    If A6%(J, I) = 0 Then 9414
    A6%(J, I) = A6%(J, I) - A6%(J, I - 1)

9414 Next
For I = 2 To 1 Step -1
    If A9%(J, I) = 0 Then 9418
    A9%(J, I) = A9%(J, I) - A9%(J, I - 1)

9418 Next
For I = 2 To 1 Step -1
    If B1%(J, I) = 0 Then 9422
    B1%(J, I) = B1%(J, I) - B1%(J, I - 1)

9422 Next
For I = 1 To 1 Step -1
    If B5%(J, I) = 0 Then 9426
    B5%(J, I) = B5%(J, I) - B5%(J, I - 1)

9426 Next
For I = 1 To 1 Step -1
    If B8%(J, I) = 0 Then 9430
    B8%(J, I) = B8%(J, I) - B8%(J, I - 1)

9430 Next
Return

9500 Open D$(0) + "FOOT." + YN$(J) For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$
Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$
Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$
Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$
Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$
Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$
Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$
Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)
Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)
Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)
Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)
    FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$
Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)
Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)
Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)
Get #1, T(J)
A$(J) = Q$(1): AA$(J) = Q$(2)
For I = 0 To 7: B6%(J, I) = CVI(Q$(I + 3)): Next I
For I = 0 To 4: PA%(J, I) = CVI(Q$(I + 11)): Next I
For I = 0 To 9
    A1$(J, I) = Q$(I * 5 + 16)
    A0%(J, I) = CVI(Q$(I * 5 + 17))
    A1%(J, I) = CVI(Q$(I * 5 + 18))
    A2%(J, I) = CVI(Q$(I * 5 + 19))
    A3%(J, I) = CVI(Q$(I * 5 + 20))
Next I
For I = 0 To 5
    A2$(J, I) = Q$(I * 3 + 66)
    A4%(J, I) = CVI(Q$(I * 3 + 67))
    A5%(J, I) = CVI(Q$(I * 3 + 68))
Next I
For I = 0 To 3
    A3$(J, I) = Q$(I * 5 + 84)
    A6%(J, I) = CVI(Q$(I * 5 + 85))
    A7%(J, I) = CVI(Q$(I * 5 + 86))
    A8%(J, I) = CVI(Q$(I * 5 + 87))
    QX%(J, I) = CVI(Q$(I * 5 + 88))
Next I
For I = 0 To 2
    A4$(J, I) = Q$(I * 3 + 104)
    A9%(J, I) = CVI(Q$(I * 3 + 105))
    B%(J, I) = CVI(Q$(I * 3 + 106))
Next I
For I = 0 To 2
    A5$(J, I) = Q$(I * 3 + 113)
    B1%(J, I) = CVI(Q$(I * 3 + 114))
    B2%(J, I) = CVI(Q$(I * 3 + 115))
Next I
A6$(J, 0) = Q$(122)
B3%(J, 0) = CVI(Q$(123))
A7$(J, 0) = Q$(124)
B4%(J, 0) = CVI(Q$(125))
B5%(J, 0) = CVI(Q$(126))
B7%(J, 0) = CVI(Q$(127))
B8%(J, 0) = CVI(Q$(128))
A7$(J, 1) = Q$(129)
B4%(J, 1) = CVI(Q$(130))
B5%(J, 1) = CVI(Q$(131))
B7%(J, 1) = CVI(Q$(132))
B8%(J, 1) = CVI(Q$(133))
YC(J) = CVI(Q$(134))
For I = 0 To 14
    DS$(J, I) = Q$(I * 2 + 135)
    DS%(J, I) = CVI(Q$(I * 2 + 136))
Next I
For I = 0 To 9
    DI$(J, I) = Q$(I * 2 + 165)
    DI%(J, I) = CVI(Q$(I * 2 + 166))
Next I

9598 If Right$(A$(J), 1) = " " Then A$(J) = Left$(A$(J), Len(A$(J)) - 1): GoTo 9598
SA$(J) = A$(J)
Close #1: GoTo 9400

9600 Rem
YC%(J) = YC(J)
S0 = 0: S1 = 0: S2 = 0: S3 = 0
For I = 0 To 9: S0 = S0 + A0%(J, I): S2 = S2 + A2%(J, I): Next
S4 = 100 / S0
For I = 0 To 9: If A0%(J, I) <> 0 Then A0%(J, I) = Int(S4 * A0%(J, I) + .5) + S1: S1 = A0%(J, I)
    If A0%(J, I) > 100 Then A0%(J, I) = 100
Next I
For I = 0 To 5: S2 = S2 + A4%(J, I): Next
S5 = 100 / S2
For I = 0 To 9: If A2%(J, I) <> 0 Then A2%(J, I) = Int(S5 * A2%(J, I) + .5) + S3: S3 = A2%(J, I)
    If A2%(J, I) > 100 Then A2%(J, I) = 100
Next I
For I = 0 To 5: If A4%(J, I) <> 0 Then A4%(J, I) = Int(S5 * A4%(J, I) + .5) + S3: S3 = A4%(J, I)
    If A4%(J, I) > 100 Then A4%(J, I) = 100
Next I
S6 = 0: S7 = 0
For I = 0 To 3: S6 = S6 + A6%(J, I): Next
S8 = 100 / S6
For I = 0 To 3: If A6%(J, I) <> 0 Then A6%(J, I) = Int(S8 * A6%(J, I) + .5) + S7: S7 = A6%(J, I)
    If A6%(J, I) > 100 Then A6%(J, I) = 100
Next
G0 = 0: G1 = 0: G3 = 0: G4 = 0
For I = 0 To 2: G0 = G0 + A9%(J, I): G3 = G3 + B1%(J, I): Next
G2 = 100 / G0: G5 = 100 / G3
For I = 0 To 2: If A9%(J, I) <> 0 Then A9%(J, I) = Int(G2 * A9%(J, I) + .5) + G1: G1 = A9%(J, I)
    If A9%(J, I) > 100 Then A9%(J, I) = 100
Next
For I = 0 To 2: If B1%(J, I) <> 0 Then B1%(J, I) = Int(G5 * B1%(J, I) + .5) + G4: G4 = B1%(J, I)
    If B1%(J, I) > 100 Then B1%(J, I) = 100
Next
G0 = 0: G1 = 0: G3 = 0: G4 = 0
For I = 0 To 1: G0 = G0 + B5%(J, I): G3 = G3 + B8%(J, I): Next
G2 = 100 / G0: G5 = 100 / G3
For I = 0 To 1: If B5%(J, I) <> 0 Then B5%(J, I) = Int(G2 * B5%(J, I) + .5) + G1: G1 = B5%(J, I)
    If B5%(J, I) > 100 Then B5%(J, I) = 100
Next
For I = 0 To 1: If B8%(J, I) <> 0 Then B8%(J, I) = Int(G5 * B8%(J, I) + .5) + G4: G4 = B8%(J, I)
    If B8%(J, I) > 100 Then B8%(J, I) = 100
Next
For I = 9 To 0 Step -1: If A0%(J, I) <> 0 Then A0%(J, I) = 100: GoTo 9776
Next

9776 For I = 5 To 0 Step -1: If A4%(J, I) <> 0 Then A4%(J, I) = 100: GoTo 9778
Next

9778 For I = 3 To 0 Step -1: If A6%(J, I) <> 0 Then A6%(J, I) = 100: GoTo 9780
Next

9780 For I = 2 To 0 Step -1: If A9%(J, I) <> 0 Then A9%(J, I) = 100: GoTo 9782
Next

9782 For I = 2 To 0 Step -1: If B1%(J, I) <> 0 Then B1%(J, I) = 100: GoTo 9784
Next

9784 For I = 1 To 0 Step -1: If B5%(J, I) <> 0 Then B5%(J, I) = 100: GoTo 9786
Next

9786 For I = 1 To 0 Step -1: If B8%(J, I) <> 0 Then B8%(J, I) = 100: GoTo 9800
Next

9800 Open D$(0) + "FOOT." + YN$(J) For Random As #1 Len = 934
    FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)
Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$
Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$
Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$
Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$
Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$
Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$
Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$
Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)
Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)
Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)
Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)
    FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$
Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)
Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)
Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)
Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)
LSet Q$(1) = A$(J): LSet Q$(2) = AA$(J)
For I = 0 To 7: LSet Q$(I + 3) = MKI$(B6%(J, I)): Next I
For I = 0 To 4: LSet Q$(I + 11) = MKI$(PA%(J, I)): Next I
For I = 0 To 9
    LSet Q$(I * 5 + 16) = A1$(J, I)
    LSet Q$(I * 5 + 17) = MKI$(A0%(J, I))
    LSet Q$(I * 5 + 18) = MKI$(A1%(J, I))
    LSet Q$(I * 5 + 19) = MKI$(A2%(J, I))
    LSet Q$(I * 5 + 20) = MKI$(A3%(J, I))
Next I
For I = 0 To 5
    LSet Q$(I * 3 + 66) = A2$(J, I)
    LSet Q$(I * 3 + 67) = MKI$(A4%(J, I))
    LSet Q$(I * 3 + 68) = MKI$(A5%(J, I))
Next I
For I = 0 To 3
    LSet Q$(I * 5 + 84) = A3$(J, I)
    LSet Q$(I * 5 + 85) = MKI$(A6%(J, I))
    LSet Q$(I * 5 + 86) = MKI$(A7%(J, I))
    LSet Q$(I * 5 + 87) = MKI$(A8%(J, I))
    LSet Q$(I * 5 + 88) = MKI$(QX%(J, I))
Next I
For I = 0 To 2
    LSet Q$(I * 3 + 104) = A4$(J, I)
    LSet Q$(I * 3 + 105) = MKI$(A9%(J, I))
    LSet Q$(I * 3 + 106) = MKI$(B%(J, I))
Next I
For I = 0 To 2
    LSet Q$(I * 3 + 113) = A5$(J, I)
    LSet Q$(I * 3 + 114) = MKI$(B1%(J, I))
    LSet Q$(I * 3 + 115) = MKI$(B2%(J, I))
Next I
LSet Q$(122) = A6$(J, 0)
LSet Q$(123) = MKI$(B3%(J, 0))
LSet Q$(124) = A7$(J, 0)
LSet Q$(125) = MKI$(B4%(J, 0))
LSet Q$(126) = MKI$(B5%(J, 0))
LSet Q$(127) = MKI$(B7%(J, 0))
LSet Q$(128) = MKI$(B8%(J, 0))
LSet Q$(129) = A7$(J, 1)
LSet Q$(130) = MKI$(B4%(J, 1))
LSet Q$(131) = MKI$(B5%(J, 1))
LSet Q$(132) = MKI$(B7%(J, 1))
LSet Q$(133) = MKI$(B8%(J, 1))
LSet Q$(134) = MKI$(YC%(J))
For I = 0 To 14
    LSet Q$(I * 2 + 135) = DS$(J, I)
    LSet Q$(I * 2 + 136) = MKI$(DS%(J, I))
Next I
For I = 0 To 9
    LSet Q$(I * 2 + 165) = DI$(J, I)
    LSet Q$(I * 2 + 166) = MKI$(DI%(J, I))
Next I
Put #1, T(J)
Close #1

GoSub 1000
Return

9900 Call TITLE
For XY = 2 To 23: Locate XY, 1: Print Tab(79);
Next
Return


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
