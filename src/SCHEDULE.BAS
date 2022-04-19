'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
''$INCLUDE: 'QEditype.BI'

_Title "3-in-1 Football - Scheduler"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()
'DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)

Dim teamItems$(500), V%(500), U5$(3), G9$(5), WN$(3), WT$(3), SD$(3), R5$(1)
Dim Y8$(3), Y9$(5), LO$(2), A6(3), A7(3), A8(3), QX%(3), MO$(3), A3$(4)
Dim PA%(4), DRI$(2), menuItems$(9), E%(13)
Dim Q$(375), AP%(1)
'DIm A$(11), A(11), T(34)
Dim H$(20), V$(20), Z1$(30), Z2$(30), Z1%(30)
Dim QV$(20), QH$(20), SC$(2), HS%(1500), VS%(1500)
Dim Y As Long, U6 As Single, D As Long, M As Long
' DIM Ed AS EditInfo              'create the editor's data structure

'commenting out here as it is mostly irrelevant
'CALL InitMouse(There)

F2$ = Chr$(0) + Chr$(60) 'extended code for the F2 function key
F3$ = Chr$(0) + Chr$(61) 'extended code for the F3 function key
MO$(0) = "HH"
MO$(1) = "C1"
MO$(2) = "C2"
MO$(3) = "CC"

10 Color 7, 0: Cls
ReDim YN$(1500, 1), NG%(1500, 20)
ReDim DY(1500) As Long
NG%(0, 8) = 0
NG%(0, 3) = 1
NG%(0, 7) = 0
NG%(0, 2) = 1
A3$(4) = "RANDOM"
YN$(0, 0) = ""
YN$(0, 1) = ""
For I = 1 To 1500
    NG%(I, 13) = 0
Next
DT$ = "10-01-91"
TM$ = "07:30P"
    
Open "DEV.BAS" For Input As #1
Input #1, DRI$(0)
Input #1, DRI$(1)
Input #1, DRI$(2)
Close 1

U5$(0) = "HUMAN OPPONENT      ": U5$(1) = "COMPUTER IS TEAM 1  "
U5$(2) = "COMPUTER IS TEAM 2  ": U5$(3) = "COMPUTER VS COMPUTER"
G9$(0) = "PLAY USING COLLEGE RULES"
G9$(1) = "PLAY USING NFL RULES    "
G9$(2) = "PLAY USING USFL RULES   "
G9$(3) = "PLAY USING AFL RULES    "
G9$(4) = "PLAY USING WFL RULES    "
G9$(5) = "PLAY USING WLAF RULES   "
WN$(0) = "NONE             "
WN$(1) = "LIGHT TO MODERATE"
WN$(2) = "STRONG AND GUSTY "
WN$(3) = "COMPUTER SELECT  "
WT$(0) = "GOOD                          "
WT$(1) = "BAD (RAIN/SNOW)               "
WT$(2) = "TERRIBLE (BLIZZARD/HEAVY RAIN)"
WT$(3) = "COMPUTER SELECT               "
SD$(0) = "NO OVERTIME          "
SD$(1) = "ONE 15-MINUTE QUARTER"
SD$(2) = "SUDDEN DEATH TO WIN  "
SD$(3) = "NCAA DIVISION I-AA   "
R5$(0) = "Y": R5$(1) = "N"
Y8$(0) = "1978-1993": Y8$(1) = "PRE-1974 ": Y8$(2) = "1974-1977": Y8$(3) = "POST-1993"
Y9$(0) = "PRE-1968 ": Y9$(1) = "1968-1980": Y9$(2) = "1981-1983": Y9$(3) = "1984-1991": Y9$(4) = "1991     ": Y9$(5) = "POST-1991"
LO$(1) = "Y": LO$(2) = "N"

11 Call TITLE
Locate 16: Print: Color 2
Print "GENERATING A STANDARD SEASON SCHEDULE IS TO BE USED IN CONJUNCTION"
Print "WITH THE STANDINGS AND LEAGUE LEADERS OPTIONS. YOU MUST ALREADY HAVE"
Print "CREATED AN APPROPRIATE LEAGUE FOR THIS OPTION TO PROPERLY FUNCTION."
ERASE menuItems$
menuItems$(1) = "ENTER A NEW SCHEDULE                             "
menuItems$(2) = "RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE          "
menuItems$(3) = "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE "
menuItems$(4) = "EXTRACT A CONFERENCE SCHEDULE FROM MAIN SCHEDULE "
menuItems$(5) = "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE "
menuItems$(6) = "MAKE GLOBAL CHANGES TO A SCHEDULE                "
menuItems$(7) = "BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS  "
menuItems$(8) = "EXIT AUTO SCHEDULER TO 3 IN 1 MENU               "
SELECTION% = 1
Count% = 8
Start% = 1
SCANCODE% = 8
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 16

CALL box0(5, 15, 14, 65, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 8
D3% = SELECTION% - 1
On SELECTION% GOTO 26, 600, 600, 600, 600, 600, 4500, 15

15 RUN "HELLO.EXE"


26 AP%(0) = 0: NS% = 0: TS% = 0

21 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
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
Locate , 12: Print "ENTER NAME FOR SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE ARKANSAS, LSU68, IU76, ARK90, ETC"
Locate 7, 37: Input scheduleFile$
If scheduleFile$ = "" Then 21
Locate 13, 12: Print "ANY CHANGE (YN) "
22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 21
Color 15, 0

25 Cls
Locate 23, 1: Color , 1: Print Tab(80);: Locate 23, 1
Color 13: Print "HF";: Color 14: Print "-HOME FIELD ADV  ";
Color 13: Print "MA";: Color 14: Print "-MODE OF PLAY  ";
Color 11: Print " G";: Color 14: Print "-GOTO GAME";
Color 11: Print " B";: Color 14: Print "-BOXSCORE OPTIONS"
Locate 24, 1: Color , 1: Print Tab(80);: Locate 24, 1
Color 11: Print "C";: Color 14: Print "-SAVE AND CONTINUE  ";
Color 11: Print "A";: Color 14: Print "-ADD A GAME  ";
Color 11: Print "D";: Color 14: Print "-DELETE A GAME  ";
Color 11: Print "E";: Color 14: Print "-EDIT A GAME  ";
Color 11: Print "PGUP  PGDN";
Locate 25, 1: Color , 1: Print Tab(80);: Locate 25, 1
Color 11: Print "S";: Color 14: Print "-SORT GAMES  ";
Color 11: Print "P";: Color 14: Print "-PLAY GAMES   ";
Color 11: Print "X";: Color 14: Print "-SAVE AND EXIT  ";
Color 11: Print "Q";: Color 14: Print "-QUIT  ";
Color 11: Print "R";: Color 14: Print "-RESET SCORES";
Locate 1, 1: Print Tab(80);: Locate 1, 1: Print scheduleFile$
Locate 1, 33: Color 14: Print "AUTO SCHEDULER"
Color , 0

50 Locate 2, 1
Color 14
Print "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  HF": Color 15
For X = 1 To 20
    Locate X + 2, 1
    If X + NS% * 20 > AP%(0) Then 100
    T1 = NG%(X + NS% * 20, 0): YN$ = YN$(X + NS% * 20, 0)
    GoSub 8600
    V$(X) = N$
    QV$(X) = A3$(NG%(X + NS% * 20, 5))
    T1 = NG%(X + NS% * 20, 1): YN$ = YN$(X + NS% * 20, 1)
    GoSub 8600
    H$(X) = N$
    QH$(X) = A3$(NG%(X + NS% * 20, 8))
    Print Using "####  "; X + NS% * 20;
    Print Using "##/##/##"; NG%(X + NS% * 20, 17); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 18);
    Locate , 17
    I$ = "A"
    HR% = NG%(X + NS% * 20, 15) / 100
    MIN% = NG%(X + NS% * 20, 15) - HR% * 100
    If HR% = 12 Then I$ = "P"
    If HR% > 12 Then HR% = HR% - 12: I$ = "P"
    TM$ = Right$(Str$(HR%), 2) + ":"
    If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
    If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)
    TM$ = TM$ + I$
    Print TM$;
    Locate , 25: Print V$(X); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; VS%(X + NS% * 20);
    Locate , 46: Print H$(X); " ";
    If NG%(X + NS% * 20, 13) = 1 Then Print Using "###"; HS%(X + NS% * 20);
    Locate , 67: Print MO$(NG%(X + NS% * 20, 10));
    Locate , 72: If NG%(X + NS% * 20, 2) = 1 Then Print "Y" Else Print "N"
Next
100 I$ = GetKeyPress$
If UCase$(I$) = "A" Then GoSub 150: GoTo 25
If UCase$(I$) = "E" Then GoSub 140: GoTo 25
'IF UCASE$(I$) = "B" THEN 5000
If UCase$(I$) = "G" Then GoSub 120: GoTo 25
If UCase$(I$) = "D" Then GoSub 200: GoTo 25
If UCase$(I$) = "S" Then GoSub 8500: GoTo 25
If UCase$(I$) = "C" Then GoSub 350: GoTo 25
If UCase$(I$) = "X" Then GoSub 350: GoTo 10
If I$ = Chr$(0) + "Q" Then GoSub 130: GoTo 25
If I$ = Chr$(0) + "I" Then GoSub 135: GoTo 25
If UCase$(I$) = "Q" Then 10
If UCase$(I$) = "P" Then GoSub 350: GoTo 300
If UCase$(I$) = "R" Then GoSub 400: GoTo 25
GoTo 100

120 'GOTO A SPECIFIC PAGE
Locate 23, 1: Print "ENTER GAME # TO GO TO        "
Locate 23, 1: Input "ENTER GAME # TO GO TO "; NG%
If NG% < 1 Or NG% > AP%(0) Then 120
NS% = Int((NG% - 1) / 20)
Return

130 ' *** PAGE DOWN ***
If NS% >= BS% Then NS% = BS%: GoTo 133
NS% = NS% + 1

133 Return

135 ' *** PAGE UP ***
If NS% = 0 Then 137
NS% = NS% - 1

137 Return

140 ' *** EDIT A GAME ***
Locate 23, 1: Print "ENTER GAME # TO EDIT        "
Locate 23, 1: Input "ENTER GAME # TO EDIT "; NG%
If NG% < 1 Or NG% > AP%(0) Then 140
V$ = V$(NG% - Int(NG% / 20) * 20)
H$ = H$(NG% - Int(NG% / 20) * 20)
QV$ = QV$(NG% - Int(NG% / 20) * 20)
QH$ = QH$(NG% - Int(NG% / 20) * 20)
If NG% - Int(NG% / 20) * 20 = 0 Then V$ = V$(20): H$ = H$(20): QV$ = QV$(20): QH$ = QH$(20)
I$ = "A"
HR% = Int(NG%(NG%, 15) / 100)
MIN% = NG%(NG%, 15) - HR% * 100
If HR% = 12 Then I$ = "P"
If HR% > 12 Then HR% = HR% - 12: I$ = "P"
TM$ = Right$(Str$(HR%), 2) + ":"
If MIN% < 10 Then TM$ = TM$ + "0" + Right$(Str$(MIN%), 1)
If MIN% >= 10 Then TM$ = TM$ + Right$(Str$(MIN%), 2)
TM$ = TM$ + I$
DT$ = Right$(Str$(NG%(NG%, 17)), 2) + "/"
DYMO% = Val(Right$(Str$(NG%(NG%, 16)), 2))
If DYMO% < 10 Then DYMO$ = "0" + Right$(Str$(NG%(NG%, 16)), 1)
If DYMO% >= 10 Then DYMO$ = Right$(Str$(NG%(NG%, 16)), 2)
DT$ = DT$ + DYMO$ + "/" + Right$(Str$(NG%(NG%, 18)), 2)
GoTo 149

150 ' *** ADD A GAME TO THE SCHEDULE ***
V$ = "": H$ = ""
QV$ = "": QH$ = ""
AP%(0) = AP%(0) + 1
NG% = AP%(0)
BS% = Int((NG% - 1) / 20): If NS% < BS% Then NS% = BS%
For X = 0 To 20
    If X = 13 Then 139
    NG%(NG%, X) = NG%(NG% - 1, X)

139 Next
For X = 0 To 1: YN$(NG%, X) = YN$(NG% - 1, X): Next

149 Cls: Color 2: Print Tab(29); "SELECT OPENING OPTIONS": Print

151 Locate 2, 2: Color 14: Print "(1) ";: Color 15: Print U5$(NG%(NG%, 10))
Locate 3, 2: Color 14: Print "(2) ";: Color 15: Print "VISITING TEAM / QB: "; V$; " / "; QV$
Locate 4, 2: Color 14: Print "(3) ";: Color 15: Print "HOME TEAM / QB: "; H$; " / "; QH$
Locate 5, 2: Color 14: Print "(4) ";: Color 15: Print "TYPE OF GAME: "; G9$(NG%(NG%, 3))
Locate 6, 2: Color 14: Print "(5) ";: Color 15: Print "HOME FIELD ADVANTAGE: "; LO$(NG%(NG%, 2))
Locate 7, 2: Color 14: Print "(6) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#"; NG%(NG%, 4) / 10
Locate 8, 2: Color 14: Print "(7) ";: Color 15: Print "COLLEGE RULES "; Y9$(NG%(NG%, 7))
Locate 9, 2: Color 14: Print "(8) ";: Color 15: Print "PRO RULES "; Y8$(NG%(NG%, 11))

160 Locate 10, 2: Color 14: Print "(9) ";: Color 15: Print "GAME DATE (XX-XX-XX) "; DT$; "   "

161 Locate 11, 2: Color 14: Print "(A) ";: Color 15: Print "GAME TIME (XX:XXP) "; TM$; "   "
Locate 12, 2: Color 14: Print "(B) ";: Color 15: Print "SUDDEN DEATH "; SD$(NG%(NG%, 14))
Locate 13, 2: Color 14: Print "(C) ";: Color 15: Print "WIND "; WN$(NG%(NG%, 19))
Locate 14, 2: Color 14: Print "(D) ";: Color 15: Print "WEATHER "; WT$(NG%(NG%, 20))
Locate 15, 2: Color 14: Print "(0) ";: Color 15: Print "ACCEPT SETTINGS"
Locate 16, 2: Print "NOTES: CLOCK IS STOPPED AFTER A 1ST DOWN IN COLLEGE BEGINNING IN 1968"
Locate , 2: Print "       HOLDING IS A 15 YARD PENALTY FOR COLLEGE PRE-1981 AND PRO PRE-1974"
Locate , 2: Print "       1991-PRESENT, COLLEGE HOLDING MARKED FROM POINT OF INFRACTION"
Locate , 2: Print "       1974-77, PRO HOLDING ON A RUN IS 10 YDS AND 15 ON A PASS"
Locate , 2: Print "       FROM 1978-PRESENT, ALL PRO HOLDING PENALTIES ARE 10 YARDS"
Locate , 2: Print "       PRE-1974, IN THE PROS, GOAL POSTS WERE ON THE GOAL LINE, THE BALL WAS"
Locate , 2: Print "       PLACED ON THE 20 AFTER MISSED FIELD GOALS, KICKOFFS WERE FROM THE 40"
Locate , 2: Print "       1992-PRESENT, COLLEGE RULES ALLOW ADVANCEMENT OF A FUMBLE"
Locate , 2: Print "       PRE-1984, IN COLLEGE PASS INTERFERENCE AT SPOT OF INFRACTION";: Locate 1, 1
175 I$ = GetKeyPress$
If UCase$(I$) = "A" Then I$ = "10"
If UCase$(I$) = "B" Then I$ = "11"
If UCase$(I$) = "C" Then I$ = "12"
If UCase$(I$) = "D" Then I$ = "13"
I5 = Val(I$): If I5 < 0 Or I5 > 13 Then 175
On I5 GOTO 180, 181, 181, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193
If V$ <> "" And H$ <> "" And TM$ <> "" And DT$ <> "" Then 250
GoTo 175

250 I$ = Right$(TM$, 1): TM$ = Left$(TM$, Len(TM$) - 1)
MIN% = Val(Right$(TM$, 2))
TM$ = Left$(TM$, Len(TM$) - 3)
HR% = Val(TM$)
If UCase$(I$) = "P" And HR% < 12 Then HR% = HR% + 12
NG%(NG%, 15) = HR% * 100 + MIN%
NG%(NG%, 17) = Val(Left$(DT$, 2))
NG%(NG%, 18) = Val(Mid$((DT$), 7, 2))
NG%(NG%, 16) = Val(Mid$((DT$), 4, 2))
Return

180 NG%(NG%, 10) = NG%(NG%, 10) + 1: If NG%(NG%, 10) > 3 Then NG%(NG%, 10) = 0
GoTo 151

181 Color 15, 0: Cls
JJ = 7
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
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
ERASE teamItems$
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 181

183 Locate 2, 33: Color PA%(2), PA%(1): Print N$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 181
Color 15, 0
Locate 24, 1: Print "VAL 1 = "; NG%, I5 - 2
NG%(NG%, I5 - 2) = T1: YN$(NG%, I5 - 2) = YN$
Locate 24, 1: Print "VALUE"; NG% - Int(NG% / 20) * 20
If I5 = 2 Then V$ = N$: V$(NG% - Int(NG% / 20) * 20) = N$
If I5 = 3 Then H$ = N$: H$(NG% - Int(NG% / 20) * 20) = N$
GoSub 8300
If I5 = 2 Then QV$ = QB$: QV$(NG% - Int(NG% / 20) * 20) = QB$
If I5 = 3 Then QH$ = QB$: QH$(NG% - Int(NG% / 20) * 20) = QB$
GoTo 149

184 NG%(NG%, 3) = NG%(NG%, 3) + 1: If NG%(NG%, 3) > 5 Then NG%(NG%, 3) = 0
GoTo 151

185 N = 1 - (NG%(NG%, 2) - 1): NG%(NG%, 2) = N + 1: GoTo 151

186 Locate 7, 32: Input U6: NG%(NG%, 4) = U6 * 10: GoTo 151

187 NG%(NG%, 7) = 1 + NG%(NG%, 7): If NG%(NG%, 7) > 5 Then NG%(NG%, 7) = 0
GoTo 151

188 NG%(NG%, 11) = 1 + NG%(NG%, 11): If NG%(NG%, 11) > 3 Then NG%(NG%, 11) = 0
GoTo 151

189 Locate 10, 27: Input DT$: GoTo 160

190 Locate 11, 25: Input TM$: GoTo 161

191 NG%(NG%, 14) = NG%(NG%, 14) + 1: If NG%(NG%, 14) > 3 Then NG%(NG%, 14) = 0
GoTo 161

192 NG%(NG%, 19) = NG%(NG%, 19) + 1: If NG%(NG%, 19) > 3 Then NG%(NG%, 19) = 0
GoTo 161

193 NG%(NG%, 20) = NG%(NG%, 20) + 1: If NG%(NG%, 20) > 3 Then NG%(NG%, 20) = 0
GoTo 161

200 ' *** DELETE A GAME FROM THE SCHEDULE ***
Locate 24, 1: Input "ENTER GAME # TO DELETE "; NG%
If NG% < 1 Or NG% > AP%(0) Then 200
For X = NG% To AP%(0) - 1
    For X1 = 0 To 20: NG%(X, X1) = NG%(X + 1, X1): Next
    VS%(X) = VS%(X + 1)
    HS%(X) = HS%(X + 1)
    For X1 = 0 To 1: YN$(X, X1) = YN$(X + 1, X1): Next
Next
AP%(0) = AP%(0) - 1: BS% = Int((AP%(0) - 1) / 20)
If NS% > BS% Then NS% = BS%
Return

300 ' *** PLAY GAMES ***
Color , 0: Cls
JJ = 7
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
Locate , 12: Print "TOTAL GAMES ="; AP%(0)
Locate , 12: Print "SELECT GAME # TO START AND END WITH": Print
Locate , 12: Input "ENTER START GAME #: "; SG%: Print
Locate , 12: Input "ENTER END GAME #:"; EG%: Print
Locate , 12: Print "ANY CHANGE (YN)"
301 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 300
If SG% <= 0 Then SG% = 1
If EG% > AP%(0) Then EG% = AP%(0)

Open "GAME82" For Output As #2
Print #2, scheduleFile$ + ".SCD"
Print #2, SG% 'Start Game
Print #2, EG% 'End Game
Print #2, 0
Close 2

GoSub 1200
Open "TICKER" For Output As #2
For I = 1 To 32: For I1 = 0 To 1: For I2 = 1 To 14
Print #2, -1: Next: Next: Next
For I = 1 To 32: For I1 = 0 To 1: Print #2, "XXX"
Next: Next
For I = SG% To SG% + 31
    Print #2, NG%(I, 12)
Next
Close 2

335 Run "FOOTBALL.EXE"

350 ' *** SAVE SCHEDULE FILE ***
If _FileExists(DRI$(0) + scheduleFile$ + ".SCD") Then Kill DRI$(0) + scheduleFile$ + ".SCD"
Open DRI$(0) + scheduleFile$ + ".SCD" For Random As #1 Len = 52
For X = 0 To 20
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$
Next
Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)
For I = 1 To AP%(0)
    For X = 0 To 20: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
    For X = 0 To 1: LSet Q$(22 + X) = YN$(I, X): Next
    LSet Q$(24) = MKI$(VS%(I)): LSet Q$(25) = MKI$(HS%(I))
    Put #1, I
Next
Close 1
Return

400 ' *** RESET GAMES ***
For I = 1 To AP%(0)
    NG%(I, 13) = 0
Next
Return

600 ' *** LOAD IN A SAVED SCHEDULE ***
Spec$ = DRI$(0) + "*.SCD"
GoSub 2100
Open DRI$(0) + scheduleFile$ + ".SCD" For Random As #1 Len = 52
Y = LOF(1)
Close 1
AP%(0) = Y / 52
BS% = Int((AP%(0) - 1) / 20)
Open DRI$(0) + scheduleFile$ + ".SCD" For Random As #1 Len = 52
For X = 0 To 20
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$
Next
Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)
For I = 1 To AP%(0)
    Get #1, I
    For X = 0 To 20: NG%(I, X) = CVI(Q$(X + 1)): Next
    For X = 0 To 1: YN$(I, X) = Q$(22 + X): Next
    VS%(I) = CVI(Q$(24)): HS%(I) = CVI(Q$(25))
Next
Close 1: NS% = 0: TS% = 0
If D3% = 2 Or D3% = 3 Or D3% = 4 Or D3% = 5 Then 700
GoTo 25

700 'EXTRACT A SCHEDULE FROM ANOTHER
If D3% = 4 Then 725
If D3% = 5 Then GoSub 900: GoSub 350: GoTo 25
Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
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
Locate , 12: Print "ENTER NAME FOR SUB-SCHEDULE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .SCD"
Locate , 12: Print "SAMPLE NAMES ARE ARKANSAS, LSU68, IU76, ARK90, ETC"
Locate 7, 37: Input SF$
If SF$ = "" Then 700
Locate 13, 12: Print "ANY CHANGE (YN) "
705 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 700
Color 15, 0
Cls

725 If D3% = 3 Then 800
Color 15, 0: Cls
JJ = 7
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
Locate , 12: Print
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 730
YN$ = NY$
ERASE teamItems$
GoSub 1100

730 If BO% = 1 Then BO% = 0: GoTo 725

732 Locate 2, 33: Color PA%(2), PA%(1): Print N$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 725
Color 15, 0
If D3% = 4 Then GoSub 775
'HAVE T1 AND YN$
For I = 1 To AP%(0)

    735 If Right$(YN$(I, 0), 1) = " " Then YN$(I, 0) = Left$(YN$(I, 0), Len(YN$(I, 0)) - 1): GoTo 735

    736 If Right$(YN$(I, 1), 1) = " " Then YN$(I, 1) = Left$(YN$(I, 1), Len(YN$(I, 1)) - 1): GoTo 736
    If T1 = NG%(I, 0) And YN$ = YN$(I, 0) Or T1 = NG%(I, 1) And YN$ = YN$(I, 1) Then GoSub 780: GoSub 750
Next
If D3% = 4 Then GoSub 350
GoTo 10

750 If D3% = 4 Then 755
Locate 1, 1: Print NG% + 1; " MATCHES"
'IF NOT _FILEEXISTS(DRI$(0) + SF$ + ".SCD") THEN 752
Open DRI$(0) + SF$ + ".SCD" For Random As #1 Len = 52
Y = LOF(1): GoTo 753

752 Y = 0

753 NG% = Y / 52
For X = 0 To 20
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$
Next
Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)
For X = 0 To 20: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next
For X = 0 To 1: LSet Q$(22 + X) = YN$(I, X): Next
LSet Q$(24) = MKI$(VS%(I)): LSet Q$(25) = MKI$(HS%(I))
Put #1, NG% + 1
Close 1

755 Return

775 Cls
Color PA%(2), PA%(1): Print N$;: Color 15, 0: Print " IS TO BE:"
Print
Print "(0) MANUALLY CONTROLLED": Print
Print "(1) COMPUTER CONTROLLED"
776 I$ = GetKeyPress$
I1 = Val(I$)
If I1 < 0 Or I1 > 1 Then 776
Input "ENTER DISPLAY VARIABLE FOR THESE GAMES "; U6
U6 = U6 * 10
Return

780 ' CHANGE SETTINGS FOR A TEAM
If T1 = NG%(I, 0) And YN$ = YN$(I, 0) Then NG%(I, 10) = 2
If T1 = NG%(I, 1) And YN$ = YN$(I, 1) Then NG%(I, 10) = 1
If (T1 = NG%(I, 0) And YN$ = YN$(I, 0) Or T1 = NG%(I, 1) And YN$ = YN$(I, 1)) And I1 = 1 And D3% = 4 Then NG%(I, 10) = 3
NG%(I, 4) = U6
Return

800 Spec$ = DRI$(2) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4400
ReDim leagueFiles$(0 To Count%)
For X = 1 To Count%
    leagueFiles$(X) = Space$(12)
Next
leagueFiles$(0) = Spec$
Call ReadFile(leagueFiles$())
'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)
GoSub 1310
Open DRI$(2) + targetFile$ + ".LIB" For Input As #1
Input #1, X%
If X% = 0 Then
    Input #1, LG$
    Input #1, X$: Input #1, X$
    For X = 1 To 30
        Input #1, Z1$(X): Input #1, Z1%(X): Input #1, Z2$(X)
    Next
    NT = 30
    Close 1
End If
If X% = 1 Then Input #1, LG$: For X = 1 To 30: Input #1, Z1$(X): Input #1, Z1%(X): Input #1, Z2$(X): Next: NT = 30: Close 1
For I = 1 To AP%(0)

    835 If Right$(YN$(I, 0), 1) = " " Then YN$(I, 0) = Left$(YN$(I, 0), Len(YN$(I, 0)) - 1): GoTo 835

    836 If Right$(YN$(I, 1), 1) = " " Then YN$(I, 1) = Left$(YN$(I, 1), Len(YN$(I, 1)) - 1): GoTo 836
    For I1 = 1 To NT
        If Z1$(I1) = "XXX" Then 837
        If Z1%(I1) = NG%(I, 0) And Z2$(I1) = YN$(I, 0) Or Z1%(I1) = NG%(I, 1) And Z2$(I1) = YN$(I, 1) Then GoSub 850: GoTo 838

    837 Next I1

838 Next I
GoTo 10

850 Print "FOUND A MATCH"
'    IF NOT _FILEEXISTS(DRI$(0) + SF$ + ".SCD") THEN 852
Open DRI$(0) + SF$ + ".SCD" For Random As #1 Len = 52
Y = LOF(1): GoTo 853

852 Y = 0

853 NG% = Y / 52

854 Print Y, NG%

855 For X = 0 To 20

    856 Field #1, X * 2 As X$, 2 As Q$(X + 1), 52 - 2 - 2 * X As X$

857 Next

858 Field #1, 42 As X$, 3 As Q$(22), 3 As Q$(23), 2 As Q$(24), 2 As Q$(25)

859 For X = 0 To 20: LSet Q$(X + 1) = MKI$(NG%(I, X)): Next

860 For X = 0 To 1: LSet Q$(22 + X) = YN$(I, X): Next

861 LSet Q$(24) = MKI$(VS%(I)): LSet Q$(25) = MKI$(HS%(I))

862 Put #1, NG% + 1

863 Close 1

864 Return

900 ' GLOBAL SETTINGS FOR A SCHEDULE
WN% = 3: WT% = 3
Color 15, 0: Cls: Print Tab(30); "GLOBAL SETTINGS FOR SCHEDULE"

901 Locate 3, 2: Color 14: Print "(1) ";: Color 15: Print "TYPE OF GAME: "; G9$(G9%)
Locate 4, 2: Color 14: Print "(2) ";: Color 15: Print Using "DISPLAY PAUSE IN SECONDS: ##.#  "; U6 / 10
Locate 5, 2: Color 14: Print "(3) ";: Color 15: Print "FUNCTION NOT AVAILABLE"
Locate 6, 2: Color 14: Print "(4) ";: Color 15: Print "COLLEGE RULES: "; Y9$(Y9%)
Locate 7, 2: Color 14: Print "(5) ";: Color 15: Print "PRO RULES: "; Y8$(Y8%)
Locate 8, 2: Color 14: Print "(6) ";: Color 15: Print "SUDDEN DEATH: "; SD$(SD%)
Locate 9, 2: Color 14: Print "(7) ";: Color 15: Print "WIND OPTION: "; WN$(WN%)
Locate 10, 2: Color 14: Print "(8) ";: Color 15: Print "WEATHER OPTION: "; WT$(WT%)
Locate 12, 2: Color 14: Print "(9) ";: Color 15: Print "ACCEPT GLOBAL SETTINGS"
905 I$ = GetKeyPress$
I = Val(I$)
If I < 1 Or I > 9 Then 905
If I = 1 Then
    G9% = G9% + 1
    If G9% > 5 Then G9% = 0
    GoTo 901
End If
If I = 3 Then 901
If I = 4 Then
    Y9% = Y9% + 1
    If Y9% > 5 Then Y9% = 0
    GoTo 901
End If
If I = 5 Then
    Y8% = Y8% + 1
    If Y8% > 3 Then Y8% = 0
    GoTo 901
End If
If I = 6 Then
    SD% = SD% + 1
    If SD% > 3 Then SD% = 0
    GoTo 901
End If
If I = 7 Then
    WN% = WN% + 1
    If WN% > 3 Then WN% = 0
    GoTo 901
End If
If I = 8 Then
    WT% = WT% + 1
    If WT% > 3 Then WT% = 0
    GoTo 901
End If
If I = 2 Then Input "DISPLAY VARIABLE "; U6: U6 = U6 * 10: GoTo 901
For I = 1 To AP%(0)
    NG%(I, 3) = G9%
    NG%(I, 4) = U6
    NG%(I, 7) = Y9%
    NG%(I, 11) = Y8%
    NG%(I, 14) = SD%
    NG%(I, 19) = WN%
    NG%(I, 20) = WT%
Next
Return

1100 NT = 0
Open DRI$(0) + "FOOT." + YN$ For Random As #1 Len = 934
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
T1 = V%(SELECTION%)
GoTo 9000

1200 For I = 1 To AP%(0)
    HR% = Int(NG%(I, 15) / 100)
    MIN% = NG%(I, 15) - HR% * 100
    TM% = HR% * 60 + MIN%
    NG%(I, 12) = Int(TM% / 30 - 18)
Next
Return

2100 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4400
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
Print "SELECT SCHEDULE TO VIEW": Color 15
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
Print "-SELECTS SCHEDULE";

_MouseShow "DEFAULT"
320 MAMenu leagueFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 2900
If EXITCODE% = -71 Then Start% = 1: EXITCODE% = 3: SELECTION% = 1: GoTo 320
If EXITCODE% = -79 Then Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GoTo 320
If EXITCODE% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If EXITCODE% = -81 Then Start% = Start% + 80: If Start% > Count% Then Start% = Count%
If EXITCODE% = -81 Or EXITCODE% = -73 Then EXITCODE% = 3: SELECTION% = Start%: GoTo 320
targetFile$ = leagueFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
WE$ = Left$(targetFile$, Len(targetFile$) - 4)
Color 15, 0
targetFile$ = WE$
Return

8500 ' *** SORT SCHEDULE ***
For I = 1 To AP%(0)
    HR% = Int(NG%(I, 15) / 100)
    MIN% = NG%(I, 15) - HR% * 100
    TM% = HR% * 60 + MIN%
    NG%(I, 12) = Int(TM% / 30 - 18)
    D = (NG%(I, 18) - 1): D = D * 365
    M = (NG%(I, 17) - 1): M = M * 31
    Y = NG%(I, 16)
    DY(I) = (D + M + Y) * 24 * 60 + TM%
Next
For Z = 1 To AP%(0) - 1: For Z1 = Z + 1 To AP%(0)
    Locate 1, 1: Print Z, Z1
    If NG%(Z, 12) <= NG%(Z1, 12) Then 8505
        GoSub 8525

8505 Next Z1: Next Z
For Z = 1 To AP%(0) - 1: For Z1 = Z + 1 To AP%(0)
    If DY(Z) <= DY(Z1) Then 8510
        GoSub 8525

8510 Next Z1: Next Z
Return

8525 For I = 0 To 20: Swap NG%(Z, I), NG%(Z1, I): Next
For I = 0 To 1: Swap YN$(Z, I), YN$(Z1, I): Next
Swap DY(Z), DY(Z1)
Swap VS%(Z), VS%(Z1)
Swap HS%(Z), HS%(Z1)
Return

8600 Open DRI$(0) + "FOOT." + YN$ For Random As #1 Len = 934
Field #1, 15 As Q$(1)
Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)
Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)
Get #1, T1
N$ = Q$(1)
For I1 = 0 To 3
    A3$(I1) = Q$(I1 * 5 + 84)
Next I1
Close 1
Return

9000 Open DRI$(0) + "FOOT." + YN$ For Random As #1 Len = 934

9501 FIELD #1, 15 AS Q$(1), 15 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 2 AS Q$(14), 2 AS Q$(15), 200 AS Q$(0), 200 AS Q$(199), 200 AS Q$, 200  _
AS Q$, 78 AS Q$(0)

9502 Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$

9503 Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$

9504 Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$

9505 Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$

9506 Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$

9507 Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$

9508 Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$

9509 Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)

9510 Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)

9511 Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)

9512 Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)

9513 FIELD #1, 200 AS Q$, 200 AS Q$, 128 AS Q$, 12 AS Q$(122), 2 AS Q$(123), 12 AS Q$(124), 2 AS Q$(125), 2 AS Q$(126), 2 AS Q$(127), 2 AS Q$(128), 12 AS Q$(129), 2 AS Q$(130), 2 AS Q$(131), 2 AS Q$(132), 2 AS Q$(133), 2 AS Q$(134), 200 AS Q$, 150  _
AS Q$

9514 Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)

9515 Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)

9516 Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)

9517 Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)

9518 Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

9519 Get #1, T1

9520 N$ = Q$(1)

9522 For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1

9535 For I1 = 0 To 3

    9536 A3$(I1) = Q$(I1 * 5 + 84)

    9537 A6(I1) = CVI(Q$(I1 * 5 + 85))

    9538 A7(I1) = CVI(Q$(I1 * 5 + 86))

    9539 A8(I1) = CVI(Q$(I1 * 5 + 87))

    9540 QX%(I1) = CVI(Q$(I1 * 5 + 88))

9548 Next I1
Close 1

9005 If Right$(N$, 1) = " " Then N$ = Left$(N$, Len(N$) - 1): GoTo 9005
Return

8300 Cls: Print "SELECT QUARTERBACK": Print " # QB NAME    % OF  COMP  INT": Print "              ATTS  PCT   PCT"

8301 For I1 = 0 To 3: If A3$(I1) = "XXX" Or A3$(I1) = "END" Or A3$(I1) = "" Then 8307

    8302 If I1 = 0 Then 8304

    8303 If A6(I1 - 1) >= 100 Then 8307

    8304 Print I1; "-"; A3$(I1);: Locate , 14: If I1 = 0 Then Print A6(I1);: GoTo 8306

    8305 If I1 > 0 Then Print Int(A6(I1)) - A6(I1 - 1);

    8306 Locate , 20: Print A7(I1);: Locate , 26: Print A8(I1)

8307 Locate , 1: Next I1
Print " 4 -COMPUTER SELECT QUARTERBACK"

8309 Print: Locate , 1: Print "SELECT (0-4) ";
8310 I$ = GetKeyPress$

8311 I2 = Val(I$): If I2 < 0 Or I2 > 4 Then 8300

8312 Q6% = I2

8313 If NG%(NG%, 3) = 0 Or I2 > 3 Then 8320

8314 Print "ENTER "; A3$(I2); " MOBILITY RATING (1-9)"
8318 I$ = GetKeyPress$

8319 R9% = Val(I$): If R9% < 1 Or R9% > 9 Then 8314

8320 NG%(NG%, (I5 - 2) * 3 + 5) = Q6%: NG%(NG%, (I5 - 2) * 3 + 6) = R9%
A3$(4) = "RANDOM"
QB$ = A3$(I2)
Return

4400 Locate 10, 8
GoSub 4600
Print "NO LEAGUE FILES WERE FOUND"
Print "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
GoTo 2900

4600 JJ = 4
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

2900 Run "HELLO.EXE"

'EDIT THE PARAMETERS TO SAVE A SPECIAL EVENTS BOXSCORE

5070 If Not _FileExists("EVENTS.BOX") Then 5075
Open "EVENTS.BOX" For Input As #1
For X = 1 To 13
    Input #1, E%(X)
Next
Close 1

5075 Cls: Color 14
Print "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES": X% = 2: Color 15
Print " 1. IF QUARTERS ARE OVER "; E%(1)
Print " 2. A TEAM'S POINTS MORE THAN "; E%(2)
Print " 3. A TEAM'S POINTS LESS THAN "; E%(3)
Print " 4. RUSHER'S YARDS MORE THAN "; E%(4)
Print " 5. RUSHER'S TDS MORE THAN "; E%(5)
Print " 6. RECEIVER'S YARDS MORE THAN "; E%(6)
Print " 7. RECEIVER'S TDS MORE THAN "; E%(7)
Print " 8. PASSER'S YARDS MORE THAN "; E%(8)
Print " 9. PASSER'S TDS MORE THAN "; E%(9)
Print "10. KICKER'S FGM MORE THAN "; E%(10)
GoTo 197

147 Locate 2: Input " 1. IF QUARTERS ARE OVER "; E%(1): If C1 = 1 Then 197

162 Locate 3: Input " 2. A TEAM'S POINTS MORE THAN "; E%(2): If C1 = 1 Then 197

163 Locate 4: Input " 3. A TEAM'S POINTS LESS THAN "; E%(3): If C1 = 1 Then 197

148 Locate 5: Input " 4. RUSHER'S YARDS MORE THAN "; E%(4): If C1 = 1 Then 197

178 Locate 6: Input " 5. RUSHER'S TDS MORE THAN "; E%(5): If C1 = 1 Then 197

152 Locate 7: Input " 6. RECEIVER'S YARDS MORE THAN "; E%(6): If C1 = 1 Then 197

111 Locate 8: Input " 7. RECEIVER'S TDS MORE THAN "; E%(7): If C1 = 1 Then 197

112 Locate 9: Input " 8. PASSER'S YARDS MORE THAN "; E%(8): If C1 = 1 Then 197

153 Locate 10: Input " 9. PASSER'S TDS MORE THAN "; E%(9): If C1 = 1 Then 197

114 Locate 11: Input "10. KICKER'S FGM MORE THAN "; E%(10): If C1 = 1 Then 197

197 C1 = 1
Locate 23, 1: Print "SELECT # TO CHANGE 1-13 (0 TO CONTINUE) ";
Input B9
On B9 GOTO 147, 162, 163, 148, 178, 152, 111, 112, 153, 114
Open "EVENTS.BOX" For Output As #1
For X = 1 To 13
    Print #1, E%(X)
Next
Close 1
GoTo 4500

4000 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10

4010 Locate 10, 8
Call CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 4500

4020 Locate 10, 8
Call CSMsgBox("NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 4500

4050 Locate 10, 8
Call CSMsgBox("NO SCHEDULE FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 10

4060 Locate 10, 8
Call CSMsgBox("BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU", 23, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
Return

4500 'BOXSCORE MAINTENANCE
Call TITLE
ERASE menuItems$
menuItems$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
menuItems$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
menuItems$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
menuItems$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
menuItems$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
menuItems$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 6
Column% = 16
CALL box0(5, 15, 12, 65, 2,  15, 1)
    
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then SELECTION% = 6
On SELECTION% GOTO 5070, 5060, 5010, 5010, 5010
GoTo 10

5000 ReDim array$(500) 'holds the main editor string array
Locate 23, 1: Print "ENTER GAME # TO GO TO        "
Locate 23, 1: Input "ENTER GAME # TO GO TO "; NG%
If NG% < 1 Or NG% > AP%(0) Then 120
NS% = Int((NG% - 1) / 20)
BN$ = DRI$(1) + "box" + Mid$(Str$(NG%), 2, Len(Str$(NG%)))
'GOSUB 5050
GoTo 25

5010 If SELECTION% = 3 Then Spec$ = DRI$(1) + "BOX*."
If SELECTION% = 4 Then Spec$ = DRI$(1) + "*.SPC"
If SELECTION% = 5 Then Spec$ = DRI$(1) + "*.SPC"
GoSub 5020
If SELECTION% = 5 Then Spec$ = DRI$(1) + "BOX*.": GoSub 5020
GoTo 4500

5020 null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
    
If Count% = 0 And SELECTION% = 4 Then 4010
If Count% = 0 And SELECTION% = 3 Then 4020
If Count% = 0 And SELECTION% = 5 Then Return
ReDim boxFiles$(0 To Count%)
For X = 1 To Count%
    boxFiles$(X) = Space$(12)
Next
boxFiles$(0) = Spec$
Call ReadFile(boxFiles$())
For X = 1 To Count%
    Kill (DRI$(1) + boxFiles$(X))
    Locate 12, 34: Print "DELETING "; boxFiles$(X)
Next
Return

5060 Spec$ = DRI$(1) + "*.SPC"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4010
ReDim boxFiles$(0 To Count%)
For X = 1 To Count%
    boxFiles$(X) = Space$(12)
Next
boxFiles$(0) = Spec$
Call ReadFile(boxFiles$())
'CALL SortStr(VARPTR(boxFiles$(1)), Count, 0)

5065 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT BOXSCORE TO VIEW": Color 15
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
Print "-SELECTS BOXSCORE";

_MouseShow "DEFAULT"
MAMenu boxFiles$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If EXITCODE% = 27 Then 4500
targetFile$ = boxFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3912 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 5065
BN$ = DRI$(1) + targetFile$
ReDim array$(500) 'holds the main editor string array
'GOSUB 5050
GoTo 5060

'EDIT A BOXSCORE LOADED FROM THE FILE NAME BN$

5050 If Not _FileExists(BN$) Then GoSub 4060: Return

Open BN$ For Input As #1
CURLINE = 1
While Not EOF(1)
    Line Input #1, array$(CURLINE)
    CURLINE = CURLINE + 1
Wend
Close #1

Cls '  show how QEdit saves the screen
    
Color 15, 0 'print instructions on bottom of screen
Locate 25, 1
Print " ESC - Quit    F1 - Help    F2 - Print Boxscore   F3 - Save Boxscore"; Tab(79);

'----- fill in editing parameters
Select Case Monitor% 'determine the monitor type
    Case 3, 5, 7, 9, 10, 12 'CGA, EGA, VGA monitors
        Ed.AColor = 23 'white on blue
    Case Else 'all other monitors
        Ed.AColor = 7 'white on black
End Select
Ed.Wrap = 0 'word wrap at column 65 (use 0 for to disable word wrap)
Ed.Frame = 0 'frame code (1 = show ruler line & scroll bars, 0 = don't)
Ed.Rows = 24 'number of text lines to display on the screen (3 - 25)
Ed.Wide = 80 'width of the active screen (10 - 80)
Ed.HTab = 4 'set tab stops to 4 spaces
Locate 1, 1 'locate the upper left corner of the edit window frame
Action = 1 'Action 1 to save underlying screen and
'  display text, operate in polled mode

'----- main loop for editing
Do
    '----- call QEdit in polled mode
    'CALL QEdit(array$(), Ky$, Action, Ed)
    '----- F2 function key pressed? - change margin
    If Ky$ = F2$ Then: For I = 1 To CURLINE - 1: LPrint array$(I): Next
    If Ky$ = F3$ Then
        Open BN$ For Output As #1
        For I = 1 To CURLINE - 1
            Print #1, array$(I)
        Next
        Close 1

        Call CSMsgBox("REVISED BOXSCORE HAS BEEN SAVED...PRESS ANY KEY TO CONTINUE", 31, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    End If

Loop Until Ky$ = Chr$(27) 'keep calling QEdit until Esc is pressed

'----- call QEdit again with an Action flag of 5 to restore the screen
'CALL QEdit(array$(), Ky$, 5, Ed)

Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 33: Color 14
    Print "AUTO SCHEDULER"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1995";
    Locate 5, 1: Color , 0
End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
