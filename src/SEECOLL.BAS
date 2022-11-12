SUB SEECOLL ()

_Title "3-in-1 Football - View Conference Stats"

Dim L(60), LD(240), LZ(240), PTSF(60), PTSA(60), T(60), TT(10, 20, 9), W(60)
'Dim O2(60)
'Dim O$(20), H1$(60), Z5$(40), H$(20)
'Dim O%(20), ZM%(40)
Dim P$(60), T$(60), TR$(10, 20), Z2$(240), Z3$(240), Z4$(240)

5 '

Do

	4000 Spec$ = diskPaths$(2) + "*.LDR"
	Count% = FCount%(Spec$)

	If Count% = 0 Then 
		4500 Locate 10, 8
		Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
		I$ = GetKeyPress$
		Call CSMsgBox("", 0, Pass)
		GoTo 2900
	Else

	'--- start edits here

	ReDim leagueLeaderFiles$(0 To Count%)
	For X = 1 To Count%
		leagueLeaderFiles$(X) = Space$(12)
	Next
	leagueLeaderFiles$(0) = Spec$
	Call ReadFile(leagueLeaderFiles$())
	'CALL SortStr(VARPTR(leagueLeaderFiles$(1)), Count%, 0)

	Do

		1310 Cls
		Color , 1
		Cls
		Locate , 28
		Color 14
		Print "SELECT CONFERENCE TO VIEW": Color 15
		Selection% = 1
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
		Print "-SELECTS CONFERENCE";

		_MouseShow "DEFAULT"
		MAMenu leagueLeaderFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
		_MouseHide

		If ExitCode% = 27 Then Exit Sub

		targetFile$ = leagueLeaderFiles$(Selection%)
		Locate 2, 33: Print targetFile$
		Locate , 33: Print "ANY CHANGE (YN)";
		Color 15, 0
		3910 I$ = GetKeyPress$
		'If UCase$(I$) = "Y" Then 1310
	Loop Until UCase$(I$) = "N"

	3911 targetFile$ = RTrim$(targetFile$)
	WE$ = Left$(targetFile$, Len(targetFile$) - 4)
	'GoTo 7

	7 WE$ = Left$(WE$, 8)
	Open diskPaths$(2) + WE$ + ".TMS" For Input As #2
	Input #2, FF$, D1$, D2$
	Close 2
	GoSub 200
	Cls
	Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

	Open diskPaths$(2) + WE$ + ".LDR" For Input As #2
	For X = 1 To 60
		Input #2, T$(X), W(X), L(X), T(X), PTSF(X), PTSA(X)
	Next
	For X = 1 To 240
		Input #2, Z2$(X), Z3$(X), LD(X), LZ(X)
	Next
	For I = 1 To 10
		For I1 = 1 To 20
			For I2 = 1 To 9
				Input #2, TT(I, I1, I2)
	Next: Next: Next
	For I = 1 To 10
		For I1 = 1 To 20
			Input #2, TR$(I, I1)
	Next: Next
	Close 2
	GoTo 1119

	200 L = 8: If Len(D1$) < L Then D1$ = D1$ + " ": GoTo 200

	201 L = 8: If Len(D2$) < L Then D2$ = D2$ + " ": GoTo 201

	204 Return


	900 If Len(P$(P)) < L Then P$(P) = P$(P) + " ": GoTo 900
	Return


	930 A = Int(1000 * A + .5)
	If A = 1000 Then P$(P) = P$(P) + "1.000 ": Return

	A$ = Str$(A)
	L = Len(A$)
	A$ = Right$(A$, L - 1)

	936 If Len(A$) < 3 Then A$ = "0" + A$: GoTo 936
	A$ = " ." + Right$(A$, 3)
	P$(P) = P$(P) + A$
	Return


	960 A$ = "     " + Str$(A)
	A$ = Right$(A$, W)
	P$(P) = P$(P) + A$
	Return


	1119 D1 = W(1) - L(1)
	For P = 1 To 15: If T$(P) = "*" Or T$(P) = "" Then P$(P) = "": GoTo 1151
		P$(P) = T$(P): L = 12: GoSub 900: A = Int(W(P)): W = 3: GoSub 960: L = 16: GoSub 900
		A = Int(L(P)): W = 3: GoSub 960: L = 20: GoSub 900: A = Int(T(P)): GoSub 960: L = 24: GoSub 900: A = 0
		If W(P) + L(P) > 0 Then A = Int((W(P) + .5 * T(P)) / (W(P) + L(P) + T(P)) * 1000 + .5) / 1000
		GoSub 930: L = 30: GoSub 900: A = PTSF(P): W = 3: GoSub 960: A = PTSA(P): W = 4: GoSub 960
		P$(P) = P$(P)

	1151 Next P
	D1 = W(16) - L(16)
	For P = 16 To 30: If T$(P) = "*" Or T$(P) = "" Then P$(P) = "": GoTo 1152
		P$(P) = T$(P): L = 12: GoSub 900: A = Int(W(P)): W = 3: GoSub 960: L = 16: GoSub 900
		A = Int(L(P)): W = 3: GoSub 960: L = 20: GoSub 900: A = Int(T(P)): GoSub 960: L = 24: GoSub 900: A = 0
		If W(P) + L(P) > 0 Then A = Int(W(P) / (W(P) + L(P) + T(P)) * 1000 + .5) / 1000
		GoSub 930: L = 30: GoSub 900: A = PTSF(P): W = 3: GoSub 960: A = PTSA(P): W = 4: GoSub 960
		P$(P) = P$(P)

	1152 Next P
	D1 = W(31) - L(31)
	For P = 31 To 45: If T$(P) = "*" Or T$(P) = "" Then P$(P) = "": GoTo 1153
		P$(P) = T$(P): L = 12: GoSub 900: A = Int(W(P)): W = 3: GoSub 960: L = 16: GoSub 900
		A = Int(L(P)): W = 3: GoSub 960: L = 20: GoSub 900: A = Int(T(P)): GoSub 960: L = 24: GoSub 900: A = 0
		If W(P) + L(P) > 0 Then A = Int((W(P) + .5 * T(P)) / (W(P) + L(P) + T(P)) * 1000 + .5) / 1000
		GoSub 930: L = 30: GoSub 900: A = PTSF(P): W = 3: GoSub 960: A = PTSA(P): W = 4: GoSub 960
		P$(P) = P$(P)

	1153 Next P
	D1 = W(46) - L(46)
	For P = 46 To 60: If T$(P) = "*" Or T$(P) = "" Then P$(P) = "": GoTo 1154
		P$(P) = T$(P): L = 12: GoSub 900: A = Int(W(P)): W = 3: GoSub 960: L = 16: GoSub 900
		A = Int(L(P)): W = 3: GoSub 960: L = 20: GoSub 900: A = Int(T(P)): GoSub 960: L = 24: GoSub 900: A = 0
		If W(P) + L(P) > 0 Then A = Int(W(P) / (W(P) + L(P) + T(P)) * 1000 + .5) / 1000
		GoSub 930: L = 30: GoSub 900: A = PTSF(P): W = 3: GoSub 960: A = PTSA(P): W = 4: GoSub 960
		P$(P) = P$(P)

	1154 Next P

	1202 Cls
	Call TITLE_STATS
	J = 5: GoSub 2075: Color 7
	Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
	Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
	Locate , 30: Print "STATS TO ";: Color 15: Print "F";: Color 7: Print "ILE"
	8651 I$ = GetKeyPress$
	Color 14, 0
	If UCase$(I$) = "P" Then 3200
	If UCase$(I$) = "F" Then X1 = 1: GoTo 3100

	1200 Cls
	Color 12: Print FF$ + " OVERALL RECORD"
	If D1$ <> "" Then Print D1$; " DIVISION"
	Locate 2, 41
	If D2$ <> "" Then Print D2$; " DIVISION"
	Locate , 1: Color 14: Print "              W   L   T   PCT  PF  PA": Color 15
	For P = 1 To 15: If P$(P) = "" Then RT = RT + 1: GoTo 1256
		Locate (P + 3 - RT): Print T$(P); Tab(14); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print "  .---";: GoTo 1255
		If L(P) + T(P) = 0 Then Print " 1.000";: GoTo 1255
		If W(P) = 0 Then Print "  .000";: GoTo 1255
		Print Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		1255 Print Tab(31); Using "### ###"; PTSF(P); PTSA(P)

	1256 Next
	RT = 0
	If D2$ = "" Then 1257
	Locate 3, 41
	Color 14: Print "              W   L   T   PCT  PF  PA": Color 15
	For P = 16 To 30
		If P$(P) = "" Then RT = RT + 1: GoTo 1258
		Locate (P - 12 - RT), 41
		Print T$(P); Tab(54); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print "  .---";: GoTo 1260
		If L(P) + T(P) = 0 Then Print " 1.000";: GoTo 1260
		If W(P) = 0 Then Print "  .000";: GoTo 1260
		Print Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		1260 Print Tab(71); Using "### ###"; PTSF(P); PTSA(P)

	1258 Next
	RT = 0

	1257 GoSub 1930
	Cls
	Color 12
	Print FF$ + " CONFERENCE RECORD"
	If D1$ <> "" Then Print D1$; " DIVISION"
	Locate 2, 41
	If D2$ <> "" Then Print D2$; " DIVISION"
	Locate , 1: Color 14: Print "              W   L   T   PCT  PF  PA": Color 15
	For P = 31 To 45
		If P$(P) = "" Then RT = RT + 1: GoTo 1266
		Locate (P - 27 - RT): Print T$(P); Tab(14); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print "  .---";: GoTo 1259
		If L(P) + T(P) = 0 Then Print " 1.000";: GoTo 1259
		If W(P) = 0 Then Print "  .000";: GoTo 1259
		Print Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		1259 Print Tab(31); Using "### ###"; PTSF(P); PTSA(P)

	1266 Next
	RT = 0
	If D2$ = "" Then 1265
	Locate 3, 41
	Color 14: Print "              W   L   T   PCT  PF  PA": Color 15
	For P = 46 To 60
		If P$(P) = "" Then RT = RT + 1: GoTo 1262
		Locate (P - 42 - RT), 41
		Print T$(P); Tab(54); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print "  .---";: GoTo 1261
		If L(P) + T(P) = 0 Then Print " 1.000";: GoTo 1261
		If W(P) = 0 Then Print "  .000";: GoTo 1261
		Print Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		1261 Print Tab(71); Using "### ###"; PTSF(P); PTSA(P)

	1262 Next
	RT = 0

	1265 GoSub 1930
	Cls: Color 14: Print "RUSHING LEADERS (MIN 10 ATT/G)"
	Color 12: Print "                            AVG  ATT": Color 15
	For P = 1 To 20
		If Z2$(P) = "*" Then 1320
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "##.# ####"; LD(P); LZ(P)

	1320 Next
	Locate 1, 41: Color 14: Print "RECEIVING LEADERS (MIN 4 REC/G)"
	Locate 2, 41: Color 12: Print "                            AVG  REC": Color 15
	For P = 21 To 40
		If Z2$(P) = "*" Then 1327
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "##.#  ###"; LD(P); LZ(P)

	1327 Next
	GoSub 1900
	Cls
	Color 14: Print "RUSHING YARDS TOTAL": Color 15
	For P = 41 To 50
		If Z2$(P) = "*" Then 1360
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "####"; LD(P)

	1360 Next
	Locate 1, 41: Color 14: Print "RECEIVING YARDS TOTAL": Color 15
	For P = 51 To 60
		If Z2$(P) = "*" Then 1420
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "####"; LD(P)

	1420 Next
	Locate 13: Color 14: Print "RUSHING ATTEMPTS": Color 15
	For P = 81 To 90
		If Z2$(P) = "*" Then 1425
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "####"; LD(P)

	1425 Next
	Locate 13, 41: Color 14: Print "TOTAL RECEPTIONS": Color 15
	For P = 91 To 100
		If Z2$(P) = "*" Then 1434
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "####"; LD(P)

	1434 Next
	GoSub 1900
	Cls
	Color 14: Print "ALL PURPOSE YARDAGE": Color 15
	For P = 101 To 110
		If Z2$(P) = "*" Then 1464
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "####"; LD(P)

	1464 Next
	Locate 1, 41: Color 14: Print "SCORING NON-KICKERS": Color 15
	For P = 111 To 120
		If Z2$(P) = "*" Then 1469
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "####"; LD(P)

	1469 Next
	Locate 13: Color 14: Print "RUSHING TOUCHDOWNS": Color 15
	For P = 61 To 70
		If Z2$(P) = "*" Then 1474
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "####"; LD(P)

	1474 Next
	Locate 13, 41: Color 14: Print "RECEIVING TOUCHDOWNS": Color 15
	For P = 71 To 80
		If Z2$(P) = "*" Then 1479
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "####"; LD(P)

	1479 Next
	GoSub 1900
	Cls
	Color 14: Print "COMPLETION PERCENTAGE": Color 15
	For P = 121 To 130
		If Z2$(P) = "*" Then 1494
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "###.#"; LD(P)

	1494 Next
	Locate 1, 41: Color 14: Print "QUARTERBACK RATING": Color 15
	For P = 131 To 140
		If Z2$(P) = "*" Then 1504
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "###.#"; LD(P)

	1504 Next
	Locate 13: Color 14: Print "PASSING YARDAGE": Color 15
	For P = 141 To 150
		If Z2$(P) = "*" Then 1509
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "####"; LD(P)

	1509 Next
	Locate 13, 41: Color 14: Print "PASSING TOUCHDOWNS": Color 15
	For P = 151 To 160
		If Z2$(P) = "*" Then 1514
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "####"; LD(P)

	1514 Next
	GoSub 1900
	Cls
	Color 14: Print "KO RETURN AVERAGE                 #": Color 15
	For P = 161 To 170
		If Z2$(P) = "*" Then 1604
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "##.#  ##"; LD(P); LZ(P)

	1604 Next
	Locate 1, 41: Color 14: Print "PUNT RETURN AVERAGE               #"
	Color 15
	For P = 171 To 180
		If Z2$(P) = "*" Then 1609
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "##.#  ##"; LD(P); LZ(P)

	1609 Next
	Locate 13: Color 14: Print "PUNTING AVERAGE": Color 15
	For P = 181 To 190
		If Z2$(P) = "*" Then 1614
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "##.#  ###"; LD(P); LZ(P)

	1614 Next
	Locate 13, 41: Color 14: Print "SCORING (KICKERS)": Color 15
	For P = 191 To 200
		If Z2$(P) = "*" Then 1619
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "###"; LD(P)

	1619 Next
	GoSub 1900
	Cls
	Color 14: Print "FIELD GOAL %                     FGA": Color 15
	For P = 201 To 210
		If Z2$(P) = "*" Then 1624
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "###.#  ##"; LD(P); LZ(P)

	1624 Next
	Locate 1, 41: Color 14: Print "EXTRA POINT %                    EPA": Color 15
	For P = 211 To 220
		If Z2$(P) = "*" Then 1629
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "###.#  ##"; LD(P); LZ(P)

	1629 Next
	Locate 13: Color 14: Print "INTERCEPTIONS": Color 15
	For P = 221 To 230
		If Z2$(P) = "*" Then 1740
		Print Z2$(P) + ", " + Z3$(P);: Locate , 28
		Print Using "##"; LD(P)

	1740 Next
	Locate 13, 41: Color 14: Print "SACKS": Color 15
	For P = 231 To 240
		If Z2$(P) = "*" Then 1790
		Locate , 41: Print Z2$(P) + ", " + Z3$(P);: Locate , 68
		Print Using "##"; LD(P)

	1790 Next
	GoSub 1900

	1792 Cls
	Color 14: Print "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G": Color 15
	CA = 1
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1793
		Print TR$(CA, P);: Locate , 15
		Print Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

	1793 Next
	Locate 1, 41: Color 14: Print "TOTAL DEFENSE  G PLAYS  YDS  AVG YDS/G": Color 15
	CA = 2
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1794
		Locate , 41: Print TR$(CA, P);: Locate , 55
		Print Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

	1794 Next
	GoSub 1900
	Cls
	Color 14: Print "RUSHING OFF    G PLAYS  YDS  AVG YDS/G": Color 15
	CA = 3
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1795
		Print TR$(CA, P);: Locate , 15
		Print Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

	1795 Next
	Locate 1, 41: Color 14: Print "RUSHING DEF    G PLAYS  YDS  AVG YDS/G": Color 15
	CA = 4
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1796
		Locate , 41: Print TR$(CA, P);: Locate , 55
		Print Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

	1796 Next
	GoSub 1900
	Cls
	Color 14: Print "SCORING OFF    G  POINTS    AVG": Color 15
	CA = 5
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1797
		Print TR$(CA, P);: Locate , 15
		Print Using "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3)

	1797 Next
	Locate 1, 41: Color 14: Print "SCORING DEF    G  POINTS    AVG": Color 15
	CA = 6
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1798
		Locate , 41: Print TR$(CA, P);: Locate , 55
		Print Using "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3)

	1798 Next
	GoSub 1900
	Cls
	Color 14: Print "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G": Color 15
	CA = 7
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1799
		Print TR$(CA, P);: Locate , 15
		Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

	1799 Next
	GoSub 1900
	Cls
	Color 14: Print "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G": Color 15
	CA = 8
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1800
		Print TR$(CA, P);: Locate , 15
		Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

	1800 Next
	GoSub 1900
	Cls
	Color 14: Print "PUNTING        NO     AVG": Color 15
	CA = 9
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1801
		Print TR$(CA, P);: Locate , 15
		Print Using "###    ##.#"; TT(CA, P, 1); TT(CA, P, 2)

	1801 Next
	GoSub 1900
	Cls
	Color 14
	Print "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
	Print "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "
	Color 15
	CA = 10
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 1802
		Print TR$(CA, P);: Locate , 18
		Print Using "### ###  ###    ### ###  ###       ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5); TT(CA, P, 6); TT(CA, P, 7)

	1802 Next
	GoSub 1900

	3900 'GoSub 4600
	4600 JJ = 4
	Locate 6, 8: Color 15, 1
	Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
	For II = 1 To JJ: Locate 6 + II, 8
		Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
	Next
	Locate 7 + JJ, 8
	Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
	U$ = "VIEW CONFERENCE STATS AGAIN (YN)"
	Locate , (80 - Len(U$)) / 2
	Print U$
	Color 15, 0

	1820 A$ = GetKeyPress$
	If UCase$(A$) = "Y" Then 1202
	GoTo 1310

	1880 L = 26: If Len(Z4$(P)) < L Then Z4$(P) = Z4$(P) + " ": GoTo 1880
	Return


	1900 Locate 24: Print "HIT ANY KEY TO CONTINUE!!";

	1910 QK$ = GetKeyPress$
	Return


	1930 Locate 23: Print "HIT ANY KEY TO CONTINUE!!";

	1940 QK$ = GetKeyPress$
	Return


	2900 Exit Sub



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


	3100 Color 15, 0: Cls
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
	Locate , 12: Print "ENTER NAME FOR FILE: ": Print
	Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
	Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
	Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
	Locate 7, 37: Input SF$
	If SF$ = "" Then 3100
	Locate 13, 12: Print "ANY CHANGE (YN) "
	705 I$ = GetKeyPress$
	If UCase$(I$) = "Y" Then 3100
	Color 15, 0

	3200 Cls
	If X1 <> 1 Then Open "REPORT" For Output As #2 Else Open SF$ + ".RPT" For Output As #2
	Print #2, FF$ + " OVERALL RECORD"
	If D1$ <> "" Then Print #2, D1$; " DIVISION"
	Print #2, "              W   L   T   PCT  PF  PA"
	For P = 1 To 15
		If P$(P) = "" Then 3255
		Print #2, T$(P); Tab(14); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print #2, "  .---";: GoTo 3256
		If L(P) + T(P) = 0 Then Print #2, " 1.000";: GoTo 3256
		If W(P) = 0 Then Print #2, "  .000";: GoTo 3256
		Print #2, Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		3256 Print #2, Tab(31); Using "### ###"; PTSF(P); PTSA(P)

	3255 Next
	Print #2,
	If D2$ <> "" Then Print #2, D2$; " DIVISION"
	Print #2, "              W   L   T   PCT  PF  PA"
	For P = 16 To 30
		If P$(P) = "" Then 3257
		Print #2, T$(P); Tab(14); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print #2, "  .---";: GoTo 3258
		If L(P) + T(P) = 0 Then Print #2, " 1.000";: GoTo 3258
		If W(P) = 0 Then Print #2, "  .000";: GoTo 3258
		Print #2, Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		3258 Print #2, Tab(31); Using "### ###"; PTSF(P); PTSA(P)

	3257 Next
	Print #2,
	Print #2, FF$ + " CONFERENCE RECORD"
	If D1$ <> "" Then Print #2, D1$; " DIVISION"
	Print #2, "              W   L   T   PCT  PF  PA"
	For P = 31 To 45
		If P$(P) = "" Then 3259
		Print #2, T$(P); Tab(14); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print #2, "  .---";: GoTo 3260
		If L(P) + T(P) = 0 Then Print #2, " 1.000";: GoTo 3260
		If W(P) = 0 Then Print #2, "  .000";: GoTo 3260
		Print #2, Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		3260 Print #2, Tab(31); Using "### ###"; PTSF(P); PTSA(P)

	3259 Next
	Print #2,
	If D2$ <> "" Then Print #2, D2$; " DIVISION"
	Print #2, "              W   L   T   PCT  PF  PA"
	For P = 46 To 60
		If P$(P) = "" Then 3261
		Print #2, T$(P); Tab(14); Using "##  ##  ##"; W(P); L(P); T(P);
		If W(P) + L(P) + T(P) = 0 Then Print #2, "  .---";: GoTo 3262
		If L(P) + T(P) = 0 Then Print #2, " 1.000";: GoTo 3262
		If W(P) = 0 Then Print #2, "  .000";: GoTo 3262
		Print #2, Using "  .###"; W(P) / (W(P) + L(P) + T(P));

		3262 Print #2, Tab(31); Using "### ###"; PTSF(P); PTSA(P)

	3261 Next
	Print #2, Chr$(12)
	Print #2, "RUSHING LEADERS (MIN 10 ATT/G)"; Tab(41); "RECEIVING LEADERS (MIN 4 REC/G)"
	Print #2, "                            AVG  ATT"; Tab(41); "                            AVG  REC"
	For P = 1 To 20
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "##.# ####"; LD(P); LZ(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 20) = "*" Then
			Print #2,
			GoTo 3320
		Else
			Print #2, Z2$(P + 20) + ", " + Z3$(P + 20);
			Print #2, Tab(68);
			Print #2, Using "##.# ####"; LD(P + 20); LZ(P + 20)
		End If

	3320 Next
	Print #2,
	Print #2, "RUSHING YARDS TOTAL"; Tab(41); "RECEIVING YARDS TOTAL"
	For P = 41 To 50
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "####"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3360
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "####"; LD(P + 10)
		End If

	3360 Next
	Print #2,
	Print #2, "RUSHING ATTEMPTS"; Tab(41); "TOTAL RECEPTIONS"
	For P = 81 To 90
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "####"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3425
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "####"; LD(P + 10)
		End If

	3425 Next
	Print #2,
	Print #2, "ALL PURPOSE YARDAGE"; Tab(41); "SCORING NON-KICKERS"
	For P = 101 To 110
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "####"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3464
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "####"; LD(P + 10)
		End If

	3464 Next
	Print #2, Chr$(12)
	Print #2, "RUSHING TOUCHDOWNS"; Tab(41); "RECEIVING TOUCHDOWNS"
	For P = 61 To 70
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "####"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3474
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "####"; LD(P + 10)
		End If

	3474 Next
	Print #2,
	Print #2, "COMPLETION PERCENTAGE"; Tab(41); "QUARTERBACK RATING"
	For P = 121 To 130
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "###.#"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3494
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "###.#"; LD(P + 10)
		End If

	3494 Next
	Print #2,
	Print #2, "PASSING YARDAGE"; Tab(41); "PASSING TOUCHDOWNS"
	For P = 141 To 150
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "####"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3509
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "####"; LD(P + 10)
		End If

	3509 Next
	Print #2,
	Print #2, "KO RETURN AVERAGE                 #"; Tab(41); "PUNT RETURN AVERAGE               #"
	For P = 161 To 170
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "##.#  ##"; LD(P); LZ(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3604
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "##.#  ##"; LD(P + 10); LZ(P + 10)
		End If

	3604 Next
	Print #2,
	Print #2, "PUNTING AVERAGE"; Tab(41); "SCORING (KICKERS)"
	For P = 181 To 190
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "##.#  ###"; LD(P); LZ(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3619
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "###"; LD(P + 10)
		End If

	3619 Next
	Print #2,
	Print #2, "FIELD GOAL %                     FGA"; Tab(41); "EXTRA POINT %                   EPA"
	For P = 201 To 210
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "###.#  ###"; LD(P); LZ(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3624
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "###.#  ##"; LD(P + 10); LZ(P + 10)
		End If

	3624 Next
	Print #2,
	Print #2, "INTERCEPTIONS"; Tab(41); "SACKS"
	For P = 221 To 230
		If Z2$(P) = "*" Then
			Print #2, Tab(41);
		Else
			Print #2, Z2$(P) + ", " + Z3$(P);
			Print #2, Tab(28);
			Print #2, Using "##"; LD(P);
			Print #2, Tab(41);
		End If
		If Z2$(P + 10) = "*" Then
			Print #2,
			GoTo 3640
		Else
			Print #2, Z2$(P + 10) + ", " + Z3$(P + 10);
			Print #2, Tab(68);
			Print #2, Using "##"; LD(P + 10)
		End If

	3640 Next
	Print #2, Chr$(12)
	Print #2, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; Tab(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"
	For P = 1 To 20
		CA = 1
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
			Print #2, Tab(41);
		Else
			Print #2, TR$(CA, P); Tab(15);
			Print #2, Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5);
			Print #2, Tab(41);
		End If
		CA = 2
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
			Print #2,
			GoTo 3793
		Else
			Print #2, TR$(CA, P); Tab(56);
			Print #2, Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)
		End If

	3793 Next
	Print #2,
	Print #2, "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"; Tab(41); "RUSHING DEF     G PLAYS  YDS  AVG YDS/G"
	For P = 1 To 20
		CA = 3
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
			Print #2, Tab(41);
		Else
			Print #2, TR$(CA, P); Tab(15);
			Print #2, Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5);
			Print #2, Tab(41);
		End If
		CA = 4
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
			Print #2,
			GoTo 3795
		Else
			Print #2, TR$(CA, P); Tab(56);
			Print #2, Using "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)
		End If

	3795 Next
	Print #2, Chr$(12)
	Print #2, "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
	CA = 7
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 3799
		Print #2, TR$(CA, P); Tab(15);
		Print #2, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

	3799 Next
	Print #2,
	Print #2, "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
	CA = 8
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 3800
		Print #2, TR$(CA, P); Tab(15);
		Print #2, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

	3800 Next
	Print #2, Chr$(12)
	Print #2, "SCORING OFF    G  POINTS    AVG"; Tab(41); "SCORING DEF     G  POINTS    AVG"
	For P = 1 To 20
		CA = 5
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
			Print #2, Tab(41);
		Else
			Print #2, TR$(CA, P); Tab(15);
			Print #2, Using "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3);
			Print #2, Tab(41);
		End If
		CA = 6
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
			Print #2,
			GoTo 3797
		Else
			Print #2, TR$(CA, P); Tab(56);
			Print #2, Using "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3)
		End If

	3797 Next
	Print #2,
	Print #2, "PUNTING        NO     AVG"
	CA = 9
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 3801
		Print #2, TR$(CA, P); Tab(15);
		Print #2, Using "###    ##.#"; TT(CA, P, 1); TT(CA, P, 2)

	3801 Next
	Print #2, Chr$(12)
	Print #2, "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
	Print #2, "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "
	CA = 10
	For P = 1 To 20
		If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then 3802
		Print #2, TR$(CA, P); Tab(18);
		Print #2, Using "### ###  ###    ### ###  ###       ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5); TT(CA, P, 6); TT(CA, P, 7)

	3802 Next
	Close 2
	If X1 = 1 Then 3900
	Open "REPORT" For Input As #2
	Do While Not EOF(2)
		Line Input #2, X$
		LPrint X$
	Loop
	Close 2
	LPrint Chr$(12)
	GoTo 3900

	'--- end edits here

	End If

'...Until we press ESC
Loop Until ExitCode% = 27 Or Count% = 0

END SUB
