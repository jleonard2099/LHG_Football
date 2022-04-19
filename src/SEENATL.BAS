'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - View Org Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DEFINT A-Z
DECLARE SUB TITLE ()

DIM PTSF(60), PTSA(60), O2(60), Z2$(240), H1$(60), Z3$(240)
DIM T$(60), W(60), L(60), T(60), Z5$(40), LD(240) AS SINGLE, ZM%(40), H$(20)
DIM LZ(240) AS SINGLE, P$(60), Z4$(240), TT(10, 20, 9) AS SINGLE, TR$(10, 20)
DIM PT(1200, 5) AS SINGLE, PR$(1200), D$(2)

5 OPEN "DEV.BAS" FOR INPUT AS #1: FOR I = 0 TO 2: INPUT #1, D$(I): NEXT: CLOSE 1

4000 Spec$ = D$(2) + "*.CON"
	COUNT% = FCount%(Spec$)
	IF COUNT% = 0 THEN 4500
REDIM conferenceFiles$0 TO COUNT%)
	FOR X = 1 TO COUNT%
	  conferenceFiles$X) = SPACE$(12)
	NEXT
	conferenceFiles$0) = Spec$
	CALL ReadFile(conferenceFiles$))
	'CALL SortStr(VARPTR(conferenceFiles$1)), COUNT%, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE TO VIEW": COLOR 15
	SELECTION% = 1
	START% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1
	COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS LEAGUE";

_MouseShow "DEFAULT"
20 MAMenu conferenceFiles$), SELECTION%, START%, COUNT%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN 2900
	IF EXITCODE% = -71 THEN START% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
	IF EXITCODE% = -79 THEN START% = COUNT%: EXITCODE% = 3: SELECTION% = COUNT%: GOTO 20
	IF EXITCODE% = -73 THEN START% = START% - 80: IF START% < 1 THEN START% = 1
	IF EXITCODE% = -81 THEN START% = START% + 80: IF START% > COUNT% THEN START% = COUNT%
	IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = START%: GOTO 20
	targetFile$ = conferenceFiles$SELECTION%)
	LOCATE 2, 33: PRINT targetFile$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 targetFile$ = RTrim$(targetFile$)
	WE$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
	GOTO 7

4500 LOCATE 10, 8
	GOSUB 4600
	PRINT "NO LEAGUE FILES WERE FOUND"
	PRINT "PRESS ANY KEY TO RETURN TO MAIN MENU"
3805 I$ = GetKeyPress$
	GOTO 2900

7 WE$ = LEFT$(WE$, 8)
	CLS
	LOCATE 9, 1: PRINT "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"
OPEN D$(2) + WE$ + ".NLD" FOR INPUT AS #2
	FOR X = 1 TO 60
	INPUT #2, T$(X), W(X), L(X), T(X), PTSF(X), PTSA(X)
	NEXT
	FOR X = 1 TO 240
	INPUT #2, Z2$(X), Z3$(X), LD(X), LZ(X)
	NEXT
	FOR I = 1 TO 10
	FOR I1 = 1 TO 20
	FOR I2 = 1 TO 9
	INPUT #2, TT(I, I1, I2)
	NEXT: NEXT: NEXT
	FOR I = 1 TO 10
	FOR I1 = 1 TO 20
	INPUT #2, TR$(I, I1)
	NEXT: NEXT
CLOSE 2
OPEN D$(2) + WE$ + ".POL" FOR INPUT AS #2
	INPUT #2, NRT
	FOR X = 1 TO NRT
	INPUT #2, PR$(X), PT(X, 1), PT(X, 2), PT(X, 3), PT(X, 4), PT(X, 5)
	NEXT
CLOSE 2

1202     CLS : CALL TITLE
	J = 5: GOSUB 2075: COLOR 7
	LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "S"; : COLOR 7: PRINT "CREEN"
	LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "P"; : COLOR 7: PRINT "RINTER"
	LOCATE , 30: PRINT "STATS TO "; : COLOR 15: PRINT "F"; : COLOR 7: PRINT "ILE"
8651 I$ = GetKeyPress$
	COLOR 14, 0
	IF UCASE$(I$) = "P" THEN 3200
	IF UCASE$(I$) = "F" THEN X1 = 1:GOTO 3100

1200 CLS
	COLOR 12: PRINT FF$ + " OVERALL RANKINGS"
	LOCATE , 1
	COLOR 14: PRINT "                    RATING     W    L    T   STRENGTH OF SCHEDULE"
	COLOR 15
	FOR P = 1 TO NRT
	PRINT USING "###  "; P;
	PRINT PR$(P); TAB(22); USING "###.#    ##   ##   ##         ###.#"; PT(P, 1); PT(P, 2); PT(P, 3); PT(P, 4); PT(P, 5)
	IF P / 20 = INT(P / 20) THEN GOSUB 1950

1256 NEXT
	GOSUB 1900
	  CLS
	COLOR 14
	PRINT "RUSHING LEADERS (MIN 10 ATT/G)"
	COLOR 12: PRINT "                            AVG  ATT": COLOR 15
	FOR P = 1 TO 20
	IF Z2$(P) = "*" THEN 1320
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "##.# ####"; LD(P); LZ(P)

1320 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "RECEIVING LEADERS (MIN 4 REC/G)"
	LOCATE 2, 41: COLOR 12: PRINT "                            AVG  REC": COLOR 15
	FOR P = 21 TO 40
	IF Z2$(P) = "*" THEN 1327
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "##.#  ###"; LD(P); LZ(P)

1327 NEXT
	GOSUB 1900
	  CLS
	COLOR 14: PRINT "RUSHING YARDS TOTAL": COLOR 15
	FOR P = 41 TO 50
	IF Z2$(P) = "*" THEN 1360
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "####"; LD(P)

1360 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "RECEIVING YARDS TOTAL": COLOR 15
	FOR P = 51 TO 60
	IF Z2$(P) = "*" THEN 1420
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "####"; LD(P)

1420 NEXT
	  LOCATE 13: COLOR 14: PRINT "RUSHING ATTEMPTS": COLOR 15
	FOR P = 81 TO 90
	IF Z2$(P) = "*" THEN 1425
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "####"; LD(P)

1425 NEXT
	LOCATE 13, 41: COLOR 14: PRINT "TOTAL RECEPTIONS": COLOR 15
	FOR P = 91 TO 100
	IF Z2$(P) = "*" THEN 1434
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "####"; LD(P)

1434 NEXT
	GOSUB 1900
	  CLS
	COLOR 14: PRINT "ALL PURPOSE YARDAGE": COLOR 15
	FOR P = 101 TO 110
	IF Z2$(P) = "*" THEN 1464
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "####"; LD(P)

1464 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "SCORING NON-KICKERS": COLOR 15
	FOR P = 111 TO 120
	IF Z2$(P) = "*" THEN 1469
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "####"; LD(P)

1469 NEXT
	LOCATE 13: COLOR 14: PRINT "RUSHING TOUCHDOWNS": COLOR 15
	FOR P = 61 TO 70
	IF Z2$(P) = "*" THEN 1474
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "####"; LD(P)

1474 NEXT
	LOCATE 13, 41: COLOR 14: PRINT "RECEIVING TOUCHDOWNS": COLOR 15
	FOR P = 71 TO 80
	IF Z2$(P) = "*" THEN 1479
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "####"; LD(P)

1479 NEXT
	GOSUB 1900
	  CLS
	COLOR 14: PRINT "COMPLETION PERCENTAGE": COLOR 15
	FOR P = 121 TO 130
	IF Z2$(P) = "*" THEN 1494
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "###.#"; LD(P)

1494 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "QUARTERBACK RATING": COLOR 15
	FOR P = 131 TO 140
	IF Z2$(P) = "*" THEN 1504
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "###.#"; LD(P)

1504 NEXT
	LOCATE 13: COLOR 14: PRINT "PASSING YARDAGE": COLOR 15
	FOR P = 141 TO 150
	IF Z2$(P) = "*" THEN 1509
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "####"; LD(P)

1509 NEXT
	LOCATE 13, 41: COLOR 14: PRINT "PASSING TOUCHDOWNS": COLOR 15
	FOR P = 151 TO 160
	IF Z2$(P) = "*" THEN 1514
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "####"; LD(P)

1514 NEXT
	GOSUB 1900
	  CLS
	COLOR 14: PRINT "KO RETURN AVERAGE                 #": COLOR 15
	FOR P = 161 TO 170
	IF Z2$(P) = "*" THEN 1604
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "##.#  ##"; LD(P); LZ(P)

1604 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "PUNT RETURN AVERAGE               #"
	COLOR 15
	FOR P = 171 TO 180
	IF Z2$(P) = "*" THEN 1609
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "##.#  ##"; LD(P); LZ(P)

1609 NEXT
	LOCATE 13: COLOR 14: PRINT "PUNTING AVERAGE": COLOR 15
	FOR P = 181 TO 190
	IF Z2$(P) = "*" THEN 1614
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "##.#  ###"; LD(P); LZ(P)

1614 NEXT
	LOCATE 13, 41: COLOR 14: PRINT "SCORING (KICKERS)": COLOR 15
	FOR P = 191 TO 200
	IF Z2$(P) = "*" THEN 1619
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "###"; LD(P)

1619 NEXT
	GOSUB 1900
	  CLS
	COLOR 14: PRINT "FIELD GOAL %                     FGA": COLOR 15
	FOR P = 201 TO 210
	IF Z2$(P) = "*" THEN 1624
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "###.#  ##"; LD(P); LZ(P)

1624 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "EXTRA POINT %                    EPA": COLOR 15
	FOR P = 211 TO 220
	IF Z2$(P) = "*" THEN 1629
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "###.#  ##"; LD(P); LZ(P)

1629 NEXT
	LOCATE 13: COLOR 14: PRINT "INTERCEPTIONS": COLOR 15
	FOR P = 221 TO 230
	IF Z2$(P) = "*" THEN 1740
	PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 28
	PRINT USING "##"; LD(P)

1740 NEXT
	LOCATE 13, 41: COLOR 14: PRINT "SACKS": COLOR 15
	FOR P = 231 TO 240
	IF Z2$(P) = "*" THEN 1790
	LOCATE , 41: PRINT Z2$(P) + ", " + Z3$(P); : LOCATE , 68
	PRINT USING "##"; LD(P)

1790 NEXT
	GOSUB 1900

1792 CLS
	COLOR 14: PRINT "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G": COLOR 15
	CA = 1
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1793
	PRINT TR$(CA, P); : LOCATE , 15
	PRINT USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

1793 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "TOTAL DEFENSE  G PLAYS  YDS  AVG YDS/G": COLOR 15
	CA = 2
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1794
	LOCATE , 41: PRINT TR$(CA, P); : LOCATE , 55
	PRINT USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

1794 NEXT
	GOSUB 1900
	CLS
	COLOR 14: PRINT "RUSHING OFF    G PLAYS  YDS  AVG YDS/G": COLOR 15
	CA = 3
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1795
	PRINT TR$(CA, P); : LOCATE , 15
	PRINT USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

1795 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "RUSHING DEF    G PLAYS  YDS  AVG YDS/G": COLOR 15
	CA = 4
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1796
	LOCATE , 41: PRINT TR$(CA, P); : LOCATE , 55
	PRINT USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)

1796 NEXT
	GOSUB 1900
	CLS
	COLOR 14: PRINT "SCORING OFF    G  POINTS    AVG": COLOR 15
	CA = 5
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1797
	PRINT TR$(CA, P); : LOCATE , 15
	PRINT USING "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3)

1797 NEXT
	LOCATE 1, 41: COLOR 14: PRINT "SCORING DEF    G  POINTS    AVG": COLOR 15
	CA = 6
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1798
	LOCATE , 41: PRINT TR$(CA, P); : LOCATE , 55
	PRINT USING "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3)

1798 NEXT
	GOSUB 1900
	CLS
	COLOR 14: PRINT "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G": COLOR 15
	CA = 7
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1799
	PRINT TR$(CA, P); : LOCATE , 15
	PRINT USING "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

1799 NEXT
	GOSUB 1900
	CLS
	COLOR 14: PRINT "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G": COLOR 15
	CA = 8
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1800
	PRINT TR$(CA, P); : LOCATE , 15
	PRINT USING "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

1800 NEXT
	GOSUB 1900
	CLS
	COLOR 14: PRINT "PUNTING        NO     AVG": COLOR 15
	CA = 9
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1801
	PRINT TR$(CA, P); : LOCATE , 15
	PRINT USING "###    ##.#"; TT(CA, P, 1); TT(CA, P, 2)

1801 NEXT
	GOSUB 1900
	  CLS
	COLOR 14
	PRINT "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
	PRINT "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "
	COLOR 15
	CA = 10
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 1802
	PRINT TR$(CA, P); : LOCATE , 18
	PRINT USING "### ###  ###    ### ###  ###       ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5); TT(CA, P, 6); TT(CA, P, 7)

1802 NEXT
	GOSUB 1900

3900 GOSUB 4600
	U$ = "VIEW/PRINT NATIONAL STATS AGAIN (YN)"
	LOCATE , (80 - LEN(U$)) / 2
	PRINT U$
	COLOR 15, 0

1820 A$ = GetKeyPress$
	IF UCASE$(A$) = "Y" THEN 1202
	GOTO 1310

1880 L = 26: IF LEN(Z4$(P)) < L THEN Z4$(P) = Z4$(P) + " ": GOTO 1880
	RETURN

1900 LOCATE 24: PRINT "HIT ANY KEY TO CONTINUE!!";

1910 QK$=GetKeyPress$
	RETURN

1930 LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE!!";

1940 QK$=GetKeyPress$
	RETURN

1950 LOCATE 24: PRINT "HIT ANY KEY TO CONTINUE!!";

1955 QK$=GetKeyPress$
	CLS
	COLOR 12: PRINT FF$ + " OVERALL RANKINGS"
	LOCATE , 1
	COLOR 14: PRINT "                    RATING     W    L    T   STRENGTH OF SCHEDULE"
	COLOR 15
	RETURN

2900 RUN "HELLO.EXE 3"

4600 JJ = 4
	LOCATE 6, 8: COLOR 15, 1
	PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
	FOR II = 1 TO JJ: LOCATE 6 + II, 8
	Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
	Next
	LOCATE 7 + JJ, 8
	PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
	RETURN

2075 '
	LOCATE 8, 8: COLOR 15, 1
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
	LOCATE 10, 15
	RETURN

3100 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 1
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
	LOCATE , 12: PRINT "ENTER NAME FOR FILE: ": PRINT
	LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
	LOCATE , 12: PRINT "SPACES. DO NOT ADD A FILE EXTENSION, THE PROGRAM"
	LOCATE , 12: PRINT "WILL AUTOMATICALLY ADD .RPT"
	LOCATE 7, 37: INPUT SF$
	IF SF$ = "" THEN 3100
	LOCATE 13, 12: PRINT "ANY CHANGE (YN) "
705 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 3100
	COLOR 15, 0

3200 CLS
	IF X1<>1 THEN OPEN "REPORT" FOR OUTPUT AS #2 ELSE OPEN SF$ + ".RPT" FOR OUTPUT AS #2
	PRINT #2, FF$ + " OVERALL RANKINGS"
	PRINT #2, "                    RATING     W    L    T   STRENGTH OF SCHEDULE"
	FOR P = 1 TO NRT
	PRINT #2, USING "###  "; P;
	PRINT #2, PR$(P); TAB(22); USING "###.#    ##   ##   ##         ###.#"; PT(P, 1); PT(P, 2); PT(P, 3); PT(P, 4); PT(P, 5)
	NEXT
	PRINT #2, CHR$(12)
	  PRINT #2, "RUSHING LEADERS (MIN 10 ATT/G)"; TAB(41); "RECEIVING LEADERS (MIN 4 REC/G)"
	PRINT #2, "                            AVG  ATT"; TAB(41); "                            AVG  REC"
	FOR P = 1 TO 20
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "##.# ####"; LD(P); LZ(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 20) = "*" THEN
		PRINT #2,
		GOTO 3320
	ELSE
		PRINT #2, Z2$(P + 20) + ", " + Z3$(P + 20);
		PRINT #2, TAB(68);
		PRINT #2, USING "##.# ####"; LD(P + 20); LZ(P + 20)
	END IF

3320 NEXT
	PRINT #2,
	PRINT #2, "RUSHING YARDS TOTAL"; TAB(41); "RECEIVING YARDS TOTAL"
	FOR P = 41 TO 50
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "####"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3360
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "####"; LD(P + 10)
	END IF

3360 NEXT
	PRINT #2,
	PRINT #2, "RUSHING ATTEMPTS"; TAB(41); "TOTAL RECEPTIONS"
	FOR P = 81 TO 90
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "####"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3425
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "####"; LD(P + 10)
	END IF

3425 NEXT
	PRINT #2,
	PRINT #2, "ALL PURPOSE YARDAGE"; TAB(41); "SCORING NON-KICKERS"
	FOR P = 101 TO 110
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "####"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3464
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "####"; LD(P + 10)
	END IF

3464 NEXT
	PRINT #2, CHR$(12)
	PRINT #2, "RUSHING TOUCHDOWNS"; TAB(41); "RECEIVING TOUCHDOWNS"
	FOR P = 61 TO 70
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "####"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3474
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "####"; LD(P + 10)
	END IF

3474 NEXT
	PRINT #2,
	PRINT #2, "COMPLETION PERCENTAGE"; TAB(41); "QUARTERBACK RATING"
	FOR P = 121 TO 130
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "###.#"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3494
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "###.#"; LD(P + 10)
	END IF

3494 NEXT
	PRINT #2,
	PRINT #2, "PASSING YARDAGE"; TAB(41); "PASSING TOUCHDOWNS"
	FOR P = 141 TO 150
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "####"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3509
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "####"; LD(P + 10)
	END IF

3509 NEXT
	PRINT #2,
	PRINT #2, "KO RETURN AVERAGE                 #"; TAB(41); "PUNT RETURN AVERAGE               #"
	FOR P = 161 TO 170
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "##.#  ##"; LD(P),LZ(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3604
	ELSE
	PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
	PRINT #2, TAB(68);
	PRINT #2, USING "##.#  ##"; LD(P + 10), LZ(P + 10)
	END IF

3604 NEXT
	PRINT #2,
	PRINT #2, "PUNTING AVERAGE"; TAB(41); "SCORING (KICKERS)"
	FOR P = 181 TO 190
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "##.#  ###"; LD(P),LZ(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3619
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "###"; LD(P + 10)
	END IF

3619 NEXT
	PRINT #2,
	PRINT #2, "FIELD GOAL %                     FGA"; TAB(41); "EXTRA POINT %                   EPA"
	FOR P = 201 TO 210
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "###.#  ###"; LD(P),LZ(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3624
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "###.#  ##"; LD(P + 10), LZ(P + 10)
	END IF

3624 NEXT
	PRINT #2,
	PRINT #2, "INTERCEPTIONS"; TAB(41); "SACKS"
	FOR P = 221 TO 230
	IF Z2$(P) = "*" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, Z2$(P) + ", " + Z3$(P);
	PRINT #2, TAB(28);
	PRINT #2, USING "##"; LD(P);
	PRINT #2, TAB(41);
	END IF
	IF Z2$(P + 10) = "*" THEN
		PRINT #2,
		GOTO 3640
	ELSE
		PRINT #2, Z2$(P + 10) + ", " + Z3$(P + 10);
		PRINT #2, TAB(68);
		PRINT #2, USING "##"; LD(P + 10)
	END IF

3640 NEXT
	PRINT #2, CHR$(12)
	PRINT #2, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; TAB(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"
	FOR P = 1 TO 20
	CA = 1
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, TR$(CA, P); TAB(15);
	PRINT #2, USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5);
	PRINT #2, TAB(41);
	END IF
	CA = 2
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN
		PRINT #2,
		GOTO 3793
	ELSE
	PRINT #2, TR$(CA, P); TAB(56);
	PRINT #2, USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)
	END IF

3793 NEXT
	PRINT #2,
	  PRINT #2, "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"; TAB(41); "RUSHING DEF     G PLAYS  YDS  AVG YDS/G"
	FOR P = 1 TO 20
	CA = 3
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, TR$(CA, P); TAB(15);
	PRINT #2, USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5);
	PRINT #2, TAB(41);
	END IF
	CA = 4
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN
		PRINT #2,
		GOTO 3795
	ELSE
	PRINT #2, TR$(CA, P); TAB(56);
	PRINT #2, USING "## #### ##### ##.# ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5)
	END IF

3795 NEXT
	PRINT #2, CHR$(12)
	PRINT #2, "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
	CA = 7
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 3799
	PRINT #2, TR$(CA, P); TAB(15);
	PRINT #2, USING "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

3799 NEXT
	PRINT #2,
	PRINT #2, "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
	CA = 8
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 3800
	PRINT #2, TR$(CA, P); TAB(15);
	PRINT #2, USING "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5) * 100; TT(CA, P, 6); TT(CA, P, 7); TT(CA, P, 8)

3800 NEXT
	PRINT #2, CHR$(12)
	PRINT #2, "SCORING OFF    G  POINTS    AVG"; TAB(41); "SCORING DEF     G  POINTS    AVG"
	FOR P = 1 TO 20
	CA = 5
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN
	PRINT #2, TAB(41);
	ELSE
	PRINT #2, TR$(CA, P); TAB(15);
	PRINT #2, USING "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3);
	PRINT #2, TAB(41);
	END IF
	CA = 6
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN
		PRINT #2,
		GOTO 3797
	ELSE
	PRINT #2, TR$(CA, P); TAB(56);
	PRINT #2, USING "##   ####   ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3)
	END IF

3797 NEXT
	PRINT #2,
	PRINT #2, "PUNTING        NO     AVG"
	CA = 9
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 3801
	PRINT #2, TR$(CA, P); TAB(15);
	PRINT #2, USING "###    ##.#"; TT(CA, P, 1); TT(CA, P, 2)

3801 NEXT
	PRINT #2, CHR$(12)
	PRINT #2, "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
	PRINT #2, "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "
	CA = 10
	FOR P = 1 TO 20
	IF TR$(CA, P) = "*" OR TR$(CA, P) = "" THEN 3802
	PRINT #2, TR$(CA, P); TAB(18);
	PRINT #2, USING "### ###  ###    ### ###  ###       ###.#"; TT(CA, P, 1); TT(CA, P, 2); TT(CA, P, 3); TT(CA, P, 4); TT(CA, P, 5); TT(CA, P, 6); TT(CA, P, 7)

3802 NEXT
CLOSE 2
	IF X1 = 1 THEN 3900
OPEN "REPORT" FOR INPUT AS #2
	DO WHILE NOT EOF(2)
	LINE INPUT #2, X$
	LPRINT X$
	LOOP
CLOSE 2
	LPRINT CHR$(12)
	GOTO 3900


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
SUB TITLE ()
	COLOR , 2
	LOCATE 1, 1: PRINT TAB(80); : LOCATE , 26: COLOR 0
	PRINT "3 IN 1 FOOTBALL STATISTICS"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1991"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 4, 1: COLOR 2, 0
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
