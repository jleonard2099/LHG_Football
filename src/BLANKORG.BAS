'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Delete National Stats"
''$ExeIcon:'./lhg_3in1fb.ico'
'_Icon
_AllowFullScreen

DEFINT A-Z

'Dim O1(40), TM$(40), JS(46, 42) As Single, ZM%(46), NM$(46)
Dim H1$(40), D$(2), Z4$(40)
'Dim PS$(40)
'Dim BA$(40), DFT(40, 20) As Single, ER$(40), OFT(40, 20) As Single
'Dim O2(40), Z2$(255), Z3$(255), Z1(255, 2) As Single, BAT(24), PIT(23)
'Dim J As Single

4 OPEN "DEV.BAS" FOR INPUT AS #2
	FOR I = 0 TO 1
		INPUT #2, D$(I)
	NEXT
CLOSE 2

5 CLS
APP = 0

4000 Spec$ = D$(1) + "*.CON"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4500
REDIM conferenceFiles$(0 TO Count%)
	FOR X = 1 TO Count%
	  conferenceFiles$(X) = SPACE$(12)
	NEXT
	conferenceFiles$(0) = Spec$
	CALL ReadFile(conferenceFiles$())
	'CALL SortStr(VARPTR(conferenceFiles$(1)), Count%, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT ORGANIZATION TO ERASE": COLOR 15
	SELECTION% = 1
	Start% = 1
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
	PRINT "-SELECTS ORGANIZATION   ";
	COLOR 14
	PRINT "HIT ESCAPE KEY TO EXIT";
	COLOR 15

_MouseShow "DEFAULT"
MAMenu conferenceFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 99
	targetFile$ = conferenceFiles$(SELECTION%)
	LOCATE 2, 33: PRINT targetFile$
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 targetFile$ = RTrim$(targetFile$)
	WE$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
	LH$ = LEFT$(WE$, 8)

15 CLS
	LOCATE 10, 8
	CALL CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	IF I$ <> "Y" THEN 1310

30 OPEN D$(1) + LH$ + ".CON" FOR INPUT AS #2
	INPUT #2, LG$
	FOR X = 1 TO 40
	INPUT #2, Z4$(X)
	NEXT
CLOSE 2

FOR X = 1 TO 40
	IF Z4$(X) = "XXX" THEN 75

	OPEN D$(1) + Z4$(X) + ".TMS" FOR INPUT AS #2
		INPUT #2, LG$, D1$, D2$
		FOR XW = 1 TO 30
35 			INPUT #2, H1$(XW)
		NEXT
	CLOSE 2

71 FOR XW = 1 TO 30
	IF H1$(XW) = CHR$(42) THEN 74

801 PRINT "DELETING "; H1$(XW); " STATS"
	KILL (D$(1) + H1$(XW))
	KILL (D$(1) + H1$(XW) + "S")

74 NEXT

75 NEXT
	CLS
	GOTO 4000

99  RUN "HELLO.EXE"

4500 LOCATE 10, 8
	CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	GOTO 99


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
