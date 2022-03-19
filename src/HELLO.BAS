'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

' PROGRAM NAME: HELLO.BAS
' COPYRIGHT 1984-2000 BY LANCE HAFFNER ALL RIGHTS RESERVED.
DEFINT A-Z
DECLARE FUNCTION FnExt$ (X$, Length)

DIM D$(3), ITEM$(22)
DIM A$(1 To 15)

_BLINK OFF

OPEN "DEV.BAS" For Input As #1
	For I = 0 To 2
	Input #1, D$(I)
	Next
Close 1

Call Comline(N, A$(), 10)     ' Get what was typed on the command line
'FOR I = 1 TO N: PRINT A$(I): NEXT I
If A$(1) = "3" Then GoTo 800
If A$(1) = "2" Then GoTo 700

1  Call TITLE
	ITEM$(1) = "PLAY SINGLE GAME                "
	ITEM$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
	ITEM$(3) = "REPLAY STATISTICS AND REPORTS   "
	ITEM$(4) = "LEADERS / STANDINGS / RECORDS   "
	ITEM$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
	ITEM$(6) = "HARD DRIVE INSTALLATION         "
	ITEM$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
	ITEM$(8) = "EXIT TO DOS                     "
	SELECTION% = 1
	Count% = 8
	Start% = 1
	SCANCODE% = 8
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 16, 57, 2, 31)
_MouseShow "DEFAULT"
MMenuVert VarPtr(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	If SCANCODE% = 27 Then End
	 If SELECTION% = 1 Then RUN "FOOTBALL.EXE"
	If SELECTION% = 2 Then GoTo 700
	If SELECTION% = 3 Then GoTo 750
	If SELECTION% = 4 Then GoTo 800
	If SELECTION% = 5 Then RUN "SCHEDULE.EXE"
	If SELECTION% = 6 Then RUN "HDCOPY.EXE"
	If SELECTION% = 7 Then GoTo 505
	If SELECTION% = 8 Then CLS: End

505 COLOR 15, 1
	 FG = 0
	 BG = 7
	 For I = 0 To 2
	 D$(I) = Left$(D$(I), Len(D$(I)) - 1)
	 Next
	Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
CLS:      LOCATE , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

510 LOCATE , 31: Print "PATHNAME EXAMPLES:": Print

515 LOCATE , 31: Print "A:"

520 LOCATE , 31: Print "C:\FULL\TEAMS"
	LOCATE 7, 1: QPrint "Path for Teams ", 30, -1
	LOCATE , 16: QPrint FnExt$(D$(0), 30), FG, BG, -1
	LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
	LOCATE , 16: QPrint FnExt$(D$(1), 30), FG, BG, -1
	LOCATE 11, 1: QPrint "Path for Leaders ", 30, -1
	LOCATE , 18: QPrint FnExt$(D$(2), 30), FG, BG, -1

GetDiv1:
	LOCATE 7, 16                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
	Call TextIn(D$(0), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	If XCode = 1 Then GoTo GetDiv1:      'can't back up any more, stay here
	If XCode = 2 Then GoTo Done          'all done entering data
	D$(0) = QPTRIM$(D$(0))
GetDiv2:
	LOCATE 9, 16                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
	Call TextIn(D$(1), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	If XCode = 1 Then GoTo GetDiv1:      'can't back up any more, stay here
	If XCode = 2 Then GoTo Done          'all done entering data
	D$(1) = QPTRIM$(D$(1))
GetDiv3:
	LOCATE 11, 18                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
	Call TextIn(D$(2), Max, NumOnly, CapsOn, XCode, Colr)        'get input
	If XCode = 1 Then GoTo GetDiv2:      'can't back up any more, stay here
	If XCode = 2 Then GoTo Done          'all done entering data
	D$(2) = QPTRIM$(D$(2))

Done:
	LOCATE 23, 1
	Print "Is this okay? (Yes/No) ";
	X$ = ""
	Call YesNoB(X$, XCode, Colr)
	LOCATE , 1
	ClearEOL 30
	If XCode = 1 Then GoTo GetDiv3
	If X$ = "N" Then GoTo GetDiv1
	If X$ <> "Y" Then GoTo Done          'insist on either Y or N or up-arrow

580 Open "DEV.BAS" For Output As #1

585 For R = 0 To 2: D$(R) = D$(R) + "\": Next R

590 Print #1, D$(0)

595 Print #1, D$(1)
	Print #1, D$(2)

600 Close #1
	 GoTo 1

700 Call TITLE
	ITEM$(1) = "INPUT A NEW TEAM            "
	ITEM$(2) = "EDIT AN EXISTING TEAM       "
	ITEM$(3) = "PRINT TEAM ROSTER / REPORT  "
	ITEM$(4) = "VIEW TEAM ROSTER / REPORT   "
	ITEM$(5) = "DRAFT TO NEW / EXISTING TEAM"
	ITEM$(6) = "TRADE PLAYERS               "
	ITEM$(7) = "RETURN TO 3 IN 1 MAIN MENU  "
	SELECTION% = 1
	Count% = 7
	Start% = 1
	SCANCODE% = 7
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 27
CALL box0(7, 26, 15, 55, 2, 31)

_MouseShow "DEFAULT"
MMenuVert VarPtr(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	If SCANCODE% = 27 Then GoTo 1
	If SELECTION% = 1 Then RUN "TINPUT.EXE"
	If SELECTION% = 2 Then RUN "CNGTM.EXE 1"
	If SELECTION% = 3 Then RUN "CNGTM.EXE 2"
	If SELECTION% = 4 Then RUN "CNGTM.EXE 3"
	If SELECTION% = 5 Then RUN "DRAFT.EXE"
	If SELECTION% = 6 Then RUN "TRADE.EXE"
	If SELECTION% = 7 Then GoTo 1

750 Call TITLE
	ITEM$(1) = "VIEW / PRINT COMPILED STATISTICS "
	ITEM$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
	ITEM$(3) = "MERGE SEPARATE STATISTICAL FILES "
	ITEM$(4) = "RETURN TO 3 IN 1 MAIN MENU       "
	SELECTION% = 1
	Count% = 4
	Start% = 1
	SCANCODE% = 4
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

Call box0(7, 24, 12, 58, 2, 31)
_MouseShow "DEFAULT"
MMenuVert VarPtr(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	If SCANCODE% = 27 Then GoTo 1
	If SELECTION% = 1 Then RUN "LOOKY.EXE"
	If SELECTION% = 2 Then RUN "CAREER.EXE"
	If SELECTION% = 3 Then RUN "MERGE.EXE"
	If SELECTION% = 4 Then GoTo 1

800 Call TITLE
	ITEM$(1) = "CREATE/ EDIT           "
	ITEM$(2) = "UPDATE STATS/ STANDINGS"
	ITEM$(3) = "VIEW/ PRINT REPORTS    "
	ITEM$(4) = "HEAD-TO-HEAD STATS     "
	ITEM$(5) = "LEAGUE RECORDS         "
	ITEM$(6) = "EXPANDED LEADERS       "
	ITEM$(7) = "DELETE LEAGUE STATS    "
	ITEM$(8) = "CREATE/ EDIT           "
	ITEM$(9) = "UPDATE STATS/ STANDINGS"
	ITEM$(10) = "VIEW/ PRINT REPORTS    "
	ITEM$(11) = "HEAD-TO-HEAD STATS     "
	ITEM$(12) = "CONFERENCE RECORDS     "
	ITEM$(13) = "EXPANDED LEADERS       "
	ITEM$(14) = "DELETE CONFERENCE STATS"
	ITEM$(15) = "CREATE/ EDIT           "
	ITEM$(16) = "UPDATE POLL/ STATS     "
	ITEM$(17) = "VIEW/ PRINT REPORTS    "
	ITEM$(18) = "NATIONAL RECORDS       "
	ITEM$(19) = "DELETE NATIONAL STATS  "
	ITEM$(20) = "RETURN TO 3 IN 1 MENU  "
	ITEM$(21) = "EXIT TO DOS            "
	SELECTION% = 1
	Count% = 21
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 7
	Columns% = 3
	Spacing% = 3
	Row% = 9
	Column% = 2
CALL box0(7, 1, 16, 25, 2, 31)
CALL box0(7, 27, 16, 51, 2, 31)
CALL box0(7, 53, 16, 77, 2, 31)
	COLOR 15, 0: LOCATE 8, 4: Print "PROFESSIONAL LEAGUE"
	LOCATE 8, 30: Print "COLLEGE CONFERENCE"
	LOCATE 8, 55: Print "NATIONAL ORGANIZATION"
	LOCATE 18
	Print "PROFESSIONAL LEAGUE IS USED TO DEFINE A GROUP OF PRO TEAMS AND DIVISIONS"
	Print
	Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A SINGLE CONFERENCE"
	Print
	Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE COLLEGE CONFERENCES TO"
	Print "TRACK AND UPDATE TO GENERATE A NATIONAL POLL, NATIONAL LEADERS."
	_MouseShow "DEFAULT"

20   MAMenu VarPtr(ITEM$(1)), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide
	If ExitCode% = 27 Then GoTo 1
	If ExitCode% = -71 Then Start% = 1: ExitCode% = 3: SELECTION% = 1: GoTo 20
	If ExitCode% = -79 Then Start% = 15: ExitCode% = 3: SELECTION% = 15: GoTo 20
	If ExitCode% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
	If ExitCode% = -81 Then Start% = Start% + 80: If Start% > 15 Then Start% = 15
	If ExitCode% = -81 Or ExitCode% = -73 Then ExitCode% = 3: SELECTION% = Start%: GoTo 20
	COLOR , 0
	If SELECTION% = 1 Then RUN "CREATPRO.EXE"
	If SELECTION% = 2 Then RUN "COMPPRO.EXE"
	If SELECTION% = 3 Then RUN "SEEPRO.EXE"
	If SELECTION% = 4 Then RUN "HD2HDPRO.EXE"
	If SELECTION% = 5 Then RUN "REC.EXE 1"
	If SELECTION% = 6 Then RUN "EXPANPRO.EXE"
	If SELECTION% = 7 Then RUN "BLANKPRO.EXE"
	If SELECTION% = 8 Then RUN "CREATCOL.EXE"
	If SELECTION% = 9 Then RUN "COMPCOLL.EXE"
	If SELECTION% = 10 Then RUN "SEECOLL.EXE"
	If SELECTION% = 11 Then RUN "HD2HDCOL.EXE"
	If SELECTION% = 12 Then RUN "REC.EXE 2"
	If SELECTION% = 13 Then RUN "EXPANPRO.EXE"
	If SELECTION% = 14 Then RUN "BLANKCOL.EXE"
	If SELECTION% = 15 Then RUN "CREATNAT.EXE"
	If SELECTION% = 16 Then RUN "COMPNATL.EXE"
	If SELECTION% = 17 Then RUN "SEENATL.EXE"
	If SELECTION% = 18 Then RUN "REC.EXE 3"
	If SELECTION% = 19 Then RUN "BLANKORG.EXE"
	If SELECTION% = 20 Then GoTo 1
	If SELECTION% = 21 Then CLS: End

FUNCTION FnExt$ (X$, Length) 
'Essentially this pads space to the right of the string
	X$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION

Sub TITLE()
	COLOR , 0: Key OFF: CLS
	COLOR , 1: LOCATE 1, 1: Print Tab(80);: LOCATE , 32: COLOR 12
	Print "3 IN 1 FOOTBALL V6.3"
	LOCATE 24, 1: Print Tab(80);: LOCATE , 1
	Print "(C) LANCE HAFFNER 1984-2000"; Tab(70); Left$(Time$, 5);
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15
	Print
	COLOR 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER"
	  Print "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR LANCE@LHGAMES.COM"
	  Print "      LANCE HAFFNER GAMES ON THE INTERNET AT  HTTP://WWW.LHGAMES.COM"
	LOCATE 12, 1
	Print "LANCE HAFFNER GAMES"
	Print "P.O. BOX 100594"
	Print "NASHVILLE, TN 37224"
	Print "LANCE@LHGAMES.COM"
	COLOR 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
