
Const GAME_VERSION$ = "6.35"
Const MAX_TEAMS = 500
Const MAX_GAMES = 1500
'Const MAX_CONFERENCES = 40
'Const TEAMS_PER_CONFERENCE = 30
'Const TEAMS_PER_DIV = 15
'Const DIVS_PER_CONFERENCE = 2

'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'
'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'StatRoutines.BI'
'$INCLUDE: 'TITLE.BI'

' --- let's consider thet impact of this
'	with all variables being declared here,
'	we can have a quicker success leaving
'	this statement out.
'	In fact, most routines are explicitly
'	creating integers where needed (%),
'	so this only forcing temporary "counting"
'	variables as integers
' DefInt A-Z


'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared diskPaths$(0 to 3), Q$(0 To 230)

Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

Dim Shared DirCount%

'File size - does this really need shared?
Dim Shared Y As Long

'Dim yearNumber$(MAX_GAMES, 1)
Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)


'----------------------------------------
' Used across EXPANPRO.BAS routines
'----------------------------------------
Dim categories$(74)

Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,RUSING TOUCHDOWNS,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING AVERAGE,RECEIVING TOUCHDOWNS,LONGEST RECEPTION
Data PASS ATTEMPTS,PASS COMPLETIONS,INTERCEPTIONS,PASSING YARDS,TOUCHDOWN PASSES,COMPLETION PERCENTAGE,AVERAGE YDS/COMP.,QUARTERBACK RATING,LONGEST PASS
Data KICKOFF RETURNS,KICKOFF RETURN YARDS,AVERAGE GAIN/KO RETURN,KICKOFF RETURN TD'S,LONGEST KICKOFF Return

Data PUNT RETURNS,PUNT RETURN YARDS,AVERAGE GAIN/PUNT RETURN,PUNT RETURN TD'S,LONGEST PUNT Return

Data PUNTS,PUNTING YARDS,AVERAGE PER PUNT
Data F.G.'s MADE,F.G. ATTEMPES,F.G. PERCENTAGE,E.P.'s MADE,E.P. ATTEMPTS,E.P. PERCENTAGE,LONGEST FIELD GOAL
Data INTERCEPTIONS,SACKS
Data NON-KICKERS,KICKERS,ALL PLAYERS
Data FIRST DOWNS,RUSH ATTEMPTS,RUSHING YARDS,RUSHING AVERAGE,PASSING YARDS,TOTAL YARDS,PASS COMPLETION PCT.,FUMBLES,FUMBLES LOST,INTERCEPTIONS THROWN,TURNOVER RATIO,PENALTIES,PENALTY YARDAGE,SACKS ALLOWED,SACK YARDAGE
Data OPP. FIRST DOWNS,OPP. RUSH ATTEMPTS,OPP. RUSHING YARDS,OPP. RUSHING AVERAGE,OPP. PASSING YARDS,OPP. TOTAL YARDS,OPP. PASS COMPLETION PCT.,OPP. FUMBLES,OPP. FUMBLES RECOVERED,INTERCEPTIONS,OPP. PENALTIES,OPP. PENALTY YARDAGE,SACKS
Data SACK YARDAGE LOST,ALL-PURPOSE YARDS


'----------------------------------------
' 	Used across LOOKY.BAS routines
'----------------------------------------
Dim GL, PW, SS

Dim O%(120), T%(120), P%(1), PA%(5)

'	These should all be Single
'	let's be sure it works that way!
Dim AF(1, 4), AM(1, 4), CRD(120)
Dim GI(1, 9, 2), GS(1, 14, 1), K(1, 12), K2(1, 13, 17)
Dim LC(15), LF(1), LI(9), LK(2), LP(3), LR(9)
Dim PK(2), W6(1, 1), Z(38), Z1(38), Z2(13, 17)

Dim A$(1), A1$(1, 9), A2$(1, 5), A3$(1, 3), A4$(1, 2), A5$(1, 2), A6$(1, 2), A7$(0, 1)
Dim DI$(1, 9), DS$(1, 14), H$(120)
Dim LC$(15, 1), LI$(9, 1), LK$(2, 1), LP$(3, 1), LR$(9, 1)
Dim O$(120), PK$(2, 1), SITE$(120)



'----------------------------------------
' Used across REC.BAS routines
'----------------------------------------
Dim TP$(46), TP1$(43)
Data RUSHING ATTEMPTS,RUSHING YARDS,RUSHING TOUCHDOWNS,RUSHING AVERAGE,LONGEST RUN
Data RECEPTIONS,RECEIVING YARDS,RECEIVING TOUCHDOWNS,RECEIVING AVERAGE,LONGEST CATCH
Data PASS ATTEMPTS,PASS COMPLETIONS,PASSING TOUCHDOWNS,HAD INTERCEPTED,PASSING YARDS
Data COMPLETION PCT.,PASSER RATING,LONGEST PASS,TOTAL YARDS,SCORING (NON-KICKER)
Data SCORING (KICKERS),TOTAL SCORING,KICKOFF RETURNS,KICKOFF YARDS,KICKOFF TOUCHDOWNS
Data KICKOFF RETURN AVERAGE,LONGEST KO RETURN,PUNT RETURNS,PUNT RETURN YARDS
Data PUNT RETURN TOUCHDOWNS,PUNT RETURN AVERAGE,LONGEST PUNT RETURN,PUNTS
Data PUNTING YARDS,PUNTING AVERAGE,FIELD GOALS MADE,FIELD GOALS ATTEMPTED
Data FIELD GOAL %,LONGEST FIELD GOAL,POINT AFTER MADE,POINT AFTER ATTEMPTS,PAT%
Data INTERCEPTIONS,INTERCEPTION YARDS,INTERCEPTION TD'S,LONGEST INTERCEPTION
For I = 0 To 45: Read TP$(I): Next

Data YARDS/GAME,TOTAL YARDS,GAIN/PLAY,RUSHING YARDS/GAME,TOTAL RUSHING
Data GAIN/RUSH,PASSING YARDS/GAME,PASSING YARDS,YARDS/ATTEMPT
Data YARDS/COMPLETION,COMPLETION PCT.,INTERCEPTION PCT.,POINTS SCORED
Data POINTS/GAME,MARGIN OF VICTORY,PUNTING AVERAGE,PUNT RETURN AVG
Data KICKOFF RETURN AVG,FIRST DOWNS,FIRST DOWNS/GAME,MOST FUMBLES
Data MOST FUMBLES LOST,FEWEST FUMBLES LOST,PENALTIES/GAME,PENALTY YARDS/GAME
Data YARDS/GAME,YARDS ALLOWED,YARDS/PLAY,MOST YARDS/GAME
Data RUSHING YARDS/GAME,RUSHING YARDS,GAIN/RUSH,PASSING YARDS/GAME
Data PASSING YARDS,YARDS/ATTEMPT,YARDS/COMPLETION,COMPLETION PCT.
Data INTERCEPTION PCT.,POINTS ALLOWED,POINTS/GAME,MOST POINTS ALLOWED
Data MOST PTS ALLOWED/GAME,PUNT RET AVERAGE,KICKOFF RET AVERAGE

For I = 0 To 43: Read TP1$(I): Next


'----------------------------------------
' Used across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$

Dim NG%(MAX_GAMES, 20)

Dim scheduleAP%(1), scheduleZ1%(30)
Dim scheduleL%(13)

Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleQB_V$(20), scheduleQB_H$(20)
Dim scheduleYN$(MAX_GAMES, 1)
scheduleYN$(0, 0) = ""
scheduleYN$(0, 1) = ""


Dim A6(3), A7(3), A8(3)
Dim A3$(4), MO$(3), Z1$(30), Z2$(30)

Dim E%(13), PA%(4), QX%(3)

Dim MO$(3)
MO$(0) = "HH"
MO$(1) = "C1"
MO$(2) = "C2"
MO$(3) = "CC"



'----------------------------------------
'				MAIN CODE
'----------------------------------------
_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

Open "DEV.BAS" For Input As #1
For I = 0 To 1
    Input #1, diskPaths$(I)
Next
Close 1

Selection% = 1

Do
    Call Main_Menu(Selection%)
Loop Until Selection% = 8

End

System


'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

	Dim A$(1 To 15)
	ReDim menuChoices$(1 to 8)
	
	Do
		Cls
		
		Call TITLE
		
		'Erase menuChoices$
		menuChoices$(1) = "PLAY SINGLE GAME                "
		menuChoices$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
		menuChoices$(3) = "REPLAY STATISTICS AND REPORTS   "
		menuChoices$(4) = "LEADERS / STANDINGS / RECORDS   "
		menuChoices$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
		menuChoices$(6) = "HARD DRIVE INSTALLATION         "
		menuChoices$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
		menuChoices$(8) = "EXIT TO DOS                     "
		menuSelection% = 1
		Count% = 8
		Start% = 1
		ExitCode% = 8
		FileFGColr% = 15: FileBGColr% = 1
		HiLiteFGColr% = 0: HiLiteBGColr% = 7
		Spacing% = 1
		Row% = 8
		Column% = 25

		Call box0(7, 24, 16, 57, 2, 15, 1)
		_MouseShow "DEFAULT"
		MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
		_MouseHide

		Color , 0
		If ExitCode% <> 27 Then
			Select Case menuSelection%
				Case 1: Run "FOOTBALL.EXE"
				Case 2: Call Team_Menu
				Case 3: Call Replay_Menu
				Case 4: GoTo Leaders_Menu
				Case 5: Call SCHEDULE
				Case 6: '
				Case 7: Call ConfigureDrives
				Case 8: Cls
			End Select
		End If

	Loop Until menuSelection% = 8 OR ExitCode% = 27

	If ExitCode% = 27 Then menuSelection% = 8

End Sub


'----------------------------------------
'	  	ConfigureDrives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

505 Color 15, 1
FG = 0
BG = 7
For I = 0 To 2
    diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
Next
'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
Cls: Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

510 Locate , 31: Print "PATHNAME EXAMPLES:": Print

515 Locate , 31: Print "A:"

520 Locate , 31: Print "C:\FULL\TEAMS"
Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
Locate , 16: QPrint PadRight$(diskPaths$(0), 30), FG, BG, -1
Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
Locate , 16: QPrint PadRight$(diskPaths$(1), 30), FG, BG, -1
Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
Locate , 18: QPrint PadRight$(diskPaths$(2), 30), FG, BG, -1

GetDiv1:
Locate 7, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 Then GoTo Done 'all done entering data
diskPaths$(0) = _Trim$(diskPaths$(0))
GetDiv2:
Locate 9, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 Then GoTo Done 'all done entering data
diskPaths$(1) = _Trim$(diskPaths$(1))
GetDiv3:
Locate 11, 18 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 Then GoTo GetDiv2: 'can't back up any more, stay here
If XCode = 2 Then GoTo Done 'all done entering data
diskPaths$(2) = _Trim$(diskPaths$(2))

Done:
Locate 23, 1
Print "Is this okay? (Yes/No) ";
X$ = ""
Call YesNoB(X$, XCode, FG, BG)
Locate , 1
ClearEOL 14, 1
If XCode = 1 Then GoTo GetDiv3
If X$ = "N" Then GoTo GetDiv1
If X$ <> "Y" Then GoTo Done 'insist on either Y or N or up-arrow

580 Open "DEV.BAS" For Output As #1

585 For R = 0 To 2: diskPaths$(R) = diskPaths$(R) + "\": Next R

590 Print #1, diskPaths$(0)

595 Print #1, diskPaths$(1)
Print #1, diskPaths$(2)

600 Close #1

End Sub


'----------------------------------------
'	  	Sub Team_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Team_Menu ()

Do
	Cls
	
	700 Call TITLE
	ReDim menuChoices$(1 to 7)
	'Erase menuChoices$
	menuChoices$(1) = "INPUT A NEW TEAM            "
	menuChoices$(2) = "EDIT AN EXISTING TEAM       "
	menuChoices$(3) = "PRINT TEAM ROSTER / REPORT  "
	menuChoices$(4) = "VIEW TEAM ROSTER / REPORT   "
	menuChoices$(5) = "DRAFT TO NEW / EXISTING TEAM"
	menuChoices$(6) = "TRADE PLAYERS               "
	menuChoices$(7) = "RETURN TO 3 IN 1 MAIN MENU  "
	menuSelection% = 1
	Count% = 7
	Start% = 1
	ExitCode% = 7
	FileFGColr% = 15: FileBGColr% = 1
	HiLiteFGColr% = 0: HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 27
	Call box0(7, 26, 15, 55, 2, 15, 1)

	_MouseShow "DEFAULT"
	MMenuVert menuChoices$(), menuSelection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
	_MouseHide

	Color , 0

	If ExitCode% <> 27 Then
		Select Case menuSelection%
			Case 1: Call TINPUT
			Case 2: Call CNGTM 1
			Case 3: Call CNGTM 2
			Case 4: Call CNGTM 3
			Case 5: Call DRAFT
			Case 6: Call TRADE
			Case 7: Cls
		End Select
	End If

Loop Until menuSelection% = 7 OR ExitCode% = 27

End Sub



'----------------------------------------
'	  	Sub Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

Do
	Cls
	
	750 Call TITLE
	ReDim menuChoices$(1 to 4)
	'Erase menuChoices$
	menuChoices$(1) = "VIEW / PRINT COMPILED STATISTICS "
	menuChoices$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
	menuChoices$(3) = "MERGE SEPARATE STATISTICAL FILES "
	menuChoices$(4) = "RETURN TO 3 IN 1 MAIN MENU       "
	Selection% = 1
	Count% = 4
	Start% = 1
	ExitCode% = 4
	FileFGColr% = 15: FileBGColr% = 1
	HiLiteFGColr% = 0: HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

	Call box0(7, 24, 12, 58, 2, 15, 1)
	_MouseShow "DEFAULT"
	MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
	_MouseHide

	Color , 0
	If ExitCode% <> 27 Then
		Select Case Selection%
			Case 1: Call LOOKY
			Case 2: Call CAREER
			Case 3: Call MERGE
			Case 4: Cls
		End Select
	End If

Loop Until Selection% = 4 OR ExitCode% = 27

End Sub


'----------------------------------------
'	  	Sub Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

Do

	800 Call TITLE
	ReDim menuChoices$(1 to 21)
	'Erase menuChoices$
	menuChoices$(1) = "CREATE/ EDIT           "
	menuChoices$(2) = "UPDATE STATS/ STANDINGS"
	menuChoices$(3) = "VIEW/ PRINT REPORTS    "
	menuChoices$(4) = "HEAD-TO-HEAD STATS     "
	menuChoices$(5) = "LEAGUE RECORDS         "
	menuChoices$(6) = "EXPANDED LEADERS       "
	menuChoices$(7) = "DELETE LEAGUE STATS    "
	menuChoices$(8) = "CREATE/ EDIT           "
	menuChoices$(9) = "UPDATE STATS/ STANDINGS"
	menuChoices$(10) = "VIEW/ PRINT REPORTS    "
	menuChoices$(11) = "HEAD-TO-HEAD STATS     "
	menuChoices$(12) = "CONFERENCE RECORDS     "
	menuChoices$(13) = "EXPANDED LEADERS       "
	menuChoices$(14) = "DELETE CONFERENCE STATS"
	menuChoices$(15) = "CREATE/ EDIT           "
	menuChoices$(16) = "UPDATE POLL/ STATS     "
	menuChoices$(17) = "VIEW/ PRINT REPORTS    "
	menuChoices$(18) = "NATIONAL RECORDS       "
	menuChoices$(19) = "DELETE NATIONAL STATS  "
	menuChoices$(20) = "RETURN TO 3 IN 1 MENU  "
	menuChoices$(21) = "EXIT TO DOS            "
	Selection% = 1
	Count% = 21
	Start% = 1
	FileFGColr% = 15: FileBGColr% = 1
	HiLiteFGColr% = 0: HiLiteBGColr% = 7
	Rows% = 7
	Columns% = 3
	Spacing% = 3
	Row% = 9
	Column% = 2
	Call box0(7, 1, 16, 25, 2, 15, 1)
	Call box0(7, 27, 16, 51, 2, 15, 1)
	Call box0(7, 53, 16, 77, 2, 15, 1)
	Color 15, 0: Locate 8, 4: Print "PROFESSIONAL LEAGUE"
	Locate 8, 30: Print "COLLEGE CONFERENCE"
	Locate 8, 55: Print "NATIONAL ORGANIZATION"
	Locate 18
	Print "PROFESSIONAL LEAGUE IS USED TO DEFINE A GROUP OF PRO TEAMS AND DIVISIONS"
	Print
	Print "COLLEGE CONFERENCE IS USED TO DEFINE THE TEAMS IN A SINGLE CONFERENCE"
	Print
	Print "NATIONAL ORGANIZATION ALLOWS YOU TO SELECT MULTIPLE COLLEGE CONFERENCES TO"
	Print "TRACK AND UPDATE TO GENERATE A NATIONAL POLL, NATIONAL LEADERS."

	_MouseShow "DEFAULT"
	MAMenu menuChoices$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
	_MouseHide

	If ExitCode% = 27 Then Exit Sub

	Color , 0

	If ExitCode% <> 27 Then
		Select Case Selection%		
			Case 1: Call CREATPRO
			Case 2: Call COMPPRO
			Case 3: Call SEEPRO
			Case 4: Call HD2HDPRO
			Case 5: Call REC 1
			Case 6: Call EXPANPRO
			Case 7: Call BLANKPRO
			Case 8: Call CREATCOL
			Case 9: Call COMPCOLL
			Case 10: Call SEECOLL
			Case 11: Call HD2HDCOL
			Case 12: Call REC 2
			Case 13: Call EXPANPRO
			Case 14: Call BLANKCOL
			Case 15: Call CREATNAT
			Case 16: Call COMPNATL
			Case 17: Call SEENATL
			Case 18: Call REC 3
			Case 19: Call BLANKORG
			Case 20 or 21: Cls
		End Select
	End If

	If Selection% = 21 Then 
		End
		SYSTEM
	End If

Loop Until Selection% >= 20 OR ExitCode% = 27

End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'MainRoutines.BM'
'$INCLUDE: 'PadRight.BM'
'$INCLUDE: 'StatRoutines.BI'
'$INCLUDE: 'TITLE.BM'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANKCOL.BAS'
'$INCLUDE: 'BLANKORG.BAS'
'$INCLUDE: 'BLANKPRO.BAS'
'$INCLUDE: 'CAREDIT.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CAREER1.BAS'
'$INCLUDE: 'CARPRNT.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CNGTM.BAS'
'$INCLUDE: 'COMPCOLL.BAS'
'$INCLUDE: 'COMPNAT.BAS'
'$INCLUDE: 'COMPPRO.BAS'
'$INCLUDE: 'CREATCOL.BAS'
'$INCLUDE: 'CREATNAT.BAS'
'$INCLUDE: 'CREATPRO.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'EXPANPRO.BAS'
'$INCLUDE: 'HD2HDCOL.BAS'
'$INCLUDE: 'HD2HDPRO.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECOLL.BAS'
'$INCLUDE: 'SEENATL.BAS'
'$INCLUDE: 'SEEPRO.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
