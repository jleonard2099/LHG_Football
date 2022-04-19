'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

' PROGRAM NAME: HELLO.BAS
' COPYRIGHT 1984-2000 BY LANCE HAFFNER ALL RIGHTS RESERVED.
DefInt A-Z
DECLARE FUNCTION PadRight$ (X$, Length)

Dim D$(3), menuItems$(22)
Dim A$(1 To 15)

_Blink Off

Open "DEV.BAS" For Input As #1
For I = 0 To 2
    Input #1, D$(I)
Next
Close 1

Call Comline(N, A$(), 10) ' Get what was typed on the command line
'FOR I = 1 TO N: PRINT A$(I): NEXT I
If A$(1) = "3" Then GoTo 800
If A$(1) = "2" Then GoTo 700

1 Call TITLE
ERASE menuItems$
menuItems$(1) = "PLAY SINGLE GAME                "
menuItems$(2) = "TEAM MANAGEMENT / ORGANIZATION  "
menuItems$(3) = "REPLAY STATISTICS AND REPORTS   "
menuItems$(4) = "LEADERS / STANDINGS / RECORDS   "
menuItems$(5) = "SCHEDULER / AUTOPLAY / BOXSCORES"
menuItems$(6) = "HARD DRIVE INSTALLATION         "
menuItems$(7) = "CONFIGURE DRIVES FOR TEAMS,STATS"
menuItems$(8) = "EXIT TO DOS                     "
SELECTION% = 1
Count% = 8
Start% = 1
SCANCODE% = 8
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 16, 57, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
If SELECTION% = 1 Then Run "FOOTBALL.EXE"
If SELECTION% = 2 Then GoTo 700
If SELECTION% = 3 Then GoTo 750
If SELECTION% = 4 Then GoTo 800
If SELECTION% = 5 Then Run "SCHEDULE.EXE"
'If SELECTION% = 6 Then Run "HDCOPY.EXE"
If SELECTION% = 7 Then GoTo 505
If SELECTION% = 8 Then Cls: End

505 Color 15, 1
FG = 0
BG = 7
For I = 0 To 2
    D$(I) = Left$(D$(I), Len(D$(I)) - 1)
Next
Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
Cls: Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

510 Locate , 31: Print "PATHNAME EXAMPLES:": Print

515 Locate , 31: Print "A:"

520 Locate , 31: Print "C:\FULL\TEAMS"
Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(0), 30), FG, BG, -1
Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(1), 30), FG, BG, -1
Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
Locate , 18: QPrint PadRight$(D$(2), 30), FG, BG, -1

GetDiv1:
Locate 7, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 Then GoTo Done 'all done entering data
D$(0) = _Trim$(D$(0))
GetDiv2:
Locate 9, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 Then GoTo Done 'all done entering data
D$(1) = _Trim$(D$(1))
GetDiv3:
Locate 11, 18 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 Then GoTo GetDiv2: 'can't back up any more, stay here
If XCode = 2 Then GoTo Done 'all done entering data
D$(2) = _Trim$(D$(2))

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

585 For R = 0 To 2: D$(R) = D$(R) + "\": Next R

590 Print #1, D$(0)

595 Print #1, D$(1)
Print #1, D$(2)

600 Close #1
GoTo 1

700 Call TITLE
ERASE menuItems$
menuItems$(1) = "INPUT A NEW TEAM            "
menuItems$(2) = "EDIT AN EXISTING TEAM       "
menuItems$(3) = "PRINT TEAM ROSTER / REPORT  "
menuItems$(4) = "VIEW TEAM ROSTER / REPORT   "
menuItems$(5) = "DRAFT TO NEW / EXISTING TEAM"
menuItems$(6) = "TRADE PLAYERS               "
menuItems$(7) = "RETURN TO 3 IN 1 MAIN MENU  "
SELECTION% = 1
Count% = 7
Start% = 1
SCANCODE% = 7
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 27
CALL box0(7, 26, 15, 55, 2,  15, 1)

_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then GoTo 1
If SELECTION% = 1 Then Run "TINPUT.EXE"
If SELECTION% = 2 Then Run "CNGTM.EXE 1"
If SELECTION% = 3 Then Run "CNGTM.EXE 2"
If SELECTION% = 4 Then Run "CNGTM.EXE 3"
If SELECTION% = 5 Then Run "DRAFT.EXE"
If SELECTION% = 6 Then Run "TRADE.EXE"
If SELECTION% = 7 Then GoTo 1

750 Call TITLE
ERASE menuItems$
menuItems$(1) = "VIEW / PRINT COMPILED STATISTICS "
menuItems$(2) = "ENCYCLOPEDIA/ TEAM REPLAY OPTIONS"
menuItems$(3) = "MERGE SEPARATE STATISTICAL FILES "
menuItems$(4) = "RETURN TO 3 IN 1 MAIN MENU       "
SELECTION% = 1
Count% = 4
Start% = 1
SCANCODE% = 4
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 25

CALL box0(7, 24, 12, 58, 2,  15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then GoTo 1
If SELECTION% = 1 Then Run "LOOKY.EXE"
If SELECTION% = 2 Then Run "CAREER.EXE"
If SELECTION% = 3 Then Run "MERGE.EXE"
If SELECTION% = 4 Then GoTo 1

800 Call TITLE
ERASE menuItems$
menuItems$(1) = "CREATE/ EDIT           "
menuItems$(2) = "UPDATE STATS/ STANDINGS"
menuItems$(3) = "VIEW/ PRINT REPORTS    "
menuItems$(4) = "HEAD-TO-HEAD STATS     "
menuItems$(5) = "LEAGUE RECORDS         "
menuItems$(6) = "EXPANDED LEADERS       "
menuItems$(7) = "DELETE LEAGUE STATS    "
menuItems$(8) = "CREATE/ EDIT           "
menuItems$(9) = "UPDATE STATS/ STANDINGS"
menuItems$(10) = "VIEW/ PRINT REPORTS    "
menuItems$(11) = "HEAD-TO-HEAD STATS     "
menuItems$(12) = "CONFERENCE RECORDS     "
menuItems$(13) = "EXPANDED LEADERS       "
menuItems$(14) = "DELETE CONFERENCE STATS"
menuItems$(15) = "CREATE/ EDIT           "
menuItems$(16) = "UPDATE POLL/ STATS     "
menuItems$(17) = "VIEW/ PRINT REPORTS    "
menuItems$(18) = "NATIONAL RECORDS       "
menuItems$(19) = "DELETE NATIONAL STATS  "
menuItems$(20) = "RETURN TO 3 IN 1 MENU  "
menuItems$(21) = "EXIT TO DOS            "
SELECTION% = 1
Count% = 21
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 7
Columns% = 3
Spacing% = 3
Row% = 9
Column% = 2
CALL box0(7, 1, 16, 25, 2,  15, 1)
CALL box0(7, 27, 16, 51, 2,  15, 1)
CALL box0(7, 53, 16, 77, 2,  15, 1)
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
20 MAMenu menuItems$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then GoTo 1
If ExitCode% = -71 Then Start% = 1: ExitCode% = 3: SELECTION% = 1: GoTo 20
If ExitCode% = -79 Then Start% = 15: ExitCode% = 3: SELECTION% = 15: GoTo 20
If ExitCode% = -73 Then Start% = Start% - 80: If Start% < 1 Then Start% = 1
If ExitCode% = -81 Then Start% = Start% + 80: If Start% > 15 Then Start% = 15
If ExitCode% = -81 Or ExitCode% = -73 Then ExitCode% = 3: SELECTION% = Start%: GoTo 20
Color , 0
If SELECTION% = 1 Then Run "CREATPRO.EXE"
If SELECTION% = 2 Then Run "COMPPRO.EXE"
If SELECTION% = 3 Then Run "SEEPRO.EXE"
If SELECTION% = 4 Then Run "HD2HDPRO.EXE"
If SELECTION% = 5 Then Run "REC.EXE 1"
If SELECTION% = 6 Then Run "EXPANPRO.EXE"
If SELECTION% = 7 Then Run "BLANKPRO.EXE"
If SELECTION% = 8 Then Run "CREATCOL.EXE"
If SELECTION% = 9 Then Run "COMPCOLL.EXE"
If SELECTION% = 10 Then Run "SEECOLL.EXE"
If SELECTION% = 11 Then Run "HD2HDCOL.EXE"
If SELECTION% = 12 Then Run "REC.EXE 2"
If SELECTION% = 13 Then Run "EXPANPRO.EXE"
If SELECTION% = 14 Then Run "BLANKCOL.EXE"
If SELECTION% = 15 Then Run "CREATNAT.EXE"
If SELECTION% = 16 Then Run "COMPNATL.EXE"
If SELECTION% = 17 Then Run "SEENATL.EXE"
If SELECTION% = 18 Then Run "REC.EXE 3"
If SELECTION% = 19 Then Run "BLANKORG.EXE"
If SELECTION% = 20 Then GoTo 1
If SELECTION% = 21 Then Cls: End


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 32: Color 12
    Print "3 IN 1 FOOTBALL V6.3"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER 1984-2000"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15
    Print
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER"
    Print "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR LANCE@LHGAMES.COM"
    Print "      LANCE HAFFNER GAMES ON THE INTERNET AT  HTTP://WWW.LHGAMES.COM"
    Locate 12, 1
    Print "LANCE HAFFNER GAMES"
    Print "P.O. BOX 100594"
    Print "NASHVILLE, TN 37224"
    Print "LANCE@LHGAMES.COM"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
