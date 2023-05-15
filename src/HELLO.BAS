$Debug

Const GAME_VERSION$ = "6.40"
Const MAX_TEAMS = 1000
Const MAX_GAMES = 1500
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

'Const NUM_STATRECORDS = 162

DefInt A-Z

'$INCLUDE: 'KeyInput.bi'
'$INCLUDE: 'QPProEqu.bi'
'$INCLUDE: 'PadRight.bi'
'$INCLUDE: 'Variables.bi'


Dim AR1$(9), AR2$(5), AR3$(3), AR4$(2), AR5$(2), AR6$(3), AR7$(1)
Dim statsA1$(9), statsA2$(5), statsA3$(4), statsA4$(2), statsA5$(2), statsA6$(3), statsA7$(1)


' For Utils
'$INCLUDE: 'MainRoutines.bi'
'$INCLUDE: 'TITLE.bi'

' For Game
'$INCLUDE: 'GameRoutines.bi'


On Error GoTo Errhandler


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
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
For I = 0 To 73: Read expCategories$(I): Next


'----------------------------------------
'          READ IN ALL DATA
'----------------------------------------
Data L,1,1,1,2,2,2,H,H,3,3,3,4,4,F
Data 4-3 READ,5-2 READ,4-3 PINCH,5-2 PINCH,4-3 INSIDE STUNT
Data 5-2 INSIDE STUNT,4-3 OUTSIDE STUNT,5-2 OUTSIDE STUNT
Data 4-3 PASS RUSH,5-2 PASS RUSH
Data STANDARD ZONE,STANDARD M-M,SHORT ZONE,MAN UNDER,NICKEL,PREVENT
Data BLITZ (1-ILB),BLITZ (1-OLB),BLITZ (2-ILB),BLITZ (2-OLB),BLITZ ENGAGE
Data BLAST,DIVE,OFF TACKLE,END SWEEP,OPTION RUN
Data DRAW PLAY,TRAP,3 STEP DROP,SHORT PASS,MED PASS
Data LONG PASS,SHORT SCREEN,MED-PLAY ACT PASS,LONG-PLAY ACT PASS,SIDELINE PASS
Data ROLLOUT PASS,BOOTLEG PASS,PUNT,FG ATT,SCOUT REPORT
Data 0,3,-6,0,-3,0,0,3,-3,-3: 'BLAST DATA
Data 0,3,-3,0,-6,0,0,6,0,0: 'DIVE
Data -3,0,0,3,3,0,0,-3,0,0: 'OFF TACKLE
Data 0,-3,3,0,3,3,-3,-3,0,0: 'SWEEP
Data 0,-3,6,0,6,3,-3,-6,0,0: 'OPTION
Data -6,-6,-3,-3,-3,-3,-3,-3,6,6: 'DRAW
Data 3,3,-3,-3,-6,-6,-6,-6,-3,-3: 'TRAP
Data OFF,5,DEF,5,OFF,10: 'PENALTY INFO
Data 90,88,80,78,60,68,20,62,-5,52: 'FIELD GOAL INFO
Data -3,-4,0,3,-6,-6,0,0,0,0,0: ' INT HITCH
Data -3,-4,0,-1,-6,-6,0,0,0,0,0: ' INT SP
Data 0,-1,0,-1,0,-1,0,0,0,0,0: ' INT MP
Data 3,2,0,-1,4,5,0,0,0,0,0: ' INT LP
Data -6,-6,-6,-6,-6,-6,0,0,0,0,0: ' INT SCREEN
Data 0,-1,0,0,2,0,0,0,0,0,0: ' INT MED PA
Data 3,2,3,2,4,5,0,0,0,0,0: ' INT L PA
Data 0,1,-1,-1,0,0,0,0,0,0,0: ' INT SL
Data 0,0,0,0,0,0,0,0,0,0,0: ' INT ROLLOUT
Data 0,0,0,0,0,0,0,0,0,0,0: ' INT BOOTLEG
Data 31,21,21,11,40,50,8,8,-5,-5,-13: ' C% HITCH
Data 21,11,11,1,30,40,-10,-10,-20,-20,-23: ' C% SP
Data 1,-9,6,-4,-5,-10,-19,-19,-17,-17,-13: ' C% MP
Data -19,-29,-9,-19,-30,-40,-25,-25,-25,-25,-17: ' C% LP
Data 40,40,40,40,50,50,20,20,-24,-24,-33: ' C% SCREEN
Data 6,-19,-9,-19,-10,-10,-17,-17,-13,-13,-6: ' C% MED PA
Data -14,-29,-19,-29,-30,-40,-25,-25,-17,-17,-6: ' C% L PA
Data 6,-14,1,-4,10,10,-13,-13,-19,-19,-17: ' C% SL
Data -5,5,-10,10,0,0,-19,-17,-19,-13,-13: ' C% ROLLOUT
Data -5,5,-10,10,0,0,-19,-17,-19,-13,-13: ' C% BOOTLEG
Data 0,1,-1,0,1,2,1,1,2,2,3: ' Y/C HITCH
Data 0,1,-1,0,1,2,3,3,21,21,25: ' Y/C SP
Data 0,1,0,0,1,2,21,21,25,25,28: ' Y/C MED
Data 0,1,1,2,-1,-1,33,33,42,42,42: ' Y/C LP
Data 0,1,-1,0,1,2,1,1,25,25,36: ' Y/C SCREEN
Data 1,1,-1,-2,-1,-1,25,25,28,28,31: ' Y/C MED PA
Data 1,1,1,2,-1,-1,42,42,42,42,42: ' Y/C L PA
Data 0,-1,1,2,1,2,18,18,28,28,33: ' Y/C SL
Data 0,1,-1,0,1,2,21,25,21,28,28: ' Y/C ROLL
Data 0,1,-1,0,1,2,21,25,21,28,28: ' Y/C BOOTLEG
Data -8,-8,-4,-4,-8,-8,-6,-8,-4,-8,-4: ' SCK HITCH
Data -4,-4,0,0,-8,-8,-2,-2,0,0,2: ' SCK SP
Data 0,0,-2,-2,2,-6,3,3,13,13,23: ' SCK MP
Data 4,4,0,0,8,-4,13,13,23,23,33: ' SCK LP
Data -8,-8,-8,-4,-8,-8,-4,-4,0,0,3: ' SCK SCRN
Data 0,0,-2,-2,2,-4,13,13,23,23,33: ' SCK MED PA
Data 4,4,0,0,8,-2,23,23,33,33,43: ' SCK L PA
Data -4,-4,-2,-2,2,-6,0,0,3,3,13: ' SCK SL
Data -6,-6,-4,-4,-8,-8,3,13,3,23,23: ' SCK ROLL
Data -6,-6,-4,-4,-8,-8,3,13,3,23,23: ' SCK BOOTLEG
Data 0,0,1,4,11,21,31,49,62,74,81,85,88,90,92: ' RM%(0, )
Data 3,5,8,10,15,23,32,47,58,68,75,80,85,88,92: ' RM%(1, )
Data 69,80,77,61,62,77,64,45,36,69,49,11,2,15,48,45,31,21,29,78,55,48,65,70,58,61,78,70,68,65,70,63,65,70,76,81,87,79: ' RN%(0, )
Data -4,-3,-3,-3,-3,-2,-2,-2,-2,-1,-1,-1,1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,6,6,6,6,6,6,7: ' RN%(1, )
'BY%
Data 11,-5,11,0
Data 11,-5,11,0
Data 11,0,21,0
Data 11,0,21,0
Data 11,0,11,10
Data 11,0,11,10
Data 11,0,21,10
Data 11,0,21,10
Data 11,0,21,10
Data 11,10,21,10
Data 11,10,31,10
Data 11,10,31,10
Data 21,10,31,10
Data 21,10,51,10
Data 21,10,51,10
Data 21,10,41,20
Data 21,10,51,20
Data 21,10,81,20
Data 21,10,71,30
Data 21,10,61,40
Data 21,10,31,70
Data 31,10,51,50
Data 31,10,31,70
Data 41,10,31,70
Data 51,10,31,70
Data 41,20,11,90
Data 41,20,11,90
Data 21,40,11,90
Data 21,40,11,90
Data 71,30,11,90
Data 61,40,11,90
Data 51,50,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90
Data 11,90,11,90

'GOAL LINE
Data 72,-5,83,-4,75,-4,68,-4,80,-3,70,-3,60,-3,75,-2,60,-2,45,-2,60,-1
Data 30,-1,100,0,30,1,60,1,45,2,60,2,75,2,60,3,70,3,80,3,68,4,75,4,83,4
Data 72,5,78,5,84,5,90,5,96,5,96,5

Data 28,89,99,100,0,0,0,0,0,0,0,0,0,0: 'HITCH 1
Data 10,53,96,97,98,99,100,0,0,0,0,0,0,0: 'HITCH 2
Data 2,37,83,96,97,98,99,100,0,0,0,0,0,0: ' HITCH 3
Data 2,32,71,92,94,96,97,98,99,100,0,0,0,0: ' HITCH 4
Data 2,22,59,92,93,94,100,0,0,0,0,0,0,0: ' HITCH 5
Data 2,12,63,88,90,92,93,94,95,96,97,98,99,100: ' HITCH 6
Data 2,6,38,86,88,90,92,94,95,96,97,98,99,100: ' HITCH 7
Data 2,6,25,74,82,90,92,94,95,96,97,98,99,100: ' HITCH 8
Data 2,6,25,69,77,85,87,90,92,95,97,98,99,100: ' HITCH 9
Data 2,6,25,66,74,82,84,87,89,92,94,96,98,100: ' HITCH 10
Data 34,72,98,99,100,0,0,0,0,0,0,0,0,0: ' SCREEN 1
Data 14,54,97,98,99,100,0,0,0,0,0,0,0,0: ' SCREEN 2
Data 10,55,80,95,96,97,98,99,100,0,0,0,0,0: ' SCREEN 3
Data 10,51,75,91,93,95,96,97,98,99,100,0,0,0: ' SCREEN 4
Data 10,30,70,90,92,94,95,96,97,98,99,100,0,0: ' SCREEN 5
Data 10,35,71,86,88,90,92,94,95,96,97,98,99,100: ' SCREEN 6
Data 10,32,68,83,85,87,89,91,93,95,96,98,99,100: ' SCREEN 7
Data 10,29,65,80,82,85,87,89,91,93,95,97,99,100: ' SCREEN 8
Data 10,21,57,72,77,82,86,89,91,93,95,97,99,100: ' SCREEN 9
Data 10,13,49,64,72,79,85,89,91,93,95,97,99,100: ' SCREEN 10
Data 0,97,99,100,0,0,0,0,0,0,0,0,0,0: ' SP 1
Data 0,45,96,98,99,100,0,0,0,0,0,0,0,0: ' SP 2
Data 0,10,82,97,98,99,100,0,0,0,0,0,0,0: ' SP 3
Data 0,0,63,94,95,96,97,98,99,100,0,0,0,0: ' SP 4
Data 0,0,45,91,93,95,96,97,98,99,100,0,0,0: ' SP 5
Data 0,0,45,80,86,92,94,95,96,97,98,99,100,0: ' SP 6
Data 0,0,40,70,80,90,93,94,95,96,97,98,99,100: ' SP 7
Data 0,0,40,65,75,85,88,90,93,94,96,98,99,100: ' SP 8
Data 0,0,30,51,66,81,88,90,92,94,96,98,99,100: ' SP 9
Data 0,0,20,37,57,77,88,90,92,94,96,98,99,100: ' SP 10
Data 17,77,94,99,100,0,0,0,0,0,0,0,0,0: ' MP 1
Data 6,35,76,96,97,98,99,100,0,0,0,0,0,0: ' MP 2
Data 0,0,52,92,96,98,100,0,0,0,0,0,0,0: ' MP 3
Data 0,0,40,80,87,93,95,97,99,100,0,0,0,0: ' MP 4
Data 0,0,4,69,81,89,94,96,98,100,0,0,0,0: ' MP 5
Data 0,0,5,58,70,78,83,87,91,95,98,100,0,0: ' MP 6
Data 0,0,0,41,57,70,78,83,88,92,95,98,100,0: ' MP 7
Data 0,0,0,19,36,51,66,78,88,92,94,96,98,100: ' MP 8
Data 0,0,0,13,26,43,61,76,86,90,93,96,98,100: ' MP 9
Data 0,0,0,11,22,33,51,66,76,82,87,92,96,100: ' MP 10
Data 1,56,98,100,0,0,0,0,0,0,0,0,0,0: ' LP 1
Data 0,0,53,95,96,100,0,0,0,0,0,0,0,0: ' LP 2
Data 0,0,0,67,87,97,98,99,100,0,0,0,0,0: ' LP 3
Data 0,0,0,13,65,97,98,99,100,0,0,0,0,0: ' LP 4
Data 0,0,0,0,32,64,96,97,98,99,100,0,0,0: ' LP 5
Data 0,0,0,0,12,40,82,86,90,93,96,98,100,0: ' LP 6
Data 0,0,0,0,0,26,69,75,81,86,91,94,97,100: ' LP 7
Data 0,0,0,0,0,0,17,40,64,72,80,88,94,100: ' LP 8
Data 0,0,0,0,0,0,6,29,54,64,74,84,92,100: ' LP 9
Data 0,0,0,0,0,0,3,23,44,56,68,80,90,100: ' LP 10
Data 0,10,20,30,40,50,60,70,80,90,50,55,60,65,70,75,80,85,90,95,100
Data 0,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2

'QB Y/C ADJ
Data 0,10,20,30,40,50,60,70,40,45,50,55,60,65,70,50,53,57,60,63,67
Data 0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3

For I = 0 To 14: Read R$(I): Next
For I = 1 To 10: Read D$(I): Next I
For I = 1 To 11: Read D1$(I): Next I
For I = 1 To 20: Read O$(I): Next I
For I = 1 To 7: For I1 = 1 To 10: Read D3%(I, I1): Next I1: Next I
For I = 0 To 2: Read P$(I): Read P(I): Next I
For I = 0 To 4: Read F(I): Read F1(I): Next I
For I = 0 To 3: For I1 = 1 To 10: For J = 1 To 11: Read S1%(I, I1, J): Next J: Next I1: Next I
For I = 0 To 1: For I1 = 0 To 14: Read RM%(I, I1): Next: Next
For I = 0 To 1: For I1 = 1 To 38: Read RN%(I, I1): Next: Next
For I = 1 To 38: For I1 = 1 To 4: Read BY%(I, I1): Next: Next
For I = 1 To 30: For I1 = 1 To 2: Read GL%(I, I1): Next: Next
For I = 1 To 5: For I1 = 1 To 10: For J = 1 To 14: Read S2%(I, I1, J): Next: Next: Next
For I = 1 To 2: For I1 = 1 To 21: Read PS%(I, I1): Next: Next
For I = 1 To 2: For I1 = 0 To 20: Read QR%(I1, I): Next: Next
For I = 1 To 29: Read RP$(I): Next

'I think these are alternate offense options
'that did not get implemented. There is some
'overlap so perhaps a rewrite of sorts was
'in progress.
Data BLAST,DIVE,OFF TACKLE,TOSS SWEEP,OPTION RUN,DRAW PLAY,MISDIRECTION
Data QB SNEAK,POWER SWEEP,QUICK PITCH,QUICK TRAP,ISOLATION,SPRINT DRAW
Data OVER THE TOP,COUNTER
Data 3 STEP DROP,SHORT PASS,MEDIUM PASS,LONG PASS,SCREEN PASS
Data MED PLAY ACT PASS,LONG PLAY ACT PASS,SIDELINE PASS,ROLLOUT PASS
Data BOOTLEG PASS,CROSS,CURL PASS,DEEP POST,DEEP CORNER


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

opSysType$ = Left$(_OS$, InStr(2, _OS$, "[") - 1)

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 2
        Select Case opSysType$
            Case "[WINDOWS]": Print #1, _CWD$ + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": Print #1, _CWD$ + "/"
        End Select
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 2
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

Errhandler:
Open "errlog" For Append As #9
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
Print #9,
Close #9
Resume Next ' moves program to code following the error.

'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    'Dim A$(1 To 15)

    Do
        Cls

        Call TITLE

        'Erase menuChoices$
        ReDim menuChoices$(1 To 8)
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
                Case 1: Call FOOTBALL
                Case 2: Call TeamMgt_Menu
                Case 3: Call Replay_Menu
                Case 4: Call Leaders_Menu
                Case 5: Call SCHEDULE
                Case 6: '
                Case 7: Call ConfigureDrives
                Case 8: Cls
            End Select
        End If

    Loop Until menuSelection% = 8 Or ExitCode% = 27

    If ExitCode% = 27 Then menuSelection% = 8

End Sub


'----------------------------------------
'       ConfigureDrives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

    Shared opSysType$

    Color 15, 1
    FG = 0
    BG = 7

    For I = 0 To 2
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print
    Locate , 31: Print "PATHNAME EXAMPLES:"
    Select Case opSysType$
        Case "[WINDOWS]":
            Locate , 31: Print "A:"
            Locate , 31: Print "C:\FOOT\TEAMS"
        Case "[LINUX]":
            Locate , 31: Print "/mnt/floppy"
            Locate , 31: Print "/usr/bin/foot/teams"
        Case "[MAC]", "[MACOSX]":
            Locate , 31: Print "/Volumes/floppy"
            Locate , 31: Print "/Applications/Foot/teams"
    End Select

    Print
    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 61), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 61), FG, BG, -1
    Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
    Locate , 18: QPrint PadRight$(diskPaths$(2), 59), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    GetDiv3:
    Locate 11, 18 'location of beginning of field
    CapsOn = 0
    NumOnly = 0
    Max = 50
    Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 Then GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 Then GoTo Done 'all done entering data
    diskPaths$(2) = _Trim$(diskPaths$(2))

    Done:
    Locate 23, 1
    Color 14, 1
    Print "Is this okay? (Yes/No) ";
    X$ = ""
    Call YesNoB(X$, XCode, FG, BG)
    Locate , 1
    ClearEOL 14, 1
    If XCode = 1 Then GoTo GetDiv3
    If X$ = "N" Then GoTo GetDiv1
    If X$ <> "Y" Then GoTo Done 'insist on either Y or N or up-arrow

    Open "DEV.BAS" For Output As #1

    For R = 0 To 2:
        Select Case opSysType$
            Case "[WINDOWS]": diskPaths$(R) = diskPaths$(R) + "\"
            Case "[MAC]", "[MACOSX]", "[LINUX]": diskPaths$(R) = diskPaths$(R) + "/"
        End Select
        Print #1, diskPaths$(R)
    Next R

    Close #1

End Sub


'----------------------------------------
'        TeamMgmt_Menu Subroutine
'----------------------------------------
'...explanation...
Sub TeamMgt_Menu ()

    Do
        Cls

        Call TITLE
        ReDim menuChoices$(1 To 7)
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
                Case 1:
                    Call TINPUT
                Case 2:
                    Call GetTeam_CNGTM(YN$, tmNbr)
                    Call ChangeTeam(YN$, tmNbr)
                Case 3:
                    Call GetTeam_CNGTM(YN$, tmNbr)
                    Call PrintRoster
                Case 4:
                    Call GetTeam_CNGTM(YN$, tmNbr)
                    Call ViewRoster
                Case 5:
                    Call DRAFT
                Case 6:
                    Call TRADE
                Case 7:
                    Cls
            End Select
        End If

    Loop Until menuSelection% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'         Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

    Do

        Cls

        Call TITLE

        'Erase menuChoices$
        ReDim menuChoices$(1 To 4)
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

    Loop Until Selection% = 4 Or ExitCode% = 27

End Sub


'----------------------------------------
'        Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

    Do

        Call TITLE
        ReDim menuChoices$(1 To 21)
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

        Color 15, 0
        Locate 8, 4: Print "PROFESSIONAL LEAGUE"
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
                Case 5: Call REC(1)
                Case 6: Call NEWLDR
                Case 7: Call BLANKPRO
                Case 8: Call CREATCOL
                Case 9: Call COMPCOLL
                Case 10: Call SEECOLL
                Case 11: Call HD2HDCOL
                Case 12: Call REC(2)
                Case 13: Call NEWLDR
                Case 14: Call BLANKCOL
                Case 15: Call CREATNAT
                Case 16: Call COMPNATL
                Case 17: Call SEENATL
                Case 18: Call REC(3)
                Case 19: Call BLANKORG
                Case 20 Or 21: Cls
            End Select
        End If

        If Selection% = 21 Then
            End
            System
        End If
    Loop Until Selection% >= 20 Or ExitCode% = 27

End Sub


'$INCLUDE: 'KeyInput.bm'
'$INCLUDE: 'QPProEqu.bm'
'$INCLUDE: 'PadRight.bm'

' For Utils
'$INCLUDE: 'MainRoutines.bm'
'$INCLUDE: 'TITLE.bm'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BLANKCOL.BAS'
'$INCLUDE: 'BLANKORG.BAS'
'$INCLUDE: 'BLANKPRO.BAS'
'$INCLUDE: 'CAREDIT.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARMERGE.BAS'
'$INCLUDE: 'CARPRNT.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CNGTM.BAS'
'$INCLUDE: 'COMPCOLL.BAS'
'$INCLUDE: 'COMPNATL.BAS'
'$INCLUDE: 'COMPPRO.BAS'
'$INCLUDE: 'CREATCOL.BAS'
'$INCLUDE: 'CREATNAT.BAS'
'$INCLUDE: 'CREATPRO.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'HD2HDCOL.BAS'
'$INCLUDE: 'HD2HDPRO.BAS'
'INCLUDE: 'LOOKY.BAS'




Sub LOOKY ()

    _Title "3-in-1 Football - View Stats"

    Shared AF!(), AM!(), GI!(), GS!()
    Shared K!(), K2!(), PK!(), W6!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared Z!(), Z1!(), Z2!()

    Shared statsA$()
    Shared lookyDI$(), lookyDS$()
    Shared LC$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared statsPK$()

    Shared lookyP%()

    'This is read in from the file but not used???
    Dim statsLF$(1)

    Do

        Color , 0
        Cls

        JJ = 6

        Locate 8, 8: Color 15, 1

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 8 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 9 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate 10, 15
        Locate , 30: Color 3: Print "VIEW/PRINT STATS IN:"
        Locate , 30: Color 15: Print "C";: Color 7: Print "OLLEGE FORMAT"
        Locate , 30: Color 15: Print "P";: Color 7: Print "RO FORMAT"
        Locate , 30: Color 15: Print "E";: Color 7: Print "XIT TO MAIN MENU"

        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "C": statFormat = 0
            Case "P": statFormat = 1
            Case "E", Chr$(27): statFile$ = "END"
        End Select

        If statFile$ <> "END" Then

            'for "C", "P"
            Call GetDisk_LOOKY(numberTeams, YN$)

            B$ = A$

            For XX = 4 To 11
                If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
            Next XX

            For XX = 12 To 14
                If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
            Next XX

            QQ = 8
            For XX = 11 To 4 Step -1
                If Mid$(B$, XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            statFile$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

            Color 15, 0
            Cls
            Call TITLE_STATS

            D% = 0
            PX = 0
            lookyP%(0) = 0
            lookyP%(1) = 0
            L% = 0
            W% = 0
            X% = 0

            Call LoadTeamStats(statFile$, numberGames%, teamPower, SS)

            If Not _FileExists(diskPaths$(1) + statFile$) Then
                Locate 10, 8
                Call CSMsgBox("UNABLE TO LOCATE COMPILED STATS FILE PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else
                'Load player stats
                Open diskPaths$(1) + statFile$ For Input As #1

                Input #1, statsA$(PX)

                For I = 0 To 9: Input #1, lookyA1$(PX, I): Next I
                For I = 0 To 5: Input #1, lookyA2$(PX, I): Next I
                For I = 0 To 3: Input #1, lookyA3$(PX, I): Next I
                For I = 0 To 2: Input #1, lookyA4$(PX, I): Next I
                For I = 0 To 2: Input #1, lookyA5$(PX, I): Next I

                Input #1, lookyA6$(PX, 0)

                For I = 0 To 1: Input #1, lookyA7$(PX, I): Next I
                For J = 0 To 13
                    For K = 0 To 17: Input #1, Z2!(J, K): Next K
                Next J

                For L = 0 To 38: Input #1, Z!(L): Next L
                For L = 0 To 38: Input #1, Z1!(L): Next L
                For J = 0 To 9: Input #1, lookyDI$(PX, J): For J1 = 0 To 2: Input #1, GI!(PX, J, J1): Next: Next
                For J = 0 To 14: Input #1, lookyDS$(PX, J): For J1 = 0 To 1: Input #1, GS!(PX, J, J1): Next: Next
                For I = 0 To 9: Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
                For I = 0 To 15: Input #1, LC!(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
                For I = 0 To 3: Input #1, LP!(I): Input #1, statsLP$(I, 0): Input #1, statsLP$(I, 1): Next
                For I = 0 To 2: Input #1, LK!(I): Input #1, statsLK$(I, 0): Input #1, statsLK$(I, 1): Next
                For I = 0 To 2: Input #1, PK!(I): Input #1, statsPK$(I, 0): Input #1, statsPK$(I, 1): Next
                For I = 0 To 1: Input #1, LF!(I): Input #1, statsLF$(I): For I1 = 0 To 4: Input #1, AM!(I, I1): Input #1, AF!(I, I1): Next: Next
                For I = 0 To 9: Input #1, LI!(I): Input #1, statsLI$(I, 0): Input #1, statsLI$(I, 1): Next

                Close #1

                For J = 0 To 13
                    For K = 0 To 17: K2!(PX, J, K) = Z2!(J, K): Next K
                Next J

                For M = 0 To 12: K!(0, M) = Z!(M): Next M
                For M = 0 To 12: K!(1, M) = Z1!(M): Next M

                W6!(0, 0) = Z!(13)
                W6!(0, 1) = Z!(14)
                W6!(1, 0) = Z1!(13): W6!(1, 1) = Z1!(14)

                ' *** VIEW STATS ON: SCREEN X=0, PRINTER X=1 ***
                Do
                    DM% = 0

                    Cls

                    Call TITLE_STATS

                    J = 4
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

                    Do
                        Locate 10, 15
                        Color 7, 1

                        Locate , 30: Print "STATS TO ";: Color 15: Print "S";: Color 7: Print "CREEN"
                        Locate , 30: Print "STATS TO ";: Color 15: Print "P";: Color 7: Print "RINTER"
                        I$ = GetKeyPress$

                        Color 14, 0

                    Loop Until UCase$(I$) = "S" Or UCase$(I$) = "P"

                    Select Case UCase$(I$)
                        Case "S": X = 0: Call StatsToScreen(X, numberGames%, statFormat, teamPower, SS)
                        Case "P": X = 1: Call StatsToPrinter(X, numberGames%, statFormat, teamPower, SS)
                    End Select

                    Cls
                    Print "DISPLAY STATISTICS AGAIN (YN)";

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    'Call ResetStatsPrompt (statFile$)

                Loop Until UCase$(I$) = "N"

            End If 'Done checking for file existence

        End If 'Done checking for statFile$ = "END"

    Loop Until UCase$(I$) = "N" Or statFile$ = "END"

End Sub

'----------------------------------------
'       LoadTeamStats Subroutine
'----------------------------------------
'...explanation...
Sub LoadTeamStats (statFile$, numberGames%, teamPower, SS)

    Shared CRD!()
    Shared statsO%(), statsT%(), lookyP%()
    Shared statsH$(), compO$(), gameSITE$()

    Dim compAP%(1, 120)

    If Not _FileExists(diskPaths$(1) + statFile$) Then
        Cls
        Print "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
        I$ = GetKeyPress$
    Else

        Open diskPaths$(1) + statFile$ + "S" For Input As #2
        Input #2, X$
        numberGames% = Val(X$)

        'Opponent Score
        For I = 1 To 120: Input #2, statsO%(I): Next I

        'Opponent
        For I = 1 To 120: Input #2, compO$(I): Next I

        'Team Score
        For I = 1 To 120: Input #2, statsT%(I): Next I

        'Home / Away
        For I = 1 To 120: Input #2, statsH$(I): Next I

        For I = 0 To 1
            For I1 = 1 To 120
                ' ???
                Input #2, compAP%(I, I1)
            Next
        Next

        'Attendance
        For I = 1 To 120: Input #2, CRD!(I): Next I

        'Stadium
        For I = 1 To 120: Input #2, gameSITE$(I): Next I

        Close #2

        'STRENGTH OF SCHEDULE
        SS = 0

        For I = 1 To numberGames%
            SS = compAP%(1, I) + SS
        Next

        SS = SS / numberGames%

        'INDIVIDUAL GAME POWER RATINGS
        teamPower = 0

        For I = 1 To numberGames%
            RAT = statsO%(I) - statsT%(I) + compAP%(1, I)
            If Left$(statsH$(I), 1) = "H" Then RAT = RAT - 3
            If Left$(statsH$(I), 1) = "A" Then RAT = RAT + 3
            teamPower = teamPower + RAT
        Next

        GL = 0

        If numberGames% <= 4 Then
            GL = 5 - numberGames%
            teamPower = teamPower + GL * compAP%(0, 1)
        End If

        teamPower = teamPower / (numberGames% + GL)

    End If

End Sub

'----------------------------------------
'      GetDisk_LOOKY Subroutine
'----------------------------------------
'...explanation...
Sub GetDisk_LOOKY (numberTeams, yearNumber$)

    Shared statsPA%()

    Do

        Color 15, 0
        Cls

        JJ = 6
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
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ = Chr$(13) Then
                Call SelectTeam_LOOKY(numberTeams, yearNumber$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" Then
                    Call SelectTeam_LOOKY(numberTeams, NY$)
                Else
                    yearNumber$ = NY$
                    Erase teamNames$
                    numberTeams = 0

                    Open diskPaths$(0) + "FOOT." + yearNumber$ For Random As #1 Len = 934
                    Y = LOF(1)
                    Field #1, 15 As Q$(0), 640 As X$

                    For I1 = 1 To Y / 934
                        Get #1, I1
                        For I2 = 1 To 15
                            If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                        Next I2
                        If nullTeam <> 1 Then
                            numberTeams = numberTeams + 1
                            teamNames$(numberTeams) = Q$(0)
                            teamIndex%(numberTeams) = I1
                        End If
                    Next I1

                    Close 1

                    Call SelectTeam_LOOKY(numberTeams, yearNumber$)

                End If

            End If

            If BO% = 1 Then
                BO% = 0
            Else
                A$ = RTrim$(Q$(1))
                Locate 2, 33
                Color statsPA%(2), statsPA%(1): Print A$

                Color 15, 4

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                'Read in stats here???

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

    Color 15, 0

End Sub

'----------------------------------------
'       SelectTeam_LOOKY Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_LOOKY (numberTeams, yearNumber$)

    Shared statsPA%()

    Selection% = 1
    Count% = numberTeams
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
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then
        T1 = teamIndex%(Selection%)

        If Not _FileExists(diskPaths$(0) + "FOOT." + yearNumber$) Then
            BO% = 1
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else
            Open diskPaths$(0) + "FOOT." + yearNumber$ For Random As #1 Len = 934

            Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

            Get #1, T1

            For I1 = 0 To 4
                statsPA%(I1) = CVI(Q$(I1 + 11))
            Next I1

            Close 1

        End If

    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'      StatsToScreen Subroutine
'----------------------------------------
'...explanation...
Sub StatsToScreen (X, numberGames%, statFormat, teamPower, SS)

    Shared AF!(), AM!(), CRD!(), GI!(), GS!()
    Shared K!(), K2!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared PK!(), W6!(), Z!(), Z1!(), Z2!()

    Shared statsO%(), statsT%(), lookyP%()

    Shared statsA$(), lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared lookyDI$(), lookyDS$(), statsH$()
    Shared LC$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared compO$(), statsPK$(), gameSITE$()

    If X = 0 Then Cls

    ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***
    Cls
    Color 12: Print "GAME-BY-GAME REPORT FOR "; statsA$(0)
    Color 14
    Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"

    For I = 1 To numberGames%
        Color 7

        If I / 2 = Int(I / 2) Then Color 15

        Print compO$(I);
        Locate , 19: Print Using "###"; statsO%(I);: Print "-";: Print Using "###   "; statsT%(I);

        If statsO%(I) > statsT%(I) Then W% = W% + 1: Print "W";
        If statsO%(I) < statsT%(I) Then L% = L% + 1: Print "L";
        If statsO%(I) = statsT%(I) Then X% = X% + 1: Print "T";

        Locate , 31: Print Using "####,###   "; CRD!(I);
        Print statsH$(I); "  "; Left$(gameSITE$(I), 24);
        Locate , 69
        Print "(";: Print Using "##"; W%;: Print "-";: Print Using "##"; L%;: Print "-";: Print Using "##"; X%;: Print ")"

        lookyP%(0) = lookyP%(0) + statsO%(I): lookyP%(1) = lookyP%(1) + statsT%(I)

        If I = 20 Or I = 40 Or I = 60 Or I = 80 Or I = 100 Or I = 120 Then
            I$ = GetKeyPress$
            ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***
            Cls
            Color 12: Print "GAME-BY-GAME REPORT FOR "; statsA$(0)
            Color 14
            Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"
        End If
    Next I

    If statFormat = 0 Then Locate 24, 1: Print Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS;

    lookyP%(0) = 0
    lookyP%(1) = 0
    W% = 0
    L% = 0
    X% = 0

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If

    Call TITLE_STATS

    Locate 2
    Color 12
    Print statsA$(0); " STATISTICS REPORT"
    Print

    Color 14: Print "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"

    Color 15: Print statsA$(0);
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; Z!(32); Z!(33); Z!(34); Z!(35); Z!(32) + Z!(33) + Z!(34) + Z!(35) + Z!(36)
    Print "OPPONENTS";
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; Z1!(32); Z1!(33); Z1!(34); Z1!(35); Z1!(32) + Z1!(33) + Z1!(34) + Z1!(35) + Z!(36)
    Print
    Color 14: Print "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Color 15: Print statsA$(0);
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z!(32) / numberGames%; Z!(33) / numberGames%; Z!(34) / numberGames%; Z!(35) / numberGames%; (Z!(32) + Z!(33) + Z!(34) + Z!(35) + Z!(36)) / numberGames%
    Print "OPPONENTS";
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z1!(32) / numberGames%; Z1!(33) / numberGames%; Z1!(34) / numberGames%; Z1!(35) / numberGames%; (Z1!(32) + Z1!(33) + Z1!(34) + Z1!(35) + Z!(36)) / numberGames%
    Print

    'H = Home; A = Away; N = Neutral; T = Total
    'D = Dates; T = Total; A = Average
    HD& = 0: AD& = 0: ND& = 0: TD& = 0
    HT& = 0: CT& = 0: NT& = 0: TT& = 0
    HA& = 0: AA& = 0: NA& = 0: TA& = 0

    For I = 1 To numberGames%
        If Left$(statsH$(I), 1) = "H" Then HD& = HD& + 1: HT& = HT& + CRD!(I)
        If Left$(statsH$(I), 1) = "A" Then AD& = AD& + 1: CT& = CT& + CRD!(I)
        If Left$(statsH$(I), 1) = "X" Then ND& = ND& + 1: NT& = NT& + CRD!(I)
        TD& = TD& + 1
        TT& = TT& + CRD!(I)
    Next

    If HD& <> 0 Then HA& = HT& / HD&
    If AD& <> 0 Then AA& = CT& / AD&
    If ND& <> 0 Then NA& = NT& / ND&
    If TD& <> 0 Then TA& = TT& / TD&

    Color 14: Print "ATTENDANCE    DATES      TOTAL    AVERAGE"
    Color 15: Print Using "HOME            ##    #####,###   ###,###"; HD&; HT&; HA&
    Color 15: Print Using "AWAY            ##    #####,###   ###,###"; AD&; CT&; AA&
    Color 15: Print Using "NEUTRAL         ##    #####,###   ###,###"; ND&; NT&; NA&
    Color 15: Print Using "TOTAL           ##    #####,###   ###,###"; TD&; TT&; TA&

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If

    Call TITLE_STATS

    Locate 2
    Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); statsA$(0); Tab(61); "OPPONENT TOTALS"

    Color 7
    Print "FIRST DOWNS"; Tab(45); Using "####"; K!(0, 0);: Print Tab(65); Using "####"; K!(1, 0)
    Print "RUSHING ATTEMPTS"; Tab(45);
    'Team
    If statFormat = 0 Then Print Using "####"; K!(0, 1); Else Print Using "####"; K!(0, 1) - W6!(0, 0);
    Print Tab(65);
    'Opponent
    If statFormat = 0 Then Print Using "####"; K!(1, 1) Else Print Using "####"; K!(1, 1) - W6!(1, 0)

    Color 15
    Print "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then Print Using "####"; K!(0, 2); Else Print Using "####"; K!(0, 2) + W6!(0, 1);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; K!(1, 2) Else Print Using "####"; K!(1, 2) + W6!(1, 1)

    Color 7
    Print "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And K!(0, 1) <> 0 Then Print Using "##.#"; K!(0, 2) / K!(0, 1);
    If statFormat <> 0 And K!(0, 1) - W6!(0, 0) <> 0 Then Print Using "##.#"; (K!(0, 2) + W6!(0, 1)) / (K!(0, 1) - W6!(0, 0));
    Print Tab(65);
    If statFormat = 0 And K!(1, 1) <> 0 Then Print Using "##.#"; K!(1, 2) / K!(1, 1)
    If statFormat <> 0 And K!(1, 1) - W6!(1, 0) <> 0 Then Print Using "##.#"; (K!(1, 2) + W6!(1, 1)) / (K!(1, 1) - W6!(1, 0))
    If statFormat = 0 And K!(1, 1) = 0 Or statFormat <> 0 And K!(1, 1) - W6!(1, 0) = 0 Then Print
    Print "RUSHING YARDS PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; K!(0, 2) / numberGames%; Else Print Using "###.#"; (K!(0, 2) + W6!(0, 1)) / numberGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; K!(1, 2) / numberGames% Else Print Using "###.#"; (K!(1, 2) + W6!(1, 1)) / numberGames%
    Print
    Print "PASSES ATTEMPTED"; Tab(45); Using "####"; K!(0, 3);: Print Tab(65); Using "####"; K!(1, 3)
    Print "PASSES COMPLETED"; Tab(45); Using "####"; K!(0, 4);: Print Tab(65); Using "####"; K!(1, 4)
    Print "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; K!(0, 5);: Print Tab(65); Using "####"; K!(1, 5)
    Print "PASS COMPLETION PERCENTAGE"; Tab(44);

    If K!(0, 3) <> 0 Then Print Using "###.#"; K!(0, 4) / K!(0, 3) * 100;
    If K!(1, 3) <> 0 Then Print Tab(64); Using "###.#"; K!(1, 4) / K!(1, 3) * 100
    If K!(1, 3) = 0 Then Print

    Color 15
    Print "NET YARDS PASSING"; Tab(45); Using "####"; K!(0, 6);: Print Tab(65); Using "####"; K!(1, 6)

    Color 7
    Print "YARDS PER PASS ATTEMPT"; Tab(45);

    If K!(0, 3) <> 0 Then Print Using "##.#"; K!(0, 6) / K!(0, 3);
    If K!(1, 3) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 6) / K!(1, 3)
    If K!(1, 3) = 0 Then Print

    Print "YARDS PER PASS COMPLETION"; Tab(45);

    If K!(0, 4) <> 0 Then Print Using "##.#"; K!(0, 6) / K!(0, 4);
    If K!(1, 4) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 6) / K!(1, 4)
    If K!(1, 4) = 0 Then Print

    Print "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; K!(0, 6) / numberGames%;: Print Tab(64); Using "###.#"; K!(1, 6) / numberGames%
    Print

    Print "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then Print Using "####"; K!(0, 1) + K!(0, 3); Else Print Using "####"; K!(0, 1) - W6!(0, 0) + K!(0, 3);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; K!(1, 1) + K!(1, 3) Else Print Using "####"; K!(1, 1) - W6!(1, 0) + K!(1, 3)

    Print "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; (K!(0, 1) + K!(0, 3)) / numberGames%; Else Print Using "###.#"; (K!(0, 1) - W6!(0, 0) + K!(0, 3)) / numberGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; (K!(1, 1) + K!(1, 3)) / numberGames% Else Print Using "###.#"; (K!(1, 1) - W6!(1, 0) + K!(1, 3)) / numberGames%

    Color 15
    Print "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then Print Using "####"; K!(0, 2) + K!(0, 6); Else Print Using "####"; K!(0, 2) + W6!(0, 1) + K!(0, 6);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; K!(1, 2) + K!(1, 6) Else Print Using "####"; K!(1, 2) + W6!(1, 1) + K!(1, 6)

    Color 7
    Print "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then Print Using "##.#"; (K!(0, 2) + K!(0, 6)) / (K!(0, 1) + K!(0, 3)); Else Print Using "##.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / (K!(0, 1) - W6!(0, 0) + K!(0, 3));
    Print Tab(64);
    If statFormat = 0 Then Print Using "##.#"; (K!(1, 2) + K!(1, 6)) / (K!(1, 1) + K!(1, 3)) Else Print Using "##.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / (K!(1, 1) - W6!(1, 0) + K!(1, 3))

    Print "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; (K!(0, 2) + K!(0, 6)) / numberGames%; Else Print Using "###.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / numberGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; (K!(1, 2) + K!(1, 6)) / numberGames% Else Print Using "###.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / numberGames%

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If

    Call TITLE_STATS
    Locate 2: Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); statsA$(0); Tab(61); "OPPONENT TOTALS"

    Color 7

    Print "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print Using "###"; Z!(22);: Print "/";: Print Using "####"; Z!(23);
    Print Tab(61); Using "###"; Z1!(22);: Print "/";: Print Using "####"; Z1!(23)

    Print "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If Z!(22) <> 0 Then Print Using "##.#"; Z!(23) / Z!(22);
    If Z1!(22) <> 0 Then Print Tab(65); Using "##.#"; Z1!(23) / Z1!(22)
    If Z1!(22) = 0 Then Print

    Print "KICKOFF RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; Z!(22) / numberGames%;: Print Tab(65); Using "##.#"; Z1!(22) / numberGames%

    Print "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print Using "###"; Z!(24);: Print "/";: Print Using "####"; Z!(25);
    Print Tab(61); Using "###"; Z1!(24);: Print "/";: Print Using "####"; Z1!(25)

    Print "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If Z!(24) <> 0 Then Print Using "##.#"; Z!(25) / Z!(24);
    If Z1!(24) <> 0 Then Print Tab(65); Using "##.#"; Z1!(25) / Z1!(24)
    If Z1!(24) = 0 Then Print

    Print "PUNT RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; Z!(24) / numberGames%;: Print Tab(65); Using "##.#"; Z1!(24) / numberGames%

    Print "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print Using "###"; Z!(26);: Print "/";: Print Using "####"; Z!(27);
    Print Tab(61); Using "###"; Z1!(26);: Print "/";: Print Using "####"; Z1!(27)

    Print "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If Z!(26) <> 0 Then Print Using "##.#"; Z!(27) / Z!(26);
    If Z1!(26) <> 0 Then Print Tab(65); Using "##.#"; Z1!(27) / Z1!(26)
    If Z1!(26) = 0 Then Print

    Print "INTERCEPTION RETURNS PER GAME"; Tab(45);: Print Using "##.#"; Z!(26) / numberGames%;: Print Tab(65); Using "##.#"; Z1!(26) / numberGames%
    Print

    Print "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print Using "###"; K!(0, 7);: Print "/";: Print Using "####"; K!(0, 8);
    Print Tab(61); Using "###"; K!(1, 7);: Print "/";: Print Using "####"; K!(1, 8)

    Print "AVERAGE YARDS PER PUNT"; Tab(45);
    If K!(0, 7) <> 0 Then Print Using "##.#"; K!(0, 8) / K!(0, 7);
    If K!(1, 7) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 8) / K!(1, 7)
    If K!(1, 7) = 0 Then Print

    Print "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print Using "##.#"; K!(0, 7) / numberGames%;: Print Tab(65); Using "##.#"; K!(1, 7) / numberGames%
    Print

    Print "FUMBLES / FUMBLES LOST"; Tab(41);
    Print Using "###"; K!(0, 9);: Print "/";: Print Using "####"; K!(0, 10);
    Print Tab(61); Using "###"; K!(1, 9);: Print "/";: Print Using "####"; K!(1, 10)

    Print "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print Using "###"; K!(0, 11);: Print "/";: Print Using "####"; K!(0, 12);
    Print Tab(61); Using "###"; K!(1, 11);: Print "/";: Print Using "####"; K!(1, 12)

    Print "AVERAGE YARDS PER PENALTY"; Tab(45);
    If K!(0, 11) <> 0 Then Print Using "##.#"; K!(0, 12) / K!(0, 11);
    If K!(1, 11) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 12) / K!(1, 11)
    If K!(1, 11) = 0 Then Print

    Print "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print Using "##.#"; K!(0, 11) / numberGames%;: Print "/";: Print Using "##.#"; K!(0, 12) / numberGames%;
    Print Tab(60); Using "##.#"; K!(1, 11) / numberGames%;: Print "/";: Print Using "##.#"; K!(1, 12) / numberGames%

    Print "SAFETIES"; Tab(45);: Print Using "####"; Z!(21);: Print Tab(65); Using "####"; Z1!(21)

    Print "TIMES SACKED / YARDS LOST"; Tab(41);
    Print Using "###"; W6!(0, 0);: Print "/";: Print Using "####"; W6!(0, 1);
    Print Tab(61); Using "###"; W6!(1, 0);: Print "/";: Print Using "####"; W6!(1, 1)

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"
    I = 0

    If D% <> 1 Then
        For I1 = 0 To 9
            For I2 = 0 To 3
                If lookyA1$(I, I1) = lookyA3$(I, I2) Then
                    K2!(I, I1, 0) = K2!(I, I1, 0) + K2!(I, I2 + 10, 0)
                    K2!(I, I1, 1) = K2!(I, I1, 1) + K2!(I, I2 + 10, 1)
                    K2!(I, I1, 2) = K2!(I, I1, 2) + K2!(I, I2 + 10, 2)
                    K2!(I, I2 + 10, 0) = 0
                    K2!(I, I2 + 10, 1) = 0
                    K2!(I, I2 + 10, 2) = 0
                End If
            Next I2
        Next I1
    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13

        If K2!(I, I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            If I1 <= 9 Then Print lookyA1$(I, I1);
            If I1 > 9 Then Print lookyA3$(I, I1 - 10);
            Print Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 0); K2!(I, I1, 1); K2!(I, I1, 1) / K2!(I, I1, 0); K2!(I, I1, 1) / numberGames%; K2!(I, I1, 2);
            If I1 > 9 Then
                Print
            Else
                Print Using "###"; LR!(I1);: Print LR$(I1, 0); Tab(64); LR$(I1, 1)
            End If
        End If

    Next I1

    Color 14
    Print
    Print statsA$(0); Tab(18);

    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; K!(0, 1); K!(0, 2); K!(0, 2) / K!(0, 1); K!(0, 2) / numberGames%; Z!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; K!(0, 1) - W6!(0, 0); K!(0, 2) + W6!(0, 1); (K!(0, 2) + W6!(0, 1)) / (K!(0, 1) - W6!(0, 0)); (K!(0, 2) + W6!(0, 1)) / numberGames%; Z!(15)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; K!(1, 1); K!(1, 2); K!(1, 2) / K!(1, 1); K!(1, 2) / numberGames%; Z1!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; K!(1, 1) - W6!(1, 0); K!(1, 2) + W6!(1, 1); (K!(1, 2) + W6!(1, 1)) / (K!(1, 1) - W6!(1, 0)); (K!(1, 2) + W6!(1, 1)) / numberGames%; Z1!(15)
    End If

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5

        If K2!(I, I1, 6) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyA2$(I, I1); Tab(18);
            Print Using "####  ####   "; K2!(I, I1, 6); K2!(I, I1, 7);
            If K2!(I, I1, 6) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I, I1, 7) / K2!(I, I1, 6);
            Print Using "      ###.#    ##    "; K2!(I, I1, 7) / numberGames%; K2!(I, I1, 8);
            Print Using "###"; LC!(I1 + 10);: Print LC$(I1 + 10, 0); Tab(64); LC$(I1 + 10, 1)
        End If

    Next I1

    For I1 = 0 To 9

        If K2!(I, I1, 3) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyA1$(I, I1); Tab(18);
            Print Using "####  ####   "; K2!(I, I1, 3); K2!(I, I1, 4);
            If K2!(I, I1, 3) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I, I1, 4) / K2!(I, I1, 3);
            Print Using "      ###.#    ##    "; K2!(I, I1, 4) / numberGames%; K2!(I, I1, 5);
            Print Using "###"; LC!(I1);: Print LC$(I1, 0); Tab(64); LC$(I1, 1)
        End If

    Next I1

    Print
    Color 14
    Print statsA$(0); Tab(18);

    Print Using "####  ####   "; K!(0, 4); K!(0, 6);
    If K!(0, 4) = 0 Then Print "  --"; Else Print Using "##.#"; K!(0, 6) / K!(0, 4);
    Print Using "      ###.#    ##"; K!(0, 6) / numberGames%; Z!(17)

    Print "OPPONENT TOTALS"; Tab(18);
    Print Using "####  ####   "; K!(1, 4); K!(1, 6);
    If K!(1, 4) = 0 Then Print "  --"; Else Print Using "##.#"; K!(1, 6) / K!(1, 4);
    Print Using "      ###.#    ##"; K!(1, 6) / numberGames%; Z1!(17)

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3

        If K2!(I, I1, 9) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            'Att / Comp / Comp % / Yds
            Print lookyA3$(I, I1); Tab(18); Using "####  ### ###.# #### "; K2!(I, I1, 9); K2!(I, I1, 10); K2!(I, I1, 10) / K2!(I, I1, 9) * 100; K2!(I, I1, 12);

            'Yds / Att
            If K2!(I, I1, 9) = 0 Then Print "   --"; Else Print Using "##.#"; K2!(I, I1, 12) / K2!(I, I1, 9);

            ' Yds / Catch
            If K2!(I, I1, 10) = 0 Then Print "   --"; Else Print Using " ##.#"; K2!(I, I1, 12) / K2!(I, I1, 10);

            'TD / INT / INT %
            Print Using " ###.# ### ### ###.#     "; K2!(I, I1, 12) / numberGames%; K2!(I, I1, 13); K2!(I, I1, 11); K2!(I, I1, 11) / K2!(I, I1, 9) * 100;

            'Eff Rating
            RT! = QBRating!(I, I1, statFormat)
            Print Using "###.#"; RT!

            'PRINT TAB(57); USING "###"; LP!(I1); : PRINT statsLP$(I1,0)
        End If

    Next I1

    Print

    Color 14
    'Team Totals
    Print statsA$(0); Tab(18);

    'Att / Comp
    Print Using "####  ### "; K!(0, 3); K!(0, 4);

    'Comp %
    If K!(0, 3) = 0 Then Print "   --"; Else Print Using "###.#"; K!(0, 4) / K!(0, 3) * 100;

    'Yds
    Print Using " #### "; K!(0, 6);

    'Yds / Att
    If K!(0, 3) = 0 Then Print " --"; Else Print Using "##.#"; K!(0, 6) / K!(0, 3);

    'Yds / Catch
    If K!(0, 4) = 0 Then Print "   --"; Else Print Using " ##.#"; K!(0, 6) / K!(0, 4);

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; K!(0, 6) / numberGames%; Z!(17); K!(0, 5);

    'INT %
    If K!(0, 3) = 0 Then Print "   --" Else Print Using "  #.#"; K!(0, 5) / K!(0, 3) * 100

    Print "OPPONENT TOTALS"; Tab(18);

    'Att / Comp
    Print Using "####  ### "; K!(1, 3); K!(1, 4);

    'Comp %
    If K!(1, 3) = 0 Then Print "   --" Else Print Using "###.#"; K!(1, 4) / K!(1, 3) * 100;

    'Yds
    Print Using " #### "; K!(1, 6);

    'Yds / Att
    If K!(1, 3) = 0 Then Print " --"; Else Print Using "##.#"; K!(1, 6) / K!(1, 3);

    'Yds / Catch
    If K!(1, 4) = 0 Then Print "   --"; Else Print Using " ##.#"; K!(1, 6) / K!(1, 4);

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; K!(1, 6) / numberGames%; Z1!(17); K!(1, 5);

    'INT %
    If K!(1, 3) = 0 Then Print "   --" Else Print Using "  #.#"; K!(1, 5) / K!(1, 3) * 100

    Print
    Print
    Color 12

    Print "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If K2!(I, I1, 9) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyA3$(I, I1); Tab(22); Using "###"; LP!(I1);: Print statsLP$(I1, 0); Tab(32); statsLP$(I1, 1)
        End If
    Next

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If
    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 0 To 2
        If K2!(I, I1, 14) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyA4$(I, I1); Tab(18);
            Print Using "####  ####   "; K2!(I, I1, 14); K2!(I, I1, 15);
            If K2!(I, I1, 14) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I, I1, 15) / K2!(I, I1, 14);
            Print Using "      ###.#    ##    "; K2!(I, I1, 15) / numberGames%; K2!(I, I1, 16);
            Print Using "###"; LK!(I1);: Print statsLK$(I1, 0); Tab(64); statsLK$(I1, 1)
        End If
    Next I1

    Print
    Color 14
    Print statsA$(0); Tab(18);
    If Z!(22) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z!(22); Z!(23); Z!(23) / Z!(22); Z!(23) / numberGames%; Z!(18)

    Print "OPPONENT TOTALS"; Tab(18);
    If Z1!(22) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z1!(22); Z1!(23); Z1!(23) / Z1!(22); Z1!(23) / numberGames%; Z1!(18)
    Print
    Print
    Print
    Color 12
    Print "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 3 To 5
        If K2!(I, I1, 14) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyA5$(I, I1 - 3); Tab(18);
            Print Using "####  ####   "; K2!(I, I1, 14); K2!(I, I1, 15);
            If K2!(I, I1, 14) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I, I1, 15) / K2!(I, I1, 14);
            Print Using "      ###.#    ##    "; K2!(I, I1, 15) / numberGames%; K2!(I, I1, 16);
            Print Using "###"; PK!(I1 - 3);: Print statsPK$(I1 - 3, 0); Tab(64); statsPK$(I1 - 3, 1)
        End If
    Next I1

    Print
    Color 14
    Print statsA$(0); Tab(18);
    If Z!(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z!(24); Z!(25); Z!(25) / Z!(24); Z!(25) / numberGames%; Z!(19)

    Print "OPPONENT TOTALS"; Tab(18);
    If Z1!(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; Z1!(24); Z1!(25); Z1!(25) / Z1!(24); Z1!(25) / numberGames%; Z1!(19)

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If
    Call TITLE_STATS

    I = 0
    Locate 3, 1
    Color 12
    Print "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Color 15
    Print lookyA6$(I, 0); Tab(18); Using "####  ####    "; K!(0, 7); K!(0, 8);
    If K!(0, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(0, 8) / K!(0, 7);
    Print Using "    ##.#      ##     ##      ##"; K!(0, 7) / numberGames%; Z!(31); Z!(30); Z!(28)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; K!(1, 7); K!(1, 8);
    If K!(1, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(1, 8) / K!(1, 7);
    Print Using "    ##.#      ##     ##"; K!(1, 7) / numberGames%; Z1!(31); Z1!(30)

    Print
    Print
    Print
    Color 12
    Print "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Color 15
    Print statsA$(0); Tab(18); Using "####  ####    "; K!(0, 7); K!(0, 8);
    If K!(0, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(0, 8) / K!(0, 7);
    Print Using "    ###       ####        ##.#     ##.#"; Z1!(24); Z1!(25); Z1!(25) / Z1!(24); (K!(0, 8) - Z1!(25)) / K!(0, 7)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; K!(1, 7); K!(1, 8);
    If K!(1, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(1, 8) / K!(1, 7);
    Print Using "    ###       ####        ##.#     ##.#"; Z!(24); Z!(25); Z!(25) / Z!(24); (K!(1, 8) - Z!(25)) / K!(1, 7)

    Print
    Print
    Print
    Color 12
    Print "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
    Color 15

    If K2!(I, 4, 17) <> 0 And K2!(I, 6, 17) <> 0 Then
        Print lookyA7$(I, 0);
        Print Tab(16);
        If K2!(I, 6, 17) <> 0 Then Print Using "### ### ###.# "; K2!(I, 5, 17); K2!(I, 6, 17); K2!(I, 5, 17) / K2!(I, 6, 17) * 100;
        If K2!(I, 4, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; K2!(I, 3, 17); K2!(I, 4, 17); K2!(I, 3, 17) / K2!(I, 4, 17) * 100; LF!(0); AM!(0, 0);
            Print "-";: Print Using "## ##"; AF!(0, 0); AM!(0, 1);
            Print "-";: Print Using "## ##"; AF!(0, 1); AM!(0, 2);
            Print "-";: Print Using "## ##"; AF!(0, 2); AM!(0, 3);
            Print "-";: Print Using "## ##"; AF!(0, 3); AM!(0, 4);
            Print "-";: Print Using "##"; AF!(0, 4)
        End If
    End If

    If K2!(I, 8, 17) <> 0 And K2!(I, 10, 17) <> 0 Then
        Print lookyA7$(I, 1); Tab(16);
        If K2!(I, 10, 17) <> 0 Then Print Using "### ### ###.# "; K2!(I, 9, 17); K2!(I, 10, 17); K2!(I, 9, 17) / K2!(I, 10, 17) * 100;
        If K2!(I, 8, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; K2!(I, 7, 17); K2!(I, 8, 17); K2!(I, 7, 17) / K2!(I, 8, 17) * 100; LF!(1); AM!(1, 0);
            Print "-";: Print Using "## ##"; AF!(1, 0); AM!(1, 1);
            Print "-";: Print Using "## ##"; AF!(1, 1); AM!(1, 2);
            Print "-";: Print Using "## ##"; AF!(1, 2); AM!(1, 3);
            Print "-";: Print Using "## ##"; AF!(1, 3); AM!(1, 4);
            Print "-";: Print Using "##"; AF!(1, 4)
        End If
    End If

    Print

    NOONE:

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If
    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 0 To 9

        If GI!(I, I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print lookyDI$(I, I1); Tab(18);

            Print Using "####  ####   "; GI!(I, I1, 0); GI!(I, I1, 1);

            'Yds / Ret
            If GI!(I, I1, 0) = 0 Then Print " --"; Else Print Using "##.#"; GI!(I, I1, 1) / GI!(I, I1, 0);

            'Yds / Gm, TD
            Print Using "      ###.#    ##    "; GI!(I, I1, 1) / numberGames%; GI!(I, I1, 2);

            'Long Ret / Against
            Print Using "###"; LI!(I1);: Print statsLI$(I1, 0); Tab(64); statsLI$(I1, 1)

        End If

    Next I1

    If X = 0 Then
        I$ = GetKeyPress$
        Cls
    End If
    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If GS!(I, I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyDS$(I, I1); Tab(20); Using "###            ###"; GS!(I, I1, 0); GS!(I, I1, 1)
        End If
    Next

    I$ = GetKeyPress

End Sub

'----------------------------------------
'      StatsToPrinter Subroutine
'----------------------------------------
'...explanation...
Sub StatsToPrinter (X, numberGames%, statFormat, teamPower, SS)

    Shared AF!(), AM!(), CRD!(), GI!(), GS!()
    Shared K!(), K2!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared PK!(), W6!(), Z!(), Z1!(), Z2!()

    Shared statsO%(), statsT%(), lookyP%()

    Shared statsA$(), lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared lookyDI$(), lookyDS$(), statsH$()
    Shared LC$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared compO$(), statsPK$(), gameSITE$()

    Color 0, 15

    Open "REPORT" For Output As #2
    Print #2, "GAME-BY-GAME REPORT FOR "; statsA$(0)
    Print #2,
    Print #2, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"

    For I = 1 To numberGames%

        Print #2, compO$(I); Tab(19); Using "###"; statsO%(I);
        Print #2, "-";: Print #2, Using "###   "; statsT%(I);

        If statsO%(I) > statsT%(I) Then W% = W% + 1: Print #2, "W";
        If statsO%(I) < statsT%(I) Then L% = L% + 1: Print #2, "L";
        If statsO%(I) = statsT%(I) Then X% = X% + 1: Print #2, "T";

        Print #2, Tab(31);: Print #2, Using "####,###   "; CRD!(I);
        Print #2, statsH$(I); "  "; Left$(gameSITE$(I), 24);
        Print #2, Tab(69);
        Print #2, "(";: Print #2, Using "##"; W%;: Print #2, "-";: Print #2, Using "##"; L%;: Print #2, "-";: Print #2, Using "##"; X%;: Print #2, ")"
        lookyP%(0) = lookyP%(0) + statsO%(I)
        lookyP%(1) = lookyP%(1) + statsT%(I)
    Next I

    Print #2,
    If statFormat = 0 Then Print #2, Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS

    lookyP%(0) = 0
    lookyP%(1) = 0
    W% = 0
    L% = 0
    X% = 0

    Print #2,
    Print #2,
    Print #2, "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
    Print #2, statsA$(0); Tab(30); Using " ###  ###  ###  ###   ###"; Z!(32); Z!(33); Z!(34); Z!(35); Z!(32) + Z!(33) + Z!(34) + Z!(35) + Z!(36)
    Print #2, "OPPONENTS"; Tab(30); Using " ###  ###  ###  ###   ###"; Z1!(32); Z1!(33); Z1!(34); Z1!(35); Z1!(32) + Z1!(33) + Z1!(34) + Z1!(35) + Z!(36)
    Print #2,

    '-- Would numberGames% ever be 0?
    Print #2, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Print #2, statsA$(0); Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z!(32) / numberGames%; Z!(33) / numberGames%; Z!(34) / numberGames%; Z!(35) / numberGames%; (Z!(32) + Z!(33) + Z!(34) + Z!(35) + Z!(36)) / numberGames%
    Print #2, "OPPONENTS"; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; Z1!(32) / numberGames%; Z1!(33) / numberGames%; Z1!(34) / numberGames%; Z1!(35) / numberGames%; (Z1!(32) + Z1!(33) + Z1!(34) + Z1!(35) + Z!(36)) / numberGames%
    Print #2,

    HD = 0: AD = 0: ND = 0: TD = 0
    HT = 0: CT = 0: NT = 0: TT = 0
    HA = 0: AA = 0: NA = 0: TA = 0

    For I = 1 To numberGames%
        If Left$(statsH$(I), 1) = "H" Then HD = HD + 1: HT = HT + CRD!(I)
        If Left$(statsH$(I), 1) = "A" Then AD = AD + 1: CT = CT + CRD!(I)
        If Left$(statsH$(I), 1) = "X" Then ND = ND + 1: NT = NT + CRD!(I)
        TD = TD + 1: TT = TT + CRD!(I)
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

    Print #2, "TEAM STATISTICS CATEGORY"; Tab(41); statsA$(0); Tab(61); "OPPONENT TOTALS"

    Print #2, "FIRST DOWNS"; Tab(45); Using "####"; K!(0, 0);
    Print #2, Tab(65); Using "####"; K!(1, 0)

    Print #2, "RUSHING ATTEMPTS"; Tab(45);
    If statFormat = 0 Then Print #2, Using "####"; K!(0, 1); Else Print #2, Using "####"; K!(0, 1) - W6!(0, 0);
    Print #2, Tab(65);
    If statFormat = 0 Then Print #2, Using "####"; K!(1, 1) Else Print #2, Using "####"; K!(1, 1) - W6!(1, 0)

    Print #2, "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then Print #2, Using "####"; K!(0, 2); Else Print #2, Using "####"; K!(0, 2) + W6!(0, 1);
    Print #2, Tab(65);
    If statFormat = 0 Then Print #2, Using "####"; K!(1, 2) Else Print #2, Using "####"; K!(1, 2) + W6!(1, 1)

    Print #2, "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And K!(0, 1) <> 0 Then Print #2, Using "##.#"; K!(0, 2) / K!(0, 1);
    If statFormat <> 0 And K!(0, 1) - W6!(0, 0) <> 0 Then Print #2, Using "##.#"; (K!(0, 2) + W6!(0, 1)) / (K!(0, 1) - W6!(0, 0));

    Print #2, Tab(65);

    If statFormat = 0 And K!(1, 1) <> 0 Then Print #2, Using "##.#"; K!(1, 2) / K!(1, 1)
    If statFormat <> 0 And K!(1, 1) - W6!(1, 0) <> 0 Then Print #2, Using "##.#"; (K!(1, 2) + W6!(1, 1)) / (K!(1, 1) - W6!(1, 0))
    If statFormat = 0 And K!(1, 1) = 0 Or statFormat <> 0 And K!(1, 1) - W6!(1, 0) = 0 Then Print #2,

    Print #2, "RUSHING YARDS PER GAME"; Tab(44);

    If statFormat = 0 Then Print #2, Using "###.#"; K!(0, 2) / numberGames%; Else Print #2, Using "###.#"; (K!(0, 2) + W6!(0, 1)) / numberGames%;
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "###.#"; K!(1, 2) / numberGames% Else Print #2, Using "###.#"; (K!(1, 2) + W6!(1, 1)) / numberGames%

    Print #2,
    Print #2, "PASSES ATTEMPTED"; Tab(45); Using "####"; K!(0, 3);: Print #2, Tab(65); Using "####"; K!(1, 3)
    Print #2, "PASSES COMPLETED"; Tab(45); Using "####"; K!(0, 4);: Print #2, Tab(65); Using "####"; K!(1, 4)
    Print #2, "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; K!(0, 5);: Print #2, Tab(65); Using "####"; K!(1, 5)
    Print #2, "PASS COMPLETION PERCENTAGE"; Tab(44);

    If K!(0, 3) <> 0 Then Print #2, Using "###.#"; K!(0, 4) / K!(0, 3) * 100;
    If K!(1, 3) <> 0 Then
        Print #2, Tab(64); Using "###.#"; K!(1, 4) / K!(1, 3) * 100
    Else
        Print #2,
    End If

    Print #2, "NET YARDS PASSING"; Tab(45); Using "####"; K!(0, 6);
    Print #2, Tab(65); Using "####"; K!(1, 6)

    Print #2, "YARDS PER PASS ATTEMPT"; Tab(45);
    If K!(0, 3) <> 0 Then Print #2, Using "##.#"; K!(0, 6) / K!(0, 3);
    If K!(1, 3) <> 0 Then
        Print #2, Tab(65); Using "##.#"; K!(1, 6) / K!(1, 3)
    Else
        Print #2,
    End If

    Print #2, "YARDS PER PASS COMPLETION"; Tab(45);
    If K!(0, 4) <> 0 Then Print #2, Using "##.#"; K!(0, 6) / K!(0, 4);
    If K!(1, 4) <> 0 Then
        Print #2, Tab(65); Using "##.#"; K!(1, 6) / K!(1, 4)
    Else
        Print #2,
    End If

    Print #2, "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; K!(0, 6) / numberGames%;
    Print #2, Tab(64); Using "###.#"; K!(1, 6) / numberGames%
    Print #2,

    Print #2, "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then Print #2, Using "####"; K!(0, 1) + K!(0, 3); Else Print #2, Using "####"; K!(0, 1) - W6!(0, 0) + K!(0, 3);
    Print #2, Tab(65);
    If statFormat = 0 Then Print #2, Using "####"; K!(1, 1) + K!(1, 3) Else Print #2, Using "####"; K!(1, 1) - W6!(1, 0) + K!(1, 3)

    Print #2, "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(0, 1) + K!(0, 3)) / numberGames%; Else Print #2, Using "###.#"; (K!(0, 1) - W6!(0, 0) + K!(0, 3)) / numberGames%;
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(1, 1) + K!(1, 3)) / numberGames% Else Print #2, Using "###.#"; (K!(1, 1) - W6!(1, 0) + K!(1, 3)) / numberGames%

    Print #2, "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then Print #2, Using "####"; K!(0, 2) + K!(0, 6); Else Print #2, Using "####"; K!(0, 2) + W6!(0, 1) + K!(0, 6);
    Print #2, Tab(65);
    If statFormat = 0 Then Print #2, Using "####"; K!(1, 2) + K!(1, 6) Else Print #2, Using "####"; K!(1, 2) + W6!(1, 1) + K!(1, 6)

    Print #2, "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then Print #2, Using "##.#"; (K!(0, 2) + K!(0, 6)) / (K!(0, 1) + K!(0, 3)); Else Print #2, Using "##.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / (K!(0, 1) - W6!(0, 0) + K!(0, 3));
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "##.#"; (K!(1, 2) + K!(1, 6)) / (K!(1, 1) + K!(1, 3)) Else Print #2, Using "##.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / (K!(1, 1) - W6!(1, 0) + K!(1, 3))

    Print #2, "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(0, 2) + K!(0, 6)) / numberGames%; Else Print #2, Using "###.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / numberGames%;
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(1, 2) + K!(1, 6)) / numberGames% Else Print #2, Using "###.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / numberGames%

    Print #2,
    Print #2, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print #2, Using "###"; Z!(22);: Print #2, "/";: Print #2, Using "####"; Z!(23);
    Print #2, Tab(61); Using "###"; Z1!(22);: Print #2, "/";: Print #2, Using "####"; Z1!(23)

    Print #2, "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If Z!(22) <> 0 Then Print #2, Using "##.#"; Z!(23) / Z!(22);
    If Z1!(22) <> 0 Then
        Print #2, Tab(65); Using "##.#"; Z1!(23) / Z1!(22)
    Else
        Print #2,
    End If

    Print #2, "KICKOFF RETURNS PER GAME"; Tab(45);
    Print #2, Using "##.#"; Z!(22) / numberGames%;
    Print #2, Tab(65); Using "##.#"; Z1!(22) / numberGames%

    Print #2, "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print #2, Using "###"; Z!(24);: Print #2, "/";: Print #2, Using "####"; Z!(25);
    Print #2, Tab(61); Using "###"; Z1!(24);: Print #2, "/";: Print #2, Using "####"; Z1!(25)

    Print #2, "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If Z!(24) <> 0 Then Print #2, Using "##.#"; Z!(25) / Z!(24);
    If Z1!(24) <> 0 Then
        Print #2, Tab(65); Using "##.#"; Z1!(25) / Z1!(24)
    Else
        Print #2,
    End If

    Print #2, "PUNT RETURNS PER GAME"; Tab(45);
    Print #2, Using "##.#"; Z!(24) / numberGames%;
    Print #2, Tab(65); Using "##.#"; Z1!(24) / numberGames%

    Print #2, "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print #2, Using "###"; Z!(26);: Print #2, "/";: Print #2, Using "####"; Z!(27);
    Print #2, Tab(61); Using "###"; Z1!(26);: Print #2, "/";: Print #2, Using "####"; Z1!(27)

    Print #2, "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If Z!(26) <> 0 Then Print #2, Using "##.#"; Z!(27) / Z!(26);
    If Z1!(26) <> 0 Then
        Print #2, Tab(65); Using "##.#"; Z1!(27) / Z1!(26)
    Else
        Print #2,
    End If

    Print #2, "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print #2, Using "##.#"; Z!(26) / numberGames%;
    Print #2, Tab(65); Using "##.#"; Z1!(26) / numberGames%

    Print #2,

    Print #2, "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print #2, Using "###"; K!(0, 7);: Print #2, "/";: Print #2, Using "####"; K!(0, 8);
    Print #2, Tab(61); Using "###"; K!(1, 7);: Print #2, "/";: Print #2, Using "####"; K!(1, 8)

    Print #2, "AVERAGE YARDS PER PUNT"; Tab(45);
    If K!(0, 7) <> 0 Then Print #2, Using "##.#"; K!(0, 8) / K!(0, 7);
    If K!(1, 7) <> 0 Then
        Print #2, Tab(65); Using "##.#"; K!(1, 8) / K!(1, 7)
    Else
        Print #2,
    End If

    Print #2, "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);
    Print #2, Using "##.#"; K!(0, 7) / numberGames%;
    Print #2, Tab(65); Using "##.#"; K!(1, 7) / numberGames%
    Print #2,

    Print #2, "FUMBLES / FUMBLES LOST"; Tab(41);
    Print #2, Using "###"; K!(0, 9);: Print #2, "/";: Print #2, Using "####"; K!(0, 10);
    Print #2, Tab(61); Using "###"; K!(1, 9);: Print #2, "/";: Print #2, Using "####"; K!(1, 10)

    Print #2, "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print #2, Using "###"; K!(0, 11);: Print #2, "/";: Print #2, Using "####"; K!(0, 12);
    Print #2, Tab(61); Using "###"; K!(1, 11);: Print #2, "/";: Print #2, Using "####"; K!(1, 12)

    Print #2, "AVERAGE YARDS PER PENALTY"; Tab(45);
    If K!(0, 11) <> 0 Then Print #2, Using "##.#"; K!(0, 12) / K!(0, 11);
    If K!(1, 11) <> 0 Then
        Print #2, Tab(65); Using "##.#"; K!(1, 12) / K!(1, 11)
    Else
        Print #2,
    End If

    Print #2, "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print #2, Using "##.#"; K!(0, 11) / numberGames%;: Print #2, "/";: Print #2, Using "##.#"; K!(0, 12) / numberGames%;
    Print #2, Tab(60); Using "##.#"; K!(1, 11) / numberGames%;: Print #2, "/";: Print #2, Using "##.#"; K!(1, 12) / numberGames%

    Print #2, "SAFETIES"; Tab(45);
    Print #2, Using "####"; Z!(21);
    Print #2, Tab(65); Using "####"; Z1!(21)

    Print #2, "TIMES SACKED / YARDS LOST"; Tab(41);
    Print #2, Using "###"; W6!(0, 0);: Print #2, "/";: Print #2, Using "####"; W6!(0, 1);
    Print #2, Tab(61); Using "###"; W6!(1, 0);: Print #2, "/";: Print #2, Using "####"; W6!(1, 1)
    Print #2, Chr$(12)

    Print #2, "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"

    I = 0

    If D% <> 1 Then
        For I1 = 0 To 9
            For I2 = 0 To 3
                If lookyA1$(I, I1) = lookyA3$(I, I2) Then
                    K2!(I, I1, 0) = K2!(I, I1, 0) + K2!(I, I2 + 10, 0)
                    K2!(I, I1, 1) = K2!(I, I1, 1) + K2!(I, I2 + 10, 1)
                    K2!(I, I1, 2) = K2!(I, I1, 2) + K2!(I, I2 + 10, 2)
                    K2!(I, I2 + 10, 0) = 0
                    K2!(I, I2 + 10, 1) = 0
                    K2!(I, I2 + 10, 2) = 0
                End If
            Next I2
        Next I1
    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13
        If K2!(I, I1, 0) <> 0 Then
            If I1 <= 9 Then Print #2, lookyA1$(I, I1);
            If I1 > 9 Then Print #2, lookyA3$(I, I1 - 10);
            Print #2, Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 0); K2!(I, I1, 1); K2!(I, I1, 1) / K2!(I, I1, 0); K2!(I, I1, 1) / numberGames%; K2!(I, I1, 2);
            If I1 > 9 Then
                Print #2,
            Else
                Print #2, Using "###"; LR!(I1);
                Print #2, LR$(I1, 0); Tab(64); LR$(I1, 1)
            End If
        End If
    Next I1

    Print #2, statsA$(0); Tab(18);
    If statFormat = 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(0, 1); K!(0, 2); K!(0, 2) / K!(0, 1); K!(0, 2) / numberGames%; Z!(15)
    If statFormat <> 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(0, 1) - W6!(0, 0); K!(0, 2) + W6!(0, 1); (K!(0, 2) + W6!(0, 1)) / (K!(0, 1) - W6!(0, 0)); (K!(0, 2) + W6!(0, 1)) / numberGames%; Z!(15)

    Print #2, "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(1, 1); K!(1, 2); K!(1, 2) / K!(1, 1); K!(1, 2) / numberGames%; Z1!(15)
    If statFormat <> 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(1, 1) - W6!(1, 0); K!(1, 2) + W6!(1, 1); (K!(1, 2) + W6!(1, 1)) / (K!(1, 1) - W6!(1, 0)); (K!(1, 2) + W6!(1, 1)) / numberGames%; Z1!(15)

    Print #2,
    Print #2, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5
        If K2!(I, I1, 6) <> 0 Then
            Print #2, lookyA2$(I, I1); Tab(18);
            Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 6); K2!(I, I1, 7); K2!(I, I1, 7) / K2!(I, I1, 6); K2!(I, I1, 7) / numberGames%; K2!(I, I1, 8);
            Print #2, Using "###"; LC!(I1 + 10);
            Print #2, LC$(I1 + 10, 0); Tab(64); LC$(I1 + 10, 1)
        End If
    Next I1

    For I1 = 0 To 9
        If K2!(I, I1, 3) <> 0 Then
            Print #2, lookyA1$(I, I1); Tab(18);
            Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 3); K2!(I, I1, 4); K2!(I, I1, 4) / K2!(I, I1, 3); K2!(I, I1, 4) / numberGames%; K2!(I, I1, 5);
            Print #2, Using "###"; LC!(I1);
            Print #2, LC$(I1, 0); Tab(64); LC$(I1, 1)
        End If
    Next I1

    Print #2, statsA$(0); Tab(18);
    Print #2, Using "####  ####   "; K!(0, 4); K!(0, 6);

    If K!(0, 4) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(0, 6) / K!(0, 4);

    Print #2, Using "      ###.#    ##"; K!(0, 6) / numberGames%; Z!(17)
    Print #2, "OPPONENT TOTALS"; Tab(18);
    Print #2, Using "####  ####   "; K!(1, 4); K!(1, 6);

    If K!(1, 4) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(1, 6) / K!(1, 4);

    Print #2, Using "      ###.#    ##"; K!(1, 6) / numberGames%; Z1!(17)

    Print #2,
    Print #2, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3
        If K2!(I, I1, 9) <> 0 Then
            Print #2, lookyA3$(I, I1); Tab(18); Using "####  ### ###.# #### ##.#"; K2!(I, I1, 9); K2!(I, I1, 10); K2!(I, I1, 10) / K2!(I, I1, 9) * 100; K2!(I, I1, 12); K2!(I, I1, 12) / K2!(I, I1, 9);
            If K2!(I, I1, 10) = 0 Then Print #2, "   --"; Else Print #2, Using " ##.#"; K2!(I, I1, 12) / K2!(I, I1, 10);
            Print #2, Using " ###.# ### ### ###.#     "; K2!(I, I1, 12) / numberGames%; K2!(I, I1, 13); K2!(I, I1, 11); K2!(I, I1, 11) / K2!(I, I1, 9) * 100;
            RT! = QBRating!(I, I1, statFormat)
            Print #2, Using "###.#"; RT!
        End If
    Next I1

    Print #2, statsA$(0); Tab(18);
    Print #2, Using "####  ### "; K!(0, 3); K!(0, 4);

    If K!(0, 3) = 0 Then Print #2, "   --"; Else Print #2, Using "###.#"; K!(0, 4) / K!(0, 3) * 100;

    Print #2, Using "#### "; K!(0, 6);

    If K!(0, 3) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(0, 6) / K!(0, 3);
    If K!(0, 4) = 0 Then Print #2, "   --"; Else Print #2, Using " ##.#"; K!(0, 6) / K!(0, 4);

    Print #2, Using " ###.# ### ### "; K!(0, 6) / numberGames%; Z!(17); K!(0, 5);

    If K!(0, 3) = 0 Then Print #2, "   --"; Else Print #2, Using "###.#     "; K!(0, 5) / K!(0, 3) * 100

    Print #2, "OPPONENT TOTALS"; Tab(18);
    Print #2, Using "####  ### "; K!(1, 3); K!(1, 4);

    If K!(1, 3) = 0 Then Print #2, "   --"; Else Print #2, Using "###.#"; K!(1, 4) / K!(1, 3) * 100;

    Print #2, Using "#### "; K!(1, 6);

    If K!(1, 3) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(1, 6) / K!(1, 3);
    If K!(1, 4) = 0 Then Print #2, "  --"; Else Print #2, Using " ##.#"; K!(1, 6) / K!(1, 4);

    Print #2, Using " ###.# ### ### "; K!(1, 6) / numberGames%; Z1!(17); K!(1, 5);

    If K!(1, 3) = 0 Then Print #2, "   --" Else Print #2, Using "###.#     "; K!(1, 5) / K!(1, 3) * 100

    Print #2,
    Print #2,
    Print #2, "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If K2!(I, I1, 9) <> 0 Then
            Print #2, lookyA3$(I, I1); Tab(22); Using "###"; LP!(I1);
            Print #2, statsLP$(I1, 0); Tab(32); statsLP$(I1, 1)
        End If
    Next

    Print #2, Chr$(12)
    Print #2, "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If K2!(I, I1, 14) <> 0 Then
            Print #2, lookyA4$(I, I1); Tab(18);
            Print #2, Using "####  ####   "; K2!(I, I1, 14); K2!(I, I1, 15);
            If K2!(I, I1, 14) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K2!(I, I1, 15) / K2!(I, I1, 14);
            Print #2, Using "      ###.#    ##    "; K2!(I, I1, 15) / numberGames%; K2!(I, I1, 16);
            Print #2, Using "###"; LK!(I1);: Print #2, statsLK$(I1, 0); Tab(64); statsLK$(I1, 1)
        End If
    Next I1

    Print #2, statsA$(0); Tab(18);

    If Z!(22) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z!(22); Z!(23); Z!(23) / Z!(22); Z!(23) / numberGames%; Z!(18)

    Print #2, "OPPONENT TOTALS"; Tab(18);

    If Z1!(22) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z1!(22); Z1!(23); Z1!(23) / Z1!(22); Z1!(23) / numberGames%; Z1!(18)

    Print #2,
    Print #2,
    Print #2,
    Print #2, "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 3 To 5
        If K2!(I, I1, 14) <> 0 Then
            Print #2, lookyA5$(I, I1 - 3); Tab(18);
            Print #2, Using "####  ####   "; K2!(I, I1, 14); K2!(I, I1, 15);
            If K2!(I, I1, 14) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K2!(I, I1, 15) / K2!(I, I1, 14);
            Print #2, Using "      ###.#    ##    "; K2!(I, I1, 15) / numberGames%; K2!(I, I1, 16);
            Print #2, Using "###"; PK!(I1 - 3);: Print #2, statsPK$(I1 - 3, 0); Tab(64); statsPK$(I1 - 3, 1)
        End If
    Next I1

    Print #2, statsA$(0); Tab(18);

    If Z!(24) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z!(24); Z!(25); Z!(25) / Z!(24); Z!(25) / numberGames%; Z!(19)

    Print #2, "OPPONENT TOTALS"; Tab(18);

    If Z1!(24) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; Z1!(24); Z1!(25); Z1!(25) / Z1!(24); Z1!(25) / numberGames%; Z1!(19)

    Print #2,

    I = 0

    Print #2, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Print #2, lookyA6$(I, 0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; K!(0, 7); K!(0, 8); K!(0, 8) / K!(0, 7); K!(0, 7) / numberGames%; Z!(31); Z!(30); Z!(28)
    Print #2, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; K!(1, 7); K!(1, 8); K!(1, 8) / K!(1, 7); K!(1, 7) / numberGames%; Z1!(31); Z1!(30)
    Print #2,
    Print #2,
    Print #2,
    Print #2, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Print #2, statsA$(0); Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K!(0, 7); K!(0, 8); K!(0, 8) / K!(0, 7); Z1!(24); Z1!(25); Z1!(25) / Z1!(24); (K!(0, 8) - Z1!(25)) / K!(0, 7)
    Print #2, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K!(1, 7); K!(1, 8); K!(1, 8) / K!(1, 7); Z!(24); Z!(25); Z!(25) / Z!(24); (K!(1, 8) - Z!(25)) / K!(1, 7)
    Print #2,
    Print #2,
    Print #2,
    Print #2, "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

    If K2!(I, 4, 17) <> 0 Or K2!(I, 6, 17) <> 0 Then
        Print #2, lookyA7$(I, 0);
        Print #2, Tab(16);
        If K2!(I, 6, 17) <> 0 Then Print #2, Using "### ### ###.# "; K2!(I, 5, 17); K2!(I, 6, 17); K2!(I, 5, 17) / K2!(I, 6, 17) * 100;
        If K2!(I, 4, 17) = 0 Then
            Print #2,
        Else
            Print #2, Tab(30);
            Print #2, Using "### ### ###.# ## ##"; K2!(I, 3, 17); K2!(I, 4, 17); K2!(I, 3, 17) / K2!(I, 4, 17) * 100; LF!(0); AM!(0, 0);
            Print #2, "-";: Print #2, Using "## ##"; AF!(0, 0); AM!(0, 1);
            Print #2, "-";: Print #2, Using "## ##"; AF!(0, 1); AM!(0, 2);
            Print #2, "-";: Print #2, Using "## ##"; AF!(0, 2); AM!(0, 3);
            Print #2, "-";: Print #2, Using "## ##"; AF!(0, 3); AM!(0, 4);
            Print #2, "-";: Print #2, Using "##"; AF!(0, 4)
        End If
    End If

    If K2!(I, 8, 17) <> 0 Or K2!(I, 10, 17) <> 0 Then
        Print #2, lookyA7$(I, 1); Tab(16);
        If K2!(I, 10, 17) <> 0 Then Print #2, Using "### ### ###.# "; K2!(I, 9, 17); K2!(I, 10, 17); K2!(I, 9, 17) / K2!(I, 10, 17) * 100;
        If K2!(I, 8, 17) = 0 Then
            Print #2,
        Else
            Print #2, Tab(30);
            Print #2, Using "### ### ###.# ## ##"; K2!(I, 7, 17); K2!(I, 8, 17); K2!(I, 7, 17) / K2!(I, 8, 17) * 100; LF!(1); AM!(1, 0);
            Print #2, "-";: Print #2, Using "## ##"; AF!(1, 0); AM!(1, 1);
            Print #2, "-";: Print #2, Using "## ##"; AF!(1, 1); AM!(1, 2);
            Print #2, "-";: Print #2, Using "## ##"; AF!(1, 2); AM!(1, 3);
            Print #2, "-";: Print #2, Using "## ##"; AF!(1, 3); AM!(1, 4);
            Print #2, "-";: Print #2, Using "##"; AF!(1, 4)
        End If
    End If

    Print #2,
    Print #2,
    Print #2, "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 0 To 9
        If GI!(I, I1, 0) <> 0 Then
            Print #2, lookyDI$(I, I1); Tab(18);
            Print #2, Using "####  ####   ##.#      ###.#    ##    "; GI!(I, I1, 0); GI!(I, I1, 1); GI!(I, I1, 1) / GI!(I, I1, 0); GI!(I, I1, 1) / numberGames%; GI!(I, I1, 2);
            Print #2, Using "###"; LI!(I1);: Print #2, statsLI$(I1, 0); Tab(64); statsLI$(I1, 1)
        End If
    Next I1

    Print #2,
    Print #2, "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If GS!(I, I1, 0) <> 0 Then
            Print #2, lookyDS$(I, I1); Tab(20); Using "###            ###"; GS!(I, I1, 0); GS!(I, I1, 1)
        End If
    Next

    Close 2

    Open "REPORT" For Input As #2

    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    Close 2

    LPrint Chr$(12)

    I$ = GetKeyPress$

End Sub

'----------------------------------------
'       QBRating Function
'----------------------------------------
'...explanation...
Function QBRating! (I, I1, teamType)

    Dim II!(3), PP!(3), QB!(3, 10)
    Dim TT!(3), YY!(3)

    'This is the important one!
    Shared K2!()

    Select Case teamType

        Case 0:
            ' *** COLLEGE QB RATING ***
            QB!(I1, 7) = Int(K2!(I, I1, 12) / K2!(I, I1, 9)) * 8.399999 + K2!(I, I1, 10) / K2!(I, I1, 9) * 100 + Int(K2!(I, I1, 13) / K2!(I, I1, 9) * 1000) / 10 * 3.3 - Int(K2!(I, I1, 11) / K2!(I, I1, 9) * 1000) / 10 * 2
            QB!(I1, 7) = Int(QB!(I1, 7) * 10) / 10

        Case 1: '
            ' *** NFL QB RATING ***
            QB!(I1, 8) = Int(K2!(I, I1, 13) / K2!(I, I1, 9) * 1000) / 10
            QB!(I1, 6) = K2!(I, I1, 10) / K2!(I, I1, 9) * 100
            QB!(I1, 10) = Int(K2!(I, I1, 12) / K2!(I, I1, 9) * 100) / 100
            QB!(I1, 9) = Int(K2!(I, I1, 11) / K2!(I, I1, 9) * 1000) / 10

            PP!(I) = QB!(I1, 6) - 30

            If PP!(I) < 0 Then
                PP!(I) = 0
            Else
                If PP!(I) > 77.5 Then
                    PP!(I) = 2.375
                Else
                    PP!(I) = PP!(I) * .05
                End If
            End If

            If QB!(I1, 8) > 11.9 Then
                TT!(I) = 2.375
            Else
                TT!(I) = QB!(I1, 8) * .2
            End If

            If QB!(I1, 9) = 0 Then
                II!(I) = 2.375
            Else
                If QB!(I1, 9) > 9.5 Then
                    II!(I) = 0
                Else
                    II!(I) = 2.375 - (.25 * QB!(I1, 9))
                End If
            End If

            QB!(I1, 10) = QB!(I1, 10) - 3

            If QB!(I1, 10) < 0 Then
                YY!(I) = 0
            Else
                If QB!(I1, 10) > 12.5 Then
                    YY!(I) = 2.375
                Else
                    D = Int(QB!(I1, 10) * 100) / 2
                    If D = QB!(I1, 10) / 2 Then
                        YY!(I) = QB!(I1, 10) / 2 * .3 + QB!(I1, 10) / 2 * .2
                    Else
                        YY!(I) = QB!(I1, 10) / 2 * .3 + QB!(I1, 10) / 2 * .2 + .003
                    End If
                End If
            End If

            QB!(I1, 7) = PP!(I) + TT!(I) + II!(I) + YY!(I)

            If QB!(I1, 7) >= 9.5 Then
                QB!(I1, 7) = 158.3
            Else
                QB!(I1, 7) = QB!(I1, 7) * 50 / 3
            End If

            QB!(I1, 7) = Int(QB!(I1, 7) * 10) / 10

    End Select

    QBRating! = QB!(I1, 7)

End Function


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
' Prompts user with choice to reset stats
' If they chose so, and confirm, then
' it will proceed to delete the files.
Sub ResetStatPrompt (statFile$)

    Cls

    JJ = 1
    Locate 6, 8

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

    Locate 7

    Do
        U$ = "RESET COMPILED STATS TO 0 (YN)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        'Do
        JJ = 1
        Locate 6, 8

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

        Locate 7

        Do
            U$ = "ARE YOU SURE (YN)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        If UCase$(I$) = "Y" Then
            Kill diskPaths$(1) + statFile$
            Kill diskPaths$(1) + statFile$ + "S"
            If _FileExists(diskPaths$(1) + statFile$ + "R") Then Kill diskPaths$(1) + statFile$ + "R"
        End If

        'Loop Until UCase$(I$) = "N"

    End If 'Done checking if reset stats

End Sub






'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECOLL.BAS'
'$INCLUDE: 'SEENATL.BAS'
'$INCLUDE: 'SEEPRO.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'

' For Game
'$INCLUDE: 'FOOTBALL.BAS'
'$INCLUDE: 'GameRoutines.bm'

