$Debug

Const GAME_VERSION$ = "6.40"
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
'$INCLUDE: 'TITLE.BI'
'$INCLUDE: 'Variables.BI'

DefInt A-Z


'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "3-in-1 Football - Menu"
$ExeIcon:'./lhg_3in1fb.ico'
_Icon
_AllowFullScreen

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
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    Dim A$(1 To 15)

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
                Case 1: Run "FOOTBALL.EXE"
                Case 2: Call Team_Menu
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

    Color 15, 1
    FG = 0
    BG = 7

    For I = 0 To 2
        diskPaths$(I) = Left$(diskPaths$(I), Len(diskPaths$(I)) - 1)
    Next

    'Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
    Cls

    Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS"
    Print

    Locate , 31: Print "PATHNAME EXAMPLES:": Print
    Locate , 31: Print "A:"
    Locate , 31: Print "C:\FULL\TEAMS"
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

    Open "DEV.BAS" For Output As #1
    For R = 0 To 2: diskPaths$(R) = diskPaths$(R) + "\": Next R
    Print #1, diskPaths$(0)
    Print #1, diskPaths$(1)
    Print #1, diskPaths$(2)
    Close #1

End Sub


'----------------------------------------
'       Sub Team_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Team_Menu ()

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
                    Call GetTeam_CNGTM(YN$)
                    Call ChangeTeam(YN$)
                Case 3:
                    Call GetTeam_CNGTM(YN$)
                    Call PrintRoster
                Case 4:
                    Call GetTeam_CNGTM(YN$)
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
'       Sub Replay_Menu Subroutine
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
'       Sub Leaders_Menu Subroutine
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


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'MainRoutines.BM'
'$INCLUDE: 'PadRight.BM'
'$INCLUDE: 'TITLE.BM'
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
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECOLL.BAS'
'$INCLUDE: 'SEENATL.BAS'
'$INCLUDE: 'SEEPRO.BAS'
'$INCLUDE: 'TINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'

