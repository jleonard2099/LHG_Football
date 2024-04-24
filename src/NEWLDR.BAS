'----------------------------------------
'           NEWLDR Subroutine
'----------------------------------------
'This subroutine provides the user a menu
'for viewing stats compiled for the new
'"Expanded" Stats/Standings screens
Sub NEWLDR (statType)

    _Title "3-in-1 Football - View Stat Leaders"

    ReDim menuChoices$(1 To 3)

    If statType = 1 Then
        menuChoices$(1) = "UPDATE EXPANDED PRO LEAGUE LEADERS"
        menuChoices$(2) = "VIEW EXPANDED PRO LEAGUE LEADERS  "
        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU      "
    Else
        menuChoices$(1) = "UPDATE EXPANDED COLLEGE CONFERENCE LEADERS"
        menuChoices$(2) = "VIEW EXPANDED COLLEGE CONFERENCE LEADERS  "
        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU              "
    End If

    Do

        Cls
        Color 15, 0

        Call TITLE

        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1
                    Do
                        confStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call ReadConferenceStats(confStatFile$, statType)
                            Call UpdateExpandedLeaders(statType, totalStats)
                            Call SAVELEADER(confStatFile$, totalStats)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

                Case 2
                    Do
                        leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDS", "LEAGUE LEADERS", FileExit%)

                        If FileExit% <> 27 And FileExit% <> 0 Then
                            Call READLEADER(leaderFile$, totalStats)
                            Call ExpandedLeaders_Menu(totalStats)
                        End If

                    Loop Until FileExit% = 27 Or FileExit% = 0

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'     ReadConferenceStats Subroutine
'----------------------------------------
'This subroutine reads in the league stats
'from the .TMS file and assigns them to
'variables for use later in the routine.

'statType
'   0 = College
'   1 = Pro
Sub ReadConferenceStats (statFile$, statType)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared TMM$(), TPP$(), statsZ1$()

    Erase statsZ1$

    For X = 1 To 30
        statsZ1$(X) = "XXX"
    Next

    JA = 0
    JB = 0
    JC = 0
    JD = 0
    JE = 0
    JF = 0
    JG = 0
    JH = 0
    JI = 0
    JJ = 0
    totalStats = 0

    div1$ = ""
    div2$ = ""
    div3$ = ""

    Erase TMM$, TPP$

    Open diskPaths$(2) + statFile$ + ".TMS" For Input As #1

    If statType = 1 Then
        'Pro
        Input #1, league$
        Input #1, div1$
        Input #1, div2$
        Input #1, div3$
    Else
        'College
        Input #1, league$
        Input #1, div1$
        Input #1, div2$
    End If

    For X = 1 To 30:
        Input #1, statsZ1$(X)
    Next

    Close #1

End Sub


'----------------------------------------
'      ViewExpandedStats Subroutine
'----------------------------------------
'This subroutine reads in the league stats
'from the .TMS file. It then relies on other
'routines to calculate the expanded stats,
'write them to a file and present a categ-
'orized menu to use for reviewing them

'statType
'   0 = College
'   1 = Pro
Sub ViewExpandedStats (statType)

    Dim printOrView

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared TMM$(), TPP$(), statsZ1$()

    confStatFile$ = ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

    If FileExit% <> 27 Then

        Erase statsZ1$

        For X = 1 To 30
            statsZ1$(X) = "XXX"
        Next

        JA = 0
        JB = 0
        JC = 0
        JD = 0
        JE = 0
        JF = 0
        JG = 0
        JH = 0
        JI = 0
        JJ = 0
        printOrView = 0
        totalStats = 0

        div1$ = ""
        div2$ = ""
        div3$ = ""

        Erase TMM$, TPP$

        Open diskPaths$(2) + confStatFile$ + ".TMS" For Input As #1

        If statType = 1 Then
            'Pro
            Input #1, league$
            Input #1, div1$
            Input #1, div2$
            Input #1, div3$
        Else
            'College
            Input #1, league$
            Input #1, div1$
            Input #1, div2$
        End If

        For X = 1 To 30: Input #1, statsZ1$(X): Next

        Close #1

        Cls

        Call UpdateExpandedLeaders(statType, totalStats)

        Call SAVELEADER(confStatFile$, totalStats)

        Call ExpandedLeaders_Menu(totalStats)

    End If

End Sub


'----------------------------------------
'    UpdateExpandedLeaders Subroutine
'----------------------------------------
'This subroutine calculates the expanded
'stats from those previously read in to
'a variety of arrays.

'statType
'   0 = College
'   1 = Pro
Sub UpdateExpandedLeaders (statType, totalStats)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()

    Shared AM!(), AF!()
    Shared statsLC!(), statsLK!(), statsLF!(), statsLI!(), statsLP!(), statsLR!()
    Shared statsPK!(), lookyTT!(), statsZ!(), statsZ1!(), statsZ2!()

    Shared statsGI%(), statsGS%()

    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared intName$(), statsLC$(), statsLR$()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), PKL$()
    Shared lookyRBacks$(), lookyWdRec$(), lookyQBacks$(), lookyKickRet$(), lookyPRet$(), lookyPunter$(), lookyKicker$()
    Shared sackName$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), statsPK$()
    Shared QBL$(), lookyTT$(), statsZ1$()

    Shared GMA!(), GMB!(), GMC!()
    Shared gameCount()

    Color 15, 0
    Cls

    totalStats = 0

    For ZX = 1 To 30

        teamStatFile$ = statsZ1$(ZX)

        If teamStatFile$ <> "XXX" And teamStatFile$ <> "*" And teamStatFile$ <> " " Then

            If _FileExists(diskPaths$(1) + teamStatFile$) Then

                Print "CONVERTING STAT FILE OF "; teamStatFile$

                Open diskPaths$(1) + teamStatFile$ For Input As #1

                Input #1, statsA$

                'RB Names
                For I = 0 To 9:
                    Input #1, lookyRBacks$(I)
                Next I

                'WR Names
                For I = 0 To 5:
                    Input #1, lookyWdRec$(I)
                Next I

                'QB Names
                For I = 0 To 3:
                    Input #1, lookyQBacks$(I)
                Next I

                'KR Names
                For I = 0 To 2:
                    Input #1, lookyKickRet$(I)
                Next I

                'PR Names
                For I = 0 To 2:
                    Input #1, lookyPRet$(I)
                Next I

                'Punters
                Input #1, lookyPunter$(0)

                'Place Kickers
                For I = 0 To 1:
                    Input #1, lookyKicker$(I)
                Next I

                'Leader Values
                '--------------------
                'Rushers / Receivers
                For J = 0 To 13
                    For K = 0 To 17:
                        Input #1, statsZ2!(J, K)
                    Next K
                Next J

                'Offensive Team stats
                For L = 0 To 38:
                    Input #1, statsZ!(L)
                Next L

                'Defensive Team stats
                For L = 0 To 38:
                    Input #1, statsZ1!(L)
                Next L

                For J = 0 To 9:
                    'Interception Names
                    Input #1, intName$(J)
                    For J1 = 0 To 2:
                        Input #1, statsGI%(J, J1)
                    Next
                Next

                For J = 0 To 14:
                    'Sack Names
                    Input #1, sackName$(J)
                    For J1 = 0 To 1:
                        Input #1, statsGS%(J, J1)
                    Next
                Next

                'Longest rush
                For I = 0 To 9:
                    Input #1, statsLR!(I)
                    Input #1, statsLR$(I, 0)
                    Input #1, statsLR$(I, 1)
                Next

                'Longest catch
                For I = 0 To 15:
                    Input #1, statsLC!(I)
                    Input #1, statsLC$(I, 0)
                    Input #1, statsLC$(I, 1)
                Next

                'Longest pass
                For I = 0 To 3:
                    Input #1, statsLP!(I)
                    Input #1, statsLP$(I, 0)
                    Input #1, statsLP$(I, 1)
                Next

                'Longest kick return
                For I = 0 To 2:
                    Input #1, statsLK!(I)
                    Input #1, statsLK$(I, 0)
                    Input #1, statsLK$(I, 1)
                Next

                'Longest punt return
                For I = 0 To 2:
                    Input #1, statsPK!(I)
                    Input #1, statsPK$(I, 0)
                    Input #1, statsPK$(I, 1)
                Next

                'Longest FG
                For I = 0 To 1
                    Input #1, statsLF!(I)
                    Input #1, statsLF$(I)
                    For I1 = 0 To 4
                        Input #1, AM!(I, I1)
                        Input #1, AF!(I, I1)
                    Next
                Next

                'Longest Interception
                '--> was previously NOT read in
                '--> is it not even calculated later!!!
                For I = 0 To 9:
                    Input #1, statsLI!(I)
                    Input #1, statsLI$(I, 0)
                    Input #1, statsLI$(I, 1)
                Next

                Close #1

                Open diskPaths$(1) + teamStatFile$ + "S" For Input As #2
                Input #2, nbrGames
                Close #2

                totalStats = totalStats + 1

                For I = 0 To 14
                    lookyTT!(totalStats, I) = statsZ!(I)
                    lookyTT$(totalStats, I) = statsA$
                Next

                For I = 15 To 29
                    lookyTT!(totalStats, I) = statsZ1!(I - 15)
                    lookyTT$(totalStats, I) = statsA$
                Next

                For I = 0 To 9
                    JA = JA + 1
                    A1L$(JA) = lookyRBacks$(I)
                    A1T$(JA) = statsA$
                    LRL$(JA) = statsLR$(I, 0)
                    LCL$(JA) = statsLC$(I, 0)
                    'I added the line below based on patterns in this routine
                    'In original code GMA!() was written to output files,
                    'but was never assigned a value!!!
                    GMA!(JA) = nbrGames

                    For I1 = 0 To 5:
                        A1L!(JA, I1) = statsZ2!(I, I1)
                    Next I1

                    A1L!(JA, 6) = statsLR!(I)
                    A1L!(JA, 7) = statsLC!(I)
                Next I

                For I = 0 To 5
                    JB = JB + 1
                    A2L$(JB) = lookyWdRec$(I)
                    A2T$(JB) = statsA$
                    LCL$(JB) = statsLC$(I + 10, 0)
                    GMB!(JB) = nbrGames

                    For I1 = 6 To 8:
                        A2L!(JB, I1 - 6) = statsZ2!(I, I1)
                    Next I1

                    A2L!(JB, 3) = statsLC!(I + 10)
                Next I

                For I = 0 To 3

                    JC = JC + 1
                    QBL$(JC) = lookyQBacks$(I)
                    A3T$(JC) = statsA$
                    LPL$(JC) = statsLP$(I, 0)
                    GMC!(JC) = nbrGames

                    For I1 = 9 To 13:
                        A3L!(JC, I1 - 9) = statsZ2!(I, I1)
                    Next I1

                    A3L!(JC, 5) = statsLP!(I)
                    Call QBRating_Expanded(JC, statType)
                    A3R!(JC, 0) = A3L!(JC, 12)

                Next I

                For I = 0 To 2

                    JD = JD + 1
                    A4L$(JD) = lookyKickRet$(I)
                    A4T$(JD) = statsA$
                    LKL$(JD) = statsLK$(I, 0)

                    For I1 = 14 To 16:
                        A4L!(JD, I1 - 14) = statsZ2!(I, I1)
                    Next I1

                    A4L!(JD, 3) = statsLK!(I)

                Next I

                For I = 0 To 2

                    JE = JE + 1
                    A5L$(JE) = lookyPRet$(I)
                    A5T$(JE) = statsA$
                    PKL$(JE) = statsPK$(I, 0)

                    For I1 = 14 To 16:
                        A5L!(JE, I1 - 14) = statsZ2!(I + 3, I1)
                    Next I1

                    A5L!(JE, 3) = statsPK!(I)

                Next I

                JF = JF + 1
                A6L$(JF) = lookyPunter$(0)
                A6T$(JF) = statsA$
                A6L!(JF, 0) = statsZ!(7)
                A6L!(JF, 1) = statsZ!(8)
                JG = JG + 1
                A7L$(JG) = lookyKicker$(0)
                A7T$(JG) = statsA$

                For I = 0 To 3:
                    A7L!(JG, I) = statsZ2!(I + 3, 17)
                Next

                A7L!(JG, 4) = statsLF!(0)
                JG = JG + 1
                A7L$(JG) = lookyKicker$(1)
                A7T$(JG) = statsA$

                For I = 0 To 3:
                    A7L!(JG, I) = statsZ2!(I + 7, 17)
                Next

                A7L!(JG, 4) = statsLF!(1)

                For I = 0 To 9
                    JH = JH + 1
                    A8L$(JH) = intName$(I)
                    A8T$(JH) = statsA$
                    A8L!(JH) = statsGI%(I, 0)
                Next

                For I = 0 To 14
                    JI = JI + 1
                    A9L$(JI) = sackName$(I)
                    A9T$(JI) = statsA$
                    A9L!(JI) = statsGS%(I, 0)
                Next

                JJ = JJ + 1
                gameCount(JJ) = nbrGames

            End If

        End If

    Next ZX

End Sub


'----------------------------------------
'        READLEADER Subroutine
'----------------------------------------
'This subroutine reads in the leader files
'(.LDS) for the expanded standings modules
Sub READLEADER (leaderFile$, totalStats)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), lookyTT$()
    Shared PKL$(), QBL$()

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()
    Shared lookyTT!()

    Shared GMA!(), GMB!(), GMC!()
    Shared gameCount()

    TMR = 1660

    'Print "READING LEADER FILES . . . . PLEASE WAIT"

    If Not _FileExists(diskPaths$(2) + leaderFile$ + ".LDS") Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open diskPaths$(2) + leaderFile$ + ".LDS" For Input As #1

        Input #1, JA, JB, JC, JD, JE, JF, JG, JH, JI, totalStats, JJ

        For I = 1 To 30:

            For I1 = 0 To 29:
                Input #1, lookyTT!(I, I1), lookyTT$(I, I1)
            Next

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next

        For I = 1 To 300:

            Input #1, A1L$(I), A1T$(I), LRL$(I), LCL$(I), GMA!(I)

            For I1 = 0 To 7:
                Input #1, A1L!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 180:

            Input #1, A2L$(I), A2T$(I), LCL$(I), GMB!(I)

            For I1 = 0 To 3:
                Input #1, A2L!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 120:

            Input #1, QBL$(I), A3T$(I), LPL$(I), A3R!(I, 0), GMC!(I)

            For I1 = 0 To 6:
                Input #1, A3L!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 90:

            Input #1, A4L$(I), A4T$(I), LKL$(I)

            For I1 = 0 To 3: Input #1, A4L!(I, I1): Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 90:

            Input #1, A5L$(I), A5T$(I), PKL$(I)

            For I1 = 0 To 3:
                Input #1, A5L!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 30:

            Input #1, A6L$(I), A6T$(I)

            For I1 = 0 To 1:
                Input #1, A6L!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 60:

            Input #1, A7L$(I), A7T$(I)

            For I1 = 0 To 4:
                Input #1, A7L!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 300:

            Input #1, A8L$(I), A8T$(I), A8L!(I)

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 450:

            Input #1, A9L$(I), A9T$(I), A9L!(I)

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 30:
            Input #1, gameCount(I)
        Next

        Close #1

    End If

End Sub


'----------------------------------------
'        SAVELEADER Subroutine
'----------------------------------------
'This subroutine compiles the leader files
'(.LDS) for the expanded standings modules
Sub SAVELEADER (leaderFile$, totalStats)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), lookyTT$()
    Shared PKL$(), QBL$()

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()
    Shared lookyTT!()

    Shared GMA!(), GMB!(), GMC!()
    Shared gameCount()

    TMR = 1660

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + leaderFile$ + ".LDS" For Output As #1

    Write #1, JA, JB, JC, JD, JE, JF, JG, JH, JI, totalStats, JJ

    For I = 1 To 30:

        For I1 = 0 To 29:
            Write #1, lookyTT!(I, I1), lookyTT$(I, I1)
        Next

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next

    For I = 1 To 300:

        Write #1, A1L$(I), A1T$(I), LRL$(I), LCL$(I), GMA!(I)

        For I1 = 0 To 7:
            Write #1, A1L!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 180:

        Write #1, A2L$(I), A2T$(I), LCL$(I), GMB!(I)

        For I1 = 0 To 3:
            Write #1, A2L!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 120:

        Write #1, QBL$(I), A3T$(I), LPL$(I), A3R!(I, 0), GMC!(I)

        For I1 = 0 To 6:
            Write #1, A3L!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 90:

        Write #1, A4L$(I), A4T$(I), LKL$(I)

        For I1 = 0 To 3: Write #1, A4L!(I, I1): Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 90:

        Write #1, A5L$(I), A5T$(I), PKL$(I)

        For I1 = 0 To 3:
            Write #1, A5L!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 30:

        Write #1, A6L$(I), A6T$(I)

        For I1 = 0 To 1:
            Write #1, A6L!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 60:

        Write #1, A7L$(I), A7T$(I)

        For I1 = 0 To 4:
            Write #1, A7L!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 300:

        Write #1, A8L$(I), A8T$(I), A8L!(I)

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 450:

        Write #1, A9L$(I), A9T$(I), A9L!(I)

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 30:
        Write #1, gameCount(I)
    Next

    Close #1

End Sub


'----------------------------------------
'    ExpandedLeaders_Menu Subroutine
'----------------------------------------
'This subroutine presents a categorized
'menu for viewing expanded leaders. It then
'relies on various subroutines for the end
'user display of those statistics.
Sub ExpandedLeaders_Menu (totalStats)

    Dim PPP, sortType

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared expCategories$(), lookyTT$()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), PKL$()
    Shared QBL$(), TMM$(), TPP$(), TYY$(), statsZ1$()

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()
    Shared lookyTT!(), TYP!()

    Shared gameCount()

    Do

        Color 0, 0
        Cls

        J = 15

        Locate 1, 15

        Color 4, 15

        Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";

        For II = 1 To J
            Color 4, 15
            Locate 1 + II, 15
            Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
            Color 7, 4: Print String$(2, 177)
        Next

        Color 4, 15
        Locate 1 + J, 15
        Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ";

        Color 7, 4: Print String$(2, 177)
        Locate , 17: Print String$(44, 177)

        RR = 0
        newCategory = 0

        Locate 2, 24
        Color 4, 15: Print "LEADER SELECTION MENU"
        Print
        Locate , 20: Color 14: Print "(A) ";
        Color 1: Print "VIEW RUSHING LEADERS"
        Locate , 20: Color 14: Print "(B) ";
        Color 1: Print "VIEW RECEIVING LEADERS"
        Locate , 20: Color 14: Print "(C) ";
        Color 1: Print "VIEW QUARTERBACK LEADERS"
        Locate , 20: Color 14: Print "(D) ";
        Color 1: Print "VIEW KICKOFF RETURN LEADERS"
        Locate , 20: Color 14: Print "(E) ";
        Color 1: Print "VIEW PUNT RETURN LEADERS"
        Locate , 20: Color 14: Print "(F) ";
        Color 1: Print "VIEW PUNTING LEADERS"
        Locate , 20: Color 14: Print "(G) ";
        Color 1: Print "VIEW PLACE KICKING LEADERS"
        Locate , 20: Color 14: Print "(H) ";
        Color 1: Print "VIEW INTERCEPTION, SACK LEADERS"
        Locate , 20: Color 14: Print "(I) ";
        Color 1: Print "VIEW ALL-PURPOSE LEADERS"
        Locate , 20: Color 14: Print "(J) ";
        Color 1: Print "VIEW SCORING LEADERS"
        Locate , 20: Color 14: Print "(K) ";
        Color 1: Print "VIEW TEAM TOTALS"
        Locate , 20: Color 14: Print "(L) ";
        Color 1: Print "RETURN TO MAIN MENU"

        key$ = GetKeyPress$

        If key$ <> Chr$(27) Then

            Select Case UCase$(key$)

                Case "A":
                    J = 8
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 4

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 0 To 4:
                            Color 14
                            Locate I + 3, 24
                            Print Using "##"; I;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 5

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "B":
                    J = 8
                    ZZ = 1

                    Locate 2, 22

                    Color 0, 2

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 5 To 9:
                            Color 14
                            Locate I - 2, 24
                            Print Using "##"; I - 5;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 5

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "C":
                    J = 12
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 13

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 10 To 18:
                            Color 14
                            Locate I - 7, 24
                            Print Using "##"; I - 10;: Print " ";
                            Color 15: Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 8

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "D":
                    J = 8
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 5

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 19 To 23:
                            Color 14
                            Locate I - 16, 24
                            Print Using "##"; I - 19;: Print " ";
                            Color 15
                            Print expCategories$(I):
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 4

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "E":
                    J = 8
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 5

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 24 To 28:
                            Color 14
                            Locate I - 21, 24
                            Print Using "##"; I - 24;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 4

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "F":
                    J = 6
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 5

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 29 To 31:
                            Color 14
                            Locate I - 26, 24
                            Print Using "##"; I - 29;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 2

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "G":
                    J = 10
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 10

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 32 To 38:
                            Color 14
                            Locate I - 29, 24
                            Print Using "##"; I - 32;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 6

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "H":
                    J = 6
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 12

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 39 To 40:
                            Color 14
                            Locate I - 36, 24
                            Print Using "##"; I - 39;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 1

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "I":
                    Call SortOptions(printOrView, sortType, newCategory)

                Case "J":
                    J = 6
                    ZZ = 1

                    Locate 2, 22
                    Color 0, 13

                    Call CATEGORY_BOX(J)

                    Do
                        For I = 41 To 43:
                            Color 14
                            Locate I - 38, 24
                            Print Using "##"; I - 41;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 2

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "K":
                    J = 17
                    ZZ = 1

                    Locate 2, 2
                    Color 0, 14

                    Print " " + Chr$(214); String$(66, 196); Chr$(183) + " ";

                    For II = 1 To J:
                        Locate 2 + II, 2: Print " " + Chr$(186); String$(66, 32); Chr$(186) + " ";
                    Next

                    Locate 3 + J, 2
                    Print " " + Chr$(211); String$(66, 196); Chr$(189) + " ";
                    Locate 10, 15

                    Do
                        For I = 44 To 58:
                            Color 14
                            Locate I - 41, 4
                            Print Using "##"; I - 44;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        For I = 59 To 72:
                            Color 14
                            Locate I - 56, 40
                            Print Using "##"; I - 44;: Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; B9

                    Loop Until B9 <= 28

                    Call SortOptions(printOrView, sortType, newCategory)

            End Select

            If UCase$(key$) <> "I" And UCase$(key$) <> "L" And UCase$(key$) <> "K" Then
                stpl = 0
                PPP = 0
            End If

            If UCase$(key$) <> "L" And newCategory = 0 Then

                Select Case UCase$(key$)

                    Case "A":

                        Select Case B9
                            Case 0: X = 0: XX = 0
                            Case 1: X = 1: XX = 1
                            Case 2: X = 0: X1 = 1: XX = 2
                            Case 3: X = 2: XX = 3
                            Case 4: X = 6: XX = 4
                        End Select

                        Cls
                        PPP = 0

                        Select Case B9

                            Case 0, 1, 3

                                For I = 1 To JA
                                    If A1L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = A1L!(I, X)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                                TVN = 1

                            Case 2

                                For I = 1 To JA
                                    If A1L!(I, X) > gameCount(1) * 10 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = A1L!(I, X1) / A1L!(I, X)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                                TVN = 2

                            Case 4

                                Cls
                                PPP = 0

                                For I = 1 To JA
                                    If A1L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = A1L!(I, X)
                                        TYY$(PPP) = LRL$(I)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                                TVN = 3

                        End Select

                    Case "B":

                        Select Case B9
                            Case 0: X = 3: X1 = 0: XX = 5
                            Case 1: X = 4: X1 = 1: XX = 6
                            Case 2: X = 3: X1 = 4: X2 = 0: X3 = 1: XX = 7
                            Case 3: X = 5: X1 = 2: XX = 8
                            Case 4: X = 3: X1 = 7: XX = 9
                        End Select

                        Cls
                        PPP = 0

                        Select Case B9

                            Case 0, 1, 3:
                                For I = 1 To JA
                                    If A1L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = A1L!(I, X)
                                    End If
                                Next I

                                For I = 1 To JB
                                    If A2L!(I, X1) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A2L$(I)
                                        TMM$(PPP) = A2T$(I)
                                        TYP!(PPP) = A2L!(I, X1)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:
                                Cls
                                PPP = 0

                                For I = 1 To JA
                                    If A1L!(I, X) >= gameCount(1) * 4 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = A1L!(I, X1) / A1L!(I, X)
                                    End If
                                Next I

                                For I = 1 To JB
                                    If A2L!(I, X2) >= gameCount(1) * 4 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A2L$(I)
                                        TMM$(PPP) = A2T$(I)
                                        TYP!(PPP) = A2L!(I, X3) / A2L!(I, X2)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 3:
                                Cls
                                PPP = 0

                                For I = 1 To JB
                                    If A2L!(I, 0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A2L$(I)
                                        TMM$(PPP) = A2T$(I)
                                        TYP!(PPP) = A2L!(I, X)
                                        TYY$(PPP) = LCL$(I)
                                    End If
                                Next I

                                For I = 1 To JA
                                    If A1L!(I, 4) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = A1L!(I, X1)
                                        TYY$(PPP) = LCL$(I)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 3

                        End Select

                    Case "C":

                        Select Case B9

                            Case 0: X = 0: XX = 10
                            Case 1: X = 1: XX = 11
                            Case 2: X = 2: XX = 12
                            Case 3: X = 3: XX = 13
                            Case 4: X = 4: XX = 14
                            Case 5: X = 0: X1 = 1: XX = 15
                            Case 6: X = 1: X1 = 3: X2 = 0: XX = 16
                            Case 7: X = 0: XX = 17
                            Case 8: X = 5: XX = 18

                        End Select

                        Cls
                        PPP = 0

                        Select Case B9

                            Case 0 To 4:

                                For I = 1 To JC
                                    If A3L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = QBL$(I)
                                        TMM$(PPP) = A3T$(I)
                                        TYP!(PPP) = A3L!(I, X)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 5:

                                For I = 1 To JC

                                    If A3L!(I, X) >= gameCount(1) * 15 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                        If A3L!(I, X1) <> 0 Then
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = 0
                                        Else
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = (A3L!(I, X1) / A3L!(I, X)) * 100
                                        End If

                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 6:

                                For I = 1 To JC
                                    If A3L!(I, X2) >= gameCount(1) * 15 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                        If A3L!(I, X1) <> 0 Then
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = A3L!(I, X1) / A3L!(I, X)
                                        Else
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = 0
                                        End If
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 4

                            Case 7:

                                For I = 1 To JC

                                    If A3L!(I, X) >= gameCount(1) * 15 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = QBL$(I)
                                        TMM$(PPP) = A3T$(I)
                                        TYP!(PPP) = A3R!(I, X)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 4

                            Case 8:

                                Cls
                                PPP = 0

                                For I = 1 To JC

                                    If A3L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = QBL$(I)
                                        TMM$(PPP) = A3T$(I)
                                        TYP!(PPP) = A3L!(I, X)
                                        TYY$(PPP) = LPL$(I)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 3

                        End Select

                    Case "D":

                        Select Case B9
                            Case 0: X = 0: XX = 19
                            Case 1: X = 1: XX = 20
                            Case 2: X = 0: X1 = 1: XX = 21
                            Case 3: X = 2: XX = 22
                            Case 4: X = 3: XX = 23
                        End Select

                        Cls
                        PPP = 0

                        Select Case B9

                            Case 0, 1, 3:

                                For I = 1 To JD

                                    If A4L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A4L$(I)
                                        TMM$(PPP) = A4T$(I)
                                        TYP!(PPP) = A4L!(I, X)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:

                                For I = 1 To JD

                                    If A4L!(I, X) <> 0 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        If A4L!(I, X1) <> 0 Then
                                            TPP$(PPP) = A4L$(I)
                                            TMM$(PPP) = A4T$(I)
                                            TYP!(PPP) = A4L!(I, X1) / A4L!(I, X)
                                        Else
                                            TPP$(PPP) = A4L$(I)
                                            TMM$(PPP) = A4T$(I)
                                            TYP = 0
                                        End If

                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 4:

                                For I = 1 To JD

                                    If A4L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A4L$(I)
                                        TMM$(PPP) = A4T$(I)
                                        TYP!(PPP) = A4L!(I, X): TYY$(PPP) = LKL$(I)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 3

                        End Select

                    Case "E":

                        Select Case B9
                            Case 0: X = 0: XX = 24
                            Case 1: X = 1: XX = 25
                            Case 2: X = 0: X1 = 1: XX = 26
                            Case 3: X = 2: XX = 27
                            Case 4: X = 3: XX = 28
                        End Select

                        Cls
                        PPP = 0

                        Select Case B9

                            Case 0, 1, 3:

                                For I = 1 To JE

                                    If A5L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A5L$(I)
                                        TMM$(PPP) = A5T$(I)
                                        TYP!(PPP) = A5L!(I, X)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:

                                For I = 1 To JE

                                    If A5L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        If A5L!(I, X1) <> 0 Then
                                            TPP$(PPP) = A5L$(I)
                                            TMM$(PPP) = A5T$(I)
                                            TYP!(PPP) = A5L!(I, X1) / A5L!(I, X)
                                        Else
                                            TPP$(PPP) = A5L$(I)
                                            TMM$(PPP) = A5T$(I)
                                            TYP = 0
                                        End If
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 4:

                                For I = 1 To JE

                                    If A5L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A5L$(I)
                                        TMM$(PPP) = A5T$(I)
                                        TYP!(PPP) = A5L!(I, X)
                                        TYY$(PPP) = PKL$(I)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                        End Select

                    Case "F":

                        Select Case B9
                            Case 0: X = 0: XX = 29
                            Case 1: X = 1: XX = 30
                            Case 2: X = 0: X1 = 1: XX = 31
                        End Select

                        Select Case B9

                            Case 0, 1:

                                For I = 1 To JF

                                    If A6L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A6L$(I)
                                        TMM$(PPP) = A6T$(I)
                                        TYP!(PPP) = A6L!(I, X)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:

                                For I = 1 To JF

                                    If A6L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        If A6L!(I, X1) <> 0 Then
                                            TPP$(PPP) = A6L$(I)
                                            TMM$(PPP) = A6T$(I)
                                            TYP!(PPP) = A6L!(I, X1) / A6L!(I, X)
                                        Else
                                            TPP$(PPP) = A6L$(I)
                                            TMM$(PPP) = A6T$(I)
                                            TYP = 0
                                        End If
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                        End Select

                    Case "G":

                        Select Case B9
                            Case 0: X = 0: XX = 32
                            Case 1: X = 1: XX = 33
                            Case 2: X = 0: X1 = 1: XX = 34
                            Case 3: X = 2: XX = 35
                            Case 4: X = 3: XX = 36
                            Case 5: X = 2: X1 = 3: XX = 37
                            Case 6: X = 4: XX = 38
                        End Select

                        Cls
                        PPP = 0

                        Select Case B9

                            Case 0, 1, 3, 4, 6

                                For I = 1 To JG

                                    If A7L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A7L$(I)
                                        TMM$(PPP) = A7T$(I)
                                        TYP!(PPP) = A7L!(I, X)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2, 5

                                For I = 1 To JG

                                    If A7L!(I, X) <> 0 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                        If A7L!(I, X1) <> 0 Then
                                            TPP$(PPP) = A7L$(I)
                                            TMM$(PPP) = A7T$(I)
                                            TYP!(PPP) = (A7L!(I, X) / A7L!(I, X1)) * 100
                                        Else
                                            TPP$(PPP) = A7L$(I)
                                            TMM$(PPP) = A7T$(I)
                                            TYP = 0
                                        End If

                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 4

                        End Select

                    Case "H":

                        If B9 = 0 Then
                            X = 0: XX = 39
                        End If
                        If B9 = 1 Then
                            X = 0: XX = 40
                        End If

                        Cls
                        PPP = 0

                        If B9 = 0 Then

                            For I = 1 To JH
                                If A8L!(I) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A8L$(I)
                                    TMM$(PPP) = A8T$(I)
                                    TYP!(PPP) = A8L!(I)
                                End If
                            Next I

                        End If

                        If B9 = 1 Then
                            For I = 1 To JI
                                If A9L!(I) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A9L$(I)
                                    TMM$(PPP) = A9T$(I)
                                    TYP!(PPP) = A9L!(I)
                                End If
                            Next I

                        End If

                        Call SortPlayer(PPP, stpl, sortType)

                        TVN = 1

                    Case "I":
                        Cls

                        PPP = 0
                        X = 1
                        X1 = 4
                        XX = 73

                        For I = 1 To JA
                            If A1L!(I, X) + A1L!(I, X1) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5
                                Print "CHANGING PLAYER "; PPP
                                TPP$(PPP) = A1L$(I)
                                TMM$(PPP) = A1T$(I)
                                TYP!(PPP) = A1L!(I, X) + A1L!(I, X1)
                            End If
                        Next I

                        For I = 1 To JB
                            If A2L!(I, X) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5
                                Print "CHANGING PLAYER "; PPP
                                TPP$(PPP) = A2L$(I)
                                TMM$(PPP) = A2T$(I)
                                TYP!(PPP) = A2L!(I, X)
                            End If
                        Next I

                        Call SortPlayer(PPP, stpl, sortType)

                        TVN = 1

                    Case "J":
                        Select Case B9
                            Case 0
                                X = 2
                                X1 = 5
                                XX = 41
                                KC = 1

                            Case 1
                                XF = 0
                                XF1 = 2
                                XX = 42
                                KC = 2

                            Case 2
                                X = 2
                                X1 = 5
                                XF = 0
                                XF1 = 2
                                XX = 43
                                KC = 3

                        End Select

                        Cls
                        PPP = 0

                        If KC = 1 Or KC = 3 Then
                            For I = 1 To JA
                                If A1L!(I, X) + A1L!(I, X1) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A1L$(I)
                                    TMM$(PPP) = A1T$(I)
                                    TYP!(PPP) = (A1L!(I, X) + A1L!(I, X1)) * 6
                                End If
                            Next I

                            For I = 1 To JB
                                If A2L!(I, X) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A2L$(I)
                                    TMM$(PPP) = A2T$(I)
                                    TYP!(PPP) = A2L!(I, X) * 6
                                End If
                            Next I

                        End If

                        If KC <> 1 Then
                            For I = 1 To JG
                                If A7L!(I, XF) + A7L!(I, XF1) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A7L$(I)
                                    TMM$(PPP) = A7T$(I)
                                    TYP!(PPP) = (A7L!(I, XF) * 3) + A7L!(I, XF1)
                                End If
                            Next I
                        End If

                        Call SortPlayer(PPP, stpl, sortType)

                        TVN = 1

                    Case "K":
                        Select Case B9
                            Case 0: X = 0: XX = 44
                            Case 1: X = 1: X1 = 13: XX = 45
                            Case 2: X = 2: X1 = 14: XX = 46
                            Case 3: X = 1: X1 = 13: X2 = 2: X3 = 14: XX = 47
                            Case 4: X = 6: XX = 48
                            Case 5: X = 2: X1 = 6: XX = 49
                            Case 6: X = 4: X1 = 3: XX = 50
                            Case 7: X = 9: XX = 51
                            Case 8: X = 10: XX = 52
                            Case 9: X = 5: XX = 53
                            Case 10: X = 5: X1 = 10: X2 = 20: X3 = 25: XX = 54
                            Case 11: X = 11: XX = 55
                            Case 12: X = 12: XX = 56
                            Case 13: X = 13: XX = 57
                            Case 14: X = 14: XX = 58
                            Case 15: X = 15: XX = 59
                            Case 16: X = 16: X1 = 28: XX = 60
                            Case 17: X = 17: X1 = 29: XX = 61
                            Case 18: X = 16: X1 = 28: X2 = 17: X3 = 29: XX = 62
                            Case 19: X = 21: XX = 63
                            Case 20: X = 17: X1 = 21: XX = 64
                            Case 21: X = 19: X1 = 18: XX = 65
                            Case 22: X = 24: XX = 66
                            Case 23: X = 25: XX = 67
                            Case 24: X = 20: XX = 68
                            Case 25: X = 26: XX = 69
                            Case 26: X = 27: XX = 70
                            Case 27: X = 28: XX = 71
                            Case 28: X = 29: XX = 72
                        End Select

                        Select Case B9
                            Case 0, 4, 7 To 9, 11 To 15, 19, 22 To 28
                                For I = 1 To totalStats
                                    TMM$(I) = lookyTT$(I, X)
                                    TYP!(I) = lookyTT!(I, X)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                            Case 1, 16
                                For I = 1 To totalStats
                                    TMM$(I) = lookyTT$(I, X)
                                    TYP!(I) = lookyTT!(I, X) - lookyTT!(I, X1)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                            Case 2, 5, 17, 20
                                For I = 1 To totalStats
                                    TMM$(I) = lookyTT$(I, X)
                                    TYP!(I) = lookyTT!(I, X) + lookyTT!(I, X1)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                            Case 3, 18
                                For I = 1 To totalStats
                                    TMM$(I) = lookyTT$(I, X)
                                    TYP!(I) = (lookyTT!(I, X2) + lookyTT!(I, X3)) / (lookyTT!(I, X) - lookyTT!(I, X1))
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 2

                            Case 6, 21
                                For I = 1 To totalStats
                                    TMM$(I) = lookyTT$(I, X)
                                    TYP!(I) = (lookyTT!(I, X) / lookyTT!(I, X1)) * 100
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 2

                            Case 10
                                For I = 1 To totalStats
                                    TMM$(I) = lookyTT$(I, X)
                                    TYP!(I) = lookyTT!(I, X2) + lookyTT!(I, X3) - lookyTT!(I, X) - lookyTT!(I, X1)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                        End Select

                End Select

                If UCase$(key$) = "K" Then
                    QW = totalStats
                Else
                    QW = PPP
                End If

                If printOrView = 1 Then
                    Call DisplayLeaders(XX, QW, TVN)
                Else
                    'printOrView = 2
                    Call GetListLength(printOrView, LG, QW)

                    Color 15, 0
                    Cls

                    Call MenuBox(8, 25, 24, 5, 15, 1)

                    Do

                        Locate 10, 32
                        Color 7, 1

                        Locate , 32: Print "SEND TO ";
                        Color 15: Print "F";
                        Color 7: Print "ILE"
                        Locate , 32: Print "SEND TO ";
                        Color 15: Print "P";
                        Color 7: Print "RINTER"

                        I$ = GetKeyPress$

                        Color 14, 0

                    Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P"

                    Select Case UCase$(I$)

                        Case "F"
                            userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                            If userFile$ <> "" Then
                                Call PrintLeaders(0, userFile$, XX, LG, TVN)
                            End If

                        Case "P"
                            Call PrintLeaders(1, "REPORT", XX, LG, TVN)

                    End Select

                End If

            End If

        End If

    Loop Until UCase$(key$) = "L" Or key$ = Chr$(27)

End Sub


'----------------------------------------
'        CATEGORY_BOX Subroutine
'----------------------------------------
'This subroutines draws a simple box around
'several of the category-based selections
'of the expanded standings menus
Sub CATEGORY_BOX (J)

    Print " " + Chr$(214); String$(36, 196); Chr$(183) + " ";

    For II = 1 To J:
        Locate 2 + II, 22: Print " " + Chr$(186); String$(36, 32); Chr$(186) + " ";
    Next

    Locate 3 + J, 22
    Print " " + Chr$(211); String$(36, 196); Chr$(189) + " ";
    Locate 10, 15

End Sub


'----------------------------------------
'         SortOptions Subroutine
'----------------------------------------
'This subroutine prompts the users for the
'different sort options available in the
'"expanded" stats menus.

'statType
'   0 = College
'   1 = Pro

'sortType
'   1: Sort HIGH to LOW
'   2: Sort LOW to HIGH
Sub SortOptions (printOrView, sortType, reselect)

    Do

        J = 6

        Locate 8, 8: Color 15, 1
        Print " " + Chr$(214); String$(57, 196); Chr$(183) + " ";

        For II = 1 To J:
            Locate 8 + II, 8
            Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";
        Next

        Locate 9 + J, 8
        Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";
        Locate 10, 15

        Color 3

        Locate , 30: Print "SELECT OPTION:"
        Locate , 30: Color 15: Print "H";
        Color 7: Print "IGHEST TO LOWEST"
        Locate , 30: Color 15: Print "L";
        Color 7: Print "OWEST TO HIGHEST"
        Locate , 30: Color 15: Print "R";
        Color 7: Print "E-SELECT CATEGORY"

        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "H" Or UCase$(I$) = "L" Or UCase$(I$) = "R"

        Select Case UCase$(I$)

            Case "H":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P": printOrView = 2
                    Case "V": printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 1

            Case "L":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P": printOrView = 2
                    Case "V": printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 2

            Case "R", Chr$(27):
                'If ZZ = 1 Then Call ExpandedLeaders_Menu(totalStats)
                reselect = 1

        End Select

    Loop Until UCase$(I2$) <> "R" Or UCase$(I$) = "R"

End Sub


'----------------------------------------
'       PrintViewReselect Subroutine
'----------------------------------------
'This subroutine presents the end user
'with a choice to Print or View the exist-
'ing category, or select a new category
Sub PrintViewReselect (userOpt$)

    J = 4

    Locate 9, 15
    Color 15, 0

    Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";

    For II = 1 To J
        Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
    Next

    Locate 10 + J, 15
    Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ";
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'         SortPlayer Subroutine
'----------------------------------------
'This subroutine sorts the players involved
'in the chosen stat category
Sub SortPlayer (stpl, PPP, sortType)

    Shared TMM$(), TPP$(), TYY$()
    Shared TYP!()

    For I = 2 To PPP

        stpl = stpl + 1

        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TYY$(J - 1), TYY$(J)
                End If
            End If

            If sortType = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TYY$(J - 1), TYY$(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'           SortTeam Subroutine
'----------------------------------------
'This subroutine sorts the teams involved
'in the chosen stat category
Sub SortTeam (sortMax, stpl, sortType)

    Shared TYP!()
    Shared TMM$()

    For I = 2 To sortMax

        stpl = stpl + 1

        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

            If sortType = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'       DisplayLeaders Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the user display
Sub DisplayLeaders (XX, QW, TVN)

    Shared expCategories$(), TMM$(), TPP$(), TYY$()
    Shared TYP!()

    Do

        Cls

        J = 18

        Locate 1, 5: Color 15, 1

        Print " " + Chr$(214); String$(67, 196); Chr$(183) + " ";

        For II = 1 To J
            Color 15, 1
            Locate 1 + II, 5
            Print " " + Chr$(186); String$(67, 32); Chr$(186) + " ";
            Color 7, 1: Print String$(2, 177)
        Next

        Color 15, 1
        Locate 1 + J, 5
        Print " " + Chr$(211); String$(67, 196); Chr$(189) + " ";

        Color 7, 1: Print String$(2, 177)
        Locate , 7: Print String$(71, 177)

        R = Len(expCategories$(XX))

        Locate 2, 11

        Color 14, 1: Print "PLAYER             TEAM";

        Locate , 60 - (Int(R) / 2): Print expCategories$(XX)

        I$ = ""

        For W = 1 To QW

            RR = RR + 1

            If W <> 1 And (W Mod 16 = 1) Then

                RR = 1

                Color 0, 15
                Locate 22, 17
                Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
                Color 1, 0: Print Chr$(220)
                Locate , 18: Print String$(50, 223)

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then

                    For I = 3 To 18:
                        Color 1, 1
                        Locate I, 7
                        Print Space$(66)
                    Next

                End If

            End If

            If I$ <> Chr$(27) Then

                Color 15
                Locate RR + 2, 7
                Print Using "###"; W;: Print " ";
                Color 3

                Select Case TVN

                    Case 1:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "#####"; TYP!(W)

                    Case 2:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.##"; TYP!(W)

                    Case 3:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.#"; TYP!(W);: Print Tab(63); TYY$(W)

                    Case 4:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.#"; TYP!(W)

                End Select

            Else

                Exit For

            End If

        Next W

        If I$ <> Chr$(27) Then

            Color 0, 15
            Locate 22, 17
            Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
            Color 1, 0: Print Chr$(220)
            Locate , 18: Print String$(50, 223)

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Color 0, 0
                Locate 22: Print Space$(69)
                Locate 23: Print Space$(69)

                Do
                    Locate 22, 32
                    Color 0, 15
                    Print "VIEW AGAIN? (Y/N)";

                    Color 1, 0
                    Print Chr$(220)
                    Locate , 33
                    Print String$(16, 223)

                    I$ = GetKeyPress$

                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then
                    RR = 0
                End If

            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         PrintLeaders Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the default printer
Sub PrintLeaders (printDest, destFile$, XX, LG, TVN)

    Shared expCategories$(), TMM$(), TPP$(), TYY$()
    Shared TYP!()

	Color 15, 0
	Cls

	Open destFile$ For Output As #1
	
    Print #1, "     PLAYER                  TEAM                     "; expCategories$(XX)

    For W = 1 To LG

        If (W <> 1 And W Mod 50 = 1) Then
            Print #1, Chr$(12)
            Print #1, "     PLAYER                  TEAM                     "; expCategories$(XX)
        End If

        Print #1, Using "###"; W;
        Print #1, " ";
        Print #1, TPP$(W);
        Print #1, Tab(25); TMM$(W);

        Select Case TVN
            Case 1
                Print #1, Tab(58); Using "#####"; TYP!(W)

            Case 2
                Print #1, Tab(58); Using "###.##"; TYP!(W)

            Case 3
                Print #1, Tab(58); Using "###.#"; TYP!(W);
                Print #1, Tab(63); TYY$(W)

            Case 4
                Print #1, Tab(58); Using "###.#"; TYP!(W)

        End Select

    Next W

	If printDest = 1 Then

		Open destFile$ For Input As #2

		Do While Not EOF(2)
			Line Input #2, X$
			LPrint X$
		Loop

		Close #2

		LPrint Chr$(12)

		Kill destFile$

	End If

End Sub


'----------------------------------------
'       TeamStatsToReport Subroutine
'----------------------------------------
'This subroutine displays the chosen team
'stat category to the user display
Sub TeamStatsToReport ()

    Shared expCategories$(), TMM$()
    Shared TYP!()

    Do

        Cls

        J = 18
        Locate 1, 5: Color 15, 4

        Print " " + Chr$(214); String$(67, 196); Chr$(183) + " ";

        For II = 1 To J
            Color 15, 4
            Locate 1 + II, 5
            Print " " + Chr$(186); String$(67, 32); Chr$(186) + " ";
            Color 7, 4: Print String$(2, 177)
        Next

        Color 15, 4: Locate 1 + J, 5
        Print " " + Chr$(211); String$(67, 196); Chr$(189) + " ";

        Color 7, 4: Print String$(2, 177)
        Locate , 7: Print String$(71, 177)

        Color 14, 4
        R = Len(expCategories$(XX))
        Locate 2, 40 - (Int(R) / 2): Print expCategories$(XX)

        Locate 3, 11

        Color 14, 4: Print "TEAM                               TEAM "

        For W = 1 To 15
            If TVN = 1 Then
                If TMM$(W) <> "" Then
                    Locate W + 3, 7
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(28); Using "#####"; TYP!(W)
                End If
            End If

            If TVN = 2 Then
                If TMM$(W) <> "" Then
                    Locate W + 3, 7
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(29); Using "##.#"; TYP!(W)
                End If
            End If
        Next W

        For W = 16 To 30
            If TVN = 1 Then
                If TMM$(W) <> "" Then
                    Locate W - 12, 42
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(63); Using "#####"; TYP!(W)
                End If
            End If

            If TVN = 2 Then
                If TMM$(W) <> "" Then
                    Locate W - 12, 42
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(64); Using "##.#"; TYP!(W)
                End If
            End If

        Next W

        Locate 22, 17
        Color 0, 15
        Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
        Color 4, 0: Print Chr$(220)
        Locate , 18: Print String$(50, 223)

        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Color 0, 0
            Locate 22: Print Space$(68)
            Locate 23: Print Space$(68)

            Do
                Locate 22, 32
                Color 0, 15
                Print "VIEW AGAIN? (Y/N)";

                Color 4, 0
                Print Chr$(220)
                Locate , 33
                Print String$(16, 223)

                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                RR = 0
            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'        GetListLength Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'maximum number of items to be displayed
'or returned by the standings view. This
'helps the user have a custom view of the
'categories without needing to page through
'every result.
Sub GetListLength (printOrView, listLength, maxItems)

    Color , 0
    Cls

    If printOrView <> 1 Then
        'If printing...

        Do
            Color , 0
            Cls

            Print
            Print
            Print
            Print
            Print
            J = 2

            Locate 10, 20
            Color 4, 15
            Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";

            For II = 1 To J:
                Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
            Next

            Locate 11 + J, 20
            Print " " + Chr$(211); String$(30, 196); Chr$(189) + " "
            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub

