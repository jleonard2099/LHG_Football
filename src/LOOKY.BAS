Sub LOOKY ()

    _Title "3-in-1 Football - View Stats"

    Do

        Color 15, 0
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
            Case "E", Chr$(27): '
        End Select

        If UCase$(I$) = "C" Or UCase$(I$) = "P" Then

            Call GetDisk_LOOKY(numberTeams, YN$, A$)
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

            Z$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

            If Not _FileExists(diskPaths$(1) + Z$) Then
                Call CSMsgBox("UNABLE TO LOCATE COMPILED STATS FILE PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)
            Else
                Call LoadTeamStats(Z$, A$, totalGames%, teamPower, SS)
                Call ViewStatSummary(Z$, A$, totalGames%, teamPower, SS)
            End If

        End If

    Loop Until UCase$(I$) = "E" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       LoadTeamStats Subroutine
'----------------------------------------
'...explanation...
Sub LoadTeamStats (statFile$, teamName$, totalGames%, teamPower, SS)

    Shared AF!(), AM!(), GI!(), GS!()
    Shared CRD!()
    Shared K!(), K2!(), PK!(), W6!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared statsZ!(), statsZ1!(), statsZ2!()

    Shared statsO%(), statsT%(), lookyP%()
    Shared lookyP%(), statsPA%()

    Shared statsH$(), compO$(), gameSITE$()
    Shared lookyDI$(), lookyDS$()
    Shared LC$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared statsPK$()

    Dim statsLF$(1)

    Dim compAP%(1, 120)

    Open diskPaths$(1) + statFile$ + "S" For Input As #2
    Input #2, totalGames%

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

    For I = 1 To totalGames%
        SS = compAP%(1, I) + SS
    Next

    SS = SS / totalGames%

    'INDIVIDUAL GAME POWER RATINGS
    teamPower = 0

    For I = 1 To totalGames%
        RAT = statsO%(I) - statsT%(I) + compAP%(1, I)
        If Left$(statsH$(I), 1) = "H" Then RAT = RAT - 3
        If Left$(statsH$(I), 1) = "A" Then RAT = RAT + 3
        teamPower = teamPower + RAT
    Next

    GL = 0

    If totalGames% <= 4 Then
        GL = 5 - totalGames%
        teamPower = teamPower + GL * compAP%(0, 1)
    End If

    teamPower = teamPower / (totalGames% + GL)

    L% = 0
    W% = 0
    X% = 0

    D% = 0
    PX = 0
    lookyP%(0) = 0
    lookyP%(1) = 0

    'Load player stats
    Open diskPaths$(1) + statFile$ For Input As #1

    'We are passed teamName$ as a variable from the disk routine
    'However the format of the files for football is such that
    'it's also contained here.
    'To be consistent with other games, it needs evaluated
    'if this belongs here or somewhere else.
    Input #1, teamName$

    For I = 0 To 9: Input #1, lookyA1$(PX, I): Next I
    For I = 0 To 5: Input #1, lookyA2$(PX, I): Next I
    For I = 0 To 3: Input #1, lookyA3$(PX, I): Next I
    For I = 0 To 2: Input #1, lookyA4$(PX, I): Next I
    For I = 0 To 2: Input #1, lookyA5$(PX, I): Next I

    Input #1, lookyA6$(PX, 0)

    For I = 0 To 1: Input #1, lookyA7$(PX, I): Next I
    For J = 0 To 13
        For K = 0 To 17: Input #1, statsZ2!(J, K): Next K
    Next J

    For L = 0 To 38: Input #1, statsZ!(L): Next L
    For L = 0 To 38: Input #1, statsZ1!(L): Next L
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
        For K = 0 To 17: K2!(PX, J, K) = statsZ2!(J, K): Next K
    Next J

    For M = 0 To 12: K!(0, M) = statsZ!(M): Next M
    For M = 0 To 12: K!(1, M) = statsZ1!(M): Next M

    W6!(0, 0) = statsZ!(13)
    W6!(0, 1) = statsZ!(14)
    W6!(1, 0) = statsZ1!(13)
    W6!(1, 1) = statsZ1!(14)

End Sub


'----------------------------------------
'       ViewStatSummary Subroutine
'----------------------------------------
Sub ViewStatSummary (statFile$, teamName$, totalGames%, teamPower, SS)

    Shared statsPA%()

    Do

        DM% = 0

        Color 15, 0
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
            Locate 10
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"

            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "S" Or UCase$(I$) = "P"

        Select Case UCase$(I$)

            Case "S"
                Call StatsToScreen(teamName$, totalGames%, statFormat, teamPower, SS)

            Case "P"
                Call StatsToPrinter(teamName$, totalGames%, statFormat, teamPower, SS)

        End Select

        Color 15, 0

        Cls
        JJ = 1
        Locate 6, 8
        Color statsPA%(2), statsPA%(1)

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

        Do
            Locate 7
            U$ = "DISPLAY STATS AGAIN (YN)"
            Locate , (80 - Len(U$)) / 2
            Print U$
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Color 15, 0

        'Call ResetStatsPrompt (statFile$)

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'      StatsToScreen Subroutine
'----------------------------------------
'...explanation...
Sub StatsToScreen (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared AF!(), AM!(), CRD!(), GI!(), GS!()
    Shared K!(), K2!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared PK!(), W6!(), statsZ!(), statsZ1!(), statsZ2!()

    Shared statsO%(), statsPA%(), statsT%(), lookyP%()

    Shared lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared lookyDI$(), lookyDS$(), statsH$()
    Shared LC$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared compO$(), statsPK$(), gameSITE$()

    Color , 0
    Cls

    Locate 1, 1
    Color 14, 1
    Print Tab(80);

    header$ = "GAME-BY-GAME REPORT FOR " + teamName$
    Xcoord = Len(header$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord

    Print "GAME-BY-GAME REPORT FOR ";
    Color statsPA%(2), statsPA%(1)
    Print teamName$

    Color 14, 1
    Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD   "
    Color 15, 0

    For I = 1 To totalGames%

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

        If (I Mod 20 = 0) Then
            I$ = GetKeyPress$
            ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***
            Cls
            Color , 0
            Locate 1, 1: Print Tab(80);
            Locate , Xcoord

            Color 14
            Print "GAME-BY-GAME REPORT FOR ";
            Color statsPA%(2), statsPA%(1)
            Print teamName$

            Color 14
            Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD   "
            Color 15, 0

        End If

    Next I

    If statFormat = 0 Then
        Locate 24, 1
        Print Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS;
    End If

    lookyP%(0) = 0
    lookyP%(1) = 0
    W% = 0
    L% = 0
    X% = 0

    I$ = GetKeyPress$
    Cls

    Call TITLE_STATS

    Locate 2
    Color 12
    Print teamName$; " STATISTICS REPORT"
    Print

    Color 14
    Print "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
    Color 15
    Print teamName$;
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; statsZ!(32); statsZ!(33); statsZ!(34); statsZ!(35); statsZ!(32) + statsZ!(33) + statsZ!(34) + statsZ!(35) + statsZ!(36)
    Print "OPPONENTS";
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; statsZ1!(32); statsZ1!(33); statsZ1!(34); statsZ1!(35); statsZ1!(32) + statsZ1!(33) + statsZ1!(34) + statsZ1!(35) + statsZ!(36)
    Print

    Color 14
    Print "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Color 15
    Print teamName$;
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsZ!(32) / totalGames%; statsZ!(33) / totalGames%; statsZ!(34) / totalGames%; statsZ!(35) / totalGames%; (statsZ!(32) + statsZ!(33) + statsZ!(34) + statsZ!(35) + statsZ!(36)) / totalGames%
    Print "OPPONENTS";
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsZ1!(32) / totalGames%; statsZ1!(33) / totalGames%; statsZ1!(34) / totalGames%; statsZ1!(35) / totalGames%; (statsZ1!(32) + statsZ1!(33) + statsZ1!(34) + statsZ1!(35) + statsZ!(36)) / totalGames%
    Print

    'H = Home; A = Away; N = Neutral; T = Total
    'D = Dates; T = Total; A = Average
    HD& = 0: AD& = 0: ND& = 0: TD& = 0
    HT& = 0: CT& = 0: NT& = 0: TT& = 0
    HA& = 0: AA& = 0: NA& = 0: TA& = 0

    For I = 1 To totalGames%
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

    I$ = GetKeyPress$
    Cls

    Call TITLE_STATS

    Locate 2
    Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

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
    If statFormat = 0 Then Print Using "###.#"; K!(0, 2) / totalGames%; Else Print Using "###.#"; (K!(0, 2) + W6!(0, 1)) / totalGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; K!(1, 2) / totalGames% Else Print Using "###.#"; (K!(1, 2) + W6!(1, 1)) / totalGames%

    Print
    Print "PASSES ATTEMPTED"; Tab(45); Using "####"; K!(0, 3);
    Print Tab(65); Using "####"; K!(1, 3)

    Print "PASSES COMPLETED"; Tab(45); Using "####"; K!(0, 4);
    Print Tab(65); Using "####"; K!(1, 4)

    Print "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; K!(0, 5);
    Print Tab(65); Using "####"; K!(1, 5)

    Print "PASS COMPLETION PERCENTAGE"; Tab(44);
    If K!(0, 3) <> 0 Then Print Using "###.#"; K!(0, 4) / K!(0, 3) * 100;
    If K!(1, 3) <> 0 Then Print Tab(64); Using "###.#"; K!(1, 4) / K!(1, 3) * 100
    If K!(1, 3) = 0 Then Print

    Color 15
    Print "NET YARDS PASSING"; Tab(45); Using "####"; K!(0, 6);
    Print Tab(65); Using "####"; K!(1, 6)

    Color 7
    Print "YARDS PER PASS ATTEMPT"; Tab(45);
    If K!(0, 3) <> 0 Then Print Using "##.#"; K!(0, 6) / K!(0, 3);
    If K!(1, 3) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 6) / K!(1, 3)
    If K!(1, 3) = 0 Then Print

    Print "YARDS PER PASS COMPLETION"; Tab(45);
    If K!(0, 4) <> 0 Then Print Using "##.#"; K!(0, 6) / K!(0, 4);
    If K!(1, 4) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 6) / K!(1, 4)
    If K!(1, 4) = 0 Then Print

    Print "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; K!(0, 6) / totalGames%;
    Print Tab(64); Using "###.#"; K!(1, 6) / totalGames%
    Print

    Print "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then Print Using "####"; K!(0, 1) + K!(0, 3); Else Print Using "####"; K!(0, 1) - W6!(0, 0) + K!(0, 3);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; K!(1, 1) + K!(1, 3) Else Print Using "####"; K!(1, 1) - W6!(1, 0) + K!(1, 3)

    Print "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; (K!(0, 1) + K!(0, 3)) / totalGames%; Else Print Using "###.#"; (K!(0, 1) - W6!(0, 0) + K!(0, 3)) / totalGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; (K!(1, 1) + K!(1, 3)) / totalGames% Else Print Using "###.#"; (K!(1, 1) - W6!(1, 0) + K!(1, 3)) / totalGames%

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
    If statFormat = 0 Then Print Using "###.#"; (K!(0, 2) + K!(0, 6)) / totalGames%; Else Print Using "###.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / totalGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; (K!(1, 2) + K!(1, 6)) / totalGames% Else Print Using "###.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / totalGames%

    I$ = GetKeyPress$
    Cls

    Call TITLE_STATS

    Locate 2: Color 12
    Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Color 7

    Print "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print Using "###"; statsZ!(22);: Print "/";: Print Using "####"; statsZ!(23);
    Print Tab(61); Using "###"; statsZ1!(22);: Print "/";: Print Using "####"; statsZ1!(23)

    Print "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsZ!(22) <> 0 Then Print Using "##.#"; statsZ!(23) / statsZ!(22);
    If statsZ1!(22) <> 0 Then Print Tab(65); Using "##.#"; statsZ1!(23) / statsZ1!(22)
    If statsZ1!(22) = 0 Then Print

    Print "KICKOFF RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsZ!(22) / totalGames%;: Print Tab(65); Using "##.#"; statsZ1!(22) / totalGames%

    Print "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print Using "###"; statsZ!(24);: Print "/";: Print Using "####"; statsZ!(25);
    Print Tab(61); Using "###"; statsZ1!(24);: Print "/";: Print Using "####"; statsZ1!(25)

    Print "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsZ!(24) <> 0 Then Print Using "##.#"; statsZ!(25) / statsZ!(24);
    If statsZ1!(24) <> 0 Then Print Tab(65); Using "##.#"; statsZ1!(25) / statsZ1!(24)
    If statsZ1!(24) = 0 Then Print

    Print "PUNT RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsZ!(24) / totalGames%;: Print Tab(65); Using "##.#"; statsZ1!(24) / totalGames%

    Print "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print Using "###"; statsZ!(26);: Print "/";: Print Using "####"; statsZ!(27);
    Print Tab(61); Using "###"; statsZ1!(26);: Print "/";: Print Using "####"; statsZ1!(27)

    Print "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsZ!(26) <> 0 Then Print Using "##.#"; statsZ!(27) / statsZ!(26);
    If statsZ1!(26) <> 0 Then Print Tab(65); Using "##.#"; statsZ1!(27) / statsZ1!(26)
    If statsZ1!(26) = 0 Then Print

    Print "INTERCEPTION RETURNS PER GAME"; Tab(45);: Print Using "##.#"; statsZ!(26) / totalGames%;: Print Tab(65); Using "##.#"; statsZ1!(26) / totalGames%
    Print

    Print "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print Using "###"; K!(0, 7);: Print "/";: Print Using "####"; K!(0, 8);
    Print Tab(61); Using "###"; K!(1, 7);: Print "/";: Print Using "####"; K!(1, 8)

    Print "AVERAGE YARDS PER PUNT"; Tab(45);
    If K!(0, 7) <> 0 Then Print Using "##.#"; K!(0, 8) / K!(0, 7);
    If K!(1, 7) <> 0 Then Print Tab(65); Using "##.#"; K!(1, 8) / K!(1, 7)
    If K!(1, 7) = 0 Then Print

    Print "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print Using "##.#"; K!(0, 7) / totalGames%;: Print Tab(65); Using "##.#"; K!(1, 7) / totalGames%
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
    Print Using "##.#"; K!(0, 11) / totalGames%;: Print "/";: Print Using "##.#"; K!(0, 12) / totalGames%;
    Print Tab(60); Using "##.#"; K!(1, 11) / totalGames%;: Print "/";: Print Using "##.#"; K!(1, 12) / totalGames%

    Print "SAFETIES"; Tab(45);: Print Using "####"; statsZ!(21);: Print Tab(65); Using "####"; statsZ1!(21)

    Print "TIMES SACKED / YARDS LOST"; Tab(41);
    Print Using "###"; W6!(0, 0);: Print "/";: Print Using "####"; W6!(0, 1);
    Print Tab(61); Using "###"; W6!(1, 0);: Print "/";: Print Using "####"; W6!(1, 1)

    I$ = GetKeyPress$
    Cls

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
            Print Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 0); K2!(I, I1, 1); K2!(I, I1, 1) / K2!(I, I1, 0); K2!(I, I1, 1) / totalGames%; K2!(I, I1, 2);
            If I1 > 9 Then
                Print
            Else
                Print Using "###"; LR!(I1);: Print LR$(I1, 0); Tab(64); LR$(I1, 1)
            End If
        End If

    Next I1

    Color 14
    Print
    Print teamName$; Tab(18);

    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; K!(0, 1); K!(0, 2); K!(0, 2) / K!(0, 1); K!(0, 2) / totalGames%; statsZ!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; K!(0, 1) - W6!(0, 0); K!(0, 2) + W6!(0, 1); (K!(0, 2) + W6!(0, 1)) / (K!(0, 1) - W6!(0, 0)); (K!(0, 2) + W6!(0, 1)) / totalGames%; statsZ!(15)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; K!(1, 1); K!(1, 2); K!(1, 2) / K!(1, 1); K!(1, 2) / totalGames%; statsZ1!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; K!(1, 1) - W6!(1, 0); K!(1, 2) + W6!(1, 1); (K!(1, 2) + W6!(1, 1)) / (K!(1, 1) - W6!(1, 0)); (K!(1, 2) + W6!(1, 1)) / totalGames%; statsZ1!(15)
    End If

    I$ = GetKeyPress$
    Cls

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
            Print Using "      ###.#    ##    "; K2!(I, I1, 7) / totalGames%; K2!(I, I1, 8);
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
            Print Using "      ###.#    ##    "; K2!(I, I1, 4) / totalGames%; K2!(I, I1, 5);
            Print Using "###"; LC!(I1);: Print LC$(I1, 0); Tab(64); LC$(I1, 1)
        End If

    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);

    Print Using "####  ####   "; K!(0, 4); K!(0, 6);
    If K!(0, 4) = 0 Then Print "  --"; Else Print Using "##.#"; K!(0, 6) / K!(0, 4);
    Print Using "      ###.#    ##"; K!(0, 6) / totalGames%; statsZ!(17)

    Print "OPPONENT TOTALS"; Tab(18);
    Print Using "####  ####   "; K!(1, 4); K!(1, 6);
    If K!(1, 4) = 0 Then Print "  --"; Else Print Using "##.#"; K!(1, 6) / K!(1, 4);
    Print Using "      ###.#    ##"; K!(1, 6) / totalGames%; statsZ1!(17)

    I$ = GetKeyPress$
    Cls

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
            Print Using " ###.# ### ### ###.#     "; K2!(I, I1, 12) / totalGames%; K2!(I, I1, 13); K2!(I, I1, 11); K2!(I, I1, 11) / K2!(I, I1, 9) * 100;

            'Eff Rating
            RT! = QBRating!(I, I1, statFormat)
            Print Using "###.#"; RT!

            'PRINT TAB(57); USING "###"; LP!(I1); : PRINT statsLP$(I1,0)
        End If

    Next I1

    Print

    Color 14
    'Team Totals
    Print teamName$; Tab(18);

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
    Print Using " ###.# ### ### ###.#     "; K!(0, 6) / totalGames%; statsZ!(17); K!(0, 5);

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
    Print Using " ###.# ### ### ###.#     "; K!(1, 6) / totalGames%; statsZ1!(17); K!(1, 5);

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
            Print Using "      ###.#    ##    "; K2!(I, I1, 15) / totalGames%; K2!(I, I1, 16);
            Print Using "###"; LK!(I1);: Print statsLK$(I1, 0); Tab(64); statsLK$(I1, 1)
        End If
    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);
    If statsZ!(22) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; statsZ!(22); statsZ!(23); statsZ!(23) / statsZ!(22); statsZ!(23) / totalGames%; statsZ!(18)

    Print "OPPONENT TOTALS"; Tab(18);
    If statsZ1!(22) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; statsZ1!(22); statsZ1!(23); statsZ1!(23) / statsZ1!(22); statsZ1!(23) / totalGames%; statsZ1!(18)
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
            Print Using "      ###.#    ##    "; K2!(I, I1, 15) / totalGames%; K2!(I, I1, 16);
            Print Using "###"; PK!(I1 - 3);: Print statsPK$(I1 - 3, 0); Tab(64); statsPK$(I1 - 3, 1)
        End If
    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);
    If statsZ!(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); statsZ!(25) / totalGames%; statsZ!(19)

    Print "OPPONENT TOTALS"; Tab(18);
    If statsZ1!(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); statsZ1!(25) / totalGames%; statsZ1!(19)

    I$ = GetKeyPress$
    Cls

    Call TITLE_STATS

    I = 0
    Locate 3, 1
    Color 12
    Print "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Color 15
    Print lookyA6$(I, 0); Tab(18); Using "####  ####    "; K!(0, 7); K!(0, 8);
    If K!(0, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(0, 8) / K!(0, 7);
    Print Using "    ##.#      ##     ##      ##"; K!(0, 7) / totalGames%; statsZ!(31); statsZ!(30); statsZ!(28)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; K!(1, 7); K!(1, 8);
    If K!(1, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(1, 8) / K!(1, 7);
    Print Using "    ##.#      ##     ##"; K!(1, 7) / totalGames%; statsZ1!(31); statsZ1!(30)

    Print
    Print
    Print
    Color 12
    Print "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Color 15
    Print teamName$; Tab(18); Using "####  ####    "; K!(0, 7); K!(0, 8);
    If K!(0, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(0, 8) / K!(0, 7);
    Print Using "    ###       ####        ##.#     ##.#"; statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); (K!(0, 8) - statsZ1!(25)) / K!(0, 7)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; K!(1, 7); K!(1, 8);
    If K!(1, 7) = 0 Then Print "  --"; Else Print Using "##.#"; K!(1, 8) / K!(1, 7);
    Print Using "    ###       ####        ##.#     ##.#"; statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); (K!(1, 8) - statsZ!(25)) / K!(1, 7)

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
    I$ = GetKeyPress$
    Cls

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
            Print Using "      ###.#    ##    "; GI!(I, I1, 1) / totalGames%; GI!(I, I1, 2);

            'Long Ret / Against
            Print Using "###"; LI!(I1);: Print statsLI$(I1, 0); Tab(64); statsLI$(I1, 1)

        End If

    Next I1

    I$ = GetKeyPress$
    Cls

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
Sub StatsToPrinter (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared AF!(), AM!(), CRD!(), GI!(), GS!()
    Shared K!(), K2!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared PK!(), W6!(), statsZ!(), statsZ1!(), statsZ2!()

    Shared statsO%(), statsT%(), lookyP%()

    Shared lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared lookyDI$(), lookyDS$(), statsH$()
    Shared LC$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared compO$(), statsPK$(), gameSITE$()

    Color 0, 15

    Open "REPORT" For Output As #2
    Print #2, "GAME-BY-GAME REPORT FOR "; teamName$
    Print #2,
    Print #2, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"

    For I = 1 To totalGames%

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
    Print #2, teamName$; Tab(30); Using " ###  ###  ###  ###   ###"; statsZ!(32); statsZ!(33); statsZ!(34); statsZ!(35); statsZ!(32) + statsZ!(33) + statsZ!(34) + statsZ!(35) + statsZ!(36)
    Print #2, "OPPONENTS"; Tab(30); Using " ###  ###  ###  ###   ###"; statsZ1!(32); statsZ1!(33); statsZ1!(34); statsZ1!(35); statsZ1!(32) + statsZ1!(33) + statsZ1!(34) + statsZ1!(35) + statsZ!(36)
    Print #2,

    '-- Would totalGames% ever be 0?
    Print #2, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Print #2, teamName$; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsZ!(32) / totalGames%; statsZ!(33) / totalGames%; statsZ!(34) / totalGames%; statsZ!(35) / totalGames%; (statsZ!(32) + statsZ!(33) + statsZ!(34) + statsZ!(35) + statsZ!(36)) / totalGames%
    Print #2, "OPPONENTS"; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsZ1!(32) / totalGames%; statsZ1!(33) / totalGames%; statsZ1!(34) / totalGames%; statsZ1!(35) / totalGames%; (statsZ1!(32) + statsZ1!(33) + statsZ1!(34) + statsZ1!(35) + statsZ!(36)) / totalGames%
    Print #2,

    HD = 0: AD = 0: ND = 0: TD = 0
    HT = 0: CT = 0: NT = 0: TT = 0
    HA = 0: AA = 0: NA = 0: TA = 0

    For I = 1 To totalGames%
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

    Print #2, "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

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

    If statFormat = 0 Then Print #2, Using "###.#"; K!(0, 2) / totalGames%; Else Print #2, Using "###.#"; (K!(0, 2) + W6!(0, 1)) / totalGames%;
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "###.#"; K!(1, 2) / totalGames% Else Print #2, Using "###.#"; (K!(1, 2) + W6!(1, 1)) / totalGames%

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

    Print #2, "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; K!(0, 6) / totalGames%;
    Print #2, Tab(64); Using "###.#"; K!(1, 6) / totalGames%
    Print #2,

    Print #2, "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then Print #2, Using "####"; K!(0, 1) + K!(0, 3); Else Print #2, Using "####"; K!(0, 1) - W6!(0, 0) + K!(0, 3);
    Print #2, Tab(65);
    If statFormat = 0 Then Print #2, Using "####"; K!(1, 1) + K!(1, 3) Else Print #2, Using "####"; K!(1, 1) - W6!(1, 0) + K!(1, 3)

    Print #2, "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(0, 1) + K!(0, 3)) / totalGames%; Else Print #2, Using "###.#"; (K!(0, 1) - W6!(0, 0) + K!(0, 3)) / totalGames%;
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(1, 1) + K!(1, 3)) / totalGames% Else Print #2, Using "###.#"; (K!(1, 1) - W6!(1, 0) + K!(1, 3)) / totalGames%

    Print #2, "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then Print #2, Using "####"; K!(0, 2) + K!(0, 6); Else Print #2, Using "####"; K!(0, 2) + W6!(0, 1) + K!(0, 6);
    Print #2, Tab(65);
    If statFormat = 0 Then Print #2, Using "####"; K!(1, 2) + K!(1, 6) Else Print #2, Using "####"; K!(1, 2) + W6!(1, 1) + K!(1, 6)

    Print #2, "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then Print #2, Using "##.#"; (K!(0, 2) + K!(0, 6)) / (K!(0, 1) + K!(0, 3)); Else Print #2, Using "##.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / (K!(0, 1) - W6!(0, 0) + K!(0, 3));
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "##.#"; (K!(1, 2) + K!(1, 6)) / (K!(1, 1) + K!(1, 3)) Else Print #2, Using "##.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / (K!(1, 1) - W6!(1, 0) + K!(1, 3))

    Print #2, "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(0, 2) + K!(0, 6)) / totalGames%; Else Print #2, Using "###.#"; (K!(0, 2) + W6!(0, 1) + K!(0, 6)) / totalGames%;
    Print #2, Tab(64);
    If statFormat = 0 Then Print #2, Using "###.#"; (K!(1, 2) + K!(1, 6)) / totalGames% Else Print #2, Using "###.#"; (K!(1, 2) + W6!(1, 1) + K!(1, 6)) / totalGames%

    Print #2,
    Print #2, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print #2, Using "###"; statsZ!(22);: Print #2, "/";: Print #2, Using "####"; statsZ!(23);
    Print #2, Tab(61); Using "###"; statsZ1!(22);: Print #2, "/";: Print #2, Using "####"; statsZ1!(23)

    Print #2, "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsZ!(22) <> 0 Then Print #2, Using "##.#"; statsZ!(23) / statsZ!(22);
    If statsZ1!(22) <> 0 Then
        Print #2, Tab(65); Using "##.#"; statsZ1!(23) / statsZ1!(22)
    Else
        Print #2,
    End If

    Print #2, "KICKOFF RETURNS PER GAME"; Tab(45);
    Print #2, Using "##.#"; statsZ!(22) / totalGames%;
    Print #2, Tab(65); Using "##.#"; statsZ1!(22) / totalGames%

    Print #2, "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print #2, Using "###"; statsZ!(24);: Print #2, "/";: Print #2, Using "####"; statsZ!(25);
    Print #2, Tab(61); Using "###"; statsZ1!(24);: Print #2, "/";: Print #2, Using "####"; statsZ1!(25)

    Print #2, "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsZ!(24) <> 0 Then Print #2, Using "##.#"; statsZ!(25) / statsZ!(24);
    If statsZ1!(24) <> 0 Then
        Print #2, Tab(65); Using "##.#"; statsZ1!(25) / statsZ1!(24)
    Else
        Print #2,
    End If

    Print #2, "PUNT RETURNS PER GAME"; Tab(45);
    Print #2, Using "##.#"; statsZ!(24) / totalGames%;
    Print #2, Tab(65); Using "##.#"; statsZ1!(24) / totalGames%

    Print #2, "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print #2, Using "###"; statsZ!(26);: Print #2, "/";: Print #2, Using "####"; statsZ!(27);
    Print #2, Tab(61); Using "###"; statsZ1!(26);: Print #2, "/";: Print #2, Using "####"; statsZ1!(27)

    Print #2, "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsZ!(26) <> 0 Then Print #2, Using "##.#"; statsZ!(27) / statsZ!(26);
    If statsZ1!(26) <> 0 Then
        Print #2, Tab(65); Using "##.#"; statsZ1!(27) / statsZ1!(26)
    Else
        Print #2,
    End If

    Print #2, "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print #2, Using "##.#"; statsZ!(26) / totalGames%;
    Print #2, Tab(65); Using "##.#"; statsZ1!(26) / totalGames%

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
    Print #2, Using "##.#"; K!(0, 7) / totalGames%;
    Print #2, Tab(65); Using "##.#"; K!(1, 7) / totalGames%
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
    Print #2, Using "##.#"; K!(0, 11) / totalGames%;: Print #2, "/";: Print #2, Using "##.#"; K!(0, 12) / totalGames%;
    Print #2, Tab(60); Using "##.#"; K!(1, 11) / totalGames%;: Print #2, "/";: Print #2, Using "##.#"; K!(1, 12) / totalGames%

    Print #2, "SAFETIES"; Tab(45);
    Print #2, Using "####"; statsZ!(21);
    Print #2, Tab(65); Using "####"; statsZ1!(21)

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
            Print #2, Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 0); K2!(I, I1, 1); K2!(I, I1, 1) / K2!(I, I1, 0); K2!(I, I1, 1) / totalGames%; K2!(I, I1, 2);
            If I1 > 9 Then
                Print #2,
            Else
                Print #2, Using "###"; LR!(I1);
                Print #2, LR$(I1, 0); Tab(64); LR$(I1, 1)
            End If
        End If
    Next I1

    Print #2, teamName$; Tab(18);
    If statFormat = 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(0, 1); K!(0, 2); K!(0, 2) / K!(0, 1); K!(0, 2) / totalGames%; statsZ!(15)
    If statFormat <> 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(0, 1) - W6!(0, 0); K!(0, 2) + W6!(0, 1); (K!(0, 2) + W6!(0, 1)) / (K!(0, 1) - W6!(0, 0)); (K!(0, 2) + W6!(0, 1)) / totalGames%; statsZ!(15)

    Print #2, "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(1, 1); K!(1, 2); K!(1, 2) / K!(1, 1); K!(1, 2) / totalGames%; statsZ1!(15)
    If statFormat <> 0 Then Print #2, Using "####  ####   ##.#      ###.#    ##"; K!(1, 1) - W6!(1, 0); K!(1, 2) + W6!(1, 1); (K!(1, 2) + W6!(1, 1)) / (K!(1, 1) - W6!(1, 0)); (K!(1, 2) + W6!(1, 1)) / totalGames%; statsZ1!(15)

    Print #2,
    Print #2, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5
        If K2!(I, I1, 6) <> 0 Then
            Print #2, lookyA2$(I, I1); Tab(18);
            Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 6); K2!(I, I1, 7); K2!(I, I1, 7) / K2!(I, I1, 6); K2!(I, I1, 7) / totalGames%; K2!(I, I1, 8);
            Print #2, Using "###"; LC!(I1 + 10);
            Print #2, LC$(I1 + 10, 0); Tab(64); LC$(I1 + 10, 1)
        End If
    Next I1

    For I1 = 0 To 9
        If K2!(I, I1, 3) <> 0 Then
            Print #2, lookyA1$(I, I1); Tab(18);
            Print #2, Using "####  ####   ##.#      ###.#    ##    "; K2!(I, I1, 3); K2!(I, I1, 4); K2!(I, I1, 4) / K2!(I, I1, 3); K2!(I, I1, 4) / totalGames%; K2!(I, I1, 5);
            Print #2, Using "###"; LC!(I1);
            Print #2, LC$(I1, 0); Tab(64); LC$(I1, 1)
        End If
    Next I1

    Print #2, teamName$; Tab(18);
    Print #2, Using "####  ####   "; K!(0, 4); K!(0, 6);

    If K!(0, 4) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(0, 6) / K!(0, 4);

    Print #2, Using "      ###.#    ##"; K!(0, 6) / totalGames%; statsZ!(17)
    Print #2, "OPPONENT TOTALS"; Tab(18);
    Print #2, Using "####  ####   "; K!(1, 4); K!(1, 6);

    If K!(1, 4) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(1, 6) / K!(1, 4);

    Print #2, Using "      ###.#    ##"; K!(1, 6) / totalGames%; statsZ1!(17)

    Print #2,
    Print #2, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3
        If K2!(I, I1, 9) <> 0 Then
            Print #2, lookyA3$(I, I1); Tab(18); Using "####  ### ###.# #### ##.#"; K2!(I, I1, 9); K2!(I, I1, 10); K2!(I, I1, 10) / K2!(I, I1, 9) * 100; K2!(I, I1, 12); K2!(I, I1, 12) / K2!(I, I1, 9);
            If K2!(I, I1, 10) = 0 Then Print #2, "   --"; Else Print #2, Using " ##.#"; K2!(I, I1, 12) / K2!(I, I1, 10);
            Print #2, Using " ###.# ### ### ###.#     "; K2!(I, I1, 12) / totalGames%; K2!(I, I1, 13); K2!(I, I1, 11); K2!(I, I1, 11) / K2!(I, I1, 9) * 100;
            RT! = QBRating!(I, I1, statFormat)
            Print #2, Using "###.#"; RT!
        End If
    Next I1

    Print #2, teamName$; Tab(18);
    Print #2, Using "####  ### "; K!(0, 3); K!(0, 4);

    If K!(0, 3) = 0 Then Print #2, "   --"; Else Print #2, Using "###.#"; K!(0, 4) / K!(0, 3) * 100;

    Print #2, Using "#### "; K!(0, 6);

    If K!(0, 3) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(0, 6) / K!(0, 3);
    If K!(0, 4) = 0 Then Print #2, "   --"; Else Print #2, Using " ##.#"; K!(0, 6) / K!(0, 4);

    Print #2, Using " ###.# ### ### "; K!(0, 6) / totalGames%; statsZ!(17); K!(0, 5);

    If K!(0, 3) = 0 Then Print #2, "   --"; Else Print #2, Using "###.#     "; K!(0, 5) / K!(0, 3) * 100

    Print #2, "OPPONENT TOTALS"; Tab(18);
    Print #2, Using "####  ### "; K!(1, 3); K!(1, 4);

    If K!(1, 3) = 0 Then Print #2, "   --"; Else Print #2, Using "###.#"; K!(1, 4) / K!(1, 3) * 100;

    Print #2, Using "#### "; K!(1, 6);

    If K!(1, 3) = 0 Then Print #2, "  --"; Else Print #2, Using "##.#"; K!(1, 6) / K!(1, 3);
    If K!(1, 4) = 0 Then Print #2, "  --"; Else Print #2, Using " ##.#"; K!(1, 6) / K!(1, 4);

    Print #2, Using " ###.# ### ### "; K!(1, 6) / totalGames%; statsZ1!(17); K!(1, 5);

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
            Print #2, Using "      ###.#    ##    "; K2!(I, I1, 15) / totalGames%; K2!(I, I1, 16);
            Print #2, Using "###"; LK!(I1);: Print #2, statsLK$(I1, 0); Tab(64); statsLK$(I1, 1)
        End If
    Next I1

    Print #2, teamName$; Tab(18);

    If statsZ!(22) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; statsZ!(22); statsZ!(23); statsZ!(23) / statsZ!(22); statsZ!(23) / totalGames%; statsZ!(18)

    Print #2, "OPPONENT TOTALS"; Tab(18);

    If statsZ1!(22) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; statsZ1!(22); statsZ1!(23); statsZ1!(23) / statsZ1!(22); statsZ1!(23) / totalGames%; statsZ1!(18)

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
            Print #2, Using "      ###.#    ##    "; K2!(I, I1, 15) / totalGames%; K2!(I, I1, 16);
            Print #2, Using "###"; PK!(I1 - 3);: Print #2, statsPK$(I1 - 3, 0); Tab(64); statsPK$(I1 - 3, 1)
        End If
    Next I1

    Print #2, teamName$; Tab(18);

    If statsZ!(24) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); statsZ!(25) / totalGames%; statsZ!(19)

    Print #2, "OPPONENT TOTALS"; Tab(18);

    If statsZ1!(24) = 0 Then Print #2, "   0     0   --.-        -.-     0" Else Print #2, Using "####  ####   ##.#      ###.#    ##"; statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); statsZ1!(25) / totalGames%; statsZ1!(19)

    Print #2,

    I = 0

    Print #2, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Print #2, lookyA6$(I, 0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; K!(0, 7); K!(0, 8); K!(0, 8) / K!(0, 7); K!(0, 7) / totalGames%; statsZ!(31); statsZ!(30); statsZ!(28)
    Print #2, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; K!(1, 7); K!(1, 8); K!(1, 8) / K!(1, 7); K!(1, 7) / totalGames%; statsZ1!(31); statsZ1!(30)
    Print #2,
    Print #2,
    Print #2,
    Print #2, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Print #2, teamName$; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K!(0, 7); K!(0, 8); K!(0, 8) / K!(0, 7); statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); (K!(0, 8) - statsZ1!(25)) / K!(0, 7)
    Print #2, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; K!(1, 7); K!(1, 8); K!(1, 8) / K!(1, 7); statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); (K!(1, 8) - statsZ!(25)) / K!(1, 7)
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
            Print #2, Using "####  ####   ##.#      ###.#    ##    "; GI!(I, I1, 0); GI!(I, I1, 1); GI!(I, I1, 1) / GI!(I, I1, 0); GI!(I, I1, 1) / totalGames%; GI!(I, I1, 2);
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


'----------------------------------------
'      GetDisk_LOOKY Subroutine
'----------------------------------------
'...explanation...
Sub GetDisk_LOOKY (numberTeams, yearNumber$, A$)

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
                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    If Not _FileExists(diskPaths$(0) + "FOOT." + yearNumber$) Then
                        BO% = 1
                        Locate 10, 8
                        Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                    Else
                        Open diskPaths$(0) + "FOOT." + yearNumber$ For Random As #1 Len = 934
                        fileLength& = LOF(1)
                        Field #1, 15 As Q$(0), 640 As X$

                        For I1 = 1 To fileLength& / 934
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

            End If

            If BO% = 1 Then
                BO% = 0
            Else
                A$ = RTrim$(Q$(1))
                Locate 2, 33
                Color statsPA%(2), statsPA%(1): Print A$

                Do
                    Color 15, 4
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
        teamYear$ = RTrim$(yearNumber$)

        If Not _FileExists(diskPaths$(0) + "FOOT." + teamYear$) Then
            BO% = 1
            Locate 10, 8
            Call CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else
            Open diskPaths$(0) + "FOOT." + teamYear$ For Random As #1 Len = 934

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

