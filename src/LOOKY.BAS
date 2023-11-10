'----------------------------------------
'            LOOKY Subroutine
'----------------------------------------
'This subroutine guides the user through
'choosing a team file and then calls other
'routines to load and view the stats
Sub LOOKY ()

    _Title "3-in-1 Football - View Stats"

    Do

        Color 15, 0
        Cls

        Call MenuBox(8, 8, 61, 6, 15, 1)

        Locate 10, 15

        Locate , 30: Color 3: Print "VIEW/PRINT STATS IN:"
        Locate , 30: Color 15: Print "C";
        Color 7: Print "OLLEGE FORMAT"
        Locate , 30: Color 15: Print "P";
        Color 7: Print "RO FORMAT"
        Locate , 30: Color 15: Print "E";
        Color 7: Print "XIT TO MAIN MENU"

        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "C": statFormat = 0
            Case "P": statFormat = 1
            Case "E", Chr$(27): '
        End Select

        If UCase$(I$) = "C" Or UCase$(I$) = "P" Then

            'Call GetDisk_LOOKY(numberTeams, YN$, A$)
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                Call LoadTeamStats(statFile$, team$, totalGames%, teamPower, SS)
                Call ViewStatSummary(team$, totalGames%, statFormat, teamPower, SS)

            End If

        End If

    Loop Until UCase$(I$) = "E" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, totalGames%, teamPower, SS)

    Shared AF!(), AM!(), statsGI%(), statsGS%()
    Shared CRD!()
    Shared teamStats!(), K2!(), statsPK!(), sackStats!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsZ!(), statsZ1!(), statsZ2!()

    Shared statsO%(), compT%(), lookyP%()
    Shared lookyP%(), teamRatings%()

    Shared statsH$(), statsO$(), gameSite$()
    Shared intName$(), sackName$()
    Shared statsLC$(), statsLI$(), statsLK$(), statsLP$(), statsLR$()
    Shared lookyRBacks$(), lookyWdRec$(), lookyQBacks$(), lookyKickRet$(), lookyPRet$(), lookyPunter$(), lookyKicker$()
    Shared statsPK$()

    Dim statsLF$(1)

    Dim compAP%(1, 120)

    Open diskPaths$(1) + statFile$ + "S" For Input As #1

    Input #1, totalGames%

    'Opponent Score
    For I = 1 To NUM_STATRECORDS: Input #1, statsO%(I): Next I

    'Opponent
    For I = 1 To NUM_STATRECORDS: Input #1, statsO$(I): Next I

    'Team Score
    For I = 1 To NUM_STATRECORDS: Input #1, compT%(I): Next I

    'Home / Away
    For I = 1 To NUM_STATRECORDS: Input #1, statsH$(I): Next I

    For I = 0 To 1
        For I1 = 1 To NUM_STATRECORDS
            ' ???
            Input #1, compAP%(I, I1)
        Next
    Next

    'Attendance
    For I = 1 To NUM_STATRECORDS: Input #1, CRD!(I): Next I

    'Stadium
    For I = 1 To NUM_STATRECORDS: Input #1, gameSite$(I): Next I

    Close #1

    'STRENGTH OF SCHEDULE
    SS = 0

    For I = 1 To totalGames%
        SS = compAP%(1, I) + SS
    Next

    SS = SS / totalGames%

    'INDIVIDUAL GAME POWER RATINGS
    teamPower = 0

    For I = 1 To totalGames%
        pwrRating = statsO%(I) - compT%(I) + compAP%(1, I)
        If Left$(statsH$(I), 1) = "H" Then pwrRating = pwrRating - 3
        If Left$(statsH$(I), 1) = "A" Then pwrRating = pwrRating + 3
        teamPower = teamPower + pwrRating
    Next

    GL = 0

    If totalGames% <= 4 Then
        GL = 5 - totalGames%
        teamPower = teamPower + GL * compAP%(0, 1)
    End If

    teamPower = teamPower / (totalGames% + GL)

    L% = 0
    W% = 0
    tiedGames = 0

    D% = 0
    lookyP%(0) = 0
    lookyP%(1) = 0

    'Load player stats
    Open diskPaths$(1) + statFile$ For Input As #2

    'We are passed teamName$ as a variable from the disk routine
    'However the format of the files for football is such that
    'it's also contained here.
    'To be consistent with other games, it needs evaluated
    'if this belongs here or somewhere else.
    Input #2, teamName$

    For I = 0 To 9: Input #2, lookyRBacks$(I): Next I
    For I = 0 To 5: Input #2, lookyWdRec$(I): Next I
    For I = 0 To 3: Input #2, lookyQBacks$(I): Next I
    For I = 0 To 2: Input #2, lookyKickRet$(I): Next I
    For I = 0 To 2: Input #2, lookyPRet$(I): Next I

    Input #2, lookyPunter$(0)

    For I = 0 To 1:
        Input #2, lookyKicker$(I)
    Next I

    For J = 0 To 13
        For K = 0 To 17:
            Input #2, statsZ2!(J, K)
        Next K
    Next J

    For L = 0 To 38:
        Input #2, statsZ!(L)
    Next L

    For L = 0 To 38:
        Input #2, statsZ1!(L)
    Next L

    For J = 0 To 9:
        Input #2, intName$(J)
        For J1 = 0 To 2:
            Input #2, statsGI%(J, J1)
        Next
    Next

    For J = 0 To 14:
        Input #2, sackName$(J)
        For J1 = 0 To 1:
            Input #2, statsGS%(J, J1)
        Next
    Next

    For I = 0 To 9:
        Input #2, statsLR!(I)
        Input #2, statsLR$(I, 0)
        Input #2, statsLR$(I, 1)
    Next

    For I = 0 To 15:
        Input #2, statsLC!(I)
        Input #2, statsLC$(I, 0)
        Input #2, statsLC$(I, 1)
    Next

    For I = 0 To 3:
        Input #2, statsLP!(I)
        Input #2, statsLP$(I, 0)
        Input #2, statsLP$(I, 1)
    Next

    For I = 0 To 2:
        Input #2, statsLK!(I)
        Input #2, statsLK$(I, 0)
        Input #2, statsLK$(I, 1)
    Next

    For I = 0 To 2:
        Input #2, statsPK!(I)
        Input #2, statsPK$(I, 0)
        Input #2, statsPK$(I, 1)
    Next

    For I = 0 To 1:
        Input #2, statsLF!(I)
        Input #2, statsLF$(I)
        For I1 = 0 To 4:
            Input #2, AM!(I, I1)
            Input #2, AF!(I, I1)
        Next
    Next

    For I = 0 To 9:
        Input #2, statsLI!(I)
        Input #2, statsLI$(I, 0)
        Input #2, statsLI$(I, 1)
    Next

    Close #2

    For J = 0 To 13
        For K = 0 To 17:
            K2!(J, K) = statsZ2!(J, K)
        Next K
    Next J

    For M = 0 To 12:
        teamStats!(0, M) = statsZ!(M)
    Next M

    For M = 0 To 12:
        teamStats!(1, M) = statsZ1!(M)
    Next M

    sackStats!(0, 0) = statsZ!(13)
    sackStats!(0, 1) = statsZ!(14)
    sackStats!(1, 0) = statsZ1!(13)
    sackStats!(1, 1) = statsZ1!(14)

End Sub


'----------------------------------------
'       ViewStatSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatSummary (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared teamRatings%()

    Do

        DM% = 0

        Color 15, 0
        Cls

        Call TITLE_STATS

        Call MenuBox(8, 8, 61, 4, 15, 1)

        Do

            Locate 10, 32
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";
            Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";
            Color 7: Print "RINTER"

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

        Call MenuBox(6, 8, 61, 1, 15, 1)

        Do
            Locate 7
            U$ = "DISPLAY STATS AGAIN? (Y/N)"
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
'This subroutine formats and outputs the
'selected team's stats to the display
Sub StatsToScreen (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared AF!(), AM!(), CRD!(), statsGI%(), statsGS%()
    Shared teamStats!(), K2!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsPK!(), sackStats!(), statsZ!(), statsZ1!(), statsZ2!()

    Shared statsO%(), teamRatings%(), compT%(), lookyP%()

    Shared lookyRBacks$(), lookyWdRec$(), lookyQBacks$(), lookyKickRet$(), lookyPRet$(), lookyPunter$(), lookyKicker$()
    Shared intName$(), sackName$(), statsH$()
    Shared statsLC$(), statsLI$(), statsLK$(), statsLP$(), statsLR$()
    Shared statsO$(), statsPK$(), gameSite$()

    Color 15, 0
    Cls

    Call TITLE_STATS

    Locate 1, 1
    Color 14, 1
    Print Tab(81);

    header$ = "GAME-BY-GAME REPORT FOR " + teamName$
    Xcoord = Len(header$)
    Xcoord = Int((80 - Xcoord) / 2)
    Locate , Xcoord

    Print "GAME-BY-GAME REPORT FOR ";
    Color teamRatings%(2), teamRatings%(1)
    Print teamName$

    Color 14, 1
    Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD    ";
    Print
    Color 15, 0

    For I = 1 To totalGames%

        Color 7

        If I / 2 = Int(I / 2) Then Color 15

        Print statsO$(I);
        Locate , 19: Print Using "###"; statsO%(I);: Print "-";: Print Using "###   "; compT%(I);

        If statsO%(I) > compT%(I) Then W% = W% + 1: Print "W";
        If statsO%(I) < compT%(I) Then L% = L% + 1: Print "L";
        If statsO%(I) = compT%(I) Then tiedGames = tiedGames + 1: Print "T";

        Locate , 31: Print Using "####,###   "; CRD!(I);
        Print statsH$(I); "  "; Left$(gameSite$(I), 24);

        Locate , 69
        Print "(";
        Print Using "##"; W%;: Print "-";: Print Using "##"; L%;: Print "-";: Print Using "##"; tiedGames;
        Print ")"

        lookyP%(0) = lookyP%(0) + statsO%(I): lookyP%(1) = lookyP%(1) + compT%(I)

        If (I Mod 20 = 0) Then

            Locate 24, 1
            Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
            I$ = GetKeyPress$
            ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***
            Cls
            Color , 0
            Locate 1, 1: Print Tab(80);
            Locate , Xcoord

            Color 14
            Print "GAME-BY-GAME REPORT FOR ";
            Color teamRatings%(2), teamRatings%(1)
            Print teamName$

            Color 14
            Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD   "
            Color 15, 0

        End If

    Next I

    If statFormat = 0 Then
        Locate 23, 31
        Print Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS;
    End If

    lookyP%(0) = 0
    lookyP%(1) = 0
    W% = 0
    L% = 0
    tiedGames = 0

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
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

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 2
    Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Color 7
    Print "FIRST DOWNS"; Tab(45); Using "####"; teamStats!(0, 0);: Print Tab(65); Using "####"; teamStats!(1, 0)
    Print "RUSHING ATTEMPTS"; Tab(45);
    'Team
    If statFormat = 0 Then Print Using "####"; teamStats!(0, 1); Else Print Using "####"; teamStats!(0, 1) - sackStats!(0, 0);
    Print Tab(65);
    'Opponent
    If statFormat = 0 Then Print Using "####"; teamStats!(1, 1) Else Print Using "####"; teamStats!(1, 1) - sackStats!(1, 0)

    Color 15
    Print "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then Print Using "####"; teamStats!(0, 2); Else Print Using "####"; teamStats!(0, 2) + sackStats!(0, 1);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; teamStats!(1, 2) Else Print Using "####"; teamStats!(1, 2) + sackStats!(1, 1)

    Color 7
    Print "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And teamStats!(0, 1) <> 0 Then Print Using "##.#"; teamStats!(0, 2) / teamStats!(0, 1);
    If statFormat <> 0 And teamStats!(0, 1) - sackStats!(0, 0) <> 0 Then Print Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0));
    Print Tab(65);
    If statFormat = 0 And teamStats!(1, 1) <> 0 Then Print Using "##.#"; teamStats!(1, 2) / teamStats!(1, 1)
    If statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) <> 0 Then Print Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0))
    If statFormat = 0 And teamStats!(1, 1) = 0 Or statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) = 0 Then Print

    Print "RUSHING YARDS PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; teamStats!(0, 2) / totalGames%; Else Print Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; teamStats!(1, 2) / totalGames% Else Print Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%

    Print
    Print "PASSES ATTEMPTED"; Tab(45); Using "####"; teamStats!(0, 3);
    Print Tab(65); Using "####"; teamStats!(1, 3)

    Print "PASSES COMPLETED"; Tab(45); Using "####"; teamStats!(0, 4);
    Print Tab(65); Using "####"; teamStats!(1, 4)

    Print "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; teamStats!(0, 5);
    Print Tab(65); Using "####"; teamStats!(1, 5)

    Print "PASS COMPLETION PERCENTAGE"; Tab(44);
    If teamStats!(0, 3) <> 0 Then Print Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;
    If teamStats!(1, 3) <> 0 Then Print Tab(64); Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100
    If teamStats!(1, 3) = 0 Then Print

    Color 15
    Print "NET YARDS PASSING"; Tab(45); Using "####"; teamStats!(0, 6);
    Print Tab(65); Using "####"; teamStats!(1, 6)

    Color 7
    Print "YARDS PER PASS ATTEMPT"; Tab(45);
    If teamStats!(0, 3) <> 0 Then Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    If teamStats!(1, 3) <> 0 Then Print Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3)
    If teamStats!(1, 3) = 0 Then Print

    Print "YARDS PER PASS COMPLETION"; Tab(45);
    If teamStats!(0, 4) <> 0 Then Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    If teamStats!(1, 4) <> 0 Then Print Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4)
    If teamStats!(1, 4) = 0 Then Print

    Print "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; teamStats!(0, 6) / totalGames%;
    Print Tab(64); Using "###.#"; teamStats!(1, 6) / totalGames%
    Print

    Print "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then Print Using "####"; teamStats!(0, 1) + teamStats!(0, 3); Else Print Using "####"; teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; teamStats!(1, 1) + teamStats!(1, 3) Else Print Using "####"; teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)

    Print "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; (teamStats!(0, 1) + teamStats!(0, 3)) / totalGames%; Else Print Using "###.#"; (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3)) / totalGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; (teamStats!(1, 1) + teamStats!(1, 3)) / totalGames% Else Print Using "###.#"; (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)) / totalGames%

    Color 15
    Print "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then Print Using "####"; teamStats!(0, 2) + teamStats!(0, 6); Else Print Using "####"; teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6);
    Print Tab(65);
    If statFormat = 0 Then Print Using "####"; teamStats!(1, 2) + teamStats!(1, 6) Else Print Using "####"; teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)

    Color 7
    Print "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then Print Using "##.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / (teamStats!(0, 1) + teamStats!(0, 3)); Else Print Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3));
    Print Tab(64);
    If statFormat = 0 Then Print Using "##.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / (teamStats!(1, 1) + teamStats!(1, 3)) Else Print Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3))

    Print "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then Print Using "###.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / totalGames%; Else Print Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / totalGames%;
    Print Tab(64);
    If statFormat = 0 Then Print Using "###.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / totalGames% Else Print Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / totalGames%

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
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
    Print Using "###"; teamStats!(0, 7);: Print "/";: Print Using "####"; teamStats!(0, 8);
    Print Tab(61); Using "###"; teamStats!(1, 7);: Print "/";: Print Using "####"; teamStats!(1, 8)

    Print "AVERAGE YARDS PER PUNT"; Tab(45);
    If teamStats!(0, 7) <> 0 Then Print Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    If teamStats!(1, 7) <> 0 Then Print Tab(65); Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7)
    If teamStats!(1, 7) = 0 Then Print

    Print "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print Using "##.#"; teamStats!(0, 7) / totalGames%;: Print Tab(65); Using "##.#"; teamStats!(1, 7) / totalGames%
    Print

    Print "FUMBLES / FUMBLES LOST"; Tab(41);
    Print Using "###"; teamStats!(0, 9);: Print "/";: Print Using "####"; teamStats!(0, 10);
    Print Tab(61); Using "###"; teamStats!(1, 9);: Print "/";: Print Using "####"; teamStats!(1, 10)

    Print "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print Using "###"; teamStats!(0, 11);: Print "/";: Print Using "####"; teamStats!(0, 12);
    Print Tab(61); Using "###"; teamStats!(1, 11);: Print "/";: Print Using "####"; teamStats!(1, 12)

    Print "AVERAGE YARDS PER PENALTY"; Tab(45);
    If teamStats!(0, 11) <> 0 Then Print Using "##.#"; teamStats!(0, 12) / teamStats!(0, 11);
    If teamStats!(1, 11) <> 0 Then Print Tab(65); Using "##.#"; teamStats!(1, 12) / teamStats!(1, 11)
    If teamStats!(1, 11) = 0 Then Print

    Print "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print Using "##.#"; teamStats!(0, 11) / totalGames%;: Print "/";: Print Using "##.#"; teamStats!(0, 12) / totalGames%;
    Print Tab(60); Using "##.#"; teamStats!(1, 11) / totalGames%;: Print "/";: Print Using "##.#"; teamStats!(1, 12) / totalGames%

    Print "SAFETIES"; Tab(45);: Print Using "####"; statsZ!(21);: Print Tab(65); Using "####"; statsZ1!(21)

    Print "TIMES SACKED / YARDS LOST"; Tab(41);
    Print Using "###"; sackStats!(0, 0);: Print "/";: Print Using "####"; sackStats!(0, 1);
    Print Tab(61); Using "###"; sackStats!(1, 0);: Print "/";: Print Using "####"; sackStats!(1, 1)

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"

    If D% <> 1 Then

        For I1 = 0 To 9

            For I2 = 0 To 3

                If lookyRBacks$(I1) = lookyQBacks$(I2) Then
                    K2!(I1, 0) = K2!(I1, 0) + K2!(I2 + 10, 0)
                    K2!(I1, 1) = K2!(I1, 1) + K2!(I2 + 10, 1)
                    K2!(I1, 2) = K2!(I1, 2) + K2!(I2 + 10, 2)
                    K2!(I2 + 10, 0) = 0
                    K2!(I2 + 10, 1) = 0
                    K2!(I2 + 10, 2) = 0
                End If

            Next I2

        Next I1

    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13

        If K2!(I1, 0) <> 0 Then

            DM% = DM% + 1
            Color 7

            If DM% / 2 = Int(DM% / 2) Then Color 15
            If I1 <= 9 Then Print lookyRBacks$(I1);
            If I1 > 9 Then Print lookyQBacks$(I1 - 10);

            Print Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2!(I1, 0); K2!(I1, 1); K2!(I1, 1) / K2!(I1, 0); K2!(I1, 1) / totalGames%; K2!(I1, 2);

            If I1 > 9 Then
                Print
            Else
                Print Using "###"; statsLR!(I1);: Print statsLR$(I1, 0); Tab(64); statsLR$(I1, 1)
            End If

        End If

    Next I1

    Color 14
    Print
    Print teamName$; Tab(18);

    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1); teamStats!(0, 2); teamStats!(0, 2) / teamStats!(0, 1); teamStats!(0, 2) / totalGames%; statsZ!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1) - sackStats!(0, 0); teamStats!(0, 2) + sackStats!(0, 1); (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0)); (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%; statsZ!(15)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1); teamStats!(1, 2); teamStats!(1, 2) / teamStats!(1, 1); teamStats!(1, 2) / totalGames%; statsZ1!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1) - sackStats!(1, 0); teamStats!(1, 2) + sackStats!(1, 1); (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0)); (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%; statsZ1!(15)
    End If

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5

        If K2!(I1, 6) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print lookyWdRec$(I1); Tab(18);
            Print Using "####  ####   "; K2!(I1, 6); K2!(I1, 7);

            If K2!(I1, 6) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I1, 7) / K2!(I1, 6);

            Print Using "      ###.#    ##    "; K2!(I1, 7) / totalGames%; K2!(I1, 8);
            Print Using "###"; statsLC!(I1 + 10);: Print statsLC$(I1 + 10, 0); Tab(64); statsLC$(I1 + 10, 1)

        End If

    Next I1

    For I1 = 0 To 9

        If K2!(I1, 3) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print lookyRBacks$(I1); Tab(18);
            Print Using "####  ####   "; K2!(I1, 3); K2!(I1, 4);

            If K2!(I1, 3) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I1, 4) / K2!(I1, 3);

            Print Using "      ###.#    ##    "; K2!(I1, 4) / totalGames%; K2!(I1, 5);
            Print Using "###"; statsLC!(I1);: Print statsLC$(I1, 0); Tab(64); statsLC$(I1, 1)

        End If

    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);

    Print Using "####  ####   "; teamStats!(0, 4); teamStats!(0, 6);
    If teamStats!(0, 4) = 0 Then Print "  --"; Else Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    Print Using "      ###.#    ##"; teamStats!(0, 6) / totalGames%; statsZ!(17)

    Print "OPPONENT TOTALS"; Tab(18);
    Print Using "####  ####   "; teamStats!(1, 4); teamStats!(1, 6);
    If teamStats!(1, 4) = 0 Then Print "  --"; Else Print Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4);
    Print Using "      ###.#    ##"; teamStats!(1, 6) / totalGames%; statsZ1!(17)

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3

        If K2!(I1, 9) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            'Att / Comp / Comp % / Yds
            Print lookyQBacks$(I1); Tab(18); Using "####  ### ###.# #### "; K2!(I1, 9); K2!(I1, 10); K2!(I1, 10) / K2!(I1, 9) * 100; K2!(I1, 12);

            'Yds / Att
            If K2!(I1, 9) = 0 Then Print "   --"; Else Print Using "##.#"; K2!(I1, 12) / K2!(I1, 9);

            ' Yds / Catch
            If K2!(I1, 10) = 0 Then Print "   --"; Else Print Using " ##.#"; K2!(I1, 12) / K2!(I1, 10);

            'TD / INT / INT %
            Print Using " ###.# ### ### ###.#     "; K2!(I1, 12) / totalGames%; K2!(I1, 13); K2!(I1, 11); K2!(I1, 11) / K2!(I1, 9) * 100;

            'QB Rating
            RT! = QBRating!(I1, statFormat)
            Print Using "###.#"; RT!

        End If

    Next I1

    Print

    Color 14
    'Team Totals
    Print teamName$; Tab(18);

    'Att / Comp
    Print Using "####  ### "; teamStats!(0, 3); teamStats!(0, 4);

    'Comp %
    If teamStats!(0, 3) = 0 Then Print "   --"; Else Print Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;

    'Yds
    Print Using " #### "; teamStats!(0, 6);

    'Yds / Att
    If teamStats!(0, 3) = 0 Then Print " --"; Else Print Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);

    'Yds / Catch
    If teamStats!(0, 4) = 0 Then Print "   --"; Else Print Using " ##.#"; teamStats!(0, 6) / teamStats!(0, 4);

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; teamStats!(0, 6) / totalGames%; statsZ!(17); teamStats!(0, 5);

    'INT %
    If teamStats!(0, 3) = 0 Then Print "   --" Else Print Using "  #.#"; teamStats!(0, 5) / teamStats!(0, 3) * 100

    Print "OPPONENT TOTALS"; Tab(18);

    'Att / Comp
    Print Using "####  ### "; teamStats!(1, 3); teamStats!(1, 4);

    'Comp %
    If teamStats!(1, 3) = 0 Then Print "   --" Else Print Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100;

    'Yds
    Print Using " #### "; teamStats!(1, 6);

    'Yds / Att
    If teamStats!(1, 3) = 0 Then Print " --"; Else Print Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3);

    'Yds / Catch
    If teamStats!(1, 4) = 0 Then Print "   --"; Else Print Using " ##.#"; teamStats!(1, 6) / teamStats!(1, 4);

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; teamStats!(1, 6) / totalGames%; statsZ1!(17); teamStats!(1, 5);

    'INT %
    If teamStats!(1, 3) = 0 Then Print "   --" Else Print Using "  #.#"; teamStats!(1, 5) / teamStats!(1, 3) * 100

    Print
    Print
    Color 12

    Print "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If K2!(I1, 9) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyQBacks$(I1); Tab(22); Using "###"; statsLP!(I1);: Print statsLP$(I1, 0); Tab(32); statsLP$(I1, 1)
        End If
    Next

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If K2!(I1, 14) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print lookyKickRet$(I1); Tab(18);
            Print Using "####  ####   "; K2!(I1, 14); K2!(I1, 15);
            If K2!(I1, 14) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I1, 15) / K2!(I1, 14);
            Print Using "      ###.#    ##    "; K2!(I1, 15) / totalGames%; K2!(I1, 16);
            Print Using "###"; statsLK!(I1);: Print statsLK$(I1, 0); Tab(64); statsLK$(I1, 1)
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

        If K2!(I1, 14) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print lookyPRet$(I1 - 3); Tab(18);
            Print Using "####  ####   "; K2!(I1, 14); K2!(I1, 15);

            If K2!(I1, 14) = 0 Then Print "  --"; Else Print Using "##.#"; K2!(I1, 15) / K2!(I1, 14);

            Print Using "      ###.#    ##    "; K2!(I1, 15) / totalGames%; K2!(I1, 16);
            Print Using "###"; statsPK!(I1 - 3);: Print statsPK$(I1 - 3, 0); Tab(64); statsPK$(I1 - 3, 1)

        End If

    Next I1

    Print
    Color 14
    Print teamName$; Tab(18);
    If statsZ!(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); statsZ!(25) / totalGames%; statsZ!(19)

    Print "OPPONENT TOTALS"; Tab(18);
    If statsZ1!(24) = 0 Then Print "   0     0   --.-        -.-     0" Else Print Using "####  ####   ##.#      ###.#    ##"; statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); statsZ1!(25) / totalGames%; statsZ1!(19)

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Color 15
    Print lookyPunter$(0); Tab(18); Using "####  ####    "; teamStats!(0, 7); teamStats!(0, 8);
    If teamStats!(0, 7) = 0 Then Print "  --"; Else Print Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    Print Using "    ##.#      ##     ##      ##"; teamStats!(0, 7) / totalGames%; statsZ!(31); statsZ!(30); statsZ!(28)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; teamStats!(1, 7); teamStats!(1, 8);
    If teamStats!(1, 7) = 0 Then Print "  --"; Else Print Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7);
    Print Using "    ##.#      ##     ##"; teamStats!(1, 7) / totalGames%; statsZ1!(31); statsZ1!(30)

    Print
    Print
    Print
    Color 12
    Print "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Color 15
    Print teamName$; Tab(18); Using "####  ####    "; teamStats!(0, 7); teamStats!(0, 8);
    If teamStats!(0, 7) = 0 Then Print "  --"; Else Print Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    Print Using "    ###       ####        ##.#     ##.#"; statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); (teamStats!(0, 8) - statsZ1!(25)) / teamStats!(0, 7)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; teamStats!(1, 7); teamStats!(1, 8);
    If teamStats!(1, 7) = 0 Then Print "  --"; Else Print Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7);
    Print Using "    ###       ####        ##.#     ##.#"; statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); (teamStats!(1, 8) - statsZ!(25)) / teamStats!(1, 7)

    Print
    Print
    Print
    Color 12
    Print "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
    Color 15

    If K2!(4, 17) <> 0 And K2!(6, 17) <> 0 Then
        Print lookyKicker$(0);
        Print Tab(16);
        If K2!(6, 17) <> 0 Then Print Using "### ### ###.# "; K2!(5, 17); K2!(6, 17); K2!(5, 17) / K2!(6, 17) * 100;
        If K2!(4, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; K2!(3, 17); K2!(4, 17); K2!(3, 17) / K2!(4, 17) * 100; statsLF!(0); AM!(0, 0);
            Print "-";: Print Using "## ##"; AF!(0, 0); AM!(0, 1);
            Print "-";: Print Using "## ##"; AF!(0, 1); AM!(0, 2);
            Print "-";: Print Using "## ##"; AF!(0, 2); AM!(0, 3);
            Print "-";: Print Using "## ##"; AF!(0, 3); AM!(0, 4);
            Print "-";: Print Using "##"; AF!(0, 4)
        End If
    End If

    If K2!(8, 17) <> 0 And K2!(10, 17) <> 0 Then
        Print lookyKicker$(1); Tab(16);
        If K2!(10, 17) <> 0 Then Print Using "### ### ###.# "; K2!(9, 17); K2!(10, 17); K2!(9, 17) / K2!(10, 17) * 100;
        If K2!(8, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; K2!(7, 17); K2!(8, 17); K2!(7, 17) / K2!(8, 17) * 100; statsLF!(1); AM!(1, 0);
            Print "-";: Print Using "## ##"; AF!(1, 0); AM!(1, 1);
            Print "-";: Print Using "## ##"; AF!(1, 1); AM!(1, 2);
            Print "-";: Print Using "## ##"; AF!(1, 2); AM!(1, 3);
            Print "-";: Print Using "## ##"; AF!(1, 3); AM!(1, 4);
            Print "-";: Print Using "##"; AF!(1, 4)
        End If
    End If

    Print

    NOONE:
    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    For I1 = 0 To 9

        If statsGI%(I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print intName$(I1); Tab(18);

            Print Using "####  ####   "; statsGI%(I1, 0); statsGI%(I1, 1);

            'Yds / Ret
            If statsGI%(I1, 0) = 0 Then Print " --"; Else Print Using "##.#"; statsGI%(I1, 1) / statsGI%(I1, 0);

            'Yds / Gm, TD
            Print Using "      ###.#    ##    "; statsGI%(I1, 1) / totalGames%; statsGI%(I1, 2);

            'Long Ret / Against
            Print Using "###"; statsLI!(I1);: Print statsLI$(I1, 0); Tab(64); statsLI$(I1, 1)

        End If

    Next I1

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If statsGS%(I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print sackName$(I1); Tab(20); Using "###            ###"; statsGS%(I1, 0); statsGS%(I1, 1)
        End If
    Next

    Locate 24, 1
    Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
    I$ = GetKeyPress

End Sub

'----------------------------------------
'       StatsToPrinter Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's stats to the default printer
Sub StatsToPrinter (teamName$, totalGames%, statFormat, teamPower, SS)

    Shared AF!(), AM!(), CRD!(), statsGI%(), statsGS%()
    Shared teamStats!(), K2!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsPK!(), sackStats!(), statsZ!(), statsZ1!(), statsZ2!()

    Shared statsO%(), compT%(), lookyP%()

    Shared lookyRBacks$(), lookyWdRec$(), lookyQBacks$(), lookyKickRet$(), lookyPRet$(), lookyPunter$(), lookyKicker$()
    Shared intName$(), sackName$(), statsH$()
    Shared statsLC$(), statsLI$(), statsLK$(), statsLP$(), statsLR$()
    Shared statsO$(), statsPK$(), gameSite$()

    Color 0, 15

    Open "REPORT" For Output As #1

    Print #1, "GAME-BY-GAME REPORT FOR "; teamName$
    Print #1,
    Print #1, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"

    For I = 1 To totalGames%

        Print #1, statsO$(I); Tab(19); Using "###"; statsO%(I);
        Print #1, "-";: Print #1, Using "###   "; compT%(I);

        If statsO%(I) > compT%(I) Then W% = W% + 1: Print #1, "W";
        If statsO%(I) < compT%(I) Then L% = L% + 1: Print #1, "L";
        If statsO%(I) = compT%(I) Then tiedGames = tiedGames + 1: Print #1, "T";

        Print #1, Tab(31);
        Print #1, Using "####,###   "; CRD!(I);
        Print #1, statsH$(I); "  "; Left$(gameSite$(I), 24);

        Print #1, Tab(69);
        Print #1, "(";
        Print #1, Using "##"; W%;: Print #1, "-";: Print #1, Using "##"; L%;: Print #1, "-";: Print #1, Using "##"; tiedGames;
        Print #1, ")"
        lookyP%(0) = lookyP%(0) + statsO%(I)
        lookyP%(1) = lookyP%(1) + compT%(I)
    Next I

    Print #1,
    If statFormat = 0 Then Print #1, Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS

    lookyP%(0) = 0
    lookyP%(1) = 0
    W% = 0
    L% = 0
    tiedGames = 0

    Print #1,
    Print #1,
    Print #1, "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
    Print #1, teamName$; Tab(30); Using " ###  ###  ###  ###   ###"; statsZ!(32); statsZ!(33); statsZ!(34); statsZ!(35); statsZ!(32) + statsZ!(33) + statsZ!(34) + statsZ!(35) + statsZ!(36)
    Print #1, "OPPONENTS"; Tab(30); Using " ###  ###  ###  ###   ###"; statsZ1!(32); statsZ1!(33); statsZ1!(34); statsZ1!(35); statsZ1!(32) + statsZ1!(33) + statsZ1!(34) + statsZ1!(35) + statsZ!(36)
    Print #1,

    '-- Would totalGames% ever be 0?
    Print #1, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Print #1, teamName$; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsZ!(32) / totalGames%; statsZ!(33) / totalGames%; statsZ!(34) / totalGames%; statsZ!(35) / totalGames%; (statsZ!(32) + statsZ!(33) + statsZ!(34) + statsZ!(35) + statsZ!(36)) / totalGames%
    Print #1, "OPPONENTS"; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsZ1!(32) / totalGames%; statsZ1!(33) / totalGames%; statsZ1!(34) / totalGames%; statsZ1!(35) / totalGames%; (statsZ1!(32) + statsZ1!(33) + statsZ1!(34) + statsZ1!(35) + statsZ!(36)) / totalGames%
    Print #1,

    HD = 0: AD = 0: ND = 0: TD = 0
    HT = 0: CT = 0: NT = 0: TT = 0
    HA = 0: AA = 0: NA = 0: TA = 0

    For I = 1 To totalGames%
        If Left$(statsH$(I), 1) = "H" Then HD = HD + 1: HT = HT + CRD!(I)
        If Left$(statsH$(I), 1) = "A" Then AD = AD + 1: CT = CT + CRD!(I)
        If Left$(statsH$(I), 1) = "X" Then ND = ND + 1: NT = NT + CRD!(I)
        TD = TD + 1
        TT = TT + CRD!(I)
    Next

    If HD <> 0 Then HA = HT / HD
    If AD <> 0 Then AA = CT / AD
    If ND <> 0 Then NA = NT / ND
    If TD <> 0 Then TA = TT / TD

    Print #1, "ATTENDANCE    DATES      TOTAL    AVERAGE"
    Print #1, Using "HOME            ##    #####,###   ###,###"; HD; HT; HA
    Print #1, Using "AWAY            ##    #####,###   ###,###"; AD; CT; AA
    Print #1, Using "NEUTRAL         ##    #####,###   ###,###"; ND; NT; NA
    Print #1, Using "TOTAL           ##    #####,###   ###,###"; TD; TT; TA
    Print #1, Chr$(12)

    Print #1, "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Print #1, "FIRST DOWNS"; Tab(45); Using "####"; teamStats!(0, 0);
    Print #1, Tab(65); Using "####"; teamStats!(1, 0)

    Print #1, "RUSHING ATTEMPTS"; Tab(45);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(0, 1); Else Print #1, Using "####"; teamStats!(0, 1) - sackStats!(0, 0);
    Print #1, Tab(65);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(1, 1) Else Print #1, Using "####"; teamStats!(1, 1) - sackStats!(1, 0)

    Print #1, "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(0, 2); Else Print #1, Using "####"; teamStats!(0, 2) + sackStats!(0, 1);
    Print #1, Tab(65);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(1, 2) Else Print #1, Using "####"; teamStats!(1, 2) + sackStats!(1, 1)

    Print #1, "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And teamStats!(0, 1) <> 0 Then Print #1, Using "##.#"; teamStats!(0, 2) / teamStats!(0, 1);
    If statFormat <> 0 And teamStats!(0, 1) - sackStats!(0, 0) <> 0 Then Print #1, Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0));

    Print #1, Tab(65);

    If statFormat = 0 And teamStats!(1, 1) <> 0 Then Print #1, Using "##.#"; teamStats!(1, 2) / teamStats!(1, 1)
    If statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) <> 0 Then Print #1, Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0))
    If statFormat = 0 And teamStats!(1, 1) = 0 Or statFormat <> 0 And teamStats!(1, 1) - sackStats!(1, 0) = 0 Then Print #1,

    Print #1, "RUSHING YARDS PER GAME"; Tab(44);

    If statFormat = 0 Then Print #1, Using "###.#"; teamStats!(0, 2) / totalGames%; Else Print #1, Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%;
    Print #1, Tab(64);
    If statFormat = 0 Then Print #1, Using "###.#"; teamStats!(1, 2) / totalGames% Else Print #1, Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%

    Print #1,
    Print #1, "PASSES ATTEMPTED"; Tab(45); Using "####"; teamStats!(0, 3);: Print #1, Tab(65); Using "####"; teamStats!(1, 3)
    Print #1, "PASSES COMPLETED"; Tab(45); Using "####"; teamStats!(0, 4);: Print #1, Tab(65); Using "####"; teamStats!(1, 4)
    Print #1, "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; teamStats!(0, 5);: Print #1, Tab(65); Using "####"; teamStats!(1, 5)
    Print #1, "PASS COMPLETION PERCENTAGE"; Tab(44);

    If teamStats!(0, 3) <> 0 Then Print #1, Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;
    If teamStats!(1, 3) <> 0 Then
        Print #1, Tab(64); Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100
    Else
        Print #1,
    End If

    Print #1, "NET YARDS PASSING"; Tab(45); Using "####"; teamStats!(0, 6);
    Print #1, Tab(65); Using "####"; teamStats!(1, 6)

    Print #1, "YARDS PER PASS ATTEMPT"; Tab(45);
    If teamStats!(0, 3) <> 0 Then Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    If teamStats!(1, 3) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3)
    Else
        Print #1,
    End If

    Print #1, "YARDS PER PASS COMPLETION"; Tab(45);
    If teamStats!(0, 4) <> 0 Then Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);
    If teamStats!(1, 4) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4)
    Else
        Print #1,
    End If

    Print #1, "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; teamStats!(0, 6) / totalGames%;
    Print #1, Tab(64); Using "###.#"; teamStats!(1, 6) / totalGames%
    Print #1,

    Print #1, "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(0, 1) + teamStats!(0, 3); Else Print #1, Using "####"; teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3);
    Print #1, Tab(65);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(1, 1) + teamStats!(1, 3) Else Print #1, Using "####"; teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)

    Print #1, "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then Print #1, Using "###.#"; (teamStats!(0, 1) + teamStats!(0, 3)) / totalGames%; Else Print #1, Using "###.#"; (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3)) / totalGames%;
    Print #1, Tab(64);
    If statFormat = 0 Then Print #1, Using "###.#"; (teamStats!(1, 1) + teamStats!(1, 3)) / totalGames% Else Print #1, Using "###.#"; (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3)) / totalGames%

    Print #1, "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(0, 2) + teamStats!(0, 6); Else Print #1, Using "####"; teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6);
    Print #1, Tab(65);
    If statFormat = 0 Then Print #1, Using "####"; teamStats!(1, 2) + teamStats!(1, 6) Else Print #1, Using "####"; teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)

    Print #1, "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then Print #1, Using "##.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / (teamStats!(0, 1) + teamStats!(0, 3)); Else Print #1, Using "##.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / (teamStats!(0, 1) - sackStats!(0, 0) + teamStats!(0, 3));
    Print #1, Tab(64);
    If statFormat = 0 Then Print #1, Using "##.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / (teamStats!(1, 1) + teamStats!(1, 3)) Else Print #1, Using "##.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / (teamStats!(1, 1) - sackStats!(1, 0) + teamStats!(1, 3))

    Print #1, "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then Print #1, Using "###.#"; (teamStats!(0, 2) + teamStats!(0, 6)) / totalGames%; Else Print #1, Using "###.#"; (teamStats!(0, 2) + sackStats!(0, 1) + teamStats!(0, 6)) / totalGames%;
    Print #1, Tab(64);
    If statFormat = 0 Then Print #1, Using "###.#"; (teamStats!(1, 2) + teamStats!(1, 6)) / totalGames% Else Print #1, Using "###.#"; (teamStats!(1, 2) + sackStats!(1, 1) + teamStats!(1, 6)) / totalGames%

    Print #1,
    Print #1, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsZ!(22);: Print #1, "/";: Print #1, Using "####"; statsZ!(23);
    Print #1, Tab(61); Using "###"; statsZ1!(22);: Print #1, "/";: Print #1, Using "####"; statsZ1!(23)

    Print #1, "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsZ!(22) <> 0 Then Print #1, Using "##.#"; statsZ!(23) / statsZ!(22);
    If statsZ1!(22) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsZ1!(23) / statsZ1!(22)
    Else
        Print #1,
    End If

    Print #1, "KICKOFF RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsZ!(22) / totalGames%;
    Print #1, Tab(65); Using "##.#"; statsZ1!(22) / totalGames%

    Print #1, "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsZ!(24);: Print #1, "/";: Print #1, Using "####"; statsZ!(25);
    Print #1, Tab(61); Using "###"; statsZ1!(24);: Print #1, "/";: Print #1, Using "####"; statsZ1!(25)

    Print #1, "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsZ!(24) <> 0 Then Print #1, Using "##.#"; statsZ!(25) / statsZ!(24);
    If statsZ1!(24) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsZ1!(25) / statsZ1!(24)
    Else
        Print #1,
    End If

    Print #1, "PUNT RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsZ!(24) / totalGames%;
    Print #1, Tab(65); Using "##.#"; statsZ1!(24) / totalGames%

    Print #1, "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsZ!(26);: Print #1, "/";: Print #1, Using "####"; statsZ!(27);
    Print #1, Tab(61); Using "###"; statsZ1!(26);: Print #1, "/";: Print #1, Using "####"; statsZ1!(27)

    Print #1, "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsZ!(26) <> 0 Then Print #1, Using "##.#"; statsZ!(27) / statsZ!(26);
    If statsZ1!(26) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsZ1!(27) / statsZ1!(26)
    Else
        Print #1,
    End If

    Print #1, "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsZ!(26) / totalGames%;
    Print #1, Tab(65); Using "##.#"; statsZ1!(26) / totalGames%

    Print #1,

    Print #1, "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print #1, Using "###"; teamStats!(0, 7);: Print #1, "/";: Print #1, Using "####"; teamStats!(0, 8);
    Print #1, Tab(61); Using "###"; teamStats!(1, 7);: Print #1, "/";: Print #1, Using "####"; teamStats!(1, 8)

    Print #1, "AVERAGE YARDS PER PUNT"; Tab(45);
    If teamStats!(0, 7) <> 0 Then Print #1, Using "##.#"; teamStats!(0, 8) / teamStats!(0, 7);
    If teamStats!(1, 7) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 8) / teamStats!(1, 7)
    Else
        Print #1,
    End If

    Print #1, "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);
    Print #1, Using "##.#"; teamStats!(0, 7) / totalGames%;
    Print #1, Tab(65); Using "##.#"; teamStats!(1, 7) / totalGames%
    Print #1,

    Print #1, "FUMBLES / FUMBLES LOST"; Tab(41);
    Print #1, Using "###"; teamStats!(0, 9);: Print #1, "/";: Print #1, Using "####"; teamStats!(0, 10);
    Print #1, Tab(61); Using "###"; teamStats!(1, 9);: Print #1, "/";: Print #1, Using "####"; teamStats!(1, 10)

    Print #1, "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print #1, Using "###"; teamStats!(0, 11);: Print #1, "/";: Print #1, Using "####"; teamStats!(0, 12);
    Print #1, Tab(61); Using "###"; teamStats!(1, 11);: Print #1, "/";: Print #1, Using "####"; teamStats!(1, 12)

    Print #1, "AVERAGE YARDS PER PENALTY"; Tab(45);
    If teamStats!(0, 11) <> 0 Then Print #1, Using "##.#"; teamStats!(0, 12) / teamStats!(0, 11);
    If teamStats!(1, 11) <> 0 Then
        Print #1, Tab(65); Using "##.#"; teamStats!(1, 12) / teamStats!(1, 11)
    Else
        Print #1,
    End If

    Print #1, "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print #1, Using "##.#"; teamStats!(0, 11) / totalGames%;: Print #1, "/";: Print #1, Using "##.#"; teamStats!(0, 12) / totalGames%;
    Print #1, Tab(60); Using "##.#"; teamStats!(1, 11) / totalGames%;: Print #1, "/";: Print #1, Using "##.#"; teamStats!(1, 12) / totalGames%

    Print #1, "SAFETIES"; Tab(45);
    Print #1, Using "####"; statsZ!(21);
    Print #1, Tab(65); Using "####"; statsZ1!(21)

    Print #1, "TIMES SACKED / YARDS LOST"; Tab(41);
    Print #1, Using "###"; sackStats!(0, 0);: Print #1, "/";: Print #1, Using "####"; sackStats!(0, 1);
    Print #1, Tab(61); Using "###"; sackStats!(1, 0);: Print #1, "/";: Print #1, Using "####"; sackStats!(1, 1)
    Print #1, Chr$(12)

    Print #1, "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"

    I = 0

    If D% <> 1 Then
        For I1 = 0 To 9
            For I2 = 0 To 3
                If lookyRBacks$(I1) = lookyQBacks$(I2) Then
                    K2!(I1, 0) = K2!(I1, 0) + K2!(I2 + 10, 0)
                    K2!(I1, 1) = K2!(I1, 1) + K2!(I2 + 10, 1)
                    K2!(I1, 2) = K2!(I1, 2) + K2!(I2 + 10, 2)
                    K2!(I2 + 10, 0) = 0
                    K2!(I2 + 10, 1) = 0
                    K2!(I2 + 10, 2) = 0
                End If
            Next I2
        Next I1
    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13
        If K2!(I1, 0) <> 0 Then
            If I1 <= 9 Then Print #1, lookyRBacks$(I1);
            If I1 > 9 Then Print #1, lookyQBacks$(I1 - 10);
            Print #1, Tab(18); Using "####  ####   ##.#      ###.#    ##    "; K2!(I1, 0); K2!(I1, 1); K2!(I1, 1) / K2!(I1, 0); K2!(I1, 1) / totalGames%; K2!(I1, 2);
            If I1 > 9 Then
                Print #1,
            Else
                Print #1, Using "###"; statsLR!(I1);
                Print #1, statsLR$(I1, 0); Tab(64); statsLR$(I1, 1)
            End If
        End If
    Next I1

    Print #1, teamName$; Tab(18);
    If statFormat = 0 Then 
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1); teamStats!(0, 2); teamStats!(0, 2) / teamStats!(0, 1); teamStats!(0, 2) / totalGames%; statsZ!(15)
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(0, 1) - sackStats!(0, 0); teamStats!(0, 2) + sackStats!(0, 1); (teamStats!(0, 2) + sackStats!(0, 1)) / (teamStats!(0, 1) - sackStats!(0, 0)); (teamStats!(0, 2) + sackStats!(0, 1)) / totalGames%; statsZ!(15)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then 
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1); teamStats!(1, 2); teamStats!(1, 2) / teamStats!(1, 1); teamStats!(1, 2) / totalGames%; statsZ1!(15)
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; teamStats!(1, 1) - sackStats!(1, 0); teamStats!(1, 2) + sackStats!(1, 1); (teamStats!(1, 2) + sackStats!(1, 1)) / (teamStats!(1, 1) - sackStats!(1, 0)); (teamStats!(1, 2) + sackStats!(1, 1)) / totalGames%; statsZ1!(15)
    End If

    Print #1,
    Print #1, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5
        If K2!(I1, 6) <> 0 Then
            Print #1, lookyWdRec$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; K2!(I1, 6); K2!(I1, 7); K2!(I1, 7) / K2!(I1, 6); K2!(I1, 7) / totalGames%; K2!(I1, 8);
            Print #1, Using "###"; statsLC!(I1 + 10);
            Print #1, statsLC$(I1 + 10, 0); Tab(64); statsLC$(I1 + 10, 1)
        End If
    Next I1

    For I1 = 0 To 9
        If K2!(I1, 3) <> 0 Then
            Print #1, lookyRBacks$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; K2!(I1, 3); K2!(I1, 4); K2!(I1, 4) / K2!(I1, 3); K2!(I1, 4) / totalGames%; K2!(I1, 5);
            Print #1, Using "###"; statsLC!(I1);
            Print #1, statsLC$(I1, 0); Tab(64); statsLC$(I1, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);
    Print #1, Using "####  ####   "; teamStats!(0, 4); teamStats!(0, 6);

    If teamStats!(0, 4) = 0 Then Print #1, "  --"; Else Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 4);

    Print #1, Using "      ###.#    ##"; teamStats!(0, 6) / totalGames%; statsZ!(17)
    Print #1, "OPPONENT TOTALS"; Tab(18);
    Print #1, Using "####  ####   "; teamStats!(1, 4); teamStats!(1, 6);

    If teamStats!(1, 4) = 0 Then Print #1, "  --"; Else Print #1, Using "##.#"; teamStats!(1, 6) / teamStats!(1, 4);

    Print #1, Using "      ###.#    ##"; teamStats!(1, 6) / totalGames%; statsZ1!(17)

    Print #1,
    Print #1, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3

        If K2!(I1, 9) <> 0 Then

            Print #1, lookyQBacks$(I1); Tab(18); Using "####  ### ###.# #### ##.#"; K2!(I1, 9); K2!(I1, 10); K2!(I1, 10) / K2!(I1, 9) * 100; K2!(I1, 12); K2!(I1, 12) / K2!(I1, 9);

            If K2!(I1, 10) = 0 Then Print #1, "   --"; Else Print #1, Using " ##.#"; K2!(I1, 12) / K2!(I1, 10);

            Print #1, Using " ###.# ### ### ###.#     "; K2!(I1, 12) / totalGames%; K2!(I1, 13); K2!(I1, 11); K2!(I1, 11) / K2!(I1, 9) * 100;

            RT! = QBRating!(I1, statFormat)

            Print #1, Using "###.#"; RT!

        End If

    Next I1

    Print #1, teamName$; Tab(18);
    Print #1, Using "####  ### "; teamStats!(0, 3); teamStats!(0, 4);

    If teamStats!(0, 3) = 0 Then Print #1, "   --"; Else Print #1, Using "###.#"; teamStats!(0, 4) / teamStats!(0, 3) * 100;

    Print #1, Using "#### "; teamStats!(0, 6);

    If teamStats!(0, 3) = 0 Then Print #1, "  --"; Else Print #1, Using "##.#"; teamStats!(0, 6) / teamStats!(0, 3);
    If teamStats!(0, 4) = 0 Then Print #1, "   --"; Else Print #1, Using " ##.#"; teamStats!(0, 6) / teamStats!(0, 4);

    Print #1, Using " ###.# ### ### "; teamStats!(0, 6) / totalGames%; statsZ!(17); teamStats!(0, 5);

    If teamStats!(0, 3) = 0 Then Print #1, "   --"; Else Print #1, Using "###.#     "; teamStats!(0, 5) / teamStats!(0, 3) * 100

    Print #1, "OPPONENT TOTALS"; Tab(18);
    Print #1, Using "####  ### "; teamStats!(1, 3); teamStats!(1, 4);

    If teamStats!(1, 3) = 0 Then Print #1, "   --"; Else Print #1, Using "###.#"; teamStats!(1, 4) / teamStats!(1, 3) * 100;

    Print #1, Using "#### "; teamStats!(1, 6);

    If teamStats!(1, 3) = 0 Then Print #1, "  --"; Else Print #1, Using "##.#"; teamStats!(1, 6) / teamStats!(1, 3);
    If teamStats!(1, 4) = 0 Then Print #1, "  --"; Else Print #1, Using " ##.#"; teamStats!(1, 6) / teamStats!(1, 4);

    Print #1, Using " ###.# ### ### "; teamStats!(1, 6) / totalGames%; statsZ1!(17); teamStats!(1, 5);

    If teamStats!(1, 3) = 0 Then Print #1, "   --" Else Print #1, Using "###.#     "; teamStats!(1, 5) / teamStats!(1, 3) * 100

    Print #1,
    Print #1,
    Print #1, "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If K2!(I1, 9) <> 0 Then
            Print #1, lookyQBacks$(I1); Tab(22); Using "###"; statsLP!(I1);
            Print #1, statsLP$(I1, 0); Tab(32); statsLP$(I1, 1)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If K2!(I1, 14) <> 0 Then
            Print #1, lookyKickRet$(I1); Tab(18);
            Print #1, Using "####  ####   "; K2!(I1, 14); K2!(I1, 15);
            If K2!(I1, 14) = 0 Then Print #1, "  --"; Else Print #1, Using "##.#"; K2!(I1, 15) / K2!(I1, 14);
            Print #1, Using "      ###.#    ##    "; K2!(I1, 15) / totalGames%; K2!(I1, 16);
            Print #1, Using "###"; statsLK!(I1);: Print #1, statsLK$(I1, 0); Tab(64); statsLK$(I1, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);

    If statsZ!(22) = 0 Then Print #1, "   0     0   --.-        -.-     0" Else Print #1, Using "####  ####   ##.#      ###.#    ##"; statsZ!(22); statsZ!(23); statsZ!(23) / statsZ!(22); statsZ!(23) / totalGames%; statsZ!(18)

    Print #1, "OPPONENT TOTALS"; Tab(18);

    If statsZ1!(22) = 0 Then Print #1, "   0     0   --.-        -.-     0" Else Print #1, Using "####  ####   ##.#      ###.#    ##"; statsZ1!(22); statsZ1!(23); statsZ1!(23) / statsZ1!(22); statsZ1!(23) / totalGames%; statsZ1!(18)

    Print #1,
    Print #1,
    Print #1,
    Print #1, "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 3 To 5
        If K2!(I1, 14) <> 0 Then
            Print #1, lookyPRet$(I1 - 3); Tab(18);
            Print #1, Using "####  ####   "; K2!(I1, 14); K2!(I1, 15);
            If K2!(I1, 14) = 0 Then Print #1, "  --"; Else Print #1, Using "##.#"; K2!(I1, 15) / K2!(I1, 14);
            Print #1, Using "      ###.#    ##    "; K2!(I1, 15) / totalGames%; K2!(I1, 16);
            Print #1, Using "###"; statsPK!(I1 - 3);: Print #1, statsPK$(I1 - 3, 0); Tab(64); statsPK$(I1 - 3, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);

    If statsZ!(24) = 0 Then Print #1, "   0     0   --.-        -.-     0" Else Print #1, Using "####  ####   ##.#      ###.#    ##"; statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); statsZ!(25) / totalGames%; statsZ!(19)

    Print #1, "OPPONENT TOTALS"; Tab(18);

    If statsZ1!(24) = 0 Then Print #1, "   0     0   --.-        -.-     0" Else Print #1, Using "####  ####   ##.#      ###.#    ##"; statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); statsZ1!(25) / totalGames%; statsZ1!(19)

    Print #1,

    I = 0

    Print #1, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Print #1, lookyPunter$(0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; teamStats!(0, 7); teamStats!(0, 8); teamStats!(0, 8) / teamStats!(0, 7); teamStats!(0, 7) / totalGames%; statsZ!(31); statsZ!(30); statsZ!(28)
    Print #1, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; teamStats!(1, 7); teamStats!(1, 8); teamStats!(1, 8) / teamStats!(1, 7); teamStats!(1, 7) / totalGames%; statsZ1!(31); statsZ1!(30)
    Print #1,
    Print #1,
    Print #1,
    Print #1, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Print #1, teamName$; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; teamStats!(0, 7); teamStats!(0, 8); teamStats!(0, 8) / teamStats!(0, 7); statsZ1!(24); statsZ1!(25); statsZ1!(25) / statsZ1!(24); (teamStats!(0, 8) - statsZ1!(25)) / teamStats!(0, 7)
    Print #1, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; teamStats!(1, 7); teamStats!(1, 8); teamStats!(1, 8) / teamStats!(1, 7); statsZ!(24); statsZ!(25); statsZ!(25) / statsZ!(24); (teamStats!(1, 8) - statsZ!(25)) / teamStats!(1, 7)
    Print #1,
    Print #1,
    Print #1,
    Print #1, "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

    If K2!(4, 17) <> 0 Or K2!(6, 17) <> 0 Then
        Print #1, lookyKicker$(0);
        Print #1, Tab(16);
        If K2!(6, 17) <> 0 Then Print #1, Using "### ### ###.# "; K2!(5, 17); K2!(6, 17); K2!(5, 17) / K2!(6, 17) * 100;
        If K2!(4, 17) = 0 Then
            Print #1,
        Else
            Print #1, Tab(30);
            Print #1, Using "### ### ###.# ## ##"; K2!(3, 17); K2!(4, 17); K2!(3, 17) / K2!(4, 17) * 100; statsLF!(0); AM!(0, 0);
            Print #1, "-";: Print #1, Using "## ##"; AF!(0, 0); AM!(0, 1);
            Print #1, "-";: Print #1, Using "## ##"; AF!(0, 1); AM!(0, 2);
            Print #1, "-";: Print #1, Using "## ##"; AF!(0, 2); AM!(0, 3);
            Print #1, "-";: Print #1, Using "## ##"; AF!(0, 3); AM!(0, 4);
            Print #1, "-";: Print #1, Using "##"; AF!(0, 4)
        End If
    End If

    If K2!(8, 17) <> 0 Or K2!(10, 17) <> 0 Then
        Print #1, lookyKicker$(1); Tab(16);
        If K2!(10, 17) <> 0 Then Print #1, Using "### ### ###.# "; K2!(9, 17); K2!(10, 17); K2!(9, 17) / K2!(10, 17) * 100;
        If K2!(8, 17) = 0 Then
            Print #1,
        Else
            Print #1, Tab(30);
            Print #1, Using "### ### ###.# ## ##"; K2!(7, 17); K2!(8, 17); K2!(7, 17) / K2!(8, 17) * 100; statsLF!(1); AM!(1, 0);
            Print #1, "-";: Print #1, Using "## ##"; AF!(1, 0); AM!(1, 1);
            Print #1, "-";: Print #1, Using "## ##"; AF!(1, 1); AM!(1, 2);
            Print #1, "-";: Print #1, Using "## ##"; AF!(1, 2); AM!(1, 3);
            Print #1, "-";: Print #1, Using "## ##"; AF!(1, 3); AM!(1, 4);
            Print #1, "-";: Print #1, Using "##"; AF!(1, 4)
        End If
    End If

    Print #1,
    Print #1,
    Print #1, "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 0 To 9
        If statsGI%(I1, 0) <> 0 Then
            Print #1, intName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; statsGI%(I1, 0); statsGI%(I1, 1); statsGI%(I1, 1) / statsGI%(I1, 0); statsGI%(I1, 1) / totalGames%; statsGI%(I1, 2);
            Print #1, Using "###"; statsLI!(I1);: Print #1, statsLI$(I1, 0); Tab(64); statsLI$(I1, 1)
        End If
    Next I1

    Print #1,
    Print #1, "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If statsGS%(I1, 0) <> 0 Then
            Print #1, sackName$(I1); Tab(20); Using "###            ###"; statsGS%(I1, 0); statsGS%(I1, 1)
        End If
    Next

    Close #1

    Open "REPORT" For Input As #2

    Do While Not EOF(2)
        Line Input #2, X$
        LPrint X$
    Loop

    Close #2

    LPrint Chr$(12)

    I$ = GetKeyPress$

End Sub


'----------------------------------------
'       ResetStatPrompt Subroutine
'----------------------------------------
'This subroutine prompts the user with
'a choice to reset stats. If they chose,
'and confirm, it will then proceed to
'delete the files.
Sub ResetStatPrompt (statFile$)

    Cls

    Call MenuBox(6, 8, 61, 1, 15, 1)

    Do
        Locate 7
        U$ = "RESET COMPILED STATS TO 0? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    Color 15, 0

    If UCase$(I$) = "Y" Then

        Call MenuBox(6, 8, 61, 1, 15, 1)

        Do

            Locate 7
            U$ = "ARE YOU SURE? (Y/N)"
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

