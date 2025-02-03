'----------------------------------------
'    UpdateExpandedStandings Subroutine
'----------------------------------------
'This subroutine takes the stats read in
'for a league, then calculates the standings
'for that league and writes a  corresponding
'.STD file.
Sub UpdateExpandedStandings (orgType)

    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak()
    Shared totHomeLosses(), totHomeWins(), totHomeTies()
    Shared totNeutralLosses(), totNeutralWins(), totNeutralTies()
    Shared totRoadLosses(), totRoadWins(), totRoadTies()
    Shared totTeamLosses(), totTeamWins(), totTeamTies()

    Shared teamScore(), oppScore()
    Shared gameAttendance&()

    Shared locIndicator$(), oppName$()
    Shared memberName$(), expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$, leagueName$

    If orgType = 1 Then
        TEAM_LIMIT = TEAMS_PER_LEAGUE
    Else
        TEAM_LIMIT = TEAMS_PER_CONFERENC
    End If

    TMR = TEAM_LIMIT

    Erase expStdLeagName$

    Erase teamScore, oppScore
    Erase gameAttendance&
    Erase locIndicator$, oppName$

    Erase lastWinStreak, lastLoseStreak, teamWinStreak, teamLoseStreak
    Erase totHomeLosses, totHomeWins, totHomeTies
    Erase totNeutralLosses, totNeutralWins, totNeutralTies
    Erase totRoadLosses, totRoadWins, totRoadTies
    Erase totTeamLosses, totTeamWins, totTeamTies

    currRecord = 0

    For KK = 1 To TEAM_LIMIT

        nbrGames = 0

        currRecord = currRecord + 1
        Color 14
        Locate 7, 39
        TMR = TMR - 1
        Print Using "##"; TMR

        If memberName$(KK) <> "*" Then

            expStdLeagName$(currRecord) = memberName$(KK)

            Color 15, 0
            Cls

            Locate 5: Print "COMPUTING RECORDS . . . . . . PLEASE WAIT"

            If _FileExists(diskPaths$(1) + memberName$(KK) + "S") Then

                test$ = memberName$(KK)
                Call ReadScheduleStats(memberName$(KK), currRecord, nbrGames)

                lastWinStreak(currRecord) = 0
                lastLoseStreak(currRecord) = 0
                teamWinStreak(currRecord) = 0
                teamLoseStreak(currRecord) = 0

                For I = 1 To nbrGames

                    If teamScore(I) > oppScore(I) Then

                        totTeamWins(currRecord) = totTeamWins(currRecord) + 1
                        lastWinStreak(currRecord) = lastWinStreak(currRecord) + 1
                        lastLoseStreak(currRecord) = 0

                        If locIndicator$(I) = "A" Then
                            totRoadWins(currRecord) = totRoadWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then
                            totHomeWins(currRecord) = totHomeWins(currRecord) + 1
                        End If
                        If locIndicator$(I) = "X" Then
                            totNeutralWins(currRecord) = totNeutralWins(currRecord) + 1
                        End If

                    End If

                    If teamScore(I) < oppScore(I) Then

                        totTeamLosses(currRecord) = totTeamLosses(currRecord) + 1
                        lastLoseStreak(currRecord) = lastLoseStreak(currRecord) + 1
                        lastWinStreak(currRecord) = 0

                        If locIndicator$(I) = "A" Then
                            totRoadLosses(currRecord) = totRoadLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then
                            totHomeLosses(currRecord) = totHomeLosses(currRecord) + 1
                        End If
                        If locIndicator$(I) = "X" Then
                            totNeutralLosses(currRecord) = totNeutralLosses(currRecord) + 1
                        End If

                    End If

                    If teamScore(I) = oppScore(I) Then

                        totTeamTies(currRecord) = totTeamTies(currRecord) + 1
                        lastLoseStreak(currRecord) = 0
                        lastWinStreak(currRecord) = 0

                        If locIndicator$(I) = "A" Then
                            totRoadTies(currRecord) = totRoadTies(currRecord) + 1
                        End If
                        If locIndicator$(I) = "H" Then
                            totHomeTies(currRecord) = totHomeTies(currRecord) + 1
                        End If
                        If locIndicator$(I) = "X" Then
                            totNeutralTies(currRecord) = totNeutralTies(currRecord) + 1
                        End If

                    End If

                    If lastWinStreak(currRecord) > teamWinStreak(currRecord) Then
                        teamWinStreak(currRecord) = lastWinStreak(currRecord)
                    End If
                    If lastLoseStreak(currRecord) > teamLoseStreak(currRecord) Then
                        teamLoseStreak(currRecord) = lastLoseStreak(currRecord)
                    End If

                Next I

            End If

        End If 'Done checking for *

    Next KK

    Open diskPaths$(2) + leagueName$ + ".STD" For Output As #3

    Write #3, orgType

    Write #3, leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

    For I = 1 To TEAM_LIMIT
        Write #3, expStdLeagName$(I), totTeamWins(I), totHomeWins(I), totRoadWins(I), totNeutralWins(I), totTeamLosses(I), totHomeLosses(I), totRoadLosses(I), totNeutralLosses(I), totTeamTies(I), totHomeTies(I), totRoadTies(I), totNeutralTies(I), teamWinStreak(I), teamLoseStreak(I)
    Next

    Close #3

End Sub

'----------------------------------------
'     NewStandings_Screen Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the user
'display in a formatted view
Sub NewStandings_Screen (league$, orgType)

    Shared totHomeLosses(), totHomeWins(), totHomeTies()
    Shared totRoadLosses(), totRoadWins(), totRoadTies()
    Shared totNeutralLosses(), totNeutralWins(), totNeutralTies()
    Shared totTeamLosses(), totTeamWins(), totTeamTies()
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    If orgType = 1 Then
        DIV_LIMIT = 4
    Else
        DIV_LIMIT = 3
    End If

    For NT = 1 To DIV_LIMIT

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Cls

        Color 11
        Locate 1, 35: Print league$
        Color 14
        Locate 2: Print Tab(20); "                              HOME   ROAD   NEUTRAL"
        Locate 3

        Select Case NT

            Case 1
                Print div1Name$;

            Case 2
                Print div2Name$;

            Case 3
                Print div3Name$;

            Case 4
                Print div4Name$;

        End Select

        Print Tab(20); "G  W  L  T  PCT  GB  WS  LS   W-L-T  W-L-T  W-L-T"

        For I = AA To ZZ

            If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                Color 3
                Print expStdLeagName$(I);
                Color 15

                'G / W / L
                Print Tab(19); Using "## ## ## ##"; totTeamWins(I) + totTeamLosses(I) + totTeamTies(I); totTeamWins(I); totTeamLosses(I); totTeamTies(I);

                ' PCT
                If totTeamLosses(I) = 0 And totTeamLosses(I) + totTeamWins(I) <> 0 Then
                    Print "1.000";
                Else
                    If totTeamWins(I) = 0 Then
                        Print " .000";
                    Else
                        If totTeamWins(I) + totTeamLosses(I) <> 0 Then
                            'Ties count as 1/2 win
                            winPCT! = (totTeamWins(I) + (totTeamTies(I) * 0.5)) / (totTeamWins(I) + totTeamLosses(I) + totTeamTies(I))
                            Print Using " .###"; winPCT!;
                        Else
                            Print " .000";
                        End If
                    End If
                End If

                ' GB
                calcGB! = ((totTeamWins(AA) - totTeamWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2
                If calcGB! = 0 Then
                    Print Tab(36); " ---";
                Else
                    Print Tab(36); Using "##.#"; calcGB!;
                End If

                ' Win Streak
                Print Tab(41); Using "##"; teamWinStreak(I); Tab(45); teamLoseStreak(I);

                ' Records: Home / Road / Neutral
                Print Tab(50); Using "#"; totHomeWins(I); Tab(52); totHomeLosses(I); Tab(54); totHomeTies(I); Tab(57); totRoadWins(I); Tab(59); totRoadLosses(I); Tab(61); totRoadTies(I); Tab(64); totNeutralWins(I); Tab(66); totNeutralLosses(I); Tab(68); totNeutralTies(I)

            End If

        Next

        Color 14
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

    Next NT

End Sub


'----------------------------------------
'     NewStandings_Report Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub NewStandings_Report (printDest, destFile$, league$, orgType)

    Shared totHomeLosses(), totHomeWins(), totHomeTies()
    Shared totRoadLosses(), totRoadWins(), totRoadTies()
    Shared totNeutralLosses(), totNeutralWins(), totNeutralTies()
    Shared totTeamLosses(), totTeamWins(), totTeamTies()
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak()

    Shared expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    MJ = 0

    Color 15, 0
    Cls

    If orgType = 1 Then
        DIV_LIMIT = 4
    Else
        DIV_LIMIT = 3
    End If

    Open destFile$ For Output As #1

    For NT = 1 To DIV_LIMIT

        AA = ((NT - 1) * 10) + 1
        ZZ = NT * 10

        Call SortStandings(AA, ZZ)

        Print #1, league$
        Print #1, Tab(20); "                              HOME   ROAD   NEUTRAL"

        Select Case NT

            Case 1
                Print #1, div1Name$;

            Case 2
                Print #1, div2Name$;

            Case 3
                Print #1, div3Name$;

            Case 4
                Print #1, div4Name$;

        End Select

        Print #1, Tab(20); "G  W  L  T  PCT  GB  WS  LS   W-L-T  W-L-T  W-L-T"

        For I = AA To ZZ

            If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                Print #1, expStdLeagName$(I);
                Print #1, Tab(19); Using "## ## ##"; totTeamWins(I) + totTeamLosses(I) + totTeamTies(I); totTeamWins(I); totTeamLosses(I); totTeamTies(I);

                'PCT
                If totTeamLosses(I) = 0 And totTeamLosses(I) + totTeamWins(I) <> 0 Then
                    Print #1, "1.000";
                Else
                    If totTeamWins(I) = 0 Then
                        Print #1, " .000";
                    Else
                        If totTeamWins(I) + totTeamLosses(I) = 0 Then
                            Print #1, " .000";
                        Else
                            'Ties count as 1/2 win
                            winPCT! = (totTeamWins(I) + (totTeamTies(I) * 0.5)) / (totTeamWins(I) + totTeamLosses(I) + totTeamTies(I))
                            Print #1, Using " .###"; winPCT!;
                        End If
                    End If
                End If

                'Games Behind
                If ((totTeamWins(AA) - totTeamWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2 = 0 Then
                    Print #1, Tab(36); " ---";
                Else
                    Print #1, Tab(36); Using "##.#"; ((totTeamWins(AA) - totTeamWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2;
                End If

                ' Win Streak
                Print #1, Tab(41); Using "##"; teamWinStreak(I); Tab(45); teamLoseStreak(I);

                ' Records: Home / Road / Neutral
                Print #1, Tab(50); Using "##"; totHomeWins(I); Tab(52); totHomeLosses(I); Tab(54); totHomeTies(I); Tab(57); totRoadWins(I); Tab(59); totRoadLosses(I); Tab(61); totRoadTies(I); Tab(64); totNeutralWins(I); Tab(66); totNeutralLosses(I); Tab(68); totNeutralTies(I)

            End If

        Next

        Print #1,
        Print #1,

    Next NT

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
'         SortStandings Subroutine
'----------------------------------------
'This subroutine simply sorts the league
'standings records so that teams that are
'leading can be displayed first.
'This sorting is only done on the arrays
'holding the standing data, so no changes
'are permanent to the underlying files.
Sub SortStandings (AA, ZZ)

    Shared totHomeLosses(), totHomeWins(), totHomeTies()
    Shared totRoadLosses(), totRoadWins(), totRoadTies()
    Shared totNeutralLosses(), totNeutralWins(), totNeutralTies()
    Shared totTeamLosses(), totTeamWins(), totTeamTies()
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak()

    Shared expStdLeagName$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = totTeamWins(X) - totTeamLosses(X)
            diffY = totTeamWins(Y) - totTeamLosses(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (totTeamWins(X) <= totTeamWins(Y) And totTeamWins(X) > totTeamLosses(X)) Or Not (totTeamWins(X) >= totTeamWins(Y) And totTeamWins(X) < totTeamLosses(X)) Then

                        Swap expStdLeagName$(X), expStdLeagName$(Y)
                        Swap totTeamWins(X), totTeamWins(Y)
                        Swap totHomeWins(X), totHomeWins(Y)
                        Swap totRoadWins(X), totRoadWins(Y)
                        Swap totNeutralWins(X), totNeutralWins(Y)
                        Swap totTeamLosses(X), totTeamLosses(Y)
                        Swap totHomeLosses(X), totHomeLosses(Y)
                        Swap totRoadLosses(X), totRoadLosses(Y)
                        Swap totNeutralLosses(X), totNeutralLosses(Y)
                        Swap totTeamTies(X), totTeamTies(Y)
                        Swap totHomeTies(X), totHomeTies(Y)
                        Swap totRoadTies(X), totRoadTies(Y)
                        Swap totNeutralTies(X), totNeutralTies(Y)
                        Swap teamWinStreak(X), teamWinStreak(Y)
                        Swap teamLoseStreak(X), teamLoseStreak(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub
