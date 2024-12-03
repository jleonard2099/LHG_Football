'----------------------------------------
'    UpdateExpandedStandings Subroutine
'----------------------------------------
'This subroutine takes the stats read in
'for a league, then calculates the standings
'for that league and writes a  corresponding
'.STD file.
Sub UpdateExpandedStandings (league$, orgType)

    Shared lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totRoadLosses(), totRoadWins()
    Shared totNeutralLosses(), totNeutralWins()

    Shared totTeamLosses(), lastLoseStreak()
    Shared teamLoseStreak(), teamWinStreak(), teamTotWins()

    Shared teamScore(), oppScore()

    Shared locIndicator$(), oppName$()
    Shared memberName$(), expStdLeagName$()

    Shared div1Name$, div2Name$, div3Name$, div4Name$, league$

    TMR = TEAM_LIMIT
    currRecord = 0

    '-- Need to update this to handle College or Pro
    If orgType = 1 Then 
        TEAM_LIMIT = TEAMS_PER_LEAGUE
    Else
        TEAM_LIMIT = TEAMS_PER_CONFERENCE
    End If

    For KK = 1 To TEAM_LIMIT

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

                Open diskPaths$(1) + memberName$(KK) + "S" For Input As #2

                Input #2, nbrGames

                For J = 1 To MAX_SCHED_STATS:
                    Input #2, teamScore(J)
                Next

                For J = 1 To MAX_SCHED_STATS:
                    Input #2, oppName$(J)
                Next

                For J = 1 To MAX_SCHED_STATS:
                    Input #2, oppScore(J)
                Next

                For J = 1 To MAX_SCHED_STATS:
                    Input #2, locIndicator$(J)
                Next

                Close #2

                lastWinStreak(currRecord) = 0
                lastLoseStreak(currRecord) = 0
                teamWinStreak(currRecord) = 0
                teamLoseStreak(currRecord) = 0

                For I = 1 To nbrGames

                    If teamScore(I) > oppScore(I) Then

                        teamTotWins(currRecord) = teamTotWins(currRecord) + 1
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

    Open diskPaths$(2) + league$ + ".STD" For Output As #3

    Write #3, orgType

    Write #3, league$, div1Name$, div2Name$, div3Name$, div4Name$

    For I = 1 To TEAM_LIMIT
        Write #3, teamTotWins(I), totHomeWins(I), totRoadWins(I), totTeamLosses(I), totHomeLosses(I), totRoadLosses(I), expStdLeagName$(I)
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

    Shared lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared totRoadLosses(), totRoadWins()
    Shared totNeutralLosses(), totNeutralWins()
    Shared teamLoseStreak(), teamWinStreak(), teamTotWins()

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
        Locate 2: Print Tab(20); "                                  HOME    ROAD    NEUTRAL"
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

        Print Tab(20); "G   W   L   PCT   GB     LS  WS   W- L    W- L    W- L"

        For I = AA To ZZ

            If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                Color 3
                Print expStdLeagName$(I);
                Color 15

                'G / W / L
                Print Tab(18); Using "### ### ###"; teamTotWins(I) + totTeamLosses(I); teamTotWins(I); totTeamLosses(I); Tab(30);

                ' PCT
                If totTeamLosses(I) = 0 And totTeamLosses(I) + teamTotWins(I) <> 0 Then
                    Print "1.000";
                Else
                    If teamTotWins(I) = 0 Then
                        Print " .000";
                    Else
                        If teamTotWins(I) + totTeamLosses(I) <> 0 Then
                            winPCT! = teamTotWins(I) / (teamTotWins(I) + totTeamLosses(I))
                            Print Using " .###"; winPCT!;
                        Else
                            Print " .000";
                        End If
                    End If
                End If

                ' GB
                calcGB! = ((teamTotWins(AA) - teamTotWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2
                If calcGB! = 0 Then
                    Print Tab(36); " ---";
                Else
                    Print Tab(36); Using "##.#"; calcGB!;
                End If

                Print Tab(45); Using "##"; teamWinStreak(I); Tab(49); teamLoseStreak(I);

                ' Records: Home / Road / Neutral
                Print Tab(53); Using "##"; totHomeWins(I); Tab(56); totHomeLosses(I); Tab(61); totRoadWins(I); Tab(64); totRoadLosses(I); Tab(69); totNeutralWins(I); Tab(72); totNeutralWins(I)
            End If

        Next

        Color 14
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

    Next NT

End Sub


'----------------------------------------
'       NewStandings_Report Subroutine
'----------------------------------------
'This subroutine sorts the expanded league
'standings and outputs them to the default
'printer in a formatted view
Sub NewStandings_Report (printDest, destFile$, league$, orgType)

    Shared lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared totRoadLosses(), totRoadWins()
    Shared totNeutralLosses(), totNeutralWins()
    Shared teamLoseStreak(), teamWinStreak(), teamTotWins()

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
        Print #1, Tab(20); "                                  HOME    ROAD   NEUTRAL"

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

        Print #1, Tab(20); "G   W   L   PCT   GB     WS LS   W- L    W- L     W- L"

        For I = AA To ZZ

            If expStdLeagName$(I) <> "" And expStdLeagName$(I) <> "*" Then

                Print #1, expStdLeagName$(I);
                Print #1, Tab(18); Using "### ### ###"; teamTotWins(I) + totTeamLosses(I); teamTotWins(I); totTeamLosses(I); Tab(30);

                'PCT
                If totTeamLosses(I) = 0 And totTeamLosses(I) + teamTotWins(I) <> 0 Then
                    Print #1, "1.000";
                Else
                    If teamTotWins(I) = 0 Then
                        Print #1, " .000";
                    Else
                        If teamTotWins(I) + totTeamLosses(I) = 0 Then
                            Print #1, " .000";
                        Else
                            winPCT! = teamTotWins(I) / (teamTotWins(I) + totTeamLosses(I))
                            Print #1, Using " .###"; winPCT!;
                        End If
                    End If
                End If

                'Games Behind
                If ((teamTotWins(AA) - teamTotWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2 = 0 Then
                    Print #1, Tab(36); " ---";
                Else
                    Print #1, Tab(36); Using "##.#"; ((teamTotWins(AA) - teamTotWins(I)) + (totTeamLosses(I) - totTeamLosses(AA))) / 2;
                End If

                Print #1, Tab(45); Using "##"; teamWinStreak(I); Tab(49); teamLoseStreak(I);
                Print #1, Tab(53); Using "##"; totHomeWins(I); Tab(56); totHomeLosses(I); Tab(61); totRoadWins(I); Tab(64); totRoadLosses(I); Tab(69); totNeutralWins(I); Tab(72); totNeutralWins(I)

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

    Shared lastWinStreak()
    Shared totHomeLosses(), totHomeWins()
    Shared totTeamLosses(), lastLoseStreak()
    Shared totRoadLosses(), totRoadWins()
    Shared totNeutralLosses(), totNeutralWins()
    Shared teamLoseStreak(), teamWinStreak(), teamTotWins()

    Shared expStdLeagName$()

    For X = AA To ZZ - 1

        For Y = X + 1 To ZZ

            diffX = teamTotWins(X) - totTeamLosses(X)
            diffY = teamTotWins(Y) - totTeamLosses(Y)

            If diffX <= diffY Then

                If diffX <> diffY Then

                    If Not (teamTotWins(X) <= teamTotWins(Y) And teamTotWins(X) > totTeamLosses(X)) Or Not (teamTotWins(X) >= teamTotWins(Y) And teamTotWins(X) < totTeamLosses(X)) Then

                        Swap expStdLeagName$(X), expStdLeagName$(Y)
                        Swap teamTotWins(X), teamTotWins(Y)
                        Swap totTeamLosses(X), totTeamLosses(Y)
                        Swap totHomeLosses(X), totHomeLosses(Y)
                        Swap totRoadLosses(X), totRoadLosses(Y)
                        Swap totHomeWins(X), totHomeWins(Y)
                        Swap totRoadWins(X), totRoadWins(Y)
                        Swap teamWinStreak(X), teamWinStreak(Y)
                        Swap teamLoseStreak(X), teamLoseStreak(Y)

                    End If

                End If

            End If

        Next Y

    Next X

End Sub
