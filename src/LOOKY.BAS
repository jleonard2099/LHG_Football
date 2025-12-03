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

        Call MenuBox(8, 25, 30, 6, 15, 1)

        Locate 10, 15

        Locate , 32: Color 3: Print "VIEW/PRINT STATS IN:"
        Locate , 32: Color 15: Print "C";
        Color 7: Print "OLLEGE FORMAT"
        Locate , 32: Color 15: Print "P";
        Color 7: Print "RO FORMAT"
        Locate , 32: Color 15: Print "E";
        Color 7: Print "XIT TO MAIN MENU"

        I$ = GetKeyPress$

        Select Case UCase$(I$)
            Case "C": statFormat = 0
            Case "P": statFormat = 1
            Case "E", Chr$(27): '
        End Select

        If UCase$(I$) = "C" Or UCase$(I$) = "P" Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                Color 15, 0

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$) And teamNbr > 0 Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Call LoadTeamStats(statFile$, team$, totalGames%, teamPower, SS)
                    Call ViewStatsSummary(team$, totalGames%, statFormat, teamPower, SS)

                End If

            End If

        End If

    Loop Until UCase$(I$) = "E" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'       LoadTeamStats Subroutine
'----------------------------------------
'This subroutine loads the selected team's
'stats into arrays
Sub LoadTeamStats (statFile$, teamName$, totalGames, teamPower, SS)

    Shared oppScore(), powerRating()
    Shared statsGI(), statsGS()
    Shared teamRatings(), teamScore()

    Shared gameAttendance&()

    Shared allPlayerStats!(), allTeamStats!()
    Shared scoreTotals()

    Shared statsFGA!(), statsFGM!()
    Shared statsLPR!(), sackStats!()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsTeam!(), statsOpp!(), statsPlayers!()

    Shared gameSite$(), locIndicator$(), oppName$()

    Shared intName$(), sackName$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()

    Shared statsLFG$(), statsLRec$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Call ReadScheduleStats (statFile$, null, totalGames)

    'STRENGTH OF SCHEDULE
    SS = 0

    For I = 1 To totalGames
        SS = powerRating(1, I) + SS
    Next

    SS = SS / totalGames

    'INDIVIDUAL GAME POWER RATINGS
    teamPower = 0

    For I = 1 To totalGames

        pwrRating = teamScore(I) - oppScore(I) + powerRating(1, I)

        If Left$(locIndicator$(I), 1) = "H" Then
            pwrRating = pwrRating - 3
        End If

        If Left$(locIndicator$(I), 1) = "A" Then
            pwrRating = pwrRating + 3
        End If

        teamPower = teamPower + pwrRating

    Next

    GL = 0

    If totalGames <= 4 Then
        GL = 5 - totalGames
        teamPower = teamPower + GL * powerRating(0, 1)
    End If

    teamPower = teamPower / (totalGames + GL)

    totalLosses = 0
    totalWins = 0
    tiedGames = 0

    D% = 0
    scoreTotals(0) = 0
    scoreTotals(1) = 0

    Call ReadTeamStats (teamName$, statFile$)

    For J = 0 To 13
        For K = 0 To 17:
            allPlayerStats!(J, K) = statsPlayers!(J, K)
        Next K
    Next J

    For M = 0 To 12:
        allTeamStats!(0, M) = statsTeam!(M)
    Next M

    For M = 0 To 12:
        allTeamStats!(1, M) = statsOpp!(M)
    Next M

    sackStats!(0, 0) = statsTeam!(13)
    sackStats!(0, 1) = statsTeam!(14)
    sackStats!(1, 0) = statsOpp!(13)
    sackStats!(1, 1) = statsOpp!(14)

End Sub


'----------------------------------------
'       ViewStatsSummary Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's record to the display,
'then provides routines for output of
'the other stat categories
Sub ViewStatsSummary (teamName$, totalGames, statFormat, teamPower, SS)

    Shared teamRatings()

    Do

        DM% = 0

        Color 15, 0
        Cls

        Call TITLE_STATS

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Stats Report", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call StatsToReport(0, userFile$, teamName$, totalGames, statFormat, teamPower, SS)
                    End If

                Case "P"
                    Call StatsToReport(1, "REPORT", teamName$, totalGames, statFormat, teamPower, SS)

                Case "S"
                    Call StatsToScreen(teamName$, totalGames, statFormat, teamPower, SS)

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

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'      StatsToScreen Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's stats to the display
Sub StatsToScreen (teamName$, totalGames, statFormat, teamPower, SS)

    Shared statsFGA!(), statsFGM!()
    Shared allTeamStats!(), allPlayerStats!()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsLPR!(), sackStats!(), statsTeam!(), statsOpp!(), statsPlayers!()

    Shared gameAttendance&()

    Shared statsGI(), statsGS()

    Shared teamScore(), teamRatings(), oppScore(), scoreTotals()

    Shared intName$(), locIndicator$(), oppName$(), sackName$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared statsLRec$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$()
    Shared statsLPR$(), gameSite$()

    Shared HD&, AD&, ND&, TD&
    Shared HT&, CT&, NT&, TT&
    Shared HA&, AA&, NA&, TA&

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
    Color teamRatings(2), teamRatings(1)
    Print teamName$

    Color 14, 1
    Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD    ";
    Print
    Color 15, 0

    For I = 1 To totalGames

        Color 7

        If (I Mod 2) = 0 Then Color 15

        Print oppName$(I);
        Locate , 19: Print Using "###"; teamScore(I);
        Print "-";
        Print Using "###   "; oppScore(I);

        If teamScore(I) > oppScore(I) Then
            totalWins = totalWins + 1
            Print "W";
        End If

        If teamScore(I) < oppScore(I) Then
            totalLosses = totalLosses + 1
            Print "L";
        End If

        If teamScore(I) = oppScore(I) Then
            tiedGames = tiedGames + 1
            Print "T";
        End If

        Locate , 31: Print Using "####,###"; gameAttendance&(I);
        Locate , 43: Print locIndicator$(I); "  "; Left$(gameSite$(I), 24);

        Locate , 69
        Print "(";
        Print Using "##"; totalWins;: Print "-";: Print Using "##"; totalLosses;: Print "-";: Print Using "##"; tiedGames;
        Print ")"

        scoreTotals(0) = scoreTotals(0) + teamScore(I)
        scoreTotals(1) = scoreTotals(1) + oppScore(I)

        If (I Mod (LAST_ROW - 5) = 0) Then

            Locate LAST_ROW - 2, 1
            Color 15, 0: Print Space$(27); "PRESS ANY KEY TO CONTINUE"; Space$(28);
            I$ = GetKeyPress$
            ' *** ADD IN CLEAR SCREEN AND TITLE BAR ***
            Color , 0
            Cls

            Locate 1, 1
            Color 14, 1
            Print Tab(81);

            header$ = "GAME-BY-GAME REPORT FOR " + teamName$
            Xcoord = Len(header$)
            Xcoord = Int((80 - Xcoord) / 2)
            Locate , Xcoord

            Print "GAME-BY-GAME REPORT FOR ";
            Color teamRatings(2), teamRatings(1)
            Print teamName$

            Color 14, 1

            Print "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD   "
            Color 15, 0

        End If

    Next I

    If statFormat = 0 Then
        Locate LAST_ROW - 2, 31
        Print Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS;
    End If

    scoreTotals(0) = 0
    scoreTotals(1) = 0
    totalWins = 0
    totalLosses = 0
    tiedGames = 0

    Locate LAST_ROW - 2, 1
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
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; statsTeam!(32); statsTeam!(33); statsTeam!(34); statsTeam!(35); statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)
    Print "OPPONENTS";
    Locate , 30: Print Using " ###  ###  ###  ###   ###"; statsOpp!(32); statsOpp!(33); statsOpp!(34); statsOpp!(35); statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsOpp!(36)
    Print

    Color 14
    Print "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Color 15
    Print teamName$;
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsTeam!(32) / totalGames; statsTeam!(33) / totalGames; statsTeam!(34) / totalGames; statsTeam!(35) / totalGames; (statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)) / totalGames
    Print "OPPONENTS";
    Locate , 30: Print Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsOpp!(32) / totalGames; statsOpp!(33) / totalGames; statsOpp!(34) / totalGames; statsOpp!(35) / totalGames; (statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)) / totalGames
    Print

    Call CalcAttendanceCategories(totalGames)

    Color 14: Print "ATTENDANCE    DATES      TOTAL    AVERAGE"
    Color 15: Print Using "HOME            ##    #####,###   ###,###"; HD&; HT&; HA&
    Color 15: Print Using "AWAY            ##    #####,###   ###,###"; AD&; CT&; AA&
    Color 15: Print Using "NEUTRAL         ##    #####,###   ###,###"; ND&; NT&; NA&
    Color 15: Print Using "TOTAL           ##    #####,###   ###,###"; TD&; TT&; TA&

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 2
    Color 12: Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Color 14: Print "RUSHING"
    Color 7
    Print "RUSHING ATTEMPTS"; Tab(45);
    'Team
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(0, 1);
    Else
        Print Using "####"; allTeamStats!(0, 1) - sackStats!(0, 0);
    End If
    Print Tab(65);
    'Opponent
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(1, 1)
    Else
        Print Using "####"; allTeamStats!(1, 1) - sackStats!(1, 0)
    End If

    Color 15
    Print "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(0, 2);
    Else
        Print Using "####"; allTeamStats!(0, 2) + sackStats!(0, 1);
    End If
    Print Tab(65);
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(1, 2)
    Else
        Print Using "####"; allTeamStats!(1, 2) + sackStats!(1, 1)
    End If

    Color 7
    Print "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And allTeamStats!(0, 1) <> 0 Then
        Print Using "##.#"; allTeamStats!(0, 2) / allTeamStats!(0, 1);
    End If
    If statFormat <> 0 And allTeamStats!(0, 1) - sackStats!(0, 0) <> 0 Then
        Print Using "##.#"; (allTeamStats!(0, 2) + sackStats!(0, 1)) / (allTeamStats!(0, 1) - sackStats!(0, 0));
    End If
    Print Tab(65);
    If statFormat = 0 And allTeamStats!(1, 1) <> 0 Then
        Print Using "##.#"; allTeamStats!(1, 2) / allTeamStats!(1, 1)
    End If
    If statFormat <> 0 And allTeamStats!(1, 1) - sackStats!(1, 0) <> 0 Then
        Print Using "##.#"; (allTeamStats!(1, 2) + sackStats!(1, 1)) / (allTeamStats!(1, 1) - sackStats!(1, 0))
    End If
    If statFormat = 0 And allTeamStats!(1, 1) = 0 Or statFormat <> 0 And allTeamStats!(1, 1) - sackStats!(1, 0) = 0 Then
        Print
    End If

    Print "RUSHING YARDS PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print Using "###.#"; allTeamStats!(0, 2) / totalGames;
    Else
        Print Using "###.#"; (allTeamStats!(0, 2) + sackStats!(0, 1)) / totalGames;
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "###.#"; allTeamStats!(1, 2) / totalGames
    Else
        Print Using "###.#"; (allTeamStats!(1, 2) + sackStats!(1, 1)) / totalGames
    End If

    Print
    Color 14: Print "PASSING"
    Color 7

    Print "PASSES ATTEMPTED"; Tab(45); Using "####"; allTeamStats!(0, 3);
    Print Tab(65); Using "####"; allTeamStats!(1, 3)

    Print "PASSES COMPLETED"; Tab(45); Using "####"; allTeamStats!(0, 4);
    Print Tab(65); Using "####"; allTeamStats!(1, 4)

    Print "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; allTeamStats!(0, 5);
    Print Tab(65); Using "####"; allTeamStats!(1, 5)

    Print "PASS COMPLETION PERCENTAGE"; Tab(44);
    If allTeamStats!(0, 3) <> 0 Then
        Print Using "###.#"; allTeamStats!(0, 4) / allTeamStats!(0, 3) * 100;
    End If
    If allTeamStats!(1, 3) <> 0 Then
        Print Tab(64); Using "###.#"; allTeamStats!(1, 4) / allTeamStats!(1, 3) * 100
    End If
    If allTeamStats!(1, 3) = 0 Then
        Print
    End If

    Color 15
    Print "NET YARDS PASSING"; Tab(45); Using "####"; allTeamStats!(0, 6);
    Print Tab(65); Using "####"; allTeamStats!(1, 6)

    Color 7
    Print "YARDS PER PASS ATTEMPT"; Tab(45);
    If allTeamStats!(0, 3) <> 0 Then
        Print Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 3);
    End If
    If allTeamStats!(1, 3) <> 0 Then
        Print Tab(65); Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 3)
    End If
    If allTeamStats!(1, 3) = 0 Then
        Print
    End If

    Print "YARDS PER PASS COMPLETION"; Tab(45);
    If allTeamStats!(0, 4) <> 0 Then
        Print Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 4);
    End If
    If allTeamStats!(1, 4) <> 0 Then
        Print Tab(65); Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 4)
    End If
    If allTeamStats!(1, 4) = 0 Then
        Print
    End If

    Print "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; allTeamStats!(0, 6) / totalGames;
    Print Tab(64); Using "###.#"; allTeamStats!(1, 6) / totalGames

    Print
    Color 14: Print "PLAYS / YARDS"

    Color 7
    Print "FIRST DOWNS"; Tab(45); Using "####"; allTeamStats!(0, 0);: Print Tab(65); Using "####"; allTeamStats!(1, 0)
    Print "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(0, 1) + allTeamStats!(0, 3);
    Else
        Print Using "####"; allTeamStats!(0, 1) - sackStats!(0, 0) + allTeamStats!(0, 3);
    End If
    Print Tab(65);
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(1, 1) + allTeamStats!(1, 3)
    Else
        Print Using "####"; allTeamStats!(1, 1) - sackStats!(1, 0) + allTeamStats!(1, 3)
    End If

    Print "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print Using "###.#"; (allTeamStats!(0, 1) + allTeamStats!(0, 3)) / totalGames;
    Else
        Print Using "###.#"; (allTeamStats!(0, 1) - sackStats!(0, 0) + allTeamStats!(0, 3)) / totalGames;
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "###.#"; (allTeamStats!(1, 1) + allTeamStats!(1, 3)) / totalGames
    Else
        Print Using "###.#"; (allTeamStats!(1, 1) - sackStats!(1, 0) + allTeamStats!(1, 3)) / totalGames
    End If

    Color 15
    Print "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(0, 2) + allTeamStats!(0, 6);
    Else
        Print Using "####"; allTeamStats!(0, 2) + sackStats!(0, 1) + allTeamStats!(0, 6);
    End If
    Print Tab(65);
    If statFormat = 0 Then
        Print Using "####"; allTeamStats!(1, 2) + allTeamStats!(1, 6)
    Else
        Print Using "####"; allTeamStats!(1, 2) + sackStats!(1, 1) + allTeamStats!(1, 6)
    End If

    Color 7
    Print "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then
        Print Using "##.#"; (allTeamStats!(0, 2) + allTeamStats!(0, 6)) / (allTeamStats!(0, 1) + allTeamStats!(0, 3));
    Else
        Print Using "##.#"; (allTeamStats!(0, 2) + sackStats!(0, 1) + allTeamStats!(0, 6)) / (allTeamStats!(0, 1) - sackStats!(0, 0) + allTeamStats!(0, 3));
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "##.#"; (allTeamStats!(1, 2) + allTeamStats!(1, 6)) / (allTeamStats!(1, 1) + allTeamStats!(1, 3))
    Else
        Print Using "##.#"; (allTeamStats!(1, 2) + sackStats!(1, 1) + allTeamStats!(1, 6)) / (allTeamStats!(1, 1) - sackStats!(1, 0) + allTeamStats!(1, 3))
    End If

    Print "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print Using "###.#"; (allTeamStats!(0, 2) + allTeamStats!(0, 6)) / totalGames;
    Else
        Print Using "###.#"; (allTeamStats!(0, 2) + sackStats!(0, 1) + allTeamStats!(0, 6)) / totalGames;
    End If
    Print Tab(64);
    If statFormat = 0 Then
        Print Using "###.#"; (allTeamStats!(1, 2) + allTeamStats!(1, 6)) / totalGames
    Else
        Print Using "###.#"; (allTeamStats!(1, 2) + sackStats!(1, 1) + allTeamStats!(1, 6)) / totalGames
    End If

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 2: Color 12
    Print "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"

    Color 14: Print "RETURNS"
    Color 7

    Print "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print Using "###"; statsTeam!(22);: Print "/";: Print Using "####"; statsTeam!(23);
    Print Tab(61); Using "###"; statsOpp!(22);: Print "/";: Print Using "####"; statsOpp!(23)

    Print "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsTeam!(22) <> 0 Then
        Print Using "##.#"; statsTeam!(23) / statsTeam!(22);
    End If
    If statsOpp!(22) <> 0 Then
        Print Tab(65); Using "##.#"; statsOpp!(23) / statsOpp!(22)
    End If
    If statsOpp!(22) = 0 Then
        Print
    End If

    Print "KICKOFF RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsTeam!(22) / totalGames;: Print Tab(65); Using "##.#"; statsOpp!(22) / totalGames

    Print "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print Using "###"; statsTeam!(24);: Print "/";: Print Using "####"; statsTeam!(25);
    Print Tab(61); Using "###"; statsOpp!(24);: Print "/";: Print Using "####"; statsOpp!(25)

    Print "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsTeam!(24) <> 0 Then
        Print Using "##.#"; statsTeam!(25) / statsTeam!(24);
    End If
    If statsOpp!(24) <> 0 Then
        Print Tab(65); Using "##.#"; statsOpp!(25) / statsOpp!(24)
    End If
    If statsOpp!(24) = 0 Then
        Print
    End If

    Print "PUNT RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsTeam!(24) / totalGames;: Print Tab(65); Using "##.#"; statsOpp!(24) / totalGames

    Print "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print Using "###"; statsTeam!(26);: Print "/";: Print Using "####"; statsTeam!(27);
    Print Tab(61); Using "###"; statsOpp!(26);: Print "/";: Print Using "####"; statsOpp!(27)

    Print "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsTeam!(26) <> 0 Then
        Print Using "##.#"; statsTeam!(27) / statsTeam!(26);
    End If
    If statsOpp!(26) <> 0 Then
        Print Tab(65); Using "##.#"; statsOpp!(27) / statsOpp!(26)
    End If
    If statsOpp!(26) = 0 Then
        Print
    End If

    Print "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print Using "##.#"; statsTeam!(26) / totalGames;
    Print Tab(65); Using "##.#"; statsOpp!(26) / totalGames

    Print
    Color 14: Print "PUNTS"
    Color 7

    Print "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print Using "###"; allTeamStats!(0, 7);: Print "/";: Print Using "####"; allTeamStats!(0, 8);
    Print Tab(61); Using "###"; allTeamStats!(1, 7);: Print "/";: Print Using "####"; allTeamStats!(1, 8)

    Print "AVERAGE YARDS PER PUNT"; Tab(45);
    If allTeamStats!(0, 7) <> 0 Then
        Print Using "##.#"; allTeamStats!(0, 8) / allTeamStats!(0, 7);
    End If
    If allTeamStats!(1, 7) <> 0 Then
        Print Tab(65); Using "##.#"; allTeamStats!(1, 8) / allTeamStats!(1, 7)
    End If
    If allTeamStats!(1, 7) = 0 Then
        Print
    End If

    Print "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);: Print Using "##.#"; allTeamStats!(0, 7) / totalGames;: Print Tab(65); Using "##.#"; allTeamStats!(1, 7) / totalGames

    Print
    Color 14: Print "DEFENSE / PENALTIES"
    Color 7

    Print "FUMBLES / FUMBLES LOST"; Tab(41);
    Print Using "###"; allTeamStats!(0, 9);: Print "/";: Print Using "####"; allTeamStats!(0, 10);
    Print Tab(61); Using "###"; allTeamStats!(1, 9);: Print "/";: Print Using "####"; allTeamStats!(1, 10)

    Print "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print Using "###"; allTeamStats!(0, 11);: Print "/";: Print Using "####"; allTeamStats!(0, 12);
    Print Tab(61); Using "###"; allTeamStats!(1, 11);: Print "/";: Print Using "####"; allTeamStats!(1, 12)

    Print "AVERAGE YARDS PER PENALTY"; Tab(45);
    If allTeamStats!(0, 11) <> 0 Then
        Print Using "##.#"; allTeamStats!(0, 12) / allTeamStats!(0, 11);
    End If
    If allTeamStats!(1, 11) <> 0 Then
        Print Tab(65); Using "##.#"; allTeamStats!(1, 12) / allTeamStats!(1, 11)
    End If
    If allTeamStats!(1, 11) = 0 Then
        Print
    End If

    Print "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print Using "##.#"; allTeamStats!(0, 11) / totalGames;: Print "/";: Print Using "##.#"; allTeamStats!(0, 12) / totalGames;
    Print Tab(60); Using "##.#"; allTeamStats!(1, 11) / totalGames;: Print "/";: Print Using "##.#"; allTeamStats!(1, 12) / totalGames

    Print "SAFETIES"; Tab(45);: Print Using "####"; statsTeam!(21);: Print Tab(65); Using "####"; statsOpp!(21)

    Print "TIMES SACKED / YARDS LOST"; Tab(41);
    Print Using "###"; sackStats!(0, 0);: Print "/";: Print Using "####"; sackStats!(0, 1);
    Print Tab(61); Using "###"; sackStats!(1, 0);: Print "/";: Print Using "####"; sackStats!(1, 1)

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
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

                If rbName$(I1) = qbName$(I2) Then
                    allPlayerStats!(I1, 0) = allPlayerStats!(I1, 0) + allPlayerStats!(I2 + 10, 0)
                    allPlayerStats!(I1, 1) = allPlayerStats!(I1, 1) + allPlayerStats!(I2 + 10, 1)
                    allPlayerStats!(I1, 2) = allPlayerStats!(I1, 2) + allPlayerStats!(I2 + 10, 2)
                    allPlayerStats!(I2 + 10, 0) = 0
                    allPlayerStats!(I2 + 10, 1) = 0
                    allPlayerStats!(I2 + 10, 2) = 0
                End If

            Next I2

        Next I1

    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13

        If allPlayerStats!(I1, 0) <> 0 Then

            DM% = DM% + 1
            Color 7

            If DM% / 2 = Int(DM% / 2) Then Color 15
            If I1 <= 9 Then Print rbName$(I1);
            If I1 > 9 Then Print qbName$(I1 - 10);

            Print Tab(18); Using "####  ####   ##.#      ###.#    ##    "; allPlayerStats!(I1, 0); allPlayerStats!(I1, 1); allPlayerStats!(I1, 1) / allPlayerStats!(I1, 0); allPlayerStats!(I1, 1) / totalGames; allPlayerStats!(I1, 2);

            If I1 > 9 Then
                Print
            Else
                Print Using "###"; statsLRush!(I1);: Print statsLRush$(I1, 0); Tab(64); statsLRush$(I1, 1)
            End If

        End If

    Next I1

    Color 3
    Print
    Print teamName$; Tab(18);

    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(0, 1); allTeamStats!(0, 2); allTeamStats!(0, 2) / allTeamStats!(0, 1); allTeamStats!(0, 2) / totalGames; statsTeam!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(0, 1) - sackStats!(0, 0); allTeamStats!(0, 2) + sackStats!(0, 1); (allTeamStats!(0, 2) + sackStats!(0, 1)) / (allTeamStats!(0, 1) - sackStats!(0, 0)); (allTeamStats!(0, 2) + sackStats!(0, 1)) / totalGames; statsTeam!(15)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(1, 1); allTeamStats!(1, 2); allTeamStats!(1, 2) / allTeamStats!(1, 1); allTeamStats!(1, 2) / totalGames; statsOpp!(15)
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(1, 1) - sackStats!(1, 0); allTeamStats!(1, 2) + sackStats!(1, 1); (allTeamStats!(1, 2) + sackStats!(1, 1)) / (allTeamStats!(1, 1) - sackStats!(1, 0)); (allTeamStats!(1, 2) + sackStats!(1, 1)) / totalGames; statsOpp!(15)
    End If

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5

        If allPlayerStats!(I1, 6) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print wrName$(I1); Tab(18);
            Print Using "####  ####   "; allPlayerStats!(I1, 6); allPlayerStats!(I1, 7);

            If allPlayerStats!(I1, 6) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; allPlayerStats!(I1, 7) / allPlayerStats!(I1, 6);
            End If

            Print Using "      ###.#    ##    "; allPlayerStats!(I1, 7) / totalGames; allPlayerStats!(I1, 8);
            Print Using "###"; statsLRec!(I1 + 10);: Print statsLRec$(I1 + 10, 0); Tab(64); statsLRec$(I1 + 10, 1)

        End If

    Next I1

    For I1 = 0 To 9

        If allPlayerStats!(I1, 3) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print rbName$(I1); Tab(18);
            Print Using "####  ####   "; allPlayerStats!(I1, 3); allPlayerStats!(I1, 4);

            If allPlayerStats!(I1, 3) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; allPlayerStats!(I1, 4) / allPlayerStats!(I1, 3);
            End If

            Print Using "      ###.#    ##    "; allPlayerStats!(I1, 4) / totalGames; allPlayerStats!(I1, 5);
            Print Using "###"; statsLRec!(I1);: Print statsLRec$(I1, 0); Tab(64); statsLRec$(I1, 1)

        End If

    Next I1

    Print
    Color 3
    Print teamName$; Tab(18);

    Print Using "####  ####   "; allTeamStats!(0, 4); allTeamStats!(0, 6);
    If allTeamStats!(0, 4) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 4);
    End If
    Print Using "      ###.#    ##"; allTeamStats!(0, 6) / totalGames; statsTeam!(17)

    Print "OPPONENT TOTALS"; Tab(18);
    Print Using "####  ####   "; allTeamStats!(1, 4); allTeamStats!(1, 6);
    If allTeamStats!(1, 4) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 4);
    End If
    Print Using "      ###.#    ##"; allTeamStats!(1, 6) / totalGames; statsOpp!(17)

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD  INT  INT%  RATING"

    For I1 = 0 To 3

        If allPlayerStats!(I1, 9) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            'Att / Comp / Comp % / Yds
            Print qbName$(I1); Tab(18); Using "####  ### ###.# #### "; allPlayerStats!(I1, 9); allPlayerStats!(I1, 10); allPlayerStats!(I1, 10) / allPlayerStats!(I1, 9) * 100; allPlayerStats!(I1, 12);

            'Yds / Att
            If allPlayerStats!(I1, 9) = 0 Then
                Print "   --";
            Else
                Print Using "##.#"; allPlayerStats!(I1, 12) / allPlayerStats!(I1, 9);
            End If

            ' Yds / Catch
            If allPlayerStats!(I1, 10) = 0 Then
                Print "   --";
            Else
                Print Using " ##.#"; allPlayerStats!(I1, 12) / allPlayerStats!(I1, 10);
            End If

            'TD / INT / INT %
            Print Using " ###.# ### ### ###.#   "; allPlayerStats!(I1, 12) / totalGames; allPlayerStats!(I1, 13); allPlayerStats!(I1, 11); allPlayerStats!(I1, 11) / allPlayerStats!(I1, 9) * 100;

            'QB Rating
            awayTies = QBRating!(I1, statFormat)
            Print Using "###.#"; awayTies

        End If

    Next I1

    Print

    Color 3
    'Team Totals
    Print teamName$; Tab(18);

    'Att / Comp
    Print Using "####  ### "; allTeamStats!(0, 3); allTeamStats!(0, 4);

    'Comp %
    If allTeamStats!(0, 3) = 0 Then
        Print "   --";
    Else
        Print Using "###.#"; allTeamStats!(0, 4) / allTeamStats!(0, 3) * 100;
    End If

    'Yds
    Print Using " #### "; allTeamStats!(0, 6);

    'Yds / Att
    If allTeamStats!(0, 3) = 0 Then
        Print " --";
    Else
        Print Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 3);
    End If

    'Yds / Catch
    If allTeamStats!(0, 4) = 0 Then
        Print "   --";
    Else
        Print Using " ##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 4);
    End If

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; allTeamStats!(0, 6) / totalGames; statsTeam!(17); allTeamStats!(0, 5);

    'INT %
    If allTeamStats!(0, 3) = 0 Then
        Print "   --"
    Else
        Print Using "  #.#"; allTeamStats!(0, 5) / allTeamStats!(0, 3) * 100
    End If

    Print "OPPONENT TOTALS"; Tab(18);

    'Att / Comp
    Print Using "####  ### "; allTeamStats!(1, 3); allTeamStats!(1, 4);

    'Comp %
    If allTeamStats!(1, 3) = 0 Then
        Print "   --"
    Else
        Print Using "###.#"; allTeamStats!(1, 4) / allTeamStats!(1, 3) * 100;
    End If

    'Yds
    Print Using " #### "; allTeamStats!(1, 6);

    'Yds / Att
    If allTeamStats!(1, 3) = 0 Then
        Print " --";
    Else
        Print Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 3);
    End If

    'Yds / Catch
    If allTeamStats!(1, 4) = 0 Then
        Print "   --";
    Else
        Print Using " ##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 4);
    End If

    'Yds / Gm, TD, INT
    Print Using " ###.# ### ### ###.#     "; allTeamStats!(1, 6) / totalGames; statsOpp!(17); allTeamStats!(1, 5);

    'INT %
    If allTeamStats!(1, 3) = 0 Then
        Print "   --"
    Else
        Print Using "  #.#"; allTeamStats!(1, 5) / allTeamStats!(1, 3) * 100
    End If

    Print
    Print
    Color 12

    Print "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If allPlayerStats!(I1, 9) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print qbName$(I1); Tab(22); Using "###"; statsLPass!(I1);: Print statsLPass$(I1, 0); Tab(32); statsLPass$(I1, 1)
        End If
    Next

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If allPlayerStats!(I1, 14) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print krName$(I1); Tab(18);
            Print Using "####  ####   "; allPlayerStats!(I1, 14); allPlayerStats!(I1, 15);
            If allPlayerStats!(I1, 14) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; allPlayerStats!(I1, 15) / allPlayerStats!(I1, 14);
            End If
            Print Using "      ###.#    ##    "; allPlayerStats!(I1, 15) / totalGames; allPlayerStats!(I1, 16);
            Print Using "###"; statsLKR!(I1);: Print statsLKR$(I1, 0); Tab(64); statsLKR$(I1, 1)
        End If
    Next I1

    Print
    Color 3
    Print teamName$; Tab(18);
    If statsTeam!(22) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsTeam!(22); statsTeam!(23); statsTeam!(23) / statsTeam!(22); statsTeam!(23) / totalGames; statsTeam!(18)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statsOpp!(22) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsOpp!(22); statsOpp!(23); statsOpp!(23) / statsOpp!(22); statsOpp!(23) / totalGames; statsOpp!(18)
    End If

    Print
    Print
    Print
    Color 12
    Print "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 3 To 5

        If allPlayerStats!(I1, 14) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print prName$(I1 - 3); Tab(18);
            Print Using "####  ####   "; allPlayerStats!(I1, 14); allPlayerStats!(I1, 15);

            If allPlayerStats!(I1, 14) = 0 Then
                Print "  --";
            Else
                Print Using "##.#"; allPlayerStats!(I1, 15) / allPlayerStats!(I1, 14);
            End If

            Print Using "      ###.#    ##    "; allPlayerStats!(I1, 15) / totalGames; allPlayerStats!(I1, 16);
            Print Using "###"; statsLPR!(I1 - 3);: Print statsLPR$(I1 - 3, 0); Tab(64); statsLPR$(I1 - 3, 1)

        End If

    Next I1

    Print
    Color 3
    Print teamName$; Tab(18);
    If statsTeam!(24) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsTeam!(24); statsTeam!(25); statsTeam!(25) / statsTeam!(24); statsTeam!(25) / totalGames; statsTeam!(19)
    End If

    Print "OPPONENT TOTALS"; Tab(18);
    If statsOpp!(24) = 0 Then
        Print "   0     0   --.-        -.-     0"
    Else
        Print Using "####  ####   ##.#      ###.#    ##"; statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); statsOpp!(25) / totalGames; statsOpp!(19)
    End If

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1
    Color 12
    Print "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Color 15
    Print punterName$(0); Tab(18); Using "####  ####    "; allTeamStats!(0, 7); allTeamStats!(0, 8);
    If allTeamStats!(0, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; allTeamStats!(0, 8) / allTeamStats!(0, 7);
    End If
    Print Using "    ##.#      ##     ##      ##"; allTeamStats!(0, 7) / totalGames; statsTeam!(31); statsTeam!(30); statsTeam!(28)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; allTeamStats!(1, 7); allTeamStats!(1, 8);
    If allTeamStats!(1, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; allTeamStats!(1, 8) / allTeamStats!(1, 7);
    End If
    Print Using "    ##.#      ##     ##"; allTeamStats!(1, 7) / totalGames; statsOpp!(31); statsOpp!(30)

    Print
    Print
    Print
    Color 12
    Print "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Color 15
    Print teamName$; Tab(18); Using "####  ####    "; allTeamStats!(0, 7); allTeamStats!(0, 8);
    If allTeamStats!(0, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; allTeamStats!(0, 8) / allTeamStats!(0, 7);
    End If

    Print Using "    ###       ####        ##.#     ##.#"; statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); (allTeamStats!(0, 8) - statsOpp!(25)) / allTeamStats!(0, 7)

    Print "OPPONENT TOTALS"; Tab(18); Using "####  ####    "; allTeamStats!(1, 7); allTeamStats!(1, 8);
    If allTeamStats!(1, 7) = 0 Then
        Print "  --";
    Else
        Print Using "##.#"; allTeamStats!(1, 8) / allTeamStats!(1, 7);
    End If

    '                Ret       Yds         Avg      Net Avg
    If statsTeam!(24) > 0 Then
        oppPuntAvg! = statsTeam!(25) / statsTeam!(24)
    Else
        oppPuntAvg! = 0
    End If
    Print Using "    ###       ####        ##.#     ##.#"; statsTeam!(24); statsTeam!(25); oppPuntAvg!; (allTeamStats!(1, 8) - statsTeam!(25)) / allTeamStats!(1, 7)

    Print
    Print
    Print
    Color 12
    Print "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"
    Color 15

    testFGA = allPlayerStats!(4, 17)
    testFGM = allPlayerStats!(3, 17)
    testEPA = allPlayerStats!(6, 17)
    testEPM = allPlayerStats!(5, 17)

    If allPlayerStats!(4, 17) <> 0 And allPlayerStats!(6, 17) <> 0 Then

        Print pkName$(0);
        Print Tab(16);

        If allPlayerStats!(6, 17) <> 0 Then
            'Extra Points
            Print Using "### ### ###.# "; allPlayerStats!(5, 17); allPlayerStats!(6, 17); allPlayerStats!(5, 17) / allPlayerStats!(6, 17) * 100;
        End If

        'Field Goals
        If allPlayerStats!(4, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; allPlayerStats!(3, 17); allPlayerStats!(4, 17); allPlayerStats!(3, 17) / allPlayerStats!(4, 17) * 100; statsLFG!(0); statsFGM!(0, 0);
            Print "-";: Print Using "## ##"; statsFGA!(0, 0); statsFGM!(0, 1);
            Print "-";: Print Using "## ##"; statsFGA!(0, 1); statsFGM!(0, 2);
            Print "-";: Print Using "## ##"; statsFGA!(0, 2); statsFGM!(0, 3);
            Print "-";: Print Using "## ##"; statsFGA!(0, 3); statsFGM!(0, 4);
            Print "-";: Print Using "##"; statsFGA!(0, 4)
        End If

    End If

    If allPlayerStats!(8, 17) <> 0 And allPlayerStats!(10, 17) <> 0 Then

        Print pkName$(1); Tab(16);

        If allPlayerStats!(10, 17) <> 0 Then
            Print Using "### ### ###.# "; allPlayerStats!(9, 17); allPlayerStats!(10, 17); allPlayerStats!(9, 17) / allPlayerStats!(10, 17) * 100;
        End If

        If allPlayerStats!(8, 17) = 0 Then
            Print
        Else
            Print Tab(30);
            Print Using "### ### ###.# ## ##"; allPlayerStats!(7, 17); allPlayerStats!(8, 17); allPlayerStats!(7, 17) / allPlayerStats!(8, 17) * 100; statsLFG!(1); statsFGM!(1, 0);
            Print "-";: Print Using "## ##"; statsFGA!(1, 0); statsFGM!(1, 1);
            Print "-";: Print Using "## ##"; statsFGA!(1, 1); statsFGM!(1, 2);
            Print "-";: Print Using "## ##"; statsFGA!(1, 2); statsFGM!(1, 3);
            Print "-";: Print Using "## ##"; statsFGA!(1, 3); statsFGM!(1, 4);
            Print "-";: Print Using "##"; statsFGA!(1, 4)
        End If

    End If

    Print

    NOONE:
    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    For I1 = 0 To 9

        If statsGI(I1, 0) <> 0 Then

            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15

            Print intName$(I1); Tab(18);

            '            Ret   Yds
            Print Using "####  ####   "; statsGI(I1, 0); statsGI(I1, 1);

            'Yds / Ret
            If statsGI(I1, 0) = 0 Then
                Print " --";
            Else
                Print Using "##.#"; statsGI(I1, 1) / statsGI(I1, 0);
            End If

            'Yds / Gm, TD
            Print Using "      ###.#    ##    "; statsGI(I1, 1) / totalGames; statsGI(I1, 2);

            'Long Ret / Against
            Print Using "###"; statsLInt!(I1);: Print statsLInt$(I1, 0); Tab(64); statsLInt$(I1, 1)

        End If

    Next I1

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Color , 0
    Cls

    Call TITLE_STATS

    Locate 3, 1: Color 12
    Print "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If statsGS(I1, 0) <> 0 Then
            DM% = DM% + 1
            Color 7
            If DM% / 2 = Int(DM% / 2) Then Color 15
            Print sackName$(I1); Tab(20); Using "###            ###"; statsGS(I1, 0); statsGS(I1, 1)
        End If
    Next

    Color 15
    Locate LAST_ROW - 2, 27
    Print "PRESS ANY KEY TO CONTINUE"
    I$ = GetKeyPress

End Sub


'----------------------------------------
'       StatsToReport Subroutine
'----------------------------------------
'This subroutines uses attendance data
'for each type of game to calculate
'different categories of attendance
Sub CalcAttendanceCategories (totalGames)

    Shared gameAttendance&()
    Shared locIndicator$()

    Shared HD&, AD&, ND&, TD&
    Shared HT&, CT&, NT&, TT&
    Shared HA&, AA&, NA&, TA&

    'H = Home; A = Away; N = Neutral; T = Total
    'D = Dates; T = Total; A = Average
    HD& = 0: AD& = 0: ND& = 0: TD& = 0
    HT& = 0: CT& = 0: NT& = 0: TT& = 0
    HA& = 0: AA& = 0: NA& = 0: TA& = 0

    For I = 1 To totalGames

        If Left$(locIndicator$(I), 1) = "H" Then
            HD& = HD& + 1
            HT& = HT& + gameAttendance&(I)
        End If

        If Left$(locIndicator$(I), 1) = "A" Then
            AD& = AD& + 1
            CT& = CT& + gameAttendance&(I)
        End If

        If Left$(locIndicator$(I), 1) = "X" Then
            ND& = ND& + 1
            NT& = NT& + gameAttendance&(I)
        End If

        TD& = TD& + 1
        TT& = TT& + gameAttendance&(I)

    Next

    If HD& <> 0 Then HA& = HT& / HD&
    If AD& <> 0 Then AA& = CT& / AD&
    If ND& <> 0 Then NA& = NT& / ND&
    If TD& <> 0 Then TA& = TT& / TD&

End Sub


'----------------------------------------
'       StatsToReport Subroutine
'----------------------------------------
'This subroutine formats and outputs the
'selected team's stats to the default printer
Sub StatsToReport (printDest, destFile$, teamName$, totalGames, statFormat, teamPower, SS)

    Shared statsFGA!(), statsFGM!(), gameAttendance&(), statsGI(), statsGS()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsLPR!(), sackStats!(), statsTeam!(), statsOpp!(), statsPlayers!()

    Shared allPlayerStats!(), allTeamStats!()

    Shared teamScore(), oppScore(), scoreTotals()

    Shared gameSite$(), oppName$()
    Shared intName$(), sackName$(), locIndicator$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()

    Shared statsLRec$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Shared HD&, AD&, ND&, TD&
    Shared HT&, CT&, NT&, TT&
    Shared HA&, AA&, NA&, TA&

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, "GAME-BY-GAME REPORT FOR "; teamName$
    Print #1,
    Print #1, "OPPONENT            SCORE  W/L  ATTEND   SITE                         RECORD"

    For I = 1 To totalGames

        Print #1, oppName$(I); Tab(19); Using "###"; teamScore(I);
        Print #1, "-";: Print #1, Using "###   "; oppScore(I);

        If teamScore(I) > oppScore(I) Then
            totalWins = totalWins + 1
            Print #1, "W";
        End If

        If teamScore(I) < oppScore(I) Then
            totalLosses = totalLosses + 1
            Print #1, "L";
        End If

        If teamScore(I) = oppScore(I) Then
            tiedGames = tiedGames + 1
            Print #1, "T";
        End If

        Print #1, Tab(31);
        Print #1, Using "####,###   "; gameAttendance&(I);
        Print #1, locIndicator$(I); "  "; Left$(gameSite$(I), 24);

        Print #1, Tab(69);
        Print #1, "(";
        Print #1, Using "##"; totalWins;: Print #1, "-";: Print #1, Using "##"; totalLosses;: Print #1, "-";: Print #1, Using "##"; tiedGames;
        Print #1, ")"

        scoreTotals(0) = scoreTotals(0) + teamScore(I)
        scoreTotals(1) = scoreTotals(1) + oppScore(I)

    Next I

    Print #1,
    If statFormat = 0 Then
        Print #1, Using "POWER RATING ###.#    STRENGTH OF SCHEDULE ###.#"; teamPower; SS
    End If

    scoreTotals(0) = 0
    scoreTotals(1) = 0
    totalWins = 0
    totalLosses = 0
    tiedGames = 0

    Print #1,
    Print #1, "CUMULATIVE SCORE BY QUARTERS   1    2    3    4    TOT"
    Print #1, teamName$; Tab(30); Using " ###  ###  ###  ###   ###"; statsTeam!(32); statsTeam!(33); statsTeam!(34); statsTeam!(35); statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)
    Print #1, "OPPONENTS"; Tab(30); Using " ###  ###  ###  ###   ###"; statsOpp!(32); statsOpp!(33); statsOpp!(34); statsOpp!(35); statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)
    Print #1,

    Print #1, "AVERAGE SCORE BY QUARTERS       1     2     3     4     TOT"
    Print #1, teamName$; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsTeam!(32) / totalGames; statsTeam!(33) / totalGames; statsTeam!(34) / totalGames; statsTeam!(35) / totalGames; (statsTeam!(32) + statsTeam!(33) + statsTeam!(34) + statsTeam!(35) + statsTeam!(36)) / totalGames
    Print #1, "OPPONENTS"; Tab(30); Using " ##.#  ##.#  ##.#  ##.#   ##.#"; statsOpp!(32) / totalGames; statsOpp!(33) / totalGames; statsOpp!(34) / totalGames; statsOpp!(35) / totalGames; (statsOpp!(32) + statsOpp!(33) + statsOpp!(34) + statsOpp!(35) + statsTeam!(36)) / totalGames
    Print #1,

    Call CalcAttendanceCategories(totalGames)

    Print #1, "ATTENDANCE    DATES      TOTAL    AVERAGE"
    Print #1, Using "HOME            ##    #####,###   ###,###"; HD&; HT&; HA&
    Print #1, Using "AWAY            ##    #####,###   ###,###"; AD&; CT&; AA&
    Print #1, Using "NEUTRAL         ##    #####,###   ###,###"; ND&; NT&; NA&
    Print #1, Using "TOTAL           ##    #####,###   ###,###"; TD&; TT&; TA&
    Print #1, Chr$(12)

    Print #1, "TEAM STATISTICS CATEGORY"; Tab(41); teamName$; Tab(61); "OPPONENT TOTALS"
    Print #1, "RUSHING"
    Print #1, "RUSHING ATTEMPTS"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(0, 1);
    Else
        Print #1, Using "####"; allTeamStats!(0, 1) - sackStats!(0, 0);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(1, 1)
    Else
        Print #1, Using "####"; allTeamStats!(1, 1) - sackStats!(1, 0)
    End If

    Print #1, "NET RUSHING YARDAGE"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(0, 2);
    Else
        Print #1, Using "####"; allTeamStats!(0, 2) + sackStats!(0, 1);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(1, 2)
    Else
        Print #1, Using "####"; allTeamStats!(1, 2) + sackStats!(1, 1)
    End If

    Print #1, "YARDS PER RUSH"; Tab(45);
    If statFormat = 0 And allTeamStats!(0, 1) <> 0 Then
        Print #1, Using "##.#"; allTeamStats!(0, 2) / allTeamStats!(0, 1);
    End If
    If statFormat <> 0 And allTeamStats!(0, 1) - sackStats!(0, 0) <> 0 Then
        Print #1, Using "##.#"; (allTeamStats!(0, 2) + sackStats!(0, 1)) / (allTeamStats!(0, 1) - sackStats!(0, 0));
    End If

    Print #1, Tab(65);

    If statFormat = 0 And allTeamStats!(1, 1) <> 0 Then
        Print #1, Using "##.#"; allTeamStats!(1, 2) / allTeamStats!(1, 1)
    End If
    If statFormat <> 0 And allTeamStats!(1, 1) - sackStats!(1, 0) <> 0 Then
        Print #1, Using "##.#"; (allTeamStats!(1, 2) + sackStats!(1, 1)) / (allTeamStats!(1, 1) - sackStats!(1, 0))
    End If
    If statFormat = 0 And allTeamStats!(1, 1) = 0 Or statFormat <> 0 And allTeamStats!(1, 1) - sackStats!(1, 0) = 0 Then
        Print #1,
    End If

    Print #1, "RUSHING YARDS PER GAME"; Tab(44);

    If statFormat = 0 Then
        Print #1, Using "###.#"; allTeamStats!(0, 2) / totalGames;
    Else
        Print #1, Using "###.#"; (allTeamStats!(0, 2) + sackStats!(0, 1)) / totalGames;
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "###.#"; allTeamStats!(1, 2) / totalGames
    Else
        Print #1, Using "###.#"; (allTeamStats!(1, 2) + sackStats!(1, 1)) / totalGames
    End If

    Print #1,
    Print #1, "PASSING"
    Print #1, "PASSES ATTEMPTED"; Tab(45); Using "####"; allTeamStats!(0, 3);: Print #1, Tab(65); Using "####"; allTeamStats!(1, 3)
    Print #1, "PASSES COMPLETED"; Tab(45); Using "####"; allTeamStats!(0, 4);: Print #1, Tab(65); Using "####"; allTeamStats!(1, 4)
    Print #1, "PASSES HAD INTERCEPTED"; Tab(45); Using "####"; allTeamStats!(0, 5);: Print #1, Tab(65); Using "####"; allTeamStats!(1, 5)
    Print #1, "PASS COMPLETION PERCENTAGE"; Tab(44);

    If allTeamStats!(0, 3) <> 0 Then
        Print #1, Using "###.#"; allTeamStats!(0, 4) / allTeamStats!(0, 3) * 100;
    End If
    If allTeamStats!(1, 3) <> 0 Then
        Print #1, Tab(64); Using "###.#"; allTeamStats!(1, 4) / allTeamStats!(1, 3) * 100
    Else
        Print #1,
    End If

    Print #1, "NET YARDS PASSING"; Tab(45); Using "####"; allTeamStats!(0, 6);
    Print #1, Tab(65); Using "####"; allTeamStats!(1, 6)

    Print #1, "YARDS PER PASS ATTEMPT"; Tab(45);
    If allTeamStats!(0, 3) <> 0 Then
        Print #1, Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 3);
    End If
    If allTeamStats!(1, 3) <> 0 Then
        Print #1, Tab(65); Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 3)
    Else
        Print #1,
    End If

    Print #1, "YARDS PER PASS COMPLETION"; Tab(45);
    If allTeamStats!(0, 4) <> 0 Then
        Print #1, Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 4);
    End If
    If allTeamStats!(1, 4) <> 0 Then
        Print #1, Tab(65); Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 4)
    Else
        Print #1,
    End If

    Print #1, "PASSING YARDS PER GAME"; Tab(44); Using "###.#"; allTeamStats!(0, 6) / totalGames;
    Print #1, Tab(64); Using "###.#"; allTeamStats!(1, 6) / totalGames
    Print #1,

    Print #1, "PLAYS / YARDS"
    Print #1, "FIRST DOWNS"; Tab(45); Using "####"; allTeamStats!(0, 0);
    Print #1, Tab(65); Using "####"; allTeamStats!(1, 0)
    Print #1, "TOTAL PLAYS"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(0, 1) + allTeamStats!(0, 3);
    Else
        Print #1, Using "####"; allTeamStats!(0, 1) - sackStats!(0, 0) + allTeamStats!(0, 3);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(1, 1) + allTeamStats!(1, 3)
    Else
        Print #1, Using "####"; allTeamStats!(1, 1) - sackStats!(1, 0) + allTeamStats!(1, 3)
    End If

    Print #1, "TOTAL PLAYS PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (allTeamStats!(0, 1) + allTeamStats!(0, 3)) / totalGames;
    Else
        Print #1, Using "###.#"; (allTeamStats!(0, 1) - sackStats!(0, 0) + allTeamStats!(0, 3)) / totalGames;
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (allTeamStats!(1, 1) + allTeamStats!(1, 3)) / totalGames
    Else
        Print #1, Using "###.#"; (allTeamStats!(1, 1) - sackStats!(1, 0) + allTeamStats!(1, 3)) / totalGames
    End If

    Print #1, "TOTAL NET YARDS"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(0, 2) + allTeamStats!(0, 6);
    Else
        Print #1, Using "####"; allTeamStats!(0, 2) + sackStats!(0, 1) + allTeamStats!(0, 6);
    End If
    Print #1, Tab(65);
    If statFormat = 0 Then
        Print #1, Using "####"; allTeamStats!(1, 2) + allTeamStats!(1, 6)
    Else
        Print #1, Using "####"; allTeamStats!(1, 2) + sackStats!(1, 1) + allTeamStats!(1, 6)
    End If

    Print #1, "YARDS GAINED PER PLAY"; Tab(45);
    If statFormat = 0 Then
        Print #1, Using "##.#"; (allTeamStats!(0, 2) + allTeamStats!(0, 6)) / (allTeamStats!(0, 1) + allTeamStats!(0, 3));
    Else
        Print #1, Using "##.#"; (allTeamStats!(0, 2) + sackStats!(0, 1) + allTeamStats!(0, 6)) / (allTeamStats!(0, 1) - sackStats!(0, 0) + allTeamStats!(0, 3));
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "##.#"; (allTeamStats!(1, 2) + allTeamStats!(1, 6)) / (allTeamStats!(1, 1) + allTeamStats!(1, 3))
    Else
        Print #1, Using "##.#"; (allTeamStats!(1, 2) + sackStats!(1, 1) + allTeamStats!(1, 6)) / (allTeamStats!(1, 1) - sackStats!(1, 0) + allTeamStats!(1, 3))
    End If

    Print #1, "YARDS GAINED PER GAME"; Tab(44);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (allTeamStats!(0, 2) + allTeamStats!(0, 6)) / totalGames;
    Else
        Print #1, Using "###.#"; (allTeamStats!(0, 2) + sackStats!(0, 1) + allTeamStats!(0, 6)) / totalGames;
    End If
    Print #1, Tab(64);
    If statFormat = 0 Then
        Print #1, Using "###.#"; (allTeamStats!(1, 2) + allTeamStats!(1, 6)) / totalGames
    Else
        Print #1, Using "###.#"; (allTeamStats!(1, 2) + sackStats!(1, 1) + allTeamStats!(1, 6)) / totalGames
    End If

    Print #1,
    Print #1, "RETURNS"
    Print #1, "KICKOFF RETURNS / KICKOFF RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsTeam!(22);: Print #1, "/";: Print #1, Using "####"; statsTeam!(23);
    Print #1, Tab(61); Using "###"; statsOpp!(22);: Print #1, "/";: Print #1, Using "####"; statsOpp!(23)

    Print #1, "AVERAGE YARDAGE PER KICKOFF RETURN"; Tab(45);
    If statsTeam!(22) <> 0 Then
        Print #1, Using "##.#"; statsTeam!(23) / statsTeam!(22);
    End If
    If statsOpp!(22) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsOpp!(23) / statsOpp!(22)
    Else
        Print #1,
    End If

    Print #1, "KICKOFF RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsTeam!(22) / totalGames;
    Print #1, Tab(65); Using "##.#"; statsOpp!(22) / totalGames

    Print #1, "PUNT RETURNS / PUNT RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsTeam!(24);: Print #1, "/";: Print #1, Using "####"; statsTeam!(25);
    Print #1, Tab(61); Using "###"; statsOpp!(24);: Print #1, "/";: Print #1, Using "####"; statsOpp!(25)

    Print #1, "AVERAGE YARDAGE PER PUNT RETURN"; Tab(45);
    If statsTeam!(24) <> 0 Then
        Print #1, Using "##.#"; statsTeam!(25) / statsTeam!(24);
    End If
    If statsOpp!(24) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsOpp!(25) / statsOpp!(24)
    Else
        Print #1,
    End If

    Print #1, "PUNT RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsTeam!(24) / totalGames;
    Print #1, Tab(65); Using "##.#"; statsOpp!(24) / totalGames

    Print #1, "INTERCEPTION RETURNS / RETURN YARDS"; Tab(41);
    Print #1, Using "###"; statsTeam!(26);: Print #1, "/";: Print #1, Using "####"; statsTeam!(27);
    Print #1, Tab(61); Using "###"; statsOpp!(26);: Print #1, "/";: Print #1, Using "####"; statsOpp!(27)

    Print #1, "AVERAGE YARDAGE PER RETURN"; Tab(45);
    If statsTeam!(26) <> 0 Then
        Print #1, Using "##.#"; statsTeam!(27) / statsTeam!(26);
    End If
    If statsOpp!(26) <> 0 Then
        Print #1, Tab(65); Using "##.#"; statsOpp!(27) / statsOpp!(26)
    Else
        Print #1,
    End If

    Print #1, "INTERCEPTION RETURNS PER GAME"; Tab(45);
    Print #1, Using "##.#"; statsTeam!(26) / totalGames;
    Print #1, Tab(65); Using "##.#"; statsOpp!(26) / totalGames

    Print #1,
    Print #1, "PUNTS"
    Print #1, "PUNTS / TOTAL PUNT YARDAGE"; Tab(41);
    Print #1, Using "###"; allTeamStats!(0, 7);: Print #1, "/";: Print #1, Using "####"; allTeamStats!(0, 8);
    Print #1, Tab(61); Using "###"; allTeamStats!(1, 7);: Print #1, "/";: Print #1, Using "####"; allTeamStats!(1, 8)

    Print #1, "AVERAGE YARDS PER PUNT"; Tab(45);
    If allTeamStats!(0, 7) <> 0 Then
        Print #1, Using "##.#"; allTeamStats!(0, 8) / allTeamStats!(0, 7);
    End If
    If allTeamStats!(1, 7) <> 0 Then
        Print #1, Tab(65); Using "##.#"; allTeamStats!(1, 8) / allTeamStats!(1, 7)
    Else
        Print #1,
    End If

    Print #1, "AVERAGE NUMBER OF PUNTS PER GAME"; Tab(45);
    Print #1, Using "##.#"; allTeamStats!(0, 7) / totalGames;
    Print #1, Tab(65); Using "##.#"; allTeamStats!(1, 7) / totalGames

    Print #1,
    Print #1, "DEFENSE / PENALTIES"
    Print #1, "FUMBLES / FUMBLES LOST"; Tab(41);
    Print #1, Using "###"; allTeamStats!(0, 9);: Print #1, "/";: Print #1, Using "####"; allTeamStats!(0, 10);
    Print #1, Tab(61); Using "###"; allTeamStats!(1, 9);: Print #1, "/";: Print #1, Using "####"; allTeamStats!(1, 10)

    Print #1, "PENALTIES / YARDS PENALIZED"; Tab(41);
    Print #1, Using "###"; allTeamStats!(0, 11);: Print #1, "/";: Print #1, Using "####"; allTeamStats!(0, 12);
    Print #1, Tab(61); Using "###"; allTeamStats!(1, 11);: Print #1, "/";: Print #1, Using "####"; allTeamStats!(1, 12)

    Print #1, "AVERAGE YARDS PER PENALTY"; Tab(45);
    If allTeamStats!(0, 11) <> 0 Then
        Print #1, Using "##.#"; allTeamStats!(0, 12) / allTeamStats!(0, 11);
    End If
    If allTeamStats!(1, 11) <> 0 Then
        Print #1, Tab(65); Using "##.#"; allTeamStats!(1, 12) / allTeamStats!(1, 11)
    Else
        Print #1,
    End If

    Print #1, "PENALTIES PER GAME / YARDS PER GAME"; Tab(40);
    Print #1, Using "##.#"; allTeamStats!(0, 11) / totalGames;: Print #1, "/";: Print #1, Using "##.#"; allTeamStats!(0, 12) / totalGames;
    Print #1, Tab(60); Using "##.#"; allTeamStats!(1, 11) / totalGames;: Print #1, "/";: Print #1, Using "##.#"; allTeamStats!(1, 12) / totalGames

    Print #1, "SAFETIES"; Tab(45);
    Print #1, Using "####"; statsTeam!(21);
    Print #1, Tab(65); Using "####"; statsOpp!(21)

    Print #1, "TIMES SACKED / YARDS LOST"; Tab(41);
    Print #1, Using "###"; sackStats!(0, 0);: Print #1, "/";: Print #1, Using "####"; sackStats!(0, 1);
    Print #1, Tab(61); Using "###"; sackStats!(1, 0);: Print #1, "/";: Print #1, Using "####"; sackStats!(1, 1)
    Print #1, Chr$(12)

    Print #1, "RUSHING           ATT   YDS  YDS/ATT  YDS/GAME   TD  LONG RUN  AGAINST"

    I = 0

    If D% <> 1 Then
        For I1 = 0 To 9
            For I2 = 0 To 3
                If rbName$(I1) = qbName$(I2) Then
                    allPlayerStats!(I1, 0) = allPlayerStats!(I1, 0) + allPlayerStats!(I2 + 10, 0)
                    allPlayerStats!(I1, 1) = allPlayerStats!(I1, 1) + allPlayerStats!(I2 + 10, 1)
                    allPlayerStats!(I1, 2) = allPlayerStats!(I1, 2) + allPlayerStats!(I2 + 10, 2)
                    allPlayerStats!(I2 + 10, 0) = 0
                    allPlayerStats!(I2 + 10, 1) = 0
                    allPlayerStats!(I2 + 10, 2) = 0
                End If
            Next I2
        Next I1
    Else
        'This should be implied???
        D% = 1
    End If

    For I1 = 0 To 13
        If allPlayerStats!(I1, 0) <> 0 Then
            If I1 <= 9 Then Print #1, rbName$(I1);
            If I1 > 9 Then Print #1, qbName$(I1 - 10);
            Print #1, Tab(18); Using "####  ####   ##.#      ###.#    ##    "; allPlayerStats!(I1, 0); allPlayerStats!(I1, 1); allPlayerStats!(I1, 1) / allPlayerStats!(I1, 0); allPlayerStats!(I1, 1) / totalGames; allPlayerStats!(I1, 2);
            If I1 > 9 Then
                Print #1,
            Else
                Print #1, Using "###"; statsLRush!(I1);
                Print #1, statsLRush$(I1, 0); Tab(64); statsLRush$(I1, 1)
            End If
        End If
    Next I1

    Print #1, teamName$; Tab(18);
    If statFormat = 0 Then
        Print #1, Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(0, 1); allTeamStats!(0, 2); allTeamStats!(0, 2) / allTeamStats!(0, 1); allTeamStats!(0, 2) / totalGames; statsTeam!(15)
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(0, 1) - sackStats!(0, 0); allTeamStats!(0, 2) + sackStats!(0, 1); (allTeamStats!(0, 2) + sackStats!(0, 1)) / (allTeamStats!(0, 1) - sackStats!(0, 0)); (allTeamStats!(0, 2) + sackStats!(0, 1)) / totalGames; statsTeam!(15)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);
    If statFormat = 0 Then
        Print #1, Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(1, 1); allTeamStats!(1, 2); allTeamStats!(1, 2) / allTeamStats!(1, 1); allTeamStats!(1, 2) / totalGames; statsOpp!(15)
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; allTeamStats!(1, 1) - sackStats!(1, 0); allTeamStats!(1, 2) + sackStats!(1, 1); (allTeamStats!(1, 2) + sackStats!(1, 1)) / (allTeamStats!(1, 1) - sackStats!(1, 0)); (allTeamStats!(1, 2) + sackStats!(1, 1)) / totalGames; statsOpp!(15)
    End If

    Print #1,
    Print #1, "RECEIVING         REC   YDS  YDS/REC  YDS/GAME   TD  LONG REC  AGAINST"

    I = 0

    For I1 = 0 To 5
        If allPlayerStats!(I1, 6) <> 0 Then
            Print #1, wrName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; allPlayerStats!(I1, 6); allPlayerStats!(I1, 7); allPlayerStats!(I1, 7) / allPlayerStats!(I1, 6); allPlayerStats!(I1, 7) / totalGames; allPlayerStats!(I1, 8);
            Print #1, Using "###"; statsLRec!(I1 + 10);
            Print #1, statsLRec$(I1 + 10, 0); Tab(64); statsLRec$(I1 + 10, 1)
        End If
    Next I1

    For I1 = 0 To 9
        If allPlayerStats!(I1, 3) <> 0 Then
            Print #1, rbName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; allPlayerStats!(I1, 3); allPlayerStats!(I1, 4); allPlayerStats!(I1, 4) / allPlayerStats!(I1, 3); allPlayerStats!(I1, 4) / totalGames; allPlayerStats!(I1, 5);
            Print #1, Using "###"; statsLRec!(I1);
            Print #1, statsLRec$(I1, 0); Tab(64); statsLRec$(I1, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);
    Print #1, Using "####  ####   "; allTeamStats!(0, 4); allTeamStats!(0, 6);

    If allTeamStats!(0, 4) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 4);
    End If

    Print #1, Using "      ###.#    ##"; allTeamStats!(0, 6) / totalGames; statsTeam!(17)
    Print #1, "OPPONENT TOTALS"; Tab(18);
    Print #1, Using "####  ####   "; allTeamStats!(1, 4); allTeamStats!(1, 6);

    If allTeamStats!(1, 4) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 4);
    End If

    Print #1, Using "      ###.#    ##"; allTeamStats!(1, 6) / totalGames; statsOpp!(17)

    Print #1,
    Print #1, "PASSING           ATT  COM COM %  YDS  Y/A  Y/C   Y/G  TD   I    I%  EFF RATING"

    For I1 = 0 To 3

        If allPlayerStats!(I1, 9) <> 0 Then

            Print #1, qbName$(I1); Tab(18); Using "####  ### ###.# #### ##.#"; allPlayerStats!(I1, 9); allPlayerStats!(I1, 10); allPlayerStats!(I1, 10) / allPlayerStats!(I1, 9) * 100; allPlayerStats!(I1, 12); allPlayerStats!(I1, 12) / allPlayerStats!(I1, 9);

            If allPlayerStats!(I1, 10) = 0 Then
                Print #1, "   --";
            Else
                Print #1, Using " ##.#"; allPlayerStats!(I1, 12) / allPlayerStats!(I1, 10);
            End If

            Print #1, Using " ###.# ### ### ###.#     "; allPlayerStats!(I1, 12) / totalGames; allPlayerStats!(I1, 13); allPlayerStats!(I1, 11); allPlayerStats!(I1, 11) / allPlayerStats!(I1, 9) * 100;

            awayTies = QBRating!(I1, statFormat)

            Print #1, Using "###.#"; awayTies

        End If

    Next I1

    Print #1, teamName$; Tab(18);
    Print #1, Using "####  ### "; allTeamStats!(0, 3); allTeamStats!(0, 4);

    If allTeamStats!(0, 3) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using "###.#"; allTeamStats!(0, 4) / allTeamStats!(0, 3) * 100;
    End If

    Print #1, Using "#### "; allTeamStats!(0, 6);

    If allTeamStats!(0, 3) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 3);
    End If
    If allTeamStats!(0, 4) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using " ##.#"; allTeamStats!(0, 6) / allTeamStats!(0, 4);
    End If

    Print #1, Using " ###.# ### ### "; allTeamStats!(0, 6) / totalGames; statsTeam!(17); allTeamStats!(0, 5);

    If allTeamStats!(0, 3) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using "###.#     "; allTeamStats!(0, 5) / allTeamStats!(0, 3) * 100
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);
    Print #1, Using "####  ### "; allTeamStats!(1, 3); allTeamStats!(1, 4);

    If allTeamStats!(1, 3) = 0 Then
        Print #1, "   --";
    Else
        Print #1, Using "###.#"; allTeamStats!(1, 4) / allTeamStats!(1, 3) * 100;
    End If

    Print #1, Using "#### "; allTeamStats!(1, 6);

    If allTeamStats!(1, 3) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using "##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 3);
    End If
    If allTeamStats!(1, 4) = 0 Then
        Print #1, "  --";
    Else
        Print #1, Using " ##.#"; allTeamStats!(1, 6) / allTeamStats!(1, 4);
    End If

    Print #1, Using " ###.# ### ### "; allTeamStats!(1, 6) / totalGames; statsOpp!(17); allTeamStats!(1, 5);

    If allTeamStats!(1, 3) = 0 Then
        Print #1, "   --"
    Else
        Print #1, Using "###.#     "; allTeamStats!(1, 5) / allTeamStats!(1, 3) * 100
    End If

    Print #1,
    Print #1,
    Print #1, "PASSING           LONG PASS    AGAINST"

    For I1 = 0 To 3
        If allPlayerStats!(I1, 9) <> 0 Then
            Print #1, qbName$(I1); Tab(22); Using "###"; statsLPass!(I1);
            Print #1, statsLPass$(I1, 0); Tab(32); statsLPass$(I1, 1)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "KICKOFF RETURNS   RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 0 To 2
        If allPlayerStats!(I1, 14) <> 0 Then
            Print #1, krName$(I1); Tab(18);
            Print #1, Using "####  ####   "; allPlayerStats!(I1, 14); allPlayerStats!(I1, 15);
            If allPlayerStats!(I1, 14) = 0 Then
                Print #1, "  --";
            Else
                Print #1, Using "##.#"; allPlayerStats!(I1, 15) / allPlayerStats!(I1, 14);
            End If
            Print #1, Using "      ###.#    ##    "; allPlayerStats!(I1, 15) / totalGames; allPlayerStats!(I1, 16);
            Print #1, Using "###"; statsLKR!(I1);: Print #1, statsLKR$(I1, 0); Tab(64); statsLKR$(I1, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);

    If statsTeam!(22) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsTeam!(22); statsTeam!(23); statsTeam!(23) / statsTeam!(22); statsTeam!(23) / totalGames; statsTeam!(18)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);

    If statsOpp!(22) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsOpp!(22); statsOpp!(23); statsOpp!(23) / statsOpp!(22); statsOpp!(23) / totalGames; statsOpp!(18)
    End If

    Print #1,
    Print #1,
    Print #1,
    Print #1, "PUNT RETURNS      RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"

    I = 0

    For I1 = 3 To 5
        If allPlayerStats!(I1, 14) <> 0 Then
            Print #1, prName$(I1 - 3); Tab(18);
            Print #1, Using "####  ####   "; allPlayerStats!(I1, 14); allPlayerStats!(I1, 15);
            If allPlayerStats!(I1, 14) = 0 Then
                Print #1, "  --";
            Else
                Print #1, Using "##.#"; allPlayerStats!(I1, 15) / allPlayerStats!(I1, 14);
            End If
            Print #1, Using "      ###.#    ##    "; allPlayerStats!(I1, 15) / totalGames; allPlayerStats!(I1, 16);
            Print #1, Using "###"; statsLPR!(I1 - 3);: Print #1, statsLPR$(I1 - 3, 0); Tab(64); statsLPR$(I1 - 3, 1)
        End If
    Next I1

    Print #1, teamName$; Tab(18);

    If statsTeam!(24) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsTeam!(24); statsTeam!(25); statsTeam!(25) / statsTeam!(24); statsTeam!(25) / totalGames; statsTeam!(19)
    End If

    Print #1, "OPPONENT TOTALS"; Tab(18);

    If statsOpp!(24) = 0 Then
        Print #1, "   0     0   --.-        -.-     0"
    Else
        Print #1, Using "####  ####   ##.#      ###.#    ##"; statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); statsOpp!(25) / totalGames; statsOpp!(19)
    End If

    Print #1,

    I = 0

    Print #1, "PUNTING           PNT   YDS  YDS/PNT  PNT/GAME  IN-20  BLKD  LONG PNT"
    Print #1, punterName$(0); Tab(18); Using "####  ####    ##.#    ##.#      ##     ##      ##"; allTeamStats!(0, 7); allTeamStats!(0, 8); allTeamStats!(0, 8) / allTeamStats!(0, 7); allTeamStats!(0, 7) / totalGames; statsTeam!(31); statsTeam!(30); statsTeam!(28)
    Print #1, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ##.#      ##     ##"; allTeamStats!(1, 7); allTeamStats!(1, 8); allTeamStats!(1, 8) / allTeamStats!(1, 7); allTeamStats!(1, 7) / totalGames; statsOpp!(31); statsOpp!(30)
    Print #1,
    Print #1,
    Print #1,
    Print #1, "NET PUNTING       PNT   YDS  YDS/PNT  OPP RET  OPP RET YDS  OPP AVG  NET AVG"
    Print #1, teamName$; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; allTeamStats!(0, 7); allTeamStats!(0, 8); allTeamStats!(0, 8) / allTeamStats!(0, 7); statsOpp!(24); statsOpp!(25); statsOpp!(25) / statsOpp!(24); (allTeamStats!(0, 8) - statsOpp!(25)) / allTeamStats!(0, 7)
    Print #1, "OPPONENT TOTALS"; Tab(18); Using "####  ####    ##.#    ###       ####        ##.#     ##.#"; allTeamStats!(1, 7); allTeamStats!(1, 8); allTeamStats!(1, 8) / allTeamStats!(1, 7); statsTeam!(24); statsTeam!(25); statsTeam!(25) / statsTeam!(24); (allTeamStats!(1, 8) - statsTeam!(25)) / allTeamStats!(1, 7)
    Print #1,
    Print #1,
    Print #1,
    Print #1, "KICKING"; Tab(16); "EPM EPA   EP% FGM FGA   FG% LG  0-19 20-29 30-39 40-49  50+"

    If allPlayerStats!(4, 17) <> 0 Or allPlayerStats!(6, 17) <> 0 Then
        Print #1, pkName$(0);
        Print #1, Tab(16);
        If allPlayerStats!(6, 17) <> 0 Then
            Print #1, Using "### ### ###.# "; allPlayerStats!(5, 17); allPlayerStats!(6, 17); allPlayerStats!(5, 17) / allPlayerStats!(6, 17) * 100;
        End If
        If allPlayerStats!(4, 17) = 0 Then
            Print #1,
        Else
            Print #1, Tab(30);
            Print #1, Using "### ### ###.# ## ##"; allPlayerStats!(3, 17); allPlayerStats!(4, 17); allPlayerStats!(3, 17) / allPlayerStats!(4, 17) * 100; statsLFG!(0); statsFGM!(0, 0);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 0); statsFGM!(0, 1);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 1); statsFGM!(0, 2);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 2); statsFGM!(0, 3);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(0, 3); statsFGM!(0, 4);
            Print #1, "-";: Print #1, Using "##"; statsFGA!(0, 4)
        End If
    End If

    If allPlayerStats!(8, 17) <> 0 Or allPlayerStats!(10, 17) <> 0 Then
        Print #1, pkName$(1); Tab(16);
        If allPlayerStats!(10, 17) <> 0 Then
            Print #1, Using "### ### ###.# "; allPlayerStats!(9, 17); allPlayerStats!(10, 17); allPlayerStats!(9, 17) / allPlayerStats!(10, 17) * 100;
        End If
        If allPlayerStats!(8, 17) = 0 Then
            Print #1,
        Else
            Print #1, Tab(30);
            Print #1, Using "### ### ###.# ## ##"; allPlayerStats!(7, 17); allPlayerStats!(8, 17); allPlayerStats!(7, 17) / allPlayerStats!(8, 17) * 100; statsLFG!(1); statsFGM!(1, 0);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 0); statsFGM!(1, 1);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 1); statsFGM!(1, 2);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 2); statsFGM!(1, 3);
            Print #1, "-";: Print #1, Using "## ##"; statsFGA!(1, 3); statsFGM!(1, 4);
            Print #1, "-";: Print #1, Using "##"; statsFGA!(1, 4)
        End If
    End If

    Print #1,
    Print #1,
    Print #1, "INTERCEPTIONS     RET   YDS  YDS/RET  YDS/GAME   TD  LONG RET  AGAINST"
    I = 0

    For I1 = 0 To 9
        If statsGI(I1, 0) <> 0 Then
            Print #1, intName$(I1); Tab(18);
            Print #1, Using "####  ####   ##.#      ###.#    ##    "; statsGI(I1, 0); statsGI(I1, 1); statsGI(I1, 1) / statsGI(I1, 0); statsGI(I1, 1) / totalGames; statsGI(I1, 2);
            Print #1, Using "###"; statsLInt!(I1);: Print #1, statsLInt$(I1, 0); Tab(64); statsLInt$(I1, 1)
        End If
    Next I1

    Print #1,
    Print #1, "DEFENSIVE SACKS    SACKS      YARDS LOST"

    For I1 = 0 To 14
        If statsGS(I1, 0) <> 0 Then
            Print #1, sackName$(I1); Tab(20); Using "###            ###"; statsGS(I1, 0); statsGS(I1, 1)
        End If
    Next

    Close #1

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
