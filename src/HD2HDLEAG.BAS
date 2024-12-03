'----------------------------------------
'          HD2HDLEAG Subroutine
'----------------------------------------
'This subroutine presents the user a menu
'of options for viewing Head-to-Head team
'comparisons.
Sub HD2HDLEAG ()

    _Title "3-in-1 Football - Head-to-Head League Stats"

    Color 15, 0
    Cls

    Call TITLE_UTILS

    Locate 4, 1

    Do

        Color 2, 0
        Call MenuBox(8, 10, 54, 6, 15, 1)

        Locate 10, 15
        Color 3

        Locate , 17: Print "SELECT OPTION:"
        Locate , 17: Color 15: Print "O";
        Color 7: Print "NE LEAGUE / CONFERENCE"
        Locate , 17: Color 15: Print "T";
        Color 7: Print "WO LEAGUES / CONFERENCES WITH COMPETING PLAY"
        Locate , 17: Color 15: Print "R";
        Color 7: Print "ETURN TO MAIN MENU"

        I$ = GetKeyPress$

        If UCase$(I$) = "O" Or UCase$(I$) = "T" Then

            If UCase$(I$) = "O" Then 
                Call Hd2HdPro_League(0)
            Else
                Call Hd2HdPro_League(1)
            End If

        End If

    Loop Until UCase$(I$) = "R" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'     Hd2HdPro_League Subroutine
'----------------------------------------
'This subroutine assists the user with the
'selection of a team for comparison, then
'loads the associated stats, calculates
'the comparisons and presents output options
Sub Hd2HdPro_League (secondLeague)

    Shared div1Name$, div2Name$, div3Name$, div4Name$
    Shared div1Name_2$, div2Name_2$, div3Name_2$, div4Name_2$

    Shared awayLosses!(), awayLosses_2nd!(), awayTies_2nd!(), awayTies!()
    Shared awayWins!(), awayWins_2nd!()
    Shared awayScoreTeam!(), awayScoreTeam_2nd!(), awayScoreOpp!(), awayScoreOpp_2nd!()

    Shared homeLosses!(), homeLosses_2nd!(), homeTies!(), homeTies_2nd!()
    Shared homeWins!(), homeWins_2nd!()
    Shared homeScoreTeam!(), homeScoreTeam_2nd!(), homeScoreOpp!(), homeScoreOpp_2nd!()

    Shared totAwayLosses!(), totAwayLosses_2nd!(), totHomeTies!(), totHomeTies_2nd!()
    Shared totAwayWins!(), totAwayWins_2nd!()
    Shared totAwayScoreTeam!(), totAwayScoreTeam!(), totAwayScoreOpp!(), totAwayScoreOpp_2nd!()

    Shared totHomeLosses!(), totHomeLosses_2nd!(), totAwayTies!(), totAwayTies_2nd!()
    Shared totHomeWins!(), totHomeWins_2nd!()
    Shared totHomeScoreTeam!(), totHomeScoreTeam_2nd(), totHomeScoreOpp!(), totHomeScoreOpp_2nd!()

    Shared teamScore(), oppScore(), memberIdx()

    Dim memberIdx_2nd(30)

    Shared divNames$(), divNames_2nd$(), locIndicator$(), oppName$()
    Shared memberName$(), memberNames_2nd$(), memberYear$(), memberYear_2nd$()

    newTeam = 1

    Do

        If newTeam = 1 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            If teamNbr > 0 Then

                statFile$ = nameStatFile$(team$)

                If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    J$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Open diskPaths$(1) + statFile$ + "S" For Input As #1

                    Input #1, nbrGames

                    For I = 1 To MAX_SCHED_STATS:
                        Input #1, teamScore(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS:
                        Input #1, oppName$(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS:
                        Input #1, oppScore(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS:
                        Input #1, locIndicator$(I)
                    Next I

                    Close #1

                    'Check if same league
                    If sameLeague <> 1 Then

                        leag1$ = ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)

                        If secondLeague <> 0 Then
                            leag2$ = ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)
                        End If

                    End If

                End If

            Else
                'If a team wasn't chosen, treat it like ESC
                ExitCode% = 27

            End If 'Done checking for valid team
 
        End If 'Done checking for new team

        Color 7, 0
        Cls

        If teamNbr > 0 And ExitCode% <> 27 And ExitCode% <> 0 Then

            userKey$ = GetOutputOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"                   
                        printChoice = 0
                        VPP = 0
                    Case "P"
                        printChoice = 1
                        VPP = 0
                    Case "S"
                        VPP = 1

                End Select

                'Haven't checked that file exists
                Open diskPaths$(2) + leag1$ + ".TMI" For Input As #2

                Input #2, statType

                If statType = 0 Then

                    Input #2, leag1$
                    Input #2, div1Name$
                    Input #2, div2Name$

                    For X = 1 To 15:
                        Input #2, memberName$(X)
                        Input #2, memberIdx(X)
                        Input #2, memberYear$(X)
                    Next

                End If

                If statType = 1 Then

                    Input #2, leag1$
                    Input #2, div1Name$
                    Input #2, div2Name$
                    Input #2, div3Name$
                    Input #2, div4Name$

                    For X = 1 To TEAMS_PER_LEAGUE:
                        Input #2, memberName$(X)
                        Input #2, memberIdx(X)
                        Input #2, memberYear$(X)
                    Next

                End If

                Close #2

                If secondLeague = 1 Then

                    'Haven't checked that file exists
                    Open diskPaths$(2) + leag2$ + ".TMI" For Input As #3

                    Input #3, statType

                    If statType = 0 Then
                        Input #3, leag2$
                        Input #3, div1Name_2$
                        Input #3, div2Name_2$

                        For X = 1 To 15:
                            Input #3, memberNames_2nd$(X)
                            Input #3, memberIdx_2nd(X)
                            Input #3, memberYear_2nd$(X)
                        Next

                        NT = 15

                    End If

                    If statType = 1 Then
                        Input #3, leag2$
                        Input #3, div1Name_2$
                        Input #3, div2Name_2$
                        Input #3, div3Name_2$
                        Input #3, div4Name_2$

                        For X = 1 To TEAMS_PER_LEAGUE:
                            Input #3, memberNames_2nd$(X)
                            Input #3, memberIdx_2nd(X)
                            Input #3, memberYear_2nd$(X)
                        Next

                    End If

                End If

                Close #3

                Color 15
                Locate 18, 18: Print "COMPUTING W/L/T RECORDS . . . PLEASE WAIT"

                'Initialization for 1st league
                Erase awayLosses!, awayTies!, awayWins!
                Erase awayScoreTeam!, awayScoreOpp!
                Erase homeLosses!, homeTies!, homeWins!
                Erase homeScoreTeam!, homeScoreOpp!
                Erase totAwayLosses!, totAwayWins!, totHomeLosses!, totAwayTies!
                Erase totHomeScoreTeam!, totHomeScoreOpp!, totAwayScoreTeam!, totAwayScoreOpp!

                'Calculations for 1st league
                For K = 1 To TEAMS_PER_LEAGUE

                    If memberName$(K) <> "XXX" Then

                        For I = 1 To nbrGames

                            If Left$(locIndicator$(I), 1) = "A" And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    awayWins!(K) = awayWins!(K) + 1
                                End If

                                If teamScore(I) < oppScore(I) Then
                                    awayLosses!(K) = awayLosses!(K) + 1
                                End If

                                If teamScore(I) = oppScore(I) Then
                                   awayTies!(K) = awayTies!(K) + 1
                                End If

                                awayScoreTeam!(K) = awayScoreTeam!(K) + teamScore(I)
                                awayScoreOpp!(K) = awayScoreOpp!(K) + oppScore(I)

                            End If

                            If Left$(locIndicator$(I), 1) = "H" And oppName$(I) = memberName$(K) Then

                                If teamScore(I) > oppScore(I) Then
                                    homeWins!(K) = homeWins!(K) + 1
                                End If
                                If teamScore(I) < oppScore(I) Then
                                    homeLosses!(K) = homeLosses!(K) + 1
                                End If
                                If teamScore(I) = oppScore(I) Then
                                    homeTies!(K) = homeTies!(K) + 1
                                End If

                                homeScoreTeam!(K) = homeScoreTeam!(K) + teamScore(I)
                                homeScoreOpp!(K) = homeScoreOpp!(K) + oppScore(I)

                            End If

                        Next I

                    End If

                Next K

                'If we're doing a 2nd league..
                If secondLeague = 1 Then

                    'Initialization for 2nd league
                    Erase awayLosses_2nd!, awayTies_2nd!, awayWins_2nd!
                    Erase awayScoreTeam_2nd!, awayScoreOpp_2nd!

                    Erase homeLosses_2nd!, homeTies_2nd!, homeWins_2nd!
                    Erase homeScoreTeam_2nd!, homeScoreOpp_2nd!

                    Erase totAwayLosses_2nd!, totAwayTies_2nd!, totAwayWins_2nd!
                    Erase totAwayScoreTeam!, totAwayScoreOpp_2nd!
                    Erase totHomeWins_2nd!, totHomeLosses_2nd!, totHomeTies_2nd!
                    Erase totHomeScoreTeam_2nd, totHomeScoreOpp_2nd!

                    'Calculations for 2nd league
                    For K = 1 To TEAMS_PER_LEAGUE

                        If memberNames_2nd$(K) <> "XXX" Then

                            For I = 1 To nbrGames

                                If Left$(locIndicator$(I), 1) = "A" And oppName$(I) = memberNames_2nd$(K) Then

                                    If teamScore(I) > oppScore(I) Then
                                        awayWins_2nd!(K) = awayWins_2nd!(K) + 1
                                    End If

                                    If teamScore(I) < oppScore(I) Then
                                        awayLosses_2nd!(K) = awayLosses_2nd!(K) + 1
                                    End If

                                    If teamScore(I) = oppScore(I) Then
                                        awayTies_2nd!(K) = awayTies_2nd!(K) + 1
                                    End If

                                    awayScoreTeam_2nd!(K) = awayScoreTeam_2nd!(K) + teamScore(I)
                                    awayScoreOpp_2nd!(K) = awayScoreOpp_2nd!(K) + oppScore(I)

                                End If

                                If Left$(locIndicator$(I), 1) = "H" And oppName$(I) = memberNames_2nd$(K) Then

                                    If teamScore(I) > oppScore(I) Then
                                        homeWins_2nd!(K) = homeWins_2nd!(K) + 1
                                    End If

                                    If teamScore(I) < oppScore(I) Then
                                        homeLosses_2nd!(K) = homeLosses_2nd!(K) + 1
                                    End If

                                    If teamScore(I) = oppScore(I) Then
                                        homeTies_2nd!(K) = homeTies_2nd!(K) + 1
                                    End If

                                    homeScoreTeam_2nd!(K) = homeScoreTeam_2nd!(K) + teamScore(I)
                                    homeScoreOpp_2nd!(K) = homeScoreOpp_2nd!(K) + oppScore(I)

                                End If

                            Next I

                        End If

                    Next K

                End If

                'Generate output
                If VPP <> 1 Then
                    Call Hd2HdPro_Report(team$, leag1$, leag2$, divNames$(), divNames_2nd$(), secondLeague, printChoice)
                Else
                    Call Hd2HdPro_Screen(team$, leag1$, leag2$, divNames$(), divNames_2nd$(), secondLeague)
                End If

                Cls

                Call TITLE_UTILS

                Call MenuBox(8, 17, 40, 6, 15, 1)

                Do

                    Locate 10, 15

                    Color 3: Locate , 24: Print "SELECT OPTION:"
                    Color 7: Locate , 24: Print "VIEW ";
                    Color 15: Print "S";
                    Color 7: Print "AME TEAM AGAIN"
                    Color 7: Locate , 24: Print "VIEW ";
                    Color 15: Print "A";
                    Color 7: Print "NOTHER TEAM (SAME LEAGUE)"
                    Color 15: Locate , 24: Print "R";
                    Color 7: Print "ETURN TO OPTIONS"

                    K$ = GetKeyPress$

                    Select Case UCase$(K$)

                        Case "A"
                            newTeam = 1
                            sameLeague = 1
                            RDD = 0

                        Case "R"
                            newTeam = 0
                            sameLeague = 0
                            RDD = 0

                        Case "S"
                            newTeam = 0
                            sameLeague = 1
                            RDD = 0

                            'Re-initialize for the next loop
                            Erase totAwayLosses!, totAwayTies!, totAwayWins!
                            Erase totHomeLosses!, totHomeTies!, totHomeWins!
                            Erase totHomeScoreTeam!, totHomeScoreOpp!, totAwayScoreTeam!, totAwayScoreOpp!
                            Erase totAwayLosses_2nd!, totAwayWins_2nd!, totHomeLosses_2nd!, totHomeTies_2nd!, totHomeWins_2nd!, totAwayTies_2nd!
                            Erase totHomeScoreTeam_2nd, totHomeScoreOpp_2nd!, totAwayScoreTeam!, totAwayScoreOpp_2nd!

                    End Select

                Loop Until UCase$(K$) = "A" Or UCase$(K$) = "R" Or UCase$(K$) = "S" Or K$ = Chr$(27)
                
            End If 'Done checking for stats file

        End If

    Loop Until UCase$(K$) = "R" Or K$ = Chr$(27) Or userKey$ = Chr$(27) Or ExitCode% = 27

End Sub




'----------------------------------------
'       Hd2HdPro_Report Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to a report file
Sub Hd2HdPro_Report (teamName$, leag1$, leag2$, divNames$(), divNames_2nd$(), secondLeague, printDest)

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Shared awayLosses!(), awayLosses_2nd!(), awayTies_2nd!(), awayTies!()
    Shared awayWins!(), awayWins_2nd!()
    Shared awayScoreTeam!(), awayScoreTeam_2nd!(), awayScoreOpp!(), awayScoreOpp_2nd!()

    Shared homeLosses!(), homeLosses_2nd!(), homeTies!(), homeTies_2nd!()
    Shared homeWins!(), homeWins_2nd!()
    Shared homeScoreTeam!(), homeScoreTeam_2nd!(), homeScoreOpp!(), homeScoreOpp_2nd!()

    Shared totAwayLosses!(), totAwayLosses_2nd!(), totHomeTies!(), totHomeTies_2nd!()
    Shared totAwayWins!(), totAwayWins_2nd!()
    Shared totAwayScoreTeam!(), totAwayScoreTeam!(), totAwayScoreOpp!(), totAwayScoreOpp_2nd!()

    Shared totHomeLosses!(), totHomeLosses_2nd!(), totAwayTies!(), totAwayTies_2nd!()
    Shared totHomeWins!(), totHomeWins_2nd!()
    Shared totHomeScoreTeam!(), totHomeScoreTeam_2nd(), totHomeScoreOpp!(), totHomeScoreOpp_2nd!()

    Shared memberName$(), memberNames_2nd$()

    If printDest = 0 Then
        destFile$ = _SaveFileDialog$("Save Comparison Report", _CWD$ + "\Comparison", "*.txt", "Text File")
    Else
        destFile$ = "Comparison"
    End If

    Color 15, 0
    Cls

    divNames$(1) = div1Name$
    divNames$(2) = div2Name$
    divNames$(3) = div3Name$
    divNames$(4) = div4Name$

    Open destFile$ For Output As #1

    div = 0

    Do

        RDD = div + 1

        Print #1, Tab(48); teamName$
        Print #1,

        Print #1, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
        Print #1, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
        Print #1, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
        Print #1, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
        Print #1, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"

        Print #1, leag1$; " "; divNames$(RDD)

        DD = ((RDD - 1) * TEAMS_PER_DIV_PRO) + 1
        EE = RDD * TEAMS_PER_DIV_PRO

        'If divNames$(RDD) <> "XXX" And divNames$(RDD) <> "" Then

        For K = DD To EE

            If memberName$(K) <> "XXX" And memberName$(K) <> teamName$ Then

                Print #1, Left$(memberName$(K), 15);
                Print #1, Tab(17); Using "##"; homeWins!(K);
                Print #1, Tab(20); Using "##"; homeLosses!(K);
                Print #1, Tab(23); Using "##"; homeTies!(K);
                Print #1, Tab(27); Using "###"; homeScoreTeam!(K);
                Print #1, Tab(32); Using "###"; homeScoreOpp!(K);
                Print #1, Tab(38); Using "##"; awayWins!(K);
                Print #1, Tab(41); Using "##"; awayLosses!(K);
                Print #1, Tab(44); Using "##"; awayTies!(K);
                Print #1, Tab(48); Using "###"; awayScoreTeam!(K);
                Print #1, Tab(52); Using "###"; awayScoreOpp!(K);
                Print #1, Tab(58); Using "##"; homeWins!(K) + awayWins!(K);
                Print #1, Tab(61); Using "##"; homeLosses!(K) + awayLosses!(K);
                Print #1, Tab(64); Using "##"; homeTies!(K) + awayTies!(K);
                Print #1, Tab(68); Using "###"; homeScoreTeam!(K) + awayScoreTeam!(K);
                Print #1, Tab(72); Using "###"; homeScoreOpp!(K) + awayScoreOpp!(K)
                totHomeWins!(RDD) = totHomeWins!(RDD) + homeWins!(K)
                totHomeLosses!(RDD) = totHomeLosses!(RDD) + homeLosses!(K)
                totAwayWins!(RDD) = totAwayWins!(RDD) + awayWins!(K)
                totAwayLosses!(RDD) = totAwayLosses!(RDD) + awayLosses!(K)
                totAwayTies!(RDD) = totAwayTies!(RDD) + awayTies!(K)
                totHomeTies!(RDD) = totHomeTies!(RDD) + homeTies!(K)
                totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

            End If

        Next K

        Print #1,
        Print #1, "TOTALS";
        Print #1, Tab(17); Using "##"; totHomeWins!(RDD);
        Print #1, Tab(20); Using "##"; totHomeLosses!(RDD);
        Print #1, Tab(23); Using "##"; totHomeTies!(RDD);
        Print #1, Tab(27); Using "###"; totHomeScoreTeam!(RDD);
        Print #1, Tab(32); Using "###"; totHomeScoreOpp!(RDD);
        Print #1, Tab(38); Using "##"; totAwayWins!(RDD);
        Print #1, Tab(41); Using "##"; totAwayLosses!(RDD);
        Print #1, Tab(44); Using "##"; totAwayTies!(RDD);
        Print #1, Tab(48); Using "###"; totAwayScoreTeam!(RDD);
        Print #1, Tab(52); Using "###"; totAwayScoreOpp!(RDD);
        Print #1, Tab(58); Using "##"; totHomeWins!(RDD) + totAwayWins!(RDD);
        Print #1, Tab(61); Using "##"; totHomeLosses!(RDD) + totAwayLosses!(RDD);
        Print #1, Tab(64); Using "##"; totHomeTies!(RDD) + totAwayTies!(RDD);
        Print #1, Tab(68); Using "###"; totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD);
        Print #1, Tab(72); Using "###"; totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)
        Print #1,
        Print #1,

        div = div + 1

    Loop Until div > DIVS_PER_LEAGUE Or divNames$(div) = "XXX" Or divNames$(div) = ""

    If secondLeague = 1 Then

        divNames_2nd$(1) = div1Name_2$
        divNames_2nd$(2) = div2Name_2$
        divNames_2nd$(3) = div3Name_2$
        divNames_2nd$(4) = div4Name_2$

        div = 0

        Do

            RDD = div + 1

            Print #1, Tab(48); teamName$
            Print #1,

            Print #1, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
            Print #1, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
            Print #1, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
            Print #1, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
            Print #1, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"

            Print #1, leag2$; " "; divNames_2nd$(RDD)

            DD = ((RDD - 1) * TEAMS_PER_DIV_PRO) + 1
            EE = RDD * TEAMS_PER_DIV_PRO

            'If divNames$(RDD) <> "XXX" And divNames$(RDD) <> "" Then

            For K = DD To EE

                If memberNames_2nd$(K) <> "XXX" And memberNames_2nd$(K) <> teamName$ Then

                    Print #1, Left$(memberNames_2nd$(K), 15);
                    Print #1, Tab(17); Using "##"; homeWins_2nd!(K);
                    Print #1, Tab(20); Using "##"; homeLosses_2nd!(K);
                    Print #1, Tab(23); Using "##"; homeTies_2nd!(K);
                    Print #1, Tab(27); Using "###"; homeScoreTeam_2nd!(K);
                    Print #1, Tab(32); Using "###"; homeScoreOpp_2nd!(K);
                    Print #1, Tab(38); Using "##"; awayWins_2nd!(K);
                    Print #1, Tab(41); Using "##"; awayLosses_2nd!(K);
                    Print #1, Tab(44); Using "##"; awayTies_2nd!(K);
                    Print #1, Tab(48); Using "###"; awayScoreTeam_2nd!(K);
                    Print #1, Tab(52); Using "###"; awayScoreOpp_2nd!(K);
                    Print #1, Tab(58); Using "##"; homeWins_2nd!(K) + awayWins_2nd!(K);
                    Print #1, Tab(61); Using "##"; homeLosses_2nd!(K) + awayLosses_2nd!(K);
                    Print #1, Tab(64); Using "##"; homeTies_2nd!(K) + awayTies_2nd!(K);
                    Print #1, Tab(68); Using "###"; homeScoreTeam_2nd!(K) + awayScoreTeam_2nd!(K);
                    Print #1, Tab(72); Using "###"; homeScoreOpp_2nd!(K) + awayScoreOpp_2nd!(K)

                    totHomeWins_2nd!(RDD) = totHomeWins_2nd!(RDD) + homeWins_2nd!(K)
                    totHomeLosses_2nd!(RDD) = totHomeLosses_2nd!(RDD) + homeLosses_2nd!(K)
                    totAwayWins_2nd!(RDD) = totAwayWins_2nd!(RDD) + awayWins_2nd!(K)
                    totAwayLosses_2nd!(RDD) = totAwayLosses_2nd!(RDD) + awayLosses_2nd!(K)
                    totAwayTies_2nd!(RDD) = totAwayTies_2nd!(RDD) + awayTies_2nd!(K)
                    totHomeTies_2nd!(RDD) = totHomeTies_2nd!(RDD) + homeTies_2nd!(K)
                    totHomeScoreTeam_2nd(RDD) = totHomeScoreTeam_2nd(RDD) + homeScoreTeam_2nd!(K)
                    totHomeScoreOpp_2nd!(RDD) = totHomeScoreOpp_2nd!(RDD) + homeScoreOpp_2nd!(K)
                    totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam_2nd!(K)
                    totAwayScoreOpp_2nd!(RDD) = totAwayScoreOpp_2nd!(RDD) + awayScoreOpp_2nd!(K)

                End If

            Next K

            Print #1,
            Print #1, "TOTALS";
            Print #1, Tab(17); Using "##"; totHomeWins_2nd!(RDD);
            Print #1, Tab(20); Using "##"; totHomeLosses_2nd!(RDD);
            Print #1, Tab(23); Using "##"; totHomeTies_2nd!(RDD);
            Print #1, Tab(27); Using "###"; totHomeScoreTeam_2nd(RDD);
            Print #1, Tab(32); Using "###"; totHomeScoreOpp_2nd!(RDD);
            Print #1, Tab(38); Using "##"; totAwayWins_2nd!(RDD);
            Print #1, Tab(41); Using "##"; totAwayLosses_2nd!(RDD);
            Print #1, Tab(44); Using "##"; totAwayTies_2nd!(RDD);
            Print #1, Tab(48); Using "###"; totAwayScoreTeam!(RDD);
            Print #1, Tab(52); Using "###"; totAwayScoreOpp_2nd!(RDD);
            Print #1, Tab(58); Using "##"; totHomeWins_2nd!(RDD) + totAwayWins_2nd!(RDD);
            Print #1, Tab(61); Using "##"; totHomeLosses_2nd!(RDD) + totAwayLosses_2nd!(RDD);
            Print #1, Tab(64); Using "##"; totHomeTies_2nd!(RDD) + totAwayTies_2nd!(RDD);
            Print #1, Tab(68); Using "###"; totHomeScoreTeam_2nd(RDD) + totAwayScoreTeam!(RDD);
            Print #1, Tab(72); Using "###"; totHomeScoreOpp_2nd!(RDD) + totAwayScoreOpp_2nd!(RDD)
            Print #1,
            Print #1,

            div = div + 1

        Loop Until div > DIVS_PER_LEAGUE Or divNames$(div) = "XXX" Or divNames$(div) = ""

    End If

    Print #1, Tab(48); teamName$
    Print #1, Tab(18); "------ HOME -----"; Tab(39); "------ AWAY -----"; Tab(59); "---- TOTALS -----"
    Print #1, Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF";
    Print #1, Tab(32); "PTA"; Tab(39); "W"; Tab(42); "L"; Tab(45); "T";
    Print #1, Tab(48); "PTF"; Tab(52); "PTA"; Tab(59); "W"; Tab(62); "L";
    Print #1, Tab(65); "T"; Tab(69); "PTF"; Tab(72); "PTA"
    Print #1, leag1$

    For W = 1 To DIVS_PER_LEAGUE

        If divNames$(W) <> "XXX" Then

            Print #1, divNames$(W);
            Print #1, Tab(17); Using "##"; totHomeWins!(W);
            Print #1, Tab(20); Using "##"; totHomeLosses!(W);
            Print #1, Tab(23); Using "##"; totHomeTies!(W);
            Print #1, Tab(27); Using "###"; totHomeScoreTeam!(W);
            Print #1, Tab(32); Using "###"; totHomeScoreOpp!(W);
            Print #1, Tab(38); Using "##"; totAwayWins!(W);
            Print #1, Tab(41); Using "##"; totAwayLosses!(W);
            Print #1, Tab(44); Using "##"; totAwayTies!(W);
            Print #1, Tab(48); Using "###"; totAwayScoreTeam!(W);
            Print #1, Tab(52); Using "###"; totAwayScoreOpp!(W);
            Print #1, Tab(58); Using "##"; totHomeWins!(W) + totAwayWins!(W);
            Print #1, Tab(61); Using "##"; totHomeLosses!(W) + totAwayLosses!(W);
            Print #1, Tab(64); Using "##"; totHomeTies!(W) + totAwayTies!(W);
            Print #1, Tab(68); Using "###"; totHomeScoreTeam!(W) + totAwayScoreTeam!(W);
            Print #1, Tab(72); Using "###"; totHomeScoreOpp!(W) + totAwayScoreOpp!(W)
            
        End If

    Next W

    THWW = 0
    THLL = 0
    THTT = 0
    TAWW = 0
    TALL = 0
    TRTT = 0

    For I = 1 To 4
        THWW = THWW + totHomeWins!(I)
        THLL = THLL + totHomeLosses!(I)
        THTT = THTT + totHomeTies!(I)
        TAWW = TAWW + totAwayWins!(I)
        TALL = TALL + totAwayLosses!(I)
        TRTT = TRTT + totAwayTies!(I)
    Next

    TG11 = 0
    TG22 = 0
    TG33 = 0
    TG44 = 0

    For I = 1 To 4
        TG11 = TG11 + totHomeScoreTeam!(I)
        TG22 = TG22 + totHomeScoreOpp!(I)
        TG33 = TG33 + totAwayScoreTeam!(I)
        TG44 = TG44 + totAwayScoreOpp!(I)
    Next

    Print #1,
    Print #1, "TOTALS";
    Print #1, Tab(17); Using "##"; THWW;
    Print #1, Tab(20); Using "##"; THLL;
    Print #1, Tab(23); Using "##"; THTT;
    Print #1, Tab(27); Using "###"; TG11;
    Print #1, Tab(32); Using "###"; TG22;
    Print #1, Tab(38); Using "##"; TAWW;
    Print #1, Tab(41); Using "##"; TALL;
    Print #1, Tab(44); Using "##"; TRTT;
    Print #1, Tab(48); Using "###"; TG33;
    Print #1, Tab(52); Using "###"; TG44;
    Print #1, Tab(58); Using "##"; THWW + TAWW;
    Print #1, Tab(61); Using "##"; THLL + TALL;
    Print #1, Tab(64); Using "##"; THTT + TRTT;
    Print #1, Tab(68); Using "###"; TG11 + TG33;
    Print #1, Tab(72); Using "###"; TG22 + TG44

    If secondLeague = 1 Then

        Print #1,
        Print #1, leag2$

        For W = 1 To DIVS_PER_LEAGUE

            If divNames$(W) <> "XXX" Then

                Print #1, divNames_2nd$(W);
                Print #1, Tab(17); Using "##"; totHomeWins_2nd!(W);
                Print #1, Tab(20); Using "##"; totHomeLosses_2nd!(W);
                Print #1, Tab(23); Using "##"; totHomeTies_2nd!(W);
                Print #1, Tab(27); Using "###"; totHomeScoreTeam_2nd(W);
                Print #1, Tab(32); Using "###"; totHomeScoreOpp_2nd!(W);
                Print #1, Tab(38); Using "##"; totAwayWins_2nd!(W);
                Print #1, Tab(41); Using "##"; totAwayLosses_2nd!(W);
                Print #1, Tab(44); Using "##"; totAwayTies_2nd!(W);
                Print #1, Tab(48); Using "###"; totAwayScoreTeam!(W);
                Print #1, Tab(52); Using "###"; totAwayScoreOpp_2nd!(W);
                Print #1, Tab(58); Using "##"; totHomeWins_2nd!(W) + totAwayWins_2nd!(W);
                Print #1, Tab(61); Using "##"; totHomeLosses_2nd!(W) + totAwayLosses_2nd!(W);
                Print #1, Tab(64); Using "##"; totHomeTies_2nd!(W) + totAwayTies_2nd!(W);
                Print #1, Tab(68); Using "###"; totHomeScoreTeam_2nd(W) + totAwayScoreTeam!(W);
                Print #1, Tab(72); Using "###"; totHomeScoreOpp_2nd!(W) + totAwayScoreOpp_2nd!(W)

            End If

        Next W

        THWW2 = 0
        THLL2 = 0
        THTT2 = 0
        TAWW2 = 0
        TALL2 = 0
        TRTT2 = 0

        For I = 1 To 4
            THWW2 = THWW2 + totHomeWins_2nd!(I)
            THLL2 = THLL2 + totHomeLosses_2nd!(I)
            THTT2 = THTT2 + totHomeTies_2nd!(I)
            TAWW2 = TAWW2 + totAwayWins_2nd!(I)
            TALL2 = TALL2 + totAwayLosses_2nd!(I)
            TRTT2 = TRTT2 + totAwayTies_2nd!(I)
        Next

        TG112 = 0
        TG222 = 0
        TG332 = 0
        TG442 = 0

        For I = 1 To 4
            TG112 = TG112 + totHomeScoreTeam_2nd(I)
            TG222 = TG222 + totHomeScoreOpp_2nd!(I)
            TG332 = TG332 + totAwayScoreTeam!(I)
            TG442 = TG442 + totAwayScoreOpp_2nd!(I)
        Next

        Print #1,
        Print #1, "TOTALS";
        Print #1, Tab(17); Using "##"; THWW2;
        Print #1, Tab(20); Using "##"; THLL2;
        Print #1, Tab(23); Using "##"; THTT2;
        Print #1, Tab(27); Using "###"; TG112;
        Print #1, Tab(32); Using "###"; TG222;
        Print #1, Tab(38); Using "##"; TAWW2;
        Print #1, Tab(41); Using "##"; TALL2;
        Print #1, Tab(44); Using "##"; TRTT2;
        Print #1, Tab(48); Using "###"; TG332;
        Print #1, Tab(52); Using "###"; TG442;
        Print #1, Tab(58); Using "##"; THWW2 + TAWW2;
        Print #1, Tab(61); Using "##"; THLL2 + TALL2;
        Print #1, Tab(64); Using "##"; THTT2 + TRTT2;
        Print #1, Tab(68); Using "###"; TG112 + TG332;
        Print #1, Tab(72); Using "###"; TG222 + TG442

    End If

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
'     Hd2HdPro_Screen Subroutine
'----------------------------------------
'This subroutine outputs the calculated
'comparison values to the end user display
Sub Hd2HdPro_Screen (teamName$, leag1$, leag2$, divNames$(), divNames_2nd$(), secondLeague)

    Shared div1Name$, div2Name$, div3Name$, div4Name$
    Shared div1Name_2$, div2Name_2$, div3Name_2$, div4Name_2$

    Shared teamRatings()

    Shared awayLosses!(), awayLosses_2nd!(), awayTies_2nd!(), awayTies!()
    Shared awayWins!(), awayWins_2nd!()
    Shared awayScoreTeam!(), awayScoreTeam_2nd!(), awayScoreOpp!(), awayScoreOpp_2nd!()

    Shared homeLosses!(), homeLosses_2nd!(), homeTies!(), homeTies_2nd!()
    Shared homeWins!(), homeWins_2nd!()
    Shared homeScoreTeam!(), homeScoreTeam_2nd!(), homeScoreOpp!(), homeScoreOpp_2nd!()

    Shared totAwayLosses!(), totAwayLosses_2nd!(), totHomeTies!(), totHomeTies_2nd!()
    Shared totAwayWins!(), totAwayWins_2nd!()
    Shared totAwayScoreTeam!(), totAwayScoreTeam!(), totAwayScoreOpp!(), totAwayScoreOpp_2nd!()

    Shared totHomeLosses!(), totHomeLosses_2nd!(), totAwayTies!(), totAwayTies_2nd!()
    Shared totHomeWins!(), totHomeWins_2nd!()
    Shared totHomeScoreTeam!(), totHomeScoreTeam_2nd(), totHomeScoreOpp!(), totHomeScoreOpp_2nd!()

    Shared divNames$(), divNames_2nd$()
    Shared memberName$(), memberNames_2nd$()

    divNames$(1) = div1Name$
    divNames$(2) = div2Name$
    divNames$(3) = div3Name$
    divNames$(4) = div4Name$

    div = 0

    Do

        RDD = div + 1

        Cls

        Locate 2, 35
        Color teamRatings(2), teamRatings(1): Print teamName$
        Color 3, 0

        Locate 4
        Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
        Locate 5
        Print Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF"; Tab(32); "PTA"; Tab(35); Chr$(179); Tab(37); "W"; Tab(40); "L"; Tab(43); "T"; Tab(46); "PTF"; Tab(50); "PTA"; Tab(53); Chr$(179); Tab(55); "W"; Tab(58); "L"; Tab(61); "T"; Tab(64); "PTF"; Tab(68); "PTA"

        Color 11
        Print leag1$; " "; divNames$(RDD);

        Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)

        DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
        EE = RDD * TEAMS_PER_DIV

        'If divNames$(RDD) <> "XXX" And divNames$(RDD) <> "" Then

        For K = DD To EE

            If memberName$(K) <> "XXX" And memberName$(K) <> teamName$ And memberName$(K) <> "" Then

                Color 15
                Locate , 1: Print Left$(memberName$(K), 15);

                Color 7
                Locate , 17: Print Using "##"; homeWins!(K);
                Locate , 20: Print Using "##"; homeLosses!(K);
                Locate , 23: Print Using "##"; homeTies!(K);
                Locate , 27: Print Using "###"; homeScoreTeam!(K);
                Locate , 32: Print Using "###"; homeScoreOpp!(K);

                Color 3: Print Tab(35); Chr$(179);

                Color 7
                Locate , 36: Print Using "##"; awayWins!(K);
                Locate , 39: Print Using "##"; awayLosses!(K);
                Locate , 42: Print Using "##"; awayTies!(K);
                Locate , 47: Print Using "##"; awayScoreTeam!(K);
                Locate , 51: Print Using "##"; awayScoreOpp!(K);

                Color 3: Print Tab(53); Chr$(179);

                Color 7
                Locate , 54: Print Using "##"; homeWins!(K) + awayWins!(K);
                Locate , 57: Print Using "##"; homeLosses!(K) + awayLosses!(K);
                Locate , 60: Print Using "##"; homeTies!(K) + awayTies!(K);
                Locate , 64: Print Using "###"; homeScoreTeam!(K) + awayScoreTeam!(K);
                Locate , 68: Print Using "###"; homeScoreOpp!(K) + awayScoreOpp!(K)

                Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

                totHomeWins!(RDD) = totHomeWins!(RDD) + homeWins!(K)
                totHomeLosses!(RDD) = totHomeLosses!(RDD) + homeLosses!(K)
                totAwayWins!(RDD) = totAwayWins!(RDD) + awayWins!(K)
                totAwayLosses!(RDD) = totAwayLosses!(RDD) + awayLosses!(K)
                totAwayTies!(RDD) = totAwayTies!(RDD) + awayTies!(K)
                totHomeTies!(RDD) = totHomeTies!(RDD) + homeTies!(K)
                totHomeScoreTeam!(RDD) = totHomeScoreTeam!(RDD) + homeScoreTeam!(K)
                totHomeScoreOpp!(RDD) = totHomeScoreOpp!(RDD) + homeScoreOpp!(K)
                totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam!(K)
                totAwayScoreOpp!(RDD) = totAwayScoreOpp!(RDD) + awayScoreOpp!(K)

            End If

        Next K

        Print

        Color 11
        Locate , 2: Print "TOTALS";
        Locate , 17: Print Using "##"; totHomeWins!(RDD);
        Locate , 20: Print Using "##"; totHomeLosses!(RDD);
        Locate , 23: Print Using "##"; totHomeTies!(RDD);
        Locate , 27: Print Using "###"; totHomeScoreTeam!(RDD);
        Locate , 32: Print Using "###"; totHomeScoreOpp!(RDD);

        Color 3: Print Tab(35); Chr$(179);

        Color 11
        Locate , 36: Print Using "##"; totAwayWins!(RDD);
        Locate , 39: Print Using "##"; totAwayLosses!(RDD);
        Locate , 42: Print Using "##"; totAwayTies!(RDD);
        Locate , 46: Print Using "###"; totAwayScoreTeam!(RDD);
        Locate , 50: Print Using "###"; totAwayScoreOpp!(RDD);

        Color 3: Print Tab(53); Chr$(179);

        Color 11
        Locate , 54: Print Using "##"; totHomeWins!(RDD) + totAwayWins!(RDD);
        Locate , 57: Print Using "##"; totHomeLosses!(RDD) + totAwayLosses!(RDD);
        Locate , 60: Print Using "##"; totHomeTies!(RDD) + totAwayTies!(RDD);
        Locate , 64: Print Using "###"; totHomeScoreTeam!(RDD) + totAwayScoreTeam!(RDD);
        Locate , 68: Print Using "###"; totHomeScoreOpp!(RDD) + totAwayScoreOpp!(RDD)

        Color 14
        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

        I$ = GetKeyPress$

        div = div + 1

    Loop Until div > DIVS_PER_LEAGUE Or divNames$(div) = "XXX" Or divNames$(div) = ""

    'Second League
    If secondLeague = 1 Then

        divNames_2nd$(1) = div1Name_2$
        divNames_2nd$(2) = div2Name_2$
        divNames_2nd$(3) = div3Name_2$

        div = 0

        Do

            RDD = div + 1

            Cls

            Locate 2, 35
            Color teamRatings(2), teamRatings(1): Print teamName$

            Color 3, 0

            Locate 4
            Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
            Locate 5
            Print Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF"; Tab(32); "PTA"; Tab(35); Chr$(179); Tab(37); "W"; Tab(40); "L"; Tab(43); "T"; Tab(46); "PTF"; Tab(50); "PTA"; Tab(53); Chr$(179); Tab(55); "W"; Tab(58); "L"; Tab(61); "T"; Tab(64); "PTF"; Tab(68); "PTA"

            Color 11
            Print leag2$; " "; divNames_2nd$(RDD);

            Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)

            DD = ((RDD - 1) * TEAMS_PER_DIV) + 1
            EE = RDD * TEAMS_PER_DIV

            'If divNames$(RDD) <> "XXX" And divNames$(RDD) <> "" Then

            For K = DD To EE

                If memberNames_2nd$(K) <> "XXX" And memberNames_2nd$(K) <> teamName$ Then

                    Color 15
                    Locate , 1: Print Left$(memberNames_2nd$(K), 15);
                    Color 7
                    Locate , 17: Print Using "##"; homeWins_2nd!(K);
                    Locate , 20: Print Using "##"; homeLosses_2nd!(K);
                    Locate , 23: Print Using "##"; homeTies_2nd!(K);
                    Locate , 27: Print Using "###"; homeScoreTeam_2nd!(K);
                    Locate , 32: Print Using "###"; homeScoreOpp_2nd!(K);

                    Color 3: Print Tab(35); Chr$(179);

                    Color 7
                    Locate , 36: Print Using "##"; awayWins_2nd!(K);
                    Locate , 39: Print Using "##"; awayLosses_2nd!(K);
                    Locate , 42: Print Using "##"; awayTies_2nd!(K);
                    Locate , 47: Print Using "##"; awayScoreTeam_2nd!(K);
                    Locate , 51: Print Using "##"; awayScoreOpp_2nd!(K);

                    Color 3: Print Tab(53); Chr$(179);

                    Color 7
                    Locate , 54: Print Using "##"; homeWins_2nd!(K) + awayWins_2nd!(K);
                    Locate , 57: Print Using "##"; homeLosses_2nd!(K) + awayLosses_2nd!(K);
                    Locate , 60: Print Using "##"; homeTies_2nd!(K) + awayTies_2nd!(K);
                    Locate , 64: Print Using "###"; homeScoreTeam_2nd!(K) + awayScoreTeam_2nd!(K);
                    Locate , 68: Print Using "###"; homeScoreOpp_2nd!(K) + awayScoreOpp_2nd!(K)

                    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

                    totHomeWins_2nd!(RDD) = totHomeWins_2nd!(RDD) + homeWins_2nd!(K)
                    totHomeLosses_2nd!(RDD) = totHomeLosses_2nd!(RDD) + homeLosses_2nd!(K)
                    totAwayWins_2nd!(RDD) = totAwayWins_2nd!(RDD) + awayWins_2nd!(K)
                    totAwayLosses_2nd!(RDD) = totAwayLosses_2nd!(RDD) + awayLosses_2nd!(K)
                    totAwayTies_2nd!(RDD) = totAwayTies_2nd!(RDD) + awayTies_2nd!(K)
                    totHomeTies_2nd!(RDD) = totHomeTies_2nd!(RDD) + homeTies_2nd!(K)
                    totHomeScoreTeam_2nd(RDD) = totHomeScoreTeam_2nd(RDD) + homeScoreTeam_2nd!(K)
                    totHomeScoreOpp_2nd!(RDD) = totHomeScoreOpp_2nd!(RDD) + homeScoreOpp_2nd!(K)
                    totAwayScoreTeam!(RDD) = totAwayScoreTeam!(RDD) + awayScoreTeam_2nd!(K)
                    totAwayScoreOpp_2nd!(RDD) = totAwayScoreOpp_2nd!(RDD) + awayScoreOpp_2nd!(K)

                End If

            Next K

            Print

            Color 11
            Locate , 2: Print "TOTALS";
            Locate , 17: Print Using "##"; totHomeWins_2nd!(RDD);
            Locate , 20: Print Using "##"; totHomeLosses_2nd!(RDD);
            Locate , 23: Print Using "##"; totHomeTies_2nd!(RDD);
            Locate , 27: Print Using "###"; totHomeScoreTeam_2nd(RDD);
            Locate , 32: Print Using "###"; totHomeScoreOpp_2nd!(RDD);

            Color 3: Print Tab(35); Chr$(179);

            Color 11
            Locate , 36: Print Using "##"; totAwayWins_2nd!(RDD);
            Locate , 39: Print Using "##"; totAwayLosses_2nd!(RDD);
            Locate , 42: Print Using "##"; totAwayTies_2nd!(RDD);
            Locate , 46: Print Using "###"; totAwayScoreTeam!(RDD);
            Locate , 50: Print Using "###"; totAwayScoreOpp_2nd!(RDD);

            Color 3: Print Tab(53); Chr$(179);

            Color 11
            Locate , 54: Print Using "##"; totHomeWins_2nd!(RDD) + totAwayWins_2nd!(RDD);
            Locate , 57: Print Using "##"; totHomeLosses_2nd!(RDD) + totAwayLosses_2nd!(RDD);
            Locate , 60: Print Using "##"; totHomeTies_2nd!(RDD) + totAwayTies_2nd!(RDD);
            Locate , 64: Print Using "###"; totHomeScoreTeam_2nd(RDD) + totAwayScoreTeam!(RDD);
            Locate , 68: Print Using "###"; totHomeScoreOpp_2nd!(RDD) + totAwayScoreOpp_2nd!(RDD)

            Color 14
            Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
            I$ = GetKeyPress$

            div = div + 1

        Loop Until div > DIVS_PER_LEAGUE Or divNames$(div) = "XXX" Or divNames$(div) = ""

    End If

    Cls

    Locate 2, 35
    Color teamRatings(2), teamRatings(1): Print teamName$
    Color 3, 0

    Locate 4
    Print Tab(18); String$(6, 196); " HOME "; String$(5, 196); Tab(35); Chr$(194); String$(6, 196); " AWAY "; String$(5, 196); Tab(53); Chr$(194); String$(4, 196); " TOTALS "; String$(5, 196)
    Locate 5
    Print Tab(18); "W"; Tab(21); "L"; Tab(24); "T"; Tab(27); "PTF"; Tab(32); "PTA"; Tab(35); Chr$(179); Tab(37); "W"; Tab(40); "L"; Tab(43); "T"; Tab(46); "PTF"; Tab(50); "PTA"; Tab(53); Chr$(179); Tab(55); "W"; Tab(58); "L"; Tab(61); "T"; Tab(64); "PTF"; Tab(68); "PTA"

    Color 11
    Print leag1$;

    Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179)

    For W = 1 To DIVS_PER_LEAGUE

        If divNames$(W) <> "XXX" Then

            Color 15

            'We have to limit this to 16 because we start stats on column 17
            Locate , 1: Print Left$(divNames$(W), 16);

            Color 7
            Locate , 17: Print Using "##"; totHomeWins!(W);
            Locate , 20: Print Using "##"; totHomeLosses!(W);
            Locate , 23: Print Using "##"; totHomeTies!(W);
            Locate , 27: Print Using "###"; totHomeScoreTeam!(W);
            Locate , 32: Print Using "###"; totHomeScoreOpp!(W);

            Color 3: Print Tab(35); Chr$(179);

            Color 7
            Locate , 36: Print Using "##"; totAwayWins!(W);
            Locate , 39: Print Using "##"; totAwayLosses!(W);
            Locate , 42: Print Using "##"; totAwayTies!(W);
            Locate , 46: Print Using "###"; totAwayScoreTeam!(W);
            Locate , 50: Print Using "###"; totAwayScoreOpp!(W);

            Color 3: Print Tab(53); Chr$(179);

            Color 7
            Locate , 54: Print Using "##"; totHomeWins!(W) + totAwayWins!(W);
            Locate , 57: Print Using "##"; totHomeLosses!(W) + totAwayLosses!(W);
            Locate , 60: Print Using "##"; totHomeTies!(W) + totAwayTies!(W);
            Locate , 64: Print Using "###"; totHomeScoreTeam!(W) + totAwayScoreTeam!(W);
            Locate , 68: Print Using "###"; totHomeScoreOpp!(W) + totAwayScoreOpp!(W)

            Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

        End If

    Next W

    THWW = 0
    THLL = 0
    THTT = 0
    TAWW = 0
    TALL = 0
    TRTT = 0

    For I = 1 To 4
        THWW = THWW + totHomeWins!(I)
        THLL = THLL + totHomeLosses!(I)
        THTT = THTT + totHomeTies!(I)
        TAWW = TAWW + totAwayWins!(I)
        TALL = TALL + totAwayLosses!(I)
        TRTT = TRTT + totAwayTies!(I)
    Next

    TG11 = 0
    TG22 = 0
    TG33 = 0
    TG44 = 0

    For I = 1 To 4
        TG11 = TG11 + totHomeScoreTeam!(I)
        TG22 = TG22 + totHomeScoreOpp!(I)
        TG33 = TG33 + totAwayScoreTeam!(I)
        TG44 = TG44 + totAwayScoreOpp!(I)
    Next

    Print

    Color 11
    Locate , 2: Print "TOTALS";
    Locate , 17: Print Using "##"; THWW;
    Locate , 20: Print Using "##"; THLL;
    Locate , 23: Print Using "##"; THTT;
    Locate , 27: Print Using "###"; TG11;
    Locate , 32: Print Using "###"; TG22;

    Color 3: Print Tab(35); Chr$(179);

    Color 11
    Locate , 36: Print Using "##"; TAWW;
    Locate , 39: Print Using "##"; TALL;
    Locate , 42: Print Using "##"; TRTT;
    Locate , 46: Print Using "###"; TG33;
    Locate , 50: Print Using "###"; TG44;

    Color 3: Print Tab(53); Chr$(179);

    Color 11
    Locate , 54: Print Using "##"; THWW + TAWW;
    Locate , 57: Print Using "##"; THLL + TALL;
    Locate , 60: Print Using "##"; THTT + TRTT;
    Locate , 64: Print Using "###"; TG11 + TG33;
    Locate , 68: Print Using "###"; TG22 + TG44

    If secondLeague = 1 Then

        Print
        Print leag2$

        For W = 1 To DIVS_PER_LEAGUE

            If divNames_2nd$(W) <> "XXX" Then

                Color 15
                Locate , 2: Print divNames_2nd$(W);

                Color 7
                Locate , 17: Print Using "##"; totHomeWins_2nd!(W);
                Locate , 20: Print Using "##"; totHomeLosses_2nd!(W);
                Locate , 23: Print Using "##"; totHomeTies_2nd!(W);
                Locate , 27: Print Using "###"; totHomeScoreTeam_2nd(W);
                Locate , 32: Print Using "###"; totHomeScoreOpp_2nd!(W);

                Color 3: Print Tab(35); Chr$(179);

                Color 7
                Locate , 36: Print Using "##"; totAwayWins_2nd!(W);
                Locate , 39: Print Using "##"; totAwayLosses_2nd!(W);
                Locate , 42: Print Using "##"; totAwayTies_2nd!(W);
                Locate , 46: Print Using "###"; totAwayScoreTeam!(W);
                Locate , 50: Print Using "###"; totAwayScoreOpp_2nd!(W);

                Color 3: Print Tab(53); Chr$(179);

                Color 7
                Locate , 54: Print Using "##"; totHomeWins_2nd!(W) + totAwayWins_2nd!(W);
                Locate , 57: Print Using "##"; totHomeLosses_2nd!(W) + totAwayLosses_2nd!(W);
                Locate , 60: Print Using "##"; totHomeTies_2nd!(W) + totAwayTies_2nd!(W);
                Locate , 64: Print Using "###"; totHomeScoreTeam_2nd(W) + totAwayScoreTeam!(W);
                Locate , 68: Print Using "###"; totHomeScoreOpp_2nd!(W) + totAwayScoreOpp_2nd!(W)

                Color 3: Print Tab(35); Chr$(179); Tab(53); Chr$(179);

            End If

        Next W

        THWW2 = 0
        THLL2 = 0
        THTT2 = 0
        TAWW2 = 0
        TALL2 = 0
        TRTT2 = 0

        For I = 1 To 4
            THWW2 = THWW2 + totHomeWins_2nd!(I)
            THLL2 = THLL2 + totHomeLosses_2nd!(I)
            THTT2 = THTT2 + totHomeTies_2nd!(I)
            TAWW2 = TAWW2 + totAwayWins_2nd!(I)
            TALL2 = TALL2 + totAwayLosses_2nd!(I)
            TRTT2 = TRTT2 + totAwayTies_2nd!(I)
        Next

        TG112 = 0
        TG222 = 0
        TG332 = 0
        TG442 = 0

        For I = 1 To 4
            TG112 = TG112 + totHomeScoreTeam_2nd(I)
            TG222 = TG222 + totHomeScoreOpp_2nd!(I)
            TG332 = TG332 + totAwayScoreTeam_2nd!(I)
            TG442 = TG442 + totAwayScoreOpp_2nd!(I)
        Next

        Print

        Color 11
        Locate , 2: Print "TOTALS";
        Locate , 17: Print Using "##"; THWW2;
        Locate , 20: Print Using "##"; THLL2;
        Locate , 23: Print Using "##"; THTT2;
        Locate , 27: Print Using "###"; TG112;
        Locate , 32: Print Using "###"; TG222;

        Color 3: Print Tab(35); Chr$(179);

        Color 11
        Locate , 36: Print Using "##"; TAWW2;
        Locate , 39: Print Using "##"; TALL2;
        Locate , 42: Print Using "##"; TRTT2;
        Locate , 46: Print Using "###"; TG332;
        Locate , 50: Print Using "###"; TG442;

        Color 3: Print Tab(53); Chr$(179);

        Color 11
        Locate , 54: Print Using "##"; THWW2 + TAWW2;
        Locate , 57: Print Using "##"; THLL2 + TALL2;
        Locate , 60: Print Using "##"; THTT2 + TRTT2;
        Locate , 64: Print Using "###"; TG112 + TG332;
        Locate , 68: Print Using "###"; TG222 + TG442

    End If

    Color 14
    Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"

    I$ = GetKeyPress$

End Sub
