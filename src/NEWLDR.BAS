'----------------------------------------
'           NEWLDR Subroutine
'----------------------------------------
'This subroutine provides the user a menu
'for viewing stats compiled for the new
'"Expanded" Stats/Standings screens
Sub NEWLDR (statType)

    _Title "3-in-1 Football - Expanded Leaders / Standings"

    ReDim menuChoices$(1 To 5)

    Dim printOrView

    Shared leagueName$
    Shared div1Name$, div2Name$, div3Name$, div4Name$

    'Needed for Expanded Standings
    Shared lastLoseStreak(), lastWinStreak()
    Shared teamLoseStreak(), teamWinStreak()
    Shared totHomeLosses(), totHomeWins(), totHomeTies()
    Shared totAwayLosses(), totAwayWins(), totAwayTies()
    Shared totNeutralLosses(), totNeutralWins(), totNeutralTies()
    Shared totTeamLosses(), totTeamWins(), totTeamTies()

    Shared expStdLeagName$()

    If statType = 1 Then
        menuChoices$(1) = "UPDATE EXPANDED LEAGUE LEADERS  "
        menuChoices$(2) = "VIEW EXPANDED LEAGUE LEADERS    "
        menuChoices$(3) = "UPDATE EXPANDED LEAGUE STANDINGS"
        menuChoices$(4) = "VIEW EXPANDED LEAGUE STANDINGS  "
        menuChoices$(5) = "RETURN TO LEAGUE LEADERS MENU   "
    Else
        menuChoices$(1) = "UPDATE EXPANDED LEAGUE LEADERS  "
        menuChoices$(2) = "VIEW EXPANDED LEAGUE LEADERS    "
        menuChoices$(3) = "UPDATE EXPANDED LEAGUE STANDINGS"
        menuChoices$(4) = "VIEW EXPANDED LEAGUE STANDINGS  "
        menuChoices$(5) = "RETURN TO LEAGUE LEADERS MENU   "
    End If

    Do

        Cls
        Color 15, 0

        Call TITLE

        SELECTION% = 1
        Count% = 5
        Start% = 1
        ExitCode% = 5
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1
                    confStatFile$ = _OpenFileDialog$("Choose Conference Definition file", diskPaths$(2), "*.TMS", "Conference Definition files [.TMS]")
                    'ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

                    If confStatFile$ <> "" Then
                        Call ReadConferenceStats(confStatFile$, statType)
                        Call UpdateExpandedLeaders(statType, totalStats)
                        Call SaveExpandedLeaders(GetFileBaseName$(confStatFile$), totalStats)
                    End If

                Case 2
                    leaderFile$ = _OpenFileDialog$("Choose Expanded Leaders file", diskPaths$(2), "*.LDS", "Expanded Leader files [.LDS]")
                    'ChooseFile$(diskPaths$(2) + "*.LDS", "CONFERENCE LEADERS", FileExit%)

                    If leaderFile$ <> "" Then
                        Call ReadExpandedLeaders(leaderFile$, totalStats)
                        Call ExpandedLeaders_Menu(totalStats)
                    End If

                Case 3
                    confStatFile$ = _OpenFileDialog$("Choose Conference Definition file", diskPaths$(2), "*.TMS", "Conference Definition files [.TMS]")
                    'ChooseFile$(diskPaths$(2) + "*.TMS", "CONFERENCE STATS", FileExit%)

                    If confStatFile$ <> "" Then
                        Color 15, 0
                        Cls
                        Call ReadConferenceStats(confStatFile$, statType)
                        Call UpdateExpandedStandings(statType)
                    End If

                Case 4
                    standingsFile$ = _OpenFileDialog$("Choose Expanded Standings file", diskPaths$(2), "*.STD", "Expanded Standings files [.STD]")
                    'ChooseFile$(diskPaths$(2) + "*.STD", "CONFERENCE STANDINGS", FileExit%)

                    wrongFormat% = 0

                    If standingsFile$ <> "" Then

                        If statType = 1 Then
                            TEAM_LIMIT = TEAMS_PER_LEAGUE
                        Else
                            TEAM_LIMIT = TEAMS_PER_CONFERENCE
                        End If

                        Open standingsFile$ For Input As #2

                        Input #2, fileType

                        If fileType = statType Then

                            Input #2, leagueName$, div1Name$, div2Name$, div3Name$, div4Name$

                            For I = 1 To TEAM_LIMIT
                                Input #2, expStdLeagName$(I), totTeamWins(I), totHomeWins(I), totAwayWins(I), totNeutralWins(I), totTeamLosses(I), totHomeLosses(I), totAwayLosses(I), totNeutralLosses(I), totTeamTies(I), totHomeTies(I), totAwayTies(I), totNeutralTies(I), teamWinStreak(I), teamLoseStreak(I)
                            Next

                        Else

                            Cls
                            Print "THE FILE YOU TRIED TO OPEN IS NOT COLLEGE FORMAT"
                            Print "EITHER TRY AGAIN OR USE THE PRO OPTION TO EDIT"
                            Locate LAST_ROW - 2, 1: Print "PRESS ANY KEY TO CONTINUE"

                            I$ = GetKeyPress$

                            wrongFormat% = 1

                        End If

                        Close #2

                        If wrongFormat% = 0 Then

                            userKey$ = GetOutputOptions$

                            If userKey$ <> Chr$(27) Then

                                Select Case userKey$

                                    Case "F"
                                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                        If userFile$ <> "" Then
                                            Call NewStandings_Report(0, userFile$, leagueName$, statType)
                                        End If

                                    Case "P"
                                        Call NewStandings_Report(1, "REPORT", leagueName$, statType)

                                    Case "S":
                                        Call NewStandings_Screen(leagueName$, statType)

                                End Select

                            End If

                        End If 'Done checking right file format

                    End If

            End Select

        End If

    Loop Until SELECTION% = 5 Or ExitCode% = 27

End Sub


'----------------------------------------
'    UpdateExpandedLeaders Subroutine
'----------------------------------------
'This subroutine calculates the Expanded
'stats from those previously read in to
'a variety of arrays.

'statType
'   0 = College
'   1 = Pro
Sub UpdateExpandedLeaders (statType, totalStats)

    Shared div1Name$, div2Name$, div3Name$, div4Name$

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared expLdrRBVal!(), expLdrWRVal!(), expLdrQBVal!(), expLdrQBRat!(), expLdrKRVal!()
    Shared expLdrPRVal!(), expLdrPuntVal!(), expLdrKickVal!(), expLdrIntVal!(), expLdrSackVal!()

    Shared statsFGM!(), statsFGA!()
    Shared statsLRec!(), statsLKR!(), statsLFG!(), statsLInt!(), statsLPass!(), statsLRush!()
    Shared statsLPR!(), schedStatVal!(), statsTeam!(), statsOpp!(), statsPlayers!()

    Shared statsGI(), statsGS()

    Shared expLdrRBName$(), expLdrRBTeam$(), expLdrWRName$(), expLdrWRTeam$()
    Shared expLdrQBTeam$(), expLdrKRName$(), expLdrKRTeam$()
    Shared expLdrPRName$(), expLdrPRTeam$(), expLdrPuntName$(), expLdrPuntTeam$()
    Shared expLdrKickName$(), expLdrKickTeam$()
    Shared expLdrIntName$(), expLdrIntTeam$(), expLdrSackName$(), expLdrSackTeam$()
    Shared expLdrLRec$(), expLdrLKR$(), expLdrLPass$(), expLdrLRush$(), expLdrLPR$()
    Shared expLdrQBName$()
    Shared expLdrTeam$(), expLdrPlyrName$()

    Shared expLdrRBGames!(), expLdrWRGames!(), expLdrQBGames!()

    Shared memberName$(), intName$()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared schedStatTeam$()

    Shared sackName$(), statsLFG$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLPR$()
    Shared statsLRec$(), statsLRush$()
    Shared gameCount()

    Erase expLdrRBName$, expLdrRBTeam$, expLdrWRName$, expLdrWRTeam$
    Erase expLdrQBTeam$, expLdrKRName$, expLdrKRTeam$
    Erase expLdrPRName$, expLdrPRTeam$, expLdrPuntName$, expLdrPuntTeam$
    Erase expLdrKickName$, expLdrKickTeam$
    Erase expLdrIntName$, expLdrIntTeam$, expLdrSackName$, expLdrSackTeam$
    Erase expLdrLRec$, expLdrLKR$, expLdrLPass$, expLdrLRush$, expLdrLPR$
    Erase expLdrQBName$
    Erase expLdrTeam$, expLdrPlyrName$

    Color 15, 0
    Cls

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

    div1Name$ = ""
    div2Name$ = ""
    div3Name$ = ""
    div4Name$ = ""

    If statType = 0 Then
        totalUnits = TEAMS_PER_CONFERENCE
    Else
        totalUnits = TEAMS_PER_LEAGUE
    End If

    Erase schedStatVal!
    Erase schedStatTeam$

    For ZX = 1 To totalUnits

        teamStatFile$ = memberName$(ZX)

        If teamStatFile$ <> "XXX" And teamStatFile$ <> "*" And teamStatFile$ <> " " Then

            If _FileExists(diskPaths$(1) + teamStatFile$) Then

                Print "CONVERTING STAT FILE OF "; teamStatFile$

                Call ReadTeamStats(teamName$, teamStatFile$)

                Open diskPaths$(1) + teamStatFile$ + "S" For Input As #2
                Input #2, nbrGames
                Close #2

                totalStats = totalStats + 1

                For I = 0 To 14
                    schedStatVal!(totalStats, I) = statsTeam!(I)
                    schedStatTeam$(totalStats, I) = teamName$
                Next

                For I = 15 To 29
                    schedStatVal!(totalStats, I) = statsOpp!(I - 15)
                    schedStatTeam$(totalStats, I) = teamName$
                Next

                For I = 0 To 9
                    JA = JA + 1
                    expLdrRBName$(JA) = rbName$(I)
                    expLdrRBTeam$(JA) = teamName$
                    expLdrLRush$(JA) = statsLRush$(I, 0)
                    expLdrLRec$(JA) = statsLRec$(I, 0)
                    expLdrRBGames!(JA) = nbrGames

                    For I1 = 0 To 5:
                        expLdrRBVal!(JA, I1) = statsPlayers!(I, I1)
                    Next I1

                    expLdrRBVal!(JA, 6) = statsLRush!(I)
                    expLdrRBVal!(JA, 7) = statsLRec!(I)
                Next I

                For I = 0 To 5
                    JB = JB + 1
                    expLdrWRName$(JB) = wrName$(I)
                    expLdrWRTeam$(JB) = teamName$
                    expLdrLRec$(JB) = statsLRec$(I + 10, 0)
                    expLdrWRGames!(JB) = nbrGames

                    For I1 = 6 To 8:
                        expLdrWRVal!(JB, I1 - 6) = statsPlayers!(I, I1)
                    Next I1

                    expLdrWRVal!(JB, 3) = statsLRec!(I + 10)
                Next I

                For I = 0 To 3
                    JC = JC + 1
                    expLdrQBName$(JC) = qbName$(I)
                    expLdrQBTeam$(JC) = teamName$
                    expLdrLPass$(JC) = statsLPass$(I, 0)
                    expLdrQBGames!(JC) = nbrGames

                    For I1 = 9 To 13:
                        expLdrQBVal!(JC, I1 - 9) = statsPlayers!(I, I1)
                    Next I1

                    expLdrQBVal!(JC, 5) = statsLPass!(I)
                    Call QBRating_Expanded(JC, statType)
                    expLdrQBRat!(JC, 0) = expLdrQBVal!(JC, 12)

                Next I

                For I = 0 To 2
                    JD = JD + 1
                    expLdrKRName$(JD) = krName$(I)
                    expLdrKRTeam$(JD) = teamName$
                    expLdrLKR$(JD) = statsLKR$(I, 0)

                    For I1 = 14 To 16:
                        expLdrKRVal!(JD, I1 - 14) = statsPlayers!(I, I1)
                    Next I1

                    expLdrKRVal!(JD, 3) = statsLKR!(I)
                Next I

                For I = 0 To 2
                    JE = JE + 1
                    expLdrPRName$(JE) = prName$(I)
                    expLdrPRTeam$(JE) = teamName$
                    expLdrLPR$(JE) = statsLPR$(I, 0)

                    For I1 = 14 To 16:
                        expLdrPRVal!(JE, I1 - 14) = statsPlayers!(I + 3, I1)
                    Next I1

                    expLdrPRVal!(JE, 3) = statsLPR!(I)
                Next I

                JF = JF + 1
                expLdrPuntName$(JF) = punterName$(0)
                expLdrPuntTeam$(JF) = teamName$
                expLdrPuntVal!(JF, 0) = statsTeam!(7)
                expLdrPuntVal!(JF, 1) = statsTeam!(8)

                JG = JG + 1
                expLdrKickName$(JG) = pkName$(0)
                expLdrKickTeam$(JG) = teamName$

                For I = 0 To 3:
                    expLdrKickVal!(JG, I) = statsPlayers!(I + 3, 17)
                Next

                expLdrKickVal!(JG, 4) = statsLFG!(0)
                JG = JG + 1
                expLdrKickName$(JG) = pkName$(1)
                expLdrKickTeam$(JG) = teamName$

                For I = 0 To 3:
                    expLdrKickVal!(JG, I) = statsPlayers!(I + 7, 17)
                Next

                expLdrKickVal!(JG, 4) = statsLFG!(1)

                For I = 0 To 9
                    JH = JH + 1
                    expLdrIntName$(JH) = intName$(I)
                    expLdrIntTeam$(JH) = teamName$
                    expLdrIntVal!(JH) = statsGI(I, 0)
                Next

                For I = 0 To 14
                    JI = JI + 1
                    expLdrSackName$(JI) = sackName$(I)
                    expLdrSackTeam$(JI) = teamName$
                    expLdrSackVal!(JI) = statsGS(I, 0)
                Next

                JJ = JJ + 1
                gameCount(JJ) = nbrGames

            End If

        End If

    Next ZX

End Sub


'----------------------------------------
'     ReadExpandedLeaders Subroutine
'----------------------------------------
'This subroutine reads in the leader files
'(.LDS) for the Expanded standings modules
Sub ReadExpandedLeaders (leaderFile$, totalStats)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared expLdrRBName$(), expLdrRBTeam$(), expLdrWRName$(), expLdrWRTeam$(), expLdrQBTeam$(), expLdrKRName$(), expLdrKRTeam$()
    Shared expLdrPRName$(), expLdrPRTeam$(), expLdrPuntName$(), expLdrPuntTeam$(), expLdrKickName$(), expLdrKickTeam$()
    Shared expLdrIntName$(), expLdrIntTeam$(), expLdrSackName$(), expLdrSackTeam$()
    Shared expLdrLRec$(), expLdrLKR$(), expLdrLPass$(), expLdrLRush$(), schedStatTeam$()
    Shared expLdrLPR$(), expLdrQBName$()
    Shared expLdrRBVal!(), expLdrWRVal!(), expLdrQBVal!(), expLdrQBRat!(), expLdrKRVal!()
    Shared expLdrRBGames!(), expLdrWRGames!(), expLdrQBGames!()
    Shared expLdrPRVal!(), expLdrPuntVal!(), expLdrKickVal!(), expLdrIntVal!(), expLdrSackVal!()

    Shared schedStatVal!()

    Shared gameCount()

    TMR = 30 + 300 + 180 + 120 + 90 + 90 + 30 + 60 + 450
    '1660

    'Print "READING LEADER FILES . . . . PLEASE WAIT"
    Erase expLdrRBName$, expLdrRBTeam$, expLdrWRName$, expLdrWRTeam$, expLdrQBTeam$, expLdrKRName$, expLdrKRTeam$
    Erase expLdrPRName$, expLdrPRTeam$, expLdrPuntName$, expLdrPuntTeam$, expLdrKickName$, expLdrKickTeam$
    Erase expLdrIntName$, expLdrIntTeam$, expLdrSackName$, expLdrSackTeam$
    Erase expLdrLRec$, expLdrLKR$, expLdrLPass$, expLdrLRush$, schedStatTeam$
    Erase expLdrLPR$, expLdrQBName$
    Erase expLdrRBVal!, expLdrWRVal!, expLdrQBVal!, expLdrQBRat!, expLdrKRVal!
    Erase expLdrRBGames!, expLdrWRGames!, expLdrQBGames!
    Erase expLdrPRVal!, expLdrPuntVal!, expLdrKickVal!, expLdrIntVal!, expLdrSackVal!

    If Not _FileExists(leaderFile$) Then

        MsgText$ = "NO COMPILED LEADER FILES WERE FOUND. PRESS ANY KEY TO RETURN TO MAIN MENU"
        BoxWidth = 20
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        I$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Open leaderFile$ For Input As #1

        Input #1, JA, JB, JC, JD, JE, JF, JG, JH, JI, totalStats, JJ

        For I = 1 To 30:

            For I1 = 0 To 29:
                Input #1, schedStatVal!(I, I1), schedStatTeam$(I, I1)
            Next

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next

        For I = 1 To 300:

            Input #1, expLdrRBName$(I), expLdrRBTeam$(I), expLdrLRush$(I), expLdrLRec$(I), expLdrRBGames!(I)

            For I1 = 0 To 7:
                Input #1, expLdrRBVal!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 180:

            Input #1, expLdrWRName$(I), expLdrWRTeam$(I), expLdrLRec$(I), expLdrWRGames!(I)

            For I1 = 0 To 3:
                Input #1, expLdrWRVal!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 120:

            Input #1, expLdrQBName$(I), expLdrQBTeam$(I), expLdrLPass$(I), expLdrQBRat!(I, 0), expLdrQBGames!(I)

            For I1 = 0 To 6:
                Input #1, expLdrQBVal!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 90:

            Input #1, expLdrKRName$(I), expLdrKRTeam$(I), expLdrLKR$(I)

            For I1 = 0 To 3: Input #1, expLdrKRVal!(I, I1): Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 90:

            Input #1, expLdrPRName$(I), expLdrPRTeam$(I), expLdrLPR$(I)

            For I1 = 0 To 3:
                Input #1, expLdrPRVal!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 30:

            Input #1, expLdrPuntName$(I), expLdrPuntTeam$(I)

            For I1 = 0 To 1:
                Input #1, expLdrPuntVal!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 60:

            Input #1, expLdrKickName$(I), expLdrKickTeam$(I)

            For I1 = 0 To 4:
                Input #1, expLdrKickVal!(I, I1)
            Next I1

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 300:

            Input #1, expLdrIntName$(I), expLdrIntTeam$(I), expLdrIntVal!(I)

            Color 14
            Locate 16, 38
            TMR = TMR - 1: Print Using "####"; TMR

        Next I

        For I = 1 To 450:

            Input #1, expLdrSackName$(I), expLdrSackTeam$(I), expLdrSackVal!(I)

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
'     SaveExpandedLeaders Subroutine
'----------------------------------------
'This subroutine compiles the leader files
'(.LDS) for the Expanded standings modules
Sub SaveExpandedLeaders (leaderFile$, totalStats)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared expLdrRBName$(), expLdrRBTeam$(), expLdrWRName$(), expLdrWRTeam$(), expLdrQBTeam$(), expLdrKRName$(), expLdrKRTeam$()
    Shared expLdrPRName$(), expLdrPRTeam$(), expLdrPuntName$(), expLdrPuntTeam$(), expLdrKickName$(), expLdrKickTeam$()
    Shared expLdrIntName$(), expLdrIntTeam$(), expLdrSackName$(), expLdrSackTeam$()
    Shared expLdrLRec$(), expLdrLKR$(), expLdrLPass$(), expLdrLRush$(), schedStatTeam$()
    Shared expLdrLPR$(), expLdrQBName$()

    Shared expLdrRBVal!(), expLdrWRVal!(), expLdrQBVal!(), expLdrQBRat!(), expLdrKRVal!()
    Shared expLdrPRVal!(), expLdrPuntVal!(), expLdrKickVal!(), expLdrIntVal!(), expLdrSackVal!()
    Shared schedStatVal!()

    Shared expLdrRBGames!(), expLdrWRGames!(), expLdrQBGames!()
    Shared gameCount()

    TMR = 30 + 300 + 180 + 120 + 90 + 90 + 30 + 60 + 450
    '1660

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + leaderFile$ + ".LDS" For Output As #1

    Write #1, JA, JB, JC, JD, JE, JF, JG, JH, JI, totalStats, JJ

    For I = 1 To 30:

        For I1 = 0 To 29:
            Write #1, schedStatVal!(I, I1), schedStatTeam$(I, I1)
        Next

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next

    For I = 1 To 300:

        Write #1, expLdrRBName$(I), expLdrRBTeam$(I), expLdrLRush$(I), expLdrLRec$(I), expLdrRBGames!(I)

        For I1 = 0 To 7:
            Write #1, expLdrRBVal!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 180:

        Write #1, expLdrWRName$(I), expLdrWRTeam$(I), expLdrLRec$(I), expLdrWRGames!(I)

        For I1 = 0 To 3:
            Write #1, expLdrWRVal!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 120:

        Write #1, expLdrQBName$(I), expLdrQBTeam$(I), expLdrLPass$(I), expLdrQBRat!(I, 0), expLdrQBGames!(I)

        For I1 = 0 To 6:
            Write #1, expLdrQBVal!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 90:

        Write #1, expLdrKRName$(I), expLdrKRTeam$(I), expLdrLKR$(I)

        For I1 = 0 To 3: Write #1, expLdrKRVal!(I, I1): Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 90:

        Write #1, expLdrPRName$(I), expLdrPRTeam$(I), expLdrLPR$(I)

        For I1 = 0 To 3:
            Write #1, expLdrPRVal!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 30:

        Write #1, expLdrPuntName$(I), expLdrPuntTeam$(I)

        For I1 = 0 To 1:
            Write #1, expLdrPuntVal!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 60:

        Write #1, expLdrKickName$(I), expLdrKickTeam$(I)

        For I1 = 0 To 4:
            Write #1, expLdrKickVal!(I, I1)
        Next I1

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 300:

        Write #1, expLdrIntName$(I), expLdrIntTeam$(I), expLdrIntVal!(I)

        Color 14
        Locate 16, 38
        TMR = TMR - 1: Print Using "####"; TMR

    Next I

    For I = 1 To 450:

        Write #1, expLdrSackName$(I), expLdrSackTeam$(I), expLdrSackVal!(I)

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
'menu for viewing Expanded leaders. It then
'relies on various subroutines for the end
'user display of those statistics.
Sub ExpandedLeaders_Menu (totalStats)

    'Dim X0, X1
    Dim PPP, sortType

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ

    Shared expLdrRBName$(), expLdrRBTeam$(), expLdrWRName$(), expLdrWRTeam$(), expLdrQBTeam$(), expLdrKRName$(), expLdrKRTeam$()
    Shared expLdrPRName$(), expLdrPRTeam$(), expLdrPuntName$(), expLdrPuntTeam$(), expLdrKickName$(), expLdrKickTeam$()
    Shared expLdrIntName$(), expLdrIntTeam$(), expLdrSackName$(), expLdrSackTeam$()
    Shared expCategories$(), schedStatTeam$()
    Shared expLdrLRec$(), expLdrLKR$(), expLdrLPass$(), expLdrLRush$(), expLdrLPR$()
    Shared expLdrQBName$(), expLdrTeam$(), expLdrPlyrName$(), expLdrTD$(), memberName$()

    Shared expLdrRBVal!(), expLdrWRVal!(), expLdrQBVal!(), expLdrQBRat!(), expLdrKRVal!()
    Shared expLdrPRVal!(), expLdrPuntVal!(), expLdrKickVal!(), expLdrIntVal!(), expLdrSackVal!()
    Shared schedStatVal!(), expLdrVal!()

    Shared gameCount()

    Do

        Color 0, 0
        Cls

        Call ShadowBox(1, 15, 40, 15, 4, 7)

        RR = 0
        newCategory = 0

        Locate 2, 24
        Print "LEADER SELECTION MENU"
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

        Do
            key$ = GetKeyPress$
        Loop Until (Asc(UCase$(key$)) >= 65 And Asc(UCase$(key$)) <= 76) Or key$ = Chr$(27)

        If key$ <> Chr$(27) Then

            Select Case UCase$(key$)

                Case "A":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 8, 0, 4)

                    Do
                        For I = 0 To 4:
                            Color 14
                            Locate I + 3, 24
                            Print Using "##"; I;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 5

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "B":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 8, 0, 2)

                    Do
                        For I = 5 To 9:
                            Color 14
                            Locate I - 2, 24
                            Print Using "##"; I - 5;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 5

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "C":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 12, 0, 13)

                    Do
                        For I = 10 To 18:
                            Color 14
                            Locate I - 7, 24
                            Print Using "##"; I - 10;
                            Print " ";
                            Color 15: Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 8

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "D":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 8, 0, 5)

                    Do
                        For I = 19 To 23:
                            Color 14
                            Locate I - 16, 24
                            Print Using "##"; I - 19;
                            Print " ";
                            Color 15
                            Print expCategories$(I):
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 4

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "E":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 8, 0, 5)

                    Do
                        For I = 24 To 28:
                            Color 14
                            Locate I - 21, 24
                            Print Using "##"; I - 24;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 4

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "F":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 6, 0, 5)

                    Do
                        For I = 29 To 31:
                            Color 14
                            Locate I - 26, 24
                            Print Using "##"; I - 29;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 2

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "G":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 10, 0, 10)

                    Do
                        For I = 32 To 38:
                            Color 14
                            Locate I - 29, 24
                            Print Using "##"; I - 32;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 6

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "H":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 6, 0, 12)

                    Do
                        For I = 39 To 40:
                            Color 14
                            Locate I - 36, 24
                            Print Using "##"; I - 39;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 1

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "I":
                    Call SortOptions(printOrView, sortType, newCategory)

                Case "J":
                    ZZ = 1

                    Call MenuBox(2, 22, 36, 6, 0, 13)

                    Do
                        For I = 41 To 43:
                            Color 14
                            Locate I - 38, 24
                            Print Using "##"; I - 41;
                            Print " ";
                            Color 15
                            Print expCategories$(I)
                        Next

                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 2

                    Call SortOptions(printOrView, sortType, newCategory)

                Case "K":
                    ZZ = 1

                    Call MenuBox(2, 2, 66, 17, 0, 14)
                    Locate 10, 15

                    Do
                        For I = 44 To 58:
                            Color 0
                            Locate I - 41, 4
                            Print Using "##"; I - 44;
                            Print " ";
                            Print expCategories$(I)
                        Next

                        For I = 59 To 72:
                            Color 0
                            Locate I - 56, 40
                            Print Using "##"; I - 44;
                            Print " ";
                            Print expCategories$(I)
                        Next

                        Print
                        Print
                        Locate , 25
                        Input "SELECT NUMBER TO VIEW"; categoryId

                    Loop Until categoryId <= 28

                    Call SortOptions(printOrView, sortType, newCategory)

            End Select

            If UCase$(key$) <> "I" And UCase$(key$) <> "L" And UCase$(key$) <> "K" Then
                stpl = 0
                PPP = 0
            End If

            If UCase$(key$) <> "L" And newCategory = 0 Then

                Select Case UCase$(key$)

                    Case "A":

                        Select Case categoryId
                            Case 0: X0 = 0: XX = 0
                            Case 1: X0 = 1: XX = 1
                            Case 2: X0 = 0: X1 = 1: XX = 2
                            Case 3: X0 = 2: XX = 3
                            Case 4: X0 = 6: XX = 4
                        End Select

                        Cls
                        PPP = 0

                        Select Case categoryId

                            Case 0, 1, 3

                                For I = 1 To JA
                                    If expLdrRBVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                        expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                        expLdrVal!(PPP) = expLdrRBVal!(I, X0)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                                TVN = 1

                            Case 2

                                For I = 1 To JA
                                    If expLdrRBVal!(I, X0) > gameCount(1) * 10 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                        expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                        expLdrVal!(PPP) = expLdrRBVal!(I, X1) / expLdrRBVal!(I, X0)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                                TVN = 2

                            Case 4

                                Cls
                                PPP = 0

                                For I = 1 To JA
                                    If expLdrRBVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                        expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                        expLdrVal!(PPP) = expLdrRBVal!(I, X0)
                                        expLdrTD$(PPP) = expLdrLRush$(I)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                                TVN = 3

                        End Select

                    Case "B":

                        Select Case categoryId
                            Case 0: X0 = 3: X1 = 0: XX = 5
                            Case 1: X0 = 4: X1 = 1: XX = 6
                            Case 2: X0 = 3: X1 = 4: X2 = 0: X3 = 1: XX = 7
                            Case 3: X0 = 5: X1 = 2: XX = 8
                            Case 4: X0 = 3: X1 = 7: XX = 9
                        End Select

                        Cls
                        PPP = 0

                        Select Case categoryId

                            Case 0, 1, 3:
                                For I = 1 To JA
                                    If expLdrRBVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                        expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                        expLdrVal!(PPP) = expLdrRBVal!(I, X0)
                                    End If
                                Next I

                                For I = 1 To JB
                                    If expLdrWRVal!(I, X1) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrWRName$(I)
                                        expLdrTeam$(PPP) = expLdrWRTeam$(I)
                                        expLdrVal!(PPP) = expLdrWRVal!(I, X1)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:
                                Cls
                                PPP = 0

                                For I = 1 To JA
                                    If expLdrRBVal!(I, X0) >= gameCount(1) * 4 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                        expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                        expLdrVal!(PPP) = expLdrRBVal!(I, X1) / expLdrRBVal!(I, X0)
                                    End If
                                Next I

                                For I = 1 To JB
                                    If expLdrWRVal!(I, X2) >= gameCount(1) * 4 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrWRName$(I)
                                        expLdrTeam$(PPP) = expLdrWRTeam$(I)
                                        expLdrVal!(PPP) = expLdrWRVal!(I, X3) / expLdrWRVal!(I, X2)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 3:
                                Cls
                                PPP = 0

                                For I = 1 To JB
                                    If expLdrWRVal!(I, 0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrWRName$(I)
                                        expLdrTeam$(PPP) = expLdrWRTeam$(I)
                                        expLdrVal!(PPP) = expLdrWRVal!(I, X0)
                                        expLdrTD$(PPP) = expLdrLRec$(I)
                                    End If
                                Next I

                                For I = 1 To JA
                                    If expLdrRBVal!(I, 4) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                        expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                        expLdrVal!(PPP) = expLdrRBVal!(I, X1)
                                        expLdrTD$(PPP) = expLdrLRec$(I)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 3

                        End Select

                    Case "C":

                        Select Case categoryId

                            Case 0: X0 = 0: XX = 10
                            Case 1: X0 = 1: XX = 11
                            Case 2: X0 = 2: XX = 12
                            Case 3: X0 = 3: XX = 13
                            Case 4: X0 = 4: XX = 14
                            Case 5: X0 = 0: X1 = 1: XX = 15
                            Case 6: X0 = 1: X1 = 3: X2 = 0: XX = 16
                            Case 7: X0 = 0: XX = 17
                            Case 8: X0 = 5: XX = 18

                        End Select

                        Cls
                        PPP = 0

                        Select Case categoryId

                            Case 0 To 4:

                                For I = 1 To JC
                                    If expLdrQBVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                        expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                        expLdrVal!(PPP) = expLdrQBVal!(I, X0)
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 5:

                                For I = 1 To JC

                                    If expLdrQBVal!(I, X0) >= gameCount(1) * 15 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                        If expLdrQBVal!(I, X1) <> 0 Then
                                            expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                            expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                            expLdrVal!(PPP) = 0
                                        Else
                                            expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                            expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                            expLdrVal!(PPP) = (expLdrQBVal!(I, X1) / expLdrQBVal!(I, X0)) * 100
                                        End If

                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 6:

                                For I = 1 To JC
                                    If expLdrQBVal!(I, X2) >= gameCount(1) * 15 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                        If expLdrQBVal!(I, X1) <> 0 Then
                                            expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                            expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                            expLdrVal!(PPP) = expLdrQBVal!(I, X1) / expLdrQBVal!(I, X0)
                                        Else
                                            expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                            expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                            expLdrVal!(PPP) = 0
                                        End If
                                    End If
                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 4

                            Case 7:

                                For I = 1 To JC

                                    If expLdrQBVal!(I, X0) >= gameCount(1) * 15 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                        expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                        expLdrVal!(PPP) = expLdrQBRat!(I, X0)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 4

                            Case 8:

                                Cls
                                PPP = 0

                                For I = 1 To JC

                                    If expLdrQBVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrQBName$(I)
                                        expLdrTeam$(PPP) = expLdrQBTeam$(I)
                                        expLdrVal!(PPP) = expLdrQBVal!(I, X0)
                                        expLdrTD$(PPP) = expLdrLPass$(I)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 3

                        End Select

                    Case "D":

                        Select Case categoryId
                            Case 0: X0 = 0: XX = 19
                            Case 1: X0 = 1: XX = 20
                            Case 2: X0 = 0: X1 = 1: XX = 21
                            Case 3: X0 = 2: XX = 22
                            Case 4: X0 = 3: XX = 23
                        End Select

                        Cls
                        PPP = 0

                        Select Case categoryId

                            Case 0, 1, 3:

                                For I = 1 To JD

                                    If expLdrKRVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrKRName$(I)
                                        expLdrTeam$(PPP) = expLdrKRTeam$(I)
                                        expLdrVal!(PPP) = expLdrKRVal!(I, X0)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:

                                For I = 1 To JD

                                    If expLdrKRVal!(I, X0) <> 0 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        If expLdrKRVal!(I, X1) <> 0 Then
                                            expLdrPlyrName$(PPP) = expLdrKRName$(I)
                                            expLdrTeam$(PPP) = expLdrKRTeam$(I)
                                            expLdrVal!(PPP) = expLdrKRVal!(I, X1) / expLdrKRVal!(I, X0)
                                        Else
                                            expLdrPlyrName$(PPP) = expLdrKRName$(I)
                                            expLdrTeam$(PPP) = expLdrKRTeam$(I)
                                        End If

                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 4:

                                For I = 1 To JD

                                    If expLdrKRVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrKRName$(I)
                                        expLdrTeam$(PPP) = expLdrKRTeam$(I)
                                        expLdrVal!(PPP) = expLdrKRVal!(I, X0): expLdrTD$(PPP) = expLdrLKR$(I)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 3

                        End Select

                    Case "E":

                        Select Case categoryId
                            Case 0: X0 = 0: XX = 24
                            Case 1: X0 = 1: XX = 25
                            Case 2: X0 = 0: X1 = 1: XX = 26
                            Case 3: X0 = 2: XX = 27
                            Case 4: X0 = 3: XX = 28
                        End Select

                        Cls
                        PPP = 0

                        Select Case categoryId

                            Case 0, 1, 3:

                                For I = 1 To JE

                                    If expLdrPRVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrPRName$(I)
                                        expLdrTeam$(PPP) = expLdrPRTeam$(I)
                                        expLdrVal!(PPP) = expLdrPRVal!(I, X0)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:

                                For I = 1 To JE

                                    If expLdrPRVal!(I, X0) <> 0 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        If expLdrPRVal!(I, X1) <> 0 Then
                                            expLdrPlyrName$(PPP) = expLdrPRName$(I)
                                            expLdrTeam$(PPP) = expLdrPRTeam$(I)
                                            expLdrVal!(PPP) = expLdrPRVal!(I, X1) / expLdrPRVal!(I, X0)
                                        Else
                                            expLdrPlyrName$(PPP) = expLdrPRName$(I)
                                            expLdrTeam$(PPP) = expLdrPRTeam$(I)
                                        End If

                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                            Case 4:

                                For I = 1 To JE

                                    If expLdrPRVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrPRName$(I)
                                        expLdrTeam$(PPP) = expLdrPRTeam$(I)
                                        expLdrVal!(PPP) = expLdrPRVal!(I, X0)
                                        expLdrTD$(PPP) = expLdrLPR$(I)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)

                        End Select

                    Case "F":

                        Select Case categoryId
                            Case 0: X0 = 0: XX = 29
                            Case 1: X0 = 1: XX = 30
                            Case 2: X0 = 0: X1 = 1: XX = 31
                        End Select

                        Select Case categoryId

                            Case 0, 1:

                                For I = 1 To JF

                                    If expLdrPuntVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrPuntName$(I)
                                        expLdrTeam$(PPP) = expLdrPuntTeam$(I)
                                        expLdrVal!(PPP) = expLdrPuntVal!(I, X0)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2:

                                For I = 1 To JF

                                    If expLdrPuntVal!(I, X0) <> 0 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        If expLdrPuntVal!(I, X1) <> 0 Then
                                            expLdrPlyrName$(PPP) = expLdrPuntName$(I)
                                            expLdrTeam$(PPP) = expLdrPuntTeam$(I)
                                            expLdrVal!(PPP) = expLdrPuntVal!(I, X1) / expLdrPuntVal!(I, X0)
                                        Else
                                            expLdrPlyrName$(PPP) = expLdrPuntName$(I)
                                            expLdrTeam$(PPP) = expLdrPuntTeam$(I)
                                        End If

                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 2

                        End Select

                    Case "G":

                        Select Case categoryId
                            Case 0: X0 = 0: XX = 32
                            Case 1: X0 = 1: XX = 33
                            Case 2: X0 = 0: X1 = 1: XX = 34
                            Case 3: X0 = 2: XX = 35
                            Case 4: X0 = 3: XX = 36
                            Case 5: X0 = 2: X1 = 3: XX = 37
                            Case 6: X0 = 4: XX = 38
                        End Select

                        Cls
                        PPP = 0

                        Select Case categoryId

                            Case 0, 1, 3, 4, 6

                                For I = 1 To JG

                                    If expLdrKickVal!(I, X0) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        expLdrPlyrName$(PPP) = expLdrKickName$(I)
                                        expLdrTeam$(PPP) = expLdrKickTeam$(I)
                                        expLdrVal!(PPP) = expLdrKickVal!(I, X0)
                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 1

                            Case 2, 5

                                For I = 1 To JG

                                    If expLdrKickVal!(I, X0) <> 0 Then

                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                        If expLdrKickVal!(I, X1) <> 0 Then
                                            expLdrPlyrName$(PPP) = expLdrKickName$(I)
                                            expLdrTeam$(PPP) = expLdrKickTeam$(I)
                                            expLdrVal!(PPP) = (expLdrKickVal!(I, X0) / expLdrKickVal!(I, X1)) * 100
                                        Else
                                            expLdrPlyrName$(PPP) = expLdrKickName$(I)
                                            expLdrTeam$(PPP) = expLdrKickTeam$(I)
                                        End If

                                    End If

                                Next I

                                Call SortPlayer(PPP, stpl, sortType)
                                TVN = 4

                        End Select

                    Case "H":

                        If categoryId = 0 Then
                            X0 = 0: XX = 39
                        End If
                        If categoryId = 1 Then
                            X0 = 0: XX = 40
                        End If

                        Cls
                        PPP = 0

                        If categoryId = 0 Then

                            For I = 1 To JH
                                If expLdrIntVal!(I) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    expLdrPlyrName$(PPP) = expLdrIntName$(I)
                                    expLdrTeam$(PPP) = expLdrIntTeam$(I)
                                    expLdrVal!(PPP) = expLdrIntVal!(I)
                                End If
                            Next I

                        End If

                        If categoryId = 1 Then
                            For I = 1 To JI
                                If expLdrSackVal!(I) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    expLdrPlyrName$(PPP) = expLdrSackName$(I)
                                    expLdrTeam$(PPP) = expLdrSackTeam$(I)
                                    expLdrVal!(PPP) = expLdrSackVal!(I)
                                End If
                            Next I

                        End If

                        Call SortPlayer(PPP, stpl, sortType)

                        TVN = 1

                    Case "I":
                        Cls

                        PPP = 0
                        X0 = 1
                        X1 = 4
                        XX = 73

                        ' RB's - Rushing + Receiving Yds
                        For I = 1 To JA
                            If expLdrRBVal!(I, X0) + expLdrRBVal!(I, X1) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5
                                Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                'Add Rushing + Receivng Yards
                                expLdrVal!(PPP) = expLdrRBVal!(I, X0) + expLdrRBVal!(I, X1)

                                'Look for any KR / PR yards
                                For I2 = 1 To JD
                                    If expLdrKRName$(I2) = expLdrRBName$(I) And expLdrKRName$(I2) <> "OTHERS" Then
                                        expLdrVal!(PPP) = expLdrVal!(PPP) + expLdrKRVal!(I2, X0)
                                    End If
                                Next I2

                                For I2 = 1 To JE
                                    If expLdrPRName$(I2) = expLdrRBName$(I) And expLdrPRName$(I2) <> "OTHERS" Then
                                        expLdrVal!(PPP) = expLdrVal!(PPP) + expLdrPRVal!(I2, X0)
                                    End If
                                Next I2

                            End If
                        Next I

                        'WR's - Receiving Yds
                        For I = 1 To JB

                            If expLdrWRVal!(I, X0) <> 0 Then
                                PPP = PPP + 1
                                Locate 1, 5
                                Print "CHANGING PLAYER "; PPP
                                expLdrPlyrName$(PPP) = expLdrWRName$(I)
                                expLdrTeam$(PPP) = expLdrWRTeam$(I)
                                expLdrVal!(PPP) = expLdrWRVal!(I, X0)
                            End If

                            'Look for any KR / PR yards
                            For I2 = 1 To JD
                                If expLdrKRName$(I2) = expLdrWRName$(I) And expLdrKRName$(I2) <> "OTHERS" Then
                                    expLdrVal!(PPP) = expLdrVal!(PPP) + expLdrKRVal!(I2, X0)
                                End If
                            Next I2

                            For I2 = 1 To JE
                                If expLdrPRName$(I2) = expLdrWRName$(I) And expLdrPRName$(I2) <> "OTHERS" Then
                                    expLdrVal!(PPP) = expLdrVal!(PPP) + expLdrPRVal!(I2, X0)
                                End If
                            Next I2

                        Next I

                        'KR Yardage
                        For I = 1 To JD

                            If expLdrKRVal!(I, X0) <> 0 And expLdrKRName$(I) <> "OTHERS" Then

                                'Loop through all RB's / WR's
                                'Check for the name to match
                                found = 0

                                For I2 = 1 To JA
                                    If expLdrKRName$(I) = expLdrRBName$(I2) Then
                                        'We found a match; skip
                                        'Was previously added
                                        found = 1
                                    End If
                                Next I2

                                For I2 = 1 To JB
                                    If expLdrKRName$(I) = expLdrWRName$(I2) Then
                                        'We found a match; skip
                                        'Was previously added
                                        found = 1
                                    End If
                                Next I2

                                'If no matching RB / WR, start a new entry
                                If found = 0 Then

                                    PPP = PPP + 1
                                    Locate 1, 5
                                    Print "CHANGING PLAYER "; PPP

                                    expLdrPlyrName$(PPP) = expLdrKRName$(I)
                                    expLdrTeam$(PPP) = expLdrKRTeam$(I)
                                    expLdrVal!(PPP) = expLdrKRVal!(I, X0)
                                End If

                            End If

                        Next I

                        'PR Yardage
                        For I = 1 To JE

                            If expLdrPRVal!(I, X0) <> 0 And expLdrPRName$(I) <> "OTHERS" Then

                                PPP = PPP + 1
                                Locate 1, 5
                                Print "CHANGING PLAYER "; PPP

                                'Now we loop through all RB's / WR's
                                'Check for the name to match
                                found = 0

                                For I2 = 1 To JA
                                    If expLdrPRName$(I) = expLdrRBName$(I2) Then
                                        'We found a match; skip
                                        'Was previously added
                                        found = 1
                                    End If
                                Next I2

                                For I2 = 1 To JB
                                    If expLdrPRName$(I) = expLdrWRName$(I2) Then
                                        'We found a match; skip
                                        'Was previously added
                                        found = 1
                                    End If
                                Next I2

                                'If no matching RB / WR, start a new entry
                                If found = 0 Then
                                    expLdrPlyrName$(PPP) = expLdrPRName$(I)
                                    expLdrTeam$(PPP) = expLdrPRTeam$(I)
                                    expLdrVal!(PPP) = expLdrPRVal!(I, X0)
                                End If

                            End If

                        Next I

                        Call SortPlayer(PPP, stpl, sortType)

                        TVN = 1

                    Case "J":
                        Select Case categoryId
                            Case 0
                                X0 = 2
                                X1 = 5
                                XX = 41
                                KC = 1

                            Case 1
                                XF = 0
                                XF1 = 2
                                XX = 42
                                KC = 2

                            Case 2
                                X0 = 2
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
                                If expLdrRBVal!(I, X0) + expLdrRBVal!(I, X1) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    expLdrPlyrName$(PPP) = expLdrRBName$(I)
                                    expLdrTeam$(PPP) = expLdrRBTeam$(I)
                                    expLdrVal!(PPP) = (expLdrRBVal!(I, X0) + expLdrRBVal!(I, X1)) * 6
                                End If
                            Next I

                            For I = 1 To JB
                                If expLdrWRVal!(I, X0) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    expLdrPlyrName$(PPP) = expLdrWRName$(I)
                                    expLdrTeam$(PPP) = expLdrWRTeam$(I)
                                    expLdrVal!(PPP) = expLdrWRVal!(I, X0) * 6
                                End If
                            Next I

                        End If

                        If KC <> 1 Then
                            For I = 1 To JG
                                If expLdrKickVal!(I, XF) + expLdrKickVal!(I, XF1) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                    expLdrPlyrName$(PPP) = expLdrKickName$(I)
                                    expLdrTeam$(PPP) = expLdrKickTeam$(I)
                                    expLdrVal!(PPP) = (expLdrKickVal!(I, XF) * 3) + expLdrKickVal!(I, XF1)
                                End If
                            Next I
                        End If

                        Call SortPlayer(PPP, stpl, sortType)

                        TVN = 1

                    Case "K":

                        'If any individual leaders have been loaded,
                        'we need to clear them out
                        Erase expLdrPlyrName$

                        Select Case categoryId
                            Case 0: X0 = 0: XX = 44
                            Case 1: X0 = 1: X1 = 13: XX = 45
                            Case 2: X0 = 2: X1 = 14: XX = 46
                            Case 3: X0 = 1: X1 = 13: X2 = 2: X3 = 14: XX = 47
                            Case 4: X0 = 6: XX = 48
                            Case 5: X0 = 2: X1 = 6: XX = 49
                            Case 6: X0 = 4: X1 = 3: XX = 50
                            Case 7: X0 = 9: XX = 51
                            Case 8: X0 = 10: XX = 52
                            Case 9: X0 = 5: XX = 53
                            Case 10: X0 = 5: X1 = 10: X2 = 20: X3 = 25: XX = 54
                            Case 11: X0 = 11: XX = 55
                            Case 12: X0 = 12: XX = 56
                            Case 13: X0 = 13: XX = 57
                            Case 14: X0 = 14: XX = 58
                            Case 15: X0 = 15: XX = 59
                            Case 16: X0 = 16: X1 = 28: XX = 60
                            Case 17: X0 = 17: X1 = 29: XX = 61
                            Case 18: X0 = 16: X1 = 28: X2 = 17: X3 = 29: XX = 62
                            Case 19: X0 = 21: XX = 63
                            Case 20: X0 = 17: X1 = 21: XX = 64
                            Case 21: X0 = 19: X1 = 18: XX = 65
                            Case 22: X0 = 24: XX = 66
                            Case 23: X0 = 25: XX = 67
                            Case 24: X0 = 20: XX = 68
                            Case 25: X0 = 26: XX = 69
                            Case 26: X0 = 27: XX = 70
                            Case 27: X0 = 28: XX = 71
                            Case 28: X0 = 29: XX = 72
                        End Select

                        Select Case categoryId
                            Case 0, 4, 7 To 9, 11 To 15, 19, 22 To 28
                                For I = 1 To totalStats
                                    expLdrTeam$(I) = schedStatTeam$(I, X0)
                                    expLdrVal!(I) = schedStatVal!(I, X0)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                            Case 1, 16
                                For I = 1 To totalStats
                                    expLdrTeam$(I) = schedStatTeam$(I, X0)
                                    expLdrVal!(I) = schedStatVal!(I, X0) - schedStatVal!(I, X1)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                            Case 2, 5, 17, 20
                                For I = 1 To totalStats
                                    expLdrTeam$(I) = schedStatTeam$(I, X0)
                                    expLdrVal!(I) = schedStatVal!(I, X0) + schedStatVal!(I, X1)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                            Case 3, 18
                                For I = 1 To totalStats
                                    expLdrTeam$(I) = schedStatTeam$(I, X0)
                                    expLdrVal!(I) = (schedStatVal!(I, X2) + schedStatVal!(I, X3)) / (schedStatVal!(I, X0) - schedStatVal!(I, X1))
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 2

                            Case 6, 21
                                For I = 1 To totalStats
                                    expLdrTeam$(I) = schedStatTeam$(I, X0)
                                    expLdrVal!(I) = (schedStatVal!(I, X0) / schedStatVal!(I, X1)) * 100
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 2

                            Case 10
                                For I = 1 To totalStats
                                    expLdrTeam$(I) = schedStatTeam$(I, X0)
                                    expLdrVal!(I) = schedStatVal!(I, X2) + schedStatVal!(I, X3) - schedStatVal!(I, X0) - schedStatVal!(I, X1)
                                Next I

                                Call SortTeam(totalStats, stpl, sortType)
                                TVN = 1

                        End Select

                End Select

                If UCase$(key$) = "K" Then
                    maxRecords = totalStats
                    Call DisplayTeamView(printOrView, TVN, maxRecords, categoryId)
                Else
                    maxRecords = PPP
                    If printOrView = 1 Then
                        Call NewLeaders_Screen(TVN, maxRecords, XX)
                    Else
                        'printOrView = 2
                        Call GetListLength(printOrView, LG, maxRecords)

                        userKey$ = GetPrintOptions$

                        If userKey$ <> Chr$(27) Then

                            Select Case userKey$

                                Case "F"
                                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                                    If userFile$ <> "" Then
                                        Call NewLeaders_Report(0, userFile$, XX, LG, TVN)
                                    End If

                                Case "P"
                                    Call NewLeaders_Report(1, "REPORT", XX, LG, TVN)

                            End Select

                        End If

                    End If

                End If

            End If

        End If

    Loop Until UCase$(key$) = "L" Or key$ = Chr$(27)

End Sub


'----------------------------------------
'         SortOptions Subroutine
'----------------------------------------
'This subroutine prompts the users for the
'different sort options available in the
'"Expanded" stats menus.

'statType
'   0 = College
'   1 = Pro

'sortType
'   1: Sort HIGH to LOW
'   2: Sort LOW to HIGH
Sub SortOptions (printOrView, sortType, reselect)

    Do

        Call MenuBox(8, 8, 57, 6, 15, 1)
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
                    Case "P":
                        printOrView = 2
                    Case "V":
                        printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 1

            Case "L":
                Call PrintViewReselect(I2$)

                Select Case UCase$(I2$)
                    Case "P":
                        printOrView = 2
                    Case "V":
                        printOrView = 1
                    Case "R":
                        reselect = 1
                End Select

                sortType = 2

            Case "R", Chr$(27):
                reselect = 1

        End Select

    Loop Until UCase$(I2$) <> "R" Or UCase$(I$) = "R"

End Sub


'----------------------------------------
'      PrintViewReselect Subroutine
'----------------------------------------
'This subroutine presents the end user
'with a choice to Print or View the exist-
'ing category, or select a new category
Sub PrintViewReselect (userOpt$)

    Call MenuBox(9, 15, 40, 4, 15, 0)
    Locate 10, 10

    Color 12
    Locate , 30: Print "SELECT OPTION:"
    Locate , 30: Color 14: Print "P";
    Color 7: Print "RINT"
    Locate , 30: Color 14: Print "V";
    Color 7: Print "IEW"
    Locate , 30: Color 14: Print "R";
    Color 7: Print "E-SELECT OPTION"

    Do
        userOpt$ = GetKeyPress$
    Loop Until UCase$(userOpt$) = "P" Or UCase$(userOpt$) = "V" Or UCase$(userOpt$) = "R"

End Sub


'----------------------------------------
'         SortPlayer Subroutine
'----------------------------------------
'This subroutine sorts the players involved
'in the chosen stat category
'sortType = 1: Low to High
'sortType = 2: High to Low
Sub SortPlayer (PPP, stpl, sortType)

    Shared expLdrVal!()
    Shared expLdrTeam$(), expLdrPlyrName$(), expLdrTD$()

    For I = 2 To PPP

        stpl = stpl + 1
        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType = 1 Then
                If expLdrVal!(J - 1) < expLdrVal!(J) Then
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                    Swap expLdrTD$(J - 1), expLdrTD$(J)
                End If
            End If

            If sortType = 2 Then
                If expLdrVal!(J - 1) > expLdrVal!(J) Then
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrPlyrName$(J - 1), expLdrPlyrName$(J)
                    Swap expLdrTD$(J - 1), expLdrTD$(J)
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
'sortType = 1: Low to High
'sortType = 2: High to Low
Sub SortTeam (sortMax, stpl, sortType)

    Shared expLdrVal!()
    Shared expLdrTeam$()

    For I = 2 To sortMax

        stpl = stpl + 1

        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1

            If sortType = 1 Then
                If expLdrVal!(J - 1) < expLdrVal!(J) Then
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                End If
            End If

            If sortType = 2 Then
                If expLdrVal!(J - 1) > expLdrVal!(J) Then
                    Swap expLdrTeam$(J - 1), expLdrTeam$(J)
                    Swap expLdrVal!(J - 1), expLdrVal!(J)
                End If
            End If

        Next J

    Next I

End Sub


'----------------------------------------
'       NewLeaders_Screen Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the user display
Sub NewLeaders_Screen (TVN, maxRecords, categoryId)

    Shared expLdrVal!()
    Shared expCategories$(), expLdrTeam$(), expLdrPlyrName$(), expLdrTD$()

    userExit% = 0

    Do

        Color 14
        Cls

        Call ShadowBox(1, 5, 67, 23, 7, 1)

        I$ = ""

        For W = 1 To maxRecords

            RR = RR + 1

            ' Every 22 records ? new page (but skip prompt on the first page)
            If (W - 1) Mod 22 = 0 Then

                If W > 1 Then

                    Color 15, 1
                    Locate LAST_ROW - 2, 17: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
                    Color 7, 0: Print Chr$(220)
                    Locate , 18
                    Print String$(50, 223);

                    I$ = GetKeyPress$

                    If I$ = Chr$(27) Then
                        userExit% = 1
                        Exit For
                    End If

                    For I = 3 To 24:
                        Color 1, 1
                        Locate I, 7
                        Print Space$(66)
                    Next

                    RR = 1
                    Color 7, 1

                End If

                ' Print page header
                Locate 2, 11
                Print "PLAYER             TEAM";

                Locate , 60 - (Int(Len(expCategories$(categoryId))) / 2): Print expCategories$(categoryId)

            End If

            If userExit% <> 1 Then

                Color 15
                Locate RR + 2, 7
                If expLdrPlyrName$(W) <> "" Then

                    Print Using "### "; W;
                    Color 3: Print expLdrPlyrName$(W);
                    Print Tab(30); expLdrTeam$(W);:

                    Select Case TVN

                        Case 1:
                            Print Tab(58); Using "#####"; expLdrVal!(W)

                        Case 2:
                            Print Tab(58); Using "###.##"; expLdrVal!(W)

                        Case 3:
                            Print Tab(58); Using "###.#"; expLdrVal!(W);
                            Print Tab(63); expLdrTD$(W)

                        Case 4:
                            Print Tab(58); Using "###.#"; expLdrVal!(W)

                    End Select

                End If

            Else
                Exit Do
            End If

        Next W

        If userExit% <> 1 Then

            Color 15, 1
            Locate LAST_ROW - 2, 17
            Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";

            Color 7, 0: Print Chr$(220)
            Locate , 18
            Print String$(50, 223);

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Color 0, 0
                Locate LAST_ROW - 2, 17: Print Space$(51);
                Locate LAST_ROW - 1, 18: Print Space$(51);

                Locate LAST_ROW - 2, 32
                Color 15, 1
                Print "VIEW AGAIN? (Y/N)";

                Color 7, 0
                Print Chr$(220)
                Locate , 33
                Print String$(17, 223);

                Do
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
'       DisplayTeamView Subroutine
'----------------------------------------
'This subroutine displays the chosen team
'stat category to the user display
Sub DisplayTeamView (printOrView, TVN, maxRecords, categoryId)

    Shared expLdrVal!()
    Shared expLdrTeam$(), expCategories$()

    'expCategories$(44-72)

    userExit% = 0

    Select Case printOrView

        Case 1:

            Do

                Color 14, 0
                Cls

                For W = 1 To maxRecords

                    ' Every 25 records ? new page (but skip prompt on the first page)
                    If (W - 1) Mod 25 = 0 Then

                        If W > 1 Then

                            Color 14
                            Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                            Color 15

                            I$ = GetKeyPress$

                            If I$ = Chr$(27) Then
                                userExit% = 1
                                Exit For
                            End If

                            Color 14, 0
                            Cls

                        End If

                        ' Print page header
                        Locate 2, 1
                        Print "     TEAM                     "; expCategories$(categoryId)

                    End If

                    If userExit% <> 1 Then

                        Color 12
                        Print Using "### "; W;
                        Color 3: Print expLdrTeam$(W);
                        Color 15

                        Select Case TVN

                            Case 1:
                                Print Tab(32); Using "#####"; expLdrVal!(W)

                            Case 2:
                                Print Tab(33); Using ".###"; expLdrVal!(W)

                            Case 3:
                                Print Tab(33); Using "###.#"; expLdrVal!(W)

                        End Select

                    Else

                        Exit Do

                    End If

                Next W

                If userExit% <> 1 Then

                    Color 14
                    Locate LAST_ROW - 1, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)"
                    Color 15

                    I$ = GetKeyPress$

                    If I$ <> Chr$(27) Then

                        Color 14
                        Locate LAST_ROW - 1, 15
                        Print Space$(50);
                        Locate LAST_ROW - 1, 32
                        Print "VIEW AGAIN? (Y/N)";

                        Do
                            A$ = GetKeyPress$
                        Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

                    Else
                        I$ = "N"
                    End If

                End If

            Loop Until UCase$(I$) = "N" Or I$ = Chr$(27) Or UCase$(A$) = "N"

        Case 2:
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                        End If

                    Case "P"
                        printDest = 1
                        userFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, "     TEAM                     "; expTeamCategory$(categoryId)

                For W = 1 To maxRecords

                    Print #1, Using "### "; W;
                    Print #1, expLdrTeam$(W);

                    Select Case TVN

                        Case 1:
                            Print #1, Tab(32); Using "#####"; expLdrVal!(W)

                        Case 2:
                            Print #1, Tab(33); Using ".###"; expLdrVal!(W)

                        Case 3:
                            Print #1, Tab(33); Using "###.#"; expLdrVal!(W)

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

            End If

    End Select

End Sub


'----------------------------------------
'       NewLeaders_Report Subroutine
'----------------------------------------
'This subroutine displays individual player
'stat categories to the default printer
Sub NewLeaders_Report (printDest, destFile$, categoryId, maxRecords, TVN)

    Shared expLdrVal!()
    Shared expCategories$(), expLdrTeam$(), expLdrPlyrName$(), expLdrTD$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    For W = 1 To maxRecords

        If (W - 1) Mod 50 = 0 Then
            Print #1, 
            If printDest = 1 Then Print #1, Chr$(12)
            Print #1, 

            Print #1, "     PLAYER                  TEAM                     "; expCategories$(categoryId)
            Print #1, "     ------                  ----                     ";
            Print #1, String$(Len(expCategories$(categoryId)), 45)
        End If

        Print #1, Using "### "; W;
        Print #1, expLdrPlyrName$(W);
        Print #1, Tab(25); expLdrTeam$(W);

        Select Case TVN

            Case 1
                Print #1, Tab(58); Using "#####"; expLdrVal!(W)

            Case 2
                Print #1, Tab(58); Using "###.##"; expLdrVal!(W)

            Case 3
                Print #1, Tab(58); Using "###.#"; expLdrVal!(W);
                Print #1, Tab(63); expLdrTD$(W)

            Case 4
                Print #1, Tab(58); Using "###.#"; expLdrVal!(W)

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
'        GetListLength Subroutine
'----------------------------------------
'This subroutine prompts the user for the
'maximum number of items to be displayed
'or returned by the standings view. This
'helps the user have a custom view of the
'categories without needing to page through
'every result.
Sub GetListLength (printOrView, listLength, maxItems)

    If printOrView <> 1 Then
        'If printing...

        Do
            Color , 0
            Cls

            Call MenuBox(10, 20, 30, 2, 4, 15)
            Locate 11

            Color 1
            Locate , 25: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub
