'----------------------------------------
'        CompileLeague SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'league, and then relies on subroutines to
'read stats and compare to any existing
'leaders and records
Sub CompileLeague ()

    _Title "3-in-1 Football - Update League Stats"

    Shared leagPtsAgnst!(), leagPtsFor!()

    Shared totalTeamVal!(), leaderVal!(), statsGeneral!()
    Shared divWins(), divLosses(), divTies()

    Shared indRecords!(), teamRecords!()

    Shared indRecDesc$()
    Shared leaderName$(), leaderTeam$()
    Shared memberName$(), teamRecDesc$(), totalTeamName$()

    'Read in any existing records first
    If _FileExists(diskPaths$(1) + "RECORDS.PFB") Then

        Call ReadSeasonRecords("RECORDS.PFB")

    Else

        Erase indRecords!, teamRecords!
        Erase indRecDesc$, teamRecDesc$

    End If

    'Initialize values first
    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0

    teamRecords!(11) = 999
    teamRecords!(22) = 999

    For I = 25 To 27:
        teamRecords!(I) = 9999
    Next

    For I = 29 To 36:
        teamRecords!(I) = 9999
    Next

    teamRecords!(38) = 999
    teamRecords!(39) = 999
    teamRecords!(42) = 999
    teamRecords!(43) = 999

    Erase totalTeamVal!, leaderVal!, statsGeneral!
    Erase memberName$, leaderTeam$, totalTeamName$

    For X = 1 To 60
        memberName$(X) = "*"
    Next

    For X = 1 To 240
        leaderName$(X) = "*"
        leaderTeam$(X) = "*"
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            totalTeamName$(I, I1) = "*"
        Next
    Next

    For I = 1 To 20
        totalTeamVal!(2, I, 5) = 9999
        totalTeamVal!(4, I, 5) = 9999
        totalTeamVal!(6, I, 3) = 9999
        totalTeamVal!(8, I, 8) = 9999
        totalTeamVal!(10, I, 7) = -99
    Next

    C$ = ","

    leagueFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + leagueFile$ + ".TMI" For Input As #3
        Input #3, statType
        Close #3

        If statType <> 1 Then

            MsgText$ = "ERROR!! THIS IS NOT A PRO LEAGUE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call ReadConferenceStats(leagueFile$, 1)

            Erase divWins, divLosses, divTies
            Erase leagPtsAgnst!, leagPtsFor!

            For XW = 1 To TEAMS_PER_LEAGUE

                'Look for * as terminating char
                If memberName$(XW) <> Chr$(42) Then

                    haveStats = FindStatsLeague%(XW, memberName$(XW), totalGames)

                    If (haveStats) Then

                        statName$ = memberName$(XW)
                        sameTeam = 0

                        For X = 1 To TEAMS_PER_LEAGUE:

                            If memberName$(X) = statName$ Then

                                If divWins(X) = fullWins And divLosses(X) = fullLosses And divTies(X) = fullTies Then

                                    sameTeam = 1
                                    Exit For

                                Else

                                    divWins(X) = fullWins
                                    divLosses(X) = fullLosses
                                    divTies(X) = fullTies

                                End If

                            End If
                            'alternatively, use a Do / Until loop; sameTeam = 1 or X = 30

                        Next X

                        If sameTeam = 0 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                            Call ReadStatsLeaders(memberName$(XW), totalGames, TM$, 1)
                            Call CalcStatTotalsPro(totalGames, TM$, statName$, finalUpdate$)
                            Call CalcStatLeaders(totalGames, TM$, statName$, finalUpdate$)

                        End If

                    End If

                End If

            Next XW

            Call SortStandingsLeague
            Call PostLeadersLeague(leagueFile$)
            Call PostRecordsPro

        End If 'Done checking file type

    End If

End Sub


'----------------------------------------
'        FindStatsLeague Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the league and loads them if
'found, notifying the user
Function FindStatsLeague% (XW, statFile$, totalGames)

    Shared oppScore(), teamScore()

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Locate , 1
        Print "NO STATS FOUND FOR "; statFile$
        FindStatsLeague% = 0

    Else

        Call ReadScheduleStats(statFile$, XW, totalGames)

        fullWins = 0
        fullLosses = 0
        fullTies = 0

        For I = 1 To totalGames

            If teamScore(I) > oppScore(I) Then
                fullWins = fullWins + 1
            End If
            If teamScore(I) < oppScore(I) Then
                fullLosses = fullLosses + 1
            End If
            If teamScore(I) = oppScore(I) Then
                fullTies = fullTies + 1
            End If

        Next

        Cls

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsLeague% = 1

    End If

End Function


'----------------------------------------
'       ReadStatsLeaders Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the league
Sub ReadStatsLeaders (statFile$, totalGames, teamName$, proQB)

    Shared puntAvg_COMP!

    Shared statsGI(), statsGS()

    Shared statsFGA!(), statsFGM!()
    Shared compFGA!(), compFGM!()
    Shared compKR!(), compPR!(), compQB!(), compRB!(), compWR!()
    Shared compPAT!(), compPAM!()
    Shared defTotals!(), compPAPct!()
    Shared compFGPct!()
    Shared offTotals!()

    Shared statsGeneral!()
    Shared statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLPR!(), statsLRec!(), statsLRush!()

    Shared leaderAllPurpTDs!(), leaderAllPurpYds!()
    Shared compPunts!(), totalScoring!()

    Shared compKR$(), compKicker$(), compPR$()
    Shared compQB$(), compRB$(), compWR$()
    Shared memberName$(), intName$()
    Shared sackName$()
    Shared statsLRec$(), statsLFG$(), statsLInt$()
    Shared statsLKR$(), statsLPR$()
    Shared statsLPass$(), statsLRush$()
    Shared leaderAllPurpName$()

    Call ReadTeamStatsComp(teamName$, statFile$)

    For I = 1 To 10:
        For J = 1 To 6:
            compRB!(I, J) = statsGeneral!(I, J)
        Next
    Next

    For I = 1 To 6:
        For J = 1 To 3:
            compWR!(I + 10, J) = statsGeneral!(I, J + 6)
        Next
    Next

    For I = 1 To 4:
        For J = 1 To 5:
            compQB!(I, J) = statsGeneral!(I, J + 9)
        Next
    Next

    For I = 1 To 3:
        For J = 1 To 3:
            compKR!(I, J) = statsGeneral!(I, J + 14)
        Next
    Next

    For I = 1 To 3:
        For J = 1 To 3:
            compPR!(I, J) = statsGeneral!(I + 3, J + 14)
        Next
    Next

    compPunts!(8) = offTotals!(7)
    compPunts!(9) = offTotals!(8)

    compFGM!(1) = statsGeneral!(4, 18)
    compFGA!(1) = statsGeneral!(5, 18)

    compPAM!(1) = statsGeneral!(6, 18)
    compPAT!(1) = statsGeneral!(7, 18)

    compFGM!(2) = statsGeneral!(8, 18)
    compFGA!(2) = statsGeneral!(9, 18)

    compPAM!(2) = statsGeneral!(10, 18)
    compPAT!(2) = statsGeneral!(11, 18)

    For I = 1 To 10:
        'Rush Yds + Rec yds
        compRB!(I, 7) = compRB!(I, 2)
        'Rush TDs + Rec TDs
        compRB!(I, 8) = compRB!(I, 3)
        compWR$(I) = compRB$(I)
        compWR!(I, 2) = compRB!(I, 5)
        compWR!(I, 1) = compRB!(I, 4)
        compWR!(I, 3) = compRB!(I, 6)
    Next

    For I = 1 To 10
        leaderAllPurpName$(I) = compRB$(I)
        leaderAllPurpYds!(I) = compRB!(I, 7) + compWR!(I, 2)
        leaderAllPurpTDs!(I) = compRB!(I, 8) + compWR!(I, 3)
    Next

    For I = 1 To 6
        leaderAllPurpName$(I + 10) = compWR$(I + 10)
        leaderAllPurpYds!(I + 10) = compWR!(I + 10, 2)
        leaderAllPurpTDs!(I + 10) = compWR!(I + 10, 3)
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If compKR$(I1) = leaderAllPurpName$(I) Then
                leaderAllPurpYds!(I) = leaderAllPurpYds!(I) + compKR!(I1, 2)
                leaderAllPurpTDs!(I) = leaderAllPurpTDs!(I) + compKR!(I1, 3)
            End If
        Next
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If compPR$(I1) = leaderAllPurpName$(I) Then
                leaderAllPurpYds!(I) = leaderAllPurpYds!(I) + compPR!(I1, 2)
                leaderAllPurpTDs!(I) = leaderAllPurpTDs!(I) + compPR!(I1, 3)
            End If
        Next
    Next

    For I = 1 To 16
        totalScoring!(I) = leaderAllPurpTDs!(I) * 6
    Next

    For I = 1 To 2
        leaderAllPurpName$(16 + I) = compKicker$(I)
        totalScoring!(16 + I) = compPAM!(I) + compFGM!(I) * 3
    Next

    For I = 1 To 10:
        compRB!(I, 9) = 0
        If compRB!(I, 1) > 0 Then
            compRB!(I, 9) = Int(compRB!(I, 2) / compRB!(I, 1) * 10) / 10
        End If
    Next

    For I = 1 To 10:
        compRB!(I, 10) = 0
        If compRB!(I, 4) > 0 Then
            compRB!(I, 10) = Int(compRB!(I, 5) / compRB!(I, 4) * 10) / 10
        End If
    Next

    For I = 1 To 16:
        compWR!(I, 4) = 0
        If compWR!(I, 1) > 0 Then
            compWR!(I, 4) = Int(compWR!(I, 2) / compWR!(I, 1) * 10) / 10
        End If
    Next

    For I = 1 To 4:
        compQB!(I, 6) = 0
        If proQB = 1 Then
            Call QBRatingPro(I)
        Else
            Call QBRatingColl(I)
        End If
    Next

    For I = 1 To 3:
        compKR!(I, 4) = 0
        If compKR!(I, 1) > 0 And compKR!(I, 1) >= totalGames Then
            compKR!(I, 4) = Int(compKR!(I, 2) / compKR!(I, 1) * 10) / 10
        Else
            compKR!(I, 4) = 0
        End If

    Next

    For I = 1 To 3:
        compPR!(I, 4) = 0
        If compPR!(I, 1) > 0 And compPR!(I, 1) >= totalGames Then
            compPR!(I, 4) = Int(compPR!(I, 2) / compPR!(I, 1) * 10) / 10
        Else
            compPR!(I, 4) = 0
        End If
    Next

    puntAvg_COMP! = 0

    If compPunts!(8) > 0 Then
        puntAvg_COMP! = Int(compPunts!(9) / compPunts!(8) * 10) / 10
    End If

    Erase compPAPct!, compFGPct!

    For I = 1 To 2
        If compFGA!(I) >= totalGames Then
            compFGPct!(I) = Int(compFGM!(I) / compFGA!(I) * 1000) / 10
        End If
        If compPAT!(I) >= totalGames Then
            compPAPct!(I) = Int(compPAM!(I) / compPAT!(I) * 1000) / 10
        End If
    Next

End Sub


'----------------------------------------
'       CalcStatLeaders Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CalcStatLeaders (totalGames, teamName$, statName$, finalUpdate$)

    Shared fumbGain, puntAvg_COMP!
    Shared punterName$()

    Shared compKR$(), compKicker$(), compPR$(), compQB$()
    Shared compRB$(), compWR$(), leaderName$(), leaderTeam$()

    Shared indRecDesc$()
    Shared intName$()

    Shared leaderAllPurpName$()

    Shared sackName$()
    Shared statsLKR$(), statsLPR$()
    Shared statsLPass$(), statsLRush$()

    Shared statsLRec$(), statsLFG$(), statsLInt$(), statsLPass$(), statsLRush$()
    Shared statsLKR$(), statsLPR$()

    Shared compKR!(), compPR!(), compQB!(), compRB!(), compWR!()

    Shared compPAT!(), compPAM!(), compPAPct!()
    Shared compFGA!(), compFGPct!(), compFGM!()
    Shared indRecords!()
    Shared leaderVal!()

    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!()
    Shared statsLPR!()

    Shared compPunts!(), totalScoring!(), leaderAllPurpYds!()

    Shared statsGI(), statsGS()

    '---------------
    ' Running Backs
    '---------------
    
    minRushAtt = totalGames * 10
    minRecept = totalGames * 3
    minComp = totalGames * 15
    minReturns = totalGames * 1.2
    
    For X = 1 To 10

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compRB$(X)

        If compRB$(X) <> "OTHERS" Then

            If compRB!(X, 1) > indRecords!(0) Then
                indRecords!(0) = compRB!(X, 1)
                indRecDesc$(0, 0) = compRB$(X)
                indRecDesc$(0, 1) = teamName$
            End If

            If compRB!(X, 2) > indRecords!(1) Then
                indRecords!(1) = compRB!(X, 2)
                indRecDesc$(1, 0) = compRB$(X)
                indRecDesc$(1, 1) = teamName$
            End If

            If compRB!(X, 3) > indRecords!(2) Then
                indRecords!(2) = compRB!(X, 3)
                indRecDesc$(2, 0) = compRB$(X)
                indRecDesc$(2, 1) = teamName$
            End If

            If compRB!(X, 9) > indRecords!(3) And UCase$(finalUpdate$) = "Y" And compRB!(X, 1) >= minRushAtt Then
                'Rushing Average
                indRecords!(3) = compRB!(X, 9)
                indRecDesc$(3, 0) = compRB$(X)
                indRecDesc$(3, 1) = teamName$
            End If

            If statsLRush!(X - 1) > indRecords!(4) Then
                'Longest Run
                indRecords!(4) = statsLRush!(X - 1)
                indRecDesc$(4, 0) = compRB$(X)
                indRecDesc$(4, 1) = teamName$
                indRecDesc$(4, 2) = statsLRush$(X - 1, 0)
            End If

            For Y = 1 To 20:

                If leaderName$(Y) = compRB$(X) And leaderTeam$(Y) = statName$ Then

                    leaderVal!(Y, 1) = compRB!(X, 9) 'Rush Avg
                    leaderVal!(Y, 2) = compRB!(X, 1) 'Rush Att

                    'Only include players with 10 ATT / game
                    If leaderVal!(Y, 2) < minRushAtt Then
                        leaderName$(Y) = "*"
                        leaderTeam$(Y) = "*"
                        leaderVal!(Y, 1) = 0
                        leaderVal!(Y, 2) = 0
                    End If

                    For Y = 1 To 19:

                        For Z = Y + 1 To 20:

                            If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                                Swap leaderTeam$(Y), leaderTeam$(Z)
                                Swap leaderName$(Y), leaderName$(Z)

                                For Z1 = 1 To 2:
                                    Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                                Next Z1

                            End If

                        Next Z

                    Next Y

                    GoTo 3118
                End If

            Next Y

            If Not (compRB!(X, 1) < minRushAtt Or compRB!(X, 9) < leaderVal!(20, 1)) Then

                leaderName$(20) = compRB$(X)
                leaderTeam$(20) = statName$
                leaderVal!(20, 1) = compRB!(X, 9)
                leaderVal!(20, 2) = compRB!(X, 1)

                For Y = 1 To 19:

                    For Z = Y + 1 To 20:

                        If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                            Swap leaderTeam$(Y), leaderTeam$(Z)
                            Swap leaderName$(Y), leaderName$(Z)
                            For Z1 = 1 To 2
                                Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                            Next Z1
                        End If

                    Next Z

                Next Y

            End If

            3118 '
            For Z = 41 To 50
                If leaderName$(Z) = compRB$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = Int(compRB!(X, 2))
                    GoTo 3123
                End If
            Next Z

            If Int(compRB!(X, 2)) > leaderVal!(50, 1) Then

                leaderVal!(50, 1) = Int(compRB!(X, 2))
                leaderName$(50) = compRB$(X)
                leaderTeam$(50) = statName$

                3123 '
                For Z = 41 To 49:
                    For Z1 = Z + 1 To 50:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 61 To 70
                If leaderName$(Z) = compRB$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compRB!(X, 3)
                    GoTo 3155
                End If
            Next Z

            If compRB!(X, 3) > leaderVal!(70, 1) Then

                leaderVal!(70, 1) = compRB!(X, 3)
                leaderName$(70) = compRB$(X)
                leaderTeam$(70) = statName$

                3155 '
                For Z = 61 To 69:
                    For Z1 = Z + 1 To 70:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 81 To 90
                If leaderName$(Z) = compRB$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compRB!(X, 1)
                    GoTo 3525
                End If
            Next Z

            If compRB!(X, 1) > leaderVal!(90, 1) Then

                leaderVal!(90, 1) = compRB!(X, 1)
                leaderName$(90) = compRB$(X)
                leaderTeam$(90) = statName$

                3525 '
                For Z = 81 To 89:
                    For Z1 = Z + 1 To 90:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' Receivers
    '---------------
    For X = 1 To 16

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compWR$(X)

        If compWR$(X) <> "OTHERS" Then

            If compWR!(X, 1) > indRecords!(5) Then
                indRecords!(5) = compWR!(X, 1)
                indRecDesc$(5, 0) = compWR$(X)
                indRecDesc$(5, 1) = teamName$
            End If

            If compWR!(X, 2) > indRecords!(6) Then
                indRecords!(6) = compWR!(X, 2)
                indRecDesc$(6, 0) = compWR$(X)
                indRecDesc$(6, 1) = teamName$
            End If

            If compWR!(X, 3) > indRecords!(7) Then
                indRecords!(7) = compWR!(X, 3)
                indRecDesc$(7, 0) = compWR$(X)
                indRecDesc$(7, 1) = teamName$
            End If

            'Look for at least 3 catches / game
            If compWR!(X, 4) > indRecords!(8) And UCase$(finalUpdate$) = "Y" And compWR!(X, 1) >= minRecept Then
                'Receiving Average
                indRecords!(8) = compWR!(X, 4)
                indRecDesc$(8, 0) = compWR$(X)
                indRecDesc$(8, 1) = teamName$
            End If

            If statsLRec!(X - 1) > indRecords!(9) Then
                'Longest Catch
                indRecords!(9) = statsLRec!(X - 1)
                indRecDesc$(9, 0) = compWR$(X)
                indRecDesc$(9, 1) = teamName$
                indRecDesc$(9, 2) = statsLRec$(X - 1, 0)
            End If

            For Z = 21 To 40:

                If leaderName$(Z) = compWR$(X) And leaderTeam$(Z) = statName$ Then

                    leaderVal!(Z, 1) = compWR!(X, 4)
                    leaderVal!(Z, 2) = compWR!(X, 1)

                    'Only include players with 3 RECs / game
                    If leaderVal!(Z, 2) < minRecept Then
                        leaderName$(Z) = "*"
                        leaderTeam$(Z) = "*"
                        leaderVal!(Z, 1) = 0
                        leaderVal!(Z, 2) = 0
                    End If

                    For Y = 21 To 39:
                        For Z = Y + 1 To 40:
                            If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                                Swap leaderTeam$(Y), leaderTeam$(Z)
                                Swap leaderName$(Y), leaderName$(Z)
                                For Z1 = 1 To 2:
                                    Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                                Next Z1
                            End If

                        Next Z

                    Next Y

                    GoTo 3178

                End If

            Next Z

            If Not ((compWR!(X, 1) < minRecept Or compWR!(X, 4) < leaderVal!(40, 1))) Then

                leaderName$(40) = compWR$(X)
                leaderTeam$(40) = statName$
                leaderVal!(40, 1) = compWR!(X, 4)
                leaderVal!(40, 2) = compWR!(X, 1)

                For Y = 21 To 39:
                    For Z = Y + 1 To 40:
                        If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then

                            Swap leaderTeam$(Y), leaderTeam$(Z)
                            Swap leaderName$(Y), leaderName$(Z)
                            For Z1 = 1 To 2:
                                Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                            Next Z1

                        End If

                    Next Z

                Next Y

            End If

            3178 '
            For Z = 51 To 60
                If leaderName$(Z) = compWR$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = Int(compWR!(X, 2))
                    GoTo 3183
                End If
            Next Z

            If Int(compWR!(X, 2)) > leaderVal!(60, 1) Then
                leaderVal!(60, 1) = Int(compWR!(X, 2))
                leaderName$(60) = compWR$(X)
                leaderTeam$(60) = statName$

                3183 '
                For Z = 51 To 59:
                    For Z1 = Z + 1 To 60:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z
            End If

            For Z = 91 To 100
                If leaderName$(Z) = compWR$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compWR!(X, 1)
                    GoTo 3215
                End If
            Next Z

            If compWR!(X, 1) > leaderVal!(100, 1) Then

                leaderVal!(100, 1) = compWR!(X, 1)
                leaderName$(100) = compWR$(X)
                leaderTeam$(100) = statName$

                3215 '
                For Z = 91 To 99:
                    For Z1 = Z + 1 To 100
                        If leaderVal!(Z, 1) <= leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 71 To 80
                If leaderName$(Z) = compWR$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compWR!(X, 3)
                    GoTo 3230
                End If
            Next Z

            If compWR!(X, 3) > leaderVal!(80, 1) Then

                leaderVal!(80, 1) = compWR!(X, 3)
                leaderName$(80) = compWR$(X)
                leaderTeam$(80) = statName$

                3230 '
                For Z = 71 To 79:
                    For Z1 = Z + 1 To 80:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' All-Purpose Yds
    '---------------
    For X = 1 To 16

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print leaderAllPurpName$(X)

        If leaderAllPurpName$(X) <> "OTHERS" Then

            If leaderAllPurpYds!(X) > indRecords!(18) Then
                indRecords!(18) = leaderAllPurpYds!(X)
                indRecDesc$(18, 0) = leaderAllPurpName$(X)
                indRecDesc$(18, 1) = teamName$
            End If

            If totalScoring!(X) > indRecords!(19) Then
                indRecords!(19) = totalScoring!(X)
                indRecDesc$(19, 0) = leaderAllPurpName$(X)
                indRecDesc$(19, 1) = teamName$
            End If

            If totalScoring!(X) > indRecords!(21) Then
                indRecords!(21) = totalScoring!(X)
                indRecDesc$(21, 0) = leaderAllPurpName$(X)
                indRecDesc$(21, 1) = teamName$
            End If

            For Z = 101 To 110
                If leaderName$(Z) = leaderAllPurpName$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = leaderAllPurpYds!(X)
                    GoTo 3245
                End If
            Next Z

            If leaderAllPurpYds!(X) > leaderVal!(110, 1) Then

                leaderVal!(110, 1) = leaderAllPurpYds!(X)
                leaderName$(110) = leaderAllPurpName$(X)
                leaderTeam$(110) = statName$

                3245 '
                For Z = 101 To 109:
                    For Z1 = Z + 1 To 110:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 111 To 120
                If leaderName$(Z) = leaderAllPurpName$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = totalScoring!(X)
                    GoTo 3260
                End If
            Next Z

            If totalScoring!(X) > leaderVal!(120, 1) Then

                leaderVal!(120, 1) = totalScoring!(X)
                leaderName$(120) = leaderAllPurpName$(X)
                leaderTeam$(120) = statName$

                3260 '
                For Z = 111 To 119:
                    For Z1 = Z + 1 To 120:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    For X = 17 To 18

        If totalScoring!(X) > indRecords!(20) Then
            indRecords!(20) = totalScoring!(X)
            indRecDesc$(20, 0) = leaderAllPurpName$(X)
            indRecDesc$(20, 1) = teamName$
        End If

        If totalScoring!(X) > indRecords!(21) Then
            indRecords!(21) = totalScoring!(X)
            indRecDesc$(21, 0) = leaderAllPurpName$(X)
            indRecDesc$(21, 1) = teamName$
        End If

        For Z = 191 To 200
            If leaderName$(Z) = leaderAllPurpName$(X) And leaderTeam$(Z) = statName$ Then
                leaderVal!(Z, 1) = totalScoring!(X)
                GoTo 3505
            End If
        Next Z

        If totalScoring!(X) > leaderVal!(200, 1) Then

            leaderVal!(200, 1) = totalScoring!(X)
            leaderName$(200) = leaderAllPurpName$(X)
            leaderTeam$(200) = statName$

            3505 '
            For Z = 191 To 199:
                For Z1 = Z + 1 To 200:
                    If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                    End If
                Next Z1
            Next Z

        End If

    Next X

    '---------------
    ' Quarterbacks
    '---------------
    For X = 1 To 4

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compQB$(X)

        If compQB$(X) <> "OTHERS" Then

            If compQB!(X, 1) > indRecords!(10) Then
                indRecords!(10) = compQB!(X, 1)
                indRecDesc$(10, 0) = compQB$(X)
                indRecDesc$(10, 1) = teamName$
            End If

            If compQB!(X, 2) > indRecords!(11) Then
                indRecords!(11) = compQB!(X, 2)
                indRecDesc$(11, 0) = compQB$(X)
                indRecDesc$(11, 1) = teamName$
            End If

            If compQB!(X, 5) > indRecords!(12) Then
                indRecords!(12) = compQB!(X, 5)
                indRecDesc$(12, 0) = compQB$(X)
                indRecDesc$(12, 1) = teamName$
            End If

            If compQB!(X, 3) > indRecords!(13) Then
                indRecords!(13) = compQB!(X, 3)
                indRecDesc$(13, 0) = compQB$(X)
                indRecDesc$(13, 1) = teamName$
            End If

            If compQB!(X, 4) > indRecords!(14) Then
                indRecords!(14) = compQB!(X, 4)
                indRecDesc$(14, 0) = compQB$(X)
                indRecDesc$(14, 1) = teamName$
            End If

            If compQB!(X, 6) > indRecords!(15) And UCase$(finalUpdate$) = "Y" And compQB!(X, 1) >= minComp Then
                indRecords!(15) = compQB!(X, 6)
                indRecDesc$(15, 0) = compQB$(X)
                indRecDesc$(15, 1) = teamName$
            End If

            If compQB!(X, 7) > indRecords!(16) And UCase$(finalUpdate$) = "Y" And compQB!(X, 1) >= minComp Then
                indRecords!(16) = compQB!(X, 7)
                indRecDesc$(16, 0) = compQB$(X)
                indRecDesc$(16, 1) = teamName$
            End If

            If statsLPass!(X - 1) > indRecords!(17) Then
                indRecords!(17) = statsLPass!(X - 1)
                indRecDesc$(17, 0) = compQB$(X)
                indRecDesc$(17, 1) = teamName$
                indRecDesc$(17, 2) = statsLPass$(X - 1, 0)
            End If

            For Y = 121 To 130:

                If leaderName$(Y) = compQB$(X) And leaderTeam$(Y) = statName$ Then

                    leaderVal!(Y, 1) = compQB!(X, 6)
                    leaderVal!(Y, 2) = compQB!(X, 1)

                    If leaderVal!(Y, 2) < minComp Then
                        leaderName$(Y) = "*"
                        leaderTeam$(Y) = "*"
                        leaderVal!(Y, 1) = 0
                        leaderVal!(Y, 2) = 0
                    End If

                    For Y = 121 To 129:
                        For Z = Y + 1 To 130:

                            If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                                Swap leaderTeam$(Y), leaderTeam$(Z)
                                Swap leaderName$(Y), leaderName$(Z)
                                For Z1 = 1 To 2:
                                    Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                                Next Z1

                            End If

                        Next Z
                    Next Y

                    GoTo 3279

                End If

            Next Y

            If Not (compQB!(X, 1) < minComp Or compQB!(X, 6) < leaderVal!(130, 1)) Then

                leaderName$(130) = compQB$(X)
                leaderTeam$(130) = statName$
                leaderVal!(130, 1) = compQB!(X, 6)
                leaderVal!(130, 2) = compQB!(X, 1)

                For Y = 121 To 129:

                    For Z = Y + 1 To 130:

                        If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                            Swap leaderTeam$(Y), leaderTeam$(Z)
                            Swap leaderName$(Y), leaderName$(Z)

                            For Z1 = 1 To 2:
                                Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                            Next Z1

                        End If

                    Next Z

                Next Y

            End If

            3279 '
            For Y = 131 To 140:

                If leaderName$(Y) = compQB$(X) And leaderTeam$(Y) = statName$ Then

                    leaderVal!(Y, 1) = compQB!(X, 7)
                    leaderVal!(Y, 2) = compQB!(X, 1)

                    If leaderVal!(Y, 2) < minComp Then
                        leaderName$(Y) = "*"
                        leaderTeam$(Y) = "*"
                        leaderVal!(Y, 1) = 0
                        leaderVal!(Y, 2) = 0
                    End If

                    For Y = 131 To 139:
                        For Z = Y + 1 To 140:

                            If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                                Swap leaderTeam$(Y), leaderTeam$(Z)
                                Swap leaderName$(Y), leaderName$(Z)

                                For Z1 = 1 To 2:
                                    Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                                Next Z1
                            End If

                        Next Z
                    Next Y

                    GoTo 3287

                End If

            Next Y

            If Not (compQB!(X, 1) < minComp Or compQB!(X, 6) < leaderVal!(140, 1)) Then

                leaderName$(140) = compQB$(X)
                leaderTeam$(140) = statName$
                leaderVal!(140, 1) = compQB!(X, 7)
                leaderVal!(140, 2) = compQB!(X, 1)

                For Y = 131 To 139:
                    For Z = Y + 1 To 140:

                        If leaderVal!(Y, 1) < leaderVal!(Z, 1) Then
                            Swap leaderTeam$(Y), leaderTeam$(Z)
                            Swap leaderName$(Y), leaderName$(Z)

                            For Z1 = 1 To 2:
                                Swap leaderVal!(Y, Z1), leaderVal!(Z, Z1)
                            Next Z1
                        End If

                    Next Z
                Next Y

            End If

            3287 '
            For Z = 141 To 150
                If leaderName$(Z) = compQB$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = Int(compQB!(X, 4))
                    GoTo 3294
                End If
            Next Z

            If Int(compQB!(X, 4)) > leaderVal!(150, 1) Then

                leaderVal!(150, 1) = Int(compQB!(X, 4))
                leaderName$(150) = compQB$(X)

                3294 '
                leaderTeam$(150) = statName$

                For Z = 141 To 149:
                    For Z1 = Z + 1 To 150:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 151 To 160
                If leaderName$(Z) = compQB$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compQB!(X, 5)
                    GoTo 3307
                End If
            Next Z

            If compQB!(X, 5) > leaderVal!(160, 1) Then

                leaderVal!(160, 1) = compQB!(X, 5)
                leaderName$(160) = compQB$(X)
                leaderTeam$(160) = statName$

                3307 '
                For Z = 151 To 159:
                    For Z1 = Z + 1 To 160:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' Kick Returns
    '---------------
    For X = 1 To 3

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compKR$(X)

        If compKR$(X) <> "OTHERS" Then

            If compKR!(X, 1) > indRecords!(22) Then
                indRecords!(22) = compKR!(X, 1)
                indRecDesc$(22, 0) = compKR$(X)
                indRecDesc$(22, 1) = teamName$
            End If

            If compKR!(X, 2) > indRecords!(23) Then
                indRecords!(23) = compKR!(X, 2)
                indRecDesc$(23, 0) = compKR$(X)
                indRecDesc$(23, 1) = teamName$
            End If

            If compKR!(X, 3) > indRecords!(24) Then
                indRecords!(24) = compKR!(X, 3)
                indRecDesc$(24, 0) = compKR$(X)
                indRecDesc$(24, 1) = teamName$
            End If

            If compKR!(X, 4) > indRecords!(25) And UCase$(finalUpdate$) = "Y" And compKR!(X, 1) >= minReturns Then
                indRecords!(25) = compKR!(X, 4)
                indRecDesc$(25, 0) = compKR$(X)
                indRecDesc$(25, 1) = teamName$
            End If

            If statsLKR!(X - 1) > indRecords!(26) Then
                indRecords!(26) = statsLKR!(X - 1)
                indRecDesc$(26, 0) = compKR$(X)
                indRecDesc$(26, 1) = teamName$
                indRecDesc$(26, 2) = statsLKR$(X - 1, 0)
            End If

            For Z = 161 To 170
                If leaderName$(Z) = compKR$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compKR!(X, 4)
                    leaderVal!(Z, 2) = compKR!(X, 1)
                    GoTo 3345
                End If
            Next Z

            If Not (compKR!(X, 1) < minReturns Or compKR!(X, 4) <= leaderVal!(170, 1)) Then

                leaderVal!(170, 1) = compKR!(X, 4)
                leaderName$(170) = compKR$(X)
                leaderTeam$(170) = statName$
                leaderVal!(170, 2) = compKR!(X, 1)

                3345 '
                For Z = 161 To 169:
                    For Z1 = Z + 1 To 170:

                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            For MN = 1 To 2
                                Swap leaderVal!(Z1, MN), leaderVal!(Z, MN)
                            Next MN
                        End If

                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' Punt Returns
    '---------------
    For X = 1 To 3

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compPR$(X)

        If compPR$(X) <> "OTHERS" Then

            If compPR!(X, 1) > indRecords!(27) Then
                indRecords!(27) = compPR!(X, 1)
                indRecDesc$(27, 0) = compPR$(X)
                indRecDesc$(27, 1) = teamName$
            End If

            If compPR!(X, 2) > indRecords!(28) Then
                indRecords!(28) = compPR!(X, 2)
                indRecDesc$(28, 0) = compPR$(X)
                indRecDesc$(28, 1) = teamName$
            End If

            If compPR!(X, 3) > indRecords!(29) Then
                indRecords!(29) = compPR!(X, 3)
                indRecDesc$(29, 0) = compPR$(X)
                indRecDesc$(29, 1) = teamName$
            End If

            If compPR!(X, 4) > indRecords!(30) And UCase$(finalUpdate$) = "Y" And compPR!(X, 1) >= minReturns Then
                indRecords!(30) = compPR!(X, 4)
                indRecDesc$(30, 0) = compPR$(X)
                indRecDesc$(30, 1) = teamName$
            End If

            If statsLPR!(X - 1) > indRecords!(31) Then
                indRecords!(31) = statsLPR!(X - 1)
                indRecDesc$(31, 0) = compPR$(X)
                indRecDesc$(31, 1) = teamName$
                indRecDesc$(31, 2) = statsLPR$(X - 1, 0)
            End If

            For Z = 171 To 180
                If leaderName$(Z) = compPR$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = compPR!(X, 4)
                    leaderVal!(Z, 2) = compPR!(X, 1)
                    GoTo 3365
                End If
            Next Z

            If Not (compPR!(X, 1) < minReturns And compPR!(X, 4) <= leaderVal!(180, 1)) Then

                leaderVal!(180, 1) = compPR!(X, 4)
                leaderName$(180) = compPR$(X)
                leaderTeam$(180) = statName$
                leaderVal!(180, 2) = compPR!(X, 1)

                3365 '
                For Z = 171 To 179:
                    For Z1 = Z + 1 To 180:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            For MN = 1 To 2
                                Swap leaderVal!(Z1, MN), leaderVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' Punting
    '---------------
    'Color 14
    Print "CHECKING STATS OF ";
    Color 15
    Print punterName$(0)

    If compPunts!(8) > indRecords!(32) Then
        indRecords!(32) = compPunts!(8)
        indRecDesc$(32, 0) = punterName$(0)
        indRecDesc$(32, 1) = teamName$
    End If

    If compPunts!(9) > indRecords!(33) Then
        indRecords!(33) = compPunts!(9)
        indRecDesc$(33, 0) = punterName$(0)
        indRecDesc$(33, 1) = teamName$
    End If

    If puntAvg_COMP! > indRecords!(34) And UCase$(finalUpdate$) = "Y" Then
        indRecords!(34) = puntAvg_COMP!
        indRecDesc$(34, 0) = punterName$(0)
        indRecDesc$(34, 1) = teamName$
    End If

    For Z = 181 To 190
        If leaderName$(Z) = punterName$(0) And leaderTeam$(Z) = statName$ Then
            leaderVal!(Z, 1) = puntAvg_COMP!
            leaderVal!(Z, 2) = compPunts!(8)
            GoTo 3382
        End If
    Next Z

    If puntAvg_COMP! > leaderVal!(190, 1) Then

        leaderVal!(190, 1) = puntAvg_COMP!
        leaderName$(190) = punterName$(0)
        leaderTeam$(190) = statName$
        leaderVal!(190, 2) = compPunts!(8)

        3382 '
        For Z = 181 To 189:
            For Z1 = Z + 1 To 190:
                If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                    Swap leaderName$(Z1), leaderName$(Z)
                    Swap leaderTeam$(Z1), leaderTeam$(Z)
                    For MN = 1 To 2
                        Swap leaderVal!(Z1, MN), leaderVal!(Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    '---------------
    ' Place Kicks
    '---------------
    For X = 1 To 2

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compKicker$(X)

        If compFGM!(X) > indRecords!(35) Then
            indRecords!(35) = compFGM!(X)
            indRecDesc$(35, 0) = compKicker$(X)
            indRecDesc$(35, 1) = teamName$
        End If

        If compFGA!(X) > indRecords!(36) Then
            indRecords!(36) = compFGA!(X)
            indRecDesc$(36, 0) = compKicker$(X)
            indRecDesc$(36, 1) = teamName$
        End If

        If compFGPct!(X) > indRecords!(37) And UCase$(finalUpdate$) = "Y" Then
            indRecords!(37) = compFGPct!(X)
            indRecDesc$(37, 0) = compKicker$(X)
            indRecDesc$(37, 1) = teamName$
        End If

        If statsLFG!(X - 1) > indRecords!(38) Then
            indRecords!(38) = statsLFG!(X - 1)
            indRecDesc$(38, 0) = compKicker$(X)
            indRecDesc$(38, 1) = teamName$
            indRecDesc$(38, 2) = statsLFG$(X - 1)
        End If

        If compPAM!(X) > indRecords!(39) Then
            indRecords!(39) = compPAM!(X)
            indRecDesc$(39, 0) = compKicker$(X)
            indRecDesc$(39, 1) = teamName$
        End If

        If compPAT!(X) > indRecords!(40) Then
            indRecords!(40) = compPAT!(X)
            indRecDesc$(40, 0) = compKicker$(X)
            indRecDesc$(40, 1) = teamName$
        End If

        If compPAPct!(X) > indRecords!(41) And UCase$(finalUpdate$) = "Y" Then
            indRecords!(41) = compPAPct!(X)
            indRecDesc$(41, 0) = compKicker$(X)
            indRecDesc$(41, 1) = teamName$
        End If

        For Z = 201 To 210
            If leaderName$(Z) = compKicker$(X) And leaderTeam$(Z) = statName$ Then
                leaderVal!(Z, 1) = fumbGain
                leaderVal!(Z, 2) = compFGA!(X)
                GoTo 3396
            End If
        Next Z

        If compFGPct!(X) > leaderVal!(210, 1) Then

            leaderVal!(210, 1) = compFGPct!(X)
            leaderName$(210) = compKicker$(X)
            leaderTeam$(210) = statName$
            leaderVal!(210, 2) = compFGA!(X)

            3396 '
            For Z = 201 To 209:
                For Z1 = Z + 1 To 210:
                    If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)

                        For MN = 1 To 2
                            Swap leaderVal!(Z1, MN), leaderVal!(Z, MN)
                        Next MN
                    End If
                Next Z1
            Next Z

        End If

        For Z = 211 To 220
            If leaderName$(Z) = compKicker$(X) And leaderTeam$(Z) = statName$ Then
                leaderVal!(Z, 1) = compPAPct!(X)
                leaderVal!(Z, 2) = compPAT!(X)
                GoTo 3409
            End If
        Next Z

        If compPAPct!(X) > leaderVal!(220, 1) Then

            leaderVal!(220, 1) = compPAPct!(X)
            leaderName$(220) = compKicker$(X)
            leaderTeam$(220) = statName$
            leaderVal!(220, 2) = compPAT!(X)

            3409 '
            For Z = 211 To 219:
                For Z1 = Z + 1 To 220:

                    If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                        Swap leaderName$(Z1), leaderName$(Z)
                        Swap leaderTeam$(Z1), leaderTeam$(Z)
                        For MN = 1 To 2
                            Swap leaderVal!(Z1, MN), leaderVal!(Z, MN)
                        Next MN
                    End If

                Next Z1
            Next Z

        End If

    Next X

    '---------------
    ' Sacks / INTs
    '---------------
    Print "   CHECKING TEAM SACKS AND INTERCEPTIONS"

    For X = 0 To 9

        If Left$(intName$(X), 1) <> " " And intName$(X) <> "" And intName$(X) <> "END" Then

            If statsGI(X, 0) > indRecords!(42) Then
                indRecords!(42) = statsGI(X, 0)
                indRecDesc$(42, 0) = intName$(X)
                indRecDesc$(42, 1) = teamName$
            End If

            If statsGI(X, 1) > indRecords!(43) Then
                indRecords!(43) = statsGI(X, 1)
                indRecDesc$(43, 0) = intName$(X)
                indRecDesc$(43, 1) = teamName$
            End If

            If statsGI(X, 2) > indRecords!(44) Then
                indRecords!(44) = statsGI(X, 2)
                indRecDesc$(44, 0) = intName$(X)
                indRecDesc$(44, 1) = teamName$
            End If

            If X > 0 Then
                If statsLInt!(X - 1) > indRecords!(45) Then
                    indRecords!(45) = statsLInt!(X - 1)
                    indRecDesc$(45, 0) = intName$(X)
                    indRecDesc$(45, 1) = teamName$
                    indRecDesc$(45, 2) = statsLInt$(X - 1, 0)
                End If
            End If

            For Z = 221 To 230
                If leaderName$(Z) = intName$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = statsGI(X, 0)
                    GoTo 3422
                End If
            Next Z

            If statsGI(X, 0) > leaderVal!(230, 1) Then

                leaderVal!(230, 1) = statsGI(X, 0)
                leaderName$(230) = intName$(X)
                leaderTeam$(230) = statName$

                3422 '
                For Z = 221 To 229:
                    For Z1 = Z + 1 To 230:

                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If

                    Next Z1

                Next Z

            End If

        End If

    Next X

    '-----

    For X = 0 To 14

        If Left$(sackName$(X), 1) <> " " And sackName$(X) <> "" And sackName$(X) <> "END" Then

            If statsGS(X, 0) > indRecords!(46) Then
                indRecords!(46) = statsGS(X, 0)
                indRecDesc$(46, 0) = sackName$(X)
                indRecDesc$(46, 1) = teamName$
            End If

            For Z = 231 To 240
                If leaderName$(Z) = sackName$(X) And leaderTeam$(Z) = statName$ Then
                    leaderVal!(Z, 1) = statsGS(X, 0)
                    GoTo 3434
                End If
            Next Z

            If statsGS(X, 0) > leaderVal!(240, 1) Then

                leaderVal!(240, 1) = statsGS(X, 0)
                leaderName$(240) = sackName$(X)
                leaderTeam$(240) = statName$

                3434 '
                For Z = 231 To 239:
                    For Z1 = Z + 1 To 240:
                        If leaderVal!(Z, 1) < leaderVal!(Z1, 1) Then
                            Swap leaderName$(Z1), leaderName$(Z)
                            Swap leaderTeam$(Z1), leaderTeam$(Z)
                            Swap leaderVal!(Z1, 1), leaderVal!(Z, 1)
                        End If
                    Next Z1

                Next Z

            End If

        End If

    Next X

End Sub


'----------------------------------------
'     CalcStatTotalsPro Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatTotalsPro (totalGames, teamName$, statName$, finalUpdate$)

    Shared fumbGain

    Shared totalTeamVal!(), defTotals!(), offTotals!(), teamRecords!()

    Shared teamRecDesc$(), totalTeamName$()

    Shared oppScore(), teamScore()

    Cls

    'TEAM CATEGORIES
    'CATEGORY 1 - TOTAL OFFENSE
    CA = 1

    If UCase$(finalUpdate$) = "Y" And (offTotals!(6) + offTotals!(2) + offTotals!(14)) / totalGames > teamRecords!(0) Then
        teamRecords!(0) = (offTotals!(6) + offTotals!(2) + offTotals!(14)) / totalGames
        teamRecDesc$(0) = teamName$
    End If

    If offTotals!(6) + offTotals!(2) + offTotals!(14) > teamRecords!(1) Then
        teamRecords!(1) = offTotals!(6) + offTotals!(2) + offTotals!(14)
        teamRecDesc$(1) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (offTotals!(6) + offTotals!(2) + offTotals!(14)) / (offTotals!(1) + offTotals!(3) - offTotals!(13)) > teamRecords!(2) Then
        teamRecords!(2) = (offTotals!(6) + offTotals!(2) + offTotals!(14)) / (offTotals!(1) + offTotals!(3) - offTotals!(13))
        teamRecDesc$(2) = teamName$
    End If

    If (offTotals!(6) + offTotals!(2) + offTotals!(14)) / totalGames > totalTeamVal!(CA, 20, 5) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = offTotals!(1) + offTotals!(3) - offTotals!(13)
        totalTeamVal!(CA, 20, 3) = offTotals!(6) + offTotals!(2) + offTotals!(14)
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 5) < totalTeamVal!(CA, Z1, 5) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 2 - TOTAL DEFENSE
    CA = 2

    If UCase$(finalUpdate$) = "Y" And (defTotals!(6) + defTotals!(2) + defTotals!(14)) / totalGames < teamRecords!(25) Then
        teamRecords!(25) = (defTotals!(6) + defTotals!(2) + defTotals!(14)) / totalGames
        teamRecDesc$(25) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(6) + defTotals!(2) + defTotals!(14) < teamRecords!(26) Then
        teamRecords!(26) = defTotals!(6) + defTotals!(2) + defTotals!(14)
        teamRecDesc$(26) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (defTotals!(6) + defTotals!(2) + defTotals!(14)) / (defTotals!(1) + defTotals!(3) - defTotals!(13)) < teamRecords!(27) Then
        teamRecords!(27) = (defTotals!(6) + defTotals!(2) + defTotals!(14)) / (defTotals!(1) + defTotals!(3) - defTotals!(13))
        teamRecDesc$(27) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (defTotals!(6) + defTotals!(2) + defTotals!(14)) / totalGames > teamRecords!(28) Then
        teamRecords!(28) = (defTotals!(6) + defTotals!(2) + defTotals!(14)) / totalGames
        teamRecDesc$(28) = teamName$
    End If

    If (defTotals!(6) + defTotals!(2) + defTotals!(14)) / totalGames < totalTeamVal!(CA, 20, 5) Then
        'Games
        totalTeamVal!(CA, 20, 1) = totalGames
        'Plays
        totalTeamVal!(CA, 20, 2) = defTotals!(1) + defTotals!(3) - defTotals!(13)
        'Yds
        totalTeamVal!(CA, 20, 3) = defTotals!(6) + defTotals!(2) + defTotals!(14)
        'Avg Yds / Gm
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        'Yds / Gm
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 5) > totalTeamVal!(CA, Z1, 5) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 3 - RUSHING OFFENSE
    CA = 3

    If UCase$(finalUpdate$) = "Y" And (offTotals!(2) + offTotals!(14)) / totalGames > teamRecords!(3) Then
        teamRecords!(3) = (offTotals!(2) + offTotals!(14)) / totalGames
        teamRecDesc$(3) = teamName$
    End If

    If offTotals!(2) + offTotals!(14) > teamRecords!(4) Then
        teamRecords!(4) = offTotals!(2) + offTotals!(14)
        teamRecDesc$(4) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (offTotals!(2) + offTotals!(14)) / (offTotals!(1) - offTotals!(13)) > teamRecords!(5) Then
        teamRecords!(5) = (offTotals!(2) + offTotals!(14)) / (offTotals!(1) - offTotals!(13))
        teamRecDesc$(5) = teamName$
    End If

    If (offTotals!(2) + offTotals!(14)) / totalGames > totalTeamVal!(CA, 20, 5) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = offTotals!(1) - offTotals!(13)
        totalTeamVal!(CA, 20, 3) = offTotals!(2) + offTotals!(14)
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 5) < totalTeamVal!(CA, Z1, 5) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 4 - RUSHING DEFENSE
    CA = 4

    If UCase$(finalUpdate$) = "Y" And (defTotals!(2) + defTotals!(14)) / totalGames < teamRecords!(29) Then
        teamRecords!(29) = (defTotals!(2) + defTotals!(14)) / totalGames
        teamRecDesc$(29) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(2) + defTotals!(14) < teamRecords!(30) Then
        teamRecords!(30) = defTotals!(2) + defTotals!(14)
        teamRecDesc$(30) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (defTotals!(2) + defTotals!(14)) / (defTotals!(1) - defTotals!(13)) < teamRecords!(31) Then
        teamRecords!(31) = (defTotals!(2) + defTotals!(14)) / (defTotals!(1) - defTotals!(13))
        teamRecDesc$(31) = teamName$
    End If

    If (defTotals!(2) + defTotals!(14)) / totalGames < totalTeamVal!(CA, 20, 5) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = defTotals!(1) - defTotals!(13)
        totalTeamVal!(CA, 20, 3) = defTotals!(2) + defTotals!(14)
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 5) > totalTeamVal!(CA, Z1, 5) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 5 - SCORING OFFENSE
    CA = 5
    PTS = 0
    OPTS = 0

    For I = 1 To totalGames
        PTS = PTS + teamScore(I)
        OPTS = OPTS + oppScore(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / totalGames > teamRecords!(13) Then
        teamRecords!(13) = PTS / totalGames
        teamRecDesc$(13) = teamName$
    End If

    If PTS > teamRecords!(12) Then
        teamRecords!(12) = PTS
        teamRecDesc$(12) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (PTS - OPTS) / totalGames > teamRecords!(14) Then
        teamRecords!(14) = (PTS - OPTS) / totalGames
        teamRecDesc$(14) = teamName$
    End If

    If PTS / totalGames > totalTeamVal!(CA, 20, 3) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = PTS
        totalTeamVal!(CA, 20, 3) = totalTeamVal!(CA, 20, 2) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 3) < totalTeamVal!(CA, Z1, 3) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 6 - SCORING DEFENSE
    CA = 6
    PTS = 0

    For I = 1 To totalGames
        PTS = PTS + oppScore(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / totalGames < teamRecords!(39) Then
        teamRecords!(39) = PTS / totalGames
        teamRecDesc$(39) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And PTS < teamRecords!(38) Then
        teamRecords!(38) = PTS
        teamRecDesc$(38) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And PTS / totalGames > teamRecords!(41) Then
        teamRecords!(41) = PTS / totalGames
        teamRecDesc$(41) = teamName$
    End If

    If PTS > teamRecords!(40) Then
        teamRecords!(40) = PTS
        teamRecDesc$(40) = teamName$
    End If

    If PTS / totalGames < totalTeamVal!(CA, 20, 3) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = PTS
        totalTeamVal!(CA, 20, 3) = totalTeamVal!(CA, 20, 2) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 3) > totalTeamVal!(CA, Z1, 3) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 7 - PASSING OFFENSE
    CA = 7

    If UCase$(finalUpdate$) = "Y" And offTotals!(6) / totalGames > teamRecords!(6) Then
        teamRecords!(6) = offTotals!(6) / totalGames
        teamRecDesc$(6) = teamName$
    End If

    If offTotals!(6) > teamRecords!(7) Then
        teamRecords!(7) = offTotals!(6)
        teamRecDesc$(7) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(6) / offTotals!(3) > teamRecords!(8) Then
        teamRecords!(8) = offTotals!(6) / offTotals!(3)
        teamRecDesc$(8) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(6) / offTotals!(4) > teamRecords!(9) Then
        teamRecords!(9) = offTotals!(6) / offTotals!(4)
        teamRecDesc$(9) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(4) / offTotals!(3) > teamRecords!(10) Then
        teamRecords!(10) = offTotals!(4) / offTotals!(3)
        teamRecDesc$(10) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(5) / offTotals!(3) < teamRecords!(11) Then
        teamRecords!(11) = offTotals!(5) / offTotals!(3)
        teamRecDesc$(11) = teamName$
    End If

    If offTotals!(6) / totalGames > totalTeamVal!(CA, 20, 8) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = offTotals!(3)
        totalTeamVal!(CA, 20, 3) = offTotals!(4)
        totalTeamVal!(CA, 20, 4) = offTotals!(5)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 6) = offTotals!(6)
        totalTeamVal!(CA, 20, 7) = offTotals!(6) / offTotals!(3)
        totalTeamVal!(CA, 20, 8) = totalTeamVal!(CA, 20, 6) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 8) < totalTeamVal!(CA, Z1, 8) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 8 - PASSING DEFENSE
    CA = 8

    If UCase$(finalUpdate$) = "Y" And defTotals!(6) / totalGames < teamRecords!(32) Then
        teamRecords!(32) = defTotals!(6) / totalGames
        teamRecDesc$(32) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(6) < teamRecords!(33) Then
        teamRecords!(33) = defTotals!(6)
        teamRecDesc$(33) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(6) / defTotals!(3) < teamRecords!(34) Then
        teamRecords!(34) = defTotals!(6) / defTotals!(3)
        teamRecDesc$(34) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(6) / defTotals!(4) < teamRecords!(35) Then
        teamRecords!(35) = defTotals!(6) / defTotals!(4)
        teamRecDesc$(35) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(4) / defTotals!(3) < teamRecords!(36) Then
        teamRecords!(36) = defTotals!(4) / defTotals!(3)
        teamRecDesc$(36) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(5) / defTotals!(3) > teamRecords!(37) Then
        teamRecords!(37) = defTotals!(5) / defTotals!(3)
        teamRecDesc$(37) = teamName$
    End If

    If defTotals!(6) / totalGames < totalTeamVal!(CA, 20, 8) Then

        totalTeamVal!(CA, 20, 1) = totalGames
        totalTeamVal!(CA, 20, 2) = defTotals!(3)
        totalTeamVal!(CA, 20, 3) = defTotals!(4)
        totalTeamVal!(CA, 20, 4) = defTotals!(5)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 6) = defTotals!(6)
        totalTeamVal!(CA, 20, 7) = defTotals!(6) / defTotals!(3)
        totalTeamVal!(CA, 20, 8) = totalTeamVal!(CA, 20, 6) / totalGames
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 8) > totalTeamVal!(CA, Z1, 8) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 9 - PUNTING
    CA = 9

    If UCase$(finalUpdate$) = "Y" And offTotals!(8) / offTotals!(7) > teamRecords!(15) Then teamRecords!(15) = offTotals!(8) / offTotals!(7): teamRecDesc$(15) = teamName$

    If offTotals!(8) / offTotals!(7) > totalTeamVal!(CA, 20, 2) Then

        totalTeamVal!(CA, 20, 1) = offTotals!(7)
        totalTeamVal!(CA, 20, 2) = offTotals!(8) / offTotals!(7)
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 2) < totalTeamVal!(CA, Z1, 2) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 10 - TURNOVER MARGIN
    CA = 10

    fumbGain = offTotals!(9) - offTotals!(10) + defTotals!(10)
    fumbLost = defTotals!(9) + offTotals!(10) - defTotals!(10)
    intGain = defTotals!(5)
    intLost = offTotals!(5)
    toMargin! = (fumbGain + intGain - fumbLost - intLost) / totalGames
    'toMargin! = (fumbLost + intLost - fumbGain - intGain) / totalGames

    If toMargin! > totalTeamVal!(CA, 20, 7) Then

        totalTeamVal!(CA, 20, 1) = fumbGain
        totalTeamVal!(CA, 20, 2) = intGain
        totalTeamVal!(CA, 20, 3) = fumbGain + intGain
        totalTeamVal!(CA, 20, 4) = fumbLost
        totalTeamVal!(CA, 20, 5) = intLost
        totalTeamVal!(CA, 20, 6) = fumbLost + intLost
        totalTeamVal!(CA, 20, 7) = toMargin!
        totalTeamName$(CA, 20) = statName$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If totalTeamVal!(CA, Z, 7) < totalTeamVal!(CA, Z1, 7) Then
                    Swap totalTeamName$(CA, Z1), totalTeamName$(CA, Z)
                    For MN = 1 To 9
                        Swap totalTeamVal!(CA, Z1, MN), totalTeamVal!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'PUNT RETURNS
    If UCase$(finalUpdate$) = "Y" And offTotals!(25) / offTotals!(24) > teamRecords!(16) Then
        teamRecords!(16) = offTotals!(25) / offTotals!(24)
        teamRecDesc$(16) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(25) / defTotals!(24) < teamRecords!(42) Then
        teamRecords!(42) = defTotals!(25) / defTotals!(24)
        teamRecDesc$(42) = teamName$
    End If

    'KO Return
    If UCase$(finalUpdate$) = "Y" And offTotals!(23) / offTotals!(22) > teamRecords!(17) Then
        teamRecords!(17) = offTotals!(23) / offTotals!(22)
        teamRecDesc$(17) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And defTotals!(23) / defTotals!(22) < teamRecords!(43) Then
        teamRecords!(43) = defTotals!(23) / defTotals!(22)
        teamRecDesc$(43) = teamName$
    End If

    'FIRST DOWNS
    If offTotals!(0) > teamRecords!(18) Then
        teamRecords!(18) = offTotals!(0)
        teamRecDesc$(18) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(0) / totalGames > teamRecords!(19) Then
        teamRecords!(19) = offTotals!(0) / totalGames
        teamRecDesc$(19) = teamName$
    End If

    'FUMBLES
    If offTotals!(9) > teamRecords!(20) Then
        teamRecords!(20) = offTotals!(9)
        teamRecDesc$(20) = teamName$
    End If

    If offTotals!(10) > teamRecords!(21) Then
        teamRecords!(21) = offTotals!(10)
        teamRecDesc$(21) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(10) < teamRecords!(22) Then
        teamRecords!(22) = offTotals!(10)
        teamRecDesc$(22) = teamName$
    End If

    'PENALTIES
    If UCase$(finalUpdate$) = "Y" And offTotals!(11) / totalGames > teamRecords!(23) Then
        teamRecords!(23) = offTotals!(11) / totalGames
        teamRecDesc$(23) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(12) / totalGames > teamRecords!(24) Then
        teamRecords!(24) = offTotals!(12) / totalGames
        teamRecDesc$(24) = teamName$
    End If

End Sub


'----------------------------------------
'     SortStandingsLeague Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target league. Currently setup using a
'bubble sort algorithm.
Sub SortStandingsLeague

    Shared divWins(), divLosses(), divTies()

    Shared leagPtsAgnst!(), leagPtsFor!()

    Shared memberName$()

    'Sort leaders in league
    For Z = 0 To 3

        AA = (Z * 10) + 1
        ZZ = (Z * 10) + 10

        For X = AA To ZZ - 1:

            For Y = X + 1 To ZZ:

                diffX = divWins(X) - divLosses(X)
                diffY = divWins(Y) - divLosses(Y)

                If diffX <= diffY Then

                    If diffX <> diffY Then
                        If Not (divWins(X) <= divWins(Y) And divWins(X) > divLosses(X)) Or Not (divWins(X) >= divWins(Y) And divWins(X) < divLosses(X)) Then
                            Swap memberName$(X), memberName$(Y)
                            Swap divWins(X), divWins(Y)
                            Swap divLosses(X), divLosses(Y)
                            Swap divTies(X), divTies(Y)
                            Swap leagPtsFor!(X), leagPtsFor!(Y)
                            Swap leagPtsAgnst!(X), leagPtsAgnst!(Y)
                        End If
                    End If

                End If

            Next Y

        Next X

    Next Z

End Sub


'----------------------------------------
'      PostLeadersLeague Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the league, and
'writes the updated stats to the file
Sub PostLeadersLeague (leaderFile$)

    Shared memberName$(), leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Shared totalTeamVal!(), leaderVal!()
    Shared divWins(), divLosses(), divTies()
    Shared leagPtsFor!(), leagPtsAgnst!()

    Cls

    Open diskPaths$(2) + leaderFile$ + ".LDR" For Output As #1

    Print "POSTING "; leaderFile$; " LEAGUE LEADERS"

    For X = 1 To TEAMS_PER_LEAGUE:
        Write #1, memberName$(X), divWins(X), divLosses(X), divTies(X), leagPtsFor!(X), leagPtsAgnst!(X)
    Next

    For X = 1 To 240
        Write #1, leaderName$(X), leaderTeam$(X), leaderVal!(X, 1), leaderVal!(X, 2)
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Write #1, totalTeamVal!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Write #1, totalTeamName$(I, I1)
        Next
    Next

    Close #1

End Sub


'----------------------------------------
'        PostRecordsPro Subroutine
'----------------------------------------
'This subroutine writes the calculated
'records to the records file
Sub PostRecordsPro

    Shared indRecDesc$(), teamRecDesc$()

    Shared indRecords!(), teamRecords!()

    Open diskPaths$(1) + "RECORDS.PFB" For Output As #2

    For I = 0 To 50:
        Print #2, indRecords!(I)
        Print #2, indRecDesc$(I, 0)
        Print #2, indRecDesc$(I, 1)
        Print #2, indRecDesc$(I, 2)
    Next

    For I = 0 To 50:
        Print #2, teamRecords!(I)
        Print #2, teamRecDesc$(I)
    Next

    Close #2

End Sub
