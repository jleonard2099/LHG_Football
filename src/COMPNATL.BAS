'----------------------------------------
'        CompileNatlOrg SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'organization, and then relies on subroutines
'to read stats and compare to any existing
'leaders and records
Sub CompileNatlOrg ()

    _Title "3-in-1 Football - Update Org Stats"

    Shared oppScore()

    Shared indRecords!(), totalTeamVal!(), leaderVal!()
    Shared natLdrVal!(), natLdrTotVal!()
    Shared divWins(), divLosses(), divTies(), offTotals!()
    Shared pollRatings!(), leagPtsAgnst!(), leagPtsFor!(), teamRecords!()

    Shared statFilesComp$(), leaderName$(), leaderTeam$()
    Shared indRecDesc$()
    Shared memberConf$()
    Shared natLdrName$(), natLdrTeam$(), natLdrTotTeam$()
    Shared pollTeam$(), teamRecDesc$(), totalTeamName$()
    Shared memberName$()

    Dim NRT

    'Read in any existing records
    If _FileExists(diskPaths$(1) + "RECORDS.CFB") Then

        Call ReadSeasonRecords ("RECORDS.CFB")

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

    Erase natLdrTotVal!
    Erase natLdrName$, natLdrTeam$, natLdrTotTeam$

    For X = 1 To 240
        natLdrName$(X) = "*"
        natLdrTeam$(X) = "*"
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            natLdrTotTeam$(I, I1) = "*"
        Next
    Next

    For I = 1 To 20
        natLdrTotVal!(2, I, 5) = 9999
        natLdrTotVal!(4, I, 5) = 9999
        natLdrTotVal!(6, I, 3) = 9999
        natLdrTotVal!(8, I, 8) = 9999
        natLdrTotVal!(10, I, 7) = -99
    Next

    C$ = ","

    organizationFile$ = _OpenFileDialog$("Choose Organization file", diskPaths$(2), "*.CON", "Organization files [.CON]")
    'ChooseFile$(diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If organizationFile$ <> "" Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Call ReadOrganizationFile(organizationFile$, orgName$)

        For currConf = 1 To MAX_CONFERENCES

            'Look for * as terminating char
            If memberConf$(currConf) <> "XXX" Then

                Cls

                Erase totalTeamVal!, leaderVal!
                Erase divWins, divLosses, divTies
                Erase leagPtsFor!, leagPtsAgnst!

                For X = 1 To 60
                    statFilesComp$(X) = "*"
                Next

                For X = 1 To 240
                    leaderName$(X) = "*"
                    leaderTeam$(X) = "*"
                Next X

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

                targetConf$ = diskPaths$(2) + memberConf$(currConf) + ".TMS"

                If _FileExists(targetConf$) Then

                    Open targetConf$ For Input As #4
                    Input #4, statType
                    Close #4

                    If statType <> 0 Then

                        MsgText$ = "ERROR!! THIS IS NOT A COLLEGE CONFERENCE. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                        Call CSMsgBox(MsgText$, BoxWidth, Pass)
                        I$ = GetKeyPress$
                        Call CSMsgBox("", 0, Pass)
                        Exit Sub

                    Else

                        Open targetConf$ For Input As #3

                        Input #3, confName$
                        Input #3, div1$
                        Input #3, div2$

                        For XW = 1 To 30
                            Input #3, memberName$(XW)
                            statFilesComp$(X) = memberName$(XW)
                        Next

                        Close #3

                        For XW = 1 To 30

                            'Look for * as terminating char
                            If memberName$(XW) <> Chr$(42) Then

                                haveStats = FindStatsNat%(XW, memberName$(XW), totalGames, NRT)

                                If (haveStats) Then

                                    statName$ = memberName$(XW)
                                    sameTeam = 0

                                    For X = 1 To 30

                                        If statFilesComp$(X) = statName$ Then

                                            If divWins(X) = fullWins And divLosses(X) = fullLosses And divTies(X) = fullTies Then

                                                sameTeam = 1
                                                Exit For

                                            Else

                                                divWins(X) = fullWins
                                                divLosses(X) = fullLosses
                                                divTies(X) = fullTies

                                                divWins(X + 30) = confWins
                                                divLosses(X + 30) = confLosses
                                                divTies(X + 30) = confTies

                                                leagPtsFor!(X + 30) = confPtsFor
                                                leagPtsAgnst!(X + 30) = confPtsAgnst
                                                statFilesComp$(X + 30) = statFilesComp$(X)

                                            End If

                                        End If

                                    Next X
                                    'alternatively, use a Do / Until loop; sameTeam = 1 or EC = 30

                                    If sameTeam = 0 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                                        Call ReadStatsLeaders(memberName$(XW), totalGames, TM$, 0)
                                        Call CalcStatTotalsConf(totalGames, TM$, statName$, finalUpdate$)
                                        Call CalcStatLeaders(totalGames, TM$, statName$, finalUpdate$)

                                    End If

                                End If 'Done looking for stats

                            End If 'Done checking for valid stat file

                        Next XW

                        Call SortStandingsLeague
                        Call PostLeadersConf(memberConf$(currConf))

                        Cls
                        Print "CHECKING "; memberConf$(currConf); " STATS AGAINST NATIONAL LEADERS"
                        
                        Call CalcStatLeadersNat (totalGames)

                    End If 'Done checking for valid conference

                End If 'Done checking if file exists

            End If

        Next currConf

        'All conferences are done; post org leaders
        Call PostLeadersOrg(GetFileBaseName$(organizationFile$), NRT)
        Call PostRecordsColl

    End If 'Done checking ESC

End Sub


'----------------------------------------
'         FindStatsNat Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the organization and loads them if
'found, notifying the user
Function FindStatsNat% (XW, statFile$, totalGames, NRT)

    Shared powerRating(), oppScore(), teamScore()

    Shared pollRatings!(), leagPtsAgnst!(), leagPtsFor!()

    Shared locIndicator$(), pollTeam$()
    Shared oppName$(), memberName$()

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsNat% = 0

    Else

        Call ReadScheduleStats(statFile$, XW, totalGames)

        confWins = 0
        confLosses = 0
        confTies = 0

        fullWins = 0
        fullLosses = 0
        fullTies = 0

        confPtsAgnst = 0
        confPtsFor = 0

        'RATINGS
        For I = 1 To totalGames

            statName$ = nameStatFile$(oppName$(I))
            confGame = 0

            For EC = 1 To 30
                'If team stat file = opponent of a conference game?
                If statName$ = memberName$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame = 1 Then
                
                If teamScore(I) > oppScore(I) Then 
                    fullWins = fullWins + 1
                    confWins = confWins + 1
                End If

                If teamScore(I) < oppScore(I) Then 
                    fullLosses = fullLosses + 1
                    confLosses = confLosses + 1
                End If

                If teamScore(I) = oppScore(I) Then 
                    fullTies = fullTies + 1
                    confTies = confTies + 1
                End If

                confPtsFor = confPtsFor + teamScore(I)
                confPtsAgnst = confPtsAgnst + oppScore(I)

            Else

                If teamScore(I) > oppScore(I) Then 
                    fullWins = fullWins + 1
                End If
                
                If teamScore(I) < oppScore(I) Then 
                    fullLosses = fullLosses + 1
                End If
                
                If teamScore(I) = oppScore(I) Then 
                    fullTies = fullTies + 1
                End If
            
            End If

            EC = 0

        Next I

        'STRENGTH OF SCHEDULE
        teamPower = 0
        SS = 0

        For I = 1 To totalGames
            SS = powerRating(1, I) + SS
        Next

        SS = SS / totalGames

        'INDIVIDUAL GAME POWER RATINGS
        GL = 0
        teamPower = 0

        For I = 1 To totalGames
            RAT = teamScore(I) - oppScore(I) + powerRating(1, I)
            If Left$(locIndicator$(I), 1) = "H" Then 
                RAT = RAT - 3
            End If
            If Left$(locIndicator$(I), 1) = "A" Then 
                RAT = RAT + 3
            End If
            teamPower = teamPower + RAT
        Next

        If totalGames <= 4 Then
            GL = 5 - totalGames
            teamPower = teamPower + GL * powerRating(0, 1)
        End If

        teamPower = teamPower / (totalGames + GL)
        NRT = NRT + 1
        pollTeam$(NRT) = statFile$
        pollRatings!(NRT, 1) = teamPower
        pollRatings!(NRT, 2) = fullWins
        pollRatings!(NRT, 3) = fullLosses
        pollRatings!(NRT, 4) = fullTies
        pollRatings!(NRT, 5) = SS

        Cls

        Locate 4, 5

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsNat% = 1

    End If

End Function


'----------------------------------------
'       CalcStatLeadersNat Subroutine
'----------------------------------------
'This subroutine both posts the leaders 
'to the National data arrays, and also
'sorts the players in those leaders
Sub CalcStatLeadersNat (totalGames)

    Shared totalTeamVal!(), leaderVal!()
    Shared natLdrVal!(), natLdrTotVal!()

    Shared leaderName$(), leaderTeam$()
    Shared natLdrName$(), natLdrTeam$(), natLdrTotTeam$(), totalTeamName$()

    minRushAtt = totalGames * 10
    minRecept = totalGames * 3
    minComp = totalGames * 15
    minReturns = totalGames * 1.2

    ' RUSHING AVERAGE
    For X = 1 To 20

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(20, 1) And leaderVal!(X, 1) >= minRushAtt Then

                natLdrVal!(20, 1) = leaderVal!(X, 1)
                natLdrVal!(20, 2) = leaderVal!(X, 2)
                natLdrName$(20) = leaderName$(X)
                natLdrTeam$(20) = leaderTeam$(X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RECEIVING AVERAGE
    For X = 21 To 40

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(40, 1) And leaderVal!(X, 1) >= minRecept Then

                natLdrVal!(40, 1) = leaderVal!(X, 1)
                natLdrVal!(40, 2) = leaderVal!(X, 2)
                natLdrName$(40) = leaderName$(X)
                natLdrTeam$(40) = leaderTeam$(X)

                For Z = 21 To 39
                    For Z1 = Z + 1 To 40
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RUSHING YARDAGE
    For X = 41 To 50

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(50, 1) Then

                natLdrVal!(50, 1) = leaderVal!(X, 1)
                natLdrVal!(50, 2) = leaderVal!(X, 2)
                natLdrName$(50) = leaderName$(X)
                natLdrTeam$(50) = leaderTeam$(X)

                For Z = 41 To 49
                    For Z1 = Z + 1 To 50
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RECEIVING YARDAGE
    For X = 51 To 60

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(60, 1) Then

                natLdrVal!(60, 1) = leaderVal!(X, 1)
                natLdrVal!(60, 2) = leaderVal!(X, 2)
                natLdrName$(60) = leaderName$(X)
                natLdrTeam$(60) = leaderTeam$(X)

                For Z = 51 To 59
                    For Z1 = Z + 1 To 60
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If
        End If

    Next X

    ' RUSHING TD'S
    For X = 61 To 70

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(70, 1) Then

                natLdrVal!(70, 1) = leaderVal!(X, 1)
                natLdrVal!(70, 2) = leaderVal!(X, 2)
                natLdrName$(70) = leaderName$(X)
                natLdrTeam$(70) = leaderTeam$(X)

                For Z = 61 To 69
                    For Z1 = Z + 1 To 70
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' RECEIVING TD'S
    For X = 71 To 80

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(80, 1) Then

                natLdrVal!(80, 1) = leaderVal!(X, 1)
                natLdrVal!(80, 2) = leaderVal!(X, 2)
                natLdrName$(80) = leaderName$(X)
                natLdrTeam$(80) = leaderTeam$(X)

                For Z = 71 To 79
                    For Z1 = Z + 1 To 80
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' RUSHING ATT
    For X = 81 To 90

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(90, 1) Then

                natLdrVal!(90, 1) = leaderVal!(X, 1)
                natLdrVal!(90, 2) = leaderVal!(X, 2)
                natLdrName$(90) = leaderName$(X)
                natLdrTeam$(90) = leaderTeam$(X)

                For Z = 81 To 89
                    For Z1 = Z + 1 To 90
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' TOTAL RECEPTIONS
    For X = 91 To 100

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(100, 1) Then

                natLdrVal!(100, 1) = leaderVal!(X, 1)
                natLdrVal!(100, 2) = leaderVal!(X, 2)
                natLdrName$(100) = leaderName$(X)
                natLdrTeam$(100) = leaderTeam$(X)

                For Z = 91 To 99
                    For Z1 = Z + 1 To 100
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' TOTAL OFFENSE
    For X = 101 To 110

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(110, 1) Then

                natLdrVal!(110, 1) = leaderVal!(X, 1)
                natLdrVal!(110, 2) = leaderVal!(X, 2)
                natLdrName$(110) = leaderName$(X)
                natLdrTeam$(110) = leaderTeam$(X)

                For Z = 101 To 109
                    For Z1 = Z + 1 To 110
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If
    Next X

    ' SCORING NON-KICKERS
    For X = 111 To 120

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(120, 1) Then

                natLdrVal!(120, 1) = leaderVal!(X, 1)
                natLdrVal!(120, 2) = leaderVal!(X, 2)
                natLdrName$(120) = leaderName$(X)
                natLdrTeam$(120) = leaderTeam$(X)
                For Z = 111 To 119
                    For Z1 = Z + 1 To 120
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' COMPLETION PERCENTAGE
    For X = 121 To 130

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(130, 1) And leaderVal!(X, 1) >= minComp Then

                natLdrVal!(130, 1) = leaderVal!(X, 1)
                natLdrVal!(130, 2) = leaderVal!(X, 2)
                natLdrName$(130) = leaderName$(X)
                natLdrTeam$(130) = leaderTeam$(X)

                For Z = 121 To 129
                    For Z1 = Z + 1 To 130
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' QB RATING
    For X = 131 To 140

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(140, 1) And leaderVal!(X, 1) >= minComp Then

                natLdrVal!(140, 1) = leaderVal!(X, 1)
                natLdrVal!(140, 2) = leaderVal!(X, 2)
                natLdrName$(140) = leaderName$(X)
                natLdrTeam$(140) = leaderTeam$(X)

                For Z = 131 To 139
                    For Z1 = Z + 1 To 140
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PASSING YARDAGE
    For X = 141 To 150

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(150, 1) Then

                natLdrVal!(150, 1) = leaderVal!(X, 1)
                natLdrVal!(150, 2) = leaderVal!(X, 2)
                natLdrName$(150) = leaderName$(X)
                natLdrTeam$(150) = leaderTeam$(X)

                For Z = 141 To 149
                    For Z1 = Z + 1 To 150
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PASSING TD'S
    For X = 151 To 160

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(160, 1) Then

                natLdrVal!(160, 1) = leaderVal!(X, 1)
                natLdrVal!(160, 2) = leaderVal!(X, 2)
                natLdrName$(160) = leaderName$(X)
                natLdrTeam$(160) = leaderTeam$(X)

                For Z = 151 To 159
                    For Z1 = Z + 1 To 160
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' KO RETURNS
    For X = 161 To 170

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(170, 1) And leaderVal!(X, 1) >= minReturns Then

                natLdrVal!(170, 1) = leaderVal!(X, 1)
                natLdrVal!(170, 2) = leaderVal!(X, 2)
                natLdrName$(170) = leaderName$(X)
                natLdrTeam$(170) = leaderTeam$(X)

                For Z = 161 To 169
                    For Z1 = Z + 1 To 170
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PR RETURNS
    For X = 171 To 180

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(180, 1) And leaderVal!(X, 1) >= minReturns Then

                natLdrVal!(180, 1) = leaderVal!(X, 1)
                natLdrVal!(180, 2) = leaderVal!(X, 2)
                natLdrName$(180) = leaderName$(X)
                natLdrTeam$(180) = leaderTeam$(X)

                For Z = 171 To 179
                    For Z1 = Z + 1 To 180
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PUNTING
    For X = 181 To 190

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(190, 1) Then

                natLdrVal!(190, 1) = leaderVal!(X, 1)
                natLdrVal!(190, 2) = leaderVal!(X, 2)
                natLdrName$(190) = leaderName$(X)
                natLdrTeam$(190) = leaderTeam$(X)

                For Z = 181 To 189
                    For Z1 = Z + 1 To 190
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' SCORING (KICKERS)
    For X = 191 To 200

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(200, 1) Then

                natLdrVal!(200, 1) = leaderVal!(X, 1)
                natLdrVal!(200, 2) = leaderVal!(X, 2)
                natLdrName$(200) = leaderName$(X)
                natLdrTeam$(200) = leaderTeam$(X)

                For Z = 191 To 199
                    For Z1 = Z + 1 To 200
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' FG%
    For X = 201 To 210

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(210, 1) Then

                natLdrVal!(210, 1) = leaderVal!(X, 1)
                natLdrVal!(210, 2) = leaderVal!(X, 2)
                natLdrName$(210) = leaderName$(X)
                natLdrTeam$(210) = leaderTeam$(X)

                For Z = 201 To 209
                    For Z1 = Z + 1 To 210
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' EP%
    For X = 211 To 220

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(220, 1) Then

                natLdrVal!(220, 1) = leaderVal!(X, 1)
                natLdrVal!(220, 2) = leaderVal!(X, 2)
                natLdrName$(220) = leaderName$(X)
                natLdrTeam$(220) = leaderTeam$(X)

                For Z = 211 To 219
                    For Z1 = Z + 1 To 220
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' INT'S
    For X = 221 To 230

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(230, 1) Then

                natLdrVal!(230, 1) = leaderVal!(X, 1)
                natLdrVal!(230, 2) = leaderVal!(X, 2)
                natLdrName$(230) = leaderName$(X)
                natLdrTeam$(230) = leaderTeam$(X)

                For Z = 221 To 229
                    For Z1 = Z + 1 To 230
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' SACKS
    For X = 231 To 240

        If leaderName$(X) <> "*" And leaderName$(X) <> "" Then

            If leaderVal!(X, 1) >= natLdrVal!(240, 1) Then

                natLdrVal!(240, 1) = leaderVal!(X, 1)
                natLdrVal!(240, 2) = leaderVal!(X, 2)
                natLdrName$(240) = leaderName$(X)
                natLdrTeam$(240) = leaderTeam$(X)

                For Z = 231 To 239
                    For Z1 = Z + 1 To 240
                        If natLdrVal!(Z, 1) < natLdrVal!(Z1, 1) Then
                            Swap natLdrName$(Z1), natLdrName$(Z)
                            Swap natLdrTeam$(Z1), natLdrTeam$(Z)
                            For MN = 1 To 2
                                Swap natLdrVal!(Z1, MN), natLdrVal!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' TEAM OFFENSE
    CA = 1

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 5) >= natLdrTotVal!(CA, 20, 5) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 5) < natLdrTotVal!(CA, Z1, 5) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' TEAM DEFENSE
    CA = 2

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 5) <= natLdrTotVal!(CA, 20, 5) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 5) > natLdrTotVal!(CA, Z1, 5) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RUSHING OFFENSE
    CA = 3

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 5) >= natLdrTotVal!(CA, 20, 5) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 5) < natLdrTotVal!(CA, Z1, 5) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RUSHING DEFENSE
    CA = 4

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 5) <= natLdrTotVal!(CA, 20, 5) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 5) > natLdrTotVal!(CA, Z1, 5) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' SCORING OFFENSE
    CA = 5

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 3) >= natLdrTotVal!(CA, 20, 3) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 3) < natLdrTotVal!(CA, Z1, 3) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' SCORING DEFENSE
    CA = 6

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 3) <= natLdrTotVal!(CA, 20, 3) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 3) > natLdrTotVal!(CA, Z1, 3) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' PASSING OFFENSE
    CA = 7

    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then

            If totalTeamVal!(CA, X, 8) >= natLdrTotVal!(CA, 20, 8) Then

                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next

                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 8) < natLdrTotVal!(CA, Z1, 8) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' PASSING DEFENSE
    CA = 8
    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then
            If totalTeamVal!(CA, X, 8) <= natLdrTotVal!(CA, 20, 8) Then
                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next
                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)
                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 8) > natLdrTotVal!(CA, Z1, 8) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PUNTING
    CA = 9
    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then
            If totalTeamVal!(CA, X, 2) >= natLdrTotVal!(CA, 20, 2) Then
                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next
                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)
                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 2) < natLdrTotVal!(CA, Z1, 2) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' TURNOVER MARGIN
    CA = 10
    For X = 1 To 20

        If totalTeamName$(CA, X) <> "*" And totalTeamName$(CA, X) <> "" Then
            If totalTeamVal!(CA, X, 7) >= natLdrTotVal!(CA, 20, 7) Then
                For I = 1 To 8
                    natLdrTotVal!(CA, 20, I) = totalTeamVal!(CA, X, I)
                Next
                natLdrTotTeam$(CA, 20) = totalTeamName$(CA, X)
                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If natLdrTotVal!(CA, Z, 7) < natLdrTotVal!(CA, Z1, 7) Then
                            Swap natLdrTotTeam$(CA, Z1), natLdrTotTeam$(CA, Z)
                            For MN = 1 To 9
                                Swap natLdrTotVal!(CA, Z1, MN), natLdrTotVal!(CA, Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

End Sub


'----------------------------------------
'        PostLeadersOrg Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the organization,
'and writes the updated stats to the file
Sub PostLeadersOrg (organization$, NRT)

    Shared statFilesComp$(), natLdrName$(), natLdrTeam$(), natLdrTotTeam$()
    Shared pollTeam$()

    Shared natLdrVal!(), natLdrTotVal!()
    Shared divWins(), divLosses(), divTies()
    Shared leagPtsAgnst!(), leagPtsFor!(), pollRatings!()

    Cls

    Open diskPaths$(2) + organization$ + ".NLD" For Output As #1

    Print "    POSTING NEW "; organization$; " LEADERS"

    For X = 1 To 60:
        Write #1, statFilesComp$(X), divWins(X), divLosses(X), divTies(X), leagPtsFor!(X), leagPtsAgnst!(X)
    Next

    For X = 1 To 240
        Write #1, natLdrName$(X), natLdrTeam$(X), natLdrVal!(X, 1), natLdrVal!(X, 2)
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Write #1, natLdrTotVal!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Write #1, natLdrTotTeam$(I, I1)
        Next
    Next

    Close #1

    For Z = 1 To NRT - 1
        For Z1 = Z + 1 To NRT
            If pollRatings!(Z, 1) < pollRatings!(Z1, 1) Then
                Swap pollTeam$(Z1), pollTeam$(Z)
                For MN = 1 To 5
                    Swap pollRatings!(Z1, MN), pollRatings!(Z, MN)
                Next MN
            End If
        Next Z1
    Next Z

    Open diskPaths$(2) + organization$ + ".POL" For Output As #6

    Print "POSTING "; organization$; " POLL"

    Write #6, NRT

    For X = 1 To 1200
        Write #6, pollTeam$(X), pollRatings!(X, 1), pollRatings!(X, 2), pollRatings!(X, 3), pollRatings!(X, 4), pollRatings!(X, 5)
    Next

    Close #6

End Sub
