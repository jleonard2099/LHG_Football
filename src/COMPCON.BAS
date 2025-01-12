    '----------------------------------------
'        CompileCollege SubRoutine
'----------------------------------------
'This subroutine reads in existing records,
'looks for stats matching each team in the
'conference, and then relies on subroutines
'to read stats and compare to any existing
'leaders and records
Sub CompileCollege ()

    _Title "3-in-1 Football - Update Conference Stats"

    Shared totalTeamVal!(), leaderVal!(), statsGeneral!()
    Shared divWins!(), divLosses!(), divTies!()
    Shared leagPtsAgnst!(), leagPtsFor!()

    Shared indRecords!(), teamRecords!()

    Shared indRecDesc$()
    Shared leaderName$(), leaderTeam$()
    Shared memberName$(), teamRecDesc$(), totalTeamName$()

    'Read in any existing records first
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

    Erase totalTeamVal!, leaderVal!
    Erase divWins!, divLosses!, divTies!

    For X = 1 To 60
        memberName$(X) = "*"
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

    C$ = ","

    conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.TMI", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + conferenceFile$ + ".TMI" For Input As #3
        Input #3, statType
        Close #3

        If statType <> 0 Then

            MsgText$ = "ERROR!! THIS IS NOT A COLLEGE CONFERENCE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call ReadConferenceStats (conferenceFile$, 0)

            Erase leagPtsAgnst!, leagPtsFor!

            For XW = 1 To TEAMS_PER_CONFERENCE

                'Look for * as terminating char
                If memberName$(XW) <> Chr$(42) Then

                    haveStats = FindStatsColl%(XW, memberName$(XW), numberGames)

                    If (haveStats) Then

                        L = Len(memberName$(XW))
                        WT$ = ""

                        For X = 1 To L:
                            A = Asc(Mid$(memberName$(XW), X, 1))
                            WT$ = WT$ + Chr$(A)
                        Next X

                        nonConfGame = 0

                        For X = 1 To TEAMS_PER_CONFERENCE

                            If memberName$(X) = WT$ Then

                                If divWins!(X) = fullWins And divLosses!(X) = fullLosses And divTies!(X) = fullTies Then

                                    nonConfGame = 1
                                    Exit For

                                Else

                                    divWins!(X) = fullWins
                                    divLosses!(X) = fullLosses
                                    divTies!(X) = fullTies

                                    divWins!(X + 30) = confWins
                                    divLosses!(X + 30) = confLosses
                                    divTies!(X + 30) = confTies

                                    leagPtsFor!(X + 30) = confPtsFor
                                    leagPtsAgnst!(X + 30) = confPtsAgnst
                                    memberName$(X + 30) = memberName$(X)

                                End If

                            End If

                        Next X
                        'alternatively, use a Do / Until loop; nonConfGame = 1 or EC = 30

                        If nonConfGame <> 1 And _FileExists(diskPaths$(1) + memberName$(XW)) Then

                            Call ReadStatsLeaders(memberName$(XW), numberGames, TM$, 0)
                            Call CalcStatTotalsConf(numberGames, TM$, WT$, finalUpdate$)
                            Call CalcStatLeaders(numberGames, TM$, WT$, finalUpdate$)

                        End If

                    End If

                End If

            Next XW

            Call SortStandingsConference
            Call PostLeadersConf(conferenceFile$)
            Call PostRecordsColl

        End If 'Done checking file type

    End If

End Sub


'----------------------------------------
'         FindStatsColl Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the conference and loads them if
'found, notifying the user
Function FindStatsColl% (XW, statFile$, numberGames)

    Shared oppScore(), teamScore()

    Shared oppName$(), memberName$()

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Locate , 1
        Print "NO STATS FOUND FOR "; statFile$
        FindStatsColl% = 0

    Else

        Call ReadScheduleStats(statFile$, XW, numberGames)

        confWins = 0
        confLosses = 0
        confTies = 0

        fullWins = 0
        fullLosses = 0
        fullTies = 0

        confPtsAgnst = 0
        confPtsFor = 0

        'RATINGS
        For I = 1 To numberGames

            statName$ = nameStatFile$(oppName$(I))
            nonConfGame = 0

            For EC = 1 To TEAMS_PER_CONFERENCE
                'If team stat file = opponent of a conference game?
                If statName$ = memberName$(EC) Then
                    nonConfGame = 1
                    Exit For
                End If

            Next EC

            If nonConfGame <> 1 Then

                If teamScore(I) > oppScore(I) Then
                    fullWins = fullWins + 1
                End If

                If teamScore(I) < oppScore(I) Then
                    fullLosses = fullLosses + 1
                End If

                If teamScore(I) = oppScore(I) Then
                    fullTies = fullTies + 1
                End If

            Else

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

            End If

            EC = 0

        Next I

        Cls

        Locate 4, 5

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsColl% = 1

    End If

End Function


'----------------------------------------
'     CalcStatTotalsConf Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub CalcStatTotalsConf (numberGames, teamName$, WT$, finalUpdate$)

    Shared fumbGain

    Shared totalTeamVal!(), defTotals!(), offTotals!(), teamRecords!()

    Shared teamRecDesc$(), totalTeamName$()

    Shared oppScore(), teamScore()

    Cls

    'TEAM CATEGORIES
    'CATEGORY 1 - TOTAL OFFENSE
    CA = 1

    If UCase$(finalUpdate$) = "Y" And (offTotals!(6) + offTotals!(2)) / numberGames > teamRecords!(0) Then
        teamRecords!(0) = (offTotals!(6) + offTotals!(2)) / numberGames
        teamRecDesc$(0) = teamName$
    End If

    If offTotals!(6) + offTotals!(2) > teamRecords!(1) Then
        teamRecords!(1) = offTotals!(6) + offTotals!(2)
        teamRecDesc$(1) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (offTotals!(6) + offTotals!(2)) / (offTotals!(1) + offTotals!(3)) > teamRecords!(2) Then
        teamRecords!(2) = (offTotals!(6) + offTotals!(2)) / (offTotals!(1) + offTotals!(3))
        teamRecDesc$(2) = teamName$
    End If

    If (offTotals!(6) + offTotals!(2)) / numberGames > totalTeamVal!(CA, 20, 5) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = offTotals!(1) + offTotals!(3)
        totalTeamVal!(CA, 20, 3) = offTotals!(6) + offTotals!(2)
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And (defTotals!(6) + defTotals!(2)) / numberGames < teamRecords!(25) Then 
        teamRecords!(25) = (defTotals!(6) + defTotals!(2)) / numberGames
        teamRecDesc$(25) = teamName$
    End If
    If UCase$(finalUpdate$) = "Y" And defTotals!(6) + defTotals!(2) < teamRecords!(26) Then 
        teamRecords!(26) = defTotals!(6) + defTotals!(2)
        teamRecDesc$(26) = teamName$
    End If
    If UCase$(finalUpdate$) = "Y" And (defTotals!(6) + defTotals!(2)) / (defTotals!(1) + defTotals!(3)) < teamRecords!(27) Then 
        teamRecords!(27) = (defTotals!(6) + defTotals!(2)) / (defTotals!(1) + defTotals!(3))
        teamRecDesc$(27) = teamName$
    End If
    If UCase$(finalUpdate$) = "Y" And (defTotals!(6) + defTotals!(2)) / numberGames > teamRecords!(28) Then 
        teamRecords!(28) = (defTotals!(6) + defTotals!(2)) / numberGames
        teamRecDesc$(28) = teamName$
    End If

    If (defTotals!(6) + defTotals!(2)) / numberGames < totalTeamVal!(CA, 20, 5) Then
        'Games
        totalTeamVal!(CA, 20, 1) = numberGames
        'Plays
        totalTeamVal!(CA, 20, 2) = defTotals!(1) + defTotals!(3)
        'Yds
        totalTeamVal!(CA, 20, 3) = defTotals!(6) + defTotals!(2)
        'Avg Yds / Gm
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        'Yds / Gm
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And offTotals!(2) / numberGames > teamRecords!(3) Then
        teamRecords!(3) = offTotals!(2) / numberGames
        teamRecDesc$(3) = teamName$
    End If
    If offTotals!(2) > teamRecords!(4) Then
        teamRecords!(4) = offTotals!(2)
        teamRecDesc$(4) = teamName$
    End If
    If UCase$(finalUpdate$) = "Y" And offTotals!(2) / offTotals!(1) > teamRecords!(5) Then
        teamRecords!(5) = offTotals!(2) / offTotals!(1)
        teamRecDesc$(5) = teamName$
    End If

    If offTotals!(2) / numberGames > totalTeamVal!(CA, 20, 5) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = offTotals!(1)
        totalTeamVal!(CA, 20, 3) = offTotals!(2)
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And defTotals!(2) / numberGames < teamRecords!(29) Then
        teamRecords!(29) = defTotals!(2) / numberGames
        teamRecDesc$(29) = teamName$
    End If
    If UCase$(finalUpdate$) = "Y" And defTotals!(2) < teamRecords!(30) Then
        teamRecords!(30) = defTotals!(2)
        teamRecDesc$(30) = teamName$
    End If
    If UCase$(finalUpdate$) = "Y" And defTotals!(2) / defTotals!(1) < teamRecords!(31) Then
        teamRecords!(31) = defTotals!(2) / defTotals!(1)
        teamRecDesc$(31) = teamName$
    End If

    If defTotals!(2) / numberGames < totalTeamVal!(CA, 20, 5) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = defTotals!(1)
        totalTeamVal!(CA, 20, 3) = defTotals!(2)
        totalTeamVal!(CA, 20, 4) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    For I = 1 To numberGames
        PTS = PTS + teamScore(I)
        OPTS = OPTS + oppScore(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames > teamRecords!(13) Then
        teamRecords!(13) = PTS / numberGames
        teamRecDesc$(13) = teamName$
    End If
    If PTS > teamRecords!(12) Then
        teamRecords!(12) = PTS
        teamRecDesc$(12) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And (PTS - OPTS) / numberGames > teamRecords!(14) Then
        teamRecords!(14) = (PTS - OPTS) / numberGames
        teamRecDesc$(14) = teamName$
    End If

    If PTS / numberGames > totalTeamVal!(CA, 20, 3) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = PTS
        totalTeamVal!(CA, 20, 3) = totalTeamVal!(CA, 20, 2) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    For I = 1 To numberGames
        PTS = PTS + oppScore(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames < teamRecords!(39) Then
        teamRecords!(39) = PTS / numberGames
        teamRecDesc$(39) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And PTS < teamRecords!(38) Then
        teamRecords!(38) = PTS
        teamRecDesc$(38) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames > teamRecords!(41) Then
        teamRecords!(41) = PTS / numberGames
        teamRecDesc$(41) = teamName$
    End If

    If PTS > teamRecords!(40) Then
        teamRecords!(40) = PTS
        teamRecDesc$(40) = teamName$
    End If

    If PTS / numberGames < totalTeamVal!(CA, 20, 3) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = PTS
        totalTeamVal!(CA, 20, 3) = totalTeamVal!(CA, 20, 2) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And offTotals!(6) / numberGames > teamRecords!(6) Then
        teamRecords!(6) = offTotals!(6) / numberGames
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

    If offTotals!(6) / numberGames > totalTeamVal!(CA, 20, 8) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = offTotals!(3)
        totalTeamVal!(CA, 20, 3) = offTotals!(4)
        totalTeamVal!(CA, 20, 4) = offTotals!(5)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 6) = offTotals!(6)
        totalTeamVal!(CA, 20, 7) = offTotals!(6) / offTotals!(3)
        totalTeamVal!(CA, 20, 8) = totalTeamVal!(CA, 20, 6) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And defTotals!(6) / numberGames < teamRecords!(32) Then
        teamRecords!(32) = defTotals!(6) / numberGames
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

    If defTotals!(6) / numberGames < totalTeamVal!(CA, 20, 8) Then

        totalTeamVal!(CA, 20, 1) = numberGames
        totalTeamVal!(CA, 20, 2) = defTotals!(3)
        totalTeamVal!(CA, 20, 3) = defTotals!(4)
        totalTeamVal!(CA, 20, 4) = defTotals!(5)
        totalTeamVal!(CA, 20, 5) = totalTeamVal!(CA, 20, 3) / totalTeamVal!(CA, 20, 2)
        totalTeamVal!(CA, 20, 6) = defTotals!(6)
        totalTeamVal!(CA, 20, 7) = defTotals!(6) / defTotals!(3)
        totalTeamVal!(CA, 20, 8) = totalTeamVal!(CA, 20, 6) / numberGames
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And offTotals!(8) / offTotals!(7) > teamRecords!(15) Then 
        teamRecords!(15) = offTotals!(8) / offTotals!(7)
        teamRecDesc$(15) = teamName$
    End If

    If offTotals!(8) / offTotals!(7) > totalTeamVal!(CA, 20, 2) Then

        totalTeamVal!(CA, 20, 1) = offTotals!(7)
        totalTeamVal!(CA, 20, 2) = offTotals!(8) / offTotals!(7)
        totalTeamName$(CA, 20) = WT$

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
    toMargin! = (fumbGain + intGain - fumbLost - intLost) / numberGames
    'toMargin! = (fumbLost + intLost - fumbGain - intGain) / numberGames

    If toMargin! > totalTeamVal!(CA, 20, 7) Then

        totalTeamVal!(CA, 20, 1) = fumbGain
        totalTeamVal!(CA, 20, 2) = intGain
        totalTeamVal!(CA, 20, 3) = fumbGain + intGain
        totalTeamVal!(CA, 20, 4) = fumbLost
        totalTeamVal!(CA, 20, 5) = intLost
        totalTeamVal!(CA, 20, 6) = fumbLost + intLost
        totalTeamVal!(CA, 20, 7) = toMargin!
        totalTeamName$(CA, 20) = WT$

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

    If UCase$(finalUpdate$) = "Y" And offTotals!(0) / numberGames > teamRecords!(19) Then
        teamRecords!(19) = offTotals!(0) / numberGames
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
    If UCase$(finalUpdate$) = "Y" And offTotals!(11) / numberGames > teamRecords!(23) Then
        teamRecords!(23) = offTotals!(11) / numberGames
        teamRecDesc$(23) = teamName$
    End If

    If UCase$(finalUpdate$) = "Y" And offTotals!(12) / numberGames > teamRecords!(24) Then
        teamRecords!(24) = offTotals!(12) / numberGames
        teamRecDesc$(24) = teamName$
    End If

End Sub


'----------------------------------------
'   SortStandingsConference Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target conference. Currently setup using
'a bubble sort algorithm.
Sub SortStandingsConference

    Shared divWins!(), divLosses!(), divTies!()

    Shared leagPtsAgnst!(), leagPtsFor!()

    Shared memberName$()

    'Currently sorts by Conference Record
    '   To sort by Season Record, use Z = 0 to 1
    For Z = 2 To 3

        AA = (Z * 15) + 1
        ZZ = (Z * 15) + 15

        For X = AA To ZZ - 1:

            For Y = X + 1 To ZZ:

                diffX = divWins!(X) - divLosses!(X)
                diffY = divWins!(Y) - divLosses!(Y)

                If diffX <= diffY Then

                    If diffX <> diffY Then
                        If Not (divWins!(X) <= divWins!(Y) And divWins!(X) > divLosses!(X)) Or Not (divWins!(X) >= divWins!(Y) And divWins!(X) < divLosses!(X)) Then
                            Swap memberName$(X), memberName$(Y)
                            Swap divWins!(X), divWins!(Y)
                            Swap divLosses!(X), divLosses!(Y)
                            Swap divTies!(X), divTies!(Y)
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
'       PostLeadersConf Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the conference,
'and writes the updated stats to the file
Sub PostLeadersConf (confName$)

    Shared memberName$(), leaderName$(), leaderTeam$()
    Shared totalTeamName$()

    Shared totalTeamVal!(), leaderVal!()
    Shared divWins!(), divLosses!(), divTies!()
    Shared leagPtsAgnst!(), leagPtsFor!()

    Cls

    Open diskPaths$(2) + confName$ + ".LDR" For Output As #1

    Print "POSTING "; confName$; " CONFERENCE LEADERS"

    For X = 1 To 60:
        Write #1, memberName$(X), divWins!(X), divLosses!(X), divTies!(X), leagPtsFor!(X), leagPtsAgnst!(X)
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
'       PostRecordsColl Subroutine
'----------------------------------------
'This subroutine writes the calculated
'records to the records file
Sub PostRecordsColl

    Shared indRecDesc$(), teamRecDesc$()

    Shared indRecords!(), teamRecords!()

    Open diskPaths$(1) + "RECORDS.CFB" For Output As #2

    For I = 0 To 50:
        Print #2, indRecords!(I)
        Print #2, indRecDesc$(I, 0)
        Print #2, indRecDesc$(I, 1)
        Print #2, indRecDesc$(I, 2)
    Next

    For I = 0 To 50
        Print #2, teamRecords!(I)
        Print #2, teamRecDesc$(I)
    Next

    Close #2

End Sub
