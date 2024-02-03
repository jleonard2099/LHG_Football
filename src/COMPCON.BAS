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

    Shared compTT!(), compZ1!(), compZ2!()
    Shared O1!(), O2!(), O3!()
    Shared PTSA!(), PTSF!()

    Shared indRecords!(), teamRecords!()

    Shared indRecDesc$()
    Shared compZ1$(), compZ2$(), compZ3$()
    Shared statFiles$(), teamRecDesc$(), TT$()

    Shared div1$, div2$

    'Read in any existing records first
    If _FileExists(diskPaths$(1) + "RECORDS.CFB") Then

        Open diskPaths$(1) + "RECORDS.CFB" For Input As #1

        For I = 0 To 50:
            Input #1, indRecords!(I)
            Input #1, indRecDesc$(I, 0)
            Input #1, indRecDesc$(I, 1)
            Input #1, indRecDesc$(I, 2)
        Next

        For I = 0 To 50:
            Input #1, teamRecords!(I)
            Input #1, teamRecDesc$(I)
        Next

        Close #1

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

    Erase compTT!, compZ1!
    Erase O1!, O2!, O3!
    Erase PTSF!, PTSA!

    For X = 1 To 60
        compZ1$(X) = "*"
    Next

    For X = 1 To 240
        compZ2$(X) = "*"
        compZ3$(X) = "*"
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            TT$(I, I1) = "*"
        Next
    Next

    For I = 1 To 20
        compTT!(2, I, 5) = 9999
        compTT!(4, I, 5) = 9999
        compTT!(6, I, 3) = 9999
        compTT!(8, I, 8) = 9999
        compTT!(10, I, 7) = -99
    Next

    C$ = ","

    conferenceFile$ = ChooseFile$(diskPaths$(2) + "*.LIB", "CONFERENCE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #2

        Input #2, FF$
        Input #2, div1$
        Input #2, div2$

        For XW = 1 To 30
            Input #2, statFiles$(XW)
        Next

        Close #2

        Open diskPaths$(2) + conferenceFile$ + ".LIB" For Input As #3
        Input #3, LG%
        Close #3

        If LG% <> 0 Then

            MsgText$ = "ERROR!! THIS IS NOT A COLLEGE CONFERENCE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Open diskPaths$(2) + conferenceFile$ + ".TMS" For Input As #4

            Input #4, FF$
            Input #4, div1$
            Input #4, div2$

            For X = 1 To 30
                Input #4, compZ1$(X)
            Next X

            Close #4

            For XW = 1 To 30

                'Look for * as terminating char
                If statFiles$(XW) <> Chr$(42) Then

                    haveStats = FindStatsColl%(XW, statFiles$(XW), numberGames)

                    If (haveStats) Then

                        L = Len(statFiles$(XW))
                        WT$ = ""

                        For X = 1 To L:
                            A = Asc(Mid$(statFiles$(XW), X, 1))
                            WT$ = WT$ + Chr$(A)
                        Next X

                        nonConfGame = 0

                        For X = 1 To 30

                            If compZ1$(X) = WT$ Then

                                If O1!(X) = fullWins And O2!(X) = fullLosses And O3!(X) = fullTies Then

                                    nonConfGame = 1
                                    Exit For

                                Else

                                    O1!(X) = fullWins
                                    O2!(X) = fullLosses
                                    O3!(X) = fullTies

                                    O1!(X + 30) = confWins
                                    O2!(X + 30) = confLosses
                                    O3!(X + 30) = confTies

                                    PTSF!(X + 30) = PTSFC
                                    PTSA!(X + 30) = PTSAC
                                    compZ1$(X + 30) = compZ1$(X)

                                End If

                            End If

                        Next X
                        'alternatively, use a Do / Until loop; nonConfGame = 1 or EC = 30

                        If nonConfGame <> 1 And _FileExists(diskPaths$(1) + statFiles$(XW)) Then

                            Call ReadStatsLeaders(statFiles$(XW), numberGames, TM$, 0)
                            Call CalcStatTotalsConf(numberGames, TM$, WT$, finalUpdate$)
                            Call CalcStatLeaders(numberGames, TM$, WT$, finalUpdate$)

                        End If

                    End If

                End If

            Next XW

            Call SortStandings
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

    Shared oppName$(), statFiles$()

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

        PTSAC = 0
        PTSFC = 0

        'RATINGS
        For I = 1 To numberGames

            statName$ = nameStatFile$(oppName$(I))
            nonConfGame = 0

            For EC = 1 To 30
                'If team stat file = opponent of a conference game?
                If statName$ = statFiles$(EC) Then
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

                PTSFC = PTSFC + teamScore(I)
                PTSAC = PTSAC + oppScore(I)

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
Sub CalcStatTotalsConf (numberGames, TM$, WT$, finalUpdate$)

    Shared fumbGain

    Shared compTT!(), DT!(), OT!(), teamRecords!()

    Shared teamRecDesc$(), TT$()

    Shared oppScore(), teamScore()

    Cls

    'TEAM CATEGORIES
    'CATEGORY 1 - TOTAL OFFENSE
    CA = 1

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2)) / numberGames > teamRecords!(0) Then
        teamRecords!(0) = (OT!(6) + OT!(2)) / numberGames
        teamRecDesc$(0) = TM$
    End If

    If OT!(6) + OT!(2) > teamRecords!(1) Then
        teamRecords!(1) = OT!(6) + OT!(2)
        teamRecDesc$(1) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2)) / (OT!(1) + OT!(3)) > teamRecords!(2) Then
        teamRecords!(2) = (OT!(6) + OT!(2)) / (OT!(1) + OT!(3))
        teamRecDesc$(2) = TM$
    End If

    If (OT!(6) + OT!(2)) / numberGames > compTT!(CA, 20, 5) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = OT!(1) + OT!(3)
        compTT!(CA, 20, 3) = OT!(6) + OT!(2)
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 5) < compTT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 2 - TOTAL DEFENSE
    CA = 2

    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2)) / numberGames < teamRecords!(25) Then teamRecords!(25) = (DT!(6) + DT!(2)) / numberGames: teamRecDesc$(25) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) + DT!(2) < teamRecords!(26) Then teamRecords!(26) = DT!(6) + DT!(2): teamRecDesc$(26) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2)) / (DT!(1) + DT!(3)) < teamRecords!(27) Then teamRecords!(27) = (DT!(6) + DT!(2)) / (DT!(1) + DT!(3)): teamRecDesc$(27) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2)) / numberGames > teamRecords!(28) Then teamRecords!(28) = (DT!(6) + DT!(2)) / numberGames: teamRecDesc$(28) = TM$

    If (DT!(6) + DT!(2)) / numberGames < compTT!(CA, 20, 5) Then
        'Games
        compTT!(CA, 20, 1) = numberGames
        'Plays
        compTT!(CA, 20, 2) = DT!(1) + DT!(3)
        'Yds
        compTT!(CA, 20, 3) = DT!(6) + DT!(2)
        'Avg Yds / Gm
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        'Yds / Gm
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 5) > compTT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 3 - RUSHING OFFENSE
    CA = 3

    If UCase$(finalUpdate$) = "Y" And OT!(2) / numberGames > teamRecords!(3) Then teamRecords!(3) = OT!(2) / numberGames: teamRecDesc$(3) = TM$
    If OT!(2) > teamRecords!(4) Then teamRecords!(4) = OT!(2): teamRecDesc$(4) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(2) / OT!(1) > teamRecords!(5) Then teamRecords!(5) = OT!(2) / OT!(1): teamRecDesc$(5) = TM$

    If OT!(2) / numberGames > compTT!(CA, 20, 5) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = OT!(1)
        compTT!(CA, 20, 3) = OT!(2)
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 5) < compTT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 4 - RUSHING DEFENSE
    CA = 4

    If UCase$(finalUpdate$) = "Y" And DT!(2) / numberGames < teamRecords!(29) Then teamRecords!(29) = DT!(2) / numberGames: teamRecDesc$(29) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(2) < teamRecords!(30) Then teamRecords!(30) = DT!(2): teamRecDesc$(30) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(2) / DT!(1) < teamRecords!(31) Then teamRecords!(31) = DT!(2) / DT!(1): teamRecDesc$(31) = TM$

    If DT!(2) / numberGames < compTT!(CA, 20, 5) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = DT!(1)
        compTT!(CA, 20, 3) = DT!(2)
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 5) > compTT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
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

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames > teamRecords!(13) Then teamRecords!(13) = PTS / numberGames: teamRecDesc$(13) = TM$
    If PTS > teamRecords!(12) Then teamRecords!(12) = PTS: teamRecDesc$(12) = TM$

    If UCase$(finalUpdate$) = "Y" And (PTS - OPTS) / numberGames > teamRecords!(14) Then teamRecords!(14) = (PTS - OPTS) / numberGames: teamRecDesc$(14) = TM$
    If PTS / numberGames > compTT!(CA, 20, 3) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = PTS
        compTT!(CA, 20, 3) = compTT!(CA, 20, 2) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 3) < compTT!(CA, Z1, 3) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
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

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames < teamRecords!(39) Then teamRecords!(39) = PTS / numberGames: teamRecDesc$(39) = TM$
    If UCase$(finalUpdate$) = "Y" And PTS < teamRecords!(38) Then teamRecords!(38) = PTS: teamRecDesc$(38) = TM$
    If UCase$(finalUpdate$) = "Y" And PTS / numberGames > teamRecords!(41) Then teamRecords!(41) = PTS / numberGames: teamRecDesc$(41) = TM$
    If PTS > teamRecords!(40) Then teamRecords!(40) = PTS: teamRecDesc$(40) = TM$

    If PTS / numberGames < compTT!(CA, 20, 3) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = PTS
        compTT!(CA, 20, 3) = compTT!(CA, 20, 2) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19

            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 3) > compTT!(CA, Z1, 3) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1

        Next Z

    End If

    'CATEGORY 7 - PASSING OFFENSE
    CA = 7

    If UCase$(finalUpdate$) = "Y" And OT!(6) / numberGames > teamRecords!(6) Then teamRecords!(6) = OT!(6) / numberGames: teamRecDesc$(6) = TM$
    If OT!(6) > teamRecords!(7) Then teamRecords!(7) = OT!(6): teamRecDesc$(7) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(6) / OT!(3) > teamRecords!(8) Then teamRecords!(8) = OT!(6) / OT!(3): teamRecDesc$(8) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(6) / OT!(4) > teamRecords!(9) Then teamRecords!(9) = OT!(6) / OT!(4): teamRecDesc$(9) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(4) / OT!(3) > teamRecords!(10) Then teamRecords!(10) = OT!(4) / OT!(3): teamRecDesc$(10) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(5) / OT!(3) < teamRecords!(11) Then teamRecords!(11) = OT!(5) / OT!(3): teamRecDesc$(11) = TM$

    If OT!(6) / numberGames > compTT!(CA, 20, 8) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = OT!(3)
        compTT!(CA, 20, 3) = OT!(4)
        compTT!(CA, 20, 4) = OT!(5)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 6) = OT!(6)
        compTT!(CA, 20, 7) = OT!(6) / OT!(3)
        compTT!(CA, 20, 8) = compTT!(CA, 20, 6) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 8) < compTT!(CA, Z1, 8) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 8 - PASSING DEFENSE
    CA = 8

    If UCase$(finalUpdate$) = "Y" And DT!(6) / numberGames < teamRecords!(32) Then
        teamRecords!(32) = DT!(6) / numberGames
        teamRecDesc$(32) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(6) < teamRecords!(33) Then
        teamRecords!(33) = DT!(6)
        teamRecDesc$(33) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(6) / DT!(3) < teamRecords!(34) Then
        teamRecords!(34) = DT!(6) / DT!(3)
        teamRecDesc$(34) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(6) / DT!(4) < teamRecords!(35) Then
        teamRecords!(35) = DT!(6) / DT!(4)
        teamRecDesc$(35) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(4) / DT!(3) < teamRecords!(36) Then
        teamRecords!(36) = DT!(4) / DT!(3)
        teamRecDesc$(36) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(5) / DT!(3) > teamRecords!(37) Then
        teamRecords!(37) = DT!(5) / DT!(3)
        teamRecDesc$(37) = TM$
    End If

    If DT!(6) / numberGames < compTT!(CA, 20, 8) Then

        compTT!(CA, 20, 1) = numberGames
        compTT!(CA, 20, 2) = DT!(3)
        compTT!(CA, 20, 3) = DT!(4)
        compTT!(CA, 20, 4) = DT!(5)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 6) = DT!(6)
        compTT!(CA, 20, 7) = DT!(6) / DT!(3)
        compTT!(CA, 20, 8) = compTT!(CA, 20, 6) / numberGames
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 8) > compTT!(CA, Z1, 8) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 9 - PUNTING
    CA = 9

    If UCase$(finalUpdate$) = "Y" And OT!(8) / OT!(7) > teamRecords!(15) Then teamRecords!(15) = OT!(8) / OT!(7): teamRecDesc$(15) = TM$

    If OT!(8) / OT!(7) > compTT!(CA, 20, 2) Then

        compTT!(CA, 20, 1) = OT!(7)
        compTT!(CA, 20, 2) = OT!(8) / OT!(7)
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 2) < compTT!(CA, Z1, 2) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 10 - TURNOVER MARGIN
    CA = 10

    'IG = Interceptions Gained
    'FL = Fumbles Lost
    'IL = Interceptions Lost
    fumbGain = OT!(9) - OT!(10) + DT!(10)
    IG = DT!(5)
    FL = OT!(10) + DT!(9) - DT!(10)
    IL = OT!(5)
    TM! = (fumbGain + IG - FL - IL) / numberGames

    If TM! > compTT!(CA, 20, 7) Then

        compTT!(CA, 20, 1) = fumbGain
        compTT!(CA, 20, 2) = IG
        compTT!(CA, 20, 3) = fumbGain + IG
        compTT!(CA, 20, 4) = FL
        compTT!(CA, 20, 5) = IL
        compTT!(CA, 20, 6) = FL + IL
        compTT!(CA, 20, 7) = TM!
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If compTT!(CA, Z, 7) < compTT!(CA, Z1, 7) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap compTT!(CA, Z1, MN), compTT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'PUNT RETURNS
    If UCase$(finalUpdate$) = "Y" And OT!(25) / OT!(24) > teamRecords!(16) Then
        teamRecords!(16) = OT!(25) / OT!(24)
        teamRecDesc$(16) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(25) / DT!(24) < teamRecords!(42) Then
        teamRecords!(42) = DT!(25) / DT!(24)
        teamRecDesc$(42) = TM$
    End If

    'KO Return
    If UCase$(finalUpdate$) = "Y" And OT!(23) / OT!(22) > teamRecords!(17) Then
        teamRecords!(17) = OT!(23) / OT!(22)
        teamRecDesc$(17) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And DT!(23) / DT!(22) < teamRecords!(43) Then
        teamRecords!(43) = DT!(23) / DT!(22)
        teamRecDesc$(43) = TM$
    End If

    'FIRST DOWNS
    If OT!(0) > teamRecords!(18) Then
        teamRecords!(18) = OT!(0)
        teamRecDesc$(18) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And OT!(0) / numberGames > teamRecords!(19) Then
        teamRecords!(19) = OT!(0) / numberGames
        teamRecDesc$(19) = TM$
    End If

    'FUMBLES
    If OT!(9) > teamRecords!(20) Then
        teamRecords!(20) = OT!(9)
        teamRecDesc$(20) = TM$
    End If

    If OT!(10) > teamRecords!(21) Then
        teamRecords!(21) = OT!(10)
        teamRecDesc$(21) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And OT!(10) < teamRecords!(22) Then
        teamRecords!(22) = OT!(10)
        teamRecDesc$(22) = TM$
    End If

    'PENALTIES
    If UCase$(finalUpdate$) = "Y" And OT!(11) / numberGames > teamRecords!(23) Then
        teamRecords!(23) = OT!(11) / numberGames
        teamRecDesc$(23) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And OT!(12) / numberGames > teamRecords!(24) Then
        teamRecords!(24) = OT!(12) / numberGames
        teamRecDesc$(24) = TM$
    End If

End Sub


'----------------------------------------
'       PostLeadersConf Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the conference,
'and writes the updated stats to the file
Sub PostLeadersConf (confName$)

    Shared compZ1$(), compZ2$(), compZ3$()
    Shared TT$()

    Shared compTT!(), compZ1!()
    Shared O1!(), O2!(), O3!()
    Shared PTSA!(), PTSF!()

    Cls

    Open diskPaths$(2) + confName$ + ".LDR" For Output As #1

    Print "POSTING "; confName$; " CONFERENCE LEADERS"

    For X = 1 To 60:
        Write #1, compZ1$(X), O1!(X), O2!(X), O3!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Write #1, compZ2$(X), compZ3$(X), compZ1!(X, 1), compZ1!(X, 2)
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Write #1, compTT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Write #1, TT$(I, I1)
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

