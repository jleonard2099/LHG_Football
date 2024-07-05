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

    Shared indRecords!(), compTT!(), compZ1!()
    Shared N1!(), NT!()
    Shared O1!(), O2!(), O3!(), OT!()
    Shared PT!(), PTSA!(), PTSF!(), teamRecords!()

    Shared statFilesComp$(), compZ2$(), compZ3$()
    Shared indRecDesc$()
    Shared memberName$()
    Shared N2$(), N3$(), NT$()
    Shared PT$(), teamRecDesc$(), TT$()
    Shared statFiles$()

    Dim NRT

    'Read in any existing records
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

    Erase NT!
    Erase N2$, N3$, NT$

    For X = 1 To 240
        N2$(X) = "*"
        N3$(X) = "*"
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            NT$(I, I1) = "*"
        Next
    Next

    For I = 1 To 20
        NT!(2, I, 5) = 9999
        NT!(4, I, 5) = 9999
        NT!(6, I, 3) = 9999
        NT!(8, I, 8) = 9999
        NT!(10, I, 7) = -99
    Next

    C$ = ","

    organizationFile$ = ChooseFile$ (diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON RECORDS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + organizationFile$ + ".CON" For Input As #2

        Input #2, X$

        For I = 1 To MAX_CONFERENCES
            Input #2, memberName$(I)
        Next

        Close #2

        For currConf = 1 To MAX_CONFERENCES

            'Look for * as terminating char
            If memberName$(currConf) <> "XXX" Then

                Cls

                Erase compTT!, compZ1!
                Erase O1!, O2!, O3!
                Erase PTSF!, PTSA!

                For X = 1 To 60
                    statFilesComp$(X) = "*"
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

                targetConf$ = diskPaths$(2) + memberName$(currConf) + ".TMS"

                If _FileExists(targetConf$) Then

                    Open targetConf$ For Input As #4
                    Input #4, statType
                    Close #4

                    If statType <> 0 Then

                        MsgText$ = "ERROR!! THIS IS NOT A COLLEGE CONFERENCE. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
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
                            Input #3, statFiles$(XW)
                            statFilesComp$(X) = statFiles$(XW)
                        Next

                        Close #3

                        For XW = 1 To 30

                            'Look for * as terminating char
                            If statFiles$(XW) <> Chr$(42) Then

                                haveStats = FindStatsNat%(XW, statFiles$(XW), numberGames, NRT)

                                If (haveStats) Then

                                    L = Len(statFiles$(XW))
                                    WT$ = ""

                                    For X = 1 To L
                                        A = Asc(Mid$(statFiles$(XW), X, 1))
                                        WT$ = WT$ + Chr$(A)
                                    Next X

                                    nonConfGame = 0

                                    For X = 1 To 30

                                        If statFilesComp$(X) = WT$ Then

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
                                                statFilesComp$(X + 30) = statFilesComp$(X)

                                            End If

                                        End If

                                    Next X
                                    'alternatively, use a Do / Until loop; nonConfGame = 1 or EC = 30

                                    If nonConfGame <> 1 And _FileExists(diskPaths$(1) + statFiles$(XW)) Then

                                        Call ReadStatsLeaders(statFiles$(XW), numberGames, TM$, 0)
                                        Call CalcStatTotalsConf(numberGames, TM$, WT$, finalUpdate$)
                                        Call CalcStatLeaders(numberGames, TM$, WT$, finalUpdate$)

                                    End If

                                End If 'Done looking for stats

                            End If 'Done checking for valid stat file

                        Next XW

                        Call SortStandingsLeague
                        Call PostLeadersConf(memberName$(currConf))
                        Call SortLeadersNat

                    End If 'Done checking for valid conference

                End If 'Done checking if file exists

            End If

        Next currConf

        'All conferences are done; post org leaders
        Call PostLeadersOrg(organizationFile$, NRT)
        Call PostRecordsColl

    End If 'Done checking ESC

End Sub


'----------------------------------------
'         FindStatsNat Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the organization and loads them if
'found, notifying the user
Function FindStatsNat% (XW, statFile$, numberGames, NRT)

    Shared powerRating(), oppScore(), teamScore()

    Shared PT!(), PTSA!(), PTSF!()

    Shared locIndicator$(), PT$()
    Shared oppName$(), statFiles$()

    findFile$ = diskPaths$(1) + statFile$ + "S"

    If Not _FileExists(findFile$) Then

        Locate , 1
        Print "NO STATS FOUND FOR "; statFile$
        FindStatsNat% = 0

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

        'STRENGTH OF SCHEDULE
        teamPower = 0
        SS = 0

        For I = 1 To numberGames
            SS = powerRating(1, I) + SS
        Next

        SS = SS / numberGames

        'INDIVIDUAL GAME POWER RATINGS
        GL = 0
        teamPower = 0

        For I = 1 To numberGames
            RAT = teamScore(I) - oppScore(I) + powerRating(1, I)
            If Left$(locIndicator$(I), 1) = "H" Then RAT = RAT - 3
            If Left$(locIndicator$(I), 1) = "A" Then RAT = RAT + 3
            teamPower = teamPower + RAT
        Next

        If numberGames <= 4 Then
            GL = 5 - numberGames
            teamPower = teamPower + GL * powerRating(0, 1)
        End If

        teamPower = teamPower / (numberGames + GL)
        NRT = NRT + 1
        PT$(NRT) = statFile$
        PT!(NRT, 1) = teamPower
        PT!(NRT, 2) = fullWins
        PT!(NRT, 3) = fullLosses
        PT!(NRT, 4) = fullTies
        PT!(NRT, 5) = SS

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
'       SortLeadersNat Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target organization
Sub SortLeadersNat ()

    Shared compTT!(), compZ1!()
    Shared N1!(), NT!()

    Shared compZ2$(), compZ3$()
    Shared N2$(), N3$(), NT$(), TT$()

    ' RUSHING AVERAGE
    For X = 1 To 20

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then

            If compZ1!(X, 1) >= N1!(20, 1) Then

                N1!(20, 1) = compZ1!(X, 1)
                N1!(20, 2) = compZ1!(X, 2)
                N2$(20) = compZ2$(X)
                N3$(20) = compZ3$(X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RECEIVING AVERAGE
    For X = 21 To 40

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then

            If compZ1!(X, 1) >= N1!(40, 1) Then

                N1!(40, 1) = compZ1!(X, 1)
                N1!(40, 2) = compZ1!(X, 2)
                N2$(40) = compZ2$(X)
                N3$(40) = compZ3$(X)

                For Z = 21 To 39
                    For Z1 = Z + 1 To 40
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RUSHING YARDAGE
    For X = 41 To 50

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(50, 1) Then
                N1!(50, 1) = compZ1!(X, 1): N1!(50, 2) = compZ1!(X, 2)
                N2$(50) = compZ2$(X): N3$(50) = compZ3$(X)

                For Z = 41 To 49
                    For Z1 = Z + 1 To 50
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' RECEIVING YARDAGE
    For X = 51 To 60

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(60, 1) Then
                N1!(60, 1) = compZ1!(X, 1)
                N1!(60, 2) = compZ1!(X, 2)
                N2$(60) = compZ2$(X)
                N3$(60) = compZ3$(X)

                For Z = 51 To 59
                    For Z1 = Z + 1 To 60
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If
        End If

    Next X

    ' RUSHING TD'S
    For X = 61 To 70

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(70, 1) Then
                N1!(70, 1) = compZ1!(X, 1): N1!(70, 2) = compZ1!(X, 2)
                N2$(70) = compZ2$(X): N3$(70) = compZ3$(X)
                For Z = 61 To 69
                    For Z1 = Z + 1 To 70
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' RECEIVING TD'S
    For X = 71 To 80

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(80, 1) Then
                N1!(80, 1) = compZ1!(X, 1)
                N1!(80, 2) = compZ1!(X, 2)
                N2$(80) = compZ2$(X)
                N3$(80) = compZ3$(X)

                For Z = 71 To 79
                    For Z1 = Z + 1 To 80
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' RUSHING ATT
    For X = 81 To 90

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(90, 1) Then
                N1!(90, 1) = compZ1!(X, 1): N1!(90, 2) = compZ1!(X, 2)
                N2$(90) = compZ2$(X): N3$(90) = compZ3$(X)
                For Z = 81 To 89
                    For Z1 = Z + 1 To 90
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' TOTAL RECEPTIONS
    For X = 91 To 100

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(100, 1) Then
                N1!(100, 1) = compZ1!(X, 1): N1!(100, 2) = compZ1!(X, 2)
                N2$(100) = compZ2$(X): N3$(100) = compZ3$(X)
                For Z = 91 To 99
                    For Z1 = Z + 1 To 100
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' TOTAL OFFENSE
    For X = 101 To 110

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(110, 1) Then
                N1!(110, 1) = compZ1!(X, 1)
                N1!(110, 2) = compZ1!(X, 2)
                N2$(110) = compZ2$(X)
                N3$(110) = compZ3$(X)
                For Z = 101 To 109
                    For Z1 = Z + 1 To 110
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If
    Next X

    ' SCORING NON-KICKERS
    For X = 111 To 120

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(120, 1) Then
                N1!(120, 1) = compZ1!(X, 1)
                N1!(120, 2) = compZ1!(X, 2)
                N2$(120) = compZ2$(X)
                N3$(120) = compZ3$(X)
                For Z = 111 To 119
                    For Z1 = Z + 1 To 120
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' COMPLETION PERCENTAGE
    For X = 121 To 130

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(130, 1) Then
                N1!(130, 1) = compZ1!(X, 1)
                N1!(130, 2) = compZ1!(X, 2)
                N2$(130) = compZ2$(X)
                N3$(130) = compZ3$(X)
                For Z = 121 To 129
                    For Z1 = Z + 1 To 130
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' QB RATING
    For X = 131 To 140

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(140, 1) Then
                N1!(140, 1) = compZ1!(X, 1)
                N1!(140, 2) = compZ1!(X, 2)
                N2$(140) = compZ2$(X)
                N3$(140) = compZ3$(X)
                For Z = 131 To 139
                    For Z1 = Z + 1 To 140
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PASSING YARDAGE
    For X = 141 To 150

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(150, 1) Then
                N1!(150, 1) = compZ1!(X, 1)
                N1!(150, 2) = compZ1!(X, 2)
                N2$(150) = compZ2$(X)
                N3$(150) = compZ3$(X)

                For Z = 141 To 149
                    For Z1 = Z + 1 To 150
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PASSING TD'S
    For X = 151 To 160

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(160, 1) Then
                N1!(160, 1) = compZ1!(X, 1)
                N1!(160, 2) = compZ1!(X, 2)
                N2$(160) = compZ2$(X)
                N3$(160) = compZ3$(X)

                For Z = 151 To 159
                    For Z1 = Z + 1 To 160
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' KO RETURNS
    For X = 161 To 170

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(170, 1) Then
                N1!(170, 1) = compZ1!(X, 1)
                N1!(170, 2) = compZ1!(X, 2)
                N2$(170) = compZ2$(X)
                N3$(170) = compZ3$(X)
                For Z = 161 To 169
                    For Z1 = Z + 1 To 170
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PR RETURNS
    For X = 171 To 180

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(180, 1) Then
                N1!(180, 1) = compZ1!(X, 1)
                N1!(180, 2) = compZ1!(X, 2)
                N2$(180) = compZ2$(X)
                N3$(180) = compZ3$(X)
                For Z = 171 To 179
                    For Z1 = Z + 1 To 180
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' PUNTING
    For X = 181 To 190

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(190, 1) Then
                N1!(190, 1) = compZ1!(X, 1)
                N1!(190, 2) = compZ1!(X, 2)
                N2$(190) = compZ2$(X)
                N3$(190) = compZ3$(X)
                For Z = 181 To 189
                    For Z1 = Z + 1 To 190
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' SCORING (KICKERS)
    For X = 191 To 200

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(200, 1) Then
                N1!(200, 1) = compZ1!(X, 1)
                N1!(200, 2) = compZ1!(X, 2)
                N2$(200) = compZ2$(X)
                N3$(200) = compZ3$(X)
                For Z = 191 To 199
                    For Z1 = Z + 1 To 200
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' FG%
    For X = 201 To 210

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(210, 1) Then
                N1!(210, 1) = compZ1!(X, 1)
                N1!(210, 2) = compZ1!(X, 2)
                N2$(210) = compZ2$(X)
                N3$(210) = compZ3$(X)
                For Z = 201 To 209
                    For Z1 = Z + 1 To 210
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' EP%
    For X = 211 To 220

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then
            If compZ1!(X, 1) >= N1!(220, 1) Then
                N1!(220, 1) = compZ1!(X, 1)
                N1!(220, 2) = compZ1!(X, 2)
                N2$(220) = compZ2$(X)
                N3$(220) = compZ3$(X)
                For Z = 211 To 219
                    For Z1 = Z + 1 To 220
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z
            End If
        End If

    Next X

    ' INT'S
    For X = 221 To 230

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then

            If compZ1!(X, 1) >= N1!(230, 1) Then
                N1!(230, 1) = compZ1!(X, 1)
                N1!(230, 2) = compZ1!(X, 2)
                N2$(230) = compZ2$(X)
                N3$(230) = compZ3$(X)

                For Z = 221 To 229
                    For Z1 = Z + 1 To 230
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
                            Next MN
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    ' SACKS
    For X = 231 To 240

        If compZ2$(X) <> "*" And compZ2$(X) <> "" Then

            If compZ1!(X, 1) >= N1!(240, 1) Then

                N1!(240, 1) = compZ1!(X, 1)
                N1!(240, 2) = compZ1!(X, 2)
                N2$(240) = compZ2$(X)
                N3$(240) = compZ3$(X)

                For Z = 231 To 239
                    For Z1 = Z + 1 To 240
                        If N1!(Z, 1) < N1!(Z1, 1) Then
                            Swap N2$(Z1), N2$(Z)
                            Swap N3$(Z1), N3$(Z)
                            For MN = 1 To 2
                                Swap N1!(Z1, MN), N1!(Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 5) >= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 5) < NT!(CA, Z1, 5) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 5) <= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 5) > NT!(CA, Z1, 5) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 5) >= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20

                        If NT!(CA, Z, 5) < NT!(CA, Z1, 5) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 5) <= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 5) > NT!(CA, Z1, 5) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 3) >= NT!(CA, 20, 3) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 3) < NT!(CA, Z1, 3) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 3) <= NT!(CA, 20, 3) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 3) > NT!(CA, Z1, 3) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then

            If compTT!(CA, X, 8) >= NT!(CA, 20, 8) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next

                NT$(CA, 20) = TT$(CA, X)

                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 8) < NT!(CA, Z1, 8) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then
            If compTT!(CA, X, 8) <= NT!(CA, 20, 8) Then
                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next
                NT$(CA, 20) = TT$(CA, X)
                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 8) > NT!(CA, Z1, 8) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then
            If compTT!(CA, X, 2) >= NT!(CA, 20, 2) Then
                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next
                NT$(CA, 20) = TT$(CA, X)
                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 2) < NT!(CA, Z1, 2) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

        If TT$(CA, X) <> "*" And TT$(CA, X) <> "" Then
            If compTT!(CA, X, 7) >= NT!(CA, 20, 7) Then
                For I = 1 To 8
                    NT!(CA, 20, I) = compTT!(CA, X, I)
                Next
                NT$(CA, 20) = TT$(CA, X)
                For Z = 1 To 19
                    For Z1 = Z + 1 To 20
                        If NT!(CA, Z, 7) < NT!(CA, Z1, 7) Then
                            Swap NT$(CA, Z1), NT$(CA, Z)
                            For MN = 1 To 9
                                Swap NT!(CA, Z1, MN), NT!(CA, Z, MN)
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

    Shared statFilesComp$(), N2$(), N3$(), NT$()
    Shared PT$()

    Shared N1!(), NT!()
    Shared O1!(), O2!(), O3!()
    Shared PTSA!(), PTSF!(), PT!()

    Cls

    Open diskPaths$(2) + organization$ + ".NLD" For Output As #1

    Print "POSTING "; organization$; " LEADERS"

    For X = 1 To 60:
        Write #1, statFilesComp$(X), O1!(X), O2!(X), O3!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Write #1, N2$(X), N3$(X), N1!(X, 1), N1!(X, 2)
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Write #1, NT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Write #1, NT$(I, I1)
        Next
    Next

    Close #1

    For Z = 1 To NRT - 1
        For Z1 = Z + 1 To NRT
            If PT!(Z, 1) < PT!(Z1, 1) Then
                Swap PT$(Z1), PT$(Z)
                For MN = 1 To 5
                    Swap PT!(Z1, MN), PT!(Z, MN)
                Next MN
            End If
        Next Z1
    Next Z

    Open diskPaths$(2) + organization$ + ".POL" For Output As #6

    Print "POSTING "; organization$; " POLL"

    Write #6, NRT

    For X = 1 To 1200
        Write #6, PT$(X), PT!(X, 1), PT!(X, 2), PT!(X, 3), PT!(X, 4), PT!(X, 5)
    Next

    Close #6

End Sub


'----------------------------------------
'      ReadScheduleStats Subroutine
'----------------------------------------
'This subroutine reads in all data from the
'team's schedule stats file.
Sub ReadScheduleStats (statFile$, XW, numberGames)

    Shared oppName$(), locIndicator$()

    Shared powerRating(), oppScore(), teamScore()

    Shared PTSA!(), PTSF!()

    findFile$ = diskPaths$(1) + statFile$ + "S"

    Open findFile$ For Input As #1

    Input #1, numberGames

    For I = 1 to MAX_SCHED_STATS:
        Input #1, teamScore(I)
        PTSF!(XW) = PTSF!(XW) + teamScore(I)
    Next I

    For I = 1 to MAX_SCHED_STATS:
        Input #1, oppName$(I)
    Next I

    For I = 1 to MAX_SCHED_STATS:
        Input #1, oppScore(I)
        PTSA!(XW) = PTSA!(XW) + oppScore(I)
    Next I

    For I = 1 to MAX_SCHED_STATS:
        Input #1, locIndicator$(I)
    Next I

    For I = 0 To 1
        For I1 = 1 to MAX_SCHED_STATS
            Input #1, powerRating(I, I1)
        Next
    Next

    Close #1

End Sub
