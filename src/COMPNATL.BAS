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

    Shared indRecDesc$()
    Shared compZ1$(), compZ2$(), compZ3$()
    Shared H1$()
    Shared N2$(), N3$(), NT$()
    Shared PT$(), teamRecDesc$(), TT$()

    Shared div1$, div2$

    Dim NRT

    Dim CN$(40)

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

        For I = 1 To 40
            Input #2, CN$(I)
        Next

        Close #2

        For NC = 1 To 40

            'Look for * as terminating char
            If CN$(NC) <> "XXX" Then

                Cls

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

                Open diskPaths$(2) + CN$(NC) + ".TMS" For Input As #3

                Input #3, FF$
                Input #3, div1$
                Input #3, div2$

                For XW = 1 To 30
                    Input #3, H1$(XW)
                Next

                Close #3

                Open diskPaths$(2) + CN$(NC) + ".LIB" For Input As #4
                Input #4, LG%
                Close #4

                If LG% <> 0 Then

                    MsgText$ = "ERROR!! THIS IS NOT A COLLEGE CONFERENCE. PRESS ANY KEY TO RETURN TO MAIN MENU"
                    BoxWidth = 20
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)
                    Exit Sub

                Else

                    Open diskPaths$(2) + CN$(NC) + ".TMS" For Input As #5

                    Input #5, FF$
                    Input #5, div1$
                    Input #5, div2$

                    For X = 1 To 30
                        Input #5, compZ1$(X)
                    Next X

                    Close #5

                    For XW = 1 To 30

                        'Look for * as terminating char
                        If H1$(XW) <> Chr$(42) Then

                            haveStats = FindStatsNat%(XW, H1$(XW), numberGames, NRT)

                            If (haveStats) Then

                                L = Len(H1$(XW))
                                WT$ = ""

                                For X = 1 To L
                                    A = Asc(Mid$(H1$(XW), X, 1))
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

                                If nonConfGame <> 1 And _FileExists(diskPaths$(1) + H1$(XW)) Then

                                    Call ReadStatsNat(H1$(XW), numberGames, TM$)
                                    Call StatsByCategoryNatl(numberGames, TM$, WT$, finalUpdate$)
                                    Call CheckStatsNatl(numberGames, TM$, WT$, finalUpdate$)

                                End If

                            End If

                        End If

                    Next XW

                End If 'Done checking for valid conference

                Call SortLeadersConf
                Call PostLeadersConf(CN$(NC))
                Call SortLeadersNat

            End If

        Next NC

        'All conferences are done; post org leaders
        Call PostLeadersOrg(organizationFile$)

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

        Open diskPaths$(2) + organizationFile$ + ".POL" For Output As #6

        Print "POSTING "; organizationFile$; " POLL"

        Write #6, NRT

        For X = 1 To 1200
            Write #6, PT$(X), PT!(X, 1), PT!(X, 2), PT!(X, 3), PT!(X, 4), PT!(X, 5)
        Next

        Close #6

        Open diskPaths$(1) + "RECORDS.CFB" For Output As #7

        For I = 0 To 50:
            Print #7, indRecords!(I)
            Print #7, indRecDesc$(I, 0)
            Print #7, indRecDesc$(I, 1)
            Print #7, indRecDesc$(I, 2)
        Next

        For I = 0 To 50:
            Print #7, teamRecords!(I)
            Print #7, teamRecDesc$(I)
        Next

        Close #7

    End If 'Done checking ESC

End Sub


'----------------------------------------
'         FindStatsNat Function
'----------------------------------------
'...explanation...
Function FindStatsNat% (XW, statFile$, numberGames, NRT)

    Shared H1$(), PT$(), locIndicator$(), oppName$()

    Shared powerRating(), oppScore(), teamScore()

    Shared PT!(), PTSA!(), PTSF!()

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
                If statName$ = H1$(EC) Then
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
'       ReadStatsNat Subroutine
'----------------------------------------
'This subroutine reads stats in for a 
'single team in the organization
Sub ReadStatsNat (statFile$, numberGames, TM$)

    Shared puntNum
    Shared puntName$

    Shared compKR$(), compPK$(), compPR$(), compQB$(), compRB$(), compWR$()
    Shared H1$(), intName$(), PKR$(), sackName$()
    Shared statsLC$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), statsLR$()
    Shared TB$()

    Shared AM!(), AF!()
    Shared compPR!(), compQB!(), compRB!(), compWR!(), compZ2!()
    Shared DT!(), EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared NT!(), OT!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsPK!()
    Shared TD!(), TG!(), TP!(), YD!()

    Shared statsGI%(), statsGS%()

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, TM$

    For I = 1 To 10: 
        Input #1, compRB$(I)
    Next

    For I = 1 To 6: 
        Input #1, compWR$(I)
    Next

    For I = 1 To 4: 
        Input #1, compQB$(I)
    Next

    For I = 1 To 3: 
        Input #1, compKR$(I)
    Next

    For I = 1 To 3: 
        Input #1, compPR$(I)
    Next I

    Input #1, puntName$

    For I = 1 To 2: 
        Input #1, compPK$(I)
    Next

    For J = 1 To 14
        For K = 1 To 18
            Input #1, compZ2!(J, K)
        Next K
    Next J

    For L = 0 To 38: 
        Input #1, OT!(L)
    Next L

    For L = 0 To 38: 
        Input #1, DT!(L)
    Next L

    For J = 0 To 9: 
        Input #1, intName$(J)
        For J1 = 0 To 2
            Input #1, statsGI%(J, J1)
        Next
    Next

    For J = 0 To 14: 
        Input #1, sackName$(J)
        For J1 = 0 To 1: 
            Input #1, statsGS%(J, J1)
        Next
    Next

    For I = 0 To 9: 
        Input #1, statsLR!(I)
        Input #1, statsLR$(I, 0)
        Input #1, statsLR$(I, 1)
    Next

    For I = 0 To 15: 
        Input #1, statsLC!(I)
        Input #1, statsLC$(I, 0)
        Input #1, statsLC$(I, 1)
    Next

    For I = 0 To 3: 
        Input #1, statsLP!(I)
        Input #1, statsLP$(I, 0)
        Input #1, statsLP$(I, 1)
    Next

    For I = 0 To 2: 
        Input #1, statsLK!(I)
        Input #1, statsLK$(I, 0)
        Input #1, statsLK$(I, 1)
    Next

    For I = 0 To 2: 
        Input #1, statsPK!(I)
        Input #1, PKR$(I, 0)
        Input #1, PKR$(I, 1)
    Next

    For I = 0 To 1: 
        Input #1, statsLF!(I)
        Input #1, statsLF$(I)
        For I1 = 0 To 4: 
            Input #1, AM!(I, I1)
            Input #1, AF!(I, I1)
        Next
    Next

    For I = 0 To 9: 
        Input #1, statsLI!(I)
        Input #1, statsLI$(I, 0)
        Input #1, statsLI$(I, 1)
    Next

    Close #1

    For I = 1 To 10: 
        For J = 1 To 6: 
            compRB!(I, J) = compZ2!(I, J)
        Next
    Next

    For I = 1 To 6: 
        For J = 1 To 3: 
            compWR!(I, J) = compZ2!(I, J + 6)
        Next
    Next

    For I = 1 To 4: 
        For J = 1 To 5: 
            compQB!(I, J) = compZ2!(I, J + 9)
        Next
    Next

    For I = 1 To 3: 
        For J = 1 To 3: 
            KR!(I, J) = compZ2!(I, J + 14)
        Next
    Next

    For I = 1 To 3: 
        For J = 1 To 3: 
            compPR!(I, J) = compZ2!(I + 3, J + 14)
        Next
    Next

    TG!(8) = OT!(7)
    TG!(9) = OT!(8)
    FM!(1) = compZ2!(4, 18)
    FA!(1) = compZ2!(5, 18)
    EM!(1) = compZ2!(6, 18)
    EA!(1) = compZ2!(7, 18)
    FM!(2) = compZ2!(8, 18)
    FA!(2) = compZ2!(9, 18)
    EM!(2) = compZ2!(10, 18)
    EA!(2) = compZ2!(11, 18)

    For I = 1 To 10:
        compRB!(I, 7) = compRB!(I, 2) + compRB!(I, 5)
        compRB!(I, 8) = compRB!(I, 3) + compRB!(I, 6)
        compWR$(I + 6) = compRB$(I)
        compWR!(I + 6, 2) = compRB!(I, 5)
        compWR!(I + 6, 1) = compRB!(I, 4)
        compWR!(I + 6, 3) = compRB!(I, 6)
    Next

    For I = 1 To 10: 
        TB$(I) = compRB$(I)
        YD!(I) = compRB!(I, 7)
        TD!(I) = compRB!(I, 8)
    Next
    
    For I = 1 To 6: 
        TB$(I + 10) = compWR$(I)
        YD!(I + 10) = compWR!(I, 2)
        TD!(I + 10) = compWR!(I, 3)
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If compKR$(I1) = TB$(I) Then YD!(I) = YD!(I) + KR!(I1, 2): TD!(I) = TD!(I) + KR!(I1, 3)
        Next
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If compPR$(I1) = TB$(I) Then YD!(I) = YD!(I) + compPR!(I1, 2): TD!(I) = TD!(I) + compPR!(I1, 3)
        Next
    Next

    For I = 1 To 16: 
        TP!(I) = TD!(I) * 6
    Next

    For I = 1 To 2: 
        TB$(16 + I) = compPK$(I)
        TP!(16 + I) = EM!(I) + FM!(I) * 3
    Next

    For I = 1 To 10
        compRB!(I, 9) = 0
        If compRB!(I, 1) > 0 Then compRB!(I, 9) = Int(compRB!(I, 2) / compRB!(I, 1) * 10) / 10
    Next

    For I = 1 To 10
        compRB!(I, 10) = 0
        If compRB!(I, 4) > 0 Then compRB!(I, 10) = Int(compRB!(I, 5) / compRB!(I, 4) * 10) / 10
    Next

    For I = 1 To 16
        compWR!(I, 4) = 0
        If compWR!(I, 1) > 0 Then compWR!(I, 4) = Int(compWR!(I, 2) / compWR!(I, 1) * 10) / 10
    Next

    For I = 1 To 4
        compQB!(I, 6) = 0
        Call QBRatingColl(I)
    Next

    For I = 1 To 3:
        KR!(I, 4) = 0
        If KR!(I, 1) > 0 And KR!(I, 1) >= numberGames Then
            KR!(I, 4) = Int(KR!(I, 2) / KR!(I, 1) * 10) / 10
        Else
            KR!(I, 4) = 0
        End If

    Next

    For I = 1 To 3:
        compPR!(I, 4) = 0
        If compPR!(I, 1) > 0 And compPR!(I, 1) >= numberGames Then
            compPR!(I, 4) = Int(compPR!(I, 2) / compPR!(I, 1) * 10) / 10
        Else
            compPR!(I, 4) = 0
        End If
    Next

    puntNum = 0

    If TG!(8) > 0 Then puntNum = Int(TG!(9) / TG!(8) * 10) / 10

    Erase FG!, EP!

    For I = 1 To 2
        If FA!(I) >= numberGames Then FG!(I) = Int(FM!(I) / FA!(I) * 1000) / 10
        If EA!(I) >= numberGames Then EP!(I) = Int(EM!(I) / EA!(I) * 1000) / 10
    Next

End Sub


'----------------------------------------
'       CheckStatsNatl Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsNatl (numberGames, TM$, WT$, finalUpdate$)

    Shared fumbGain, puntNum
    Shared puntName$

    Shared indRecDesc$()
    Shared compKR$(), compPK$(), compPR$(), compQB$()
    Shared compRB$(), compWR$(), compZ2$(), compZ3$()
    Shared intName$(), PKR$(), PS$(), sackName$()
    Shared statsLC$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), statsLR$()
    Shared TB$()

    Shared indRecords!(), compPR!(), compQB!(), compRB!(), compZ1!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsPK!()
    Shared TG!(), TP!(), compWR!(), YD!()

    Shared statsGI%(), statsGS%()

    '---------------
    ' Running Backs
    '---------------
    For X = 1 To 10

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compRB$(X)

        If compRB$(X) <> "OTHERS" Then

            If compRB!(X, 1) > indRecords!(0) Then indRecords!(0) = compRB!(X, 1): indRecDesc$(0, 0) = compRB$(X): indRecDesc$(0, 1) = TM$
            If compRB!(X, 2) > indRecords!(1) Then indRecords!(1) = compRB!(X, 2): indRecDesc$(1, 0) = compRB$(X): indRecDesc$(1, 1) = TM$
            If compRB!(X, 3) > indRecords!(2) Then indRecords!(2) = compRB!(X, 3): indRecDesc$(2, 0) = compRB$(X): indRecDesc$(2, 1) = TM$

            If compRB!(X, 9) > indRecords!(3) And UCase$(finalUpdate$) = "Y" And compRB!(X, 1) >= numberGames * 10 Then
                'Rushing Average
                indRecords!(3) = compRB!(X, 9)
                indRecDesc$(3, 0) = compRB$(X)
                indRecDesc$(3, 1) = TM$
            End If

            If statsLR!(X - 1) > indRecords!(4) Then
                'Longest Run
                indRecords!(4) = statsLR!(X - 1)
                indRecDesc$(4, 0) = compRB$(X)
                indRecDesc$(4, 1) = TM$
                indRecDesc$(4, 2) = statsLR$(X - 1, 0)
            End If

            PS$(X) = WT$

            For Y = 1 To 20:

                If compZ2$(Y) = compRB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = compRB!(X, 9) 'Rush Avg
                    compZ1!(Y, 2) = compRB!(X, 1) 'Rush Att

                    'Only include players with 10 ATT / game
                    If compZ1!(Y, 2) < numberGames * 10 Then
                        compZ2$(Y) = "*"
                        compZ3$(Y) = "*"
                        compZ1!(Y, 1) = 0
                        compZ1!(Y, 2) = 0
                    End If

                    For Y = 1 To 19:

                        For Z = Y + 1 To 20:

                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                Swap compZ3$(Y), compZ3$(Z)
                                Swap compZ2$(Y), compZ2$(Z)

                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1

                            End If

                        Next Z

                    Next Y

                    GoTo 3118
                End If

            Next Y

            If Not (compRB!(X, 1) < numberGames * 10 Or compRB!(X, 9) < compZ1!(20, 1)) Then

                compZ2$(20) = compRB$(X)
                compZ3$(20) = WT$
                compZ1!(20, 1) = compRB!(X, 9)
                compZ1!(20, 2) = compRB!(X, 1)

                For Y = 1 To 19:

                    For Z = Y + 1 To 20:

                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)
                            For Z1 = 1 To 2
                                Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                            Next Z1
                        End If

                    Next Z

                Next Y

            End If

            3118 '
            For Z = 41 To 50
                If compZ2$(Z) = compRB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(compRB!(X, 2))
                    GoTo 3123
                End If
            Next Z

            If Int(compRB!(X, 2)) > compZ1!(50, 1) Then

                compZ1!(50, 1) = Int(compRB!(X, 2))
                compZ2$(50) = compRB$(X)
                compZ3$(50) = WT$

                3123 '
                For Z = 41 To 49:
                    For Z1 = Z + 1 To 50:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 61 To 70
                If compZ2$(Z) = compRB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compRB!(X, 3)
                    GoTo 3155
                End If
            Next Z

            If compRB!(X, 3) > compZ1!(70, 1) Then

                compZ1!(70, 1) = compRB!(X, 3)
                compZ2$(70) = compRB$(X)
                compZ3$(70) = WT$

                3155 '
                For Z = 61 To 69:
                    For Z1 = Z + 1 To 70:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 81 To 90
                If compZ2$(Z) = compRB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compRB!(X, 1)
                    GoTo 3525
                End If
            Next Z

            If compRB!(X, 1) > compZ1!(90, 1) Then

                compZ1!(90, 1) = compRB!(X, 1)
                compZ2$(90) = compRB$(X)
                compZ3$(90) = WT$

                3525 '
                For Z = 81 To 89:
                    For Z1 = Z + 1 To 90:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' Receivers
    '---------------
    For X = 1 To 16:

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print compWR$(X)

        If compWR$(X) <> "OTHERS" Then

            If compWR!(X, 1) > indRecords!(5) Then indRecords!(5) = compWR!(X, 1): indRecDesc$(5, 0) = compWR$(X): indRecDesc$(5, 1) = TM$
            If compWR!(X, 2) > indRecords!(6) Then indRecords!(6) = compWR!(X, 2): indRecDesc$(6, 0) = compWR$(X): indRecDesc$(6, 1) = TM$
            If compWR!(X, 3) > indRecords!(7) Then indRecords!(7) = compWR!(X, 3): indRecDesc$(7, 0) = compWR$(X): indRecDesc$(7, 1) = TM$

            'Look for at least 3 catches / game
            If compWR!(X, 4) > indRecords!(8) And UCase$(finalUpdate$) = "Y" And compWR!(X, 1) >= numberGames * 3 Then
                'Receiving Average
                indRecords!(8) = compWR!(X, 4)
                indRecDesc$(8, 0) = compWR$(X)
                indRecDesc$(8, 1) = TM$
            End If

            If statsLC!(X - 1) > indRecords!(9) Then
                'Longest Catch
                indRecords!(9) = statsLC!(X - 1)
                indRecDesc$(9, 0) = compWR$(X)
                indRecDesc$(9, 1) = TM$
                indRecDesc$(9, 2) = statsLC$(X - 1, 0)
            End If

            For Z = 21 To 40:

                If compZ2$(Z) = compWR$(X) And compZ3$(Z) = WT$ Then

                    compZ1!(Z, 1) = compWR!(X, 4)
                    compZ1!(Z, 2) = compWR!(X, 1)

                    'Only include players with 4 RECs / game
                    If compZ1!(Z, 2) < numberGames * 4 Then

                        compZ2$(Z) = "*"
                        compZ3$(Z) = "*"
                        compZ1!(Z, 1) = 0
                        compZ1!(Z, 2) = 0
                    End If

                    For Y = 21 To 39:
                        For Z = Y + 1 To 40:
                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                Swap compZ3$(Y), compZ3$(Z)
                                Swap compZ2$(Y), compZ2$(Z)
                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1
                            End If

                        Next Z

                    Next Y

                    GoTo 3178

                End If

            Next Z

            If Not ((compWR!(X, 1) < numberGames * 4 Or compWR!(X, 4) < compZ1!(40, 1))) Then

                compZ2$(40) = compWR$(X)
                compZ3$(40) = WT$
                compZ1!(40, 1) = compWR!(X, 4)
                compZ1!(40, 2) = compWR!(X, 1)

                For Y = 21 To 39:
                    For Z = Y + 1 To 40:
                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then

                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)
                            For Z1 = 1 To 2
                                Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                            Next Z1

                        End If

                    Next Z

                Next Y

            End If

            3178 '
            For Z = 51 To 60
                If compZ2$(Z) = compWR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(compWR!(X, 2))
                    GoTo 3183
                End If
            Next Z

            If Int(compWR!(X, 2)) > compZ1!(60, 1) Then
                compZ1!(60, 1) = Int(compWR!(X, 2))
                compZ2$(60) = compWR$(X)
                compZ3$(60) = WT$

                3183 '
                For Z = 51 To 59:
                    For Z1 = Z + 1 To 60:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z
            End If

            For Z = 91 To 100
                If compZ2$(Z) = compWR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compWR!(X, 1)
                    GoTo 3215
                End If
            Next Z

            If compWR!(X, 1) > compZ1!(100, 1) Then

                compZ1!(100, 1) = compWR!(X, 1)
                compZ2$(100) = compWR$(X)
                compZ3$(100) = WT$

                3215 '
                For Z = 91 To 99:
                    For Z1 = Z + 1 To 100:
                        If compZ1!(Z, 1) <= compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 71 To 80
                If compZ2$(Z) = compWR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compWR!(X, 3)
                    GoTo 3230
                End If
            Next Z

            If compWR!(X, 3) > compZ1!(80, 1) Then

                compZ1!(80, 1) = compWR!(X, 3)
                compZ2$(80) = compWR$(X)
                compZ3$(80) = WT$

                3230 '
                For Z = 71 To 79:
                    For Z1 = Z + 1 To 80:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    '---------------
    ' All Purpose Yds
    '---------------
    For X = 1 To 16:

        'Color 14
        Print "CHECKING STATS OF ";
        Color 15
        Print TB$(X)

        If TB$(X) <> "OTHERS" Then

            If YD!(X) > indRecords!(18) Then indRecords!(18) = YD!(X): indRecDesc$(18, 0) = TB$(X): indRecDesc$(18, 1) = TM$
            If TP!(X) > indRecords!(19) Then indRecords!(19) = TP!(X): indRecDesc$(19, 0) = TB$(X): indRecDesc$(19, 1) = TM$
            If TP!(X) > indRecords!(21) Then indRecords!(21) = TP!(X): indRecDesc$(21, 0) = TB$(X): indRecDesc$(21, 1) = TM$

            For Z = 101 To 110
                If compZ2$(Z) = TB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = YD!(X)
                    GoTo 3245
                End If
            Next Z

            If YD!(X) > compZ1!(110, 1) Then

                compZ1!(110, 1) = YD!(X)
                compZ2$(110) = TB$(X)
                compZ3$(110) = WT$

                3245 '
                For Z = 101 To 109:
                    For Z1 = Z + 1 To 110:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 111 To 120
                If compZ2$(Z) = TB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = TP!(X)
                    GoTo 3260
                End If
            Next Z

            If TP!(X) > compZ1!(120, 1) Then

                compZ1!(120, 1) = TP!(X)
                compZ2$(120) = TB$(X)
                compZ3$(120) = WT$

                3260 '
                For Z = 111 To 119:
                    For Z1 = Z + 1 To 120:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

        End If

    Next X

    For X = 17 To 18

        If TP!(X) > indRecords!(20) Then
            indRecords!(20) = TP!(X)
            indRecDesc$(20, 0) = TB$(X)
            indRecDesc$(20, 1) = TM$
        End If

        If TP!(X) > indRecords!(21) Then
            indRecords!(21) = TP!(X)
            indRecDesc$(21, 0) = TB$(X)
            indRecDesc$(21, 1) = TM$
        End If

        For Z = 191 To 200
            If compZ2$(Z) = TB$(X) And compZ3$(Z) = WT$ Then
                compZ1!(Z, 1) = TP!(X)
                GoTo 3505
            End If
        Next Z

        If TP!(X) > compZ1!(200, 1) Then

            compZ1!(200, 1) = TP!(X)
            compZ2$(200) = TB$(X)
            compZ3$(200) = WT$

            3505 '
            For Z = 191 To 199:
                For Z1 = Z + 1 To 200:
                    If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                        Swap compZ2$(Z1), compZ2$(Z)
                        Swap compZ3$(Z1), compZ3$(Z)
                        Swap compZ1!(Z1, 1), compZ1!(Z, 1)
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
                indRecDesc$(10, 1) = TM$
            End If

            If compQB!(X, 2) > indRecords!(11) Then
                indRecords!(11) = compQB!(X, 2)
                indRecDesc$(11, 0) = compQB$(X)
                indRecDesc$(11, 1) = TM$
            End If

            If compQB!(X, 5) > indRecords!(12) Then
                indRecords!(12) = compQB!(X, 5)
                indRecDesc$(12, 0) = compQB$(X)
                indRecDesc$(12, 1) = TM$
            End If

            If compQB!(X, 3) > indRecords!(13) Then
                indRecords!(13) = compQB!(X, 3)
                indRecDesc$(13, 0) = compQB$(X)
                indRecDesc$(13, 1) = TM$
            End If

            If compQB!(X, 4) > indRecords!(14) Then
                indRecords!(14) = compQB!(X, 4)
                indRecDesc$(14, 0) = compQB$(X)
                indRecDesc$(14, 1) = TM$
            End If

            If compQB!(X, 6) > indRecords!(15) And UCase$(finalUpdate$) = "Y" And compQB!(X, 1) >= numberGames * 15 Then
                indRecords!(15) = compQB!(X, 6)
                indRecDesc$(15, 0) = compQB$(X)
                indRecDesc$(15, 1) = TM$
            End If

            If compQB!(X, 7) > indRecords!(16) And UCase$(finalUpdate$) = "Y" And compQB!(X, 1) >= numberGames * 15 Then
                indRecords!(16) = compQB!(X, 7)
                indRecDesc$(16, 0) = compQB$(X)
                indRecDesc$(16, 1) = TM$
            End If

            If statsLP!(X - 1) > indRecords!(17) Then
                indRecords!(17) = statsLP!(X - 1)
                indRecDesc$(17, 0) = compQB$(X)
                indRecDesc$(17, 1) = TM$
                indRecDesc$(17, 2) = statsLP$(X - 1, 0)
            End If

            For Y = 121 To 130:
            
                If compZ2$(Y) = compQB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = compQB!(X, 6)
                    compZ1!(Y, 2) = compQB!(X, 1)

                    If compZ1!(Y, 2) < numberGames * 15 Then
                        compZ2$(Y) = "*"
                        compZ3$(Y) = "*"
                        compZ1!(Y, 1) = 0
                        compZ1!(Y, 2) = 0
                    End If

                    For Y = 121 To 129:
                        For Z = Y + 1 To 130:
                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                Swap compZ3$(Y), compZ3$(Z)
                                Swap compZ2$(Y), compZ2$(Z)
                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1

                            End If
                        Next Z
                    Next Y

                    GoTo 3279

                End If

            Next Y

            If Not (compQB!(X, 1) < numberGames * 15 Or compQB!(X, 6) < compZ1!(130, 1)) Then

                compZ2$(130) = compQB$(X)
                compZ3$(130) = WT$
                compZ1!(130, 1) = compQB!(X, 6)
                compZ1!(130, 2) = compQB!(X, 1)

                For Y = 121 To 129:

                    For Z = Y + 1 To 130:

                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)

                            For Z1 = 1 To 2
                                Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                            Next Z1

                        End If

                    Next Z

                Next Y

            End If

            3279 '
            For Y = 131 To 140:

                If compZ2$(Y) = compQB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = compQB!(X, 7)
                    compZ1!(Y, 2) = compQB!(X, 1)

                    If compZ1!(Y, 2) < numberGames * 15 Then
                        compZ2$(Y) = "*"
                        compZ3$(Y) = "*"
                        compZ1!(Y, 1) = 0
                        compZ1!(Y, 2) = 0
                    End If

                    For Y = 131 To 139:
                        For Z = Y + 1 To 140:

                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then

                                Swap compZ3$(Y), compZ3$(Z)
                                Swap compZ2$(Y), compZ2$(Z)

                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1
                            End If

                        Next Z
                    Next Y

                    GoTo 3287

                End If

            Next Y

            If Not (compQB!(X, 1) < numberGames * 15 Or compQB!(X, 6) < compZ1!(140, 1)) Then

                compZ2$(140) = compQB$(X)
                compZ3$(140) = WT$
                compZ1!(140, 1) = compQB!(X, 7)
                compZ1!(140, 2) = compQB!(X, 1)

                For Y = 131 To 139:
                    For Z = Y + 1 To 140:

                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)
                            For Z1 = 1 To 2
                                Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                            Next Z1
                        End If

                    Next Z
                Next Y

            End If

            3287 '
            For Z = 141 To 150
                If compZ2$(Z) = compQB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(compQB!(X, 4))
                    GoTo 3294
                End If
            Next Z

            If Int(compQB!(X, 4)) > compZ1!(150, 1) Then

                compZ1!(150, 1) = Int(compQB!(X, 4))
                compZ2$(150) = compQB$(X)

                3294 '
                compZ3$(150) = WT$

                For Z = 141 To 149:
                    For Z1 = Z + 1 To 150:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1
                Next Z

            End If

            For Z = 151 To 160
                If compZ2$(Z) = compQB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compQB!(X, 5)
                    GoTo 3307
                End If
            Next Z

            If compQB!(X, 5) > compZ1!(160, 1) Then

                compZ1!(160, 1) = compQB!(X, 5)
                compZ2$(160) = compQB$(X)
                compZ3$(160) = WT$

                3307 '
                For Z = 151 To 159:
                    For Z1 = Z + 1 To 160:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
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

            If KR!(X, 1) > indRecords!(22) Then
                indRecords!(22) = KR!(X, 1)
                indRecDesc$(22, 0) = compKR$(X)
                indRecDesc$(22, 1) = TM$
            End If

            If KR!(X, 2) > indRecords!(23) Then
                indRecords!(23) = KR!(X, 2)
                indRecDesc$(23, 0) = compKR$(X)
                indRecDesc$(23, 1) = TM$
            End If

            If KR!(X, 3) > indRecords!(24) Then
                indRecords!(24) = KR!(X, 3)
                indRecDesc$(24, 0) = compKR$(X)
                indRecDesc$(24, 1) = TM$
            End If

            If KR!(X, 4) > indRecords!(25) And UCase$(finalUpdate$) = "Y" And KR!(X, 1) >= numberGames * 1.2 Then
                indRecords!(25) = KR!(X, 4)
                indRecDesc$(25, 0) = compKR$(X)
                indRecDesc$(25, 1) = TM$
            End If

            If statsLK!(X - 1) > indRecords!(26) Then
                indRecords!(26) = statsLK!(X - 1)
                indRecDesc$(26, 0) = compKR$(X)
                indRecDesc$(26, 1) = TM$
                indRecDesc$(26, 2) = statsLK$(X - 1, 0)
            End If

            For Z = 161 To 170
                If compZ2$(Z) = compKR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = KR!(X, 4)
                    compZ1!(Z, 2) = KR!(X, 1)
                    GoTo 3345
                End If
            Next Z

            If Not (KR!(X, 1) < numberGames * 1.2 Or KR!(X, 4) <= compZ1!(170, 1)) Then

                compZ1!(170, 1) = KR!(X, 4)
                compZ2$(170) = compKR$(X)
                compZ3$(170) = WT$
                compZ1!(170, 2) = KR!(X, 1)

                3345 '
                For Z = 161 To 169:
                    For Z1 = Z + 1 To 170:

                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            For MN = 1 To 2
                                Swap compZ1!(Z1, MN), compZ1!(Z, MN)
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
                indRecDesc$(27, 1) = TM$
            End If

            If compPR!(X, 2) > indRecords!(28) Then
                indRecords!(28) = compPR!(X, 2)
                indRecDesc$(28, 0) = compPR$(X)
                indRecDesc$(28, 1) = TM$
            End If

            If compPR!(X, 3) > indRecords!(29) Then
                indRecords!(29) = compPR!(X, 3)
                indRecDesc$(29, 0) = compPR$(X)
                indRecDesc$(29, 1) = TM$
            End If

            If compPR!(X, 4) > indRecords!(30) And UCase$(finalUpdate$) = "Y" And compPR!(X, 1) >= numberGames * 1.2 Then
                indRecords!(30) = compPR!(X, 4)
                indRecDesc$(30, 0) = compPR$(X)
                indRecDesc$(30, 1) = TM$
            End If

            If statsPK!(X - 1) > indRecords!(31) Then
                indRecords!(31) = statsPK!(X - 1)
                indRecDesc$(31, 0) = compPR$(X)
                indRecDesc$(31, 1) = TM$
                indRecDesc$(31, 2) = PKR$(X - 1, 0)
            End If

            For Z = 171 To 180
                If compZ2$(Z) = compPR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compPR!(X, 4)
                    compZ1!(Z, 2) = compPR!(X, 1)
                    GoTo 3365
                End If
            Next Z

            If Not (compPR!(X, 1) < numberGames * 1.2 And compPR!(X, 4) <= compZ1!(180, 1)) Then

                compZ1!(180, 1) = compPR!(X, 4)
                compZ2$(180) = compPR$(X)
                compZ3$(180) = WT$
                compZ1!(180, 2) = compPR!(X, 1)

                3365 '
                For Z = 171 To 179:
                    For Z1 = Z + 1 To 180:
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            For MN = 1 To 2
                                Swap compZ1!(Z1, MN), compZ1!(Z, MN)
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
    Print puntName$

    If TG!(8) > indRecords!(32) Then indRecords!(32) = TG!(8): indRecDesc$(32, 0) = puntName$: indRecDesc$(32, 1) = TM$
    If TG!(9) > indRecords!(33) Then indRecords!(33) = TG!(9): indRecDesc$(33, 0) = puntName$: indRecDesc$(33, 1) = TM$
    If puntNum > indRecords!(34) And UCase$(finalUpdate$) = "Y" Then indRecords!(34) = puntNum: indRecDesc$(34, 0) = puntName$: indRecDesc$(34, 1) = TM$

    For Z = 181 To 190
        If compZ2$(Z) = puntName$ And compZ3$(Z) = WT$ Then
            compZ1!(Z, 1) = puntNum
            compZ1!(Z, 2) = TG!(8)
            GoTo 3382
        End If
    Next Z

    If puntNum > compZ1!(190, 1) Then

        compZ1!(190, 1) = puntNum
        compZ2$(190) = puntName$
        compZ3$(190) = WT$
        compZ1!(190, 2) = TG!(8)

        3382 '
        For Z = 181 To 189:
            For Z1 = Z + 1 To 190:
                If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                    Swap compZ2$(Z1), compZ2$(Z)
                    Swap compZ3$(Z1), compZ3$(Z)
                    For MN = 1 To 2
                        Swap compZ1!(Z1, MN), compZ1!(Z, MN)
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
        Print compPK$(X)

        If FM!(X) > indRecords!(35) Then
            indRecords!(35) = FM!(X)
            indRecDesc$(35, 0) = compPK$(X)
            indRecDesc$(35, 1) = TM$
        End If

        If FA!(X) > indRecords!(36) Then
            indRecords!(36) = FA!(X)
            indRecDesc$(36, 0) = compPK$(X)
            indRecDesc$(36, 1) = TM$
        End If

        If FG!(X) > indRecords!(37) And UCase$(finalUpdate$) = "Y" Then
            indRecords!(37) = FG!(X)
            indRecDesc$(37, 0) = compPK$(X)
            indRecDesc$(37, 1) = TM$
        End If

        If statsLF!(X - 1) > indRecords!(38) Then
            indRecords!(38) = statsLF!(X - 1)
            indRecDesc$(38, 0) = compPK$(X)
            indRecDesc$(38, 1) = TM$
            indRecDesc$(38, 2) = statsLF$(X - 1)
        End If

        If EM!(X) > indRecords!(39) Then
            indRecords!(39) = EM!(X)
            indRecDesc$(39, 0) = compPK$(X)
            indRecDesc$(39, 1) = TM$
        End If

        If EA!(X) > indRecords!(40) Then
            indRecords!(40) = EA!(X)
            indRecDesc$(40, 0) = compPK$(X)
            indRecDesc$(40, 1) = TM$
        End If

        If EP!(X) > indRecords!(41) And UCase$(finalUpdate$) = "Y" Then
            indRecords!(41) = EP!(X)
            indRecDesc$(41, 0) = compPK$(X)
            indRecDesc$(41, 1) = TM$
        End If

        For Z = 201 To 210
            If compZ2$(Z) = compPK$(X) And compZ3$(Z) = WT$ Then
                compZ1!(Z, 1) = fumbGain
                compZ1!(Z, 2) = FA!(X)
                GoTo 3396
            End If
        Next Z

        If FG!(X) > compZ1!(210, 1) Then

            compZ1!(210, 1) = FG!(X)
            compZ2$(210) = compPK$(X)
            compZ3$(210) = WT$
            compZ1!(210, 2) = FA!(X)

            3396 '
            For Z = 201 To 209:
                For Z1 = Z + 1 To 210:
                    If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                        Swap compZ2$(Z1), compZ2$(Z)
                        Swap compZ3$(Z1), compZ3$(Z)

                        For MN = 1 To 2
                            Swap compZ1!(Z1, MN), compZ1!(Z, MN)
                        Next MN
                    End If
                Next Z1
            Next Z

        End If

        For Z = 211 To 220
            If compZ2$(Z) = compPK$(X) And compZ3$(Z) = WT$ Then
                compZ1!(Z, 1) = EP!(X)
                compZ1!(Z, 2) = EA!(X)
                GoTo 3409
            End If
        Next Z

        If EP!(X) > compZ1!(220, 1) Then

            compZ1!(220, 1) = EP!(X)
            compZ2$(220) = compPK$(X)
            compZ3$(220) = WT$
            compZ1!(220, 2) = EA!(X)

            3409 '
            For Z = 211 To 219:
                For Z1 = Z + 1 To 220:

                    If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                        Swap compZ2$(Z1), compZ2$(Z)
                        Swap compZ3$(Z1), compZ3$(Z)
                        For MN = 1 To 2
                            Swap compZ1!(Z1, MN), compZ1!(Z, MN)
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

            If statsGI%(X, 0) > indRecords!(42) Then indRecords!(42) = statsGI%(X, 0): indRecDesc$(42, 0) = intName$(X): indRecDesc$(42, 1) = TM$
            If statsGI%(X, 1) > indRecords!(43) Then indRecords!(43) = statsGI%(X, 1): indRecDesc$(43, 0) = intName$(X): indRecDesc$(43, 1) = TM$
            If statsGI%(X, 2) > indRecords!(44) Then indRecords!(44) = statsGI%(X, 2): indRecDesc$(44, 0) = intName$(X): indRecDesc$(44, 1) = TM$

            If X > 0 Then
                If statsLI!(X - 1) > indRecords!(45) Then
                    indRecords!(45) = statsLI!(X - 1)
                    indRecDesc$(45, 0) = intName$(X)
                    indRecDesc$(45, 1) = TM$
                    indRecDesc$(45, 2) = statsLI$(X - 1, 0)
                End If
            End If

            For Z = 221 To 230
                If compZ2$(Z) = intName$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = statsGI%(X, 0)
                    GoTo 3422
                End If
            Next Z

            If statsGI%(X, 0) > compZ1!(230, 1) Then

                compZ1!(230, 1) = statsGI%(X, 0)
                compZ2$(230) = intName$(X)
                compZ3$(230) = WT$

                3422 '
                For Z = 221 To 229:
                    For Z1 = Z + 1 To 230:

                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If

                    Next Z1

                Next Z

            End If

        End If

    Next X

    '-----

    For X = 0 To 14

        If Left$(sackName$(X), 1) <> " " And sackName$(X) <> "" And sackName$(X) <> "END" Then

            If statsGS%(X, 0) > indRecords!(46) Then
                indRecords!(46) = statsGS%(X, 0)
                indRecDesc$(46, 0) = sackName$(X)
                indRecDesc$(46, 1) = TM$
            End If

            For Z = 231 To 240
                If compZ2$(Z) = sackName$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = statsGS%(X, 0)
                    GoTo 3434
                End If
            Next Z

            If statsGS%(X, 0) > compZ1!(240, 1) Then

                compZ1!(240, 1) = statsGS%(X, 0)
                compZ2$(240) = sackName$(X)
                compZ3$(240) = WT$

                3434 '
                For Z = 231 To 239:
                    For Z1 = Z + 1 To 240
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
                            Swap compZ2$(Z1), compZ2$(Z)
                            Swap compZ3$(Z1), compZ3$(Z)
                            Swap compZ1!(Z1, 1), compZ1!(Z, 1)
                        End If
                    Next Z1

                Next Z

            End If

        End If

    Next X

End Sub


'----------------------------------------
'     StatsByCategoryNatl Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded 
'stats to each of the record categories to 
'see if any should be updated
Sub StatsByCategoryNatl (numberGames, TM$, WT$, finalUpdate$)

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
    End if

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
'       SortLeadersNat Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target organization
Sub SortLeadersNat ()

    Shared compTT!(), compZ1!()
    Shared N1!(), NT!()

    Shared compZ1$(), compZ2$(), compZ3$()
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
Sub PostLeadersOrg (leaderFile$)

    Shared compZ1$(), N2$(), N3$(), NT$()

    Shared N1!(), NT!()
    Shared O1!(), O2!(), O3!(), PTSA!(), PTSF!()

    Cls

    Open diskPaths$(2) + leaderFile$ + ".NLD" For Output As #1

    Print "POSTING "; leaderFile$; " LEADERS"

    For X = 1 To 60:
        Write #1, compZ1$(X), O1!(X), O2!(X), O3!(X), PTSF!(X), PTSA!(X)
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
