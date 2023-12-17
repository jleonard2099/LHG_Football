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

    Shared BRC!()
    Shared compTT!(), compZ1!(), compZ2!()
    Shared O1!(), O2!(), O3!()
    Shared PTSF!(), PTSA!()
    Shared TRC!()

    Shared BRC$()
    Shared compZ1$(), compZ2$(), compZ3$()
    Shared H1$(), TRC$(), TT$()

    Shared div1$, div2$, div3$

    'Read in any existing records first
    If _FileExists(diskPaths$(1) + "RECORDS.PFB") Then

        Open diskPaths$(1) + "RECORDS.PFB" For Input As #1

        For I = 0 To 50:
            Input #1, BRC!(I)
            Input #1, BRC$(I, 0)
            Input #1, BRC$(I, 1)
            Input #1, BRC$(I, 2)
        Next

        For I = 0 To 50:
            Input #1, TRC!(I)
            Input #1, TRC$(I)
        Next

        Close #1
    Else

        Erase BRC!, TRC!
        Erase BRC$, TRC$

    End If

    'Initialize values first
    'These are defensive record values, so they should be really high
    'To determine if a "record" was broken, the stat values will be
    'compared to see if they are lower than this.
    'If we initialized to 0, the "record" would always be 0

    TRC!(11) = 999
    TRC!(22) = 999

    For I = 25 To 27:
        TRC!(I) = 9999
    Next

    For I = 29 To 36:
        TRC!(I) = 9999
    Next

    TRC!(38) = 999
    TRC!(39) = 999
    TRC!(42) = 999
    TRC!(43) = 999

    Erase compTT!, compZ1!, compZ2!
    Erase compZ1$, compZ3$, TT$

    For X = 1 To 240
        compZ2$(X) = "*"
    Next

    For I = 1 To 20
        compTT!(2, I, 5) = 9999
        compTT!(4, I, 5) = 9999
        compTT!(6, I, 3) = 9999
        compTT!(8, I, 8) = 9999
        compTT!(10, I, 7) = -99
    Next

    C$ = ","

    leagueFile$ = ChooseFile$(diskPaths$(2) + "*.LIB", "LEAGUE", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        finalUpdate$ = "N"

        Do
            Color 15, 0
            Cls
            Print "WOULD YOU LIKE TO POST SEASON LEADERS? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #2

        Input #2, FF$, div1$, div2$, div3$

        For XW = 1 To 30
            Input #2, H1$(XW)
        Next XW

        Close #2

        Open diskPaths$(2) + leagueFile$ + ".LIB" For Input As #3

        Input #3, LG%

        Close #3

        If LG% <> 1 Then

            MsgText$ = "ERROR!! THIS IS NOT A PRO LEAGUE. PRESS ANY KEY TO RETURN TO MAIN MENU"
            BoxWidth = 20
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #4

            Input #4, FF$, div1$, div2$, div3$

            For X = 1 To 30
                Input #4, compZ1$(X)
            Next X

            Close #4

            For XW = 1 To 30

                If H1$(XW) <> Chr$(42) Then

                    haveStats = FindStatsPro%(XW, H1$(XW), numberGames%)

                    If (haveStats) Then

                        L = Len(H1$(XW))
                        WT$ = ""

                        For X = 1 To L:
                            A = Asc(Mid$(H1$(XW), X, 1))
                            WT$ = WT$ + Chr$(A)
                        Next X

                        confGame = 0

                        For EC = 1 To 30:

                            If compZ1$(EC) = WT$ Then

                                If O1!(EC) = fullWins And O2!(EC) = fullLosses And O3!(EC) = fullTies Then
                                    confGame = 1
                                Else
                                    O1!(EC) = fullWins
                                    O2!(EC) = fullLosses
                                    O3!(EC) = fullTies
                                    'Why don't we do PTSF / PTSA ?!?!
                                End If

                                If (confGame) Then Exit For

                            End If
                            'alternatively, use a Do / Until loop; confGame = 1 or EC = 30

                        Next EC

                        If confGame <> 1 And _FileExists(diskPaths$(1) + H1$(XW)) Then

                            Call ReadStatsPro(H1$(XW), numberGames%, TM$)
                            Call StatsByCategoryPro(numberGames%, TM$, WT$, finalUpdate$)
                            Call CheckStatsPro(numberGames%, TM$, WT$, finalUpdate$)

                        End If

                    End If

                End If

            Next XW

            Call SortLeadersPro
            Call PostLeadersPro(leagueFile$)

        End If 'Done checking file type

    End If

End Sub

'----------------------------------------
'         FindStatsPro Function
'----------------------------------------
'Looks for schedule stats for a specified
'team in the league and loads them if
'found, notifying the user
Function FindStatsPro% (XW, statFile$, numberGames%)

    Shared powerRating()
    Shared H1$(), locIndicator$(), oppName$()

    Shared statsGI%(), statsGS%()
    Shared oppScore(), teamScore()

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Locate , 1
        Print "NO STATS FOUND FOR "; statFile$
        FindStatsPro% = 0

    Else

        Call ReadScheduleStats(statFile$, XW, numberGames%)

        fullWins = 0
        fullLosses = 0
        fullTies = 0

        For I = 1 To numberGames%
            If teamScore(I) > oppScore(I) Then fullWins = fullWins + 1
            If teamScore(I) < oppScore(I) Then fullLosses = fullLosses + 1
            If teamScore(I) = oppScore(I) Then fullTies = fullTies + 1
        Next

        Cls

        Color 15
        Print "FINDING ";
        Color 14
        Print statFile$;
        Color 15
        Print " STATS            "

        FindStatsPro% = 1

    End If

End Function


'----------------------------------------
'       ReadStatsPro Subroutine
'----------------------------------------
'This subroutine reads stats in for a
'single team in the league
Sub ReadStatsPro (statFile$, numberGames%, TM$)

    Shared puntNum
    Shared puntName$

    Shared statsGI%(), statsGS%()

    Shared AM!(), AF!()
    Shared compPR!(), compQB!(), compRB!(), compWR!(), compZ2!()
    Shared DT!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared OT!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsPK!()
    Shared TD!(), TG!(), TP!(), YD!()

    Shared compKR$(), compPK$(), compPR$()
    Shared compQB$(), compRB$(), compWR$()
    Shared H1$(), intName$(), sackName$()
    Shared statsLC$(), statsLF$(), statsLI$()
    Shared statsLK$(), statsLP$(), statsLR$()
    Shared PKR$()
    Shared TB$()

    'Erase compKR$, compPK$, compPR$, compQB$, compRB$, compWR$
    'Erase intName$

    'Erase AF!, AM!
    'Erase compZ2!, OT!, DT!
    'Erase statsGI%, statsGS%

    'Erase statsLC!, statsLF!, statsLI!, statsLK!, statsLP!, statsLR!
    'Erase statsLC$, statsLF$, statsLI$, statsLK$, statsLP$, statsLR$

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, TM$

    'Positions
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

    'Punter
    Input #1, puntName$

    'Kickers
    For I = 1 To 2:
        Input #1, compPK$(I)
    Next

    'Rushers / Receivers
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
        For I1 = 0 To 4
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

    For I = 1 To 10
        TB$(I) = compRB$(I)
        YD!(I) = compRB!(I, 7)
        TD!(I) = compRB!(I, 8)
    Next

    For I = 1 To 6
        TB$(I + 10) = compWR$(I)
        YD!(I + 10) = compWR!(I, 2)
        TD!(I + 10) = compWR!(I, 3)
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If compKR$(I1) = TB$(I) Then
                YD!(I) = YD!(I) + KR!(I1, 2)
                TD!(I) = TD!(I) + KR!(I1, 3)
            End If
        Next
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If compPR$(I1) = TB$(I) Then
                YD!(I) = YD!(I) + compPR!(I1, 2)
                TD!(I) = TD!(I) + compPR!(I1, 3)
            End If
        Next
    Next

    For I = 1 To 16
        TP!(I) = TD!(I) * 6
    Next

    For I = 1 To 2
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

    For I = 1 To 16:
        compWR!(I, 4) = 0
        If compWR!(I, 1) > 0 Then compWR!(I, 4) = Int(compWR!(I, 2) / compWR!(I, 1) * 10) / 10
    Next

    For I = 1 To 4:
        compQB!(I, 6) = 0
        Call QBRatingPro(I)
    Next

    For I = 1 To 3:
        KR!(I, 4) = 0
        If KR!(I, 1) > 0 And KR!(I, 1) >= numberGames% Then
            KR!(I, 4) = Int(KR!(I, 2) / KR!(I, 1) * 10) / 10
        Else
            KR!(I, 4) = 0
        End If

    Next

    For I = 1 To 3:
        compPR!(I, 4) = 0
        If compPR!(I, 1) > 0 And compPR!(I, 1) >= numberGames% Then
            compPR!(I, 4) = Int(compPR!(I, 2) / compPR!(I, 1) * 10) / 10
        Else
            compPR!(I, 4) = 0
        End If
    Next

    puntNum = 0

    If TG!(8) > 0 Then puntNum = Int(TG!(9) / TG!(8) * 10) / 10

    Erase EP!, FG!

    For I = 1 To 2
        If FA!(I) >= numberGames% Then FG!(I) = Int(FM!(I) / FA!(I) * 1000) / 10
        If EA!(I) >= numberGames% Then EP!(I) = Int(EM!(I) / EA!(I) * 1000) / 10
    Next

End Sub


'----------------------------------------
'       CheckStatsPro Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsPro (numberGames%, TM$, WT$, finalUpdate$)

    Shared fumbGain, puntNum
    Shared puntName$

    Shared BRC$()
    Shared compKR$(), compPK$(), compPR$(), compQB$()
    Shared compRB$(), compWR$(), compZ2$(), compZ3$()
    Shared intName$(), PKR$(), PS$()
    Shared sackName$()
    Shared statsLC$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), statsLR$()
    Shared TB$()

    Shared BRC!()
    Shared compPR!(), compQB!(), compRB!(), compWR!(), compZ1!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!()
    Shared statsPK!()
    Shared TG!(), TP!(), YD!()

    Shared statsGI%(), statsGS%()

    '---------------
    ' Running Backs
    '---------------
    For X = 1 To 10

        Print "   CHECKING STATS OF "; compRB$(X)

        If compRB$(X) <> "OTHERS" Then

            If compRB!(X, 1) > BRC!(0) Then BRC!(0) = compRB!(X, 1): BRC$(0, 0) = compRB$(X): BRC$(0, 1) = TM$
            If compRB!(X, 2) > BRC!(1) Then BRC!(1) = compRB!(X, 2): BRC$(1, 0) = compRB$(X): BRC$(1, 1) = TM$
            If compRB!(X, 3) > BRC!(2) Then BRC!(2) = compRB!(X, 3): BRC$(2, 0) = compRB$(X): BRC$(2, 1) = TM$

            If compRB!(X, 9) > BRC!(3) And UCase$(finalUpdate$) = "Y" And compRB!(X, 1) >= numberGames% * 10 Then
                'Rushing Average
                BRC!(3) = compRB!(X, 9)
                BRC$(3, 0) = compRB$(X)
                BRC$(3, 1) = TM$
            End If

            If statsLR!(X - 1) > BRC!(4) Then
                'Longest Run
                BRC!(4) = statsLR!(X - 1)
                BRC$(4, 0) = compRB$(X)
                BRC$(4, 1) = TM$
                BRC$(4, 2) = statsLR$(X - 1, 0)
            End If

            PS$(X) = WT$

            For Y = 1 To 20:

                If compZ2$(Y) = compRB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = compRB!(X, 9) 'Rush Avg
                    compZ1!(Y, 2) = compRB!(X, 1) 'Rush Att

                    'Only include players with 10 ATT / game
                    If compZ1!(Y, 2) < numberGames% * 10 Then
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

                                For Z1 = 1 To 2:
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1

                            End If

                        Next Z

                    Next Y

                    GoTo 3118
                End If

            Next Y

            If Not (compRB!(X, 1) < numberGames% * 10 Or compRB!(X, 9) < compZ1!(20, 1)) Then

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
    For X = 1 To 16

        Print "   CHECKING STATS OF "; compWR$(X)

        If compWR$(X) <> "OTHERS" Then

            If compWR!(X, 1) > BRC!(5) Then BRC!(5) = compWR!(X, 1): BRC$(5, 0) = compWR$(X): BRC$(5, 1) = TM$
            If compWR!(X, 2) > BRC!(6) Then BRC!(6) = compWR!(X, 2): BRC$(6, 0) = compWR$(X): BRC$(6, 1) = TM$
            If compWR!(X, 3) > BRC!(7) Then BRC!(7) = compWR!(X, 3): BRC$(7, 0) = compWR$(X): BRC$(7, 1) = TM$

            'Look for at least 3 catches / game
            If compWR!(X, 4) > BRC!(8) And UCase$(finalUpdate$) = "Y" And compWR!(X, 1) >= numberGames% * 3 Then
                'Receiving Average
                BRC!(8) = compWR!(X, 4)
                BRC$(8, 0) = compWR$(X)
                BRC$(8, 1) = TM$
            End If

            If statsLC!(X - 1) > BRC!(9) Then
                'Longest Catch
                BRC!(9) = statsLC!(X - 1)
                BRC$(9, 0) = compWR$(X)
                BRC$(9, 1) = TM$
                BRC$(9, 2) = statsLC$(X - 1, 0)
            End If

            For Z = 21 To 40:

                If compZ2$(Z) = compWR$(X) And compZ3$(Z) = WT$ Then

                    compZ1!(Z, 1) = compWR!(X, 4)
                    compZ1!(Z, 2) = compWR!(X, 1)

                    'Only include players with 3 RECs / game
                    If compZ1!(Z, 2) < numberGames% * 3 Then

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
                                For Z1 = 1 To 2:
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1
                            End If

                        Next Z

                    Next Y

                    GoTo 3178

                End If

            Next Z

            If Not ((compWR!(X, 1) < numberGames% * 4 Or compWR!(X, 4) < compZ1!(40, 1))) Then

                compZ2$(40) = compWR$(X)
                compZ3$(40) = WT$
                compZ1!(40, 1) = compWR!(X, 4)
                compZ1!(40, 2) = compWR!(X, 1)

                For Y = 21 To 39:
                    For Z = Y + 1 To 40:
                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)
                            For Z1 = 1 To 2:
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
                    For Z1 = Z + 1 To 100
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
    For X = 1 To 16

        Print "   CHECKING STATS OF "; TB$(X)

        If TB$(X) <> "OTHERS" Then

            If YD!(X) > BRC!(18) Then BRC!(18) = YD!(X): BRC$(18, 0) = TB$(X): BRC$(18, 1) = TM$
            If TP!(X) > BRC!(19) Then BRC!(19) = TP!(X): BRC$(19, 0) = TB$(X): BRC$(19, 1) = TM$
            If TP!(X) > BRC!(21) Then BRC!(21) = TP!(X): BRC$(21, 0) = TB$(X): BRC$(21, 1) = TM$

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

        If TP!(X) > BRC!(20) Then
            BRC!(20) = TP!(X)
            BRC$(20, 0) = TB$(X)
            BRC$(20, 1) = TM$
        End If

        If TP!(X) > BRC!(21) Then
            BRC!(21) = TP!(X)
            BRC$(21, 0) = TB$(X)
            BRC$(21, 1) = TM$
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

        Print "   CHECKING STATS OF "; compQB$(X)

        If compQB$(X) <> "OTHERS" Then

            If compQB!(X, 1) > BRC!(10) Then
                BRC!(10) = compQB!(X, 1)
                BRC$(10, 0) = compQB$(X)
                BRC$(10, 1) = TM$
            End If

            If compQB!(X, 2) > BRC!(11) Then
                BRC!(11) = compQB!(X, 2)
                BRC$(11, 0) = compQB$(X)
                BRC$(11, 1) = TM$
            End If

            If compQB!(X, 5) > BRC!(12) Then
                BRC!(12) = compQB!(X, 5)
                BRC$(12, 0) = compQB$(X)
                BRC$(12, 1) = TM$
            End If

            If compQB!(X, 3) > BRC!(13) Then
                BRC!(13) = compQB!(X, 3)
                BRC$(13, 0) = compQB$(X)
                BRC$(13, 1) = TM$
            End If

            If compQB!(X, 4) > BRC!(14) Then
                BRC!(14) = compQB!(X, 4)
                BRC$(14, 0) = compQB$(X)
                BRC$(14, 1) = TM$
            End If

            If compQB!(X, 6) > BRC!(15) And UCase$(finalUpdate$) = "Y" And compQB!(X, 1) >= numberGames% * 15 Then
                BRC!(15) = compQB!(X, 6)
                BRC$(15, 0) = compQB$(X)
                BRC$(15, 1) = TM$
            End If

            If compQB!(X, 7) > BRC!(16) And UCase$(finalUpdate$) = "Y" And compQB!(X, 1) >= numberGames% * 15 Then
                BRC!(16) = compQB!(X, 7)
                BRC$(16, 0) = compQB$(X)
                BRC$(16, 1) = TM$
            End If

            If statsLP!(X - 1) > BRC!(17) Then
                BRC!(17) = statsLP!(X - 1)
                BRC$(17, 0) = compQB$(X)
                BRC$(17, 1) = TM$
                BRC$(17, 2) = statsLP$(X - 1, 0)
            End If

            For Y = 121 To 130:

                If compZ2$(Y) = compQB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = compQB!(X, 6)
                    compZ1!(Y, 2) = compQB!(X, 1)

                    If compZ1!(Y, 2) < numberGames% * 15 Then
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
                                For Z1 = 1 To 2:
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1

                            End If
                        Next Z
                    Next Y

                    GoTo 3279
                End If

            Next Y

            If Not (compQB!(X, 1) < numberGames% * 15 Or compQB!(X, 6) < compZ1!(130, 1)) Then

                compZ2$(130) = compQB$(X)
                compZ3$(130) = WT$
                compZ1!(130, 1) = compQB!(X, 6)
                compZ1!(130, 2) = compQB!(X, 1)

                For Y = 121 To 129:

                    For Z = Y + 1 To 130:

                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)

                            For Z1 = 1 To 2:
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

                    If compZ1!(Y, 2) < numberGames% * 15 Then
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

                                For Z1 = 1 To 2:
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1
                            End If

                        Next Z
                    Next Y

                    GoTo 3287
                End If
            Next Y

            If Not (compQB!(X, 1) < numberGames% * 15 Or compQB!(X, 6) < compZ1!(140, 1)) Then

                compZ2$(140) = compQB$(X)
                compZ3$(140) = WT$
                compZ1!(140, 1) = compQB!(X, 7)
                compZ1!(140, 2) = compQB!(X, 1)

                For Y = 131 To 139:
                    For Z = Y + 1 To 140:

                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            Swap compZ3$(Y), compZ3$(Z)
                            Swap compZ2$(Y), compZ2$(Z)
                            For Z1 = 1 To 2:
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

        Print "   CHECKING STATS OF "; compKR$(X)

        If compKR$(X) <> "OTHERS" Then

            If KR!(X, 1) > BRC!(22) Then
                BRC!(22) = KR!(X, 1)
                BRC$(22, 0) = compKR$(X)
                BRC$(22, 1) = TM$
            End If

            If KR!(X, 2) > BRC!(23) Then
                BRC!(23) = KR!(X, 2)
                BRC$(23, 0) = compKR$(X)
                BRC$(23, 1) = TM$
            End If

            If KR!(X, 3) > BRC!(24) Then
                BRC!(24) = KR!(X, 3)
                BRC$(24, 0) = compKR$(X)
                BRC$(24, 1) = TM$
            End If

            If KR!(X, 4) > BRC!(25) And UCase$(finalUpdate$) = "Y" And KR!(X, 1) >= numberGames% * 1.2 Then
                BRC!(25) = KR!(X, 4)
                BRC$(25, 0) = compKR$(X)
                BRC$(25, 1) = TM$
            End If

            If statsLK!(X - 1) > BRC!(26) Then
                BRC!(26) = statsLK!(X - 1)
                BRC$(26, 0) = compKR$(X)
                BRC$(26, 1) = TM$
                BRC$(26, 2) = statsLK$(X - 1, 0)
            End If

            For Z = 161 To 170
                If compZ2$(Z) = compKR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = KR!(X, 4)
                    compZ1!(Z, 2) = KR!(X, 1)
                    GoTo 3345
                End If
            Next Z

            If Not (KR!(X, 1) < numberGames% * 1.2 Or KR!(X, 4) <= compZ1!(170, 1)) Then

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

        Print "   CHECKING STATS OF "; compPR$(X)

        If compPR$(X) <> "OTHERS" Then

            If compPR!(X, 1) > BRC!(27) Then BRC!(27) = compPR!(X, 1): BRC$(27, 0) = compPR$(X): BRC$(27, 1) = TM$
            If compPR!(X, 2) > BRC!(28) Then BRC!(28) = compPR!(X, 2): BRC$(28, 0) = compPR$(X): BRC$(28, 1) = TM$
            If compPR!(X, 3) > BRC!(29) Then BRC!(29) = compPR!(X, 3): BRC$(29, 0) = compPR$(X): BRC$(29, 1) = TM$
            If compPR!(X, 4) > BRC!(30) And UCase$(finalUpdate$) = "Y" And compPR!(X, 1) >= numberGames% * 1.2 Then BRC!(30) = compPR!(X, 4): BRC$(30, 0) = compPR$(X): BRC$(30, 1) = TM$
            If statsPK!(X - 1) > BRC!(31) Then BRC!(31) = statsPK!(X - 1): BRC$(31, 0) = compPR$(X): BRC$(31, 1) = TM$: BRC$(31, 2) = PKR$(X - 1, 0)

            For Z = 171 To 180
                If compZ2$(Z) = compPR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = compPR!(X, 4)
                    compZ1!(Z, 2) = compPR!(X, 1)
                    GoTo 3365
                End If
            Next Z

            If Not (compPR!(X, 1) < numberGames% * 1.2 And compPR!(X, 4) <= compZ1!(180, 1)) Then

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
    Print "   CHECKING STATS OF "; puntName$

    If TG!(8) > BRC!(32) Then BRC!(32) = TG!(8): BRC$(32, 0) = puntName$: BRC$(32, 1) = TM$
    If TG!(9) > BRC!(33) Then BRC!(33) = TG!(9): BRC$(33, 0) = puntName$: BRC$(33, 1) = TM$
    If puntNum > BRC!(34) And UCase$(finalUpdate$) = "Y" Then BRC!(34) = puntNum: BRC$(34, 0) = puntName$: BRC$(34, 1) = TM$

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

        Print "   CHECKING STATS OF "; compPK$(X)

        If FM!(X) > BRC!(35) Then
            BRC!(35) = FM!(X)
            BRC$(35, 0) = compPK$(X)
            BRC$(35, 1) = TM$
        End If

        If FA!(X) > BRC!(36) Then
            BRC!(36) = FA!(X)
            BRC$(36, 0) = compPK$(X)
            BRC$(36, 1) = TM$
        End If

        If FG!(X) > BRC!(37) And UCase$(finalUpdate$) = "Y" Then
            BRC!(37) = FG!(X)
            BRC$(37, 0) = compPK$(X)
            BRC$(37, 1) = TM$
        End If

        If statsLF!(X - 1) > BRC!(38) Then
            BRC!(38) = statsLF!(X - 1)
            BRC$(38, 0) = compPK$(X)
            BRC$(38, 1) = TM$
            BRC$(38, 2) = statsLF$(X - 1)
        End If

        If EM!(X) > BRC!(39) Then
            BRC!(39) = EM!(X)
            BRC$(39, 0) = compPK$(X)
            BRC$(39, 1) = TM$
        End If

        If EA!(X) > BRC!(40) Then
            BRC!(40) = EA!(X)
            BRC$(40, 0) = compPK$(X)
            BRC$(40, 1) = TM$
        End If

        If EP!(X) > BRC!(41) And UCase$(finalUpdate$) = "Y" Then
            BRC!(41) = EP!(X)
            BRC$(41, 0) = compPK$(X)
            BRC$(41, 1) = TM$
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

        'PRINT"   CHECKING STATS OF ";EP$
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

            If statsGI%(X, 0) > BRC!(42) Then BRC!(42) = statsGI%(X, 0): BRC$(42, 0) = intName$(X): BRC$(42, 1) = TM$
            If statsGI%(X, 1) > BRC!(43) Then BRC!(43) = statsGI%(X, 1): BRC$(43, 0) = intName$(X): BRC$(43, 1) = TM$
            If statsGI%(X, 2) > BRC!(44) Then BRC!(44) = statsGI%(X, 2): BRC$(44, 0) = intName$(X): BRC$(44, 1) = TM$

            If X > 0 Then
                If statsLI!(X - 1) > BRC!(45) Then
                    BRC!(45) = statsLI!(X - 1)
                    BRC$(45, 0) = intName$(X)
                    BRC$(45, 1) = TM$
                    BRC$(45, 2) = statsLI$(X - 1, 0)
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

            If statsGS%(X, 0) > BRC!(46) Then
                BRC!(46) = statsGS%(X, 0)
                BRC$(46, 0) = sackName$(X)
                BRC$(46, 1) = TM$
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
                    For Z1 = Z + 1 To 240:
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
'     StatsByCategoryPro Subroutine
'----------------------------------------
'This subroutine compares all pre-loaded
'stats to each of the record categories to
'see if any should be updated
Sub StatsByCategoryPro (numberGames%, TM$, WT$, finalUpdate$)

    Shared compTT!(), DT!(), OT!(), TRC!()

    Shared TRC$(), TT$()

    Shared oppScore(), teamScore()

    Cls

    'TEAM CATEGORIES
    'CATEGORY 1 - TOTAL OFFENSE
    CA = 1

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2) + OT!(14)) / numberGames% > TRC!(0) Then
        TRC!(0) = (OT!(6) + OT!(2) + OT!(14)) / numberGames%
        TRC$(0) = TM$
    End If

    If OT!(6) + OT!(2) + OT!(14) > TRC!(1) Then
        TRC!(1) = OT!(6) + OT!(2) + OT!(14)
        TRC$(1) = TM$
    End If

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2) + OT!(14)) / (OT!(1) + OT!(3) - OT!(13)) > TRC!(2) Then
        TRC!(2) = (OT!(6) + OT!(2) + OT!(14)) / (OT!(1) + OT!(3) - OT!(13))
        TRC$(2) = TM$
    End If

    If (OT!(6) + OT!(2) + OT!(14)) / numberGames% > compTT!(CA, 20, 5) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = OT!(1) + OT!(3) - OT!(13)
        compTT!(CA, 20, 3) = OT!(6) + OT!(2) + OT!(14)
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames%
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

    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2) + DT!(14)) / numberGames% < TRC!(25) Then TRC!(25) = (DT!(6) + DT!(2) + DT!(14)) / numberGames%: TRC$(25) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) + DT!(2) + DT!(14) < TRC!(26) Then TRC!(26) = DT!(6) + DT!(2) + DT!(14): TRC$(26) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2) + DT!(14)) / (DT!(1) + DT!(3) - DT!(13)) < TRC!(27) Then TRC!(27) = (DT!(6) + DT!(2) + DT!(14)) / (DT!(1) + DT!(3) - DT!(13)): TRC$(27) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2) + DT!(14)) / numberGames% > TRC!(28) Then TRC!(28) = (DT!(6) + DT!(2) + DT!(14)) / numberGames%: TRC$(28) = TM$

    If (DT!(6) + DT!(2) + DT!(14)) / numberGames% < compTT!(CA, 20, 5) Then
        'Games
        compTT!(CA, 20, 1) = numberGames%
        'Plays
        compTT!(CA, 20, 2) = DT!(1) + DT!(3) - DT!(13)
        'Yds
        compTT!(CA, 20, 3) = DT!(6) + DT!(2) + DT!(14)
        'Avg Yds / Gm
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        'Yds / Gm
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames%
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

    If UCase$(finalUpdate$) = "Y" And (OT!(2) + OT!(14)) / numberGames% > TRC!(3) Then TRC!(3) = (OT!(2) + OT!(14)) / numberGames%: TRC$(3) = TM$
    If OT!(2) + OT!(14) > TRC!(4) Then TRC!(4) = OT!(2) + OT!(14): TRC$(4) = TM$
    If UCase$(finalUpdate$) = "Y" And (OT!(2) + OT!(14)) / (OT!(1) - OT!(13)) > TRC!(5) Then TRC!(5) = (OT!(2) + OT!(14)) / (OT!(1) - OT!(13)): TRC$(5) = TM$

    If (OT!(2) + OT!(14)) / numberGames% > compTT!(CA, 20, 5) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = OT!(1) - OT!(13)
        compTT!(CA, 20, 3) = OT!(2) + OT!(14)
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames%
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

    If UCase$(finalUpdate$) = "Y" And (DT!(2) + DT!(14)) / numberGames% < TRC!(29) Then TRC!(29) = (DT!(2) + DT!(14)) / numberGames%: TRC$(29) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(2) + DT!(14) < TRC!(30) Then TRC!(30) = DT!(2) + DT!(14): TRC$(30) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(2) + DT!(14)) / (DT!(1) - DT!(13)) < TRC!(31) Then TRC!(31) = (DT!(2) + DT!(14)) / (DT!(1) - DT!(13)): TRC$(31) = TM$

    If (DT!(2) + DT!(14)) / numberGames% < compTT!(CA, 20, 5) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = DT!(1) - DT!(13)
        compTT!(CA, 20, 3) = DT!(2) + DT!(14)
        compTT!(CA, 20, 4) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / numberGames%
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

    For I = 1 To numberGames%
        PTS = PTS + teamScore(I)
        OPTS = OPTS + oppScore(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames% > TRC!(13) Then TRC!(13) = PTS / numberGames%: TRC$(13) = TM$
    If PTS > TRC!(12) Then TRC!(12) = PTS: TRC$(12) = TM$
    If UCase$(finalUpdate$) = "Y" And (PTS - OPTS) / numberGames% > TRC!(14) Then TRC!(14) = (PTS - OPTS) / numberGames%: TRC$(14) = TM$

    If PTS / numberGames% > compTT!(CA, 20, 3) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = PTS
        compTT!(CA, 20, 3) = compTT!(CA, 20, 2) / numberGames%
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

    For I = 1 To numberGames%
        PTS = PTS + oppScore(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames% < TRC!(39) Then TRC!(39) = PTS / numberGames%: TRC$(39) = TM$
    If UCase$(finalUpdate$) = "Y" And PTS < TRC!(38) Then TRC!(38) = PTS: TRC$(38) = TM$
    If UCase$(finalUpdate$) = "Y" And PTS / numberGames% > TRC!(41) Then TRC!(41) = PTS / numberGames%: TRC$(41) = TM$
    If PTS > TRC!(40) Then TRC!(40) = PTS: TRC$(40) = TM$

    If PTS / numberGames% < compTT!(CA, 20, 3) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = PTS
        compTT!(CA, 20, 3) = compTT!(CA, 20, 2) / numberGames%
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

    If UCase$(finalUpdate$) = "Y" And OT!(6) / numberGames% > TRC!(6) Then TRC!(6) = OT!(6) / numberGames%: TRC$(6) = TM$
    If OT!(6) > TRC!(7) Then TRC!(7) = OT!(6): TRC$(7) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(6) / OT!(3) > TRC!(8) Then TRC!(8) = OT!(6) / OT!(3): TRC$(8) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(6) / OT!(4) > TRC!(9) Then TRC!(9) = OT!(6) / OT!(4): TRC$(9) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(4) / OT!(3) > TRC!(10) Then TRC!(10) = OT!(4) / OT!(3): TRC$(10) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(5) / OT!(3) < TRC!(11) Then TRC!(11) = OT!(5) / OT!(3): TRC$(11) = TM$

    If OT!(6) / numberGames% > compTT!(CA, 20, 8) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = OT!(3)
        compTT!(CA, 20, 3) = OT!(4)
        compTT!(CA, 20, 4) = OT!(5)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 6) = OT!(6)
        compTT!(CA, 20, 7) = OT!(6) / OT!(3)
        compTT!(CA, 20, 8) = compTT!(CA, 20, 6) / numberGames%
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

    If UCase$(finalUpdate$) = "Y" And DT!(6) / numberGames% < TRC!(32) Then TRC!(32) = DT!(6) / numberGames%: TRC$(32) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) < TRC!(33) Then TRC!(33) = DT!(6): TRC$(33) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) / DT!(3) < TRC!(34) Then TRC!(34) = DT!(6) / DT!(3): TRC$(34) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) / DT!(4) < TRC!(35) Then TRC!(35) = DT!(6) / DT!(4): TRC$(35) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(4) / DT!(3) < TRC!(36) Then TRC!(36) = DT!(4) / DT!(3): TRC$(36) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(5) / DT!(3) > TRC!(37) Then TRC!(37) = DT!(5) / DT!(3): TRC$(37) = TM$

    If DT!(6) / numberGames% < compTT!(CA, 20, 8) Then

        compTT!(CA, 20, 1) = numberGames%
        compTT!(CA, 20, 2) = DT!(3)
        compTT!(CA, 20, 3) = DT!(4)
        compTT!(CA, 20, 4) = DT!(5)
        compTT!(CA, 20, 5) = compTT!(CA, 20, 3) / compTT!(CA, 20, 2)
        compTT!(CA, 20, 6) = DT!(6)
        compTT!(CA, 20, 7) = DT!(6) / DT!(3)
        compTT!(CA, 20, 8) = compTT!(CA, 20, 6) / numberGames%
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

    If UCase$(finalUpdate$) = "Y" And OT!(8) / OT!(7) > TRC!(15) Then TRC!(15) = OT!(8) / OT!(7): TRC$(15) = TM$

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

    fumbGain = OT!(9) - OT!(10) + DT!(10)
    IG = DT!(5)
    FL = OT!(10) + DT!(9) - DT!(10)
    IL = OT!(5)
    TM! = (fumbGain + IG - FL - IL) / numberGames%

    If TM > compTT!(CA, 20, 7) Then
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
    If UCase$(finalUpdate$) = "Y" And OT!(25) / OT!(24) > TRC!(16) Then TRC!(16) = OT!(25) / OT!(24): TRC$(16) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(25) / DT!(24) < TRC!(42) Then TRC!(42) = DT!(25) / DT!(24): TRC$(42) = TM$

    'KO Return
    If UCase$(finalUpdate$) = "Y" And OT!(23) / OT!(22) > TRC!(17) Then TRC!(17) = OT!(23) / OT!(22): TRC$(17) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(23) / DT!(22) < TRC!(43) Then TRC!(43) = DT!(23) / DT!(22): TRC$(43) = TM$

    'FIRST DOWNS
    If OT!(0) > TRC!(18) Then TRC!(18) = OT!(0): TRC$(18) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(0) / numberGames% > TRC!(19) Then TRC!(19) = OT!(0) / numberGames%: TRC$(19) = TM$

    'FUMBLES
    If OT!(9) > TRC!(20) Then TRC!(20) = OT!(9): TRC$(20) = TM$
    If OT!(10) > TRC!(21) Then TRC!(21) = OT!(10): TRC$(21) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(10) < TRC!(22) Then TRC!(22) = OT!(10): TRC$(22) = TM$

    'PENALTIES
    If UCase$(finalUpdate$) = "Y" And OT!(11) / numberGames% > TRC!(23) Then TRC!(23) = OT!(11) / numberGames%: TRC$(23) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(12) / numberGames% > TRC!(24) Then TRC!(24) = OT!(12) / numberGames%: TRC$(24) = TM$

End Sub


'----------------------------------------
'        SortLeadersPro Subroutine
'----------------------------------------
'This subroutine sorts the leaders in the
'target league
Sub SortLeadersPro

    Shared O1!(), O2!(), O3!()
    Shared PTSA!(), PTSF!()

    Shared compZ1$()

    'Sort leaders in league
    For Z = 1 To 3

        AA = (10 * (Z - 1)) + 1
        ZZ = 10 * Z

        For X = AA To ZZ - 1:

            For Y = X + 1 To ZZ:

                diffX = O1!(X) - O2!(X)
                diffY = O1!(Y) - O2!(Y)

                If diffX <= diffY Then

                    If diffX <> diffY Then
                        If Not (O1!(X) <= O1!(Y) And O1!(X) > O2!(X)) Or Not (O1!(X) >= O1!(Y) And O1!(X) < O2!(X)) Then
                            Swap compZ1$(X), compZ1$(Y)
                            Swap O1!(X), O1!(Y)
                            Swap O2!(X), O2!(Y)
                            Swap O3!(X), O3!(Y)
                            Swap PTSF!(X), PTSF!(Y)
                            Swap PTSA!(X), PTSA!(Y)
                        End If
                    End If

                End If

            Next Y

        Next X

    Next Z

End Sub


'----------------------------------------
'        PostLeadersPro Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the league, and
'writes the updated stats to the file
Sub PostLeadersPro (leaderFile$)

    Shared BRC$()
    Shared compZ1$(), compZ2$(), compZ3$()
    Shared TRC$(), TT$()

    Shared BRC!(), compTT!(), compZ1!()
    Shared O1!(), O2!(), O3!()
    Shared PTSF!(), PTSA!()
    Shared TRC!()

    Cls

    Open diskPaths$(2) + leaderFile$ + ".LDR" For Output As #1

    Print "POSTING "; leaderFile$; " LEAGUE LEADERS"

    For X = 1 To 30:
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

    Open diskPaths$(1) + "RECORDS.PFB" For Output As #2

    For I = 0 To 50:
        Print #2, BRC!(I)
        Print #2, BRC$(I, 0)
        Print #2, BRC$(I, 1)
        Print #2, BRC$(I, 2)
    Next

    For I = 0 To 50:
        Print #2, TRC!(I)
        Print #2, TRC$(I)
    Next

    Close #2

End Sub




'$INCLUDE: 'COMPNATL.BAS'
'$INCLUDE: 'CREATCOL.BAS'
'$INCLUDE: 'CREATNAT.BAS'
'$INCLUDE: 'CREATPRO.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'HD2HDCOL.BAS'
'$INCLUDE: 'HD2HDPRO.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEECOLL.BAS'
'$INCLUDE: 'SEENATL.BAS'
'INCLUDE: 'SEEPRO.BAS'


'----------------------------------------
'            SEEPRO Subroutine
'----------------------------------------
'This subroutine lets the user choose a
'league stat file to view, then calls
'other routines to both load the Stats
'and then provide options for display
Sub SEEPRO ()

    _Title "3-in-1 Football - View League Stats"

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.LDR", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadLeaderFiles_Pro(league$, leaderFile$)
        Call StatOptions_Pro(league$)

    End If

End Sub

'----------------------------------------
'     LoadLeaderFiles_Pro Subroutine
'----------------------------------------
'This subroutine loads the conference stats
'from the file into several arrays
Sub LoadLeaderFiles_Pro (league$, leagueFile$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TR$()

    Shared seeL!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared seeT!(), compTT!(), seeW!()

    Shared div1$, div2$, div3$

    Open diskPaths$(2) + leagueFile$ + ".TMS" For Input As #1
    Input #1, league$, div1$, div2$, div3$
    Close #1

    div1$ = PadRight$(div1$, 8)
    div2$ = PadRight$(div2$, 8)
    div3$ = PadRight$(div3$, 8)

    Cls

    Locate 9, 1: Print "      PLEASE WAIT WHILE THE LEADER FILES ARE LOADING!!"

    Open diskPaths$(2) + leagueFile$ + ".LDR" For Input As #2

    For X = 1 To 30
        Input #2, TM$(X), seeW!(X), seeL!(X), seeT!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Input #2, compZ2$(X), compZ3$(X), LD!(X), LZ!(X)
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Input #2, compTT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Input #2, TR$(I, I1)
        Next
    Next

    Close #2

    D1 = seeW!(1) - seeL!(1)

    For P = 1 To 10:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            rightStr$ = Right$("     " + Str$(Int(seeW!(P))), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 16)

            rightStr$ = Right$("     " + Str$(Int(seeL!(P))), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 20)

            rightStr$ = Right$("     " + Str$(Int(seeT!(P))), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 24)

            A = 0

            If seeW!(P) + seeL!(P) > 0 Then A = Int((seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                rightStr$ = Str$(A)
                L = Len(rightStr$)
                rightStr$ = Right$(rightStr$, L - 1)

                Do While (Len(rightStr$) < 3)
                    rightStr$ = "0" + rightStr$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(rightStr$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)
            A = PTSF!(P)

            seeP$(P) = seeP$(P) + Right$("     " + Str$(A), 3)
            A = PTSA!(P)

            seeP$(P) = seeP$(P) + Right$("     " + Str$(A), 4)

        End If

    Next P

    D1 = seeW!(11) - seeL!(11)

    For P = 11 To 20:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(seeW!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 16)

            A = Int(seeL!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 20)

            A = Int(seeT!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 24)

            A = 0

            If seeW!(P) + seeL!(P) > 0 Then A = Int((seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                rightStr$ = Str$(A)
                L = Len(rightStr$)
                rightStr$ = Right$(rightStr$, L - 1)

                Do While (Len(rightStr$) < 3)
                    rightStr$ = "0" + rightStr$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(rightStr$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)
            A = PTSF!(P)
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = seeP$(P) + rightStr$

            A = PTSA!(P)
            rightStr$ = Right$("     " + Str$(A), 4)
            seeP$(P) = seeP$(P) + rightStr$

            'seeP$(P) = seeP$(P)
        End If

    Next P

    D1 = seeW!(21) - seeL!(21)

    For P = 21 To 30:

        If TM$(P) = "*" Or TM$(P) = "" Then
            seeP$(P) = ""
        Else

            seeP$(P) = PadRight$(TM$(P), 12)

            A = Int(seeW!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 16)

            A = Int(seeL!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 20)

            A = Int(seeT!(P))
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = PadRight$(seeP$(P) + rightStr$, 24)

            A = 0

            If seeW!(P) + seeL!(P) > 0 Then A = Int((seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P)) * 1000 + .5) / 1000

            A = Int(1000 * A + .5)

            If A = 1000 Then
                seeP$(P) = seeP$(P) + "1.000 "
            Else
                rightStr$ = Str$(A)
                L = Len(rightStr$)
                rightStr$ = Right$(rightStr$, L - 1)

                Do While (Len(rightStr$) < 3)
                    rightStr$ = "0" + rightStr$
                Loop

                seeP$(P) = seeP$(P) + " ." + Right$(rightStr$, 3)
            End If

            seeP$(P) = PadRight$(seeP$(P), 30)

            A = PTSF!(P)
            rightStr$ = Right$("     " + Str$(A), 3)
            seeP$(P) = seeP$(P) + rightStr$

            A = PTSA!(P)
            rightStr$ = Right$("     " + Str$(A), 4)
            seeP$(P) = seeP$(P) + rightStr$

            'seeP$(P) = seeP$(P)
        End If

    Next P

End Sub

'----------------------------------------
'       StatOptions_Pro Subroutine
'----------------------------------------
'This subroutine gives the user various
'options for viewing the previously loaded
'stats, and then calls routines for each
Sub StatOptions_Pro (league$)

    Do

        Cls

        Call TITLE_STATS

        Call MenuBox(8, 8, 61, 5, 15, 1)

        Locate 7, 32

        Do
            Locate 10
            Color 7, 1

            Locate , 30: Print "STATS TO ";
            Color 15: Print "S";: Color 7: Print "CREEN"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "P";: Color 7: Print "RINTER"
            Locate , 30: Print "STATS TO ";
            Color 15: Print "F";: Color 7: Print "ILE"

            I$ = GetKeyPress$

            Color 14, 0

        Loop Until UCase$(I$) = "F" Or UCase$(I$) = "P" Or UCase$(I$) = "S"

        If I$ <> Chr$(27) Then

            Select Case UCase$(I$)

                Case "F":
                    Call ReportFilePrompt_Pro(SF$)
                    Call SavePrintProLeaders(1, SF$, league$)

                Case "P":
                    Call SavePrintProLeaders(0, SF$, league$)

                Case "S":
                    Call LeaderStatsPro_Screen

            End Select

            Color 15, 0
            Cls

            Call box0(6, 8, 11, 70, 2, 15, 1)

            Do
                Color 15, 1
                Locate 8
                U$ = "VIEW LEAGUE STATS AGAIN? (Y/N)"
                Locate , (80 - Len(U$)) / 2
                Print U$
                Color 15, 0
                A$ = GetKeyPress$
            Loop Until UCase$(A$) = "Y" Or UCase$(A$) = "N"

        End If

    Loop Until UCase$(A$) = "N" Or A$ = Chr$(27)

End Sub


'----------------------------------------
'     ReportFilePrompt_Pro Subroutine
'----------------------------------------
'This subroutine prompts the user for
'the name of the report file to be saved
Sub ReportFilePrompt_Pro (reportFile$)

    Do

        Color 15, 0
        Cls

        Call MenuBox(6, 8, 61, 7, 15, 1)

        Locate 7, 32

        Locate , 12: Print "ENTER NAME FOR FILE: ": Print
        Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
        Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
        Locate , 12: Print "WILL AUTOMATICALLY ADD .RPT"
        Locate 7, 37: Input reportFile$

        If reportFile$ <> "" Then

            Do
                Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" And reportFile$ <> ""

End Sub


'----------------------------------------
'     LeaderStatsPro_Screen Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to the end
'user display.
Sub LeaderStatsPro_Screen ()

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TR$()

    Shared seeL!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared seeT!(), compTT!(), seeW!()

    Shared div1$, div2$, div3$

    Color 15, 0
    Cls

    Color 12
    Print div1$; " DIVISION"

    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 1 To 10:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P + 2 - RT)
            Print TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (seeW!(P) + seeT!(P) * .5) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    RT = 0

    Locate 1, 41
    Color 12
    Print div2$ + " DIVISION"
    Color 14

    Locate 2, 41
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 11 To 20:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 8 - RT), 41
            Print TM$(P); Tab(54); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print Tab(71); Using "### ###"; PTSF!(P); PTSA!(P)
        End If
    Next

    RT = 0

    Locate 13, 20

    Color 12
    Print div3$ + " DIVISION"

    Locate 14, 20
    Color 14
    Print "              W   L   T   PCT  PF  PA"
    Color 15

    For P = 21 To 30:

        If seeP$(P) = "" Then
            RT = RT + 1
        Else
            Locate (P - 6 - RT), 20
            Print TM$(P); Tab(33); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print "  .000";
                    Else
                        Print Using "  .###"; (seeW!(P) + .5 * seeT!(P)) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print Tab(50); Using "### ###"; PTSF!(P); PTSA!(P)

        End If

    Next

    RT = 0

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "RUSHING LEADERS (MIN 10 ATT/G)"
    Color 12
    Print "                            AVG  ATT"
    Color 15

    For P = 1 To 20
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##.# ####"; LD!(P); LZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RECEIVING LEADERS (MIN 3 REC/G)"
    Locate 2, 41

    Color 12
    Print "                            AVG  REC"
    Color 15

    For P = 21 To 40
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "##.#  ###"; LD!(P); LZ!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "RUSHING YARDS TOTAL"
    Color 15

    For P = 41 To 50
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RECEIVING YARDS TOTAL"
    Color 15

    For P = 51 To 60
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "RUSHING ATTEMPTS"
    Color 15

    For P = 81 To 90
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "TOTAL RECEPTIONS"
    Color 15

    For P = 91 To 100
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "ALL PURPOSE YARDAGE"
    Color 15

    For P = 101 To 110
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING NON-KICKERS"
    Color 15

    For P = 111 To 120
        If compZ2$(P) <> "*" Then
            Locate , 41: Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "RUSHING TOUCHDOWNS"
    Color 15

    For P = 61 To 70
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "RECEIVING TOUCHDOWNS"
    Color 15

    For P = 71 To 80
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "COMPLETION PERCENTAGE"
    Color 15

    For P = 121 To 130
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "###.#"; LD!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "QUARTERBACK RATING"
    Color 15

    For P = 131 To 140
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "###.#"; LD!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "PASSING YARDAGE"
    Color 15

    For P = 141 To 150
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "####"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "PASSING TOUCHDOWNS"
    Color 15

    For P = 151 To 160
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "####"; LD!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "KO RETURN AVERAGE                 #"
    Color 15

    For P = 161 To 170
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "PUNT RETURN AVERAGE               #"
    Color 15

    For P = 171 To 180
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "##.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "PUNTING AVERAGE"
    Color 15

    For P = 181 To 190
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##.#  ###"; LD!(P); LZ!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "SCORING (KICKERS)"
    Color 15

    For P = 191 To 200
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "###"; LD!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "FIELD GOAL %                     FGA"
    Color 15

    For P = 201 To 210
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "###.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "EXTRA POINT %                    EPA"
    Color 15

    For P = 211 To 220
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "###.#  ##"; LD!(P); LZ!(P)
        End If
    Next

    Locate 13
    Color 14
    Print "INTERCEPTIONS"
    Color 15

    For P = 221 To 230
        If compZ2$(P) <> "*" Then
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 28
            Print Using "##"; LD!(P)
        End If
    Next

    Locate 13, 41
    Color 14
    Print "SACKS"
    Color 15

    For P = 231 To 240
        If compZ2$(P) <> "*" Then
            Locate , 41
            Print compZ2$(P) + ", " + compZ3$(P);
            Locate , 68
            Print Using "##"; LD!(P)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 1

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "TOTAL DEFENSE  G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 2

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Locate , 41
            Print TR$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 3

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "RUSHING DEF    G PLAYS  YDS  AVG YDS/G"
    Color 15

    CA = 4

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Locate , 41
            Print TR$(CA, P);
            Locate , 55
            Print Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "SCORING OFF    G  POINTS    AVG"
    Color 15

    CA = 5

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3)
        End If
    Next

    Locate 1, 41
    Color 14
    Print "SCORING DEF    G  POINTS    AVG"
    Color 15

    CA = 6

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Locate , 41
            Print TR$(CA, P);
            Locate , 55
            Print Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
    Color 15

    CA = 7

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"
    Color 15

    CA = 8

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);
            Locate , 15
            Print Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "PUNTING        NO     AVG"
    Color 15

    CA = 9

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);: Locate , 15
            Print Using "###    ##.#"; compTT!(CA, P, 1); compTT!(CA, P, 2)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

    Cls

    Color 14
    Print "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
    Print "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "
    Color 15

    CA = 10

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print TR$(CA, P);: Locate , 18
            Print Using "### ###  ###    ### ###  ###       ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5); compTT!(CA, P, 6); compTT!(CA, P, 7)
        End If
    Next

    '-----
    Color 12
    'Locate 24: Print "HIT ANY KEY TO CONTINUE!";
    Color 15
    QK$ = GetKeyPress$
    '-----

End Sub


'----------------------------------------
'       SavePrintProLeaders Subroutine
'----------------------------------------
'This subroutine formats the previously
'loaded stats and outputs them to a file
'that is then looped through to send to
'the default printer
Sub SavePrintProLeaders (destination, reportFile$, league$)

    Shared compZ2$(), compZ3$()
    Shared seeP$(), TM$(), TR$()

    Shared seeL!(), LD!(), LZ!()
    Shared PTSA!(), PTSF!()
    Shared seeT!(), compTT!(), seeW!()

    Shared div1$, div2$, div3$

    If destination <> 1 Then
        Open "REPORT" For Output As #1
    Else
        Open reportFile$ + ".RPT" For Output As #1
    End If

    Color 0, 15
    Cls

    Print #1, league$ + " OVERALL RECORD"
    Print #1, div1$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 1 To 10

        If seeP$(P) <> "" Then
            Print #1, TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print #1, "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; seeW!(P) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #1,
    Print #1, div2$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 11 To 20

        If seeP$(P) <> "" Then
            Print #1, TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print #1, "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; seeW!(P) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #1,
    Print #1, div3$; " DIVISION"
    Print #1, "              W   L   T   PCT  PF  PA"

    For P = 21 To 30

        If seeP$(P) <> "" Then

            Print #1, TM$(P); Tab(14); Using "##  ##  ##"; seeW!(P); seeL!(P); seeT!(P);

            If seeW!(P) + seeL!(P) + seeT!(P) = 0 Then
                Print #1, "  .---";
            Else
                If seeL!(P) + seeT!(P) = 0 Then
                    Print #1, " 1.000";
                Else
                    If seeW!(P) = 0 Then
                        Print #1, "  .000";
                    Else
                        Print #1, Using "  .###"; seeW!(P) / (seeW!(P) + seeL!(P) + seeT!(P));
                    End If
                End If
            End If

            Print #1, Tab(31); Using "### ###"; PTSF!(P); PTSA!(P)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING LEADERS (MIN 10 ATT/G)"; Tab(41); "RECEIVING LEADERS (MIN 4 REC/G)"
    Print #1, "                            AVG  ATT"; Tab(41); "                            AVG  REC"

    For P = 1 To 20

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.# ####"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 20) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 20) + ", " + compZ3$(P + 20);
            Print #1, Tab(68);
            Print #1, Using "##.# ####"; LD!(P + 20); LZ!(P + 20)
        End If

    Next

    Print #1,
    Print #1, "RUSHING YARDS TOTAL"; Tab(41); "RECEIVING YARDS TOTAL"

    For P = 41 To 50

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "RUSHING ATTEMPTS"; Tab(41); "TOTAL RECEPTIONS"

    For P = 81 To 90

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "ALL PURPOSE YARDAGE"; Tab(41); "SCORING NON-KICKERS"

    For P = 101 To 110
        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "RUSHING TOUCHDOWNS"; Tab(41); "RECEIVING TOUCHDOWNS"

    For P = 61 To 70
        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "COMPLETION PERCENTAGE"; Tab(41); "QUARTERBACK RATING"

    For P = 121 To 130

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PASSING YARDAGE"; Tab(41); "PASSING TOUCHDOWNS"

    For P = 141 To 150
        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "####"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "####"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "KO RETURN AVERAGE                 #"; Tab(41); "PUNT RETURN AVERAGE               #"

    For P = 161 To 170

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ##"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##.#  ##"; LD!(P + 10); LZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "PUNTING AVERAGE"; Tab(41); "SCORING (KICKERS)"

    For P = 181 To 190

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##.#  ###"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###"; LD!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "FIELD GOAL %                     FGA"; Tab(41); "EXTRA POINT %                   EPA"

    For P = 201 To 210

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "###.#  ###"; LD!(P); LZ!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "###.#  ##"; LD!(P + 10); LZ!(P + 10)
        End If

    Next

    Print #1,
    Print #1, "INTERCEPTIONS"; Tab(41); "SACKS"

    For P = 221 To 230

        If compZ2$(P) = "*" Then
            Print #1, Tab(41);
        Else
            Print #1, compZ2$(P) + ", " + compZ3$(P);
            Print #1, Tab(28);
            Print #1, Using "##"; LD!(P);
            Print #1, Tab(41);
        End If

        If compZ2$(P + 10) = "*" Then
            Print #1,
        Else
            Print #1, compZ2$(P + 10) + ", " + compZ3$(P + 10);
            Print #1, Tab(68);
            Print #1, Using "##"; LD!(P + 10)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "TOTAL OFFENSE  G PLAYS  YDS  AVG YDS/G"; Tab(41); "TOTAL DEFENSE   G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 1

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, TR$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5);
            Print #1, Tab(41);
        End If

        CA = 2

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, TR$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If

    Next

    Print #1,
    Print #1, "RUSHING OFF    G PLAYS  YDS  AVG YDS/G"; Tab(41); "RUSHING DEF     G PLAYS  YDS  AVG YDS/G"

    For P = 1 To 20

        CA = 3

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, TR$(CA, P); Tab(15);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5);
            Print #1, Tab(41);
        End If

        CA = 4

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, TR$(CA, P); Tab(56);
            Print #1, Using "## #### ##### ##.# ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5)
        End If

    Next

    Print #1, Chr$(12)
    Print #1, "PASSING OFF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 7

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #1, TR$(CA, P); Tab(15);
            Print #1, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
        End If
    Next

    Print #1,
    Print #1, "PASSING DEF    G   ATT   COM  INT    PCT  YARDS  YDS/ATT   YDS/G"

    CA = 8

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #1, TR$(CA, P); Tab(15);
            Print #1, Using "##  ####  ####  ###  ###.#  #####   ##.#     ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5) * 100; compTT!(CA, P, 6); compTT!(CA, P, 7); compTT!(CA, P, 8)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "SCORING OFF    G  POINTS    AVG"; Tab(41); "SCORING DEF     G  POINTS    AVG"

    For P = 1 To 20

        CA = 5

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #1, Tab(41);
        Else
            Print #1, TR$(CA, P); Tab(15);
            Print #1, Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3);
            Print #1, Tab(41);
        End If

        CA = 6

        If TR$(CA, P) = "*" Or TR$(CA, P) = "" Then
            Print #1,
        Else
            Print #1, TR$(CA, P); Tab(56);
            Print #1, Using "##   ####   ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3)
        End If

    Next

    Print #1,
    Print #1, "PUNTING        NO     AVG"

    CA = 9

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #1, TR$(CA, P); Tab(15);
            Print #1, Using "###    ##.#"; compTT!(CA, P, 1); compTT!(CA, P, 2)
        End If
    Next

    Print #1, Chr$(12)
    Print #1, "TURNOVER MARGIN      GAINED          LOST           MARGIN/"
    Print #1, "                 FUM INT TOTAL   FUM INT TOTAL       GAME  "

    CA = 10

    For P = 1 To 20
        If TR$(CA, P) <> "*" And TR$(CA, P) <> "" Then
            Print #1, TR$(CA, P); Tab(18);
            Print #1, Using "### ###  ###    ### ###  ###       ###.#"; compTT!(CA, P, 1); compTT!(CA, P, 2); compTT!(CA, P, 3); compTT!(CA, P, 4); compTT!(CA, P, 5); compTT!(CA, P, 6); compTT!(CA, P, 7)
        End If
    Next

    Close #1

    If destination <> 1 Then

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

    End If

End Sub

