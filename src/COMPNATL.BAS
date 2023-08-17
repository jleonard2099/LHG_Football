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

    Shared statsT%()

    Shared BRC!()
    Shared compZ1!(), N1!(), NT!()
    Shared O1!(), O2!(), O3!(), OT!()
    Shared PT!(), PTSA!(), PTSF!(), TRC!(), TT!()

    Shared BRC$()
    Shared compZ1$(), compZ2$(), compZ3$()
    Shared H1$()
    Shared N2$(), N3$(), NT$()
    Shared PT$(), TRC$(), TT$()

    Shared div1$, div2$

    Dim NRT

    Dim CN$(40)

    'Initialize values first
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

    Erase compZ1!, NT!, TT!
    Erase O1!, O2!, O3!
    Erase PTSF!, PTSA!

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
    finalUpdate$ = "N"

    If _FileExists(diskPaths$(1) + "RECORDS.CFB") Then

        Open diskPaths$(1) + "RECORDS.CFB" For Input As #1

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

        Close 1

    End If

    organizationFile$ = ChooseFile$ (diskPaths$(2) + "*.CON", "ORGANIZATION", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Do
            Color 15, 0
            Cls
            Print "IS THIS THE FINAL UPDATE FOR THE SEASON? (Y/N)";
            I$ = GetKeyPress$
            finalUpdate$ = UCase$(I$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

        Open diskPaths$(2) + organizationFile$ + ".CON" For Input As #2

        Input #2, X$

        For I = 1 To 40
            Input #2, CN$(I)
        Next

        Close 2

        For NC = 1 To 40

            If CN$(NC) <> "XXX" Then

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
                    TT!(2, I, 5) = 9999
                    TT!(4, I, 5) = 9999
                    TT!(6, I, 3) = 9999
                    TT!(8, I, 8) = 9999
                    TT!(10, I, 7) = -99
                Next

                Cls

                test$ = CN$(NC)
                Open diskPaths$(2) + CN$(NC) + ".TMS" For Input As #2

                Input #2, FF$
                Input #2, div1$
                Input #2, div2$

                For XW = 1 To 30
                    Input #2, H1$(XW)
                Next

                Close 2

                Open diskPaths$(2) + CN$(NC) + ".LIB" For Input As #2
                Input #2, LG%
                Close 2

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

                    Open diskPaths$(2) + CN$(NC) + ".TMS" For Input As #2

                    Input #2, FF$
                    Input #2, div1$
                    Input #2, div2$

                    For X = 1 To 30
                        Input #2, compZ1$(X)
                    Next X

                    Close 2

                    haveStats = 1

                    For XW = 1 To 30

                        If H1$(XW) <> Chr$(42) Then
                            test$ = H1$(XW)
                            haveStats = FindStatsNat%(XW, H1$(XW), numberGames%, NRT)

                            If (haveStats) Then

                                L = Len(H1$(XW))
                                WT$ = ""

                                For X = 1 To L
                                    A = Asc(Mid$(H1$(XW), X, 1))
                                    WT$ = WT$ + Chr$(A)
                                Next X

                                confGame = 0

                                For X = 1 To 30

                                    If compZ1$(X) = WT$ Then

                                        If O1!(X) = fullWins And O2!(X) = fullLosses And O3!(X) = fullTies Then
                                            confGame = 1
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

                                        If (confGame) Then Exit For

                                    End If

                                Next X
                                'alternatively, use a Do / Until loop; confGame = 1 or EC = 30

                                If confGame <> 1 And _FileExists(diskPaths$(1) + H1$(XW)) Then
                                    Call ReadStatsNat(H1$(XW), numberGames%, TM$)
                                    Call StatsByCategoryNatl(numberGames%, TM$, WT$, finalUpdate$)
                                    Call CheckStatsNatl(numberGames%, TM$, WT$, finalUpdate$)
                                End If

                            End If

                        End If

                    Next XW

                End If 'Done checking for valid conference

                Call SortLeadersConf
                Call POSTLEADERS(CN$(NC))
                Call SortLeadersNat

            End If

        Next NC

        'All conferences are done; post org leaders
        Call POSTNATIONAL(organizationFile$)

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

        Open diskPaths$(2) + organizationFile$ + ".POL" For Output As #2
        Print "POSTING "; organizationFile$; " POLL"
        Write #2, NRT

        For X = 1 To 1200
            Write #2, PT$(X), PT!(X, 1), PT!(X, 2), PT!(X, 3), PT!(X, 4), PT!(X, 5)
        Next

        Close 2

        Open diskPaths$(1) + "RECORDS.CFB" For Output As #1

        For I = 0 To 50:
            Print #1, BRC!(I)
            Print #1, BRC$(I, 0)
            Print #1, BRC$(I, 1)
            Print #1, BRC$(I, 2)
        Next

        For I = 0 To 50:
            Print #1, TRC!(I)
            Print #1, TRC$(I)
        Next

        Close 1

    End If 'Done checking ESC

End Sub


'----------------------------------------
'       ReadStatsNat Subroutine
'----------------------------------------
'This subroutine reads stats in for a 
'single team in the organization
Sub ReadStatsNat (statFile$, numberGames%, TM$)

    Shared puntNum
    Shared PU$

    Shared compPK$(), intName$(), sackName$(), KR$(), H1$()
    Shared LC$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared PKR$(), PR$(), QB$(), RB$(), TB$(), WR$()

    Shared AM!(), AF!(), DT!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared NT!(), OT!()
    Shared PK!(), PR!(), QB!(), RB!()
    Shared TD!(), TG!(), TP!(), WR!()
    Shared YD!(), compZ2!()

    Shared statsGI%(), statsGS%()

    Open diskPaths$(1) + statFile$ For Input As #1

    Input #1, TM$

    For I = 1 To 10: Input #1, RB$(I): Next

    For I = 1 To 6: Input #1, WR$(I): Next

    For I = 1 To 4: Input #1, QB$(I): Next

    For I = 1 To 3: Input #1, KR$(I): Next

    For I = 1 To 3: Input #1, PR$(I): Next I

    Input #1, PU$

    For I = 1 To 2: Input #1, compPK$(I): Next

    For J = 1 To 14
        For K = 1 To 18
            Input #1, compZ2!(J, K)
        Next K
    Next J

    For L = 0 To 38: Input #1, OT!(L): Next L

    For L = 0 To 38: Input #1, DT!(L): Next L

    For J = 0 To 9: Input #1, intName$(J)
        For J1 = 0 To 2
            Input #1, statsGI%(J, J1)
        Next
    Next

    For J = 0 To 14: Input #1, sackName$(J): For J1 = 0 To 1: Input #1, statsGS%(J, J1): Next: Next

    For I = 0 To 9: Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next

    For I = 0 To 15: Input #1, LC!(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next

    For I = 0 To 3: Input #1, LP!(I): Input #1, statsLP$(I, 0): Input #1, statsLP$(I, 1): Next

    For I = 0 To 2: Input #1, LK!(I): Input #1, statsLK$(I, 0): Input #1, statsLK$(I, 1): Next

    For I = 0 To 2: Input #1, PK!(I): Input #1, PKR$(I, 0): Input #1, PKR$(I, 1): Next

    For I = 0 To 1: Input #1, LF!(I): Input #1, statsLF$(I): For I1 = 0 To 4: Input #1, AM!(I, I1): Input #1, AF!(I, I1): Next: Next

    For I = 0 To 9: Input #1, LI!(I): Input #1, statsLI$(I, 0): Input #1, statsLI$(I, 1): Next

    Close 1

    For I = 1 To 10: For J = 1 To 6: RB!(I, J) = compZ2!(I, J): Next: Next

    For I = 1 To 6: For J = 1 To 3: WR!(I, J) = compZ2!(I, J + 6): Next: Next

    For I = 1 To 4: For J = 1 To 5: QB!(I, J) = compZ2!(I, J + 9): Next: Next

    For I = 1 To 3: For J = 1 To 3: KR!(I, J) = compZ2!(I, J + 14): Next: Next

    For I = 1 To 3: For J = 1 To 3: PR!(I, J) = compZ2!(I + 3, J + 14): Next: Next

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
        RB!(I, 7) = RB!(I, 2) + RB!(I, 5)
        RB!(I, 8) = RB!(I, 3) + RB!(I, 6)
        WR$(I + 6) = RB$(I)
        WR!(I + 6, 2) = RB!(I, 5)
        WR!(I + 6, 1) = RB!(I, 4)
        WR!(I + 6, 3) = RB!(I, 6)
    Next

    For I = 1 To 10: TB$(I) = RB$(I): YD!(I) = RB!(I, 7): TD!(I) = RB!(I, 8): Next
    For I = 1 To 6: TB$(I + 10) = WR$(I): YD!(I + 10) = WR!(I, 2): TD!(I + 10) = WR!(I, 3): Next

    For I = 1 To 16
        For I1 = 1 To 3
            If KR$(I1) = TB$(I) Then YD!(I) = YD!(I) + KR!(I1, 2): TD!(I) = TD!(I) + KR!(I1, 3)
        Next
    Next

    For I = 1 To 16
        For I1 = 1 To 3
            If PR$(I1) = TB$(I) Then YD!(I) = YD!(I) + PR!(I1, 2): TD!(I) = TD!(I) + PR!(I1, 3)
        Next
    Next

    For I = 1 To 16: TP!(I) = TD!(I) * 6: Next

    For I = 1 To 2: TB$(16 + I) = compPK$(I): TP!(16 + I) = EM!(I) + FM!(I) * 3: Next

    For I = 1 To 10
        RB!(I, 9) = 0:
        If RB!(I, 1) > 0 Then RB!(I, 9) = Int(RB!(I, 2) / RB!(I, 1) * 10) / 10
    Next

    For I = 1 To 10
        RB!(I, 10) = 0:
        If RB!(I, 4) > 0 Then RB!(I, 10) = Int(RB!(I, 5) / RB!(I, 4) * 10) / 10
    Next

    For I = 1 To 16
        WR!(I, 4) = 0:
        If WR!(I, 1) > 0 Then WR!(I, 4) = Int(WR!(I, 2) / WR!(I, 1) * 10) / 10
    Next

    For I = 1 To 4
        QB!(I, 6) = 0
        Call QBRatingColl(I)
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
        PR!(I, 4) = 0
        If PR!(I, 1) > 0 And PR!(I, 1) >= numberGames% Then
            PR!(I, 4) = Int(PR!(I, 2) / PR!(I, 1) * 10) / 10
        Else
            PR!(I, 4) = 0
        End If
    Next

    puntNum = 0

    If TG!(8) > 0 Then puntNum = Int(TG!(9) / TG!(8) * 10) / 10

    For I = 1 To 2
        'FG = FieldGoal Made
        'FGA = FieldGoal Attempts
        FG!(I) = 0
        If FA!(I) >= numberGames% Then FG!(I) = Int(FM!(I) / FA!(I) * 1000) / 10

        'EP = ExtraPoint Mode ??
        'EA = ExtraPoint Attempts ??
        EP!(I) = 0
        If EA!(I) >= numberGames% Then EP!(I) = Int(EM!(I) / EA!(I) * 1000) / 10
    Next

End Sub


'----------------------------------------
'       CheckStatsNatl Subroutine
'----------------------------------------
'This subroutine compares pre-loaded stats
'to existing leaders and updates as needed
Sub CheckStatsNatl (numberGames%, TM$, WT$, finalUpdate$)

    Shared FG, puntNum
    Shared PU$

    Shared BRC$()
    Shared compPK$(), compZ2$(), compZ3$()
    Shared intName$(), sackName$(), KR$()
    Shared LC$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), LR$()
    Shared PKR$(), PR$(), compPS$()
    Shared QB$(), RB$(), TB$(), WR$()

    Shared BRC!(), compZ1!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()

    Shared PK!(), PR!(), QB!(), RB!()
    Shared TG!(), TP!(), WR!(), YD!()

    Shared statsGI%(), statsGS%()

    '---------------
    ' Running Backs
    '---------------
    For X = 1 To 10

        Print "   CHECKING STATS OF "; RB$(X)

        If RB$(X) <> "OTHERS" Then

            If RB!(X, 1) > BRC!(0) Then BRC!(0) = RB!(X, 1): BRC$(0, 0) = RB$(X): BRC$(0, 1) = TM$
            If RB!(X, 2) > BRC!(1) Then BRC!(1) = RB!(X, 2): BRC$(1, 0) = RB$(X): BRC$(1, 1) = TM$
            If RB!(X, 3) > BRC!(2) Then BRC!(2) = RB!(X, 3): BRC$(2, 0) = RB$(X): BRC$(2, 1) = TM$

            If RB!(X, 9) > BRC!(3) And UCase$(finalUpdate$) = "Y" And RB!(X, 1) >= numberGames% * 10 Then
                'Rushing Average
                BRC!(3) = RB!(X, 9)
                BRC$(3, 0) = RB$(X)
                BRC$(3, 1) = TM$
            End If

            If LR!(X - 1) > BRC!(4) Then
                'Longest Run
                BRC!(4) = LR!(X - 1)
                BRC$(4, 0) = RB$(X)
                BRC$(4, 1) = TM$
                BRC$(4, 2) = LR$(X - 1, 0)
            End If

            compPS$(X) = WT$

            For Y = 1 To 20:

                If compZ2$(Y) = RB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = RB!(X, 9)
                    compZ1!(Y, 2) = RB!(X, 1)

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

                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1

                            End If

                        Next Z

                    Next Y

                    GoTo 3118
                End If

            Next Y

            If Not (RB!(X, 1) < numberGames% * 10 Or RB!(X, 9) < compZ1!(20, 1)) Then

                compZ2$(20) = RB$(X)
                compZ3$(20) = WT$
                compZ1!(20, 1) = RB!(X, 9)
                compZ1!(20, 2) = RB!(X, 1)

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
                If compZ2$(Z) = RB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(RB!(X, 2))
                    GoTo 3123
                End If
            Next Z

            If Int(RB!(X, 2)) > compZ1!(50, 1) Then

                compZ1!(50, 1) = Int(RB!(X, 2))
                compZ2$(50) = RB$(X)
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
                If compZ2$(Z) = RB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = RB!(X, 3)
                    GoTo 3155
                End If
            Next Z

            If RB!(X, 3) > compZ1!(70, 1) Then

                compZ1!(70, 1) = RB!(X, 3)
                compZ2$(70) = RB$(X)
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
                If compZ2$(Z) = RB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = RB!(X, 1)
                    GoTo 3525
                End If
            Next Z

            If RB!(X, 1) > compZ1!(90, 1) Then

                compZ1!(90, 1) = RB!(X, 1)
                compZ2$(90) = RB$(X)
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

        Print "   CHECKING STATS OF "; WR$(X)

        If WR$(X) <> "OTHERS" Then

            If WR!(X, 1) > BRC!(5) Then BRC!(5) = WR!(X, 1): BRC$(5, 0) = WR$(X): BRC$(5, 1) = TM$
            If WR!(X, 2) > BRC!(6) Then BRC!(6) = WR!(X, 2): BRC$(6, 0) = WR$(X): BRC$(6, 1) = TM$
            If WR!(X, 3) > BRC!(7) Then BRC!(7) = WR!(X, 3): BRC$(7, 0) = WR$(X): BRC$(7, 1) = TM$

            If WR!(X, 4) > BRC!(8) And UCase$(finalUpdate$) = "Y" And WR!(X, 1) >= numberGames% * 4 Then
                'Receiving Average
                BRC!(8) = WR!(X, 4)
                BRC$(8, 0) = WR$(X)
                BRC$(8, 1) = TM$
            End If

            If LC!(X - 1) > BRC!(9) Then
                'Longest Catch
                BRC!(9) = LC!(X - 1)
                BRC$(9, 0) = WR$(X)
                BRC$(9, 1) = TM$
                BRC$(9, 2) = LC$(X - 1, 0)
            End If

            For Z = 21 To 40:

                If compZ2$(Z) = WR$(X) And compZ3$(Z) = WT$ Then

                    compZ1!(Z, 1) = WR!(X, 4)
                    compZ1!(Z, 2) = WR!(X, 1)

                    'Only include players with 4 RECs / game
                    If compZ1!(Z, 2) < numberGames% * 4 Then

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

            If Not ((WR!(X, 1) < numberGames% * 4 Or WR!(X, 4) < compZ1!(40, 1))) Then

                compZ2$(40) = WR$(X)
                compZ3$(40) = WT$
                compZ1!(40, 1) = WR!(X, 4)
                compZ1!(40, 2) = WR!(X, 1)

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
                If compZ2$(Z) = WR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(WR!(X, 2))
                    GoTo 3183
                End If
            Next Z

            If Int(WR!(X, 2)) > compZ1!(60, 1) Then
                compZ1!(60, 1) = Int(WR!(X, 2))
                compZ2$(60) = WR$(X)
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
                If compZ2$(Z) = WR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = WR!(X, 1)
                    GoTo 3215
                End If
            Next Z

            If WR!(X, 1) > compZ1!(100, 1) Then

                compZ1!(100, 1) = WR!(X, 1)
                compZ2$(100) = WR$(X)
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

            3225 '
            For Z = 71 To 80
                If compZ2$(Z) = WR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = WR!(X, 3)
                    GoTo 3230
                End If
            Next Z

            If WR!(X, 3) > compZ1!(80, 1) Then

                compZ1!(80, 1) = WR!(X, 3)
                compZ2$(80) = WR$(X)
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

        If TP!(X) > BRC!(20) Then BRC!(20) = TP!(X): BRC$(20, 0) = TB$(X): BRC$(20, 1) = TM$
        If TP!(X) > BRC!(21) Then BRC!(21) = TP!(X): BRC$(21, 0) = TB$(X): BRC$(21, 1) = TM$

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

        Print "   CHECKING STATS OF "; QB$(X)

        If QB$(X) <> "OTHERS" Then

            If QB!(X, 1) > BRC!(10) Then BRC!(10) = QB!(X, 1): BRC$(10, 0) = QB$(X): BRC$(10, 1) = TM$
            If QB!(X, 2) > BRC!(11) Then BRC!(11) = QB!(X, 2): BRC$(11, 0) = QB$(X): BRC$(11, 1) = TM$
            If QB!(X, 5) > BRC!(12) Then BRC!(12) = QB!(X, 5): BRC$(12, 0) = QB$(X): BRC$(12, 1) = TM$
            If QB!(X, 3) > BRC!(13) Then BRC!(13) = QB!(X, 3): BRC$(13, 0) = QB$(X): BRC$(13, 1) = TM$
            If QB!(X, 4) > BRC!(14) Then BRC!(14) = QB!(X, 4): BRC$(14, 0) = QB$(X): BRC$(14, 1) = TM$
            If QB!(X, 6) > BRC!(15) And UCase$(finalUpdate$) = "Y" And QB!(X, 1) >= numberGames% * 15 Then BRC!(15) = QB!(X, 6): BRC$(15, 0) = QB$(X): BRC$(15, 1) = TM$
            If QB!(X, 7) > BRC!(16) And UCase$(finalUpdate$) = "Y" And QB!(X, 1) >= numberGames% * 15 Then BRC!(16) = QB!(X, 7): BRC$(16, 0) = QB$(X): BRC$(16, 1) = TM$
            If LP!(X - 1) > BRC!(17) Then BRC!(17) = LP!(X - 1): BRC$(17, 0) = QB$(X): BRC$(17, 1) = TM$: BRC$(17, 2) = statsLP$(X - 1, 0)

            For Y = 121 To 130:
                If compZ2$(Y) = QB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = QB!(X, 6)
                    compZ1!(Y, 2) = QB!(X, 1)

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
                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1

                            End If
                        Next Z
                    Next Y

                    GoTo 3279
                End If
            Next Y

            If Not (QB!(X, 1) < numberGames% * 15 Or QB!(X, 6) < compZ1!(130, 1)) Then

                compZ2$(130) = QB$(X)
                compZ3$(130) = WT$
                compZ1!(130, 1) = QB!(X, 6)
                compZ1!(130, 2) = QB!(X, 1)

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

                If compZ2$(Y) = QB$(X) And compZ3$(Y) = WT$ Then

                    compZ1!(Y, 1) = QB!(X, 7)
                    compZ1!(Y, 2) = QB!(X, 1)

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

                                For Z1 = 1 To 2
                                    Swap compZ1!(Y, Z1), compZ1!(Z, Z1)
                                Next Z1
                            End If

                        Next Z
                    Next Y

                    GoTo 3287
                End If
            Next Y

            If Not (QB!(X, 1) < numberGames% * 15 Or QB!(X, 6) < compZ1!(140, 1)) Then

                compZ2$(140) = QB$(X)
                compZ3$(140) = WT$
                compZ1!(140, 1) = QB!(X, 7)
                compZ1!(140, 2) = QB!(X, 1)

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
                If compZ2$(Z) = QB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(QB!(X, 4))
                    GoTo 3294
                End If
            Next Z

            If Int(QB!(X, 4)) > compZ1!(150, 1) Then

                compZ1!(150, 1) = Int(QB!(X, 4))
                compZ2$(150) = QB$(X)

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
                If compZ2$(Z) = QB$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = QB!(X, 5)
                    GoTo 3307
                End If
            Next Z

            If QB!(X, 5) > compZ1!(160, 1) Then

                compZ1!(160, 1) = QB!(X, 5)
                compZ2$(160) = QB$(X)
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

        Print "   CHECKING STATS OF "; KR$(X)

        If KR$(X) <> "OTHERS" Then

            If KR!(X, 1) > BRC!(22) Then BRC!(22) = KR!(X, 1): BRC$(22, 0) = KR$(X): BRC$(22, 1) = TM$
            If KR!(X, 2) > BRC!(23) Then BRC!(23) = KR!(X, 2): BRC$(23, 0) = KR$(X): BRC$(23, 1) = TM$
            If KR!(X, 3) > BRC!(24) Then BRC!(24) = KR!(X, 3): BRC$(24, 0) = KR$(X): BRC$(24, 1) = TM$
            If KR!(X, 4) > BRC!(25) And UCase$(finalUpdate$) = "Y" And KR!(X, 1) >= numberGames% * 1.2 Then BRC!(25) = KR!(X, 4): BRC$(25, 0) = KR$(X): BRC$(25, 1) = TM$
            If LK!(X - 1) > BRC!(26) Then BRC!(26) = LK!(X - 1): BRC$(26, 0) = KR$(X): BRC$(26, 1) = TM$: BRC$(26, 2) = statsLK$(X - 1, 0)

            For Z = 161 To 170
                If compZ2$(Z) = KR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = KR!(X, 4)
                    compZ1!(Z, 2) = KR!(X, 1)
                    GoTo 3345
                End If
            Next Z

            If Not (KR!(X, 1) < numberGames% * 1.2 Or KR!(X, 4) <= compZ1!(170, 1)) Then

                compZ1!(170, 1) = KR!(X, 4)
                compZ2$(170) = KR$(X)
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

        Print "   CHECKING STATS OF "; PR$(X)

        If PR$(X) <> "OTHERS" Then

            If PR!(X, 1) > BRC!(27) Then BRC!(27) = PR!(X, 1): BRC$(27, 0) = PR$(X): BRC$(27, 1) = TM$
            If PR!(X, 2) > BRC!(28) Then BRC!(28) = PR!(X, 2): BRC$(28, 0) = PR$(X): BRC$(28, 1) = TM$
            If PR!(X, 3) > BRC!(29) Then BRC!(29) = PR!(X, 3): BRC$(29, 0) = PR$(X): BRC$(29, 1) = TM$
            If PR!(X, 4) > BRC!(30) And UCase$(finalUpdate$) = "Y" And PR!(X, 1) >= numberGames% * 1.2 Then BRC!(30) = PR!(X, 4): BRC$(30, 0) = PR$(X): BRC$(30, 1) = TM$
            If PK!(X - 1) > BRC!(31) Then BRC!(31) = PK!(X - 1): BRC$(31, 0) = PR$(X): BRC$(31, 1) = TM$: BRC$(31, 2) = PKR$(X - 1, 0)

            For Z = 171 To 180
                If compZ2$(Z) = PR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = PR!(X, 4)
                    compZ1!(Z, 2) = PR!(X, 1)
                    GoTo 3365
                End If
            Next Z

            If Not (PR!(X, 1) < numberGames% * 1.2 And PR!(X, 4) <= compZ1!(180, 1)) Then

                compZ1!(180, 1) = PR!(X, 4)
                compZ2$(180) = PR$(X)
                compZ3$(180) = WT$
                compZ1!(180, 2) = PR!(X, 1)

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
    Print "   CHECKING STATS OF "; PU$

    If TG!(8) > BRC!(32) Then BRC!(32) = TG!(8): BRC$(32, 0) = PU$: BRC$(32, 1) = TM$
    If TG!(9) > BRC!(33) Then BRC!(33) = TG!(9): BRC$(33, 0) = PU$: BRC$(33, 1) = TM$
    If puntNum > BRC!(34) And UCase$(finalUpdate$) = "Y" Then BRC!(34) = puntNum: BRC$(34, 0) = PU$: BRC$(34, 1) = TM$

    For Z = 181 To 190
        If compZ2$(Z) = PU$ And compZ3$(Z) = WT$ Then
            compZ1!(Z, 1) = puntNum
            compZ1!(Z, 2) = TG!(8)
            GoTo 3382
        End If
    Next Z

    If puntNum > compZ1!(190, 1) Then

        compZ1!(190, 1) = puntNum
        compZ2$(190) = PU$
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
    3390 '
    For X = 1 To 2

        Print "   CHECKING STATS OF "; compPK$(X)

        If FM!(X) > BRC!(35) Then BRC!(35) = FM!(X): BRC$(35, 0) = compPK$(X): BRC$(35, 1) = TM$
        If FA!(X) > BRC!(36) Then BRC!(36) = FA!(X): BRC$(36, 0) = compPK$(X): BRC$(36, 1) = TM$
        If FG!(X) > BRC!(37) And UCase$(finalUpdate$) = "Y" Then BRC!(37) = FG!(X): BRC$(37, 0) = compPK$(X): BRC$(37, 1) = TM$

        If LF!(X - 1) > BRC!(38) Then BRC!(38) = LF!(X - 1): BRC$(38, 0) = compPK$(X): BRC$(38, 1) = TM$: BRC$(38, 2) = statsLF$(X - 1)

        If EM!(X) > BRC!(39) Then BRC!(39) = EM!(X): BRC$(39, 0) = compPK$(X): BRC$(39, 1) = TM$
        If EA!(X) > BRC!(40) Then BRC!(40) = EA!(X): BRC$(40, 0) = compPK$(X): BRC$(40, 1) = TM$
        If EP!(X) > BRC!(41) And UCase$(finalUpdate$) = "Y" Then BRC!(41) = EP!(X): BRC$(41, 0) = compPK$(X): BRC$(41, 1) = TM$

        For Z = 201 To 210
            If compZ2$(Z) = compPK$(X) And compZ3$(Z) = WT$ Then
                compZ1!(Z, 1) = FG
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
                If LI!(X - 1) > BRC!(45) Then
                    BRC!(45) = LI!(X - 1)
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
Sub StatsByCategoryNatl (numberGames%, TM$, WT$, finalUpdate$)

    Shared DT!(), OT!(), TRC!(), TT!()

    Shared TRC$(), TT$()

    Shared statsO%(), statsT%()

    Cls

    'TEAM CATEGORIES
    'CATEGORY 1 - TOTAL OFFENSE
    CA = 1

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2)) / numberGames% > TRC!(0) Then TRC!(0) = (OT!(6) + OT!(2)) / numberGames%: TRC$(0) = TM$
    If OT!(6) + OT!(2) > TRC!(1) Then TRC!(1) = OT!(6) + OT!(2): TRC$(1) = TM$
    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2)) / (OT!(1) + OT!(3)) > TRC!(2) Then TRC!(2) = (OT!(6) + OT!(2)) / (OT!(1) + OT!(3)): TRC$(2) = TM$

    If (OT!(6) + OT!(2)) / numberGames% > TT!(CA, 20, 5) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = OT!(1) + OT!(3)
        TT!(CA, 20, 3) = OT!(6) + OT!(2)
        TT!(CA, 20, 4) = TT!(CA, 20, 3) / TT!(CA, 20, 2)
        TT!(CA, 20, 5) = TT!(CA, 20, 3) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 5) < TT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z
    End If

    'CATEGORY 2 - TOTAL DEFENSE
    CA = 2

    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2)) / numberGames% < TRC!(25) Then TRC!(25) = (DT!(6) + DT!(2)) / numberGames%: TRC$(25) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) + DT!(2) < TRC!(26) Then TRC!(26) = DT!(6) + DT!(2): TRC$(26) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2)) / (DT!(1) + DT!(3)) < TRC!(27) Then TRC!(27) = (DT!(6) + DT!(2)) / (DT!(1) + DT!(3)): TRC$(27) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2)) / numberGames% > TRC!(28) Then TRC!(28) = (DT!(6) + DT!(2)) / numberGames%: TRC$(28) = TM$

    If (DT!(6) + DT!(2)) / numberGames% < TT!(CA, 20, 5) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = DT!(1) + DT!(3)
        TT!(CA, 20, 3) = DT!(6) + DT!(2)
        TT!(CA, 20, 4) = TT!(CA, 20, 3) / TT!(CA, 20, 2)
        TT!(CA, 20, 5) = TT!(CA, 20, 3) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 5) > TT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z
    End If

    'CATEGORY 3 - RUSHING OFFENSE
    CA = 3

    If UCase$(finalUpdate$) = "Y" And OT!(2) / numberGames% > TRC!(3) Then TRC!(3) = OT!(2) / numberGames%: TRC$(3) = TM$
    If OT!(2) > TRC!(4) Then TRC!(4) = OT!(2): TRC$(4) = TM$
    If UCase$(finalUpdate$) = "Y" And OT!(2) / OT!(1) > TRC!(5) Then TRC!(5) = OT!(2) / OT!(1): TRC$(5) = TM$

    test! = OT!(2) / numberGames%
    test2! = TT!(CA, 20, 5)

    If OT!(2) / numberGames% > TT!(CA, 20, 5) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = OT!(1)
        TT!(CA, 20, 3) = OT!(2)
        TT!(CA, 20, 4) = TT!(CA, 20, 3) / TT!(CA, 20, 2)
        TT!(CA, 20, 5) = TT!(CA, 20, 3) / numberGames%
        TT$(CA, 20) = WT$
        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 5) < TT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 4 - RUSHING DEFENSE
    CA = 4

    If UCase$(finalUpdate$) = "Y" And DT!(2) / numberGames% < TRC!(29) Then TRC!(29) = DT!(2) / numberGames%: TRC$(29) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(2) < TRC!(30) Then TRC!(30) = DT!(2): TRC$(30) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(2) / DT!(1) < TRC!(31) Then TRC!(31) = DT!(2) / DT!(1): TRC$(31) = TM$

    If DT!(2) / numberGames% < TT!(CA, 20, 5) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = DT!(1)
        TT!(CA, 20, 3) = DT!(2)
        TT!(CA, 20, 4) = TT!(CA, 20, 3) / TT!(CA, 20, 2)
        TT!(CA, 20, 5) = TT!(CA, 20, 3) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 5) > TT!(CA, Z1, 5) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
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
        PTS = PTS + statsO%(I)
        OPTS = OPTS + statsT%(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames% > TRC!(13) Then TRC!(13) = PTS / numberGames%: TRC$(13) = TM$
    If PTS > TRC!(12) Then TRC!(12) = PTS: TRC$(12) = TM$
    If UCase$(finalUpdate$) = "Y" And (PTS - OPTS) / numberGames% > TRC!(14) Then TRC!(14) = (PTS - OPTS) / numberGames%: TRC$(14) = TM$
    If PTS / numberGames% > TT!(CA, 20, 3) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = PTS
        TT!(CA, 20, 3) = TT!(CA, 20, 2) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 3) < TT!(CA, Z1, 3) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 6 - SCORING DEFENSE
    CA = 6

    PTS = 0

    For I = 1 To numberGames%
        PTS = PTS + statsT%(I)
    Next

    If UCase$(finalUpdate$) = "Y" And PTS / numberGames% < TRC!(39) Then TRC!(39) = PTS / numberGames%: TRC$(39) = TM$
    If UCase$(finalUpdate$) = "Y" And PTS < TRC!(38) Then TRC!(38) = PTS: TRC$(38) = TM$
    If UCase$(finalUpdate$) = "Y" And PTS / numberGames% > TRC!(41) Then TRC!(41) = PTS / numberGames%: TRC$(41) = TM$

    If PTS > TRC!(40) Then TRC!(40) = PTS: TRC$(40) = TM$

    If PTS / numberGames% < TT!(CA, 20, 3) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = PTS
        TT!(CA, 20, 3) = TT!(CA, 20, 2) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19

            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 3) > TT!(CA, Z1, 3) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
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

    If OT!(6) / numberGames% > TT!(CA, 20, 8) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = OT!(3)
        TT!(CA, 20, 3) = OT!(4)
        TT!(CA, 20, 4) = OT!(5)
        TT!(CA, 20, 5) = TT!(CA, 20, 3) / TT!(CA, 20, 2)
        TT!(CA, 20, 6) = OT!(6)
        TT!(CA, 20, 7) = OT!(6) / OT!(3)
        TT!(CA, 20, 8) = TT!(CA, 20, 6) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 8) < TT!(CA, Z1, 8) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
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

    If DT!(6) / numberGames% < TT!(CA, 20, 8) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = DT!(3)
        TT!(CA, 20, 3) = DT!(4)
        TT!(CA, 20, 4) = DT!(5)
        TT!(CA, 20, 5) = TT!(CA, 20, 3) / TT!(CA, 20, 2)
        TT!(CA, 20, 6) = DT!(6)
        TT!(CA, 20, 7) = DT!(6) / DT!(3)
        TT!(CA, 20, 8) = TT!(CA, 20, 6) / numberGames%
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 8) > TT!(CA, Z1, 8) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 9 - PUNTING
    CA = 9

    If UCase$(finalUpdate$) = "Y" And OT!(8) / OT!(7) > TRC!(15) Then TRC!(15) = OT!(8) / OT!(7): TRC$(15) = TM$

    If OT!(8) / OT!(7) > TT!(CA, 20, 2) Then

        TT!(CA, 20, 1) = OT!(7)
        TT!(CA, 20, 2) = OT!(8) / OT!(7)
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 2) < TT!(CA, Z1, 2) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
                    Next MN
                End If
            Next Z1
        Next Z

    End If

    'CATEGORY 10 - TURNOVER MARGIN
    CA = 10

    FG = OT!(9) - OT!(10) + DT!(10)
    IG = DT!(5)
    FL = OT!(10) + DT!(9) - DT!(10)
    IL = OT!(5)
    TM = (FG + IG - FL - IL) / numberGames%

    If TM > TT!(CA, 20, 7) Then

        TT!(CA, 20, 1) = FG
        TT!(CA, 20, 2) = IG
        TT!(CA, 20, 3) = FG + IG
        TT!(CA, 20, 4) = FL
        TT!(CA, 20, 5) = IL
        TT!(CA, 20, 6) = FL + IL
        TT!(CA, 20, 7) = TM
        TT$(CA, 20) = WT$

        For Z = 1 To 19
            For Z1 = Z + 1 To 20
                If TT!(CA, Z, 7) < TT!(CA, Z1, 7) Then
                    Swap TT$(CA, Z1), TT$(CA, Z)
                    For MN = 1 To 9
                        Swap TT!(CA, Z1, MN), TT!(CA, Z, MN)
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
'         FindStatsNat Function
'----------------------------------------
'...explanation...
Function FindStatsNat% (XW, statFile$, numberGames%, NRT)

    Shared compO$(), H1$(), statsH$()

    Shared compAP%()
    Shared statsO%(), statsT%()

    Shared PT!(), PT$()
    Shared PTSA!(), PTSF!()

    fullWins = 0
    fullLosses = 0
    fullTies = 0

    If Not _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Print "NO STATS FOUND FOR "; statFile$
        FindStatsNat% = 0

    Else

        Call ReadScheduleStats(statFile$, XW, numberGames%)

        fullWins = 0
        fullLosses = 0
        fullTies = 0
        confWins = 0
        confTies = 0
        confLosses = 0

        PTSFC = 0
        PTSAC = 0

        'RATINGS
        For I = 1 To numberGames%

            teamName$ = compo$(I)

            FOR P = 4 TO 11: 
                IF MID$(teamName$, P, 1) = " " OR MID$(teamName$, P, 1) = "." THEN MID$(teamName$, P, 1) = "_"
            NEXT P

            FOR P = 12 TO 14: 
                IF MID$(teamName$, P, 2) = "ST" THEN MID$(teamName$, 9, 1) = "_": MID$(teamName$, 10, 2) = "ST"
            NEXT P

            QQ = 8

            FOR P = 11 TO 4 STEP -1: 
                IF MID$(teamName$, P, 1) = "_" THEN 
                    QQ = P - 4
                Else
                    Exit For
                End If

            NEXT P

            teamName$ = MID$(teamName$, 4, QQ) + "." + LEFT$(teamName$, 2)

            compo$(I) = teamName$

            confGame = 0

            For EC = 1 To 30

                If compO$(I) = H1$(EC) Then
                    confGame = 1
                    Exit For
                End If

            Next EC

            If confGame <> 1 Then
                If statsO%(I) > statsT%(I) Then fullWins = fullWins + 1
                If statsO%(I) < statsT%(I) Then fullLosses = fullLosses + 1
                If statsO%(I) = statsT%(I) Then fullTies = fullTies + 1
            Else
                If statsO%(I) > statsT%(I) Then fullWins = fullWins + 1: confWins = confWins + 1
                If statsO%(I) < statsT%(I) Then fullLosses = fullLosses + 1: confLosses = confLosses + 1
                If statsO%(I) = statsT%(I) Then fullTies = fullTies + 1: confTies = confTies + 1
            End If

            PTSFC = PTSFC + statsO%(I)
            PTSAC = PTSAC + statsT%(I)
            EC = 0

        Next I

        'STRENGTH OF SCHEDULE
        teamPower = 0
        SS = 0

        For I = 1 To numberGames%
            SS = compAP%(1, I) + SS
        Next

        SS = SS / numberGames%

        'INDIVIDUAL GAME POWER RATINGS
        GL = 0
        teamPower = 0

        For I = 1 To numberGames%
            RAT = statsO%(I) - statsT%(I) + compAP%(1, I)
            If Left$(statsH$(I), 1) = "H" Then RAT = RAT - 3
            If Left$(statsH$(I), 1) = "A" Then RAT = RAT + 3
            teamPower = teamPower + RAT
        Next

        If numberGames% <= 4 Then
            GL = 5 - numberGames%
            teamPower = teamPower + GL * compAP%(0, 1)
        End If

        teamPower = teamPower / (numberGames% + GL)

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

    Shared compZ1!()
    Shared N1!(), NT!(), TT!()

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

            If TT!(CA, X, 5) >= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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

            If TT!(CA, X, 5) <= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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

            If TT!(CA, X, 5) >= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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

            If TT!(CA, X, 5) <= NT!(CA, 20, 5) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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

            If TT!(CA, X, 3) >= NT!(CA, 20, 3) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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

            If TT!(CA, X, 3) <= NT!(CA, 20, 3) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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

            If TT!(CA, X, 8) >= NT!(CA, 20, 8) Then

                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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
            If TT!(CA, X, 8) <= NT!(CA, 20, 8) Then
                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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
            If TT!(CA, X, 2) >= NT!(CA, 20, 2) Then
                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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
            If TT!(CA, X, 7) >= NT!(CA, 20, 7) Then
                For I = 1 To 8
                    NT!(CA, 20, I) = TT!(CA, X, I)
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
'         POSTLEADERS Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the conference,
'and writes the updated stats to the file
Sub POSTLEADERS (confName$)

    Shared compZ1$(), compZ2$(), compZ3$()
    Shared TT$()

    Shared compZ1!(), O1!(), O2!(), O3!()
    Shared TT!(), PTSA!(), PTSF!()

    Cls

    Open diskPaths$(2) + confName$ + ".LDR" For Output As #2

    Print "POSTING "; confName$; " CONFERENCE LEADERS"

    For X = 1 To 60:
        Write #2, compZ1$(X), O1!(X), O2!(X), O3!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Write #2, compZ2$(X), compZ3$(X), compZ1!(X, 1), compZ1!(X, 2)
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Write #2, TT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Write #2, TT$(I, I1)
        Next
    Next

    Close 2

End Sub

'----------------------------------------
'        POSTNATIONAL Subroutine
'----------------------------------------
'This subroutine opens the existing leader
'and record files for the organization,
'and writes the updated stats to the file
Sub POSTNATIONAL (leaderFile$)

    Shared compZ1$(), N2$(), N3$(), NT$()

    Shared O1!(), O2!(), O3!(), PTSA!(), PTSF!()

    Shared N1!(), NT!()

    Cls

    Open diskPaths$(2) + leaderFile$ + ".NLD" For Output As #2

    Print "POSTING "; leaderFile$; " LEADERS"

    For X = 1 To 60:
        Write #2, compZ1$(X), O1!(X), O2!(X), O3!(X), PTSF!(X), PTSA!(X)
    Next

    For X = 1 To 240
        Write #2, N2$(X), N3$(X), N1!(X, 1), N1!(X, 2)
    Next X

    For I = 1 To 10
        For I1 = 1 To 20
            For I2 = 1 To 9
                Write #2, NT!(I, I1, I2)
            Next
        Next
    Next

    For I = 1 To 10
        For I1 = 1 To 20
            Write #2, NT$(I, I1)
        Next
    Next

    Close 2

End Sub


'----------------------------------------
'      ReadScheduleStats Subroutine
'----------------------------------------
'This subroutine reads in all data from the
'team's schedule stats file.
Sub ReadScheduleStats (statFile$, XW, numberGames%)

    Shared compO$(), statsH$()

    Shared compAP%(), statsO%(), statsT%()

    Shared PTSA!(), PTSF!()

    Open diskPaths$(1) + statFile$ + "S" For Input As #2

    Input #2, numberGames%

    For I = 1 To 120:
        Input #2, statsO%(I)
        PTSF!(XW) = PTSF!(XW) + statsO%(I)
    Next I

    For I = 1 To 120:
        Input #2, compO$(I)
    Next I

    For I = 1 To 120:
        Input #2, statsT%(I)
        PTSA!(XW) = PTSA!(XW) + statsT%(I)
    Next I

    For I = 1 To 120:
        Input #2, statsH$(I)
    Next I

    For I = 0 To 1
        For I1 = 1 To 120
            Input #2, compAP%(I, I1)
        Next
    Next

    Close 2

End Sub
