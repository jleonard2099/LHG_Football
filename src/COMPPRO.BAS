Sub COMPPRO ()

    Shared BRC$()
    Shared compZ1$(), compZ2$(), compZ3$()
    Shared H1$(), TRC$()

    Shared BRC!()
    Shared O1!(), O2!(), O3!()
    Shared PTSF!(), PTSA!()
    Shared TRC!(), TT!()

    Shared D1$, D2$, D3$

    _Title "3-in-1 Football - Update League Stats"

    Erase BRC!
    Erase BRC$
    Erase TRC!
    Erase TRC$

    Cls

    'Initialize certain record fields
    'Presumably this is to help with
    'later calculations?
    TRC!(11) = 999
    TRC!(22) = 999

    For I = 25 To 27: TRC!(I) = 9999: Next
    For I = 29 To 36: TRC!(I) = 9999: Next

    TRC!(38) = 999
    TRC!(39) = 999
    TRC!(42) = 999
    TRC!(43) = 999

    'Read in any existing records
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

        Close 1

    End If


    '-- this should be it's own routine now that I see it

    Do
        'Display list of files...

        Spec$ = diskPaths$(2) + "*.LIB"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else

            ReDim leagueFiles$(0 To Count%)
            For X = 1 To Count%
                leagueFiles$(X) = Space$(12)
            Next
            leagueFiles$(0) = Spec$
            Call ReadFile(leagueFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT LEAGUE TO UPDATE": Color 15
                Selection% = 1
                START% = 1
                FileFGColr% = 15: FileBGColr% = 1
                HiLiteFGColr% = 0: HiLiteBGColr% = 7
                Rows% = 20
                Columns% = 4
                Spacing% = 6
                Row% = 4
                Column% = 1
                Locate 25, 1
                Color 14
                Print "ENTER";: Color 15
                Print "-SELECTS LEAGUE";

                _MouseShow "DEFAULT"
                MAMenu leagueFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = leagueFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate , 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                WE$ = Left$(targetFile$, Len(targetFile$) - 4)

                Cls
                Print "IS THIS THE FINAL UPDATE FOR THE SEASON (YN)";
                Color 15, 0

                Do
                    I$ = GetKeyPress$
                    finalUpdate$ = UCase$(I$)
                Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N"

                C$ = ","

                'Initialize values first
                For X = 1 To 30
                    PTSF!(X) = 0
                    PTSA!(X) = 0
                Next X

                For X = 1 To 240
                    compZ2$(X) = "*"
                Next

                CA = 2

                For I = 1 To 20
                    TT!(CA, I, 5) = 9999
                Next

                CA = 4

                For I = 1 To 20
                    TT!(CA, I, 5) = 9999
                Next

                CA = 6

                For I = 1 To 20
                    TT!(CA, I, 3) = 9999
                Next

                CA = 8

                For I = 1 To 20
                    TT!(CA, I, 8) = 9999
                Next

                CA = 10

                For I = 1 To 20
                    TT!(CA, I, 7) = -99
                Next

                Open diskPaths$(2) + WE$ + ".TMS" For Input As #2
                Input #2, FF$, D1$, D2$, D3
                For XW = 1 To 30
                    Input #2, H1$(XW)
                Next XW
                Close 2


                3018 '
                Open diskPaths$(2) + WE$ + ".LIB" For Input As #2
                Input #2, LG%
                Close 2


                If LG% <> 1 Then
                    Cls
                    Print "ERROR:: THIS IS A COLLEGE CONFERENCE"
                    Print "        RETURNING TO MAIN MENU"
                    Locate 23, 1: Print "PRESS ANY KEY TO CONTINUE"
                    I$ = GetKeyPress$
                Else

                    Open diskPaths$(2) + WE$ + ".TMS" For Input As #2
                    Input #2, FF$, D1$, D2$, D3

                    For X = 1 To 30
                        Input #2, compZ1$(X)
                    Next X
                    Close 2

                    For XW = 1 To 30

                        Call FindStatsPro(XW, numberGames%)

                        L = Len(H1$(XW))

                        WT$ = ""

                        For X = 1 To L:
                            A = Asc(Mid$(H1$(XW), X, 1))
                            WT$ = WT$ + Chr$(A)
                        Next X

                        skipMe% = 0

                        For X = 1 To 30:

                            If compZ1$(X) = WT$ Then

                                If O1!(X) = WN And O2!(X) = LS And O3!(X) = TY Then skipMe% = 1: Exit For

                                O1!(X) = WN
                                O2!(X) = LS
                                O3!(X) = TY

                            End If

                        Next X

                        If skipMe% <> 1 Then
                            Call ReadStatsPro(XW, numberGames%, TM$)
                            Call StatsByCategoryPro(TM$, numberGames%, WT$, finalUpdate$)
                            Call CheckStatsPro(numberGames%, TM$, WT$, finalUpdate$)
                        End If

                    Next

                    950 '
                    For Z = 1 To 3

                        AA = (10 * (Z - 1)) + 1
                        ZZ = 10 * Z

                        For X = AA To ZZ - 1:

                            For Y = X + 1 To ZZ:
                                diffX = O1!(X) - O2!(X)
                                diffY = O1!(Y) - O2!(Y)

                                If diffX <= diffY Then
                                    If diffX <> diffY Then
                                        If O1!(X) > O1!(Y) Or O1!(X) <= O2!(X) Or O1!(X) < O1!(Y) Or O1!(X) >= O2!(X) Then
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

                    Call SortStatsPro(WE$)

                End If 'Done checking file type
        
            End If 'Done checking for ESC
        
        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'       FindStatsPro Subroutine
'----------------------------------------
'...explanation...
Sub FindStatsPro (XW, numberGames%)

    Shared AP%()
    Shared H$(), H1$(), O$()

    Shared GI%(), GS%()
    Shared O%(), T%()

    Shared PTSA!(), PTSF!()

    WN = 0
    LS = 0
    TY = 0

    If H1$(XW) <> Chr$(42) Then

        H1$(XW) = Right$(H1$(XW), 2) + " " + Left$(H1$(XW), Len(H1$(XW)) - 3)

        Call FILENAME(XW)

        If _FileExists(diskPaths$(1) + H1$(XW) + "S") Then

            Open diskPaths$(1) + H1$(XW) + "S" For Input As #2

            Input #2, numberGames%

            For I = 1 To 120: Input #2, O%(I): Next I

            For I = 1 To 120: Input #2, O$(I): PTSF!(XW) = PTSF!(XW) + O%(I): Next I

            For I = 1 To 120: Input #2, T%(I): PTSA!(XW) = PTSA!(XW) + T%(I): Next I

            For I = 1 To 120: Input #2, H$(I): Next I

            For I = 0 To 1
                For I1 = 1 To 120
                    Input #2, AP%(I, I1)
                Next
            Next

            Close 2

            WN = 0
            LS = 0
            TY = 0
            CW = 0
            CT = 0
            CL = 0

            5500 '
            For I = 1 To numberGames%
                If O%(I) > T%(I) Then WN = WN + 1
                If O%(I) < T%(I) Then LS = LS + 1
                If O%(I) = T%(I) Then TY = TY + 1
            Next

            Print "FINDING "; H1$(XW); " STATS"

        End If 'Done checking for file

    End If 'Done checking for '*'

End Sub

'----------------------------------------
'       ReadStatsPro Subroutine
'----------------------------------------
'...explanation...
Sub ReadStatsPro (XW, numberGames%, TM$)

    Shared PN
    Shared PU$

    Shared DI$(), DS$(), H1$(), KR$()
    Shared LC$(), LF$(), LI$(), LK$(), LP$(), LR$()
    Shared PKR$(), RB$(), QB$(), PR$(), compPK$()
    Shared TB$(), WR$()

    Shared AM!(), AF!(), DT!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared OT!(), PK!(), PR!(), QB!(), RB!()
    Shared TD!(), TG!(), TP!()
    Shared WR!(), YD!(), Z2!()

    Shared GI%(), GS%()

    Open diskPaths$(1) + H1$(XW) For Input As #1
    Input #1, TM$

    'Positions
    For I = 1 To 10: Input #1, RB$(I): Next
    For I = 1 To 6: Input #1, WR$(I): Next
    For I = 1 To 4: Input #1, QB$(I): Next
    For I = 1 To 3: Input #1, KR$(I): Next
    For I = 1 To 3: Input #1, PR$(I): Next I

    'Punter
    Input #1, PU$

    'Kickers
    For I = 1 To 2: Input #1, compPK$(I): Next

    For J = 1 To 14
        For K = 1 To 18
            Input #1, Z2!(J, K)
        Next K
    Next J

    For L = 0 To 38: Input #1, OT!(L): Next L
    For L = 0 To 38: Input #1, DT!(L): Next L

    For J = 0 To 9: Input #1, DI$(J)
        For J1 = 0 To 2
            Input #1, GI%(J, J1)
        Next
    Next

    For J = 0 To 14:
        Input #1, DS$(J)
        For J1 = 0 To 1:
            Input #1, GS%(J, J1)
        Next
    Next

    For I = 0 To 9:
        Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1)
    Next

    For I = 0 To 15:
        Input #1, LC!(I)
        Input #1, LC$(I, 0)
        Input #1, LC$(I, 1)
    Next

    For I = 0 To 3:
        Input #1, LP!(I)
        Input #1, LP$(I, 0)
        Input #1, LP$(I, 1)
    Next

    For I = 0 To 2:
        Input #1, LK!(I)
        Input #1, LK$(I, 0)
        Input #1, LK$(I, 1)
    Next

    For I = 0 To 2:
        Input #1, PK!(I)
        Input #1, PKR$(I, 0)
        Input #1, PKR$(I, 1)
    Next

    For I = 0 To 1:
        Input #1, LF!(I)
        Input #1, LF$(I)
        For I1 = 0 To 4
            Input #1, AM!(I, I1)
            Input #1, AF!(I, I1)
        Next
    Next

    For I = 0 To 9:
        Input #1, LI!(I)
        Input #1, LI$(I, 0)
        Input #1, LI$(I, 1)
    Next

    Close 1

    For I = 1 To 10:
        For J = 1 To 6:
            RB!(I, J) = Z2!(I, J)
        Next
    Next

    For I = 1 To 6:
        For J = 1 To 3:
            WR!(I, J) = Z2!(I, J + 6)
        Next
    Next

    For I = 1 To 4:
        For J = 1 To 5:
            QB!(I, J) = Z2!(I, J + 9)
        Next
    Next

    For I = 1 To 3:
        For J = 1 To 3:
            KR!(I, J) = Z2!(I, J + 14)
        Next
    Next

    For I = 1 To 3:
        For J = 1 To 3:
            PR!(I, J) = Z2!(I + 3, J + 14)
        Next
    Next

    TG!(8) = OT!(7)
    TG!(9) = OT!(8)
    FM!(1) = Z2!(4, 18)
    FA!(1) = Z2!(5, 18)
    EM!(1) = Z2!(6, 18)
    EA!(1) = Z2!(7, 18)
    FM!(2) = Z2!(8, 18)
    FA!(2) = Z2!(9, 18)
    EM!(2) = Z2!(10, 18)
    EA!(2) = Z2!(11, 18)

    For I = 1 To 10:
        RB!(I, 7) = RB!(I, 2) + RB!(I, 5)
        RB!(I, 8) = RB!(I, 3) + RB!(I, 6)
        WR$(I + 6) = RB$(I)
        WR!(I + 6, 2) = RB!(I, 5)
        WR!(I + 6, 1) = RB!(I, 4)
        WR!(I + 6, 3) = RB!(I, 6)
    Next

    For I = 1 To 10
        TB$(I) = RB$(I)
        YD!(I) = RB!(I, 7)
        TD!(I) = RB!(I, 8)
    Next

    For I = 1 To 6
        TB$(I + 10) = WR$(I)
        YD!(I + 10) = WR!(I, 2)
        TD!(I + 10) = WR!(I, 3)
    Next

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

    For I = 1 To 16
        TP!(I) = TD!(I) * 6
    Next

    For I = 1 To 2
        TB$(16 + I) = compPK$(I)
        TP!(16 + I) = EM!(I) + FM!(I) * 3
    Next

    For I = 1 To 10
        RB!(I, 9) = 0
        If RB!(I, 1) > 0 Then RB!(I, 9) = Int(RB!(I, 2) / RB!(I, 1) * 10) / 10
    Next

    For I = 1 To 10
        RB!(I, 10) = 0
        If RB!(I, 4) > 0 Then RB!(I, 10) = Int(RB!(I, 5) / RB!(I, 4) * 10) / 10
    Next

    For I = 1 To 16:
        WR!(I, 4) = 0
        If WR!(I, 1) > 0 Then WR!(I, 4) = Int(WR!(I, 2) / WR!(I, 1) * 10) / 10
    Next

    For I = 1 To 4:
        QB!(I, 6) = 0
        If QB!(I, 1) > 0 Then QB!(I, 6) = Int(QB!(I, 2) / QB!(I, 1) * 1000) / 10
        Call QBCalculations(I)
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

    PN = 0

    If TG!(8) > 0 Then PN = Int(TG!(9) / TG!(8) * 10) / 10

    For I = 1 To 2
        FG!(I) = 0
        If FA!(I) >= numberGames% Then FG!(I) = Int(FM!(I) / FA!(I) * 1000) / 10
        EP!(I) = 0
        If EA!(I) >= numberGames% Then EP!(I) = Int(EM!(I) / EA!(I) * 1000) / 10
    Next

End Sub

'----------------------------------------
'       SortStatsPro Subroutine
'----------------------------------------
'...explanation...
Sub SortStatsPro (statsFile$)

    Shared BRC$()
    Shared compZ2$(), compZ3$()
    Shared TRC$(), TT$()
    Shared compZ1$()

    Shared BRC!(), compZ1!()
    Shared O1!(), O2!(), O3!()
    Shared PTSF!(), PTSA!()
    Shared TRC!(), TT!()

    4010 '
    Cls

    Open diskPaths$(2) + statsFile$ + ".LDR" For Output As #2

    Print "POSTING "; statsFile$; " LEAGUE LEADERS"

    For X = 1 To 30:
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

    Open diskPaths$(1) + "RECORDS.PFB" For Output As #1

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

End Sub

'----------------------------------------
'     StatsByCategoryPro Subroutine
'----------------------------------------
'...explanation...
Sub StatsByCategoryPro (TM$, numberGames%, WT$, finalUpdate$)

    Shared DT!(), OT!(), TRC!(), TT!()

    Shared TRC$(), TT$()

    Shared O%(), T%()

    Cls

    'TEAM CATEGORIES
    'CATEGORY 1 - TOTAL OFFENSE
    CA = 1

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2) + OT!(14)) / numberGames% > TRC!(0) Then
        TRC!(0) = (OT!(6) + OT!(2) + OT!(14)) / numberGames%: TRC$(0) = TM$
    End If

    If OT!(6) + OT!(2) + OT!(14) > TRC!(1) Then TRC!(1) = OT!(6) + OT!(2) + OT!(14): TRC$(1) = TM$

    If UCase$(finalUpdate$) = "Y" And (OT!(6) + OT!(2) + OT!(14)) / (OT!(1) + OT!(3) - OT!(13)) > TRC!(2) Then
        TRC!(2) = (OT!(6) + OT!(2) + OT!(14)) / (OT!(1) + OT!(3) - OT!(13)): TRC$(2) = TM$
    End If

    If (OT!(6) + OT!(2) + OT!(14)) / numberGames% > TT!(CA, 20, 5) Then

        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = OT!(1) + OT!(3) - OT!(13)
        TT!(CA, 20, 3) = OT!(6) + OT!(2) + OT!(14)
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

    2506 'CATEGORY 2 - TOTAL DEFENSE
    CA = 2

    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2) + DT!(14)) / numberGames% < TRC!(25) Then TRC!(25) = (DT!(6) + DT!(2) + DT!(14)) / numberGames%: TRC$(25) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) + DT!(2) + DT!(14) < TRC!(26) Then TRC!(26) = DT!(6) + DT!(2) + DT!(14): TRC$(26) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2) + DT!(14)) / (DT!(1) + DT!(3) - DT!(13)) < TRC!(27) Then TRC!(27) = (DT!(6) + DT!(2) + DT!(14)) / (DT!(1) + DT!(3) - DT!(13)): TRC$(27) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(6) + DT!(2) + DT!(14)) / numberGames% > TRC!(28) Then TRC!(28) = (DT!(6) + DT!(2) + DT!(14)) / numberGames%: TRC$(28) = TM$

    If (DT!(6) + DT!(2) + DT!(14)) / numberGames% < TT!(CA, 20, 5) Then
        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = DT!(1) + DT!(3) - DT!(13)
        TT!(CA, 20, 3) = DT!(6) + DT!(2) + DT!(14)
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

    2507 'CATEGORY 3 - RUSHING OFFENSE
    CA = 3

    If UCase$(finalUpdate$) = "Y" And (OT!(2) + OT!(14)) / numberGames% > TRC!(3) Then TRC!(3) = (OT!(2) + OT!(14)) / numberGames%: TRC$(3) = TM$
    If OT!(2) + OT!(14) > TRC!(4) Then TRC!(4) = OT!(2) + OT!(14): TRC$(4) = TM$
    If UCase$(finalUpdate$) = "Y" And (OT!(2) + OT!(14)) / (OT!(1) - OT!(13)) > TRC!(5) Then TRC!(5) = (OT!(2) + OT!(14)) / (OT!(1) - OT!(13)): TRC$(5) = TM$

    If (OT!(2) + OT!(14)) / numberGames% > TT!(CA, 20, 5) Then
        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = OT!(1) - OT!(13)
        TT!(CA, 20, 3) = OT!(2) + OT!(14)
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

    2516 'CATEGORY 4 - RUSHING DEFENSE
    CA = 4

    If UCase$(finalUpdate$) = "Y" And (DT!(2) + DT!(14)) / numberGames% < TRC!(29) Then TRC!(29) = (DT!(2) + DT!(14)) / numberGames%: TRC$(29) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(2) + DT!(14) < TRC!(30) Then TRC!(30) = DT!(2) + DT!(14): TRC$(30) = TM$
    If UCase$(finalUpdate$) = "Y" And (DT!(2) + DT!(14)) / (DT!(1) - DT!(13)) < TRC!(31) Then TRC!(31) = (DT!(2) + DT!(14)) / (DT!(1) - DT!(13)): TRC$(31) = TM$

    If (DT!(2) + DT!(14)) / numberGames% < TT!(CA, 20, 5) Then
        TT!(CA, 20, 1) = numberGames%
        TT!(CA, 20, 2) = DT!(1) - DT!(13)
        TT!(CA, 20, 3) = DT!(2) + DT!(14)
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

    2521 'CATEGORY 5 - SCORING OFFENSE
    CA = 5
    PTS = 0
    OPTS = 0

    For I = 1 To numberGames%
        PTS = PTS + O%(I)
        OPTS = OPTS + T%(I)
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

    2526 'CATEGORY 6 - SCORING DEFENSE
    CA = 6

    PTS = 0

    For I = 1 To numberGames%
        PTS = PTS + T%(I)
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

    2531 'CATEGORY 7 - PASSING OFFENSE
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

    2536 'CATEGORY 8 - PASSING DEFENSE
    CA = 8

    If UCase$(finalUpdate$) = "Y" And DT!(6) / numberGames% < TRC!(32) Then TRC!(32) = DT!(6) / numberGames%: TRC$(32) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) < TRC!(33) Then TRC!(33) = DT!(6): TRC$(33) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) / DT!(3) < TRC!(34) Then TRC!(34) = DT!(6) / DT!(3): TRC$(34) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(6) / DT!(4) < TRC!(35) Then TRC!(35) = DT!(6) / DT!(4): TRC$(35) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(4) / DT!(3) < TRC!(36) Then TRC!(36) = DT!(4) / DT!(3): TRC$(36) = TM$
    If UCase$(finalUpdate$) = "Y" And DT!(5) / DT!(3) > TRC!(37) Then TRC!(37) = DT!(5) / DT!(3): TRC$(37) = TM$

    If DT!(6) / numberGames% >= TT!(CA, 20, 8) Then 2541
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

    2541 'CATEGORY 9 - PUNTING
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

    2546 'CATEGORY 10 - TURNOVER MARGIN
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

    2599 'PUNT RETURNS
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
'       CheckStatsPro Subroutine
'----------------------------------------
'...explanation...
Sub CheckStatsPro (numberGames%, TM$, WT$, finalUpdate$)

    Shared PN
    Shared PU$

    Shared BRC$()
    Shared compZ2$(), compZ3$()
    Shared DI$(), DS$(), KR$()
    Shared LC$(), LF$(), LI$(), LK$(), LP$(), LR$()
    Shared PKR$(), RB$(), QB$(), PR$(), compPK$(), PS$()
    Shared TB$(), WR$()

    Shared BRC!(), compZ1!()
    Shared EA!(), EM!(), EP!()
    Shared FA!(), FG!(), FM!(), KR!()
    Shared LC!(), LF!(), LI!(), LK!(), LP!(), LR!()
    Shared PK!(), PR!(), QB!(), RB!()
    Shared TG!(), TP!(), WR!(), YD!()

    Shared GI%(), GS%()

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

            PS$(X) = WT$

            For Y = 1 To 20:
                If compZ2$(Y) = RB$(X) And compZ3$(Y) = WT$ Then
                    4818 '
                    compZ1!(Y, 1) = RB!(X, 9)
                    compZ1!(Y, 2) = RB!(X, 1)

                    If compZ1!(Y, 2) < RA Then
                        compZ2$(Y) = "*"
                        compZ3$(Y) = "*"
                        compZ1!(Y, 1) = 0
                        compZ1!(Y, 2) = 0
                    End If

                    4820 '
                    For Y = 1 To 19:

                        For Z = Y + 1 To 20:

                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                TN$ = compZ3$(Y): compZ3$(Y) = compZ3$(Z)
                                compZ3$(Z) = TN$
                                TN$ = compZ2$(Y): compZ2$(Y) = compZ2$(Z)
                                compZ2$(Z) = TN$

                                For Z1 = 1 To 2:
                                    J = compZ1!(Y, Z1): compZ1!(Y, Z1) = compZ1!(Z, Z1): compZ1!(Z, Z1) = J
                                Next Z1

                            End If

                        Next Z

                    Next Y
                    GoTo 3118
                End If
            Next Y

            If RB!(X, 1) >= numberGames% * 10 And RB!(X, 9) >= compZ1!(20, 1) Then
                compZ2$(20) = RB$(X)
                compZ3$(20) = WT$
                compZ1!(20, 1) = RB!(X, 9)
                compZ1!(20, 2) = RB!(X, 1)
                For Y = 1 To 19:

                    For Z = Y + 1 To 20:

                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            TN$ = compZ3$(Y): compZ3$(Y) = compZ3$(Z)
                            compZ3$(Z) = TN$
                            TN$ = compZ2$(Y): compZ2$(Y) = compZ2$(Z)
                            compZ2$(Z) = TN$

                            For Z1 = 1 To 2:
                                J = compZ1!(Y, Z1): compZ1!(Y, Z1) = compZ1!(Z, Z1): compZ1!(Z, Z1) = J
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

            3150 For Z = 61 To 70
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

                '3162
                3520 '
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

        End If

    Next X

    '---------------
    ' Receivers
    '---------------
    For X = 1 To 16

        Print "   CHECKING STATS OF "; WR$(X)

        If WR$(X) <> "OTHERS" Then

            If WR!(X, 1) > BRC!(5) Then BRC!(5) = WR!(X, 1): BRC$(5, 0) = WR$(X): BRC$(5, 1) = TM$
            If WR!(X, 2) > BRC!(6) Then BRC!(6) = WR!(X, 2): BRC$(6, 0) = WR$(X): BRC$(6, 1) = TM$
            If WR!(X, 3) > BRC!(7) Then BRC!(7) = WR!(X, 3): BRC$(7, 0) = WR$(X): BRC$(7, 1) = TM$

            If WR!(X, 4) > BRC!(8) And UCase$(finalUpdate$) = "Y" And WR!(X, 1) >= numberGames% * 3 Then
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
                    4825 '
                    compZ1!(Z, 1) = WR!(X, 4)
                    compZ1!(Z, 2) = WR!(X, 1)

                    If compZ1!(Z, 2) < RC Then
                        compZ2$(Z) = "*"
                        compZ3$(Z) = "*"
                        compZ1!(Z, 1) = 0
                        compZ1!(Z, 2) = 0
                    End If

                    4827 '
                    For Y = 21 To 39:
                        For Z = Y + 1 To 40:
                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                TN$ = compZ3$(Y)
                                compZ3$(Y) = compZ3$(Z)
                                compZ3$(Z) = TN$
                                TN$ = compZ2$(Y)
                                compZ2$(Y) = compZ2$(Z)
                                compZ2$(Z) = TN$

                                For Z1 = 1 To 2:
                                    J = compZ1!(Y, Z1)
                                    compZ1!(Y, Z1) = compZ1!(Z, Z1)
                                    compZ1!(Z, Z1) = J
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3178
                End If
            Next Z

            If WR!(X, 1) >= numberGames% * 3 And WR!(X, 4) >= compZ1!(40, 1) Then
                compZ2$(40) = WR$(X)
                compZ3$(40) = WT$
                compZ1!(40, 1) = WR!(X, 4)
                compZ1!(40, 2) = WR!(X, 1)
                GoSub 4827
            End If

            3178 '
            For Z = 51 To 60
                If compZ2$(Z) = WR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = Int(WR!(X, 2))
                    GoTo 3183
                End If
            Next Z

            If Int(WR!(X, 2)) > compZ1!(60, 1) Then
                compZ1!(60, 1) = Int(WR!(X, 2)): compZ2$(60) = WR$(X)
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
                    For Z1 = Z + 1 To 100
                        If compZ1!(Z, 1) < compZ1!(Z1, 1) Then
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
    ' ???
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

            3255 '
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

    3500 '
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
            If LP!(X - 1) > BRC!(17) Then BRC!(17) = LP!(X - 1): BRC$(17, 0) = QB$(X): BRC$(17, 1) = TM$: BRC$(17, 2) = LP$(X - 1, 0)

            For Y = 121 To 130:
                If compZ2$(Y) = QB$(X) And compZ3$(Y) = WT$ Then
                    4832 '
                    compZ1!(Y, 1) = QB!(X, 6)
                    compZ1!(Y, 2) = QB!(X, 1)

                    If compZ1!(Y, 2) < PA Then
                        compZ2$(Y) = "*"
                        compZ3$(Y) = "*"
                        compZ1!(Y, 1) = 0
                        compZ1!(Y, 2) = 0
                    End If

                    4834 '
                    For Y = 121 To 129:
                        For Z = Y + 1 To 130:
                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                TN$ = compZ3$(Y)
                                compZ3$(Y) = compZ3$(Z)
                                compZ3$(Z) = TN$
                                TN$ = compZ2$(Y)
                                compZ2$(Y) = compZ2$(Z)
                                compZ2$(Z) = TN$

                                For Z1 = 1 To 2:
                                    J = compZ1!(Y, Z1)
                                    compZ1!(Y, Z1) = compZ1!(Z, Z1)
                                    compZ1!(Z, Z1) = J
                                Next Z1

                            End If
                        Next Z
                    Next Y

                    GoTo 3279
                End If
            Next Y

            If QB!(X, 1) >= numberGames% * 15 And QB!(X, 6) >= compZ1!(130, 1) Then
                compZ2$(130) = QB$(X)
                compZ3$(130) = WT$: compZ1!(130, 1) = QB!(X, 6)
                compZ1!(130, 2) = QB!(X, 1)
                For Y = 121 To 129:
                    For Z = Y + 1 To 130:
                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            TN$ = compZ3$(Y)
                            compZ3$(Y) = compZ3$(Z)
                            compZ3$(Z) = TN$
                            TN$ = compZ2$(Y)
                            compZ2$(Y) = compZ2$(Z)
                            compZ2$(Z) = TN$

                            For Z1 = 1 To 2:
                                J = compZ1!(Y, Z1)
                                compZ1!(Y, Z1) = compZ1!(Z, Z1)
                                compZ1!(Z, Z1) = J
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
                    If compZ1!(Y, 2) < PA Then
                        compZ2$(Y) = "*"
                        compZ3$(Y) = "*"
                        compZ1!(Y, 1) = 0
                        compZ1!(Y, 2) = 0
                    End If

                    4841 '
                    For Y = 131 To 139:
                        For Z = Y + 1 To 140:
                            If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                                TN$ = compZ3$(Y)
                                compZ3$(Y) = compZ3$(Z)
                                compZ3$(Z) = TN$
                                TN$ = compZ2$(Y)
                                compZ2$(Y) = compZ2$(Z)
                                compZ2$(Z) = TN$

                                For Z1 = 1 To 2:
                                    J = compZ1!(Y, Z1)
                                    compZ1!(Y, Z1) = compZ1!(Z, Z1)
                                    compZ1!(Z, Z1) = J
                                Next Z1
                            End If
                        Next Z
                    Next Y

                    GoTo 3287
                End If
            Next Y

            If QB!(X, 1) >= numberGames% * 15 And QB!(X, 7) >= compZ1!(140, 1) Then
                compZ2$(140) = QB$(X)
                compZ3$(140) = WT$
                compZ1!(140, 1) = QB!(X, 7)
                compZ1!(140, 2) = QB!(X, 1)
                For Y = 131 To 139:
                    For Z = Y + 1 To 140:
                        If compZ1!(Y, 1) < compZ1!(Z, 1) Then
                            TN$ = compZ3$(Y)
                            compZ3$(Y) = compZ3$(Z)
                            compZ3$(Z) = TN$
                            TN$ = compZ2$(Y)
                            compZ2$(Y) = compZ2$(Z)
                            compZ2$(Z) = TN$

                            For Z1 = 1 To 2:
                                J = compZ1!(Y, Z1)
                                compZ1!(Y, Z1) = compZ1!(Z, Z1)
                                compZ1!(Z, Z1) = J
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

            3302 '
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
            If LK!(X - 1) > BRC!(26) Then BRC!(26) = LK!(X - 1): BRC$(26, 0) = KR$(X): BRC$(26, 1) = TM$: BRC$(26, 2) = LK$(X - 1, 0)

            For Z = 161 To 170
                If compZ2$(Z) = KR$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = KR!(X, 4)
                    compZ1!(Z, 2) = KR!(X, 1)
                    GoTo 3345
                End If
            Next Z

            If KR!(X, 1) >= numberGames% * 1.2 And KR!(X, 4) > compZ1!(170, 1) Then
                compZ1!(170, 1) = KR!(X, 4): compZ2$(170) = KR$(X)
                compZ3$(170) = WT$: compZ1!(170, 2) = KR!(X, 1)

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
                If compZ2$(Z) = PR$(X) And compZ3$(Z) = WT$ Then compZ1!(Z, 1) = PR!(X, 4): compZ1!(Z, 2) = PR!(X, 1): GoTo 3365
            Next Z

            If PR!(X, 1) >= numberGames% * 1.2 And PR!(X, 4) > compZ1!(180, 1) Then
                compZ1!(180, 1) = PR!(X, 4)
                compZ2$(180) = PR$(X)
                compZ3$(180) = WT$: compZ1!(180, 2) = PR!(X, 1)

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
    If PN > BRC!(34) And UCase$(finalUpdate$) = "Y" Then BRC!(34) = PN: BRC$(34, 0) = PU$: BRC$(34, 1) = TM$

    For Z = 181 To 190
        If compZ2$(Z) = PU$ And compZ3$(Z) = WT$ Then
            compZ1!(Z, 1) = PN
            compZ1!(Z, 2) = TG!(8)
            GoTo 3382
        End If
    Next Z

    If PN > compZ1!(190, 1) Then

        compZ1!(190, 1) = PN
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
        If LF!(X - 1) > BRC!(38) Then BRC!(38) = LF!(X - 1): BRC$(38, 0) = compPK$(X): BRC$(38, 1) = TM$: BRC$(38, 2) = LF$(X - 1)
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

        3403 'PRINT"   CHECKING STATS OF ";EP$
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

        If Left$(DI$(X), 1) <> " " And DI$(X) <> "" And DI$(X) <> "END" Then

            If GI%(X, 0) > BRC!(42) Then BRC!(42) = GI%(X, 0): BRC$(42, 0) = DI$(X): BRC$(42, 1) = TM$
            If GI%(X, 1) > BRC!(43) Then BRC!(43) = GI%(X, 1): BRC$(43, 0) = DI$(X): BRC$(43, 1) = TM$
            If GI%(X, 2) > BRC!(44) Then BRC!(44) = GI%(X, 2): BRC$(44, 0) = DI$(X): BRC$(44, 1) = TM$
            If X > 0 Then
                If LI!(X - 1) > BRC!(45) Then
                    BRC!(45) = LI!(X - 1)
                    BRC$(45, 0) = DI$(X)
                    BRC$(45, 1) = TM$
                    BRC$(45, 2) = LI$(X - 1, 0)
                End If
            End If

            For Z = 221 To 230
                If compZ2$(Z) = DI$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = GI%(X, 0)
                    GoTo 3422
                End If
            Next Z

            If GI%(X, 0) > compZ1!(230, 1) Then

                compZ1!(230, 1) = GI%(X, 0)
                compZ2$(230) = DI$(X)
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

        If Left$(DS$(X), 1) <> " " And DS$(X) <> "" And DS$(X) <> "END" Then

            If GS%(X, 0) > BRC!(46) Then BRC!(46) = GS%(X, 0): BRC$(46, 0) = DS$(X): BRC$(46, 1) = TM$

            For Z = 231 To 240
                If compZ2$(Z) = DS$(X) And compZ3$(Z) = WT$ Then
                    compZ1!(Z, 1) = GS%(X, 0)
                    GoTo 3434
                End If
            Next Z

            If GS%(X, 0) > compZ1!(240, 1) Then
                compZ1!(240, 1) = GS%(X, 0)
                compZ2$(240) = DS$(X)
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
'       QBCalculations Subroutine
'----------------------------------------
'...explanation...
Sub QBCalculations (I)

    Dim PP!(4), QT!(4), II!(4), YY!(4)
    Shared QB!()

    If QB!(I, 1) = 0 Then
        QB!(I, 6) = 0
        QB!(I, 8) = 0
        QB!(I, 9) = 0
        QB!(I, 10) = 0
    Else
        QB!(I, 8) = Int(QB!(I, 5) / QB!(I, 1) * 1000) / 10
        QB!(I, 9) = Int(QB!(I, 3) / QB!(I, 1) * 1000) / 10
        QB!(I, 10) = Int(QB!(I, 4) / QB!(I, 1) * 100) / 100
    End If

    PP!(I) = QB!(I, 6) - 30
    If PP!(I) < 0 Then
        PP!(I) = 0
    Else
        If PP!(I) > 77.5 Then
            PP!(I) = 2.375
        Else
            PP!(I) = PP!(I) * .05
        End If
    End If

    If QB!(I, 8) > 11.9 Then
        QT!(I) = 2.375
    Else
        QT!(I) = QB!(I, 8) * .2
    End If

    If QB!(I, 9) = 0 Then
        II!(I) = 2.375
    Else
        If QB!(I, 9) > 9.5 Then
            II!(I) = 0
        Else
            II!(I) = 2.375 - (.25 * QB!(I, 9))
        End If
    End If

    QB!(I, 10) = QB!(I, 10) - 3

    If QB!(I, 10) < 0 Then
        YY!(I) = 0
    Else
        If QB!(I, 10) > 12.5 Then
            YY!(I) = 2.375
        Else
            D = Int(QB!(I, 10) * 100) / 2
            If D = QB!(I, 10) / 2 Then
                YY!(I) = QB!(I, 10) / 2 * .3 + QB!(I, 10) / 2 * .2
            Else
                YY!(I) = QB!(I, 10) / 2 * .3 + QB!(I, 10) / 2 * .2 + .003
            End If
        End If
    End If

    QB!(I, 7) = PP!(I) + QT!(I) + II!(I) + YY!(I)

    If QB!(I, 7) >= 9.5 Then
        QB!(I, 7) = 158.3
    Else
        QB!(I, 7) = QB!(I, 7) * 50 / 3
    End If

    QB!(I, 7) = Int(QB!(I, 7) * 10) / 10

End Sub

' This does not appear to be referenced / used
'   L = Len(TN$): WX$ = ""
'   For X = 1 To (L - 3):
'       A = Asc(Mid$(TN$, X, 1))
'       WT$ = WT$ + Chr$(A)
'   Next X
'   TN$ = WT$
'   Return
