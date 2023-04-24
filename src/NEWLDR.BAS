Sub NEWLDR ()

    Shared TYY$()

    _Title "3-in-1 Football - View Stat Leaders"

    ReDim menuChoices$(1 To 4)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ, totStats

    Do

        Cls

        Call TITLE

        menuChoices$(1) = "VIEW EXPANDED PRO LEAGUE FILES  "
        menuChoices$(2) = "VIEW EXPANDED COLLEGE CONFERENCE"
        menuChoices$(3) = "RETURN TO LEAGUE LEADER MENU    "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1, 2:
                    Call ViewExpanded(Selection%)
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'------------------------------
'    ViewExpanded Subroutine
'------------------------------
' ...explanation...
Sub ViewExpanded (statType%)

    Dim printOrView%, PPP, sortType%
    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ, totStats

    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared expCategories$(), lookyTT$()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), PKL$()
    Shared QBL$(), TMM$(), TPP$(), Z1$()

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()
    Shared lookyTT!(), TYP!()

    Shared Y%()

    Dim CT%

    CT% = statType%

    printOrView% = 0

    Call ChooseConference_Expanded(targetFile$, menuExit%)

    If menuExit% <> 27 Then

        Erase Z1$

        For X = 1 To 30
            Z1$(X) = "XXX"
        Next

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
        totStats = 0

        LGF$ = ""
        div1$ = ""
        div2$ = ""
        div3$ = ""

        Erase TMM$, TPP$

        LG$ = targetFile$

        Open diskPaths$(2) + targetFile$ + ".TMS" For Input As #2

        If statType% = 1 Then
            Input #2, LGF$
            Input #2, div1$
            Input #2, div2$
            Input #2, div3$
            For X = 1 To 30: Input #2, Z1$(X): Next
        End If

        If statType% = 2 Then
            Input #2, LGF$
            Input #2, div1$
            Input #2, div2$
            For X = 1 To 30: Input #2, Z1$(X): Next
        End If

        Close 2

        Cls

        Call BUILDSTATS(CT%)

        Call SAVELEADER(LG$)

        170 '
        Do

            Color 0, 0

            Cls

            J = 15

            Locate 1, 15

            Color 4, 15

            Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";

            For II = 1 To J
                Color 4, 15
                Locate 1 + II, 15
                Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
                Color 7, 4: Print String$(2, 177)
            Next

            Color 4, 15
            Locate 1 + J, 15
            Print " " + Chr$(211); String$(40, 196); Chr$(189) + " ";

            Color 7, 4: Print String$(2, 177)
            Locate , 17: Print String$(44, 177)

            RR = 0

            Locate 2, 24
            Color 4, 15: Print "LEADER SELECTION MENU"
            Print
            Locate , 20: Color 14: Print "(A) ";: Color 1: Print "VIEW RUSHING LEADERS"
            Locate , 20: Color 14: Print "(B) ";: Color 1: Print "VIEW RECEIVING LEADERS"
            Locate , 20: Color 14: Print "(C) ";: Color 1: Print "VIEW QUARTERBACK LEADERS"
            Locate , 20: Color 14: Print "(D) ";: Color 1: Print "VIEW KICKOFF RETURN LEADERS"
            Locate , 20: Color 14: Print "(E) ";: Color 1: Print "VIEW PUNT RETURN LEADERS"
            Locate , 20: Color 14: Print "(F) ";: Color 1: Print "VIEW PUNTING LEADERS"
            Locate , 20: Color 14: Print "(G) ";: Color 1: Print "VIEW PLACE KICKING LEADERS"
            Locate , 20: Color 14: Print "(H) ";: Color 1: Print "VIEW INTERCEPTION, SACK LEADERS"
            Locate , 20: Color 14: Print "(I) ";: Color 1: Print "VIEW ALL-PURPOSE LEADERS"
            Locate , 20: Color 14: Print "(J) ";: Color 1: Print "VIEW SCORING LEADERS"
            Locate , 20: Color 14: Print "(K) ";: Color 1: Print "VIEW TEAM TOTALS"
            Locate , 20: Color 14: Print "(L) ";: Color 1: Print "RETURN TO MAIN MENU"

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                If UCase$(I$) <> "L" And UCase$(I$) <> "K" Then

                    Color , 0

                    Cls

                    Print
                    Print
                    Print
                    Print

                End If

                Select Case UCase$(I$)
                    Case "A":
                        J = 8
                        ZZ = 1

                        Locate 2, 22

                        Color 0, 4

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 0 To 4:
                                Color 14
                                Locate I + 3, 24
                                Print Using "##"; I;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 5

                    Case "B":
                        J = 8
                        ZZ = 1

                        Locate 2, 22

                        Color 0, 2

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 5 To 9:
                                Color 14
                                Locate I - 2, 24
                                Print Using "##"; I - 5;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 5

                    Case "C":
                        200 '
                        J = 12
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 13

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 10 To 18:
                                Color 14
                                Locate I - 7, 24
                                Print Using "##"; I - 10;: Print " ";
                                Color 15: Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 8

                    Case "D":
                        J = 8
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 5

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 19 To 23:
                                Color 14
                                Locate I - 16, 24
                                Print Using "##"; I - 19;: Print " ";
                                Color 15
                                Print expCategories$(I):
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 4

                    Case "E":

                        J = 8
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 5

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 24 To 28:
                                Color 14
                                Locate I - 21, 24
                                Print Using "##"; I - 24;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 4

                    Case "F":
                        J = 6
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 5

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 29 To 31:
                                Color 14
                                Locate I - 26, 24
                                Print Using "##"; I - 29;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 2

                    Case "G":
                        J = 10
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 10

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 32 To 38:
                                Color 14
                                Locate I - 29, 24
                                Print Using "##"; I - 32;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 6

                    Case "H":
                        J = 6
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 12

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 39 To 40:
                                Color 14
                                Locate I - 36, 24
                                Print Using "##"; I - 39;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 1

                    Case "I":
                        Color 0, 0

                        Cls

                    Case "J":
                        J = 6
                        ZZ = 1

                        Locate 2, 22
                        Color 0, 13

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 41 To 43:
                                Color 14
                                Locate I - 38, 24
                                Print Using "##"; I - 41;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 2

                    Case "K":
                        J = 17
                        ZZ = 1

                        Locate 2, 2

                        Color 0, 14

                        Call CATEGORY_BOX(J)

                        Do
                            For I = 44 To 58:
                                Color 14
                                Locate I - 41, 4
                                Print Using "##"; I - 44;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            For I = 59 To 72:
                                Color 14
                                Locate I - 56, 40
                                Print Using "##"; I - 44;: Print " ";
                                Color 15
                                Print expCategories$(I)
                            Next

                            Print
                            Print
                            Locate , 25
                            Input "SELECT NUMBER TO VIEW"; B9

                        Loop Until B9 <= 28

                End Select

                ' This is how we should do it
                If UCase$(I$) <> "I" And UCase$(I$) <> "L" And UCase$(I$) <> "K" Then
                    stpl = 0
                    PPP = 0
                End If

                If UCase$(I$) <> "L" Then
                    Print
                    Print
                    Print
                    Print

                    J = 6

                    Locate 8, 8: Color 15, 1: Print " " + Chr$(214); String$(57, 196); Chr$(183) + " ";

                    For II = 1 To J:
                        Locate 8 + II, 8
                        Print " " + Chr$(186); String$(57, 32); Chr$(186) + " ";
                    Next

                    Locate 9 + J, 8
                    Print " " + Chr$(211); String$(57, 196); Chr$(189) + " ";
                    Locate 10, 15

                    Color 3: Locate , 30: Print "SELECT OPTION:"
                    Locate , 30: Color 15: Print "H";: Color 7: Print "IGHEST TO LOWEST"
                    Locate , 30: Color 15: Print "L";: Color 7: Print "OWEST TO HIGHEST"
                    Locate , 30: Color 15: Print "R";: Color 7: Print "E-SELECT CATEGORY"

                    Do
                        J$ = GetKeyPress$

                        If UCase$(J$) = "H" Or UCase$(J$) = "L" Then

                            Print
                            Print
                            Print
                            Print
                            Print

                            J = 4

                            Locate 9, 15
                            Color 15, 0

                            Print " " + Chr$(214); String$(40, 196); Chr$(183) + " ";

                            For II = 1 To J
                                Locate 9 + II, 15: Print " " + Chr$(186); String$(40, 32); Chr$(186) + " ";
                            Next

                            Locate 10 + J, 15
                            Print " " + Chr$(211); String$(40, 196); Chr$(189) + " "
                            Locate 10, 10

                            Color 12

                            Locate , 30: Print "SELECT OPTION:"
                            Locate , 30: Color 14: Print "P";: Color 7: Print "RINT"
                            Locate , 30: Color 14: Print "V";: Color 7: Print "IEW"
                            Locate , 30: Color 14: Print "R";: Color 7: Print "E-SELECT OPTION"

                            Do
                                K$ = GetKeyPress$

                                Select Case UCase$(K$)
                                    Case "P":
                                        printOrView% = 2

                                    Case "V":
                                        printOrView% = 1

                                    Case "R", Chr$(27):
                                        If ZZ = 1 Then
                                            GoTo 170
                                            'this is part of View Expanded
                                        End If
                                End Select

                            Loop Until UCase$(K$) = "P" Or UCase$(K$) = "V" Or UCase$(K$) = "R" Or K$ = Chr$(27)

                        End If

                        Select Case UCase$(J$)
                            Case "H":
                                sortType% = 1

                            Case "L"
                                sortType% = 2

                            Case "R", Chr$(27)
                                If ZZ = 1 Then
                                    GoTo 170
                                    'this is part of View Expanded
                                End If

                        End Select

                    Loop Until UCase$(J$) = "H" Or UCase$(J$) = "L" Or UCase$(J$) = "R" Or J$ = Chr$(27)

                    Select Case UCase$(I$)

                        Case "A":

                            Select Case B9
                                Case 0:
                                    X = 0: XX = 0
                                Case 1
                                    X = 1: XX = 1
                                Case 2
                                    X = 0: X1 = 1: XX = 2
                                Case 3
                                    X = 2: XX = 3
                                Case 4
                                    X = 6: XX = 4
                            End Select

                            Cls
                            PPP = 0

                            Select Case B9
                                Case 0, 1, 3

                                    For I = 1 To JA
                                        If A1L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A1L$(I)
                                            TMM$(PPP) = A1T$(I)
                                            TYP!(PPP) = A1L!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 2
                                    For I = 1 To JA
                                        If A1L!(I, X) > Y%(1) * 10 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A1L$(I)
                                            TMM$(PPP) = A1T$(I)
                                            TYP!(PPP) = A1L!(I, X1) / A1L!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 2

                                Case 4
                                    Cls
                                    PPP = 0
                                    For I = 1 To JA
                                        If A1L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A1L$(I)
                                            TMM$(PPP) = A1T$(I)
                                            TYP!(PPP) = A1L!(I, X)
                                            TYY$(PPP) = LRL$(I)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 3

                            End Select

                            QW = PPP

                            If printOrView% = 1 Then 
								Call ExpandedStatsToScreen(XX, QW, TVN)
							Else
								'printOrView% = 2 
								Call GetListLength(printOrView%, LG, QW)
								Call PlayerToPrinter(XX, LG, TVN)
							End If

                        Case "B":
                            Select Case B9
                                Case 0: X = 3: X1 = 0: XX = 5
                                Case 1: X = 4: X1 = 1: XX = 6
                                Case 2: X = 3: X1 = 4: X2 = 0: X3 = 1: XX = 7
                                Case 3: X = 5: X1 = 2: XX = 8
                                Case 4: X = 3: X1 = 7: XX = 9

                            End Select

                            Cls
                            PPP = 0

                            Select Case B9
                                Case 0, 1, 3:
                                    For I = 1 To JA
                                        If A1L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A1L$(I)
                                            TMM$(PPP) = A1T$(I)
                                            TYP!(PPP) = A1L!(I, X)
                                        End If
                                    Next I

                                    For I = 1 To JB
                                        If A2L!(I, X1) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A2L$(I)
                                            TMM$(PPP) = A2T$(I)
                                            TYP!(PPP) = A2L!(I, X1)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 2:
                                    Cls
                                    PPP = 0

                                    For I = 1 To JA
                                        If A1L!(I, X) >= Y%(1) * 4 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A1L$(I)
                                            TMM$(PPP) = A1T$(I)
                                            TYP!(PPP) = A1L!(I, X1) / A1L!(I, X)
                                        End If
                                    Next I

                                    For I = 1 To JB
                                        If A2L!(I, X2) >= Y%(1) * 4 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A2L$(I)
                                            TMM$(PPP) = A2T$(I)
                                            TYP!(PPP) = A2L!(I, X3) / A2L!(I, X2)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 2

                                Case 3:
                                    Cls
                                    PPP = 0

                                    For I = 1 To JB
                                        If A2L!(I, 0) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A2L$(I)
                                            TMM$(PPP) = A2T$(I)
                                            TYP!(PPP) = A2L!(I, X)
                                            TYY$(PPP) = LCL$(I)
                                        End If
                                    Next I

                                    For I = 1 To JA
                                        If A1L!(I, 4) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A1L$(I)
                                            TMM$(PPP) = A1T$(I)
                                            TYP!(PPP) = A1L!(I, X1)
                                            TYY$(PPP) = LCL$(I)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 3

                            End Select

                        Case "C":
                            Select Case B9
                                Case 0: X = 0: XX = 10
                                Case 1: X = 1: XX = 11
                                Case 2: X = 2: XX = 12
                                Case 3: X = 3: XX = 13
                                Case 4: X = 4: XX = 14
                                Case 5: X = 0: X1 = 1: XX = 15
                                Case 6: X = 1: X1 = 3: X2 = 0: XX = 16
                                Case 7: X = 0: XX = 17
                                Case 8: X = 5: XX = 18

                            End Select

                            Cls
                            PPP = 0

                            Select Case B9
                                Case 0 To 4:
                                    For I = 1 To JC
                                        If A3L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = A3L!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 5:
                                    For I = 1 To JC
                                        If A3L!(I, X) >= Y%(1) * 15 Then
                                            PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            If A3L!(I, X1) <> 0 Then
                                                TPP$(PPP) = QBL$(I)
                                                TMM$(PPP) = A3T$(I)
                                                TYP!(PPP) = 0
                                            Else
                                                TPP$(PPP) = QBL$(I)
                                                TMM$(PPP) = A3T$(I)
                                                TYP!(PPP) = (A3L!(I, X1) / A3L!(I, X)) * 100
                                            End If

                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 2

                                Case 6:
                                    For I = 1 To JC
                                        If A3L!(I, X2) >= Y%(1) * 15 Then
                                            PPP = PPP + 1: Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            If A3L!(I, X1) <> 0 Then
                                                TPP$(PPP) = QBL$(I)
                                                TMM$(PPP) = A3T$(I)
                                                TYP!(PPP) = A3L!(I, X1) / A3L!(I, X)
                                            Else
                                                TPP$(PPP) = QBL$(I)
                                                TMM$(PPP) = A3T$(I)
                                                TYP!(PPP) = 0
                                            End If
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 4

                                Case 7:
                                    For I = 1 To JC
                                        If A3L!(I, X) >= Y%(1) * 15 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = A3R!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 4

                                Case 8:
                                    Cls
                                    PPP = 0

                                    For I = 1 To JC
                                        If A3L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = QBL$(I)
                                            TMM$(PPP) = A3T$(I)
                                            TYP!(PPP) = A3L!(I, X)
                                            TYY$(PPP) = LPL$(I)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 3

                            End Select

                        Case "D":
                            Select Case B9
                                Case 0: X = 0: XX = 19
                                Case 1: X = 1: XX = 20
                                Case 2: X = 0: X1 = 1: XX = 21
                                Case 3: X = 2: XX = 22
                                Case 4: X = 3: XX = 23
                            End Select

                            Cls
                            PPP = 0

                            Select Case B9
                                Case 0, 1, 3:
                                    For I = 1 To JD
                                        If A4L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A4L$(I)
                                            TMM$(PPP) = A4T$(I)
                                            TYP!(PPP) = A4L!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 2:
                                    For I = 1 To JD
                                        If A4L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            If A4L!(I, X1) <> 0 Then
                                                TPP$(PPP) = A4L$(I)
                                                TMM$(PPP) = A4T$(I)
                                                TYP!(PPP) = A4L!(I, X1) / A4L!(I, X)
                                            Else
                                                TPP$(PPP) = A4L$(I)
                                                TMM$(PPP) = A4T$(I)
                                                TYP = 0
                                            End If
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 2

                                Case 4:
                                    For I = 1 To JD
                                        If A4L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A4L$(I)
                                            TMM$(PPP) = A4T$(I)
                                            TYP!(PPP) = A4L!(I, X): TYY$(PPP) = LKL$(I)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 3

                            End Select

                        Case "E":
                            Select Case B9
                                Case 0: X = 0: XX = 24
                                Case 1: X = 1: XX = 25
                                Case 2: X = 0: X1 = 1: XX = 26
                                Case 3: X = 2: XX = 27
                                Case 4: X = 3: XX = 28
                            End Select

                            Select Case B9
                                Case 0, 1, 3:
                                    For I = 1 To JE
                                        If A5L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A5L$(I)
                                            TMM$(PPP) = A5T$(I)
                                            TYP!(PPP) = A5L!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 2:
                                    620 '
                                    For I = 1 To JE
                                        If A5L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            If A5L!(I, X1) <> 0 Then
                                                TPP$(PPP) = A5L$(I)
                                                TMM$(PPP) = A5T$(I)
                                                TYP!(PPP) = A5L!(I, X1) / A5L!(I, X)
                                            Else
                                                TPP$(PPP) = A5L$(I)
                                                TMM$(PPP) = A5T$(I)
                                                TYP = 0
                                            End If
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 2

                                Case 4:
                                    For I = 1 To JE
                                        If A5L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A5L$(I)
                                            TMM$(PPP) = A5T$(I)
                                            TYP!(PPP) = A5L!(I, X)
                                            TYY$(PPP) = PKL$(I)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                            End Select

                            Cls
                            PPP = 0

                        Case "F":
                            Select Case B9
                                Case 0: X = 0: XX = 29
                                Case 1: X = 1: XX = 30
                                Case 2: X = 0: X1 = 1: XX = 31
                            End Select

                            Select Case B9
                                Case 0, 1:
                                    For I = 1 To JF
                                        If A6L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A6L$(I)
                                            TMM$(PPP) = A6T$(I)
                                            TYP!(PPP) = A6L!(I, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 2:
                                    For I = 1 To JF
                                        If A6L!(I, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            If A6L!(I, X1) <> 0 Then
                                                TPP$(PPP) = A6L$(I)
                                                TMM$(PPP) = A6T$(I)
                                                TYP!(PPP) = A6L!(I, X1) / A6L!(I, X)
                                            Else
                                                TPP$(PPP) = A6L$(I)
                                                TMM$(PPP) = A6T$(I)
                                                TYP = 0
                                            End If
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 2

                            End Select

                        Case "G":
                            Select Case B9
                                Case 0: X = 0: XX = 32
                                Case 1: X = 1: XX = 33
                                Case 2: X = 0: X1 = 1: XX = 34
                                Case 3: X = 2: XX = 35
                                Case 4: X = 3: XX = 36
                                Case 5: X = 2: X1 = 3: XX = 37
                                Case 6: X = 4: XX = 38

                            End Select

                            Cls
                            PPP = 0

                            Select Case B9
                                Case 0, 1, 3, 4, 6
                                    For I = 1 To JG
                                        If A7L!(JI, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                            TPP$(PPP) = A7L$(I)
                                            TMM$(PPP) = A7T$(I)
                                            TYP!(PPP) = A7L!(JI, X)
                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 1

                                Case 2, 5
                                    For I = 1 To JG
                                        If A7L!(JI, X) <> 0 Then
                                            PPP = PPP + 1
                                            Locate 1, 5: Print "CHANGING PLAYER "; PPP

                                            If A7L!(JI, X1) <> 0 Then
                                                TPP$(PPP) = A7L$(I)
                                                TMM$(PPP) = A7T$(I)
                                                TYP!(PPP) = (A7L!(JI, X) / A7L!(JI, X1)) * 100
                                            Else
                                                TPP$(PPP) = A7L$(I)
                                                TMM$(PPP) = A7T$(I)
                                                TYP = 0
                                            End If

                                        End If
                                    Next I

                                    Call SortPlayer(stpl, PPP, sortType%)

                                    TVN = 4

                            End Select

                            QW = PPP

                            If printOrView% = 1 Then 
								Call ExpandedStatsToScreen(XX, QW, TVN)
							Else
								'printOrView% = 2 
								Call GetListLength(printOrView%, LG, QW)
								Call PlayerToPrinter(XX, LG, TVN)
							End If

                        Case "H":
                            If B9 = 0 Then X = 0: XX = 39
                            If B9 = 1 Then X = 0: XX = 40

                            Cls
                            PPP = 0

                            If B9 = 0 Then

                                For I = 1 To JH
                                    If A8L!(I) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A8L$(I)
                                        TMM$(PPP) = A8T$(I)
                                        TYP!(PPP) = A8L!(I)
                                    End If
                                Next I

                            End If

                            If B9 = 1 Then
                                For I = 1 To JI
                                    If A9L!(I) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A9L$(I)
                                        TMM$(PPP) = A9T$(I)
                                        TYP!(PPP) = A9L!(I)
                                    End If
                                Next I

                            End If

                            Call SortPlayer(stpl, PPP, sortType%)

                            TVN = 1
                            QW = PPP

                            If printOrView% = 1 Then 
								Call ExpandedStatsToScreen(XX, QW, TVN)
							Else
								'printOrView% = 2 
								Call GetListLength(printOrView%, LG, QW)
								Call PlayerToPrinter(XX, LG, TVN)
							End If

                        Case "I":
                            Cls

                            PPP = 0
                            X = 1
                            X1 = 4
                            XX = 73

                            For I = 1 To JA
                                If A1L!(I, X) + A1L!(I, X1) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5
                                    Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A1L$(I)
                                    TMM$(PPP) = A1T$(I)
                                    TYP!(PPP) = A1L!(I, X) + A1L!(I, X1)
                                End If
                            Next I

                            For I = 1 To JB
                                If A2L!(I, X) <> 0 Then
                                    PPP = PPP + 1
                                    Locate 1, 5
                                    Print "CHANGING PLAYER "; PPP
                                    TPP$(PPP) = A2L$(I)
                                    TMM$(PPP) = A2T$(I)
                                    TYP!(PPP) = A2L!(I, X)
                                End If
                            Next I

                            Call SortPlayer(stpl, PPP, sortType%)

                            TVN = 1
                            QW = PPP

                            If printOrView% = 1 Then 
								Call ExpandedStatsToScreen(XX, QW, TVN)
							Else
								'printOrView% = 2 
								Call GetListLength(printOrView%, LG, QW)
								Call PlayerToPrinter(XX, LG, TVN)
							End If

                        Case "J":
                            Select Case B9
                                Case 0
                                    X = 2: X1 = 5: XX = 41: KC = 1

                                Case 1
                                    XF = 0: XF1 = 2: XX = 42: KC = 2

                                Case 2
                                    X = 2: X1 = 5: XF = 0: XF1 = 2: XX = 43: KC = 3

                            End Select

                            Cls
                            PPP = 0

                            If KC = 1 Or KC = 3 Then
                                For I = 1 To JA
                                    If A1L!(I, X) + A1L!(I, X1) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A1L$(I)
                                        TMM$(PPP) = A1T$(I)
                                        TYP!(PPP) = (A1L!(I, X) + A1L!(I, X1)) * 6
                                    End If
                                Next I

                                For I = 1 To JB
                                    If A2L!(I, X) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A2L$(I)
                                        TMM$(PPP) = A2T$(I)
                                        TYP!(PPP) = A2L!(I, X) * 6
                                    End If
                                Next I

                            End If

                            If KC <> 1 Then
                                For I = 1 To JG
                                    If A7L!(JI, XF) + A7L!(JI, XF1) <> 0 Then
                                        PPP = PPP + 1
                                        Locate 1, 5: Print "CHANGING PLAYER "; PPP
                                        TPP$(PPP) = A7L$(I)
                                        TMM$(PPP) = A7T$(I)
                                        TYP!(PPP) = (A7L!(JI, XF) * 3) + A7L!(JI, XF1)
                                    End If
                                Next I
                            End If

                            Call SortPlayer(stpl, PPP, sortType%)

                            TVN = 1
                            QW = PPP

                            If printOrView% = 1 Then 
								Call ExpandedStatsToScreen(XX, QW, TVN)
							Else
								'printOrView% = 2 
								Call GetListLength(printOrView%, LG, QW)
								Call PlayerToPrinter(XX, LG, TVN)
							End If

                        Case "K":
                            Select Case B9
                                Case 0: X = 0: XX = 44
                                Case 1: X = 1: X1 = 13: XX = 45
                                Case 2: X = 2: X1 = 14: XX = 46
                                Case 3: X = 1: X1 = 13: X2 = 2: X3 = 14: XX = 47
                                Case 4: X = 6: XX = 48
                                Case 5: X = 2: X1 = 6: XX = 49
                                Case 6: X = 4: X1 = 3: XX = 50
                                Case 7: X = 9: XX = 51
                                Case 8: X = 10: XX = 52
                                Case 9: X = 5: XX = 53
                                Case 10: X = 5: X1 = 10: X2 = 20: X3 = 25: XX = 54
                                Case 11: X = 11: XX = 55
                                Case 12: X = 12: XX = 56
                                Case 13: X = 13: XX = 57
                                Case 14: X = 14: XX = 58
                                Case 15: X = 15: XX = 59
                                Case 16: X = 16: X1 = 28: XX = 60
                                Case 17: X = 17: X1 = 29: XX = 61
                                Case 18: X = 16: X1 = 28: X2 = 17: X3 = 29: XX = 62
                                Case 19: X = 21: XX = 63
                                Case 20: X = 17: X1 = 21: XX = 64
                                Case 21: X = 19: X1 = 18: XX = 65
                                Case 22: X = 24: XX = 66
                                Case 23: X = 25: XX = 67
                                Case 24: X = 20: XX = 68
                                Case 25: X = 26: XX = 69
                                Case 26: X = 27: XX = 70
                                Case 27: X = 28: XX = 71
                                Case 28: X = 29: XX = 72
                            End Select

                            Select Case B9
                                Case 0, 4, 7 To 9, 11 To 15, 19, 22 To 28
                                    For I = 1 To totStats
                                        TMM$(I) = lookyTT$(I, X): TYP!(I) = lookyTT!(I, X)
                                    Next I

                                    Call SortTeam(totStats, stpl, sortType%)

                                    TVN = 1

                                Case 1, 16
                                    For I = 1 To totStats
                                        TMM$(I) = lookyTT$(I, X): TYP!(I) = lookyTT!(I, X) - lookyTT!(I, X1)
                                    Next I

                                    Call SortTeam(totStats, stpl, sortType%)

                                    TVN = 1

                                Case 2, 5, 17, 20
                                    For I = 1 To totStats
                                        TMM$(I) = lookyTT$(I, X): TYP!(I) = lookyTT!(I, X) + lookyTT!(I, X1)
                                    Next I

                                    Call SortTeam(totStats, stpl, sortType%)

                                    TVN = 1

                                Case 3, 18
                                    For I = 1 To totStats
                                        TMM$(I) = lookyTT$(I, X): TYP!(I) = (lookyTT!(I, X2) + lookyTT!(I, X3)) / (lookyTT!(I, X) - lookyTT!(I, X1))
                                    Next I

                                    Call SortTeam(totStats, stpl, sortType%)

                                    TVN = 2

                                Case 6, 21
                                    For I = 1 To totStats
                                        TMM$(I) = lookyTT$(I, X): TYP!(I) = (lookyTT!(I, X) / lookyTT!(I, X1)) * 100
                                    Next I

                                    Call SortTeam(totStats, stpl, sortType%)

                                    TVN = 2

                                Case 10
                                    For I = 1 To totStats
                                        TMM$(I) = lookyTT$(I, X): TYP!(I) = lookyTT!(I, X2) + lookyTT!(I, X3) - lookyTT!(I, X) - lookyTT!(I, X1)
                                    Next I

                                    Call SortTeam(totStats, stpl, sortType%)

                                    TVN = 1

                            End Select

                            QW = totStats

                            If printOrView% = 1 Then 
								Call ExpandedStatsToScreen(XX, QW, TVN)
							Else
								'printOrView% = 2 
								Call GetListLength(printOrView%, LG, QW)
								Call PlayerToPrinter(XX, LG, TVN)
							End If

                    End Select

                End If

            End If

        Loop Until UCase$(I$) = "L" Or I$ = Chr$(27)

    End If

End Sub

'----------------------------------------
'    TeamStatsToPrinter Subroutine
'----------------------------------------
'...explanation...
Sub TeamStatsToPrinter ()

    Shared expCategories$(), TMM$()
    Shared TYP!()

    Do

        Cls

        J = 18
        Locate 1, 5: Color 15, 4

        Print " " + Chr$(214); String$(67, 196); Chr$(183) + " ";

        For II = 1 To J
            Color 15, 4
            Locate 1 + II, 5
            Print " " + Chr$(186); String$(67, 32); Chr$(186) + " ";
            Color 7, 4: Print String$(2, 177)
        Next

        Color 15, 4: Locate 1 + J, 5
        Print " " + Chr$(211); String$(67, 196); Chr$(189) + " ";

        Color 7, 4: Print String$(2, 177)
        Locate , 7: Print String$(71, 177)

        Color 14, 4
        R = Len(expCategories$(XX))
        Locate 2, 40 - (Int(R) / 2): Print expCategories$(XX)

        Locate 3, 11

        Color 14, 4: Print "TEAM                               TEAM "

        For W = 1 To 15
            If TVN = 1 Then
                If TMM$(W) <> "" Then
                    Locate W + 3, 7
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(28); Using "#####"; TYP!(W)
                End If
            End If

            If TVN = 2 Then
                If TMM$(W) <> "" Then
                    Locate W + 3, 7
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(29); Using "##.#"; TYP!(W)
                End If
            End If
        Next W

        For W = 16 To 30
            If TVN = 1 Then
                If TMM$(W) <> "" Then
                    Locate W - 12, 42
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(63); Using "#####"; TYP!(W)
                End If
            End If

            If TVN = 2 Then
                If TMM$(W) <> "" Then
                    Locate W - 12, 42
                    Color 12
                    Print Using "###"; W;: Print " ";
                    Color 15: Print TMM$(W);
                    Print Tab(64); Using "##.#"; TYP!(W)
                End If
            End If

        Next W

        Locate 22, 17
        Color 0, 15: Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";

        Color 4, 0: Print Chr$(220)
        Locate , 18: Print String$(50, 223)
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            Color 0, 0
            Locate 22: Print Space$(68)
            Locate 23: Print Space$(68)

            Do
                Locate 22, 32
                Color 0, 15
                Print "VIEW AGAIN (Y/N)";

                Color 4, 0
                Print Chr$(220)
                Locate , 33
                Print String$(16, 223)

                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                RR = 0
            End If

        End If

    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub

'----------------------------------------
'       CategoryToPrinter Subroutine
'----------------------------------------
'...explanation...
Sub CategoryToPrinter ()

    Shared expCategories$(), TMM$(), TYY$()
    Shared TYP!()

    LPrint "     TEAM                     "; expCategories$(XX)

    LPrint Using "###"; W;: LPrint " ";: LPrint TMM$(W);

    For W = 1 To LG

        Select Case TVN

            Case 1
                LPrint Tab(35); Using "#####"; TYP!(W)

            Case 2
                LPrint Tab(35); Using "###.##"; TYP!(W)

            Case 3
                LPrint Tab(35); Using "###.#"; TYP!(W);: LPrint Tab(63); TYY$(W)

            Case 4
                LPrint Tab(35); Using "###.#"; TYP!(W)

        End Select

    Next W

    LPrint Chr$(12)

End Sub

'----------------------------------------
'    ExpandedStatsToScreen Subroutine
'----------------------------------------
'...explanation...
Sub ExpandedStatsToScreen (XX, QW, TVN)

    Shared expCategories$(), TMM$(), TPP$(), TYY$()
    Shared TYP!()

    Do
        '10000
        Cls
        J = 18

        Locate 1, 5: Color 15, 1

        Print " " + Chr$(214); String$(67, 196); Chr$(183) + " ";

        For II = 1 To J
            Color 15, 1
            Locate 1 + II, 5
            Print " " + Chr$(186); String$(67, 32); Chr$(186) + " ";
            Color 7, 1: Print String$(2, 177)
        Next

        Color 15, 1
        Locate 1 + J, 5
        Print " " + Chr$(211); String$(67, 196); Chr$(189) + " ";

        Color 7, 1: Print String$(2, 177)
        Locate , 7: Print String$(71, 177)

        R = Len(expCategories$(XX))

        Locate 2, 11

        Color 14, 1: Print "PLAYER             TEAM";

        Locate , 60 - (Int(R) / 2): Print expCategories$(XX)

        I$ = ""

        For W = 1 To QW

            RR = RR + 1

            If W <> 1 And (W Mod 16 = 1) Then

                RR = 1
                Locate 22, 17: Color 0, 15
                Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";
                Color 1, 0: Print Chr$(220)

                Locate , 18: Print String$(50, 223)

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then

                    For I = 3 To 18:
                        Color 1, 1
                        Locate I, 7
                        Print Space$(66)
                    Next

                End If

            End If

            If I$ <> Chr$(27) Then

                Locate RR + 2, 7: Color 15
                Print Using "###"; W;: Print " ";
                Color 3

                Select Case TVN

                    Case 1:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "#####"; TYP!(W)

                    Case 2:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.##"; TYP!(W)

                    Case 3:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.#"; TYP!(W);: Print Tab(63); TYY$(W)

                    Case 4:
                        Print TPP$(W);: Print Tab(30); TMM$(W);: Print Tab(58); Using "###.#"; TYP!(W)

                End Select

            Else

                Exit For

            End If

        Next W

        If I$ <> Chr$(27) Then

            Locate 22, 17
            Color 0, 15
            Print "HIT ENTER TO CONTINUE (ESC TO RETURN TO MAIN MENU)";

            Color 1, 0: Print Chr$(220)
            Locate , 18: Print String$(50, 223)

            I$ = GetKeyPress$

            If I$ <> Chr$(27) Then

                Color 0, 0
                Locate 22: Print Space$(69)
                Locate 23: Print Space$(69)

                Do
                    Locate 22, 32
                    Color 0, 15
                    Print "VIEW AGAIN (Y/N)";

                    Color 1, 0
                    Print Chr$(220)
                    Locate , 33
                    Print String$(16, 223)

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
'    PlayerToPrinter Subroutine
'----------------------------------------
'...explanation...
Sub PlayerToPrinter (XX, LG, TVN)

    Shared expCategories$(), TMM$(), TPP$(), TYY$()
    Shared TYP!()

	Color 0, 15

    LPrint "     PLAYER                  TEAM                     "; expCategories$(XX)

    For W = 1 To LG

        If (W <> 0 And W Mod 50 = 0) Then
            LPrint Chr$(12)
            LPrint "     PLAYER                  TEAM                     "; expCategories$(XX)

        End If

        LPrint Using "###"; W;: LPrint " ";: LPrint TPP$(W);: LPrint Tab(25); TMM$(W);

        Select Case TVN
            Case 1
                LPrint Tab(58); Using "#####"; TYP!(W)
            Case 2
                LPrint Tab(58); Using "###.##"; TYP!(W)
            Case 3
                LPrint Tab(58); Using "###.#"; TYP!(W);: LPrint Tab(63); TYY$(W)
            Case 4
                LPrint Tab(58); Using "###.#"; TYP!(W)

        End Select

    Next W

    LPrint Chr$(12)

End Sub

'----------------------------------------
'       SortPlayer Subroutine
'----------------------------------------
'...explanation...
Sub SortPlayer (stpl, PPP, sortType%)

    Shared TMM$(), TPP$(), TYY$()
    Shared TYP!()

    For I = 2 To PPP

        stpl = stpl + 1

        Locate 5, 5: Print "sorting player "; stpl

        For J = PPP To I Step -1

            If sortType% = 1 Then
                If TYP!(J - 1) < TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TYY$(J - 1), TYY$(J)
                End If
            End If

            If sortType% = 2 Then
                If TYP!(J - 1) > TYP!(J) Then
                    Swap TPP$(J - 1), TPP$(J)
                    Swap TMM$(J - 1), TMM$(J)
                    Swap TYP!(J - 1), TYP!(J)
                    Swap TYY$(J - 1), TYY$(J)
                End If
            End If

        Next J

    Next I

End Sub

'----------------------------------------
'       SortTeam Subroutine
'----------------------------------------
'...explanation...
Sub SortTeam (sortMax, stpl, sortType%)

    Shared TMM$()
    Shared TYP!()

    Cls

    For I = 2 To sortMax

        stpl = stpl + 1

        Locate 5, 5: Print "sorting team "; stpl

        For J = sortMax To I Step -1
            If sortType% = 1 Then
                If TYP!(J - 1) < TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            End If

            If sortType% = 2 Then
                If TYP!(J - 1) > TYP!(J) Then Swap TMM$(J - 1), TMM$(J): Swap TYP!(J - 1), TYP!(J)
            End If

        Next J

    Next I

End Sub

'------------------------------
'  GetListLength Subroutine
'------------------------------
' ...explanation...
Sub GetListLength (printOrView%, listLength, maxItems)

    Color , 0
    Cls

    If printOrView% <> 1 Then
		'If printing...

        Do
            Print
            Print
            Print
            Print
            Print
            J = 2

            Locate 10, 20
            Color 4, 15
            Print " " + Chr$(214); String$(30, 196); Chr$(183) + " ";
            For II = 1 To J:
                Locate 10 + II, 20: Print " " + Chr$(186); String$(30, 32); Chr$(186) + " ";
            Next

            Locate 11 + J, 20
            Print " " + Chr$(211); String$(30, 196); Chr$(189) + " "
            Locate 11

            Locate , 25: Color 1: Print "MAXIMUM = "; maxItems
            Locate , 25: Input "ENTER LENGTH OF LIST"; listLength

        Loop Until listLength <= maxItems

    End If

End Sub

'------------------------------
'  ChooseConference_Expanded
'   Subroutine
'------------------------------
' ...explanation...
Sub ChooseConference_Expanded (targetFile$, ExitCode%)

    Do

        Spec$ = diskPaths$(2) + "*.TMS"
        null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
        Count% = DirCount%

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CONFERENCE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim conferenceFiles$(0 To Count%)
            For X = 1 To Count%
                conferenceFiles$(X) = Space$(12)
            Next
            conferenceFiles$(0) = Spec$
            Call ReadFile(conferenceFiles$())

            Do

                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT CONFERENCE": Color 15
                Selection% = 1
                Start% = 1
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
                Print "-SELECTS CONFERENCE";

                _MouseShow "DEFAULT"
                MAMenu conferenceFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = conferenceFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

                    Do
                        Locate 3, 33: Print "ANY CHANGE (YN)";
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then
                targetFile$ = Left$(RTrim$(targetFile$), Len(targetFile$) - 4)
            End If

        End If

    Loop Until ExitCode% = 27 Or Count% = 0 Or UCase$(I$) = "N"

End Sub

'------------------------------
'    BUILDSTATS Subroutine
'------------------------------
' ...explanation...
Sub BUILDSTATS (CT%)

    Dim PX

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ, totStats

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()

    Shared AM!(), AF!(), GI!(), GS!()
    Shared LC!(), LK!(), LF!(), LP!(), LR!()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), PKL$()
    Shared PK!(), lookyTT!(), Z!(), Z1!(), Z2!()

    Dim statsDI$(1, 10), statsDS$(1, 15), LC$(16, 1), statsLK$(2, 1), statsLP$(3, 1), LR$(9, 1)

    Shared statsA$()
    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared lookyA1$(), lookyA2$(), lookyA3$(), lookyA4$(), lookyA5$(), lookyA6$(), lookyA7$()
    Shared statsLF$(), statsPK$()
    Shared QBL$(), lookyTT$(), Z1$()

    Shared GMA!(), GMB!(), GMC!()
    Shared Y%()

    Color 15, 0
    Cls

    'PX is always a 0 value ?!?!



    For ZX = 1 To 30

        teamStatFile$ = Z1$(ZX)

        If teamStatFile$ <> "XXX" And teamStatFile$ <> "*" And teamStatFile$ <> " " Then

            If _FileExists(diskPaths$(1) + teamStatFile$) Then

                Print "CONVERTING STAT FILE OF "; teamStatFile$

                Open diskPaths$(1) + teamStatFile$ For Input As #1

                Input #1, statsA$(PX)

                For I = 0 To 9: Input #1, lookyA1$(PX, I): Next I
                For I = 0 To 5: Input #1, lookyA2$(PX, I): Next I
                For I = 0 To 3: Input #1, lookyA3$(PX, I): Next I
                For I = 0 To 2: Input #1, lookyA4$(PX, I): Next I
                For I = 0 To 2: Input #1, lookyA5$(PX, I): Next I

                Input #1, lookyA6$(PX, 0)

                For I = 0 To 1: Input #1, lookyA7$(PX, I): Next I

                For J = 0 To 13
                    For K = 0 To 17: Input #1, Z2!(J, K): Next K
                Next J

                For L = 0 To 38: Input #1, Z!(L): Next L

                For L = 0 To 38: Input #1, Z1!(L): Next L

                For J = 0 To 9: Input #1, statsDI$(PX, J): For J1 = 0 To 2: Input #1, GI!(PX, J, J1): Next: Next

                For J = 0 To 14: Input #1, statsDS$(PX, J): For J1 = 0 To 1: Input #1, GS!(PX, J, J1): Next: Next

                For I = 0 To 9: Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next

                For I = 0 To 15: Input #1, LC!(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next

                For I = 0 To 3: Input #1, LP!(I): Input #1, statsLP$(I, 0): Input #1, statsLP$(I, 1): Next

                For I = 0 To 2: Input #1, LK!(I): Input #1, statsLK$(I, 0): Input #1, statsLK$(I, 1): Next

                For I = 0 To 2: Input #1, PK!(I): Input #1, statsPK$(I, 0): Input #1, statsPK$(I, 1): Next

                For I = 0 To 1
                    Input #1, LF!(I)
                    Input #1, statsLF$(I)
                    For I1 = 0 To 4
                        Input #1, AM!(I, I1)
                        Input #1, AF!(I, I1)
                    Next
                Next

                Close #1

                Open diskPaths$(1) + teamStatFile$ + "S" For Input As #1
                Input #1, Y%
                Close #1

                totStats = totStats + 1

                For I = 0 To 14
                    lookyTT!(totStats, I) = Z!(I)
                    lookyTT$(totStats, I) = statsA$(PX)
                Next

                For I = 15 To 29
                    lookyTT!(totStats, I) = Z1!(I - 15)
                    lookyTT$(totStats, I) = statsA$(PX)
                Next

                For I = 0 To 9
                    JA = JA + 1
                    A1L$(JA) = lookyA1$(PX, I)
                    A1T$(JA) = statsA$(PX)
                    LRL$(JA) = LR$(I, 0)
                    LCL$(JA) = LC$(I, 0)
                    'I added the line below based on patterns in this routine
                    'In original code GMA!() was written to output files,
                    'but was never assigned a value!!!
                    GMA!(JA) = Y%

                    For I1 = 0 To 5: A1L!(JA, I1) = Z2!(I, I1): Next I1

                    A1L!(JA, 6) = LR!(I)
                    A1L!(JA, 7) = LC!(I)
                Next I

                For I = 0 To 5
                    JB = JB + 1
                    A2L$(JB) = lookyA2$(PX, I)
                    A2T$(JB) = statsA$(PX)
                    LCL$(JB) = LC$(I + 10, 0)
                    GMB!(JB) = Y%

                    For I1 = 6 To 8: A2L!(JB, I1 - 6) = Z2!(I, I1): Next I1

                    A2L!(JB, 3) = LC!(I + 10)
                Next I

                For I = 0 To 3
                    JC = JC + 1
                    QBL$(JC) = lookyA3$(PX, I)
                    A3T$(JC) = statsA$(PX)
                    LPL$(JC) = statsLP$(I, 0)
                    GMC!(JC) = Y%

                    For I1 = 9 To 13: A3L!(JC, I1 - 9) = Z2!(I, I1): Next I1

                    A3L!(JC, 5) = LP!(I)

                    Call QBRatings(CT%)

                    A3R!(JC, 0) = A3L!(JC, 12)

                Next I

                For I = 0 To 2
                    JD = JD + 1
                    A4L$(JD) = lookyA4$(PX, I)
                    A4T$(JD) = statsA$(PX)
                    LKL$(JD) = statsLK$(I, 0)

                    For I1 = 14 To 16: A4L!(JD, I1 - 14) = Z2!(I, I1): Next I1
                    A4L!(JD, 3) = LK!(I)
                Next I

                For I = 0 To 2
                    JE = JE + 1
                    A5L$(JE) = lookyA5$(PX, I)
                    A5T$(JE) = statsA$(PX)
                    PKL$(JE) = statsPK$(I, 0)

                    For I1 = 14 To 16: A5L!(JE, I1 - 14) = Z2!(I + 3, I1): Next I1

                    A5L!(JE, 3) = PK!(I)
                Next I

                JF = JF + 1
                A6L$(JF) = lookyA6$(PX, 0)
                A6T$(JF) = statsA$(PX)
                A6L!(JF, 0) = Z!(7)
                A6L!(JF, 1) = Z!(8)
                JG = JG + 1
                A7L$(JG) = lookyA7$(PX, 0)
                A7T$(JG) = statsA$(PX)

                For I = 0 To 3: A7L!(JJG, I) = Z2!(I + 3, 17): Next

                A7L!(JJG, 4) = LF!(0)
                JG = JG + 1
                A7L$(JG) = lookyA7$(PX, 1)
                A7T$(JG) = statsA$(PX)

                For I = 0 To 3: A7L!(JJG, I) = Z2!(I + 7, 17): Next

                A7L!(JJG, 4) = LF!(1)

                For I = 0 To 9
                    JH = JH + 1
                    A8L$(JH) = statsDI$(PX, I)
                    A8T$(JH) = statsA$(PX)
                    A8L!(JH) = GI!(PX, I, 0)
                Next

                For I = 0 To 14
                    JI = JI + 1
                    A9L$(JI) = statsDS$(PX, I)
                    A9T$(JI) = statsA$(PX)
                    A9L!(JI) = GS!(PX, I, 0)
                Next

                JJ = JJ + 1
                Y%(JJ) = Y%

            End If

        End If

    Next ZX

End Sub

'------------------------------
'    QBRatings Subroutine
'------------------------------
' ...explanation...
Sub QBRatings (CT%)

    Shared A3L!()
    Dim FF!(120), II!(120), PP!(120), YY!(120)

    'QUARTERBACK PASSER RATINGS
    If A3L!(JC, 0) > 0 Then A3L!(JC, 11) = Int(A3L!(JC, 1) / A3L!(JC, 0) * 1000) / 10

    If CT% = 3 Then
        'COL QB RATING
        ' ********** CALCULATE QUARTERBACK PASSING EFFICIENCY ****************
        ' * QB(1)=PASS ATT, QB(2)=COMP, QB(3)=INT, QB(4)=YDS, QB(5)=TD'S     *
        ' * QB(6)=COMP%, QB(7)=RATING, QB(8)=TD%, QB(9)=INT%, QB(10)=YDS/ATT *
        ' ********************************************************************
        If A3L!(JC, 0) = 0 Then
            A3L!(JC, 11) = 0
            A3L!(JC, 8) = 0
            A3L!(JC, 10) = 0
            A3L!(JC, 9) = 0
        Else
            A3L!(JC, 12) = Int(A3L!(JC, 3) / A3L!(JC, 0)) * 8.399999 + A3L!(JC, 11) + Int(A3L!(JC, 4) / A3L!(JC, 0) * 1000) / 10 * 3.3 - Int(A3L!(JC, 2) / A3L!(JC, 0) * 1000) / 10 * 2
            A3L!(JC, 12) = Int(A3L!(JC, 12) * 10) / 10
        End If

    End If

    If A3L!(JC, 0) = 0 Then
        A3L!(JC, 11) = 0
        A3L!(JC, 8) = 0
        A3L!(JC, 10) = 0
        A3L!(JC, 9) = 0
    Else
        A3L!(JC, 8) = Int(A3L!(JC, 4) / A3L!(JC, 0) * 1000) / 10
        A3L!(JC, 10) = Int(A3L!(JC, 3) / A3L!(JC, 0) * 100) / 100
        A3L!(JC, 9) = Int(A3L!(JC, 2) / A3L!(JC, 0) * 1000) / 10
    End If

    PP!(JC) = A3L!(JC, 11) - 30

    If PP!(JC) < 0 Then
        PP!(JC) = 0
    Else
        If PP!(JC) > 77.5 Then
            PP!(JC) = 2.375
        Else
            PP!(JC) = PP!(JC) * .05
        End If
    End If

    If A3L!(JC, 8) > 11.9 Then
        FF!(JC) = 2.375
    Else
        FF!(JC) = A3L!(JC, 8) * .2
    End If

    If A3L!(JC, 9) = 0 Then
        II!(JC) = 0
    Else
        If A3L!(JC, 9) > 9.5 Then
            II!(JC) = 0
        Else
            II!(JC) = 2.375 - (.25 * A3L!(JC, 9))
        End If
    End If

    A3L!(JC, 10) = A3L!(JC, 10) - 3

    If A3L!(JC, 10) < 0 Then
        YY!(JC) = 0
    Else
        If A3L!(JC, 10) > 12.5 Then
            YY!(JC) = 2.375
        Else
            D = Int(A3L!(JC, 10) * 100) / 2
            If D = A3L!(JC, 10) / 2 Then
                YY!(JC) = A3L!(JC, 10) / 2 * .3 + A3L!(JC, 10) / 2 * .2
            Else
                YY!(JC) = A3L!(JC, 10) / 2 * .3 + A3L!(JC, 10) / 2 * .2 + .003
            End If
        End If
    End If

    A3L!(JC, 12) = PP!(JC) + FF!(JC) + II!(JC) + YY!(JC)

    If A3L!(JC, 12) > 9.5 Then
        A3L!(JC, 12) = 158.3
    Else
        A3L!(JC, 12) = A3L!(JC, 12) * 50 / 3
    End If

    A3L!(JC, 12) = Int(A3L!(JC, 12) * 10) / 10

End Sub

'------------------------------
'    SAVELEADER Subroutine
'------------------------------
' ...explanation...
Sub SAVELEADER (leaderFile$)

    Shared JA, JB, JC, JD, JE, JF, JG, JH, JI, JJ, totStats

    Shared A1L$(), A1T$(), A2L$(), A2T$(), A3T$(), A4L$(), A4T$()
    Shared A5L$(), A5T$(), A6L$(), A6T$(), A7L$(), A7T$()
    Shared A8L$(), A8T$(), A9L$(), A9T$()
    Shared LCL$(), LKL$(), LPL$(), LRL$(), lookyTT$()
    Shared PKL$(), QBL$()

    Shared A1L!(), A2L!(), A3L!(), A3R!(), A4L!()
    Shared A5L!(), A6L!(), A7L!(), A8L!(), A9L!()
    Shared lookyTT!()

    Shared GMA!(), GMB!(), GMC!()
    Shared Y%()

    TMR = 1660

    Print "SAVING LEADER FILES . . . . PLEASE WAIT"

    Open diskPaths$(2) + leaderFile$ + ".LDS" For Output As #1
    Write #1, JA, JB, JC, JD, JE, JF, JG, JH, JI, totStats, JJ

    For I = 1 To 30:
        For I1 = 0 To 29:
            Write #1, lookyTT!(I, I1), lookyTT$(I, I1)
        Next
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 1 To 300:
        Write #1, A1L$(I), A1T$(I), LRL$(I), LCL$(I), GMA!(I)
        For I1 = 0 To 7: Write #1, A1L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 180:
        Write #1, A2L$(I), A2T$(I), LCL$(I), GMB!(I)
        For I1 = 0 To 3: Write #1, A2L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 120:
        Write #1, QBL$(I), A3T$(I), LPL$(I), A3R!(I, 0), GMC!(I)
        For I1 = 0 To 6: Write #1, A3L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 90:
        Write #1, A4L$(I), A4T$(I), LKL$(I)
        For I1 = 0 To 3: Write #1, A4L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 90:
        Write #1, A5L$(I), A5T$(I), PKL$(I)
        For I1 = 0 To 3: Write #1, A5L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 30:
        Write #1, A6L$(I), A6T$(I)
        For I1 = 0 To 1: Write #1, A6L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 60:
        Write #1, A7L$(I), A7T$(I)
        For I1 = 0 To 4: Write #1, A7L!(I, I1): Next I1
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 300:
        Write #1, A8L$(I), A8T$(I), A8L!(I)
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 450:
        Write #1, A9L$(I), A9T$(I), A9L!(I)
        Color 14: Locate 16, 45: TMR = TMR - 1: Print Using "####"; TMR
    Next I

    For I = 1 To 30: Write #1, Y%(I): Next

    Close #1

End Sub

'------------------------------
'    CATEGORY_BOX Subroutine
'------------------------------
' ...explanation...
Sub CATEGORY_BOX (J)

    Print " " + Chr$(214); String$(36, 196); Chr$(183) + " ";

    For II = 1 To J:
        Locate 2 + II, 22: Print " " + Chr$(186); String$(36, 32); Chr$(186) + " ";
    Next

    Locate 3 + J, 22
    Print " " + Chr$(211); String$(36, 196); Chr$(189) + " ";
    Locate 10, 15

End Sub

