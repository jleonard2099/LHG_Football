Sub CARVIEW ()

    _Title "3-in-1 Football - View Career Files"

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!(), careerFGS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerTT!()
    Shared careerWRS!()

    ' We could probably not need these extra variables
    ' If we simply acounted for the first dimension
    ' within our code
    Shared carviewA1$(), carviewA2$(), carviewA3$(), carviewA4$(), carviewA5$(), carviewA6$(), carviewA7$()
    Shared carviewDI$(), carviewDS$()

    TMR = 6420

    Do
        'Display list of files...
        Color 0, 0
        Cls

        Spec$ = diskPaths$(1) + "*.CAR"
        Count% = FCount%(Spec$)

        If Count% = 0 Then
            Locate 10, 8
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
        Else
            ReDim careerFiles$(0 To Count%)

            For X = 1 To Count%
                careerFiles$(X) = Space$(12)
            Next

            careerFiles$(0) = Spec$
            Call ReadFile(careerFiles$())

            Do
                Color , 1
                Cls

                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
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
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerFiles$(), Selection%, START%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% <> 27 Then

                    Color 15, 1
                    targetFile$ = careerFiles$(Selection%)
                    Locate 2, 33: Print targetFile$

					Do
						Locate 3, 33: Print "ANY CHANGE (YN)";
						I$ = GetKeyPress$
					Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

					Color 15, 0

                End If

            Loop Until UCase$(I$) = "N" Or ExitCode% = 27

            If ExitCode% <> 27 Then

                targetFile$ = RTrim$(targetFile$)
                Z$ = Left$(targetFile$, Len(targetFile$) - 4)

				Color 15, 0
				Cls

                Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

                Open diskPaths$(1) + Z$ + ".CAR" For Input As #1

                Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

                For I = 0 To 49: 
                    Input #1, carviewA1$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 39
					Input #1, carviewA2$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 19
					Input #1, carviewA3$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 14
					Input #1, carviewA4$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 14
					Input #1, carviewA5$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 9
					Input #1, carviewA6$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 9
					Input #1, carviewA7$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 54
					Input #1, carviewDI$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For I = 0 To 54
					Input #1, carviewDS$(PX, I)
					Locate 21, 38: Color 5
					TMR = TMR - 1: Print Using "####"; TMR
                Next

                For N = 1 To 15: 
                    For J = 0 To 49: 
                        For K = 0 To 9
                            Input #1, careerRBS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 49
                        Input #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 39: 
                        For K = 0 To 5
                            Input #1, careerWRS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 39
                        Input #1, careerWRS$(N, J, 0)
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 19: 
                        For K = 0 To 7
                            Input #1, careerQBS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 19
                        Input #1, careerQBS$(N, J, 0)
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 14: 
                        For K = 0 To 5
                            Input #1, careerKRS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 14
                        Input #1, careerKRS$(N, J, 0)
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 14: 
                        For K = 0 To 5
                            Input #1, careerPRS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 14
                        Input #1, careerPRS$(N, J, 0)
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 9: 
                        For K = 0 To 3
                            Input #1, careerPUS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 9: 
                        For K = 0 To 16
                            Input #1, careerFGS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 54: 
                        For K = 0 To 2
                            Input #1, careerGIS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                For N = 1 To 15: 
                    For J = 0 To 54: 
                        For K = 0 To 2
                            Input #1, careerGSS!(N, J, K)
                        Next K
                        Locate 21, 38: Color 5
                        TMR = TMR - 1: Print Using "####"; TMR
                    Next J
                Next N

                Close #1

                '----------
				
                Do

					Erase careerTT!

                    Color 15, 0
                    Cls

                    Color 7: Print "VIEW ";
                    Color 12: Print "R";: Color 7
                    Print "UNNING BACKS, ";
                    Color 12: Print "W";: Color 7
                    Print "IDE RECEIVERS, ";
                    Color 12: Print "Q";: Color 7
                    Print "UARTERBACKS, ";
                    Color 12: Print "K";: Color 7
                    Print "ICK RETURNERS, P";
                    Color 12: Print "U";: Color 7
                    Print "NT"
                    Print "RETURNERS, ";
                    Color 12: Print "P";: Color 7
                    Print "UNTERS, KI";
                    Color 12: Print "C";: Color 7
                    Print "KERS, ";
                    Color 12: Print "I";: Color 7
                    Print "NTERCEPTIONS, ";
                    Color 12: Print "D";: Color 7
                    Print "EFENSIVE SACKS,": Color 7
                    Print "R";: Color 12: Print "O";: Color 7
                    Print "STER IN SEQUENCE, OR ";
                    Color 12: Print "E";: Color 7
                    Print "XIT TO MAIN MENU."

                    escPressed% = 0

                    I$ = GetKeyPress$

                    Select Case UCase$(I$)

                        Case "R"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 16: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA1$(PX, I), 12): Next
                            For I = 17 To 33: Locate I - 15, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA1$(PX, I), 12): Next
                            For I = 34 To 49: Locate I - 32, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA1$(PX, I), 12): Next

                            Print
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 1, escPressed%)

                        Case "W"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 19: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA2$(PX, I), 12): Next
                            For I = 20 To 39: Locate I - 18, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA2$(PX, I), 12): Next
                            Print
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 2, escPressed%)

                        Case "Q"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA3$(PX, I), 12): Next
                            For I = 10 To 19: Locate I - 8, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA3$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 3, escPressed%)

                        Case "K"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 7: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA4$(PX, I), 12): Next
                            For I = 8 To 14: Locate I - 6, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA4$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 4, escPressed%)

                        Case "U"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 7: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA5$(PX, I), 12): Next
                            For I = 8 To 14: Locate I - 6, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA5$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 5, escPressed%)

                        Case "P"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA6$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 6, escPressed%)

                        Case "C"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 9: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewA7$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 7, escPressed%)

                        Case "I"
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 18: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewDI$(PX, I), 12): Next
                            For I = 19 To 36: Locate I - 17, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewDI$(PX, I), 12): Next
                            For I = 37 To 54: Locate I - 35, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewDI$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 8, escPressed%)

                        Case "D"
                            2920 '
                            Cls
                            Color 12: Print Z$
                            For I = 0 To 18: Locate I + 2: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewDS$(PX, I), 12): Next
                            For I = 19 To 36: Locate I - 17, 20: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewDS$(PX, I), 12): Next
                            For I = 37 To 54: Locate I - 35, 40: Color 11: Print Using "##"; I;: Print " ";: Color 7: Print Left$(carviewDS$(PX, I), 12): Next
                            Print
                            Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                            RT = 2
                            Cls

                            Call CareerPrintCategory(RT, B9, 9, escPressed%)

                        Case "O"
                            RT = 1
                            Cls
                            For I = 1 To 50: careerTT!(I) = 0: Next

                            For category% = 1 To 9:
                                Call CareerPrintCategory(RT, B9, category%, escPressed%)
                                If escPressed% = 1 Then Exit For
                            Next

                            'If escPressed% = 1 Then I$ = "E"

                        Case "E"
                            '

                    End Select

                Loop Until UCase$(I$) = "E" Or escPressed% = 1

            End If ' Done checking for ESC

        End If 'Done checking for file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0 Or escPressed% = 1

    Color 0, 0

End Sub

'----------------------------------------
'    CareerPrintCategory Subroutine
'----------------------------------------
'...explanation...
Sub CareerPrintCategory (RT, B9, categoryID%, escPressed%)

    Shared careerRC$()
    Shared careerRC!()

    Shared careerFF!(), careerII!(), careerPP!(), careerTT!(), careerYY!()

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!(), careerFGS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Shared carviewA1$(), carviewA2$(), carviewA3$(), carviewA4$(), carviewA5$(), carviewA6$(), carviewA7$()
    Shared carviewDI$(), carviewDS$()

    Do

        Select Case categoryID%

            Case 1

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2: Print "RUSHER         YEAR  ATT.  YARDS AVG. TDS   REC. YARDS  AVG. TDS RUN-LONGEST-REC"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 49

                    If carviewA1$(PX, I) <> "" And carviewA1$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewA1$(PX, I), 13);

                        VV = careerRBS!(1, I, 7) + careerRBS!(2, I, 7) + careerRBS!(3, I, 7) + careerRBS!(4, I, 7) + careerRBS!(5, I, 7) + careerRBS!(6, I, 7) + careerRBS!(7, I, 7) + careerRBS!(7, I, 7) + careerRBS!(9, I, 7) + careerRBS!(10, I, 7) + careerRBS!(11, I, 7) + careerRBS!(12, I, 7) + careerRBS!(13, I, 7) + careerRBS!(14, I, 7) + careerRBS!(15, I, 7)

                        For V = 1 To VV

                            Color 7
                            Print Tab(15); careerRBS!(V, I, 8);
                            Print Tab(23); Using "###"; careerRBS!(V, I, 0);
                            Print Tab(29); Using "####"; careerRBS!(V, I, 1);

                            If careerRBS!(V, I, 1) = 0 Or careerRBS!(V, I, 0) = 0 Then Print Tab(34); " 0.0";
                            If careerRBS!(V, I, 0) > 0 Then Print Tab(34); Using "##.#"; careerRBS!(V, I, 1) / careerRBS!(V, I, 0);

                            Print Tab(39); Using "###"; careerRBS!(V, I, 2);
                            Print Tab(45); Using "###"; careerRBS!(V, I, 3);
                            Print Tab(51); Using "####"; careerRBS!(V, I, 4);

                            If careerRBS!(V, I, 4) = 0 Or careerRBS!(V, I, 3) = 0 Then Print Tab(57); " 0.0";
                            If careerRBS!(V, I, 3) > 0 Then Print Tab(57); Using "##.#"; careerRBS!(V, I, 4) / careerRBS!(V, I, 3);

                            Print Tab(63); Using "##"; careerRBS!(V, I, 5);
                            Print Tab(67); Using "##"; careerRBS!(V, I, 6);
                            Print Tab(69); careerRBS$(V, I, 0);
                            Print Tab(77); Using "##"; careerRBS!(V, I, 9);
                            Print Tab(79); careerRBS$(V, I, 1)

                        Next V

                        VV = careerRBS!(1, B9, 7) + careerRBS!(2, B9, 7) + careerRBS!(3, B9, 7) + careerRBS!(4, B9, 7) + careerRBS!(5, B9, 7) + careerRBS!(6, B9, 7) + careerRBS!(7, B9, 7) + careerRBS!(7, B9, 7) + careerRBS!(9, B9, 7) + careerRBS!(10, B9, 7) + careerRBS!(11, B9, 7) + careerRBS!(12, B9, 7) + careerRBS!(13, B9, 7) + careerRBS!(14, B9, 7) + careerRBS!(15, B9, 7)

                        For V = 1 To VV
                            careerTT!(1) = careerTT!(1) + careerRBS!(V, I, 0)
                            careerTT!(2) = careerTT!(2) + careerRBS!(V, I, 1)
                            careerTT!(3) = careerTT!(3) + careerRBS!(V, I, 2)
                            careerTT!(4) = careerTT!(4) + careerRBS!(V, I, 3)
                            careerTT!(5) = careerTT!(5) + careerRBS!(V, I, 4)
                            careerTT!(6) = careerTT!(6) + careerRBS!(V, I, 5)
                        Next V

                        careerRC!(1) = careerRBS!(1, I, 6): careerRC$(1) = careerRBS$(1, I, 0)
                        careerRC!(2) = careerRBS!(1, I, 9): careerRC$(2) = careerRBS$(1, I, 1)

                        For V = 2 To VV

                            If careerRBS!(V, I, 6) >= careerRC!(1) Then
                                careerRC!(1) = careerRBS!(V, I, 6)
                                careerRC$(1) = careerRBS$(V, I, 0)
                            Else
                                If careerRBS!(V, I, 9) >= careerRC!(2) Then
                                    careerRC!(2) = careerRBS!(V, I, 9)
                                    careerRC$(2) = careerRBS$(V, I, 1)
                                End If
                            End If

                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(18); Using "##"; VV;
                        Print Tab(22); Using "####"; careerTT!(1);
                        Print Tab(28); Using "#####"; careerTT!(2);

                        If careerTT!(1) = 0 Or careerTT!(2) = 0 Then Print Tab(34); " 0.0";
                        If careerTT!(1) > 0 Then Print Tab(34); Using "##.#"; careerTT!(2) / careerTT!(1);

                        Print Tab(39); Using "###"; careerTT!(3);
                        Print Tab(44); Using "####"; careerTT!(4);
                        Print Tab(50); Using "#####"; careerTT!(5);

                        If careerTT!(4) = 0 Or careerTT!(5) = 0 Then Print Tab(57); " 0.0";
                        If careerTT!(4) > 0 Then Print Tab(57); Using "##.#"; careerTT!(5) / careerTT!(4);

                        Print Tab(62); Using "###"; careerTT!(6);
                        Print Tab(67); Using "##"; careerRC!(1);
                        Print Tab(69); careerRC$(1);
                        Print Tab(77); Using "##"; careerRC!(2);
                        Print Tab(79); careerRC$(2);

                        For J = 1 To 6: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(90, " ")
                            Next
                        Else
                            escPressed% = 1
                        End If

                    End If

                    If escPressed% = 1 Then Exit For

                Next I

            Case 2

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2: Print "RECEIVER        YEAR    REC.   YARDS    AVG.  TD  LONGEST REC."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 39

                    If carviewA2$(PX, I) <> "" And carviewA2$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewA2$(PX, I), 13);
                        VV = careerWRS!(1, I, 4) + careerWRS!(2, I, 4) + careerWRS!(3, I, 4) + careerWRS!(4, I, 4) + careerWRS!(5, I, 4) + careerWRS!(6, I, 4) + careerWRS!(7, I, 4) + careerWRS!(8, I, 4) + careerWRS!(9, I, 4) + careerWRS!(10, I, 4) + careerWRS!(11, I, 4) + careerWRS!(12, I, 4) + careerWRS!(13, I, 4) + careerWRS!(14, I, 4) + careerWRS!(15, I, 4)

                        For V = 1 To VV
                            Color 7: Print Tab(16); careerWRS!(V, I, 5);
                            Print Tab(25); Using "###"; careerWRS!(V, I, 0);
                            Print Tab(33); Using "####"; careerWRS!(V, I, 1);

                            If careerWRS!(V, I, 0) = 0 Or careerWRS!(V, I, 1) = 0 Then Print Tab(41); " 0.0";
                            If careerWRS!(V, I, 0) > 0 Then Print Tab(41); Using "##.#"; careerWRS!(V, I, 1) / careerWRS!(V, I, 0);

                            Print Tab(47); Using "##"; careerWRS!(V, I, 2);
                            Print Tab(54); Using "##"; careerWRS!(V, I, 3);
                            Print Tab(56); careerWRS$(V, I, 0)
                        Next V

                        VV = careerWRS!(1, I, 4) + careerWRS!(2, I, 4) + careerWRS!(3, I, 4) + careerWRS!(4, I, 4) + careerWRS!(5, I, 4) + careerWRS!(6, I, 4) + careerWRS!(7, I, 4) + careerWRS!(8, I, 4) + careerWRS!(9, I, 4) + careerWRS!(10, I, 4) + careerWRS!(11, I, 4) + careerWRS!(12, I, 4) + careerWRS!(13, I, 4) + careerWRS!(14, I, 4) + careerWRS!(15, I, 4)

                        For V = 1 To VV
                            careerTT!(7) = careerTT!(7) + careerWRS!(V, I, 0)
                            careerTT!(8) = careerTT!(8) + careerWRS!(V, I, 1)
                            careerTT!(9) = careerTT!(9) + careerWRS!(V, I, 2)
                        Next V

                        careerRC!(2) = careerWRS!(1, I, 3): careerRC$(2) = careerWRS$(1, I, 0)

                        For V = 2 To VV
                            If careerWRS!(V, I, 3) >= careerRC!(2) Then
                                careerRC!(2) = careerWRS!(V, I, 3)
                                careerRC$(2) = careerWRS$(V, I, 0)
                            End If

                        Next

                        Print
                        Color 3: Print "TOTALS";
                        Print Tab(19); Using "##"; VV;
                        Print Tab(24); Using "####"; careerTT!(7);
                        Print Tab(32); Using "#####"; careerTT!(8);

                        If careerTT!(7) = 0 Or careerTT!(8) = 0 Then Print Tab(41); " 0.0";
                        If careerTT!(7) > 0 Then Print Tab(41); Using "##.#"; careerTT!(8) / careerTT!(7);

                        Print Tab(46); Using "###"; careerTT!(9);
                        Print Tab(54); Using "##"; careerRC!(2);
                        Print Tab(56); careerRC$(2)

                        For J = 7 To 9: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 3

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2: Print "QUARTERBACK  YEAR  COMP. ATT. INT TDS YARDS  PCT. YD/COMP RATING LONG.PASS"; ""

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 19

                    If carviewA3$(PX, I) <> "" And carviewA3$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewA3$(PX, I), 12);

                        VV = careerQBS!(1, I, 6) + careerQBS!(2, I, 6) + careerQBS!(3, I, 6) + careerQBS!(4, I, 6) + careerQBS!(5, I, 6) + careerQBS!(6, I, 6) + careerQBS!(7, I, 6) + careerQBS!(8, I, 6) + careerQBS!(9, I, 6) + careerQBS!(10, I, 6) + careerQBS!(11, I, 6) + careerQBS!(12, I, 6) + careerQBS!(13, I, 6) + careerQBS!(14, I, 6) + careerQBS!(15, I, 6)

                        For V = 1 To VV
                            Color 7: Print Tab(13); careerQBS!(V, I, 7);

                            Print Tab(21); Using "###"; careerQBS!(V, I, 1);
                            Print Tab(26); Using "###"; careerQBS!(V, I, 0);
                            Print Tab(32); Using "##"; careerQBS!(V, I, 2);
                            Print Tab(36); Using "##"; careerQBS!(V, I, 4);
                            Print Tab(40); Using "####"; careerQBS!(V, I, 3);

                            If careerQBS!(V, I, 1) = 0 Or careerQBS!(V, I, 0) = 0 Then Print Tab(47); " 00";
                            If careerQBS!(V, I, 1) > 0 Then Print Tab(47); Using "##"; (careerQBS!(V, I, 1) / careerQBS!(V, I, 0)) * 100;
                            If careerQBS!(V, I, 1) = 0 Then Print Tab(53); " 0.0";
                            If careerQBS!(V, I, 1) > 0 Then Print Tab(53); Using "##.#"; careerQBS!(V, I, 3) / careerQBS!(V, I, 1);

                            If careerQBS!(V, I, 0) > 0 Then careerQBS!(V, I, 11) = Int(careerQBS!(V, I, 1) / careerQBS!(V, I, 0) * 1000) / 10

                            If careerQBS!(V, I, 0) = 0 Then
                                careerQBS!(V, I, 11) = 0: careerQBS!(V, I, 8) = 0: careerQBS!(V, I, 10) = 0: careerQBS!(V, I, 9) = 0
                            Else
                                careerQBS!(V, I, 8) = Int(careerQBS!(V, I, 4) / careerQBS!(V, I, 0) * 1000) / 10
                                careerQBS!(V, I, 10) = Int(careerQBS!(V, I, 3) / careerQBS!(V, I, 0) * 100) / 100
                                careerQBS!(V, I, 9) = Int(careerQBS!(V, I, 2) / careerQBS!(V, I, 0) * 1000) / 10
                            End If

                            careerPP!(V, I) = careerQBS!(V, I, 11) - 30

                            If careerPP!(V, I) < 0 Then
                                careerPP!(V, I) = 0
                            Else
                                If careerPP!(V, I) > 77.5 Then
                                    careerPP!(V, I) = 2.375
                                Else
                                    careerPP!(V, I) = careerPP!(V, I) * .05
                                End If
                            End If

                            If careerQBS!(V, I, 8) > 11.9 Then
                                careerFF!(V, I) = 2.375
                            Else
                                careerFF!(V, I) = careerQBS!(V, I, 8) * .2
                            End If

                            If careerQBS!(V, I, 9) = 0 Then
                                careerII!(V, I) = 0
                            Else
                                If careerQBS!(V, I, 9) > 9.5 Then
                                    careerII!(V, I) = 0
                                Else
                                    careerII!(V, I) = 2.375 - (.25 * careerQBS!(V, I, 9))
                                End If
                            End If

                            careerQBS!(V, I, 10) = careerQBS!(V, I, 10) - 3

                            If careerQBS!(V, I, 10) < 0 Then
                                careerYY!(V, I) = 0
                            Else
                                If careerQBS!(V, I, 10) > 12.5 Then
                                    careerYY!(V, I) = 2.375
                                Else
                                    D = Int(careerQBS!(V, I, 10) * 100) / 2
                                    If D = careerQBS!(V, I, 10) / 2 Then
                                        careerYY!(V, I) = careerQBS!(V, I, 10) / 2 * .3 + careerQBS!(V, I, 10) / 2 * .2
                                    Else
                                        careerYY!(V, I) = careerQBS!(V, I, 10) / 2 * .3 + careerQBS!(V, I, 10) / 2 * .2 + .003
                                    End If
                                End If
                            End If

                            careerQBS!(V, I, 12) = careerPP!(V, I) + careerFF!(V, I) + careerII!(V, I) + careerYY!(V, I)

                            If careerQBS!(V, I, 12) > 9.5 Then
                                careerQBS!(V, I, 12) = 158.3
                            Else
                                careerQBS!(V, I, 12) = careerQBS!(V, I, 12) * 50 / 3
                            End If

                            careerQBS!(V, I, 12) = Int(careerQBS!(V, I, 12) * 10) / 10

                            Print Tab(60); Using "###.#"; careerQBS!(V, I, 12);
                            Print Tab(70); Using "##"; careerQBS!(V, I, 5);
                            Print Tab(72); careerQBS$(V, I, 0)
                        Next V

                        VV = careerQBS!(1, I, 6) + careerQBS!(2, I, 6) + careerQBS!(3, I, 6) + careerQBS!(4, I, 6) + careerQBS!(5, I, 6) + careerQBS!(6, I, 6) + careerQBS!(7, I, 6) + careerQBS!(8, I, 6) + careerQBS!(9, I, 6) + careerQBS!(10, I, 6) + careerQBS!(11, I, 6) + careerQBS!(12, I, 6) + careerQBS!(13, I, 6) + careerQBS!(14, I, 6) + careerQBS!(15, I, 6)

                        For V = 1 To VV
                            careerTT!(10) = careerTT!(10) + careerQBS!(V, I, 1)
                            careerTT!(11) = careerTT!(11) + careerQBS!(V, I, 0)
                            careerTT!(12) = careerTT!(12) + careerQBS!(V, I, 2)
                            careerTT!(13) = careerTT!(13) + careerQBS!(V, I, 4)
                            careerTT!(14) = careerTT!(14) + careerQBS!(V, I, 3)
                        Next V

                        careerRC!(3) = careerQBS!(1, I, 5)
                        careerRC$(3) = careerQBS$(1, I, 0)

                        For V = 2 To VV
                            If careerQBS!(V, I, 5) >= careerRC!(3) Then
                                careerRC!(3) = careerQBS!(V, I, 5): careerRC$(3) = careerQBS$(V, I, 0)
                            End If
                        Next

                        Print
                        Color 3: Print: Print "TOTALS";
                        Print Tab(16); Using "##"; VV;
                        Print Tab(20); Using "####"; careerTT!(10);
                        Print Tab(25); Using "####"; careerTT!(11);
                        Print Tab(31); Using "###"; careerTT!(12);
                        Print Tab(34); Using "####"; careerTT!(13);
                        Print Tab(39); Using "#####"; careerTT!(14);

                        If careerTT!(10) = 0 Or careerTT!(11) = 0 Then Print Tab(47); " 00";
                        If careerTT!(10) > 0 Then Print Tab(47); Using "##"; (careerTT!(10) / careerTT!(11)) * 100;
                        If careerTT!(11) = 0 Then Print Tab(53); " 0.0";
                        If careerTT!(10) > 0 Then Print Tab(53); Using "##.#"; careerTT!(14) / careerTT!(10);

                        If careerTT!(11) > 0 Then careerTT!(30) = Int(careerTT!(10) / careerTT!(11) * 1000) / 10
                        If careerTT!(11) = 0 Then
                            careerTT!(30) = 0: careerTT!(27) = 0: careerTT!(29) = 0: careerTT!(28) = 0
                        Else
                            careerTT!(27) = Int(careerTT!(13) / careerTT!(11) * 1000) / 10
                            careerTT!(29) = Int(careerTT!(14) / careerTT!(11) * 100) / 100
                            careerTT!(28) = Int(careerTT!(12) / careerTT!(11) * 1000) / 10
                        End If

                        PPT! = careerTT!(30) - 30

                        If PPT! < 0 Then
                            PPT! = 0
                        Else
                            If PPT! > 77.5 Then
                                PPT! = 2.375
                            Else
                                PPT! = PPT! * .05
                            End If
                        End If

                        If careerTT!(27) > 11.9 Then
                            FFT! = 2.375
                        Else
                            FFT! = careerTT!(27) * .2
                        End If

                        If careerTT!(28) = 0 Then
                            IIT! = 0
                        Else
                            If careerTT!(28) > 9.5 Then
                                IIT! = 0
                            Else
                                IIT! = 2.375 - (.25 * careerTT!(28))
                            End If
                        End If

                        careerTT!(29) = careerTT!(29) - 3

                        If careerTT!(29) < 0 Then
                            YYT! = 0
                        Else
                            If careerTT!(29) > 12.5 Then
                                YYT! = 2.375
                            Else
                                D = Int(careerTT!(29) * 100) / 2
                                If D = careerTT!(29) / 2 Then
                                    YYT! = careerTT!(29) / 2 * .3 + careerTT!(29) / 2 * .2
                                Else
                                    YYT! = careerTT!(29) / 2 * .3 + careerTT!(29) / 2 * .2 + .003
                                End If
                            End If
                        End If

                        careerTT!(31) = PPT! + FFT! + IIT! + YYT!

                        If careerTT!(31) > 9.5 Then
                            careerTT!(31) = 158.3
                        Else
                            careerTT!(31) = careerTT!(31) * 50 / 3
                        End If

                        careerTT!(31) = Int(careerTT!(31) * 10) / 10

                        Print Tab(60); Using "###.#"; careerTT!(31);
                        Print Tab(70); Using "##"; careerRC!(3);
                        Print Tab(72); careerRC$(3)

                        For J = 10 To 14: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 4

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2
                Print "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST RET."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 14

                    If carviewA4$(PX, I) <> "" And carviewA4$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewA4$(PX, I), 13);

                        VV = careerKRS!(1, I, 4) + careerKRS!(2, I, 4) + careerKRS!(3, I, 4) + careerKRS!(4, I, 4) + careerKRS!(5, I, 4) + careerKRS!(6, I, 4) + careerKRS!(7, I, 4) + careerKRS!(8, I, 4) + careerKRS!(9, I, 4) + careerKRS!(10, I, 4) + careerKRS!(11, I, 4) + careerKRS!(12, I, 4) + careerKRS!(13, I, 4) + careerKRS!(14, I, 4) + careerKRS!(15, I, 4)

                        For V = 1 To VV
                            Color 7: Print Tab(22); careerKRS!(V, I, 5);

                            Print Tab(32); Using "###"; careerKRS!(V, I, 0);
                            Print Tab(42); Using "####"; careerKRS!(V, I, 1);

                            If careerKRS!(V, I, 0) = 0 Or careerKRS!(V, I, 1) = 0 Then Print Tab(52); " 00";
                            If careerKRS!(V, I, 0) > 0 Then Print Tab(52); Using "##.#"; careerKRS!(V, I, 1) / careerKRS!(V, I, 0);

                            Print Tab(62); Using "##"; careerKRS!(V, I, 2);
                            Print Tab(70); Using "##"; careerKRS!(V, I, 3);
                            Print Tab(72); careerKRS$(V, I, 0)
                        Next V

                        VV = careerKRS!(1, I, 4) + careerKRS!(2, I, 4) + careerKRS!(3, I, 4) + careerKRS!(4, I, 4) + careerKRS!(5, I, 4) + careerKRS!(6, I, 4) + careerKRS!(7, I, 4) + careerKRS!(8, I, 4) + careerKRS!(9, I, 4) + careerKRS!(10, I, 4) + careerKRS!(11, I, 4) + careerKRS!(12, I, 4) + careerKRS!(13, I, 4) + careerKRS!(14, I, 4) + careerKRS!(15, I, 4)

                        For V = 1 To VV
                            careerTT!(15) = careerTT!(15) + careerKRS!(V, I, 0)
                            careerTT!(16) = careerTT!(16) + careerKRS!(V, I, 1)
                            careerTT!(17) = careerTT!(17) + careerKRS!(V, I, 2)
                        Next V

                        careerRC!(4) = careerKRS!(1, I, 3): careerRC$(4) = careerKRS$(1, I, 0)

                        For V = 2 To VV
                            If careerKRS!(V, I, 3) >= careerRC!(4) Then
                                careerRC!(4) = careerKRS!(V, I, 3)
                                careerRC$(4) = careerKRS$(V, I, 0)
                            End If
                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(25); Using "##"; VV;
                        Print Tab(32); Using "###"; careerTT!(15);
                        Print Tab(41); Using "#####"; careerTT!(16);

                        If careerTT!(15) = 0 Or careerTT!(16) = 0 Then Print Tab(52); " 00";
                        If careerTT!(15) > 0 Then Print Tab(52); Using "##.#"; careerTT!(16) / careerTT!(15);

                        Print Tab(61); Using "###"; careerTT!(17);
                        Print Tab(70); Using "##"; careerRC!(4);
                        Print Tab(72); careerRC$(4)

                        For J = 15 To 17: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 5

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2
                Print "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST RET."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 14

                    If carviewA5$(PX, I) <> "" And carviewA5$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewA5$(PX, I), 13);

                        VV = careerPRS!(1, I, 4) + careerPRS!(2, I, 4) + careerPRS!(3, I, 4) + careerPRS!(4, I, 4) + careerPRS!(5, I, 4) + careerPRS!(6, I, 4) + careerPRS!(7, I, 4) + careerPRS!(8, I, 4) + careerPRS!(9, I, 4) + careerPRS!(10, I, 4) + careerPRS!(11, I, 4) + careerPRS!(12, I, 4) + careerPRS!(13, I, 4) + careerPRS!(14, I, 4) + careerPRS!(15, I, 4)

                        For V = 1 To VV
                            Color 7: Print Tab(22); careerPRS!(V, I, 5);

                            Print Tab(32); Using "###"; careerPRS!(V, I, 0);
                            Print Tab(42); Using "####"; careerPRS!(V, I, 1);

                            If careerPRS!(V, I, 0) = 0 Or careerPRS!(V, I, 1) = 0 Then Print Tab(52); " 00";
                            If careerPRS!(V, I, 0) > 0 Then Print Tab(52); Using "##.#"; careerPRS!(V, I, 1) / careerPRS!(V, I, 0);

                            Print Tab(62); Using "##"; careerPRS!(V, I, 2);
                            Print Tab(70); Using "##"; careerPRS!(V, I, 3);
                            Print Tab(72); careerPRS$(V, I, 0)
                        Next V

                        VV = careerPRS!(1, I, 4) + careerPRS!(2, I, 4) + careerPRS!(3, I, 4) + careerPRS!(4, I, 4) + careerPRS!(5, I, 4) + careerPRS!(6, I, 4) + careerPRS!(7, I, 4) + careerPRS!(8, I, 4) + careerPRS!(9, I, 4) + careerPRS!(10, I, 4) + careerPRS!(11, I, 4) + careerPRS!(12, I, 4) + careerPRS!(13, I, 4) + careerPRS!(14, I, 4) + careerPRS!(15, I, 4)

                        For V = 1 To VV
                            careerTT!(18) = careerTT!(18) + careerPRS!(V, I, 0)
                            careerTT!(19) = careerTT!(19) + careerPRS!(V, I, 1)
                            careerTT!(20) = careerTT!(20) + careerPRS!(V, I, 2)
                        Next V

                        careerRC!(5) = careerPRS!(1, I, 3)
                        careerRC$(5) = careerPRS$(1, I, 0)

                        For V = 2 To VV
                            If careerPRS!(V, I, 3) >= careerRC!(5) Then
                                careerRC!(5) = careerPRS!(V, I, 3)
                                careerRC$(5) = careerPRS$(V, I, 0)
                            End If
                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(25); Using "##"; VV;
                        Print Tab(32); Using "###"; careerTT!(18);
                        Print Tab(41); Using "#####"; careerTT!(19);

                        If careerTT!(18) = 0 Or careerTT!(19) = 0 Then Print Tab(52); " 00";
                        If careerTT!(18) > 0 Then Print Tab(52); Using "##.#"; careerTT!(19) / careerTT!(18);

                        Print Tab(61); Using "###"; careerTT!(20);
                        Print Tab(70); Using "##"; careerRC!(5);
                        Print Tab(72); careerRC$(5)

                        For J = 18 To 20: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 6

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2
                Print "PUNTERS           YEAR     ATT.     YARDS      AVG."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 9

                    If carviewA6$(PX, I) <> "" And carviewA6$(PX, I) <> "XXX" Then

                        Locate 3: Color 15: Print Left$(carviewA6$(PX, I), 13);

                        VV = careerPUS!(1, I, 2) + careerPUS!(2, I, 2) + careerPUS!(3, I, 2) + careerPUS!(4, I, 2) + careerPUS!(5, I, 2) + careerPUS!(6, I, 2) + careerPUS!(7, I, 2) + careerPUS!(8, I, 2) + careerPUS!(9, I, 2) + careerPUS!(10, I, 2) + careerPUS!(11, I, 2) + careerPUS!(12, I, 2) + careerPUS!(13, I, 2) + careerPUS!(14, I, 2) + careerPUS!(15, I, 2)

                        For V = 1 To VV
                            Color 7: Print Tab(18); careerPUS!(V, I, 3);
                            Print Tab(28); Using "###"; careerPUS!(V, I, 0);
                            Print Tab(38); Using "####"; careerPUS!(V, I, 1);

                            If careerPUS!(V, I, 0) = 0 Or careerPUS!(V, I, 1) = 0 Then Print Tab(47); "00.0";
                            If careerPUS!(V, I, 0) > 0 Then Print Tab(47); Using "##.#"; careerPUS!(V, I, 1) / careerPUS!(V, I, 0);
                        Next V

                        VV = careerPUS!(1, I, 2) + careerPUS!(2, I, 2) + careerPUS!(3, I, 2) + careerPUS!(4, I, 2) + careerPUS!(5, I, 2) + careerPUS!(6, I, 2) + careerPUS!(7, I, 2) + careerPUS!(8, I, 2) + careerPUS!(9, I, 2) + careerPUS!(10, I, 2) + careerPUS!(11, I, 2) + careerPUS!(12, I, 2) + careerPUS!(13, I, 2) + careerPUS!(14, I, 2) + careerPUS!(15, I, 2)

                        For V = 1 To VV
                            careerTT!(21) = careerTT!(21) + careerPUS!(V, I, 0)
                            careerTT!(22) = careerTT!(22) + careerPUS!(V, I, 1)
                        Next V

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(21); Using "##"; VV;
                        Print Tab(27); Using "####"; careerTT!(21);
                        Print Tab(37); Using "#####"; careerTT!(22);

                        If careerTT!(21) = 0 Or careerTT!(22) = 0 Then Print Tab(47); " 00"
                        If careerTT!(21) > 0 Then Print Tab(47); Using "##.#"; careerTT!(22) / careerTT!(21)

                        For J = 21 To 22: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 7

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2
                Print "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50- +"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 9

                    If carviewA7$(PX, I) <> "" And carviewA7$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewA7$(PX, I), 13);

                        VV = careerFGS!(1, I, 15) + careerFGS!(2, I, 15) + careerFGS!(3, I, 15) + careerFGS!(4, I, 15) + careerFGS!(5, I, 15) + careerFGS!(6, I, 15) + careerFGS!(7, I, 15) + careerFGS!(8, I, 15) + careerFGS!(9, I, 15) + careerFGS!(10, I, 15) + careerFGS!(11, I, 15) + careerFGS!(12, I, 15) + careerFGS!(13, I, 15) + careerFGS!(14, I, 15) + careerFGS!(15, I, 15)

                        For V = 1 To VV
                            Color 7: Print Tab(13); careerFGS!(V, I, 16);

                            Print Tab(19); Using "###"; careerFGS!(V, I, 3);
                            Print Tab(23); Using "###"; careerFGS!(V, I, 2);

                            If careerFGS!(V, I, 2) = 0 Or careerFGS!(V, I, 3) = 0 Then Print Tab(27); "  0";
                            If careerFGS!(V, I, 2) > 0 Then Print Tab(27); Using "###"; (careerFGS!(V, I, 2) / careerFGS!(V, I, 3)) * 100;

                            Print Tab(32); Using "###"; careerFGS!(V, I, 1);
                            Print Tab(36); Using "###"; careerFGS!(V, I, 0);

                            If careerFGS!(V, I, 0) = 0 Or careerFGS!(V, I, 1) = 0 Then Print Tab(40); "  0";
                            If careerFGS!(V, I, 0) > 0 Then Print Tab(40); Using "###"; (careerFGS!(V, I, 0) / careerFGS!(V, I, 1)) * 100;

                            Print Tab(46); Using "##"; careerFGS!(V, I, 4);
                            Print Tab(49); Using "##"; careerFGS!(V, I, 5);: Print "-";
                            Print Tab(52); Using "##"; careerFGS!(V, I, 10);
                            Print Tab(55); Using "##"; careerFGS!(V, I, 6);: Print "-";
                            Print Tab(58); Using "##"; careerFGS!(V, I, 11);
                            Print Tab(61); Using "##"; careerFGS!(V, I, 7);: Print "-";
                            Print Tab(64); Using "##"; careerFGS!(V, I, 12);
                            Print Tab(67); Using "##"; careerFGS!(V, I, 8);: Print "-";
                            Print Tab(70); Using "##"; careerFGS!(V, I, 13);
                            Print Tab(73); Using "##"; careerFGS!(V, I, 9);: Print "-";
                            Print Tab(76); Using "##"; careerFGS!(V, I, 14)
                        Next V

                        VV = careerFGS!(1, I, 15) + careerFGS!(2, I, 15) + careerFGS!(3, I, 15) + careerFGS!(4, I, 15) + careerFGS!(5, I, 15) + careerFGS!(6, I, 15) + careerFGS!(7, I, 15) + careerFGS!(8, I, 15) + careerFGS!(9, I, 15) + careerFGS!(10, I, 15) + careerFGS!(11, I, 15) + careerFGS!(12, I, 15) + careerFGS!(13, I, 15) + careerFGS!(14, I, 15) + careerFGS!(15, I, 15)

                        For V = 1 To VV
                            careerTT!(23) = careerTT!(23) + careerFGS!(V, I, 0)
                            careerTT!(24) = careerTT!(24) + careerFGS!(V, I, 1)
                            careerTT!(34) = careerTT!(34) + careerFGS!(V, I, 2)
                            careerTT!(35) = careerTT!(35) + careerFGS!(V, I, 3)
                            careerTT!(36) = careerTT!(36) + careerFGS!(V, I, 5)
                            careerTT!(37) = careerTT!(37) + careerFGS!(V, I, 6)
                            careerTT!(38) = careerTT!(38) + careerFGS!(V, I, 7)
                            careerTT!(39) = careerTT!(39) + careerFGS!(V, I, 8)
                            careerTT!(40) = careerTT!(40) + careerFGS!(V, I, 9)
                            careerTT!(41) = careerTT!(41) + careerFGS!(V, I, 10)
                            careerTT!(42) = careerTT!(42) + careerFGS!(V, I, 11)
                            careerTT!(43) = careerTT!(43) + careerFGS!(V, I, 12)
                            careerTT!(44) = careerTT!(44) + careerFGS!(V, I, 13)
                            careerTT!(45) = careerTT!(45) + careerFGS!(V, I, 14)
                        Next V

                        careerRC!(6) = careerFGS!(1, I, 4)

                        For V = 2 To VV
                            If careerFGS!(V, I, 4) >= careerRC!(6) Then
                                careerRC!(6) = careerFGS!(V, I, 4)
                            End If
                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(16); VV;
                        Print Tab(19); Using "###"; careerTT!(35);
                        Print Tab(23); Using "###"; careerTT!(34);

                        If careerTT!(34) = 0 Or careerTT!(35) = 0 Then Print Tab(27); "  0";
                        If careerTT!(34) > 0 Then Print Tab(27); Using "###"; (careerTT!(34) / careerTT!(35)) * 100;

                        Print Tab(32); Using "###"; careerTT!(24);
                        Print Tab(36); Using "###"; careerTT!(23);

                        If careerTT!(23) = 0 Or careerTT!(24) = 0 Then Print Tab(40); "  0";
                        If careerTT!(23) > 0 Then Print Tab(40); Using "###"; (careerTT!(23) / careerTT!(24)) * 100;

                        Print Tab(46); Using "##"; careerRC!(6);
                        Print Tab(49); Using "##"; careerTT!(36);: Print "-";
                        Print Tab(52); Using "##"; careerTT!(41);
                        Print Tab(55); Using "##"; careerTT!(37);: Print "-";
                        Print Tab(58); Using "##"; careerTT!(42);
                        Print Tab(61); Using "##"; careerTT!(38);: Print "-";
                        Print Tab(64); Using "##"; careerTT!(43);
                        Print Tab(67); Using "##"; careerTT!(39);: Print "-";
                        Print Tab(70); Using "##"; careerTT!(44);
                        Print Tab(73); Using "##"; careerTT!(40);: Print "-";
                        Print Tab(76); Using "##"; careerTT!(45)

                        For J = 23 To 45: careerTT!(J) = 0: Next

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20:
                                Locate T
                                Print String$(90, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 8

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2
                Print "INTERCEPTORS        YEAR     INTERCEPTIONS"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 54

                    If carviewDI$(PX, I) <> "" And carviewDI$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewDI$(PX, I), 13);

                        VV = careerGIS!(1, I, 1) + careerGIS!(2, I, 1) + careerGIS!(3, I, 1) + careerGIS!(4, I, 1) + careerGIS!(5, I, 1) + careerGIS!(6, I, 1) + careerGIS!(7, I, 1) + careerGIS!(8, I, 1) + careerGIS!(9, I, 1) + careerGIS!(10, I, 1) + careerGIS!(11, I, 1) + careerGIS!(12, I, 1) + careerGIS!(13, I, 1) + careerGIS!(14, I, 1) + careerGIS!(15, I, 1)

                        For V = 1 To VV
                            Color 7: Print Tab(20); careerGIS!(V, I, 2);
                            Print Tab(34); Using "###"; careerGIS!(V, I, 0);
                        Next V

                        1820 '
                        VV = careerGIS!(1, I, 1) + careerGIS!(2, I, 1) + careerGIS!(3, I, 1) + careerGIS!(4, I, 1) + careerGIS!(5, I, 1) + careerGIS!(6, I, 1) + careerGIS!(7, I, 1) + careerGIS!(8, I, 1) + careerGIS!(9, I, 1) + careerGIS!(10, I, 1) + careerGIS!(11, I, 1) + careerGIS!(12, I, 1) + careerGIS!(13, I, 1) + careerGIS!(14, I, 1) + careerGIS!(15, I, 1)

                        For V = 1 To VV
                            careerTT!(32) = careerTT!(32) + careerGIS!(V, I, 0)
                        Next V

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(23); Using "##"; VV;
                        Print Tab(33); Using "####"; careerTT!(32)

                        careerTT!(32) = 0

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 9

                Cls

                Color 11
                Locate 1: Print Z$
                Color 12
                Locate 2
                Print "DEFENSIVE SACKS     YEAR        SACKS"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 54

                    If carviewDS$(PX, I) <> "" And carviewDS$(PX, I) <> "XXX" Then

                        Color 15
                        Locate 3: Print Left$(carviewDS$(PX, I), 13);

                        VV = careerGSS!(1, I, 1) + careerGSS!(2, I, 1) + careerGSS!(3, I, 1) + careerGSS!(4, I, 1) + careerGSS!(5, I, 1) + careerGSS!(6, I, 1) + careerGSS!(7, I, 1) + careerGSS!(8, I, 1) + careerGSS!(9, I, 1) + careerGSS!(10, I, 1) + careerGSS!(11, I, 1) + careerGSS!(12, I, 1) + careerGSS!(13, I, 1) + careerGSS!(14, I, 1) + careerGSS!(15, I, 1)

                        For V = 1 To VV
                            Color 7: Print Tab(20); careerGSS!(V, I, 2);
                            Print Tab(34); Using "###"; careerGSS!(V, I, 0);
                        Next V

                        VV = careerGSS!(1, I, 1) + careerGSS!(2, I, 1) + careerGSS!(3, I, 1) + careerGSS!(4, I, 1) + careerGSS!(5, I, 1) + careerGSS!(6, I, 1) + careerGSS!(7, I, 1) + careerGSS!(8, I, 1) + careerGSS!(9, I, 1) + careerGSS!(10, I, 1) + careerGSS!(11, I, 1) + careerGSS!(12, I, 1) + careerGSS!(13, I, 1) + careerGSS!(14, I, 1) + careerGSS!(15, I, 1)

                        For V = 1 To VV
                            careerTT!(33) = careerTT!(33) + careerGSS!(V, I, 0)

                        Next V

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(23); Using "##"; VV;
                        Print Tab(33); Using "####"; careerTT!(33)

                        careerTT!(33) = 0

                        Print
                        Print
                        Print
                        Color 14
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 3 To 20: Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

        End Select

        If escPressed% = 0 Then
            Cls

			Color 14

            Do
				Locate 5, 20
				Print "VIEW AGAIN (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or escPressed% = 1 Or targetFile$ <> ""

End Sub
