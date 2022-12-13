Sub CAREER ()

    _Title "3-in-1 Football - Career Files Menu"

    DefInt A-Z

    ReDim menuChoices$(1 To 8)

    TMR = 6420
    NB = -1
    NC = -1
    ND = -1
    NE = -1
    NF = -1
    NG = -1
    NH = -1
    NI = -1
    NJ = 0
    NK = 0
    D = 0
    JX = 0
    VV = 0

    Do

        Color 7, 0

        Cls

        Call TITLE_UTILS
        menuChoices$(1) = "CREATE AND MERGE INITIAL STAT FILE"
        menuChoices$(2) = "MERGE / UPDATE CAREER FILES       "
        menuChoices$(3) = "VIEW CAREER FILE                  "
        menuChoices$(4) = "PRINT CAREER FILE                 "
        menuChoices$(5) = "EDIT CAREER FILE                  "
        menuChoices$(6) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(7) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 15, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call CreateMergeInitial
                Case 2: Call CARMERGE
                Case 3: Call CARVIEW
                Case 4: Call CARPRNT
                Case 5: Call CAREDIT
                Case 6: Call ViewPrintCareer
                Case 7: Cls
            End Select
        End If

    Loop Until Selection% = 7 Or ExitCode% = 27

End Sub

'----------------------------------------
'     CreateMergeInitial Subroutine
'----------------------------------------
'...explanation...
Sub CreateMergeInitial ()

    Shared careerA1$(), careerA2$(), careerA3$(), careerA4$(), careerA5$(), careerA6$(), careerA7$()
    Shared careerDI$(), careerDS$()
    Shared careerQBS$(), careerRBS$()

    Shared Z!(), Z1!(), Z2!()

    Dim careerAF!(9, 4), careerAM!(9, 4)
    Dim careerLR!(50), careerLC!(60), careerLP!(20), careerLK!(15)
    Dim careerPK!(15)
    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared LF!(), LI!()
    Shared QBBS!(), RBBS!(), WRRS!()

    Shared KRRS$(), PRRS$()
    Dim careerLC$(60, 1), careerLK$(15, 1)
    Dim careerLP$(20, 1), careerLR$(50, 1)
    Dim careerPK$(15, 1)
    Shared LF$(), LI$(), careerWRS$()

    Dim careerGS%(55, 1), careerGI%(55, 2)

    Color 14, 0
    Cls
    Locate 5, 27: Print " - - - C A U T I O N - - -"

    Color 15
    Locate 8, 18: Print "THIS OPTION SHOULD ONLY BE USED WHEN CREATING"
    Locate , 18: Print "AND MERGING THE INITIAL STAT FILE.  IF YOU ARE"
    Locate , 18: Print "ADDING AN ADDITIONAL YEAR TO AN EXISTING FILE,"
    Locate , 18: Print "            USE OPTION #2."

    Print
    Print
    Print

    Color 12
    Locate , 25: Print "DO YOU WISH TO CONTINUE? (Y/N)"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Cls
        For J = 15 To 18: Locate J: Print Space$(68): Next
        Color 11
        Locate 15, 5: Input "ENTER NAME OF CAREER STAT FILE (MAX. - 8 CHARACTERS)"; Z$

        Call GetTeam_CAREER(A$)

        B$ = A$

        For XX = 4 To 11:
            If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
        Next XX

        For XX = 12 To 14:
            If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
        Next XX

        QQ = 8

        For XX = 11 To 4 Step -1:
            If Mid$(B$, XX, 1) = "_" Then
                QQ = XX - 4
            Else
                Exit For
            End If
        Next XX

        Z1$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)
        Cls

        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear

        Locate 9, 25: Print "ANY CHANGE (YN)"
        Do
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

        If UCase$(I$) = "N" Then
            VV = 1
            Locate , 5: Print "IS THIS THE FINAL UPDATE FOR THE CURRENT SEASON? (Y/N)"

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

            If UCase$(I$) = "Y" Then JX = 1 Else JX = 0

            If Not _FileExists(diskPaths$(1) + Z1$) Then
                Cls
                Color 7: Print "UNABLE TO LOCATE STAT FILE"
                Print
                Line Input "HIT ENTER TO CONTINUE"; DUMMY$
            Else

                Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
                Locate 20, 20: Print String$(50, " ")

                Open diskPaths$(1) + Z1$ + "S" For Input As #1
                Input #1, GAMES
                Close 1

                Open diskPaths$(1) + Z1$ For Input As #1
                Input #1, A$
                For I = 0 To 9: Input #1, careerA1$(I): Next I
                For I = 0 To 5: Input #1, careerA2$(I): Next I
                For I = 0 To 3: Input #1, careerA3$(I): Next I
                For I = 0 To 2: Input #1, careerA4$(I): Next I
                For I = 0 To 2: Input #1, careerA5$(I): Next I
                Input #1, careerA6$(0)
                For I = 0 To 1: Input #1, careerA7$(I): Next I
                For J = 0 To 13
                    For K = 0 To 17: Input #1, Z2!(J, K): Next K
                Next J
                For L = 0 To 38: Input #1, Z!(L): Next L
                For L = 0 To 38: Input #1, Z1!(L): Next L
                For J = 0 To 9: Input #1, careerDI$(J): For J1 = 0 To 2: Input #1, careerGI%(J, J1): Next: Next
                For J = 0 To 14: Input #1, careerDS$(J): For J1 = 0 To 1: Input #1, careerGS%(J, J1): Next: Next
                For I = 0 To 9: Input #1, careerLR!(I): Input #1, careerLR$(I, 0): Input #1, careerLR$(I, 1): Next
                For I = 0 To 15: Input #1, careerLC!(I): Input #1, careerLC$(I, 0): Input #1, careerLC$(I, 1): Next
                For I = 0 To 3: Input #1, careerLP!(I): Input #1, careerLP$(I, 0): Input #1, careerLP$(I, 1): Next
                For I = 0 To 2: Input #1, careerLK!(I): Input #1, careerLK$(I, 0): Input #1, careerLK$(I, 1): Next
                For I = 0 To 2: Input #1, careerPK!(I): Input #1, careerPK$(I, 0): Input #1, careerPK$(I, 1): Next
                For I = 0 To 1: Input #1, LF!(I): Input #1, LF$(I): For I1 = 0 To 4: Input #1, careerAM!(I, I1): Input #1, careerAF!(I, I1): Next: Next
                For I = 0 To 9: Input #1, LI!(I): Input #1, LI$(I, 0): Input #1, LI$(I, 1): Next
                Close #1

                For I = 0 To 9
                    T% = Len(careerA1$(I))
                    If T% = 0 Then careerA1$(I) = "XXX"
                Next

                For I = 0 To 5
                    T% = Len(careerA2$(I))
                    If T% = 0 Then careerA2$(I) = "XXX"
                Next

                For I = 0 To 3
                    T% = Len(careerA3$(I))
                    If T% = 0 Then careerA3$(I) = "XXX"
                Next

                For I = 0 To 2
                    T% = Len(careerA4$(I))
                    If T% = 0 Then careerA4$(I) = "XXX"
                Next

                For I = 0 To 2
                    T% = Len(careerA5$(I))
                    If T% = 0 Then careerA5$(I) = "XXX"
                Next

                For I = 0 To 1
                    T% = Len(careerA7$(I))
                    If T% = 0 Then careerA7$(I) = "XXX"
                Next

                For I = 0 To 9
                    T% = Len(careerDI$(I))
                    If T% = 0 Then careerDI$(I) = "XXX"
                Next

                For I = 0 To 14
                    T% = Len(careerDS$(I))
                    If T% = 0 Then careerDS$(I) = "XXX"
                Next

                For I = 0 To 49

                    N = 1

                    If careerA1$(I) <> "END" And careerA1$(I) <> "XXX" And careerA1$(I) <> "" Then
                        NB = NB + 1
                    Else
                        careerA1$(I) = ""
                    End If

                    For J = 0 To 5
                        RBBS!(N, I, J) = Z2!(I, J)
                    Next J

                    RBBS!(N, I, 6) = careerLR!(I): careerRBS$(N, I, 0) = careerLR$(I, 0)
                    RBBS!(N, I, 9) = careerLC!(I): careerRBS$(N, I, 1) = careerLC$(I, 0)
                    RBBS!(N, I, 7) = 1: RBBS!(N, I, 8) = statYear

                Next I

                For I = 0 To 39

                    N = 1

                    If careerA2$(I) <> "END" And careerA2$(I) <> "XXX" And careerA2$(I) <> "" Then
                        NC = NC + 1
                    Else
                        careerA2$(I) = ""
                    End If

                    For J = 6 To 8
                        WRRS!(N, I, J - 6) = Z2!(I, J)
                    Next J

                    WRRS!(N, I, 3) = careerLC!(I + 10)
                    careerWRS$(N, I, 0) = careerLC$(I + 10, 0)
                    WRRS!(N, I, 4) = 1
                    WRRS!(N, I, 5) = statYear

                Next I

                For I = 0 To 19

                    N = 1

                    If careerA3$(I) <> "END" And careerA3$(I) <> "XXX" And careerA3$(I) <> "" Then
                        ND = ND + 1
                    Else
                        careerA3$(I) = ""
                    End If

                    For J = 9 To 13:
                        QBBS!(N, I, J - 9) = Z2!(I, J)
                    Next J

                    QBBS!(N, I, 5) = careerLP!(I)
                    careerQBS$(N, I, 0) = careerLP$(I, 0)
                    QBBS!(N, I, 6) = 1
                    QBBS!(N, I, 7) = statYear

                Next I

                For I = 0 To 14

                    N = 1

                    If careerA4$(I) <> "END" And careerA4$(I) <> "XXX" And careerA4$(I) <> "" Then

                        NE = NE + 1
                    Else
                        careerA4$(I) = ""
                    End If

                    For J = 14 To 16:
                        careerKRS!(N, I, J - 14) = Z2!(I, J)
                    Next J

                    careerKRS!(N, I, 3) = careerLK!(I): KRRS$(N, I, 0) = careerLK$(I, 0)
                    careerKRS!(N, I, 4) = 1: careerKRS!(N, I, 5) = statYear

                Next I

                For I = 0 To 14

                    N = 1

                    If careerA5$(I) <> "END" And careerA5$(I) <> "XXX" And careerA5$(I) <> "" Then
                        NF = NF + 1
                    Else
                        careerA5$(I) = ""
                    End If

                    For J = 14 To 16:
                        careerPRS!(N, I, J - 14) = Z2!(I + 3, J)
                    Next J

                    careerPRS!(N, I, 3) = careerPK!(I)
                    PRRS$(N, I, 0) = careerPK$(I, 0)
                    careerPRS!(N, I, 4) = 1
                    careerPRS!(N, I, 5) = statYear

                Next I

                For I = 0 To 9

                    N = 1

                    If careerA6$(I) <> "END" And careerA6$(I) <> "XXX" And careerA6$(I) <> "" Then
                        NG = NG + 1
                    Else
                        careerA6$(I) = ""
                    End If

                    careerPUS!(N, I, 0) = Z!(7)
                    careerPUS!(N, I, 1) = Z!(8)
                    careerPUS!(N, I, 2) = 1
                    careerPUS!(N, I, 3) = statYear
                Next I

                For I = 0 To 9

                    N = 1

                    If careerA7$(I) <> "END" And careerA7$(I) <> "XXX" And careerA7$(I) <> "" Then
                        NH = NH + 1
                    Else
                        careerA7$(I) = ""
                    End If

                    If I = 0 Or I = 2 Or I = 4 Or I = 6 Or I = 8 Then

                        For J = 3 To 6
                            careerFGS!(N, I, J - 3) = Z2!(J, 17)
                        Next J

                        careerFGS!(N, I, 4) = LF!(I)
                        careerFGS!(N, I, 15) = 1: careerFGS!(N, I, 16) = statYear

                        For I1 = 0 To 4: careerFGS!(N, I, I1 + 5) = careerAM!(I, I1): Next
                        For I1 = 0 To 4: careerFGS!(N, I, I1 + 10) = careerAF!(I, I1): Next

                    End If

                    If I = 1 Or I = 3 Or I = 5 Or I = 7 Or I = 9 Then

                        For J = 7 To 10:
                            careerFGS!(N, I, J - 7) = Z2!(J, 17)
                        Next J

                        careerFGS!(N, I, 4) = LF!(I)
                        careerFGS!(N, I, 15) = 1: careerFGS!(N, I, 16) = statYear

                        For I1 = 0 To 4: careerFGS!(N, I, I1 + 5) = careerAM!(I, I1): Next
                        For I1 = 0 To 4: careerFGS!(N, I, I1 + 10) = careerAF!(I, I1): Next

                    End If

                Next I

                For I = 0 To 54

                    N = 1

                    If careerDI$(I) <> "END" And careerDI$(I) <> "XXX" And careerDI$(I) <> "" Then
                        NJ = NJ + 1
                    Else
                        careerDI$(I) = ""
                    End If

                    careerGIS!(N, I, 0) = careerGI%(I, 0)
                    careerGIS!(N, I, 1) = 1
                    careerGIS!(N, I, 2) = statYear
                Next I

                For I = 0 To 54
                    N = 1

                    If careerDS$(I) <> "END" And careerDS$(I) <> "XXX" And careerDS$(I) <> "" Then
                        NK = NK + 1
                    Else
                        careerDS$(I) = ""
                    End If

                    careerGSS!(N, I, 0) = careerGS%(I, 0)
                    careerGSS!(N, I, 1) = 1
                    careerGSS!(N, I, 2) = statYear

                Next I

                If JX = 1 Then Call PerformFinalUpdate(Z$)
                'If JX = 0 Then 705

                Locate 20, 20: Color 15: Print "MERGING CAREER STATS . . . . PLEASE WAIT"
                Locate 20, 20: Print Space$(50)

                Open diskPaths$(1) + Z$ + ".CAR" For Output As #2
                Write #2, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK
                For I = 0 To 49: Write #2, careerA1$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 39: Write #2, careerA2$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 19: Write #2, careerA3$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 14: Write #2, careerA4$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 14: Write #2, careerA5$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 9: Write #2, careerA6$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 9: Write #2, careerA7$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 54: Write #2, careerDI$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For I = 0 To 54: Write #2, careerDS$(I): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next
                For N = 1 To 15: For J = 0 To 49: For K = 0 To 9: Write #2, RBBS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 49: Write #2, careerRBS$(N, J, 0), careerRBS$(N, J, 1): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 39: For K = 0 To 5: Write #2, WRRS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 39: Write #2, careerWRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 19: For K = 0 To 7: Write #2, QBBS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 19: Write #2, careerQBS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #2, careerKRS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 14: Write #2, KRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 14: For K = 0 To 5: Write #2, careerPRS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 14: Write #2, PRRS$(N, J, 0): Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 9: For K = 0 To 3: Write #2, careerPUS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 9: For K = 0 To 16: Write #2, careerFGS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #2, careerGIS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                For N = 1 To 15: For J = 0 To 54: For K = 0 To 2: Write #2, careerGSS!(N, J, K): Next K: Locate 21, 38: Color 5: TMR = TMR - 1: Print Using "####"; TMR: Next J: Next N
                Close #2

            End If

        End If

    End If

End Sub

'----------------------------------------
'      ViewPrintCareer Subroutine
'----------------------------------------
'...explanation...
Sub ViewPrintCareer ()

    Shared careerRBC$()

    Shared DFC$(), FGC$(), KRC$()
    Shared PRC$(), PUC$(), QBC$()
    Shared RBR$(), WRC$()

    Shared DFYR%(), FGYR%(), KRYR%()
    Shared PRYR%(), PUYR%(), QBYR%()
    Shared RBYR%(), WRYR%()

    Shared DFFC!(), FGGC!(), KRRC!()
    Shared PRRC!(), PUUC!(), QBBC!()
    Shared RBBC!(), RBBS!(), WRRC!()

    Do
        'Display list of files...

        Spec$ = diskPaths$(1) + "*.CLD"
        Count% = FCount%(Spec$)
        If Count% = 0 Then
            4500 'GoSub 4600
            Call CSMsgBox("NO CAREER FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)
            Cls

        Else

            ReDim careerLeaderFiles$(0 To Count%)
            For X = 1 To Count%
                careerLeaderFiles$(X) = Space$(12)
            Next
            careerLeaderFiles$(0) = Spec$
            Call ReadFile(careerLeaderFiles$())

            Do
                Cls
                Color , 1
                Cls
                Locate , 28
                Color 14
                Print "SELECT FILE TO VIEW": Color 15
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
                Print "-SELECTS FILE";

                _MouseShow "DEFAULT"
                MAMenu careerLeaderFiles$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
                _MouseHide

                If ExitCode% = 27 Then Exit Sub

                targetFile$ = careerLeaderFiles$(Selection%)
                Locate 2, 33: Print targetFile$
                Locate , 33: Print "ANY CHANGE (YN)";
                Color 15, 0

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Loop Until UCase$(I$) = "N"

            targetFile$ = RTrim$(targetFile$)
            ZZ$ = Left$(targetFile$, Len(targetFile$) - 4)

            Open diskPaths$(1) + ZZ$ + ".CLD" For Input As #1
            For I = 0 To 9: Input #1, RBBC!(I), careerRBC$(I), RBYR%(I): Next
            Input #1, RBR$(0), RBR$(1)
            For I = 0 To 4: Input #1, WRRC!(I), WRC$(I), WRYR%(I): Next
            Input #1, WRR$
            For I = 0 To 8: Input #1, QBBC!(I), QBC$(I), QBYR%(I): Next
            Input #1, QBR$
            For I = 0 To 4: Input #1, KRRC!(I), KRC$(I), KRYR%(I): Next
            Input #1, KRR$
            For I = 0 To 4: Input #1, PRRC!(I), PRC$(I), PRYR%(I): Next
            Input #1, PRR$
            For I = 0 To 2: Input #1, PUUC!(I), PUC$(I), PUYR%(I): Next
            For I = 0 To 6: Input #1, FGGC!(I), FGC$(I), FGYR%(I): Next
            For I = 0 To 1: Input #1, DFFC!(I), DFC$(I), DFYR%(I): Next
            Close #1

            Cls
            Locate 5, 10
            Color 12: Print "P";: Color 7: Print "rint or ";
            Color 12: Print "V";: Color 7: Print "iew"

            Do
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "V" Or UCase$(I$) = "P"

            If UCase$(I$) = "V" Then
                Do
                    Cls
                    Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                    Color 3: Print "RUSHING ATTEMPTS";
                    Color 7: Print Tab(32); Using "####"; RBBC!(0);: Print Tab(50); careerRBC$(0);: Print Tab(64); RBYR%(0)
                    Color 3: Print "RUSHING YARDS";
                    Color 7: Print Tab(32); Using "####"; RBBC!(1);: Print Tab(50); careerRBC$(1);: Print Tab(64); RBYR%(1)
                    Color 3: Print "RUSHING AVERAGE";
                    Color 7: Print Tab(32); Using "##.#"; RBBC!(2);: Print Tab(50); careerRBC$(2);: Print Tab(64); RBYR%(2)
                    Color 3: Print "RUSHING TOUCHDOWNS";
                    Color 7: Print Tab(33); Using "###"; RBBC!(3);: Print Tab(50); careerRBC$(3);: Print Tab(64); RBYR%(3)
                    Color 3: Print "LONGEST RUN";
                    Color 7: Print Tab(33); Using "###"; RBBC!(8);: Print Tab(36); RBR$(0);: Print Tab(50); careerRBC$(8);: Print Tab(64); RBYR%(8)
                    Color 3: Print "RECEPTIONS (BACKS)";
                    Color 7: Print Tab(32); Using "####"; RBBC!(4);: Print Tab(50); careerRBC$(4);: Print Tab(64); RBYR%(4)
                    Color 3: Print "RECEIVING YARDS (BACKS)";
                    Color 7: Print Tab(32); Using "####"; RBBC!(5);: Print Tab(50); careerRBC$(5);: Print Tab(64); RBYR%(5)
                    Color 3: Print "RECEIVING AVERAGE (BACKS)";
                    Color 7: Print Tab(32); Using "##.#"; RBBC!(6);: Print Tab(50); careerRBC$(6);: Print Tab(64); RBYR%(6)
                    Color 3: Print "RECEIVING TOUCHDOWNS (BACKS)";
                    Color 7: Print Tab(32); Using " ###"; RBBC!(7);: Print Tab(50); careerRBC$(7);: Print Tab(64); RBYR%(7)
                    Color 3: Print "LONGEST RECEPTION (BACKS)";
                    Color 7: Print Tab(33); Using "###"; RBBC!(9);: Print Tab(37); RBR$(1);: Print Tab(50); careerRBC$(9);: Print Tab(64); RBYR%(9)
                    Color 3: Print "RECEPTIONS";
                    Color 7: Print Tab(32); Using "####"; WRRC!(0);: Print Tab(50); WRC$(0);: Print Tab(64); WRYR%(0)
                    Color 3: Print "RECEIVING YARDS";
                    Color 7: Print Tab(32); Using "####"; WRRC!(1);: Print Tab(50); WRC$(1);: Print Tab(64); WRYR%(1)
                    Color 3: Print "RECEIVING AVERAGE";
                    Color 7: Print Tab(32); Using "##.#"; WRRC!(2);: Print Tab(50); WRC$(2);: Print Tab(64); WRYR%(2)
                    Color 3: Print "RECEIVING TOUCHDOWNS";
                    Color 7: Print Tab(32); Using " ###"; WRRC!(3);: Print Tab(50); WRC$(3);: Print Tab(64); WRYR%(3)
                    Color 3: Print "LONGEST RECEPTION";
                    Color 7: Print Tab(32); Using " ###"; WRRC!(4);: Print Tab(36); WRR$;: Print Tab(50); WRC$(4);: Print Tab(64); WRYR%(4)
                    Color 3: Print "COMPLETIONS";
                    Color 7: Print Tab(32); Using "####"; QBBC!(0);: Print Tab(50); QBC$(0);: Print Tab(64); QBYR%(0)
                    Color 3: Print "ATTEMPTS";
                    Color 7: Print Tab(32); Using "####"; QBBC!(1);: Print Tab(50); QBC$(1);: Print Tab(64); QBYR%(1)
                    Color 3: Print "INTERCEPTIONS";
                    Color 7: Print Tab(32); Using "####"; QBBC!(2);: Print Tab(50); QBC$(2);: Print Tab(64); QBYR%(2)
                    Color 15: Print: Print "HIT ANY KEY TO CONTINUE"

                    I$ = GetKeyPress$

                    Cls
                    Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                    Color 3: Print "TOUCHDOWNS";
                    Color 7: Print Tab(31); Using " ####"; QBBC!(3);: Print Tab(50); QBC$(3);: Print Tab(64); QBYR%(3)
                    Color 3: Print "PASSING YARDS";
                    Color 7: Print Tab(32); Using "####"; QBBC!(4);: Print Tab(50); QBC$(4);: Print Tab(64); QBYR%(4)
                    Color 3: Print "COMPLETION PERCENTAGE";
                    Color 7: Print Tab(32); Using "##.#"; QBBC!(5) * 100;: Print Tab(50); QBC$(5);: Print Tab(64); QBYR%(5)
                    Color 3: Print "YARDS PER COMPLETION";
                    Color 7: Print Tab(32); Using "##.#"; QBBC!(6);: Print Tab(50); QBC$(6);: Print Tab(64); QBYR%(6)
                    Color 3: Print "QUARTERBACK RATING";
                    Color 7: Print Tab(31); Using " ##.#"; QBBC!(7);: Print Tab(50); QBC$(7);: Print Tab(64); QBYR%(7)
                    Color 3: Print "LONGEST PASS";
                    Color 7: Print Tab(32); Using " ###"; QBBC!(8);: Print Tab(36); QBR$;: Print Tab(50); QBC$(8);: Print Tab(64); QBYR%(8)
                    Color 3: Print "KICK RETURN ATTEMPTS";
                    Color 7: Print Tab(33); Using "###"; KRRC!(0);: Print Tab(50); KRC$(0);: Print Tab(64); KRYR%(0)
                    Color 3: Print "KICK RETURN YARDS";
                    Color 7: Print Tab(32); Using "####"; KRRC!(1);: Print Tab(50); KRC$(1);: Print Tab(64); KRYR%(1)
                    Color 3: Print "KICK RETURN AVERAGE";
                    Color 7: Print Tab(32); Using "##.#"; KRRC!(2);: Print Tab(50); KRC$(2);: Print Tab(64); KRYR%(2)
                    Color 3: Print "KICK RETURN TOUCHDOWNS";
                    Color 7: Print Tab(32); Using " ###"; KRRC!(3);: Print Tab(50); KRC$(3);: Print Tab(64); KRYR%(3)
                    Color 3: Print "LONGEST KICK RETURN";
                    Color 7: Print Tab(32); Using " ###"; KRRC!(4);: Print Tab(36); KRR$;: Print Tab(50); KRC$(4);: Print Tab(64); KRYR%(4)
                    Color 3: Print "PUNT RETURN ATTEMPTS";
                    Color 7: Print Tab(33); Using "###"; PRRC!(0);: Print Tab(50); PRC$(0);: Print Tab(64); PRYR%(0)
                    Color 3: Print "PUNT RETURN YARDS";
                    Color 7: Print Tab(32); Using "####"; PRRC!(1);: Print Tab(50); PRC$(1);: Print Tab(64); PRYR%(1)
                    Color 3: Print "PUNT RETURN AVERAGE";
                    Color 7: Print Tab(32); Using "##.#"; PRRC!(2);: Print Tab(50); PRC$(2);: Print Tab(64); PRYR%(2)
                    Color 3: Print "PUNT RETURN TOUCHDOWNS";
                    Color 7: Print Tab(32); Using " ###"; PRRC!(3);: Print Tab(50); PRC$(3);: Print Tab(64); PRYR%(3)
                    Color 3: Print "LONGEST PUNT RETURN";
                    Color 7: Print Tab(32); Using " ###"; PRRC!(4);: Print Tab(36); PRR$;: Print Tab(50); PRC$(4);: Print Tab(64); PRYR%(4)
                    Color 3: Print "PUNT ATTEMPTS";
                    Color 7: Print Tab(32); Using "####"; PUUC!(0);: Print Tab(50); PUC$(0);: Print Tab(64); PUYR%(0)
                    Color 3: Print "PUNTING YARDS";
                    Color 7: Print Tab(32); Using "####"; PUUC!(1);: Print Tab(50); PUC$(1);: Print Tab(64); PUYR%(1)
                    Color 15: Print: Print "HIT ANY KEY TO CONTINUE"

                    I$ = GetKeyPress$

                    Cls
                    Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                    Color 3: Print "PUNTING AVERAGE";
                    Color 7: Print Tab(32); Using "##.#"; PUUC!(2);: Print Tab(50); PUC$(2);: Print Tab(64); PUYR%(2)
                    Color 3: Print "FIELD GOAL ATTEMPTS";
                    Color 7: Print Tab(32); Using "####"; FGGC!(0);: Print Tab(50); FGC$(0);: Print Tab(64); FGYR%(0)
                    Color 3: Print "FIELD GOALS MADE";
                    Color 7: Print Tab(32); Using "####"; FGGC!(1);: Print Tab(50); FGC$(1);: Print Tab(64); FGYR%(1)
                    Color 3: Print "FIELD GOAL PERCENTAGE";
                    Color 7: Print Tab(31); Using "###.#"; FGGC!(2) * 100;: Print Tab(50); FGC$(2);: Print Tab(64); FGYR%(2)
                    Color 3: Print "LONGEST FIELD GOAL MADE";
                    Color 7: Print Tab(34); Using "##"; FGGC!(6);: Print Tab(50); FGC$(6);: Print Tab(64); FGYR%(6)
                    Color 3: Print "EXTRA POINT ATTEMPTS";
                    Color 7: Print Tab(32); Using "####"; FGGC!(3);: Print Tab(50); FGC$(3);: Print Tab(64); FGYR%(3)
                    Color 3: Print "EXTRA POINTS MADE";
                    Color 7: Print Tab(32); Using "####"; FGGC!(4);: Print Tab(50); FGC$(4);: Print Tab(64); FGYR%(4)
                    Color 3: Print "EXTRA POINT PERCENTAGE";
                    Color 7: Print Tab(31); Using "###.#"; FGGC!(5) * 100;: Print Tab(50); FGC$(5);: Print Tab(64); FGYR%(5)
                    Color 15: Print: Print "VIEW FILE AGAIN (Y/N)"

                    Do
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Loop Until UCase$(I$) = "N"

            End If

            If UCase$(I$) = "P" Then
                LPrint Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                LPrint "RUSHING ATTEMPTS";
                LPrint Tab(32); Using "####"; RBBC!(0);: LPrint Tab(50); careerRBC$(0);: LPrint Tab(64); RBYR%(0)
                LPrint "RUSHING YARDS";
                LPrint Tab(32); Using "####"; RBBC!(1);: LPrint Tab(50); careerRBC$(1);: LPrint Tab(64); RBYR%(1)
                LPrint "RUSHING AVERAGE";
                LPrint Tab(32); Using "##.#"; RBBC!(2);: LPrint Tab(50); careerRBC$(2);: LPrint Tab(64); RBYR%(2)
                LPrint "RUSHING TOUCHDOWNS";
                LPrint Tab(33); Using "###"; RBBC!(3);: LPrint Tab(50); careerRBC$(3);: LPrint Tab(64); RBYR%(3)
                LPrint "LONGEST RUN";
                LPrint Tab(33); Using "###"; RBBC!(8);
                LPrint Tab(36); RBR$(0);: LPrint Tab(50); careerRBC$(8);: LPrint Tab(64); RBYR%(8)
                LPrint "RECEPTIONS (BACKS)";
                LPrint Tab(32); Using "####"; RBBC!(4);: LPrint Tab(50); careerRBC$(4);: LPrint Tab(64); RBYR%(4)
                LPrint "RECEIVING YARDS (BACKS)";
                LPrint Tab(32); Using "####"; RBBC!(5);: LPrint Tab(50); careerRBC$(5);: LPrint Tab(64); RBYR%(5)
                LPrint "RECEIVING AVERAGE (BACKS)";
                LPrint Tab(32); Using "##.#"; RBBC!(6);: LPrint Tab(50); careerRBC$(6);: LPrint Tab(64); RBYR%(6)
                LPrint "RECEIVING TOUCHDOWNS (BACKS)";
                LPrint Tab(32); Using " ###"; RBBC!(7);: LPrint Tab(50); careerRBC$(7);: LPrint Tab(64); RBYR%(7)
                LPrint "LONGEST RECEPTION (BACKS)";
                LPrint Tab(33); Using "###"; RBBC!(9);
                LPrint Tab(37); RBR$(1);: LPrint Tab(50); careerRBC$(9);: LPrint Tab(64); RBYR%(9)
                LPrint "RECEPTIONS";
                LPrint Tab(32); Using "####"; WRRC!(0);: LPrint Tab(50); WRC$(0);: LPrint Tab(64); WRYR%(0)
                LPrint "RECEIVING YARDS";
                LPrint Tab(32); Using "####"; WRRC!(1);: LPrint Tab(50); WRC$(1);: LPrint Tab(64); WRYR%(1)
                LPrint "RECEIVING AVERAGE";
                LPrint Tab(32); Using "##.#"; WRRC!(2);: LPrint Tab(50); WRC$(2);: LPrint Tab(64); WRYR%(2)
                LPrint "RECEIVING TOUCHDOWNS";
                LPrint Tab(32); Using " ###"; WRRC!(3);: LPrint Tab(50); WRC$(3);: LPrint Tab(64); WRYR%(3)
                LPrint "LONGEST RECEPTION";
                LPrint Tab(32); Using " ###"; WRRC!(4);
                LPrint Tab(36); WRR$;: LPrint Tab(50); WRC$(4);: LPrint Tab(64); WRYR%(4)
                LPrint "COMPLETIONS";
                LPrint Tab(32); Using "####"; QBBC!(0);: LPrint Tab(50); QBC$(0);: LPrint Tab(64); QBYR%(0)
                LPrint "ATTEMPTS";
                LPrint Tab(32); Using "####"; QBBC!(1);: LPrint Tab(50); QBC$(1);: LPrint Tab(64); QBYR%(1)
                LPrint "INTERCEPTIONS";
                LPrint Tab(32); Using "####"; QBBC!(2);: LPrint Tab(50); QBC$(2);: LPrint Tab(64); QBYR%(2)
                LPrint "TOUCHDOWNS";
                LPrint Tab(31); Using " ####"; QBBC!(3);: LPrint Tab(50); QBC$(3);: LPrint Tab(64); QBYR%(3)
                LPrint "PASSING YARDS";
                LPrint Tab(32); Using "####"; QBBC!(4);: LPrint Tab(50); QBC$(4);: LPrint Tab(64); QBYR%(4)
                LPrint "COMPLETION PERCENTAGE";
                LPrint Tab(32); Using "##.#"; QBBC!(5) * 100;: LPrint Tab(50); QBC$(5);: LPrint Tab(64); QBYR%(5)
                LPrint "YARDS PER COMPLETION";
                LPrint Tab(32); Using "##.#"; QBBC!(6);: LPrint Tab(50); QBC$(6);: LPrint Tab(64); QBYR%(6)
                LPrint "QUARTERBACK RATING";
                LPrint Tab(31); Using " ##.#"; QBBC!(7);: LPrint Tab(50); QBC$(7);: LPrint Tab(64); QBYR%(7)
                LPrint "LONGEST PASS";
                LPrint Tab(32); Using " ###"; QBBC!(8);
                LPrint Tab(36); QBR$;: LPrint Tab(50); QBC$(8);: LPrint Tab(64); QBYR%(8)
                LPrint "KICK RETURN ATTEMPTS";
                LPrint Tab(33); Using "###"; KRRC!(0);: LPrint Tab(50); KRC$(0);: LPrint Tab(64); KRYR%(0)
                LPrint "KICK RETURN YARDS";
                LPrint Tab(32); Using "####"; KRRC!(1);: LPrint Tab(50); KRC$(1);: LPrint Tab(64); KRYR%(1)
                LPrint "KICK RETURN AVERAGE";
                LPrint Tab(32); Using "##.#"; KRRC!(2);: LPrint Tab(50); KRC$(2);: LPrint Tab(64); KRYR%(2)
                LPrint "KICK RETURN TOUCHDOWNS";
                LPrint Tab(32); Using " ###"; KRRC!(3);: LPrint Tab(50); KRC$(3);: LPrint Tab(64); KRYR%(3)
                LPrint "LONGEST KICK RETURN";
                LPrint Tab(32); Using " ###"; KRRC!(4);
                LPrint Tab(36); KRR$;: LPrint Tab(50); KRC$(4);: LPrint Tab(64); KRYR%(4)
                LPrint "PUNT RETURN ATTEMPTS";
                LPrint Tab(33); Using "###"; PRRC!(0);: LPrint Tab(50); PRC$(0);: LPrint Tab(64); PRYR%(0)
                LPrint "PUNT RETURN YARDS";
                LPrint Tab(32); Using "####"; PRRC!(1);: LPrint Tab(50); PRC$(1);: LPrint Tab(64); PRYR%(1)
                LPrint "PUNT RETURN AVERAGE";
                LPrint Tab(32); Using "##.#"; PRRC!(2);: LPrint Tab(50); PRC$(2);: LPrint Tab(64); PRYR%(2)
                LPrint "PUNT RETURN TOUCHDOWNS";
                LPrint Tab(32); Using " ###"; PRRC!(3);: LPrint Tab(50); PRC$(3);: LPrint Tab(64); PRYR%(3)
                LPrint "LONGEST PUNT RETURN";
                LPrint Tab(32); Using " ###"; PRRC!(4);
                LPrint Tab(36); PRR$;: LPrint Tab(50); PRC$(4);: LPrint Tab(64); PRYR%(4)
                LPrint "PUNT ATTEMPTS";
                LPrint Tab(32); Using "####"; PUUC!(0);: LPrint Tab(50); PUC$(0);: LPrint Tab(64); PUYR%(0)
                LPrint "PUNTING YARDS";
                LPrint Tab(32); Using "####"; PUUC!(1);: LPrint Tab(50); PUC$(1);: LPrint Tab(64); PUYR%(1)
                LPrint "PUNTING AVERAGE";
                LPrint Tab(32); Using "##.#"; PUUC!(2);: LPrint Tab(50); PUC$(2);: LPrint Tab(64); PUYR%(2)
                LPrint "FIELD GOAL ATTEMPTS";
                LPrint Tab(32); Using "####"; FGGC!(0);: LPrint Tab(50); FGC$(0);: LPrint Tab(64); FGYR%(0)
                LPrint "FIELD GOALS MADE";
                LPrint Tab(32); Using "####"; FGGC!(1);: LPrint Tab(50); FGC$(1);: LPrint Tab(64); FGYR%(1)
                LPrint "FIELD GOAL PERCENTAGE";
                LPrint Tab(31); Using "###.#"; FGGC!(2) * 100;: LPrint Tab(50); FGC$(2);: LPrint Tab(64); FGYR%(2)
                LPrint "LONGEST FIELD GOAL MADE";
                LPrint Tab(34); Using "##"; FGGC!(6);: LPrint Tab(50); FGC$(6);: LPrint Tab(64); FGYR%(6)
                LPrint "EXTRA POINT ATTEMPTS";
                LPrint Tab(32); Using "####"; FGGC!(3);: LPrint Tab(50); FGC$(3);: LPrint Tab(64); FGYR%(3)
                LPrint "EXTRA POINTS MADE";
                LPrint Tab(32); Using "####"; FGGC!(4);: LPrint Tab(50); FGC$(4);: LPrint Tab(64); FGYR%(4)
                LPrint "EXTRA POINT PERCENTAGE";
                LPrint Tab(31); Using "###.#"; FGGC!(5) * 100;: LPrint Tab(50); FGC$(5);: LPrint Tab(64); FGYR%(5)
                'GoTo 1

            End If

        End If 'Done checking file count

        '...Until we press ESC
    Loop Until ExitCode% = 27 Or Count% = 0

End Sub

'----------------------------------------
'      GetTeam_CAREER Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam_CAREER (A$)

    Shared PA%()

    Do

        Color 15, 0: Cls

        JJ = 6
        SN = 1
        Locate 6, 8: Color 15, 4
        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"

        I$ = GetKeyPress$

        If I$ = Chr$(27) Then
            Z$ = "END"
        Else

            If I$ = Chr$(13) Then
                Call SelectTeam_CAREER(YN$, numberTeams, T1)
            Else

                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" Then
                    Call SelectTeam_CAREER(YN$, numberTeams, T1)
                Else
                    YN$ = NY$
                    Erase teamNames$

                    numberTeams = 0
                    Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
                    Y = LOF(1)
                    Field #1, 15 As Q$(0), 640 As X$

                    For I1 = 1 To Y / 934
                        Get #1, I1

                        For I2 = 1 To 15
                            If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                        Next I2

                        If nullTeam <> 1 Then
                            numberTeams = numberTeams + 1
                            teamNames$(numberTeams) = Q$(0)
                            teamIndex%(numberTeams) = I1
                        End If

                    Next I1

                    Close 1

                    Call SelectTeam_CAREER(YN$, numberTeams, T1)

                End If

            End If

            A$ = RTrim$(Q$(1))

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color PA%(2), PA%(1): Print A$
                Color 15, 4
                Locate , 33: Print "ANY CHANGE (YN)";

                Do
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

            End If

        End If 'Done checking for ESC

    Loop Until UCase$(I$) = "N" Or Z$ = "END"

End Sub

'----------------------------------------
'      SelectTeam_CAREER Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_CAREER (yearNumber$, numberTeams, teamIdx%)

    Shared PA%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";

    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% = 27 Then
        BO% = 1
    Else
        teamIdx% = teamIndex%(Selection%)

        Open diskPaths$(0) + "FOOT." + yearNumber$ For Random As #1 Len = 934
        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)
        Get #1, teamIdx%

        'Positions 1 and 2 will hold team colors
        For I1 = 0 To 2
            PA%(I1) = CVI(Q$(I1 + 11))
        Next I1

        Close 1

    End If

End Sub

'----------------------------------------
'      PerformFinalUpdate Subroutine
'----------------------------------------
'...explanation...
Sub PerformFinalUpdate (careerFile$)

    Shared careerA1$(), careerA2$(), careerA3$(), careerA4$(), careerA5$(), careerA6$(), careerA7$()
    Shared careerDI$(), careerDS$(), careerQBS$()
    Shared careerRBC$(), careerRBS$(), careerWRS$()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()

    Shared DFC$(), FGC$(), KRC$(), KRRS$()
    Shared PRC$(), PRRS$(), PUC$(), QBC$()
    Shared RBR$(), WRC$()

    Shared DFYR%(), FGYR%(), KRYR%()
    Shared PRYR%(), PUYR%(), QBYR%()
    Shared RBYR%(), WRYR%()

    Shared DFFC!(), FGGC!(), KRRC!()
    Shared PRRC!(), PUUC!(), QBBC!(), QBBS!()
    Shared RBBC!(), RBBS!(), WRRC!(), WRRS!()

    Dim PP(15, 20), FF(15, 20), II(15, 20), YY(15, 20)

    If _FileExists(diskPaths$(1) + Left$(careerFile$, JX) + ".CLD") Then
        D = 0

        For J = 0 To 8
            D = D + 1
            X$ = Mid$(careerFile$, D, 1)
            If X$ = "." Then JX = D - 1
        Next J

        Open diskPaths$(1) + Left$(careerFile$, JX) + ".CLD" For Input As #1
        For I = 0 To 9: Input #1, RBBC!(I), careerRBC$(I), RBYR%(I): Next
        Input #1, RBR$(0), RBR$(1)
        For I = 0 To 4: Input #1, WRRC!(I), WRC$(I), WRYR%(I): Next
        Input #1, WRR$
        For I = 0 To 8: Input #1, QBBC!(I), QBC$(I), QBYR%(I): Next
        Input #1, QBR$
        For I = 0 To 4: Input #1, KRRC!(I), KRC$(I), KRYR%(I): Next
        Input #1, KRR$
        For I = 0 To 4: Input #1, PRRC!(I), PRC$(I), PRYR%(I): Next
        Input #1, PRR$
        For I = 0 To 2: Input #1, PUUC!(I), PUC$(I), PUYR%(I): Next
        For I = 0 To 6: Input #1, FGGC!(I), FGC$(I), FGYR%(I): Next
        For I = 0 To 1: Input #1, DFFC!(I), DFC$(I), DFYR%(I): Next
        Close #1

    End If

    '-----

    For I = 0 To 49

        If careerA1$(I) <> "" And careerA1$(I) <> "XXX" And careerA1$(I) <> "END" Then

            For J = 1 To 15

                If RBBS!(J, I, 8) = 0 Then Exit For

                If RBBS!(J, I, 0) > RBBC!(0) Then
                    RBBC!(0) = RBBS!(J, I, 0)
                    careerRBC$(0) = careerA1$(I)
                    RBYR%(0) = RBBS!(J, I, 8)
                End If

                If RBBS!(J, I, 1) > RBBC!(1) Then
                    RBBC!(1) = RBBS!(J, I, 1): careerRBC$(1) = careerA1$(I): RBYR%(1) = RBBS!(J, I, 8)
                End If

                If RBBS!(J, I, 0) <> 0 Then

                    If RBBS!(J, I, 1) / RBBS!(J, I, 0) > RBBC!(2) And RBBS!(J, I, 0) >= GAMES * 10 Then
                        RBBC!(2) = RBBS!(J, I, 1) / RBBS!(J, I, 0)
                        careerRBC$(2) = careerA1$(I): RBYR%(2) = RBBS!(J, I, 8)
                    End If

                End If

                If RBBS!(J, I, 2) > RBBC!(3) Then
                    RBBC!(3) = RBBS!(J, I, 2)
                    careerRBC$(3) = careerA1$(I)
                    RBYR%(3) = RBBS!(J, I, 8)
                End If

                If RBBS!(J, I, 3) > RBBC!(4) Then
                    RBBC!(4) = RBBS!(J, I, 3)
                    careerRBC$(4) = careerA1$(I)
                    RBYR%(4) = RBBS!(J, I, 8)
                End If

                If RBBS!(J, I, 4) > RBBC!(5) Then
                    RBBC!(5) = RBBS!(J, I, 4)
                    careerRBC$(5) = careerA1$(I)
                    RBYR%(5) = RBBS!(J, I, 8)
                End If

                If RBBS!(J, I, 3) <> 0 Then
                    If RBBS!(J, I, 4) / RBBS!(J, I, 3) > RBBC!(6) And RBBS!(J, I, 3) >= GAMES * 3 Then
                        RBBC!(6) = RBBS!(J, I, 4) / RBBS!(J, I, 3)
                        careerRBC$(6) = careerA1$(I)
                        RBYR%(6) = RBBS!(J, I, 8)
                    End If
                End If

                If RBBS!(J, I, 5) > RBBC!(7) Then
                    RBBC!(7) = RBBS!(J, I, 5)
                    careerRBC$(7) = careerA1$(I)
                    RBYR%(7) = RBBS!(J, I, 8)
                End If

                If RBBS!(J, I, 6) > RBBC!(8) Then
                    RBBC!(8) = RBBS!(J, I, 6): careerRBC$(8) = careerA1$(I): RBYR%(8) = RBBS!(J, I, 8): RBR$(0) = careerRBS$(J, I, 0)
                End If

                If RBBS!(J, I, 9) > RBBC!(9) Then
                    RBBC!(9) = RBBS!(J, I, 9)
                    careerRBC$(9) = careerA1$(I)
                    RBYR%(9) = RBBS!(J, I, 8)
                    RBR$(1) = careerRBS$(J, I, 1)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 39

        If careerA2$(I) <> "" And careerA2$(I) <> "XXX" And careerA2$(I) <> "END" Then

            For J = 1 To 15

                If WRRS!(J, I, 5) = 0 Then
                    Exit For
                Else

                    If WRRS!(J, I, 0) > WRRC!(0) Then
                        WRRC!(0) = WRRS!(J, I, 0)
                        WRC$(0) = careerA2$(I)
                        WRYR%(0) = WRRS!(J, I, 5)
                    End If

                    If WRRS!(J, I, 1) > WRRC!(1) Then
                        WRRC!(1) = WRRS!(J, I, 1): WRC$(1) = careerA2$(I): WRYR%(1) = WRRS!(J, I, 5)
                    End If

                    If WRRS!(J, I, 0) <> 0 Then
                        If WRRS!(J, I, 1) / WRRS!(J, I, 0) > WRRC!(2) And WRRS!(J, I, 0) >= GAMES * 3 Then
                            WRRC!(2) = WRRS!(J, I, 1) / WRRS!(J, I, 0): WRC$(2) = careerA2$(I): WRYR%(2) = WRRS!(J, I, 5)
                        End If
                    End If

                    If WRRS!(J, I, 2) > WRRC!(3) Then
                        WRRC!(3) = WRRS!(J, I, 2)
                        WRC$(3) = careerA2$(I)
                        WRYR%(3) = WRRS!(J, I, 5)
                    End If

                    If WRRS!(J, I, 3) > WRRC!(4) Then
                        WRRC!(4) = WRRS!(J, I, 3)
                        WRC$(4) = careerA2$(I)
                        WRYR%(4) = WRRS!(J, I, 5)
                        WRR$ = careerWRS$(J, I, 0)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 19

        If careerA3$(I) <> "" And careerA3$(I) <> "XXX" And careerA3$(I) <> "END" Then

            For J = 1 To 15:

                If QBBS!(J, I, 0) > 0 Then QBBS!(J, I, 11) = Int(QBBS!(J, I, 1) / QBBS!(J, I, 0) * 1000) / 10

                If QBBS!(J, I, 0) = 0 Then
                    QBBS!(J, I, 11) = 0
                    QBBS!(J, I, 8) = 0
                    QBBS!(J, I, 10) = 0
                    QBBS!(J, I, 9) = 0
                Else
                    QBBS!(J, I, 8) = Int(QBBS!(J, I, 5) / QBBS!(J, I, 0) * 1000) / 10
                    QBBS!(J, I, 10) = Int(QBBS!(J, I, 4) / QBBS!(J, I, 0) * 100) / 100
                    QBBS!(J, I, 9) = Int(QBBS!(J, I, 3) / QBBS!(J, I, 0) * 1000) / 10
                End If

                PP(J, I) = QBBS!(J, I, 11) - 30
                If PP(J, I) < 0 Then
                    PP(J, I) = 0
                Else
                    If PP(J, I) > 77.5 Then
                        PP(J, I) = 2.375
                    Else
                        PP(J, I) = PP(J, I) * .05
                    End If
                End If

                If QBBS!(J, I, 8) > 11.9 Then
                    FF(J, I) = 2.375
                Else
                    FF(J, I) = QBBS!(J, I, 8) * .2
                End If

                If QBBS!(J, I, 9) = 0 Then
                    II(J, I) = 0
                Else
                    If QBBS!(J, I, 9) > 9.5 Then
                        II(J, I) = 0
                    Else
                        II(J, I) = 2.375 - (.25 * QBBS!(J, I, 9))
                    End If
                End If

                QBBS!(J, I, 10) = QBBS!(J, I, 10) - 3

                If QBBS!(J, I, 10) < 0 Then
                    YY(J, I) = 0
                Else
                    If QBBS!(J, I, 10) > 12.5 Then
                        YY(J, I) = 2.375
                    Else
                        D = Int(QBBS!(J, I, 10) * 100) / 2
                        If D = QBBS!(J, I, 10) / 2 Then
                            YY(J, I) = QBBS!(J, I, 10) / 2 * .3 + QBBS!(J, I, 10) / 2 * .2
                        Else
                            YY(J, I) = QBBS!(J, I, 10) / 2 * .3 + QBBS!(J, I, 10) / 2 * .2 + .003
                        End If
                    End If
                End If

                QBBS!(J, I, 12) = PP(J, I) + FF(J, I) + II(J, I) + YY(J, I)
                If QBBS!(J, I, 12) > 9.5 Then
                    QBBS!(J, I, 12) = 158.3
                Else
                    QBBS!(J, I, 12) = QBBS!(J, I, 12) * 50 / 3
                End If

                QBBS!(J, I, 12) = Int(QBBS!(J, I, 12) * 10) / 10

                If QBBS!(J, I, 7) = 0 Then Exit For

                If QBBS!(J, I, 1) > QBBC!(0) Then
                    QBBC!(0) = QBBS!(J, I, 1)
                    QBC$(0) = careerA3$(I)
                    QBYR%(0) = QBBS!(J, I, 7)
                End If

                If QBBS!(J, I, 0) > QBBC!(1) Then
                    QBBC!(1) = QBBS!(J, I, 0)
                    QBC$(1) = careerA3$(I)
                    QBYR%(1) = QBBS!(J, I, 7)
                End If

                If QBBS!(J, I, 2) > QBBC!(2) Then
                    QBBC!(2) = QBBS!(J, I, 2)
                    QBC$(2) = careerA3$(I)
                    QBYR%(2) = QBBS!(J, I, 7)
                End If

                If QBBS!(J, I, 4) > QBBC!(3) Then
                    QBBC!(3) = QBBS!(J, I, 4)
                    QBC$(3) = careerA3$(I)
                    QBYR%(3) = QBBS!(J, I, 7)
                End If

                If QBBS!(J, I, 3) > QBBC!(4) Then
                    QBBC!(4) = QBBS!(J, I, 3)
                    QBC$(4) = careerA3$(I)
                    QBYR%(4) = QBBS!(J, I, 7)
                End If

                If QBBS!(J, I, 0) <> 0 Then
                    If QBBS!(J, I, 1) / QBBS!(J, I, 0) > QBBC!(5) And QBBS!(J, I, 0) >= GAMES * 15 Then
                        QBBC!(5) = QBBS!(J, I, 1) / QBBS!(J, I, 0)
                        QBC$(5) = careerA3$(I)
                        QBYR%(5) = QBBS!(J, I, 7)
                    End If
                End If

                If QBBS!(J, I, 1) <> 0 Then
                    If QBBS!(J, I, 3) / QBBS!(J, I, 1) > QBBC!(6) And QBBS!(J, I, 0) >= GAMES * 15 Then
                        QBBC!(6) = QBBS!(J, I, 3) / QBBS!(J, I, 1)
                        QBC$(6) = careerA3$(I)
                        QBYR%(6) = QBBS!(J, I, 7)
                    End If

                End If

                If QBBS!(J, I, 12) > QBBC!(7) Then
                    QBBC!(7) = QBBS!(J, I, 12): QBC$(7) = careerA3$(I): QBYR%(7) = QBBS!(J, I, 7)
                End If

                If QBBS!(J, I, 5) > QBBC!(8) Then
                    QBBC!(8) = QBBS!(J, I, 5)
                    QBC$(8) = careerA3$(I)
                    QBYR%(8) = QBBS!(J, I, 7)
                    QBR$ = careerQBS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 14

        If careerA4$(I) <> "" And careerA4$(I) <> "XXX" And careerA4$(I) <> "END" Then

            For J = 1 To 15

                If careerKRS!(J, I, 5) = 0 Then Exit For

                If careerKRS!(J, I, 0) <= KRRC!(0) Then
                    KRRC!(0) = careerKRS!(J, I, 0)
                    KRC$(0) = careerA4$(I)
                    KRYR%(0) = careerKRS!(J, I, 5)
                End If

                If careerKRS!(J, I, 1) <= KRRC!(1) Then
                    KRRC!(1) = careerKRS!(J, I, 1)
                    KRC$(1) = careerA4$(I)
                    KRYR%(1) = careerKRS!(J, I, 5)
                End If

                If careerKRS!(J, I, 0) = 0 Then
                    If careerKRS!(J, I, 1) / careerKRS!(J, I, 0) > KRRC!(2) And careerKRS!(J, I, 0) >= GAMES * 1.2 Then
                        KRRC!(2) = careerKRS!(J, I, 1) / careerKRS!(J, I, 0)
                        KRC$(2) = careerA4$(I)
                        KRYR%(2) = careerKRS!(J, I, 5)
                    End If
                End If

                If careerKRS!(J, I, 2) > KRRC!(3) Then
                    KRRC!(3) = careerKRS!(J, I, 2)
                    KRC$(3) = careerA4$(I)
                    KRYR%(3) = careerKRS!(J, I, 5)
                End If

                If careerKRS!(J, I, 3) > KRRC!(4) Then
                    KRRC!(4) = careerKRS!(J, I, 3)
                    KRC$(4) = careerA4$(I)
                    KRYR%(4) = careerKRS!(J, I, 5)
                    KRR$ = KRRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 14

        If careerA5$(I) <> "" And careerA5$(I) <> "XXX" And careerA5$(I) <> "END" Then

            For J = 1 To 15

                If careerPRS!(J, I, 5) = 0 Then Exit For

                If careerPRS!(J, I, 0) > PRRC!(0) Then
                    PRRC!(0) = careerPRS!(J, I, 0)
                    PRC$(0) = careerA5$(I)
                    PRYR%(0) = careerPRS!(J, I, 5)
                End If

                If careerPRS!(J, I, 1) > PRRC!(1) Then
                    PRRC!(1) = careerPRS!(J, I, 1)
                    PRC$(1) = careerA5$(I)
                    PRYR%(1) = careerPRS!(J, I, 5)
                End If

                If careerPRS!(J, I, 0) <> 0 Then
                    If careerPRS!(J, I, 1) / careerPRS!(J, I, 0) > PRRC!(2) And careerPRS!(J, I, 0) >= GAMES * 1.2 Then
                        PRRC!(2) = careerPRS!(J, I, 1) / careerPRS!(J, I, 0)
                        PRC$(2) = careerA5$(I)
                        PRYR%(2) = careerPRS!(J, I, 5)
                    End If
                End If

                If careerPRS!(J, I, 2) > PRRC!(3) Then
                    PRRC!(3) = careerPRS!(J, I, 2)
                    PRC$(3) = careerA5$(I)
                    PRYR%(3) = careerPRS!(J, I, 5)
                End If

                If careerPRS!(J, I, 3) > PRRC!(4) Then
                    PRRC!(4) = careerPRS!(J, I, 3)
                    PRC$(4) = careerA5$(I)
                    PRYR%(4) = careerPRS!(J, I, 5)
                    PRR$ = PRRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 9

        If careerA6$(I) <> "" And careerA6$(I) <> "XXX" And careerA6$(I) <> "END" Then

            For J = 1 To 15

                If careerPUS!(J, I, 3) = 0 Then Exit For

                If careerPUS!(J, I, 0) > PUUC!(0) Then
                    PUUC!(0) = careerPUS!(J, I, 0)
                    PUC$(0) = careerA6$(I)
                    PUYR%(0) = careerPUS!(J, I, 3)
                End If

                If careerPUS!(J, I, 1) > PUUC!(1) Then
                    PUUC!(1) = careerPUS!(J, I, 1)
                    PUC$(1) = careerA6$(I)
                    PUYR%(1) = careerPUS!(J, I, 3)
                End If

                If careerPUS!(J, I, 0) <> 0 Then
                    If careerPUS!(J, I, 1) / careerPUS!(J, I, 0) > PUUC!(2) Then
                        PUUC!(2) = careerPUS!(J, I, 1) / careerPUS!(J, I, 0)
                        PUC$(2) = careerA6$(I)
                        PUYR%(2) = careerPUS!(J, I, 3)
                    End If
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 9

        If careerA7$(I) <> "" Or careerA7$(I) <> "XXX" And careerA7$(I) <> "END" Then

            For J = 1 To 15

                If careerFGS!(J, I, 16) = 0 Then Exit For

                If careerFGS!(J, I, 1) > FGGC!(0) Then
                    FGGC!(0) = careerFGS!(J, I, 1)
                    FGC$(0) = careerA7$(I)
                    FGYR%(0) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 0) > FGGC!(1) Then
                    FGGC!(1) = careerFGS!(J, I, 0)
                    FGC$(1) = careerA7$(I)
                    FGYR%(1) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 1) <> 0 Then
                    If careerFGS!(J, I, 0) / careerFGS!(J, I, 1) > FGGC!(2) And careerFGS!(J, I, 1) >= GAMES Then
                        FGGC!(2) = careerFGS!(J, I, 0) / careerFGS!(J, I, 1)
                        FGC$(2) = careerA7$(I)
                        FGYR%(2) = careerFGS!(J, I, 16)
                    End If
                End If

                If careerFGS!(J, I, 3) > FGGC!(3) Then
                    FGGC!(3) = careerFGS!(J, I, 3)
                    FGC$(3) = careerA7$(I)
                    FGYR%(3) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 2) > FGGC!(4) Then
                    FGGC!(4) = careerFGS!(J, I, 2)
                    FGC$(4) = careerA7$(I)
                    FGYR%(4) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 2) <> 0 Then
                    If careerFGS!(J, I, 2) / careerFGS!(J, I, 3) > FGGC!(5) And careerFGS!(J, I, 3) >= GAMES Then
                        FGGC!(5) = careerFGS!(J, I, 2) / careerFGS!(J, I, 3)
                        FGC$(5) = careerA7$(I)
                        FGYR%(5) = careerFGS!(J, I, 16)
                    End If
                End If

                If careerFGS!(J, I, 4) > FGGC!(6) Then
                    FGGC!(6) = careerFGS!(J, I, 4)
                    FGC$(6) = careerA7$(I)
                    FGYR%(6) = careerFGS!(J, I, 16)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 54

        If careerDI$(I) <> "" And careerDI$(I) <> "XXX" And careerDI$(I) <> "END" Then

            For J = 1 To 15

                If careerGIS!(J, I, 2) = 0 Then Exit For

                If careerGIS!(J, I, 0) > DFFC!(0) Then
                    DFFC!(0) = careerGIS!(J, I, 0)
                    DFC$(0) = careerDI$(I)
                    FGYR%(0) = careerGIS!(J, I, 2)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 54

        If careerDS$(I) <> "" And careerDS$(I) <> "XXX" And careerDS$(I) <> "END" Then

            For J = 1 To 15
                If careerGSS!(J, I, 2) = 0 Then Exit For

                If careerGSS!(J, I, 0) > DFFC!(1) Then
                    DFFC!(1) = careerGSS!(J, I, 0)
                    DFC$(1) = careerDS$(I)
                    FGYR%(1) = careerGSS!(J, I, 2)
                End If

            Next J

        End If

    Next I

    '-----

    If VV = 0 Then
        D = 0

        For J = 0 To 8
            D = D + 1
            X$ = Mid$(careerFile$, D, 1)
            If X$ = "." Then JX = D - 1
        Next J

        Open diskPaths$(1) + Left$(careerFile$, JX) + ".CLD" For Output As #1

    End If

    If VV = 1 Then Open diskPaths$(1) + careerFile$ + ".CLD" For Output As #1

    For I = 0 To 9: Write #1, RBBC!(I), careerRBC$(I), RBYR%(I): Next

    Write #1, RBR$(0), RBR$(1)

    For I = 0 To 4: Write #1, WRRC!(I), WRC$(I), WRYR%(I): Next

    Write #1, WRR$

    For I = 0 To 8: Write #1, QBBC!(I), QBC$(I), QBYR%(I): Next

    Write #1, QBR$

    For I = 0 To 4: Write #1, KRRC!(I), KRC$(I), KRYR%(I): Next

    Write #1, KRR$

    For I = 0 To 4: Write #1, PRRC!(I), PRC$(I), PRYR%(I): Next

    Write #1, PRR$

    For I = 0 To 2: Write #1, PUUC!(I), PUC$(I), PUYR%(I): Next

    For I = 0 To 6: Write #1, FGGC!(I), FGC$(I), FGYR%(I): Next

    For I = 0 To 1: Write #1, DFFC!(I), DFC$(I), DFYR%(I): Next

    Close #1

End Sub
