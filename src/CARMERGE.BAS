'----------------------------------------
'           CARMERGE Subroutine
'----------------------------------------
'This routine assists thet user with the
'addition of career stats from any extra
'season years that have been compiled.
Sub MergeCareerFiles ()

    _Title "3-in-1 Football - Update Career Files"

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
    finalUpdate = 0
    VV = 0

    Do

        Color 14, 0
        Cls

        Locate 5, 27
        Print "- - - C A U T I O N - - -"
        
        Color 15
        Locate 8, 1

        Print "THIS OPTION SHOULD ONLY BE USED WHEN ADDING YEARS TO AN EXISTING STAT"
        Print "FILE. IF YOU ARE CREATING A NEW CAREER FILE, USE THE CREATE OPTION."
        Print
        Print "IF YOU ARE ADDING AN ADDITIONAL CAREER YEAR, MAKE SURE THAT ANY UNUSED PLAYER"
        Print "SLOTS (XXX, END, ETC.) ARE AT THE END OF THE SEASON STAT LIST BEFORE YOU MERGE."
        Print
        Print "ALSO MAKE SURE THAT THE PLAYERS NAMES IN THE FILE YOU ARE ADDING, MATCH"
        Print "THE EXACT SPELLING OF THE NAME OF THE SAME PLAYER IN THE CAREER FILE FOR THE"
        Print "PLAYERS THAT HAD PLAYED IN PREVIOUS YEARS.  EXAMPLE - IF DARREN NELSON IS"
        Print "LISTED AS 'NELSON' IN THE CAREER FILE AND THE SEASON STATS HAD HIM AS"
        Print "'D.NELSON', HE SHOULD BE CHANGED TO 'NELSON' IN THE SEASON FILE BEFORE"
        Print "MERGING, OR YOU COULD CHANGE HIM TO 'D.NELSON' IN THE CAREER FILE"
        Print
        Print
        
        Color 12: Locate , 25
        Print "DO YOU WISH TO CONTINUE? (Y/N)"

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then 

                MsgText$ = "NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU"
                BoxWidth = 24
                BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                Call CSMsgBox(MsgText$, BoxWidth, Pass)
                I$ = GetKeyPress$
                Call CSMsgBox("", 0, Pass)

            Else

                Call MergeAddYear(careerFile$, statFile$)

            End If

        End If

    End If

End Sub


'----------------------------------------
'         MergeAddYear Subroutine
'----------------------------------------
'This routine actually prompts the user
'for parameters for the team / year to
'be updated.  
Sub MergeAddYear (selectedFile$, statFile$)

    Color 15, 0
    Cls

    Do
        Locate 8, 25: Input "ENTER YEAR TO ADD (EX. - 1961)"; targetYear
        Locate 9, 25: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        VV = 0

        Do
            Locate , 5
            Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then doPostLeaders = 1 Else doPostLeaders = 0

        Call AddCareerYear(finalUpdate, selectedFile$, statFile$)

    End If

End Sub


'----------------------------------------
'       AddCareerYear Subroutine
'----------------------------------------
'This routine loads the existing career
'stats, adds the team's stats to them,
'then overwrites the career file with the
'new values. 
Sub AddCareerYear (finalUpdate, careerFile$, statFile$)

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunters$(), carKicker$()
    Shared carDefInts$(), carDefSacks$()
    Shared careerRBS$(), careerQBS$(), careerWRS$()

    Shared A1C$(), A2C$(), A3C$(), A4C$(), A5C$(), A6C$(), A7C$()
    Shared DIC$(), DSC$(), KRRS$()
    Shared PRRS$(), PUC$()

    Shared KRS!(), PRS!()
    Shared QBBS!(), RBBS!(), WRRS!()

    Shared AF!(), AM!()
    Shared careerGIS!(), careerGSS!(), statsLI!()
    Shared careerFGS!(), careerPUS!()
    Shared statsZ!(), statsZ1!(), statsZ2!()

    Dim statsLC$(60, 1), statsLF$(1), statsLI$(9, 1), statsLK$(15, 1), statsLP$(20, 1), statsLR$(50, 1)
    Dim careerPK$(15, 1)

    Dim careerLC!(60), careerLF!(1), careerLK!(15)
    Dim careerLP!(20), careerLR!(50), careerPK!(15)

    Dim careerGS%(55, 1), careerGI%(55, 2)


    Locate 20, 5
    Print "LOADING CAREER STATS . . . . PLEASE WAIT"

    Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #1

    Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    For I = 0 To 49: Input #1, A1C$(I): Next

    For I = 0 To 39: Input #1, A2C$(I): Next

    For I = 0 To 19: Input #1, A3C$(I): Next

    For I = 0 To 14: Input #1, A4C$(I): Next

    For I = 0 To 14: Input #1, A5C$(I): Next

    For I = 0 To 9: Input #1, A6C$(I): Next

    For I = 0 To 9: Input #1, A7C$(I): Next

    For I = 0 To 54: Input #1, DIC$(I): Next

    For I = 0 To 54: Input #1, DSC$(I): Next

    TMR = 12840

    For N = 1 To 15:

        For J = 0 To 49:

            For K = 0 To 9
                Input #1, RBBS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR

        Next J

    Next N

    For N = 1 To 15: 
        For J = 0 To 49: 
            Input #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 39: 
            For K = 0 To 5: 
                Input #1, WRRS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 39: 
            Input #1, careerWRS$(N, J, 0)

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 19: 
            For K = 0 To 7: 
                Input #1, QBBS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 19: 
            Input #1, careerQBS$(N, J, 0)

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 14: 
            For K = 0 To 5: 
                Input #1, KRS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 14: 
            Input #1, KRRS$(N, J, 0)

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 14: 
            For K = 0 To 5: 
                Input #1, PRS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 14: 
            Input #1, PRRS$(N, J, 0)

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 9: 
            For K = 0 To 3: 
                Input #1, careerPUS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15
        For J = 0 To 9
            For K = 0 To 16: 
                Input #1, careerFGS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15: 
        For J = 0 To 54: 
            For K = 0 To 2: 
                Input #1, careerGIS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    For N = 1 To 15:
        For J = 0 To 54: 
            For K = 0 To 2: 
                Input #1, careerGSS!(N, J, K)
            Next K

            Color 5
            Locate 21, 38
            TMR = TMR - 1
            Print Using "#####"; TMR
        Next J
    Next N

    Close #1

    For J = 20 To 22
        Locate J
        Print Space$(80)
    Next

    Locate 20, 5
    Print "LOADING STATS FILE . . . . PLEASE WAIT"

    Open diskPaths$(1) + statFile$ + "S" For Input As #2
    Input #2, GAMES
    Close #2

    Open diskPaths$(1) + statFile$ For Input As #3

    Input #3, temp$

    For I = 0 To 9: Input #3, carRBacks$(I): Next I

    For I = 0 To 5: Input #3, carWdRec$(I): Next I

    For I = 0 To 3: Input #3, carQBacks$(I): Next I

    For I = 0 To 2: Input #3, carKickRet$(I): Next I

    For I = 0 To 2: Input #3, carPRet$(I): Next I

    Input #3, carPunters$(0)

    For I = 0 To 1: Input #3, carKicker$(I): Next I

    For J = 0 To 13
        For K = 0 To 17: 
            Input #3, statsZ2!(J, K)
        Next K
    Next J

    For L = 0 To 38: Input #3, statsZ!(L): Next L

    For L = 0 To 38: Input #3, statsZ1!(L): Next L

    For J = 0 To 9: 
        Input #3, carDefInts$(J)
        For J1 = 0 To 2: 
            Input #3, careerGI%(J, J1)
        Next
    Next

    For J = 0 To 14: 
        Input #3, carDefSacks$(J)
        For J1 = 0 To 1: 
            Input #3, careerGS%(J, J1)
        Next
    Next

    For I = 0 To 9: 
        Input #3, careerLR!(I)
        Input #3, statsLR$(I, 0)
        Input #3, statsLR$(I, 1)
    Next

    For I = 0 To 15: 
        Input #3, careerLC!(I)
        Input #3, statsLC$(I, 0)
        Input #3, statsLC$(I, 1)
    Next

    For I = 0 To 3: 
        Input #3, careerLP!(I)
        Input #3, statsLP$(I, 0)
        Input #3, statsLP$(I, 1)
    Next

    For I = 0 To 2: 
        Input #3, careerLK!(I)
        Input #3, statsLK$(I, 0)
        Input #3, statsLK$(I, 1)
    Next

    For I = 0 To 2: 
        Input #3, careerPK!(I)
        Input #3, careerPK$(I, 0)
        Input #3, careerPK$(I, 1)
    Next

    For I = 0 To 1: 
        Input #3, careerLF!(I)
        Input #3, statsLF$(I)
        For I1 = 0 To 4: 
            Input #3, AM!(I, I1)
            Input #3, AF!(I, I1)
        Next
    Next

    For I = 0 To 9: 
        Input #3, statsLI!(I)
        Input #3, statsLI$(I, 0)
        Input #3, statsLI$(I, 1)
    Next

    Close #3

    For I = 0 To 9: T% = Len(carRBacks$(I))
        If T% = 0 Then carRBacks$(I) = "XXX"
    Next

    For I = 0 To 5: T% = Len(carWdRec$(I))
        If T% = 0 Then carWdRec$(I) = "XXX"
    Next

    For I = 0 To 3: T% = Len(carQBacks$(I))
        If T% = 0 Then carQBacks$(I) = "XXX"
    Next

    For I = 0 To 2: T% = Len(carKickRet$(I))
        If T% = 0 Then carKickRet$(I) = "XXX"
    Next

    For I = 0 To 2: T% = Len(carPRet$(I))
        If T% = 0 Then carPRet$(I) = "XXX"
    Next

    For I = 0 To 1: T% = Len(carKicker$(I))
        If T% = 0 Then carKicker$(I) = "XXX"
    Next

    For I = 0 To 9: T% = Len(carDefInts$(I))
        If T% = 0 Then carDefInts$(I) = "XXX"
    Next

    For I = 0 To 14: T% = Len(carDefSacks$(I))
        If T% = 0 Then carDefSacks$(I) = "XXX"
    Next

    '-----

    For I = 0 To 9
        RR = 0
        CHK = 0

        For J = 0 To 49

            If carRBacks$(I) = A1C$(J) Then

                For W = 1 To 15:
                    If RBBS!(W, J, 8) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For W = 1 To 15
                    N = N + RBBS!(W, J, 7)
                Next

                If CHK = 0 Then N = N + 1

                For K = 0 To 5
                    RBBS!(N, J, K) = statsZ2!(I, K)
                Next K

                RBBS!(N, J, 6) = careerLR!(I): careerRBS$(N, J, 0) = statsLR$(I, 0)
                RBBS!(N, J, 9) = careerLC!(I): careerRBS$(N, J, 1) = statsLC$(I, 0)
                RBBS!(N, J, 7) = 1: RBBS!(N, J, 8) = targetYear

                Exit For

            End If

            'Technically this is an "Else" clause
            'from the above, but the Exit means
            'it won't ever get here anyway
            RR = RR + 1

            If RR = 50 Then

                If carRBacks$(I) <> "END" And carRBacks$(I) <> "XXX" And Left$(carRBacks$(I), 1) <> " " Then
                    NB = NB + 1
                    A1C$(NB) = carRBacks$(I)
                    N = 1

                    For K = 0 To 5: RBBS!(N, NB, K) = statsZ2!(I, K): Next

                    RBBS!(N, NB, 6) = careerLR!(I)
                    careerRBS$(N, NB, 0) = statsLR$(I, 0)
                    RBBS!(N, NB, 9) = careerLC!(I)
                    careerRBS$(N, NB, 1) = statsLC$(I, 0)
                    RBBS!(N, NB, 7) = 1
                    RBBS!(N, NB, 8) = targetYear
                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 5

        RR = 0
        CHK = 0

        For J = 0 To 39

            If carWdRec$(I) = A2C$(J) Then

                For W = 1 To 15
                    If WRRS!(W, J, 5) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For W = 1 To 15
                    N = N + WRRS!(W, J, 4)
                Next

                If CHK = 0 Then N = N + 1

                For K = 6 To 8
                    WRRS!(N, J, K - 6) = statsZ2!(I, K)
                Next K

                WRRS!(N, J, 3) = careerLC!(I + 10)
                careerWRS$(N, J, 0) = statsLC$(I + 10, 0)
                WRRS!(N, J, 4) = 1
                WRRS!(N, J, 5) = targetYear

                Exit For

            End If

            1473
            RR = RR + 1
            If RR = 40 Then

                If carWdRec$(I) <> "END" And carWdRec$(I) <> "XXX" And Left$(carWdRec$(I), 1) <> " " Then
                    NC = NC + 1
                    A2C$(NC) = carWdRec$(I)
                    N = 1

                    For K = 6 To 8: WRRS!(N, NC, K - 6) = statsZ2!(I, K): Next

                    WRRS!(N, NC, 3) = careerLC!(I + 10)
                    careerWRS$(N, NC, 0) = statsLC$(I + 10, 0)
                    WRRS!(N, NC, 4) = 1
                    WRRS!(N, NC, 5) = targetYear
                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 3

        RR = 0
        CHK = 0

        For J = 0 To 19

            If carQBacks$(I) = A3C$(J) Then

                For W = 1 To 15:
                    If QBBS!(W, J, 7) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For W = 1 To 15
                    N = N + QBBS!(W, J, 6)
                Next

                If CHK = 0 Then N = N + 1

                For K = 9 To 13
                    QBBS!(N, J, K - 9) = statsZ2!(I, K)
                Next K

                QBBS!(N, J, 5) = careerLP!(I)
                careerQBS$(N, J, 0) = statsLP$(I, 0)
                QBBS!(N, J, 6) = 1
                QBBS!(N, J, 7) = targetYear

                Exit For

            Else
                RR = RR + 1

                If RR = 20 Then

                    If carQBacks$(I) <> "END" And carQBacks$(I) <> "XXX" And Left$(carQBacks$(I), 1) <> " " Then

                        ND = ND + 1
                        A3C$(ND) = carQBacks$(I)
                        N = 1
                        For K = 9 To 13: QBBS!(N, ND, K - 9) = statsZ2!(I, K): Next
                        QBBS!(N, ND, 5) = careerLP!(I): careerQBS$(N, ND, 0) = statsLP$(I, 0)
                        QBBS!(N, ND, 6) = 1: QBBS!(N, ND, 7) = targetYear
                    End If

                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 2

        RR = 0
        CHK = 0

        For J = 0 To 14

            If carKickRet$(I) = A4C$(J) Then

                For W = 1 To 15:
                    If KRS!(W, J, 5) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For W = 1 To 15
                    N = N + KRS!(W, J, 4)
                Next

                If CHK = 0 Then N = N + 1

                For K = 14 To 16
                    KRS!(N, J, K - 14) = statsZ2!(I, K)
                Next K

                KRS!(N, J, 3) = careerLK!(I)
                KRRS$(N, J, 0) = statsLK$(I, 0)
                KRS!(N, J, 4) = 1
                KRS!(N, J, 5) = targetYear

                Exit For

            Else

                RR = RR + 1

                If RR = 15 Then

                    If carKickRet$(I) <> "END" And carKickRet$(I) <> "XXX" And Left$(carKickRet$(I), 1) <> " " Then

                        NE = NE + 1
                        A4C$(NE) = carKickRet$(I)
                        N = 1

                        For K = 14 To 16: KRS!(N, NE, K - 14) = statsZ2!(I, K): Next

                        KRS!(N, NE, 3) = careerLK!(I)
                        KRRS$(N, NE, 0) = statsLK$(I, 0)
                        KRS!(N, NE, 4) = 1
                        KRS!(N, NE, 5) = targetYear

                    End If

                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 2

        RR = 0
        CHK = 0

        For J = 0 To 14

            If carPRet$(I) = A5C$(J) Then

                For W = 1 To 15:
                    If PRS!(W, J, 5) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For W = 1 To 15
                    N = N + PRS!(W, J, 4)
                Next

                If CHK = 0 Then N = N + 1

                For K = 14 To 16
                    PRS!(N, J, K - 14) = statsZ2!(I + 3, K)
                Next K

                PRS!(N, J, 3) = careerPK!(I)
                PRRS$(N, J, 0) = careerPK$(I, 0)
                PRS!(N, J, 4) = 1
                PRS!(N, J, 5) = targetYear

                Exit For

            Else
                RR = RR + 1
                If RR = 15 Then

                    If carPRet$(I) <> "END" And carPRet$(I) <> "XXX" And Left$(carPRet$(I), 1) <> " " Then
                        NF = NF + 1
                        A5C$(NF) = carPRet$(I)
                        N = 1

                        For K = 14 To 16: PRS!(N, NF, K - 14) = statsZ2!(I + 3, K): Next

                        PRS!(N, NF, 3) = careerPK!(I)
                        PRRS$(N, NF, 0) = careerPK$(I, 0)
                        PRS!(N, NF, 4) = 1
                        PRS!(N, NF, 5) = targetYear
                    End If

                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 0
        RR = 0
        CHK = 0

        For J = 0 To 9

            If carPunters$(I) = A6C$(J) Then

                For W = 1 To 15:
                    If careerPUS!(W, J, 3) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                If CHK > 0 Then
                    N = careerPUS!(1, J, 2) + careerPUS!(2, J, 2) + careerPUS!(3, J, 2) + careerPUS!(4, J, 2) + careerPUS!(5, J, 2) + careerPUS!(6, J, 2) + careerPUS!(7, J, 2) + careerPUS!(8, J, 2) + careerPUS!(9, J, 2) + careerPUS!(10, J, 2) + careerPUS!(11, J, 2) + careerPUS!(12, J, 2) + careerPUS!(13, J, 2) + careerPUS!(14, J, 2) + careerPUS!(15, J, 2)
                Else
                    N = careerPUS!(1, J, 2) + careerPUS!(2, J, 2) + careerPUS!(3, J, 2) + careerPUS!(4, J, 2) + careerPUS!(5, J, 2) + careerPUS!(6, J, 2) + careerPUS!(7, J, 2) + careerPUS!(8, J, 2) + careerPUS!(9, J, 2) + careerPUS!(10, J, 2) + careerPUS!(11, J, 2) + careerPUS!(12, J, 2) + careerPUS!(13, J, 2) + careerPUS!(14, J, 2) + careerPUS!(15, J, 2) + 1
                End If

                careerPUS!(N, J, 0) = statsZ!(7): careerPUS!(N, J, 1) = statsZ!(8)
                careerPUS!(N, J, 2) = 1: careerPUS!(N, J, 3) = targetYear

                Exit For

            Else

                RR = RR + 1

                If RR = 10 Then

                    If carPunters$(I) <> "END" And carPunters$(I) <> "XXX" And Left$(carPunters$(I), 1) <> " " Then
                        NG = NG + 1
                        A6C$(NG) = carPunters$(I)
                        N = 1
                        careerPUS!(N, NG, 0) = statsZ!(7)
                        careerPUS!(N, NG, 1) = statsZ!(8)
                        careerPUS!(N, NG, 2) = 1
                        careerPUS!(N, NG, 3) = targetYear
                    End If

                End If

            End If

        Next J

    Next I

    '-----
    skipLoop% = 0

    For I = 0 To 1

        RR = 0
        CHK = 0

        For J = 0 To 9

            If carKicker$(I) = A7C$(J) Then
                For W = 1 To 15:
                    If careerFGS!(W, J, 16) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                If CHK > 0 Then
                    N = careerFGS!(1, J, 15) + careerFGS!(2, J, 15) + careerFGS!(3, J, 15) + careerFGS!(4, J, 15) + careerFGS!(5, J, 15) + careerFGS!(6, J, 15) + careerFGS!(7, J, 15) + careerFGS!(8, J, 15) + careerFGS!(9, J, 15) + careerFGS!(10, J, 15) + careerFGS!(11, J, 15) + careerFGS!(12, J, 15) + careerFGS!(13, J, 15) + careerFGS!(14, J, 15) + careerFGS!(15, J, 15)
                Else
                    N = careerFGS!(1, J, 15) + careerFGS!(2, J, 15) + careerFGS!(3, J, 15) + careerFGS!(4, J, 15) + careerFGS!(5, J, 15) + careerFGS!(6, J, 15) + careerFGS!(7, J, 15) + careerFGS!(8, J, 15) + careerFGS!(9, J, 15) + careerFGS!(10, J, 15) + careerFGS!(11, J, 15) + careerFGS!(12, J, 15) + careerFGS!(13, J, 15) + careerFGS!(14, J, 15) + careerFGS!(15, J, 15) + 1
                End If

                If I = 0 Then
                    For K = 3 To 6
                        careerFGS!(N, J, K - 3) = statsZ2!(K, 17)
                    Next K

                    careerFGS!(N, J, 4) = careerLF!(I)
                    careerFGS!(N, J, 15) = 1: careerFGS!(N, J, 16) = targetYear
                    For J1 = 0 To 4: careerFGS!(N, J, J1 + 5) = AM!(I, J1): Next
                    For J1 = 0 To 4: careerFGS!(N, J, J1 + 10) = AF!(I, J1): Next
                Else

                    If I = 1 Then

                        For K = 7 To 10
                            careerFGS!(N, J, K - 7) = statsZ2!(K, 17)
                        Next K

                        careerFGS!(N, J, 4) = careerLF!(I)
                        careerFGS!(N, J, 15) = 1: careerFGS!(N, J, 16) = targetYear

                        For J1 = 0 To 4: careerFGS!(N, J, J1 + 5) = AM!(I, J1): Next
                        For J1 = 0 To 4: careerFGS!(N, J, J1 + 10) = AF!(I, J1): Next

                    End If

                End If

                Exit For

            Else
                RR = RR + 1

                If RR = 10 Then

                    If carKicker$(I) <> "END" And carKicker$(I) <> "XXX" And Left$(carKicker$(I), 1) <> " " Then

                        NH = NH + 1
                        A7C$(NH) = carKicker$(I)
                        N = 1

                        If I = 0 Then
                            For K = 3 To 6:
                                careerFGS!(N, NH, K - 3) = statsZ2!(K, 17)
                            Next K
                            careerFGS!(N, NH, 4) = careerLF!(I)
                            careerFGS!(N, NH, 15) = 1: careerFGS!(N, NH, 16) = targetYear
                            For J1 = 0 To 4: careerFGS!(N, NH, J1 + 5) = AM!(I, J1): Next
                            For J1 = 0 To 4: careerFGS!(N, NH, J1 + 10) = AF!(I, J1): Next
                            skipLoop% = 1
                            Exit For
                        End If

                        If I = 1 Then

                            For K = 7 To 10:
                                careerFGS!(N, NH, K - 7) = statsZ2!(K, 17)
                            Next K

                            careerFGS!(N, NH, 4) = careerLF!(I)
                            careerFGS!(N, NH, 15) = 1: careerFGS!(N, J, 16) = targetYear
                            For J1 = 0 To 4: careerFGS!(N, NH, J1 + 5) = AM!(I, J1): Next
                            For J1 = 0 To 4: careerFGS!(N, NH, J1 + 10) = AF!(I, J1): Next

                        End If

                    End If

                End If

            End If

        Next J

        If skipLoop% = 1 Then Exit For

    Next I

    '-----

    For I = 0 To 9

        RR = 0
        CHK = 0

        For J = 0 To 54

            If carDefInts$(I) = DIC$(J) Then
                For W = 1 To 15:
                    If careerGIS!(W, J, 2) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                If CHK > 0 Then
                    N = careerGIS!(1, J, 1) + careerGIS!(2, J, 1) + careerGIS!(3, J, 1) + careerGIS!(4, J, 1) + careerGIS!(5, J, 1) + careerGIS!(6, J, 1) + careerGIS!(7, J, 1) + careerGIS!(8, J, 1) + careerGIS!(9, J, 1) + careerGIS!(10, J, 1) + careerGIS!(11, J, 1) + careerGIS!(12, J, 1) + careerGIS!(13, J, 1) + careerGIS!(14, J, 1) + careerGIS!(15, J, 1)
                Else
                    N = careerGIS!(1, J, 1) + careerGIS!(2, J, 1) + careerGIS!(3, J, 1) + careerGIS!(4, J, 1) + careerGIS!(5, J, 1) + careerGIS!(6, J, 1) + careerGIS!(7, J, 1) + careerGIS!(8, J, 1) + careerGIS!(9, J, 1) + careerGIS!(10, J, 1) + careerGIS!(11, J, 1) + careerGIS!(12, J, 1) + careerGIS!(13, J, 1) + careerGIS!(14, J, 1) + careerGIS!(15, J, 1) + 1
                End If

                careerGIS!(N, J, 0) = careerGI%(I, 0)
                careerGIS!(N, J, 1) = 1: careerGIS!(N, J, 2) = targetYear
                Exit For
            Else
                RR = RR + 1
                If RR = 10 Then

                    If carDefInts$(I) <> "END" And carDefInts$(I) <> "XXX" And Left$(carDefInts$(I), 1) <> " " Then

                        NJ = NJ + 1
                        DIC$(NJ) = carDefInts$(I)
                        N = 1
                        careerGIS!(N, NJ, 0) = careerGI%(I, 0)
                        careerGIS!(N, NJ, 1) = 1
                        careerGIS!(N, NJ, 2) = targetYear
                    End If

                End If
            End If

        Next J

    Next I

    '-----

    For I = 0 To 14
        RR = 0
        CHK = 0

        For J = 0 To 54

            If carDefSacks$(I) = DSC$(J) Then

                For W = 1 To 15:
                    If careerGSS!(W, J, 2) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                If CHK > 0 Then
                    N = careerGSS!(1, J, 1) + careerGSS!(2, J, 1) + careerGSS!(3, J, 1) + careerGSS!(4, J, 1) + careerGSS!(5, J, 1) + careerGSS!(6, J, 1) + careerGSS!(7, J, 1) + careerGSS!(8, J, 1) + careerGSS!(9, J, 1) + careerGSS!(10, J, 1) + careerGSS!(11, J, 1) + careerGSS!(12, J, 1) + careerGSS!(13, J, 1) + careerGSS!(14, J, 1) + careerGSS!(15, J, 1)
                Else
                    N = careerGSS!(1, J, 1) + careerGSS!(2, J, 1) + careerGSS!(3, J, 1) + careerGSS!(4, J, 1) + careerGSS!(5, J, 1) + careerGSS!(6, J, 1) + careerGSS!(7, J, 1) + careerGSS!(8, J, 1) + careerGSS!(9, J, 1) + careerGSS!(10, J, 1) + careerGSS!(11, J, 1) + careerGSS!(12, J, 1) + careerGSS!(13, J, 1) + careerGSS!(14, J, 1) + careerGSS!(15, J, 1) + 1
                End If

                careerGSS!(N, J, 0) = careerGS%(I, 0)
                careerGSS!(N, J, 1) = 1
                careerGSS!(N, J, 2) = targetYear

                Exit For
            Else

                RR = RR + 1

                If RR = 15 Then

                    If carDefSacks$(I) <> "END" And carDefSacks$(I) <> "XXX" And Left$(carDefSacks$(I), 1) <> " " Then

                        NK = NK + 1
                        DSC$(NK) = carDefSacks$(I)
                        N = 1
                        careerGSS!(N, NK, 0) = careerGS%(I, 0)
                        careerGSS!(N, NK, 1) = 1
                        careerGSS!(N, NK, 2) = targetYear
                    End If

                End If

            End If

        Next J

    Next I

    '-----

    If finalUpdate = 1 Then Call ProcessLeaders (careerFile$)

    For J = 20 To 22
        Locate J
        Print Space$(80)
    Next

    TMR = 12840

    Locate 20, 5
    Print "MERGING CAREER STATS . . . . PLEASE WAIT"

    Open diskPaths$(1) + careerFile$ + ".CAR" For Output As #4

    Write #4, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    For I = 0 To 49: Write #4, A1C$(I): Next

    For I = 0 To 39: Write #4, A2C$(I): Next

    For I = 0 To 19: Write #4, A3C$(I): Next

    For I = 0 To 14: Write #4, A4C$(I): Next

    For I = 0 To 14: Write #4, A5C$(I): Next

    For I = 0 To 9: Write #4, A6C$(I): Next

    For I = 0 To 9: Write #4, A7C$(I): Next

    For I = 0 To 54: Write #4, DIC$(I): Next

    For I = 0 To 54: Write #4, DSC$(I): Next

    For N = 1 To 15
        For J = 0 To 49
            For K = 0 To 9: Write #4, RBBS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 49: 
            Write #4, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 39
            For K = 0 To 5: Write #4, WRRS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 39: 
            Write #4, careerWRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 19
            For K = 0 To 7: Write #4, QBBS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 19: 
            Write #4, careerQBS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 14
            For K = 0 To 5: Write #4, KRS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 14: 
            Write #4, KRRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 14
            For K = 0 To 5: Write #4, PRS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 14: 
            Write #4, PRRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 9
            For K = 0 To 3: Write #4, careerPUS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 9
            For K = 0 To 16: Write #4, careerFGS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 54
            For K = 0 To 2: Write #4, careerGIS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N
    
    For N = 1 To 15
        For J = 0 To 54
            For K = 0 To 2: Write #4, careerGSS!(N, J, K): Next K
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "#####"; TMR
        Next J
    Next N

    Close #4

End Sub

'----------------------------------------
'       ProcessLeaders Subroutine
'----------------------------------------
'If the user has specified that this is
'the final update for the season, this
'routine is called to post stat leaders
'for viewing. Despite the logic, it is 
'not in any sense a final action, and 
'can be performed throughout a sseason 
'if you want to see leaders at different 
'intervals.
Sub ProcessLeaders (careerFile$)

    Shared FGC$(), KRC$(), PRC$(), QBC$()
    Shared RBR$(), careerRBC$(), WRC$()
    Shared careerRBS$(), careerQBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!()
    Shared careerFGS!(), careerPUS!()
    Shared DFYR%(), FGYR%(), KRYR%(), PRYR%()
    Shared PUYR%(), QBYR%(), RBYR%(), WRYR%()

    Shared A1C$(), A2C$(), A3C$(), A4C$(), A5C$(), A6C$(), A7C$()
    Shared DIC$(), DSC$(), KRRS$()
    Shared PRRS$(), PUC$()

    Shared KRS!(), PRS!()
    Shared QBBS!(), RBBS!(), WRRS!()

    Dim FF!(15, 20), II!(15, 20), YY!(15, 20)
    Dim DFFC!(1), FGGC!(6), KRRC!(4)
    Dim PP!(15, 20), PRRC!(4), PUUC!(2)
    Dim QBBC!(8), RBBC!(9), WRRC!(4)

    Dim DFC$(1)

    If _FileExists(diskPaths$(2) + careerFile$ + ".CLD") Then

        Open diskPaths$(2) + careerFile$ + ".CLD" For Input As #1

        For I = 0 To 9: 
            Input #1, RBBC!(I), careerRBC$(I), RBYR%(I)
        Next

        Input #1, RBR$(0), RBR$(1)

        For I = 0 To 4: 
            Input #1, WRRC!(I), WRC$(I), WRYR%(I)
        Next

        Input #1, WRR$

        For I = 0 To 8: 
            Input #1, QBBC!(I), QBC$(I), QBYR%(I)
        Next

        Input #1, QBR$

        For I = 0 To 4: 
            Input #1, KRRC!(I), KRC$(I), KRYR%(I)
        Next

        Input #1, KRR$

        For I = 0 To 4: 
            Input #1, PRRC!(I), PRC$(I), PRYR%(I)
        Next

        Input #1, PRR$

        For I = 0 To 2: 
            Input #1, PUUC!(I), PUC$(I), PUYR%(I)
        Next

        For I = 0 To 6: 
            Input #1, FGGC!(I), FGC$(I), FGYR%(I)
        Next

        For I = 0 To 1: 
            Input #1, DFFC!(I), DFC$(I), DFYR%(I)
        Next

        Close #1

    End If

    '-----

    For I = 0 To 49

        If A1C$(I) <> "" And A1C$(I) <> "XXX" And A1C$(I) <> "END" Then

            For J = 1 To 15
                If RBBS!(J, I, 8) = 0 Then
                    Exit For
                Else

                    If RBBS!(J, I, 0) > RBBC!(0) Then
                        RBBC!(0) = RBBS!(J, I, 0)
                        careerRBC$(0) = A1C$(I)
                        RBYR%(0) = RBBS!(J, I, 8)
                    End If

                    If RBBS!(J, I, 1) > RBBC!(1) Then
                        RBBC!(1) = RBBS!(J, I, 1)
                        careerRBC$(1) = A1C$(I)
                        RBYR%(1) = RBBS!(J, I, 8)
                    End If

                    If RBBS!(J, I, 0) <> 0 Then
                        If RBBS!(J, I, 1) / RBBS!(J, I, 0) > RBBC!(2) And RBBS!(J, I, 0) >= GAMES * 10 Then
                            RBBC!(2) = RBBS!(J, I, 1) / RBBS!(J, I, 0)
                            careerRBC$(2) = A1C$(I)
                            RBYR%(2) = RBBS!(J, I, 8)
                        End If
                    End If

                    If RBBS!(J, I, 2) > RBBC!(3) Then
                        RBBC!(3) = RBBS!(J, I, 2)
                        careerRBC$(3) = A1C$(I)
                        RBYR%(3) = RBBS!(J, I, 8)
                    End If

                    If RBBS!(J, I, 3) > RBBC!(4) Then
                        RBBC!(4) = RBBS!(J, I, 3)
                        careerRBC$(4) = A1C$(I)
                        RBYR%(4) = RBBS!(J, I, 8)
                    End If

                    If RBBS!(J, I, 4) > RBBC!(5) Then
                        RBBC!(5) = RBBS!(J, I, 4)
                        careerRBC$(5) = A1C$(I)
                        RBYR%(5) = RBBS!(J, I, 8)
                    End If

                    If RBBS!(J, I, 3) <> 0 Then
                        If RBBS!(J, I, 4) / RBBS!(J, I, 3) > RBBC!(6) And RBBS!(J, I, 3) >= GAMES * 3 Then
                            RBBC!(6) = RBBS!(J, I, 4) / RBBS!(J, I, 3)
                            careerRBC$(6) = A1C$(I)
                            RBYR%(6) = RBBS!(J, I, 8)
                        End If
                    End If

                    If RBBS!(J, I, 5) > RBBC!(7) Then
                        RBBC!(7) = RBBS!(J, I, 5)
                        careerRBC$(7) = A1C$(I)
                        RBYR%(7) = RBBS!(J, I, 8)
                    End If

                    If RBBS!(J, I, 6) > RBBC!(8) Then
                        RBBC!(8) = RBBS!(J, I, 6)
                        careerRBC$(8) = A1C$(I)
                        RBYR%(8) = RBBS!(J, I, 8)
                        RBR$(0) = careerRBS$(J, I, 0)
                    End If

                    If RBBS!(J, I, 9) > RBBC!(9) Then
                        RBBC!(9) = RBBS!(J, I, 9)
                        careerRBC$(9) = A1C$(I)
                        RBYR%(9) = RBBS!(J, I, 8)
                        RBR$(1) = careerRBS$(J, I, 1)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 39

        If A2C$(I) <> "" And A2C$(I) <> "XXX" And A2C$(I) <> "END" Then
            For J = 1 To 15

                If WRRS!(J, I, 5) = 0 Then Exit For

                If WRRS!(J, I, 0) > WRRC!(0) Then
                    WRRC!(0) = WRRS!(J, I, 0)
                    WRC$(0) = A2C$(I)
                    WRYR%(0) = WRRS!(J, I, 5)
                End If

                If WRRS!(J, I, 1) > WRRC!(1) Then
                    WRRC!(1) = WRRS!(J, I, 1)
                    WRC$(1) = A2C$(I)
                    WRYR%(1) = WRRS!(J, I, 5)
                End If

                If WRRS!(J, I, 0) <> 0 Then
                    If WRRS!(J, I, 1) / WRRS!(J, I, 0) > WRRC!(2) And WRRS!(J, I, 0) >= GAMES * 3 Then
                        WRRC!(2) = WRRS!(J, I, 1) / WRRS!(J, I, 0): WRC$(2) = A2C$(I): WRYR%(2) = WRRS!(J, I, 5)
                    End If
                End If

                If WRRS!(J, I, 2) > WRRC!(3) Then
                    WRRC!(3) = WRRS!(J, I, 2)
                    WRC$(3) = A2C$(I)
                    WRYR%(3) = WRRS!(J, I, 5)
                End If

                If WRRS!(J, I, 3) > WRRC!(4) Then
                    WRRC!(4) = WRRS!(J, I, 3)
                    WRC$(4) = A2C$(I)
                    WRYR%(4) = WRRS!(J, I, 5)
                    WRR$ = careerWRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 19

        If A3C$(I) <> "" And A3C$(I) <> "XXX" And A3C$(I) <> "END" Then

            For J = 1 To 15:

                If QBBS!(J, I, 0) > 0 Then QBBS!(J, I, 11) = Int(QBBS!(J, I, 1) / QBBS!(J, I, 0) * 1000) / 10

                If QBBS!(J, I, 0) = 0 Then
                    QBBS!(J, I, 8) = 0
                    QBBS!(J, I, 9) = 0
                    QBBS!(J, I, 10) = 0
                    QBBS!(J, I, 11) = 0
                Else
                    QBBS!(J, I, 8) = Int(QBBS!(J, I, 5) / QBBS!(J, I, 0) * 1000) / 10
                    QBBS!(J, I, 9) = Int(QBBS!(J, I, 3) / QBBS!(J, I, 0) * 1000) / 10
                    QBBS!(J, I, 10) = Int(QBBS!(J, I, 4) / QBBS!(J, I, 0) * 100) / 100
                End If

                PP!(J, I) = QBBS!(J, I, 11) - 30
                If PP!(J, I) < 0 Then
                    PP!(J, I) = 0
                Else
                    If PP!(J, I) > 77.5 Then
                        PP!(J, I) = 2.375
                    Else
                        PP!(J, I) = PP!(J, I) * .05
                    End If
                End If

                If QBBS!(J, I, 8) > 11.9 Then
                    FF!(J, I) = 2.375
                Else
                    FF!(J, I) = QBBS!(J, I, 8) * .2
                End If

                If QBBS!(J, I, 9) = 0 Then
                    II!(J, I) = 0
                Else
                    If QBBS!(J, I, 9) > 9.5 Then
                        II!(J, I) = 0
                    Else
                        II!(J, I) = 2.375 - (.25 * QBBS!(J, I, 9))
                    End If
                End If

                QBBS!(J, I, 10) = QBBS!(J, I, 10) - 3

                If QBBS!(J, I, 10) < 0 Then
                    YY!(J, I) = 0
                Else
                    If QBBS!(J, I, 10) > 12.5 Then
                        YY!(J, I) = 2.375
                    Else
                        D = Int(QBBS!(J, I, 10) * 100) / 2
                        If D = QBBS!(J, I, 10) / 2 Then
                            YY!(J, I) = QBBS!(J, I, 10) / 2 * .3 + QBBS!(J, I, 10) / 2 * .2
                        Else
                            YY!(J, I) = QBBS!(J, I, 10) / 2 * .3 + QBBS!(J, I, 10) / 2 * .2 + .003
                        End If
                    End If
                End If

                QBBS!(J, I, 12) = PP!(J, I) + FF!(J, I) + II!(J, I) + YY!(J, I)

                If QBBS!(J, I, 12) > 9.5 Then
                    QBBS!(J, I, 12) = 158.3
                Else
                    QBBS!(J, I, 12) = QBBS!(J, I, 12) * 50 / 3
                End If

                QBBS!(J, I, 12) = Int(QBBS!(J, I, 12) * 10) / 10

                If QBBS!(J, I, 7) = 0 Then
                    Exit For
                Else
                    If QBBS!(J, I, 1) > QBBC!(0) Then
                        QBBC!(0) = QBBS!(J, I, 1)
                        QBC$(0) = A3C$(I)
                        QBYR%(0) = QBBS!(J, I, 7)
                    End If

                    If QBBS!(J, I, 0) > QBBC!(1) Then
                        QBBC!(1) = QBBS!(J, I, 0)
                        QBC$(1) = A3C$(I)
                        QBYR%(1) = QBBS!(J, I, 7)
                    End If

                    If QBBS!(J, I, 2) > QBBC!(2) Then
                        QBBC!(2) = QBBS!(J, I, 2)
                        QBC$(2) = A3C$(I)
                        QBYR%(2) = QBBS!(J, I, 7)
                    End If

                    If QBBS!(J, I, 4) > QBBC!(3) Then
                        QBBC!(3) = QBBS!(J, I, 4)
                        QBC$(3) = A3C$(I)
                        QBYR%(3) = QBBS!(J, I, 7)
                    End If

                    If QBBS!(J, I, 3) > QBBC!(4) Then
                        QBBC!(4) = QBBS!(J, I, 3)
                        QBC$(4) = A3C$(I)
                        QBYR%(4) = QBBS!(J, I, 7)
                    End If

                    If QBBS!(J, I, 0) <> 0 Then
                        If QBBS!(J, I, 1) / QBBS!(J, I, 0) > QBBC!(5) And QBBS!(J, I, 0) >= GAMES * 15 Then
                            QBBC!(5) = QBBS!(J, I, 1) / QBBS!(J, I, 0)
                            QBC$(5) = A3C$(I)
                            QBYR%(5) = QBBS!(J, I, 7)
                        End If
                    End If

                    If QBBS!(J, I, 1) <> 0 Then
                        If QBBS!(J, I, 3) / QBBS!(J, I, 1) > QBBC!(6) And QBBS!(J, I, 0) >= GAMES * 15 Then
                            QBBC!(6) = QBBS!(J, I, 3) / QBBS!(J, I, 1)
                            QBC$(6) = A3C$(I): QBYR%(6) = QBBS!(J, I, 7)
                        End If
                    End If

                    If QBBS!(J, I, 12) > QBBC!(7) Then
                        QBBC!(7) = QBBS!(J, I, 12)
                        QBC$(7) = A3C$(I)
                        QBYR%(7) = QBBS!(J, I, 7)
                    End If

                    If QBBS!(J, I, 5) > QBBC!(8) Then
                        QBBC!(8) = QBBS!(J, I, 5)
                        QBC$(8) = A3C$(I)
                        QBYR%(8) = QBBS!(J, I, 7)
                        QBR$ = careerQBS$(J, I, 0)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 14

        If A4C$(I) <> "" And A4C$(I) <> "XXX" And A4C$(I) <> "END" Then

            For J = 1 To 15

                If KRS!(J, I, 5) = 0 Then Exit For

                If KRS!(J, I, 0) > KRRC!(0) Then
                    KRRC!(0) = KRS!(J, I, 0)
                    KRC$(0) = A4C$(I)
                    KRYR%(0) = KRS!(J, I, 5)
                End If

                If KRS!(J, I, 1) > KRRC!(1) Then
                    KRRC!(1) = KRS!(J, I, 1)
                    KRC$(1) = A4C$(I)
                    KRYR%(1) = KRS!(J, I, 5)
                End If

                If KRS!(J, I, 0) <> 0 Then
                    If KRS!(J, I, 1) / KRS!(J, I, 0) > KRRC!(2) And KRS!(J, I, 0) >= GAMES * 1.2 Then
                        KRRC!(2) = KRS!(J, I, 1) / KRS!(J, I, 0)
                        KRC$(2) = A4C$(I)
                        KRYR%(2) = KRS!(J, I, 5)
                    End If
                End If

                If KRS!(J, I, 2) > KRRC!(3) Then
                    KRRC!(3) = KRS!(J, I, 2)
                    KRC$(3) = A4C$(I)
                    KRYR%(3) = KRS!(J, I, 5)
                End If

                If KRS!(J, I, 3) > KRRC!(4) Then
                    KRRC!(4) = KRS!(J, I, 3)
                    KRC$(4) = A4C$(I)
                    KRYR%(4) = KRS!(J, I, 5)
                    KRR$ = KRRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 14

        If A5C$(I) <> "" And A5C$(I) <> "XXX" And A5C$(I) <> "END" Then

            For J = 1 To 15

                If PRS!(J, I, 5) = 0 Then Exit For

                If PRS!(J, I, 0) > PRRC!(0) Then
                    PRRC!(0) = PRS!(J, I, 0)
                    PRC$(0) = A5C$(I)
                    PRYR%(0) = PRS!(J, I, 5)
                End If

                If PRS!(J, I, 1) > PRRC!(1) Then
                    PRRC!(1) = PRS!(J, I, 1)
                    PRC$(1) = A5C$(I)
                    PRYR%(1) = PRS!(J, I, 5)
                End If

                If PRS!(J, I, 0) <> 0 Then
                    If PRS!(J, I, 1) / PRS!(J, I, 0) > PRRC!(2) And PRS!(J, I, 0) >= GAMES * 1.2 Then
                        PRRC!(2) = PRS!(J, I, 1) / PRS!(J, I, 0)
                        PRC$(2) = A5C$(I)
                        PRYR%(2) = PRS!(J, I, 5)
                    End If
                End If

                If PRS!(J, I, 2) > PRRC!(3) Then
                    PRRC!(3) = PRS!(J, I, 2)
                    PRC$(3) = A5C$(I)
                    PRYR%(3) = PRS!(J, I, 5)
                End If

                If PRS!(J, I, 3) > PRRC!(4) Then
                    PRRC!(4) = PRS!(J, I, 3)
                    PRC$(4) = A5C$(I)
                    PRYR%(4) = PRS!(J, I, 5)
                    PRR$ = PRRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 9

        If A6C$(I) <> "" And A6C$(I) <> "XXX" And A6C$(I) <> "END" Then

            For J = 1 To 15

                If careerPUS!(J, I, 3) = 0 Then Exit For

                If careerPUS!(J, I, 0) > PUUC!(0) Then
                    PUUC!(0) = careerPUS!(J, I, 0)
                    PUC$(0) = A6C$(I)
                    PUYR%(0) = careerPUS!(J, I, 3)
                End If

                If careerPUS!(J, I, 1) > PUUC!(1) Then
                    PUUC!(1) = careerPUS!(J, I, 1)
                    PUC$(1) = A6C$(I)
                    PUYR%(1) = careerPUS!(J, I, 3)
                End If

                If careerPUS!(J, I, 0) <> 0 Then
                    If careerPUS!(J, I, 1) / careerPUS!(J, I, 0) > PUUC!(2) Then
                        PUUC!(2) = careerPUS!(J, I, 1) / careerPUS!(J, I, 0)
                        PUC$(2) = A6C$(I)
                        PUYR%(2) = careerPUS!(J, I, 3)
                    End If
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 9

        If A7C$(I) <> "" And A7C$(I) <> "XXX" And A7C$(I) <> "END" Then

            For J = 1 To 15

                If careerFGS!(J, I, 16) = 0 Then Exit For

                If careerFGS!(J, I, 1) > FGGC!(0) Then
                    FGGC!(0) = careerFGS!(J, I, 1): FGC$(0) = A7C$(I): FGYR%(0) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 0) > FGGC!(1) Then
                    FGGC!(1) = careerFGS!(J, I, 0)
                    FGC$(1) = A7C$(I)
                    FGYR%(1) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 1) <> 0 Then
                    If careerFGS!(J, I, 0) / careerFGS!(J, I, 1) > FGGC!(2) And careerFGS!(J, I, 1) >= GAMES Then
                        FGGC!(2) = careerFGS!(J, I, 0) / careerFGS!(J, I, 1)
                        FGC$(2) = A7C$(I)
                        FGYR%(2) = careerFGS!(J, I, 16)
                    End If
                End If

                If careerFGS!(J, I, 3) > FGGC!(3) Then
                    FGGC!(3) = careerFGS!(J, I, 3)
                    FGC$(3) = A7C$(I)
                    FGYR%(3) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 2) > FGGC!(4) Then
                    FGGC!(4) = careerFGS!(J, I, 2)
                    FGC$(4) = A7C$(I)
                    FGYR%(4) = careerFGS!(J, I, 16)
                End If

                If careerFGS!(J, I, 2) <> 0 Then
                    If careerFGS!(J, I, 2) / careerFGS!(J, I, 3) > FGGC!(5) And careerFGS!(J, I, 3) >= GAMES Then
                        FGGC!(5) = careerFGS!(J, I, 2) / careerFGS!(J, I, 3)
                        FGC$(5) = A7C$(I)
                        FGYR%(5) = careerFGS!(J, I, 16)
                    End If
                End If

                If careerFGS!(J, I, 4) > FGGC!(6) Then
                    FGGC!(6) = careerFGS!(J, I, 4)
                    FGC$(6) = A7C$(I)
                    FGYR%(6) = careerFGS!(J, I, 16)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 54

        If DIC$(I) <> "" And DIC$(I) <> "XXX" And DIC$(I) <> "END" Then

            For J = 1 To 15
                If careerGIS!(J, I, 2) = 0 Then Exit For

                If careerGIS!(J, I, 0) > DFFC!(0) Then
                    DFFC!(0) = careerGIS!(J, I, 0)
                    DFC$(0) = DIC$(I)
                    FGYR%(0) = careerGIS!(J, I, 2)
                End If
            Next J

        End If

    Next I

    '-----

    For I = 0 To 54

        If DSC$(I) <> "" And DSC$(I) <> "XXX" And DSC$(I) <> "END" Then

            For J = 1 To 15
                If careerGSS!(J, I, 2) = 0 Then Exit For

                If careerGSS!(J, I, 0) > DFFC!(1) Then
                    DFFC!(1) = careerGSS!(J, I, 0): DFC$(1) = DSC$(I): FGYR%(1) = careerGSS!(J, I, 2)
                End If

            Next J

        End If

    Next I

    '-----

    Open diskPaths$(2) + careerFile$ + ".CLD" For Output As #2

    For I = 0 To 9: 
        Write #2, RBBC!(I), careerRBC$(I), RBYR%(I)
    Next

    Write #2, RBR$(0), RBR$(1)

    For I = 0 To 4: 
        Write #2, WRRC!(I), WRC$(I), WRYR%(I)
    Next

    Write #2, WRR$

    For I = 0 To 8: 
        Write #2, QBBC!(I), QBC$(I), QBYR%(I)
    Next

    Write #2, QBR$

    For I = 0 To 4: 
        Write #2, KRRC!(I), KRC$(I), KRYR%(I)
    Next

    Write #2, KRR$

    For I = 0 To 4: 
        Write #2, PRRC!(I), PRC$(I), PRYR%(I)
    Next

    Write #2, PRR$

    For I = 0 To 2: 
        Write #2, PUUC!(I), PUC$(I), PUYR%(I)
    Next

    For I = 0 To 6: 
        Write #2, FGGC!(I), FGC$(I), FGYR%(I)
    Next

    For I = 0 To 1: 
        Write #2, DFFC!(I), DFC$(I), DFYR%(I)
    Next

    Close #2

End Sub

