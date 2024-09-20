'----------------------------------------
'          CAREER Subroutine
'----------------------------------------
'This routine gives the user a menu from
'which they can choose several options
'for managing career files
Sub CAREER ()

    _Title "3-in-1 Football - Career Stats"

    ReDim menuChoices$(1 To 8)

    Do

        Color 15, 0
        Cls

        Call TITLE_CAREER

        menuChoices$(1) = "CREATE INITIAL CAREER STATS FILE  "
        menuChoices$(2) = "ADD CAREER YEAR TO STATS          "
        menuChoices$(3) = "REMOVE PLAYERS FROM CAREER STATS  "
        menuChoices$(4) = "VIEW / PRINT CAREER STATS        "
        menuChoices$(5) = "VIEW / PRINT CAREER RECORDS       "
        menuChoices$(6) = "UPDATE / VIEW TEAM CAREER LEADERS "
        menuChoices$(7) = "EXIT TO MAIN MENU                 "
        SELECTION% = 1
        Count% = 7
        Start% = 1
        ExitCode% = 7
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call CreateCareerFile

                Case 2:
                    Call MergeCareerFiles

                Case 3:
                    Call CAREDIT

                Case 4:
                    Call CARVIEW

                Case 5:
                    Call ReviewCareerRecords

                Case 6:
                    Call CARLDR

            End Select

        End If

    Loop Until SELECTION% = 7 Or ExitCode% = 27

End Sub


'----------------------------------------
'       CreateCareerFile Subroutine
'----------------------------------------
'This routine assists the user with the
'creation of a team's first career file.
'This naturally means it will guide them
'through also adding the first year's
'stats to the file.
Sub CreateCareerFile ()

    Do

        Cls
        Color 14

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

        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or I$ = Chr$(27)

    If UCase$(I$) = "Y" Then

        Call MergeFirstYear

    End If 'Done checking if user is ready

End Sub


'----------------------------------------
'        MergeFirstYear Subroutine
'----------------------------------------
'This routine actually prompts the user for
'parameters to be used for merging the first
'year's worth of stats to the career file
Sub MergeFirstYear

    Shared TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared carDefInts$(), carDefSacks$()

    Shared statsZ!(), statsZ1!(), statsZ2!()
    Shared statsLF!(), statsLI!()
    Shared carGameSacks(), carGameInts()

    Shared careerAF!(), careerAM!()
    Shared careerLC!(), careerLK!(), careerLR!(), careerLP!()
    Shared careerPK!()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Shared careerRBS$(), careerWRS$(), careerQBS$()
    Shared careerKRS$(), careerPRS$()

    Shared careerLF$(), careerLI$(), careerLC$()
    Shared careerLK$(), careerLP$(), careerLR$()
    Shared careerPK$()

    NB = -1
    NC = -1
    ND = -1
    NE = -1
    NF = -1
    NG = -1
    NH = -1
    NI = -1

    'Why are these set to 0 ?!?!
    NJ = 0
    NK = 0

    D = 0

    Do

        Color 11, 0
        Cls

        For J = 15 To 18
            Locate J
            Print Space$(68)
        Next

        Locate 15, 5
        Input "ENTER NAME OF CAREER STAT FILE "; careerFile$

        Do

            Color 15, 0
            Cls

            Call MenuBox(8, 8, 61, 6, 15, 1)

            Locate 10, 15

            Locate , 30: Color 3: Print "COMPILE STATS FOR:"
            Locate , 30: Color 15: Print "C";
            Color 7: Print "OLLEGE TEAM"
            Locate , 30: Color 15: Print "P";
            Color 7: Print "RO TEAM"

            I$ = GetKeyPress$

            Select Case UCase$(I$)
                Case "C": TT = 0
                Case "P": TT = 1
            End Select

        Loop Until UCase$(I$) = "C" Or UCase$(I$) = "P" 'Or I$ = Chr$(27)

        teamNbr = 0
        Call ChooseTeam(YN$, NTMS, teamNbr, team$)

        If teamNbr <> 0 Then

            statFile$ = nameStatFile$(team$)

            Do

                Color 15, 0
                Cls

                Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; statYear
                Locate 9, 22: Print "ANY CHANGE? (Y/N)"

                J$ = GetKeyPress$

            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N" Or J$ = Chr$(27)

            If UCase$(J$) = "N" Then

                Do
                    Locate , 5
                    Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
                    K$ = GetKeyPress$
                Loop Until UCase$(K$) = "Y" Or UCase$(K$) = "N" Or K$ = Chr$(27)

                If UCase$(K$) = "Y" Then
                    doPostLeaders = 1
                Else
                    doPostLeaders = 0
                End If

                If Not _FileExists(diskPaths$(1) + statFile$) Then

                    MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
                    BoxWidth = 24
                    BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                    Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
                    Call CSMsgBox(MsgText$, BoxWidth, Pass)
                    I$ = GetKeyPress$
                    Call CSMsgBox("", 0, Pass)

                Else

                    Locate 20, 5
                    Color 15
                    Print "MERGING AND UPDATING CAREER STATS . . . . PLEASE WAIT"

                    Open diskPaths$(1) + statFile$ + "S" For Input As #1

                    Input #1, GAMES

                    Close #1

                    Open diskPaths$(1) + statFile$ For Input As #2

                    Input #2, temp$

                    For I = 0 To 9:
                        Input #2, carRBacks$(I)
                    Next I

                    For I = 0 To 5:
                        Input #2, carWdRec$(I)
                    Next I

                    For I = 0 To 3:
                        Input #2, carQBacks$(I)
                    Next I

                    For I = 0 To 2:
                        Input #2, carKickRet$(I)
                    Next I

                    For I = 0 To 2:
                        Input #2, carPRet$(I)
                    Next I

                    Input #2, carPunter$(0)

                    For I = 0 To 1:
                        Input #2, carKicker$(I)
                    Next I

                    For J = 0 To 13
                        For K = 0 To 17:
                            Input #2, statsZ2!(J, K)
                        Next K
                    Next J

                    For L = 0 To 38:
                        Input #2, statsZ!(L)
                    Next L

                    For L = 0 To 38:
                        Input #2, statsZ1!(L)
                    Next L

                    For J = 0 To 9:
                        Input #2, carDefInts$(J)
                        For J1 = 0 To 2:
                            Input #2, carGameInts(J, J1)
                        Next
                    Next

                    For J = 0 To 14:
                        Input #2, carDefSacks$(J)
                        For J1 = 0 To 1:
                            Input #2, carGameSacks(J, J1)
                        Next
                    Next

                    For I = 0 To 9:
                        Input #2, careerLR!(I)
                        Input #2, careerLR$(I, 0)
                        Input #2, careerLR$(I, 1)
                    Next

                    For I = 0 To 15:
                        Input #2, careerLC!(I)
                        Input #2, careerLC$(I, 0)
                        Input #2, careerLC$(I, 1)
                    Next

                    For I = 0 To 3:
                        Input #2, careerLP!(I)
                        Input #2, careerLP$(I, 0)
                        Input #2, careerLP$(I, 1)
                    Next

                    For I = 0 To 2:
                        Input #2, careerLK!(I)
                        Input #2, careerLK$(I, 0)
                        Input #2, careerLK$(I, 1)
                    Next

                    For I = 0 To 2:
                        Input #2, careerPK!(I)
                        Input #2, careerPK$(I, 0)
                        Input #2, careerPK$(I, 1)
                    Next

                    For I = 0 To 1:
                        Input #2, statsLF!(I)
                        Input #2, careerLF$(I)
                        For I1 = 0 To 4:
                            Input #2, careerAM!(I, I1)
                            Input #2, careerAF!(I, I1)
                        Next
                    Next

                    For I = 0 To 9:
                        Input #2, statsLI!(I)
                        Input #2, careerLI$(I, 0)
                        Input #2, careerLI$(I, 1)
                    Next

                    Close #2

                    For I = 0 To 49

                        N = 1

                        If carRBacks$(I) = "END" Or carRBacks$(I) = "XXX" Or carRBacks$(I) = "" Then
                            carRBacks$(I) = ""
                        Else
                            NB = NB + 1
                        End If

                        For J = 0 To 5
                            careerRBS!(N, I, J) = statsZ2!(I, J)
                        Next J

                        careerRBS!(N, I, 6) = careerLR!(I)
                        careerRBS$(N, I, 0) = careerLR$(I, 0)
                        careerRBS!(N, I, 9) = careerLC!(I)
                        careerRBS$(N, I, 1) = careerLC$(I, 0)
                        careerRBS!(N, I, 7) = 1
                        careerRBS!(N, I, 8) = statYear

                    Next I

                    For I = 0 To 39

                        N = 1

                        If carWdRec$(I) = "END" Or carWdRec$(I) = "XXX" Or carWdRec$(I) = "" Then
                            carWdRec$(I) = ""
                        Else
                            NC = NC + 1
                        End If

                        For J = 6 To 8
                            careerWRS!(N, I, J - 6) = statsZ2!(I, J)
                        Next J

                        careerWRS!(N, I, 3) = careerLC!(I + 10)
                        careerWRS$(N, I, 0) = careerLC$(I + 10, 0)
                        careerWRS!(N, I, 4) = 1
                        careerWRS!(N, I, 5) = statYear

                    Next I

                    For I = 0 To 19

                        N = 1

                        If carQBacks$(I) = "END" Or carQBacks$(I) = "XXX" Or carQBacks$(I) = "" Then
                            carQBacks$(I) = ""
                        Else
                            ND = ND + 1
                        End If

                        For J = 9 To 13:
                            careerQBS!(N, I, J - 9) = statsZ2!(I, J)
                        Next J

                        careerQBS!(N, I, 5) = careerLP!(I)
                        careerQBS$(N, I, 0) = careerLP$(I, 0)
                        careerQBS!(N, I, 6) = 1
                        careerQBS!(N, I, 7) = statYear

                    Next I

                    For I = 0 To 14

                        N = 1

                        If carKickRet$(I) = "END" Or carKickRet$(I) = "XXX" Or carKickRet$(I) = "" Then
                            carKickRet$(I) = ""
                        Else
                            NE = NE + 1
                        End If

                        For J = 14 To 16:
                            careerKRS!(N, I, J - 14) = statsZ2!(I, J)
                        Next J

                        careerKRS!(N, I, 3) = careerLK!(I)
                        careerKRS$(N, I, 0) = careerLK$(I, 0)
                        careerKRS!(N, I, 4) = 1
                        careerKRS!(N, I, 5) = statYear

                    Next I

                    For I = 0 To 14

                        N = 1

                        If carPRet$(I) = "END" Or carPRet$(I) = "XXX" Or carPRet$(I) = "" Then
                            carPRet$(I) = ""
                        Else
                            NF = NF + 1
                        End If

                        For J = 14 To 16:
                            careerPRS!(N, I, J - 14) = statsZ2!(I + 3, J)
                        Next J

                        careerPRS!(N, I, 3) = careerPK!(I)
                        careerPRS$(N, I, 0) = careerPK$(I, 0)
                        careerPRS!(N, I, 4) = 1
                        careerPRS!(N, I, 5) = statYear

                    Next I

                    For I = 0 To 9

                        N = 1

                        If carPunter$(I) = "END" Or carPunter$(I) = "XXX" Or carPunter$(I) = "" Then
                            carPunter$(I) = ""
                        Else
                            NG = NG + 1
                        End If

                        careerPUS!(N, I, 0) = statsZ!(7)
                        careerPUS!(N, I, 1) = statsZ!(8)
                        careerPUS!(N, I, 2) = 1
                        careerPUS!(N, I, 3) = statYear
                    Next I

                    For I = 0 To 9

                        N = 1

                        If carKicker$(I) = "END" Or carKicker$(I) = "XXX" Or carKicker$(I) = "" Then
                            carKicker$(I) = ""
                        Else
                            NH = NH + 1
                        End If

                        If I = 0 Or I = 2 Or I = 4 Or I = 6 Or I = 8 Then

                            For J = 3 To 6
                                careerFGS!(N, I, J - 3) = statsZ2!(J, 17)
                            Next J

                            careerFGS!(N, I, 4) = statsLF!(I)
                            careerFGS!(N, I, 15) = 1
                            careerFGS!(N, I, 16) = statYear

                            For I1 = 0 To 4:
                                careerFGS!(N, I, I1 + 5) = careerAM!(I, I1)
                            Next

                            For I1 = 0 To 4:
                                careerFGS!(N, I, I1 + 10) = careerAF!(I, I1)
                            Next

                        End If

                        If I = 1 Or I = 3 Or I = 5 Or I = 7 Or I = 9 Then

                            For J = 7 To 10:
                                careerFGS!(N, I, J - 7) = statsZ2!(J, 17)
                            Next J

                            test = statsLF!(I)
                            careerFGS!(N, I, 4) = statsLF!(I)
                            careerFGS!(N, I, 15) = 1
                            careerFGS!(N, I, 16) = statYear

                            For I1 = 0 To 4:
                                careerFGS!(N, I, I1 + 5) = careerAM!(I, I1)
                            Next

                            For I1 = 0 To 4:
                                careerFGS!(N, I, I1 + 10) = careerAF!(I, I1)
                            Next

                        End If

                    Next I

                    For I = 0 To 54

                        N = 1

                        If carDefInts$(I) = "END" Or carDefInts$(I) = "XXX" Or carDefInts$(I) = "" Then
                            carDefInts$(I) = ""
                        Else
                            NJ = NJ + 1
                        End If

                        careerGIS!(N, I, 0) = carGameInts(I, 0)
                        careerGIS!(N, I, 1) = 1
                        careerGIS!(N, I, 2) = statYear
                    Next I

                    For I = 0 To 54

                        N = 1

                        If carDefSacks$(I) = "END" Or carDefSacks$(I) = "XXX" Or carDefSacks$(I) = "" Then
                            carDefSacks$(I) = ""
                        Else
                            NK = NK + 1
                        End If

                        careerGSS!(N, I, 0) = carGameSacks(I, 0)
                        careerGSS!(N, I, 1) = 1
                        careerGSS!(N, I, 2) = statYear

                    Next I

                    If doPostLeaders = 1 Then Call UpdateCareerRecords(careerFile$)

                    Call SaveCareerFile(careerFile$)

                End If 'Done checking for stat file

            End If 'Done checking for changes

        End If 'Done checking if team was chosen

    Loop Until UCase$(I$) = "N" Or UCase$(J$) = "N"

End Sub


'----------------------------------------
'    ReviewCareerRecords Subroutine
'----------------------------------------
'This routine loads in leader records from
'the .CLD file (which were compiled from
'the career file). It then gives the user
'an option to View or Print results, AND
'formats the results for them as directed.
Sub ReviewCareerRecords ()

    Shared careerWRR$, careerQBR$, careerKRR$, careerPRR$

    Shared careerDefRecPlyr$(), careerFGRecPlyr$(), careerKRRecPlyr$()
    Shared careerPRRecPlyr$(), careerPuntRecPlyr$(), careerQBRecPlyr$()
    Shared careerRBRecPlyr$(), careerRBR$(), careerWRRecPlyr$()

    Shared careerDefRecVal!(), careerFGRecVal!(), careerKRRecVal!()
    Shared careerPRRecVal!(), careerPuntRecVal!(), careerQBRecVal!()
    Shared careerRBRecVal!(), careerRBS!(), careerWRRecVal!()

    Shared carYear_DF(), carYear_FG(), carYear_KRet()
    Shared carYear_PRet(), carYear_Punt(), carYear_QB()
    Shared carYear_RB(), carYear_WR()

    recordFile$ = ChooseFile$(diskPaths$(2) + "*.CLD", "CAREER RECORD", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Call LoadCareerLeaders(recordFile$)

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Career", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CareerRecords_Report(0, userFile$)
                    End If

                Case "P"
                    Call CareerRecords_Report(1, "CAREER")

                Case "S"

                    Do
                        Cls
                        Color 15, 0

                        Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                        Print

                        Color 3: Print "RUSHING ATTEMPTS";
                        Color 7: Print Tab(32); Using "####"; careerRBRecVal!(0);: Print Tab(50); careerRBRecPlyr$(0);: Print Tab(64); carYear_RB(0)
                        Color 3: Print "RUSHING YARDS";
                        Color 7: Print Tab(32); Using "####"; careerRBRecVal!(1);: Print Tab(50); careerRBRecPlyr$(1);: Print Tab(64); carYear_RB(1)
                        Color 3: Print "RUSHING AVERAGE";
                        Color 7: Print Tab(32); Using "##.#"; careerRBRecVal!(2);: Print Tab(50); careerRBRecPlyr$(2);: Print Tab(64); carYear_RB(2)
                        Color 3: Print "RUSHING TOUCHDOWNS";
                        Color 7: Print Tab(33); Using "###"; careerRBRecVal!(3);: Print Tab(50); careerRBRecPlyr$(3);: Print Tab(64); carYear_RB(3)
                        Color 3: Print "LONGEST RUN";
                        Color 7: Print Tab(33); Using "###"; careerRBRecVal!(8);: Print Tab(36); careerRBR$(0);: Print Tab(50); careerRBRecPlyr$(8);: Print Tab(64); carYear_RB(8)
                        Color 3: Print "RECEPTIONS (BACKS)";
                        Color 7: Print Tab(32); Using "####"; careerRBRecVal!(4);: Print Tab(50); careerRBRecPlyr$(4);: Print Tab(64); carYear_RB(4)
                        Color 3: Print "RECEIVING YARDS (BACKS)";
                        Color 7: Print Tab(32); Using "####"; careerRBRecVal!(5);: Print Tab(50); careerRBRecPlyr$(5);: Print Tab(64); carYear_RB(5)
                        Color 3: Print "RECEIVING AVERAGE (BACKS)";
                        Color 7: Print Tab(32); Using "##.#"; careerRBRecVal!(6);: Print Tab(50); careerRBRecPlyr$(6);: Print Tab(64); carYear_RB(6)
                        Color 3: Print "RECEIVING TOUCHDOWNS (BACKS)";
                        Color 7: Print Tab(32); Using " ###"; careerRBRecVal!(7);: Print Tab(50); careerRBRecPlyr$(7);: Print Tab(64); carYear_RB(7)
                        Color 3: Print "LONGEST RECEPTION (BACKS)";
                        Color 7: Print Tab(33); Using "###"; careerRBRecVal!(9);: Print Tab(37); careerRBR$(1);: Print Tab(50); careerRBRecPlyr$(9);: Print Tab(64); carYear_RB(9)
                        Color 3: Print "RECEPTIONS";
                        Color 7: Print Tab(32); Using "####"; careerWRRecVal!(0);: Print Tab(50); careerWRRecPlyr$(0);: Print Tab(64); carYear_WR(0)
                        Color 3: Print "RECEIVING YARDS";
                        Color 7: Print Tab(32); Using "####"; careerWRRecVal!(1);: Print Tab(50); careerWRRecPlyr$(1);: Print Tab(64); carYear_WR(1)
                        Color 3: Print "RECEIVING AVERAGE";
                        Color 7: Print Tab(32); Using "##.#"; careerWRRecVal!(2);: Print Tab(50); careerWRRecPlyr$(2);: Print Tab(64); carYear_WR(2)
                        Color 3: Print "RECEIVING TOUCHDOWNS";
                        Color 7: Print Tab(32); Using " ###"; careerWRRecVal!(3);: Print Tab(50); careerWRRecPlyr$(3);: Print Tab(64); carYear_WR(3)
                        Color 3: Print "LONGEST RECEPTION";
                        Color 7: Print Tab(32); Using " ###"; careerWRRecVal!(4);: Print Tab(36); careerWRR$;: Print Tab(50); careerWRRecPlyr$(4);: Print Tab(64); carYear_WR(4)
                        Color 3: Print "COMPLETIONS";
                        Color 7: Print Tab(32); Using "####"; careerQBRecVal!(0);: Print Tab(50); careerQBRecPlyr$(0);: Print Tab(64); carYear_QB(0)
                        Color 3: Print "ATTEMPTS";
                        Color 7: Print Tab(32); Using "####"; careerQBRecVal!(1);: Print Tab(50); careerQBRecPlyr$(1);: Print Tab(64); carYear_QB(1)
                        Color 3: Print "INTERCEPTIONS";
                        Color 7: Print Tab(32); Using "####"; careerQBRecVal!(2);: Print Tab(50); careerQBRecPlyr$(2);: Print Tab(64); carYear_QB(2)

                        Color 15
                        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
                        I$ = GetKeyPress$

                        Cls
                        Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                        Color 3: Print "TOUCHDOWNS";
                        Color 7: Print Tab(31); Using " ####"; careerQBRecVal!(3);: Print Tab(50); careerQBRecPlyr$(3);: Print Tab(64); carYear_QB(3)
                        Color 3: Print "PASSING YARDS";
                        Color 7: Print Tab(32); Using "####"; careerQBRecVal!(4);: Print Tab(50); careerQBRecPlyr$(4);: Print Tab(64); carYear_QB(4)
                        Color 3: Print "COMPLETION PERCENTAGE";
                        Color 7: Print Tab(32); Using "##.#"; careerQBRecVal!(5) * 100;: Print Tab(50); careerQBRecPlyr$(5);: Print Tab(64); carYear_QB(5)
                        Color 3: Print "YARDS PER COMPLETION";
                        Color 7: Print Tab(32); Using "##.#"; careerQBRecVal!(6);: Print Tab(50); careerQBRecPlyr$(6);: Print Tab(64); carYear_QB(6)
                        Color 3: Print "QUARTERBACK RATING";
                        Color 7: Print Tab(31); Using "###.#"; careerQBRecVal!(7);: Print Tab(50); careerQBRecPlyr$(7);: Print Tab(64); carYear_QB(7)
                        Color 3: Print "LONGEST PASS";
                        Color 7: Print Tab(32); Using " ###"; careerQBRecVal!(8);: Print Tab(36); careerQBR$;: Print Tab(50); careerQBRecPlyr$(8);: Print Tab(64); carYear_QB(8)
                        Color 3: Print "KICK RETURN ATTEMPTS";
                        Color 7: Print Tab(33); Using "###"; careerKRRecVal!(0);: Print Tab(50); careerKRRecPlyr$(0);: Print Tab(64); carYear_KRet(0)
                        Color 3: Print "KICK RETURN YARDS";
                        Color 7: Print Tab(32); Using "####"; careerKRRecVal!(1);: Print Tab(50); careerKRRecPlyr$(1);: Print Tab(64); carYear_KRet(1)
                        Color 3: Print "KICK RETURN AVERAGE";
                        Color 7: Print Tab(32); Using "##.#"; careerKRRecVal!(2);: Print Tab(50); careerKRRecPlyr$(2);: Print Tab(64); carYear_KRet(2)
                        Color 3: Print "KICK RETURN TOUCHDOWNS";
                        Color 7: Print Tab(32); Using " ###"; careerKRRecVal!(3);: Print Tab(50); careerKRRecPlyr$(3);: Print Tab(64); carYear_KRet(3)
                        Color 3: Print "LONGEST KICK RETURN";
                        Color 7: Print Tab(32); Using " ###"; careerKRRecVal!(4);: Print Tab(36); careerKRR$;: Print Tab(50); careerKRRecPlyr$(4);: Print Tab(64); carYear_KRet(4)
                        Color 3: Print "PUNT RETURN ATTEMPTS";
                        Color 7: Print Tab(33); Using "###"; careerPRRecVal!(0);: Print Tab(50); careerPRRecPlyr$(0);: Print Tab(64); carYear_PRet(0)
                        Color 3: Print "PUNT RETURN YARDS";
                        Color 7: Print Tab(32); Using "####"; careerPRRecVal!(1);: Print Tab(50); careerPRRecPlyr$(1);: Print Tab(64); carYear_PRet(1)
                        Color 3: Print "PUNT RETURN AVERAGE";
                        Color 7: Print Tab(32); Using "##.#"; careerPRRecVal!(2);: Print Tab(50); careerPRRecPlyr$(2);: Print Tab(64); carYear_PRet(2)
                        Color 3: Print "PUNT RETURN TOUCHDOWNS";
                        Color 7: Print Tab(32); Using " ###"; careerPRRecVal!(3);: Print Tab(50); careerPRRecPlyr$(3);: Print Tab(64); carYear_PRet(3)
                        Color 3: Print "LONGEST PUNT RETURN";
                        Color 7: Print Tab(32); Using " ###"; careerPRRecVal!(4);: Print Tab(36); careerPRR$;: Print Tab(50); careerPRRecPlyr$(4);: Print Tab(64); carYear_PRet(4)
                        Color 3: Print "PUNT ATTEMPTS";
                        Color 7: Print Tab(32); Using "####"; careerPuntRecVal!(0);: Print Tab(50); careerPuntRecPlyr$(0);: Print Tab(64); carYear_Punt(0)
                        Color 3: Print "PUNTING YARDS";
                        Color 7: Print Tab(32); Using "####"; careerPuntRecVal!(1);: Print Tab(50); careerPuntRecPlyr$(1);: Print Tab(64); carYear_Punt(1)

                        Color 15
                        Locate 23, 5: Print "HIT ANY KEY TO CONTINUE"
                        I$ = GetKeyPress$

                        Cls
                        Color 12: Print Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
                        Color 3: Print "PUNTING AVERAGE";
                        Color 7: Print Tab(32); Using "##.#"; careerPuntRecVal!(2);: Print Tab(50); careerPuntRecPlyr$(2);: Print Tab(64); carYear_Punt(2)
                        Color 3: Print "FIELD GOAL ATTEMPTS";
                        Color 7: Print Tab(32); Using "####"; careerFGRecVal!(0);: Print Tab(50); careerFGRecPlyr$(0);: Print Tab(64); carYear_FG(0)
                        Color 3: Print "FIELD GOALS MADE";
                        Color 7: Print Tab(32); Using "####"; careerFGRecVal!(1);: Print Tab(50); careerFGRecPlyr$(1);: Print Tab(64); carYear_FG(1)
                        Color 3: Print "FIELD GOAL PERCENTAGE";
                        Color 7: Print Tab(31); Using "###.#"; careerFGRecVal!(2) * 100;: Print Tab(50); careerFGRecPlyr$(2);: Print Tab(64); carYear_FG(2)
                        Color 3: Print "LONGEST FIELD GOAL MADE";
                        Color 7: Print Tab(34); Using "##"; careerFGRecVal!(6);: Print Tab(50); careerFGRecPlyr$(6);: Print Tab(64); carYear_FG(6)
                        Color 3: Print "EXTRA POINT ATTEMPTS";
                        Color 7: Print Tab(32); Using "####"; careerFGRecVal!(3);: Print Tab(50); careerFGRecPlyr$(3);: Print Tab(64); carYear_FG(3)
                        Color 3: Print "EXTRA POINTS MADE";
                        Color 7: Print Tab(32); Using "####"; careerFGRecVal!(4);: Print Tab(50); careerFGRecPlyr$(4);: Print Tab(64); carYear_FG(4)
                        Color 3: Print "EXTRA POINT PERCENTAGE";
                        Color 7: Print Tab(31); Using "###.#"; careerFGRecVal!(5) * 100;: Print Tab(50); careerFGRecPlyr$(5);: Print Tab(64); carYear_FG(5)

                        Color 15

                        Do
                            Locate 22: Print "VIEW AGAIN? (Y/N)"
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y" Or I$ = Chr$(27)

                    Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

            End Select

        End If

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'    CareerRecords_Report Subroutine
'----------------------------------------
'This routine formats career records for
'output to either a file or printer, as
'specified by the user. All operations
'involve creating a file first, that can
'then be looped through if sending to
'printer
Sub CareerRecords_Report (printDest, destFile$)

    Shared careerDefRecPlyr$(), careerFGRecPlyr$(), careerKRRecPlyr$()
    Shared careerPRRecPlyr$(), careerPuntRecPlyr$(), careerQBRecPlyr$()
    Shared careerRBRecPlyr$(), careerRBR$(), careerWRRecPlyr$()

    Shared careerDefRecVal!(), careerFGRecVal!(), careerKRRecVal!()
    Shared careerPRRecVal!(), careerPuntRecVal!(), careerQBRecVal!()
    Shared careerRBRecVal!(), careerRBS!(), careerWRRecVal!()

    Shared carYear_DF(), carYear_FG(), carYear_KRet()
    Shared carYear_PRet(), carYear_Punt(), carYear_QB()
    Shared carYear_RB(), carYear_WR()

    Color 0, 15
    Cls

    Open destFile$ For Output As #1

    Print #1, Tab(10); "CATEGORY"; Tab(30); "RECORD"; Tab(50); "PLAYER"; Tab(65); "YEAR"
    Print #1,
    Print #1, "RUSHING ATTEMPTS";
    Print #1, Tab(32); Using "####"; careerRBRecVal!(0);
    Print #1, Tab(50); careerRBRecPlyr$(0);
    Print #1, Tab(64); carYear_RB(0)
    Print #1, "RUSHING YARDS";
    Print #1, Tab(32); Using "####"; careerRBRecVal!(1);
    Print #1, Tab(50); careerRBRecPlyr$(1);
    Print #1, Tab(64); carYear_RB(1)
    Print #1, "RUSHING AVERAGE";
    Print #1, Tab(32); Using "##.#"; careerRBRecVal!(2);
    Print #1, Tab(50); careerRBRecPlyr$(2);
    Print #1, Tab(64); carYear_RB(2)
    Print #1, "RUSHING TOUCHDOWNS";
    Print #1, Tab(33); Using "###"; careerRBRecVal!(3);
    Print #1, Tab(50); careerRBRecPlyr$(3);
    Print #1, Tab(64); carYear_RB(3)
    Print #1, "LONGEST RUN";
    Print #1, Tab(33); Using "###"; careerRBRecVal!(8);
    Print #1, Tab(36); careerRBR$(0);
    Print #1, Tab(50); careerRBRecPlyr$(8);
    Print #1, Tab(64); carYear_RB(8)
    Print #1, "RECEPTIONS (BACKS)";
    Print #1, Tab(32); Using "####"; careerRBRecVal!(4);
    Print #1, Tab(50); careerRBRecPlyr$(4);
    Print #1, Tab(64); carYear_RB(4)
    Print #1, "RECEIVING YARDS (BACKS)";
    Print #1, Tab(32); Using "####"; careerRBRecVal!(5);
    Print #1, Tab(50); careerRBRecPlyr$(5);
    Print #1, Tab(64); carYear_RB(5)
    Print #1, "RECEIVING AVERAGE (BACKS)";
    Print #1, Tab(32); Using "##.#"; careerRBRecVal!(6);
    Print #1, Tab(50); careerRBRecPlyr$(6);
    Print #1, Tab(64); carYear_RB(6)
    Print #1, "RECEIVING TOUCHDOWNS (BACKS)";
    Print #1, Tab(32); Using " ###"; careerRBRecVal!(7);
    Print #1, Tab(50); careerRBRecPlyr$(7);
    Print #1, Tab(64); carYear_RB(7)
    Print #1, "LONGEST RECEPTION (BACKS)";
    Print #1, Tab(33); Using "###"; careerRBRecVal!(9);
    Print #1, Tab(37); careerRBR$(1);
    Print #1, Tab(50); careerRBRecPlyr$(9);
    Print #1, Tab(64); carYear_RB(9)
    Print #1, "RECEPTIONS";
    Print #1, Tab(32); Using "####"; careerWRRecVal!(0);
    Print #1, Tab(50); careerWRRecPlyr$(0);
    Print #1, Tab(64); carYear_WR(0)
    Print #1, "RECEIVING YARDS";
    Print #1, Tab(32); Using "####"; careerWRRecVal!(1);
    Print #1, Tab(50); careerWRRecPlyr$(1);
    Print #1, Tab(64); carYear_WR(1)
    Print #1, "RECEIVING AVERAGE";
    Print #1, Tab(32); Using "##.#"; careerWRRecVal!(2);
    Print #1, Tab(50); careerWRRecPlyr$(2);
    Print #1, Tab(64); carYear_WR(2)
    Print #1, "RECEIVING TOUCHDOWNS";
    Print #1, Tab(32); Using " ###"; careerWRRecVal!(3);
    Print #1, Tab(50); careerWRRecPlyr$(3);
    Print #1, Tab(64); carYear_WR(3)
    Print #1, "LONGEST RECEPTION";
    Print #1, Tab(32); Using " ###"; careerWRRecVal!(4);
    Print #1, Tab(36); careerWRR$;
    Print #1, Tab(50); careerWRRecPlyr$(4);
    Print #1, Tab(64); carYear_WR(4)
    Print #1, "COMPLETIONS";
    Print #1, Tab(32); Using "####"; careerQBRecVal!(0);
    Print #1, Tab(50); careerQBRecPlyr$(0);
    Print #1, Tab(64); carYear_QB(0)
    Print #1, "ATTEMPTS";
    Print #1, Tab(32); Using "####"; careerQBRecVal!(1);
    Print #1, Tab(50); careerQBRecPlyr$(1);
    Print #1, Tab(64); carYear_QB(1)
    Print #1, "INTERCEPTIONS";
    Print #1, Tab(32); Using "####"; careerQBRecVal!(2);
    Print #1, Tab(50); careerQBRecPlyr$(2);
    Print #1, Tab(64); carYear_QB(2)
    Print #1, "TOUCHDOWNS";
    Print #1, Tab(31); Using " ####"; careerQBRecVal!(3);
    Print #1, Tab(50); careerQBRecPlyr$(3);
    Print #1, Tab(64); carYear_QB(3)
    Print #1, "PASSING YARDS";
    Print #1, Tab(32); Using "####"; careerQBRecVal!(4);
    Print #1, Tab(50); careerQBRecPlyr$(4);
    Print #1, Tab(64); carYear_QB(4)
    Print #1, "COMPLETION PERCENTAGE";
    Print #1, Tab(32); Using "##.#"; careerQBRecVal!(5) * 100;
    Print #1, Tab(50); careerQBRecPlyr$(5);
    Print #1, Tab(64); carYear_QB(5)
    Print #1, "YARDS PER COMPLETION";
    Print #1, Tab(32); Using "##.#"; careerQBRecVal!(6);
    Print #1, Tab(50); careerQBRecPlyr$(6);
    Print #1, Tab(64); carYear_QB(6)
    Print #1, "QUARTERBACK RATING";
    Print #1, Tab(31); Using "###.#"; careerQBRecVal!(7);
    Print #1, Tab(50); careerQBRecPlyr$(7);
    Print #1, Tab(64); carYear_QB(7)
    Print #1, "LONGEST PASS";
    Print #1, Tab(32); Using " ###"; careerQBRecVal!(8);
    Print #1, Tab(36); QBR$;
    Print #1, Tab(50); careerQBRecPlyr$(8);
    Print #1, Tab(64); carYear_QB(8)
    Print #1, "KICK RETURN ATTEMPTS";
    Print #1, Tab(33); Using "###"; careerKRRecVal!(0);
    Print #1, Tab(50); careerKRRecPlyr$(0);
    Print #1, Tab(64); carYear_KRet(0)
    Print #1, "KICK RETURN YARDS";
    Print #1, Tab(32); Using "####"; careerKRRecVal!(1);
    Print #1, Tab(50); careerKRRecPlyr$(1);
    Print #1, Tab(64); carYear_KRet(1)
    Print #1, "KICK RETURN AVERAGE";
    Print #1, Tab(32); Using "##.#"; careerKRRecVal!(2);
    Print #1, Tab(50); careerKRRecPlyr$(2);
    Print #1, Tab(64); carYear_KRet(2)
    Print #1, "KICK RETURN TOUCHDOWNS";
    Print #1, Tab(32); Using " ###"; careerKRRecVal!(3);
    Print #1, Tab(50); careerKRRecPlyr$(3);
    Print #1, Tab(64); carYear_KRet(3)
    Print #1, "LONGEST KICK RETURN";
    Print #1, Tab(32); Using " ###"; careerKRRecVal!(4);
    Print #1, Tab(36); careerKRR$;
    Print #1, Tab(50); careerKRRecPlyr$(4);
    Print #1, Tab(64); carYear_KRet(4)
    Print #1, "PUNT RETURN ATTEMPTS";
    Print #1, Tab(33); Using "###"; careerPRRecVal!(0);
    Print #1, Tab(50); careerPRRecPlyr$(0);
    Print #1, Tab(64); carYear_PRet(0)
    Print #1, "PUNT RETURN YARDS";
    Print #1, Tab(32); Using "####"; careerPRRecVal!(1);
    Print #1, Tab(50); careerPRRecPlyr$(1);
    Print #1, Tab(64); carYear_PRet(1)
    Print #1, "PUNT RETURN AVERAGE";
    Print #1, Tab(32); Using "##.#"; careerPRRecVal!(2);
    Print #1, Tab(50); careerPRRecPlyr$(2);
    Print #1, Tab(64); carYear_PRet(2)
    Print #1, "PUNT RETURN TOUCHDOWNS";
    Print #1, Tab(32); Using " ###"; careerPRRecVal!(3);
    Print #1, Tab(50); careerPRRecPlyr$(3);
    Print #1, Tab(64); carYear_PRet(3)
    Print #1, "LONGEST PUNT RETURN";
    Print #1, Tab(32); Using " ###"; careerPRRecVal!(4);
    Print #1, Tab(36); PRR$;
    Print #1, Tab(50); careerPRRecPlyr$(4);
    Print #1, Tab(64); carYear_PRet(4)
    Print #1, "PUNT ATTEMPTS";
    Print #1, Tab(32); Using "####"; careerPuntRecVal!(0);
    Print #1, Tab(50); careerPuntRecPlyr$(0);
    Print #1, Tab(64); carYear_Punt(0)
    Print #1, "PUNTING YARDS";
    Print #1, Tab(32); Using "####"; careerPuntRecVal!(1);
    Print #1, Tab(50); careerPuntRecPlyr$(1);
    Print #1, Tab(64); carYear_Punt(1)
    Print #1, "PUNTING AVERAGE";
    Print #1, Tab(32); Using "##.#"; careerPuntRecVal!(2);
    Print #1, Tab(50); careerPuntRecPlyr$(2);
    Print #1, Tab(64); carYear_Punt(2)
    Print #1, "FIELD GOAL ATTEMPTS";
    Print #1, Tab(32); Using "####"; careerFGRecVal!(0);
    Print #1, Tab(50); careerFGRecPlyr$(0);
    Print #1, Tab(64); carYear_FG(0)
    Print #1, "FIELD GOALS MADE";
    Print #1, Tab(32); Using "####"; careerFGRecVal!(1);
    Print #1, Tab(50); careerFGRecPlyr$(1);
    Print #1, Tab(64); carYear_FG(1)
    Print #1, "FIELD GOAL PERCENTAGE";
    Print #1, Tab(31); Using "###.#"; careerFGRecVal!(2) * 100;
    Print #1, Tab(50); careerFGRecPlyr$(2);
    Print #1, Tab(64); carYear_FG(2)
    Print #1, "LONGEST FIELD GOAL MADE";
    Print #1, Tab(34); Using "##"; careerFGRecVal!(6);
    Print #1, Tab(50); careerFGRecPlyr$(6);
    Print #1, Tab(64); carYear_FG(6)
    Print #1, "EXTRA POINT ATTEMPTS";
    Print #1, Tab(32); Using "####"; careerFGRecVal!(3);
    Print #1, Tab(50); careerFGRecPlyr$(3);
    Print #1, Tab(64); carYear_FG(3)
    Print #1, "EXTRA POINTS MADE";
    Print #1, Tab(32); Using "####"; careerFGRecVal!(4);
    Print #1, Tab(50); careerFGRecPlyr$(4);
    Print #1, Tab(64); carYear_FG(4)
    Print #1, "EXTRA POINT PERCENTAGE";
    Print #1, Tab(31); Using "###.#"; careerFGRecVal!(5) * 100;
    Print #1, Tab(50); careerFGRecPlyr$(5);
    Print #1, Tab(64); carYear_FG(5)

    If printDest = 1 Then

        Open destFile$ For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close #2

        LPrint Chr$(12)

        Kill destFile$

    End If

End Sub


'----------------------------------------
'      MergeCareerFiles Subroutine
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

        careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

        If ExitCode% <> 27 And ExitCode% <> 0 Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            If Not _FileExists(diskPaths$(1) + statFile$) Then

                MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
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
        Locate 8, 22: Input "ENTER YEAR TO ADD (EX. - 1961)"; targetYear
        Locate 9, 22: Print "ANY CHANGE? (Y/N)"
        J$ = GetKeyPress$
    Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

    If UCase$(J$) = "N" Then

        Do
            Locate , 5
            Print "WOULD YOU LIKE TO POST CAREER RECORDS? (Y/N)"
            K$ = GetKeyPress$
            finalUpdate$ = UCase$(K$)
        Loop Until finalUpdate$ = "Y" Or finalUpdate$ = "N" Or K$ = Chr$(27)

        If finalUpdate$ = "Y" Then
            doPostLeaders = 1
        Else
            doPostLeaders = 0
        End If

        Call AddCareerYear(statYear, doPostLeaders, selectedFile$, statFile$)

    End If ' Done checking for year to add

End Sub


'----------------------------------------
'       AddCareerYear Subroutine
'----------------------------------------
'This routine loads the existing career
'stats, adds the team's stats to them,
'then overwrites the career file with the
'new values.
Sub AddCareerYear (statYear, finalUpdate, careerFile$, statFile$)

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPunter$(), carPRet$(), carKicker$()
    Shared carDefInts$(), carDefSacks$()

    Shared carGameInts(), carGameSacks()

    Shared careerKRS$()
    Shared careerPK$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerLF$(), careerLI$(), careerLK$()
    Shared statsLC$(), statsLK$(), statsLP$()
    Shared statsLR$()

    Shared careerFGS!(), careerGIS!(), careerGSS!(), careerKRS!()
    Shared careerLC!(), careerLK!(), careerLR!(), careerLP!()
    Shared careerPK!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Shared statsAF!(), statsAM!()
    Shared statsLI!(), statsLF!()
    Shared statsZ!(), statsZ1!(), statsZ2!()

    Call LoadCareerFile(careerFile$)

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

    For I = 0 To 9:
        Input #3, rbName$(I)
    Next I

    For I = 0 To 5:
        Input #3, wrName$(I)
    Next I

    For I = 0 To 3:
        Input #3, qbName$(I)
    Next I

    For I = 0 To 2:
        Input #3, krName$(I)
    Next I

    For I = 0 To 2:
        Input #3, prName$(I)
    Next I

    Input #3, punterName$(0)

    For I = 0 To 1:
        Input #3, pkName$(I)
    Next I

    For J = 0 To 13
        For K = 0 To 17:
            Input #3, statsZ2!(J, K)
        Next K
    Next J

    For L = 0 To 38:
        Input #3, statsZ!(L)
    Next L

    For L = 0 To 38:
        Input #3, statsZ1!(L)
    Next L

    For J = 0 To 9:
        Input #3, carDefInts$(J)
        For J1 = 0 To 2:
            Input #3, carGameInts(J, J1)
        Next
    Next

    For J = 0 To 14:
        Input #3, carDefSacks$(J)
        For J1 = 0 To 1:
            Input #3, carGameSacks(J, J1)
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
        Input #3, statsLF!(I)
        Input #3, careerLF$(I)
        For I1 = 0 To 4:
            Input #3, statsAM!(I, I1)
            Input #3, statsAF!(I, I1)
        Next
    Next

    For I = 0 To 9:
        Input #3, statsLI!(I)
        Input #3, careerLI$(I, 0)
        Input #3, careerLI$(I, 1)
    Next

    Close #3

    For I = 0 To 9:
        If Len(rbName$(I)) = 0 Then rbName$(I) = "XXX"
    Next

    For I = 0 To 5:
        If Len(wrName$(I)) = 0 Then wrName$(I) = "XXX"
    Next

    For I = 0 To 3:
        If Len(qbName$(I)) = 0 Then qbName$(I) = "XXX"
    Next

    For I = 0 To 2:
        If Len(krName$(I)) = 0 Then krName$(I) = "XXX"
    Next

    For I = 0 To 2:
        If Len(prName$(I)) = 0 Then prName$(I) = "XXX"
    Next

    For I = 0 To 1:
        If Len(pkName$(I)) = 0 Then pkName$(I) = "XXX"
    Next

    For I = 0 To 9:
        If Len(carDefInts$(I)) = 0 Then carDefInts$(I) = "XXX"
    Next

    For I = 0 To 14:
        If Len(carDefSacks$(I)) = 0 Then carDefSacks$(I) = "XXX"
    Next

    '-----

    For I = 0 To 9

        RR = 0
        CHK = 0

        For J = 0 To 49

            If rbName$(I) = carRBacks$(J) Then

                For W = 1 To MAX_CAREER_YEARS:
                    If careerRBS!(W, J, 8) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerRBS!(Z, J, 7)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                For K = 0 To 5
                    careerRBS!(N, J, K) = statsZ2!(I, K)
                Next K

                careerRBS!(N, J, 6) = careerLR!(I)
                careerRBS$(N, J, 0) = statsLR$(I, 0)
                careerRBS!(N, J, 9) = careerLC!(I)
                careerRBS$(N, J, 1) = statsLC$(I, 0)
                careerRBS!(N, J, 7) = 1
                careerRBS!(N, J, 8) = targetYear

                Exit For

            End If

            'Technically this is an "Else" clause
            'from the above, but the Exit means
            'it won't ever get here anyway
            RR = RR + 1

            If RR = 50 Then

                If rbName$(I) <> "END" And rbName$(I) <> "XXX" And Left$(rbName$(I), 1) <> " " Then

                    NB = NB + 1
                    carRBacks$(NB) = rbName$(I)
                    N = 1

                    For K = 0 To 5:
                        careerRBS!(N, NB, K) = statsZ2!(I, K)
                    Next

                    careerRBS!(N, NB, 6) = careerLR!(I)
                    careerRBS$(N, NB, 0) = statsLR$(I, 0)
                    careerRBS!(N, NB, 9) = careerLC!(I)
                    careerRBS$(N, NB, 1) = statsLC$(I, 0)
                    careerRBS!(N, NB, 7) = 1
                    careerRBS!(N, NB, 8) = targetYear

                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 5

        RR = 0
        CHK = 0

        For J = 0 To 39

            If wrName$(I) = carWdRec$(J) Then

                For W = 1 To MAX_CAREER_YEARS
                    If careerWRS!(W, J, 5) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerWRS!(Z, J, 4)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                For K = 6 To 8
                    careerWRS!(N, J, K - 6) = statsZ2!(I, K)
                Next K

                careerWRS!(N, J, 3) = careerLC!(I + 10)
                careerWRS$(N, J, 0) = statsLC$(I + 10, 0)
                careerWRS!(N, J, 4) = 1
                careerWRS!(N, J, 5) = targetYear

                Exit For

            End If

            1473
            RR = RR + 1
            If RR = 40 Then

                If wrName$(I) <> "END" And wrName$(I) <> "XXX" And Left$(wrName$(I), 1) <> " " Then

                    NC = NC + 1
                    carWdRec$(NC) = wrName$(I)
                    N = 1

                    For K = 6 To 8:
                        careerWRS!(N, NC, K - 6) = statsZ2!(I, K)
                    Next

                    careerWRS!(N, NC, 3) = careerLC!(I + 10)
                    careerWRS$(N, NC, 0) = statsLC$(I + 10, 0)
                    careerWRS!(N, NC, 4) = 1
                    careerWRS!(N, NC, 5) = targetYear

                End If

            End If

        Next J

    Next I

    '-----

    For I = 0 To 3

        RR = 0
        CHK = 0

        For J = 0 To 19

            If qbName$(I) = carQBacks$(J) Then

                For W = 1 To MAX_CAREER_YEARS:
                    If careerQBS!(W, J, 7) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerQBS!(Z, J, 6)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                For K = 9 To 13
                    careerQBS!(N, J, K - 9) = statsZ2!(I, K)
                Next K

                careerQBS!(N, J, 5) = careerLP!(I)
                careerQBS$(N, J, 0) = statsLP$(I, 0)
                careerQBS!(N, J, 6) = 1
                careerQBS!(N, J, 7) = targetYear

                Exit For

            Else

                RR = RR + 1

                If RR = 20 Then

                    If qbName$(I) <> "END" And qbName$(I) <> "XXX" And Left$(qbName$(I), 1) <> " " Then

                        ND = ND + 1
                        carQBacks$(ND) = qbName$(I)
                        N = 1

                        For K = 9 To 13:
                            careerQBS!(N, ND, K - 9) = statsZ2!(I, K)
                        Next

                        careerQBS!(N, ND, 5) = careerLP!(I)
                        careerQBS$(N, ND, 0) = statsLP$(I, 0)
                        careerQBS!(N, ND, 6) = 1
                        careerQBS!(N, ND, 7) = targetYear

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

            If krName$(I) = carKickRet$(J) Then

                For W = 1 To MAX_CAREER_YEARS:
                    If careerKRS!(W, J, 5) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerKRS!(Z, J, 4)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                For K = 14 To 16
                    careerKRS!(N, J, K - 14) = statsZ2!(I, K)
                Next K

                careerKRS!(N, J, 3) = careerLK!(I)
                careerKRS$(N, J, 0) = statsLK$(I, 0)
                careerKRS!(N, J, 4) = 1
                careerKRS!(N, J, 5) = targetYear

                Exit For

            Else

                RR = RR + 1

                If RR = 15 Then

                    If krName$(I) <> "END" And krName$(I) <> "XXX" And Left$(krName$(I), 1) <> " " Then

                        NE = NE + 1
                        carKickRet$(NE) = krName$(I)
                        N = 1

                        For K = 14 To 16:
                            careerKRS!(N, NE, K - 14) = statsZ2!(I, K)
                        Next

                        careerKRS!(N, NE, 3) = careerLK!(I)
                        careerKRS$(N, NE, 0) = statsLK$(I, 0)
                        careerKRS!(N, NE, 4) = 1
                        careerKRS!(N, NE, 5) = targetYear

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

            If prName$(I) = carPRet$(J) Then

                For W = 1 To MAX_CAREER_YEARS:
                    If careerPRS!(W, J, 5) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerPRS!(Z, J, 4)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                For K = 14 To 16
                    careerPRS!(N, J, K - 14) = statsZ2!(I + 3, K)
                Next K

                careerPRS!(N, J, 3) = careerPK!(I)
                careerPRS$(N, J, 0) = careerPK$(I, 0)
                careerPRS!(N, J, 4) = 1
                careerPRS!(N, J, 5) = targetYear

                Exit For

            Else

                RR = RR + 1
                If RR = 15 Then

                    If prName$(I) <> "END" And prName$(I) <> "XXX" And Left$(prName$(I), 1) <> " " Then

                        NF = NF + 1
                        carPRet$(NF) = prName$(I)
                        N = 1

                        For K = 14 To 16:
                            careerPRS!(N, NF, K - 14) = statsZ2!(I + 3, K)
                        Next

                        careerPRS!(N, NF, 3) = careerPK!(I)
                        careerPRS$(N, NF, 0) = careerPK$(I, 0)
                        careerPRS!(N, NF, 4) = 1
                        careerPRS!(N, NF, 5) = targetYear

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

            If punterName$(I) = carPunter$(J) Then

                For W = 1 To MAX_CAREER_YEARS:
                    If careerPUS!(W, J, 3) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerPUS!(Z, J, 2)
                Next Z

                careerPUS!(N, J, 0) = statsZ!(7)
                careerPUS!(N, J, 1) = statsZ!(8)
                careerPUS!(N, J, 2) = 1
                careerPUS!(N, J, 3) = targetYear

                Exit For

            Else

                RR = RR + 1

                If RR = 10 Then

                    If punterName$(I) <> "END" And punterName$(I) <> "XXX" And Left$(punterName$(I), 1) <> " " Then

                        NG = NG + 1
                        carPunter$(NG) = punterName$(I)
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

            If pkName$(I) = carKicker$(J) Then

                For W = 1 To MAX_CAREER_YEARS:
                    If careerFGS!(W, J, 16) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerFGS!(Z, J, 15)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                If I = 0 Then
                    For K = 3 To 6
                        careerFGS!(N, J, K - 3) = statsZ2!(K, 17)
                    Next K

                    test = statsLF!(I)
                    careerFGS!(N, J, 4) = statsLF!(I)
                    careerFGS!(N, J, 15) = 1
                    careerFGS!(N, J, 16) = targetYear

                    For J1 = 0 To 4:
                        careerFGS!(N, J, J1 + 5) = statsAM!(I, J1)
                    Next

                    For J1 = 0 To 4:
                        careerFGS!(N, J, J1 + 10) = statsAF!(I, J1)
                    Next

                Else

                    If I = 1 Then

                        For K = 7 To 10
                            careerFGS!(N, J, K - 7) = statsZ2!(K, 17)
                        Next K

                        careerFGS!(N, J, 4) = statsLF!(I)
                        careerFGS!(N, J, 15) = 1
                        careerFGS!(N, J, 16) = targetYear

                        For J1 = 0 To 4:
                            careerFGS!(N, J, J1 + 5) = statsAM!(I, J1)
                        Next

                        For J1 = 0 To 4:
                            careerFGS!(N, J, J1 + 10) = statsAF!(I, J1)
                        Next

                    End If

                End If

                Exit For

            Else

                RR = RR + 1

                If RR = 10 Then

                    If pkName$(I) <> "END" And pkName$(I) <> "XXX" And Left$(pkName$(I), 1) <> " " Then

                        NH = NH + 1
                        carKicker$(NH) = pkName$(I)
                        N = 1

                        If I = 0 Then

                            For K = 3 To 6:
                                careerFGS!(N, NH, K - 3) = statsZ2!(K, 17)
                            Next K

                            careerFGS!(N, NH, 4) = statsLF!(I)
                            careerFGS!(N, NH, 15) = 1
                            careerFGS!(N, NH, 16) = targetYear

                            For J1 = 0 To 4:
                                careerFGS!(N, NH, J1 + 5) = statsAM!(I, J1)
                            Next

                            For J1 = 0 To 4:
                                careerFGS!(N, NH, J1 + 10) = statsAF!(I, J1)
                            Next

                            skipLoop% = 1
                            Exit For

                        End If

                        If I = 1 Then

                            For K = 7 To 10:
                                careerFGS!(N, NH, K - 7) = statsZ2!(K, 17)
                            Next K

                            careerFGS!(N, NH, 4) = statsLF!(I)
                            careerFGS!(N, NH, 15) = 1
                            careerFGS!(N, J, 16) = targetYear

                            For J1 = 0 To 4:
                                careerFGS!(N, NH, J1 + 5) = statsAM!(I, J1)
                            Next

                            For J1 = 0 To 4:
                                careerFGS!(N, NH, J1 + 10) = statsAF!(I, J1)
                            Next

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

            If carDefInts$(I) = carDefInts$(J) Then
                For W = 1 To MAX_CAREER_YEARS:
                    If careerGIS!(W, J, 2) = targetYear Then
                        CHK = CHK + 1
                    End If
                Next W

                N = 0
                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerGIS!(Z, J, 1)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                careerGIS!(N, J, 0) = carGameInts(I, 0)
                careerGIS!(N, J, 1) = 1
                careerGIS!(N, J, 2) = targetYear
                Exit For

            Else

                RR = RR + 1
                If RR = 10 Then

                    If carDefInts$(I) <> "END" And carDefInts$(I) <> "XXX" And Left$(carDefInts$(I), 1) <> " " Then

                        NJ = NJ + 1
                        carDefInts$(NJ) = carDefInts$(I)
                        N = 1
                        careerGIS!(N, NJ, 0) = carGameInts(I, 0)
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

            If carDefSacks$(I) = carDefSacks$(J) Then

                For W = 1 To MAX_CAREER_YEARS:

                    If careerGSS!(W, J, 2) = targetYear Then
                        CHK = CHK + 1
                    End If

                Next W

                N = 0
                For Z = 1 To MAX_CAREER_YEARS
                    N = N + careerGSS!(Z, J, 1)
                Next Z

                If CHK = 0 Then
                    N = N + 1
                End If

                careerGSS!(N, J, 0) = carGameSacks(I, 0)
                careerGSS!(N, J, 1) = 1
                careerGSS!(N, J, 2) = targetYear

                Exit For

            Else

                RR = RR + 1

                If RR = 15 Then

                    If carDefSacks$(I) <> "END" And carDefSacks$(I) <> "XXX" And Left$(carDefSacks$(I), 1) <> " " Then

                        NK = NK + 1
                        carDefSacks$(NK) = carDefSacks$(I)
                        N = 1
                        careerGSS!(N, NK, 0) = carGameSacks(I, 0)
                        careerGSS!(N, NK, 1) = 1
                        careerGSS!(N, NK, 2) = targetYear

                    End If

                End If

            End If

        Next J

    Next I

    '-----

    If finalUpdate = 1 Then Call UpdateCareerRecords(careerFile$)

    Call SaveCareerFile(careerFile$)

End Sub


'----------------------------------------
'      LoadCareerFile Subroutine
'----------------------------------------
'This routine loads the existing career
'file data into arrays.
Sub LoadCareerFile (loadFile$)

    Shared TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared carDefInts$(), carDefSacks$()

    'S = Stats
    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!(), careerFGS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Color 15, 0
    Cls

    Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

    TMR = 50 + 40 + 20 + 15 + 15 + 10 + 10 + 55 + (15 * 50) + (15 * 50) + (15 * 20) + (15 * 20) + (15 * 40) + (15 * 15) + (15 * 15) + (15 * 15) + (15 * 15) + (15 * 10) + (15 * 10) + (15 * 55) + (15 * 55)

    Open diskPaths$(1) + loadFile$ + ".CAR" For Input As #1

    Input #1, TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    For I = 0 To 49:
        Input #1, carRBacks$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 39
        Input #1, carWdRec$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 19
        Input #1, carQBacks$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 14
        Input #1, carKickRet$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 14
        Input #1, carPRet$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 9
        Input #1, carPunter$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 9
        Input #1, carKicker$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 54
        Input #1, carDefInts$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 54
        Input #1, carDefSacks$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 49:

            For K = 0 To 9
                Input #1, careerRBS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 49
            Input #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 39:

            For K = 0 To 5
                Input #1, careerWRS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 39
            Input #1, careerWRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 19:

            For K = 0 To 7
                Input #1, careerQBS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 19
            Input #1, careerQBS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14:

            For K = 0 To 5
                Input #1, careerKRS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14
            Input #1, careerKRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14:

            For K = 0 To 5
                Input #1, careerPRS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14
            Input #1, careerPRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 9:

            For K = 0 To 3
                Input #1, careerPUS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 9:

            For K = 0 To 16
                Input #1, careerFGS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 54:

            For K = 0 To 2
                Input #1, careerGIS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 54:

            For K = 0 To 2
                Input #1, careerGSS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    Close #1

End Sub


'----------------------------------------
'      SaveCareerFile Subroutine
'----------------------------------------
'This routine updates the existing career
'file with the modified stats values.
Sub SaveCareerFile (saveFile$)

    Shared TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared carDefInts$(), carDefSacks$()

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!(), careerFGS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Cls
    Color 15
    Locate 12, 6: Print "SAVING UPDATED CAREER FILE . . . PLEASE WAIT"

    TMR = 50 + 40 + 20 + 15 + 15 + 10 + 10 + 55 + (15 * 50) + (15 * 50) + (15 * 20) + (15 * 20) + (15 * 40) + (15 * 15) + (15 * 15) + (15 * 15) + (15 * 15) + (15 * 10) + (15 * 10) + (15 * 55) + (15 * 55)

    Open diskPaths$(1) + saveFile$ + ".CAR" For Output As #3

    Write #3, TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    For I = 0 To 49:
        Write #3, carRBacks$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 39:
        Write #3, carWdRec$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 19:
        Write #3, carQBacks$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 14:
        Write #3, carKickRet$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 14:
        Write #3, carPRet$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 9:
        Write #3, carPunter$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 9:
        Write #3, carKicker$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 54:
        Write #3, carDefInts$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For I = 0 To 54:
        Write #3, carDefSacks$(I)
        Locate 21, 38
        Color 5
        TMR = TMR - 1: Print Using "####"; TMR
    Next

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 49:

            For K = 0 To 9:
                Write #3, careerRBS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 49:
            Write #3, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 39

            For K = 0 To 5:
                Write #3, careerWRS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5: TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 39:
            Write #3, careerWRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 19:

            For K = 0 To 7:
                Write #3, careerQBS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 19:
            Write #3, careerQBS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14:

            For K = 0 To 5:
                Write #3, careerKRS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14:
            Write #3, careerKRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14

            For K = 0 To 5:
                Write #3, careerPRS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 14:
            Write #3, careerPRS$(N, J, 0)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 9:

            For K = 0 To 3:
                Write #3, careerPUS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 9:

            For K = 0 To 16:
                Write #3, careerFGS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 54:

            For K = 0 To 2:
                Write #3, careerGIS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    For N = 1 To MAX_CAREER_YEARS:

        For J = 0 To 54:

            For K = 0 To 2:
                Write #3, careerGSS!(N, J, K)
            Next K

            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR

        Next J

    Next N

    Close #3

End Sub


'----------------------------------------
'      LoadCareerLeaders Subroutine
'----------------------------------------
'This routine loads the existing career
'leaders file data into arrays.
Sub LoadCareerLeaders (leaderFile$)

    Shared careerWRR$, careerQBR$, careerKRR$, careerPRR$

    Shared careerDefRecPlyr$(), careerFGRecPlyr$(), careerKRRecPlyr$()
    Shared careerPRRecPlyr$(), careerPuntRecPlyr$(), careerQBRecPlyr$()
    Shared careerRBRecPlyr$(), careerRBR$(), careerWRRecPlyr$()

    Shared careerDefRecVal!(), careerFGRecVal!(), careerKRRecVal!()
    Shared careerPRRecVal!(), careerPuntRecVal!(), careerQBRecVal!()
    Shared careerRBRecVal!(), careerRBS!(), careerWRRecVal!()

    Shared carYear_DF(), carYear_FG(), carYear_KRet()
    Shared carYear_PRet(), carYear_Punt(), carYear_QB()
    Shared carYear_RB(), carYear_WR()

    Open diskPaths$(2) + leaderFile$ + ".CLD" For Input As #1

    For I = 0 To 9:
        Input #1, careerRBRecVal!(I), careerRBRecPlyr$(I), carYear_RB(I)
    Next

    Input #1, careerRBR$(0), careerRBR$(1)

    For I = 0 To 4:
        Input #1, careerWRRecVal!(I), careerWRRecPlyr$(I), carYear_WR(I)
    Next

    Input #1, careerWRR$

    For I = 0 To 8:
        Input #1, careerQBRecVal!(I), careerQBRecPlyr$(I), carYear_QB(I)
    Next

    Input #1, careerQBR$

    For I = 0 To 4:
        Input #1, careerKRRecVal!(I), careerKRRecPlyr$(I), carYear_KRet(I)
    Next

    Input #1, careerKRR$

    For I = 0 To 4:
        Input #1, careerPRRecVal!(I), careerPRRecPlyr$(I), carYear_PRet(I)
    Next

    Input #1, careerPRR$

    For I = 0 To 2:
        Input #1, careerPuntRecVal!(I), careerPuntRecPlyr$(I), carYear_Punt(I)
    Next

    For I = 0 To 6:
        Input #1, careerFGRecVal!(I), careerFGRecPlyr$(I), carYear_FG(I)
    Next

    For I = 0 To 1:
        Input #1, careerDefRecVal!(I), careerDefRecPlyr$(I), carYear_DF(I)
    Next

    Close #1

End Sub


'----------------------------------------
'     UpdateCareerRecords Subroutine
'----------------------------------------
'This routine reads in any existing career
'leader files (.CLD), adds the new stats,
'and then saves the changes back to the
'career leader file (.CLD)
Sub UpdateCareerRecords (careerFile$)

    Shared TT

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared carDefInts$(), carDefSacks$()

    Shared careerDefRecPlyr$(), careerFGRecPlyr$(), careerKRRecPlyr$()
    Shared careerPRRecPlyr$(), careerPuntRecPlyr$(), careerQBRecPlyr$()
    Shared careerRBRecPlyr$(), careerRBR$(), careerWRRecPlyr$()

    Shared careerDefRecVal!(), careerFGRecVal!(), careerKRRecVal!()
    Shared careerPRRecVal!(), careerPuntRecVal!(), careerQBRecVal!()
    Shared careerRBRecVal!(), careerWRRecVal!()

    Shared carYear_DF(), carYear_FG(), carYear_KRet()
    Shared carYear_PRet(), carYear_Punt(), carYear_QB()
    Shared carYear_RB(), carYear_WR()

    Shared careerKRS$(), careerPRS$(), careerQBS$()
    Shared careerRBR$(), careerRBS$(), careerWRS$()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    If _FileExists(diskPaths$(2) + careerFile$ + ".CLD") Then

        Call LoadCareerLeaders(recordFile$)

    End If

    '-----

    For I = 0 To 49

        If carRBacks$(I) <> "" And carRBacks$(I) <> "XXX" And carRBacks$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If careerRBS!(J, I, 8) = 0 Then
                    Exit For
                Else

                    If careerRBS!(J, I, 0) > careerRBRecVal!(0) Then
                        careerRBRecVal!(0) = careerRBS!(J, I, 0)
                        careerRBRecPlyr$(0) = carRBacks$(I)
                        carYear_RB(0) = careerRBS!(J, I, 8)
                    End If

                    If careerRBS!(J, I, 1) > careerRBRecVal!(1) Then
                        careerRBRecVal!(1) = careerRBS!(J, I, 1)
                        careerRBRecPlyr$(1) = carRBacks$(I)
                        carYear_RB(1) = careerRBS!(J, I, 8)
                    End If

                    If careerRBS!(J, I, 0) <> 0 Then

                        If careerRBS!(J, I, 1) / careerRBS!(J, I, 0) > careerRBRecVal!(2) And careerRBS!(J, I, 0) >= GAMES * 10 Then
                            careerRBRecVal!(2) = careerRBS!(J, I, 1) / careerRBS!(J, I, 0)
                            careerRBRecPlyr$(2) = carRBacks$(I)
                            carYear_RB(2) = careerRBS!(J, I, 8)
                        End If

                    End If

                    If careerRBS!(J, I, 2) > careerRBRecVal!(3) Then
                        careerRBRecVal!(3) = careerRBS!(J, I, 2)
                        careerRBRecPlyr$(3) = carRBacks$(I)
                        carYear_RB(3) = careerRBS!(J, I, 8)
                    End If

                    If careerRBS!(J, I, 3) > careerRBRecVal!(4) Then
                        careerRBRecVal!(4) = careerRBS!(J, I, 3)
                        careerRBRecPlyr$(4) = carRBacks$(I)
                        carYear_RB(4) = careerRBS!(J, I, 8)
                    End If

                    If careerRBS!(J, I, 4) > careerRBRecVal!(5) Then
                        careerRBRecVal!(5) = careerRBS!(J, I, 4)
                        careerRBRecPlyr$(5) = carRBacks$(I)
                        carYear_RB(5) = careerRBS!(J, I, 8)
                    End If

                    If careerRBS!(J, I, 3) <> 0 Then
                        If careerRBS!(J, I, 4) / careerRBS!(J, I, 3) > careerRBRecVal!(6) And careerRBS!(J, I, 3) >= GAMES * 3 Then
                            careerRBRecVal!(6) = careerRBS!(J, I, 4) / careerRBS!(J, I, 3)
                            careerRBRecPlyr$(6) = carRBacks$(I)
                            carYear_RB(6) = careerRBS!(J, I, 8)
                        End If
                    End If

                    If careerRBS!(J, I, 5) > careerRBRecVal!(7) Then
                        careerRBRecVal!(7) = careerRBS!(J, I, 5)
                        careerRBRecPlyr$(7) = carRBacks$(I)
                        carYear_RB(7) = careerRBS!(J, I, 8)
                    End If

                    If careerRBS!(J, I, 6) > careerRBRecVal!(8) Then
                        careerRBRecVal!(8) = careerRBS!(J, I, 6)
                        careerRBRecPlyr$(8) = carRBacks$(I)
                        carYear_RB(8) = careerRBS!(J, I, 8)
                        careerRBR$(0) = careerRBS$(J, I, 0)
                    End If

                    If careerRBS!(J, I, 9) > careerRBRecVal!(9) Then
                        careerRBRecVal!(9) = careerRBS!(J, I, 9)
                        careerRBRecPlyr$(9) = carRBacks$(I)
                        carYear_RB(9) = careerRBS!(J, I, 8)
                        careerRBR$(1) = careerRBS$(J, I, 1)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 39

        If carWdRec$(I) <> "" And carWdRec$(I) <> "XXX" And carWdRec$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If careerWRS!(J, I, 5) = 0 Then
                    Exit For
                Else

                    If careerWRS!(J, I, 0) > careerWRRecVal!(0) Then
                        careerWRRecVal!(0) = careerWRS!(J, I, 0)
                        careerWRRecPlyr$(0) = carWdRec$(I)
                        carYear_WR(0) = careerWRS!(J, I, 5)
                    End If

                    If careerWRS!(J, I, 1) > careerWRRecVal!(1) Then
                        careerWRRecVal!(1) = careerWRS!(J, I, 1)
                        careerWRRecPlyr$(1) = carWdRec$(I)
                        carYear_WR(1) = careerWRS!(J, I, 5)
                    End If

                    If careerWRS!(J, I, 0) <> 0 Then
                        If careerWRS!(J, I, 1) / careerWRS!(J, I, 0) > careerWRRecVal!(2) And careerWRS!(J, I, 0) >= GAMES * 3 Then
                            careerWRRecVal!(2) = careerWRS!(J, I, 1) / careerWRS!(J, I, 0)
                            careerWRRecPlyr$(2) = carWdRec$(I)
                            carYear_WR(2) = careerWRS!(J, I, 5)
                        End If
                    End If

                    If careerWRS!(J, I, 2) > careerWRRecVal!(3) Then
                        careerWRRecVal!(3) = careerWRS!(J, I, 2)
                        careerWRRecPlyr$(3) = carWdRec$(I)
                        carYear_WR(3) = careerWRS!(J, I, 5)
                    End If

                    If careerWRS!(J, I, 3) > careerWRRecVal!(4) Then
                        careerWRRecVal!(4) = careerWRS!(J, I, 3)
                        careerWRRecPlyr$(4) = carWdRec$(I)
                        carYear_WR(4) = careerWRS!(J, I, 5)
                        careerWRR$ = careerWRS$(J, I, 0)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 19

        If carQBacks$(I) <> "" And carQBacks$(I) <> "XXX" And carQBacks$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS:

                If careerQBS!(J, I, 7) = 0 Then
                    Exit For
                Else
                    If careerQBS!(J, I, 1) > careerQBRecVal!(0) Then
                        careerQBRecVal!(0) = careerQBS!(J, I, 1)
                        careerQBRecPlyr$(0) = carQBacks$(I)
                        carYear_QB(0) = careerQBS!(J, I, 7)
                    End If

                    If careerQBS!(J, I, 0) > careerQBRecVal!(1) Then
                        careerQBRecVal!(1) = careerQBS!(J, I, 0)
                        careerQBRecPlyr$(1) = carQBacks$(I)
                        carYear_QB(1) = careerQBS!(J, I, 7)
                    End If

                    If careerQBS!(J, I, 2) > careerQBRecVal!(2) Then
                        careerQBRecVal!(2) = careerQBS!(J, I, 2)
                        careerQBRecPlyr$(2) = carQBacks$(I)
                        carYear_QB(2) = careerQBS!(J, I, 7)
                    End If

                    If careerQBS!(J, I, 4) > careerQBRecVal!(3) Then
                        careerQBRecVal!(3) = careerQBS!(J, I, 4)
                        careerQBRecPlyr$(3) = carQBacks$(I)
                        carYear_QB(3) = careerQBS!(J, I, 7)
                    End If

                    If careerQBS!(J, I, 3) > careerQBRecVal!(4) Then
                        careerQBRecVal!(4) = careerQBS!(J, I, 3)
                        careerQBRecPlyr$(4) = carQBacks$(I)
                        carYear_QB(4) = careerQBS!(J, I, 7)
                    End If

                    If careerQBS!(J, I, 0) <> 0 Then
                        If careerQBS!(J, I, 1) / careerQBS!(J, I, 0) > careerQBRecVal!(5) And careerQBS!(J, I, 0) >= GAMES * 15 Then
                            careerQBRecVal!(5) = careerQBS!(J, I, 1) / careerQBS!(J, I, 0)
                            careerQBRecPlyr$(5) = carQBacks$(I)
                            carYear_QB(5) = careerQBS!(J, I, 7)
                        End If
                    End If

                    If careerQBS!(J, I, 1) <> 0 Then
                        If careerQBS!(J, I, 3) / careerQBS!(J, I, 1) > careerQBRecVal!(6) And careerQBS!(J, I, 0) >= GAMES * 15 Then
                            careerQBRecVal!(6) = careerQBS!(J, I, 3) / careerQBS!(J, I, 1)
                            careerQBRecPlyr$(6) = carQBacks$(I): carYear_QB(6) = careerQBS!(J, I, 7)
                        End If
                    End If

                    Call QBRating_Career(J, I, TT)

                    If careerQBS!(J, I, 12) > careerQBRecVal!(7) Then
                        careerQBRecVal!(7) = careerQBS!(J, I, 12)
                        careerQBRecPlyr$(7) = carQBacks$(I)
                        carYear_QB(7) = careerQBS!(J, I, 7)
                    End If

                    If careerQBS!(J, I, 5) > careerQBRecVal!(8) Then
                        careerQBRecVal!(8) = careerQBS!(J, I, 5)
                        careerQBRecPlyr$(8) = carQBacks$(I)
                        carYear_QB(8) = careerQBS!(J, I, 7)
                        QBR$ = careerQBS$(J, I, 0)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 14

        If carKickRet$(I) <> "" And carKickRet$(I) <> "XXX" And carKickRet$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If careerKRS!(J, I, 5) = 0 Then Exit For

                If careerKRS!(J, I, 0) > careerKRRecVal!(0) Then
                    careerKRRecVal!(0) = careerKRS!(J, I, 0)
                    careerKRRecPlyr$(0) = carKickRet$(I)
                    carYear_KRet(0) = careerKRS!(J, I, 5)
                End If

                If careerKRS!(J, I, 1) > careerKRRecVal!(1) Then
                    careerKRRecVal!(1) = careerKRS!(J, I, 1)
                    careerKRRecPlyr$(1) = carKickRet$(I)
                    carYear_KRet(1) = careerKRS!(J, I, 5)
                End If

                If careerKRS!(J, I, 0) <> 0 Then
                    If careerKRS!(J, I, 1) / careerKRS!(J, I, 0) > careerKRRecVal!(2) And careerKRS!(J, I, 0) >= GAMES * 1.2 Then
                        careerKRRecVal!(2) = careerKRS!(J, I, 1) / careerKRS!(J, I, 0)
                        careerKRRecPlyr$(2) = carKickRet$(I)
                        carYear_KRet(2) = careerKRS!(J, I, 5)
                    End If
                End If

                If careerKRS!(J, I, 2) > careerKRRecVal!(3) Then
                    careerKRRecVal!(3) = careerKRS!(J, I, 2)
                    careerKRRecPlyr$(3) = carKickRet$(I)
                    carYear_KRet(3) = careerKRS!(J, I, 5)
                End If

                If careerKRS!(J, I, 3) > careerKRRecVal!(4) Then
                    careerKRRecVal!(4) = careerKRS!(J, I, 3)
                    careerKRRecPlyr$(4) = carKickRet$(I)
                    carYear_KRet(4) = careerKRS!(J, I, 5)
                    careerKRR$ = careerKRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 14

        If carPRet$(I) <> "" And carPRet$(I) <> "XXX" And carPRet$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If careerPRS!(J, I, 5) = 0 Then Exit For

                If careerPRS!(J, I, 0) > careerPRRecVal!(0) Then
                    careerPRRecVal!(0) = careerPRS!(J, I, 0)
                    careerPRRecPlyr$(0) = carPRet$(I)
                    carYear_PRet(0) = careerPRS!(J, I, 5)
                End If

                If careerPRS!(J, I, 1) > careerPRRecVal!(1) Then
                    careerPRRecVal!(1) = careerPRS!(J, I, 1)
                    careerPRRecPlyr$(1) = carPRet$(I)
                    carYear_PRet(1) = careerPRS!(J, I, 5)
                End If

                If careerPRS!(J, I, 0) <> 0 Then
                    If careerPRS!(J, I, 1) / careerPRS!(J, I, 0) > careerPRRecVal!(2) And careerPRS!(J, I, 0) >= GAMES * 1.2 Then
                        careerPRRecVal!(2) = careerPRS!(J, I, 1) / careerPRS!(J, I, 0)
                        careerPRRecPlyr$(2) = carPRet$(I)
                        carYear_PRet(2) = careerPRS!(J, I, 5)
                    End If
                End If

                If careerPRS!(J, I, 2) > careerPRRecVal!(3) Then
                    careerPRRecVal!(3) = careerPRS!(J, I, 2)
                    careerPRRecPlyr$(3) = carPRet$(I)
                    carYear_PRet(3) = careerPRS!(J, I, 5)
                End If

                If careerPRS!(J, I, 3) > careerPRRecVal!(4) Then
                    careerPRRecVal!(4) = careerPRS!(J, I, 3)
                    careerPRRecPlyr$(4) = carPRet$(I)
                    carYear_PRet(4) = careerPRS!(J, I, 5)
                    PRR$ = careerPRS$(J, I, 0)
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 9

        If carPunter$(I) <> "" And carPunter$(I) <> "XXX" And carPunter$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If careerPUS!(J, I, 3) = 0 Then Exit For

                If careerPUS!(J, I, 0) > careerPuntRecVal!(0) Then
                    careerPuntRecVal!(0) = careerPUS!(J, I, 0)
                    careerPuntRecPlyr$(0) = carPunter$(I)
                    carYear_Punt(0) = careerPUS!(J, I, 3)
                End If

                If careerPUS!(J, I, 1) > careerPuntRecVal!(1) Then
                    careerPuntRecVal!(1) = careerPUS!(J, I, 1)
                    careerPuntRecPlyr$(1) = carPunter$(I)
                    carYear_Punt(1) = careerPUS!(J, I, 3)
                End If

                If careerPUS!(J, I, 0) <> 0 Then
                    If careerPUS!(J, I, 1) / careerPUS!(J, I, 0) > careerPuntRecVal!(2) Then
                        careerPuntRecVal!(2) = careerPUS!(J, I, 1) / careerPUS!(J, I, 0)
                        careerPuntRecPlyr$(2) = carPunter$(I)
                        carYear_Punt(2) = careerPUS!(J, I, 3)
                    End If
                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 9

        If carKicker$(I) <> "" And carKicker$(I) <> "XXX" And carKicker$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS

                If careerFGS!(J, I, 16) = 0 Then
                    Exit For
                Else

                    If careerFGS!(J, I, 1) > careerFGRecVal!(0) Then
                        careerFGRecVal!(0) = careerFGS!(J, I, 1)
                        careerFGRecPlyr$(0) = carKicker$(I)
                        carYear_FG(0) = careerFGS!(J, I, 16)
                    End If

                    If careerFGS!(J, I, 0) > careerFGRecVal!(1) Then
                        careerFGRecVal!(1) = careerFGS!(J, I, 0)
                        careerFGRecPlyr$(1) = carKicker$(I)
                        carYear_FG(1) = careerFGS!(J, I, 16)
                    End If

                    If careerFGS!(J, I, 1) <> 0 Then
                        If careerFGS!(J, I, 0) / careerFGS!(J, I, 1) > careerFGRecVal!(2) And careerFGS!(J, I, 1) >= GAMES Then
                            careerFGRecVal!(2) = careerFGS!(J, I, 0) / careerFGS!(J, I, 1)
                            careerFGRecPlyr$(2) = carKicker$(I)
                            carYear_FG(2) = careerFGS!(J, I, 16)
                        End If
                    End If

                    If careerFGS!(J, I, 3) > careerFGRecVal!(3) Then
                        careerFGRecVal!(3) = careerFGS!(J, I, 3)
                        careerFGRecPlyr$(3) = carKicker$(I)
                        carYear_FG(3) = careerFGS!(J, I, 16)
                    End If

                    If careerFGS!(J, I, 2) > careerFGRecVal!(4) Then
                        careerFGRecVal!(4) = careerFGS!(J, I, 2)
                        careerFGRecPlyr$(4) = carKicker$(I)
                        carYear_FG(4) = careerFGS!(J, I, 16)
                    End If

                    If careerFGS!(J, I, 2) <> 0 Then
                        If careerFGS!(J, I, 2) / careerFGS!(J, I, 3) > careerFGRecVal!(5) And careerFGS!(J, I, 3) >= GAMES Then
                            careerFGRecVal!(5) = careerFGS!(J, I, 2) / careerFGS!(J, I, 3)
                            careerFGRecPlyr$(5) = carKicker$(I)
                            carYear_FG(5) = careerFGS!(J, I, 16)
                        End If
                    End If

                    If careerFGS!(J, I, 4) > careerFGRecVal!(6) Then
                        careerFGRecVal!(6) = careerFGS!(J, I, 4)
                        careerFGRecPlyr$(6) = carKicker$(I)
                        carYear_FG(6) = careerFGS!(J, I, 16)
                    End If

                End If

            Next J

        End If

    Next I

    '-----

    For I = 0 To 54

        If carDefInts$(I) <> "" And carDefInts$(I) <> "XXX" And carDefInts$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS
                If careerGIS!(J, I, 2) = 0 Then Exit For

                If careerGIS!(J, I, 0) > careerDefRecVal!(0) Then
                    careerDefRecVal!(0) = careerGIS!(J, I, 0)
                    careerDefRecPlyr$(0) = carDefInts$(I)
                    carYear_FG(0) = careerGIS!(J, I, 2)
                End If
            Next J

        End If

    Next I

    '-----

    For I = 0 To 54

        If carDefSacks$(I) <> "" And carDefSacks$(I) <> "XXX" And carDefSacks$(I) <> "END" Then

            For J = 1 To MAX_CAREER_YEARS
                If careerGSS!(J, I, 2) = 0 Then Exit For

                If careerGSS!(J, I, 0) > careerDefRecVal!(1) Then
                    careerDefRecVal!(1) = careerGSS!(J, I, 0)
                    careerDefRecPlyr$(1) = carDefSacks$(I)
                    carYear_FG(1) = careerGSS!(J, I, 2)
                End If

            Next J

        End If

    Next I

    '-----

    Open diskPaths$(2) + careerFile$ + ".CLD" For Output As #2

    For I = 0 To 9:
        Write #2, careerRBRecVal!(I), careerRBRecPlyr$(I), carYear_RB(I)
    Next

    Write #2, careerRBR$(0), careerRBR$(1)

    For I = 0 To 4:
        Write #2, careerWRRecVal!(I), careerWRRecPlyr$(I), carYear_WR(I)
    Next

    Write #2, careerWRR$

    For I = 0 To 8:
        Write #2, careerQBRecVal!(I), careerQBRecPlyr$(I), carYear_QB(I)
    Next

    Write #2, careerQBR$

    For I = 0 To 4:
        Write #2, careerKRRecVal!(I), careerKRRecPlyr$(I), carYear_KRet(I)
    Next

    Write #2, careerKRR$

    For I = 0 To 4:
        Write #2, careerPRRecVal!(I), careerPRRecPlyr$(I), carYear_PRet(I)
    Next

    Write #2, PRR$

    For I = 0 To 2:
        Write #2, careerPuntRecVal!(I), careerPuntRecPlyr$(I), carYear_Punt(I)
    Next

    For I = 0 To 6:
        Write #2, careerFGRecVal!(I), careerFGRecPlyr$(I), carYear_FG(I)
    Next

    For I = 0 To 1:
        Write #2, careerDefRecVal!(I), careerDefRecPlyr$(I), carYear_DF(I)
    Next

    Close #2

End Sub
