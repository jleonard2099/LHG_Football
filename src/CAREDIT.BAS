'----------------------------------------
'           CAREDIT Subroutine
'----------------------------------------
'This routine allows the user to choose a
'career file, then loads in the stats and
'gives the users options for editing and
'saving the file.
Sub CAREDIT ()

    _Title "3-in-1 Football - Edit Career Files"

    Shared TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared intName$(), sackName$()
    
    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Color 15, 0
    Cls

    careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

		Call LoadCareerFile (diskPaths$(1) + careerFile$ + ".CAR")

        Do

            Cls

            Color 7: Print "EDIT ";
            Color 12: Print "R";
            Color 7: Print "UNNING BACKS"

            Color 7: Print "EDIT ";
            Color 12: Print "W";
            Color 7: Print "IDE RECEIVERS"

            Color 7: Print "EDIT ";
            Color 12: Print "Q";
            Color 7: Print "UARTERBACKS"

            Color 7: Print "EDIT ";
            Color 7: Print "KI";
            Color 12: Print "C";
            Color 7: Print"K RETURNERS"

            Color 7: Print "EDIT ";
            Color 7: Print "P";
            Color 12: Print "U";
            Color 7: Print "NT RETURNERS"

            Color 7: Print "EDIT ";
            Color 12: Print "P";
            Color 7: Print "UNTERS"

            Color 7: Print "EDIT ";
            Color 12: Print "K";
            Color 7: Print "ICKERS"

            Color 7: Print "EDIT ";
            Color 12: Print "I";
            Color 7: Print "NTERCEPTIONS"

            Color 7: Print "EDIT ";
            Color 7: Print "DE";
            Color 12:Print "F";
            Color 7: Print "ENSIVE SACKS"

            Color 12: Print "UP";
            Color 12: Print "D";
            Color 7: Print "ATE CAREER FILE"

            Color 12: Print "E";
            Color 7: Print "XIT WITHOUT UPDATING"

            J$ = GetKeyPress$

            Select Case UCase$(J$)

                Case "R"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 16:
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " "; 
                        Color 7: Print Left$(carRBacks$(I), 12)
                    Next

                    For I = 17 To 33:
                        Locate I - 15, 20
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carRBacks$(I), 12)
                    Next

                    For I = 34 To 50:
                        Locate I - 32, 40
                        Color 11: Print Using "##"; I;
                        Print " "; 
                        Color 7: Print Left$(carRBacks$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (51 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 51

                    If B9 <> 51 Then

                        If B9 <> 50 Then

                            Call CareerStatsRB_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls

                                Locate 5, 15: Print "CHANGING RUNNING BACK FILE . . . . . PLEASE WAIT"

                                For I = B9 To 49: 
                                    carRBacks$(I) = carRBacks$(I + 1)
                                Next

                                UPP = (50 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 48:
                                        For I1 = 0 To 9:
                                            careerRBS!(J, I, I1) = careerRBS!(J, I + 1, I1)
                                            careerRBS$(J, I, 0) = careerRBS$(J, I + 1, 0)
                                            careerRBS$(J, I, 1) = careerRBS$(J, I + 1, 1)
                                        Next

                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next
                                Next

                            End If

                        End If

                        If exitFlag% <> 1 Then

                            carRBacks$(49) = ""

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 9:
                                    careerRBS!(J, 49, I1) = 0
                                    careerRBS$(J, 49, 0) = ""
                                    careerRBS$(J, 49, 1) = ""
                                Next
                            Next

                        End If

                    End If

                Case "W"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 19:
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";: 
                        Color 7: Print Left$(carWdRec$(I), 12)
                    Next

                    For I = 20 To 39:
                        Locate I - 18, 20
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carWdRec$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (40 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 40

                    If B9 <> 40 Then

                        If B9 <> 39 Then

                            Call CareerStatsWR_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls
                                Locate 5, 15: Print "CHANGING RECEIVER FILE . . . . . PLEASE WAIT"

                                For I = B9 To 38: 
                                    carWdRec$(I) = carWdRec$(I + 1)
                                Next

                                UPP = (39 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 38:
                                        For I1 = 0 To 5:
                                            careerWRS!(J, I, I1) = careerWRS!(J, I + 1, I1)
                                            careerWRS$(J, I, 0) = careerWRS$(J, I + 1, 0)
                                        Next
                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next
                                Next

                            End If

                        End If

                        If exitFlag% <> 1 Then
                            carWdRec$(39) = ""

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 5:
                                    careerWRS!(J, 39, I1) = 0
                                    careerWRS$(J, 39, 0) = ""
                                Next
                            Next

                        End If

                    End If

                Case "Q"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 9:
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " "; 
                        Color 7: Print Left$(carQBacks$(I), 12)
                    Next

                    For I = 10 To 19:
                        Locate I - 8, 20
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carQBacks$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (20 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 20

                    If B9 <> 20 Then

                        If B9 <> 19 Then
                        
                            Call CareerStatsQB_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls
                                Locate 5, 15: Print "CHANGING QUARTERBACK FILE . . . . . PLEASE WAIT"

                                For I = B9 To 18: 
                                    carQBacks$(I) = carQBacks$(I + 1)
                                Next

                                UPP = (19 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 18:
                                        For I1 = 0 To 7:
                                            careerQBS!(J, I, I1) = careerQBS!(J, I + 1, I1)
                                            careerQBS$(J, I, 0) = careerQBS$(J, I + 1, 0)
                                        Next

                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next
                                Next

                            End If

                        End If

                    End If

                    If exitFlag% <> 1 Then

                        carQBacks$(19) = ""

                        For J = 1 To MAX_CAREER_YEARS:
                            For I1 = 0 To 7:
                                careerQBS!(J, 19, I1) = 0
                                careerQBS$(J, 19, 0) = ""
                            Next
                        Next

                    End If

                Case "C"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 14: 
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carKickRet$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (15 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 15

                    If B9 <> 15 Then

                        If B9 <> 14 Then

                            Call CareerStatsKR_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls
                                Locate 5, 15: Print "CHANGING KICK RETURNER FILE . . . . . PLEASE WAIT"

                                For I = B9 To 13: 
                                    carKickRet$(I) = carKickRet$(I + 1)
                                Next

                                UPP = (14 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 13:
                                        For I1 = 0 To 5:
                                            careerKRS!(J, I, I1) = careerKRS!(J, I + 1, I1)
                                            careerKRS$(J, I, 0) = careerKRS$(J, I + 1, 0)
                                        Next
                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next
                                Next
                            End If

                        End If

                        If exitFlag% <> 1 Then

                            carKickRet$(14) = ""

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 5:
                                    careerKRS!(J, 14, I1) = 0
                                    careerKRS$(J, 14, 0) = ""
                                Next
                            Next
                        End If

                    End If

                Case "U"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 14: 
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carPRet$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (15 TO EXIT)"; B9
                    Loop Until B9 <=0 And B9 <= 15

                    If B9 <> 15 Then

                        If B9 <> 14 Then

                            Call CareerStatsPR_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls

                                Locate 5, 15: Print "CHANGING PUNT RETURNER FILE . . . . . PLEASE WAIT"

                                For I = B9 To 13: 
                                    carPRet$(I) = carPRet$(I + 1)
                                Next

                                UPP = (14 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 13:
                                        For I1 = 0 To 5:
                                            careerPRS!(J, I, I1) = careerPRS!(J, I + 1, I1)
                                            careerPRS$(J, I, 0) = careerPRS$(J, I + 1, 0)
                                        Next
                                        Locate 7, 36: Color 12: UPP = UPP - 1: Print Using "####"; UPP
                                    Next
                                Next
                            End If

                        End If

                        If exitFlag% <> 1 Then

                            carPRet$(14) = ""

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 5:
                                    careerPRS!(J, 14, I1) = 0
                                    careerPRS$(J, 14, 0) = ""
                                Next
                            Next

                        End If

                    End If

                Case "P"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 9:
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carPunter$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (10 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 10

                    If B9 <> 10 Then

                        If B9 <> 9 Then

                            Call CareerStatsP_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls

                                Locate 5, 15: Print "CHANGING PUNTER FILE . . . . . PLEASE WAIT"

                                For I = B9 To 8: 
                                    carPunter$(I) = carPunter$(I + 1)
                                Next

                                UPP = (9 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 8:
                                        For I1 = 0 To 3:
                                            careerPUS!(J, I, I1) = careerPUS!(J, I + 1, I1)
                                        Next

                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next

                                Next

                            End If

                        End If

                        If exitFlag% <> 1 Then

                            carPunter$(9) = ""

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 3:
                                    careerPUS!(J, 9, I1) = 0
                                Next
                            Next

                        End If

                    End If

                Case "K"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 9:
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(carKicker$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15:
                        Input "ENTER NUMBER OF PLAYER TO DELETE (10 TO EXIT)"; B9
                    Loop Until B9 >= 0 Or B9 <= 10

                    If B9 <> 10 Then

                        If B9 <> 9 Then

                            Call CareerStatsK_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls
                                Locate 5, 15: Print "CHANGING FIELD GOAL KICKER FILE . . . . . PLEASE WAIT"

                                For I = B9 To 8: 
                                    carKicker$(I) = carKicker$(I + 1)
                                Next

                                UPP = (9 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 8:
                                        For I1 = 0 To 16:
                                            careerFGS!(J, I, I1) = careerFGS!(J, I + 1, I1)
                                        Next

                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next
                                Next

                            End If

                        End If

                        If exitFlag% <> 1 Then

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 16:
                                    careerFGS!(J, 9, I1) = 0
                                Next
                            Next

                        End If

                    End If

                Case "I"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 17:
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(intName$(I), 12)
                    Next

                    For I = 18 To 35:
                        Locate I - 16, 20
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(intName$(I), 12)
                    Next

                    For I = 36 To 54:
                        Locate I - 34, 40
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(intName$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (55 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 55

                    If B9 <> 55 Then

                        If B9 <> 54 Then

                            Call CareerStatsINT_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then
                                Cls
                                Locate 5, 15: Print "CHANGING INTERCEPTION FILE . . . . . PLEASE WAIT"

                                For I = B9 To 8: 
                                    intName$(I) = intName$(I + 1)
                                Next

                                UPP = (54 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:
                                    For I = B9 To 53:
                                        For I1 = 0 To 2:
                                            careerGIS!(J, I, I1) = careerGIS!(J, I + 1, I1)
                                        Next

                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP
                                    Next
                                Next

                            End If

                        End If

                        If exitFlag% <> 1 Then

                            intName$(54) = ""

                            For J = 1 To MAX_CAREER_YEARS:
                                For I1 = 0 To 2:
                                    careerGIS!(J, 54, I1) = 0
                                Next
                            Next

                        End If

                    End If

                Case "F"

                    Cls

                    Color 12: Print careerFile$

                    For I = 0 To 17: 
                        Locate I + 2
                        Color 11: Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(sackName$(I), 12)
                    Next

                    For I = 18 To 35: 
                        Locate I - 16, 20: Color 11
                        Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(sackName$(I), 12)
                    Next

                    For I = 36 To 54: 
                        Locate I - 34, 40: Color 11
                        Print Using "##"; I;
                        Print " ";
                        Color 7: Print Left$(sackName$(I), 12)
                    Next

                    Do
                        Locate 22: Color 15
                        Input "ENTER NUMBER OF PLAYER TO DELETE (55 TO EXIT)"; B9
                    Loop Until B9 >= 0 And B9 <= 55

                    If B9 <> 55 Then

                        If B9 <> 54 Then

                            Call CareerStatsSACK_Report(exitFlag%, B9, careerFile$)

                            If exitFlag% <> 1 Then

                                Cls
                                Locate 5, 15: Print "CHANGING INTERCEPTION FILE . . . . . PLEASE WAIT"

                                For I = B9 To 8: 
                                    sackName$(I) = sackName$(I + 1)
                                Next

                                UPP = (54 - B9) * 15
                                I = B9

                                For J = 1 To MAX_CAREER_YEARS:

                                    For I = B9 To 53:

                                        For I1 = 0 To 2:
                                            careerGSS!(J, I, I1) = careerGSS!(J, I + 1, I1)
                                        Next

                                        UPP = UPP - 1
                                        Locate 7, 36: Color 12: Print Using "####"; UPP

                                    Next

                                Next

                            End If

                        End If

                        If exitFlag% <> 1 Then

                            sackName$(54) = ""

                            For J = 1 To MAX_CAREER_YEARS: 
                                For I1 = 0 To 2: 
                                    careerGSS!(J, 54, I1) = 0
                                Next
                            Next

                        End If

                    End If

                Case "D"
                    Call SaveCareerFile(diskPaths$(1) + careerFile$ + ".CAR")

                Case "E","X"
                    '

            End Select

        Loop Until UCase$(J$) = "E" Or UCase$(J$) = "X"
    
    End If 'Done checking for ESC
            
End Sub


'----------------------------------------
'   CareerStatsRB_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's RB's
Sub CareerStatsRB_Report (exitFlag%, B9, careerFile$)

    Shared carRBacks$(), careerRBS$(), careerRecPlyr$()
    Shared careerRBS!(), careerRecVal!()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print carRBacks$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carRBacks$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "RUSHER         YEAR   ATT   YARDS AVERAGE  TDS   REC  YDS  AVG  TDS RUN-LONG-REC"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS: 
                    VV = VV + careerRBS!(I, B9, 7)
                Next

                'Name
                Print #1, Left$(carRBacks$(B9), 13);

                For V = 1 To VV

                    'Year
                    Print #1, Tab(15); careerRBS!(V, B9, 8);
                    'Att
                    Print #1, Tab(23); Using "###"; careerRBS!(V, B9, 0);
                    'Yards
                    Print #1, Tab(30); Using "####"; careerRBS!(V, B9, 1);

                    'Average
                    If careerRBS!(V, B9, 1) = 0 Or careerRBS!(V, B9, 0) = 0 Then 
                        Print #1, Tab(37); " 0.0";
                    End If
                    If careerRBS!(V, B9, 0) > 0 Then 
                        Print #1, Tab(37); Using "##.#"; careerRBS!(V, B9, 1) / careerRBS!(V, B9, 0);
                    End If

                    'TDs
                    Print #1, Tab(44); Using "###"; careerRBS!(V, B9, 2);
                    'Rec
                    Print #1, Tab(50); Using "###"; careerRBS!(V, B9, 3);
                    'Rec Yds
                    Print #1, Tab(54); Using "####"; careerRBS!(V, B9, 4);

                    'Rec Avg
                    If careerRBS!(V, B9, 4) = 0 Or careerRBS!(V, B9, 3) = 0 Then 
                        Print #1, Tab(59); " 0.0";
                    End If
                    If careerRBS!(V, B9, 3) > 0 Then 
                        Print #1, Tab(59); Using "##.#"; careerRBS!(V, B9, 4) / careerRBS!(V, B9, 3);
                    End If

                    'Rec TDs
                    Print #1, Tab(65); Using "##"; careerRBS!(V, B9, 5);
                    'Longest Run
                    Print #1, Tab(71); Using "##"; careerRBS!(V, B9, 6);: Print #1, careerRBS$(V, B9, 0);
                    'Longest Rec
                    Print #1, Tab(77); Using "##"; careerRBS!(V, B9, 9);: Print #1, careerRBS$(V, B9, 1)

                Next V

                careerRecVal!(0) = 0
                careerRecVal!(1) = 0
                careerRecPlyr$(0) = ""
                careerRecPlyr$(1) = ""

                For V = 1 To VV

                    TT1 = TT1 + careerRBS!(V, B9, 0)
                    TT2 = TT2 + careerRBS!(V, B9, 1)
                    TT3 = TT3 + careerRBS!(V, B9, 2)
                    TT4 = TT4 + careerRBS!(V, B9, 3)
                    TT5 = TT5 + careerRBS!(V, B9, 4)
                    TT6 = TT6 + careerRBS!(V, B9, 5)

                    If careerRBS!(V, B9, 6) > careerRecVal!(0) Then
                        careerRecVal!(0) = careerRBS!(V, B9, 6)
                        careerRecPlyr$(0) = careerRBS$(V, B9, 0)
                    Else
                        If careerRBS!(V, B9, 9) > careerRecVal!(1) Then
                            careerRecVal!(1) = careerRBS!(V, B9, 9)
                            careerRecPlyr$(1) = careerRBS$(V, B9, 1)
                        End If
                    End If

                Next V

                Print #1,
                Print #1, "TOTALS";
                Print #1, Tab(18); Using "##"; VV;
                Print #1, Tab(22); Using "####"; TT1;
                Print #1, Tab(29); Using "#####"; TT2;

                If TT1 = 0 Or TT2 = 0 Then 
                    Print #1, Tab(37); " 0.0";
                End If
                If TT1 > 0 Then 
                    Print #1, Tab(37); Using "##.#"; TT2 / TT1;
                End If

                Print #1, Tab(44); Using "###"; TT3;
                Print #1, Tab(49); Using "####"; TT4;
                Print #1, Tab(53); Using "#####"; TT5;

                If TT4 = 0 Or TT5 = 0 Then 
                    Print #1, Tab(59); " 0.0";
                End If
                If TT4 > 0 Then 
                    Print #1, Tab(59); Using "##.#"; TT5 / TT4;
                End If

                Print #1, Tab(64); Using "###"; TT6;
                Print #1, Tab(71); Using "##"; careerRecVal!(0);: Print #1, careerRecPlyr$(0);
                Print #1, Tab(77); Using "##"; careerRecVal!(1);: Print #1, careerRecPlyr$(1)

                TT1 = 0
                TT2 = 0
                TT3 = 0
                TT4 = 0
                TT5 = 0
                TT6 = 0

                LPrint Chr$(12)

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsWR_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's WR's
Sub CareerStatsWR_Report (exitFlag%, B9, careerFile$)

    Shared carWdRec$(), careerWRS$(), careerRecPlyr$()
    Shared careerWRS!(), careerRecVal!()

    Cls
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print carWdRec$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"
    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carWdRec$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "RECEIVER        YEAR        REC.      YARDS      AVG.     TD  LONGEST REC."

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerWRS!(I, B9, 4)
                Next

                Print #1, Left$(carWdRec$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(16); careerWRS!(V, B9, 5);
                    Print #1, Tab(29); "###"; careerWRS!(V, B9, 0);
                    Print #1, Tab(40); "####"; careerWRS!(V, B9, 1);
                    If careerWRS!(V, B9, 0) = 0 Or careerWRS!(V, B9, 1) = 0 Then 
                        Print #1, Tab(49); " 0.0";
                    End If
                    If careerWRS!(V, B9, 0) > 0 Then 
                        Print #1, Tab(49); "##.#"; careerWRS!(V, B9, 1) / careerWRS!(V, B9, 0);
                    End If
                    Print #1, Tab(59); "##"; careerWRS!(V, B9, 2);
                    Print #1, Tab(67); "##"; careerWRS!(V, B9, 3);
                    Print #1, careerWRS$(V, B9, 0)
                Next V

                careerRecVal!(0) = 0
                careerRecPlyr$(0) = ""

                For V = 1 To VV
                    TT7 = TT7 + careerWRS!(V, B9, 0)
                    TT8 = TT8 + careerWRS!(V, B9, 1)
                    TT9 = TT9 + careerWRS!(V, B9, 2)

                    If careerWRS!(V, B9, 3) > careerRecVal!(0) Then
                        careerRecVal!(0) = careerWRS!(V, B9, 3)
                        careerRecPlyr$(0) = careerWRS$(V, B9, 0)
                    End If

                Next V

                Print #1, "TOTALS";
                Print #1, Tab(19); "##"; VV;
                Print #1, Tab(28); "####"; TT7;
                Print #1, Tab(39); "#####"; TT8;

                If TT7 = 0 Or TT8 = 0 Then 
                    Print #1, Tab(49); " 0.0";
                End If
                If TT7 > 0 Then 
                    Print #1, Tab(49); "##.#"; TT8 / TT7;
                End if

                Print #1, Tab(58); "###"; TT9;
                Print #1, Tab(67); "##"; careerRecVal!(0);
                Print #1, careerRecPlyr$(0)

                TT7 = 0
                TT8 = 0
                TT9 = 0

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsQB_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's QB's
Sub CareerStatsQB_Report (exitFlag%, B9, careerFile$)

    Shared carQBacks$(), careerQBS$(), careerRecPlyr$()
    Shared careerQBS!(), careerRecVal!()

    Dim careerII(15, 30), careerPP(15, 30), careerYY(15, 30)
    'Shared careerFF!(), careerII(), careerPP(), careerRecVal!(), careerTeamTotal!(), careerYY()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print carQBacks$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carQBacks$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "QUARTERBACK  YEAR  COMP.    ATT.    INT  TDS   YARDS  PCT. YD/COMP  RATING  LONG.PASS"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerQBS!(I, B9, 6)
                Next

                Print #1, Left$(carQBacks$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(13); careerQBS!(V, B9, 7);
                    Print #1, Tab(21); "###"; careerQBS!(V, B9, 1);
                    Print #1, Tab(29); "###"; careerQBS!(V, B9, 0);
                    Print #1, Tab(38); "##"; careerQBS!(V, B9, 2);
                    Print #1, Tab(43); "##"; careerQBS!(V, B9, 4);
                    Print #1, Tab(49); "####"; careerQBS!(V, B9, 3);

                    If careerQBS!(V, B9, 1) = 0 Or careerQBS!(V, B9, 0) = 0 Then 
                        Print #1, Tab(56); " 00";
                    End If
                    If careerQBS!(V, B9, 1) > 0 Then 
                        Print #1, Tab(56); "##"; (careerQBS!(V, B9, 1) / careerQBS!(V, B9, 0)) * 100;
                    End If
                    If careerQBS!(V, B9, 0) = 0 Then 
                        Print #1, Tab(63); " 0.0";
                    End If
                    If careerQBS!(V, B9, 1) > 0 Then 
                        Print #1, Tab(63); "##.#"; careerQBS!(V, B9, 3) / careerQBS!(V, B9, 1);
                    End If

                    If careerQBS!(V, B9, 0) > 0 Then 
                        careerQBS!(V, B9, 11) = Int(careerQBS!(V, B9, 1) / careerQBS!(V, B9, 0) * 1000) / 10
                    End If

                    If careerQBS!(V, B9, 0) = 0 Then
                        careerQBS!(V, B9, 11) = 0
                        careerQBS!(V, B9, 8) = 0
                        careerQBS!(V, B9, 10) = 0
                        careerQBS!(V, B9, 9) = 0
                    Else
                        careerQBS!(V, B9, 8) = Int(careerQBS!(V, B9, 4) / careerQBS!(V, B9, 0) * 1000) / 10
                        careerQBS!(V, B9, 10) = Int(careerQBS!(V, B9, 3) / careerQBS!(V, B9, 0) * 100) / 100
                        careerQBS!(V, B9, 9) = Int(careerQBS!(V, B9, 2) / careerQBS!(V, B9, 0) * 1000) / 10
                    End If

                    careerPP(V, B9) = careerQBS!(V, B9, 11) - 30
                    If careerPP(V, B9) < 0 Then
                        careerPP(V, B9) = 0
                    Else
                        If careerPP(V, B9) > 77.5 Then
                            careerPP(V, B9) = 2.375
                        Else
                            careerPP(V, B9) = careerPP(V, B9) * .05
                        End If
                    End If

                    If careerQBS!(V, B9, 8) > 11.9 Then
                        careerFF!(V, B9) = 2.375
                    Else
                        careerFF!(V, B9) = careerQBS!(V, B9, 8) * .2
                    End If

                    If careerQBS!(V, B9, 9) = 0 Then
                        careerII(V, B9) = 0
                    Else
                        If careerQBS!(V, B9, 9) > 9.5 Then
                            careerII(V, B9) = 0
                        Else
                            careerII(V, B9) = 2.375 - (.25 * careerQBS!(V, B9, 9))
                        End If
                    End If

                    careerQBS!(V, B9, 10) = careerQBS!(V, B9, 10) - 3

                    If careerQBS!(V, B9, 10) < 0 Then
                        careerYY(V, B9) = 0
                    Else
                        If careerQBS!(V, B9, 10) > 12.5 Then
                            careerYY(V, B9) = 2.375
                        Else
                            D = Int(careerQBS!(V, B9, 10) * 100) / 2
                            If D = careerQBS!(V, B9, 10) / 2 Then
                                careerYY(V, B9) = careerQBS!(V, B9, 10) / 2 * .3 + careerQBS!(V, B9, 10) / 2 * .2
                            Else
                                careerYY(V, B9) = careerQBS!(V, B9, 10) / 2 * .3 + careerQBS!(V, B9, 10) / 2 * .2 + .003
                            End If
                        End If
                    End If

                    careerQBS!(V, B9, 12) = careerPP(V, B9) + careerFF!(V, B9) + careerII(V, B9) + careerYY(V, B9)

                    If careerQBS!(V, B9, 12) > 9.5 Then
                        careerQBS!(V, B9, 12) = 158.3
                    Else
                        careerQBS!(V, B9, 12) = careerQBS!(V, B9, 12) * 50 / 3
                    End If

                    careerQBS!(V, B9, 12) = Int(careerQBS!(V, B9, 12) * 10) / 10

                    Print #1, Tab(70); "###.#"; careerQBS!(V, B9, 12);
                    Print #1, Tab(80); "##"; careerQBS!(V, B9, 5);: Print #1, careerQBS$(V, B9, 0)

                Next V

                careerRecVal!(0) = 0
                careerRecPlyr$(0) = ""

                For V = 1 To VV
                    TT10 = TT10 + careerQBS!(V, B9, 1)
                    TT11 = TT11 + careerQBS!(V, B9, 0)
                    TT12 = TT12 + careerQBS!(V, B9, 2)
                    TT13 = TT13 + careerQBS!(V, B9, 4)
                    TT14 = TT14 + careerQBS!(V, B9, 3)

                    If careerQBS!(V, B9, 5) > careerRecVal!(O) Then
                        careerRecVal!(0) = careerQBS!(V, B9, 5)
                        careerRecPlyr$(0) = careerQBS$(V, B9, 0)
                    End If

                Next V

                Print #1, "TOTALS";
                Print #1, Tab(16); "##"; VV;
                Print #1, Tab(20); "####"; TT10;
                Print #1, Tab(28); "####"; TT11;
                Print #1, Tab(37); "###"; TT12;
                Print #1, Tab(41); "####"; TT13;
                Print #1, Tab(48); "#####"; TT14;

                If TT10 = 0 Or TT11 = 0 Then 
                    Print #1, Tab(56); " 00";
                End If
                If TT10 > 0 Then 
                    Print #1, Tab(56); "##"; (TT10 / TT11) * 100;
                End If
                If TT11 = 0 Then 
                    Print #1, Tab(63); " 0.0";
                End If
                If TT10 > 0 Then 
                    Print #1, Tab(63); "##.#"; TT14 / TT10;
                End If

                If TT11 > 0 Then 
                    TT30 = Int(TT10 / TT11 * 1000) / 10
                End If
                If TT11 = 0 Then
                    TT30 = 0
                    TT27 = 0
                    TT29 = 0
                    TT28 = 0
                Else
                    TT27 = Int(TT13 / TT11 * 1000) / 10
                    TT29 = Int(TT14 / TT11 * 100) / 100
                    TT28 = Int(TT12 / TT11 * 1000) / 10
                End If

                PPT = TT30 - 30

                If PPT < 0 Then
                    PPT = 0
                Else
                    If PPT > 77.5 Then
                        PPT = 2.375
                    Else
                        PPT = PPT * .05
                    End If
                End If

                If TT27 > 11.9 Then
                    FFT = 2.375
                Else
                    FFT = TT27 * .2
                End If

                If TT28 = 0 Then
                    IIT = 0
                Else
                    If TT28 > 9.5 Then
                        IIT = 0
                    Else
                        IIT = 2.375 - (.25 * TT28)
                    End If
                End If

                TT29 = TT29 - 3

                If TT29 < 0 Then
                    YYT = 0
                Else
                    If TT29 > 12.5 Then
                        YYT = 2.375
                    Else
                        D = Int(TT29 * 100) / 2
                        If D = TT29 / 2 Then
                            YYT = TT29 / 2 * .3 + TT29 / 2 * .2
                        Else
                            YYT = TT29 / 2 * .3 + TT29 / 2 * .2 + .003
                        End If
                    End If
                End If

                TT31 = PPT + FFT + IIT + YYT

                If TT31 > 9.5 Then
                    TT31 = 158.3
                Else
                    TT31 = TT31 * 50 / 3
                End If

                TT31 = Int(TT31 * 10) / 10

                Print #1, Tab(70); "###.#"; TT31;
                Print #1, Tab(80); "##"; careerRecVal!(0);
                Print #1, careerRecPlyr$(0)
                TT10 = 0
                TT11 = 0
                TT12 = 0
                TT13 = 0
                TT14 = 0

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsKR_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's KR's
Sub CareerStatsKR_Report (exitFlag%, B9, careerFile$)

    Shared carKickRet$(), careerKRS$(), careerRecPlyr$()
    Shared careerKRS!(), careerRecVal!()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print carKickRet$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carKickRet$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerKRS!(I, B9, 4)
                Next

                Print #1, Left$(carKickRet$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(22); careerKRS!(V, B9, 5);
                    Print #1, Tab(32); "###"; careerKRS!(V, B9, 0);
                    Print #1, Tab(42); "####"; careerKRS!(V, B9, 1);

                    If careerKRS!(V, B9, 0) = 0 Or careerKRS!(V, B9, 1) = 0 Then 
                        Print #1, Tab(52); " 00";
                    End If
                    If careerKRS!(V, B9, 0) > 0 Then 
                        Print #1, Tab(52); "##.#"; careerKRS!(V, B9, 1) / careerKRS!(V, B9, 0);
                    End If

                    Print #1, Tab(62); "##"; careerKRS!(V, B9, 2);
                    Print #1, Tab(68); "##"; careerKRS!(V, B9, 3);
                    Print #1, careerKRS$(V, B9, 0)
                Next V

                careerRecVal!(0) = 0
                careerRecPlyr$(0) = ""

                For V = 1 To VV
                    TT15 = TT15 + careerKRS!(V, B9, 0)
                    TT16 = TT16 + careerKRS!(V, B9, 1)
                    TT17 = TT17 + careerKRS!(V, B9, 2)

                    If careerKRS!(V, B9, 3) > careerRecVal!(0) Then
                        careerRecVal!(0) = careerKRS!(V, B9, 3)
                        careerRecPlyr$(0) = careerKRS$(V, B9, 0)
                    End If

                Next V

                Print #1, "TOTALS";
                Print #1, Tab(25); "##"; VV;
                Print #1, Tab(32); "###"; TT15;
                Print #1, Tab(41); "#####"; TT16;

                If TT15 = 0 Or TT16 = 0 Then 
                    Print #1, Tab(52); " 00";
                End If
                If TT15 > 0 Then 
                    Print #1, Tab(52); "##.#"; TT16 / TT15;
                End If

                Print #1, Tab(61); "###"; TT17;
                Print #1, Tab(68); "##"; careerRecVal!(0);: Print #1, careerRecPlyr$(0)

                TT15 = 0
                TT16 = 0
                TT17 = 0

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsPR_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's PR's
Sub CareerStatsPR_Report (exitFlag%, B9, careerFile$)

    Shared carPRet$(), careerPRS$(), careerRecPlyr$()
    Shared careerPRS!(), careerRecVal!()

    Cls

    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print carPRet$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carPRet$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerPRS!(I, B9, 4)
                Next

                Print #1, Left$(carPRet$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(22); careerPRS!(V, B9, 5);
                    Print #1, Tab(32); "###"; careerPRS!(V, B9, 0);
                    Print #1, Tab(42); "####"; careerPRS!(V, B9, 1);
                    If careerPRS!(V, B9, 0) = 0 Or careerPRS!(V, B9, 1) = 0 Then 
                        Print #1, Tab(52); " 00";
                    End If
                    If careerPRS!(V, B9, 0) > 0 Then 
                        Print #1, Tab(52); "##.#"; careerPRS!(V, B9, 1) / careerPRS!(V, B9, 0);
                    End If
                    Print #1, Tab(62); "##"; careerPRS!(V, B9, 2);
                    Print #1, Tab(68); "##"; careerPRS!(V, B9, 3);
                    Print #1, careerPRS$(V, B9, 0)
                Next V

                careerRecVal!(0) = 0
                careerRecPlyr$(0) = ""

                For V = 1 To VV
                    TT18 = TT18 + careerPRS!(V, B9, 0)
                    TT19 = TT19 + careerPRS!(V, B9, 1)
                    TT20 = TT20 + careerPRS!(V, B9, 2)
                    If careerPRS!(V, B9, 3) > careerRecVal!(0) Then
                        careerRecVal!(0) = careerPRS!(V, B9, 3): careerRecPlyr$(0) = careerPRS$(V, B9, 0)
                    End If
                Next V

                Print #1, "TOTALS";
                Print #1, Tab(25); "##"; VV;
                Print #1, Tab(32); "###"; TT18;
                Print #1, Tab(41); "#####"; TT19;

                If TT18 = 0 Or TT19 = 0 Then 
                    Print #1, Tab(52); " 00";
                End If
                If TT18 > 0 Then 
                    Print #1, Tab(52); "##.#"; TT19 / TT18;
                End If

                Print #1, Tab(61); "###"; TT20;
                Print #1, Tab(68); "##"; careerRecVal!(0);
                Print #1, careerRecPlyr$(0)
                TT18 = 0
                TT19 = 0
                TT20 = 0

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsP_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's Punter
Sub CareerStatsP_Report (exitFlag%, B9, careerFile$)

    Shared carPunter$(), careerRecPlyr$()
    Shared careerPUS!(), careerRecVal!()

    Cls

    Locate 5, 20: Color 15

    Print "PRINT CAREER STATS OF ";
    Color 12: Print carPunter$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"
    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carPunter$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "PUNTERS           YEAR     ATT.     YARDS      AVG."

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerPUS!(I, B9, 2)
                Next

                Print #1, Left$(carPunter$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(18); careerPUS!(V, B9, 3);
                    Print #1, Tab(28); "###"; careerPUS!(V, B9, 0);
                    Print #1, Tab(38); "####"; careerPUS!(V, B9, 1);
                    If careerPUS!(V, B9, 0) = 0 Or careerPUS!(V, B9, 1) = 0 Then 
                        Print #1, Tab(47); "00.0"
                    End If
                    If careerPUS!(V, B9, 0) > 0 Then 
                        Print #1, Tab(47); "##.#"; careerPUS!(V, B9, 1) / careerPUS!(V, B9, 0)
                    End If
                Next V

                For V = 1 To VV
                    TT21 = TT21 + careerPUS!(V, B9, 0)
                    TT22 = TT22 + careerPUS!(V, B9, 1)
                Next V

                Print #1, "TOTALS";
                Print #1, Tab(21); "##"; VV;
                Print #1, Tab(27); "####"; TT21;
                Print #1, Tab(37); "#####"; TT22;
                If TT21 = 0 Or TT22 = 0 Then 
                    Print #1, Tab(47); " 00"
                End If
                If TT21 > 0 Then 
                    Print #1, Tab(47); "##.#"; TT22 / TT21
                End If
                TT21 = 0
                TT22 = 0

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsK_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's Kickers
Sub CareerStatsK_Report (exitFlag%, B9, careerFile$)

    Shared carKicker$(), careerRecPlyr$()
    Shared careerFGS!(), careerRecVal!(), careerTeamTotal!()

    Cls
    
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print carKicker$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print carKicker$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Locate 2: Print #1, "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50+"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerFGS!(I, B9, 15)
                Next

                Print #1, Left$(carKicker$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(13); careerFGS!(V, B9, 16);
                    Print #1, Tab(19); "###"; careerFGS!(V, B9, 3);
                    Print #1, Tab(23); "###"; careerFGS!(V, B9, 2);

                    If careerFGS!(V, B9, 2) = 0 Or careerFGS!(V, B9, 3) = 0 Then 
                        Print #1, Tab(27); "  0";
                    End If
                    If careerFGS!(V, B9, 2) > 0 Then 
                        Print #1, Tab(27); "###"; (careerFGS!(V, B9, 2) / careerFGS!(V, B9, 3)) * 100;
                    End If

                    Print #1, Tab(32); "###"; careerFGS!(V, B9, 1);
                    Print #1, Tab(36); "###"; careerFGS!(V, B9, 0);

                    If careerFGS!(V, B9, 0) = 0 Or careerFGS!(V, B9, 1) = 0 Then 
                        Print #1, Tab(40); "  0";
                    End If
                    If careerFGS!(V, B9, 0) > 0 Then 
                        Print #1, Tab(40); "###"; (careerFGS!(V, B9, 0) / careerFGS!(V, B9, 1)) * 100;
                    End If

                    Print #1, Tab(46); "##"; careerFGS!(V, B9, 4);
                    Print #1, Tab(49); "##"; careerFGS!(V, B9, 5);: Print #1, "-";
                    Print #1, Tab(52); "##"; careerFGS!(V, B9, 10);
                    Print #1, Tab(55); "##"; careerFGS!(V, B9, 6);: Print #1, "-";
                    Print #1, Tab(58); "##"; careerFGS!(V, B9, 11);
                    Print #1, Tab(61); "##"; careerFGS!(V, B9, 7);: Print #1, "-";
                    Print #1, Tab(64); "##"; careerFGS!(V, B9, 12);
                    Print #1, Tab(67); "##"; careerFGS!(V, B9, 8);: Print #1, "-";
                    Print #1, Tab(70); "##"; careerFGS!(V, B9, 13);
                    Print #1, Tab(73); "##"; careerFGS!(V, B9, 9);: Print #1, "-";
                    Print #1, Tab(76); "##"; careerFGS!(V, B9, 14)
                Next V

                For V = 1 To VV
                    careerTeamTotal!(23) = careerTeamTotal!(23) + careerFGS!(V, B9, 0)
                    careerTeamTotal!(24) = careerTeamTotal!(24) + careerFGS!(V, B9, 1)
                    careerTeamTotal!(34) = careerTeamTotal!(34) + careerFGS!(V, B9, 2)
                    careerTeamTotal!(35) = careerTeamTotal!(35) + careerFGS!(V, B9, 3)
                    careerTeamTotal!(36) = careerTeamTotal!(36) + careerFGS!(V, B9, 5)
                    careerTeamTotal!(37) = careerTeamTotal!(37) + careerFGS!(V, B9, 6)
                    careerTeamTotal!(38) = careerTeamTotal!(38) + careerFGS!(V, B9, 7)
                    careerTeamTotal!(39) = careerTeamTotal!(39) + careerFGS!(V, B9, 8)
                    careerTeamTotal!(40) = careerTeamTotal!(40) + careerFGS!(V, B9, 9)
                    careerTeamTotal!(41) = careerTeamTotal!(41) + careerFGS!(V, B9, 10)
                    careerTeamTotal!(42) = careerTeamTotal!(42) + careerFGS!(V, B9, 11)
                    careerTeamTotal!(43) = careerTeamTotal!(43) + careerFGS!(V, B9, 12)
                    careerTeamTotal!(44) = careerTeamTotal!(44) + careerFGS!(V, B9, 13)
                    careerTeamTotal!(45) = careerTeamTotal!(45) + careerFGS!(V, B9, 14)
                Next V

                careerRecVal!(0) = 0

                For V = 1 To VV
                    If careerFGS!(V, B9, 4) >= careerRecVal!(0) Then
                        careerRecVal!(0) = careerFGS!(V, B9, 4)
                    End If
                Next

                Print #1, "TOTALS";
                Print #1, Tab(16); VV;
                Print #1, Tab(19); "###"; careerTeamTotal!(35);
                Print #1, Tab(23); "###"; careerTeamTotal!(34);

                If careerTeamTotal!(34) = 0 Or careerTeamTotal!(35) = 0 Then 
                    Print #1, Tab(27); "  0";
                End If
                If careerTeamTotal!(34) > 0 Then 
                    Print #1, Tab(27); "###"; (careerTeamTotal!(34) / careerTeamTotal!(35)) * 100;
                End If

                Print #1, Tab(32); "###"; careerTeamTotal!(24);
                Print #1, Tab(36); "###"; careerTeamTotal!(23);

                If careerTeamTotal!(23) = 0 Or careerTeamTotal!(24) = 0 Then 
                    Print #1, Tab(40); "  0";
                End If
                If careerTeamTotal!(23) > 0 Then 
                    Print #1, Tab(40); "###"; (careerTeamTotal!(23) / careerTeamTotal!(24)) * 100;
                End If

                Print #1, Tab(46); "##"; careerRecVal!(0);
                Print #1, Tab(49); "##"; careerTeamTotal!(36);: Print #1, "-";
                Print #1, Tab(52); "##"; careerTeamTotal!(41);
                Print #1, Tab(55); "##"; careerTeamTotal!(37);: Print #1, "-";
                Print #1, Tab(58); "##"; careerTeamTotal!(42);
                Print #1, Tab(61); "##"; careerTeamTotal!(38);: Print #1, "-";
                Print #1, Tab(64); "##"; careerTeamTotal!(43);
                Print #1, Tab(67); "##"; careerTeamTotal!(39);: Print #1, "-";
                Print #1, Tab(70); "##"; careerTeamTotal!(44);
                Print #1, Tab(73); "##"; careerTeamTotal!(40);: Print #1, "-";
                Print #1, Tab(76); "##"; careerTeamTotal!(45)

                For J = 23 To 45: careerTeamTotal!(J) = 0: Next

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsINT_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's INT's
Sub CareerStatsINT_Report (exitFlag%, B9, careerFile$)

    Shared intName$()
    Shared careerGIS!(), careerTeamTotal!()

    Cls
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print careerintName$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print intName$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "INTERCEPTORS        YEAR     INTERCEPTIONS"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerGIS!(I, B9, 1)
                Next

                Print #1, Left$(intName$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(20); careerGIS!(V, B9, 2);
                    Print #1, Tab(34); "###"; careerGIS!(V, B9, 0)
                Next V

                careerTeamTotal!(32) = 0

                For V = 1 To VV
                    careerTeamTotal!(32) = careerTeamTotal!(32) + careerGIS!(V, B9, 0)
                Next V

                Print #1, "TOTALS";
                Print #1, Tab(23); "##"; VV;
                Print #1, Tab(33); "####"; careerTeamTotal!(32)

                Close #1

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
                
            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub

'----------------------------------------
'   CareerStatsSACK_Report Subroutine
'----------------------------------------
'This routine formats and prints career 
'stats associated with the team's sacks
Sub CareerStatsSACK_Report (exitFlag%, B9, careerFile$)

    Shared sackName$()
    Shared careerGSS!(), careerTeamTotal!()

    Cls
    Locate 5, 20: Color 15
    Print "PRINT CAREER STATS OF ";
    Color 12: Print sackName$(B9);
    Color 15: Print " BEFORE "
    Locate , 20: Print "DELETING THEM (Y/N)"

    Print
    Color 14: Locate , 20
    Print "IF YOU DO NOT WISH TO PRINT/DELETE THE STATS"
    Locate , 20: Print "OF ";
    Color 12: Print sackName$(B9);
    Color 14: Print " HIT 'E', 'X' or 'Esc'"

    Do
        I$ = GetKeyPress$
    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N" Or UCase$(I$) = "E"

    Select Case UCase$(I$)

        Case "Y"
            userKey$ = GetPrintOptions$

            If userKey$ <> Chr$(27) Then

                Select Case userKey$

                    Case "F"
                        userFile$ = _SAVEFILEDIALOG$("Save Report File",_CWD$ + "\Report","*.txt","Text File")
                        If userFile$ <> "" Then
                            printDest = 0
                            destFile$ = userFile$
                        End If

                    Case "P"
                        printDest = 1
                        destFile$ = "REPORT"

                End Select

                Color 15, 0
                Cls

                Open destFile$ For Output As #1

                Print #1, careerFile$
                Print #1, "SACKERS             YEAR     INTERCEPTIONS"

                VV = 0

                For I = 1 To MAX_CAREER_YEARS 
                    VV = VV + careerGSS!(I, B9, 1)
                Next

                Print #1, Left$(sackName$(B9), 13);

                For V = 1 To VV
                    Print #1, Tab(20); careerGSS!(V, B9, 2);
                    Print #1, Tab(34); "###"; careerGSS!(V, B9, 0)
                Next V

                careerTeamTotal!(33) = 0

                For V = 1 To VV
                    careerTeamTotal!(33) = careerTeamTotal!(33) + careerGSS!(V, B9, 0)
                Next V

                Print #1, "TOTALS";
                Print #1, Tab(23); "##"; VV;
                Print #1, Tab(33); "####"; careerTeamTotal!(33)

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

            End If

        Case "N"
            'Exit Sub

        Case "E", "X", Chr$(27):
            exitFlag% = 1

    End Select

End Sub
