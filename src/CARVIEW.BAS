'----------------------------------------
'           CARVIEW Subroutine
'----------------------------------------
'This routine loads in an existing career
'file (.CAR) and gives the user options
'for viewing / printing the results
Sub CARVIEW ()

    _Title "3-in-1 Football - View Careers"

    careerFile$ = _OpenFileDialog$("Choose Career Leader file", diskPaths$(1), "*.CAR", "Career Leader files [.CAR]")
    'ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If careerFile$ <> "" Then

        Call LoadCareerFile(careerFile$)

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Career Report", _CWD$ + "\Career", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call CareerStats_Report(GetFileBaseName$(careerFile$), 0, userFile$)
                    End If

                Case "P"
                    Call CareerStats_Report(GetFileBaseName$(careerFile$), 1, "CAREER")

                Case "S"
                    Call CareerStats_Screen(GetFileBaseName$(careerFile$))

            End Select

        End If

    End If

    Color 15, 0

End Sub


'----------------------------------------
'     CareerStats_Report Subroutine
'----------------------------------------
'This subroutine facilitates formatting
'of the record data to a file, that is
'subsequently either saved, or printed.
Sub CareerStats_Report (careerName$, printDest, destFile$)

    Shared TT

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared intName$(), sackName$()

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()
    Shared careerRecPlyr$()

    Shared careerGIS!(), careerGSS!()
    Shared careerFGS!(), careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Shared careerRecVal!(), careerTeamTotal!()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1,
    Print #1, careerName$
    Print #1, "RUSHER         YEAR   ATT   YARDS AVERAGE  TDS   REC  YDS  AVG  TDS RUN-LONG-REC"

    For I = 0 To 50

        If carRBacks$(I) <> "" And carRBacks$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerRBS!(Z, I, 7)
            Next

            'Name
            Print #1, Left$(carRBacks$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(15); careerRBS!(V, I, 8);
                'Rush Att
                Print #1, Tab(23); Using "###"; careerRBS!(V, I, 0);
                'Rush Yards
                Print #1, Tab(30); Using "####"; careerRBS!(V, I, 1);

                'Rush Average
                If careerRBS!(V, I, 1) = 0 Or careerRBS!(V, I, 1) = 0 Then
                    Print #1, Tab(37); " 0.0";
                End If
                If careerRBS!(V, I, 0) > 0 Then
                    Print #1, Tab(37); Using "##.#"; careerRBS!(V, I, 1) / careerRBS!(V, I, 0);
                End If

                'TD's
                Print #1, Tab(44); Using "###"; careerRBS!(V, I, 2);
                'Rec
                Print #1, Tab(50); Using "###"; careerRBS!(V, I, 3);
                'Rec Yards
                Print #1, Tab(54); Using "####"; careerRBS!(V, I, 4);

                'Rec Avg
                If careerRBS!(V, I, 4) = 0 Or careerRBS!(V, I, 3) = 0 Then
                    Print #1, Tab(59); " 0.0";
                End If
                If careerRBS!(V, I, 3) > 0 Then
                    Print #1, Tab(59); Using "##.#"; careerRBS!(V, I, 4) / careerRBS!(V, I, 3);
                End If

                'Rec TDs
                Print #1, Tab(65); Using "##"; careerRBS!(V, I, 5);
                'Longest Rush
                Print #1, Tab(71); Using "##"; careerRBS!(V, I, 6);
                Print #1, careerRBS$(V, I, 0);
                'Longest Rec
                Print #1, Tab(77); Using "##"; careerRBS!(V, I, 9);
                Print #1, careerRBS$(V, I, 1)

            Next V

            careerRecVal!(0) = 0
            careerRecVal!(1) = 0
            careerRecPlyr$(0) = ""
            careerRecPlyr$(1) = ""

            For V = 1 To VV

                TT1 = TT1 + careerRBS!(V, I, 0)
                TT2 = TT2 + careerRBS!(V, I, 1)
                TT3 = TT3 + careerRBS!(V, I, 2)
                TT4 = TT4 + careerRBS!(V, I, 3)
                TT5 = TT5 + careerRBS!(V, I, 4)
                TT6 = TT6 + careerRBS!(V, I, 5)

                If careerRBS!(V, I, 6) > careerRecVal!(0) Then
                    careerRecVal!(0) = careerRBS!(V, I, 6)
                    careerRecPlyr$(0) = careerRBS$(V, I, 0)
                Else
                    If careerRBS!(V, I, 9) > careerRecVal!(1) Then
                        careerRecVal!(1) = careerRBS!(V, I, 9)
                        careerRecPlyr$(1) = careerRBS$(V, I, 1)
                    End If
                End If

            Next V

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
            Print #1, Tab(71); Using "##"; careerRecVal!(0);
            Print #1, careerRecPlyr$(0);
            Print #1, Tab(77); Using "##"; careerRecVal!(1);
            Print #1, careerRecPlyr$(1)

            TT1 = 0
            TT2 = 0
            TT3 = 0
            TT4 = 0
            TT5 = 0
            TT6 = 0

            Print #1,

        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "RECEIVER        YEAR        REC.      YARDS      AVG.     TD  LONGEST REC."

    For I = 0 To 40

        If carWdRec$(I) <> "" And carWdRec$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerWRS!(Z, I, 4)
            Next

            'Name
            Print #1, Left$(carWdRec$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(16); careerWRS!(V, I, 5);
                'Rec
                Print #1, Tab(29); Using "###"; careerWRS!(V, I, 0);
                'Yards
                Print #1, Tab(40); Using "####"; careerWRS!(V, I, 1);

                'Average
                If careerWRS!(V, I, 0) = 0 Or careerWRS!(V, I, 1) = 0 Then
                    Print #1, Tab(49); " 0.0";
                End If
                If careerWRS!(V, I, 0) > 0 Then
                    Print #1, Tab(49); Using "##.#"; careerWRS!(V, I, 1) / careerWRS!(V, I, 0);
                End If

                'Rec TDs
                Print #1, Tab(59); Using "##"; careerWRS!(V, I, 2);
                'Longest Rec
                Print #1, Tab(66); Using "##"; careerWRS!(V, I, 3);
                Print #1, careerWRS$(V, I, 0)

            Next V

            careerRecVal!(0) = 0
            careerRecPlyr$(0) = ""

            For V = 1 To VV
                TT7 = TT7 + careerWRS!(V, I, 0)
                TT8 = TT8 + careerWRS!(V, I, 1)
                TT9 = TT9 + careerWRS!(V, I, 2)

                If careerWRS!(V, I, 3) > careerRecVal!(0) Then
                    careerRecVal!(0) = careerWRS!(V, I, 3)
                    careerRecPlyr$(0) = careerWRS$(V, I, 0)
                End If

            Next V

            Print #1, "TOTALS";
            Print #1, Tab(19); Using "##"; VV;
            Print #1, Tab(28); Using "####"; TT7;
            Print #1, Tab(39); Using "#####"; TT8;

            If TT7 = 0 Or TT8 = 0 Then
                Print #1, Tab(49); " 0.0";
            End If
            If TT7 > 0 Then
                Print #1, Tab(49); Using "##.#"; TT8 / TT7;
            End If

            Print #1, Tab(58); Using "###"; TT9;
            Print #1, Tab(66); Using "##"; careerRecVal!(0);
            Print #1, careerRecPlyr$(0)

            TT7 = 0
            TT8 = 0
            TT9 = 0

            Print #1,

        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "QUARTERBACK  YEAR  COMP.    ATT.    INT  TDS   YARDS  PCT. YD/COMP  RATING  LONG"

    For I = 0 To 19

        If carQBacks$(I) <> "" And carQBacks$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerQBS!(Z, I, 6)
            Next

            'Name
            Print #1, Left$(carQBacks$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(13); careerQBS!(V, I, 7);
                'Pass Comp
                Print #1, Tab(21); Using "###"; careerQBS!(V, I, 1);
                'Pass Att
                Print #1, Tab(29); Using "###"; careerQBS!(V, I, 0);
                'Pass Int
                Print #1, Tab(38); Using "##"; careerQBS!(V, I, 2);
                'Pass TDs
                Print #1, Tab(43); Using "##"; careerQBS!(V, I, 4);
                'Pass Yds
                Print #1, Tab(49); Using "####"; careerQBS!(V, I, 3);

                If careerQBS!(V, I, 1) = 0 Or careerQBS!(V, I, 0) = 0 Then
                    'Comp Pct
                    Print #1, Tab(56); "  0.0";
                    'Yds / Comp
                    Print #1, Tab(61); "  0.0";
                End If
                If careerQBS!(V, I, 1) > 0 Then
                    'Comp Pct
                    Print #1, Tab(52); Using "###.#"; careerQBS!(V, I, 1) / careerQBS!(V, I, 0);
                    'Yds / Comp
                    Print #1, Tab(61); Using "###.#"; careerQBS!(V, I, 3) / careerQBS!(V, I, 1);
                End If

                Call QBRating_Career(V, I, TT)

                'QB Rating
                Print #1, Tab(70); Using "###.#"; careerQBS!(V, I, 12);
                'Longest Pass
                Print #1, Tab(77); Using "##"; careerQBS!(V, I, 5);
                Print #1, careerQBS$(V, I, 0)

            Next V

            careerRecVal!(0) = 0
            careerRecPlyr$(0) = ""

            For V = 1 To VV

                TT10 = TT10 + careerQBS!(V, I, 1)
                TT11 = TT11 + careerQBS!(V, I, 0)
                TT12 = TT12 + careerQBS!(V, I, 2)
                TT13 = TT13 + careerQBS!(V, I, 4)
                TT14 = TT14 + careerQBS!(V, I, 3)

                If careerQBS!(V, I, 5) > careerRecVal!(0) Then
                    careerRecVal!(0) = careerQBS!(V, I, 5)
                    careerRecPlyr$(0) = careerQBS$(V, I, 0)
                End If

            Next V

            Print #1, "TOTALS";
            Print #1, Tab(16); Using "##"; VV;
            Print #1, Tab(20); Using "####"; TT10;
            Print #1, Tab(28); Using "####"; TT11;
            Print #1, Tab(37); Using "###"; TT12;
            Print #1, Tab(41); Using "####"; TT13;
            Print #1, Tab(48); Using "#####"; TT14;

            If TT10 = 0 Or TT11 = 0 Then
                Print #1, Tab(56); " 00";
            End If
            If TT10 > 0 Then
                Print #1, Tab(56); Using "##"; (TT10 / TT11) * 100;
            End If
            If TT11 = 0 Then
                Print #1, Tab(63); " 0.0";
            End If
            If TT10 > 0 Then
                Print #1, Tab(63); Using "##.#"; TT14 / TT10;
            End If

            If TT11 > 0 Then
                TT30 = Int(TT10 / TT11 * 1000) / 10
            End If
            If TT11 = 0 Then
                TT27 = 0
                TT28 = 0
                TT29 = 0
                TT30 = 0
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

            Print #1, Tab(70); Using "###.#"; TT31;
            Print #1, Tab(77); Using "##"; careerRecVal!(0);
            Print #1, careerRecPlyr$(0)

            TT10 = 0
            TT11 = 0
            TT12 = 0
            TT13 = 0
            TT14 = 0

            Print #1,

        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

    For I = 0 To 14

        If carKickRet$(I) <> "" And carKickRet$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerKRS!(Z, I, 4)
            Next

            'Name
            Print #1, Left$(carKickRet$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(22); careerKRS!(V, I, 5);
                'Returns
                Print #1, Tab(32); Using "###"; careerKRS!(V, I, 0);
                'Yards
                Print #1, Tab(42); Using "####"; careerKRS!(V, I, 1);

                'Average
                If careerKRS!(V, I, 0) = 0 Or careerKRS!(V, I, 1) = 0 Then
                    Print #1, Tab(52); " 00";
                End If
                If careerKRS!(V, I, 0) > 0 Then
                    Print #1, Tab(52); Using "##.#"; careerKRS!(V, I, 1) / careerKRS!(V, I, 0);
                End If

                'Ret TDs
                Print #1, Tab(62); Using "##"; careerKRS!(V, I, 2);
                'Longest Ret
                Print #1, Tab(70); Using "###"; careerKRS!(V, I, 3);
                Print #1, careerKRS$(V, I, 0)

            Next V

            careerRecVal!(0) = 0
            careerRecPlyr$(0) = ""

            For V = 1 To VV
                TT15 = TT15 + careerKRS!(V, I, 0)
                TT16 = TT16 + careerKRS!(V, I, 1)
                TT17 = TT17 + careerKRS!(V, I, 2)

                If careerKRS!(V, I, 3) > careerRecVal!(0) Then
                    careerRecVal!(0) = careerKRS!(V, I, 3)
                    careerRecPlyr$(0) = careerKRS$(V, I, 0)
                End If

            Next V

            Print #1, "TOTALS";
            Print #1, Tab(25); Using "##"; VV;
            Print #1, Tab(32); Using "###"; TT15;
            Print #1, Tab(41); Using "#####"; TT16;

            If TT15 = 0 Or TT16 = 0 Then
                Print #1, Tab(52); " 00";
            End If
            If TT15 > 0 Then
                Print #1, Tab(52); Using "##.#"; TT16 / TT15;
            End If

            Print #1, Tab(61); Using "###"; TT17;
            Print #1, Tab(70); Using "###"; careerRecVal!(0);
		Print #1, careerRecPlyr$(0)

            TT15 = 0
            TT16 = 0
            TT17 = 0

            Print #1,
        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

    For I = 0 To 14

        If carPRet$(I) <> "" And carPRet$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerPRS!(Z, I, 4)
            Next

            Print #1, Left$(carPRet$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(22); careerPRS!(V, I, 5);
                'Returns
                Print #1, Tab(32); Using "###"; careerPRS!(V, I, 0);
                'Yards
                Print #1, Tab(42); Using "####"; careerPRS!(V, I, 1);

                'Ret Avg
                If careerPRS!(V, I, 0) = 0 Or careerPRS!(V, I, 1) = 0 Then
                    Print #1, Tab(52); " 00";
                End If
                If careerPRS!(V, I, 0) > 0 Then
                    Print #1, Tab(52); Using "##.#"; careerPRS!(V, I, 1) / careerPRS!(V, I, 0);
                End If

                'Ret TD's
                Print #1, Tab(62); Using "##"; careerPRS!(V, I, 2);
                'Longest Ret
                Print #1, Tab(70); Using "###"; careerPRS!(V, I, 3);
                Print #1, careerPRS$(V, I, 0)

            Next V

            careerRecVal!(0) = 0
            careerRecPlyr$(0) = ""

            For V = 1 To VV
                TT18 = TT18 + careerPRS!(V, I, 0)
                TT19 = TT19 + careerPRS!(V, I, 1)
                TT20 = TT20 + careerPRS!(V, I, 2)
                If careerPRS!(V, I, 3) > careerRecVal!(0) Then
                    careerRecVal!(0) = careerPRS!(V, I, 3)
                    careerRecPlyr$(0) = careerPRS$(V, I, 0)
                End If

            Next V

            Print #1, "TOTALS";
            Print #1, Tab(25); Using "##"; VV;
            Print #1, Tab(32); Using "###"; TT18;
            Print #1, Tab(41); Using "#####"; TT19;

            If TT18 = 0 Or TT19 = 0 Then
                Print #1, Tab(52); " 00";
            End If
            If TT18 > 0 Then
                Print #1, Tab(52); Using "##.#"; TT19 / TT18;
            End If

            Print #1, Tab(61); Using "###"; TT20;
            Print #1, Tab(70); Using "###"; careerRecVal!(0);
		Print #1, careerRecPlyr$(0)

            TT18 = 0: TT19 = 0: TT20 = 0

            Print #1,

        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "PUNTERS           YEAR     ATT.     YARDS      AVG."

    For I = 0 To 9

        If carPunter$(I) <> "" And carPunter$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerPUS!(Z, I, 2)
            Next

            'Name
            Print #1, Left$(carPunter$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(18); careerPUS!(V, I, 3);
                'Att
                Print #1, Tab(28); Using "###"; careerPUS!(V, I, 0);
                'Yds
                Print #1, Tab(38); Using "####"; careerPUS!(V, I, 1);

                'Average
                If careerPUS!(V, I, 0) = 0 Or careerPUS!(V, I, 1) = 0 Then
                    Print #1, Tab(47); " 00.0"
                End If
                If careerPUS!(V, I, 0) > 0 Then
                    Print #1, Tab(47); Using "###.#"; careerPUS!(V, I, 1) / careerPUS!(V, I, 0)
                End If

            Next V

            For V = 1 To VV
                TT21 = TT21 + careerPUS!(V, I, 0)
                TT22 = TT22 + careerPUS!(V, I, 1)
            Next V

            Print #1, "TOTALS";
            Print #1, Tab(21); Using "##"; VV;
            Print #1, Tab(27); Using "####"; TT21;
            Print #1, Tab(37); Using "#####"; TT22;

            If TT21 = 0 Or TT22 = 0 Then
                Print #1, Tab(47); " 00"
            End If
            If TT21 > 0 Then
                Print #1, Tab(47); Using "##.#"; TT22 / TT21
            End If

            TT21 = 0
            TT22 = 0

            Print #1,

        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49  50+"

    For I = 0 To 9

        If carKicker$(I) <> "" And carKicker$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerFGS!(Z, I, 15)
            Next

            'Name
            Print #1, Left$(carKicker$(I), 13);

            For V = 1 To VV

                'Year
                Print #1, Tab(13); careerFGS!(V, I, 16);
                'EPA
                Print #1, Tab(19); Using "###"; careerFGS!(V, I, 3);
                'EPM
                Print #1, Tab(23); Using "###"; careerFGS!(V, I, 2);

                'EP %
                If careerFGS!(V, I, 2) = 0 Or careerFGS!(V, I, 3) = 0 Then
                    Print #1, Tab(27); "  0";
                End If
                If careerFGS!(V, I, 2) > 0 Then
                    Print #1, Tab(27); Using "###"; (careerFGS!(V, I, 2) / careerFGS!(V, I, 3)) * 100;
                End If

                'FGA
                Print #1, Tab(32); Using "###"; careerFGS!(V, I, 1);
                'FGM
                Print #1, Tab(36); Using "###"; careerFGS!(V, I, 0);

                'FG %
                If careerFGS!(V, I, 0) = 0 Or careerFGS!(V, I, 1) = 0 Then
                    Print #1, Tab(40); "  0";
                End If
                If careerFGS!(V, I, 0) > 0 Then
                    Print #1, Tab(40); Using "###"; (careerFGS!(V, I, 0) / careerFGS!(V, I, 1)) * 100;
                End If

                'Longest FG
                Print #1, Tab(46); Using "##"; careerFGS!(V, I, 4);

                'FG's 0-19
                Print #1, Tab(49); Using "##"; careerFGS!(V, I, 5);
				Print #1, "-";
                Print #1, Tab(52); Using "##"; careerFGS!(V, I, 10);

                'FG's 20-29
                Print #1, Tab(55); Using "##"; careerFGS!(V, I, 6);
				Print #1, "-";
                Print #1, Tab(58); Using "##"; careerFGS!(V, I, 11);

                'FG's 30-39
                Print #1, Tab(61); Using "##"; careerFGS!(V, I, 7);
				Print #1, "-";
                Print #1, Tab(64); Using "##"; careerFGS!(V, I, 12);

                'FG's 40-49
                Print #1, Tab(67); Using "##"; careerFGS!(V, I, 8);
				Print #1, "-";
                Print #1, Tab(70); Using "##"; careerFGS!(V, I, 13);

                'FG's 50+
                Print #1, Tab(73); Using "##"; careerFGS!(V, I, 9);
				Print #1, "-";
                Print #1, Tab(76); Using "##"; careerFGS!(V, I, 14)
            Next V

            For V = 1 To VV
                careerTeamTotal!(23) = careerTeamTotal!(23) + careerFGS!(V, I, 0)
                careerTeamTotal!(24) = careerTeamTotal!(24) + careerFGS!(V, I, 1)
                careerTeamTotal!(34) = careerTeamTotal!(34) + careerFGS!(V, I, 2)
                careerTeamTotal!(35) = careerTeamTotal!(35) + careerFGS!(V, I, 3)
                careerTeamTotal!(36) = careerTeamTotal!(36) + careerFGS!(V, I, 5)
                careerTeamTotal!(37) = careerTeamTotal!(37) + careerFGS!(V, I, 6)
                careerTeamTotal!(38) = careerTeamTotal!(38) + careerFGS!(V, I, 7)
                careerTeamTotal!(39) = careerTeamTotal!(39) + careerFGS!(V, I, 8)
                careerTeamTotal!(40) = careerTeamTotal!(40) + careerFGS!(V, I, 9)
                careerTeamTotal!(41) = careerTeamTotal!(41) + careerFGS!(V, I, 10)
                careerTeamTotal!(42) = careerTeamTotal!(42) + careerFGS!(V, I, 11)
                careerTeamTotal!(43) = careerTeamTotal!(43) + careerFGS!(V, I, 12)
                careerTeamTotal!(44) = careerTeamTotal!(44) + careerFGS!(V, I, 13)
                careerTeamTotal!(45) = careerTeamTotal!(45) + careerFGS!(V, I, 14)
            Next V

            careerRecVal!(0) = 0

            For V = 1 To VV
                If careerFGS!(V, I, 4) >= careerRecVal!(0) Then
                    careerRecVal!(0) = careerFGS!(V, I, 4)
                End If
            Next

            Print #1, "TOTALS";
            Print #1, Tab(16); VV;
            Print #1, Tab(19); Using "###"; careerTeamTotal!(35);
            Print #1, Tab(23); Using "###"; careerTeamTotal!(34);

            If careerTeamTotal!(34) = 0 Or careerTeamTotal!(35) = 0 Then
                Print #1, Tab(27); "  0";
            End If
            If careerTeamTotal!(34) > 0 Then
                Print #1, Tab(27); Using "###"; (careerTeamTotal!(34) / careerTeamTotal!(35)) * 100;
            End If

            Print #1, Tab(32); Using "###"; careerTeamTotal!(24);
            Print #1, Tab(36); Using "###"; careerTeamTotal!(23);

            If careerTeamTotal!(23) = 0 Or careerTeamTotal!(24) = 0 Then
                Print #1, Tab(40); "  0";
            End If
            If careerTeamTotal!(23) > 0 Then
                Print #1, Tab(40); Using "###"; (careerTeamTotal!(23) / careerTeamTotal!(24)) * 100;
            End If

            Print #1, Tab(46); Using "##"; careerRecVal!(0);
            Print #1, Tab(49); Using "##"; careerTeamTotal!(36);
			Print #1, "-";
            Print #1, Tab(52); Using "##"; careerTeamTotal!(41);
            Print #1, Tab(55); Using "##"; careerTeamTotal!(37);
			Print #1, "-";
            Print #1, Tab(58); Using "##"; careerTeamTotal!(42);
            Print #1, Tab(61); Using "##"; careerTeamTotal!(38);
			Print #1, "-";
            Print #1, Tab(64); Using "##"; careerTeamTotal!(43);
            Print #1, Tab(67); Using "##"; careerTeamTotal!(39);
			Print #1, "-";
            Print #1, Tab(70); Using "##"; careerTeamTotal!(44);
            Print #1, Tab(73); Using "##"; careerTeamTotal!(40);
			Print #1, "-";
            Print #1, Tab(76); Using "##"; careerTeamTotal!(45)

            For J = 23 To 45: careerTeamTotal!(J) = 0: Next

            Print #1,

        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "INTERCEPTORS        YEAR     INTERCEPTIONS"

    For I = 0 To 54

        If intName$(I) <> "" And intName$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerGIS!(Z, I, 1)
            Next

            'Name
            Print #1, Left$(intName$(I), 13);

            For V = 1 To VV
                'Year
                Print #1, Tab(20); careerGIS!(V, I, 2);
                'Int's
                Print #1, Tab(34); Using "###"; careerGIS!(V, I, 0)
            Next V

            careerTeamTotal!(32) = 0

            For V = 1 To VV
                careerTeamTotal!(32) = careerTeamTotal!(32) + careerGIS!(V, I, 0)
            Next V

            Print #1, "TOTALS";
            Print #1, Tab(23); Using "##"; VV;
            Print #1, Tab(33); Using "####"; careerTeamTotal!(32)
            Print #1,
        End If

    Next I

    Print #1,
    Print #1, careerName$
    Print #1, "SACKERS             YEAR          SACKS"

    For I = 0 To 54

        If sackName$(I) <> "" And sackName$(I) <> "XXX" Then

            VV = 0

            For Z = 1 To MAX_CAREER_YEARS:
                VV = VV + careerGSS!(Z, I, 1)
            Next

            'Name
            Print #1, Left$(sackName$(I), 13);

            For V = 1 To VV
                'Year
                Print #1, Tab(20); careerGSS!(V, I, 2);
                'Sacks
                Print #1, Tab(34); Using "###"; careerGSS!(V, I, 0)
            Next V

            careerTeamTotal!(33) = 0

            For V = 1 To VV
                careerTeamTotal!(33) = careerTeamTotal!(33) + careerGSS!(V, I, 0)
            Next V

            Print #1, "TOTALS";
            Print #1, Tab(23); Using "##"; VV;
            Print #1, Tab(33); Using "####"; careerTeamTotal!(33)
            Print #1,
        End If

    Next I

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

End Sub


'----------------------------------------
'      CareerStats_Screen Subroutine
'----------------------------------------
'This routine loads in an existing career
'file (.CAR) and gives the user options
'for viewing / printing the results.
'Options are organized by each of the
'position categories in football.
'A separate routine is called to display
'the chosen options to the user screen.
Sub CareerStats_Screen (careerName$)

    _Title "3-in-1 Football - View Careers"

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared carIntName$(), carSackName$()

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!(), careerFGS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()
    Shared careerTeamTotal!()

    Do

        Erase careerTeamTotal!

        Color 15, 0
        Cls

        Locate 4, 10
        Color 7: Print "VIEW ";
        Color 12: Print "R";
        Color 7: Print "UNNING BACKS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 12: Print "W";
        Color 7: Print "IDE RECEIVERS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 12: Print "Q";
        Color 7: Print "UARTERBACKS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 7: Print "KI";
        Color 12: Print "C";
        Color 7: Print "K RETURNERS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 7: Print "P";
        Color 12: Print "U";
        Color 7: Print "NT RETURNERS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 12: Print "P";
        Color 7: Print "UNTERS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 12: Print "K";
        Color 7: Print "ICKERS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 12: Print "I";
        Color 7: Print "NTERCEPTIONS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 7: Print "DE";
        Color 12: Print "F";
        Color 7: Print "ENSIVE SACKS"

        Locate , 10
        Color 7: Print "VIEW ";
        Color 7: Print "R";
        Color 12: Print "O";
        Color 7: Print "STER IN SEQUENCE"

        Locate , 10
        Color 12: Print "E";
        Color 7: Print "XIT TO MAIN MENU."

        escPressed% = 0

        I$ = GetKeyPress$

        Select Case UCase$(I$)

            Case "R"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 16:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carRBacks$(I), 12)
                Next

                For I = 17 To 33:
                    Locate I - 13, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carRBacks$(I), 12)
                Next

                For I = 34 To 49:
                    Locate I - 30, 40
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carRBacks$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 1, escPressed%, careerName$)

            Case "W"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 19:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carWdRec$(I), 12)
                Next

                For I = 20 To 39:
                    Locate I - 16, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carWdRec$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 2, escPressed%, careerName$)

            Case "Q"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 9:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carQBacks$(I), 12)
                Next

                For I = 10 To 19:
                    Locate I - 6, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carQBacks$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 3, escPressed%, careerName$)

            Case "C"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 7:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carKickRet$(I), 12)
                Next

                For I = 8 To 14:
                    Locate I - 4, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carKickRet$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 4, escPressed%, careerName$)

            Case "U"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 7:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carPRet$(I), 12)
                Next

                For I = 8 To 14:
                    Locate I - 4, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carPRet$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 5, escPressed%, careerName$)

            Case "P"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 9:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carPunter$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 6, escPressed%, careerName$)

            Case "K"

                Cls
                Locate 2, (80 - Len(careerName$)) / 2

                Color 11
                Print careerName$

                For I = 0 To 9:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carKicker$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 7, escPressed%, careerName$)

            Case "I"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 18:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carIntName$(I), 12)
                Next

                For I = 19 To 36:
                    Locate I - 15, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carIntName$(I), 12)
                Next

                For I = 37 To 54:
                    Locate I - 33, 40
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carIntName$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 8, escPressed%, careerName$)

            Case "F"

                Cls

                Locate 2, (80 - Len(careerName$)) / 2
                Color 11
                Print careerName$

                For I = 0 To 18:
                    Locate I + 4
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carSackName$(I), 12)
                Next

                For I = 19 To 36:
                    Locate I - 15, 20
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carSackName$(I), 12)
                Next

                For I = 37 To 54:
                    Locate I - 33, 40
                    Color 11: Print Using "##"; I;
                    Print " ";
                    Color 7: Print Left$(carSackName$(I), 12)
                Next

                Print
                Print
                Color 15: Input "ENTER NUMBER OF PLAYER TO VIEW"; B9
                RT = 2
                Cls

                Call CareerCategory_Screen(RT, B9, 9, escPressed%, careerName$)

            Case "O"

                RT = 1
                Cls

                For I = 1 To 50:
                    careerTeamTotal!(I) = 0
                Next

                For category% = 1 To 9:
                    Call CareerCategory_Screen(RT, B9, category%, escPressed%, careerName$)
                    If escPressed% = 1 Then Exit For
                Next

                'If escPressed% = 1 Then I$ = "E"

            Case "E"
                '

        End Select

    Loop Until UCase$(I$) = "E" Or escPressed% = 1

    Color 15, 0

End Sub


'----------------------------------------
'    CareerCategory_Screen Subroutine
'----------------------------------------
'Depending on the stat category chosen,
'formats the corresponding career stats
'and outputs them to the user display
Sub CareerCategory_Screen (RT, B9, categoryID%, escPressed%, careerName$)

    Shared TT

    Shared careerRecPlyr$()
    Shared careerRecVal!()

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared carIntName$(), carSackName$()

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerGIS!(), careerGSS!(), careerFGS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()
    Shared careerTeamTotal!()

    Do

        Cls

        Locate 2, (80 - Len(careerName$)) / 2
        Color 11
        Print careerName$

        Color 12

        Select Case categoryID%

            Case 1

                Locate 4, 1
                Print "RUSHER         YEAR  ATT.  YARDS AVG. TDS   REC. YARDS  AVG. TDS RUN-LONGEST-REC"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 49

                    If carRBacks$(I) <> "" And carRBacks$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carRBacks$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerRBS!(Z, I, 7)
                        Next

                        For V = 1 To VV

                            Color 7
                            'Year
                            Print Tab(15); careerRBS!(V, I, 8);
                            'Rush Attempts
                            Print Tab(23); Using "###"; careerRBS!(V, I, 0);
                            'Rush Yards
                            Print Tab(29); Using "####"; careerRBS!(V, I, 1);

                            'Rush Avg
                            If careerRBS!(V, I, 1) = 0 Or careerRBS!(V, I, 0) = 0 Then
                                Print Tab(34); " 0.0";
                            End If
                            If careerRBS!(V, I, 0) > 0 Then
                                Print Tab(34); Using "##.#"; careerRBS!(V, I, 1) / careerRBS!(V, I, 0);
                            End If

                            'Rush TDs
                            Print Tab(39); Using "###"; careerRBS!(V, I, 2);
                            'Rush Rec
                            Print Tab(45); Using "###"; careerRBS!(V, I, 3);
                            'Rush Rec Yards
                            Print Tab(51); Using "####"; careerRBS!(V, I, 4);

                            'Rush Rec Avg
                            If careerRBS!(V, I, 4) = 0 Or careerRBS!(V, I, 3) = 0 Then
                                Print Tab(57); " 0.0";
                            End If
                            If careerRBS!(V, I, 3) > 0 Then
                                Print Tab(57); Using "##.#"; careerRBS!(V, I, 4) / careerRBS!(V, I, 3);
                            End If

                            'Rush Rec TDs
                            Print Tab(63); Using "##"; careerRBS!(V, I, 5);

                            'Longest Run
                            Print Tab(67); Using "##"; careerRBS!(V, I, 6);
                            Print Tab(69); careerRBS$(V, I, 0);

                            'Longest Rec
                            Print Tab(77); Using "##"; careerRBS!(V, I, 9);
                            Print Tab(79); careerRBS$(V, I, 1)

                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(1) = careerTeamTotal!(1) + careerRBS!(V, I, 0)
                            careerTeamTotal!(2) = careerTeamTotal!(2) + careerRBS!(V, I, 1)
                            careerTeamTotal!(3) = careerTeamTotal!(3) + careerRBS!(V, I, 2)
                            careerTeamTotal!(4) = careerTeamTotal!(4) + careerRBS!(V, I, 3)
                            careerTeamTotal!(5) = careerTeamTotal!(5) + careerRBS!(V, I, 4)
                            careerTeamTotal!(6) = careerTeamTotal!(6) + careerRBS!(V, I, 5)
                        Next V

                        careerRecVal!(1) = careerRBS!(1, I, 6)
                        careerRecPlyr$(1) = careerRBS$(1, I, 0)
                        careerRecVal!(2) = careerRBS!(1, I, 9)
                        careerRecPlyr$(2) = careerRBS$(1, I, 1)

                        For V = 2 To VV

                            If careerRBS!(V, I, 6) >= careerRecVal!(1) Then
                                careerRecVal!(1) = careerRBS!(V, I, 6)
                                careerRecPlyr$(1) = careerRBS$(V, I, 0)
                            Else
                                If careerRBS!(V, I, 9) >= careerRecVal!(2) Then
                                    careerRecVal!(2) = careerRBS!(V, I, 9)
                                    careerRecPlyr$(2) = careerRBS$(V, I, 1)
                                End If
                            End If

                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(18); Using "##"; VV;
                        Print Tab(22); Using "####"; careerTeamTotal!(1);
                        Print Tab(28); Using "#####"; careerTeamTotal!(2);

                        If careerTeamTotal!(1) = 0 Or careerTeamTotal!(2) = 0 Then
                            Print Tab(34); " 0.0";
                        End If
                        If careerTeamTotal!(1) > 0 Then
                            Print Tab(34); Using "##.#"; careerTeamTotal!(2) / careerTeamTotal!(1);
                        End If

                        Print Tab(39); Using "###"; careerTeamTotal!(3);
                        Print Tab(44); Using "####"; careerTeamTotal!(4);
                        Print Tab(50); Using "#####"; careerTeamTotal!(5);

                        If careerTeamTotal!(4) = 0 Or careerTeamTotal!(5) = 0 Then
                            Print Tab(57); " 0.0";
                        End If
                        If careerTeamTotal!(4) > 0 Then
                            Print Tab(57); Using "##.#"; careerTeamTotal!(5) / careerTeamTotal!(4);
                        End If

                        Print Tab(62); Using "###"; careerTeamTotal!(6);
                        Print Tab(67); Using "##"; careerRecVal!(1);
                        Print Tab(69); careerRecPlyr$(1);
                        Print Tab(77); Using "##"; careerRecVal!(2);
                        Print Tab(79); careerRecPlyr$(2)

                        For J = 1 To 6:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(90, " ")
                            Next
                        Else
                            escPressed% = 1
                        End If

                    End If

                    If escPressed% = 1 Then Exit For

                Next I

            Case 2

                Locate 4, 1
                Print "RECEIVER        YEAR    REC.   YARDS    AVG.  TD  LONGEST REC."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 39

                    If carWdRec$(I) <> "" And carWdRec$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carWdRec$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerWRS!(Z, I, 4)
                        Next

                        For V = 1 To VV

                            Color 7
                            'Year
                            Print Tab(16); careerWRS!(V, I, 5);
                            'Rec
                            Print Tab(25); Using "###"; careerWRS!(V, I, 0);
                            'Yards
                            Print Tab(33); Using "####"; careerWRS!(V, I, 1);

                            'Avg
                            If careerWRS!(V, I, 0) = 0 Or careerWRS!(V, I, 1) = 0 Then
                                Print Tab(41); " 0.0";
                            End If
                            If careerWRS!(V, I, 0) > 0 Then
                                Print Tab(41); Using "##.#"; careerWRS!(V, I, 1) / careerWRS!(V, I, 0);
                            End If

                            'TD
                            Print Tab(47); Using "##"; careerWRS!(V, I, 2);
                            'Longest
                            Print Tab(54); Using "##"; careerWRS!(V, I, 3);
                            Print Tab(56); careerWRS$(V, I, 0)
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(7) = careerTeamTotal!(7) + careerWRS!(V, I, 0)
                            careerTeamTotal!(8) = careerTeamTotal!(8) + careerWRS!(V, I, 1)
                            careerTeamTotal!(9) = careerTeamTotal!(9) + careerWRS!(V, I, 2)
                        Next V

                        careerRecVal!(2) = careerWRS!(1, I, 3)
                        careerRecPlyr$(2) = careerWRS$(1, I, 0)

                        For V = 2 To VV
                            If careerWRS!(V, I, 3) >= careerRecVal!(2) Then
                                careerRecVal!(2) = careerWRS!(V, I, 3)
                                careerRecPlyr$(2) = careerWRS$(V, I, 0)
                            End If

                        Next

                        Print
                        Color 3: Print "TOTALS";
                        Print Tab(19); Using "##"; VV;
                        Print Tab(24); Using "####"; careerTeamTotal!(7);
                        Print Tab(32); Using "#####"; careerTeamTotal!(8);

                        If careerTeamTotal!(7) = 0 Or careerTeamTotal!(8) = 0 Then
                            Print Tab(41); " 0.0";
                        End If
                        If careerTeamTotal!(7) > 0 Then
                            Print Tab(41); Using "##.#"; careerTeamTotal!(8) / careerTeamTotal!(7);
                        End If

                        Print Tab(46); Using "###"; careerTeamTotal!(9);
                        Print Tab(54); Using "##"; careerRecVal!(2);
                        Print Tab(56); careerRecPlyr$(2)

                        For J = 7 To 9:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 3

                Locate 4, 1
                Print "QUARTERBACK  YEAR  COMP. ATT. INT TDS YARDS  PCT. YD/COMP RATING LONG.PASS"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 19

                    If carQBacks$(I) <> "" And carQBacks$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carQBacks$(I), 12);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerQBS!(Z, I, 6)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(13); careerQBS!(V, I, 7);
                            'Comp
                            Print Tab(21); Using "###"; careerQBS!(V, I, 1);
                            'Att
                            Print Tab(26); Using "###"; careerQBS!(V, I, 0);
                            'Int
                            Print Tab(32); Using "##"; careerQBS!(V, I, 2);
                            'TDs
                            Print Tab(36); Using "##"; careerQBS!(V, I, 4);
                            'Yards
                            Print Tab(40); Using "####"; careerQBS!(V, I, 3);

                            'Comp Pct
                            If careerQBS!(V, I, 1) = 0 Or careerQBS!(V, I, 0) = 0 Then
                                Print Tab(47); " 00";
                            End If
                            If careerQBS!(V, I, 1) > 0 Then
                                Print Tab(47); Using "##"; (careerQBS!(V, I, 1) / careerQBS!(V, I, 0)) * 100;
                            End If

                            'Yds / Comp
                            If careerQBS!(V, I, 1) = 0 Then
                                Print Tab(53); " 0.0";
                            End If
                            If careerQBS!(V, I, 1) > 0 Then
                                Print Tab(53); Using "##.#"; careerQBS!(V, I, 3) / careerQBS!(V, I, 1);
                            End If

                            'Rating
                            Call QBRating_Career(V, I, TT)
                            Print Tab(60); Using "###.#"; careerQBS!(V, I, 12);

                            'Longest Pass
                            Print Tab(70); Using "##"; careerQBS!(V, I, 5);
                            Print careerQBS$(V, I, 0)
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(10) = careerTeamTotal!(10) + careerQBS!(V, I, 1)
                            careerTeamTotal!(11) = careerTeamTotal!(11) + careerQBS!(V, I, 0)
                            careerTeamTotal!(12) = careerTeamTotal!(12) + careerQBS!(V, I, 2)
                            careerTeamTotal!(13) = careerTeamTotal!(13) + careerQBS!(V, I, 4)
                            careerTeamTotal!(14) = careerTeamTotal!(14) + careerQBS!(V, I, 3)
                        Next V

                        careerRecVal!(3) = careerQBS!(1, I, 5)
                        careerRecPlyr$(3) = careerQBS$(1, I, 0)

                        For V = 2 To VV
                            If careerQBS!(V, I, 5) >= careerRecVal!(3) Then
                                careerRecVal!(3) = careerQBS!(V, I, 5): careerRecPlyr$(3) = careerQBS$(V, I, 0)
                            End If
                        Next

                        Print
                        Color 3: Print: Print "TOTALS";
                        Print Tab(16); Using "##"; VV;
                        Print Tab(20); Using "####"; careerTeamTotal!(10);
                        Print Tab(25); Using "####"; careerTeamTotal!(11);
                        Print Tab(31); Using "###"; careerTeamTotal!(12);
                        Print Tab(34); Using "####"; careerTeamTotal!(13);
                        Print Tab(39); Using "#####"; careerTeamTotal!(14);

                        If careerTeamTotal!(10) = 0 Or careerTeamTotal!(11) = 0 Then
                            Print Tab(47); " 00";
                        End If
                        If careerTeamTotal!(10) > 0 Then
                            Print Tab(47); Using "##"; (careerTeamTotal!(10) / careerTeamTotal!(11)) * 100;
                        End If
                        If careerTeamTotal!(11) = 0 Then
                            Print Tab(53); " 0.0";
                        End If
                        If careerTeamTotal!(10) > 0 Then
                            Print Tab(53); Using "##.#"; careerTeamTotal!(14) / careerTeamTotal!(10);
                        End If

                        If careerTeamTotal!(11) > 0 Then
                            careerTeamTotal!(30) = Int(careerTeamTotal!(10) / careerTeamTotal!(11) * 1000) / 10
                        End If
                        If careerTeamTotal!(11) = 0 Then
                            careerTeamTotal!(30) = 0
                            careerTeamTotal!(27) = 0
                            careerTeamTotal!(29) = 0
                            careerTeamTotal!(28) = 0
                        Else
                            careerTeamTotal!(27) = Int(careerTeamTotal!(13) / careerTeamTotal!(11) * 1000) / 10
                            careerTeamTotal!(29) = Int(careerTeamTotal!(14) / careerTeamTotal!(11) * 100) / 100
                            careerTeamTotal!(28) = Int(careerTeamTotal!(12) / careerTeamTotal!(11) * 1000) / 10
                        End If

                        PPT! = careerTeamTotal!(30) - 30

                        If PPT! < 0 Then
                            PPT! = 0
                        Else
                            If PPT! > 77.5 Then
                                PPT! = 2.375
                            Else
                                PPT! = PPT! * .05
                            End If
                        End If

                        If careerTeamTotal!(27) > 11.9 Then
                            FFT! = 2.375
                        Else
                            FFT! = careerTeamTotal!(27) * .2
                        End If

                        If careerTeamTotal!(28) = 0 Then
                            IIT! = 0
                        Else
                            If careerTeamTotal!(28) > 9.5 Then
                                IIT! = 0
                            Else
                                IIT! = 2.375 - (.25 * careerTeamTotal!(28))
                            End If
                        End If

                        careerTeamTotal!(29) = careerTeamTotal!(29) - 3

                        If careerTeamTotal!(29) < 0 Then
                            YYT! = 0
                        Else
                            If careerTeamTotal!(29) > 12.5 Then
                                YYT! = 2.375
                            Else
                                D = Int(careerTeamTotal!(29) * 100) / 2
                                If D = careerTeamTotal!(29) / 2 Then
                                    YYT! = careerTeamTotal!(29) / 2 * .3 + careerTeamTotal!(29) / 2 * .2
                                Else
                                    YYT! = careerTeamTotal!(29) / 2 * .3 + careerTeamTotal!(29) / 2 * .2 + .003
                                End If
                            End If
                        End If

                        careerTeamTotal!(31) = PPT! + FFT! + IIT! + YYT!

                        If careerTeamTotal!(31) > 9.5 Then
                            careerTeamTotal!(31) = 158.3
                        Else
                            careerTeamTotal!(31) = careerTeamTotal!(31) * 50 / 3
                        End If

                        careerTeamTotal!(31) = Int(careerTeamTotal!(31) * 10) / 10

                        Print Tab(60); Using "###.#"; careerTeamTotal!(31);
                        Print Tab(70); Using "##"; careerRecVal!(3);
                        Print careerRecPlyr$(3)

                        For J = 10 To 14:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 4

                Locate 4, 1
                Print "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST RET."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 14

                    If carKickRet$(I) <> "" And carKickRet$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carKickRet$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerKRS!(Z, I, 4)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(22); careerKRS!(V, I, 5);
                            'Att
                            Print Tab(32); Using "###"; careerKRS!(V, I, 0);
                            'Yards
                            Print Tab(42); Using "####"; careerKRS!(V, I, 1);

                            'Average
                            If careerKRS!(V, I, 0) = 0 Or careerKRS!(V, I, 1) = 0 Then
                                Print Tab(52); " 00";
                            End If
                            If careerKRS!(V, I, 0) > 0 Then
                                Print Tab(52); Using "##.#"; careerKRS!(V, I, 1) / careerKRS!(V, I, 0);
                            End If

                            '# TDs
                            Print Tab(62); Using "##"; careerKRS!(V, I, 2);
                            'Longest
                            Print Tab(70); Using "###"; careerKRS!(V, I, 3);
                            Print careerKRS$(V, I, 0)
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(15) = careerTeamTotal!(15) + careerKRS!(V, I, 0)
                            careerTeamTotal!(16) = careerTeamTotal!(16) + careerKRS!(V, I, 1)
                            careerTeamTotal!(17) = careerTeamTotal!(17) + careerKRS!(V, I, 2)
                        Next V

                        careerRecVal!(4) = careerKRS!(1, I, 3)
                        careerRecPlyr$(4) = careerKRS$(1, I, 0)

                        For V = 2 To VV
                            If careerKRS!(V, I, 3) >= careerRecVal!(4) Then
                                careerRecVal!(4) = careerKRS!(V, I, 3)
                                careerRecPlyr$(4) = careerKRS$(V, I, 0)
                            End If
                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(25); Using "##"; VV;
                        Print Tab(32); Using "###"; careerTeamTotal!(15);
                        Print Tab(41); Using "#####"; careerTeamTotal!(16);

                        If careerTeamTotal!(15) = 0 Or careerTeamTotal!(16) = 0 Then
                            Print Tab(52); " 00";
                        End If
                        If careerTeamTotal!(15) > 0 Then
                            Print Tab(52); Using "##.#"; careerTeamTotal!(16) / careerTeamTotal!(15);
                        End If

                        Print Tab(61); Using "###"; careerTeamTotal!(17);
                        Print Tab(70); Using "##"; careerRecVal!(4);
                        Print careerRecPlyr$(4)

                        For J = 15 To 17:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 5

                Locate 4, 1
                Print "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST RET."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 14

                    If carPRet$(I) <> "" And carPRet$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carPRet$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerPRS!(Z, I, 4)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(22); careerPRS!(V, I, 5);
                            'Att
                            Print Tab(32); Using "###"; careerPRS!(V, I, 0);
                            'Yards
                            Print Tab(42); Using "####"; careerPRS!(V, I, 1);

                            'Average
                            If careerPRS!(V, I, 0) = 0 Or careerPRS!(V, I, 1) = 0 Then
                                Print Tab(52); " 00";
                            End If
                            If careerPRS!(V, I, 0) > 0 Then
                                Print Tab(52); Using "##.#"; careerPRS!(V, I, 1) / careerPRS!(V, I, 0);
                            End If

                            'TDs
                            Print Tab(62); Using "##"; careerPRS!(V, I, 2);
                            'Longest
                            Print Tab(70); Using "###"; careerPRS!(V, I, 3);
                            Print careerPRS$(V, I, 0)
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(18) = careerTeamTotal!(18) + careerPRS!(V, I, 0)
                            careerTeamTotal!(19) = careerTeamTotal!(19) + careerPRS!(V, I, 1)
                            careerTeamTotal!(20) = careerTeamTotal!(20) + careerPRS!(V, I, 2)
                        Next V

                        careerRecVal!(5) = careerPRS!(1, I, 3)
                        careerRecPlyr$(5) = careerPRS$(1, I, 0)

                        For V = 2 To VV
                            If careerPRS!(V, I, 3) >= careerRecVal!(5) Then
                                careerRecVal!(5) = careerPRS!(V, I, 3)
                                careerRecPlyr$(5) = careerPRS$(V, I, 0)
                            End If
                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(25); Using "##"; VV;
                        Print Tab(32); Using "###"; careerTeamTotal!(18);
                        Print Tab(41); Using "#####"; careerTeamTotal!(19);

                        If careerTeamTotal!(18) = 0 Or careerTeamTotal!(19) = 0 Then
                            Print Tab(52); " 00";
                        End If
                        If careerTeamTotal!(18) > 0 Then
                            Print Tab(52); Using "##.#"; careerTeamTotal!(19) / careerTeamTotal!(18);
                        End If

                        Print Tab(61); Using "###"; careerTeamTotal!(20);
                        Print Tab(70); Using "##"; careerRecVal!(5);
                        Print careerRecPlyr$(5)

                        For J = 18 To 20:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 6

                Locate 4, 1
                Print "PUNTERS           YEAR     ATT.     YARDS      AVG."

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 9

                    If carPunter$(I) <> "" And carPunter$(I) <> "XXX" Then

                        Locate 5: Color 15: Print Left$(carPunter$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerPUS!(Z, I, 2)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(18); careerPUS!(V, I, 3);
                            'Att
                            Print Tab(28); Using "###"; careerPUS!(V, I, 0);
                            'Yards
                            Print Tab(38); Using "####"; careerPUS!(V, I, 1);

                            'Average
                            If careerPUS!(V, I, 0) = 0 Or careerPUS!(V, I, 1) = 0 Then
                                Print Tab(47); "00.0";
                            End If
                            If careerPUS!(V, I, 0) > 0 Then
                                Print Tab(47); Using "##.#"; careerPUS!(V, I, 1) / careerPUS!(V, I, 0);
                            End If
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(21) = careerTeamTotal!(21) + careerPUS!(V, I, 0)
                            careerTeamTotal!(22) = careerTeamTotal!(22) + careerPUS!(V, I, 1)
                        Next V

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(21); Using "##"; VV;
                        Print Tab(27); Using "####"; careerTeamTotal!(21);
                        Print Tab(37); Using "#####"; careerTeamTotal!(22);

                        If careerTeamTotal!(21) = 0 Or careerTeamTotal!(22) = 0 Then
                            Print Tab(47); " 00"
                        End If
                        If careerTeamTotal!(21) > 0 Then
                            Print Tab(47); Using "##.#"; careerTeamTotal!(22) / careerTeamTotal!(21)
                        End If

                        For J = 21 To 22:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 7

                Locate 4, 1
                Print "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49  50+"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 9

                    If carKicker$(I) <> "" And carKicker$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carKicker$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerFGS!(Z, I, 15)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(13); careerFGS!(V, I, 16);
                            'EPA
                            Print Tab(19); Using "###"; careerFGS!(V, I, 3);
                            'EPM
                            Print Tab(23); Using "###"; careerFGS!(V, I, 2);

                            'EP %
                            If careerFGS!(V, I, 2) = 0 Or careerFGS!(V, I, 3) = 0 Then
                                Print Tab(27); "  0";
                            End If
                            If careerFGS!(V, I, 2) > 0 Then
                                Print Tab(27); Using "###"; (careerFGS!(V, I, 2) / careerFGS!(V, I, 3)) * 100;
                            End If

                            'FGA
                            Print Tab(32); Using "###"; careerFGS!(V, I, 1);
                            'FGM
                            Print Tab(36); Using "###"; careerFGS!(V, I, 0);

                            'FG%
                            If careerFGS!(V, I, 0) = 0 Or careerFGS!(V, I, 1) = 0 Then
                                Print Tab(40); "  0";
                            End If
                            If careerFGS!(V, I, 0) > 0 Then
                                Print Tab(40); Using "###"; (careerFGS!(V, I, 0) / careerFGS!(V, I, 1)) * 100;
                            End If

                            'Longest FG
                            Print Tab(46); Using "##"; careerFGS!(V, I, 4);

                            'FGs 0-19 Yds
                            Print Tab(49); Using "##"; careerFGS!(V, I, 5);
							Print "-";
                            Print Tab(52); Using "##"; careerFGS!(V, I, 10);

                            'FGs 20-29 Yds
                            Print Tab(55); Using "##"; careerFGS!(V, I, 6);
							Print "-";
                            Print Tab(58); Using "##"; careerFGS!(V, I, 11);

                            'FGs 30-39 Yds
                            Print Tab(61); Using "##"; careerFGS!(V, I, 7);
							Print "-";
                            Print Tab(64); Using "##"; careerFGS!(V, I, 12);

                            'FGs 40-49 Yds
                            Print Tab(67); Using "##"; careerFGS!(V, I, 8);
							Print "-";
                            Print Tab(70); Using "##"; careerFGS!(V, I, 13);

                            'FGs 50+ Yds
                            Print Tab(73); Using "##"; careerFGS!(V, I, 9);
							Print "-";
                            Print Tab(76); Using "##"; careerFGS!(V, I, 14)

                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(23) = careerTeamTotal!(23) + careerFGS!(V, I, 0)
                            careerTeamTotal!(24) = careerTeamTotal!(24) + careerFGS!(V, I, 1)
                            careerTeamTotal!(34) = careerTeamTotal!(34) + careerFGS!(V, I, 2)
                            careerTeamTotal!(35) = careerTeamTotal!(35) + careerFGS!(V, I, 3)
                            careerTeamTotal!(36) = careerTeamTotal!(36) + careerFGS!(V, I, 5)
                            careerTeamTotal!(37) = careerTeamTotal!(37) + careerFGS!(V, I, 6)
                            careerTeamTotal!(38) = careerTeamTotal!(38) + careerFGS!(V, I, 7)
                            careerTeamTotal!(39) = careerTeamTotal!(39) + careerFGS!(V, I, 8)
                            careerTeamTotal!(40) = careerTeamTotal!(40) + careerFGS!(V, I, 9)
                            careerTeamTotal!(41) = careerTeamTotal!(41) + careerFGS!(V, I, 10)
                            careerTeamTotal!(42) = careerTeamTotal!(42) + careerFGS!(V, I, 11)
                            careerTeamTotal!(43) = careerTeamTotal!(43) + careerFGS!(V, I, 12)
                            careerTeamTotal!(44) = careerTeamTotal!(44) + careerFGS!(V, I, 13)
                            careerTeamTotal!(45) = careerTeamTotal!(45) + careerFGS!(V, I, 14)
                        Next V

                        careerRecVal!(6) = careerFGS!(1, I, 4)

                        For V = 2 To VV
                            If careerFGS!(V, I, 4) >= careerRecVal!(6) Then
                                careerRecVal!(6) = careerFGS!(V, I, 4)
                            End If
                        Next

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(16); VV;
                        Print Tab(19); Using "###"; careerTeamTotal!(35);
                        Print Tab(23); Using "###"; careerTeamTotal!(34);

                        If careerTeamTotal!(34) = 0 Or careerTeamTotal!(35) = 0 Then
                            Print Tab(27); "  0";
                        End If
                        If careerTeamTotal!(34) > 0 Then
                            Print Tab(27); Using "###"; (careerTeamTotal!(34) / careerTeamTotal!(35)) * 100;
                        End If

                        Print Tab(32); Using "###"; careerTeamTotal!(24);
                        Print Tab(36); Using "###"; careerTeamTotal!(23);

                        If careerTeamTotal!(23) = 0 Or careerTeamTotal!(24) = 0 Then
                            Print Tab(40); "  0";
                        End If
                        If careerTeamTotal!(23) > 0 Then
                            Print Tab(40); Using "###"; (careerTeamTotal!(23) / careerTeamTotal!(24)) * 100;
                        End If

                        Print Tab(46); Using "##"; careerRecVal!(6);

                        Print Tab(49); Using "##"; careerTeamTotal!(36);
						Print "-";
                        Print Tab(52); Using "##"; careerTeamTotal!(41);

                        Print Tab(55); Using "##"; careerTeamTotal!(37);
						Print "-";
                        Print Tab(58); Using "##"; careerTeamTotal!(42);

                        Print Tab(61); Using "##"; careerTeamTotal!(38);
						Print "-";
                        Print Tab(64); Using "##"; careerTeamTotal!(43);

                        Print Tab(67); Using "##"; careerTeamTotal!(39);
						Print "-";
                        Print Tab(70); Using "##"; careerTeamTotal!(44);

                        Print Tab(73); Using "##"; careerTeamTotal!(40);
						Print "-";
                        Print Tab(76); Using "##"; careerTeamTotal!(45)

                        For J = 23 To 45:
                            careerTeamTotal!(J) = 0
                        Next

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(90, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 8

                Locate 4, 1
                Print "INTERCEPTORS        YEAR     INTERCEPTIONS"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 54

                    If carIntName$(I) <> "" And carIntName$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carIntName$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerGIS!(Z, I, 1)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(20); careerGIS!(V, I, 2);
                            '# Interceptions
                            Print Tab(34); Using "###"; careerGIS!(V, I, 0);
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(32) = careerTeamTotal!(32) + careerGIS!(V, I, 0)
                        Next V

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(23); Using "##"; VV;
                        Print Tab(33); Using "####"; careerTeamTotal!(32)

                        careerTeamTotal!(32) = 0

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

            Case 9

                Locate 4, 1
                Print "DEFENSIVE SACKS     YEAR        SACKS"

                If RT = 1 Then start% = 0
                If RT = 2 Then start% = B9

                For I = start% To 54

                    If carSackName$(I) <> "" And carSackName$(I) <> "XXX" Then

                        Color 15
                        Locate 5: Print Left$(carSackName$(I), 13);

                        VV = 0

                        For Z = 1 To MAX_CAREER_YEARS:
                            VV = VV + careerGSS!(Z, I, 1)
                        Next

                        For V = 1 To VV
                            Color 7
                            'Year
                            Print Tab(20); careerGSS!(V, I, 2);
                            '# Sacks
                            Print Tab(34); Using "###"; careerGSS!(V, I, 0);
                        Next V

                        For V = 1 To VV
                            careerTeamTotal!(33) = careerTeamTotal!(33) + careerGSS!(V, I, 0)
                        Next V

                        Print
                        Color 3
                        Print
                        Print "TOTALS";
                        Print Tab(23); Using "##"; VV;
                        Print Tab(33); Using "####"; careerTeamTotal!(33)

                        careerTeamTotal!(33) = 0

                        Color 14
                        Locate 9 + VV, 16
                        Print "HIT ENTER TO CONTINUE OR ESC TO GO TO MAIN MENU"

                        Do
                            I$ = GetKeyPress$
                        Loop Until I$ = Chr$(13) Or I$ = Chr$(27)

                        If I$ = Chr$(13) Then
                            For T = 5 To 22:
                                Locate T
                                Print String$(76, " ")
                            Next
                        Else
                            escPressed% = 1: Exit For
                        End If

                    End If

                Next I

        End Select

        If escPressed% = 0 Then

            Color 14

            Do

                Locate 9 + VV, 16
                Print Space$(47)

                Locate 9 + VV, 20
                Print "VIEW AGAIN? (Y/N)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N" Or escPressed% = 1 Or targetFile$ <> ""

End Sub

