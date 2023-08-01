Sub CARPRNT ()

    _Title "3-in-1 Football - Print Career Files"

    Shared careerA1$(), careerA2$(), careerA3$(), careerA4$(), careerA5$(), careerA6$(), careerA7$()
    Shared careerDI$(), careerDS$()

    Shared careerKRS$(), careerPRS$(), careerQBS$(), careerRBS$(), careerWRS$()
    Shared careerRC$()

    Shared careerFF!()
    
    Shared careerGIS!(), careerGSS!()
    Shared careerFGS!(), careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Shared careerII!(), careerPP!(), careerTT!(), careerYY!()
    Shared careerRC!()

    Dim statsGS%(1, 55), statsGI%(1, 55)

    careerFile$ = ChooseFile$ (diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        TMR = 6420

        Color 15, 0
        Cls
        Locate 12, 10: Print "LOADING CAREER FILE . . . . PLEASE WAIT"

        Open diskPaths$(1) + careerFile$ + ".CAR" For Input As #1

        Input #1, TN$, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

        For I = 0 To 49
            Input #1, careerA1$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 39
            Input #1, careerA2$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 19
            Input #1, careerA3$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 14
            Input #1, careerA4$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 14
            Input #1, careerA5$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 9
            Input #1, careerA6$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 9
            Input #1, careerA7$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 54
            Input #1, careerDI$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next
        
        For I = 0 To 54
            Input #1, careerDS$(I)
            Locate 21, 38
            Color 5
            TMR = TMR - 1: Print Using "####"; TMR
        Next

        For N = 1 To 15:
            For J = 0 To 49:
                For K = 0 To 9:
                    Input #1, careerRBS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 49:
                Input #1, careerRBS$(N, J, 0), careerRBS$(N, J, 1)
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 39:

                For K = 0 To 5:
                    Input #1, careerWRS!(N, J, K)
                Next K

                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR

            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 39:
                Input #1, careerWRS$(N, J, 0)
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 19:
                For K = 0 To 7:
                    Input #1, careerQBS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 19:
                Input #1, careerQBS$(N, J, 0)
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 14:
                For K = 0 To 5:
                    Input #1, careerKRS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR:
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 14:
                Input #1, careerKRS$(N, J, 0)
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 14:
                For K = 0 To 5:
                    Input #1, careerPRS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 14:
                Input #1, careerPRS$(N, J, 0)
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 9:
                For K = 0 To 3:
                    Input #1, careerPUS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 9:
                For K = 0 To 16:
                    Input #1, careerFGS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 54:
                For K = 0 To 2:
                    Input #1, careerGIS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        For N = 1 To 15:
            For J = 0 To 54:
                For K = 0 To 2:
                    Input #1, careerGSS!(N, J, K)
                Next K
                Locate 21, 38
                Color 5
                TMR = TMR - 1: Print Using "####"; TMR
            Next J
        Next N

        Close #1

        Color 0, 15

        Open "REPORT" For Output As #2
        Print #2, careerFile$
        Print #2, "RUSHER         YEAR   ATT   YARDS AVERAGE  TDS   REC  YDS  AVG  TDS RUN-LONG-REC"

        For I = 0 To 50

            If careerA1$(I) <> "" And careerA1$(I) <> "XXX" Then

                VV = careerRBS!(1, I, 7) + careerRBS!(2, I, 7) + careerRBS!(3, I, 7) + careerRBS!(4, I, 7) + careerRBS!(5, I, 7) + careerRBS!(6, I, 7) + careerRBS!(7, I, 7) + careerRBS!(7, I, 7) + careerRBS!(9, I, 7) + careerRBS!(10, I, 7) + careerRBS!(11, I, 7) + careerRBS!(12, I, 7) + careerRBS!(13, I, 7) + careerRBS!(14, I, 7) + careerRBS!(15, I, 7)

                Print #2, Left$(careerA1$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(15); careerRBS!(V, I, 8);
                    Print #2, Tab(23); Using "###"; careerRBS!(V, I, 0);
                    Print #2, Tab(30); Using "####"; careerRBS!(V, I, 1);

                    If careerRBS!(V, I, 1) = 0 Or careerRBS!(V, I, 1) = 0 Then Print #2, Tab(37); " 0.0";
                    If careerRBS!(V, I, 0) > 0 Then Print #2, Tab(37); Using "##.#"; careerRBS!(V, I, 1) / careerRBS!(V, I, 0);

                    Print #2, Tab(44); Using "###"; careerRBS!(V, I, 2);
                    Print #2, Tab(50); Using "###"; careerRBS!(V, I, 3);
                    Print #2, Tab(54); Using "####"; careerRBS!(V, I, 4);

                    If careerRBS!(V, I, 4) = 0 Or careerRBS!(V, I, 3) = 0 Then Print #2, Tab(59); " 0.0";
                    If careerRBS!(V, I, 3) > 0 Then Print #2, Tab(59); Using "##.#"; careerRBS!(V, I, 4) / careerRBS!(V, I, 3);

                    Print #2, Tab(65); Using "##"; careerRBS!(V, I, 5);
                    Print #2, Tab(71); Using "##"; careerRBS!(V, I, 6);
                    Print #2, careerRBS$(V, I, 0);
                    Print #2, Tab(77); Using "##"; careerRBS!(V, I, 9);
                    Print #2, careerRBS$(V, I, 1)
                Next V

                careerRC!(0) = 0
                careerRC!(1) = 0
                careerRC$(0) = ""
                careerRC$(1) = ""

                For V = 1 To VV
                    TT1 = TT1 + careerRBS!(V, I, 0)
                    TT2 = TT2 + careerRBS!(V, I, 1)
                    TT3 = TT3 + careerRBS!(V, I, 2)
                    TT4 = TT4 + careerRBS!(V, I, 3)
                    TT5 = TT5 + careerRBS!(V, I, 4)
                    TT6 = TT6 + careerRBS!(V, I, 5)

                    If careerRBS!(V, I, 6) > careerRC!(0) Then
                        careerRC!(0) = careerRBS!(V, I, 6)
                        careerRC$(0) = careerRBS$(V, I, 0)
                    Else
                        If careerRBS!(V, I, 9) > careerRC!(1) Then
                            careerRC!(1) = careerRBS!(V, I, 9)
                            careerRC$(1) = careerRBS$(V, I, 1)
                        End If
                    End If

                Next V

                Print #2, "TOTALS";
                Print #2, Tab(18); Using "##"; VV;
                Print #2, Tab(22); Using "####"; TT1;
                Print #2, Tab(29); Using "#####"; TT2;

                If TT1 = 0 Or TT2 = 0 Then Print #2, Tab(37); " 0.0";
                If TT1 > 0 Then Print #2, Tab(37); Using "##.#"; TT2 / TT1;

                Print #2, Tab(44); Using "###"; TT3;
                Print #2, Tab(49); Using "####"; TT4;
                Print #2, Tab(53); Using "#####"; TT5;

                If TT4 = 0 Or TT5 = 0 Then Print #2, Tab(59); " 0.0";
                If TT4 > 0 Then Print #2, Tab(59); Using "##.#"; TT5 / TT4;

                Print #2, Tab(64); Using "###"; TT6;
                Print #2, Tab(71); Using "##"; careerRC!(0);
                Print #2, careerRC$(0);
                Print #2, Tab(77); Using "##"; careerRC!(1);
                Print #2, careerRC$(1)

                TT1 = 0
                TT2 = 0
                TT3 = 0
                TT4 = 0
                TT5 = 0
                TT6 = 0

                Print #2,

            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "RECEIVER        YEAR        REC.      YARDS      AVG.     TD  LONGEST REC."

        For I = 0 To 40

            If careerA2$(I) <> "" And careerA2$(I) <> "XXX" Then

                VV = careerWRS!(1, I, 4) + careerWRS!(2, I, 4) + careerWRS!(3, I, 4) + careerWRS!(4, I, 4) + careerWRS!(5, I, 4) + careerWRS!(6, I, 4) + careerWRS!(7, I, 4) + careerWRS!(8, I, 4) + careerWRS!(9, I, 4) + careerWRS!(10, I, 4) + careerWRS!(11, I, 4) + careerWRS!(12, I, 4) + careerWRS!(13, I, 4) + careerWRS!(14, I, 4) + careerWRS!(15, I, 4)

                Print #2, Left$(careerA2$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(16); careerWRS!(V, I, 5);
                    Print #2, Tab(29); Using "###"; careerWRS!(V, I, 0);
                    Print #2, Tab(40); Using "####"; careerWRS!(V, I, 1);

                    If careerWRS!(V, I, 0) = 0 Or careerWRS!(V, I, 1) = 0 Then Print #2, Tab(49); " 0.0";
                    If careerWRS!(V, I, 0) > 0 Then Print #2, Tab(49); Using "##.#"; careerWRS!(V, I, 1) / careerWRS!(V, I, 0);

                    Print #2, Tab(59); Using "##"; careerWRS!(V, I, 2);
                    Print #2, Tab(66); Using "##"; careerWRS!(V, I, 3);
                    Print #2, careerWRS$(V, I, 0)
                Next V

                careerRC!(0) = 0
                careerRC$(0) = ""

                For V = 1 To VV
                    TT7 = TT7 + careerWRS!(V, I, 0)
                    TT8 = TT8 + careerWRS!(V, I, 1)
                    TT9 = TT9 + careerWRS!(V, I, 2)

                    If careerWRS!(V, I, 3) > careerRC!(0) Then
                        careerRC!(0) = careerWRS!(V, I, 3)
                        careerRC$(0) = careerWRS$(V, I, 0)
                    End If

                Next V

                Print #2, "TOTALS";
                Print #2, Tab(19); Using "##"; VV;
                Print #2, Tab(28); Using "####"; TT7;
                Print #2, Tab(39); Using "#####"; TT8;

                If TT7 = 0 Or TT8 = 0 Then Print #2, Tab(49); " 0.0";
                If TT7 > 0 Then Print #2, Tab(49); Using "##.#"; TT8 / TT7;

                Print #2, Tab(58); Using "###"; TT9;
                Print #2, Tab(66); Using "##"; careerRC!(0);
                Print #2, careerRC$(0)

                TT7 = 0
                TT8 = 0
                TT9 = 0

                Print #2,

            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "QUARTERBACK  YEAR  COMP.    ATT.    INT  TDS   YARDS  PCT. YD/COMP  RATING  LONG"

        For I = 0 To 19

            If careerA3$(I) <> "" And careerA3$(I) <> "XXX" Then

                VV = careerQBS!(1, I, 6) + careerQBS!(2, I, 6) + careerQBS!(3, I, 6) + careerQBS!(4, I, 6) + careerQBS!(5, I, 6) + careerQBS!(6, I, 6) + careerQBS!(7, I, 6) + careerQBS!(8, I, 6) + careerQBS!(9, I, 6) + careerQBS!(10, I, 6) + careerQBS!(11, I, 6) + careerQBS!(12, I, 6) + careerQBS!(13, I, 6) + careerQBS!(14, I, 6) + careerQBS!(15, I, 6)

                Print #2, Left$(careerA3$(I), 13);

                For V = 1 To VV

                    Print #2, Tab(13); careerQBS!(V, I, 7);
                    Print #2, Tab(21); Using "###"; careerQBS!(V, I, 1);
                    Print #2, Tab(29); Using "###"; careerQBS!(V, I, 0);
                    Print #2, Tab(38); Using "##"; careerQBS!(V, I, 2);
                    Print #2, Tab(43); Using "##"; careerQBS!(V, I, 4);
                    Print #2, Tab(49); Using "####"; careerQBS!(V, I, 3);

                    If careerQBS!(V, I, 1) = 0 Or careerQBS!(V, I, 0) = 0 Then Print #2, Tab(56); " 00";
                    If careerQBS!(V, I, 1) > 0 Then Print #2, Tab(56); Using "##"; (careerQBS!(V, I, 1) / careerQBS!(V, I, 0)) * 100;
                    If careerQBS!(V, I, 0) = 0 Then Print #2, Tab(63); " 0.0";
                    If careerQBS!(V, I, 1) > 0 Then Print #2, Tab(63); Using "##.#"; careerQBS!(V, I, 3) / careerQBS!(V, I, 1);

                    If careerQBS!(V, I, 0) > 0 Then careerQBS!(V, I, 11) = Int(careerQBS!(V, I, 1) / careerQBS!(V, I, 0) * 1000) / 10

                    If careerQBS!(V, I, 0) = 0 Then
                        careerQBS!(V, I, 11) = 0
                        careerQBS!(V, I, 8) = 0
                        careerQBS!(V, I, 10) = 0
                        careerQBS!(V, I, 9) = 0
                    Else
                        careerQBS!(V, I, 8) = Int(careerQBS!(V, I, 5) / careerQBS!(V, I, 0) * 1000) / 10
                        careerQBS!(V, I, 10) = Int(careerQBS!(V, I, 4) / careerQBS!(V, I, 0) * 100) / 100
                        careerQBS!(V, I, 9) = Int(careerQBS!(V, I, 3) / careerQBS!(V, I, 0) * 1000) / 10
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

                    Print #2, Tab(70); Using "###.#"; careerQBS!(V, I, 12);
                    Print #2, Tab(77); Using "##"; careerQBS!(V, I, 5);
                    Print #2, careerQBS$(V, I, 0)

                Next V

                careerRC!(0) = 0
                careerRC$(0) = ""

                For V = 1 To VV
                    TT10 = TT10 + careerQBS!(V, I, 1)
                    TT11 = TT11 + careerQBS!(V, I, 0)
                    TT12 = TT12 + careerQBS!(V, I, 2)
                    TT13 = TT13 + careerQBS!(V, I, 4)
                    TT14 = TT14 + careerQBS!(V, I, 3)

                    If careerQBS!(V, I, 5) > careerRC!(0) Then
                        careerRC!(0) = careerQBS!(V, I, 5)
                        careerRC$(0) = careerQBS$(V, I, 0)
                    End If

                Next V

                Print #2, "TOTALS";
                Print #2, Tab(16); Using "##"; VV;
                Print #2, Tab(20); Using "####"; TT10;
                Print #2, Tab(28); Using "####"; TT11;
                Print #2, Tab(37); Using "###"; TT12;
                Print #2, Tab(41); Using "####"; TT13;
                Print #2, Tab(48); Using "#####"; TT14;

                If TT10 = 0 Or TT11 = 0 Then Print #2, Tab(56); " 00";
                If TT10 > 0 Then Print #2, Tab(56); Using "##"; (TT10 / TT11) * 100;
                If TT11 = 0 Then Print #2, Tab(63); " 0.0";
                If TT10 > 0 Then Print #2, Tab(63); Using "##.#"; TT14 / TT10;

                If TT11 > 0 Then TT30 = Int(TT10 / TT11 * 1000) / 10
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

                Print #2, Tab(70); Using "###.#"; TT31;
                Print #2, Tab(77); Using "##"; careerRC!(0);
                Print #2, careerRC$(0)

                TT10 = 0
                TT11 = 0
                TT12 = 0
                TT13 = 0
                TT14 = 0

                Print #2,

            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "KICK RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

        For I = 0 To 14

            If careerA4$(I) <> "" And careerA4$(I) <> "XXX" Then

                VV = careerKRS!(1, I, 4) + careerKRS!(2, I, 4) + careerKRS!(3, I, 4) + careerKRS!(4, I, 4) + careerKRS!(5, I, 4) + careerKRS!(6, I, 4) + careerKRS!(7, I, 4) + careerKRS!(8, I, 4) + careerKRS!(9, I, 4) + careerKRS!(10, I, 4) + careerKRS!(11, I, 4) + careerKRS!(12, I, 4) + careerKRS!(13, I, 4) + careerKRS!(14, I, 4) + careerKRS!(15, I, 4)

                Print #2, Left$(careerA4$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(22); careerKRS!(V, I, 5);
                    Print #2, Tab(32); Using "###"; careerKRS!(V, I, 0);
                    Print #2, Tab(42); Using "####"; careerKRS!(V, I, 1);

                    If careerKRS!(V, I, 0) = 0 Or careerKRS!(V, I, 1) = 0 Then Print #2, Tab(52); " 00";
                    If careerKRS!(V, I, 0) > 0 Then Print #2, Tab(52); Using "##.#"; careerKRS!(V, I, 1) / careerKRS!(V, I, 0);

                    Print #2, Tab(62); Using "##"; careerKRS!(V, I, 2);
                    Print #2, Tab(70); Using "###"; careerKRS!(V, I, 3);
                    Print #2, careerKRS$(V, I, 0)
                Next V

                careerRC!(0) = 0
                careerRC$(0) = ""

                For V = 1 To VV
                    TT15 = TT15 + careerKRS!(V, I, 0)
                    TT16 = TT16 + careerKRS!(V, I, 1)
                    TT17 = TT17 + careerKRS!(V, I, 2)

                    If careerKRS!(V, I, 3) > careerRC!(0) Then
                        careerRC!(0) = careerKRS!(V, I, 3)
                        careerRC$(0) = careerKRS$(V, I, 0)
                    End If

                Next V

                Print #2, "TOTALS";
                Print #2, Tab(25); Using "##"; VV;
                Print #2, Tab(32); Using "###"; TT15;
                Print #2, Tab(41); Using "#####"; TT16;

                If TT15 = 0 Or TT16 = 0 Then Print #2, Tab(52); " 00";
                If TT15 > 0 Then Print #2, Tab(52); Using "##.#"; TT16 / TT15;

                Print #2, Tab(61); Using "###"; TT17;
                Print #2, Tab(70); Using "###"; careerRC!(0);: Print #2, careerRC$(0)

                TT15 = 0
                TT16 = 0
                TT17 = 0

                Print #2,
            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "PUNT RETURNERS        YEAR     ATT.     YARDS      AVG.     TDS  LONGEST"

        For I = 0 To 14

            If careerA5$(I) <> "" And careerA5$(I) <> "XXX" Then

                VV = careerPRS!(1, I, 4) + careerPRS!(2, I, 4) + careerPRS!(3, I, 4) + careerPRS!(4, I, 4) + careerPRS!(5, I, 4) + careerPRS!(6, I, 4) + careerPRS!(7, I, 4) + careerPRS!(8, I, 4) + careerPRS!(9, I, 4) + careerPRS!(10, I, 4) + careerPRS!(11, I, 4) + careerPRS!(12, I, 4) + careerPRS!(13, I, 4) + careerPRS!(14, I, 4) + careerPRS!(15, I, 4)

                Print #2, Left$(careerA5$(I), 13);

                For V = 1 To VV

                    Print #2, Tab(22); careerPRS!(V, I, 5);
                    Print #2, Tab(32); Using "###"; careerPRS!(V, I, 0);
                    Print #2, Tab(42); Using "####"; careerPRS!(V, I, 1);

                    If careerPRS!(V, I, 0) = 0 Or careerPRS!(V, I, 1) = 0 Then Print #2, Tab(52); " 00";
                    If careerPRS!(V, I, 0) > 0 Then Print #2, Tab(52); Using "##.#"; careerPRS!(V, I, 1) / careerPRS!(V, I, 0);

                    Print #2, Tab(62); Using "##"; careerPRS!(V, I, 2);
                    Print #2, Tab(70); Using "###"; careerPRS!(V, I, 3);: Print #2, careerPRS$(V, I, 0)

                Next V

                careerRC!(0) = 0
                careerRC$(0) = ""

                For V = 1 To VV
                    TT18 = TT18 + careerPRS!(V, I, 0)
                    TT19 = TT19 + careerPRS!(V, I, 1)
                    TT20 = TT20 + careerPRS!(V, I, 2)
                    If careerPRS!(V, I, 3) > careerRC!(0) Then
                        careerRC!(0) = careerPRS!(V, I, 3)
                        careerRC$(0) = careerPRS$(V, I, 0)
                    End If

                Next V

                Print #2, "TOTALS";
                Print #2, Tab(25); Using "##"; VV;
                Print #2, Tab(32); Using "###"; TT18;
                Print #2, Tab(41); Using "#####"; TT19;

                If TT18 = 0 Or TT19 = 0 Then Print #2, Tab(52); " 00";
                If TT18 > 0 Then Print #2, Tab(52); Using "##.#"; TT19 / TT18;

                Print #2, Tab(61); Using "###"; TT20;
                Print #2, Tab(70); Using "###"; careerRC!(0);: Print #2, careerRC$(0)

                TT18 = 0: TT19 = 0: TT20 = 0

                Print #2,

            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "PUNTERS           YEAR     ATT.     YARDS      AVG."

        For I = 0 To 9

            If careerA6$(I) <> "" And careerA6$(I) <> "XXX" Then

                VV = careerPUS!(1, I, 2) + careerPUS!(2, I, 2) + careerPUS!(3, I, 2) + careerPUS!(4, I, 2) + careerPUS!(5, I, 2) + careerPUS!(6, I, 2) + careerPUS!(7, I, 2) + careerPUS!(8, I, 2) + careerPUS!(9, I, 2) + careerPUS!(10, I, 2) + careerPUS!(11, I, 2) + careerPUS!(12, I, 2) + careerPUS!(13, I, 2) + careerPUS!(14, I, 2) + careerPUS!(15, I, 2)

                Print #2, Left$(careerA6$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(18); careerPUS!(V, I, 3);
                    Print #2, Tab(28); Using "###"; careerPUS!(V, I, 0);
                    Print #2, Tab(38); Using "####"; careerPUS!(V, I, 1);
                    If careerPUS!(V, I, 0) = 0 Or careerPUS!(V, I, 1) = 0 Then Print #2, Tab(47); "00.0"
                    If careerPUS!(V, I, 0) > 0 Then Print #2, Tab(47); Using "##.#"; careerPUS!(V, I, 1) / careerPUS!(V, I, 0)
                Next V

                For V = 1 To VV
                    TT21 = TT21 + careerPUS!(V, I, 0)
                    TT22 = TT22 + careerPUS!(V, I, 1)
                Next V

                Print #2, "TOTALS";
                Print #2, Tab(21); Using "##"; VV;
                Print #2, Tab(27); Using "####"; TT21;
                Print #2, Tab(37); Using "#####"; TT22;

                If TT21 = 0 Or TT22 = 0 Then Print #2, Tab(47); " 00"
                If TT21 > 0 Then Print #2, Tab(47); Using "##.#"; TT22 / TT21

                TT21 = 0
                TT22 = 0

                Print #2,

            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "KICKERS      YEAR EPA EPM PCT. FGA FGM PCT. LFG  0-19 20-29 30-39 40-49 50- +"

        For I = 0 To 9

            If careerA7$(I) <> "" And careerA7$(I) <> "XXX" Then

                VV = careerFGS!(1, I, 15) + careerFGS!(2, I, 15) + careerFGS!(3, I, 15) + careerFGS!(4, I, 15) + careerFGS!(5, I, 15) + careerFGS!(6, I, 15) + careerFGS!(7, I, 15) + careerFGS!(8, I, 15) + careerFGS!(9, I, 15) + careerFGS!(10, I, 15) + careerFGS!(11, I, 15) + careerFGS!(12, I, 15) + careerFGS!(13, I, 15) + careerFGS!(14, I, 15) + careerFGS!(15, I, 15)

                Print #2, Left$(careerA7$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(13); careerFGS!(V, I, 16);
                    Print #2, Tab(19); Using "###"; careerFGS!(V, I, 3);
                    Print #2, Tab(23); Using "###"; careerFGS!(V, I, 2);

                    If careerFGS!(V, I, 2) = 0 Or careerFGS!(V, I, 3) = 0 Then Print #2, Tab(27); "  0";
                    If careerFGS!(V, I, 2) > 0 Then Print #2, Tab(27); Using "###"; (careerFGS!(V, I, 2) / careerFGS!(V, I, 3)) * 100;

                    Print #2, Tab(32); Using "###"; careerFGS!(V, I, 1);
                    Print #2, Tab(36); Using "###"; careerFGS!(V, I, 0);

                    If careerFGS!(V, I, 0) = 0 Or careerFGS!(V, I, 1) = 0 Then Print #2, Tab(40); "  0";
                    If careerFGS!(V, I, 0) > 0 Then Print #2, Tab(40); Using "###"; (careerFGS!(V, I, 0) / careerFGS!(V, I, 1)) * 100;

                    Print #2, Tab(46); Using "##"; careerFGS!(V, I, 4);
                    Print #2, Tab(49); Using "##"; careerFGS!(V, I, 5);: Print #2, "-";
                    Print #2, Tab(52); Using "##"; careerFGS!(V, I, 10);
                    Print #2, Tab(55); Using "##"; careerFGS!(V, I, 6);: Print #2, "-";
                    Print #2, Tab(58); Using "##"; careerFGS!(V, I, 11);
                    Print #2, Tab(61); Using "##"; careerFGS!(V, I, 7);: Print #2, "-";
                    Print #2, Tab(64); Using "##"; careerFGS!(V, I, 12);
                    Print #2, Tab(67); Using "##"; careerFGS!(V, I, 8);: Print #2, "-";
                    Print #2, Tab(70); Using "##"; careerFGS!(V, I, 13);
                    Print #2, Tab(73); Using "##"; careerFGS!(V, I, 9);: Print #2, "-";
                    Print #2, Tab(76); Using "##"; careerFGS!(V, I, 14)
                Next V

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

                careerRC!(0) = 0

                For V = 1 To VV
                    If careerFGS!(V, I, 4) >= careerRC!(0) Then
                        careerRC!(0) = careerFGS!(V, I, 4)
                    End If
                Next

                Print #2, "TOTALS";
                Print #2, Tab(16); VV;
                Print #2, Tab(19); Using "###"; careerTT!(35);
                Print #2, Tab(23); Using "###"; careerTT!(34);

                If careerTT!(34) = 0 Or careerTT!(35) = 0 Then Print #2, Tab(27); "  0";
                If careerTT!(34) > 0 Then Print #2, Tab(27); Using "###"; (careerTT!(34) / careerTT!(35)) * 100;

                Print #2, Tab(32); Using "###"; careerTT!(24);
                Print #2, Tab(36); Using "###"; careerTT!(23);

                If careerTT!(23) = 0 Or careerTT!(24) = 0 Then Print #2, Tab(40); "  0";
                If careerTT!(23) > 0 Then Print #2, Tab(40); Using "###"; (careerTT!(23) / careerTT!(24)) * 100;

                Print #2, Tab(46); Using "##"; careerRC!(0);
                Print #2, Tab(49); Using "##"; careerTT!(36);: Print #2, "-";
                Print #2, Tab(52); Using "##"; careerTT!(41);
                Print #2, Tab(55); Using "##"; careerTT!(37);: Print #2, "-";
                Print #2, Tab(58); Using "##"; careerTT!(42);
                Print #2, Tab(61); Using "##"; careerTT!(38);: Print #2, "-";
                Print #2, Tab(64); Using "##"; careerTT!(43);
                Print #2, Tab(67); Using "##"; careerTT!(39);: Print #2, "-";
                Print #2, Tab(70); Using "##"; careerTT!(44);
                Print #2, Tab(73); Using "##"; careerTT!(40);: Print #2, "-";
                Print #2, Tab(76); Using "##"; careerTT!(45)

                For J = 23 To 45: careerTT!(J) = 0: Next

                Print #2,

            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "INTERCEPTORS        YEAR     INTERCEPTIONS"

        For I = 0 To 54

            If careerDI$(I) <> "" And careerDI$(I) <> "XXX" Then
                VV = careerGIS!(1, I, 1) + careerGIS!(2, I, 1) + careerGIS!(3, I, 1) + careerGIS!(4, I, 1) + careerGIS!(5, I, 1) + careerGIS!(6, I, 1) + careerGIS!(7, I, 1) + careerGIS!(8, I, 1) + careerGIS!(9, I, 1) + careerGIS!(10, I, 1) + careerGIS!(11, I, 1) + careerGIS!(12, I, 1) + careerGIS!(13, I, 1) + careerGIS!(14, I, 1) + careerGIS!(15, I, 1)

                Print #2, Left$(careerDI$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(20); careerGIS!(V, I, 2);
                    Print #2, Tab(34); Using "###"; careerGIS!(V, I, 0)
                Next V

                careerTT!(32) = 0

                For V = 1 To VV
                    careerTT!(32) = careerTT!(32) + careerGIS!(V, I, 0)
                Next V

                Print #2, "TOTALS";
                Print #2, Tab(23); Using "##"; VV;
                Print #2, Tab(33); Using "####"; careerTT!(32)
                Print #2,
            End If

        Next I

        Print #2,
        Print #2, careerFile$
        Print #2, "SACKERS             YEAR          SACKS"

        For I = 0 To 54

            If careerDS$(I) <> "" And careerDS$(I) <> "XXX" Then
                VV = careerGSS!(1, I, 1) + careerGSS!(2, I, 1) + careerGSS!(3, I, 1) + careerGSS!(4, I, 1) + careerGSS!(5, I, 1) + careerGSS!(6, I, 1) + careerGSS!(7, I, 1) + careerGSS!(8, I, 1) + careerGSS!(9, I, 1) + careerGSS!(10, I, 1) + careerGSS!(11, I, 1) + careerGSS!(12, I, 1) + careerGSS!(13, I, 1) + careerGSS!(14, I, 1) + careerGSS!(15, I, 1)

                Print #2, Left$(careerDS$(I), 13);

                For V = 1 To VV
                    Print #2, Tab(20); careerGSS!(V, I, 2);
                    Print #2, Tab(34); Using "###"; careerGSS!(V, I, 0)
                Next V

                careerTT!(33) = 0

                For V = 1 To VV
                    careerTT!(33) = careerTT!(33) + careerGSS!(V, I, 0)
                Next V

                Print #2, "TOTALS";
                Print #2, Tab(23); Using "##"; VV;
                Print #2, Tab(33); Using "####"; careerTT!(33)
                Print #2,
            End If

        Next I

        Close 2

        Open "REPORT" For Input As #2

        Do While Not EOF(2)
            Line Input #2, X$
            LPrint X$
        Loop

        Close 2

        LPrint Chr$(12)

    End If 'Done checking for ESC

    Color 15, 0

End Sub
