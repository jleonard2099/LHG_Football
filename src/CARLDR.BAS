'----------------------------------------
'            CARLDR Subroutine
'----------------------------------------
'This routine provides a menu for the user
'that allows them to Update and View the
'Career leaders for a specific team
Sub CARLDR ()

    _Title "3-in-1 Football - Career Leaders"

    Do

        Color 15, 0
        Cls

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "UPDATE CAREER LEADERS      "
        menuChoices$(2) = "VIEW / PRINT CAREER LEADERS"
        menuChoices$(3) = "RETURN TO UTILITIES MENU   "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1:
                    Call UpdateCareerLeaders

                Case 2:
                    Call CareerLeaderOptions

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       UpdateCareerLeaders Subroutine
'----------------------------------------
'This routine reads in the current stats,
'calculates the latest values (based on
'the criteria given) and then creates /
'overwrites the TCL file with the newest
'calculated values.
Sub UpdateCareerLeaders ()

    'Arrays to store leader data we generate
    Dim carLdrRBS!(50, 10), carLdrWRS!(40, 6), carLdrQBS!(20, 13)
    Dim carLdrKRS!(15, 6), carLdrPRS!(15, 6), carLdrPUS!(10, 4)
    Dim carLdrFGS!(10, 17), carLdrGIS!(0 To 54, 0 To 2), carLdrGSS!(55, 3)

    Dim carLdrKRS$(15), carLdrPRS$(15)
    Dim carLdrQBS$(20), carLdrRBS$(50), carLdrWRS$(40)

    'Variables to store data loaded from files
    Shared TT, NB, NC, ND, NE, NF, NG, NH, NI, NJ, NK

    Shared carRBacks$(), carWdRec$(), carQBacks$(), carKickRet$(), carPRet$(), carPunter$(), carKicker$()
    Shared intName$(), sackName$()

    Shared careerKRS$(), careerPRS$()
    Shared careerQBS$(), careerRBS$(), careerWRS$()

    Shared careerFGS!(), careerGIS!(), careerGSS!()
    Shared careerKRS!(), careerPRS!(), careerPUS!()
    Shared careerQBS!(), careerRBS!(), careerWRS!()

    Shared carLdrPlyrName$(), carLdrTDIndic$()
    Shared carLdrVal!()

    '1st dim = # career years
    '2nd dim = # career players
    '3rd dim = # of categories
    TMR = (15 * 50 * 10) + (15 * 40 * 6) + (15 * 20 * 8) + (15 * 15 * 6) + (15 * 15 * 6) + (15 * 10 * 4) + (15 * 10 * 17) + (15 * 55 * 3) + (15 * 55 * 3)

    Erase carLdrPlyrName$, carLdrTDIndic$
    Erase carLdrVal!

    careerFile$ = ChooseFile$(diskPaths$(1) + "*.CAR", "CAREER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Call LoadCareerFile(careerFile$)

        Color 14

        Locate 10: Print "COMPUTING LEADER FILE . . . . . . PLEASE WAIT"

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS

            '# Players
            For I = 0 To 49:

                If carRBacks$(I) <> "" And carRBacks$(I) <> "XXX" And carRBacks$(I) <> "END" Then

                    If careerRBS$(N, I, 0) <> "" Then
                        carLdrRBS$(I) = careerRBS$(N, I, 0)
                    End If

                    '# Stats
                    For J = 0 To 9:

                        carLdrRBS!(I, J) = carLdrRBS!(I, J) + careerRBS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Players
        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            For I = 0 To 39:

                If carWdRec$(I) <> "" And carWdRec$(I) <> "XXX" And carWdRec$(I) <> "END" Then

                    If careerWRS$(N, I, 0) <> "" Then
                        carLdrWRS$(I) = careerWRS$(N, I, 0)
                    End If

                    '# Stats
                    For J = 0 To 5

                        carLdrWRS!(I, J) = carLdrWRS!(I, J) + careerWRS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            '# Players
            For I = 0 To 19:

                If carQBacks$(I) <> "" And carQBacks$(I) <> "XXX" And carQBacks$(I) <> "END" Then

                    If careerQBS$(N, I, 0) <> "" Then
                        carLdrQBS$(I) = careerQBS$(N, I, 0)
                    End If

                    Call QBRating_Career(N, I, TT)

                    '# Stats
                    For J = 0 To 12

                        carLdrQBS!(I, J) = carLdrQBS!(I, J) + careerQBS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            '# Players
            For I = 0 To 14:

                If carKickRet$(I) <> "" And carKickRet$(I) <> "XXX" And carKickRet$(I) <> "END" Then

                    If careerKRS$(N, I, 0) <> "" Then
                        carLdrKRS$(I) = careerKRS$(N, I, 0)
                    End If

                    '# Stats
                    For J = 0 To 5

                        carLdrKRS!(I, J) = carLdrKRS!(I, J) + careerKRS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:
        
            '# Players
            For I = 0 To 14:

                If carPRet$(I) <> "" And carPRet$(I) <> "XXX" And carPRet$(I) <> "END" Then

                    If careerPRS$(N, I, 0) <> "" Then
                        carLdrPRS$(I) = careerPRS$(N, I, 0)
                    End If

                    '# Stats
                    For J = 0 To 5

                        carLdrPRS!(I, J) = carLdrPRS!(I, J) + careerPRS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            '# Players
            For I = 0 To 9:

                If carPunter$(I) <> "" And carPunter$(I) <> "XXX" And carPunter$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 3

                        carLdrPUS!(I, J) = carLdrPUS!(I, J) + careerPUS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            '# Players
            For I = 0 To 9:

                If carKicker$(I) <> "" And carKicker$(I) <> "XXX" And carKicker$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 16

                        carLdrFGS!(I, J) = carLdrFGS!(I, J) + careerFGS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            '# Players
            For I = 0 To 54:

                If intName$(I) <> "" And intName$(I) <> "XXX" And intName$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 2

                        carLdrGIS!(I, J) = carLdrGIS!(I, J) + careerGIS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        '# Career Years
        For N = 1 To MAX_CAREER_YEARS:

            '# Players
            For I = 0 To 54:

                If sackName$(I) <> "" And sackName$(I) <> "XXX" And sackName$(I) <> "END" Then

                    '# Stats
                    For J = 0 To 2

                        carLdrGSS!(I, J) = carLdrGSS!(I, J) + careerGSS!(N, I, J)

                        Locate 21, 38
                        Color 5
                        TMR = TMR - 1: Print Using "####"; TMR

                    Next J

                End If

            Next I

        Next N

        ' ##############################
        ' #   Update and sort leaders
        ' ##############################

        ' *** Rushing Categories ***

        '#0 - Rush Att
        '#1 - Rush Yds
        For Z = 0 To 1

            For K = 0 To 49

                If carLdrRBS!(K, Z) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrRBS!(K, Z)
                    carLdrPlyrName$(Z, 19) = carRBacks$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#2 - Rush Avg
        For K = 0 To 49

            If carLdrRBS!(K, 1) / carLdrRBS!(K, 0) > carLdrVal!(2, 19) Then

                carLdrVal!(2, 19) = carLdrRBS!(K, 1) / carLdrRBS!(K, 0)
                carLdrPlyrName$(2, 19) = carRBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(2, J - 1) < carLdrVal!(2, J) Then
                            Swap carLdrVal!(2, J - 1), carLdrVal!(2, J)
                            Swap carLdrPlyrName$(2, J - 1), carLdrPlyrName$(2, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#3 - Rush TDs
        '#4 - Rush Rec
        '#5 - Rush Rec Yds
        For Z = 3 To 5

            For K = 0 To 49

                If carLdrRBS!(K, Z - 1) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrRBS!(K, Z - 1)
                    carLdrPlyrName$(Z, 19) = carRBacks$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                                Swap carLdrTDIndic$(Z, J - 1), carLdrTDIndic$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#6 - Rush Rec Avg
        For K = 0 To 49

            If carLdrRBS!(K, 4) / carLdrRBS!(K, 3) > carLdrVal!(6, 19) Then

                carLdrVal!(6, 19) = carLdrRBS!(K, 4) / carLdrRBS!(K, 3)
                carLdrPlyrName$(6, 19) = carRBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(6, J - 1) < carLdrVal!(6, J) Then
                            Swap carLdrVal!(6, J - 1), carLdrVal!(6, J)
                            Swap carLdrPlyrName$(6, J - 1), carLdrPlyrName$(6, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#7 - Rush Rec TDs
        '#8 - Longest Rush
        For Z = 7 To 8

            For K = 0 To 49

                If carLdrRBS!(K, Z - 2) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrRBS!(K, Z - 2)
                    carLdrTDIndic$(Z, 19) = carLdrRBS$(K)
                    carLdrPlyrName$(Z, 19) = carRBacks$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                                Swap carLdrTDIndic$(Z, J - 1), carLdrTDIndic$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#9 - Longest Rec
        For K = 0 To 49

            If carLdrRBS!(K, 9) > carLdrVal!(9, 19) Then

                carLdrVal!(9, 19) = carLdrRBS!(K, 9)
                carLdrTDIndic$(9, 19) = carLdrRBS$(K)
                carLdrPlyrName$(9, 19) = carRBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(9, J - 1) < carLdrVal!(9, J) Then
                            Swap carLdrVal!(9, J - 1), carLdrVal!(9, J)
                            Swap carLdrPlyrName$(9, J - 1), carLdrPlyrName$(9, J)
                            Swap carLdrTDIndic$(9, J - 1), carLdrTDIndic$(9, J)
                        End If
                    Next J
                Next I

            End If

        Next K


        ' *** Wide Receiver Categories ***

        '#10 - WR Recepts
        '#11 - WR Yds
        For Z = 10 To 11

            For K = 0 To 39

                If carLdrWRS!(K, Z - 10) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrWRS!(K, Z - 10)
                    carLdrPlyrName$(Z, 19) = carWdRec$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K
        Next Z

        '#12 - Rec Avg
        For K = 0 To 39

            If carLdrWRS!(K, 1) / carLdrWRS!(K, 0) > carLdrVal!(12, 19) Then

                carLdrVal!(12, 19) = carLdrWRS!(K, 1) / carLdrWRS!(K, 0)
                carLdrPlyrName$(12, 19) = carWdRec$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(12, J - 1) < carLdrVal!(12, J) Then
                            Swap carLdrVal!(12, J - 1), carLdrVal!(12, J)
                            Swap carLdrPlyrName$(12, J - 1), carLdrPlyrName$(12, J)
                            Swap carLdrTDIndic$(12, J - 1), carLdrTDIndic$(12, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#13 - WR TDs
        '#14 - WR Longest Rec
        For Z = 13 To 14

            For K = 0 To 39

                If carLdrWRS!(K, Z - 11) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrWRS!(K, Z - 11)
                    carLdrTDIndic$(Z, 19) = carLdrWRS$(K)
                    carLdrPlyrName$(Z, 19) = carWdRec$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                                Swap carLdrTDIndic$(Z, J - 1), carLdrTDIndic$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K
        Next Z


        ' *** Quarterback Categories ***

        '#15 - QB Comp
        '#16 - QB Att
        For Z = 15 To 16

            For K = 0 To 19

                If carLdrQBS!(K, Z - 15) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrQBS!(K, Z - 15)
                    carLdrPlyrName$(Z, 19) = carQBacks$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#17 - QB Comp Pct
        For K = 0 To 19

            If carLdrQBS!(K, 1) / carLdrQBS!(K, 0) > carLdrVal!(17, 19) Then

                carLdrVal!(17, 19) = carLdrQBS!(K, 1) / carLdrQBS!(K, 0)
                carLdrPlyrName$(17, 19) = carQBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(17, J - 1) < carLdrVal!(17, J) Then
                            Swap carLdrVal!(17, J - 1), carLdrVal!(17, J)
                            Swap carLdrPlyrName$(17, J - 1), carLdrPlyrName$(17, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#18 - Int
        '#19 - Yards
        '#20 - TDs
        For Z = 18 To 20

            For K = 0 To 19

                If carLdrQBS!(K, Z - 16) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrQBS!(K, Z - 16)
                    carLdrPlyrName$(Z, 19) = carQBacks$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#21 - Yds / Comp
        For K = 0 To 19

            If carLdrQBS!(K, 3) / carLdrQBS!(K, 1) > carLdrVal!(21, 19) Then

                carLdrVal!(21, 19) = carLdrQBS!(K, 3) / carLdrQBS!(K, 1)
                carLdrPlyrName$(21, 19) = carQBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(21, J - 1) < carLdrVal!(21, J) Then
                            Swap carLdrVal!(21, J - 1), carLdrVal!(21, J)
                            Swap carLdrPlyrName$(21, J - 1), carLdrPlyrName$(21, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        'QB Rating / Longest Pass
        For K = 0 To 19

            '#22 - Rating
            If carLdrQBS!(K, 12) > carLdrVal!(22, 19) Then

                carLdrVal!(22, 19) = carLdrQBS!(K, 12)
                carLdrPlyrName$(22, 19) = carQBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(22, J - 1) < carLdrVal!(22, J) Then
                            Swap carLdrVal!(22, J - 1), carLdrVal!(22, J)
                            Swap carLdrPlyrName$(22, J - 1), carLdrPlyrName$(22, J)
                        End If
                    Next J
                Next I

            End If

            '#23 - Longest Pass
            If carLdrQBS!(K, 5) > carLdrVal!(23, 19) Then

                carLdrVal!(23, 19) = carLdrQBS!(K, 5)
                carLdrTDIndic$(23, 19) = carLdrQBS$(K)
                carLdrPlyrName$(23, 19) = carQBacks$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(23, J - 1) < carLdrVal!(23, J) Then
                            Swap carLdrVal!(23, J - 1), carLdrVal!(23, J)
                            Swap carLdrPlyrName$(23, J - 1), carLdrPlyrName$(23, J)
                            Swap carLdrTDIndic$(23, J - 1), carLdrTDIndic$(23, J)
                        End If
                    Next J
                Next I

            End If

        Next K


        ' *** Kick Returner Categories ***

        '#24 - KR Att
        '#25 - KR Yards
        For Z = 24 To 25

            For K = 0 To 14

                If carLdrKRS!(K, Z - 24) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrKRS!(K, Z - 24)
                    carLdrPlyrName$(Z, 19) = carKickRet$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#26 - KR Average
        For K = 0 To 14

            If carLdrKRS!(K, 1) / carLdrKRS!(K, 0) > carLdrVal!(26, 19) Then

                carLdrVal!(26, 19) = carLdrKRS!(K, 1) / carLdrKRS!(K, 0)
                carLdrPlyrName$(26, 19) = carKickRet$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(26, J - 1) < carLdrVal!(26, J) Then
                            Swap carLdrVal!(26, J - 1), carLdrVal!(26, J)
                            Swap carLdrPlyrName$(26, J - 1), carLdrPlyrName$(26, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#27 - KR TDs
        '#28 - KR Longest
        For Z = 27 To 28

            For K = 0 To 14

                If carLdrKRS!(K, Z - 25) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrKRS!(K, Z - 25)
                    carLdrTDIndic$(Z, 19) = carLdrKRS$(K)
                    carLdrPlyrName$(Z, 19) = carKickRet$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                                Swap carLdrTDIndic$(Z, J - 1), carLdrTDIndic$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z


        ' *** Punt Returner Categories ***

        '#29 - PR Att
        '#30 - PR Yards
        For Z = 29 To 30

            For K = 0 To 14

                If carLdrPRS!(K, Z - 29) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrPRS!(K, Z - 29)
                    carLdrPlyrName$(Z, 19) = carPRet$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#31 - PR Average
        For K = 0 To 14

            If carLdrPRS!(K, 1) / carLdrPRS!(K, 0) > carLdrVal!(31, 19) Then

                carLdrVal!(31, 19) = carLdrPRS!(K, 1) / carLdrPRS!(K, 0)
                carLdrPlyrName$(31, 19) = carPRet$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(31, J - 1) < carLdrVal!(31, J) Then
                            Swap carLdrVal!(31, J - 1), carLdrVal!(31, J)
                            Swap carLdrPlyrName$(31, J - 1), carLdrPlyrName$(31, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#32 - PR TDs
        '#33 - PR Longest
        For Z = 32 To 33

            For K = 0 To 14

                If carLdrPRS!(K, Z - 30) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrPRS!(K, Z - 30)
                    carLdrTDIndic$(Z, 19) = carLdrPRS$(K)
                    carLdrPlyrName$(Z, 19) = carPRet$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                                Swap carLdrTDIndic$(Z, J - 1), carLdrTDIndic$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z


        ' *** Punter Categories ***

        '#34 - Punt Att
        '#35 - Punt Yards
        For Z = 34 To 35

            For K = 0 To 9

                If carLdrPUS!(K, Z - 34) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrPUS!(K, Z - 34)
                    carLdrPlyrName$(Z, 19) = carPunter$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#36 - Punt Average
        For K = 0 To 9

            If carLdrPUS!(K, 1) / carLdrPUS!(K, 0) > carLdrVal!(36, 19) Then

                carLdrVal!(36, 19) = carLdrPUS!(K, 1) / carLdrPUS!(K, 0)
                carLdrPlyrName$(36, 19) = carPunter$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(36, J - 1) < carLdrVal!(36, J) Then
                            Swap carLdrVal!(36, J - 1), carLdrVal!(36, J)
                            Swap carLdrPlyrName$(36, J - 1), carLdrPlyrName$(36, J)
                        End If
                    Next J
                Next I

            End If

        Next K


        ' *** Kicker Categories ***

        '#37 - EP Made
        '#38 - EP Att
        For Z = 37 To 38

            For K = 0 To 9

                If carLdrFGS!(K, Z - 35) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrFGS!(K, Z - 35)
                    carLdrPlyrName$(Z, 19) = carKicker$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        '#39 - EP %
        For K = 0 To 9

            If carLdrFGS!(K, 2) / carLdrFGS!(K, 3) > carLdrVal!(39, 19) Then

                carLdrVal!(39, 19) = carLdrFGS!(K, 2) / carLdrFGS!(K, 3)
                carLdrPlyrName$(39, 19) = carKicker$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(39, J - 1) < carLdrVal!(39, J) Then
                            Swap carLdrVal!(39, J - 1), carLdrVal!(39, J)
                            Swap carLdrPlyrName$(39, J - 1), carLdrPlyrName$(39, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#40 - FG Made
        '#41 - FG Att
        For Z = 40 To 41

            For K = 0 To 9

                If carLdrFGS!(K, Z - 40) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrFGS!(K, Z - 40)
                    carLdrPlyrName$(Z, 19) = carKicker$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

        Next Z

        'FG% / Longest FG
        For K = 0 To 9

            '#42 - FG %
            If carLdrFGS!(K, 0) / carLdrFGS!(K, 1) > carLdrVal!(42, 19) Then

                carLdrVal!(42, 19) = carLdrFGS!(K, 0) / carLdrFGS!(K, 1)
                carLdrPlyrName$(42, 19) = carKicker$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(42, J - 1) < carLdrVal!(42, J) Then
                            Swap carLdrVal!(42, J - 1), carLdrVal!(42, J)
                            Swap carLdrPlyrName$(42, J - 1), carLdrPlyrName$(42, J)
                        End If
                    Next J
                Next I

            End If

            '#43 - Longest FG
            If carLdrFGS!(K, 4) > carLdrVal!(Z, 19) Then

                carLdrVal!(43, 19) = carLdrFGS!(K, 4)
                carLdrPlyrName$(43, 19) = carKicker$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(43, J - 1) < carLdrVal!(43, J) Then
                            Swap carLdrVal!(43, J - 1), carLdrVal!(43, J)
                            Swap carLdrPlyrName$(43, J - 1), carLdrPlyrName$(43, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#44 - FGM 0-19
        '#45 - FGA 0-19
        '#46 - FG% 0-19

        '#47 - FGM 20-29
        '#48 - FGA 20-29
        '#49 - FG% 20-29

        '#50 - FGM 30-39
        '#51 - FGA 30-39
        '#52 - FG% 30-39

        '#53 - FGM 40-49
        '#54 - FGA 40-49
        '#55 - FG% 40-49

        '#56 - FGM 50+
        '#57 - FGA 50+
        '#58 - FG% 50+

        clIdx = 5

        For Z = 44 To 58 Step 3

            For K = 0 To 9

                'Z = 44, 47, 50, 53, 56
                'FGS = 5, 6, 7, 8, 9
                If carLdrFGS!(K, clIdx + 5) > carLdrVal!(Z, 19) Then

                    carLdrVal!(Z, 19) = carLdrFGS!(K, clIdx + 5)
                    carLdrPlyrName$(Z, 19) = carKicker$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z, J - 1) < carLdrVal!(Z, J) Then
                                Swap carLdrVal!(Z, J - 1), carLdrVal!(Z, J)
                                Swap carLdrPlyrName$(Z, J - 1), carLdrPlyrName$(Z, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

            For K = 0 To 9

                '45, 48, 51, 52, 53
                'FGS = 10, 11, 12, 13, 14
                If carLdrFGS!(K, clIdx + 5) > carLdrVal!(Z + 2, 19) Then

                    carLdrVal!(Z + 1, 19) = carLdrFGS!(K, clIdx + 5)
                    carLdrPlyrName$(Z + 1, 19) = carKicker$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z + 1, J - 1) < carLdrVal!(Z + 1, J) Then
                                Swap carLdrVal!(Z + 1, J - 1), carLdrVal!(Z + 1, J)
                                Swap carLdrPlyrName$(Z + 1, J - 1), carLdrPlyrName$(Z + 1, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

            For K = 0 To 9

                If carLdrFGS!(K, clIdx) / carLdrFGS!(K, clIdx + 5) > carLdrVal!(Z + 1, 19) Then

                    carLdrVal!(Z + 2, 19) = carLdrFGS!(K, clIdx) / carLdrFGS!(K, clIdx + 5)
                    carLdrPlyrName$(Z + 2, 19) = carKicker$(K)

                    For I = 1 To 20
                        For J = 20 To I Step -1
                            If carLdrVal!(Z + 2, J - 1) < carLdrVal!(Z + 2, J) Then
                                Swap carLdrVal!(Z + 2, J - 1), carLdrVal!(Z + 2, J)
                                Swap carLdrPlyrName$(Z + 2, J - 1), carLdrPlyrName$(Z + 2, J)
                            End If
                        Next J
                    Next I

                End If

            Next K

            clIdx = clIdx + 1

        Next Z

        '#59 - Def Int's
        For K = 0 To 54

            If carLdrGIS!(K, 0) > carLdrVal!(59, 19) Then

                carLdrVal!(59, 19) = carLdrGIS!(K, 0)
                carLdrPlyrName$(59, 19) = intName$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(59, J - 1) < carLdrVal!(59, J) Then
                            Swap carLdrVal!(59, J - 1), carLdrVal!(59, J)
                            Swap carLdrPlyrName$(59, J - 1), carLdrPlyrName$(59, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        '#60 - Def Sacks
        For K = 0 To 54

            If carLdrGSS!(K, 0) > carLdrVal!(60, 19) Then

                carLdrVal!(60, 19) = carLdrGSS!(K, 0)
                carLdrPlyrName$(60, 19) = sackName$(K)

                For I = 1 To 20
                    For J = 20 To I Step -1
                        If carLdrVal!(60, J - 1) < carLdrVal!(60, J) Then
                            Swap carLdrVal!(60, J - 1), carLdrVal!(60, J)
                            Swap carLdrPlyrName$(60, J - 1), carLdrPlyrName$(60, J)
                        End If
                    Next J
                Next I

            End If

        Next K

        ' ##########

        'Save leaders
        Open diskPaths$(2) + careerFile$ + ".TCL" For Output As #1

        For I = 0 To 62:
            For I1 = 0 To 19:
                Write #1, carLdrVal!(I, I1), carLdrPlyrName$(I, I1), carLdrTDIndic$(I, I1)
            Next
        Next

        Close #1

    End If

End Sub


'----------------------------------------
'     CareerLeaderOptions Subroutine
'----------------------------------------
'This routine reads in previously compiled
'career stats (.TCL file) and then provides
'the user options to print or view the stats.
Sub CareerLeaderOptions ()

    Shared carLdrPlyrName$()
    Shared carLdrVal!()
    Shared carLdrTDIndic$()

    Cls

    leaderFile$ = ChooseFile$(diskPaths$(2) + "*.TCL", "LEADER", ExitCode%)

    If ExitCode% <> 27 And ExitCode% <> 0 Then

        Color 15, 0
        Cls

        Open diskPaths$(2) + leaderFile$ + ".TCL" For Input As #1

        For I = 0 To 62:
            For I1 = 0 To 19:
                Input #1, carLdrVal!(I, I1), carLdrPlyrName$(I, I1), carLdrTDIndic$(I, I1)
            Next
        Next

        Close #1

        userKey$ = GetOutputOptions$

        If userKey$ <> Chr$(27) Then

            Select Case userKey$

                Case "F"
                    userFile$ = _SaveFileDialog$("Save Report File", _CWD$ + "\Report", "*.txt", "Text File")
                    If userFile$ <> "" Then
                        Call PrintCareerLeaders(0, userFile$)
                    End If

                Case "P"
                    Call PrintCareerLeaders(1, "REPORT")

                Case "S"
                    Call ViewCareerLeaders

            End Select

        End If

    End If 'Done checking for ESC


End Sub


'----------------------------------------
'       PrintCareerLeaders Subroutine
'----------------------------------------
'This routine sends formatted career stats
'(loaded from the .TCL file) to the user's
'default printer
Sub PrintCareerLeaders (printDest, destFile$)

    Shared carLdrPlyrName$()
    Shared carLdrVal!()
    Shared carLdrTDIndic$()

    Color 15, 0
    Cls

    Open destFile$ For Output As #1

    Print #1, Tab(5); "RUSH ATTEMPTS"; Tab(34); "RUSH YARDS"; Tab(62); "RUSH AVERAGE"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(0, I);
        Print #1, Tab(18); Using "#,###"; carLdrVal!(0, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(1, I);
        Print #1, Tab(47); Using "##,###"; carLdrVal!(1, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(2, I);
        Print #1, Tab(76); Using "###.#"; carLdrVal!(2, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "RUSH TOUCHDOWNS"; Tab(34); "RECEPTIONS (BACKS)"; Tab(62); "RECEIVING YARDS (BACKS)"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(3, I);
        Print #1, Tab(18); Using "###"; carLdrVal!(3, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(4, I);
        Print #1, Tab(47); Using "#,###"; carLdrVal!(4, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(5, I);
        Print #1, Tab(76); Using "##,###"; carLdrVal!(5, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "RECEIVING AVERAGE (BACKS)"; Tab(34); "RECEIVING TOUCHDOWNS (BACKS)"; Tab(62); "LONGEST RECEPTION (BACKS)"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(6, I);
        Print #1, Tab(18); Using "###.#"; carLdrVal!(6, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(7, I);
        Print #1, Tab(47); Using "###"; carLdrVal!(7, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(9, I);
        Print #1, Tab(76); Using "###"; carLdrVal!(9, I);
        Print #1, carLdrTDIndic$(9, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "LONGEST RUN"; Tab(34); "RECEPTIONS"; Tab(62); "RECEIVING YARDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(8, I);
        Print #1, Tab(18); Using "####"; carLdrVal!(8, I);
        Print #1, carLdrTDIndic$(8, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(10, I);
        Print #1, Tab(47); Using "#,###"; carLdrVal!(10, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(11, I);
        Print #1, Tab(76); Using "##,###"; carLdrVal!(11, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "RECEIVING AVERAGE"; Tab(34); "RECEIVING TOUCHDOWNS"; Tab(62); "LONGEST RECEPTION"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(12, I);
        Print #1, Tab(18); Using "###.#"; carLdrVal!(12, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(13, I);
        Print #1, Tab(47); Using "###"; carLdrVal!(13, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(14, I);
        Print #1, Tab(76); Using "###"; carLdrVal!(14, I);
        Print #1, carLdrTDIndic$(14, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "PASS COMPLETIONS"; Tab(34); "PASS ATTEMPTS"; Tab(62); "COMPLETION %"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(15, I);
        Print #1, Tab(18); Using "##,###"; carLdrVal!(15, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(16, I);
        Print #1, Tab(47); Using "##,###"; carLdrVal!(16, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(17, I);
        Print #1, Tab(76); Using "###.#"; carLdrVal!(17, I) * 100
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "PASSING YARDS"; Tab(34); "YARDS / COMPLETION"; Tab(62); "PASSING TOUCHDOWNS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(19, I);
        Print #1, Tab(18); Using "##,###"; carLdrVal!(19, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(21, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(21, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(20, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(20, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "PASSING INTERCEPTIONS"; Tab(34); "QUARTERBACK RATING"; Tab(62); "LONGEST PASS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(18, I);
        Print #1, Tab(18); Using "###"; carLdrVal!(18, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(22, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(22, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(23, I);
        Print #1, Tab(76); Using "###"; carLdrVal!(23, I);
        Print #1, carLdrTDIndic$(23, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "KICK RETURN ATTEMPTS"; Tab(34); "KICK RETURN YARDS"; Tab(62); "KICK RETURN AVERAGE"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(24, I);
        Print #1, Tab(18); Using "###"; carLdrVal!(24, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(25, I);
        Print #1, Tab(47); Using "##,###"; carLdrVal!(25, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(26, I);
        Print #1, Tab(76); Using "###.#"; carLdrVal!(26, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "KICK RETURN TOUCHDOWNS"; Tab(34); "LONGEST KICK RETURN"; Tab(62); "PUNT RETURN ATTEMPTS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(27, I);
        Print #1, Tab(18); Using "###"; carLdrVal!(27, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(28, I);
        Print #1, Tab(47); Using "###"; carLdrVal!(28, I); carLdrTDIndic$(28, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(29, I);
        Print #1, Tab(76); Using "###"; carLdrVal!(29, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "PUNT RETURN YARDS"; Tab(34); "PUNT RETURN AVERAGE"; Tab(62); "PUNT RETURN TOUCHDOWNS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(30, I);
        Print #1, Tab(18); Using "##,###"; carLdrVal!(30, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(31, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(31, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(32, I);
        Print #1, Tab(76); Using "###"; carLdrVal!(32, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "LONGEST PUNT RETURN"; Tab(34); "PUNT ATTEMPTS"; Tab(62); "PUNT YARDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(33, I);
        Print #1, Tab(18); Using "###"; carLdrVal!(33, I);
        Print #1, carLdrTDIndic$(33, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(34, I);
        Print #1, Tab(47); Using "#,###"; carLdrVal!(34, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(35, I);
        Print #1, Tab(76); Using "##,###"; carLdrVal!(35, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "PUNT AVERAGE"; Tab(34); "EXTRA POINTS MADE"; Tab(62); "EXTRA POINT ATTEMPTS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(36, I);
        Print #1, Tab(18); Using "###.#"; carLdrVal!(36, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(37, I);
        Print #1, Tab(47); Using "#,###"; carLdrVal!(37, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(38, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(38, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "EXTRA POINT %"; Tab(34); "FIELD GOALS MADE"; Tab(62); "FIELD GOALS ATTEMPTED"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(39, I);
        Print #1, Tab(18); Using "###.#"; carLdrVal!(39, I) * 100;
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(40, I);
        Print #1, Tab(47); Using "#,###"; carLdrVal!(40, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(41, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(41, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL %"; Tab(34); "LONGEST FIELD GOAL"; Tab(62); "FIELD GOALS MADE 0-19 YDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(42, I);
        Print #1, Tab(18); Using "###.#"; carLdrVal!(42, I) * 100;
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(43, I);
        Print #1, Tab(47); Using "##,###"; carLdrVal!(43, I);
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(44, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(44, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL ATTEMPTS 0-19 YDS"; Tab(34); "FIELD GOAL % 0-19 YDS"; Tab(62); "FIELD GOALS MADE 20-29 YDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(45, I);
        Print #1, Tab(18); Using "####"; carLdrVal!(45, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(46, I);
        Print #1, Tab(47); Using "####"; carLdrVal!(46, I) * 100;
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(47, I);
        Print #1, Tab(76); Using "####"; carLdrVal!(47, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL ATTEMPTS 20-29 YDS"; Tab(34); "FIELD GOAL % 20-29 YDS"; Tab(62); "FIELD GOALS MADE 30-39 YDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(48, I);
        Print #1, Tab(18); Using "#,###"; carLdrVal!(48, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(49, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(49, I) * 100;
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(50, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(50, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL ATTEMPTS 30-39 YDS"; Tab(34); "FIELD GOAL % 30-39 YDS"; Tab(62); "FIELD GOALS MADE 40-49 YDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(51, I);
        Print #1, Tab(18); Using "#,###"; carLdrVal!(51, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(52, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(52, I) * 100;
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(53, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(53, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL ATTEMPTS 40-49 YDS"; Tab(34); "FIELD GOAL % 40-49 YDS"; Tab(62); "FIELD GOALS MADE 50+ YDS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(54, I);
        Print #1, Tab(18); Using "#,###"; carLdrVal!(54, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(55, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(55, I) * 100;
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(56, I);
        Print #1, Tab(76); Using "#,###"; carLdrVal!(56, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "FIELD GOAL ATTEMPTS 50+ YDS"; Tab(34); "FIELD GOAL % 50+ YDS"; Tab(62); "INTERCEPTIONS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(57, I);
        Print #1, Tab(18); Using "#,###"; carLdrVal!(57, I);
        Print #1, Tab(28); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(58, I);
        Print #1, Tab(47); Using "###.#"; carLdrVal!(58, I) * 100;
        Print #1, Tab(58); Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(59, I);
        Print #1, Tab(76); Using "###"; carLdrVal!(59, I)
    Next

    Print #1,
    Print #1,

    Print #1, Tab(5); "SACKS"
    For I = 0 To 19
        Print #1, Using "##"; I + 1;: Print #1, ". ";: Print #1, carLdrPlyrName$(60, I);
        Print #1, Tab(18); Using "###"; carLdrVal!(60, I);
    Next

    Print #1,
    Print #1,

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
'      ViewCareerLeaders Subroutine
'----------------------------------------
'This routine displays formatted career
'stats (loaded from the .TCL file) to the
'primary screen
Sub ViewCareerLeaders ()

    Shared carLdrPlyrName$()
    Shared carLdrVal!()
    Shared carLdrTDIndic$()

    Color 15, 0

    '--- Rushing Leader Values ---

    Cls
    Color 12

    Print Tab(5); "RUSH ATTEMPTS"; Tab(44); "RUSH YARDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(0, I);
        Print Tab(20); Using "#,###"; carLdrVal!(0, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(1, I);
        Print Tab(60); Using "##,###"; carLdrVal!(1, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "RUSH AVERAGE"; Tab(44); "RUSH TOUCHDOWNS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(2, I);
        Print Tab(20); Using "###.#"; carLdrVal!(2, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(3, I);
        Print Tab(60); Using "###"; carLdrVal!(3, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "RECEPTIONS (BACKS)"; Tab(44); "RECEIVING YARDS (BACKS)"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(4, I);
        Print Tab(20); Using "#,###"; carLdrVal!(4, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(5, I);
        Print Tab(60); Using "##,###"; carLdrVal!(5, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "RECEIVING AVERAGE (BACKS)"; Tab(44); "RECEIVING TOUCHDOWNS (BACKS)"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(6, I);
        Print Tab(20); Using "###.#"; carLdrVal!(6, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(7, I);
        Print Tab(60); Using "###"; carLdrVal!(7, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "LONGEST RECEPTION (BACKS)"; Tab(44); "LONGEST RUN"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(9, I);
        Print Tab(20); Using "###"; carLdrVal!(9, I);
        Print carLdrTDIndic$(9, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(8, I);
        Print Tab(60); Using "###"; carLdrVal!(8, I);
        Print carLdrTDIndic$(8, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$


    '--- Wide Receiver Leader Values ---

    Cls
    Color 12

    Print Tab(5); "RECEPTIONS"; Tab(44); "RECEIVING YARDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(10, I);
        Print Tab(20); Using "#,###"; carLdrVal!(10, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(11, I);
        Print Tab(60); Using "##,###"; carLdrVal!(11, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "RECEIVING AVERAGE"; Tab(44); "RECEIVING TOUCHDOWNS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(12, I);
        Print Tab(20); Using "###.#"; carLdrVal!(12, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(13, I);
        Print Tab(60); Using "###"; carLdrVal!(13, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "LONGEST RECEPTION"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(14, I);
        Print Tab(20); Using "###"; carLdrVal!(14, I);
        Print carLdrTDIndic$(14, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$


    '--- Quarterback Leader Values ---

    Cls
    Color 12

    Print Tab(5); "PASS COMPLETIONS"; Tab(44); "PASS ATTEMPTS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(15, I);
        Print Tab(20); Using "##,###"; carLdrVal!(15, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(16, I);
        Print Tab(60); Using "##,###"; carLdrVal!(16, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "COMPLETION %"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(17, I);
        Print Tab(20); Using "###.#"; carLdrVal!(17, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "PASSING YARDS"; Tab(44); "YARDS / COMPLETION"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(19, I);
        Print Tab(20); Using "##,###"; carLdrVal!(19, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(21, I);
        Print Tab(60); Using "###.#"; carLdrVal!(21, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "PASSING TOUCHDOWNS"; Tab(44); "PASS INTERCEPTIONS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(20, I);
        Print Tab(20); Using "#,###"; carLdrVal!(20, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(18, I);
        Print Tab(60); Using "###"; carLdrVal!(18, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "QUARTERBACK RATING"; Tab(44); "LONGEST PASS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(22, I);
        Print Tab(20); Using "###.#"; carLdrVal!(22, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(23, I);
        Print Tab(60); Using "###"; carLdrVal!(23, I);
        Print carLdrTDIndic$(23, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$


    '--- Kick Returner Leader Values ---

    Cls
    Color 12

    Print Tab(5); "KICK RETURN ATTEMPTS"; Tab(44); "KICK RETURN YARDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(24, I);
        Print Tab(20); Using "###"; carLdrVal!(24, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(25, I);
        Print Tab(60); Using "##,###"; carLdrVal!(25, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "KICK RETURN AVERAGE"; Tab(44); "KICK RETURN TOUCHDOWNS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(26, I);
        Print Tab(20); Using "###.#"; carLdrVal!(26, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(27, I);
        Print Tab(60); Using "###"; carLdrVal!(27, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "LONGEST KICK RETURN"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(28, I);
        Print Tab(20); Using "###"; carLdrVal!(28, I);
        Print carLdrTDIndic$(28, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$


    '--- Punt Returner Leader Values ---

    Cls
    Color 12

    Print Tab(5); "PUNT RETURN ATTEMPTS"; Tab(44); "PUNT RETURN YARDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(29, I);
        Print Tab(20); Using "###"; carLdrVal!(29, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(30, I);
        Print Tab(60); Using "##,###"; carLdrVal!(30, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "PUNT RETURN AVERAGE"; Tab(44); "PUNT RETURN TOUCHDOWNS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(31, I);
        Print Tab(20); Using "###.#"; carLdrVal!(31, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(32, I);
        Print Tab(60); Using "###"; carLdrVal!(32, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "LONGEST PUNT RETURN"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(33, I);
        Print Tab(20); Using "###"; carLdrVal!(33, I);
        Print carLdrTDIndic$(33, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$


    '--- Punter Leader Values ---

    Cls
    Color 12

    Print Tab(5); "PUNT ATTEMPTS"; Tab(44); "PUNT YARDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(34, I);
        Print Tab(20); Using "#,###"; carLdrVal!(34, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(35, I);
        Print Tab(60); Using "##,###"; carLdrVal!(35, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "PUNT AVERAGE"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(36, I);
        Print Tab(20); Using "###.#"; carLdrVal!(36, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$


    '--- Kicker Leader Values ---

    Cls
    Color 12

    Print Tab(5); "EXTRA POINTS MADE"; Tab(44); "EXTRA POINT ATTEMPTS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(37, I);
        Print Tab(20); Using "#,###"; carLdrVal!(37, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(38, I);
        Print Tab(60); Using "#,###"; carLdrVal!(38, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "EXTRA POINT %"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(39, I);
        Print Tab(20); Using "###.#"; carLdrVal!(39, I) * 100

    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOALS MADE"; Tab(44); "FIELD GOALS ATTEMPTED"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(40, I);
        Print Tab(20); Using "#,###"; carLdrVal!(40, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(41, I);
        Print Tab(60); Using "#,###"; carLdrVal!(41, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOAL %"; Tab(44); "LONGEST FIELD GOAL"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(42, I);
        Print Tab(20); Using "###.#"; carLdrVal!(42, I) * 100;

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(43, I);
        Print Tab(60); Using "##,###"; carLdrVal!(43, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOALS MADE 0-19 YDS"; Tab(44); "FIELD GOAL ATTEMPTS 0-19 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(44, I);
        Print Tab(20); Using "#,###"; carLdrVal!(44, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(45, I);
        Print Tab(60); Using "#,###"; carLdrVal!(45, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOAL % 0-19 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(46, I);
        Print Tab(20); Using "###.#"; carLdrVal!(46, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOALS MADE 20-29 YDS"; Tab(44); "FIELD GOAL ATTEMPTS 20-29 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(47, I);
        Print Tab(20); Using "#,###"; carLdrVal!(47, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(48, I);
        Print Tab(60); Using "#,###"; carLdrVal!(48, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOAL % 20-29 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(49, I);
        Print Tab(20); Using "###.#"; carLdrVal!(49, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOALS MADE 30-39 YDS"; Tab(44); "FIELD GOAL ATTEMPTS 30-39 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(50, I);
        Print Tab(20); Using "#,###"; carLdrVal!(50, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(51, I);
        Print Tab(60); Using "#,###"; carLdrVal!(51, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOAL % 30-39 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(52, I);
        Print Tab(20); Using "###.#"; carLdrVal!(52, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOALS MADE 40-49 YDS"; Tab(44); "FIELD GOAL ATTEMPTS 40-49 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(53, I);
        Print Tab(20); Using "#,###"; carLdrVal!(53, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(54, I);
        Print Tab(60); Using "#,###"; carLdrVal!(54, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOAL % 40-49 YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(55, I);
        Print Tab(20); Using "###.#"; carLdrVal!(55, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOALS MADE 50+ YDS"; Tab(44); "FIELD GOAL ATTEMPTS 50+ YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(56, I);
        Print Tab(20); Using "#,###"; carLdrVal!(56, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(57, I);
        Print Tab(60); Using "#,###"; carLdrVal!(57, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls
    Color 12

    Print Tab(5); "FIELD GOAL % 50+ YDS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(58, I);
        Print Tab(20); Using "###.#"; carLdrVal!(58, I) * 100
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    '--- Interception / Sack Leaders ---

    Cls
    Color 12

    Print Tab(5); "INTERCEPTIONS"; Tab(44); "SACKS"
    For I = 0 To 19
        Color 3
        Locate I + 2: Print Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(59, I);
        Print Tab(20); Using "###"; carLdrVal!(59, I);

        Color 3
        Print Tab(40); Using "##"; I + 1;
        Print ". ";
        Color 15: Print carLdrPlyrName$(60, I);
        Print Tab(60); Using "###"; carLdrVal!(60, I)
    Next I

    Locate LAST_ROW - 2, 22
    Color 14: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

End Sub
