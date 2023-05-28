Sub TINPUT ()

    _Title "3-in-1 Football - Team Input"

    ReDim menuChoices$(1 To 3)

    Do

        Cls

        Call TITLE
        menuChoices$(1) = "INPUT A NEW TEAM                "
        menuChoices$(2) = "RETURN TO 3 IN 1 FOOTBALL MENU  "
        Selection% = 1
        Count% = 2
        Start% = 1
        ExitCode% = 2
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 10, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        
		If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call InputTeam
                Case 2: Cls
            End Select
        End If

    Loop Until Selection% = 2 Or ExitCode% = 27

End Sub

'----------------------------------------
'       InputTeam Subroutine
'----------------------------------------
'...explanation...
Sub InputTeam ()

    Dim A1!(9), B3!(1)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared statsDI$(), statsDS$()

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()

    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared careerDS%(), careerDI%(), statsPA%(), careerQX%()

    Do

        'Color 15, 1
        Color 15, 0

        Cls

        Locate 1: Input " 1. DISK ID  "; YN$
        Locate 2: Input " 2. TEAM # (ENTER FOR PROGRAM TO ASSIGN) "; teamNumber
        Locate 3: Input " 3. TEAM NAME (IE. 94 ARKANSAS) "; A$
        Locate 4: Input " 4. TEAM NICKNAME (IE. PANTHERS) "; AA$
        Locate 5: Input " 5. DEF RUSH ADJ "; careerB6%(0)
        Locate 6: Input " 6. DEF COMP % ADJ "; careerB6%(1)
        Locate 7: Input " 7. DEF INT % ADJ "; careerB6%(2)
        Locate 8: Input " 8. FUM % (AVG=4) "; careerB6%(3)
        Locate 9: Input " 9. DEF FUM % (AVG=0) "; careerB6%(4)
        Locate 10: Input " 10. OFF SACKED % (AVG=8) "; careerB6%(5)
        Locate 11: Input "11. DEF SACK ADJ "; careerB6%(6)
        Locate 12: Input "12. DEF YD/COMP ADJ "; Q
        Locate 13: Input "13. TM PENALTY ADJ "; statsPA%(0)
        Locate 14: Input "14. OFF RUN/PASS % "; careerB6%(7)
        Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; statsPA%(3)
        Locate 16: Input "16. POWER RATING "; statsPA%(4)
        Locate 17: Input "17. BACKGROUND COLOR (0-7) "; statsPA%(1)
        Locate 18: Input "18. TEXT COLOR (0-15) "; statsPA%(2)
        Locate 19: Input "19. OFF POWER ADJ "; O9
        Locate 20: Input "20. DEF POWER ADJ "; D9
        Locate 21: Input "21. STADIUM NAME "; S$
        Locate 22: Input "22. AVERAGE ATTENDANCE "; ATT
        Locate 1, 40: Color statsPA%(2), statsPA%(1): Print "SAMPLE TEXT COLOR"

        Color 15, 0
        C1 = 1

        Do

            Locate 23: Input "# TO CHANGE (0 TO CONTINUE ) "; B9

            Select Case B9

                Case 1
                    400 Locate 1: Input " 1. DISK ID  "; YN$

                Case 2
                    401 Locate 2: Input " 2. TEAM # (ENTER FOR PROGRAM TO ASSIGN) "; teamNumber

                Case 3
                    402 Locate 3: Input " 3. TEAM NAME (IE. 94 ARKANSAS) "; A$

                Case 4
                    403 Locate 4: Input " 4. TEAM NICKNAME (IE. PANTHERS) "; AA$

                Case 5
                    404 Locate 5: Input " 5. DEF RUSH ADJ "; careerB6%(0)

                Case 6
                    405 Locate 6: Input " 6. DEF COMP % ADJ "; careerB6%(1)

                Case 7
                    406 Locate 7: Input " 7. DEF INT % ADJ "; careerB6%(2)

                Case 8
                    407 Locate 8: Input " 8. FUM % (AVG=4) "; careerB6%(3)

                Case 9
                    408 Locate 9: Input " 9. DEF FUM % (AVG=0) "; careerB6%(4)

                Case 10
                    409 Locate 10: Input " 10. OFF SACKED % (AVG=8) "; careerB6%(5)

                Case 11
                    410 Locate 11: Input "11. DEF SACK ADJ "; careerB6%(6)

                Case 12
                    411 Locate 12: Input "12. DEF YD/COMP ADJ "; Q

                Case 13
                    412 Locate 13: Input "13. TM PENALTY ADJ "; statsPA%(0)

                Case 14
                    413 Locate 14: Input "14. OFF RUN/PASS % "; careerB6%(7)

                Case 15
                    414 Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; statsPA%(3)

                Case 16
                    415 Locate 16: Input "16. POWER RATING "; statsPA%(4)

                Case 17
                    416 Locate 17: Input "17. BACKGROUND COLOR (0-7) "; statsPA%(1)

                Case 18
                    417 Locate 18: Input "18. TEXT COLOR (0-15) "; statsPA%(2)

                Case 19
                    418 Locate 19: Input "19. OFF POWER ADJ "; O9

                Case 20
                    419 Locate 20: Input "20. DEF POWER ADJ "; D9

                Case 21
                    421 Locate 21: Input "21. STADIUM NAME "; S$

                Case 22
                    422 Locate 22: Input "22. AVERAGE ATTENDANCE "; ATT

            End Select

        Loop Until B9 = 0

        420 '
        Locate 1, 40: Color statsPA%(2), statsPA%(1): Print "SAMPLE TEXT COLOR"

        'Case 0 / Continue
        C1 = 0

        'Running Backs
        Cls
        Color 15, 0

        For I = 0 To 9: Cls

            Locate 3: Print "RUNNING BACKS -"; I + 1

            Locate 5: Input "1. NAME "; statsA1$(I)

            If UCase$(statsA1$(I)) <> "END" Then

                Locate 6: Input "2. # OF RUSHES "; careerA0%(I)
                Locate 7: Input "3. RUSHING AVERAGE "; A1!(I)
                Locate 8: Input "4. # OF RECEPTIONS "; careerA2%(I)
                Locate 9: Input "5. RECEIVING AVERAGE "; careerA3%(I)

                432 '
                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE)"; B9

                    Select Case B9
                        Case 1
                            426 Locate 5: Input "1. NAME "; statsA1$(I)

                        Case 2
                            428 Locate 6: Input "2. # OF RUSHES "; careerA0%(I)

                        Case 3
                            429 Locate 7: Input "3. RUSHING AVERAGE "; A1!(I)

                        Case 4
                            430 Locate 8: Input "4. # OF RECEPTIONS "; careerA2%(I)

                        Case 5
                            431 Locate 9: Input "5. RECEIVING AVERAGE "; careerA3%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0
            Else

                Exit For

            End If

        Next I

        'Wide Recievers
        Cls

        For I = 0 To 5:

            Cls

            Locate 3: Print "WIDE RECEIVERS/TIGHT ENDS -"; I + 1

            Locate 5: Input "1. NAME "; statsA2$(I)

            If UCase$(statsA2$(I)) <> "END" Then

                Locate 6: Input "2. # OF RECEPTIONS "; careerA4%(I)
                Locate 7: Input "3. RECEIVING AVERAGE "; careerA5%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            443 Locate 5: Input "1. NAME "; statsA2$(I)

                        Case 2
                            445 Locate 6: Input "2. # OF RECEPTIONS "; careerA4%(I)

                        Case 3
                            446 Locate 7: Input "3. RECEIVING AVERAGE "; careerA5%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0
            Else

                Exit For

            End If

        Next I

        'Quarterbacks
        460 '
        Cls

        For I = 0 To 3: Cls

            Locate 3: Print "QUARTERBACKS -"; I + 1

            Locate 5: Input "1. NAME "; statsA3$(I)

            If UCase$(statsA3$(I)) <> "END" Then

                Locate 6: Input "2. PASSES ATTEMPTED "; careerA6%(I)
                Locate 7: Input "3. COMPLETION % "; careerA7%(I)
                Locate 8: Input "4. INTERCEPTION % "; careerA8%(I)
                Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; careerQX%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            463 Locate 5: Input "1. NAME "; statsA3$(I)

                        Case 2
                            465 Locate 6: Input "2. PASSES ATTEMPTED "; careerA6%(I)

                        Case 3
                            466 Locate 7: Input "3. COMPLETION % "; careerA7%(I)

                        Case 4
                            467 Locate 8: Input "4. INTERCEPTION % "; careerA8%(I)

                        Case 5
                            468 Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; careerQX%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'Kickoff Returners
        480 '
        Cls

        For I = 0 To 2: Cls

            Locate 3: Print "KICKOFF RETURNMEN -"; I + 1

            Locate 5: Input "1. NAME "; statsA4$(I)

            If UCase$(statsA4$(I)) <> "END" Then

                Locate 6: Input "2. KICKOFFS RETURNED "; careerA9%(I)
                Locate 7: Input "3. RETURN AVERAGE "; careerB%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            483 Locate 5: Input "1. NAME "; statsA4$(I)

                        Case 2
                            485 Locate 6: Input "2. KICKOFFS RETURNED "; careerA9%(I)

                        Case 3
                            486 Locate 7: Input "3. RETURN AVERAGE "; careerB%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'Punt Returners
        490 '
        Cls

        For I = 0 To 2: Cls

            Locate 3: Print "PUNT RETURNMEN -"; I + 1

            Locate 5: Input "1. NAME "; statsA5$(I)

            If UCase$(statsA5$(I)) <> "END" Then

                Locate 6: Input "2. PUNTS RETURNED "; careerB1%(I)
                Locate 7: Input "3. PUNT RETURN AVERAGE "; careerB2%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            503 Locate 5: Input "1. NAME "; statsA5$(I)

                        Case 2
                            505 Locate 6: Input "2. PUNTS RETURNED "; careerB1%(I)

                        Case 3
                            506 Locate 7: Input "3. PUNT RETURN AVERAGE "; careerB2%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'Punters
        520 '
        Cls

        Locate 3: Input "1. PUNTER NAME "; statsA6$(0)
        Locate 4: Input "2. PUNT AVERAGE "; B3!(0)

        C1 = 1

        Do

            Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

            Select Case B9
                Case 1
                    521 Locate 3: Input "1. PUNTER NAME "; statsA6$(0)

                Case 2
                    522 Locate 4: Input "2. PUNT AVERAGE "; B3!(0)

            End Select

        Loop Until B9 = 0

        C1 = 0

        'Kickers
        Cls

        For I = 0 To 1

            Cls

            Locate 3: Print "PLACE KICKER -"; I + 1

            Locate 5: Input "1. NAME "; statsA7$(I)

            If UCase$(statsA7$(I)) <> "END" Then

                Locate 6: Input "2. FGA "; careerB5%(I)
                Locate 7: Input "3. FG% "; careerB4%(I)
                Locate 8: Input "4. PAT ATTEMPTED "; careerB8%(I)
                Locate 9: Input "5. PAT% "; careerB7%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9

                        Case 1
                            529 Locate 5: Input "1. NAME "; statsA7$(I)

                        Case 2
                            531 Locate 6: Input "2. FGA "; careerB5%(I)

                        Case 3
                            532 Locate 7: Input "3. FG% "; careerB4%(I)

                        Case 4
                            533 Locate 8: Input "4. PAT ATTEMPTED "; careerB8%(I)

                        Case 5
                            534 Locate 9: Input "5. PAT% "; careerB7%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next

        537 '
        careerB3%(0) = Int((B3!(0) - 28) / 2! + .5)

        For I = 0 To 1
            careerB4%(I) = Int((careerB4%(I) / 5) + .5) - 5
            If careerB4%(I) < 1 Then careerB4%(I) = 1
        Next

        'Interceptions
        2000 '
        Cls
        For I = 0 To 9: Cls
            Locate 3: Print "PLAYERS WITH INTERCEPTIONS -"; I + 1

            Locate 5: Input "1. NAME "; statsDI$(I)

            If UCase$(statsDI$(I)) <> "END" Then

                Locate 6: Input "2. # OF INTERCEPTIONS "; careerDI%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            Locate 5: Input "1. NAME "; statsDI$(I)

                        Case 2
                            Locate 6: Input "2. # OF INTERCEPTIONS "; careerDI%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'Sacks
        2009 '
        Cls
        For I = 0 To 14: Cls

            Locate 3: Print "PLAYERS WITH SACKS -"; I + 1

            Locate 5: Input "1. NAME "; statsDS$(I)

            If UCase$(statsDS$(I)) <> "END" Then

                Locate 6: Input "2. # OF SACKS "; careerDS%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            Locate 5: Input "1. NAME "; statsDS$(I)

                        Case 2
                            Locate 6: Input "2. # OF SACKS "; careerDS%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'All Done
        2099 '
        SM = 0
        ST = 0
        RM = 0
        RT = 0

        For I = 0 To 9: SM = SM + careerA0%(I): RM = RM + careerA2%(I): Next I

        SK = 100 / SM

        For I = 0 To 9:
            If careerA0%(I) <> 0 Then careerA0%(I) = Int(SK * careerA0%(I) + .5) + ST: ST = careerA0%(I)
            If careerA0%(I) > 100 Then careerA0%(I) = 100
        Next I

        For I = 0 To 9: careerA1%(I) = Int(A1!(I) / .3 + .5): Next I

        For I = 0 To 9: If O9 <> 0 And careerA1%(I) <> 0 Then careerA1%(I) = careerA1%(I) - O9
            'IF careerA1%(I) <  = 0 THEN careerA1%(I) = 1
        Next I

        For I = 0 To 5: RM = RM + careerA4%(I): Next I

        RK = 100 / RM

        For I = 0 To 9:
            If careerA2%(I) <> 0 Then careerA2%(I) = Int(RK * careerA2%(I) + .5) + RT: RT = careerA2%(I): If careerA2%(I) > 100 Then careerA2%(I) = 100
        Next I

        For I = 0 To 5:
            If careerA4%(I) <> 0 Then careerA4%(I) = Int(RK * careerA4%(I) + .5) + RT: RT = careerA4%(I): If careerA4%(I) > 100 Then careerA4%(I) = 100
        Next I

        For I = 0 To 9: careerA3%(I) = Int(careerA3%(I) / 3 + .5): Next I
        For I = 0 To 5: careerA5%(I) = Int(careerA5%(I) / 3 + .5): Next I

        PM = 0: PT = 0

        For I = 0 To 3: PM = PM + careerA6%(I): Next I

        PK = 100 / PM

        For I = 0 To 3:
            If careerA6%(I) <> 0 Then
                careerA6%(I) = Int(PK * careerA6%(I) + .5) + PT
                PT = careerA6%(I)
                If careerA6%(I) > 100 Then careerA6%(I) = 100
            End If
        Next I

        KM = 0
        KT = 0
        QM = 0
        QT = 0

        For I = 0 To 3:
            If O9 <> 0 And careerA7%(I) <> 0 Then careerA7%(I) = careerA7%(I) - O9
        Next I

        For I = 0 To 2:
            KM = KM + careerA9%(I)
            QM = QM + careerB1%(I)
            careerB%(I) = Int(careerB%(I) / 3! + .5)
            careerB2%(I) = Int(careerB2%(I) / 3 + .5)
        Next I

        KK = 100 / KM: QK = 100 / QM

        For I = 0 To 2:
            If careerA9%(I) <> 0 Then
                careerA9%(I) = Int(KK * careerA9%(I) + .5) + KT: KT = careerA9%(I)
                If careerA9%(I) > 100 Then careerA9%(I) = 100
            End If
        Next I

        For I = 0 To 2:
            If careerB1%(I) <> 0 Then careerB1%(I) = Int(QK * careerB1%(I) + .5) + QT
            QT = careerB1%(I): If careerB1%(I) > 100 Then careerB1%(I) = 100
        Next I

        PM = 0
        PT = 0
        careerB5%(0) = Int(careerB5%(0) / (careerB5%(0) + careerB5%(1)) * 100 + .5): careerB5%(1) = 100
        careerB8%(0) = Int(careerB8%(0) / (careerB8%(0) + careerB8%(1)) * 100 + .5): careerB8%(1) = 100

        For I = 9 To 0 Step -1:
            If careerA0%(I) <> 0 Then careerA0%(I) = 100: Exit For
        Next I

        For I = 5 To 0 Step -1:
            If careerA4%(I) <> 0 Then careerA4%(I) = 100: Exit For
        Next I

        For I = 3 To 0 Step -1:
            If careerA6%(I) <> 0 Then careerA6%(I) = 100: Exit For
        Next I

        For I = 2 To 0 Step -1:
            If careerA9%(I) <> 0 Then careerA9%(I) = 100: Exit For
        Next I

        For I = 2 To 0 Step -1:
            If careerB1%(I) <> 0 Then careerB1%(I) = 100: Exit For
        Next I

        QV = 0
        X = Int((Q / .3) + .5)
        If X < 0 Then X = 100 + X

        YC% = X

        If D9 > 0 Then careerB6%(0) = careerB6%(0) + D9: careerB6%(1) = careerB6%(1) + D9

        Call SaveTeam(teamNumber, YN$)

        For I = 0 To 7: careerB6%(I) = 0: Next I

        For I = 0 To 9:
            statsA1$(I) = ""
            careerA0%(I) = 0
            A1!(I) = 0
            careerA1%(I) = 0
            careerA2%(I) = 0
            careerA3%(I) = 0
        Next I

        For I = 0 To 5: statsA2$(I) = "": careerA4%(I) = 0: careerA5%(I) = 0: Next I

        For I = 0 To 3: statsA3$(I) = "": careerA6%(I) = 0: careerA7%(I) = 0: careerA8%(I) = 0: Next I

        For I = 0 To 2: statsA4$(I) = "": careerA9%(I) = 0: careerB%(I) = 0: Next I

        For I = 0 To 2: statsA5$(I) = "": careerB1%(I) = 0: careerB2%(I) = 0: Next I

        statsA6$(0) = "": careerB3%(0) = 0

        For I = 0 To 1:
            statsA7$(I) = ""
            careerB4%(I) = 0
            careerB7%(I) = 0
            careerB5%(I) = 0
            careerB8%(I) = 0
        Next

        For I = 0 To 9: statsDI$(I) = "": careerDI%(I) = 0: Next
        For I = 0 To 14: statsDS$(I) = "": careerDS%(I) = 0: Next

        Cls

        JJ = 3
        Locate 6, 8
        Color 15, 1

        Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";

        For II = 1 To JJ:
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        Locate 7 + JJ, 8
        Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";
		Locate 7

        U$ = "INPUT ANOTHER TEAM (YN)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        Color 15, 0
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'       InputTeam Subroutine
'----------------------------------------
'...explanation...
Sub SaveTeam (teamNumber, YN$)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared statsDI$(), statsDS$()

    Shared careerA0%(), careerA1%(), careerA2%(), careerA3%(), careerA4%()
    Shared careerA5%(), careerA6%(), careerA7%(), careerA8%(), careerA9%()

    Shared careerB%(), careerB1%(), careerB2%(), careerB3%(), careerB4%()
    Shared careerB5%(), careerB6%(), careerB7%(), careerB8%()
    Shared careerDS%(), careerDI%(), statsPA%(), careerQX%()

    9500 '
    If teamNumber = 0 Then
        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
        fileLength& = LOF(1)
        Close 1
        teamNumber = fileLength& / 934
        teamNumber = teamNumber + 1
    End If

    Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
    Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)
    Field #1, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$
    Field #1, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$
    Field #1, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$
    Field #1, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$
    Field #1, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$
    Field #1, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$
    Field #1, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$
    Field #1, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)
    Field #1, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)
    Field #1, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)
    Field #1, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)
    Field #1, 200 As Q$, 200 As Q$, 128 As Q$, 12 As Q$(122), 2 As Q$(123), 12 As Q$(124), 2 As Q$(125), 2 As Q$(126), 2 As Q$(127), 2 As Q$(128), 12 As Q$(129), 2 As Q$(130), 2 As Q$(131), 2 As Q$(132), 2 As Q$(133), 2 As Q$(134), 200 As Q$, 150 As Q$
    Field #1, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)
    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)
    Field #1, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)
    Field #1, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)
    Field #1, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

    LSet Q$(1) = A$
    LSet Q$(2) = AA$

    For I = 0 To 7: LSet Q$(I + 3) = MKI$(careerB6%(I)): Next I
    For I = 0 To 4: LSet Q$(I + 11) = MKI$(statsPA%(I)): Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = statsA1$(I)
        LSet Q$(I * 5 + 17) = MKI$(careerA0%(I))
        LSet Q$(I * 5 + 18) = MKI$(careerA1%(I))
        LSet Q$(I * 5 + 19) = MKI$(careerA2%(I))
        LSet Q$(I * 5 + 20) = MKI$(careerA3%(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = statsA2$(I)
        LSet Q$(I * 3 + 67) = MKI$(careerA4%(I))
        LSet Q$(I * 3 + 68) = MKI$(careerA5%(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = statsA3$(I)
        LSet Q$(I * 5 + 85) = MKI$(careerA6%(I))
        LSet Q$(I * 5 + 86) = MKI$(careerA7%(I))
        LSet Q$(I * 5 + 87) = MKI$(careerA8%(I))
        LSet Q$(I * 5 + 88) = MKI$(careerQX%(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = statsA4$(I)
        LSet Q$(I * 3 + 105) = MKI$(careerA9%(I))
        LSet Q$(I * 3 + 106) = MKI$(careerB%(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = statsA5$(I)
        LSet Q$(I * 3 + 114) = MKI$(careerB1%(I))
        LSet Q$(I * 3 + 115) = MKI$(careerB2%(I))
    Next I

    LSet Q$(122) = statsA6$(0)
    LSet Q$(123) = MKI$(careerB3%(0))
    LSet Q$(124) = statsA7$(0)
    LSet Q$(125) = MKI$(careerB4%(0))
    LSet Q$(126) = MKI$(careerB5%(0))
    LSet Q$(127) = MKI$(careerB7%(0))
    LSet Q$(128) = MKI$(careerB8%(0))
    LSet Q$(129) = statsA7$(1)
    LSet Q$(130) = MKI$(careerB4%(1))
    LSet Q$(131) = MKI$(careerB5%(1))
    LSet Q$(132) = MKI$(careerB7%(1))
    LSet Q$(133) = MKI$(careerB8%(1))
    LSet Q$(134) = MKI$(YC%)

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = statsDS$(I)
        LSet Q$(I * 2 + 136) = MKI$(careerDS%(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = statsDI$(I)
        LSet Q$(I * 2 + 166) = MKI$(careerDI%(I))
    Next I

    Put #1, teamNumber

    Close #1

    Open diskPaths$(0) + "STADIUMS." + YN$ For Random As #2 Len = 50
    Field #2, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)
    LSet Q$(1) = A$
    LSet Q$(2) = S$
    LSet Q$(3) = MKL$(ATT)
    LSet Q$(4) = "x"
    Put #2, teamNumber
    Close 2

End Sub
