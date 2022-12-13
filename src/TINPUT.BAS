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

    Shared A1$(), A2$(), A3$(), A4$(), A5$(), A6$(), A7$()
    Shared DI$(), DS$()

    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()

    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()
    Shared DS%(), DI%(), PA%(), QX%()

    Do

        'Color 15, 1
        Color 15, 0

        Cls

        Locate 1: Input " 1. DISK ID  "; YN$
        Locate 2: Input " 2. TEAM # (ENTER FOR PROGRAM TO ASSIGN) "; teamNumber
        Locate 3: Input " 3. TEAM NAME (IE. 94 ARKANSAS) "; A$
        Locate 4: Input " 4. TEAM NICKNAME (IE. PANTHERS) "; AA$
        Locate 5: Input " 5. DEF RUSH ADJ "; B6%(0)
        Locate 6: Input " 6. DEF COMP % ADJ "; B6%(1)
        Locate 7: Input " 7. DEF INT % ADJ "; B6%(2)
        Locate 8: Input " 8. FUM % (AVG=4) "; B6%(3)
        Locate 9: Input " 9. DEF FUM % (AVG=0) "; B6%(4)
        Locate 10: Input " 10. OFF SACKED % (AVG=8) "; B6%(5)
        Locate 11: Input "11. DEF SACK ADJ "; B6%(6)
        Locate 12: Input "12. DEF YD/COMP ADJ "; Q
        Locate 13: Input "13. TM PENALTY ADJ "; PA%(0)
        Locate 14: Input "14. OFF RUN/PASS % "; B6%(7)
        Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; PA%(3)
        Locate 16: Input "16. POWER RATING "; PA%(4)
        Locate 17: Input "17. BACKGROUND COLOR (0-7) "; PA%(1)
        Locate 18: Input "18. TEXT COLOR (0-15) "; PA%(2)
        Locate 19: Input "19. OFF POWER ADJ "; O9
        Locate 20: Input "20. DEF POWER ADJ "; D9
        Locate 21: Input "21. STADIUM NAME "; S$
        Locate 22: Input "22. AVERAGE ATTENDANCE "; ATT
        Locate 1, 40: Color PA%(2), PA%(1): Print "SAMPLE TEXT COLOR"

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
                    404 Locate 5: Input " 5. DEF RUSH ADJ "; B6%(0)

                Case 6
                    405 Locate 6: Input " 6. DEF COMP % ADJ "; B6%(1)

                Case 7
                    406 Locate 7: Input " 7. DEF INT % ADJ "; B6%(2)

                Case 8
                    407 Locate 8: Input " 8. FUM % (AVG=4) "; B6%(3)

                Case 9
                    408 Locate 9: Input " 9. DEF FUM % (AVG=0) "; B6%(4)

                Case 10
                    409 Locate 10: Input " 10. OFF SACKED % (AVG=8) "; B6%(5)

                Case 11
                    410 Locate 11: Input "11. DEF SACK ADJ "; B6%(6)

                Case 12
                    411 Locate 12: Input "12. DEF YD/COMP ADJ "; Q

                Case 13
                    412 Locate 13: Input "13. TM PENALTY ADJ "; PA%(0)

                Case 14
                    413 Locate 14: Input "14. OFF RUN/PASS % "; B6%(7)

                Case 15
                    414 Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; PA%(3)

                Case 16
                    415 Locate 16: Input "16. POWER RATING "; PA%(4)

                Case 17
                    416 Locate 17: Input "17. BACKGROUND COLOR (0-7) "; PA%(1)

                Case 18
                    417 Locate 18: Input "18. TEXT COLOR (0-15) "; PA%(2)

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
        Locate 1, 40: Color PA%(2), PA%(1): Print "SAMPLE TEXT COLOR"

        'Case 0 / Continue
        C1 = 0

        'Running Backs
        Cls
        Color 15, 0

        For I = 0 To 9: Cls

            Locate 3: Print "RUNNING BACKS -"; I + 1

            Locate 5: Input "1. NAME "; A1$(I)

            If UCase$(A1$(I)) <> "END" Then

                Locate 6: Input "2. # OF RUSHES "; A0%(I)
                Locate 7: Input "3. RUSHING AVERAGE "; A1!(I)
                Locate 8: Input "4. # OF RECEPTIONS "; A2%(I)
                Locate 9: Input "5. RECEIVING AVERAGE "; A3%(I)

                432 '
                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE)"; B9

                    Select Case B9
                        Case 1
                            426 Locate 5: Input "1. NAME "; A1$(I)

                        Case 2
                            428 Locate 6: Input "2. # OF RUSHES "; A0%(I)

                        Case 3
                            429 Locate 7: Input "3. RUSHING AVERAGE "; A1!(I)

                        Case 4
                            430 Locate 8: Input "4. # OF RECEPTIONS "; A2%(I)

                        Case 5
                            431 Locate 9: Input "5. RECEIVING AVERAGE "; A3%(I)

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

            Locate 5: Input "1. NAME "; A2$(I)

            If UCase$(A2$(I)) <> "END" Then

                Locate 6: Input "2. # OF RECEPTIONS "; A4%(I)
                Locate 7: Input "3. RECEIVING AVERAGE "; A5%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            443 Locate 5: Input "1. NAME "; A2$(I)

                        Case 2
                            445 Locate 6: Input "2. # OF RECEPTIONS "; A4%(I)

                        Case 3
                            446 Locate 7: Input "3. RECEIVING AVERAGE "; A5%(I)

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

            Locate 5: Input "1. NAME "; A3$(I)

            If UCase$(A3$(I)) <> "END" Then

                Locate 6: Input "2. PASSES ATTEMPTED "; A6%(I)
                Locate 7: Input "3. COMPLETION % "; A7%(I)
                Locate 8: Input "4. INTERCEPTION % "; A8%(I)
                Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; QX%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            463 Locate 5: Input "1. NAME "; A3$(I)

                        Case 2
                            465 Locate 6: Input "2. PASSES ATTEMPTED "; A6%(I)

                        Case 3
                            466 Locate 7: Input "3. COMPLETION % "; A7%(I)

                        Case 4
                            467 Locate 8: Input "4. INTERCEPTION % "; A8%(I)

                        Case 5
                            468 Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; QX%(I)

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

            Locate 5: Input "1. NAME "; A4$(I)

            If UCase$(A4$(I)) <> "END" Then

                Locate 6: Input "2. KICKOFFS RETURNED "; A9%(I)
                Locate 7: Input "3. RETURN AVERAGE "; B%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            483 Locate 5: Input "1. NAME "; A4$(I)

                        Case 2
                            485 Locate 6: Input "2. KICKOFFS RETURNED "; A9%(I)

                        Case 3
                            486 Locate 7: Input "3. RETURN AVERAGE "; B%(I)

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

            Locate 5: Input "1. NAME "; A5$(I)

            If UCase$(A5$(I)) <> "END" Then

                Locate 6: Input "2. PUNTS RETURNED "; B1%(I)
                Locate 7: Input "3. PUNT RETURN AVERAGE "; B2%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            503 Locate 5: Input "1. NAME "; A5$(I)

                        Case 2
                            505 Locate 6: Input "2. PUNTS RETURNED "; B1%(I)

                        Case 3
                            506 Locate 7: Input "3. PUNT RETURN AVERAGE "; B2%(I)

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

        Locate 3: Input "1. PUNTER NAME "; A6$(0)
        Locate 4: Input "2. PUNT AVERAGE "; B3!(0)

        C1 = 1

        Do

            Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

            Select Case B9
                Case 1
                    521 Locate 3: Input "1. PUNTER NAME "; A6$(0)

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

            Locate 5: Input "1. NAME "; A7$(I)

            If UCase$(A7$(I)) <> "END" Then

                Locate 6: Input "2. FGA "; B5%(I)
                Locate 7: Input "3. FG% "; B4%(I)
                Locate 8: Input "4. PAT ATTEMPTED "; B8%(I)
                Locate 9: Input "5. PAT% "; B7%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9

                        Case 1
                            529 Locate 5: Input "1. NAME "; A7$(I)

                        Case 2
                            531 Locate 6: Input "2. FGA "; B5%(I)

                        Case 3
                            532 Locate 7: Input "3. FG% "; B4%(I)

                        Case 4
                            533 Locate 8: Input "4. PAT ATTEMPTED "; B8%(I)

                        Case 5
                            534 Locate 9: Input "5. PAT% "; B7%(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next

        537 '
        B3%(0) = Int((B3!(0) - 28) / 2! + .5)

        For I = 0 To 1
            B4%(I) = Int((B4%(I) / 5) + .5) - 5
            If B4%(I) < 1 Then B4%(I) = 1
        Next

        'Interceptions
        2000 '
        Cls
        For I = 0 To 9: Cls
            Locate 3: Print "PLAYERS WITH INTERCEPTIONS -"; I + 1

            Locate 5: Input "1. NAME "; DI$(I)

            If UCase$(DI$(I)) <> "END" Then

                Locate 6: Input "2. # OF INTERCEPTIONS "; DI%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            Locate 5: Input "1. NAME "; DI$(I)

                        Case 2
                            Locate 6: Input "2. # OF INTERCEPTIONS "; DI%(I)

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

            Locate 5: Input "1. NAME "; DS$(I)

            If UCase$(DS$(I)) <> "END" Then

                Locate 6: Input "2. # OF SACKS "; DS%(I)

                C1 = 1

                Do

                    Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

                    Select Case B9
                        Case 1
                            Locate 5: Input "1. NAME "; DS$(I)

                        Case 2
                            Locate 6: Input "2. # OF SACKS "; DS%(I)

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

        For I = 0 To 9: SM = SM + A0%(I): RM = RM + A2%(I): Next I

        SK = 100 / SM

        For I = 0 To 9:
            If A0%(I) <> 0 Then A0%(I) = Int(SK * A0%(I) + .5) + ST: ST = A0%(I)
            If A0%(I) > 100 Then A0%(I) = 100
        Next I

        For I = 0 To 9: A1%(I) = Int(A1!(I) / .3 + .5): Next I

        For I = 0 To 9: If O9 <> 0 And A1%(I) <> 0 Then A1%(I) = A1%(I) - O9
            'IF A1%(I) <  = 0 THEN A1%(I) = 1
        Next I

        For I = 0 To 5: RM = RM + A4%(I): Next I

        RK = 100 / RM

        For I = 0 To 9:
            If A2%(I) <> 0 Then A2%(I) = Int(RK * A2%(I) + .5) + RT: RT = A2%(I): If A2%(I) > 100 Then A2%(I) = 100
        Next I

        For I = 0 To 5:
            If A4%(I) <> 0 Then A4%(I) = Int(RK * A4%(I) + .5) + RT: RT = A4%(I): If A4%(I) > 100 Then A4%(I) = 100
        Next I

        For I = 0 To 9: A3%(I) = Int(A3%(I) / 3 + .5): Next I
        For I = 0 To 5: A5%(I) = Int(A5%(I) / 3 + .5): Next I

        PM = 0: PT = 0

        For I = 0 To 3: PM = PM + A6%(I): Next I

        PK = 100 / PM

        For I = 0 To 3:
            If A6%(I) <> 0 Then
                A6%(I) = Int(PK * A6%(I) + .5) + PT
                PT = A6%(I)
                If A6%(I) > 100 Then A6%(I) = 100
            End If
        Next I

        KM = 0
        KT = 0
        QM = 0
        QT = 0

        For I = 0 To 3:
            If O9 <> 0 And A7%(I) <> 0 Then A7%(I) = A7%(I) - O9
        Next I

        For I = 0 To 2:
            KM = KM + A9%(I)
            QM = QM + B1%(I)
            B%(I) = Int(B%(I) / 3! + .5)
            B2%(I) = Int(B2%(I) / 3 + .5)
        Next I

        KK = 100 / KM: QK = 100 / QM

        For I = 0 To 2:
            If A9%(I) <> 0 Then
                A9%(I) = Int(KK * A9%(I) + .5) + KT: KT = A9%(I)
                If A9%(I) > 100 Then A9%(I) = 100
            End If
        Next I

        For I = 0 To 2:
            If B1%(I) <> 0 Then B1%(I) = Int(QK * B1%(I) + .5) + QT
            QT = B1%(I): If B1%(I) > 100 Then B1%(I) = 100
        Next I

        PM = 0
        PT = 0
        B5%(0) = Int(B5%(0) / (B5%(0) + B5%(1)) * 100 + .5): B5%(1) = 100
        B8%(0) = Int(B8%(0) / (B8%(0) + B8%(1)) * 100 + .5): B8%(1) = 100

        For I = 9 To 0 Step -1:
            If A0%(I) <> 0 Then A0%(I) = 100: Exit For
        Next I

        For I = 5 To 0 Step -1:
            If A4%(I) <> 0 Then A4%(I) = 100: Exit For
        Next I

        For I = 3 To 0 Step -1:
            If A6%(I) <> 0 Then A6%(I) = 100: Exit For
        Next I

        For I = 2 To 0 Step -1:
            If A9%(I) <> 0 Then A9%(I) = 100: Exit For
        Next I

        For I = 2 To 0 Step -1:
            If B1%(I) <> 0 Then B1%(I) = 100: Exit For
        Next I

        QV = 0
        X = Int((Q / .3) + .5)
        If X < 0 Then X = 100 + X

        YC% = X

        If D9 > 0 Then B6%(0) = B6%(0) + D9: B6%(1) = B6%(1) + D9

        Call SaveTeam(teamNumber, YN$)

        For I = 0 To 7: B6%(I) = 0: Next I

        For I = 0 To 9:
            A1$(I) = ""
            A0%(I) = 0
            A1!(I) = 0
            A1%(I) = 0
            A2%(I) = 0
            A3%(I) = 0
        Next I

        For I = 0 To 5: A2$(I) = "": A4%(I) = 0: A5%(I) = 0: Next I

        For I = 0 To 3: A3$(I) = "": A6%(I) = 0: A7%(I) = 0: A8%(I) = 0: Next I

        For I = 0 To 2: A4$(I) = "": A9%(I) = 0: B%(I) = 0: Next I

        For I = 0 To 2: A5$(I) = "": B1%(I) = 0: B2%(I) = 0: Next I

        A6$(0) = "": B3%(0) = 0

        For I = 0 To 1:
            A7$(I) = ""
            B4%(I) = 0
            B7%(I) = 0
            B5%(I) = 0
            B8%(I) = 0
        Next

        For I = 0 To 9: DI$(I) = "": DI%(I) = 0: Next
        For I = 0 To 14: DS$(I) = "": DS%(I) = 0: Next

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

    Shared A1$(), A2$(), A3$(), A4$(), A5$(), A6$(), A7$()
    Shared DI$(), DS$()

    Shared A0%(), A1%(), A2%(), A3%(), A4%()
    Shared A5%(), A6%(), A7%(), A8%(), A9%()

    Shared B%(), B1%(), B2%(), B3%(), B4%()
    Shared B5%(), B6%(), B7%(), B8%()
    Shared DS%(), DI%(), PA%(), QX%()

    9500 '
    If teamNumber = 0 Then
        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
        Y = LOF(1)
        Close 1
        teamNumber = Y / 934
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

    For I = 0 To 7: LSet Q$(I + 3) = MKI$(B6%(I)): Next I
    For I = 0 To 4: LSet Q$(I + 11) = MKI$(PA%(I)): Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = A1$(I)
        LSet Q$(I * 5 + 17) = MKI$(A0%(I))
        LSet Q$(I * 5 + 18) = MKI$(A1%(I))
        LSet Q$(I * 5 + 19) = MKI$(A2%(I))
        LSet Q$(I * 5 + 20) = MKI$(A3%(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = A2$(I)
        LSet Q$(I * 3 + 67) = MKI$(A4%(I))
        LSet Q$(I * 3 + 68) = MKI$(A5%(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = A3$(I)
        LSet Q$(I * 5 + 85) = MKI$(A6%(I))
        LSet Q$(I * 5 + 86) = MKI$(A7%(I))
        LSet Q$(I * 5 + 87) = MKI$(A8%(I))
        LSet Q$(I * 5 + 88) = MKI$(QX%(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = A4$(I)
        LSet Q$(I * 3 + 105) = MKI$(A9%(I))
        LSet Q$(I * 3 + 106) = MKI$(B%(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = A5$(I)
        LSet Q$(I * 3 + 114) = MKI$(B1%(I))
        LSet Q$(I * 3 + 115) = MKI$(B2%(I))
    Next I

    LSet Q$(122) = A6$(0)
    LSet Q$(123) = MKI$(B3%(0))
    LSet Q$(124) = A7$(0)
    LSet Q$(125) = MKI$(B4%(0))
    LSet Q$(126) = MKI$(B5%(0))
    LSet Q$(127) = MKI$(B7%(0))
    LSet Q$(128) = MKI$(B8%(0))
    LSet Q$(129) = A7$(1)
    LSet Q$(130) = MKI$(B4%(1))
    LSet Q$(131) = MKI$(B5%(1))
    LSet Q$(132) = MKI$(B7%(1))
    LSet Q$(133) = MKI$(B8%(1))
    LSet Q$(134) = MKI$(YC%)

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = DS$(I)
        LSet Q$(I * 2 + 136) = MKI$(DS%(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = DI$(I)
        LSet Q$(I * 2 + 166) = MKI$(DI%(I))
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
