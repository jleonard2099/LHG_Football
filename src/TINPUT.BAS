'----------------------------------------
'          InputTeam Subroutine
'----------------------------------------
'This subroutine assists the user with the
'steps for entering a new team.
Sub InputTeam ()

    Dim A1!(9), B3!(1)

    Shared leagueRatings%(), teamRatings%()
    
    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    Shared ydsCompAdj

    Do

        Color 15, 0
        Cls

        Locate 1: Input " 1. TEAM # (BLANK TO AUTO-ASSIGN) "; teamNumber
        Locate 2: Input " 2. DISK ID  "; YN$
        Locate 3: Input " 3. TEAM NAME (IE, 94 ARKANSAS) "; team$
        Locate 4: Input " 4. TEAM NICKNAME (IE, PANTHERS) "; mascot$
        Locate 5: Input " 5. DEF RUSH ADJ "; leagueRatings%(0)
        Locate 6: Input " 6. DEF COMP % ADJ "; leagueRatings%(1)
        Locate 7: Input " 7. DEF INT % ADJ "; leagueRatings%(2)
        Locate 8: Input " 8. FUM % (AVG=4) "; leagueRatings%(3)
        Locate 9: Input " 9. DEF FUM % (AVG=0) "; leagueRatings%(4)
        Locate 10: Input " 10. OFF SACKED % (AVG=8) "; leagueRatings%(5)
        Locate 11: Input "11. DEF SACK ADJ "; leagueRatings%(6)
        Locate 12: Input "12. DEF YD/COMP ADJ "; ydsCompAdj
        Locate 13: Input "13. TM PENALTY ADJ "; teamRatings%(0)
        Locate 14: Input "14. OFF RUN/PASS % "; leagueRatings%(7)
        Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)
        Locate 16: Input "16. POWER RATING "; teamRatings%(4)
        Locate 17: Input "17. TEXT COLOR (0-15) "; teamRatings%(2)
        Locate 18: Input "18. BACKGROUND COLOR (0-7) "; teamRatings%(1)

        If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
            Locate 17, 37
            Print "TEAM APPEARANCE: ";
            Locate 18, 37
            Color teamRatings%(2), teamRatings%(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
            Color 15, 0
        End If

        Locate 19: Input "19. OFF POWER ADJ "; O9
        Locate 20: Input "20. DEF POWER ADJ "; D9
        Locate 21: Input "21. STADIUM NAME "; stadium$
        Locate 22: Input "22. AVERAGE ATTENDANCE "; teamAttendance&

        Color 15, 0

        Do

            Do
                Locate 23
                Input "# TO CHANGE (0 TO CONTINUE ) "; B9
            Loop Until B9 >= 0 And B9 <= 22

            If B9 <> 0 Then

                Select Case B9

                    Case 1
                        Locate 1: Input " 1. TEAM # (BLANK TO AUTO-ASSIGN) "; teamNumber

                    Case 2
                        Locate 2: Input " 2. DISK ID  "; YN$

                    Case 3
                        Locate 3: Input " 3. TEAM NAME (IE. 94 ARKANSAS) "; team$

                    Case 4
                        Locate 4: Input " 4. TEAM NICKNAME (IE. PANTHERS) "; mascot$

                    Case 5
                        Locate 5: Input " 5. DEF RUSH ADJ "; leagueRatings%(0)

                    Case 6
                        Locate 6: Input " 6. DEF COMP % ADJ "; leagueRatings%(1)

                    Case 7
                        Locate 7: Input " 7. DEF INT % ADJ "; leagueRatings%(2)

                    Case 8
                        Locate 8: Input " 8. FUM % (AVG=4) "; leagueRatings%(3)

                    Case 9
                        Locate 9: Input " 9. DEF FUM % (AVG=0) "; leagueRatings%(4)

                    Case 10
                        Locate 10: Input " 10. OFF SACKED % (AVG=8) "; leagueRatings%(5)

                    Case 11
                        Locate 11: Input "11. DEF SACK ADJ "; leagueRatings%(6)

                    Case 12
                        Locate 12: Input "12. DEF YD/COMP ADJ "; ydsCompAdj

                    Case 13
                        Locate 13: Input "13. TM PENALTY ADJ "; teamRatings%(0)

                    Case 14
                        Locate 14: Input "14. OFF RUN/PASS % "; leagueRatings%(7)

                    Case 15
                        Locate 15: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; teamRatings%(3)

                    Case 16
                        Locate 16: Input "16. POWER RATING "; teamRatings%(4)

                    Case 17
                        Locate 17: Input "17. TEXT COLOR (0-15) "; teamRatings%(2)
                        If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                            Locate 17, 37
                            Print "TEAM APPEARANCE: ";
                            Locate 18, 37
                            Color teamRatings%(2), teamRatings%(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
                            Color 15, 0
                        End If

                    Case 18
                        Locate 18: Input "18. BACKGROUND COLOR (0-7) "; teamRatings%(1)
                        If teamRatings%(1) >= 0 And teamRatings%(2) >= 0 Then
                            Locate 17, 37
                            Print "TEAM APPEARANCE: ";
                            Locate 18, 37
                            Color teamRatings%(2), teamRatings%(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
                            Color 15, 0
                        End If

                    Case 19
                        Locate 19: Input "19. OFF POWER ADJ "; O9

                    Case 20
                        Locate 20: Input "20. DEF POWER ADJ "; D9

                    Case 21
                        Locate 21: Input "21. STADIUM NAME "; stadium$

                    Case 22
                        Locate 22: Input "22. AVERAGE ATTENDANCE "; teamAttendance&

                End Select

            End If

        Loop Until B9 = 0


        Color 15, 0

        'Running Backs
        Cls

        For I = 0 To 9:

            Cls

            Color 14
            Locate 3: Print "RUNNING BACKS - "; I + 1
            Color 15

            Locate 5: Input "1. NAME (END TO STOP)"; rbName$(I)

            If UCase$(rbName$(I)) <> "END" Then

                Locate 6: Input "2. # OF RUSHES "; rushContrib(I)
                Locate 7: Input "3. RUSHING AVERAGE "; A1!(I)
                Locate 8: Input "4. # OF RECEPTIONS "; rbRecepts(I)
                Locate 9: Input "5. RECEIVING AVERAGE "; rbRecAvg(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; rbName$(I)

                        Case 2
                            Locate 6: Input "2. # OF RUSHES "; rushContrib(I)

                        Case 3
                            Locate 7: Input "3. RUSHING AVERAGE "; A1!(I)

                        Case 4
                            Locate 8: Input "4. # OF RECEPTIONS "; rbRecepts(I)

                        Case 5
                            Locate 9: Input "5. RECEIVING AVERAGE "; rbRecAvg(I)

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next I


        'Wide Recievers
        Cls

        For I = 0 To 5:

            Cls

            Color 14
            Locate 3: Print "WIDE RECEIVERS/TIGHT ENDS - "; I + 1
            Color 15

            Locate 5: Input "1. NAME (END TO STOP)"; wrName$(I)

            If UCase$(wrName$(I)) <> "END" Then

                Locate 6: Input "2. # OF RECEPTIONS "; recContrib(I)
                Locate 7: Input "3. RECEIVING AVERAGE "; wrRecAvg(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; wrName$(I)

                        Case 2
                            Locate 6: Input "2. # OF RECEPTIONS "; recContrib(I)

                        Case 3
                            Locate 7: Input "3. RECEIVING AVERAGE "; wrRecAvg(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0
            Else

                Exit For

            End If

        Next I

        'Quarterbacks
        Cls

        For I = 0 To 3:

            Cls

            Color 14
            Locate 3: Print "QUARTERBACKS - "; I + 1
            Color 15

            Locate 5: Input "1. NAME (END TO STOP)"; qbName$(I)

            If UCase$(qbName$(I)) <> "END" Then

                Locate 6: Input "2. PASSES ATTEMPTED "; passContrib(I)
                Locate 7: Input "3. COMPLETION % "; compPct(I)
                Locate 8: Input "4. INTERCEPTION % "; qbIntPct(I)
                Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; armRating(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; qbName$(I)

                        Case 2
                            Locate 6: Input "2. PASSES ATTEMPTED "; passContrib(I)

                        Case 3
                            Locate 7: Input "3. COMPLETION % "; compPct(I)

                        Case 4
                            Locate 8: Input "4. INTERCEPTION % "; qbIntPct(I)

                        Case 5
                            Locate 9: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; armRating(I)

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next I

        'Kickoff Returners

        Cls

        For I = 0 To 2:

            Cls

            Color 14
            Locate 3: Print "KICKOFF RETURNMEN - "; I + 1
            Color 15

            Locate 5: Input "1. NAME (END TO STOP)"; krName$(I)

            If UCase$(krName$(I)) <> "END" Then

                Locate 6: Input "2. KICKOFFS RETURNED "; krContrib(I)
                Locate 7: Input "3. RETURN AVERAGE "; krRetAvg(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; krName$(I)

                        Case 2
                            Locate 6: Input "2. KICKOFFS RETURNED "; krContrib(I)

                        Case 3
                            Locate 7: Input "3. RETURN AVERAGE "; krRetAvg(I)

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next I

        'Punt Returners
        Cls

        For I = 0 To 2:

            Cls

            Color 14
            Locate 3: Print "PUNT RETURNMEN - "; I + 1
            Color 15

            Locate 5: Input "1. NAME (END TO STOP)"; prName$(I)

            If UCase$(prName$(I)) <> "END" Then

                Locate 6: Input "2. PUNTS RETURNED "; prContrib(I)
                Locate 7: Input "3. PUNT RETURN AVERAGE "; prRetAvg(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 3

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; prName$(I)

                        Case 2
                            Locate 6: Input "2. PUNTS RETURNED "; prContrib(I)

                        Case 3
                            Locate 7: Input "3. PUNT RETURN AVERAGE "; prRetAvg(I)

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'Punters
        Cls

        Color 14
        Locate 3: Print "PUNTER"
        Color 15

        Locate 4: Input "1. PUNTER NAME "; punterName$(0)
        Locate 5: Input "2. PUNT AVERAGE "; B3!(0)

        C1 = 1

        Do

            Do
                Locate 22
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 2

            Select Case B9

                Case 1
                    Locate 4: Input "1. PUNTER NAME "; punterName$(0)

                Case 2
                    Locate 5: Input "2. PUNT AVERAGE "; B3!(0)

            End Select

        Loop Until B9 = 0

        'Kickers
        Cls

        For I = 0 To 1

            Cls

            'Yellow color, centered ??
            Locate 3: Print "PLACE KICKER - "; I + 1

            Locate 5: Input "1. NAME (END TO STOP)"; pkName$(I)

            If UCase$(pkName$(I)) <> "END" Then

                Locate 6: Input "2. FGA "; pkFGContrib(I)
                Locate 7: Input "3. FG% "; pkFGA(I)
                Locate 8: Input "4. PAT ATTEMPTED "; pkPATContrib(I)
                Locate 9: Input "5. PAT% "; pkPAT(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; pkName$(I)

                        Case 2
                            Locate 6: Input "2. FGA "; pkFGContrib(I)

                        Case 3
                            Locate 7: Input "3. FG% "; pkFGA(I)

                        Case 4
                            Locate 8: Input "4. PAT ATTEMPTED "; pkPATContrib(I)

                        Case 5
                            Locate 9: Input "5. PAT% "; pkPAT(I)

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next

        puntAvg(0) = Int((B3!(0) - 28) / 2! + .5)

        For I = 0 To 1
            pkFGA(I) = Int((pkFGA(I) / 5) + .5) - 5
            If pkFGA(I) < 1 Then pkFGA(I) = 1
        Next

        'Interceptions
        Cls

        For I = 0 To 9:

            Cls

            Color 14
            Color 15
            Locate 3: Print "PLAYERS WITH INTERCEPTIONS - "; I + 1

            Locate 5: Input "1. NAME (END TO STOP)"; intName$(I)

            If UCase$(intName$(I)) <> "END" Then

                Locate 6: Input "2. # OF INTERCEPTIONS "; numInts(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 2

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; intName$(I)

                        Case 2
                            Locate 6: Input "2. # OF INTERCEPTIONS "; numInts(I)

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next I

        'Sacks
        Cls

        For I = 0 To 14:

            Cls

            Color 14
            Locate 3: Print "PLAYERS WITH SACKS - "; I + 1
            Color 15

            Locate 5: Input "1. NAME (END TO STOP)"; sackName$(I)

            If UCase$(sackName$(I)) <> "END" Then

                Locate 6: Input "2. # OF SACKS "; numSacks(I)

                Do

                    Do
                        Locate 22
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                    Loop Until B9 >= 0 And B9 <= 2

                    Select Case B9

                        Case 1
                            Locate 5: Input "1. NAME "; sackName$(I)

                        Case 2
                            Locate 6: Input "2. # OF SACKS "; numSacks(I)

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next I

        'Calculate final values
        SM = 0
        ST = 0
        RM = 0
        RT = 0

        For I = 0 To 9:
            SM = SM + rushContrib(I)
            RM = RM + rbRecepts(I)
        Next I

        SK = 100 / SM

        For I = 0 To 9:
            If rushContrib(I) <> 0 Then rushContrib(I) = Int(SK * rushContrib(I) + .5) + ST: ST = rushContrib(I)
            If rushContrib(I) > 100 Then rushContrib(I) = 100
        Next I

        For I = 0 To 9:
            rushAvg(I) = Int(A1!(I) / .3 + .5)
        Next I

        For I = 0 To 9:
            If O9 <> 0 And rushAvg(I) <> 0 Then rushAvg(I) = rushAvg(I) - O9
        Next I

        For I = 0 To 5: 
            RM = RM + recContrib(I)
        Next I

        RK = 100 / RM

        For I = 0 To 9:
            If rbRecepts(I) <> 0 Then 
                rbRecepts(I) = Int(RK * rbRecepts(I) + .5) + RT
                RT = rbRecepts(I)
                If rbRecepts(I) > 100 Then rbRecepts(I) = 100
            End If
        Next I

        For I = 0 To 5:
            If recContrib(I) <> 0 Then 
                recContrib(I) = Int(RK * recContrib(I) + .5) + RT
                RT = recContrib(I)
                If recContrib(I) > 100 Then recContrib(I) = 100
            End If
        Next I

        For I = 0 To 9:
            rbRecAvg(I) = Int(rbRecAvg(I) / 3 + .5)
        Next I

        For I = 0 To 5:
            wrRecAvg(I) = Int(wrRecAvg(I) / 3 + .5)
        Next I

        PM = 0
        PT = 0

        For I = 0 To 3:
            PM = PM + passContrib(I)
        Next I

        PK = 100 / PM

        For I = 0 To 3:

            If passContrib(I) <> 0 Then
                passContrib(I) = Int(PK * passContrib(I) + .5) + PT
                PT = passContrib(I)
                If passContrib(I) > 100 Then passContrib(I) = 100
            End If

        Next I

        KM = 0
        KT = 0
        QM = 0
        QT = 0

        For I = 0 To 3:
            If O9 <> 0 And compPct(I) <> 0 Then compPct(I) = compPct(I) - O9
        Next I

        For I = 0 To 2:
            KM = KM + krContrib(I)
            QM = QM + prContrib(I)
            krRetAvg(I) = Int(krRetAvg(I) / 3! + .5)
            prRetAvg(I) = Int(prRetAvg(I) / 3 + .5)
        Next I

        KK = 100 / KM
        QK = 100 / QM

        For I = 0 To 2:
            If krContrib(I) <> 0 Then
                krContrib(I) = Int(KK * krContrib(I) + .5) + KT
                KT = krContrib(I)
                If krContrib(I) > 100 Then krContrib(I) = 100
            End If
        Next I

        For I = 0 To 2:
            If prContrib(I) <> 0 Then prContrib(I) = Int(QK * prContrib(I) + .5) + QT
            QT = prContrib(I)
            If prContrib(I) > 100 Then prContrib(I) = 100
        Next I

        PM = 0
        PT = 0
        pkFGContrib(0) = Int(pkFGContrib(0) / (pkFGContrib(0) + pkFGContrib(1)) * 100 + .5)
        pkFGContrib(1) = 100
        pkPATContrib(0) = Int(pkPATContrib(0) / (pkPATContrib(0) + pkPATContrib(1)) * 100 + .5)
        pkPATContrib(1) = 100

        For I = 9 To 0 Step -1:
            If rushContrib(I) <> 0 Then rushContrib(I) = 100: Exit For
        Next I

        For I = 5 To 0 Step -1:
            If recContrib(I) <> 0 Then recContrib(I) = 100: Exit For
        Next I

        For I = 3 To 0 Step -1:
            If passContrib(I) <> 0 Then passContrib(I) = 100: Exit For
        Next I

        For I = 2 To 0 Step -1:
            If krContrib(I) <> 0 Then krContrib(I) = 100: Exit For
        Next I

        For I = 2 To 0 Step -1:
            If prContrib(I) <> 0 Then prContrib(I) = 100: Exit For
        Next I

        X = Int((ydsCompAdj / .3) + .5)
        If X < 0 Then X = 100 + X
        ydsCompAdj = X

        If D9 > 0 Then 
            leagueRatings%(0) = leagueRatings%(0) + D9
            leagueRatings%(1) = leagueRatings%(1) + D9
        End If

        Call SaveTeam(team$, mascot$, YN$, teamNumber, teamAttendance&)

        Cls

        Call box0 (6, 8, 8, 70, 2, 15, 1)

        Locate 7

        U$ = "INPUT ANOTHER TEAM? (Y/N)"
        Locate , (80 - Len(U$)) / 2
        Print U$
        Color 15, 0
        I$ = GetKeyPress$

    Loop Until UCase$(I$) = "N"

End Sub

'----------------------------------------
'          SaveTeam Subroutine
'----------------------------------------
'This subroutine saves the modifed team
'data (from corresponding arrays) to an
'appropriate spot in the select team file
Sub SaveTeam (team$, mascot$, YN$, teamNumber, teamAttendance&)

    Shared ydsCompAdj

    Shared mascot$, stadium$

    Shared leagueRatings%(), teamRatings%()
    
    Shared rushContrib(), rushAvg(), rbRecepts(), rbRecAvg()
    Shared recContrib(), wrRecAvg()
    Shared passContrib(), compPct(), qbIntPct()
    Shared krContrib(), krRetAvg(), prContrib(), prRetAvg()
    Shared puntAvg()
    Shared pkFGA(), pkFGContrib(), pkPAT(), pkPATContrib()
    Shared numInts(), numSacks(), armRating()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$()
    Shared punterName$(), pkName$(), intName$(), sackName$()

    If teamNumber = 0 Then

        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + YN$ For Random As #1 Len = TEAM_SIZE_BYTES
        fileLength& = LOF(1)
        Close #1

        teamNumber = (fileLength& / TEAM_SIZE_BYTES) + 1

    End If

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + YN$ For Random As #2 Len = TEAM_SIZE_BYTES

    Field #2, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)

    Field #2, 56 As Q$(0), 12 As Q$(16), 2 As Q$(17), 2 As Q$(18), 2 As Q$(19), 2 As Q$(20), 12 As Q$(21), 2 As Q$(22), 2 As Q$(23), 2 As Q$(24), 2 As Q$(25), 200 As Q$, 200 As Q$(200), 238 As Q$(201), 200 As Q$

    Field #2, 96 As Q$(0), 12 As Q$(26), 2 As Q$(27), 2 As Q$(28), 2 As Q$(29), 2 As Q$(30), 12 As Q$(31), 2 As Q$(32), 2 As Q$(33), 2 As Q$(34), 2 As Q$(35), 200 As Q$(199), 200 As Q$(200), 198 As Q$(201), 200 As Q$

    Field #2, 136 As Q$(0), 12 As Q$(36), 2 As Q$(37), 2 As Q$(38), 2 As Q$(39), 2 As Q$(40), 12 As Q$(41), 2 As Q$(42), 2 As Q$(43), 2 As Q$(44), 2 As Q$(45), 200 As Q$(199), 200 As Q$(200), 158 As Q$(201), 200 As Q$

    Field #2, 176 As Q$(0), 12 As Q$(46), 2 As Q$(47), 2 As Q$(48), 2 As Q$(49), 2 As Q$(50), 12 As Q$(51), 2 As Q$(52), 2 As Q$(53), 2 As Q$(54), 2 As Q$(55), 200 As Q$(199), 200 As Q$(200), 118 As Q$(201), 200 As Q$

    Field #2, 216 As Q$(0), 12 As Q$(56), 2 As Q$(57), 2 As Q$(58), 2 As Q$(59), 2 As Q$(60), 12 As Q$(61), 2 As Q$(62), 2 As Q$(63), 2 As Q$(64), 2 As Q$(65), 200 As Q$(199), 200 As Q$(200), 78 As Q$(201), 200 As Q$

    Field #2, 200 As Q$(0), 56 As Q$, 12 As Q$(66), 2 As Q$(67), 2 As Q$(68), 12 As Q$(69), 2 As Q$(70), 2 As Q$(71), 12 As Q$(72), 2 As Q$(73), 2 As Q$(74), 200 As Q$(99), 200 As Q$(200), 200 As Q$, 30 As Q$

    Field #2, 200 As Q$(0), 100 As Q$, 4 As Q$(201), 12 As Q$(75), 2 As Q$(76), 2 As Q$(77), 12 As Q$(78), 2 As Q$(79), 2 As Q$(80), 12 As Q$(81), 2 As Q$(82), 2 As Q$(83), 200 As Q$(199), 182 As Q$(200), 200 As Q$

    Field #2, 200 As Q$(0), 152 As Q$(201), 12 As Q$(84), 2 As Q$(85), 2 As Q$(86), 2 As Q$(87), 2 As Q$(88), 12 As Q$(89), 2 As Q$(90), 2 As Q$(91), 2 As Q$(92), 2 As Q$(93), 200 As Q$(199), 200 As Q$(200), 142 As Q$(202)

    Field #2, 200 As Q$(0), 192 As Q$(201), 12 As Q$(94), 2 As Q$(95), 2 As Q$(96), 2 As Q$(97), 2 As Q$(98), 12 As Q$(99), 2 As Q$(100), 2 As Q$(101), 2 As Q$(102), 2 As Q$(103), 200 As Q$(199), 200 As Q$(200), 102 As Q$(202)

    Field #2, 200 As Q$(0), 200 As Q$(199), 32 As Q$(200), 12 As Q$(104), 2 As Q$(105), 2 As Q$(106), 12 As Q$(107), 2 As Q$(108), 2 As Q$(109), 12 As Q$(110), 2 As Q$(111), 2 As Q$(112), 200 As Q$(201), 200 As Q$(202), 54 As Q$(203)

    Field #2, 200 As Q$(0), 200 As Q$(199), 80 As Q$(200), 12 As Q$(113), 2 As Q$(114), 2 As Q$(115), 12 As Q$(116), 2 As Q$(117), 2 As Q$(118), 12 As Q$(119), 2 As Q$(120), 2 As Q$(121), 200 As Q$(201), 200 As Q$(202), 6 As Q$(203)

    Field #2, 200 As Q$, 200 As Q$, 128 As Q$, 12 As Q$(122), 2 As Q$(123), 12 As Q$(124), 2 As Q$(125), 2 As Q$(126), 2 As Q$(127), 2 As Q$(128), 12 As Q$(129), 2 As Q$(130), 2 As Q$(131), 2 As Q$(132), 2 As Q$(133), 2 As Q$(134), 200 As Q$, 150 As Q$

    Field #2, 200 As Q$(0), 200 As Q$(199), 184 As Q$(200), 12 As Q$(135), 2 As Q$(136), 12 As Q$(137), 2 As Q$(138), 12 As Q$(139), 2 As Q$(140), 12 As Q$(141), 2 As Q$(142), 12 As Q$(143), 2 As Q$(144), 200 As Q$(201), 80 As Q$(202)

    Field #2, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 154 As Q$(200), 12 As Q$(145), 2 As Q$(146), 12 As Q$(147), 2 As Q$(148), 12 As Q$(149), 2 As Q$(150), 12 As Q$(151), 2 As Q$(152), 12 As Q$(153), 2 As Q$(154), 200 As Q$(201), 10 As Q$(202)

    Field #2, 200 As Q$(0), 100 As Q$, 200 As Q$(199), 224 As Q$(200), 12 As Q$(155), 2 As Q$(156), 12 As Q$(157), 2 As Q$(158), 12 As Q$(159), 2 As Q$(160), 12 As Q$(161), 2 As Q$(162), 12 As Q$(163), 2 As Q$(164), 140 As Q$(201)

    Field #2, 200 As Q$(0), 200 As Q$, 200 As Q$(199), 194 As Q$(200), 12 As Q$(165), 2 As Q$(166), 12 As Q$(167), 2 As Q$(168), 12 As Q$(169), 2 As Q$(170), 12 As Q$(171), 2 As Q$(172), 12 As Q$(173), 2 As Q$(174), 70 As Q$(201)

    Field #2, 232 As Q$(0), 200 As Q$, 200 As Q$(199), 232 As Q$(200), 12 As Q$(175), 2 As Q$(176), 12 As Q$(177), 2 As Q$(178), 12 As Q$(179), 2 As Q$(180), 12 As Q$(181), 2 As Q$(182), 12 As Q$(183), 2 As Q$(184)

    LSet Q$(1) = team$
    LSet Q$(2) = mascot$

    For I = 0 To 7: 
        LSet Q$(I + 3) = MKI$(leagueRatings%(I))
    Next I
    
    For I = 0 To 4: 
        LSet Q$(I + 11) = MKI$(teamRatings%(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 5 + 16) = rbName$(I)
        LSet Q$(I * 5 + 17) = MKI$(rushContrib(I))
        LSet Q$(I * 5 + 18) = MKI$(rushAvg(I))
        LSet Q$(I * 5 + 19) = MKI$(rbRecepts(I))
        LSet Q$(I * 5 + 20) = MKI$(rbRecAvg(I))
    Next I

    For I = 0 To 5
        LSet Q$(I * 3 + 66) = wrName$(I)
        LSet Q$(I * 3 + 67) = MKI$(recContrib(I))
        LSet Q$(I * 3 + 68) = MKI$(wrRecAvg(I))
    Next I

    For I = 0 To 3
        LSet Q$(I * 5 + 84) = qbName$(I)
        LSet Q$(I * 5 + 85) = MKI$(passContrib(I))
        LSet Q$(I * 5 + 86) = MKI$(compPct(I))
        LSet Q$(I * 5 + 87) = MKI$(qbIntPct(I))
        LSet Q$(I * 5 + 88) = MKI$(armRating(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 104) = krName$(I)
        LSet Q$(I * 3 + 105) = MKI$(krContrib(I))
        LSet Q$(I * 3 + 106) = MKI$(krRetAvg(I))
    Next I

    For I = 0 To 2
        LSet Q$(I * 3 + 113) = prName$(I)
        LSet Q$(I * 3 + 114) = MKI$(prContrib(I))
        LSet Q$(I * 3 + 115) = MKI$(prRetAvg(I))
    Next I

    LSet Q$(122) = punterName$(0)
    LSet Q$(123) = MKI$(puntAvg(0))
    
    LSet Q$(124) = pkName$(0)
    LSet Q$(125) = MKI$(pkFGA(0))
    LSet Q$(126) = MKI$(pkFGContrib(0))
    LSet Q$(127) = MKI$(pkPAT(0))
    LSet Q$(128) = MKI$(pkPATContrib(0))
    
    LSet Q$(129) = pkName$(1)
    LSet Q$(130) = MKI$(pkFGA(1))
    LSet Q$(131) = MKI$(pkFGContrib(1))
    LSet Q$(132) = MKI$(pkPAT(1))
    LSet Q$(133) = MKI$(pkPATContrib(1))
    LSet Q$(134) = MKI$(ydsCompAdj)

    For I = 0 To 14
        LSet Q$(I * 2 + 135) = sackName$(I)
        LSet Q$(I * 2 + 136) = MKI$(numSacks(I))
    Next I

    For I = 0 To 9
        LSet Q$(I * 2 + 165) = intName$(I)
        LSet Q$(I * 2 + 166) = MKI$(numInts(I))
    Next I

    Put #2, teamNumber

    Close #2

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + YN$ For Random As #3 Len = DATA_SIZE_BYTES

    Field #3, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)

    LSet Q$(1) = team$
    LSet Q$(2) = stadium$
    LSet Q$(3) = MKL$(teamAttendance&)
    LSet Q$(4) = "x"

    Put #3, teamNumber

    Close #3

    'Reset everything
    Erase leagueRatings%

    Erase armRating, compPct, passContrib, qbIntPct
    Erase krRetAvg, krContrib, prRetAvg, prContrib
    Erase numInts, numSacks
    Erase pkFGA, pkFGContrib, pkPAT, pkPATContrib, puntAvg
    Erase rushContrib, rushAvg, rbRecepts, rbRecAvg
    Erase wrRecAvg, recContrib
    Erase intName$, sackName$
    Erase krName$, pkName$, prName$, rbName$, wrName$, qbName$

    punterName$(0) = ""
    puntAvg(0) = 0

End Sub

