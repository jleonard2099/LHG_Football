'----------------------------------------
'          InputTeam Subroutine
'----------------------------------------
'This subroutine assists the user with the
'steps for entering a new team.
Sub InputTeam ()

    Dim rushAvgRaw!(9), puntAvgRaw!(1)

    Shared leagueRatings(), teamRatings()

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

        Locate 2, 1
        Color 14
        Print "TEAM INFORMATION"
        Color 15

        Locate 4: Input " 1. TEAM # (BLANK TO AUTO-ASSIGN) "; teamNumber
        Locate 4, 35: Print " "
        Locate 5: Input " 2. DISK ID "; teamYear$
        Locate 5, 13: Print " "
        Locate 6: Input " 3. TEAM NAME (IE, 94 ARKANSAS) "; team$
        Locate 6, 33: Print " "
        Locate 7: Input " 4. TEAM NICKNAME (IE, PANTHERS) "; mascot$
        Locate 7, 34: Print " "
        Locate 8: Input " 5. DEF RUSH ADJ "; leagueRatings(0)
        Locate 8, 18: Print " "
        Locate 9: Input " 6. DEF COMP % ADJ "; leagueRatings(1)
        Locate 9, 20: Print " "
        Locate 10: Input " 7. DEF INT % ADJ "; leagueRatings(2)
        Locate 10, 19: Print " "
        Locate 11: Input " 8. FUM % (AVG=4) "; leagueRatings(3)
        Locate 11, 19: Print " "
        Locate 12: Input " 9. DEF FUM % (AVG=0) "; leagueRatings(4)
        Locate 12, 23: Print " "
        Locate 13: Input "10. OFF SACKED % (AVG=8) "; leagueRatings(5)
        Locate 13, 26: Print " "
        Locate 14: Input "11. DEF SACK ADJ "; leagueRatings(6)
        Locate 14, 18: Print " "
        Locate 15: Input "12. DEF YD/COMP ADJ "; ydsCompAdj
        Locate 15, 21: Print " "
        Locate 16: Input "13. TM PENALTY ADJ "; teamRatings(0)
        Locate 16, 20: Print " "
        Locate 17: Input "14. OFF RUN/PASS % "; leagueRatings(7)
        Locate 17, 20: Print " "
        Locate 18: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; teamRatings(3)
        Locate 18, 32: Print " "
        Locate 19: Input "16. POWER RATING "; teamRatings(4)
        Locate 19, 18: Print " "
        Locate 20: Input "17. TEXT COLOR (0-15) "; teamRatings(2)
        Locate 20, 23: Print " "
        Locate 21: Input "18. BACKGROUND COLOR (0-7) "; teamRatings(1)
        Locate 21, 28: Print " "

        If teamRatings(1) >= 0 And teamRatings(2) >= 0 Then
            Locate 20, 37
            Print "TEAM APPEARANCE: ";
            Locate 21, 37
            Color teamRatings(2), teamRatings(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
            Color 15, 0
        End If

        Locate 22: Input "19. OFF POWER ADJ"; offPwrAdj
        Locate 22, 18: Print " "
        Locate 23: Input "20. DEF POWER ADJ"; defPwrAdj
        Locate 23, 18: Print " "
        Locate 24: Input "21. STADIUM NAME "; stadium$
        Locate 24, 18: Print " "
        Locate 25: Input "22. AVERAGE ATTENDANCE"; teamAttendance&
        Locate 25, 23: Print " "

        Color 15, 0

        Do

            Do
                Locate 27, 53
                Print Space$(10)
                Locate 27, 25
                Color 11
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Color 15
            Loop Until B9 >= 0 And B9 <= 22

            If B9 <> 0 Then

                Select Case B9

                    Case 1
                        Locate 4, 35: Print Space$(10)
                        Locate 4, 1: Input " 1. TEAM # (BLANK TO AUTO-ASSIGN) "; teamNumber
                        Locate 4, 35: Print " "

                    Case 2
                        Locate 5, 1: Input " 2. DISK ID "; teamYear$
                        Locate 5, 13: Print " "

                    Case 3
                        Locate 6, 1: Input " 3. TEAM NAME (IE. 94 ARKANSAS) "; team$
                        Locate 6, 33: Print " "

                        If teamRatings(1) >= 0 And teamRatings(2) >= 0 Then
                            Locate 20, 37
                            Print "TEAM APPEARANCE: ";
                            Locate 21, 37
                            Color teamRatings(2), teamRatings(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
                            Color 15, 0
                        End If

                    Case 4
                        Locate 7, 1: Input " 4. TEAM NICKNAME (IE. PANTHERS) "; mascot$
                        Locate 7, 34: Print " "

                        If teamRatings(1) >= 0 And teamRatings(2) >= 0 Then
                            Locate 20, 37
                            Print "TEAM APPEARANCE: ";
                            Locate 21, 37
                            Color teamRatings(2), teamRatings(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
                            Color 15, 0
                        End If

                    Case 5
                        Locate 8, 18: Print Space$(10)
                        Locate 8, 1: Input " 5. DEF RUSH ADJ "; leagueRatings(0)
                        Locate 8, 18: Print " "

                    Case 6
                        Locate 9, 20: Print Space$(10)
                        Locate 9, 1: Input " 6. DEF COMP % ADJ "; leagueRatings(1)
                        Locate 9, 20: Print " "

                    Case 7
                        Locate 10, 19: Print Space$(10)
                        Locate 10, 1: Input " 7. DEF INT % ADJ "; leagueRatings(2)
                        Locate 10, 19: Print " "

                    Case 8
                        Locate 11, 19: Print Space$(10)
                        Locate 11, 1: Input " 8. FUM % (AVG=4) "; leagueRatings(3)
                        Locate 11, 19: Print " "

                    Case 9
                        Locate 12, 23: Print Space$(10)
                        Locate 12, 1: Input " 9. DEF FUM % (AVG=0) "; leagueRatings(4)
                        Locate 12, 23: Print " "

                    Case 10
                        Locate 13, 26: Print Space$(10)
                        Locate 13, 1: Input "10. OFF SACKED % (AVG=8) "; leagueRatings(5)
                        Locate 13, 26: Print " "

                    Case 11
                        Locate 14, 18: Print Space$(10)
                        Locate 14, 1: Input "11. DEF SACK ADJ "; leagueRatings(6)
                        Locate 14, 18: Print " "

                    Case 12
                        Locate 15, 21: Print Space$(10)
                        Locate 15, 1: Input "12. DEF YD/COMP ADJ "; ydsCompAdj
                        Locate 15, 21: Print " "

                    Case 13
                        Locate 16, 20: Print Space$(10)
                        Locate 16, 1: Input "13. TM PENALTY ADJ "; teamRatings(0)
                        Locate 16, 20: Print " "

                    Case 14
                        Locate 17, 20: Print Space$(10)
                        Locate 17, 1: Input "14. OFF RUN/PASS % "; leagueRatings(7)
                        Locate 17, 20: Print " "

                    Case 15
                        Locate 18, 32: Print Space$(10)
                        Locate 18, 1: Input "15. STADIUM (0-OUTDOOR 1-DOME) "; teamRatings(3)
                        Locate 18, 32: Print " "

                    Case 16
                        Locate 19, 18: Print Space$(10)
                        Locate 19, 1: Input "16. POWER RATING "; teamRatings(4)
                        Locate 19, 18: Print " "

                    Case 17
                        Locate 20, 23: Print Space$(10)
                        Locate 20, 1: Input "17. TEXT COLOR (0-15) "; teamRatings(2)
                        Locate 20, 23: Print " "

                        If teamRatings(1) >= 0 And teamRatings(2) >= 0 Then
                            Locate 20, 37
                            Print "TEAM APPEARANCE: ";
                            Locate 21, 37
                            Color teamRatings(2), teamRatings(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
                            Color 15, 0
                        End If

                    Case 18
                        Locate 21, 29: Print Space$(10)
                        Locate 21, 1: Input "18. BACKGROUND COLOR (0-7) "; teamRatings(1)
                        Locate 21, 29: Print " "

                        If teamRatings(1) >= 0 And teamRatings(2) >= 0 Then
                            Locate 20, 37
                            Print "TEAM APPEARANCE: ";
                            Locate 21, 37
                            Color teamRatings(2), teamRatings(1): Print RTrim$(team$) + " " + RTrim$(mascot$)
                            Color 15, 0
                        End If

                    Case 19
                        Locate 22, 18: Print Space$(10)
                        Locate 22, 1: Input "19. OFF POWER ADJ"; offPwrAdj
                        Locate 22, 18: Print " "

                    Case 20
                        Locate 23, 18: Print Space$(10)
                        Locate 23, 1: Input "20. DEF POWER ADJ"; defPwrAdj
                        Locate 23, 18: Print " "

                    Case 21
                        Locate 24, 1: Input "21. STADIUM NAME "; stadium$
                        Locate 24, 18: Print " "

                    Case 22
                        Locate 25, 23: Print Space$(10)
                        Locate 25, 1: Input "22. AVERAGE ATTENDANCE"; teamAttendance&
                        Locate 25, 23: Print " "

                End Select

            End If

        Loop Until B9 = 0


        Color 15, 0

        'Running Backs
        Cls

        For I = 0 To 9:

            Cls

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "RUNNING BACKS - "; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; rbName$(I)
            Locate 6, 23: Print " "

            If UCase$(rbName$(I)) <> "END" Then

                Locate 7: Input "2. # OF RUSHES "; rushContrib(I)
                Locate 7, 16: Print " "
                Locate 8: Input "3. RUSHING AVERAGE "; rushAvgRaw!(I)
                Locate 8, 20: Print " "
                Locate 9: Input "4. # OF RECEPTIONS "; rbRecepts(I)
                Locate 9, 20: Print " "
                Locate 10: Input "5. RECEIVING AVERAGE "; rbRecAvg(I)
                Locate 10, 22: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; rbName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 16: Print Space$(10)
                            Locate 7, 1: Input "2. # OF RUSHES "; rushContrib(I)
                            Locate 7, 16: Print " "

                        Case 3
                            Locate 8, 20: Print Space$(10)
                            Locate 8, 1: Input "3. RUSHING AVERAGE "; rushAvgRaw!(I)
                            Locate 8, 20: Print " "

                        Case 4
                            Locate 9, 20: Print Space$(10)
                            Locate 9, 1: Input "4. # OF RECEPTIONS "; rbRecepts(I)
                            Locate 9, 20: Print " "

                        Case 5
                            Locate 10, 22: Print Space$(10)
                            Locate 10, 1: Input "5. RECEIVING AVERAGE "; rbRecAvg(I)
                            Locate 10, 22: Print " "

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

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "WIDE RECEIVERS/TIGHT ENDS - "; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; wrName$(I)
            Locate 6, 23: Print " "

            If UCase$(wrName$(I)) <> "END" Then

                Locate 7: Input "2. # OF RECEPTIONS "; recContrib(I)
                Locate 7, 20: Print " "
                Locate 8: Input "3. RECEIVING AVERAGE "; wrRecAvg(I)
                Locate 8, 22: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 3

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; wrName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 20: Print Space$(10)
                            Locate 7, 1: Input "2. # OF RECEPTIONS "; recContrib(I)
                            Locate 7, 20: Print " "

                        Case 3
                            Locate 8, 22: Print Space$(10)
                            Locate 8, 1: Input "3. RECEIVING AVERAGE "; wrRecAvg(I)
                            Locate 8, 22: Print " "

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

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "QUARTERBACKS - "; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; qbName$(I)
            Locate 6, 23: Print " "

            If UCase$(qbName$(I)) <> "END" Then

                Locate 7: Input "2. PASSES ATTEMPTED "; passContrib(I)
                Locate 7, 21: Print " "
                Locate 8: Input "3. COMPLETION % "; compPct(I)
                Locate 8, 17: Print " "
                Locate 9: Input "4. INTERCEPTION % "; qbIntPct(I)
                Locate 9, 19: Print " "
                Locate 10: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; armRating(I)
                Locate 10, 45: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; qbName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 21: Print Space$(10)
                            Locate 7, 1: Input "2. PASSES ATTEMPTED "; passContrib(I)
                            Locate 7, 21: Print " "

                        Case 3
                            Locate 8, 17: Print Space$(10)
                            Locate 8, 1: Input "3. COMPLETION % "; compPct(I)
                            Locate 8, 17: Print " "

                        Case 4
                            Locate 9, 19: Print Space$(10)
                            Locate 9, 1: Input "4. INTERCEPTION % "; qbIntPct(I)
                            Locate 9, 19: Print " "

                        Case 5
                            Locate 10, 45: Print Space$(10)
                            Locate 10, 1: Input "5. YARDS/COMPLETION ADJUSTMENT (ARM RATING) "; armRating(I)
                            Locate 10, 45: Print " "

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

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "KICKOFF RETURNMEN - "; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; krName$(I)
            Locate 6, 23: Print " "

            If UCase$(krName$(I)) <> "END" Then

                Locate 7: Input "2. KICKOFFS RETURNED "; krContrib(I)
                Locate 7, 22: Print " "
                Locate 8: Input "3. RETURN AVERAGE "; krRetAvg(I)
                Locate 8, 19: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 3

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; krName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 22: Print Space$(10)
                            Locate 7, 1: Input "2. KICKOFFS RETURNED "; krContrib(I)
                            Locate 7, 22: Print " "

                        Case 3
                            Locate 8, 19: Print Space$(10)
                            Locate 8, 1: Input "3. RETURN AVERAGE "; krRetAvg(I)
                            Locate 8, 19: Print " "

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

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "PUNT RETURNMEN - "; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; prName$(I)
            Locate 6, 23: Print " "

            If UCase$(prName$(I)) <> "END" Then

                Locate 7: Input "2. PUNTS RETURNED "; prContrib(I)
                Locate 7, 19: Print " "
                Locate 8: Input "3. PUNT RETURN AVERAGE "; prRetAvg(I)
                Locate 8, 24: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 3

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; prName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 19: Print Space$(10)
                            Locate 7, 1: Input "2. PUNTS RETURNED "; prContrib(I)
                            Locate 7, 19: Print " "

                        Case 3
                            Locate 8, 24: Print Space$(10)
                            Locate 8, 1: Input "3. PUNT RETURN AVERAGE "; prRetAvg(I)
                            Locate 8, 24: Print " "

                    End Select

                Loop Until B9 = 0

                C1 = 0

            Else

                Exit For

            End If

        Next I

        'Punters
        Cls

        Locate 2, 1
        Color 12
        Print "ENTER PLAYERS"

        Color 14
        Locate 4: Print "PUNTER"
        Color 15

        Locate 6, 1: Input "1. NAME "; punterName$(0)
        Locate 6, 9: Print " "

        Locate 7: Input "2. PUNT AVERAGE "; puntAvgRaw!(0)
        Locate 7, 17: Print " "

        C1 = 1

        Do

            Do
                Locate 22, 29
                Print Space$(10)
                Locate 22, 1
                Color 11
                Input "# TO CHANGE (0 TO CONTINUE) "; B9
                Color 15
            Loop Until B9 >= 0 And B9 <= 2

            Select Case B9

                Case 1
                    Locate 6, 1: Input "1. NAME (END TO STOP) "; punterName$(0)
                    Locate 6, 23: Print " "

                Case 2
                    Locate 7, 17: Print Space$(10)
                    Locate 7, 1: Input "2. PUNT AVERAGE "; puntAvgRaw!(0)
                    Locate 7, 17: Print " "

            End Select

        Loop Until B9 = 0

        'Kickers
        Cls

        For I = 0 To 1

            Cls

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "PLACE KICKER -"; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; pkName$(I)
            Locate 6, 23: Print " "

            If UCase$(pkName$(I)) <> "END" Then

                Locate 7: Input "2. FGA "; pkFGContrib(I)
                Locate 7, 8: Print " "
                Locate 8: Input "3. FG% "; pkFGA(I)
                Locate 8, 8: Print " "
                Locate 9: Input "4. PAT ATTEMPTED "; pkPATContrib(I)
                Locate 9, 18: Print " "
                Locate 10: Input "5. PAT% "; pkPAT(I)
                Locate 10, 9: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE) "; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; pkName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 8: Print Space$(10)
                            Locate 7, 1: Input "2. FGA "; pkFGContrib(I)
                            Locate 7, 8: Print " "

                        Case 3
                            Locate 8, 8: Print Space$(10)
                            Locate 8, 1: Input "3. FG% "; pkFGA(I)
                            Locate 8, 8: Print " "

                        Case 4
                            Locate 9, 18: Print Space$(10)
                            Locate 9, 1: Input "4. PAT ATTEMPTED "; pkPATContrib(I)
                            Locate 9, 18: Print " "

                        Case 5
                            Locate 10, 9: Print Space$(10)
                            Locate 10, 1: Input "5. PAT% "; pkPAT(I)
                            Locate 10, 9: Print " "

                    End Select

                Loop Until B9 = 0

            Else

                Exit For

            End If

        Next

        puntAvg(0) = CInt((puntAvgRaw!(0) - 28) / 2)

        For I = 0 To 1

            pkFGA(I) = CInt((pkFGA(I) / 5)) - 5

            If pkFGA(I) < 1 Then
                pkFGA(I) = 1
            End If

        Next

        'Interceptions
        Cls

        For I = 0 To 9:

            Cls

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "PLAYERS WITH INTERCEPTIONS -"; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; intName$(I)
            Locate 6, 23: Print " "

            If UCase$(intName$(I)) <> "END" Then

                Locate 7: Input "2. # OF INTERCEPTIONS"; numInts(I)
                Locate 7, 22: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 2

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; intName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 23: Print Space$(10)
                            Locate 7, 1: Input "2. # OF INTERCEPTIONS "; numInts(I)
                            Locate 7, 23: Print " "

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

            Locate 2, 1
            Color 12
            Print "ENTER PLAYERS"

            Color 14
            Locate 4: Print "PLAYERS WITH SACKS -"; I + 1
            Color 15

            Locate 6, 1: Input "1. NAME (END TO STOP) "; sackName$(I)
            Locate 6, 23: Print " "

            If UCase$(sackName$(I)) <> "END" Then

                Locate 7: Input "2. # OF SACKS"; numSacks(I)
                Locate 7, 14: Print " "

                Do

                    Do
                        Locate 22, 29
                        Print Space$(10)
                        Locate 22, 1
                        Color 11
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                        Color 15
                    Loop Until B9 >= 0 And B9 <= 2

                    Select Case B9

                        Case 1
                            Locate 6, 1: Input "1. NAME (END TO STOP) "; sackName$(I)
                            Locate 6, 23: Print " "

                        Case 2
                            Locate 7, 15: Print Space$(10)
                            Locate 7, 1: Input "2. # OF SACKS "; numSacks(I)
                            Locate 7, 15: Print " "

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
            If rushContrib(I) <> 0 Then
                rushContrib(I) = CInt(SK * rushContrib(I)) + ST
                ST = rushContrib(I)
            End If
            If rushContrib(I) > 100 Then
                rushContrib(I) = 100
            End If
        Next I

        For I = 0 To 9:
            rushAvg(I) = CInt(rushAvgRaw!(I) / .3)
        Next I

        For I = 0 To 9:
            If offPwrAdj <> 0 And rushAvg(I) <> 0 Then
                rushAvg(I) = rushAvg(I) - offPwrAdj
            End If
        Next I

        For I = 0 To 5:
            RM = RM + recContrib(I)
        Next I

        RK = 100 / RM

        For I = 0 To 9:
            If rbRecepts(I) <> 0 Then
                rbRecepts(I) = CInt(RK * rbRecepts(I)) + RT
                RT = rbRecepts(I)
                If rbRecepts(I) > 100 Then
                    rbRecepts(I) = 100
                End If
            End If
        Next I

        For I = 0 To 5:
            If recContrib(I) <> 0 Then
                recContrib(I) = CInt(RK * recContrib(I)) + RT
                RT = recContrib(I)
                If recContrib(I) > 100 Then
                    recContrib(I) = 100
                End If
            End If
        Next I

        For I = 0 To 9:
            rbRecAvg(I) = CInt(rbRecAvg(I) / 3)
        Next I

        For I = 0 To 5:
            wrRecAvg(I) = CInt(wrRecAvg(I) / 3)
        Next I

        PM = 0
        PT = 0

        For I = 0 To 3:
            PM = PM + passContrib(I)
        Next I

        PK = 100 / PM

        For I = 0 To 3:

            If passContrib(I) <> 0 Then
                passContrib(I) = CInt(PK * passContrib(I)) + PT
                PT = passContrib(I)
                If passContrib(I) > 100 Then
                    passContrib(I) = 100
                End If
            End If

        Next I

        KM = 0
        KT = 0
        QM = 0
        QT = 0

        For I = 0 To 3:
            If offPwrAdj <> 0 And compPct(I) <> 0 Then
                compPct(I) = compPct(I) - offPwrAdj
            End If
        Next I

        For I = 0 To 2:
            KM = KM + krContrib(I)
            QM = QM + prContrib(I)
            krRetAvg(I) = CInt(krRetAvg(I) / 3)
            prRetAvg(I) = CInt(prRetAvg(I) / 3)
        Next I

        KK = 100 / KM
        QK = 100 / QM

        For I = 0 To 2:
            If krContrib(I) <> 0 Then
                krContrib(I) = CInt(KK * krContrib(I)) + KT
                KT = krContrib(I)
                If krContrib(I) > 100 Then
                    krContrib(I) = 100
                End If
            End If
        Next I

        For I = 0 To 2:
            If prContrib(I) <> 0 Then
                prContrib(I) = CInt(QK * prContrib(I)) + QT
            End If
            QT = prContrib(I)
            If prContrib(I) > 100 Then
                prContrib(I) = 100
            End If
        Next I

        PM = 0
        PT = 0
        pkFGContrib(0) = CInt(pkFGContrib(0) / (pkFGContrib(0) + pkFGContrib(1)) * 100)
        pkFGContrib(1) = 100
        pkPATContrib(0) = CInt(pkPATContrib(0) / (pkPATContrib(0) + pkPATContrib(1)) * 100)
        pkPATContrib(1) = 100

        For I = 9 To 0 Step -1:
            If rushContrib(I) <> 0 Then
                rushContrib(I) = 100: Exit For
            End If
        Next I

        For I = 5 To 0 Step -1:
            If recContrib(I) <> 0 Then
                recContrib(I) = 100: Exit For
            End If
        Next I

        For I = 3 To 0 Step -1:
            If passContrib(I) <> 0 Then
                passContrib(I) = 100: Exit For
            End If
        Next I

        For I = 2 To 0 Step -1:
            If krContrib(I) <> 0 Then
                krContrib(I) = 100: Exit For
            End If
        Next I

        For I = 2 To 0 Step -1:
            If prContrib(I) <> 0 Then
                prContrib(I) = 100: Exit For
            End If
        Next I

        X = CInt(ydsCompAdj / .3)
        If X < 0 Then X = 100 + X
        ydsCompAdj = X

        If defPwrAdj > 0 Then
            'DEF Rush Adj
            leagueRatings(0) = leagueRatings(0) + defPwrAdj
            'DEF Comp Adj
            leagueRatings(1) = leagueRatings(1) + defPwrAdj
        End If

        Call SaveTeam(team$, mascot$, teamYear$, teamNumber, teamAttendance&)

        Cls

        Call box0(6, 8, 8, 70, 2, 15, 1)

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
Sub SaveTeam (team$, mascot$, teamYear$, teamNumber, teamAttendance&)

    Shared ydsCompAdj

    Shared mascot$, stadium$

    Shared leagueRatings(), teamRatings()

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

        Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = TEAM_SIZE_BYTES
        fileLength& = LOF(1)
        Close #1

        teamNumber = (fileLength& / TEAM_SIZE_BYTES) + 1

    End If

    Open diskPaths$(0) + TEAM_FILE_NAME$ + "." + teamYear$ For Random As #2 Len = TEAM_SIZE_BYTES

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
        LSet Q$(I + 3) = MKI$(leagueRatings(I))
    Next I

    For I = 0 To 4:
        LSet Q$(I + 11) = MKI$(teamRatings(I))
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

    For I = 0 To 1
        LSet Q$(I * 5 + 124) = pkName$(I)
        LSet Q$(I * 5 + 125) = MKI$(pkFGA(I))
        LSet Q$(I * 5 + 126) = MKI$(pkFGContrib(I))
        LSet Q$(I * 5 + 127) = MKI$(pkPAT(I))
        LSet Q$(I * 5 + 128) = MKI$(pkPATContrib(I))
    Next I

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

    Open diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$ For Random As #3 Len = DATA_SIZE_BYTES

    Field #3, 15 As Q$(1), 30 As Q$(2), 4 As Q$(3), 1 As Q$(4)

    LSet Q$(1) = team$
    LSet Q$(2) = stadium$
    LSet Q$(3) = MKL$(teamAttendance&)
    LSet Q$(4) = "x"

    Put #3, teamNumber

    Close #3

    'Reset everything
    Erase leagueRatings

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

