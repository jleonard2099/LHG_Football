'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine loads in stats for a TEAM
'and gives the user an interface for making
'edits to the stats.
Sub EditStats ()

    _Title "3-in-1 Football - Stats Input"

    Shared statsGI%(), statsGS%()

    Shared AF!(), AM!(), teamStats!(), playerStats!()
    Shared statsLC!(), statsLF!(), statsLI!(), statsLK!(), statsLP!(), statsLR!(), statsPK!()
    Shared statsZ!(), statsZ1!(), statsZ2!

    Dim sackStats!(1, 1)

    Shared intName$(), sackName$()
    Shared lookyRBacks$(), lookyWdRec$(), lookyQBacks$(), lookyKickRet$()
    Shared lookyPRet$(), lookyPunter$(), lookyKicker$()

    Shared statsLC$(), statsLF$(), statsLI$(), statsLK$(), statsLP$(), statsLR$(), statsPK$()

    teamNbr = 0
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

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, teamName$

        For I = 0 To 9:
            Input #1, lookyRBacks$(I)
        Next I

        For I = 0 To 5:
            Input #1, lookyWdRec$(I)
        Next I

        For I = 0 To 3:
            Input #1, lookyQBacks$(I)
        Next I

        For I = 0 To 2:
            Input #1, lookyKickRet$(I)
        Next I

        For I = 0 To 2
            Input #1, lookyPRet$(I)
        Next I

        Input #1, lookyPunter$(0)

        For I = 0 To 1:
            Input #1, lookyKicker$(I)
        Next I

        For J = 0 To 13

            For K = 0 To 17:
                Input #1, statsZ2!(J, K)
            Next K

        Next J

        For L = 0 To 38:
            Input #1, statsZ!(L)
        Next L

        For L = 0 To 38:
            Input #1, statsZ1!(L)
        Next L

        For J = 0 To 9:
            Input #1, intName$(J)
            For J1 = 0 To 2
                Input #1, statsGI%(J, J1)
            Next J1
        Next

        For J = 0 To 14:
            Input #1, sackName$(J)
            For J1 = 0 To 1
                Input #1, statsGS%(J, J1)
            Next J1
        Next

        For I = 0 To 9:
            Input #1, statsLR!(I)
            Input #1, statsLR$(I, 0)
            Input #1, statsLR$(I, 1)
        Next

        For I = 0 To 15:
            Input #1, statsLC!(I)
            Input #1, statsLC$(I, 0)
            Input #1, statsLC$(I, 1)
        Next

        For I = 0 To 3:
            Input #1, statsLP!(I)
            Input #1, statsLP$(I, 0)
            Input #1, statsLP$(I, 1)
        Next

        For I = 0 To 2:
            Input #1, statsLK!(I)
            Input #1, statsLK$(I, 0)
            Input #1, statsLK$(I, 1)
        Next

        For I = 0 To 2:
            Input #1, statsPK!(I)
            Input #1, statsPK$(I, 0)
            Input #1, statsPK$(I, 1)
        Next

        For I = 0 To 1:
            Input #1, statsLF!(I)
            Input #1, statsLF$(I)
            For I1 = 0 To 4:
                Input #1, AM!(I, I1)
                Input #1, AF!(I, I1)
            Next
        Next

        For I = 0 To 9:
            Input #1, statsLI!(I)
            Input #1, statsLI$(I, 0)
            Input #1, statsLI$(I, 1)
        Next

        Close #1

        For J = 0 To 13
            For K = 0 To 17:
                playerStats!(J, K) = statsZ2!(J, K)
            Next K
        Next J

        For L = 0 To 12:
            teamStats!(0, L) = statsZ!(L)
        Next L

        For L = 0 To 12:
            teamStats!(1, L) = statsZ1!(L)
        Next L

        sackStats!(0, 0) = statsZ!(13)
        sackStats!(0, 1) = statsZ!(14)
        sackStats!(1, 0) = statsZ1!(13)
        sackStats!(1, 1) = statsZ1!(14)

        '---------------
        ' Team Offense
        '---------------
        Color 15, 0
        Cls

        X$ = "TEAM OFFENSIVE STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 4, 1

        Print "1. STAT FILE NAME  "; statFile$
        Print "2. TM 1ST DOWNS "; teamStats!(0, 0)
        Print "3. TM RUSH ATT "; teamStats!(0, 1)
        Print "4. TM RUSH YDS "; Int(teamStats!(0, 2))
        Print "5. TM PASS ATT "; teamStats!(0, 3)
        Print "6. TM PASS COM "; teamStats!(0, 4)
        Print "7. TM PASS INT "; teamStats!(0, 5)
        Print "8. TM PASS YDS "; Int(teamStats!(0, 6))
        Print "9. TM PUNTS "; teamStats!(0, 7)
        Print "10. TM TOTAL PUNT YDS "; Int(teamStats!(0, 8))
        Print "11. TM FUMBLES "; teamStats!(0, 9)
        Print "12. TM FUM LOST "; teamStats!(0, 10)
        Print "13. TM PENALTIES "; teamStats!(0, 11)
        Print "14. TM PENALTY YDS "; Int(teamStats!(0, 12))
        Print "15. TM TIMES SACKED "; sackStats!(0, 0)
        Print "16. TM YARDS LOST FROM SACKS "; Int(sackStats!(0, 1))

        Do

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 16

            Select Case B9

                Case 1
                    Locate 4, 1: Input " 1. STAT FILE NAME "; statFile$
                    Locate 4, 20: Print " "

                Case 2
                    Locate 5, 18: Print Space$(10)
                    Locate 5, 1: Input " 2. TM 1ST DOWNS "; teamStats!(0, 0)
                    Locate 5, 18: Print " "

                Case 3
                    Locate 6, 17: Print Space$(10)
                    Locate 6, 1: Input " 3. TM RUSH ATT "; teamStats!(0, 1)
                    Locate 6, 17: Print " "

                Case 4
                    Locate 7, 17: Print Space$(10)
                    Locate 7, 1: Input " 4. TM RUSH YDS "; teamStats!(0, 2)
                    Locate 7, 17: Print " "

                Case 5
                    Locate 8, 17: Print Space$(10)
                    Locate 8, 1: Input " 5. TM PASS ATT "; teamStats!(0, 3)
                    Locate 8, 17: Print " "

                Case 6
                    Locate 9, 17: Print Space$(10)
                    Locate 9, 1: Input " 6. TM PASS COM "; teamStats!(0, 4)
                    Locate 9, 17: Print " "

                Case 7
                    Locate 10, 17: Print Space$(10)
                    Locate 10, 1: Input " 7. TM PASS INT "; teamStats!(0, 5)
                    Locate 10, 17: Print " "

                Case 8
                    Locate 11, 17: Print Space$(10)
                    Locate 11, 1: Input " 8. TM PASS YDS "; teamStats!(0, 6)
                    Locate 11, 17: Print " "

                Case 9
                    Locate 12, 14: Print Space$(10)
                    Locate 12, 1: Input " 9. TM PUNTS "; teamStats!(0, 7)
                    Locate 12, 14: Print " "

                Case 10
                    Locate 13, 23: Print Space$(10)
                    Locate 13, 1: Input "10. TM TOTAL PUNT YDS "; teamStats!(0, 8)
                    Locate 13, 23: Print " "

                Case 11
                    Locate 14, 16: Print Space$(10)
                    Locate 14, 1: Input "11. TM FUMBLES "; teamStats!(0, 9)
                    Locate 14, 16: Print " "

                Case 12
                    Locate 15, 17: Print Space$(10)
                    Locate 15, 1: Input "12. TM FUM LOST "; teamStats!(0, 10)
                    Locate 15, 17: Print " "

                Case 13
                    Locate 16, 18: Print Space$(10)
                    Locate 16, 1: Input "13. TM PENALTIES "; teamStats!(0, 11)
                    Locate 16, 18: Print " "

                Case 14
                    Locate 17, 20: Print Space$(10)
                    Locate 17, 1: Input "14. TM PENALTY YDS "; teamStats!(0, 12)
                    Locate 17, 20: Print " "

                Case 15
                    Locate 18, 21: Print Space$(10)
                    Locate 18, 1: Input "15. TM TIMES SACKED "; sackStats!(0, 0)
                    Locate 18, 21: Print " "

                Case 16
                    Locate 19, 30: Print Space$(10)
                    Locate 19, 1: Input "16. TM YARDS LOST FROM SACKS "; sackStats!(0, 1)
                    Locate 19, 30: Print " "

            End Select

            Locate 20, 29
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Team Defense
        '---------------
        Cls

        X$ = "TEAM DEFENSIVE STATS"
        Call TITLE_STAT_HEADER(X$)

        Locate 4, 1
        Print "1. TM 1ST DOWNS "; teamStats!(1, 0)
        Print "2. TM RUSH ATT "; teamStats!(1, 1)
        Print "3. TM RUSH YDS "; Int(teamStats!(1, 2))
        Print "4. TM PASS ATT "; teamStats!(1, 3)
        Print "5. TM PASS COM "; teamStats!(1, 4)
        Print "6. TM PASS INT "; teamStats!(1, 5)
        Print "7. TM PASS YDS "; Int(teamStats!(1, 6))
        Print "8. TM PUNTS "; teamStats!(1, 7)
        Print "9. TM TOTAL PUNT YDS "; Int(teamStats!(1, 8))
        Print "10. TM FUMBLES "; teamStats!(1, 9)
        Print "11. TM FUM LOST "; teamStats!(1, 10)
        Print "12. TM PENALTIES "; teamStats!(1, 11)
        Print "13. TM PENALTY YDS "; Int(teamStats!(1, 12))
        Print "14. TM TIMES SACKED "; sackStats!(1, 0)
        Print "15. TM YARDS LOST FROM SACKS "; Int(sackStats!(1, 1))

        Do

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 15

            Select Case B9

                Case 1
                    Locate 4, 18: Print Space$(10)
                    Locate 4, 1: Input " 1. TM 1ST DOWNS "; teamStats!(1, 0)
                    Locate 4, 18: Print " "

                Case 2
                    Locate 5, 17: Print Space$(10)
                    Locate 5, 1: Input " 2. TM RUSH ATT "; teamStats!(1, 1)
                    Locate 5, 17: Print " "

                Case 3
                    Locate 6, 17: Print Space$(10)
                    Locate 6, 1: Input " 3. TM RUSH YDS "; teamStats!(1, 2)
                    Locate 6, 17: Print " "

                Case 4
                    Locate 7, 17: Print Space$(10)
                    Locate 7, 1: Input " 4. TM PASS ATT "; teamStats!(1, 3)
                    Locate 7, 17: Print " "

                Case 5
                    Locate 8, 17: Print Space$(10)
                    Locate 8, 1: Input " 5. TM PASS COM "; teamStats!(1, 4)
                    Locate 8, 17: Print " "

                Case 6
                    Locate 9, 17: Print Space$(10)
                    Locate 9, 1: Input " 6. TM PASS INT "; teamStats!(1, 5)
                    Locate 9, 17: Print " "

                Case 7
                    Locate 10, 17: Print Space$(10)
                    Locate 10, 1: Input " 7. TM PASS YDS "; teamStats!(1, 6)
                    Locate 10, 17: Print " "

                Case 8
                    Locate 11, 14: Print Space$(10)
                    Locate 11, 1: Input " 8. TM PUNTS "; teamStats!(1, 7)
                    Locate 11, 14: Print " "

                Case 9
                    Locate 12, 23: Print Space$(10)
                    Locate 12, 1: Input " 9. TM TOTAL PUNT YDS "; teamStats!(1, 8)
                    Locate 12, 23: Print " "

                Case 10
                    Locate 13, 16: Print Space$(10)
                    Locate 13, 1: Input "10. TM FUMBLES "; teamStats!(1, 9)
                    Locate 13, 16: Print " "

                Case 11
                    Locate 14, 17: Print Space$(10)
                    Locate 14, 1: Input "11. TM FUM LOST "; teamStats!(1, 10)
                    Locate 14, 17: Print " "

                Case 12
                    Locate 15, 18: Print Space$(10)
                    Locate 15, 1: Input "12. TM PENALTIES "; teamStats!(1, 11)
                    Locate 15, 18: Print " "

                Case 13
                    Locate 16, 20: Print Space$(10)
                    Locate 16, 1: Input "13. TM PENALTY YDS "; teamStats!(1, 12)
                    Locate 16, 20: Print " "

                Case 14
                    Locate 17, 21: Print Space$(10)
                    Locate 17, 1: Input "14. TM TIMES SACKED "; sackStats!(1, 0)
                    Locate 17, 21: Print " "

                Case 15
                    Locate 18, 30: Print Space$(10)
                    Locate 18, 1: Input "15. TM YARDS LOST FROM SACKS "; sackStats!(1, 1)
                    Locate 18, 30: Print " "

            End Select

            Locate 20, 29
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Running Backs
        '---------------
        Do

            Cls
            X$ = "CHANGE RUSHERS"
            Call Title_EDITSTATS(2, X$)
            Print

            For I1 = 0 To 9:
                Print Using "##"; I1 + 1;
                Print " "; lookyRBacks$(I1)
            Next

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 10

            If I > 0 Then

                I = I - 1

                Cls
                Call Title_EDITSTATS(2, X$)

                Locate 4, 1
                Print "1. NAME  "; lookyRBacks$(I)
                Print "2. RUSHING ATTEMPTS "; playerStats!(I, 0)
                Print "3. RUSHING YARDAGE "; Int(playerStats!(I, 1))
                Print "4. RUSHING TD'S "; playerStats!(I, 2)
                Print "5. RECEPTIONS "; playerStats!(I, 3)
                Print "6. RECEIVING YARDAGE "; Int(playerStats!(I, 4))
                Print "7. RECEIVING TD'S "; playerStats!(I, 5)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 7

                    Select Case B9

                        Case 1
                            Locate 4, 1: Input "1. NAME "; lookyRBacks$(I1)
                            Locate 4, 9: Print " "

                        Case 2
                            Locate 5, 21: Print Space$(10)
                            Locate 5, 1: Input "2. RUSHING ATTEMPTS "; playerStats!(I, 0)
                            Locate 5, 21: Print " "

                        Case 3
                            Locate 6, 20: Print Space$(10)
                            Locate 6, 1: Input "3. RUSHING YARDAGE "; playerStats!(I, 1)
                            Locate 6, 20: Print " "

                        Case 4
                            Locate 7, 17: Print Space$(10)
                            Locate 7, 1: Input "4. RUSHING TD'S "; playerStats!(I, 2)
                            Locate 7, 17: Print " "

                        Case 5
                            Locate 8, 15: Print Space$(10)
                            Locate 8, 1: Input "5. RECEPTIONS "; playerStats!(I, 3)
                            Locate 8, 15: Print " "

                        Case 6
                            Locate 9, 22: Print Space$(10)
                            Locate 9, 1: Input "6. RECEIVING YARDAGE "; playerStats!(I, 4)
                            Locate 9, 22: Print " "

                        Case 7
                            Locate 10, 19: Print Space$(10)
                            Locate 10, 1: Input "7. RECEIVING TD'S "; playerStats!(I, 5)
                            Locate 10, 19: Print " "

                    End Select

                    Locate 20, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Wide Receivers
        '---------------
        Do

            Cls
            X$ = "CHANGE RECEIVERS"
            Call Title_EDITSTATS(2, X$)
            Print

            For I1 = 0 To 5:
                Print Using "##"; I1 + 1;
                Print " "; lookyWdRec$(I1)
            Next

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 6

            If I > 0 Then

                I = I - 1

                Cls
                Call Title_EDITSTATS(2, X$)

                Locate 4, 1
                Print "1. NAME  "; lookyWdRec$(I)
                Print "2. RECEPTIONS "; playerStats!(I, 6)
                Print "3. RECEIVING YARDAGE "; Int(playerStats!(I, 7))
                Print "4. RECEIVING TD'S "; playerStats!(I, 8)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    Select Case B9

                        Case 1
                            Locate 4, 1: Input "1. NAME "; lookyWdRec$(I)
                            Locate 4, 9: Print " "

                        Case 2
                            Locate 5, 15: Print Space$(10)
                            Locate 5, 1: Input "2. RECEPTIONS "; playerStats!(I, 6)
                            Locate 5, 15: Print " "

                        Case 3
                            Locate 6, 22: Print Space$(10)
                            Locate 6, 1: Input "3. RECEIVING YARDAGE "; playerStats!(I, 7)
                            Locate 6, 22: Print " "

                        Case 4
                            Locate 7, 19: Print Space$(10)
                            Locate 7, 1: Input "4. RECEIVING TD'S "; playerStats!(I, 8)
                            Locate 7, 19: Print " "

                    End Select

                    Locate 20, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Quarterbacks
        '---------------
        Do

            Cls
            X$ = "CHANGE QUARTERBACKS"
            Call Title_EDITSTATS(2, X$)
            Print

            For I1 = 0 To 3:
                Print Using "##"; I1 + 1;
                Print " "; lookyQBacks$(I1)
            Next

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 4

            If I > 0 Then

                I = I - 1

                Cls
                Call Title_EDITSTATS(2, X$)

                Locate 4, 1
                Print "1. NAME  "; lookyQBacks$(I)
                Print "2. PASSES THROWN "; playerStats!(I, 9)
                Print "3. PASSES COMPLETED "; playerStats!(I, 10)
                Print "4. PASSES INTERCEPTED "; playerStats!(I, 11)
                Print "5. TOTAL PASSING YARDS "; Int(playerStats!(I, 12))
                Print "6. TOUCHDOWN PASSES "; playerStats!(I, 13)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 6

                    Select Case B9

                        Case 1
                            Locate 4, 1: Input "1. NAME "; lookyQBacks$(I)
                            Locate 4, 9: Print " "

                        Case 2
                            Locate 5, 18: Print Space$(10)
                            Locate 5, 1: Input "2. PASSES THROWN "; playerStats!(I, 9)
                            Locate 5, 18: Print " "

                        Case 3
                            Locate 6, 21: Print Space$(10)
                            Locate 6, 1: Input "3. PASSES COMPLETED "; playerStats!(I, 10)
                            Locate 6, 21: Print " "

                        Case 4
                            Locate 7, 23: Print Space$(10)
                            Locate 7, 1: Input "4. PASSES INTERCEPTED "; playerStats!(I, 11)
                            Locate 7, 23: Print " "

                        Case 5
                            Locate 8, 24: Print Space$(10)
                            Locate 8, 1: Input "5. TOTAL PASSING YARDS "; playerStats!(I, 12)
                            Locate 8, 24: Print " "

                        Case 6
                            Locate 9, 21: Print Space$(10)
                            Locate 9, 1: Input "6. TOUCHDOWN PASSES "; playerStats!(I, 13)
                            Locate 9, 21: Print " "

                    End Select

                    Locate 20, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Kickoff Returns
        '---------------
        Do

            Cls
            X$ = "CHANGE KICKOFF RETURNS"
            Call Title_EDITSTATS(2, X$)
            Print

            For I1 = 0 To 2:
                Print Using "##"; I1 + 1;
                Print " "; lookyKickRet$(I1)
            Next

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 3

            If I > 0 Then

                I = I - 1

                Cls
                Call Title_EDITSTATS(2, X$)

                Locate 4, 1
                Print "1. NAME  "; lookyKickRet$(I)
                Print "2. KICKS RETURNED "; playerStats!(I, 14)
                Print "3. KICK RETURN YARDAGE "; Int(playerStats!(I, 15))
                Print "4. KICK RETURN TD'S "; playerStats!(I, 16)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    Select Case B9

                        Case 1
                            Locate 4, 1: Input "1. NAME "; lookyKickRet$(I)
                            Locate 4, 9: Print " "

                        Case 2
                            Locate 5, 19: Print Space$(10)
                            Locate 5, 1: Input "2. KICKS RETURNED"; playerStats!(I, 14)
                            Locate 5, 19: Print " "

                        Case 3
                            Locate 6, 24: Print Space$(10)
                            Locate 6, 1: Input "3. KICK RETURN YARDAGE"; playerStats!(I, 15)
                            Locate 6, 24: Print " "

                        Case 4
                            Locate 7, 21: Print Space$(10)
                            Locate 7, 1: Input "4. KICK RETURN TD'S "; playerStats!(I, 16)
                            Locate 7, 21: Print " "

                    End Select

                    Locate 20, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Punt Returns
        '---------------
        Do

            Cls
            X$ = "CHANGE PUNT RETURNS"
            Call Title_EDITSTATS(2, X$)
            Print

            For I1 = 3 To 5:
                Print Using "##"; I1 - 2;
                Print " "; lookyPRet$(I1 - 3)
            Next

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 3

            If I > 0 Then

                I = I + 2

                Cls
                Call Title_EDITSTATS(2, X$)

                Locate 4, 1
                Print "1. NAME  "; lookyPRet$(I - 3)
                Print "2. PUNTS RETURNED "; playerStats!(I, 14)
                Print "3. PUNT RETURN YARDAGE "; Int(playerStats!(I, 15))
                Print "4. PUNT RETURN TD'S "; playerStats!(I, 16)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 4

                    Select Case B9

                        Case 1
                            Locate 4, 1: Input "1. NAME "; lookyPRet$(I - 2)
                            Locate 4, 9: Print " "

                        Case 2
                            Locate 5, 19: Print Space$(10)
                            Locate 5, 1: Input "2. PUNTS RETURNED "; playerStats!(I, 14)
                            Locate 5, 19: Print " "

                        Case 3
                            Locate 6, 24: Print Space$(10)
                            Locate 6, 1: Input "3. PUNT RETURN YARDAGE "; playerStats!(I, 15)
                            Locate 6, 24: Print " "

                        Case 4
                            Locate 7, 21: Print Space$(10)
                            Locate 7, 1: Input "4. PUNT RETURN TD'S "; playerStats!(I, 16)
                            Locate 7, 21: Print " "

                    End Select

                    Locate 20, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '---------------
        ' Punters
        '---------------
        Cls
        X$ = "CHANGE PUNTER"
        Call Title_EDITSTATS(2, X$)

        Locate 4, 1
        Print "1. NAME  "; lookyPunter$(0)
        Print "2. PUNTS "; teamStats!(0, 7)
        Print "3. TOTAL PUNT YARDAGE "; Int(teamStats!(0, 8))

        Do

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; B9
            Loop Until B9 >= 0 And B9 <= 3

            Select Case B9

                Case 1
                    Locate 4, 1: Input "1. NAME "; lookyPunter$(0)
                    Locate 4, 9: Print " "

                Case 2
                    Locate 5, 10: Print Space$(10)
                    Locate 5, 1: Input "2. PUNTS "; teamStats!(0, 7)
                    Locate 5, 10: Print " "

                Case 3
                    Locate 6, 23: Print Space$(10)
                    Locate 6, 1: Input "3. TOTAL PUNT YARDAGE "; teamStats!(0, 8)
                    Locate 6, 23: Print " "

            End Select

            Locate 20, 29
            Print Space$(10)

        Loop Until B9 = 0


        '---------------
        ' Kickers
        '---------------
        Do

            Cls
            X$ = "CHANGE PLACE KICKERS"
            Call Title_EDITSTATS(2, X$)
            Print

            For I1 = 0 To 1:
                Print Using "##"; I1 + 1;
                Print " "; lookyKicker$(I1)
            Next

            Do
                Locate 20, 1
                Input "# TO CHANGE (0 TO CONTINUE)"; I
            Loop Until I >= 0 And I <= 2

            If I > 0 Then

                I = I - 1

                Cls
                Call Title_EDITSTATS(2, X$)

                Locate 4, 1
                Print "1. NAME  "; lookyKicker$(I)
                Print "2. FIELD GOALS ATT "; playerStats!(4 + I * 4, 17)
                Print "3. FIELD GOALS MADE "; playerStats!(3 + I * 4, 17)
                Print "4. EXTRA POINTS ATT "; playerStats!(6 + I * 4, 17)
                Print "5. EXTRA POINTS MADE "; playerStats!(5 + I * 4, 17)

                Do

                    Do
                        Locate 20, 1
                        Input "# TO CHANGE (0 TO CONTINUE)"; B9
                    Loop Until B9 >= 0 And B9 <= 5

                    Select Case B9

                        Case 1
                            Locate 4, 1: Input "1. NAME "; lookyKicker$(I)
                            Locate 4, 9: Print " "

                        Case 2
                            Locate 5, 20: Print Space$(10)
                            Locate 5, 1: Input "2. FIELD GOALS ATT "; playerStats!(4 + I * 4, 17)
                            Locate 5, 20: Print " "

                        Case 3
                            Locate 6, 21: Print Space$(10)
                            Locate 6, 1: Input "3. FIELD GOALS MADE "; playerStats!(3 + I * 4, 17)
                            Locate 6, 21: Print " "

                        Case 4
                            Locate 7, 21: Print Space$(10)
                            Locate 7, 1: Input "4. EXTRA POINTS ATT "; playerStats!(6 + I * 4, 17)
                            Locate 7, 21: Print " "

                        Case 5
                            Locate 7, 22: Print Space$(10)
                            Locate 8, 1: Input "5. EXTRA POINTS MADE "; playerStats!(5 + I * 4, 17)
                            Locate 7, 22: Print " "

                    End Select

                    Locate 20, 29
                    Print Space$(10)

                Loop Until B9 = 0

            End If

        Loop Until I = 0


        '-------------------------
        ' Update Calculations
        '-------------------------
        For J = 0 To 13
            For K = 0 To 17
                statsZ2!(J, K) = playerStats!(J, K)
            Next K
        Next J

        For L = 0 To 12:
            statsZ!(L) = teamStats!(0, L)
        Next L

        For L = 0 To 12:
            statsZ1!(L) = teamStats!(1, L)
        Next L

        statsZ!(13) = sackStats!(0, 0)
        statsZ!(14) = sackStats!(0, 1)
        statsZ1!(13) = sackStats!(1, 0)
        statsZ1!(14) = sackStats!(1, 1)
        

        '---------------
        ' Save Edits
        '---------------
        Open diskPaths$(1) + statFile$ For Output As #1

        Print #1, teamName$

        For I = 0 To 9:
            Print #1, lookyRBacks$(I)
        Next I

        For I = 0 To 5:
            Print #1, lookyWdRec$(I)
        Next I

        For I = 0 To 3:
            Print #1, lookyQBacks$(I)
        Next I

        For I = 0 To 2:
            Print #1, lookyKickRet$(I)
        Next I

        For I = 0 To 2:
            Print #1, lookyPRet$(I)
        Next I

        Print #1, lookyPunter$(0)

        For I = 0 To 1:
            Print #1, lookyKicker$(I)
        Next I

        For J = 0 To 13
            For K = 0 To 17:
                Print #1, statsZ2!(J, K)
            Next K
        Next J

        For L = 0 To 38:
            Print #1, statsZ!(L)
        Next L

        For L = 0 To 38:
            Print #1, statsZ1!(L)
        Next L

        For J = 0 To 9:
            Print #1, intName$(J)
            For J1 = 0 To 2
                Print #1, statsGI%(J, J1)
            Next J1
        Next

        For J = 0 To 14:
            Print #1, sackName$(J)
            For J1 = 0 To 1
                Print #1, statsGS%(J, J1)
            Next J1
        Next

        For I = 0 To 9:
            Print #1, statsLR!(I)
            Print #1, statsLR$(I, 0)
            Print #1, statsLR$(I, 1)
        Next

        For I = 0 To 15:
            Print #1, statsLC!(I)
            Print #1, statsLC$(I, 0)
            Print #1, statsLC$(I, 1)
        Next

        For I = 0 To 3:
            Print #1, statsLP!(I)
            Print #1, statsLP$(I, 0)
            Print #1, statsLP$(I, 1)
        Next

        For I = 0 To 2:
            Print #1, statsLK!(I)
            Print #1, statsLK$(I, 0)
            Print #1, statsLK$(I, 1)
        Next

        For I = 0 To 2:
            Print #1, statsPK!(I)
            Print #1, statsPK$(I, 0)
            Print #1, statsPK$(I, 1)
        Next

        For I = 0 To 1:
            Print #1, statsLF!(I)
            Print #1, statsLF$(I)
            For I1 = 0 To 4:
                Print #1, AM!(I, I1)
                Print #1, AF!(I, I1)
            Next
        Next

        For I = 0 To 9:
            Print #1, statsLI!(I)
            Print #1, statsLI$(I, 0)
            Print #1, statsLI$(I, 1)
        Next

        Close #1

    End If

End Sub

