'----------------------------------------
'          EditStats Subroutine
'----------------------------------------
'This subroutine presents a menu of options
'for the user to edit replay stats.
Sub EditStats ()

    _Title "3-in-1 Football - Stats Input"

    ReDim menuChoices$(1 To 3)

    Do

        Cls
        Color 15, 0

        Call TITLE

        menuChoices$(1) = "EDIT / CHANGE REPLAY STATS    "
        menuChoices$(2) = "EDIT / CHANGE SCHEDULE RESULTS"
        menuChoices$(3) = "RETURN TO MAIN MENU           "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
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

                Case 1, 2:
                    Call EditStatsSchedules(SELECTION%)

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'     EditStatsSchedules Subroutine
'----------------------------------------
'This subroutine lets the user pick a team
'and then calls the corresponding routine
'for the user to edit Stats or Schedule
'Results.
Sub EditStatsSchedules (action%)

    teamNbr = 0
    Call ChooseTeam(YN$, NTMS, teamNbr, team$)
    statFile$ = nameStatFile$(team$)

    If action% = 2 Then

        Call ChangeScheduleStats(statFile$)

    Else
        'action% = 1
        If Not _FileExists(diskPaths$(1) + statFile$) Then

            MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
            BoxWidth = 24
            BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
            Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
            Call CSMsgBox(MsgText$, BoxWidth, Pass)
            I$ = GetKeyPress$
            Call CSMsgBox("", 0, Pass)

        Else

            Call ModifyTeamStats(statFile$)

        End If

    End If

End Sub


'----------------------------------------
'       ModifyTeamStats Subroutine
'----------------------------------------
'This subroutine inputs stats from the
'chosen stat file and then gives the user
'an interface for modifying the stats.
Sub ModifyTeamStats (statFile$)

    Shared allPlayerStats!(), allTeamStats!()

    Shared statsFGA!(), statsFGM!()
    Shared statsLRec!(), statsLFG!(), statsLInt!(), statsLKR!(), statsLPass!(), statsLRush!(), statsLPR!()
    Shared statsTeam!(), statsOpp!(), statsPlayers!()

    Shared statsGI(), statsGS()

    Dim sackStats!(1, 1)

    Shared intName$(), sackName$()
    Shared rbName$(), wrName$(), qbName$(), krName$()
    Shared prName$(), punterName$(), pkName$()

    Shared statsLRec$(), statsLFG$(), statsLInt$(), statsLKR$(), statsLPass$(), statsLRush$(), statsLPR$()

    Call ReadTeamStats(teamName$, statFile$)

    '---------------
    ' Calcuations
    '---------------
    '?? is this necessary?
    For J = 0 To 13
        For K = 0 To 17:
            allPlayerStats!(J, K) = statsPlayers!(J, K)
        Next K
    Next J

    For L = 0 To 12:
        allTeamStats!(0, L) = statsTeam!(L)
    Next L

    For L = 0 To 12:
        allTeamStats!(1, L) = statsOpp!(L)
    Next L

    sackStats!(0, 0) = statsTeam!(13)
    sackStats!(0, 1) = statsTeam!(14)
    sackStats!(1, 0) = statsOpp!(13)
    sackStats!(1, 1) = statsOpp!(14)

    '---------------
    ' Team Offense
    '---------------
    Color 15, 0
    Cls

    X$ = "TEAM OFFENSIVE STATS"
    Call TITLE_STAT_HEADER(X$)

    Locate 3, 1

    Print " 1. STAT FILE NAME  "; statFile$
    Print " 2. TM 1ST DOWNS "; allTeamStats!(0, 0)
    Print " 3. TM RUSH ATT "; allTeamStats!(0, 1)
    Print " 4. TM RUSH YDS "; Int(allTeamStats!(0, 2))
    Print " 5. TM PASS ATT "; allTeamStats!(0, 3)
    Print " 6. TM PASS COM "; allTeamStats!(0, 4)
    Print " 7. TM PASS INT "; allTeamStats!(0, 5)
    Print " 8. TM PASS YDS "; Int(allTeamStats!(0, 6))
    Print " 9. TM PUNTS "; allTeamStats!(0, 7)
    Print "10. TM TOTAL PUNT YDS "; Int(allTeamStats!(0, 8))
    Print "11. TM FUMBLES "; allTeamStats!(0, 9)
    Print "12. TM FUM LOST "; allTeamStats!(0, 10)
    Print "13. TM PENALTIES "; allTeamStats!(0, 11)
    Print "14. TM PENALTY YDS "; Int(allTeamStats!(0, 12))
    Print "15. TM TIMES SACKED "; sackStats!(0, 0)
    Print "16. TM YARDS LOST FROM SACKS "; Int(sackStats!(0, 1))

    Do

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 16

        Select Case B9

            Case 1
                Locate 3, 1: Input " 1. STAT FILE NAME "; statFile$
                Locate 3, 20: Print " "

            Case 2
                Locate 4, 17: Print Space$(10)
                Locate 4, 1: Input " 2. TM 1ST DOWNS"; allTeamStats!(0, 0)
                Locate 4, 17: Print " "

            Case 3
                Locate 5, 16: Print Space$(10)
                Locate 5, 1: Input " 3. TM RUSH ATT"; allTeamStats!(0, 1)
                Locate 5, 16: Print " "

            Case 4
                Locate 6, 16: Print Space$(10)
                Locate 6, 1: Input " 4. TM RUSH YDS"; allTeamStats!(0, 2)
                Locate 6, 16: Print " "

            Case 5
                Locate 7, 16: Print Space$(10)
                Locate 7, 1: Input " 5. TM PASS ATT"; allTeamStats!(0, 3)
                Locate 7, 16: Print " "

            Case 6
                Locate 8, 16: Print Space$(10)
                Locate 8, 1: Input " 6. TM PASS COM"; allTeamStats!(0, 4)
                Locate 8, 16: Print " "

            Case 7
                Locate 9, 16: Print Space$(10)
                Locate 9, 1: Input " 7. TM PASS INT"; allTeamStats!(0, 5)
                Locate 9, 16: Print " "

            Case 8
                Locate 10, 16: Print Space$(10)
                Locate 10, 1: Input " 8. TM PASS YDS"; allTeamStats!(0, 6)
                Locate 10, 16: Print " "

            Case 9
                Locate 11, 13: Print Space$(10)
                Locate 11, 1: Input " 9. TM PUNTS"; allTeamStats!(0, 7)
                Locate 11, 13: Print " "

            Case 10
                Locate 12, 22: Print Space$(10)
                Locate 12, 1: Input "10. TM TOTAL PUNT YDS"; allTeamStats!(0, 8)
                Locate 12, 22: Print " "

            Case 11
                Locate 13, 15: Print Space$(10)
                Locate 13, 1: Input "11. TM FUMBLES"; allTeamStats!(0, 9)
                Locate 13, 15: Print " "

            Case 12
                Locate 14, 16: Print Space$(10)
                Locate 14, 1: Input "12. TM FUM LOST"; allTeamStats!(0, 10)
                Locate 14, 16: Print " "

            Case 13
                Locate 15, 17: Print Space$(10)
                Locate 15, 1: Input "13. TM PENALTIES "; allTeamStats!(0, 11)
                Locate 15, 17: Print " "
    
            Case 14
                Locate 16, 19: Print Space$(10)
                Locate 16, 1: Input "14. TM PENALTY YDS"; allTeamStats!(0, 12)
                Locate 16, 19: Print " "

            Case 15
                Locate 17, 20: Print Space$(10)
                Locate 17, 1: Input "15. TM TIMES SACKED"; sackStats!(0, 0)
                Locate 17, 20: Print " "

            Case 16
                Locate 18, 29: Print Space$(10)
                Locate 18, 1: Input "16. TM YARDS LOST FROM SACKS"; sackStats!(0, 1)
                Locate 18, 29: Print " "

        End Select

    Loop Until B9 = 0


    '---------------
    ' Team Defense
    '---------------
    Cls

    X$ = "TEAM DEFENSIVE STATS"
    Call TITLE_STAT_HEADER(X$)

    Locate 3, 1
    Print " 1. TM 1ST DOWNS "; allTeamStats!(1, 0)
    Print " 2. TM RUSH ATT "; allTeamStats!(1, 1)
    Print " 3. TM RUSH YDS "; Int(allTeamStats!(1, 2))
    Print " 4. TM PASS ATT "; allTeamStats!(1, 3)
    Print " 5. TM PASS COM "; allTeamStats!(1, 4)
    Print " 6. TM PASS INT "; allTeamStats!(1, 5)
    Print " 7. TM PASS YDS "; Int(allTeamStats!(1, 6))
    Print " 8. TM PUNTS "; allTeamStats!(1, 7)
    Print " 9. TM TOTAL PUNT YDS "; Int(allTeamStats!(1, 8))
    Print "10. TM FUMBLES "; allTeamStats!(1, 9)
    Print "11. TM FUM LOST "; allTeamStats!(1, 10)
    Print "12. TM PENALTIES "; allTeamStats!(1, 11)
    Print "13. TM PENALTY YDS "; Int(allTeamStats!(1, 12))
    Print "14. TM TIMES SACKED "; sackStats!(1, 0)
    Print "15. TM YARDS LOST FROM SACKS "; Int(sackStats!(1, 1))

    Do

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 15

        Select Case B9

            Case 1
                Locate 3, 17: Print Space$(10)
                Locate 3, 1: Input " 1. TM 1ST DOWNS"; allTeamStats!(1, 0)
                Locate 3, 17: Print " "

            Case 2
                Locate 4, 16: Print Space$(10)
                Locate 4, 1: Input " 2. TM RUSH ATT"; allTeamStats!(1, 1)
                Locate 4, 16: Print " "

            Case 3
                Locate 5, 16: Print Space$(10)
                Locate 5, 1: Input " 3. TM RUSH YDS"; allTeamStats!(1, 2)
                Locate 5, 16: Print " "

            Case 4
                Locate 6, 16: Print Space$(10)
                Locate 6, 1: Input " 4. TM PASS ATT"; allTeamStats!(1, 3)
                Locate 6, 16: Print " "

            Case 5
                Locate 7, 16: Print Space$(10)
                Locate 7, 1: Input " 5. TM PASS COM"; allTeamStats!(1, 4)
                Locate 7, 16: Print " "

            Case 6
                Locate 8, 16: Print Space$(10)
                Locate 8, 1: Input " 6. TM PASS INT"; allTeamStats!(1, 5)
                Locate 8, 16: Print " "

            Case 7
                Locate 9, 16: Print Space$(10)
                Locate 9, 1: Input " 7. TM PASS YDS"; allTeamStats!(1, 6)
                Locate 9, 16: Print " "

            Case 8
                Locate 10, 13: Print Space$(10)
                Locate 10, 1: Input " 8. TM PUNTS"; allTeamStats!(1, 7)
                Locate 10, 13: Print " "

            Case 9
                Locate 11, 22: Print Space$(10)
                Locate 11, 1: Input " 9. TM TOTAL PUNT YDS"; allTeamStats!(1, 8)
                Locate 11, 22: Print " "

            Case 10
                Locate 12, 15: Print Space$(10)
                Locate 12, 1: Input "10. TM FUMBLES"; allTeamStats!(1, 9)
                Locate 12, 15: Print " "

            Case 11
                Locate 13, 16: Print Space$(10)
                Locate 13, 1: Input "11. TM FUM LOST"; allTeamStats!(1, 10)
                Locate 13, 16: Print " "

            Case 12
                Locate 14, 17: Print Space$(10)
                Locate 14, 1: Input "12. TM PENALTIES"; allTeamStats!(1, 11)
                Locate 14, 17: Print " "

            Case 13
                Locate 15, 19: Print Space$(10)
                Locate 15, 1: Input "13. TM PENALTY YDS"; allTeamStats!(1, 12)
                Locate 15, 19: Print " "

            Case 14
                Locate 16, 20: Print Space$(10)
                Locate 16, 1: Input "14. TM TIMES SACKED"; sackStats!(1, 0)
                Locate 16, 20: Print " "

            Case 15
                Locate 17, 29: Print Space$(10)
                Locate 17, 1: Input "15. TM YARDS LOST FROM SACKS"; sackStats!(1, 1)
                Locate 17, 29: Print " "

        End Select

    Loop Until B9 = 0


    '---------------
    ' Running Backs
    '---------------
    Do

        Cls
        X$ = "CHANGE RUSHERS"
        Call Title_EDITSTATS(2, X$)

        For I1 = 0 To 9:
            Print Using "##"; I1 + 1;
            Print " "; rbName$(I1)
        Next

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 10

        If I > 0 Then

            I = I - 1

            Cls
            Call Title_EDITSTATS(2, X$)

            Locate 3, 1
            Print "1. NAME  "; rbName$(I)
            Print "2. RUSHING ATTEMPTS "; allPlayerStats!(I, 0)
            Print "3. RUSHING YARDAGE "; Int(allPlayerStats!(I, 1))
            Print "4. RUSHING TD'S "; allPlayerStats!(I, 2)
            Print "5. RECEPTIONS "; allPlayerStats!(I, 3)
            Print "6. RECEIVING YARDAGE "; Int(allPlayerStats!(I, 4))
            Print "7. RECEIVING TD'S "; allPlayerStats!(I, 5)

            Do

                Do
                    Locate 20, 1
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 7

                Select Case B9

                    Case 1
                        Locate 3, 1: Input "1. NAME "; rbName$(I1)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 20: Print Space$(10)
                        Locate 4, 1: Input "2. RUSHING ATTEMPTS"; allPlayerStats!(I, 0)
                        Locate 4, 20: Print " "

                    Case 3
                        Locate 5, 19: Print Space$(10)
                        Locate 5, 1: Input "3. RUSHING YARDAGE"; allPlayerStats!(I, 1)
                        Locate 5, 19: Print " "

                    Case 4
                        Locate 6, 16: Print Space$(10)
                        Locate 6, 1: Input "4. RUSHING TD'S"; allPlayerStats!(I, 2)
                        Locate 6, 16: Print " "

                    Case 5
                        Locate 7, 14: Print Space$(10)
                        Locate 7, 1: Input "5. RECEPTIONS"; allPlayerStats!(I, 3)
                        Locate 7, 14: Print " "

                    Case 6
                        Locate 8, 21: Print Space$(10)
                        Locate 8, 1: Input "6. RECEIVING YARDAGE"; allPlayerStats!(I, 4)
                        Locate 8, 21: Print " "

                    Case 7
                        Locate 9, 18: Print Space$(10)
                        Locate 9, 1: Input "7. RECEIVING TD'S"; allPlayerStats!(I, 5)
                        Locate 9, 18: Print " "

                End Select

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

        For I1 = 0 To 5:
            Print Using "##"; I1 + 1;
            Print " "; wrName$(I1)
        Next

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 6

        If I > 0 Then

            I = I - 1

            Cls
            Call Title_EDITSTATS(2, X$)

            Locate 3, 1
            Print "1. NAME  "; wrName$(I)
            Print "2. RECEPTIONS "; allPlayerStats!(I, 6)
            Print "3. RECEIVING YARDAGE "; Int(allPlayerStats!(I, 7))
            Print "4. RECEIVING TD'S "; allPlayerStats!(I, 8)

            Do

                Do
                    Locate 20, 1
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 4

                Select Case B9

                    Case 1
                        Locate 3, 1: Input "1. NAME "; wrName$(I)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 14: Print Space$(10)
                        Locate 4, 1: Input "2. RECEPTIONS"; allPlayerStats!(I, 6)
                        Locate 4, 14: Print " "

                    Case 3
                        Locate 5, 21: Print Space$(10)
                        Locate 5, 1: Input "3. RECEIVING YARDAGE"; allPlayerStats!(I, 7)
                        Locate 5, 21: Print " "

                    Case 4
                        Locate 6, 18: Print Space$(10)
                        Locate 6, 1: Input "4. RECEIVING TD'S"; allPlayerStats!(I, 8)
                        Locate 6, 18: Print " "

                End Select

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

        For I1 = 0 To 3:
            Print Using "##"; I1 + 1;
            Print " "; qbName$(I1)
        Next

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 4

        If I > 0 Then

            I = I - 1

            Cls
            Call Title_EDITSTATS(2, X$)

            Locate 3, 1
            Print "1. NAME  "; qbName$(I)
            Print "2. PASSES THROWN "; allPlayerStats!(I, 9)
            Print "3. PASSES COMPLETED "; allPlayerStats!(I, 10)
            Print "4. PASSES INTERCEPTED "; allPlayerStats!(I, 11)
            Print "5. TOTAL PASSING YARDS "; Int(allPlayerStats!(I, 12))
            Print "6. TOUCHDOWN PASSES "; allPlayerStats!(I, 13)

            Do

                Do
                    Locate 20, 1
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 6

                Select Case B9

                    Case 1
                        Locate 3, 1: Input "1. NAME "; qbName$(I)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 17: Print Space$(10)
                        Locate 4, 1: Input "2. PASSES THROWN"; allPlayerStats!(I, 9)
                        Locate 4, 17: Print " "

                    Case 3
                        Locate 5, 20: Print Space$(10)
                        Locate 5, 1: Input "3. PASSES COMPLETED"; allPlayerStats!(I, 10)
                        Locate 5, 20: Print " "

                    Case 4
                        Locate 6, 22: Print Space$(10)
                        Locate 6, 1: Input "4. PASSES INTERCEPTED"; allPlayerStats!(I, 11)
                        Locate 6, 22: Print " "

                    Case 5
                        Locate 7, 23: Print Space$(10)
                        Locate 7, 1: Input "5. TOTAL PASSING YARDS"; allPlayerStats!(I, 12)
                        Locate 7, 23: Print " "

                    Case 6
                        Locate 8, 20: Print Space$(10)
                        Locate 8, 1: Input "6. TOUCHDOWN PASSES"; allPlayerStats!(I, 13)
                        Locate 8, 20: Print " "

                End Select

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

        For I1 = 0 To 2:
            Print Using "##"; I1 + 1;
            Print " "; krName$(I1)
        Next

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 3

        If I > 0 Then

            I = I - 1

            Cls
            Call Title_EDITSTATS(2, X$)

            Locate 3, 1
            Print "1. NAME  "; krName$(I)
            Print "2. KICKS RETURNED "; allPlayerStats!(I, 14)
            Print "3. KICK RETURN YARDAGE "; Int(allPlayerStats!(I, 15))
            Print "4. KICK RETURN TD'S "; allPlayerStats!(I, 16)

            Do

                Do
                    Locate 20, 1
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 4

                Select Case B9

                    Case 1
                        Locate 3, 1: Input "1. NAME "; krName$(I)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 18: Print Space$(10)
                        Locate 4, 1: Input "2. KICKS RETURNED"; allPlayerStats!(I, 14)
                        Locate 4, 18: Print " "

                    Case 3
                        Locate 5, 23: Print Space$(10)
                        Locate 5, 1: Input "3. KICK RETURN YARDAGE"; allPlayerStats!(I, 15)
                        Locate 5, 23: Print " "

                    Case 4
                        Locate 6, 20: Print Space$(10)
                        Locate 6, 1: Input "4. KICK RETURN TD'S"; allPlayerStats!(I, 16)
                        Locate 6, 20: Print " "

                End Select

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

        For I1 = 3 To 5:
            Print Using "##"; I1 - 2;
            Print " "; prName$(I1 - 3)
        Next

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 3

        If I > 0 Then

            I = I + 2

            Cls
            Call Title_EDITSTATS(2, X$)

            Locate 3, 1
            Print "1. NAME  "; prName$(I - 3)
            Print "2. PUNTS RETURNED "; allPlayerStats!(I, 14)
            Print "3. PUNT RETURN YARDAGE "; Int(allPlayerStats!(I, 15))
            Print "4. PUNT RETURN TD'S "; allPlayerStats!(I, 16)

            Do

                Do
                    Locate 20, 1
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 4

                Select Case B9

                    Case 1
                        Locate 4, 1: Input "1. NAME "; prName$(I - 2)
                        Locate 4, 9: Print " "

                    Case 2
                        Locate 5, 18: Print Space$(10)
                        Locate 5, 1: Input "2. PUNTS RETURNED"; allPlayerStats!(I, 14)
                        Locate 5, 18: Print " "

                    Case 3
                        Locate 6, 23: Print Space$(10)
                        Locate 6, 1: Input "3. PUNT RETURN YARDAGE"; allPlayerStats!(I, 15)
                        Locate 6, 23: Print " "

                    Case 4
                        Locate 7, 20: Print Space$(10)
                        Locate 7, 1: Input "4. PUNT RETURN TD'S"; allPlayerStats!(I, 16)
                        Locate 7, 20: Print " "

                End Select

            Loop Until B9 = 0

        End If

    Loop Until I = 0


    '---------------
    ' Punters
    '---------------
    Cls
    X$ = "CHANGE PUNTER"
    Call Title_EDITSTATS(2, X$)

    Locate 3, 1
    Print "1. NAME  "; punterName$(0)
    Print "2. PUNTS "; allTeamStats!(0, 7)
    Print "3. TOTAL PUNT YARDAGE "; Int(allTeamStats!(0, 8))

    Do

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; B9
        Loop Until B9 >= 0 And B9 <= 3

        Select Case B9

            Case 1
                Locate 3, 1: Input "1. NAME "; punterName$(0)
                Locate 3, 9: Print " "

            Case 2
                Locate 4, 9: Print Space$(10)
                Locate 4, 1: Input "2. PUNTS "; allTeamStats!(0, 7)
                Locate 4, 9: Print " "

            Case 3
                Locate 5, 22: Print Space$(10)
                Locate 5, 1: Input "3. TOTAL PUNT YARDAGE "; allTeamStats!(0, 8)
                Locate 5, 22: Print " "

        End Select

    Loop Until B9 = 0


    '---------------
    ' Kickers
    '---------------
    Do

        Cls
        X$ = "CHANGE PLACE KICKERS"
        Call Title_EDITSTATS(2, X$)

        For I1 = 0 To 1:
            Print Using "##"; I1 + 1;
            Print " "; pkName$(I1)
        Next

        Do
            Locate 20, 29
            Print Space$(10)
            Locate 20, 1
            Input "# TO CHANGE (0 TO CONTINUE)"; I
        Loop Until I >= 0 And I <= 2

        If I > 0 Then

            I = I - 1

            Cls
            Call Title_EDITSTATS(2, X$)

            Locate 3, 1
            Print "1. NAME  "; pkName$(I)
            Print "2. FIELD GOALS ATT "; allPlayerStats!(4 + I * 4, 17)
            Print "3. FIELD GOALS MADE "; allPlayerStats!(3 + I * 4, 17)
            Print "4. EXTRA POINTS ATT "; allPlayerStats!(6 + I * 4, 17)
            Print "5. EXTRA POINTS MADE "; allPlayerStats!(5 + I * 4, 17)

            Do

                Do
                    Locate 20, 1
                    Input "# TO CHANGE (0 TO CONTINUE)"; B9
                Loop Until B9 >= 0 And B9 <= 5

                Select Case B9

                    Case 1
                        Locate 3, 1: Input "1. NAME "; pkName$(I)
                        Locate 3, 9: Print " "

                    Case 2
                        Locate 4, 19: Print Space$(10)
                        Locate 4, 1: Input "2. FIELD GOALS ATT"; allPlayerStats!(4 + I * 4, 17)
                        Locate 4, 19: Print " "

                    Case 3
                        Locate 5, 20: Print Space$(10)
                        Locate 5, 1: Input "3. FIELD GOALS MADE"; allPlayerStats!(3 + I * 4, 17)
                        Locate 5, 20: Print " "

                    Case 4
                        Locate 6, 20: Print Space$(10)
                        Locate 6, 1: Input "4. EXTRA POINTS ATT"; allPlayerStats!(6 + I * 4, 17)
                        Locate 6, 20: Print " "

                    Case 5
                        Locate 7, 21: Print Space$(10)
                        Locate 7, 1: Input "5. EXTRA POINTS MADE"; allPlayerStats!(5 + I * 4, 17)
                        Locate 7, 21: Print " "

                End Select

            Loop Until B9 = 0

        End If

    Loop Until I = 0


    '-------------------------
    ' Update Calculations
    '-------------------------
    For J = 0 To 13
        For K = 0 To 17
            statsPlayers!(J, K) = allPlayerStats!(J, K)
        Next K
    Next J

    For L = 0 To 12:
        statsTeam!(L) = allTeamStats!(0, L)
    Next L

    For L = 0 To 12:
        statsOpp!(L) = allTeamStats!(1, L)
    Next L

    statsTeam!(13) = sackStats!(0, 0)
    statsTeam!(14) = sackStats!(0, 1)
    statsOpp!(13) = sackStats!(1, 0)
    statsOpp!(14) = sackStats!(1, 1)


    '---------------
    ' Save Edits
    '---------------
    Open diskPaths$(1) + statFile$ For Output As #1

    Print #1, teamName$

    For I = 0 To 9:
        Print #1, rbName$(I)
    Next I

    For I = 0 To 5:
        Print #1, wrName$(I)
    Next I

    For I = 0 To 3:
        Print #1, qbName$(I)
    Next I

    For I = 0 To 2:
        Print #1, krName$(I)
    Next I

    For I = 0 To 2:
        Print #1, prName$(I)
    Next I

    Print #1, punterName$(0)

    For I = 0 To 1:
        Print #1, pkName$(I)
    Next I

    For J = 0 To 13
        For K = 0 To 17:
            Print #1, statsPlayers!(J, K)
        Next K
    Next J

    For L = 0 To 38:
        Print #1, statsTeam!(L)
    Next L

    For L = 0 To 38:
        Print #1, statsOpp!(L)
    Next L

    For J = 0 To 9:
        Print #1, intName$(J)
        For J1 = 0 To 2
            Print #1, statsGI(J, J1)
        Next J1
    Next

    For J = 0 To 14:
        Print #1, sackName$(J)
        For J1 = 0 To 1
            Print #1, statsGS(J, J1)
        Next J1
    Next

    For I = 0 To 9:
        Print #1, statsLRush!(I)
        Print #1, statsLRush$(I, 0)
        Print #1, statsLRush$(I, 1)
    Next

    For I = 0 To 15:
        Print #1, statsLRec!(I)
        Print #1, statsLRec$(I, 0)
        Print #1, statsLRec$(I, 1)
    Next

    For I = 0 To 3:
        Print #1, statsLPass!(I)
        Print #1, statsLPass$(I, 0)
        Print #1, statsLPass$(I, 1)
    Next

    For I = 0 To 2:
        Print #1, statsLKR!(I)
        Print #1, statsLKR$(I, 0)
        Print #1, statsLKR$(I, 1)
    Next

    For I = 0 To 2:
        Print #1, statsLPR!(I)
        Print #1, statsLPR$(I, 0)
        Print #1, statsLPR$(I, 1)
    Next

    For I = 0 To 1:
        Print #1, statsLFG!(I)
        Print #1, statsLFG$(I)
        For I1 = 0 To 4:
            Print #1, statsFGM!(I, I1)
            Print #1, statsFGA!(I, I1)
        Next
    Next

    For I = 0 To 9:
        Print #1, statsLInt!(I)
        Print #1, statsLInt$(I, 0)
        Print #1, statsLInt$(I, 1)
    Next

    Close #1

End Sub


'----------------------------------------
'       ChangeScheduleStats Subroutine
'----------------------------------------
'This subroutine inputs stats from the
'chosen Stat Schedule file and then gives
'the user an interface for modifying the
'schedule results.
Sub ChangeScheduleStats (teamName$)

    Shared gameSite$()
    Shared locIndicator$(), oppName$()

    Shared oppScore(), powerRating(), teamScore()

    Shared gameAttendance&()

    Shared leagPtsAgnst!(), leagPtsFor!()

    If Not _FileExists(diskPaths$(1) + teamName$ + "S") Then

        MsgText$ = "NO COMPILED STATS COULD BE FOUND FOR THIS TEAM. PRESS ANY KEY TO RETURN TO THE MAIN MENU"
        BoxWidth = 24
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((LAST_ROW - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        K$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    Else

        Call ReadScheduleStats(teamName$, null, nbrGames)

        Do

            Color 15, 0
            Cls

            X$ = "TEAM SCHEDULE STATS"
            Call TITLE_STAT_HEADER(X$)

            homeScore = 0
            oppTeam$ = ""
            oppPts = 0
            homeOrAway$ = ""
            attendance& = 0
            site$ = ""

            Do

                Locate 2, 1: Print "1. SCORE FOR "; teamName$; "  "; homeScore
                Locate 3, 1: Print "2. OPPONENT'S NAME  "; oppTeam$
                Locate 4, 1: Print "3. OPPONENT'S SCORE  "; oppPts
                Locate 5, 1: Print "4. (H)OME, (A)WAY OR (N)EUTRAL  "; homeOrAway$
                Locate 6, 1: Print "5. ATTENDANCE  "; attendance&
                Locate 7, 1: Print "6. GAME SITE  "; site$

                Do
                    Locate 9, 1: Print "ENTER # TO CHANGE (1-4) OR 0 TO CONTINUE"
                    J$ = GetKeyPress$
                    D = Val(J$)
                Loop Until D >= 0 And D <= 6

                Select Case D

                    Case 1:
                        Locate 2, 1: Print "1. SCORE FOR "; teamName$; " ";
                        Input homeScore
                        Locate 2, 13 + Len(teamName$): Print " "

                    Case 2:
                        Locate 3, 1: Input "2. OPPONENT'S NAME"; oppTeam$
                        Locate 3, 20: Print " "

                    Case 3:
                        Locate 4, 21: Print Space$(10)
                        Locate 4, 1: Input "3. OPPONENT'S SCORE "; oppPts
                        Locate 4, 21: Print " "

                    Case 4:
                        Do
                            Locate 5, 1: Input "4. (H)OME, (A)WAY OR (N)EUTRAL"; homeOrAway$
                            Locate 5, 31: Print " "
                        Loop Until homeOrAway$ = "A" Or homeOrAway$ = "H" Or homeOrAway$ = "N"

                    Case 5:
                        Locate 6, 15: Print Space$(10)
                        Locate 6, 1: Input "5. ATTENDANCE "; attendance&
                        Locate 6, 15: Print " "

                    Case 6:
                        Locate 7, 1: Input "6. GAME SITE"; site$
                        Locate 7, 13: Print " "

                End Select

            Loop Until D = 0

            If homeOrAway$ = "N" Then homeOrAway$ = "X"
            nbrGames = nbrGames + 1
            teamScore(nbrGames) = homeScore
            oppName$(nbrGames) = oppTeam$
            oppScore(nbrGames) = oppPts
            locIndicator$(nbrGames) = homeOrAway$
            gameAttendance&(nbrGames) = attendance&
            gameSite$(nbrGames) = site$

            Do
                Locate 11, 1
                Print "ADD ANOTHER GAME? (Y/N)";
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Loop Until UCase$(I$) = "N"

        Call SaveScheduleStats(teamName$, nbrGames)

        Cls

    End If 'Done checking if file exists

End Sub
