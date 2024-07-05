'----------------------------------------
'            MERGE Subroutine
'----------------------------------------
'This subroutine gives the user options
'for aligning / merging stat files
Sub MERGE ()

    _Title "3-in-1 Football - Merge Stats"

    ReDim menuChoices$(1 To 3)

    Do

        Color , 0
        Cls

        Call TITLE_MERGE

        menuChoices$(1) = "MERGE STATISTICAL FILES"
        menuChoices$(2) = "ALIGN STATISTICAL FILES"
        menuChoices$(3) = "EXIT TO MAIN MENU      "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor%, FileBGColor%)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case Selection%

                Case 1: 
                    Call MergeStats
                
                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'          MergeStats Subroutine
'----------------------------------------
'This subroutine determines the parameters
'for the road and existing stat files that
'can be merged. It then reads in all stat
'files, checks for player sequence, and then
'calls a supporting routine to actually
'perform the merge
Sub MergeStats ()

    Shared rbName$(), rbName_Road$(), wrName$(), wrName_Road$()
    Shared qbName$(), qbName_Road$(), krName$(), krName_Road$()
    Shared prName$(), prName_Road$(), punterName$(), punterName_Road$()
    Shared pkName$(), pkName_Road$()
    Shared intName$(), DRI$(), sackName$(), DSR$()
    Shared statsLC$(), LCR$(), statsLF$(), LFR$()
    Shared statsLI$(), LIR$(), statsLK$(), LKR$()
    Shared statsLP$(), LPR$(), statsLR$(), LRR$()
    Shared statsPK$(), PKR$()

    Shared gameSite$(), gameSite_Road$()
    Shared oppName$(), oppName_Road$()
    Shared locIndicator$(), locIndicator_Road$()

    Shared AF!(), AFR!(), AMR!(), AM!()
    Shared statsGI%(), GIR%(), statsGS%(), GSR%()
    Shared statsLC!(), LCR!(), statsLF!(), LFR!()
    Shared statsLI!(), LIR!(), statsLK!(), LKR!(), statsLP!(), LPR!()
    Shared statsLR!(), LRR!(), statsPK!(), PKR!()
    Shared statsZ!(), statsZ2!(), statsZ1!()
    Shared ZR!(), ZR1!(), ZR2!()

    Shared teamScore(), teamScore_Road()
    Shared oppScore(), oppScore_Road()
    Shared powerRating(), powerRating_Road()
    Shared gameAttendance&(), gameAtt_Road!()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            Color 15, 0
            Cls

            Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; pathRoadStats$
            Locate , 15: Print "MASTER STATS ARE ON DRIVE "; pathCurrentStats$
            Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; team$

            Do
                Locate 14, 15: Print "ANY CHANGE? (Y/N)"
                J$ = GetKeyPress$
            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

            If UCase$(J$) = "N" Then

                If _FileExists(pathRoadStats$ + statFile$) Then

                    Open pathRoadStats$ + statFile$ For Input As #1

                    Input #1, B$

                    For I = 0 To 9: 
                        Input #1, rbName_Road$(I)
                    Next I

                    For I = 0 To 5: 
                        Input #1, wrName_Road$(I)
                    Next I

                    For I = 0 To 3: 
                        Input #1, qbName_Road$(I)
                    Next I

                    For I = 0 To 2: 
                        Input #1, krName_Road$(I)
                    Next I

                    For I = 0 To 2: 
                        Input #1, prName_Road$(I)
                    Next I

                    Input #1, punterName_Road$(0)

                    For I = 0 To 1: 
                        Input #1, pkName_Road$(I)
                    Next I

                    For J = 0 To 13
                        For K = 0 To 17: 
                            Input #1, ZR2!(J, K)
                        Next K
                    Next J

                    For L = 0 To 38: 
                        Input #1, ZR!(L)
                    Next L
                        
                    For L = 0 To 38: 
                        Input #1, ZR1!(L)
                    Next L

                    For J = 0 To 9: 
                        Input #1, DRI$(J)
                        For J1 = 0 To 2: 
                            Input #1, GIR%(J, J1)
                        Next
                    Next
                    
                    For J = 0 To 14: 
                        Input #1, DSR$(J)
                        For J1 = 0 To 1: 
                            Input #1, GSR%(J, J1)
                        Next
                    Next
                    
                    For I = 0 To 9: 
                        Input #1, LRR!(I)
                        Input #1, LRR$(I, 0)
                        Input #1, LRR$(I, 1)
                    Next
                    
                    For I = 0 To 15: 
                        Input #1, LCR!(I)
                        Input #1, LCR$(I, 0)
                        Input #1, LCR$(I, 1)
                    Next
                    
                    For I = 0 To 3: 
                        Input #1, LPR!(I)
                        Input #1, statsLR$(I, 0)
                        Input #1, statsLR$(I, 1)
                    Next
                    
                    For I = 0 To 2: 
                        Input #1, LKR!(I)
                        Input #1, LKR$(I, 0)
                        Input #1, LKR$(I, 1)
                    Next
                    
                    For I = 0 To 2: 
                        Input #1, PKR!(I)
                        Input #1, PKR$(I, 0)
                        Input #1, PKR$(I, 1)
                    Next

                    For I = 0 To 1:

                        Input #1, LFR!(I)
                        Input #1, LFR$(I):

                        For I1 = 0 To 4
                            Input #1, AMR!(I, I1)
                            Input #1, AFR!(I, I1)
                        Next

                    Next

                    For I = 0 To 9: 
                        Input #1, LIR!(I)
                        Input #1, LIR$(I, 0)
                        Input #1, LIR$(I, 1)
                    Next

                    Close #1

                    Open pathRoadStats$ + statFile$ + "S" For Input As #2

                    Input #2, nbrGamesRoad

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, teamScore_Road(I)
                    Next I
                    
                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, oppName_Road$(I)
                    Next I
                    
                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, oppScore_Road(I)
                    Next I
                    
                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, locIndicator_Road$(I)
                    Next I

                    For I = 0 To 1
                        For I1 = 1 To MAX_SCHED_STATS: 
                            Input #2, powerRating_Road(I, I1)
                        Next
                    Next

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, gameAtt_Road!(I)
                    Next I

                    For I = 1 To MAX_SCHED_STATS: 
                        Input #2, (gameSite_Road$(I))
                    Next I

                    Close #2

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #3

                        Input #3, B$

                        For I = 0 To 9: 
                            Input #3, rbName$(I)
                        Next I
                        
                        For I = 0 To 5: 
                            Input #3, wrName$(I)
                        Next I
                        
                        For I = 0 To 3: 
                            Input #3, qbName$(I)
                        Next I
                        
                        For I = 0 To 2: 
                            Input #3, krName$(I)
                        Next I
                        
                        For I = 0 To 2: 
                            Input #3, prName$(I)
                        Next I

                        Input #3, punterName$(0)

                        For I = 0 To 1: 
                            Input #3, pkName$(I)
                        Next I

                        For J = 0 To 13
                            For K = 0 To 17: 
                                Input #3, statsZ2!(J, K)
                            Next K
                        Next J

                        For L = 0 To 38: 
                            Input #3, statsZ!(L)
                        Next L

                        For L = 0 To 38: 
                            Input #3, statsZ1!(L)
                        Next L

                        For J = 0 To 9: 
                            Input #3, intName$(J)
                            For J1 = 0 To 2: 
                                Input #3, statsGI%(J, J1)
                            Next
                        Next

                        For J = 0 To 14: 
                            Input #3, sackName$(J)
                            For J1 = 0 To 1: 
                                Input #3, statsGS%(J, J1)
                            Next
                        Next

                        For I = 0 To 9: 
                            Input #3, statsLR!(I)
                            Input #3, statsLR$(I, 0)
                            Input #3, statsLR$(I, 1)
                        Next

                        For I = 0 To 15: 
                            Input #3, statsLC!(I)
                            Input #3, statsLC$(I, 0)
                            Input #3, statsLC$(I, 1)
                        Next

                        For I = 0 To 3: 
                            Input #3, statsLP!(I)
                            Input #3, statsLP$(I, 0)
                            Input #3, statsLP$(I, 1)
                        Next

                        For I = 0 To 2: 
                            Input #3, statsLK!(I)
                            Input #3, statsLK$(I, 0)
                            Input #3, statsLK$(I, 1)
                        Next

                        For I = 0 To 2: 
                            Input #3, statsPK!(I)
                            Input #3, statsPK$(I, 0)
                            Input #3, statsPK$(I, 1)
                        Next

                        For I = 0 To 1: 
                            Input #3, statsLF!(I)
                            Input #3, statsLF$(I)
                            For I1 = 0 To 4: 
                                Input #3, AM!(I, I1)
                                Input #3, AF!(I, I1)
                            Next
                        Next

                        For I = 0 To 9: 
                            Input #3, statsLI!(I)
                            Input #3, statsLI$(I, 0)
                            Input #3, statsLI$(I, 1)
                        Next

                        Close #3

                        Open pathCurrentStats$ + statFile$ + "S" For Input As #4

                        Input #4, nbrGamesCurr

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, teamScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, oppName$(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, oppScore(I)
                        Next I

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, locIndicator$(I)
                        Next I

                        For I = 0 To 1
                            For I1 = 1 To MAX_SCHED_STATS: 
                                Input #4, powerRating(I, I1)
                            Next
                        Next

                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, gameAttendance&(I)
                        Next I
                        
                        For I = 1 To MAX_SCHED_STATS: 
                            Input #4, gameSite$(I)
                        Next I

                        Close #4

                        SEQ = 0

                        For K = 0 To 9
                            If UCase$(rbName_Road$(K)) <> UCase$(rbName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 5
                            If UCase$(wrName_Road$(K)) <> UCase$(wrName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 3
                            If UCase$(qbName_Road$(K)) <> UCase$(qbName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 2
                            If UCase$(krName_Road$(K)) <> UCase$(krName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 2
                            If UCase$(prName_Road$(K)) <> UCase$(prName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If UCase$(punterName_Road$(0)) <> UCase$(punterName$(0)) Then
                            SEQ = SEQ + 1
                        End If

                        If UCase$(pkName_Road$(0)) <> UCase$(pkName$(0)) Then
                            SEQ = SEQ + 1
                        End If

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, B$, nbrGamesCurr, nbrGamesRoad)

                            For J = 13 To 23
                                Locate J
                                Print Space$(65)
                            Next

                        Else

                            Cls
                            Color 14
                            Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"
                            Color 15
                            Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                            Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                            Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                            Color 12
                            Locate 11, 24: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                            Locate , 31: Print "ENTER STATS MANUALLY."
                            Color 13
                            Locate 15, 29: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                        End If 'Done checking sequence

                    Else

                        Cls
                        Color 7
                        Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                        Print
                        Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                        Do
                            Locate , 10
                            Print "TRY AGAIN? (Y/N) "
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                        If UCase$(I$) = "Y" Then
                            Print
                            Print "HIT ENTER TO RE-ENTER INFORMATION"
                            I$ = GetKeyPress$
                        End If

                    End If 'Done check if home file exists

                Else

                    Cls
                    Locate 2, 10
                    Color 7: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                    Do
                        Locate , 10
                        Print "TRY AGAIN? (Y/N) "
                        I$ = GetKeyPress$
                    Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                    If UCase$(I$) = "Y" Then
                        Print
                        Print "HIT ENTER TO RE-ENTER INFORMATION"
                        I$ = GetKeyPress$
                    End If

                End If 'Done checking if road file exists

            End If 'Done checking if ready to proceed

        End If 'Done checking for ESC

    Loop Until DUMMY$ <> "" Or UCase$(J$) = "N" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'          DoTheMerge Subroutine
'----------------------------------------
'This subroutine literally adds the stat
'values together from each file and then
'overwrites the current file with the
'newly aggregated results.
Sub DoTheMerge (pathCurrentStats$, statFile$, B$, nbrGamesCurr, nbrGamesRoad)

    Shared rbName$(), rbName_Road$(), wrName$(), wrName_Road$()
    Shared qbName$(), qbName_Road$(), krName$(), krName_Road$()
    Shared prName$(), prName_Road$(), punterName$(), punterName_Road$()
    Shared pkName$(), pkName_Road$()
    Shared intName$(), DRI$(), sackName$(), DSR$()
    Shared statsLC$(), LCR$(), statsLF$(), LFR$()
    Shared statsLI$(), LIR$(), statsLK$(), LKR$()
    Shared statsLP$(), LPR$(), statsLR$(), LRR$()
    Shared statsPK$(), PKR$()

    Shared gameSite$(), gameSite_Road$()
    Shared oppName$(), oppName_Road$()
    Shared locIndicator$(), locIndicator_Road$()

    Shared AF!(), AFR!(), AMR!(), AM!()
    Shared statsGI%(), GIR%(), statsGS%(), GSR%()
    Shared statsLC!(), LCR!(), statsLF!(), LFR!()
    Shared statsLI!(), LIR!(), statsLK!(), LKR!(), statsLP!(), LPR!()
    Shared statsLR!(), LRR!(), statsPK!(), PKR!()
    Shared statsZ!(), statsZ2!(), statsZ1!()
    Shared ZR!(), ZR1!(), ZR2!()

    Shared teamScore(), teamScore_Road()
    Shared oppScore(), oppScore_Road()
    Shared powerRating(), powerRating_Road()
    Shared gameAttendance&(), gameAtt_Road!()

    For K = 1 To nbrGamesRoad
        nbrGamesCurr = nbrGamesCurr + 1
        teamScore(nbrGamesCurr) = teamScore_Road(K)
        oppName$(nbrGamesCurr) = oppName_Road$(K)
        oppScore(nbrGamesCurr) = oppScore_Road(K)
        locIndicator$(nbrGamesCurr) = locIndicator_Road$(K)
        gameAttendance&(nbrGamesCurr) = gameAtt_Road!(K)
        gameSite$(nbrGamesCurr) = gameSiteRoad$(K)

        For I = 0 To 1
            powerRating(I, nbrGamesCurr) = powerRating_Road(I, K)
        Next
    Next

    Color 12
    Locate 18, 20: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 13
        For K = 0 To 17
            statsZ2!(I, K) = statsZ2!(I, K) + ZR2!(I, K)
        Next K
    Next I

    For I = 0 To 27:
        statsZ!(I) = statsZ!(I) + ZR!(I)
        statsZ1!(I) = statsZ1!(I) + ZR1!(I)
    Next

    For I = 29 To 38:
        statsZ!(I) = statsZ!(I) + ZR!(I)
        statsZ1!(I) = statsZ1!(I) + ZR1!(I)
    Next

    If ZR!(28) > statsZ!(28) Then 
        statsZ!(28) = ZR!(28)
    End If

    For I = 0 To 9:
        For I1 = 0 To 2
            statsGI%(I, I1) = statsGI%(I, I1) + GIR%(I, I1)
        Next
    Next

    For I = 0 To 14
        For I1 = 0 To 1
            statsGS%(I, I1) = statsGS%(I, I1) + GSR%(I, I1)
        Next
    Next

    For I = 0 To 9
        If LRR!(I) > statsLR!(I) Then
            statsLR!(I) = LRR!(I)
            statsLR$(I, 0) = LRR$(I, 0)
            statsLR$(I, 1) = LRR$(I, 1)
        End If
    Next

    For I = 0 To 15
        If LCR!(I) > statsLC!(I) Then
            statsLC!(I) = LCR!(I)
            statsLC$(I, 0) = LCR$(I, 0)
            statsLC$(I, 1) = LCR$(I, 1)
        End If
    Next

    For I = 0 To 3:
        If LPR!(I) > statsLP!(I) Then
            statsLP!(I) = LPR!(I)
            statsLP$(I, 0) = statsLR$(I, 0)
            statsLP$(I, 1) = statsLR$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If LKR!(I) > statsLK!(I) Then
            statsLK!(I) = LKR!(I)
            statsLK$(I, 0) = LKR$(I, 0)
            statsLK$(I, 1) = LKR$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If PKR!(I) > statsPK!(I) Then
            statsPK!(I) = PKR!(I)
            statsPK$(I, 0) = PKR$(I, 0)
            statsPK$(I, 1) = PKR$(I, 1)
        End If
    Next

    For I = 0 To 1:
        If LFR!(I) > statsLF!(I) Then
            statsLF!(I) = LFR!(I)
            statsLF$(I) = LFR$(I)
        End If
    Next

    For I = 0 To 1:
        For I1 = 0 To 4
            AM!(I, I1) = AM!(I, I1) + AMR!(I, I1)
            AF!(I, I1) = AF!(I, I1) + AFR!(I, I1)
        Next
    Next

    For I = 0 To 9
        If LIR!(I) > statsLI!(I) Then
            statsLI!(I) = LIR!(I)
            statsLI$(I, 0) = LIR$(I, 0)
            statsLI$(I, 1) = LIR$(I, 1)
        End If
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1

    Print #1, B$

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
        For J1 = 0 To 2: 
            Print #1, statsGI%(J, J1)
        Next
    Next

    For J = 0 To 14:
        Print #1, sackName$(J)
        For J1 = 0 To 1: Print #1, statsGS%(J, J1): Next
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

    Open pathCurrentStats$ + statFile$ + "S" For Output As #2

    Write #2, nbrGamesCurr

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, teamScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, oppName$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, oppScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, locIndicator$(I)
    Next I

    For I = 0 To 1
        For I1 = 1 To MAX_SCHED_STATS: 
            Write #2, powerRating(I, I1)
        Next
    Next

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, gameAttendance&(I)
    Next I

    For I = 1 To MAX_SCHED_STATS: 
        Write #2, gameSite$(I)
    Next I

    Close #2

    Y = 0

    Color 12
    Locate 20, 20: Print "RESULTS SUCCESSFULLY MERGED!!"

End Sub


'----------------------------------------
'       GetMergePaths Subroutine
'----------------------------------------
'This subroutine assists the user with the
'entry of the separate paths to be used 
'for the merge / align process.
Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)

    Shared opSysType$

    Do

        Color 12, 0
        Cls

        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE/ALIGN"
        Color 15
        Select Case opSysType$
            Case "[WINDOWS]":
                Locate 4, 5: Print "EXAMPLE - A:"
                Locate , 5: Print "          C:\HAFFNER"
                Locate , 5: Print "          C:\HAFFNER\1960"
                Locate , 5: Print "          D:\FOOTBALL"
            Case "[LINUX]":
                Locate 4, 5: Print "EXAMPLE - /mnt/floppy"
                Locate , 5: Print "           /usr/bin/haffner"
                Locate , 5: Print "           /usr/bin/haffner/1960"
                Locate , 5: Print "           /home/haffner/football"
            Case "[MAC]", "[MACOSX]":
                Locate 4, 5: Print "EXAMPLE - /Volumes/floppy"
                Locate , 5: Print "           /Applications/haffner"
                Locate , 5: Print "           /Applications/haffner/1960"
                Locate , 5: Print "           /Users/haffner/football"
        End Select

        For J = 10 To 13
            Locate J
            Print Space$(70)
        Next

        Color 10
        Locate 10, 5
        Line Input "ROAD STATS:     "; pathRoadStats$
        Locate , 5
        Locate , 5
        Line Input "CURRENT STATS:  "; pathCurrentStats$

        Color 14
        Locate 13, 5: Print "ANY CHANGE? (Y/N)"
        promptResponse$ = GetKeyPress$

    Loop Until UCase$(promptResponse$) = "N" Or promptResponse$ = Chr$(27)

    'Adjust path terminator for OS type
    Select Case opSysType$
        Case "[WINDOWS]":
            If Not (Right$(pathCurrentStats$, 1) = "\") Then
                pathCurrentStats$ = pathCurrentStats$ + "\"
            End If

            If Not (Right$(pathRoadStats$, 1) = "\") Then
                pathRoadStats$ = pathRoadStats$ + "\"
            End If

        Case "[MAC]", "[MACOSX]", "[LINUX]":
            If Not (Right$(pathCurrentStats$, 1) = "/") Then
                pathCurrentStats$ = pathCurrentStats$ + "/"
            End If

            If Not (Right$(pathRoadStats$, 1) = "/") Then
                pathRoadStats$ = pathRoadStats$ + "/"
            End If
    End Select

End Sub
