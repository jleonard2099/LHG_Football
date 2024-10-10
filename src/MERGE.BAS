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

        Call box0(Row%-1, Column%-1, Row%+Count%, Column%+menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%

                Case 1: 
                    Call MergeStats
                
                Case 2: 
                    Call ALIGN

            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

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
    Shared pkName$(), pkName_Road$()
    Shared prName$(), prName_Road$(), punterName$(), punterName_Road$()
    Shared intName$(), intName_Road$(), sackName$(), intName_Road$()

    Shared statsLRec$(), statsLFG$() 
    Shared statsLInt$(), statsLKR$()
    Shared statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Shared longCatchName_Road$(), longFGName_Road$()
    Shared longIntName_Road$(), longKRName_Road$()
    Shared longPassName_Road$(), longPRName_Road$()
    Shared longRunName_Road$()

    Shared longCatchVal_Road!(), longFGVal_Road!()
    Shared longIntVal_Road!(), longKRVal_Road!(), longPassVal_Road!()
    Shared longRunVal_Road!(), longPRVal_Road!()

    Shared gameSite$(), gameSite_Road$()
    Shared oppName$(), oppName_Road$()
    Shared locIndicator$(), locIndicator_Road$()

    Shared FGA_Road!(), FGM_Road!()
    Shared nbrInt_Road(), nbrSack_Road()
    Shared statsFGA!(), statsFGM!()
    Shared statsLRec!(), statsLFG!()
    Shared statsLInt!(), statsLKR!(), statsLPass!()
    Shared statsLRush!(), statsLPR!()
    Shared statsTeam!(), statsPlayers!(), statsOpp!()
    Shared statsTeam_Road!(), statsOpp_Road!(), statsPlayers_Road!()
    Shared statsGI(), statsGS()

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
                            Input #1, statsPlayers_Road!(J, K)
                        Next K
                    Next J

                    For L = 0 To 38: 
                        Input #1, statsTeam_Road!(L)
                    Next L
                        
                    For L = 0 To 38: 
                        Input #1, statsOpp_Road!(L)
                    Next L

                    For J = 0 To 9: 
                        Input #1, intName_Road$(J)
                        For J1 = 0 To 2: 
                            Input #1, nbrInt_Road(J, J1)
                        Next
                    Next
                    
                    For J = 0 To 14: 
                        Input #1, intName_Road$(J)
                        For J1 = 0 To 1: 
                            Input #1, nbrSack_Road(J, J1)
                        Next
                    Next
                    
                    For I = 0 To 9: 
                        Input #1, longRunVal_Road!(I)
                        Input #1, longRunName_Road$(I, 0)
                        Input #1, longRunName_Road$(I, 1)
                    Next
                    
                    For I = 0 To 15: 
                        Input #1, longCatchVal_Road!(I)
                        Input #1, longCatchName_Road$(I, 0)
                        Input #1, longCatchName_Road$(I, 1)
                    Next
                    
                    For I = 0 To 3: 
                        Input #1, longPassVal_Road!(I)
                        Input #1, statsLRush$(I, 0)
                        Input #1, statsLRush$(I, 1)
                    Next
                    
                    For I = 0 To 2: 
                        Input #1, longKRVal_Road!(I)
                        Input #1, longKRName_Road$(I, 0)
                        Input #1, longKRName_Road$(I, 1)
                    Next
                    
                    For I = 0 To 2: 
                        Input #1, longPRVal_Road!(I)
                        Input #1, longPRName_Road$(I, 0)
                        Input #1, longPRName_Road$(I, 1)
                    Next

                    For I = 0 To 1:

                        Input #1, longFGVal_Road!(I)
                        Input #1, longFGName_Road$(I):

                        For I1 = 0 To 4
                            Input #1, FGM_Road!(I, I1)
                            Input #1, FGA_Road!(I, I1)
                        Next

                    Next

                    For I = 0 To 9: 
                        Input #1, longIntVal_Road!(I)
                        Input #1, longIntName_Road$(I, 0)
                        Input #1, longIntName_Road$(I, 1)
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
                                Input #3, statsPlayers!(J, K)
                            Next K
                        Next J

                        For L = 0 To 38: 
                            Input #3, statsTeam!(L)
                        Next L

                        For L = 0 To 38: 
                            Input #3, statsOpp!(L)
                        Next L

                        For J = 0 To 9: 
                            Input #3, intName$(J)
                            For J1 = 0 To 2: 
                                Input #3, statsGI(J, J1)
                            Next
                        Next

                        For J = 0 To 14: 
                            Input #3, sackName$(J)
                            For J1 = 0 To 1: 
                                Input #3, statsGS(J, J1)
                            Next
                        Next

                        For I = 0 To 9: 
                            Input #3, statsLRush!(I)
                            Input #3, statsLRush$(I, 0)
                            Input #3, statsLRush$(I, 1)
                        Next

                        For I = 0 To 15: 
                            Input #3, statsLRec!(I)
                            Input #3, statsLRec$(I, 0)
                            Input #3, statsLRec$(I, 1)
                        Next

                        For I = 0 To 3: 
                            Input #3, statsLPass!(I)
                            Input #3, statsLPass$(I, 0)
                            Input #3, statsLPass$(I, 1)
                        Next

                        For I = 0 To 2: 
                            Input #3, statsLKR!(I)
                            Input #3, statsLKR$(I, 0)
                            Input #3, statsLKR$(I, 1)
                        Next

                        For I = 0 To 2: 
                            Input #3, statsLPR!(I)
                            Input #3, statsLPR$(I, 0)
                            Input #3, statsLPR$(I, 1)
                        Next

                        For I = 0 To 1: 
                            Input #3, statsLFG!(I)
                            Input #3, statsLFG$(I)
                            For I1 = 0 To 4: 
                                Input #3, statsFGM!(I, I1)
                                Input #3, statsFGA!(I, I1)
                            Next
                        Next

                        For I = 0 To 9: 
                            Input #3, statsLInt!(I)
                            Input #3, statsLInt$(I, 0)
                            Input #3, statsLInt$(I, 1)
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
    Shared intName$(), intName_Road$(), sackName$(), intName_Road$()

    Shared longCatchName_Road$(), longFGName_Road$()
    Shared longIntName_Road$(), longKRName_Road$()
    Shared longPassName_Road$(), longPRName_Road$()
    Shared longRunName_Road$()
    
    Shared statsLRec$(), statsLFG$()
    Shared statsLInt$(), statsLKR$()
    Shared statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Shared gameSite$(), gameSite_Road$()
    Shared oppName$(), oppName_Road$()
    Shared locIndicator$(), locIndicator_Road$()

    Shared FGA_Road!(), FGM_Road!(), statsFGM!()
    Shared nbrInt_Road(), nbrSack_Road()
    
    Shared longCatchVal_Road!(), longFGVal_Road!()
    Shared longIntVal_Road!(), longKRVal_Road!()
    Shared longPassVal_Road!(), longPRVal_Road!()
    Shared longRunVal_Road!()

    Shared statsLRec!(), statsLFG!()
    Shared statsLInt!(), statsLKR!(), statsLPass!()
    Shared statsLRush!(), statsLPR!()

    Shared statsFGA!()
    Shared statsTeam!(), statsPlayers!(), statsOpp!()
    Shared statsTeam_Road!(), statsOpp_Road!(), statsPlayers_Road!()
    
    Shared statsGI(), statsGS()

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
            statsPlayers!(I, K) = statsPlayers!(I, K) + statsPlayers_Road!(I, K)
        Next K
    Next I

    For I = 0 To 27:
        statsTeam!(I) = statsTeam!(I) + statsTeam_Road!(I)
        statsOpp!(I) = statsOpp!(I) + statsOpp_Road!(I)
    Next

    For I = 29 To 38:
        statsTeam!(I) = statsTeam!(I) + statsTeam_Road!(I)
        statsOpp!(I) = statsOpp!(I) + statsOpp_Road!(I)
    Next

    If statsTeam_Road!(28) > statsTeam!(28) Then 
        statsTeam!(28) = statsTeam_Road!(28)
    End If

    For I = 0 To 9:
        For I1 = 0 To 2
            statsGI(I, I1) = statsGI(I, I1) + nbrInt_Road(I, I1)
        Next
    Next

    For I = 0 To 14
        For I1 = 0 To 1
            statsGS(I, I1) = statsGS(I, I1) + nbrSack_Road(I, I1)
        Next
    Next

    For I = 0 To 9
        If longRunVal_Road!(I) > statsLRush!(I) Then
            statsLRush!(I) = longRunVal_Road!(I)
            statsLRush$(I, 0) = longRunName_Road$(I, 0)
            statsLRush$(I, 1) = longRunName_Road$(I, 1)
        End If
    Next

    For I = 0 To 15
        If longCatchVal_Road!(I) > statsLRec!(I) Then
            statsLRec!(I) = longCatchVal_Road!(I)
            statsLRec$(I, 0) = longCatchName_Road$(I, 0)
            statsLRec$(I, 1) = longCatchName_Road$(I, 1)
        End If
    Next

    For I = 0 To 3:
        If longPassVal_Road!(I) > statsLPass!(I) Then
            statsLPass!(I) = longPassVal_Road!(I)
            statsLPass$(I, 0) = statsLRush$(I, 0)
            statsLPass$(I, 1) = statsLRush$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If longKRVal_Road!(I) > statsLKR!(I) Then
            statsLKR!(I) = longKRVal_Road!(I)
            statsLKR$(I, 0) = longKRName_Road$(I, 0)
            statsLKR$(I, 1) = longKRName_Road$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If longPRVal_Road!(I) > statsLPR!(I) Then
            statsLPR!(I) = longPRVal_Road!(I)
            statsLPR$(I, 0) = longPRName_Road$(I, 0)
            statsLPR$(I, 1) = longPRName_Road$(I, 1)
        End If
    Next

    For I = 0 To 1:
        If longFGVal_Road!(I) > statsLFG!(I) Then
            statsLFG!(I) = longFGVal_Road!(I)
            statsLFG$(I) = longFGName_Road$(I)
        End If
    Next

    For I = 0 To 1:
        For I1 = 0 To 4
            statsFGM!(I, I1) = statsFGM!(I, I1) + FGM_Road!(I, I1)
            statsFGA!(I, I1) = statsFGA!(I, I1) + FGA_Road!(I, I1)
        Next
    Next

    For I = 0 To 9
        If longIntVal_Road!(I) > statsLInt!(I) Then
            statsLInt!(I) = longIntVal_Road!(I)
            statsLInt$(I, 0) = longIntName_Road$(I, 0)
            statsLInt$(I, 1) = longIntName_Road$(I, 1)
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
        For J1 = 0 To 2: 
            Print #1, statsGI(J, J1)
        Next
    Next

    For J = 0 To 14:
        Print #1, sackName$(J)
        For J1 = 0 To 1: Print #1, statsGS(J, J1): Next
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
