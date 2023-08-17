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

        menuChoices$(1) = "MERGE STATISTICAL FILES           "
        menuChoices$(2) = "ALIGN STATISTICAL FILES           "
        menuChoices$(3) = "EXIT TO MAIN MENU                 "
        Selection% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColr% = 15: FileBGColr% = 1
        HiLiteFGColr% = 0: HiLiteBGColr% = 7
        Spacing% = 1
        Row% = 8
        Column% = 25

        Call box0(7, 24, 11, 59, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
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

    Shared Ycurr%, Yroad%

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared intName$(), DRI$(), sackName$(), DSR$()
    Shared statsH$(), HR$()
    Shared LC$(), LCR$(), statsLF$(), LFR$()
    Shared statsLI$(), LIR$(), statsLK$(), LKR$()
    Shared statsLP$(), LPR$(), LR$(), LRR$()
    Shared compO$(), statsPK$(), PKR$()
    Shared gameSITE$(), SITER$()

    Shared AF!(), AFR!(), AMR!(), AM!()
    Shared CRD!(), CRDR!()
    Shared LC!(), LCR!(), LF!(), LFR!()
    Shared LI!(), LIR!(), LK!(), LKR!(), LP!(), LPR!()
    Shared LR!(), LRR!(), PK!(), PKR!()
    Shared statsZ!(), statsZ2!(), statsZ1!()
    Shared ZR!(), ZR1!(), ZR2!()

    Shared compAP%(), APR%()
    Shared statsGI%(), GIR%(), statsGS%(), GSR%()
    Shared statsO%(), statsT%(), TR%()

    Shared ORD$()
    Shared ORD%()

    Do

        Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            'Call GetDiskID_MERGE(A$)
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

                    For I = 0 To 9: Input #1, AR1$(I): Next I
                    For I = 0 To 5: Input #1, AR2$(I): Next I
                    For I = 0 To 3: Input #1, AR3$(I): Next I
                    For I = 0 To 2: Input #1, AR4$(I): Next I
                    For I = 0 To 2: Input #1, AR5$(I): Next I
                    Input #1, AR6$(0)

                    For I = 0 To 1: Input #1, AR7$(I): Next I

                    For J = 0 To 13
                        For K = 0 To 17: Input #1, ZR2!(J, K): Next K
                    Next J

                    For L = 0 To 38: Input #1, ZR!(L): Next L
                    For L = 0 To 38: Input #1, ZR1!(L): Next L
                    For J = 0 To 9: Input #1, DRI$(J): For J1 = 0 To 2: Input #1, GIR%(J, J1): Next: Next
                    For J = 0 To 14: Input #1, DSR$(J): For J1 = 0 To 1: Input #1, GSR%(J, J1): Next: Next
                    For I = 0 To 9: Input #1, LRR!(I): Input #1, LRR$(I, 0): Input #1, LRR$(I, 1): Next
                    For I = 0 To 15: Input #1, LCR!(I): Input #1, LCR$(I, 0): Input #1, LCR$(I, 1): Next
                    For I = 0 To 3: Input #1, LPR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
                    For I = 0 To 2: Input #1, LKR!(I): Input #1, LKR$(I, 0): Input #1, LKR$(I, 1): Next
                    For I = 0 To 2: Input #1, PKR!(I): Input #1, PKR$(I, 0): Input #1, PKR$(I, 1): Next

                    For I = 0 To 1:

                        Input #1, LFR!(I): Input #1, LFR$(I):

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

                    Open pathRoadStats$ + statFile$ + "S" For Input As #4

                    Input #4, Yroad%

                    For I = 1 To 120: Input #4, ORD%(I): Next I
                    For I = 1 To 120: Input #4, ORD$(I): Next I
                    For I = 1 To 120: Input #4, TR%(I): Next I
                    For I = 1 To 120: Input #4, HR$(I): Next I

                    For I = 0 To 1
                        For I1 = 1 To 120: 
                            Input #4, APR%(I, I1)
                        Next
                    Next

                    For I = 1 To 120: Input #4, CRDR!(I): Next I
                    For I = 1 To 120: Input #4, SITER$(I): Next I

                    Close #4

                    If _FileExists(pathCurrentStats$ + statFile$) Then

                        Open pathCurrentStats$ + statFile$ For Input As #1

                        Input #1, B$

                        For I = 0 To 9: Input #1, rbName$(I): Next I
                        For I = 0 To 5: Input #1, wrName$(I): Next I
                        For I = 0 To 3: Input #1, qbName$(I): Next I
                        For I = 0 To 2: Input #1, krName$(I): Next I
                        For I = 0 To 2: Input #1, prName$(I): Next I

                        Input #1, punterName$(0)

                        For I = 0 To 1: Input #1, pkName$(I): Next I

                        For J = 0 To 13
                            For K = 0 To 17: Input #1, statsZ2!(J, K): Next K
                        Next J

                        For L = 0 To 38: Input #1, statsZ!(L): Next L
                        For L = 0 To 38: Input #1, statsZ1!(L): Next L
                        For J = 0 To 9: Input #1, intName$(J): For J1 = 0 To 2: Input #1, statsGI%(J, J1): Next: Next
                        For J = 0 To 14: Input #1, sackName$(J): For J1 = 0 To 1: Input #1, statsGS%(J, J1): Next: Next
                        For I = 0 To 9: Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
                        For I = 0 To 15: Input #1, LC!(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
                        For I = 0 To 3: Input #1, LP!(I): Input #1, statsLP$(I, 0): Input #1, statsLP$(I, 1): Next
                        For I = 0 To 2: Input #1, LK!(I): Input #1, statsLK$(I, 0): Input #1, statsLK$(I, 1): Next
                        For I = 0 To 2: Input #1, PK!(I): Input #1, statsPK$(I, 0): Input #1, statsPK$(I, 1): Next
                        For I = 0 To 1: Input #1, LF!(I): Input #1, statsLF$(I): For I1 = 0 To 4: Input #1, AM!(I, I1): Input #1, AF!(I, I1): Next: Next
                        For I = 0 To 9: Input #1, LI!(I): Input #1, statsLI$(I, 0): Input #1, statsLI$(I, 1): Next

                        Close 1

                        Open pathCurrentStats$ + statFile$ + "S" For Input As #5

                        Input #5, Ycurr%

                        For I = 1 To 120: Input #5, statsO%(I): Next I
                        For I = 1 To 120: Input #5, compO$(I): Next I
                        For I = 1 To 120: Input #5, statsT%(I): Next I
                        For I = 1 To 120: Input #5, statsH$(I): Next I

                        For I = 0 To 1
                            For I1 = 1 To 120: 
                                Input #5, compAP%(I, I1)
                            Next
                        Next

                        For I = 1 To 120: Input #5, CRD!(I): Next I
                        For I = 1 To 120: Input #5, gameSITE$(I): Next I

                        Close #5

                        SEQ = 0

                        For K = 0 To 9
                            If UCase$(AR1$(K)) <> UCase$(rbName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 5
                            If UCase$(AR2$(K)) <> UCase$(wrName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 3
                            If UCase$(AR3$(K)) <> UCase$(qbName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 2
                            If UCase$(AR4$(K)) <> UCase$(krName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        For K = 0 To 2
                            If UCase$(AR5$(K)) <> UCase$(prName$(K)) Then
                                SEQ = SEQ + 1
                            End If
                        Next

                        If UCase$(AR6$(0)) <> UCase$(punterName$(0)) Then
                            SEQ = SEQ + 1
                        End If

                        If UCase$(AR7$(0)) <> UCase$(pkName$(0)) Then
                            SEQ = SEQ + 1
                        End If

                        If SEQ = 0 Then

                            Call DoTheMerge(pathCurrentStats$, statFile$, B$)

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
Sub DoTheMerge (pathCurrentStats$, statFile$, B$)

    Shared Ycurr%, Yroad%

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared intName$(), DRI$(), sackName$(), DSR$()
    Shared statsH$(), HR$()
    Shared LC$(), LCR$(), statsLF$(), LFR$()
    Shared statsLI$(), LIR$(), statsLK$(), LKR$()
    Shared statsLP$(), LPR$(), LR$(), LRR$()
    Shared compO$(), statsPK$(), PKR$()
    Shared gameSITE$(), SITER$()
    Shared ORD$()

    Shared AF!(), AFR!(), AMR!(), AM!()
    Shared CRD!(), CRDR!()
    Shared LC!(), LCR!(), LF!(), LFR!()
    Shared LI!(), LIR!(), LK!(), LKR!(), LP!(), LPR!()
    Shared LR!(), LRR!(), PK!(), PKR!()
    Shared statsZ!(), statsZ2!(), statsZ1!()
    Shared ZR!(), ZR1!(), ZR2!()

    Shared compAP%(), APR%()
    Shared ORD%()
    Shared statsGI%(), GIR%(), statsGS%(), GSR%()
    Shared statsO%(), statsT%(), TR%()

    For K = 1 To Yroad%
        Ycurr% = Ycurr% + 1
        statsO%(Ycurr%) = ORD%(K)
        compO$(Ycurr%) = ORD$(K)
        statsT%(Ycurr%) = TR%(K)
        statsH$(Ycurr%) = HR$(K)
        CRD!(Ycurr%) = CRDR!(K)
        gameSITE$(Ycurr%) = SITER$(K)

        For I = 0 To 1
            compAP%(I, Ycurr%) = APR%(I, K)
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

    If ZR!(28) > statsZ!(28) Then statsZ!(28) = ZR!(28)

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
        If LRR!(I) > LR!(I) Then
            LR!(I) = LRR!(I)
            LR$(I, 0) = LRR$(I, 0)
            LR$(I, 1) = LRR$(I, 1)
        End If
    Next

    For I = 0 To 15
        If LCR!(I) > LC!(I) Then
            LC!(I) = LCR!(I)
            LC$(I, 0) = LCR$(I, 0)
            LC$(I, 1) = LCR$(I, 1)
        End If
    Next

    For I = 0 To 3:
        If LPR!(I) > LP!(I) Then
            LP!(I) = LPR!(I)
            statsLP$(I, 0) = LR$(I, 0)
            statsLP$(I, 1) = LR$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If LKR!(I) > LK!(I) Then
            LK!(I) = LKR!(I)
            statsLK$(I, 0) = LKR$(I, 0)
            statsLK$(I, 1) = LKR$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If PKR!(I) > PK!(I) Then
            PK!(I) = PKR!(I)
            statsPK$(I, 0) = PKR$(I, 0)
            statsPK$(I, 1) = PKR$(I, 1)
        End If
    Next

    For I = 0 To 1:
        If LFR!(I) > LF!(I) Then
            LF!(I) = LFR!(I)
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
        If LIR!(I) > LI!(I) Then
            LI!(I) = LIR!(I)
            statsLI$(I, 0) = LIR$(I, 0)
            statsLI$(I, 1) = LIR$(I, 1)
        End If
    Next

    Open pathCurrentStats$ + statFile$ For Output As #1
    Print #1, B$
    For I = 0 To 9: Print #1, rbName$(I): Next I
    For I = 0 To 5: Print #1, wrName$(I): Next I
    For I = 0 To 3: Print #1, qbName$(I): Next I
    For I = 0 To 2: Print #1, krName$(I): Next I
    For I = 0 To 2: Print #1, prName$(I): Next I

    Print #1, punterName$(0)

    For I = 0 To 1: Print #1, pkName$(I): Next I

    For J = 0 To 13
        For K = 0 To 17: Print #1, statsZ2!(J, K): Next K
    Next J

    For L = 0 To 38: Print #1, statsZ!(L): Next L
    For L = 0 To 38: Print #1, statsZ1!(L): Next L

    For J = 0 To 9:
        Print #1, intName$(J)
        For J1 = 0 To 2: Print #1, statsGI%(J, J1): Next
    Next

    For J = 0 To 14:
        Print #1, sackName$(J)
        For J1 = 0 To 1: Print #1, statsGS%(J, J1): Next
    Next

    For I = 0 To 9:
        Print #1, LR!(I)
        Print #1, LR$(I, 0)
        Print #1, LR$(I, 1)
    Next

    For I = 0 To 15:
        Print #1, LC!(I)
        Print #1, LC$(I, 0)
        Print #1, LC$(I, 1)
    Next

    For I = 0 To 3:
        Print #1, LP!(I)
        Print #1, statsLP$(I, 0)
        Print #1, statsLP$(I, 1)
    Next

    For I = 0 To 2:
        Print #1, LK!(I)
        Print #1, statsLK$(I, 0)
        Print #1, statsLK$(I, 1)
    Next

    For I = 0 To 2:
        Print #1, PK!(I)
        Print #1, statsPK$(I, 0)
        Print #1, statsPK$(I, 1)
    Next

    For I = 0 To 1:
        Print #1, LF!(I)
        Print #1, statsLF$(I)
        For I1 = 0 To 4:
            Print #1, AM!(I, I1)
            Print #1, AF!(I, I1)
        Next
    Next

    For I = 0 To 9:
        Print #1, LI!(I)
        Print #1, statsLI$(I, 0)
        Print #1, statsLI$(I, 1)
    Next

    Close 1

    Open pathCurrentStats$ + statFile$ + "S" For Output As #6
    Write #6, Ycurr%
    For I = 1 To 120: Write #6, statsO%(I): Next I
    For I = 1 To 120: Write #6, compO$(I): Next I
    For I = 1 To 120: Write #6, statsT%(I): Next I
    For I = 1 To 120: Write #6, statsH$(I): Next I
    For I = 0 To 1
        For I1 = 1 To 120: Write #6, compAP%(I, I1): Next
    Next
    For I = 1 To 120: Write #6, CRD!(I): Next I
    For I = 1 To 120: Write #6, gameSITE$(I): Next I
    Close #6

    Y = 0
    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

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
