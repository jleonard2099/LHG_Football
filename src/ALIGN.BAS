    '----------------------------------------
'             ALIGN Subroutine
'----------------------------------------
'This routine presents a menu / interface
'that assists the user with aligning 
'player names in separate stat files. 
'(ie, Road Vs. Local) This should be done 
'before merging stats, since player name 
'entry may not be consistent.
Sub ALIGN ()

    _Title "3-in-1 Football - Align Stats"

    Shared AN1$(), AN2$(), AN3$(), AN4$(), AN5$(), AN6$(), AN7$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared AR$(), intName$(), DIN$(), DRI$(), sackName$(), DSN$(), DSR$()
    Shared statsLC$(), LCN$(), LCR$(), statsLF$(), LFN$(), LFR$()
    Shared statsLI$(), LIN$(), LIR$(), statsLK$(), LKN$(), LKR$()
    Shared LPN$(), LPR$(), statsLP$(), LRN$(), LRR$(), statsLR$()
    Shared PKN$(), statsPK$(), PKR$()
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()

    Shared statsGI%(), GIN%(), GIR%(), statsGS%(), GSN%(), GSR%(), teamRatings%()

    Shared AFN!(), AFR!(), AF!(), AM!(), AMN!(), AMR!()
    Shared statsLC!(), LCN!(), LCR!(), LIN!(), LIR!()
    Shared LFR!(), statsLF!(), LFN!()
    Shared statsLP!(), LPN!(), LPR!(), statsLK!(), LKN!(), LKR!()
    Shared statsLI!(), statsLR!(), LRN!(), LRR!(), statsPK!(), PKN!(), PKR!()
    Shared statsZ2!(), statsZ!(), statsZ1!(), ZN2!(), ZR2!(), ZR!(), ZR1!()

    Do

        Call GetMergePaths (pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Call ChooseTeam(YN$, NTMS, teamNbr, team$)
            statFile$ = nameStatFile$(team$)

            Color 12, 0
            Cls
            Locate 18, 5: Color 12: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1
                
                Input #1, AR$(PX)

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

                For I = 0 To 3: Input #1, LPR!(I): Input #1, LPR$(I, 0): Input #1, LPR$(I, 1): Next

                For I = 0 To 2: Input #1, LKR!(I): Input #1, LKR$(I, 0): Input #1, LKR$(I, 1): Next

                For I = 0 To 2: Input #1, PKR!(I): Input #1, PKR$(I, 0): Input #1, PKR$(I, 1): Next

                For I = 0 To 1:

                    Input #1, LFR!(I)
                    Input #1, LFR$(I)

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

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Open pathCurrentStats$ + statFile$ For Input As #2

                    Input #2, temp$

                    For I = 0 To 9: Input #2, rbName$(I): Next I

                    For I = 0 To 5: Input #2, wrName$(I): Next I

                    For I = 0 To 3: Input #2, qbName$(I): Next I

                    For I = 0 To 2: Input #2, krName$(I): Next I

                    For I = 0 To 2: Input #2, prName$(I): Next I

                    Input #2, punterName$(0)

                    For I = 0 To 1: Input #2, pkName$(I): Next I

                    For J = 0 To 13
                        For K = 0 To 17: Input #2, statsZ2!(J, K): Next K
                    Next J

                    For L = 0 To 38: Input #2, statsZ!(L): Next L

                    For L = 0 To 38: Input #2, statsZ1!(L): Next L

                    For J = 0 To 9: 
                        Input #2, intName$(J)
                        For J1 = 0 To 2: 
                            Input #2, statsGI%(J, J1)
                        Next
                    Next

                    For J = 0 To 14: 
                        Input #2, sackName$(J)
                        For J1 = 0 To 1: 
                            Input #2, statsGS%(J, J1)
                        Next
                    Next

                    For I = 0 To 9: 
                        Input #2, statsLR!(I)
                        Input #2, statsLR$(I, 0)
                        Input #2, statsLR$(I, 1)
                    Next

                    For I = 0 To 15: 
                        Input #2, statsLC!(I)
                        Input #2, statsLC$(I, 0)
                        Input #2, statsLC$(I, 1)
                    Next

                    For I = 0 To 3: 
                        Input #2, statsLP!(I)
                        Input #2, statsLP$(I, 0)
                        Input #2, statsLP$(I, 1)
                    Next

                    For I = 0 To 2: 
                        Input #2, statsLK!(I)
                        Input #2, statsLK$(I, 0)
                        Input #2, statsLK$(I, 1)
                    Next

                    For I = 0 To 2: 
                        Input #2, statsPK!(I)
                        Input #2, statsPK$(I, 0)
                        Input #2, statsPK$(I, 1)
                    Next

                    For I = 0 To 1: 
                        Input #2, statsLF!(I)
                        Input #2, statsLF$(I)
                        For I1 = 0 To 4: 
                            Input #2, AM!(I, I1)
                            Input #2, AF!(I, I1)
                        Next
                    Next

                    For I = 0 To 9: 
                        Input #2, statsLI!(I)
                        Input #2, statsLI$(I, 0)
                        Input #2, statsLI$(I, 1)
                    Next

                    Close #2

                    Call AlignStats(pathRoadStats$, statFile$)
                    DUMMY$ = "OK"

                Else

                    Cls

                    Locate 2, 10: Color 7
                    Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10
                    Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                End If 'Done checking if current file exists

            Else

                Cls

                Locate 2, 10: Color 7
                Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                Print
                Locate , 10
                Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

            End If 'Done checking if road file exists

        End If 'Done checking for change / ESC

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         AlignStats Subroutine
'----------------------------------------
'This routine presents the user with a 
'summary of the statistics in each file.
'It then performs the alignment before 
'presenting the user a chance to review 
'the results.
Sub AlignStats (pathRoadStats$, statFile$)

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared AN1$(), AN2$(), AN3$(), AN4$(), AN5$(), AN6$(), AN7$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared intName$(), DIN$(), DRI$(), sackName$(), DSN$(), DSR$()
    Shared LCN$(), LCR$(), LFN$(), LFR$()
    Shared statsLK$(), LKN$(), LKR$(), LPN$(), LPR$(), statsLP$()
    Shared LRN$(), LRR$(), statsLR$(), PKN$(), statsPK$(), PKR$()
    Shared statsGI%(), GIN%(), GIR%(), statsGS%(), GSN%(), GSR%()

    Shared AFN!(), AFR!(), AF!(), AM!(), AMN!(), AMR!()
    Shared LCN!(), LCR!(), LFR!(), LFN!()
    Shared LPN!(), LPR!(), LKN!(), LKR!()
    Shared LRN!(), LRR!(), PKN!(), PKR!()
    Shared ZN2!(), ZR2!()

    Cls
    Locate 1, 34
    Color 12: Print "STAT ROSTERS"

    Print
    Color 15: Print "CURRENT  ";
    Color 14: Print "- RUSHERS -";
    Color 7: Print " ROAD";

    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- RECEIVERS -";
    Color 7: Print " ROAD"

    For I = 0 To 9:
        Locate I + 4
        Color 12: Print I; " ";
        Color 15: Print rbName$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AR1$(I)
    Next

    For I = 0 To 5:
        Locate I + 4, 40
        Color 12: Print I; " ";
        Color 15: Print wrName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR2$(I):
    Next

    Locate 11, 4
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- KICK RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2
        Color 12
        Locate I + 12, 40: Print I; " ";
        Color 15: Print krName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR4$(I)
    Next

    Locate 15
    Color 15: Print "CURRENT  ";
    Color 14: Print "-   QB'S  -";
    Color 7: Print " ROAD"

    For I = 0 To 3:
        Color 12
        Locate I + 16: Print I; " ";
        Color 15: Print qbName$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AR3$(I)
    Next

    Locate 16, 40
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- PUNT RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Color 12
        Locate I + 17, 40: Print I; " ";
        Color 15: Print prName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR5$(I)
    Next

    Color 11
    Locate 23: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls

    Locate 1, 32
    Color 12: Print "STAT ROSTERS"

    Print

    Color 15: Print "CURRENT  ";
    Color 14: Print "- PUNTERS -";
    Color 7: Print " ROAD";

    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "-  KICKERS  -";
    Color 7: Print " ROAD"

    Color 12
    Locate 4: Print "0 ";
    Color 15: Print punterName$(0);
    Color 12: Print Tab(20); "0 ";
    Color 7: Print AR6$(0)

    For I = 0 To 1:
        Color 12
        Locate I + 4, 40: Print I; " ";
        Color 15: Print pkName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR7$(I):
    Next

    Color 15
    Locate 7: Print "CURRENT  ";
    Color 14: Print "-INTERCEPTORS-";
    Color 7: Print " ROAD";

    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "-  SACKERS  -";
    Color 7: Print " ROAD"

    For I = 0 To 9:
        Color 12
        Locate I + 8: Print Using "##"; I;
        Print " ";
        Color 15: Print intName$(I);
        Color 12: Print Tab(20); Using "##"; I;
        Print " ";
        Color 7: Print DRI$(I)
    Next

    For I = 0 To 14:
        Color 12
        Locate I + 8, 40: Print Using "##"; I;
        Print " ";
        Color 15: Print sackName$(I);
        Color 12: Print Tab(63); Using "##"; I;
        Print " ";
        Color 7: Print DSR$(I)
    Next

    Color 11
    Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 22: Print "ALIGNING FILES . . .             "

    N = 0

    Do
        For J = 0 To 9

            If rbName$(N) = AR1$(J) Then

                AN1$(N) = AR1$(J)
                LRN!(N) = LRR!(J)
                LCN!(N) = LCR!(J)

                For I = 0 To 1
                    LRN$(N, I) = LRR$(J, I)
                    LCN$(N, I) = LCR$(J, I)
                Next

                For K = 0 To 5
                    ZN2!(N, K) = ZR2!(J, K)
                Next K

            End If

        Next J

        N = N + 1

        '412 If N = 10 GoTo 414 Else GoTo 405
    Loop Until N = 10

    N = 0

    Do

        For J = 0 To 3

            If qbName$(N) = AR3$(J) Then
                AN3$(N) = AR3$(J)

                For K = 0 To 2
                    ZN2!(N + 10, K) = ZR2!(J + 10, K)
                Next K
            End If
        Next J

        N = N + 1

    Loop Until N = 4

    N = 0

    Do
        For J = 0 To 5

            If wrName$(N) = AR2$(J) Then

                AN2$(N) = AR2$(J)
                LCN!(N + 10) = LCR!(J + 10)
                LCN$(N + 10, 0) = LCR$(J + 10, 0)
                LCN$(N + 10, 1) = LCR$(J + 10, 1)

                For K = 6 To 8
                    ZN2!(N, K) = ZR2!(J, K)
                Next K

            End If
        Next J

        N = N + 1

    Loop Until N = 6

    N = 0

    Do
        For J = 0 To 3

            If qbName$(N) = AR3$(J) Then

                AN3$(N) = AR3$(J)
                LPN!(N) = LPR!(J)
                LPN$(N, 0) = LPR$(J, 0)
                LPN$(N, 1) = LPR$(J, 1)

                For K = 9 To 13:
                    ZN2!(N, K) = ZR2!(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 4

    N = 0

    Do

        For J = 0 To 2

            If krName$(N) = AR4$(J) Then

                AN4$(N) = AR4$(J)
                LKN!(N) = LKR!(J)
                LKN$(N, 0) = LKR$(J, 0)
                LKN$(N, 1) = LKR$(J, 1)

                For K = 14 To 16
                    ZN2!(N, K) = ZR2!(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 3

    N = 0

    Do

        For J = 0 To 2

            If prName$(N) = AR5$(J) Then

                AN5$(N) = AR5$(J)
                PKN!(N) = PKR!(J)
                PKN$(N, 1) = PKR$(J, 1)
                PKN$(N, 1) = PKR$(J, 1)

                For K = 14 To 16:
                    ZN2!(N + 3, K) = ZR2!(J + 3, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 3

    If punterName$(0) = AR6$(0) Then
        AN6$(0) = AR6$(0)
    End If

    N = 0

    Do

        For J = 0 To 1

            If pkName$(N) = AR7$(J) Then

                AN7$(N) = AR7$(J)
                LFN!(N) = LFR!(J)
                LFN$(N) = LFR$(J)

                If N = J Then
                    For L = 3 To 6: ZN2!(L, 17) = ZR2!(L, 17): Next L
                    For L = 7 To 10: ZN2!(L, 17) = ZR2!(L, 17): Next L
                Else
                    For L = 3 To 6: ZN2!(L, 17) = ZR2!(L + 4, 17): Next L
                    For L = 7 To 10: ZN2!(L, 17) = ZR2!(L - 4, 17): Next L
                End If

                For J1 = 0 To 4:
                    AMN!(N, J1) = AMR!(J, J1)
                    AFN!(N, J1) = AFR!(J, J1)
                Next J1

            End If

        Next J

        N = N + 1

    Loop Until N = 2

    N = 0

    Do

        For J = 0 To 9

            If intName$(N) = DRI$(J) Then

                DIN$(N) = DRI$(J)
                For J1 = 0 To 2
                    GIN%(J, J1) = GIR%(J, J1)
                Next

            End If

        Next J

        N = N + 1

    Loop Until N = 10

    N = 0

    Do

        For J = 0 To 14

            If sackName$(N) = DSR$(J) Then

                DSN$(N) = DSR$(J)
                For J1 = 0 To 1
                    GSN%(J, J1) = GSR%(J, J1)
                Next

            End If

        Next J

        N = N + 1

    Loop Until N = 15

    Locate 20: Print String$(26, " ")

    SEQ = 0

    For I = 0 To 9
        If AN1$(I) <> rbName$(I) Then
            SEQ = SEQ + 1
        End If
    Next

    For I = 0 To 5
        If AN2$(I) <> wrName$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 3
        If AN3$(I) <> qbName$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 2
        If AN4$(I) <> krName$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 2
        If AN5$(I) <> prName$(I) Then SEQ = SEQ + 1
    Next

    If AN6$(I) <> punterName$(I) Then
        SEQ = SEQ + 1
    End If
    For I = 0 To 1
        If AN7$(I) <> pkName$(I) Then SEQ = SEQ + 1
    Next

    If SEQ = 0 Then
        Call ReviewAndSaveAlignment(pathRoadStats$, statFile$)
    Else
        Color 14
        Locate 20
        Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
        Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
        Color 13
        Locate 23, 30
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    End If

End Sub


'----------------------------------------
'    ReviewAndSaveAlignment Subroutine
'----------------------------------------
'This routine displays the results of the 
'player name alignment, then gives the end
'user a chance to actually save the results 
Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared AN1$(), AN2$(), AN3$(), AN4$(), AN5$(), AN6$(), AN7$()
    Shared AR$(), intName$(), DIN$(), DRI$(), sackName$(), DSN$()
    Shared LCN$(), LCR$(), LFN$(), LFR$()
    Shared LIN$(), LKN$(), LPN$(), LRN$(), PKN$()
    Shared GIN%(), GIR%(), GSN%()

    Shared AFN!(), AMN!(), LCN!(), LIN!(), LFN!()
    Shared LPN!(), LKN!(), LRN!(), PKN!()
    Shared ZN2!(), ZR!(), ZR1!()

    Cls

    Color 12
    Locate 1, 34
    Print "STAT ROSTERS"
    Print
    
    Color 15: Print "CURRENT  ";
    Color 14: Print "- RUSHERS -";
    Color 7: Print " ROAD";

    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- RECEIVERS -";
    Color 7: Print " ROAD"

    For I = 0 To 9:
        Color 12
        Locate I + 4: Print I; " ";
        Color 15: Print rbName$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AN1$(I):
    Next

    For I = 0 To 5:
        Color 12
        Locate I + 4, 40: Print I; " ";
        Color 15: Print wrName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN2$(I)
    Next

    Color 15
    Locate 11, 40: Print Tab(45); "CURRENT  ";
    Color 14: Print "- KICK RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Color 12
        Locate I + 12, 40: Print I; " ";
        Color 15: Print krName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN4$(I)
    Next

    Color 15
    Locate 15: Print "CURRENT  ";
    Color 14: Print "-   QB'S  -";
    Color 7: Print " ROAD"

    For I = 0 To 3:
        Color 12
        Locate I + 16: Print I; " ";
        Color 15: Print qbName$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AN3$(I)
    Next


    Color 15
    Locate 16, 40: Print Tab(45); "CURRENT  ";
    Color 14: Print "- PUNT RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Color 12
        Locate I + 17, 40: Print I; " ";
        Color 15: Print prName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN5$(I)
    Next

    Color 11
    Locate 23: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls

    Locate 1, 32

    Color 12: Print "STAT ROSTERS"

    Print

    Color 15: Print "CURRENT  ";
    Color 14: Print "- PUNTERS -";
    Color 7: Print " ROAD";

    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "-  KICKERS  -";
    Color 7: Print " ROAD"

    Color 12
    Locate 4: Print "0 ";
    Color 15: Print punterName$(0);
    Color 12: Print Tab(20); "0 ";
    Color 7: Print AN6$(0)

    For I = 0 To 1
        Color 12
        Locate I + 4, 40: Print I; " ";
        Color 15: Print pkName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN7$(I)
    Next

    Color 15
    Locate 7: Print "CURRENT  ";
    Color 14: Print "-INTERCEPTORS-";
    Color 7: Print " ROAD";
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "-  SACKERS  -";
    Color 7: Print " ROAD"

    For I = 0 To 9:
        Color 12
        Locate I + 8: Print Using "##"; I;
        Print " ";
        Color 15: Print intName$(I);
        Color 12: Print Tab(20); Using "##"; I;
        Print " ";
        Color 7: Print DIN$(I):
    Next

    For I = 0 To 14:
        Color 12
        Locate I + 8, 40: Print Using "##"; I;
        Print " ";
        Color 15: Print sackName$(I);
        Color 12: Print Tab(63); Using "##"; I;
        Print " ";
        Color 7: Print DSN$(I):
    Next

    Color 11
    Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Locate 22: Print "ALIGNING FILES . . .             "

    Open pathRoadStats$ + statFile$ For Output As #1

    Write #1, AR$(0)

    For I = 0 To 9: Write #1, AN1$(I): Next I

    For I = 0 To 5: Write #1, AN2$(I): Next I

    For I = 0 To 3: Write #1, AN3$(I): Next I

    For I = 0 To 2: Write #1, AN4$(I): Next I

    For I = 0 To 2: Write #1, AN5$(I): Next I

    Write #1, AN6$(0)

    For I = 0 To 1: Write #1, AN7$(I): Next I

    For J = 0 To 13
        For K = 0 To 17:
            Write #1, ZN2!(J, K)
        Next K
    Next J

    For L = 0 To 38: Write #1, ZR!(L): Next L

    For L = 0 To 38: Write #1, ZR1!(L): Next L

    For J = 0 To 9:

        Write #1, DIN$(J)

        For J1 = 0 To 2:
            Write #1, GIN%(J, J1)
        Next

    Next

    For J = 0 To 14

        Write #1, DSN$(J)

        For J1 = 0 To 1
            Write #1, GSN%(J, J1)
        Next

    Next

    For I = 0 To 9:
        Write #1, LRN!(I)
        Write #1, LRN$(I, 0)
        Write #1, LRN$(I, 1)
    Next

    For I = 0 To 15
        Write #1, LCN!(I)
        Write #1, LCN$(I, 0)
        Write #1, LCN$(I, 1)
    Next

    For I = 0 To 3:
        Write #1, LPN!(I)
        Write #1, LPN$(I, 0)
        Write #1, LPN$(I, 1)
    Next

    For I = 0 To 2:
        Write #1, LKN!(I)
        Write #1, LKN$(I, 0)
        Write #1, LKN$(I, 1)
    Next

    For I = 0 To 2:
        Write #1, PKN!(I)
        Write #1, PKN$(I, 0)
        Write #1, PKN$(I, 1)
    Next

    For I = 0 To 1:

        Write #1, LFN!(I)
        Write #1, LFN$(I)

        For I1 = 0 To 4:
            Write #1, AMN!(I, I1)
            Write #1, AFN!(I, I1)
        Next

    Next

    For I = 0 To 9:
        Write #1, LIN!(I)
        Write #1, LIN$(I, 0)
        Write #1, LIN$(I, 1)
    Next

    Close #1

    Cls

    Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"
    Color 12
    Locate 12, 20
    Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub
