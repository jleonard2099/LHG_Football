Sub ALIGN ()

    _Title "3-in-1 Football - Align Stats"

    Shared AN1$(), AN2$(), AN3$(), AN4$(), AN5$(), AN6$(), AN7$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared AR$(), statsDI$(), DIN$(), DRI$(), statsDS$(), DSN$(), DSR$()
    Shared LC$(), LCN$(), LCR$(), statsLF$(), LFN$(), LFR$()
    Shared statsLI$(), LIN$(), LIR$(), statsLK$(), LKN$(), LKR$()
    Shared LPN$(), LPR$(), statsLP$(), LRN$(), LRR$(), LR$()
    Shared PKN$(), statsPK$(), PKR$()
    Shared statsA$(), statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()

    Shared statsGI%(), GIN%(), GIR%(), statsGS%(), GSN%(), GSR%(), statsPA%()

    Shared AFN!(), AFR!(), AF!(), AM!(), AMN!(), AMR!()
    Shared LC!(), LCN!(), LCR!(), LIN!(), LIR!()
    Shared LFR!(), LF!(), LFN!()
    Shared LP!(), LPN!(), LPR!(), LK!(), LKN!(), LKR!()
    Shared LI!(), LR!(), LRN!(), LRR!(), PK!(), PKN!(), PKR!()
    Shared Z2!(), Z!(), Z1!(), ZN2!(), ZR2!(), ZR!(), ZR1!()

    Do

        Call GetMergePaths (pathCurrentStats$, pathRoadStats$, I$)

        If UCase$(I$) = "N" And I$ <> Chr$(27) Then

            Call GetDiskID_ALIGN(A$)

            B$ = A$

            For XX = 4 To 11
                If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
            Next XX

            For XX = 12 To 14
                If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
            Next XX

            QQ = 8

            For XX = 11 To 4 Step -1
                If Mid$(B$, XX, 1) = "_" Then
                    QQ = XX - 4
                Else
                    Exit For
                End If
            Next XX

            statFile$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

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

                    Open pathCurrentStats$ + statFile$ For Input As #1

                    Input #1, statsA$(PX)

                    For I = 0 To 9: Input #1, statsA1$(I): Next I
                    For I = 0 To 5: Input #1, statsA2$(I): Next I
                    For I = 0 To 3: Input #1, statsA3$(I): Next I
                    For I = 0 To 2: Input #1, statsA4$(I): Next I
                    For I = 0 To 2: Input #1, statsA5$(I): Next I

                    Input #1, statsA6$(0)
                    For I = 0 To 1: Input #1, statsA7$(I): Next I

                    For J = 0 To 13
                        For K = 0 To 17: Input #1, Z2!(J, K): Next K
                    Next J

                    For L = 0 To 38: Input #1, Z!(L): Next L
                    For L = 0 To 38: Input #1, Z1!(L): Next L
                    For J = 0 To 9: Input #1, statsDI$(J): For J1 = 0 To 2: Input #1, statsGI%(J, J1): Next: Next
                    For J = 0 To 14: Input #1, statsDS$(J): For J1 = 0 To 1: Input #1, statsGS%(J, J1): Next: Next
                    For I = 0 To 9: Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
                    For I = 0 To 15: Input #1, LC!(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
                    For I = 0 To 3: Input #1, LP!(I): Input #1, statsLP$(I, 0): Input #1, statsLP$(I, 1): Next
                    For I = 0 To 2: Input #1, LK!(I): Input #1, statsLK$(I, 0): Input #1, statsLK$(I, 1): Next
                    For I = 0 To 2: Input #1, PK!(I): Input #1, statsPK$(I, 0): Input #1, statsPK$(I, 1): Next
                    For I = 0 To 1: Input #1, LF!(I): Input #1, statsLF$(I): For I1 = 0 To 4: Input #1, AM!(I, I1): Input #1, AF!(I, I1): Next: Next
                    For I = 0 To 9: Input #1, LI!(I): Input #1, statsLI$(I, 0): Input #1, statsLI$(I, 1): Next

                    Close #1

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
'       AlignStats Subroutine
'----------------------------------------
'...explanation...
Sub AlignStats (pathRoadStats$, statFile$)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared AN1$(), AN2$(), AN3$(), AN4$(), AN5$(), AN6$(), AN7$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared statsDI$(), DIN$(), DRI$(), statsDS$(), DSN$(), DSR$()
    Shared LCN$(), LCR$(), LFN$(), LFR$()
    Shared statsLK$(), LKN$(), LKR$(), LPN$(), LPR$(), statsLP$()
    Shared LRN$(), LRR$(), LR$(), PKN$(), statsPK$(), PKR$()
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
        Locate I + 4: Color 12: Print I; " ";
        Color 15: Print statsA1$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AR1$(I)
    Next

    For I = 0 To 5:
        Locate I + 4, 40
        Color 12: Print I; " ";
        Color 15: Print statsA2$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR2$(I):
    Next

    Locate 11, 4
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- KICK RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2
        Locate I + 12, 40: Color 12: Print I; " ";
        Color 15: Print statsA4$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR4$(I)
    Next

    Locate 15
    Color 15: Print "CURRENT  ";
    Color 14: Print "-   QB'S  -";
    Color 7: Print " ROAD"

    For I = 0 To 3:
        Locate I + 16: Color 12: Print I; " ";
        Color 15: Print statsA3$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AR3$(I)
    Next

    Locate 16, 40
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- PUNT RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Locate I + 17, 40: Color 12: Print I; " ";
        Color 15: Print statsA5$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR5$(I)
    Next

    Color 15
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

    Locate 4: Color 12: Print "0 ";
    Color 15: Print statsA6$(0);
    Color 12: Print Tab(20); "0 ";
    Color 7: Print AR6$(0)

    For I = 0 To 1:
        Locate I + 4, 40: Color 12: Print I; " ";
        Color 15: Print statsA7$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AR7$(I):
    Next

    Locate 7: Color 15: Print "CURRENT  ";
    Color 14: Print "-INTERCEPTORS-";
    Color 7: Print " ROAD";
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "-  SACKERS  -";
    Color 7: Print " ROAD"

    For I = 0 To 9:
        Locate I + 8: Color 12: Print Using "##"; I;
        Print " ";: Color 15: Print statsDI$(I);
        Color 12: Print Tab(20); Using "##"; I;
        Print " ";: Color 7: Print DRI$(I)
    Next

    For I = 0 To 14:
        Locate I + 8, 40: Color 12: Print Using "##"; I;
        Print " ";: Color 15: Print statsDS$(I);
        Color 12: Print Tab(63); Using "##"; I;
        Print " ";: Color 7: Print DSR$(I)
    Next

    Color 11
	Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Color 11
	Locate 22: Print "ALIGNING FILES . . .             "

    N = 0

    Do
        For J = 0 To 9

            If statsA1$(N) = AR1$(J) Then

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

            If statsA3$(N) = AR3$(J) Then
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

            If statsA2$(N) = AR2$(J) Then

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

            If statsA3$(N) = AR3$(J) Then

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

            If statsA4$(N) = AR4$(J) Then
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

            If statsA5$(N) = AR5$(J) Then

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

    If statsA6$(0) = AR6$(0) Then
        AN6$(0) = AR6$(0)
    End If

    N = 0

    Do
        For J = 0 To 1

            If statsA7$(N) = AR7$(J) Then

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

            If statsDI$(N) = DRI$(J) Then

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

            If statsDS$(N) = DSR$(J) Then

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
        If AN1$(I) <> statsA1$(I) Then
            SEQ = SEQ + 1
        End If
    Next

    For I = 0 To 5
        If AN2$(I) <> statsA2$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 3
        If AN3$(I) <> statsA3$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 2
        If AN4$(I) <> statsA4$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 2
        If AN5$(I) <> statsA5$(I) Then SEQ = SEQ + 1
    Next

    If AN6$(I) <> statsA6$(I) Then
        SEQ = SEQ + 1
    End If
    For I = 0 To 1
        If AN7$(I) <> statsA7$(I) Then SEQ = SEQ + 1
    Next

    If SEQ = 0 Then
        Call ReviewAndSaveAlignment(pathRoadStats$, statFile$)
    Else

		Locate 20
		Color 14
		Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
		Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
		Locate 23, 30
		Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    End If

End Sub


'----------------------------------------
'    ReviewAndSaveAlignment Subroutine
'----------------------------------------
'...explanation...
Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)

    Shared statsA1$(), statsA2$(), statsA3$(), statsA4$(), statsA5$(), statsA6$(), statsA7$()
    Shared AN1$(), AN2$(), AN3$(), AN4$(), AN5$(), AN6$(), AN7$()
    Shared AR$(), statsDI$(), DIN$(), DRI$(), statsDS$(), DSN$()
    Shared LCN$(), LCR$(), LFN$(), LFR$()
    Shared LIN$(), LKN$(), LPN$(), LRN$(), PKN$()
    Shared GIN%(), GIR%(), GSN%()

    Shared AFN!(), AMN!(), LCN!(), LIN!(), LFN!()
    Shared LPN!(), LKN!(), LRN!(), PKN!()
    Shared ZN2!(), ZR!(), ZR1!()

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
        Locate I + 4: Color 12: Print I; " ";
        Color 15: Print statsA1$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AN1$(I):
    Next
    For I = 0 To 5:
        Locate I + 4, 40: Color 12: Print I; " ";
        Color 15: Print statsA2$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN2$(I)
    Next
    Locate 11, 40: Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- KICK RET. -";
    Color 7: Print " ROAD"
    For I = 0 To 2:
        Locate I + 12, 40: Color 12: Print I; " ";
        Color 15: Print statsA4$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN4$(I)
    Next

    Locate 15: Color 15: Print "CURRENT  ";
    Color 14: Print "-   QB'S  -";
    Color 7: Print " ROAD"

    For I = 0 To 3:
        Locate I + 16: Color 12: Print I; " ";
        Color 15: Print statsA3$(I);
        Color 12: Print Tab(20); I; " ";
        Color 7: Print AN3$(I)
    Next

    Locate 16, 40: Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "- PUNT RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Locate I + 17, 40: Color 12: Print I; " ";
        Color 15: Print statsA5$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN5$(I)
    Next

    Color 15: Locate 23: Print "HIT ANY KEY TO CONTINUE"

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

    Locate 4: Color 12: Print "0 ";
    Color 15: Print statsA6$(0);
    Color 12: Print Tab(20); "0 ";
    Color 7: Print AN6$(0)

    For I = 0 To 1
        Locate I + 4, 40: Color 12: Print I; " ";
        Color 15: Print statsA7$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print AN7$(I)
    Next

    Locate 7: Color 15: Print "CURRENT  ";
    Color 14: Print "-INTERCEPTORS-";
    Color 7: Print " ROAD";
    Color 15: Print Tab(45); "CURRENT  ";
    Color 14: Print "-  SACKERS  -";
    Color 7: Print " ROAD"

    For I = 0 To 9:
        Locate I + 8: Color 12: Print Using "##"; I;
        Print " ";: Color 15: Print statsDI$(I);
        Color 12: Print Tab(20); Using "##"; I;
        Print " ";: Color 7: Print DIN$(I):
    Next
    For I = 0 To 14:
        Locate I + 8, 40: Color 12: Print Using "##"; I;
        Print " ";: Color 15: Print statsDS$(I);
        Color 12: Print Tab(63); Using "##"; I;
        Print " ";: Color 7: Print DSN$(I):
    Next

    Color 11
	Locate 22: Line Input "HIT ENTER TO ALIGN"; DUMMY$
    Color 11
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
    Print
    Locate 12, 20
    Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub


'----------------------------------------
'       GetDiskID_ALIGN  Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskID_ALIGN (A$)

    Shared statsPA%()

    Do

        Color 15, 0
        Cls
        JJ = 6
        SN = 1
        Locate 6, 8
        Color 15, 4

        'Print box top
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";

        'Print box sides
        For II = 1 To JJ
            Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
        Next

        'Print box bottom
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";

        Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Print
        Locate , 12: Print "ESC TO RETURN TO MAIN MENU"
        I$ = GetKeyPress$

        If I$ <> Chr$(27) Then

            If I$ = Chr$(13) Then
                Call SelectTeam_ALIGN(YN$, numberTeams, teamIdx)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If NY$ = "" Then
                    Call SelectTeam_ALIGN(NY$, numberTeams, teamIdx)
                Else
                    YN$ = NY$
                    Erase teamNames$

                    numberTeams = 0

                    Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
                    Y = LOF(1)
                    Field #1, 15 As Q$(0), 640 As X$

                    For I1 = 1 To Y / 934
                        Get #1, I1

                        For I2 = 1 To 15
                            If Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                        Next I2

                        If nullTeam <> 1 Then
                            numberTeams = numberTeams + 1
                            teamNames$(numberTeams) = Q$(0)
                            teamIndex%(numberTeams) = I1
                        End If
                    Next I1

                    Close 1

                    Call SelectTeam_ALIGN(YN$, numberTeams, teamIdx)
                End If

            End If

            A$ = RTrim$(Q$(1))

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
                Color statsPA%(2), statsPA%(1): Print A$

                Color 15, 4

                Do
                    Locate 3, 33: Print "ANY CHANGE (YN)";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                Color 15, 0

            End If
        Else
            statFile$ = "END"
        End If

    Loop Until UCase$(I$) = "N" Or statFile$ = "END"

End Sub


'----------------------------------------
'       SelectTeam_ALIGN Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_ALIGN (YN$, numberTeams, teamIdx)

    Shared statsPA%()

    Selection% = 1
    Count% = numberTeams
    Start% = 1
    FileFGColr% = 15: FileBGColr% = 4
    HiLiteFGColr% = 0: HiLiteBGColr% = 7
    Rows% = 20
    Columns% = 4
    Spacing% = 6
    Row% = 4
    Column% = 1
    Color , 4: Cls: Locate , 33: Color 14
    Print "TEAM SELECTION": Color 15
    Locate 25, 1: Color 14: Print "ESC";: Color 15
    Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14
    Print "ENTER";: Color 15: Print "-SELECTS TEAM";
    _MouseShow "DEFAULT"
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIdx = teamIndex%(Selection%)
        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)
        Get #1, teamIdx
        For I1 = 0 To 4: statsPA%(I1) = CVI(Q$(I1 + 11)): Next I1
        Close 1

    Else

        BO% = 1

    End If

End Sub
