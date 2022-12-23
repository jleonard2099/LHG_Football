Sub MERGE ()

    _Title "3-in-1 Football - Merge Stats"

    ReDim menuChoices$(1 To 4)

    TMR = 6420
    NB = -1
    NC = -1
    ND = -1
    NE = -1
    NF = -1
    NG = -1
    NH = -1
    NI = -1
    NJ = 0
    NK = 0
    D = 0
    JX = 0
    VV = 0

    Do

        Cls

        Call TITLE_UTILS

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
        RN = Selection%

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call MergeStats
                Case 2: Call ALIGN
                Case 3: Cls
            End Select
        End If

    Loop Until Selection% = 3 Or ExitCode% = 27

End Sub

'----------------------------------------
'       MergeStats Subroutine
'----------------------------------------
'...explanation...
Sub MergeStats ()

    Shared A1$(), A2$(), A3$(), A4$(), A5$(), A6$(), A7$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared A$(), AR$(), DI$(), DRI$(), DS$(), DSR$()
    Shared H$(), HR$()
    Shared LC$(), LCR$(), LF$(), LFR$()
    Shared LI$(), LIR$(), LK$(), LKR$()
    Shared LP$(), LPR$(), LR$(), LRR$()
    Shared O$(), PK$(), PKR$()
    Shared SITE$(), SITER$()

    Shared AF!(), AFR!(), AMR!(), AM!()
    Shared CRD!(), CRDR!()
    Shared LC!(), LCR!(), LF!(), LFR!()
    Shared LI!(), LIR!(), LK!(), LKR!(), LP!(), LPR!()
    Shared LR!(), LRR!(), PK!(), PKR!()
    Shared Z!(), Z2!(), Z1!()
    Shared ZR!(), ZR1!(), ZR2!()

    Shared AP%(), APR%()
    Shared GI%(), GIR%(), GS%(), GSR%()
    Shared O%(), T%(), TR%()

    Dim ORD$(120)
    Dim ORD%(120)

    Do

        Color 12, 0
        Cls
        Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"
        Color 15
        Locate 4, 5: Print "EXAMPLE - A:"
        Locate , 5: Print "          C:\HAFFNER"
        Locate , 5: Print "          C:\HAFFNER\1980"
        Locate , 5: Print "          D:\FOOTBALL"

		For J = 10 To 13: Locate J
			Print Space$(60)
		Next

        Do
			Color 10
            Locate 10, 5: Line Input "ROAD STATS:     "; pathRoadStats$
            Locate , 5
            Locate , 5: Line Input "CURRENT STATS:  "; pathCurrentStats$
            Color 14

            Locate 13, 5: Print "ANY CHANGE (Y/N)"
            I$ = GetKeyPress$
			
        Loop Until UCase$(I$) = "N" Or I$ = Chr$(27)

		If I$ <> Chr$(27) Then
		
			Call GetDiskID_MERGE

			B$ = A$

			For XX = 4 To 11:
				If Mid$(B$, XX, 1) = " " Or Mid$(B$, XX, 1) = "." Then Mid$(B$, XX, 1) = "_"
			Next XX

			For XX = 12 To 14:
				If Mid$(B$, XX, 2) = "ST" Then Mid$(B$, 9, 1) = "_": Mid$(B$, 10, 2) = "ST"
			Next XX

			QQ = 8
			For XX = 11 To 4 Step -1:
				If Mid$(B$, XX, 1) = "_" Then
					QQ = XX - 4
				Else
					Exit For
				End If
			Next XX

			statFile$ = Mid$(B$, 4, QQ) + "." + Left$(B$, 2)

			Cls

			Locate 10, 15: Print "ROAD STATS ARE ON DRIVE "; pathRoadStats$
			Locate , 15: Print "MASTER STATS ARE ON DRIVE "; pathCurrentStats$
			Locate , 15: Print "TEAM TO MERGE STATS FOR IS "; A$

			Do
				Locate 14, 15: Print "ANY CHANGE (YN)"
				J$ = GetKeyPress$
			Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

			If UCase$(J$) = "N" Then

				If Not _FileExists(pathRoadStats$ + "\" + statFile$) Then
					Cls
					Color 7
					Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
					Print
					Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
				Else

					Open pathRoadStats$ + "\" + statFile$ For Input As #1
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

					For I = 0 To 9: Input #1, LIR!(I): Input #1, LIR$(I, 0)
					Input #1, LIR$(I, 1): Next
					Close #1

					Open pathRoadStats$ + "\" + statFile$ + "S" For Input As #4
					Input #4, YR%
					For I = 1 To 120: Input #4, ORD%(I): Next I
					For I = 1 To 120: Input #4, ORD$(I): Next I
					For I = 1 To 120: Input #4, TR%(I): Next I
					For I = 1 To 120: Input #4, HR$(I): Next I
					For I = 0 To 1
					For I1 = 1 To 120: Input #4, APR%(I, I1): Next: Next
					For I = 1 To 120: Input #4, CRDR!(I): Next I
					For I = 1 To 120: Input #4, SITER$(I): Next I
					Close #4

					If Not _FileExists(pathCurrentStats$ + "\" + statFile$) Then
						Cls
						Color 7
						Locate 2, 10: Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
						Print
						Locate , 10: Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$
					Else

						'Open road stat file and read to arrays
						Open pathCurrentStats$ + "\" + statFile$ For Input As #1
						Input #1, A$(PX)
						For I = 0 To 9: Input #1, A1$(I): Next I
						For I = 0 To 5: Input #1, A2$(I): Next I
						For I = 0 To 3: Input #1, A3$(I): Next I
						For I = 0 To 2: Input #1, A4$(I): Next I
						For I = 0 To 2: Input #1, A5$(I): Next I

						Input #1, A6$(0)

						For I = 0 To 1: Input #1, A7$(I): Next I

						For J = 0 To 13
							For K = 0 To 17: Input #1, Z2!(J, K): Next K
						Next J

						For L = 0 To 38: Input #1, Z!(L): Next L
						For L = 0 To 38: Input #1, Z1!(L): Next L
						For J = 0 To 9: Input #1, DI$(J): For J1 = 0 To 2: Input #1, GI%(J, J1): Next: Next
						For J = 0 To 14: Input #1, DS$(J): For J1 = 0 To 1: Input #1, GS%(J, J1): Next: Next
						For I = 0 To 9: Input #1, LR!(I): Input #1, LR$(I, 0): Input #1, LR$(I, 1): Next
						For I = 0 To 15: Input #1, LC!(I): Input #1, LC$(I, 0): Input #1, LC$(I, 1): Next
						For I = 0 To 3: Input #1, LP!(I): Input #1, LP$(I, 0): Input #1, LP$(I, 1): Next
						For I = 0 To 2: Input #1, LK!(I): Input #1, LK$(I, 0): Input #1, LK$(I, 1): Next
						For I = 0 To 2: Input #1, PK!(I): Input #1, PK$(I, 0): Input #1, PK$(I, 1): Next
						For I = 0 To 1: Input #1, LF!(I): Input #1, LF$(I): For I1 = 0 To 4: Input #1, AM!(I, I1): Input #1, AF!(I, I1): Next: Next
						For I = 0 To 9: Input #1, LI!(I): Input #1, LI$(I, 0): Input #1, LI$(I, 1): Next

						Close 1

						'Open current stat file and read to arrays
						Open pathCurrentStats$ + "\" + statFile$ + "S" For Input As #5
						Input #5, Y%
						For I = 1 To 120: Input #5, O%(I): Next I
						For I = 1 To 120: Input #5, O$(I): Next I
						For I = 1 To 120: Input #5, T%(I): Next I
						For I = 1 To 120: Input #5, H$(I): Next I
						For I = 0 To 1
						For I1 = 1 To 120: Input #5, AP%(I, I1): Next: Next
						For I = 1 To 120: Input #5, CRD!(I): Next I
						For I = 1 To 120: Input #5, SITE$(I): Next I
						Close #5

						SEQ = 0

						For K = 0 To 9
							If AR1$(K) <> A1$(K) Then
								SEQ = SEQ + 1
							End If
						Next

						For K = 0 To 5
							If AR2$(K) <> A2$(K) Then
								SEQ = SEQ + 1
							End If
						Next

						For K = 0 To 3
							If AR3$(K) <> A3$(K) Then
								SEQ = SEQ + 1
							End If
						Next

						For K = 0 To 2
							If AR4$(K) <> A4$(K) Then
								SEQ = SEQ + 1
							End If
						Next

						For K = 0 To 2
							If AR5$(K) <> A5$(K) Then
								SEQ = SEQ + 1
							End If
						Next

						If AR6$(0) <> A6$(0) Then
							SEQ = SEQ + 1
						End If

						For I = 0 To 1
							If AR7$(0) <> A7$(0) Then
								SEQ = SEQ + 1
							End If
						Next

						If SEQ = 0 Then
							Call DoTheMerge (pathCurrentStats$, statFile$, ORD$(), ORD%())
						Else
							Cls
							Color 14
							Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$
							Color 15
							Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
							Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
							Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
							Color 12
							Locate 11, 24: Print "YOU MUST REALIGN ROAD STAT FILE OR"
							Locate , 31: Print "ENTER STATS MANUALLY."
							Color 13
							Locate 15, 29: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$
						End If

					End If 'Done checking for current file

				End If 'Done checking for extra file

			End If 'Done checking for changes

		End If 'Done checking for ESC
		
	Loop Until DUMMY$ <> "" Or UCase$(J$) = "N" Or UCase$(I$) = "N"Or I$ = Chr$(27)

End Sub

'----------------------------------------
'       DoTheMerge Subroutine
'----------------------------------------
'...explanation...
Sub DoTheMerge (pathCurrentStats$, statFile$, ORD$(), ORD%())

    Shared A1$(), A2$(), A3$(), A4$(), A5$(), A6$(), A7$()
    Shared AR1$(), AR2$(), AR3$(), AR4$(), AR5$(), AR6$(), AR7$()
    Shared A$(), AR$(), DI$(), DRI$(), DS$(), DSR$()
    Shared H$(), HR$()
    Shared LC$(), LCR$(), LF$(), LFR$()
    Shared LI$(), LIR$(), LK$(), LKR$()
    Shared LP$(), LPR$(), LR$(), LRR$()
    Shared O$(), PK$(), PKR$()
    Shared SITE$(), SITER$()

    Shared AF!(), AFR!(), AMR!(), AM!()
    Shared CRD!(), CRDR!()
    Shared LC!(), LCR!(), LF!(), LFR!()
    Shared LI!(), LIR!(), LK!(), LKR!(), LP!(), LPR!()
    Shared LR!(), LRR!(), PK!(), PKR!()
    Shared Z!(), Z2!(), Z1!()
    Shared ZR!(), ZR1!(), ZR2!()

    Shared AP%(), APR%()
    Shared GI%(), GIR%(), GS%(), GSR%()
    Shared O%(), T%(), TR%()

    For K = 1 To YR%
        Y% = Y% + 1
        O%(Y%) = ORD%(K)
        O$(Y%) = ORD$(K)
        T%(Y%) = TR%(K)
        H$(Y%) = HR$(K)
        CRD!(Y%) = CRDR!(K)
        SITE$(Y%) = SITER$(K)

        For I = 0 To 1
            AP%(I, Y%) = APR%(I, K)
        Next
    Next

    319
    Color 12
    Locate 18, 20: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

    For I = 0 To 13
        For K = 0 To 17
            Z2!(I, K) = Z2!(I, K) + ZR2!(I, K)
        Next K
    Next I

    For I = 0 To 27: Z!(I) = Z!(I) + ZR!(I): Z1!(I) = Z1!(I) + ZR1!(I): Next
    For I = 29 To 38: Z!(I) = Z!(I) + ZR!(I): Z1!(I) = Z1!(I) + ZR1!(I): Next
    If ZR!(28) > Z!(28) Then Z!(28) = ZR!(28)

    For I = 0 To 9:
        For I1 = 0 To 2
            GI%(I, I1) = GI%(I, I1) + GIR%(I, I1)
        Next
    Next

    For I = 0 To 14
        For I1 = 0 To 1
            GS%(I, I1) = GS%(I, I1) + GSR%(I, I1)
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
            LP$(I, 0) = LR$(I, 0)
            LP$(I, 1) = LR$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If LKR!(I) > LK!(I) Then
            LK!(I) = LKR!(I)
            LK$(I, 0) = LKR$(I, 0)
            LK$(I, 1) = LKR$(I, 1)
        End If
    Next

    For I = 0 To 2:
        If PKR!(I) > PK!(I) Then
            PK!(I) = PKR!(I)
            PK$(I, 0) = PKR$(I, 0)
            PK$(I, 1) = PKR$(I, 1)
        End If
    Next

    For I = 0 To 1:
        If LFR!(I) > LF!(I) Then
            LF!(I) = LFR!(I)
            LF$(I) = LFR$(I)
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
            LI$(I, 0) = LIR$(I, 0)
            LI$(I, 1) = LIR$(I, 1)
        End If
    Next

    Open pathCurrentStats$ + "\" + statFile$ For Output As #1
    Print #1, A$(PX)
    For I = 0 To 9: Print #1, A1$(I): Next I
    For I = 0 To 5: Print #1, A2$(I): Next I
    For I = 0 To 3: Print #1, A3$(I): Next I
    For I = 0 To 2: Print #1, A4$(I): Next I
    For I = 0 To 2: Print #1, A5$(I): Next I
    Print #1, A6$(0)
    For I = 0 To 1: Print #1, A7$(I): Next I

    For J = 0 To 13
        For K = 0 To 17: Print #1, Z2!(J, K): Next K
    Next J

    For L = 0 To 38: Print #1, Z!(L): Next L
    For L = 0 To 38: Print #1, Z1!(L): Next L
    For J = 0 To 9: Print #1, DI$(J): For J1 = 0 To 2: Print #1, GI%(J, J1): Next: Next
    For J = 0 To 14: Print #1, DS$(J): For J1 = 0 To 1: Print #1, GS%(J, J1): Next: Next
    For I = 0 To 9: Print #1, LR!(I): Print #1, LR$(I, 0): Print #1, LR$(I, 1): Next
    For I = 0 To 15: Print #1, LC!(I): Print #1, LC$(I, 0): Print #1, LC$(I, 1): Next
    For I = 0 To 3: Print #1, LP!(I): Print #1, LP$(I, 0): Print #1, LP$(I, 1): Next
    For I = 0 To 2: Print #1, LK!(I): Print #1, LK$(I, 0): Print #1, LK$(I, 1): Next
    For I = 0 To 2: Print #1, PK!(I): Print #1, PK$(I, 0): Print #1, PK$(I, 1): Next
    For I = 0 To 1: Print #1, LF!(I): Print #1, LF$(I): For I1 = 0 To 4: Print #1, AM!(I, I1): Print #1, AF!(I, I1): Next: Next
    For I = 0 To 9: Print #1, LI!(I): Print #1, LI$(I, 0): Print #1, LI$(I, 1): Next
    Close 1

    Open pathCurrentStats$ + "\" + statFile$ + "S" For Output As #6
    Write #6, Y%
    For I = 1 To 120: Write #6, O%(I): Next I
    For I = 1 To 120: Write #6, O$(I): Next I
    For I = 1 To 120: Write #6, T%(I): Next I
    For I = 1 To 120: Write #6, H$(I): Next I
    For I = 0 To 1
    For I1 = 1 To 120: Write #6, AP%(I, I1): Next: Next
    For I = 1 To 120: Write #6, CRD!(I): Next I
    For I = 1 To 120: Write #6, SITE$(I): Next I
    Close #6

    Y = 0
    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

End Sub

'----------------------------------------
'       GetDiskID_MERGE  Subroutine
'----------------------------------------
'...explanation...
Sub GetDiskID_MERGE ()

    Shared PA%()

    Do

        Color 15, 0
        Cls
        JJ = 6
        SN = 1

        Locate 6, 8: Color 15, 4

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
                Call SelectTeam_MERGE(numberTeams, YN$)
            Else
                Locate 7, 27: Print I$
                Locate 7, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0
                If NY$ = "" Then
                    Call SelectTeam_MERGE(numberTeams, NY$)
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

                    Call SelectTeam_MERGE(numberTeams, YN$)
                End If
            End If

            A$ = RTrim$(Q$(1))

            If BO% = 1 Then
                BO% = 0
            Else
                Locate 2, 33
				Color PA%(2), PA%(1): Print A$
                
				Color 15, 4

				Do
					Locate , 33: Print "ANY CHANGE (YN)";
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
'       SelectTeam_MERGE  Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_MERGE (numberTeams, YN$)

    Shared PA%()

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

        T1 = teamIndex%(Selection%)

        Open diskPaths$(0) + "FOOT." + YN$ For Random As #1 Len = 934
        Field #1, 15 As Q$(1), 15 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 2 As Q$(14), 2 As Q$(15), 200 As Q$(0), 200 As Q$(199), 200 As Q$, 200 As Q$, 78 As Q$(0)
        Get #1, T1

        For I1 = 0 To 4: PA%(I1) = CVI(Q$(I1 + 11)): Next I1

        Close 1

    End If

End Sub
