'----------------------------------------
'            ALIGN Subroutine
'----------------------------------------
'This routine presents a menu / interface
'that assists the user with aligning
'player names in separate stat files.
'(ie, Road Vs. Local) This should be done
'before merging stats, since player name
'entry may not be consistent.
Sub ALIGN ()

    _Title "3-in-1 Football - Align Stats"

    Shared teamNameAlign$

    Shared rbName_Align$(), wrName_Align$(), qbName_Align$(), krName_Align$(), prName_Align$(), punterName_Align$(), pkName_Align$()
    Shared intName_Align$(), sackName_Align$()

    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared intName$(), sackName$()

    Shared rbName_Road$(), wrName_Road$(), qbName_Road$(), krName_Road$(), prName_Road$(), punterName_Road$(), pkName_Road$()
    Shared intName_Road$(), sackName_Road$()

    Shared longCatchVal_Align!(), longCatchVal_Road!()
    Shared longFGVal_Align!(), longFGVal_Road!()
    Shared longPassVal_Align!(), longPassVal_Road!()
    Shared longKRVal_Align!(), longKRVal_Road!()
    Shared longRunVal_Align!(), longRunVal_Road!()
    Shared longPRVal_Align!(), longPRVal_Road!()
    Shared longIntVal_Align!(), longIntVal_Road!()
    Shared FGA_Align!(), FGA_Road!(), FGM_Align!(), FGM_Road!()
    Shared nbrInt_Align(), nbrInt_Road(), nbrSack_Align(), nbrSack_Road()

    Shared longCatchName_Align$(), longCatchName_Road$(), longFGName_Align$(), longFGName_Road$()
    Shared longIntName_Align$(), longIntName_Road$(), longKRName_Align$(), longKRName_Road$()

    Shared longPassName_Align$(), longPassName_Road$(), longRunName_Align$(), longRunName_Road$()
    Shared longPRName_Align$(), longPRName_Road$()

    Shared statsLRec$(), statsLFG$()
    Shared statsLInt$(), statsLKR$()
    Shared statsLPass$(), statsLRush$()
    Shared statsLPR$()

    Shared statsFGA!(), statsFGM!()
    Shared statsLRec!(), statsLFG!()
    Shared statsLPass!(), statsLKR!()
    Shared statsLInt!(), statsLRush!()
    Shared statsLPR!()
    Shared statsGI(), statsGS()

    Shared statsPlayers!(), statsTeam!(), statsOpp!()
    Shared statsPlayers_Road!(), statsTeam_Road!(), statsOpp_Road!()

    Shared teamRatings()

    Do

        'Call GetMergePaths(pathCurrentStats$, pathRoadStats$, I$)
        pathCurrentStats$ = diskPaths$(1)
        pathRoadStats$ = _SelectFolderDialog$("Select folder with ROAD stats", diskPaths$(1))

        'If UCase$(I$) = "N" And I$ <> Chr$(27) Then
        If pathRoadStats$ <> "" Then

            pathRoadStats$ = pathRoadStats$ + "\"

            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)
            Color 11

            teamNbr = 0
            Call ChooseTeam(YN$, NTMS, teamNbr, team$)

            statFile$ = nameStatFile$(team$)

            Color 12, 0
            Cls

            Locate 18, 5: Print "SEARCHING FOR FILES . . ."

            If _FileExists(pathRoadStats$ + statFile$) Then

                Open pathRoadStats$ + statFile$ For Input As #1

                Input #1, teamNameAlign$

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
                    Input #1, intName_Road$(J):
                    For J1 = 0 To 2:
                        Input #1, nbrInt_Road(J, J1)
                    Next
                Next

                For J = 0 To 14:
                    Input #1, sackName_Road$(J)
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
                    Input #1, longPassName_Road$(I, 0)
                    Input #1, longPassName_Road$(I, 1)
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
                    Input #1, longFGName_Road$(I)

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

                If _FileExists(pathCurrentStats$ + statFile$) Then

                    Call ReadTeamStats(temp$, statFile$)
                    Call AlignStats(pathRoadStats$, statFile$)
                    DUMMY$ = "OK"

                Else

                    Cls

                    Color 7
                    Locate 2, 10
                    Print "NON-EXISTANT PATH OR INCORRECT FILE NAME!!!!"
                    Print
                    Locate , 10
                    Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

                End If 'Done checking if current file exists

            Else

                Cls

                Color 7
                Locate 2, 10
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
    Shared intName$(), sackName$()

    Shared rbName_Align$(), wrName_Align$(), qbName_Align$(), krName_Align$(), prName_Align$(), punterName_Align$(), pkName_Align$()
    Shared intName_Align$(), sackName_Align$()

    Shared rbName_Road$(), wrName_Road$(), qbName_Road$(), krName_Road$(), prName_Road$(), punterName_Road$(), pkName_Road$()
    Shared intName_Road$(), sackName_Road$()

    Shared longCatchName_Align$(), longCatchName_Road$(), longFGName_Align$(), longFGName_Road$()
    Shared longKRName_Align$(), longKRName_Road$(), longPassName_Align$(), longPassName_Road$()
    Shared longRunName_Align$(), longRunName_Road$(), longPRName_Align$(), longPRName_Road$()

    Shared statsLKR$(), statsLPR$(), statsLPass$(), statsLRush$()

    Shared longCatchVal_Align!(), longCatchVal_Road!()
    Shared longFGVal_Road!(), longFGVal_Align!()
    Shared longPassVal_Align!(), longPassVal_Road!()
    Shared longKRVal_Align!(), longKRVal_Road!()
    Shared longRunVal_Align!(), longRunVal_Road!()
    Shared longPRVal_Align!(), longPRVal_Road!()
    Shared longIntVal_Align!(), longIntVal_Road!()
    Shared FGA_Align!(), FGA_Road!(), FGM_Align!(), FGM_Road!()
    Shared nbrInt_Align(), nbrInt_Road(), nbrSack_Align(), nbrSack_Road()

    Shared statsPlayers_Align!(), statsPlayers_Road!()

    Shared statsGI(), statsGS()

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Locate 3
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
        Color 7: Print rbName_Road$(I)
    Next

    For I = 0 To 5:
        Locate I + 4, 40
        Color 12: Print I; " ";
        Color 15: Print wrName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print wrName_Road$(I):
    Next

    Locate 11, 45
    Color 15: Print "CURRENT  ";
    Color 14: Print "- KICK RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2
        Color 12
        Locate I + 12, 40: Print I; " ";
        Color 15: Print krName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print krName_Road$(I)
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
        Color 7: Print qbName_Road$(I)
    Next

    Locate 16, 45
    Color 15: Print "CURRENT  ";
    Color 14: Print "- PUNT RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Color 12
        Locate I + 17, 40: Print I; " ";
        Color 15: Print prName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print prName_Road$(I)
    Next

    Color 11
    Locate LAST_ROW - 2: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Locate 3
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
    Color 7: Print punterName_Road$(0)

    For I = 0 To 1:
        Color 12
        Locate I + 4, 40: Print I; " ";
        Color 15: Print pkName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print pkName_Road$(I):
    Next

    Color 15
    Locate 7: Print "CURRENT  ";
    Color 14: Print "- INTERCEPT. -";
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
        Color 7: Print intName_Road$(I)
    Next

    For I = 0 To 14:
        Color 12
        Locate I + 8, 40: Print Using "##"; I;
        Print " ";
        Color 15: Print sackName$(I);
        Color 12: Print Tab(63); Using "##"; I;
        Print " ";
        Color 7: Print sackName_Road$(I)
    Next

    Color 11
    Locate 24, 31: Line Input "HIT ENTER TO ALIGN"; DUMMY$

    N = 0

    Do

        For J = 0 To 9

            If rbName$(N) = rbName_Road$(J) Then

                rbName_Align$(N) = rbName_Road$(J)
                longRunVal_Align!(N) = longRunVal_Road!(J)
                longCatchVal_Align!(N) = longCatchVal_Road!(J)

                For I = 0 To 1
                    longRunName_Align$(N, I) = longRunName_Road$(J, I)
                    longCatchName_Align$(N, I) = longCatchName_Road$(J, I)
                Next

                For K = 0 To 5
                    statsPlayers_Align!(N, K) = statsPlayers_Road!(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 10

    N = 0

    Do

        For J = 0 To 3

            If qbName$(N) = qbName_Road$(J) Then

                qbName_Align$(N) = qbName_Road$(J)

                For K = 0 To 2
                    statsPlayers_Align!(N + 10, K) = statsPlayers_Road!(J + 10, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 4

    N = 0

    Do
        For J = 0 To 5

            If wrName$(N) = wrName_Road$(J) Then

                wrName_Align$(N) = wrName_Road$(J)
                longCatchVal_Align!(N + 10) = longCatchVal_Road!(J + 10)
                longCatchName_Align$(N + 10, 0) = longCatchName_Road$(J + 10, 0)
                longCatchName_Align$(N + 10, 1) = longCatchName_Road$(J + 10, 1)

                For K = 6 To 8
                    statsPlayers_Align!(N, K) = statsPlayers_Road!(J, K)
                Next K

            End If
        Next J

        N = N + 1

    Loop Until N = 6

    N = 0

    Do
        For J = 0 To 3

            If qbName$(N) = qbName_Road$(J) Then

                qbName_Align$(N) = qbName_Road$(J)
                longPassVal_Align!(N) = longPassVal_Road!(J)
                longPassName_Align$(N, 0) = longPassName_Road$(J, 0)
                longPassName_Align$(N, 1) = longPassName_Road$(J, 1)

                For K = 9 To 13:
                    statsPlayers_Align!(N, K) = statsPlayers_Road!(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 4

    N = 0

    Do

        For J = 0 To 2

            If krName$(N) = krName_Road$(J) Then

                krName_Align$(N) = krName_Road$(J)
                longKRVal_Align!(N) = longKRVal_Road!(J)
                longKRName_Align$(N, 0) = longKRName_Road$(J, 0)
                longKRName_Align$(N, 1) = longKRName_Road$(J, 1)

                For K = 14 To 16
                    statsPlayers_Align!(N, K) = statsPlayers_Road!(J, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 3

    N = 0

    Do

        For J = 0 To 2

            If prName$(N) = prName_Road$(J) Then

                prName_Align$(N) = prName_Road$(J)
                longPRVal_Align!(N) = longPRVal_Road!(J)
                longPRName_Align$(N, 1) = longPRName_Road$(J, 1)
                longPRName_Align$(N, 1) = longPRName_Road$(J, 1)

                For K = 14 To 16:
                    statsPlayers_Align!(N + 3, K) = statsPlayers_Road!(J + 3, K)
                Next K

            End If

        Next J

        N = N + 1

    Loop Until N = 3

    If punterName$(0) = punterName_Road$(0) Then
        punterName_Align$(0) = punterName_Road$(0)
    End If

    N = 0

    Do

        For J = 0 To 1

            If pkName$(N) = pkName_Road$(J) Then

                pkName_Align$(N) = pkName_Road$(J)
                longFGVal_Align!(N) = longFGVal_Road!(J)
                longFGName_Align$(N) = longFGName_Road$(J)

                If N = J Then
                    For L = 3 To 6:
                        statsPlayers_Align!(L, 17) = statsPlayers_Road!(L, 17)
                    Next L
                    For L = 7 To 10:
                        statsPlayers_Align!(L, 17) = statsPlayers_Road!(L, 17)
                    Next L
                Else
                    For L = 3 To 6:
                        statsPlayers_Align!(L, 17) = statsPlayers_Road!(L + 4, 17)
                    Next L
                    For L = 7 To 10:
                        statsPlayers_Align!(L, 17) = statsPlayers_Road!(L - 4, 17)
                    Next L
                End If

                For J1 = 0 To 4:
                    FGM_Align!(N, J1) = FGM_Road!(J, J1)
                    FGA_Align!(N, J1) = FGA_Road!(J, J1)
                Next J1

            End If

        Next J

        N = N + 1

    Loop Until N = 2

    N = 0

    Do

        For J = 0 To 9

            If intName$(N) = intName_Road$(J) Then

                intName_Align$(N) = intName_Road$(J)
                For J1 = 0 To 2
                    nbrInt_Align(J, J1) = nbrInt_Road(J, J1)
                Next

            End If

        Next J

        N = N + 1

    Loop Until N = 10

    N = 0

    Do

        For J = 0 To 14

            If sackName$(N) = sackName_Road$(J) Then

                sackName_Align$(N) = sackName_Road$(J)
                For J1 = 0 To 1
                    nbrSack_Align(J, J1) = nbrSack_Road(J, J1)
                Next

            End If

        Next J

        N = N + 1

    Loop Until N = 15

    Locate 20: Print String$(26, " ")

    SEQ = 0

    For I = 0 To 9
        If rbName_Align$(I) <> rbName$(I) Then
            SEQ = SEQ + 1
        End If
    Next

    For I = 0 To 5
        If wrName_Align$(I) <> wrName$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 3
        If qbName_Align$(I) <> qbName$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 2
        If krName_Align$(I) <> krName$(I) Then SEQ = SEQ + 1
    Next

    For I = 0 To 2
        If prName_Align$(I) <> prName$(I) Then SEQ = SEQ + 1
    Next

    If punterName_Align$(I) <> punterName$(I) Then
        SEQ = SEQ + 1
    End If
    For I = 0 To 1
        If pkName_Align$(I) <> pkName$(I) Then SEQ = SEQ + 1
    Next

    If SEQ = 0 Then
        Call ReviewAndSaveAlignment(pathRoadStats$, statFile$)
    Else
        Color 14
        Locate 20
        Print "WARNING !! ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS."
        Print "YOU MUST MODIFY ROAD STAT FILE BEFORE ATTEMPTING MERGE."
        Color 13
        Locate LAST_ROW - 2, 30
        Line Input "HIT ENTER TO CONTINUE"; DUMMY$
    End If

End Sub


'----------------------------------------
'   ReviewAndSaveAlignment Subroutine
'----------------------------------------
'This routine displays the results of the
'player name alignment, then gives the end
'user a chance to actually save the results
Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)

    Shared teamNameAlign$

    'Previous name
    Shared rbName$(), wrName$(), qbName$(), krName$(), prName$(), punterName$(), pkName$()
    Shared intName$(), sackName$()

    'Current name
    Shared rbName_Align$(), wrName_Align$(), qbName_Align$()
    Shared krName_Align$(), prName_Align$(), punterName_Align$(), pkName_Align$()
    Shared intName_Align$(), sackName_Align$()

    Shared longCatchName_Align$(), longCatchName_Road$(), longFGName_Align$(), longFGName_Road$()
    Shared longIntName_Align$(), longKRName_Align$(), longPassName_Align$()
    Shared longRunName_Align$(), longPRName_Align$()

    Shared FGA_Align!(), FGM_Align!()
    Shared nbrInt_Align(), nbrInt_Road(), nbrSack_Align()

    Shared longCatchVal_Align!(), longIntVal_Align!(), longFGVal_Align!()
    Shared longPassVal_Align!(), longKRVal_Align!(), longRunVal_Align!(), longPRVal_Align!()

    Shared statsPlayers_Align!(), statsTeam_Road!(), statsOpp_Road!()

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Locate 3
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
        Color 7: Print rbName_Align$(I):
    Next

    For I = 0 To 5:
        Color 12
        Locate I + 4, 40: Print I; " ";
        Color 15: Print wrName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print wrName_Align$(I)
    Next

    Color 15
    Locate 11, 45: Print "CURRENT  ";
    Color 14: Print "- KICK RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Color 12
        Locate I + 12, 40: Print I; " ";
        Color 15: Print krName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print krName_Align$(I)
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
        Color 7: Print qbName_Align$(I)
    Next


    Color 15
    Locate 16, 45: Print "CURRENT  ";
    Color 14: Print "- PUNT RET. -";
    Color 7: Print " ROAD"

    For I = 0 To 2:
        Color 12
        Locate I + 17, 40: Print I; " ";
        Color 15: Print prName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print prName_Align$(I)
    Next

    Color 11
    Locate LAST_ROW - 2: Print "HIT ANY KEY TO CONTINUE"
    I$ = GetKeyPress$

    Cls

    Color 12
    Locate 1, 34: Print "STAT ROSTERS"

    Locate 3
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
    Color 7: Print punterName_Align$(0)

    For I = 0 To 1
        Color 12
        Locate I + 4, 40: Print I; " ";
        Color 15: Print pkName$(I);
        Color 12: Print Tab(63); I; " ";
        Color 7: Print pkName_Align$(I)
    Next

    Color 15
    Locate 7: Print "CURRENT  ";
    Color 14: Print "- INTERCEPT. -";
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
        Color 7: Print intName_Align$(I):
    Next

    For I = 0 To 14:
        Color 12
        Locate I + 8, 40: Print Using "##"; I;
        Print " ";
        Color 15: Print sackName$(I);
        Color 12: Print Tab(63); Using "##"; I;
        Print " ";
        Color 7: Print sackName_Align$(I):
    Next

    Color 11
    Locate 24, 26: Line Input "HIT ENTER TO SAVE ALIGNMENT"; DUMMY$

    Cls

    Locate 6, 30: Print "ALIGNING FILES . . ."

    Open pathRoadStats$ + statFile$ For Output As #1

    Write #1, teamNameAlign$

    For I = 0 To 9:
        Write #1, rbName_Align$(I)
    Next I

    For I = 0 To 5:
        Write #1, wrName_Align$(I)
    Next I

    For I = 0 To 3:
        Write #1, qbName_Align$(I)
    Next I

    For I = 0 To 2:
        Write #1, krName_Align$(I)
    Next I

    For I = 0 To 2:
        Write #1, prName_Align$(I)
    Next I

    Write #1, punterName_Align$(0)

    For I = 0 To 1:
        Write #1, pkName_Align$(I)
    Next I

    For J = 0 To 13
        For K = 0 To 17:
            Write #1, statsPlayers_Align!(J, K)
        Next K
    Next J

    For L = 0 To 38:
        Write #1, statsTeam_Road!(L)
    Next L

    For L = 0 To 38:
        Write #1, statsOpp_Road!(L)
    Next L

    For J = 0 To 9:

        Write #1, intName_Align$(J)

        For J1 = 0 To 2:
            Write #1, nbrInt_Align(J, J1)
        Next

    Next

    For J = 0 To 14

        Write #1, sackName_Align$(J)

        For J1 = 0 To 1
            Write #1, nbrSack_Align(J, J1)
        Next

    Next

    For I = 0 To 9:
        Write #1, longRunVal_Align!(I)
        Write #1, longRunName_Align$(I, 0)
        Write #1, longRunName_Align$(I, 1)
    Next

    For I = 0 To 15
        Write #1, longCatchVal_Align!(I)
        Write #1, longCatchName_Align$(I, 0)
        Write #1, longCatchName_Align$(I, 1)
    Next

    For I = 0 To 3:
        Write #1, longPassVal_Align!(I)
        Write #1, longPassName_Align$(I, 0)
        Write #1, longPassName_Align$(I, 1)
    Next

    For I = 0 To 2:
        Write #1, longKRVal_Align!(I)
        Write #1, longKRName_Align$(I, 0)
        Write #1, longKRName_Align$(I, 1)
    Next

    For I = 0 To 2:
        Write #1, longPRVal_Align!(I)
        Write #1, longPRName_Align$(I, 0)
        Write #1, longPRName_Align$(I, 1)
    Next

    For I = 0 To 1:

        Write #1, longFGVal_Align!(I)
        Write #1, longFGName_Align$(I)

        For I1 = 0 To 4:
            Write #1, FGM_Align!(I, I1)
            Write #1, FGA_Align!(I, I1)
        Next

    Next

    For I = 0 To 9:
        Write #1, longIntVal_Align!(I)
        Write #1, longIntName_Align$(I, 0)
        Write #1, longIntName_Align$(I, 1)
    Next

    Close #1

    Locate 8, 30: Print "RE-ALIGNMENT COMPLETED"
    Color 12
    Locate 10, 30
    Line Input "HIT ENTER TO CONTINUE"; DUMMY$

    Cls

End Sub
